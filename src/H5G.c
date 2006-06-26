/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:	H5G.c
 *		Jul 18 1997
 *		Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:	Symbol table functions.	 The functions that begin with
 *		`H5G_stab_' don't understand the naming system; they operate
 * 		on a single symbol table at a time.
 *
 *		The functions that begin with `H5G_node_' operate on the leaf
 *		nodes of a symbol table B-tree.  They should be defined in
 *		the H5Gnode.c file.
 *
 *		The remaining functions know how to traverse the group
 *		directed graph.
 *
 * Names:	Object names are a slash-separated list of components.  If
 *		the name begins with a slash then it's absolute, otherwise
 *		it's relative ("/foo/bar" is absolute while "foo/bar" is
 *		relative).  Multiple consecutive slashes are treated as
 *		single slashes and trailing slashes are ignored.  The special
 *		case `/' is the root group.  Every file has a root group.
 *
 *		API functions that look up names take a location ID and a
 *		name.  The location ID can be a file ID or a group ID and the
 *		name can be relative or absolute.
 *
 *              +--------------+----------- +--------------------------------+
 * 		| Location ID  | Name       | Meaning                        |
 *              +--------------+------------+--------------------------------+
 * 		| File ID      | "/foo/bar" | Find `foo' within `bar' within |
 *		|              |            | the root group of the specified|
 *		|              |            | file.                          |
 *              +--------------+------------+--------------------------------+
 * 		| File ID      | "foo/bar"  | Find `foo' within `bar' within |
 *		|              |            | the current working group of   |
 *		|              |            | the specified file.            |
 *              +--------------+------------+--------------------------------+
 * 		| File ID      | "/"        | The root group of the specified|
 *		|              |            | file.                          |
 *              +--------------+------------+--------------------------------+
 * 		| File ID      | "."        | The current working group of   |
 *		|              |            | the specified file.            |
 *              +--------------+------------+--------------------------------+
 * 		| Group ID     | "/foo/bar" | Find `foo' within `bar' within |
 *		|              |            | the root group of the file     |
 *		|              |            | containing the specified group.|
 *              +--------------+------------+--------------------------------+
 * 		| Group ID     | "foo/bar"  | File `foo' within `bar' within |
 *		|              |            | the specified group.           |
 *              +--------------+------------+--------------------------------+
 * 		| Group ID     | "/"        | The root group of the file     |
 *		|              |            | containing the specified group.|
 *              +--------------+------------+--------------------------------+
 * 		| Group ID     | "."        | The specified group.           |
 *              +--------------+------------+--------------------------------+
 *
 *
 *-------------------------------------------------------------------------
 */

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5G_init_interface

/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"         /* Property lists                       */

/* Local macros */
#define H5G_INIT_HEAP		8192
#define H5G_RESERVED_ATOMS	0

/* Local typedefs */

/* User data for path traversal routine for "insertion file" routine */
typedef struct {
    H5F_t *file;                                /* Pointer to the file for insertion */
} H5G_trav_ud1_t;

/* User data for path traversal routine for moving a link */
typedef struct {
    H5G_obj_t 	type;			        /* Type of object being moved */
    const char *dst_name;                       /* Destination name for moving object */
    H5G_loc_t  *dst_loc;			/* Destination location for moving object */
} H5G_trav_ud2_t;

/* User data for path traversal callback to creating link */
typedef struct {
    H5F_t *file;                                /* Pointer to the file */
    hid_t dxpl_id;                              /* Dataset transfer property list */
    H5O_link_t *lnk;                            /* Pointer to link information to insert */
} H5G_trav_ud3_t;

/* User data for path traversal routine for getting object info */
typedef struct {
    H5G_stat_t  *statbuf;			/* Stat buffer about object */
    hbool_t follow_link;                        /* Whether we are following a link or not */
    hid_t dxpl_id;                              /* Dataset transfer property list */
} H5G_trav_ud4_t;

/* User data for path traversal routine for getting soft link value */
typedef struct {
    size_t size;                                /* Size of user buffer */
    char *buf;                                  /* User buffer */
} H5G_trav_ud5_t;

/* User data for path traversal routine for removing link (i.e. unlink) */
typedef struct {
    hid_t dxpl_id;                              /* Dataset transfer property list */
} H5G_trav_ud6_t;

/* User data for path traversal routine for inserting object */
typedef struct {
    H5G_loc_t *obj_loc;         /* Object location */
    hid_t dxpl_id;              /* Dataset transfer property list */
} H5G_trav_ud7_t;

/* Package variables */

/* Local variables */

/* Declare a free list to manage the H5G_t struct */
H5FL_DEFINE(H5G_t);
H5FL_DEFINE(H5G_shared_t);

/* Private prototypes */
static char * H5G_normalize(const char *name);
static H5G_t *H5G_create(H5G_loc_t *loc, const char *name, hid_t dxpl_id,
    hid_t gcpl_id, hid_t gapl_id);
static herr_t H5G_open_oid(H5G_t *grp, hid_t dxpl_id);
static herr_t H5G_insert_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_get_objinfo_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_link_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_link_real(H5G_loc_t *link_loc, const char *link_name,
    H5F_t *file, H5O_link_t *lnk, hid_t dxpl_id);
static herr_t H5G_link(H5G_loc_t *cur_loc, const char *cur_name,
    H5G_loc_t *link_loc, const char *link_name, H5G_link_t type,
    unsigned traverse_flags, hid_t dxpl_id);
static herr_t H5G_linkval_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_linkval(H5G_loc_t *loc, const char *name, size_t size,
    char *buf/*out*/, hid_t dxpl_id);
static herr_t H5G_set_comment(H5G_loc_t *loc, const char *name,
    const char *buf, hid_t dxpl_id);
static int H5G_get_comment(H5G_loc_t *loc, const char *name,
    size_t bufsize, char *buf, hid_t dxpl_id);
static herr_t H5G_unlink_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_unlink(H5G_loc_t *loc, const char *name, hid_t dxpl_id);
static herr_t H5G_move_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_move(H5G_loc_t *src_loc, const char *src_name,
    H5G_loc_t *dst_loc, const char *dst_name, hid_t dxpl_id);
static herr_t H5G_insertion_file_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/);
static herr_t H5G_copy(H5G_loc_t *src_loc, H5G_loc_t *dst_loc, const char *dst_name, hid_t plist_id);


/*-------------------------------------------------------------------------
 * Function:	H5Gcreate
 *
 * Purpose:	Creates a new group relative to LOC_ID and gives it the
 *		specified NAME.  The group is opened for write access
 *		and it's object ID is returned.
 *
 *		The optional SIZE_HINT specifies how much file space to
 *		reserve to store the names that will appear in this
 *		group. If a non-positive value is supplied for the SIZE_HINT
 *		then a default size is chosen.
 *
 * See also:	H5Gset(), H5Gpush(), H5Gpop()
 *
 * Errors:
 *
 * Return:	Success:	The object ID of a new, empty group open for
 *				writing.  Call H5Gclose() when finished with
 *				the group.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, September 24, 1997
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate(hid_t loc_id, const char *name, size_t size_hint)
{
    H5G_loc_t	    loc;
    H5G_t	   *grp = NULL;
    hid_t           tmp_gcpl = (-1);    /* Temporary group creation property list */
    hid_t	    ret_value;

    FUNC_ENTER_API(H5Gcreate, FAIL)
    H5TRACE3("i","isz",loc_id,name,size_hint);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

#ifdef H5_GROUP_REVISION
    /* Check if we need to create a non-standard GCPL */
    if(size_hint > 0) {
        H5P_genplist_t  *gc_plist;  /* Property list created */
        H5O_ginfo_t     ginfo;          /* Group info property */

        /* Get the default property list */
        if(NULL == (gc_plist = H5I_object(H5P_GROUP_CREATE_DEFAULT)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

        /* Make a copy of the default property list */
        if((tmp_gcpl = H5P_copy_plist(gc_plist)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to copy the creation property list")

        /* Get the copy of the property list */
        if(NULL == (gc_plist = H5I_object(H5P_GROUP_CREATE_DEFAULT)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

        /* Get the group info property */
        if(H5P_get(gc_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

        /* Set the non-default local heap size hint */
        ginfo.lheap_size_hint = size_hint;
        if(H5P_set(gc_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set group info")
    } /* end if */
    else
#endif /* H5_GROUP_REVISION */
        tmp_gcpl = H5P_GROUP_CREATE_DEFAULT;

    /* Create the group */
    if(NULL == (grp = H5G_create(&loc, name, H5AC_dxpl_id, tmp_gcpl, H5P_DEFAULT)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")
    if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(tmp_gcpl > 0 && tmp_gcpl != H5P_GROUP_CREATE_DEFAULT)
        if(H5I_dec_ref(tmp_gcpl) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "unable to release property list")

    if(ret_value < 0) {
        if(grp!=NULL)
            H5G_close(grp);
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcreate() */

#ifdef H5_GROUP_REVISION

/*-------------------------------------------------------------------------
 * Function:	H5Gcreate_expand
 *
 * Purpose:	Creates a new group relative to LOC_ID and gives it the
 *		specified NAME, and creation property list GCPL_ID and access
 *              property list GAPL_ID.
 *
 *              Given the default setting, H5Gcreate_expand() will have the
 *              same function of H5Gcreate()
 *
 * Usage:       H5Gcreate_expand(loc_id, char *name, gcpl_id, gapl_id)
 *              hid_t loc_id;	  IN: File or group identifier
 *              const char *name; IN: Absolute or relative name of the new group 
 *              hid_t gcpl_id;	  IN: Property list for group creation             
 *              hid_t gapl_id;	  IN: Property list for group access             
 *
 * Example:	To create missing groups "A" and "B01" along the given path "/A/B01/grp"
 *              hid_t create_id = H5Pcreate(H5P_GROUP_CREATE);
 *              int   status = H5Pset_create_intermediate_group(create_id, TRUE);
 *              hid_t gid = H5Gcreate_expand(file_id, "/A/B01/grp", create_id, H5P_DEFAULT);
 *
 * See also:	H5Gcreate(), H5Dcreate_expand(), H5Pset_create_intermediate_group()
 *
 * Errors:
 *
 * Return:	Success:	The object ID of a new, empty group open for
 *				writing.  Call H5Gclose() when finished with
 *				the group.
 *
 *		Failure:	FAIL
 *
 * Programmer:  Peter Cao
 *	        May 08, 2005
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate_expand(hid_t loc_id, const char *name, hid_t gcpl_id, hid_t gapl_id)
{
    H5G_loc_t	    loc;
    H5G_t	   *grp = NULL;
    hid_t	    ret_value;

    FUNC_ENTER_API(H5Gcreate_expand, FAIL)
    H5TRACE4("i","isii",loc_id,name,gcpl_id,gapl_id);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

    /* Check group creation property list */
    if(H5P_DEFAULT == gcpl_id)
        gcpl_id = H5P_GROUP_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(gcpl_id, H5P_GROUP_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group create property list")

#ifdef LATER
    /* Check the group access property list */
    if(H5P_DEFAULT == gapl_id)
        gapl_id = H5P_GROUP_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(gapl_id, H5P_GROUP_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not group access property list")
#endif /* LATER */

    if(NULL == (grp = H5G_create(&loc, name, H5AC_dxpl_id, gcpl_id, gapl_id)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")
    if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(ret_value < 0) {
        if(grp!=NULL)
            H5G_close(grp);
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcreate_expand() */
#endif /* H5_GROUP_REVISION */


/*-------------------------------------------------------------------------
 * Function:	H5Gopen
 *
 * Purpose:	Opens an existing group for modification.  When finished,
 *		call H5Gclose() to close it and release resources.
 *
 * Errors:
 *
 * Return:	Success:	Object ID of the group.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 31, 1997
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gopen(hid_t loc_id, const char *name)
{
    H5G_t       *grp = NULL;
    H5G_loc_t	loc;
    H5G_loc_t   grp_loc;                /* Location used to open group */
    H5G_name_t  grp_path;            	/* Opened object group hier. path */
    H5O_loc_t   grp_oloc;            	/* Opened object object location */
    hbool_t     ent_found = FALSE;      /* Entry at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Gopen, FAIL)
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Set up opened group location to fill in */
    grp_loc.oloc = &grp_oloc;
    grp_loc.path = &grp_path;
    H5G_loc_reset(&grp_loc);

    /* Find the group object */
    if(H5G_loc_find(&loc, name, &grp_loc/*out*/, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
    ent_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5O_obj_type(&grp_oloc, H5AC_dxpl_id) != H5G_GROUP)
        HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "not a group")

    /* Open the group */
    if((grp = H5G_open(&grp_loc, H5AC_dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")

    /* Register an atom for the group */
    if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(ret_value < 0) {
        if(grp != NULL)
            H5G_close(grp);
        else {
            if(ent_found)
                H5G_name_free(&grp_path);
        } /* end else */
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* H5Gopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Gclose
 *
 * Purpose:	Closes the specified group.  The group ID will no longer be
 *		valid for accessing the group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 31, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gclose(hid_t group_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gclose, FAIL);
    H5TRACE1("e","i",group_id);

    /* Check args */
    if (NULL == H5I_object_verify(group_id,H5I_GROUP))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group");

    /*
     * Decrement the counter on the group atom.	 It will be freed if the count
     * reaches zero.
     */
    if (H5I_dec_ref(group_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close group");

done:
    FUNC_LEAVE_API(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5Giterate
 *
 * Purpose:	Iterates over the entries of a group.  The LOC_ID and NAME
 *		identify the group over which to iterate and IDX indicates
 *		where to start iterating (zero means at the beginning).	 The
 *		OPERATOR is called for each member and the iteration
 *		continues until the operator returns non-zero or all members
 *		are processed. The operator is passed a group ID for the
 *		group being iterated, a member name, and OP_DATA for each
 *		member.
 *
 * Return:	Success:	The return value of the first operator that
 *				returns non-zero, or zero if all members were
 *				processed with no operator returning non-zero.
 *
 *		Failure:	Negative if something goes wrong within the
 *				library, or the negative value returned by one
 *				of the operators.
 *
 * Programmer:	Robb Matzke
 *		Monday, March 23, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Giterate(hid_t loc_id, const char *name, int *idx_p,
	    H5G_iterate_t op, void *op_data)
{
    int         last_obj;               /* Index of last object looked at */
    int		idx;                    /* Internal location to hold index */
    herr_t	ret_value;

    FUNC_ENTER_API(H5Giterate, FAIL)
    H5TRACE5("e","is*Isxx",loc_id,name,idx_p,op,op_data);

    /* Check args */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    idx = (idx_p == NULL ? 0 : *idx_p);
    if(idx < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")
    if(!op)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no operator specified")

    /* Set number of objects looked at to zero */
    last_obj = 0;

    /* Call private function. */
    if((ret_value = H5G_obj_iterate(loc_id, name, idx, &last_obj, op, op_data, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "group iteration failed")

    /* Check for too high of a starting index (ex post facto :-) */
    /* (Skipping exactly as many entries as are in the group is currently an error) */
    if(idx > 0 && idx >= last_obj)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")

    /* Set the index we stopped at */
    if(idx_p)
        *idx_p=last_obj;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Giterate() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_num_objs
 *
 * Purpose:     Returns the number of objects in the group.  It iterates
 *              all B-tree leaves and sum up total number of group members.
 *
 * Return:	Success:        Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_num_objs(hid_t loc_id, hsize_t *num_objs)
{
    H5G_loc_t		loc;            /* Location of object */
    herr_t		ret_value;

    FUNC_ENTER_API(H5Gget_num_objs, FAIL)
    H5TRACE2("e","i*h",loc_id,num_objs);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5O_obj_type(loc.oloc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")
    if(!num_objs)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "nil pointer")

    /* Call private function. */
    if((ret_value = H5G_obj_count(loc.oloc, num_objs, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "can't determine ")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_num_objs() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_objname_by_idx
 *
 * Purpose:     Returns the name of objects in the group by giving index.
 *              If `name' is non-NULL then write up to `size' bytes into that
 *              buffer and always return the length of the entry name.
 *              Otherwise `size' is ignored and the function does not store the name,
 *              just returning the number of characters required to store the name.
 *              If an error occurs then the buffer pointed to by `name' (NULL or non-NULL)
 *              is unchanged and the function returns a negative value.
 *              If a zero is returned for the name's length, then there is no name
 *              associated with the ID.
 *
 * Return:	Success:        Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Gget_objname_by_idx(hid_t loc_id, hsize_t idx, char *name, size_t size)
{
    H5G_loc_t		loc;            /* Object location */
    ssize_t		ret_value;

    FUNC_ENTER_API(H5Gget_objname_by_idx, FAIL)
    H5TRACE4("Zs","ihsz",loc_id,idx,name,size);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5O_obj_type(loc.oloc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Call internal function*/
    if((ret_value = H5G_obj_get_name_by_idx(loc.oloc, idx, name, size, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "can't get object name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objname_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_objtype_by_idx
 *
 * Purpose:     Returns the type of objects in the group by giving index.
 *
 *
 * Return:	Success:        H5G_GROUP(1), H5G_DATASET(2), H5G_TYPE(3)
 *
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 *-------------------------------------------------------------------------
 */
H5G_obj_t
H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx)
{
    H5G_loc_t		loc;            /* Object location */
    H5G_obj_t		ret_value;

    FUNC_ENTER_API(H5Gget_objtype_by_idx, H5G_UNKNOWN)
    H5TRACE2("Go","ih",loc_id,idx);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a location ID")
    if(H5O_obj_type(loc.oloc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a group")

    /* Call internal function*/
    if((ret_value = H5G_obj_get_type_by_idx(loc.oloc, idx, H5AC_ind_dxpl_id)) == H5G_UNKNOWN)
	HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "can't get object type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objtype_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Gmove2
 *
 * Purpose:	Renames an object within an HDF5 file.  The original name SRC
 *		is unlinked from the group graph and the new name DST is
 *		inserted as an atomic operation.  Both names are interpreted
 *		relative to SRC_LOC_ID and DST_LOC_ID, which are either a file
 *		ID or a group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
	 const char *dst_name)
{
    H5G_loc_t	src_loc, *src_loc_p;
    H5G_loc_t	dst_loc, *dst_loc_p;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gmove2, FAIL)
    H5TRACE4("e","isis",src_loc_id,src_name,dst_loc_id,dst_name);

    if(src_loc_id != H5G_SAME_LOC && H5G_loc(src_loc_id, &src_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(dst_loc_id != H5G_SAME_LOC && H5G_loc(dst_loc_id, &dst_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(src_loc_id == H5G_SAME_LOC && dst_loc_id == H5G_SAME_LOC)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC")
    else if(src_loc_id == H5G_SAME_LOC)
	src_loc_p = dst_loc_p;
    else if(dst_loc_id == H5G_SAME_LOC)
	dst_loc_p = src_loc_p;
    else if(src_loc_p->oloc->file != dst_loc_p->oloc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    if(H5G_move(src_loc_p, src_name, dst_loc_p, dst_name, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to change object name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gmove2() */


/*-------------------------------------------------------------------------
 * Function:	H5Glink2
 *
 * Purpose:	Creates a link of the specified type from NEW_NAME to
 *		CUR_NAME.
 *
 *		If TYPE is H5G_LINK_HARD then CUR_NAME must name an existing
 *		object.  CUR_NAME and NEW_NAME are interpreted relative to
 *		CUR_LOC_ID and NEW_LOC_ID, which is either a file ID or a
 *		group ID.
 *
 * 		If TYPE is H5G_LINK_SOFT then CUR_NAME can be anything and is
 *		interpreted at lookup time relative to the group which
 *		contains the final component of NEW_NAME.  For instance, if
 *		CUR_NAME is `./foo' and NEW_NAME is `./x/y/bar' and a request
 *		is made for `./x/y/bar' then the actual object looked up is
 *		`./x/y/./foo'.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink2(hid_t cur_loc_id, const char *cur_name, H5G_link_t type,
	 hid_t new_loc_id, const char *new_name)
{
    H5G_loc_t	cur_loc, *cur_loc_p;
    H5G_loc_t	new_loc, *new_loc_p;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Glink2, FAIL)
    H5TRACE5("e","isGlis",cur_loc_id,cur_name,type,new_loc_id,new_name);

    /* Check arguments */
    if(cur_loc_id != H5G_SAME_LOC && H5G_loc(cur_loc_id, &cur_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(new_loc_id != H5G_SAME_LOC && H5G_loc(new_loc_id, &new_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(type != H5G_LINK_HARD && type != H5G_LINK_SOFT)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unrecognized link type")
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!new_name || !*new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    /* Set up current & new location pointers */
    cur_loc_p = &cur_loc;
    new_loc_p = &new_loc;
    if(cur_loc_id == H5G_SAME_LOC && new_loc_id == H5G_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC")
    else if(cur_loc_id == H5G_SAME_LOC)
        cur_loc_p = new_loc_p;
    else if(new_loc_id == H5G_SAME_LOC)
   	new_loc_p = cur_loc_p;
    else if(cur_loc_p->oloc->file != new_loc_p->oloc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    if(H5G_link(cur_loc_p, cur_name, new_loc_p, new_name, type, H5G_TARGET_NORMAL, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "unable to create link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Glink2() */


/*-------------------------------------------------------------------------
 * Function:	H5Gunlink
 *
 * Purpose:	Removes the specified NAME from the group graph and
 *		decrements the link count for the object to which NAME
 *		points.  If the link count reaches zero then all file-space
 *		associated with the object will be reclaimed (but if the
 *		object is open, then the reclamation of the file space is
 *		delayed until all handles to the object are closed).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gunlink(hid_t loc_id, const char *name)
{
    H5G_loc_t	loc;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gunlink, FAIL)
    H5TRACE2("e","is",loc_id,name);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Unlink */
    if(H5G_unlink(&loc, name, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to unlink object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gunlink() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_objinfo
 *
 * Purpose:	Returns information about an object.  If FOLLOW_LINK is
 *		non-zero then all symbolic links are followed; otherwise all
 *		links except the last component of the name are followed.
 *
 * Return:	Non-negative on success, with the fields of STATBUF (if
 *              non-null) initialized. Negative on failure.
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_objinfo(hid_t loc_id, const char *name, hbool_t follow_link,
	       H5G_stat_t *statbuf/*out*/)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gget_objinfo, FAIL)
    H5TRACE4("e","isbx",loc_id,name,follow_link,statbuf);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get info */
    if(H5G_get_objinfo(&loc, name, follow_link, statbuf, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_CANTINIT, FAIL, "cannot stat object")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objinfo() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_linkval
 *
 * Purpose:	Returns the value of a symbolic link whose name is NAME.  At
 *		most SIZE characters (counting the null terminator) are
 *		copied to the BUF result buffer.
 *
 * Return:	Success:	Non-negative with the link value in BUF.
 *
 * 		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_linkval(hid_t loc_id, const char *name, size_t size, char *buf/*out*/)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gget_linkval, FAIL)
    H5TRACE4("e","iszx",loc_id,name,size,buf);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get the link value */
    if(H5G_linkval(&loc, name, size, buf, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_linkval() */


/*-------------------------------------------------------------------------
 * Function:	H5Gset_comment
 *
 * Purpose:     Gives the specified object a comment.  The COMMENT string
 *		should be a null terminated string.  An object can have only
 *		one comment at a time.  Passing NULL for the COMMENT argument
 *		will remove the comment property from the object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 20, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gset_comment(hid_t loc_id, const char *name, const char *comment)
{
    H5G_loc_t	loc;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gset_comment, FAIL)
    H5TRACE3("e","iss",loc_id,name,comment);

    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    if(H5G_set_comment(&loc, name, comment, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to set comment value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gset_comment() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_comment
 *
 * Purpose:	Return at most BUFSIZE characters of the comment for the
 *		specified object.  If BUFSIZE is large enough to hold the
 *		entire comment then the comment string will be null
 *		terminated, otherwise it will not.  If the object does not
 *		have a comment value then no bytes are copied to the BUF
 *		buffer.
 *
 * Return:	Success:	Number of characters in the comment counting
 *				the null terminator.  The value returned may
 *				be larger than the BUFSIZE argument.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, July 20, 1998
 *
 *-------------------------------------------------------------------------
 */
int
H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize, char *buf)
{
    H5G_loc_t	loc;
    int	ret_value;

    FUNC_ENTER_API(H5Gget_comment, FAIL)
    H5TRACE4("Is","iszs",loc_id,name,bufsize,buf);

    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(bufsize > 0 && !buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no buffer specified")

    if((ret_value = H5G_get_comment(&loc, name, bufsize, buf, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to get comment value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_comment() */

#ifdef H5_GROUP_REVISION

/*-------------------------------------------------------------------------
 * Function:	H5Gget_create_plist
 *
 * Purpose:	Returns a copy of the group creation property list.
 *
 * Return:	Success:	ID for a copy of the group creation
 *				property list.  The property list ID should be
 *				released by calling H5Pclose().
 *
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, October 25, 2005
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gget_create_plist(hid_t group_id)
{
    htri_t	        ginfo_exists = 0;
    H5G_t		*grp = NULL;
    H5P_genplist_t      *gcpl_plist;
    H5P_genplist_t      *new_plist;
    hid_t		new_gcpl_id = FAIL;
    hid_t		ret_value = FAIL;

    FUNC_ENTER_API(H5Gget_create_plist, FAIL)
    H5TRACE1("i","i",group_id);

    /* Check args */
    if(NULL == (grp = H5I_object_verify(group_id, H5I_GROUP)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Copy the default group creation property list */
    if(NULL == (gcpl_plist = H5I_object(H5P_LST_GROUP_CREATE_g)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get default group creation property list")
    if((new_gcpl_id = H5P_copy_plist(gcpl_plist)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to copy the creation property list")
    if(NULL == (new_plist = H5I_object(new_gcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "can't get property list")

    /* Check for the group having a group info message */
    if((ginfo_exists = H5O_exists(&(grp->oloc), H5O_GINFO_ID, 0, H5AC_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header")
    if(ginfo_exists) {
        H5O_ginfo_t ginfo;		/* Group info message            */

        /* Read the group info */
        if(NULL == H5O_read(&(grp->oloc), H5O_GINFO_ID, 0, &ginfo, H5AC_dxpl_id))
            HGOTO_ERROR(H5E_SYM, H5E_BADMESG, FAIL, "can't get group info")

        /* Set the group info for the property list */
        if(H5P_set(new_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set group info")
    } /* end if */

    /* Set the return value */
    ret_value = new_gcpl_id;

done:
    if(ret_value < 0) {
        if(new_gcpl_id > 0)
            (void)H5I_dec_ref(new_gcpl_id);
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_create_plist() */
#endif /* H5_GROUP_REVISION */


/*-------------------------------------------------------------------------
 * Function:    H5Gcopy
 *
 * Purpose:     Copy an object (group or dataset) to destination location 
 *              within a file or cross files. PLIST_ID is a property list
 *              which is used to pass user options and properties to the
 *              copy. 
 *
 *              OPTIONS THAT MAY APPLY TO COPY IN THE FUTURE.
 *                  H5G_COPY_CREATE_INTERMEDIATE_GROUP_FLAG
 *                      Do not create missing groups when create a group (default)
 *                      Create missing groups when create a group
 *                  H5G_COPY_SHALLOW_HIERARCHY_FLAG
 *                      Recursively copy all objects below the group (default) 
 *                      Only immediate members.
 *                  H5G_COPY_EXPAND_SOFT_LINK_FLAG
 *                      Keep soft links as they are (default) 
 *                      Expand them into new objects
 *                  H5G_COPY_EXPAND_EXT_LINK_FLAG
 *                      Keep external links as they are (default) 
 *                      Expand them into new objects
 *                  H5G_COPY_EXPAND_OBJ_REFERENCE_FLAG
 *                      Update only the values of object references (default)
 *                      Copy objects that are pointed by references
 *                  H5G_COPY_WITHOUT_ATTR_FLAG
 *                      Copy object along with all its attributes (default)
 *                      Copy object without copying attributes
 *
 *              PROPERTIES THAT MAY APPLY TO COPY IN FUTURE
 *                  Change data layout such as chunk size
 *                  Add filter such as data compression.
 *                  Add an attribute to the copied object(s) that say the  date/time 
 *                      for the copy or other information about the source file.
 *
 * Usage:      H5Gcopy(src_loc_id, src_name, dst_loc_id, dst_name, plist_id)
 *             hid_t src_loc_id         IN: Source file or group identifier. 
 *             const char *src_name     IN: Name of the source object to be copied
 *             hid_t dst_loc_id         IN: Destination file or group identifier 
 *             const char *dst_name     IN: Name of the destination object 
 *             hid_t plist_id           IN: Properties which apply to the copy       
 *              
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao 
 *              June 4, 2005 
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gcopy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
        const char *dst_name, hid_t plist_id)
{
    H5G_loc_t	loc;                    /* Source group group location */
    H5G_loc_t	src_loc;                /* Source object group location */
    H5G_loc_t	dst_loc;                /* Destination group location */

    /* for opening the destination object */
    H5G_name_t  src_path;               /* Opened source object hier. path */
    H5O_loc_t   src_oloc;               /* Opened source object object location */
    hbool_t     ent_found = FALSE;      /* Entry at 'name' found */
    hbool_t     obj_open = FALSE;       /* Entry at 'name' found */

    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_API(H5Gcopy, FAIL)
    H5TRACE5("e","isisi",src_loc_id,src_name,dst_loc_id,dst_name,plist_id);

    /* Check arguments */
    if(H5G_loc(src_loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(H5G_loc(dst_loc_id, &dst_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no source name specified")
    if(!dst_name || !*dst_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")

    /* Set up opened group location to fill in */
    src_loc.oloc = &src_oloc;
    src_loc.path = &src_path;
    H5G_loc_reset(&src_loc);

    /* Find the source object to copy */
    if(H5G_loc_find(&loc, src_name, &src_loc/*out*/, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found")
    ent_found = TRUE;

    if(H5O_open(&src_oloc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTOPENOBJ, FAIL, "unable to open object")
    obj_open = TRUE;

    /* Get correct property list */
    if(H5P_DEFAULT == plist_id)
        plist_id = H5P_OBJECT_COPY_DEFAULT;
    else
        if(TRUE != H5P_isa_class(plist_id, H5P_OBJECT_COPY))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not object copy property list")

    if(H5G_copy(&src_loc, &dst_loc, dst_name, plist_id) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

done:
    if(ent_found)
        H5G_name_free(&src_path);
    if (obj_open)
        H5O_close(&src_oloc);

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcopy() */

/*
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 *   N O   A P I   F U N C T I O N S   B E Y O N D   T H I S   P O I N T
 *-------------------------------------------------------------------------
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function:	H5G_init_interface
 *
 * Purpose:	Initializes the H5G interface.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 * Notes:       The group creation properties are registered in the property
 *              list interface initialization routine (H5P_init_interface)
 *              so that the file creation property class can inherit from it
 *              correctly. (Which allows the file creation property list to
 *              control the group creation properties of the root group of
 *              a file) QAK - 24/10/2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_init_interface(void)
{
    H5P_genclass_t  *crt_pclass, *cpy_pclass;
    herr_t          ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_init_interface);

    /* Initialize the atom group for the group IDs */
    if(H5I_register_type(H5I_GROUP, (size_t)H5I_GROUPID_HASHSIZE, H5G_RESERVED_ATOMS,
		       (H5I_free_t)H5G_close) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize interface");

    /* ========== Group Creation Property Class Initialization ============*/
    assert(H5P_CLS_GROUP_CREATE_g!=-1);

    /* Get the pointer to group creation class */
    if(NULL == (crt_pclass = H5I_object(H5P_CLS_GROUP_CREATE_g)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class")

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_GROUP_CREATE_g == (-1)) {
        /* Register the default group creation property list */
        if((H5P_LST_GROUP_CREATE_g = H5P_create_id(crt_pclass))<0)
             HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't insert property into class")
    } /* end if */

    /* ========== Object Copy Property Class Initialization ============*/
    assert(H5P_CLS_OBJECT_COPY_g!=-1);

    /* Get the pointer to group copy class */
    if(NULL == (cpy_pclass = H5I_object(H5P_CLS_OBJECT_COPY_g)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class")

    /* Only register the default property list if it hasn't been created yet */
    if(H5P_LST_OBJECT_COPY_g == (-1)) {
        /* Register the default group copy property list */
        if((H5P_LST_OBJECT_COPY_g = H5P_create_id(cpy_pclass))<0)
             HGOTO_ERROR(H5E_PLIST, H5E_CANTREGISTER, FAIL, "can't insert property into class")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_term_interface
 *
 * Purpose:	Terminates the H5G interface
 *
 * Return:	Success:	Positive if anything is done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
int
H5G_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_term_interface)

    if (H5_interface_initialize_g) {
	if ((n = H5I_nmembers(H5I_GROUP))) {
	    H5I_clear_type(H5I_GROUP, FALSE);
	} else {
	    /* Destroy the group object id group */
	    H5I_dec_type_ref(H5I_GROUP);

            /* Free the global component buffer */
            H5G_traverse_term_interface();

	    /* Mark closed */
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5G_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5G_component
 *
 * Purpose:	Returns the pointer to the first component of the
 *		specified name by skipping leading slashes.  Returns
 *		the size in characters of the component through SIZE_P not
 *		counting leading slashes or the null terminator.
 *
 * Return:	Success:	Ptr into NAME.
 *
 *		Failure:	Ptr to the null terminator of NAME.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 *-------------------------------------------------------------------------
 */
const char *
H5G_component(const char *name, size_t *size_p)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_component)

    assert(name);

    while ('/' == *name)
        name++;
    if (size_p)
        *size_p = HDstrcspn(name, "/");

    FUNC_LEAVE_NOAPI(name)
} /* end H5G_component() */


/*-------------------------------------------------------------------------
 * Function:	H5G_normalize
 *
 * Purpose:	Returns a pointer to a new string which has duplicate and
 *              trailing slashes removed from it.
 *
 * Return:	Success:	Ptr to normalized name.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Saturday, August 16, 2003
 *
 *-------------------------------------------------------------------------
 */
static char *
H5G_normalize(const char *name)
{
    char *norm;         /* Pointer to the normalized string */
    size_t	s,d;    /* Positions within the strings */
    unsigned    last_slash;     /* Flag to indicate last character was a slash */
    char *ret_value;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_normalize);

    /* Sanity check */
    assert(name);

    /* Duplicate the name, to return */
    if (NULL==(norm=H5MM_strdup(name)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for normalized string");

    /* Walk through the characters, omitting duplicated '/'s */
    s=d=0;
    last_slash=0;
    while(name[s]!='\0') {
        if(name[s]=='/')
            if(last_slash)
                ;
            else {
                norm[d++]=name[s];
                last_slash=1;
            } /* end else */
        else {
            norm[d++]=name[s];
            last_slash=0;
        } /* end else */
        s++;
    } /* end while */

    /* Terminate normalized string */
    norm[d]='\0';

    /* Check for final '/' on normalized name & eliminate it */
    if(d>1 && last_slash)
        norm[d-1]='\0';

    /* Set return value */
    ret_value=norm;

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5G_normalize() */


/*-------------------------------------------------------------------------
 * Function:	H5G_mkroot
 *
 * Purpose:	Creates a root group in an empty file and opens it.  If a
 *		root group is already open then this function immediately
 *		returns.   If ENT is non-null then it's the symbol table
 *		entry for an existing group which will be opened as the root
 *		group.  Otherwise a new root group is created and then
 *		opened.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_mkroot(H5F_t *f, hid_t dxpl_id, H5G_loc_t *loc)
{
    H5O_loc_t	new_root_oloc;		/* New root object location */
    H5G_name_t	new_root_path;		/* New root path */
    H5G_loc_t   new_root_loc;           /* New root location information */
    H5G_loc_t   root_loc;               /* Root location information */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5G_mkroot, FAIL)

    /* check args */
    HDassert(f);
    if(f->shared->root_grp)
        HGOTO_DONE(SUCCEED)

    /* Create information needed for group nodes */
    if(H5G_node_init(f) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group node info")

    /*
     * If there is no root object then create one. The root group always starts
     * with a hard link count of one since it's pointed to by the boot block.
     */
    if (loc == NULL) {
        H5P_genplist_t *fc_plist;       /* File creation property list */
#ifdef H5_GROUP_REVISION
        H5O_ginfo_t     ginfo;          /* Group info parameters */
#endif /* H5_GROUP_REVISION */

        /* Get the file creation property list */
        /* (Which is a sub-class of the group creation property class) */
        if(NULL == (fc_plist = H5I_object(f->shared->fcpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

#ifdef H5_GROUP_REVISION
        /* Get the group info property */
        if(H5P_get(fc_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")
#endif /* H5_GROUP_REVISION */

        /* Set up group location for root group */
        new_root_loc.oloc = &new_root_oloc;
        new_root_loc.path = &new_root_path;
        H5G_loc_reset(&new_root_loc);
        loc = &new_root_loc;

	if(H5G_obj_create(f, dxpl_id,
#ifdef H5_GROUP_REVISION
                &ginfo,
#endif /* H5_GROUP_REVISION */
                loc->oloc/*out*/) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group entry")
	if(1 != H5O_link(loc->oloc, 1, dxpl_id))
	    HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "internal error (wrong link count)")
    } else {
	/*
	 * Open the root object as a group.
	 */
	if(H5O_open(loc->oloc) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open root group")
    } /* end else */

    /* Create the path names for the root group's entry */
    H5G_name_init(loc->path, "/");

    /*
     * Create the group pointer.  Also decrement the open object count so we
     * don't count the root group as an open object.  The root group will
     * never be closed.
     */
    if(NULL == (f->shared->root_grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    if(NULL == (f->shared->root_grp->shared = H5FL_CALLOC(H5G_shared_t))) {
        H5FL_FREE(H5G_t, f->shared->root_grp);
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end if */

    /* Shallow copy (take ownership) of the group object info */
    root_loc.oloc = &(f->shared->root_grp->oloc);
    root_loc.path = &(f->shared->root_grp->path);
    if(H5G_loc_copy(&root_loc, loc, H5_COPY_SHALLOW) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, FAIL, "can't copy group object location")

    f->shared->root_grp->shared->fo_count = 1;
    HDassert(1 == f->nopen_objs);
    f->nopen_objs = 0;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_mkroot() */


/*-------------------------------------------------------------------------
 * Function:	H5G_create
 *
 * Purpose:	Creates a new empty group with the specified name. The name
 *		is either an absolute name or is relative to LOC.
 *
 * Return:	Success:	A handle for the group.	 The group is opened
 *				and should eventually be close by calling
 *				H5G_close().
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 *-------------------------------------------------------------------------
 */
static H5G_t *
H5G_create(H5G_loc_t *loc, const char *name,
                  hid_t dxpl_id, hid_t gcpl_id, hid_t UNUSED gapl_id)
{
    H5G_t	*grp = NULL;	/*new group			*/
    H5F_t       *file = NULL;   /* File new group will be in    */
    H5P_genplist_t  *gc_plist;  /* Property list created */
#ifdef H5_GROUP_REVISION
    H5O_ginfo_t ginfo;          /* Group info */
#endif /* H5_GROUP_REVISION */
    unsigned    oloc_init = 0;  /* Flag to indicate that the group object location was created successfully */
    H5G_loc_t   grp_loc;        /* Group location wrapper structure */
    H5G_t	*ret_value;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_create)

    /* check args */
    HDassert(loc);
    HDassert(name && *name);
    HDassert(gcpl_id != H5P_DEFAULT);
#ifdef LATER
    HDassert(gapl_id != H5P_DEFAULT);
#endif /* LATER */

    /* create an open group */
    if(NULL == (grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (grp->shared = H5FL_CALLOC(H5G_shared_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* What file is the group being added to? */
    if(NULL == (file = H5G_insertion_file(loc, name, dxpl_id)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to locate insertion point")

    /* Get the property list */
    if(NULL == (gc_plist = H5I_object(gcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list")

#ifdef H5_GROUP_REVISION
    /* Get the group info property */
    if(H5P_get(gc_plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get group info")
#endif /* H5_GROUP_REVISION */

    /* Create the group object header */
    if(H5G_obj_create(file, dxpl_id,
#ifdef H5_GROUP_REVISION
            &ginfo,
#endif /* H5_GROUP_REVISION */
            &(grp->oloc)/*out*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to create group object header")
    oloc_init = 1;    /* Indicate that the object location information is valid */

    /* Insert child name into parent */
    grp_loc.oloc = &(grp->oloc);
    grp_loc.path = &(grp->path);
    if(H5G_insert(loc, name, &grp_loc, dxpl_id, gc_plist) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group")

    /* Add group to list of open objects in file */
    if(H5FO_top_incr(grp->oloc.file, grp->oloc.addr) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINC, NULL, "can't incr object ref. count")
    if(H5FO_insert(grp->oloc.file, grp->oloc.addr, grp->shared) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group into list of open objects")

    grp->shared->fo_count = 1;

    /* Set return value */
    ret_value = grp;

done:
    if(ret_value == NULL) {
        /* Check if we need to release the file-oriented symbol table info */
        if(oloc_init) {
            if(H5O_close(&(grp->oloc)) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "unable to release object header")
            if(H5O_delete(file, dxpl_id, grp->oloc.addr) < 0)
                HDONE_ERROR(H5E_SYM, H5E_CANTDELETE, NULL, "unable to delete object header")
        } /* end if */
        if(grp != NULL) {
            if(grp->shared != NULL)
                H5FL_FREE(H5G_shared_t, grp->shared);
            H5FL_FREE(H5G_t,grp);
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_create() */


/*-------------------------------------------------------------------------
 * Function:	H5G_open
 *
 * Purpose:	Opens an existing group.  The group should eventually be
 *		closed by calling H5G_close().
 *
 * Return:	Success:	Ptr to a new group.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_open(H5G_loc_t *loc, hid_t dxpl_id)
{
    H5G_t           *grp = NULL;
    H5G_shared_t    *shared_fo = NULL;
    H5G_t           *ret_value = NULL;

    FUNC_ENTER_NOAPI(H5G_open, NULL)

    /* Check args */
    HDassert(loc);

    /* Allocate the group structure */
    if(NULL == (grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate space for group")

    /* Shallow copy (take ownership) of the group location object */
    if(H5O_loc_copy(&(grp->oloc), loc->oloc, H5_COPY_SHALLOW) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "can't copy object location")
    if(H5G_name_copy(&(grp->path), loc->path, H5_COPY_SHALLOW) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "can't copy path")

    /* Check if group was already open */
    if((shared_fo = H5FO_opened(grp->oloc.file, grp->oloc.addr)) == NULL) {

        /* Clear any errors from H5FO_opened() */
        H5E_clear_stack(NULL);

        /* Open the group object */
        if(H5G_open_oid(grp, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "not found")

        /* Add group to list of open objects in file */
        if(H5FO_insert(grp->oloc.file, grp->oloc.addr, grp->shared) < 0) {
            H5FL_FREE(H5G_shared_t, grp->shared);
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group into list of open objects")
        } /* end if */

        /* Increment object count for the object in the top file */
        if(H5FO_top_incr(grp->oloc.file, grp->oloc.addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINC, NULL, "can't increment object count")

        /* Set open object count */
        grp->shared->fo_count = 1;
    } /* end if */
    else {
        /* Point to shared group info */
        grp->shared = shared_fo;

        /* Increment shared reference count */
        shared_fo->fo_count++;

        /* Check if the object has been opened through the top file yet */
        if(H5FO_top_count(grp->oloc.file, grp->oloc.addr) == 0) {
            /* Open the object through this top file */
            if(H5O_open(&(grp->oloc)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object header")
        } /* end if */

        /* Increment object count for the object in the top file */
        if(H5FO_top_incr(grp->oloc.file, grp->oloc.addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINC, NULL, "can't increment object count")
    } /* end else */

    /* Set return value */
    ret_value = grp;

done:
    if (!ret_value && grp)
        H5FL_FREE(H5G_t,grp);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_open() */


/*-------------------------------------------------------------------------
 * Function:	H5G_open_oid
 *
 * Purpose:	Opens an existing group.  The group should eventually be
 *		closed by calling H5G_close().
 *
 * Return:	Success:	Ptr to a new group.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *	    Wednesday, March	17, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_open_oid(H5G_t *grp, hid_t dxpl_id)
{
    hbool_t             obj_opened = FALSE;
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5G_open_oid)

    /* Check args */
    HDassert(grp);

    /* Allocate the shared information for the group */
    if(NULL == (grp->shared = H5FL_CALLOC(H5G_shared_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Grab the object header */
    if(H5O_open(&(grp->oloc)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
    obj_opened = TRUE;

    /* Check if this object has the right message(s) to be treated as a group */
    if(H5O_exists(&(grp->oloc), H5O_STAB_ID, 0, dxpl_id) <= 0
#ifdef H5_GROUP_REVISION
            && H5O_exists(&(grp->oloc), H5O_LINFO_ID, 0, dxpl_id) <= 0
#endif /* H5_GROUP_REVISION */
            )
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "not a group")

done:
    if(ret_value < 0) {
        if(obj_opened)
            H5O_close(&(grp->oloc));
        if(grp->shared)
            H5FL_FREE(H5G_shared_t, grp->shared);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_open_oid() */


/*-------------------------------------------------------------------------
 * Function:	H5G_close
 *
 * Purpose:	Closes the specified group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_close(H5G_t *grp)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_close, FAIL)

    /* Check args */
    HDassert(grp && grp->shared);
    HDassert(grp->shared->fo_count > 0);

    --grp->shared->fo_count;

    if(0 == grp->shared->fo_count) {
        HDassert(grp != H5G_rootof(H5G_fileof(grp)));

        /* Remove the group from the list of opened objects in the file */
        if(H5FO_top_decr(grp->oloc.file, grp->oloc.addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't decrement count for object")
        if(H5FO_delete(grp->oloc.file, H5AC_dxpl_id, grp->oloc.addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't remove group from list of open objects")
        if(H5O_close(&(grp->oloc)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close")
        H5FL_FREE(H5G_shared_t, grp->shared);
    } else {
        /* Decrement the ref. count for this object in the top file */
        if(H5FO_top_decr(grp->oloc.file, grp->oloc.addr) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't decrement count for object")

        /* Check reference count for this object in the top file */
        if(H5FO_top_count(grp->oloc.file, grp->oloc.addr) == 0)
            if(H5O_close(&(grp->oloc)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close")

        /* If this group is a mount point and the mount point is the last open
         * reference to the group, then attempt to close down the file hierarchy
         */
        if(grp->shared->mounted && grp->shared->fo_count == 1) {
            /* Attempt to close down the file hierarchy */
            if(H5F_try_close(grp->oloc.file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problem attempting file close")
        } /* end if */
    } /* end else */

    if(H5G_name_free(&(grp->path)) < 0) {
        H5FL_FREE(H5G_t,grp);
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't free group entry name")
    } /* end if */

    H5FL_FREE(H5G_t,grp);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_close() */


/*-------------------------------------------------------------------------
 * Function:    H5G_free
 *
 * Purpose:	Free memory used by an H5G_t struct (and its H5G_shared_t).
 *          Does not close the group or decrement the reference count.
 *          Used to free memory used by the root group.
 *
 * Return:  Success:    Non-negative
 *	        Failure:    Negative
 *
 * Programmer:  James Laird
 *              Tuesday, September 7, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_free(H5G_t *grp)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_free, FAIL)

    HDassert(grp && grp->shared);

    H5FL_FREE(H5G_shared_t, grp->shared);
    H5FL_FREE(H5G_t, grp);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_free() */


/*-------------------------------------------------------------------------
 * Function:	H5G_rootof
 *
 * Purpose:	Return a pointer to the root group of the file.  If the file
 *		is part of a virtual file then the root group of the virtual
 *		file is returned.
 *
 * Return:	Success:	Ptr to the root group of the file.  Do not
 *				free the pointer -- it points directly into
 *				the file struct.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, October 13, 1998
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_rootof(H5F_t *f)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_rootof)

    while(f->mtab.parent)
        f = f->mtab.parent;

    FUNC_LEAVE_NOAPI(f->shared->root_grp)
} /* end H5G_rootof() */


/*-------------------------------------------------------------------------
 * Function:	H5G_oloc
 *
 * Purpose:	Returns a pointer to the object location for a group.
 *
 * Return:	Success:	Ptr to group entry
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 24, 1998
 *
 *-------------------------------------------------------------------------
 */
H5O_loc_t *
H5G_oloc(H5G_t *grp)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_oloc)

    FUNC_LEAVE_NOAPI(grp ? &(grp->oloc) : NULL)
} /* end H5G_oloc() */


/*-------------------------------------------------------------------------
 * Function:	H5G_nameof
 *
 * Purpose:	Returns a pointer to the hier. name for a group.
 *
 * Return:	Success:	Ptr to hier. name
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 *-------------------------------------------------------------------------
 */
H5G_name_t *
H5G_nameof(H5G_t *grp)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_nameof)

    FUNC_LEAVE_NOAPI(grp ? &(grp->path) : NULL)
} /* end H5G_nameof() */


/*-------------------------------------------------------------------------
 * Function:	H5G_fileof
 *
 * Purpose:	Returns the file to which the specified group belongs.
 *
 * Return:	Success:	File pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 24, 1998
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5G_fileof(H5G_t *grp)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_fileof)

    HDassert(grp);

    FUNC_LEAVE_NOAPI(grp->oloc.file)
} /* end H5G_fileof() */


/*-------------------------------------------------------------------------
 * Function:	H5G_insert_cb
 *
 * Purpose:	Path traversal callback for inserting an object in a group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_insert_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud7_t *udata = (H5G_trav_ud7_t *)_udata;     /* User data passed in */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_insert_cb)

    /* Check for object using name already */
    if(obj_loc != NULL)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name already exists")

    /* Insert object into group */
    if(H5G_loc_insert(grp_loc, name, udata->obj_loc, TRUE, udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert name")

done:
    if(ret_value < 0) {
        /* Release the group location for the object */
        /* (Group traversal callbacks are responsible for either taking ownership
         *  of the group location for the object, or freeing it. - QAK)
         */
        if(obj_loc)
            H5G_loc_free(obj_loc);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_insert_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_insert
 *
 * Purpose:	Inserts a symbol table entry into the group graph.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, September 19, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_insert(H5G_loc_t *loc, const char *name, H5G_loc_t *obj_loc, hid_t dxpl_id, H5P_genplist_t *oc_plist)
{
    H5G_trav_ud7_t udata;        /* User data for callback routine */
    unsigned    target_flags = H5G_TARGET_NORMAL;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_insert, FAIL)

    /* Check args. */
    HDassert(loc);
    HDassert(name && *name);
    HDassert(obj_loc);

    /* Check for intermediate group creation flag present */
    if(oc_plist != NULL) {
        unsigned crt_intmd_group;

        if(H5P_get(oc_plist, H5G_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get property value for creating missing groups")

        if (crt_intmd_group > 0)
            target_flags |= H5G_CRT_INTMD_GROUP;
    } /* end if */

    /* Set up user data callback for path traversal */
    udata.obj_loc = obj_loc;
    udata.dxpl_id = dxpl_id;

    /*
     * Lookup and insert the name -- it shouldn't exist yet.
     */
    if(H5G_traverse(loc, name, target_flags, H5G_insert_cb, &udata, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "can't insert object in group")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_cb
 *
 * Purpose:	Callback for creating a link to an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    char *old_link_name = NULL;         /* Pointer to hold the old link name */
    hbool_t old_link_name_set = FALSE;  /* Indicate that we've replaced the old link name */
    H5G_trav_ud3_t *udata = (H5G_trav_ud3_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_cb)

    /* Check if the name in this group resolved to a valid location */
    /* (which is not what we want) */
    if(obj_loc != NULL)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name already exists")

    /* Check for crossing file boundaries with a new hard link */
    if(udata->lnk->type == H5G_LINK_HARD) {
        /* Check that both objects are in same file */
        if(grp_loc->oloc->file->shared != udata->file->shared)
            HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "interfile hard links are not allowed")
    } /* end if */

    /* Set the link's name correctly */
    /* Casting away const OK -QAK */
    old_link_name = udata->lnk->name;
    udata->lnk->name = name;
    old_link_name_set = TRUE;

    /* Insert link into group */
    if(H5G_obj_insert(grp_loc->oloc, name, udata->lnk, (hbool_t)(udata->lnk->type == H5G_LINK_HARD ? TRUE : FALSE), udata->dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create new name/link for object")

done:
    if(ret_value < 0) {
        /* Release the group location for the object */
        /* (Group traversal callbacks are responsible for either taking ownership
         *  of the group location for the object, or freeing it. - QAK)
         */
        if(obj_loc)
            H5G_loc_free(obj_loc);
    } /* end if */

    /* Return the link's name to it's original value */
    if(old_link_name_set)
        udata->lnk->name = old_link_name;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_real
 *
 * Purpose:	Creates a link at a path location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, December  5, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_real(H5G_loc_t *link_loc, const char *link_name, H5F_t *file,
    H5O_link_t *lnk, hid_t dxpl_id)
{
    H5G_trav_ud3_t udata;               /* User data for callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_real)

    /* Check args */
    HDassert(link_loc);
    HDassert(link_name && *link_name);
    HDassert(lnk);

    /* Set up user data */
    udata.file = file;
    udata.lnk = lnk;
    udata.dxpl_id = dxpl_id;

    /* Traverse the destination path & create new link */
    if(H5G_traverse(link_loc, link_name, H5G_TARGET_NORMAL, H5G_link_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "can't insert link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_real() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link
 *
 * Purpose:	Creates a link from NEW_NAME to CUR_NAME.  See H5Glink() for
 *		full documentation.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, April  6, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link(H5G_loc_t *cur_loc, const char *cur_name,
    H5G_loc_t *link_loc, const char *link_name,
    H5G_link_t type, unsigned traverse_flags, hid_t dxpl_id)
{
    char *norm_cur_name = NULL;	        /* Pointer to normalized current name */
    char *norm_link_name = NULL;	/* Pointer to normalized link name */
    H5F_t *link_file = NULL;            /* Pointer to file to link to */
    H5O_link_t lnk;                     /* Link to insert */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link)

    /* Check args */
    HDassert(cur_loc);
    HDassert(link_loc);
    HDassert(cur_name && *cur_name);
    HDassert(link_name && *link_name);

    /* Get normalized copies of the current and new names */
    if((norm_cur_name = H5G_normalize(cur_name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")
    if((norm_link_name = H5G_normalize(link_name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    switch(type) {
        case H5G_LINK_HARD:
            {
                H5O_loc_t obj_oloc;             /* Location of object to link to */

                /* Get object location for object pointed to */
                if(H5G_obj_find(cur_loc, norm_cur_name, traverse_flags, NULL, &obj_oloc, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found")

                /* Construct link information for eventual insertion */
                lnk.u.hard.addr = obj_oloc.addr;

                /* Set destination's file information */
                link_file = obj_oloc.file;
            } /* end case */
            break;

        case H5G_LINK_SOFT:
            /* Construct link information for eventual insertion */
            lnk.u.soft.name = norm_cur_name;

            /* Set destination's file information */
            link_file = NULL;      /* no file info necessary for soft link */
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */

    /* Set up common link data */
    lnk.type = type;
#ifdef H5_HAVE_GETTIMEOFDAY
    {
        struct timeval now_tv;

        HDgettimeofday(&now_tv, NULL);
        lnk.ctime = now_tv.tv_sec;
    }
#else /* H5_HAVE_GETTIMEOFDAY */
    lnk.ctime = HDtime(NULL);
#endif /* H5_HAVE_GETTIMEOFDAY */
    lnk.cset = H5T_CSET_ASCII;          /* XXX: Allow user to set this */
    /* lnk.name = name; */              /* This will be set in callback */

    /* Create actual link to the object */
    if(H5G_link_real(link_loc, norm_link_name, link_file, &lnk, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to register new name for object")

done:
    /* Free the normalized path names */
    if(norm_cur_name)
        H5MM_xfree(norm_cur_name);
    if(norm_link_name)
        H5MM_xfree(norm_link_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_objinfo_cb
 *
 * Purpose:	Callback for retrieving info about an object.  This routine
 *              gets the info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 20, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_get_objinfo_cb(H5G_loc_t *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud4_t *udata = (H5G_trav_ud4_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_objinfo_cb)

    /* Check if the name in this group resolved to a valid link */
    if(lnk == NULL && obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Only modify user's buffer if it's available */
    if(udata->statbuf) {
        H5G_stat_t *statbuf = udata->statbuf;   /* Convenience pointer for statbuf */

        /* Reset buffer */
        HDmemset(statbuf, 0, sizeof(H5G_stat_t));

        /* Common code to retrieve the file's fileno */
        /* (Use the object location's file info, if it's available) */
        if(H5F_get_fileno((obj_loc ? obj_loc : grp_loc)->oloc->file, &statbuf->fileno[0]) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "unable to read fileno")

        /* Get info for soft link */
        /* (If we don't follow the link, we can retrieve info about the soft link itself) */
        if(!udata->follow_link && lnk && lnk->type == H5G_LINK_SOFT) {
            /* Set object type */
	    statbuf->type = H5G_LINK;

            /* Get length of link value */
	    statbuf->linklen = HDstrlen(lnk->u.soft.name) + 1; /*count the null terminator*/
        } /* end if */
        /* Get info for hard link */
        else {
            /* Get object type */
	    statbuf->type = H5O_obj_type(obj_loc->oloc, udata->dxpl_id);
            if(statbuf->type == H5G_UNKNOWN)
                H5E_clear_stack(NULL);  /* clear any errors resulting from checking type */

	    /* Get basic info for object */
	    statbuf->objno[0] = (unsigned long)(obj_loc->oloc->addr);
#if H5_SIZEOF_UINT64_T > H5_SIZEOF_LONG
	    statbuf->objno[1] = (unsigned long)(obj_loc->oloc->addr >> 8 * sizeof(long));
#else
	    statbuf->objno[1] = 0;
#endif
	    statbuf->nlink = H5O_link(obj_loc->oloc, 0, udata->dxpl_id);

            /* Get creation time for object */
            if(NULL == H5O_read(obj_loc->oloc, H5O_MTIME_ID, 0, &(statbuf->mtime), udata->dxpl_id)) {
                H5E_clear_stack(NULL);
                if(NULL == H5O_read(obj_loc->oloc, H5O_MTIME_NEW_ID, 0, &(statbuf->mtime), udata->dxpl_id)) {
                    H5E_clear_stack(NULL);
                    statbuf->mtime = 0;
                } /* end if */
            } /* end if */

            /* Get object header information */
            if(H5O_get_info(obj_loc->oloc, &(statbuf->ohdr), udata->dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object header information")
        } /* end else */
    } /* end if */

done:
    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_objinfo_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_objinfo
 *
 * Purpose:	Returns information about an object.
 *
 * Return:	Success:	Non-negative with info about the object
 *				returned through STATBUF if it isn't the null
 *				pointer.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_get_objinfo(const H5G_loc_t *loc, const char *name, hbool_t follow_link,
    H5G_stat_t *statbuf/*out*/, hid_t dxpl_id)
{
    H5G_trav_ud4_t udata;           /* User data for callback */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_get_objinfo, FAIL)

    HDassert(loc);
    HDassert(name && *name);

    /* Set up user data for retrieving information */
    udata.statbuf = statbuf;
    udata.follow_link = follow_link;
    udata.dxpl_id = dxpl_id;

    /* Traverse the group hierarchy to locate the object to get info about */
    if(H5G_traverse(loc, name, (unsigned)(follow_link ? H5G_TARGET_NORMAL : H5G_TARGET_SLINK),
            H5G_get_objinfo_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_objinfo() */


/*-------------------------------------------------------------------------
 * Function:	H5G_linkval_cb
 *
 * Purpose:	Callback for retrieving soft link value for an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 20, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_linkval_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t *lnk,
    H5G_loc_t UNUSED *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud5_t *udata = (H5G_trav_ud5_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_linkval_cb)

    /* Check if the name in this group resolved to a valid link */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    if(H5G_LINK_SOFT != lnk->type)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object is not a symbolic link")

    /* Copy to output buffer */
    if(udata->size > 0 && udata->buf) {
        HDstrncpy(udata->buf, lnk->u.soft.name, udata->size);
        if(HDstrlen(lnk->u.soft.name) >= udata->size)
            udata->buf[udata->size - 1] = '\0';
    } /* end if */

done:
    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_linkval_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_linkval
 *
 * Purpose:	Returns the value of a symbolic link.
 *
 * Return:	Success:	Non-negative, with at most SIZE bytes of the
 *				link value copied into the BUF buffer.  If the
 *				link value is larger than SIZE characters
 *				counting the null terminator then the BUF
 *				result will not be null terminated.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, April 13, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_linkval(H5G_loc_t *loc, const char *name, size_t size, char *buf/*out*/, hid_t dxpl_id)
{
    H5G_trav_ud5_t udata;           /* User data for callback */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_linkval)

    /* Set up user data for retrieving information */
    udata.size = size;
    udata.buf = buf;

    /* Traverse the group hierarchy to locate the object to get info about */
    if(H5G_traverse(loc, name, H5G_TARGET_SLINK, H5G_linkval_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5G_linkval() */


/*-------------------------------------------------------------------------
 * Function:	H5G_set_comment
 *
 * Purpose:	(Re)sets the comment for an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Monday, July 20, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_set_comment(H5G_loc_t *loc, const char *name, const char *buf, hid_t dxpl_id)
{
    H5O_loc_t	obj_oloc;                  /* Object's location */
    H5O_name_t	comment;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_set_comment)

    /* Get the symbol table entry for the object */
    if(H5G_obj_find(loc, name, H5G_TARGET_NORMAL, NULL, &obj_oloc/*out*/, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Remove the previous comment message if any */
    if(H5O_remove(&obj_oloc, H5O_NAME_ID, 0, TRUE, dxpl_id) < 0)
        H5E_clear_stack(NULL);

    /* Add the new message */
    if(buf && *buf) {
        /* Casting away const OK -QAK */
	comment.s = (char *)buf;
	if(H5O_modify(&obj_oloc, H5O_NAME_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, &comment, dxpl_id) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to set comment object header message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_set_comment() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_comment
 *
 * Purpose:	Get the comment value for an object.
 *
 * Return:	Success:	Number of bytes in the comment including the
 *				null terminator.  Zero if the object has no
 *				comment.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Monday, July 20, 1998
 *
 *-------------------------------------------------------------------------
 */
static int
H5G_get_comment(H5G_loc_t *loc, const char *name, size_t bufsize, char *buf, hid_t dxpl_id)
{
    H5O_name_t	comment;
    H5O_loc_t	obj_oloc;               /* Object's location */
    int	ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_comment)

    /* Get the symbol table entry for the object */
    if(H5G_obj_find(loc, name, H5G_TARGET_NORMAL, NULL, &obj_oloc/*out*/, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the message */
    comment.s = NULL;
    if(NULL == H5O_read(&obj_oloc, H5O_NAME_ID, 0, &comment, dxpl_id)) {
	if(buf && bufsize > 0)
            buf[0] = '\0';
	ret_value = 0;
    } else {
        if(buf && bufsize)
	   HDstrncpy(buf, comment.s, bufsize);
	ret_value = (int)HDstrlen(comment.s);
	H5O_reset(H5O_NAME_ID, &comment);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_comment() */


/*-------------------------------------------------------------------------
 * Function:	H5G_unlink_cb
 *
 * Purpose:	Callback for unlinking an object.  This routine
 *              deletes the link
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_unlink_cb(H5G_loc_t *grp_loc/*in*/, const char *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud6_t *udata = (H5G_trav_ud6_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_unlink_cb)

    /* Check if the name in this group resolved to a valid link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Check for removing '.' */
    if(lnk == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't delete self")

    /* Remove the link from the group */
    if(H5G_loc_remove(grp_loc, name, obj_loc, udata->dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to unlink name from group")

done:
    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_unlink_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_unlink
 *
 * Purpose:	Unlink a name from a group.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 17, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_unlink(H5G_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5G_trav_ud6_t      udata;                  /* User data for callback */
    char		*norm_name = NULL;	/* Pointer to normalized name */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_unlink)

    /* Sanity check */
    HDassert(loc);
    HDassert(name && *name);

    /* Get normalized copy of the name */
    if((norm_name = H5G_normalize(name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Set up user data for creating soft link */
    udata.dxpl_id = dxpl_id;

    if(H5G_traverse(loc, norm_name, H5G_TARGET_SLINK|H5G_TARGET_MOUNT, H5G_unlink_cb, &udata, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

done:
    /* Free the normalized path name */
    if(norm_name)
        H5MM_xfree(norm_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_unlink() */


/*-------------------------------------------------------------------------
 * Function:	H5G_move_cb
 *
 * Purpose:	Callback for moving an object.  This routine replaces the
 *              names of open objects with the moved object in the path
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_move_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud2_t *udata = (H5G_trav_ud2_t *)_udata;   /* User data passed in */
    H5RS_str_t         *dst_name_r = NULL;      /* Ref-counted version of dest name */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_move_cb)

    /* Check if the name in this group resolved to a valid link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Fix names up */
    dst_name_r = H5RS_wrap(udata->dst_name);
    HDassert(dst_name_r);
    if(H5G_name_replace(udata->type, obj_loc, dst_name_r, udata->dst_loc, H5G_NAME_MOVE) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ")

done:
    /* Cleanup */
    if(dst_name_r)
        H5RS_decr(dst_name_r);

    /* Release the group location for the object */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    if(obj_loc)
        H5G_loc_free(obj_loc);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_move_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_move
 *
 * Purpose:	Atomically rename an object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, September 25, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_move(H5G_loc_t *src_loc, const char *src_name, H5G_loc_t *dst_loc,
		const char *dst_name, hid_t dxpl_id)
{
    H5O_loc_t           src_oloc;       /* Location of object linked to */
    H5O_link_t          lnk;            /* Link information for object to move */
    hbool_t link_valid = FALSE;         /* Flag to indicate that the link information is valid */
    H5G_trav_ud2_t      udata;          /* User data for traversal */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_move)

    /* Sanity check */
    HDassert(src_loc);
    HDassert(dst_loc);
    HDassert(src_name && *src_name);
    HDassert(dst_name && *dst_name);

    /* Get copy of link to move */
    if(H5G_obj_find(src_loc, src_name, (H5G_TARGET_MOUNT|H5G_TARGET_SLINK), &lnk, &src_oloc, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
    link_valid = TRUE;

    /* Release name for link (it will have a new name in destination) */
    lnk.name = H5MM_xfree(lnk.name);

    /* Create new link to the object */
    if(H5G_link_real(dst_loc, dst_name, src_oloc.file, &lnk, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to register new name for object")

    /* Set up user data for name replacement */
    /* Get object type */
    switch(lnk.type) {
        case H5G_LINK_HARD:
            if(H5G_UNKNOWN == (udata.type = H5O_obj_type(&src_oloc, dxpl_id)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object type to move")
            break;

        case H5G_LINK_SOFT:
            udata.type = H5G_LINK;
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */
    udata.dst_name = dst_name;
    udata.dst_loc = dst_loc;

    /* Search the open ID list and replace names for the move operation
     */
    if(H5G_traverse(src_loc, src_name, H5G_TARGET_MOUNT|H5G_TARGET_SLINK, H5G_move_cb, &udata, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link")

    /* Remove the old name */
    if(H5G_unlink(src_loc, src_name, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to unlink old object name")

done:
    /* If there's valid information in the link, reset it */
    if(link_valid) {
#ifdef H5_GROUP_REVISION
        H5O_reset(H5O_LINK_ID, &lnk);
#else /* H5_GROUP_REVISION */
        /* Free information for link (but don't free link pointer) */
        if(lnk.type == H5G_LINK_SOFT)
            lnk.u.soft.name = H5MM_xfree(lnk.u.soft.name);
        lnk.name = H5MM_xfree(lnk.name);
#endif /* H5_GROUP_REVISION */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_move() */


/*-------------------------------------------------------------------------
 * Function:	H5G_insertion_file_cb
 *
 * Purpose:	Callback for finding insertion file.  This routine sets the
 *              correct information for the file pointer to return.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_insertion_file_cb(H5G_loc_t *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/)
{
    H5G_trav_ud1_t *udata = (H5G_trav_ud1_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_insertion_file_cb)

    /* Check if the name in this group resolves to a valid location */
    /* (which is not what we want) */
    if(obj_loc != NULL)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name already exists")

    /* Get file pointer for location */
    udata->file = grp_loc->oloc->file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_insertion_file_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_insertion_file
 *
 * Purpose:	Given a location and name that specifies a not-yet-existing
 *		object return the file into which the object is about to be
 *		inserted.
 *
 * Return:	Success:	File pointer
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, October 14, 1998
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5G_insertion_file(H5G_loc_t *loc, const char *name, hid_t dxpl_id)
{
    H5F_t      *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_insertion_file, NULL)

    HDassert(loc);
    HDassert(name && *name);

    /* Check if the location the object will be inserted into is part of a
     * file mounting chain (either a parent or a child) and perform a more
     * rigorous determination of the location's file (which traverses into
     * mounted files, etc.).
     */
    if(H5F_has_mount(loc->oloc->file) || H5F_is_mount(loc->oloc->file)) {
        H5G_trav_ud1_t udata;           /* User data for traversal */

        /*
         * Look up the name to get the containing group and to make sure the name
         * doesn't already exist.
         */
        if(H5G_traverse(loc, name, H5G_TARGET_NORMAL, H5G_insertion_file_cb, &udata, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_EXISTS, NULL, "name already exists")

        /* Set return value */
        ret_value = udata.file;
    } /* end if */
    else
        /* Use the location's file */
        ret_value = loc->oloc->file;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_insertion_file() */


/*-------------------------------------------------------------------------
 * Function:	H5G_free_grp_name
 *
 * Purpose:	Free the 'ID to name' buffers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2002
 *
 * Comments: Used now only on the root group close, in H5F_close()
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_free_grp_name(H5G_t *grp)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5G_free_grp_name, FAIL)

    /* Check args */
    HDassert(grp && grp->shared);
    HDassert(grp->shared->fo_count > 0);

    /* Free the path */
    H5G_name_free(&(grp->path));

done:
     FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_free_grp_name() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_shared_count
 *
 * Purpose:	Queries the group object's "shared count"
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, July	 5, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_get_shared_count(H5G_t *grp)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_get_shared_count);

    /* Check args */
    HDassert(grp && grp->shared);

    FUNC_LEAVE_NOAPI(grp->shared->fo_count);
} /* end H5G_get_shared_count() */


/*-------------------------------------------------------------------------
 * Function:	H5G_mount
 *
 * Purpose:	Sets the 'mounted' flag for a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, July 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_mount(H5G_t *grp)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_mount);

    /* Check args */
    HDassert(grp && grp->shared);
    HDassert(grp->shared->mounted == FALSE);

    /* Set the 'mounted' flag */
    grp->shared->mounted = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5G_mount() */


/*-------------------------------------------------------------------------
 * Function:	H5G_unmount
 *
 * Purpose:	Resets the 'mounted' flag for a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, July 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_unmount(H5G_t *grp)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_unmount);

    /* Check args */
    HDassert(grp && grp->shared);
    HDassert(grp->shared->mounted == TRUE);

    /* Reset the 'mounted' flag */
    grp->shared->mounted = FALSE;

    FUNC_LEAVE_NOAPI(SUCCEED);
} /* end H5G_unmount() */


/*-------------------------------------------------------------------------
 * Function:    H5G_copy
 *
 * Purpose:     Copy an object to destination location 
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao 
 *              June 4, 2005 
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_copy(H5G_loc_t *src_loc, H5G_loc_t *dst_loc, const char *dst_name, hid_t plist_id)
{
    H5P_genplist_t  *gcrt_plist=NULL;           /* Group create property list created */
    H5P_genplist_t  *gcpy_plist=NULL;           /* Group copy property list created */
    hid_t           dxpl_id=H5AC_dxpl_id;
    H5G_name_t      new_path;                   /* Copied object group hier. path */
    H5O_loc_t       new_oloc;                   /* Copied object object location */
    H5G_loc_t       new_loc;                    /* Group location of object copied */
    hbool_t         entry_inserted=FALSE;       /* Flag to indicate that the new entry was inserted into a group */
    hbool_t         gcrt_plist_created=FALSE;   /* Flag to indicate if H5G_CREATE_INTERMEDIATE_GROUP_FLAG is set */
    unsigned        cpy_option = 0;             /* Copy options */
    H5P_genclass_t  *gcrt_class;	       /* Group creation property class */
    hid_t	    gcplist_id = H5P_DEFAULT;   /* Group creation property list */
    herr_t          ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI(H5G_copy, FAIL);

    HDassert(src_loc);
    HDassert(src_loc->oloc->file);
    HDassert(dst_loc);
    HDassert(dst_loc->oloc->file);
    HDassert(dst_name);

    /* Get the property list */
    if(NULL == (gcpy_plist = H5I_object(plist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Retrieve the copy parameters */
    if(H5P_get(gcpy_plist, H5G_CPY_OPTION_NAME, &cpy_option) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get object copy flag")

    /* Set up copied object location to fill in */
    new_loc.oloc = &new_oloc;
    new_loc.path = &new_path;
    H5G_loc_reset(&new_loc);
    new_oloc.file = dst_loc->oloc->file;

    /* copy the object from the source file to the destination file */
    if(H5O_copy_header(src_loc->oloc, &new_oloc, dxpl_id, cpy_option) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

    /* Create group creatiion property to create missing groups */
    if((cpy_option & H5G_COPY_CREATE_INTERMEDIATE_GROUP_FLAG) > 0) {
        if(NULL == (gcrt_class = H5I_object_verify(H5P_GROUP_CREATE, H5I_GENPROP_CLS)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list class")

        /* Create the new property list */
        if((gcplist_id = H5P_create_id(gcrt_class)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTCREATE, FAIL, "unable to create property list")
        gcrt_plist_created = TRUE;
    } else
        gcplist_id = H5P_GROUP_CREATE_DEFAULT;

    /* Get a pointer to the group creation property list */
    if(NULL == (gcrt_plist = H5I_object(gcplist_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Set the intermediate group creation property, if requested */
    if(gcrt_plist_created) {
        unsigned crt_intmd_group = TRUE;

        if(H5P_set(gcrt_plist, H5G_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set intermediate group creation flag")
    } /* end if */

    /* Insert the new object in the destination file's group */
    if(H5G_insert(dst_loc, dst_name, &new_loc, dxpl_id, gcrt_plist) < 0)
	HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to insert the name")
    entry_inserted = TRUE;

done:
    /* Free the ID to name buffers */
    if(entry_inserted)
        H5G_loc_free(&new_loc);

    if(gcplist_id>0 && gcrt_plist_created) {
        if(H5I_dec_ref(gcplist_id)<0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTDEC, FAIL, "unable to decrement ref count on property list")
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_copy() */

