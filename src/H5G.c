/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* File access				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5HLprivate.h"	/* Local Heaps				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/

/* Local macros */
#define H5G_INIT_HEAP		8192
#define H5G_RESERVED_ATOMS	0

/* Local typedefs */

/*
 * This table contains a list of object types, descriptions, and the
 * functions that determine if some object is a particular type.  The table
 * is allocated dynamically.
 */
typedef struct H5G_typeinfo_t {
    int 	type;			        /*one of the public H5G_* types	     */
    htri_t	(*isa)(H5G_entry_t*, hid_t);	/*function to determine type	     */
    char	*desc;			        /*description of object type	     */
} H5G_typeinfo_t;

/* Package variables */

/* Local variables */
static H5G_typeinfo_t *H5G_type_g = NULL;	/*object typing info	*/
static size_t H5G_ntypes_g = 0;			/*entries in type table	*/
static size_t H5G_atypes_g = 0;			/*entries allocated	*/

/* Declare a free list to manage the H5G_t struct */
H5FL_DEFINE(H5G_t);
H5FL_DEFINE(H5G_shared_t);

/* Declare extern the PQ free list for the wrapped strings */
H5FL_BLK_EXTERN(str_buf);

/* Declare a free list to manage haddr_t's */
H5FL_DEFINE(haddr_t);

/* Private prototypes */
static herr_t H5G_register_type(int type, htri_t(*isa)(H5G_entry_t*, hid_t),
				 const char *desc);
static const char * H5G_basename(const char *name, size_t *size_p);
static char * H5G_normalize(const char *name);
static H5G_t *H5G_create(H5G_entry_t *loc, const char *name, size_t size_hint, hid_t dxpl_id);
static htri_t H5G_isa(H5G_entry_t *ent, hid_t dxpl_id);
static htri_t H5G_link_isa(H5G_entry_t *ent, hid_t dxpl_id);
static herr_t H5G_open_oid(H5G_t *grp, hid_t dxpl_id);
static herr_t H5G_link(H5G_entry_t *cur_loc, const char *cur_name,
    H5G_entry_t *link_loc, const char *link_name, H5G_link_t type,
    unsigned namei_flags, hid_t dxpl_id);
static herr_t H5G_get_num_objs(H5G_entry_t *grp, hsize_t *num_objs, hid_t dxpl_id);
static ssize_t H5G_get_objname_by_idx(H5G_entry_t *loc, hsize_t idx, char* name, size_t size, hid_t dxpl_id);
static int H5G_get_objtype_by_idx(H5G_entry_t *loc, hsize_t idx, hid_t dxpl_id);
static herr_t H5G_linkval(H5G_entry_t *loc, const char *name, size_t size,
    char *buf/*out*/, hid_t dxpl_id);
static herr_t H5G_set_comment(H5G_entry_t *loc, const char *name,
    const char *buf, hid_t dxpl_id);
static int H5G_get_comment(H5G_entry_t *loc, const char *name,
    size_t bufsize, char *buf, hid_t dxpl_id);
static herr_t H5G_unlink(H5G_entry_t *loc, const char *name, hid_t dxpl_id);
static herr_t H5G_move(H5G_entry_t *src_loc, const char *src_name,
    H5G_entry_t *dst_loc, const char *dst_name, hid_t dxpl_id);


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
    H5G_entry_t		   *loc = NULL;
    H5G_t	   *grp = NULL;
    hid_t	    ret_value;

    FUNC_ENTER_API(H5Gcreate, FAIL)
    H5TRACE3("i","isz",loc_id,name,size_hint);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given")

    /* Create the group */
    if(NULL == (grp = H5G_create(loc, name, size_hint, H5AC_dxpl_id)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group")
    if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(ret_value < 0) {
        if(grp!=NULL)
            H5G_close(grp);
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Gcreate() */


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
    H5G_entry_t	*loc = NULL;
    H5G_entry_t	 ent;
    hbool_t      ent_found = FALSE;     /* Entry at 'name' found */
    hid_t       ret_value = FAIL;

    FUNC_ENTER_API(H5Gopen, FAIL);
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");

    /* Open the parent group, making sure it's a group */
    if(H5G_find(loc, name, &ent/*out*/, H5AC_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "group not found")
    ent_found = TRUE;

    /* Check that the object found is the correct type */
    if(H5G_get_type(&ent, H5AC_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Open the group */
    if((grp = H5G_open(&ent, H5AC_dxpl_id)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")

    /* Register an atom for the group */
    if((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group")

done:
    if(ret_value < 0) {
        if(grp != NULL)
            H5G_close(grp);
        else {
            if(ent_found && ent.header)
                H5G_name_free(&ent);
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
    H5O_stab_t		stab;		/*info about B-tree	*/
    int			idx;
    H5G_bt_it_ud1_t	udata;
    H5G_t		*grp = NULL;
    herr_t		ret_value;

    FUNC_ENTER_API(H5Giterate, FAIL)
    H5TRACE5("e","is*Isxx",loc_id,name,idx_p,op,op_data);

    /* Check args */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    idx = (idx_p == NULL ? 0 : *idx_p);
    if (!idx_p)
        idx_p = &idx;
    if(idx < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")
    if(!op)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no operator specified")

    /*
     * Open the group on which to operate.  We also create a group ID which
     * we can pass to the application-defined operator.
     */
    if ((udata.group_id = H5Gopen (loc_id, name)) <0)
        HGOTO_ERROR (H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group");
    if ((grp=H5I_object(udata.group_id))==NULL) {
        H5Gclose(udata.group_id);
        HGOTO_ERROR (H5E_ATOM, H5E_BADATOM, FAIL, "bad group atom");
    }

    /* Get the B-tree info */
    if (NULL==H5O_read (&(grp->ent), H5O_STAB_ID, 0, &stab, H5AC_dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address")

    /* Build udata to pass through H5B_iterate() to H5G_node_iterate() */
    udata.skip = idx;
    udata.heap_addr = stab.heap_addr;
    udata.op = op;
    udata.op_data = op_data;

    /* Set the number of entries looked at to zero */
    udata.final_ent = 0;

    /* Iterate over the group members */
    if ((ret_value = H5B_iterate (H5G_fileof(grp), H5AC_dxpl_id, H5B_SNODE,
              H5G_node_iterate, stab.btree_addr, &udata))<0)
        HERROR (H5E_SYM, H5E_CANTNEXT, "iteration operator failed");

    H5I_dec_ref (udata.group_id); /*also closes 'grp'*/

    /* Check for too high of a starting index (ex post facto :-) */
    /* (Skipping exactly as many entries as are in the group is currently an error) */
    if(idx > 0 && idx >= udata.final_ent)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified");

    /* Set the index we stopped at */
    *idx_p = udata.final_ent;

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
    H5G_entry_t		*loc = NULL;    /* Pointer to symbol table entry */
    herr_t		ret_value;

    FUNC_ENTER_API(H5Gget_num_objs, FAIL)
    H5TRACE2("e","i*h",loc_id,num_objs);

    /* Check args */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5G_get_type(loc,H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")
    if(!num_objs)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "nil pointer")

    /* Call private function. */
    ret_value = H5G_get_num_objs(loc, num_objs, H5AC_ind_dxpl_id);

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
    H5G_entry_t		*loc = NULL;    /* Pointer to symbol table entry */
    ssize_t		ret_value;

    FUNC_ENTER_API(H5Gget_objname_by_idx, FAIL)
    H5TRACE4("Zs","ihsz",loc_id,idx,name,size);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5G_get_type(loc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Call internal function*/
    ret_value = H5G_get_objname_by_idx(loc, idx, name, size, H5AC_ind_dxpl_id);

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
#ifdef H5_WANT_H5_V1_4_COMPAT
int
H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx)
{
    H5G_entry_t		*loc = NULL;    /* Pointer to symbol table entry */
    int 		ret_value;

    FUNC_ENTER_API(H5Gget_objtype_by_idx, FAIL);
    H5TRACE2("Is","ih",loc_id,idx);

    /* Check args */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID");
    if(H5G_get_type(loc,H5AC_ind_dxpl_id)!=H5G_GROUP)
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a group");

    /*call private function*/
    ret_value = H5G_get_objtype_by_idx(loc, idx, H5AC_ind_dxpl_id);

done:
    FUNC_LEAVE_API(ret_value);

}

#else /*H5_WANT_H5_V1_4_COMPAT*/
H5G_obj_t
H5Gget_objtype_by_idx(hid_t loc_id, hsize_t idx)
{
    H5G_entry_t		*loc = NULL;    /* Pointer to symbol table entry */
    H5G_obj_t		ret_value;

    FUNC_ENTER_API(H5Gget_objtype_by_idx, H5G_UNKNOWN)
    H5TRACE2("Go","ih",loc_id,idx);

    /* Check args */
    if(NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a location ID")
    if(H5G_get_type(loc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a group")

    /* Call internal function*/
    ret_value = (H5G_obj_t)H5G_get_objtype_by_idx(loc, idx, H5AC_ind_dxpl_id);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objtype_by_idx() */
#endif /*H5_WANT_H5_V1_4_COMPAT*/


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
    H5G_entry_t		*src_loc = NULL;
    H5G_entry_t		*dst_loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gmove2, FAIL)
    H5TRACE4("e","isis",src_loc_id,src_name,dst_loc_id,dst_name);

    if(src_loc_id != H5G_SAME_LOC && NULL==(src_loc=H5G_loc(src_loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(dst_loc_id != H5G_SAME_LOC && NULL==(dst_loc=H5G_loc(dst_loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(src_loc_id == H5G_SAME_LOC && dst_loc_id == H5G_SAME_LOC)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC")
    else if(src_loc_id == H5G_SAME_LOC)
	src_loc = dst_loc;
    else if(dst_loc_id == H5G_SAME_LOC)
	dst_loc = src_loc;
    else if(src_loc->file != dst_loc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    if(H5G_move(src_loc, src_name, dst_loc, dst_name, H5AC_dxpl_id) < 0)
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
	 hid_t link_loc_id, const char *link_name)
{
    H5G_entry_t	*cur_loc = NULL;
    H5G_entry_t *link_loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Glink2, FAIL)
    H5TRACE5("e","isGlis",cur_loc_id,cur_name,type,link_loc_id,link_name);

    /* Check arguments */
    if(cur_loc_id != H5G_SAME_LOC && NULL==(cur_loc=H5G_loc(cur_loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(link_loc_id != H5G_SAME_LOC && NULL==(link_loc=H5G_loc(link_loc_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(type != H5G_LINK_HARD && type != H5G_LINK_SOFT)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unrecognized link type")
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!link_name || !*link_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(cur_loc_id == H5G_SAME_LOC && link_loc_id == H5G_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC")
    else if(cur_loc_id == H5G_SAME_LOC)
        cur_loc = link_loc;
    else if(link_loc_id == H5G_SAME_LOC)
   	link_loc = cur_loc;
    else if(cur_loc->file != link_loc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    if(H5G_link(cur_loc, cur_name, link_loc, link_name, type, H5G_TARGET_NORMAL, H5AC_dxpl_id) <0)
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
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gunlink, FAIL)
    H5TRACE2("e","is",loc_id,name);

    /* Check arguments */
    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Unlink */
    if (H5G_unlink(loc, name, H5AC_dxpl_id)<0)
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
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gget_objinfo, FAIL)
    H5TRACE4("e","isbx",loc_id,name,follow_link,statbuf);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get info */
    if (H5G_get_objinfo (loc, name, follow_link, statbuf, H5AC_ind_dxpl_id) < 0)
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
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gget_linkval, FAIL)
    H5TRACE4("e","iszx",loc_id,name,size,buf);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Get the link value */
    if (H5G_linkval (loc, name, size, buf, H5AC_ind_dxpl_id)<0)
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
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gset_comment, FAIL)
    H5TRACE3("e","iss",loc_id,name,comment);

    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    if (H5G_set_comment(loc, name, comment, H5AC_dxpl_id)<0)
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
    H5G_entry_t	*loc = NULL;
    int	ret_value;

    FUNC_ENTER_API(H5Gget_comment, FAIL)
    H5TRACE4("Is","iszs",loc_id,name,bufsize,buf);

    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(bufsize > 0 && !buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no buffer specified")

    if ((ret_value=H5G_get_comment(loc, name, bufsize, buf, H5AC_ind_dxpl_id))<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to get comment value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_comment() */

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
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_init_interface(void)
{
    herr_t          ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_init_interface);

    /* Initialize the atom group for the group IDs */
    if (H5I_init_group(H5I_GROUP, H5I_GROUPID_HASHSIZE, H5G_RESERVED_ATOMS,
		       (H5I_free_t)H5G_close) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize interface");

    /*
     * Initialize the type info table.  Begin with the most general types and
     * end with the most specific. For instance, any object that has a data
     * type message is a datatype but only some of them are datasets.
     */
    H5G_register_type(H5G_TYPE,    H5T_isa,  "datatype");
    H5G_register_type(H5G_GROUP,   H5G_isa,  "group");
    H5G_register_type(H5G_DATASET, H5D_isa,  "dataset");
    H5G_register_type(H5G_LINK,    H5G_link_isa,  "link");

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
	    H5I_clear_group(H5I_GROUP, FALSE);
	} else {
            size_t	i;

	    /* Empty the object type table */
	    for (i=0; i<H5G_ntypes_g; i++)
		H5MM_xfree(H5G_type_g[i].desc);
	    H5G_ntypes_g = H5G_atypes_g = 0;
	    H5G_type_g = H5MM_xfree(H5G_type_g);

	    /* Destroy the group object id group */
	    H5I_destroy_group(H5I_GROUP);

            /* Free the global component buffer */
            H5G_namei_term_interface();

	    /* Mark closed */
	    H5_interface_initialize_g = 0;
	    n = 1; /*H5I*/
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5G_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5G_register_type
 *
 * Purpose:	Register a new object type so H5G_get_type() can detect it.
 *		One should always register a general type before a more
 *		specific type.  For instance, any object that has a datatype
 *		message is a datatype, but only some of those objects are
 *		datasets.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, November  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_register_type(int type, htri_t(*isa)(H5G_entry_t*, hid_t), const char *_desc)
{
    char	*desc = NULL;
    size_t	i;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_register_type);

    assert(type>=0);
    assert(isa);
    assert(_desc);

    /* Copy the description */
    if (NULL==(desc=H5MM_strdup(_desc)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for object type description");

    /*
     * If the type is already registered then just update its entry without
     * moving it to the end
     */
    for (i=0; i<H5G_ntypes_g; i++) {
	if (H5G_type_g[i].type==type) {
	    H5G_type_g[i].isa = isa;
	    H5MM_xfree(H5G_type_g[i].desc);
	    H5G_type_g[i].desc = desc;
            HGOTO_DONE(SUCCEED);
	}
    }

    /* Increase table size */
    if (H5G_ntypes_g>=H5G_atypes_g) {
	size_t n = MAX(32, 2*H5G_atypes_g);
	H5G_typeinfo_t *x = H5MM_realloc(H5G_type_g,
					 n*sizeof(H5G_typeinfo_t));
	if (!x)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for objec type table");
	H5G_atypes_g = n;
	H5G_type_g = x;
    }

    /* Add a new entry */
    H5G_type_g[H5G_ntypes_g].type = type;
    H5G_type_g[H5G_ntypes_g].isa = isa;
    H5G_type_g[H5G_ntypes_g].desc = desc; /*already copied*/
    H5G_ntypes_g++;

done:
    if (ret_value<0)
        H5MM_xfree(desc);
    FUNC_LEAVE_NOAPI(ret_value);
}


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
 * Function:	H5G_basename
 *
 * Purpose:	Returns a pointer to the last component of the specified
 *		name. The length of the component is returned through SIZE_P.
 *		The base name is followed by zero or more slashes and a null
 *		terminator, but SIZE_P does not count the slashes or the null
 *		terminator.
 *
 * Note:	The base name of the root directory is a single slash.
 *
 * Return:	Success:	Ptr to base name.
 *
 *		Failure:	Ptr to the null terminator.
 *
 * Programmer:	Robb Matzke
 *              Thursday, September 17, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static const char *
H5G_basename(const char *name, size_t *size_p)
{
    size_t	i;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_basename);

    /* Find the end of the base name */
    i = HDstrlen(name);
    while (i>0 && '/'==name[i-1])
        --i;

    /* Skip backward over base name */
    while (i>0 && '/'!=name[i-1])
        --i;

    /* Watch out for root special case */
    if ('/'==name[i] && size_p)
        *size_p = 1;

    FUNC_LEAVE_NOAPI(name+i);
}


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
H5G_mkroot(H5F_t *f, hid_t dxpl_id, H5G_entry_t *ent)
{
    H5G_entry_t	new_root;		/*new root object		*/
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
    if(!ent) {
	ent = &new_root;
        H5G_ent_reset(ent);
	if (H5G_stab_create (f, dxpl_id, (size_t)H5G_SIZE_HINT, ent/*out*/)<0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create root group")
	if (1 != H5O_link (ent, 1, dxpl_id))
	    HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "internal error (wrong link count)")
    } else {
	/*
	 * Open the root object as a group.
	 */
	if(H5O_open(ent) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open root group")
    } /* end else */

    /* Create the path names for the root group's entry */
    H5G_name_init(ent, "/");

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

    /* Shallow copy (take ownership) of the group entry object */
    if(H5G_ent_copy(&(f->shared->root_grp->ent), ent, H5_COPY_SHALLOW)<0)
        HGOTO_ERROR (H5E_SYM, H5E_CANTCOPY, FAIL, "can't copy group entry")

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
H5G_create(H5G_entry_t *loc, const char *name, size_t size_hint, hid_t dxpl_id)
{
    H5G_t	*grp = NULL;	/*new group			*/
    H5F_t       *file = NULL;   /* File new group will be in    */
    unsigned    stab_init=0;    /* Flag to indicate that the symbol table was created successfully */
    H5G_t	*ret_value;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_create)

    /* check args */
    HDassert(loc);
    HDassert(name && *name);

    /* create an open group */
    if(NULL == (grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (grp->shared = H5FL_CALLOC(H5G_shared_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* What file is the group being added to? */
    if(NULL == (file = H5G_insertion_file(loc, name, dxpl_id)))
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "unable to locate insertion point")

    /* Create the group entry */
    if (H5G_stab_create(file, dxpl_id, size_hint, &(grp->ent)/*out*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create grp");
    stab_init=1;    /* Indicate that the symbol table information is valid */

    /* insert child name into parent */
    if(H5G_insert(loc,name,&(grp->ent), dxpl_id)<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group");

    /* Add group to list of open objects in file */
    if(H5FO_top_incr(grp->ent.file, grp->ent.header)<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINC, NULL, "can't incr object ref. count")
    if(H5FO_insert(grp->ent.file, grp->ent.header, grp->shared)<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group into list of open objects")

    grp->shared->fo_count = 1;

    /* Set return value */
    ret_value = grp;

done:
    if(ret_value == NULL) {
        /* Check if we need to release the file-oriented symbol table info */
        if(stab_init) {
            if(H5O_close(&(grp->ent))<0)
                HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, NULL, "unable to release object header")
            if(H5O_delete(file, dxpl_id, grp->ent.header) < 0)
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
 * Function:	H5G_isa
 *
 * Purpose:	Determines if an object has the requisite messages for being
 *		a group.
 *
 * Return:	Success:	TRUE if the required group messages are
 *				present; FALSE otherwise.
 *
 *		Failure:	FAIL if the existence of certain messages
 *				cannot be determined.
 *
 * Programmer:	Robb Matzke
 *              Monday, November  2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5G_isa(H5G_entry_t *ent, hid_t dxpl_id)
{
    htri_t	ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5G_isa);

    assert(ent);

    if ((ret_value=H5O_exists(ent, H5O_STAB_ID, 0, dxpl_id))<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_link_isa
 *
 * Purpose:	Determines if an object has the requisite form for being
 *		a soft link.
 *
 * Return:	Success:	TRUE if the symbol table entry is of type
 *				H5G_LINK; FALSE otherwise.
 *
 *		Failure:	Shouldn't fail.
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, June 23, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5G_link_isa(H5G_entry_t *ent, hid_t UNUSED dxpl_id)
{
    htri_t	ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_link_isa);

    assert(ent);

    if(ent->type == H5G_CACHED_SLINK)
        ret_value=TRUE;
    else
        ret_value=FALSE;

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5G_link_isa() */


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
H5G_open(H5G_entry_t *ent, hid_t dxpl_id)
{
    H5G_t           *grp = NULL;
    H5G_shared_t    *shared_fo = NULL;
    H5G_t           *ret_value = NULL;

    FUNC_ENTER_NOAPI(H5G_open, NULL)

    /* Check args */
    HDassert(ent);

    /* Allocate the group structure */
    if(NULL == (grp = H5FL_CALLOC(H5G_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate space for group")

    /* Shallow copy (take ownership) of the group entry object */
    if(H5G_ent_copy(&(grp->ent), ent, H5_COPY_SHALLOW)<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTCOPY, NULL, "can't copy group entry")

    /* Check if group was already open */
    if((shared_fo = H5FO_opened(grp->ent.file, grp->ent.header))==NULL) {

        /* Clear any errors from H5FO_opened() */
        H5E_clear();

        /* Open the group object */
        if(H5G_open_oid(grp, dxpl_id) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "not found")

        /* Add group to list of open objects in file */
        if(H5FO_insert(grp->ent.file, grp->ent.header, grp->shared)<0) {
            H5FL_FREE(H5G_shared_t, grp->shared);
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert group into list of open objects")
        } /* end if */

        /* Increment object count for the object in the top file */
        if(H5FO_top_incr(grp->ent.file, grp->ent.header) < 0)
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
        if(H5FO_top_count(grp->ent.file, grp->ent.header) == 0) {
            /* Open the object through this top file */
            if(H5O_open(&(grp->ent)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open object header")
        } /* end if */

        /* Increment object count for the object in the top file */
        if(H5FO_top_incr(grp->ent.file, grp->ent.header) < 0)
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
    hbool_t             ent_opened = FALSE;
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5G_open_oid)

    /* Check args */
    HDassert(grp);

    /* Allocate the shared information for the group */
    if(NULL == (grp->shared = H5FL_CALLOC(H5G_shared_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Grab the object header */
    if (H5O_open(&(grp->ent)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group")
    ent_opened = TRUE;

    /* Check if this object has the right message(s) to be treated as a group */
    if(H5O_exists(&(grp->ent), H5O_STAB_ID, 0, dxpl_id) <= 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "not a group")

done:
    if(ret_value < 0) {
        if(grp) {
            if(ent_opened)
                H5O_close(&(grp->ent));
            if(grp->shared)
                H5FL_FREE(H5G_shared_t, grp->shared);
        } /* end if */
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
        if(H5FO_top_decr(grp->ent.file, grp->ent.header) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't decrement count for object")
        if(H5FO_delete(grp->ent.file, H5AC_dxpl_id, grp->ent.header)<0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't remove group from list of open objects")
        if(H5O_close(&(grp->ent)) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close")
        H5FL_FREE(H5G_shared_t, grp->shared);
    } else {
        /* Decrement the ref. count for this object in the top file */
        if(H5FO_top_decr(grp->ent.file, grp->ent.header) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "can't decrement count for object")

        /* Check reference count for this object in the top file */
        if(H5FO_top_count(grp->ent.file, grp->ent.header) == 0)
            if(H5O_close(&(grp->ent)) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close")

        /* If this group is a mount point and the mount point is the last open
         * reference to the group, then attempt to close down the file hierarchy
         */
        if(grp->shared->mounted && grp->shared->fo_count == 1) {
            /* Attempt to close down the file hierarchy */
            if(H5F_try_close(grp->ent.file) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problem attempting file close")
        } /* end if */

        if(H5G_name_free(&(grp->ent))<0) {
            H5FL_FREE (H5G_t,grp);
            HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "can't free group entry name")
        }
    }

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
H5G_insert(H5G_entry_t *loc, const char *name, H5G_entry_t *ent, hid_t dxpl_id)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_insert, FAIL)

    /* Check args. */
    HDassert(loc);
    HDassert(name && *name);
    HDassert(ent);

    /*
     * Lookup and insert the name -- it shouldn't exist yet.
     */
    if (H5G_namei(loc, name, NULL, NULL, NULL, H5G_TARGET_NORMAL, NULL, H5G_NAMEI_INSERT, ent, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "already exists")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_find
 *
 * Purpose:	Finds an object with the specified NAME at location LOC.  On
 *		successful return, GRP_ENT (if non-null) will be initialized
 *		with the symbol table information for the group in which the
 *		object appears (it will have an undefined object header
 *		address if the object is the root object) and OBJ_ENT will be
 *		initialized with the symbol table entry for the object
 *		(OBJ_ENT is optional when the caller is interested only in
 * 		the existence of the object).
 *
 * Errors:
 *
 * Return:	Success:	Non-negative, see above for values of GRP_ENT
 *				and OBJ_ENT.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 12 1997
 *
 * Modifications:
 *              Removed the "H5G_entry_t *grp_ent" parameter, since it was unused
 *              Quincey Koziol
 *		Aug 29 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_find(H5G_entry_t *loc, const char *name,
         H5G_entry_t *obj_ent/*out*/, hid_t dxpl_id)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_find, FAIL);

    /* check args */
    assert (loc);
    assert (name && *name);

    if (H5G_namei(loc, name, NULL, NULL, obj_ent, H5G_TARGET_NORMAL, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_entof
 *
 * Purpose:	Returns a pointer to the entry for a group.
 *
 * Return:	Success:	Ptr to group entry
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_entof (H5G_t *grp)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_entof);

    FUNC_LEAVE_NOAPI(grp ? &(grp->ent) : NULL);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5G_fileof (H5G_t *grp)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOFUNC here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_fileof);

    assert (grp);

    FUNC_LEAVE_NOAPI(grp->ent.file);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_loc
 *
 * Purpose:	Given an object ID return a symbol table entry for the
 *		object.
 *
 * Return:	Success:	Group pointer.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Tuesday, March 24, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_entry_t *
H5G_loc (hid_t loc_id)
{
    H5F_t	*f;
    H5G_entry_t	*ret_value=NULL;
    H5G_t	*group=NULL;
    H5T_t	*dt=NULL;
    H5D_t	*dset=NULL;
    H5A_t	*attr=NULL;

    FUNC_ENTER_NOAPI(H5G_loc, NULL);

    switch (H5I_get_type(loc_id)) {
        case H5I_FILE:
            if (NULL==(f=H5I_object (loc_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, NULL, "invalid file ID");
            if (NULL==(ret_value=H5G_entof(H5G_rootof(f))))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry for root group");

            /* Patch up root group's symbol table entry to reflect this file */
            /* (Since the root group info is only stored once for files which
             *  share an underlying low-level file)
             */
            /* (but only for non-mounted files) */
            if(!f->mtab.parent)
                ret_value->file = f;
            break;

        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of property list");

        case H5I_GROUP:
            if (NULL==(group=H5I_object (loc_id)))
                HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, NULL, "invalid group ID");
            if (NULL==(ret_value=H5G_entof(group)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of group");
            break;

        case H5I_DATATYPE:
            if (NULL==(dt=H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid type ID");
            if (NULL==(ret_value=H5T_entof(dt)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of datatype");
            break;

        case H5I_DATASPACE:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of dataspace");

        case H5I_DATASET:
            if (NULL==(dset=H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid data ID");
            if (NULL==(ret_value=H5D_entof(dset)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of dataset");
            break;

        case H5I_ATTR:
            if (NULL==(attr=H5I_object(loc_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid attribute ID");
            if (NULL==(ret_value=H5A_entof(attr)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of attribute");
            break;

        case H5I_REFERENCE:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of reference");

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object ID");
    }

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
H5G_link (H5G_entry_t *cur_loc, const char *cur_name, H5G_entry_t *link_loc,
	  const char *link_name, H5G_link_t type, unsigned namei_flags, hid_t dxpl_id)
{
    H5G_entry_t		cur_obj;	/*entry for the link tail	*/
    unsigned            cur_obj_init=0; /* Flag to indicate that the current object is initialized */
    H5G_entry_t		grp_ent;	/*ent for grp containing link hd*/
    H5O_stab_t		stab_mesg;	/*symbol table message		*/
    const char		*rest = NULL;	/*last component of new name	*/
    char		*norm_cur_name = NULL;	/* Pointer to normalized current name */
    char		*norm_link_name = NULL;	/* Pointer to normalized current name */
    size_t		nchars;		/*characters in component	*/
    size_t		offset;		/*offset to sym-link value	*/
    herr_t      ret_value = SUCCEED;       /* Return value */

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
            if(H5G_namei(cur_loc, norm_cur_name, NULL, NULL, &cur_obj, namei_flags, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found")
            cur_obj_init = 1;     /* Indicate that the cur_obj struct is initialized */
            if(H5G_insert(link_loc, norm_link_name, &cur_obj, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create new name/link for object")
            break;

        case H5G_LINK_SOFT:
            /*
             * Lookup the the new_name so we can get the group which will contain
             * the new entry.  The entry shouldn't exist yet.
             */
            if(H5G_namei(link_loc, norm_link_name, &rest, &grp_ent, NULL,
                            H5G_TARGET_NORMAL, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id) >= 0)
                HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "already exists")
            H5E_clear (); /*it's okay that we didn't find it*/
            rest = H5G_component(rest, &nchars);

            /*
             * There should be one component left.  Make sure it's null
             * terminated and that `rest' points to it.
             */
            HDassert(!rest[nchars]);

            /*
             * Add the link-value to the local heap for the symbol table which
             * will contain the link.
             */
            if(NULL == H5O_read(&grp_ent, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine local heap address")
            if((size_t)(-1) == (offset = H5HL_insert (grp_ent.file, dxpl_id,
                   stab_mesg.heap_addr, HDstrlen(norm_cur_name)+1, norm_cur_name)))
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to write link value to local heap")
            H5O_reset (H5O_STAB_ID, &stab_mesg);

            /*
             * Create a symbol table entry for the link.  The object header is
             * undefined and the cache contains the link-value offset.
             */
            H5G_ent_reset(&cur_obj);
            cur_obj.file = grp_ent.file;
            cur_obj.type = H5G_CACHED_SLINK;
            cur_obj.cache.slink.lval_offset = offset;
            cur_obj_init = 1;     /* Indicate that the cur_obj struct is initialized */

            /*
             * Insert the link head in the symbol table.  This shouldn't ever
             * fail because we've already checked that the link head doesn't
             * exist and the file is writable (because the local heap is
             * writable).  But if it does, the only side effect is that the local
             * heap has some extra garbage in it.
             *
             * Note: We don't increment the link count of the destination object
             */
            if(H5G_stab_insert(&grp_ent, rest, &cur_obj, FALSE, dxpl_id) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create new name/link for object")
            break;

        default:
            HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type")
    } /* end switch */

done:
    /* Free the group's ID to name buffer, if creating a soft link */
    if(type == H5G_LINK_SOFT)
        H5G_name_free(&grp_ent);

    /* Free the ID to name buffer */
    if(cur_obj_init)
        H5G_name_free(&cur_obj);

    /* Free the normalized path names */
    if(norm_cur_name)
        H5MM_xfree(norm_cur_name);
    if(norm_link_name)
        H5MM_xfree(norm_link_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_type
 *
 * Purpose:	Returns the type of object pointed to by `ent'.
 *
 * Return:	Success:	An object type defined in H5Gpublic.h
 *
 *		Failure:	H5G_UNKNOWN
 *
 * Programmer:	Robb Matzke
 *              Wednesday, November  4, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5G_get_type(H5G_entry_t *ent, hid_t dxpl_id)
{
    htri_t	isa;
    size_t	i;
    int         ret_value=H5G_UNKNOWN;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_get_type, H5G_UNKNOWN);

    for (i=H5G_ntypes_g; i>0; --i) {
	if ((isa=(H5G_type_g[i-1].isa)(ent, dxpl_id))<0) {
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type");
	} else if (isa) {
	    HGOTO_DONE(H5G_type_g[i-1].type);
	}
    }

    if (0==i)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
H5G_get_objinfo (H5G_entry_t *loc, const char *name, hbool_t follow_link,
		 H5G_stat_t *statbuf/*out*/, hid_t dxpl_id)
{
    H5G_entry_t		grp_ent, obj_ent;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_get_objinfo, FAIL)

    HDassert(loc);
    HDassert(name && *name);
    if (statbuf) HDmemset (statbuf, 0, sizeof *statbuf);

    /* Find the object's symbol table entry */
    if (H5G_namei(loc, name, NULL, &grp_ent/*out*/, &obj_ent/*out*/,
		   (unsigned)(follow_link?H5G_TARGET_NORMAL:H5G_TARGET_SLINK), NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id)<0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to stat object");

    /*
     * Initialize the stat buf.  Symbolic links aren't normal objects and
     * therefore don't have much of the normal info.  However, the link value
     * length is specific to symbolic links.
     */
    if (statbuf) {
        /* Common code to retrieve the file's fileno */
        if(H5F_get_fileno(obj_ent.file,statbuf->fileno)<0)
            HGOTO_ERROR (H5E_FILE, H5E_BADVALUE, FAIL, "unable to read fileno");

        /* Retrieve information specific to each type of entry */
	if (H5G_CACHED_SLINK==obj_ent.type) {
            H5O_stab_t	stab_mesg;      /* Symbol table message info */
            const char	*s;             /* Pointer to link value */
            const H5HL_t *heap;         /* Pointer to local heap for group */

	    /* Named object is a symbolic link */
	    if (NULL == H5O_read(&grp_ent, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
		HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read symbolic link value")

            /* Lock the local heap */
            if (NULL == (heap = H5HL_protect(grp_ent.file, dxpl_id, stab_mesg.heap_addr)))
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read protect link value")

            s = H5HL_offset_into(grp_ent.file, heap, obj_ent.cache.slink.lval_offset);

	    statbuf->linklen = HDstrlen(s) + 1; /*count the null terminator*/

            /* Release the local heap */
            if (H5HL_unprotect(grp_ent.file, dxpl_id, heap, stab_mesg.heap_addr) < 0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read unprotect link value")

            /* Set object type */
	    statbuf->type = H5G_LINK;
	} else {
	    /* Some other type of object */
	    statbuf->objno[0] = (unsigned long)(obj_ent.header);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
	    statbuf->objno[1] = (unsigned long)(obj_ent.header >>
						8*sizeof(long));
#else
	    statbuf->objno[1] = 0;
#endif
	    statbuf->nlink = H5O_link (&obj_ent, 0, dxpl_id);
	    if (NULL==H5O_read(&obj_ent, H5O_MTIME_ID, 0, &(statbuf->mtime), dxpl_id)) {
		H5E_clear();
                if (NULL==H5O_read(&obj_ent, H5O_MTIME_NEW_ID, 0, &(statbuf->mtime), dxpl_id)) {
                    H5E_clear();
                    statbuf->mtime = 0;
                }
	    }
            /* Get object type */
	    statbuf->type =
#ifndef H5_WANT_H5_V1_4_COMPAT
                (H5G_obj_t)
#endif /*H5_WANT_H5_V1_4_COMPAT*/
                H5G_get_type(&obj_ent, dxpl_id);
	    H5E_clear(); /*clear errors resulting from checking type*/

            /* Get object header information */
            if(H5O_get_info(&obj_ent, &(statbuf->ohdr), dxpl_id)<0)
                HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "unable to get object header information")
	}
    } /* end if */

done:
    /* Free the ID to name buffers */
    H5G_name_free(&grp_ent);
    H5G_name_free(&obj_ent);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_objinfo() */


/*-------------------------------------------------------------------------
 * Function:	H5G_get_num_objs
 *
 * Purpose:     Private function for H5Gget_num_objs.  Returns the number
 *              of objects in the group.  It iterates all B-tree leaves
 *              and sum up total number of group members.
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
static herr_t
H5G_get_num_objs(H5G_entry_t *loc, hsize_t *num_objs, hid_t dxpl_id)
{
    H5O_stab_t		stab_mesg;		/*info about B-tree	*/
    herr_t		ret_value;

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_num_objs);

    /* Sanity check */
    assert(loc);
    assert(num_objs);

    /* Reset the number of objects in the group */
    *num_objs = 0;

    /* Get the B-tree info */
    if (NULL==H5O_read (loc, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address");

    /* Iterate over the group members */
    if ((ret_value = H5B_iterate (loc->file, dxpl_id, H5B_SNODE,
              H5G_node_sumup, stab_mesg.btree_addr, num_objs))<0)
        HERROR (H5E_SYM, H5E_CANTINIT, "iteration operator failed");

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_get_objname_by_idx
 *
 * Purpose:     Private function for H5Gget_objname_by_idx.
 *              Returns the name of objects in the group by giving index.
 *
 * Return:	Success:        Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5G_get_objname_by_idx(H5G_entry_t *loc, hsize_t idx, char* name, size_t size, hid_t dxpl_id)
{
    H5O_stab_t		stab;		/*info about local heap	& B-tree */
    H5G_bt_it_ud2_t	udata;          /* Iteration information */
    ssize_t		ret_value;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_objname_by_idx);

    /* Sanity check */
    assert(loc);

    /* Get the B-tree & local heap info */
    if (NULL==H5O_read (loc, H5O_STAB_ID, 0, &stab, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address");

    /* Set iteration information */
    udata.idx = idx;
    udata.num_objs = 0;
    udata.heap_addr = stab.heap_addr;
    udata.name = NULL;

    /* Iterate over the group members */
    if ((ret_value = H5B_iterate (loc->file, dxpl_id, H5B_SNODE,
              H5G_node_name, stab.btree_addr, &udata))<0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "iteration operator failed");

    /* If we don't know the name now, we almost certainly went out of bounds */
    if(udata.name==NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "index out of bound");

    /* Get the length of the name */
    ret_value = (ssize_t)HDstrlen(udata.name);

    /* Copy the name into the user's buffer, if given */
    if(name) {
        HDstrncpy(name, udata.name, MIN((size_t)(ret_value+1),size));
        if((size_t)ret_value >= size)
            name[size-1]='\0';
    } /* end if */

done:
    /* Free the duplicated name */
    if(udata.name!=NULL)
        H5MM_xfree(udata.name);
    FUNC_LEAVE_NOAPI(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_get_objtype_by_idx
 *
 * Purpose:     Private function for H5Gget_objtype_by_idx.
 *              Returns the type of objects in the group by giving index.
 *
 * Return:	Success:        H5G_GROUP(1), H5G_DATASET(2), H5G_TYPE(3)
 *
 *		Failure:	UNKNOWN
 *
 * Programmer:	Raymond Lu
 *	        Nov 20, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5G_get_objtype_by_idx(H5G_entry_t *loc, hsize_t idx, hid_t dxpl_id)
{
    H5O_stab_t		stab_mesg;	/*info about local heap	& B-tree */
    H5G_bt_it_ud3_t	udata;          /* User data for B-tree callback */
    int	    ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_objtype_by_idx);

    /* Sanity check */
    assert(loc);

    /* Get the B-tree & local heap info */
    if (NULL==H5O_read (loc, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address");

    /* Set iteration information */
    udata.idx = idx;
    udata.num_objs = 0;
    udata.type = H5G_UNKNOWN;

    /* Iterate over the group members */
    if (H5B_iterate (loc->file, dxpl_id, H5B_SNODE,
              H5G_node_type, stab_mesg.btree_addr, &udata)<0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "iteration operator failed");

    /* If we don't know the type now, we almost certainly went out of bounds */
    if(udata.type==H5G_UNKNOWN)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "index out of bound");

    ret_value = udata.type;

done:
    FUNC_LEAVE_NOAPI(ret_value);
}


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
H5G_linkval(H5G_entry_t *loc, const char *name, size_t size, char *buf/*out*/, hid_t dxpl_id)
{
    const char		*s = NULL;
    H5G_entry_t		grp_ent, obj_ent;
    H5O_stab_t		stab_mesg;
    const H5HL_t        *heap;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_linkval)

    /*
     * Get the symbol table entry for the link head and the symbol table
     * entry for the group in which the link head appears.
     */
    if (H5G_namei(loc, name, NULL, &grp_ent/*out*/, &obj_ent/*out*/,
		   H5G_TARGET_SLINK, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id)<0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "symbolic link was not found");
    if (H5G_CACHED_SLINK!=obj_ent.type)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "object is not a symbolic link");

    /*
     * Get the address of the local heap for the link value and a pointer
     * into that local heap.
     */
    if (NULL==H5O_read (&grp_ent, H5O_STAB_ID, 0, &stab_mesg, dxpl_id))
	HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine local heap address");

    if (NULL == (heap = H5HL_protect(grp_ent.file, dxpl_id, stab_mesg.heap_addr)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read protect link value")

    s = H5HL_offset_into(grp_ent.file, heap, obj_ent.cache.slink.lval_offset);

    /* Copy to output buffer */
    if (size>0 && buf)
	HDstrncpy (buf, s, size);

    if (H5HL_unprotect(grp_ent.file, dxpl_id, heap, stab_mesg.heap_addr) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read unprotect link value")

done:
    /* Free the ID to name buffers */
    H5G_name_free(&grp_ent);
    H5G_name_free(&obj_ent);

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
H5G_set_comment(H5G_entry_t *loc, const char *name, const char *buf, hid_t dxpl_id)
{
    H5G_entry_t	obj_ent;
    H5O_name_t	comment;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_set_comment)

    /* Get the symbol table entry for the object */
    if (H5G_find(loc, name, &obj_ent/*out*/, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");

    /* Remove the previous comment message if any */
    if (H5O_remove(&obj_ent, H5O_NAME_ID, 0, TRUE, dxpl_id)<0)
        H5E_clear();

    /* Add the new message */
    if(buf && *buf) {
	comment.s = H5MM_xstrdup(buf);
	if(H5O_modify(&obj_ent, H5O_NAME_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, &comment, dxpl_id)<0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to set comment object header message")
	H5O_reset(H5O_NAME_ID, &comment);
    } /* end if */

done:
    /* Free the ID to name buffer */
    H5G_name_free(&obj_ent);

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
H5G_get_comment(H5G_entry_t *loc, const char *name, size_t bufsize, char *buf, hid_t dxpl_id)
{
    H5O_name_t	comment;
    H5G_entry_t	obj_ent;
    int	ret_value;                      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_comment)

    /* Get the symbol table entry for the object */
    if(H5G_find(loc, name, &obj_ent/*out*/, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the message */
    comment.s = NULL;
    if(NULL == H5O_read(&obj_ent, H5O_NAME_ID, 0, &comment, dxpl_id)) {
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
    /* Free the ID to name buffer */
    H5G_name_free(&obj_ent);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_comment() */


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
H5G_unlink(H5G_entry_t *loc, const char *name, hid_t dxpl_id)
{
    H5G_entry_t		grp_ent, obj_ent;
    const char		*base=NULL;
    char		*norm_name = NULL;	/* Pointer to normalized name */
    int                 obj_type;       /* Object type */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_unlink)

    /* Sanity check */
    HDassert(loc);
    HDassert(name && *name);

    /* Get normalized copy of the name */
    if((norm_name = H5G_normalize(name)) == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, FAIL, "can't normalize name")

    /* Reset the group entries to known values in a portable way */
    H5G_ent_reset(&grp_ent);
    H5G_ent_reset(&obj_ent);

    /* Get the entry for the group that contains the object to be unlinked */
    if (H5G_namei(loc, norm_name, NULL, &grp_ent, &obj_ent,
		  H5G_TARGET_SLINK|H5G_TARGET_MOUNT, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
    if (!H5F_addr_defined(grp_ent.header))
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no containing group specified");
    if (NULL==(base=H5G_basename(norm_name, NULL)) || '/'==*base)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "problems obtaining object base name")

    /* Get object type before unlink */
    if((obj_type = H5G_get_type(&obj_ent, dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't determine object type")

    /* Remove the name from the symbol table */
    if (H5G_stab_remove(&grp_ent, base, dxpl_id)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to unlink name from symbol table")

    /* Search the open IDs and replace names for unlinked object */
    if (H5G_name_replace(obj_type, &obj_ent, NULL, NULL, H5G_NAME_UNLINK )<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to replace name")

done:
    /* Free the ID to name buffers */
    H5G_name_free(&grp_ent);
    H5G_name_free(&obj_ent);

    /* Free the normalized path name */
    if(norm_name)
        H5MM_xfree(norm_name);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_unlink() */


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
H5G_move(H5G_entry_t *src_loc, const char *src_name, H5G_entry_t *dst_loc,
		const char *dst_name, hid_t dxpl_id)
{
    H5G_stat_t		sb;
    char		*linkval = NULL;
    size_t		lv_size = 32;
    H5G_entry_t         obj_ent;                /* Object entry for object being moved */
    H5RS_str_t         *dst_name_r = NULL;      /* Ref-counted version of dest name */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_move)

    /* Sanity check */
    HDassert(src_loc);
    HDassert(dst_loc);
    HDassert(src_name && *src_name);
    HDassert(dst_name && *dst_name);

    if(H5G_get_objinfo(src_loc, src_name, FALSE, &sb, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
    if(H5G_LINK == sb.type) {
	/*
	 * When renaming a symbolic link we rename the link but don't change
	 * the value of the link.
	 */
	do {
	    if(NULL==(linkval=H5MM_realloc(linkval, 2*lv_size)))
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate space for symbolic link value")
	    linkval[lv_size-1] = '\0';
	    if(H5G_linkval(src_loc, src_name, lv_size, linkval, dxpl_id)<0)
		HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read symbolic link value")
	} while (linkval[lv_size-1]);
	if (H5G_link(src_loc, linkval, dst_loc, dst_name, H5G_LINK_SOFT,
		     H5G_TARGET_NORMAL, dxpl_id)<0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to rename symbolic link")
	H5MM_xfree(linkval);

    } else {
	/*
	 * Rename the object.
	 */
	if(H5G_link(src_loc, src_name, dst_loc, dst_name, H5G_LINK_HARD, H5G_TARGET_MOUNT, dxpl_id) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to register new name for object")
    } /* end else */

    /* Search the open ID list and replace names for the move operation
     * This has to be done here because H5G_link and H5G_unlink have
     * internal object entries, and do not modify the entries list
    */
    if(H5G_namei(src_loc, src_name, NULL, NULL, &obj_ent, H5G_TARGET_NORMAL|H5G_TARGET_SLINK, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id))
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link")
    dst_name_r = H5RS_wrap(dst_name);
    HDassert(dst_name_r);
    if(H5G_name_replace(sb.type, &obj_ent, dst_name_r, dst_loc, H5G_NAME_MOVE) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ")
    H5G_name_free(&obj_ent);

    /* Remove the old name */
    if(H5G_unlink(src_loc, src_name, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to unlink old object name")

done:
    /* Cleanup */
    if(dst_name_r)
        H5RS_decr(dst_name_r);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_move() */


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
H5G_insertion_file(H5G_entry_t *loc, const char *name, hid_t dxpl_id)
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
    if(H5F_has_mount(loc->file) || H5F_is_mount(loc->file)) {
        const char	*rest;
        H5G_entry_t	grp_ent;
        size_t	size;

        /*
         * Look up the name to get the containing group and to make sure the name
         * doesn't already exist.
         */
        if (H5G_namei(loc, name, &rest, &grp_ent, NULL, H5G_TARGET_NORMAL, NULL, H5G_NAMEI_TRAVERSE, NULL, dxpl_id)>=0) {
            H5G_name_free(&grp_ent);
            HGOTO_ERROR(H5E_SYM, H5E_EXISTS, NULL, "name already exists");
        } /* end if */
        H5E_clear();

        /* Make sure only the last component wasn't resolved */
        rest = H5G_component(rest, &size);
        assert(*rest && size>0);
        rest = H5G_component(rest+size, NULL);
        if (*rest) {
            H5G_name_free(&grp_ent);
            HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "insertion point not found");
        } /* end if */

        /* Set return value */
        ret_value=grp_ent.file;

        /* Free the ID to name buffer */
        H5G_name_free(&grp_ent);
    } /* end if */
    else
        /* Use the location's file */
        ret_value = loc->file;

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
    H5G_entry_t *ent;           /* Group object's entry */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5G_free_grp_name, FAIL)

    /* Check args */
    HDassert(grp && grp->shared);
    HDassert(grp->shared->fo_count > 0);

    /* Get the entry for the group */
    if (NULL==( ent = H5G_entof(grp)))
        HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "cannot get entry");

    /* Free the entry */
    H5G_name_free(ent);

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

