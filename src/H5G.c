/*-------------------------------------------------------------------------
 * Copyright (C) 1997-2002 National Center for Supercomputing Applications
 *			   All rights reserved.
 *
 *-------------------------------------------------------------------------
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
 * Modifications:
 *
 *	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *	Robb Matzke, 30 Aug 1997
 *	Added `Errors:' field to function prologues.
 *
 *	Pedro Vicente, 22 Aug 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */

#define H5G_PACKAGE     /*suppress error message about including H5Gpkg.h */
#define H5F_PACKAGE     /*suppress error about including H5Fpkg	  */


/* Packages needed by this file... */
#include "H5private.h"
#include "H5Aprivate.h"
#include "H5Bprivate.h"
#include "H5Dprivate.h"
#include "H5Eprivate.h"
#include "H5Fpkg.h"         /*file access                             */
#include "H5FLprivate.h"	/*Free Lists	  */
#include "H5Gpkg.h"
#include "H5HLprivate.h"
#include "H5Iprivate.h"
#include "H5MMprivate.h"
#include "H5Oprivate.h"

#define H5G_INIT_HEAP		8192
#define H5G_RESERVED_ATOMS	0
#define PABLO_MASK		H5G_mask

/* Interface initialization */
static int interface_initialize_g = 0;
#define INTERFACE_INIT	H5G_init_interface
static herr_t H5G_init_interface(void);
static H5G_typeinfo_t *H5G_type_g = NULL;	/*object typing info	*/
static size_t H5G_ntypes_g = 0;			/*entries in type table	*/
static size_t H5G_atypes_g = 0;			/*entries allocated	*/
static char *H5G_comp_g = NULL;                 /*component buffer      */
static size_t H5G_comp_alloc_g = 0;             /*sizeof component buffer */

/* Declare a free list to manage the H5G_t struct */
H5FL_DEFINE(H5G_t);

/* Private prototypes */
static herr_t H5G_replace_ent(void *obj_ptr, hid_t obj_id, void *key);



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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gcreate(hid_t loc_id, const char *name, size_t size_hint)
{
    H5G_entry_t		   *loc = NULL;
    H5G_t		   *grp = NULL;
    hid_t		    ret_value;

    FUNC_ENTER_API(H5Gcreate, FAIL);
    H5TRACE3("i","isz",loc_id,name,size_hint);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name given");
    
    /* Create the group */
    if (NULL == (grp = H5G_create(loc, name, size_hint)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to create group");
    if ((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group");

done:
    if(ret_value<0) {
        if(grp!=NULL)
            H5G_close(grp);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Gopen(hid_t loc_id, const char *name)
{
    hid_t	ret_value = FAIL;
    H5G_t	*grp = NULL;
    H5G_entry_t	*loc = NULL;

    FUNC_ENTER_API(H5Gopen, FAIL);
    H5TRACE2("i","is",loc_id,name);

    /* Check args */
    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");
    
    /* Open the group */
    if (NULL == (grp = H5G_open(loc, name)))
	HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open group");
    /* Register an atom for the group */
    if ((ret_value = H5I_register(H5I_GROUP, grp)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register group");

done:
    if(ret_value<0) {
        if(grp!=NULL)
            H5G_close(grp);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
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
    FUNC_LEAVE(ret_value);
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Giterate(hid_t loc_id, const char *name, int *idx,
	    H5G_iterate_t op, void *op_data)
{
    int			_idx = 0;
    H5G_bt_ud2_t	udata;
    H5G_entry_t		*loc = NULL;
    herr_t		ret_value;
    
    FUNC_ENTER_API(H5Giterate, FAIL);
    H5TRACE5("e","is*Isxx",loc_id,name,idx,op,op_data);

    /* Check args */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified");
    if (!idx)
        idx = &_idx;
    if (!op)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no operator specified");

    /*
     * Open the group on which to operate.  We also create a group ID which
     * we can pass to the application-defined operator.
     */
    if (NULL==(udata.group = H5G_open (loc, name)))
	HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to open group");
    if ((udata.group_id=H5I_register (H5I_GROUP, udata.group))<0) {
	H5G_close(udata.group);
	HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to register group");
    }
    
    /* Build udata to pass through H5B_iterate() to H5G_node_iterate() */
    udata.skip = *idx;
    udata.op = op;
    udata.op_data = op_data;

    /* Set the number of entries looked at to zero */
    udata.final_ent = 0;

    /* Iterate over the group members */
    if ((ret_value = H5B_iterate (H5G_fileof(udata.group), H5B_SNODE,
              H5G_node_iterate, udata.group->ent.cache.stab.btree_addr, &udata))<0)
        HERROR (H5E_SYM, H5E_CANTINIT, "iteration operator failed");

    /* Set the index we stopped at */
    *idx=udata.final_ent;

    H5I_dec_ref (udata.group_id); /*also closes udata.group*/

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 *		Raymond Lu
 *		Thursday, April 18, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
	 const char *dst_name)
{
    H5G_entry_t		*src_loc=NULL;
    H5G_entry_t		*dst_loc=NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gmove2, FAIL);
    H5TRACE4("e","isis",src_loc_id,src_name,dst_loc_id,dst_name);

    if (src_loc_id != H5G_SAME_LOC && NULL==(src_loc=H5G_loc(src_loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (dst_loc_id != H5G_SAME_LOC && NULL==(dst_loc=H5G_loc(dst_loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!src_name || !*src_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified");
    if (!dst_name || !*dst_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified");

    if(src_loc_id == H5G_SAME_LOC && dst_loc_id == H5G_SAME_LOC) {
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC");
    } else if(src_loc_id == H5G_SAME_LOC) {
	src_loc = dst_loc;
    }
    else if(dst_loc_id == H5G_SAME_LOC) {
	dst_loc = src_loc;
    }
    else if(src_loc->file != dst_loc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.");
	
    if (H5G_move(src_loc, src_name, dst_loc, dst_name)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to change object name");

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink2(hid_t cur_loc_id, const char *cur_name, H5G_link_t type, 
	 hid_t new_loc_id, const char *new_name)
{
    H5G_entry_t	*cur_loc = NULL;
    H5G_entry_t *new_loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Glink2, FAIL);
    H5TRACE5("e","isGlis",cur_loc_id,cur_name,type,new_loc_id,new_name);

    /* Check arguments */
    if (cur_loc_id != H5G_SAME_LOC && NULL==(cur_loc=H5G_loc(cur_loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (new_loc_id != H5G_SAME_LOC && NULL==(new_loc=H5G_loc(new_loc_id)))
        HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (type!=H5G_LINK_HARD && type!=H5G_LINK_SOFT)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "unrecognized link type");
    if (!cur_name || !*cur_name)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified");
    if (!new_name || !*new_name)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified");

    if(cur_loc_id == H5G_SAME_LOC && new_loc_id == H5G_SAME_LOC) {
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5G_SAME_LOC");
    }
    else if(cur_loc_id == H5G_SAME_LOC) {
        cur_loc = new_loc;
    }
    else if(new_loc_id == H5G_SAME_LOC) {
   	new_loc = cur_loc;
    }
    else if(cur_loc->file != new_loc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.");

    if (H5G_link(cur_loc, cur_name, new_loc, new_name, type, H5G_TARGET_NORMAL) <0)
	HGOTO_ERROR (H5E_SYM, H5E_LINK, FAIL, "unable to create link");

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
	*
	* Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gunlink(hid_t loc_id, const char *name)
{
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
				H5G_stat_t		statbuf; 
    
    FUNC_ENTER_API(H5Gunlink, FAIL);
    H5TRACE2("e","is",loc_id,name);

    /* Check arguments */
    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name");

				/* Get object type before unlink */
    if (H5G_get_objinfo(loc, name, FALSE, &statbuf)<0) {
	HRETURN_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
    }

    /* Unlink */
    if (H5G_unlink(loc, name)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to unlink object");

  		/* Search the symbol table entry list and replace names through group IDs */
	   if (H5G_replace_name( statbuf.type, loc, name, NULL, OP_UNLINK )<0)
	HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ");

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_objinfo(hid_t loc_id, const char *name, hbool_t follow_link,
	       H5G_stat_t *statbuf/*out*/)
{
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_API(H5Gget_objinfo, FAIL);
    H5TRACE4("e","isbx",loc_id,name,follow_link,statbuf);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified");

    /* Get info */
    if (H5G_get_objinfo (loc, name, follow_link, statbuf)<0)
	HGOTO_ERROR (H5E_ARGS, H5E_CANTINIT, FAIL, "cannot stat object");

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_linkval(hid_t loc_id, const char *name, size_t size, char *buf/*out*/)
{
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_API(H5Gget_linkval, FAIL);
    H5TRACE4("e","iszx",loc_id,name,size,buf);

    /* Check arguments */
    if (NULL==(loc=H5G_loc (loc_id)))
	HGOTO_ERROR (H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified");

    /* Get the link value */
    if (H5G_linkval (loc, name, size, buf)<0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to get link value");

done:
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gset_comment(hid_t loc_id, const char *name, const char *comment)
{
    H5G_entry_t	*loc = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_API(H5Gset_comment, FAIL);
    H5TRACE3("e","iss",loc_id,name,comment);

    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified");

    if (H5G_set_comment(loc, name, comment)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to set comment value");

done:
    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Gget_comment(hid_t loc_id, const char *name, size_t bufsize, char *buf)
{
    H5G_entry_t	*loc = NULL;
    int	ret_value;
    
    FUNC_ENTER_API(H5Gget_comment, FAIL);
    H5TRACE4("Is","iszs",loc_id,name,bufsize,buf);

    if (NULL==(loc=H5G_loc(loc_id)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location");
    if (!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified");
    if (bufsize>0 && !buf)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no buffer specified");

    if ((ret_value=H5G_get_comment(loc, name, bufsize, buf))<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to get comment value");

done:
    FUNC_LEAVE(ret_value);
}

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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_init_interface(void)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOINIT(H5G_init_interface);

    /* Initialize the atom group for the group IDs */
    if (H5I_init_group(H5I_GROUP, H5I_GROUPID_HASHSIZE, H5G_RESERVED_ATOMS,
		       (H5I_free_t)H5G_close) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to initialize interface");

    /*
     * Initialize the type info table.  Begin with the most general types and
     * end with the most specific. For instance, any object that has a data
     * type message is a data type but only some of them are datasets.
     */
    H5G_register_type(H5G_TYPE,    H5T_isa,  "data type");
    H5G_register_type(H5G_GROUP,   H5G_isa,  "group");
    H5G_register_type(H5G_DATASET, H5D_isa,  "dataset");

done:
    FUNC_LEAVE(ret_value);
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
 * Modifications:
 *              Robb Matzke, 2002-03-28
 *              Free the global component buffer.
 *-------------------------------------------------------------------------
 */
int
H5G_term_interface(void)
{
    size_t	i;
    int	n=0;

    FUNC_ENTER_NOINIT(H5G_term_interface);
    
    if (interface_initialize_g) {
	if ((n=H5I_nmembers(H5I_GROUP))) {
	    H5I_clear_group(H5I_GROUP, FALSE);
	} else {
	    /* Empty the object type table */
	    for (i=0; i<H5G_ntypes_g; i++)
		H5MM_xfree(H5G_type_g[i].desc);
	    H5G_ntypes_g = H5G_atypes_g = 0;
	    H5G_type_g = H5MM_xfree(H5G_type_g);
    
	    /* Destroy the group object id group */
	    H5I_destroy_group(H5I_GROUP);

            /* Free the global component buffer */
            H5G_comp_g = H5MM_xfree(H5G_comp_g);
            H5G_comp_alloc_g = 0;

	    /* Mark closed */
	    interface_initialize_g = 0;
	    n = 1; /*H5I*/
	}
    }
    
    FUNC_LEAVE(n);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_register_type
 *
 * Purpose:	Register a new object type so H5G_get_type() can detect it.
 *		One should always register a general type before a more
 *		specific type.  For instance, any object that has a data type
 *		message is a data type, but only some of those objects are
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
herr_t
H5G_register_type(int type, htri_t(*isa)(H5G_entry_t*), const char *_desc)
{
    char	*desc = NULL;
    size_t	i;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_register_type, FAIL);

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
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_component
 *
 * Purpose:	Returns the pointer to the first component of the
 *		specified name by skipping leading slashes.  Returns
 *		the size in characters of the component through SIZE_P not
 *		counting leading slashes or the null terminator.
 *
 * Errors:
 *
 * Return:	Success:	Ptr into NAME.
 *
 *		Failure:	Ptr to the null terminator of NAME.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static const char *
H5G_component(const char *name, size_t *size_p)
{
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5G_component);

    assert(name);

    while ('/' == *name)
        name++;
    if (size_p)
        *size_p = HDstrcspn(name, "/");

    FUNC_LEAVE(name);
}


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
    
    FUNC_ENTER_NOINIT(H5G_basename);

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

    FUNC_LEAVE(name+i);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_namei
 *
 * Purpose:	Translates a name to a symbol table entry.
 *
 *		If the specified name can be fully resolved, then this
 *		function returns the symbol table entry for the named object
 *		through the OBJ_ENT argument. The symbol table entry for the
 *		group containing the named object is returned through the
 *		GRP_ENT argument if it is non-null.  However, if the name
 *		refers to the root object then the GRP_ENT will be
 *		initialized with an undefined object header address.  The
 *		REST argument, if present, will point to the null terminator
 *		of NAME.
 *
 *		If the specified name cannot be fully resolved, then OBJ_ENT
 *		is initialized with the undefined object header address. The
 *		REST argument will point into the NAME argument to the start
 *		of the component that could not be located.  The GRP_ENT will
 *		contain the entry for the symbol table that was being
 *		searched at the time of the failure and will have an
 *		undefined object header address if the search failed at the
 *		root object. For instance, if NAME is `/foo/bar/baz' and the
 *		root directory exists and contains an entry for `foo', and
 *		foo is a group that contains an entry for bar, but bar is not
 *		a group, then the results will be that REST points to `baz',
 *		OBJ_ENT has an undefined object header address, and GRP_ENT
 *		is the symbol table entry for `bar' in `/foo'.
 *
 *		Every file has a root group whose name is `/'.  Components of
 *		a name are separated from one another by one or more slashes
 *		(/).  Slashes at the end of a name are ignored.  If the name
 *		begins with a slash then the search begins at the root group
 *		of the file containing LOC_ENT. Otherwise it begins at
 *		LOC_ENT.  The component `.' is a no-op, but `..' is not
 *		understood by this function (unless it appears as an entry in
 *		the symbol table).
 *
 *		Symbolic links are followed automatically, but if TARGET
 *		includes the H5G_TARGET_SLINK bit and the last component of
 *		the name is a symbolic link then that link is not followed.
 *		The *NLINKS value is decremented each time a link is followed
 *		and link traversal fails if the value would become negative.
 *		If NLINKS is the null pointer then a default value is used.
 *
 *		Mounted files are handled by calling H5F_mountpoint() after
 *		each step of the translation.  If the input argument to that
 *		function is a mount point then the argument shall be replaced
 *		with information about the root group of the mounted file.
 *		But if TARGET includes the H5G_TARGET_MOUNT bit and the last
 *		component of the name is a mount point then H5F_mountpoint()
 *		is not called and information about the mount point itself is
 *		returned.
 *		
 * Errors:
 *
 * Return:	Success:	Non-negative if name can be fully resolved.
 *				See above for values of REST, GRP_ENT, and
 *				OBJ_ENT.  NLINKS has been decremented for
 *				each symbolic link that was followed.
 *
 *		Failure:	Negative if the name could not be fully
 *				resolved. See above for values of REST,
 *				GRP_ENT, and OBJ_ENT.
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Aug 11 1997
 *
 * Modifications:
 *              Robb Matzke, 2002-03-28
 *              The component name buffer on the stack has been replaced by
 *              a dynamically allocated buffer on the heap in order to
 *              remove limitations on the length of a name component.
 *              There are two reasons that the buffer pointer is global:
 *                (1) We want to be able to reuse the buffer without
 *                    allocating and freeing it each time this function is
 *                    called.
 *                (2) We need to be able to free it from H5G_term_interface()
 *                    when the library terminates.
 *
	*	 Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	 Modified to deep copies of symbol table entries
 *	 Added `id to name' support.
	*
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_namei(H5G_entry_t *loc_ent, const char *name, const char **rest/*out*/,
	  H5G_entry_t *grp_ent/*out*/, H5G_entry_t *obj_ent/*out*/,
	  unsigned target, int *nlinks)
{
    H5G_entry_t		  _grp_ent;  /*entry for current group	*/
    H5G_entry_t		  _obj_ent;	 /*entry found			*/
    size_t		       nchars;		   /*component name length		*/
    int			         _nlinks = H5G_NLINKS;
    const char		   *s = NULL;
    herr_t         ret_value=SUCCEED;  
				

				H5G_t          *tmp_grp;
				const char		   *orig_name = name;
				unsigned int   null_obj = obj_ent == NULL ? 1 : 0;
				unsigned int   null_grp = grp_ent == NULL ? 1 : 0;
				unsigned int   found_once = 0;
    
    FUNC_ENTER_NOINIT(H5G_namei);

    if (rest) *rest = name;
    if (!grp_ent) grp_ent = &_grp_ent;
    if (!obj_ent) obj_ent = &_obj_ent;
    if (!nlinks) nlinks = &_nlinks;
    
    /*
     * Where does the searching start?  For absolute names it starts at the
     * root of the file; for relative names it starts at CWG.
     */
    if (!name || !*name) {
     HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "no name given");
    } else if (!loc_ent) {
     HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "no current working group");
    } else if ('/' == *name) {
     
     tmp_grp=H5G_rootof(loc_ent->file);
     
     /* Deep copy of the symbol table entry */
     if (H5G_ent_copy( &(tmp_grp->ent), obj_ent )<0)
      HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, FAIL, "unable to copy entry");
     
     
    } else {
     /* Deep copy of the symbol table entry */
     if (H5G_ent_copy( loc_ent, obj_ent )<0)
      HGOTO_ERROR(H5E_DATATYPE, H5E_CANTOPENOBJ, FAIL, "unable to copy entry");
     
    }
    
    
    HDmemset(grp_ent, 0, sizeof(H5G_entry_t));
    grp_ent->header = HADDR_UNDEF;

    /* traverse the name */
 while ((name = H5G_component(name, &nchars)) && *name) {
	if (rest) *rest = name;

	/*
	 * Copy the component name into a null-terminated buffer so
	 * we can pass it down to the other symbol table functions.
	 */
        if (nchars+1 > H5G_comp_alloc_g) {
            H5G_comp_alloc_g = MAX3(1024, 2*H5G_comp_alloc_g, nchars+1);
            H5G_comp_g = H5MM_realloc(H5G_comp_g, H5G_comp_alloc_g);
            if (!H5G_comp_g) {
                H5G_comp_alloc_g = 0;
                HGOTO_ERROR(H5E_SYM, H5E_NOSPACE, FAIL, "unable to allocate component buffer");
            }
        }
	HDmemcpy(H5G_comp_g, name, nchars);
	H5G_comp_g[nchars] = '\0';

	/*
	 * The special name `.' is a no-op.
	 */
	if ('.' == H5G_comp_g[0] && !H5G_comp_g[1]) {
	    name += nchars;
	    continue;
	}

	/*
	 * Advance to the next component of the name.
	 */
	if(found_once) {
		if(grp_ent->name)
			H5MM_xfree(grp_ent->name);
		if(grp_ent->old_name)
			H5MM_xfree(grp_ent->old_name);
	}
	*grp_ent = *obj_ent;
	HDmemset(obj_ent, 0, sizeof(H5G_entry_t));
	obj_ent->header = HADDR_UNDEF;
	/* Set flag if at least one component was found */
	found_once =1;

	if (H5G_stab_find(grp_ent, H5G_comp_g, /*&tmp_obj_ent*/ obj_ent/*out*/ )<0) {
	    /*
	     * Component was not found in the current symbol table, possibly
	     * because GRP_ENT isn't a symbol table.
	     */
	    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
	}


	/*
	 * If we found a symbolic link then we should follow it.  But if this
	 * is the last component of the name and the H5G_TARGET_SLINK bit of
	 * TARGET is set then we don't follow it.
	 */
	if (H5G_CACHED_SLINK==obj_ent->type &&
	    (0==(target & H5G_TARGET_SLINK) ||
	     ((s=H5G_component(name+nchars, NULL)) && *s))) {
	    if ((*nlinks)-- <= 0)
		HGOTO_ERROR (H5E_SYM, H5E_SLINK, FAIL, "too many symbolic links");
	    if (H5G_traverse_slink (grp_ent, obj_ent, nlinks)<0)
		HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "symbolic link traversal failed");
	}

	/*
	 * Resolve mount points to the mounted group.  Do not do this step if
	 * the H5G_TARGET_MOUNT bit of TARGET is set and this is the last
	 * component of the name.
	 */
	if (0==(target & H5G_TARGET_MOUNT) ||
	    ((s=H5G_component(name+nchars, NULL)) && *s)) {
	    H5F_mountpoint(obj_ent/*in,out*/);
	}
	
	/* next component */
	name += nchars;

    
	}
    
	if (rest)
  *rest = name; /*final null */

done:
	if(null_obj) {
		if(obj_ent->name)
			H5MM_xfree(obj_ent->name);
			if(obj_ent->old_name)
			H5MM_xfree(obj_ent->old_name);
	}
	if(null_grp && found_once) {
		if(grp_ent->name)
			H5MM_xfree(grp_ent->name);
			if(grp_ent->old_name)
			H5MM_xfree(grp_ent->old_name);
	}
   FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_traverse_slink
 *
 * Purpose:	Traverses symbolic link.  The link head appears in the group
 *		whose entry is GRP_ENT and the link head entry is OBJ_ENT.
 *
 * Return:	Success:	Non-negative, OBJ_ENT will contain information
 *				about the object to which the link points and
 *				GRP_ENT will contain the information about
 *				the group in which the link tail appears.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Friday, April 10, 1998
 *
 * Modifications:
	*
	* Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_traverse_slink (H5G_entry_t *grp_ent/*in,out*/,
		    H5G_entry_t *obj_ent/*in,out*/,
		    int *nlinks/*in,out*/)
{
    H5O_stab_t		stab_mesg;		/*info about local heap	*/
    const char		*clv = NULL;		/*cached link value	*/
    char		      *linkval = NULL;	/*the copied link value	*/
    herr_t      ret_value=SUCCEED;       /* Return value */
				H5G_entry_t tmp;

					/*Store old name */
				char* old_name = H5MM_strdup(obj_ent->name);
    
    FUNC_ENTER_NOAPI(H5G_traverse_slink, FAIL);

    /* Get the link value */
    if (NULL==H5O_read (grp_ent, H5O_STAB, 0, &stab_mesg))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to determine local heap address");
    if (NULL==(clv=H5HL_peek (grp_ent->file, stab_mesg.heap_addr,
			      obj_ent->cache.slink.lval_offset)))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to read symbolic link value");
    linkval = H5MM_xstrdup (clv);

		
				/* Free the ID to name buffer */
				H5G_free_ent_name(obj_ent);
				H5G_free_ent_name(grp_ent);

				H5G_ent_copy(grp_ent,&tmp);

    /* Traverse the link */
    if (H5G_namei (&tmp, linkval, NULL, grp_ent, obj_ent, H5G_TARGET_NORMAL, nlinks))
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to follow symbolic link");
				
		
				/* Free the ID to name buffer */
				H5G_free_ent_name(obj_ent);

				obj_ent->name = old_name;
	
done:

				if ( ret_value == FAIL )
					H5MM_xfree (old_name);
				H5G_free_ent_name(&tmp);
    H5MM_xfree (linkval);
    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
	*	Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_mkroot (H5F_t *f, H5G_entry_t *ent)
{
    H5G_entry_t	new_root;		/*new root object		*/
    H5O_stab_t	stab;			/*symbol table message		*/
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_mkroot, FAIL);

    /* check args */
    assert(f);
    if (f->shared->root_grp)
        HGOTO_DONE(SUCCEED);

    /*
     * If there is no root object then create one. The root group always has
     * a hard link count of one since it's pointed to by the boot block.
     */
    if (!ent) {
	ent = &new_root;
	if (H5G_stab_create (f, 256, ent/*out*/)<0)
	    HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to create root group");
	if (1 != H5O_link (ent, 1))
	    HGOTO_ERROR (H5E_SYM, H5E_LINK, FAIL, "internal error (wrong link count)");
    } else {
	/*
	 * Open the root object as a group.
	 */
	if (H5O_open (ent)<0)
	    HGOTO_ERROR (H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to open root group");
	if (NULL==H5O_read (ent, H5O_STAB, 0, &stab)) {
	    H5O_close(ent);
	    HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "root object is not a group");
	}
	H5O_reset (H5O_STAB, &stab);
    }

			/*create the "/" name */
			ent->name = HDstrdup( "/" );
			ent->old_name = NULL;


    /*
     * Create the group pointer.  Also decrement the open object count so we
     * don't count the root group as an open object.  The root group will
     * never be closed.
     */
    if (NULL==(f->shared->root_grp = H5FL_ALLOC (H5G_t,1)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    f->shared->root_grp->ent = *ent;
    f->shared->root_grp->nref = 1;
    assert (1==f->nopen_objs);
    f->nopen_objs = 0;

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_create
 *
 * Purpose:	Creates a new empty group with the specified name. The name
 *		is either an absolute name or is relative to LOC.
 *
 * Errors:
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
 * Modifications:
 *
	*	  Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	  Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_create(H5G_entry_t *loc, const char *name, size_t size_hint)
{
    const char	*rest = NULL;		/*the base name			*/
    H5G_entry_t	grp_ent;		/*group containing new group	*/
    char	_comp[1024];		/*name component		*/
    size_t	nchars;			/*number of characters in compon*/
    H5G_t	*grp = NULL;		/*new group			*/
    H5G_t	*ret_value;		/* Return value */

    FUNC_ENTER_NOAPI(H5G_create, NULL);

    /* check args */
    assert(loc);
    assert(name && *name);

				HDmemset(&grp_ent, 0, sizeof(H5G_entry_t));

    /* lookup name */
    if (0 == H5G_namei(loc, name, &rest, &grp_ent, NULL, H5G_TARGET_NORMAL, NULL))
	HGOTO_ERROR(H5E_SYM, H5E_EXISTS, NULL, "already exists");
    H5E_clear(); /*it's OK that we didn't find it */
    assert(H5F_addr_defined(grp_ent.header));

    /* should be one null-terminated component left */
    rest = H5G_component(rest, &nchars);
    assert(rest && *rest);
    if (rest[nchars]) {
	const char *t = H5G_component(rest+nchars, NULL);
	if (t && *t) {
	    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "missing component");
	} else if (nchars+1 > sizeof _comp) {
	    HGOTO_ERROR(H5E_SYM, H5E_COMPLEN, NULL, "component is too long");
	} else {
	    /* null terminate */
	    HDmemcpy(_comp, rest, nchars);
	    _comp[nchars] = '\0';
	    rest = _comp;
	}
    }
    
    /* create an open group */
    if (NULL==(grp = H5FL_ALLOC(H5G_t,1)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");
    if (H5G_stab_create(grp_ent.file, size_hint, &(grp->ent)/*out*/) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't create grp");
    
    /* insert child name into parent */
    if (1!=H5O_link(&(grp->ent), 1))
	HGOTO_ERROR(H5E_SYM, H5E_LINK, NULL, "link inc failure");
    if (H5G_stab_insert(&grp_ent, rest, &(grp->ent)) < 0) {
	H5O_close(&(grp->ent));
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, NULL, "can't insert");
    }
    grp->nref = 1;

    /* Set return value */
    ret_value=grp;

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp_ent);
    
				if(ret_value==NULL) {
					        if(grp!=NULL)
            H5FL_FREE(H5G_t,grp);
    } /* end if */

    FUNC_LEAVE(ret_value);
}


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
htri_t
H5G_isa(H5G_entry_t *ent)
{
    htri_t	ret_value;
    
    FUNC_ENTER_NOAPI(H5G_isa, FAIL);

    assert(ent);

    if ((ret_value=H5O_exists(ent, H5O_STAB, 0))<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read object header");

done:
    FUNC_LEAVE(ret_value);
}
    

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
 * Modifications:
 *      Modified to call H5G_open_oid - QAK - 3/17/99
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_open(H5G_entry_t *loc, const char *name)
{
    H5G_t		*grp = NULL;
    H5G_t		*ret_value = NULL;
    H5G_entry_t ent;            	/*dataset symbol table entry	*/

    FUNC_ENTER_NOAPI(H5G_open, NULL);

    /* Check args */
    assert(loc);
    assert(name && *name);

    /* Open the object, making sure it's a group */
    if (H5G_find(loc, name, NULL, &ent/*out*/) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "group not found");

    /* Open the group object */
    if ((grp=H5G_open_oid(&ent)) ==NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "not found");

    /* Set return value */
    ret_value = grp;

done:
    if (!ret_value && grp)
        H5FL_FREE(H5G_t,grp);

				/*Free the ID to name buffer */
    H5G_free_ent_name(&ent);


    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
	*	Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	Added a deep copy of the symbol table entry
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_open_oid(H5G_entry_t *ent)
{
    H5G_t		*grp = NULL;
    H5G_t		*ret_value = NULL;
    H5O_stab_t		mesg;

    FUNC_ENTER_NOAPI(H5G_open_oid, NULL);

    /* Check args */
    assert(ent);

    /* Open the object, making sure it's a group */
    if (NULL==(grp = H5FL_ALLOC(H5G_t,1)))
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

				/*deep copy of the symbol table entry*/
				if (H5G_ent_copy(ent,&(grp->ent))<0)
					HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to copy entry");

    /* Grab the object header */
    if (H5O_open(&(grp->ent)) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, NULL, "unable to open group");
    if (NULL==H5O_read (&(grp->ent), H5O_STAB, 0, &mesg)) {
        H5O_close(&(grp->ent));
        HGOTO_ERROR (H5E_SYM, H5E_CANTOPENOBJ, NULL, "not a group");
    }
    grp->nref = 1;

    /* Set return value */
    ret_value = grp;

done:
    if (!ret_value && grp)
        H5FL_FREE(H5G_t,grp);

    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_reopen
 *
 * Purpose:	Reopens a group by incrementing the open count.
 *
 * Return:	Success:	The GRP argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Monday, January	 5, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_reopen(H5G_t *grp)
{
    H5G_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5G_reopen, NULL);

    assert(grp);
    assert(grp->nref > 0);

    grp->nref++;

    /* Set return value */
    ret_value=grp;

done:
    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_close(H5G_t *grp)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_close, FAIL);

    /* Check args */
    assert(grp);
    assert(grp->nref > 0);

    if (1 == grp->nref) {
	assert (grp!=H5G_rootof(H5G_fileof(grp)));
	if (H5O_close(&(grp->ent)) < 0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to close");
	grp->nref = 0;
	H5FL_FREE (H5G_t,grp);
    } else {
	--grp->nref;
    }

done:
    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5G_t *
H5G_rootof(H5F_t *f)
{
    H5G_t *ret_value;   /* Return value */

    FUNC_ENTER_NOAPI(H5G_rootof, NULL);
    while (f->mtab.parent)
        f = f->mtab.parent;

    /* Set return value */
    ret_value=f->shared->root_grp;

done:
    FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_insert
 *
 * Purpose:	Inserts a symbol table entry into the group graph.
 *
 * Errors:
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Friday, September 19, 1997
 *
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_insert(H5G_entry_t *loc, const char *name, H5G_entry_t *ent)
{
    const char	*rest = NULL;	/*part of name not existing yet */
    H5G_entry_t	grp;		/*entry for group to contain obj */
    size_t	nchars;		/*number of characters in name	*/
    char	_comp[1024];	/*name component		*/
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_insert, FAIL);

    /* Check args. */
    assert (loc);
    assert (name && *name);
    assert (ent);

    /*
     * Look up the name -- it shouldn't exist yet.
     */
    if (H5G_namei(loc, name, &rest, &grp, NULL, H5G_TARGET_NORMAL, NULL)>=0)
	HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "already exists");
    H5E_clear(); /*it's OK that we didn't find it */
    rest = H5G_component(rest, &nchars);

    /*
     * There should be one component left.  Make sure it's null
     * terminated.
     */
    if (rest[nchars]) {
	if (H5G_component(rest + nchars, NULL)) {
	    HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
	} else if (nchars + 1 > sizeof _comp) {
	    HGOTO_ERROR(H5E_SYM, H5E_COMPLEN, FAIL, "component is too long");
	} else {
	    /* null terminate */
	    HDmemcpy(_comp, rest, nchars);
	    _comp[nchars] = '\0';
	    rest = _comp;
	}
    }

    /*
     * Insert the object into a symbol table.
     */
    if (H5O_link(ent, 1) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_LINK, FAIL, "unable to increment hard link count");
    if (H5G_stab_insert(&grp, rest, ent) < 0) {
	H5O_link(ent, -1);
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to insert name");
    }

done:
				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp);

    FUNC_LEAVE(ret_value);
}


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
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_find(H5G_entry_t *loc, const char *name,
	 H5G_entry_t *grp_ent/*out*/, H5G_entry_t *obj_ent/*out*/)
{
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_find, FAIL);

    /* check args */
    assert (loc);
    assert (name && *name);

    if (H5G_namei(loc, name, NULL, grp_ent, obj_ent, H5G_TARGET_NORMAL, NULL)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");

done:
    FUNC_LEAVE(ret_value);
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
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5G_entof);

    FUNC_LEAVE(grp ? &(grp->ent) : NULL);
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
    /* Use FUNC_ENTER_NOINIT here to avoid performance issues */
    FUNC_ENTER_NOINIT(H5G_fileof);

    assert (grp);

    FUNC_LEAVE(grp->ent.file);
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
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of data type");
            break;

        case H5I_DATASPACE:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of data space");

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
                
        case H5I_TEMPBUF:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "unable to get symbol table entry of buffer");
        
        case H5I_NGROUPS:
        case H5I_BADID:
        case H5I_FILE_CLOSING:
        case H5I_REFERENCE:
        case H5I_VFL:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid object ID");
    }

done:
    FUNC_LEAVE (ret_value);
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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_link (H5G_entry_t *cur_loc, const char *cur_name, H5G_entry_t *new_loc,
	  const char *new_name, H5G_link_t type, unsigned namei_flags)
{
    H5G_entry_t		cur_obj;	/*entry for the link tail	*/
    H5G_entry_t		grp_ent;	/*ent for grp containing link hd*/
    H5O_stab_t		stab_mesg;	/*symbol table message		*/
    const char		*rest = NULL;	/*last component of new name	*/
    char		_comp[1024];	/*name component		*/
    size_t		nchars;		/*characters in component	*/
    size_t		offset;		/*offset to sym-link value	*/
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_link, FAIL);

    /* Check args */
    assert (cur_loc);
    assert (new_loc);
    assert (cur_name && *cur_name);
    assert (new_name && *new_name);

    switch (type) {
        case H5G_LINK_SOFT:
            /*
             * Lookup the the new_name so we can get the group which will contain
             * the new entry.  The entry shouldn't exist yet.
             */
            if (H5G_namei (new_loc, new_name, &rest, &grp_ent, NULL, 
                            H5G_TARGET_NORMAL, NULL)>=0)
                HGOTO_ERROR (H5E_SYM, H5E_EXISTS, FAIL, "already exists");
            H5E_clear (); /*it's okay that we didn't find it*/
            rest = H5G_component (rest, &nchars);

            /*
             * There should be one component left.  Make sure it's null
             * terminated and that `rest' points to it.
             */
            if (rest[nchars]) {
                if (H5G_component (rest+nchars, NULL)) {
                    HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "component not found");
                } else if (nchars+1 > sizeof _comp) {
                    HGOTO_ERROR (H5E_SYM, H5E_COMPLEN, FAIL, "name component is too long");
                } else {
                    HDmemcpy (_comp, rest, nchars);
                    _comp[nchars] = '\0';
                    rest = _comp;
                }
            }

            /*
             * Add the link-value to the local heap for the symbol table which
             * will contain the link.
             */
            if (NULL==H5O_read (&grp_ent, H5O_STAB, 0, &stab_mesg))
                HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine local heap address");
            if ((size_t)(-1)==(offset=H5HL_insert (grp_ent.file,
                   stab_mesg.heap_addr, HDstrlen(cur_name)+1, cur_name)))
                HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to write link value to local heap");
            H5O_reset (H5O_STAB, &stab_mesg);

            /*
             * Create a symbol table entry for the link.  The object header is
             * undefined and the cache contains the link-value offset.
             */
            HDmemset (&cur_obj, 0, sizeof cur_obj);
            cur_obj.header = HADDR_UNDEF;
            cur_obj.file = grp_ent.file;
            cur_obj.type = H5G_CACHED_SLINK;
            cur_obj.cache.slink.lval_offset = offset;

            /*
             * Insert the link head in the symbol table.  This shouldn't ever
             * fail because we've already checked that the link head doesn't
             * exist and the file is writable (because the local heap is
             * writable).  But if it does, the only side effect is that the local
             * heap has some extra garbage in it.
             */
            if (H5G_stab_insert (&grp_ent, rest, &cur_obj)<0)
                HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to create new name/link for object");

												
												
            break;

        case H5G_LINK_HARD:
            if (H5G_namei(cur_loc, cur_name, NULL, NULL, &cur_obj, namei_flags, NULL)<0)
                HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "source object not found");
            if (H5G_insert (new_loc, new_name, &cur_obj)<0)
                HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to create new name/link for object");

												
            break;

        default:
            HGOTO_ERROR (H5E_SYM, H5E_BADVALUE, FAIL, "unrecognized link type");
    }

done:

    if ( type == H5G_LINK_SOFT )
					/*Free the ID to name buffer */
     H5G_free_ent_name(&grp_ent);
			  
    H5G_free_ent_name(&cur_obj);
    FUNC_LEAVE (ret_value);
}


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
H5G_get_type(H5G_entry_t *ent)
{
    htri_t	isa;
    size_t	i;
    int         ret_value=H5G_UNKNOWN;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_get_type, H5G_UNKNOWN);

    for (i=H5G_ntypes_g; i>0; --i) {
	if ((isa=(H5G_type_g[i-1].isa)(ent))<0) {
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type");
	} else if (isa) {
	    HGOTO_DONE(H5G_type_g[i-1].type);
	}
    }

    if (0==i)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, H5G_UNKNOWN, "unable to determine object type");

done:
    FUNC_LEAVE(ret_value);
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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_get_objinfo (H5G_entry_t *loc, const char *name, hbool_t follow_link,
		 H5G_stat_t *statbuf/*out*/)
{
    H5O_stab_t		stab_mesg;
    H5G_entry_t		grp_ent, obj_ent;
    const char		*s = NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_get_objinfo, FAIL);

    assert (loc);
    assert (name && *name);
    if (statbuf) HDmemset (statbuf, 0, sizeof *statbuf);

    /* Find the object's symbol table entry */
    if (H5G_namei (loc, name, NULL, &grp_ent/*out*/, &obj_ent/*out*/,
		   (unsigned)(follow_link?H5G_TARGET_NORMAL:H5G_TARGET_SLINK), NULL)<0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "unable to stat object");

    /*
     * Initialize the stat buf.  Symbolic links aren't normal objects and
     * therefore don't have much of the normal info.  However, the link value
     * length is specific to symbolic links.
     */
    if (statbuf) {
	if (H5G_CACHED_SLINK==obj_ent.type) {
	    /* Named object is a symbolic link */
	    if (NULL==H5O_read (&grp_ent, H5O_STAB, 0, &stab_mesg) ||
		NULL==(s=H5HL_peek (grp_ent.file, stab_mesg.heap_addr, 
				    obj_ent.cache.slink.lval_offset)))
		HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to read symbolic link value");
	    statbuf->linklen = HDstrlen(s)+1; /*count the null terminator*/
	    statbuf->objno[0] = statbuf->objno[1] = 0;
	    statbuf->nlink = 0;
	    statbuf->type = H5G_LINK;
	    statbuf->mtime = 0;
	    
	} else {
	    /* Some other type of object */
	    statbuf->objno[0] = (unsigned long)(obj_ent.header);
#if H5_SIZEOF_UINT64_T>H5_SIZEOF_LONG
	    statbuf->objno[1] = (unsigned long)(obj_ent.header >>
						8*sizeof(long));
#else
	    statbuf->objno[1] = 0;
#endif
	    statbuf->nlink = H5O_link (&obj_ent, 0);
	    statbuf->type = H5G_LINK;
	    if (NULL==H5O_read(&obj_ent, H5O_MTIME, 0, &(statbuf->mtime))) {
		H5E_clear();
		statbuf->mtime = 0;
	    }
	    statbuf->type = H5G_get_type(&obj_ent);
	    H5E_clear(); /*clear errors resulting from checking type*/
	}

        /* Common code to retrieve the file's fileno */
        if(H5F_get_fileno(obj_ent.file,statbuf->fileno)<0)
            HGOTO_ERROR (H5E_FILE, H5E_BADVALUE, FAIL, "unable to read fileno");
    }

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp_ent);
    H5G_free_ent_name(&obj_ent);

    FUNC_LEAVE (ret_value);
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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_linkval (H5G_entry_t *loc, const char *name, size_t size, char *buf/*out*/)
{
    const char		*s = NULL;
    H5G_entry_t		grp_ent, obj_ent;
    H5O_stab_t		stab_mesg;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_linkval, FAIL);

    /*
     * Get the symbol table entry for the link head and the symbol table
     * entry for the group in which the link head appears.
     */
    if (H5G_namei (loc, name, NULL, &grp_ent/*out*/, &obj_ent/*out*/,
		   H5G_TARGET_SLINK, NULL)<0)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "symbolic link was not found");
    if (H5G_CACHED_SLINK!=obj_ent.type)
	HGOTO_ERROR (H5E_SYM, H5E_NOTFOUND, FAIL, "object is not a symbolic link");

    /*
     * Get the address of the local heap for the link value and a pointer
     * into that local heap.
     */
    if (NULL==H5O_read (&grp_ent, H5O_STAB, 0, &stab_mesg))
	HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to determine local heap address");
    if (NULL==(s=H5HL_peek (grp_ent.file, stab_mesg.heap_addr,
			    obj_ent.cache.slink.lval_offset)))
	HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "unable to read symbolic link value");
    
    /* Copy to output buffer */
    if (size>0 && buf)
	HDstrncpy (buf, s, size);

done:
				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp_ent);
    H5G_free_ent_name(&obj_ent);

    FUNC_LEAVE (ret_value);
}


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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_set_comment(H5G_entry_t *loc, const char *name, const char *buf)
{
    H5G_entry_t	obj_ent;
    H5O_name_t	comment;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_set_comment, FAIL);

    /* Get the symbol table entry for the object */
    if (H5G_namei(loc, name, NULL, NULL, &obj_ent/*out*/, H5G_TARGET_NORMAL,
		  NULL)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");

    /* Remove the previous comment message if any */
    if (H5O_remove(&obj_ent, H5O_NAME, 0)<0) H5E_clear();

    /* Add the new message */
    if (buf && *buf) {
	comment.s = H5MM_xstrdup(buf);
	if (H5O_modify(&obj_ent, H5O_NAME, H5O_NEW_MESG, 0, &comment)<0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to set comment object header message");
	H5O_reset(H5O_NAME, &comment);
    }

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&obj_ent);

    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
int
H5G_get_comment(H5G_entry_t *loc, const char *name, size_t bufsize, char *buf)
{
    H5O_name_t	comment;
    H5G_entry_t	obj_ent;
    int	ret_value;
    
    FUNC_ENTER_NOAPI(H5G_get_comment, FAIL);

    /* Get the symbol table entry for the object */
    if (H5G_namei(loc, name, NULL, NULL, &obj_ent/*out*/, H5G_TARGET_NORMAL,
		  NULL)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");

    /* Get the message */
    comment.s = NULL;
    if (NULL==H5O_read(&obj_ent, H5O_NAME, 0, &comment)) {
	if (buf && bufsize>0)
            buf[0] = '\0';
	ret_value = 0;
    } else {
	HDstrncpy(buf, comment.s, bufsize);
	ret_value = (int)HDstrlen(comment.s);
	H5O_reset(H5O_NAME, &comment);
    }

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&obj_ent);

    FUNC_LEAVE(ret_value);
}
    

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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_unlink(H5G_entry_t *loc, const char *name)
{
    H5G_entry_t		grp_ent, obj_ent;
    size_t		len;
    const char		*base=NULL;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_unlink, FAIL);
    assert(loc);
    assert(name && *name);

    /* Get the entry for the group that contains the object to be unlinked */
    if (H5G_namei(loc, name, NULL, &grp_ent, &obj_ent,
		  H5G_TARGET_SLINK|H5G_TARGET_MOUNT, NULL)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
    if (!H5F_addr_defined(grp_ent.header))
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "no containing group specified");
    if (NULL==(base=H5G_basename(name, &len)) || '/'==*base)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "problems obtaining object base name");
    
    /* Remove the name from the symbol table */
    if (H5G_stab_remove(&grp_ent, base)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to unlink name from symbol table");

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp_ent);
    H5G_free_ent_name(&obj_ent);

    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 *	Raymond Lu
 *	Thursday, April 18, 2002
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 22 Aug 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_move(H5G_entry_t *src_loc, const char *src_name, H5G_entry_t *dst_loc, 
		const char *dst_name)
{
    H5G_stat_t		sb;
    char		*linkval=NULL;
    size_t		lv_size=32;
    herr_t      ret_value=SUCCEED;       /* Return value */
    
    FUNC_ENTER_NOAPI(H5G_move, FAIL);
    assert(src_loc);
    assert(dst_loc);
    assert(src_name && *src_name);
    assert(dst_name && *dst_name);

    if (H5G_get_objinfo(src_loc, src_name, FALSE, &sb)<0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found");
    if (H5G_LINK==sb.type) {
	/*
	 * When renaming a symbolic link we rename the link but don't change
	 * the value of the link.
	 */
	do {
	    if (NULL==(linkval=H5MM_realloc(linkval, 2*lv_size)))
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate space for symbolic link value");
	    linkval[lv_size-1] = '\0';
	    if (H5G_linkval(src_loc, src_name, lv_size, linkval)<0)
		HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to read symbolic link value");
	} while (linkval[lv_size-1]);
	if (H5G_link(src_loc, linkval, dst_loc, dst_name, H5G_LINK_SOFT,
		     H5G_TARGET_NORMAL)<0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to rename symbolic link");
	H5MM_xfree(linkval);
	
    } else {
	/*
	 * Rename the object.
	 */
	if (H5G_link(src_loc, src_name, dst_loc, dst_name, H5G_LINK_HARD,
		     H5G_TARGET_MOUNT)<0)
	    HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to register new name for object");
    }

    /* Remove the old name */
    if (H5G_unlink(src_loc, src_name)<0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to deregister old object name");

				/* 
				Search the symbol table entry list and replace names through group IDs
				This has to be done here because H5G_link and H5G_unlink have 
				internal object entries , and do not modify the entries list
				*/
   if (H5G_replace_name( sb.type, src_loc, src_name, dst_name, OP_MOVE )<0)
	HRETURN_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ");

done:
    FUNC_LEAVE(ret_value);
}


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
 * Modifications:
 *
 * Pedro Vicente, <pvn@ncsa.uiuc.edu> 18 Sep 2002
 *	Added `id to name' support.
 *
 *-------------------------------------------------------------------------
 */
H5F_t *
H5G_insertion_file(H5G_entry_t *loc, const char *name)
{
    const char	*rest;
    H5G_entry_t	grp_ent;
    size_t	size;
    H5F_t      *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_insertion_file, NULL);
    assert(loc);
    assert(name && *name);

    /*
     * Look up the name to get the containing group and to make sure the name
     * doesn't already exist.
     */
    if (H5G_namei(loc, name, &rest, &grp_ent, NULL, H5G_TARGET_NORMAL, NULL)>=0)
	HGOTO_ERROR(H5E_SYM, H5E_EXISTS, NULL, "name already exists");
    H5E_clear();

    /* Make sure only the last component wasn't resolved */
    rest = H5G_component(rest, &size);
    assert(*rest && size>0);
    rest = H5G_component(rest+size, NULL);
    if (*rest)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, NULL, "insertion point not found");

    /* Set return value */
    ret_value=grp_ent.file;

done:

				/*Free the ID to name buffer */
    H5G_free_ent_name(&grp_ent);
    FUNC_LEAVE(ret_value);
}



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
 * Comments: Used now only on the root group close , on H5F_close
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_free_grp_name(H5G_t *grp)
{
 herr_t      ret_value=SUCCEED;       /* Return value */
	H5G_entry_t *ent;
 
 FUNC_ENTER_NOAPI(H5G_free_grp_name, FAIL);
 
 /* Check args */
 assert(grp);
 assert(grp->nref > 0);

	if (NULL==( ent = H5G_entof(grp)))
  HGOTO_ERROR (H5E_SYM, H5E_CANTINIT, FAIL, "cannot get entry");

	H5G_free_ent_name(ent);
 
 
done:
 FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function:	H5G_free_ent_name
 *
 * Purpose:	Free the 'ID to name' buffers.
 *
 * Return:	Success
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_free_ent_name(H5G_entry_t *ent)
{
 herr_t      ret_value=SUCCEED;       /* Return value */
 
 FUNC_ENTER_NOAPI(H5G_free_ent_name, FAIL);
 
 /* Check args */
 assert(ent);

	if ( ent->name )
		ent->name = H5MM_xfree(ent->name);
	if ( ent->old_name )
		ent->old_name = H5MM_xfree(ent->old_name);
 
done:
 FUNC_LEAVE(ret_value);
}


/*-------------------------------------------------------------------------
 * Function: H5G_insert_name
 *
 * Purpose: Insert a name into the symbol entry OBJ, located at LOC
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2002
 *
 * Comments: The allocated memory (H5MM_malloc) is freed in H5O_close
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

herr_t 
H5G_insert_name(H5G_entry_t *loc, H5G_entry_t *obj, const char *name)
{
 
 size_t  len1, len2;
 herr_t  ret_value = SUCCEED;       
 
 FUNC_ENTER_NOAPI(H5G_insert_name, FAIL);
 
 if ( loc->name ) {
  
  len1 = HDstrlen(loc->name);
  len2 = HDstrlen(name);
  assert(len2>0&&len1>0);
		if(obj->name)
			obj->name=H5MM_xfree(obj->name);

  /* this is the root group */
  if ('/'==loc->name[len1-1]) 
  {
   
   if (NULL==(obj->name = H5MM_malloc (len1+len2+1)))
    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
   HDstrcpy(obj->name, loc->name);
   HDstrcpy(obj->name+len1, name);
   
  } 
  /* this is a subgroup */
  else 
  {
   if (NULL==(obj->name = H5MM_malloc (len1+1+len2+1)))
    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
   HDstrcpy(obj->name, loc->name);
   HDstrcpy(obj->name+len1, "/");
   HDstrcpy(obj->name+len1+1, name);
  }
 }
 
done:
 FUNC_LEAVE(ret_value);
 
}



/*-------------------------------------------------------------------------
 * Function: H5G_replace_name
 *
 * Purpose: Search the symbol table entry list and replace names through group IDs
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 11, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */



herr_t H5G_replace_name( int type, H5G_entry_t *loc, const char *src_name, 
                        const char *dst_name, int op )
{
 
 H5G_names_t names;
 herr_t  ret_value = SUCCEED;       
 
 FUNC_ENTER_NOAPI(H5G_replace_name, FAIL);
 
 names.src_name=src_name;
 names.dst_name=dst_name;
 names.loc=loc;
 names.op=op;
 
 switch ( type )
 {
  
  /* Object is a group  */
 case H5G_GROUP:
  
  /* Search and replace names through group IDs */
  names.obj_type = H5I_GROUP;
  H5I_search(H5I_GROUP, (H5I_search_func_t)H5G_replace_ent, &names );
  
  break;
  
  /* Object is a dataset */
 case H5G_DATASET:
  
  /* Search and replace names through dataset IDs */
  names.obj_type = H5I_DATASET;
  H5I_search(H5I_DATASET, (H5I_search_func_t)H5G_replace_ent, &names );
  
  break;
  
  /* Object is a named data type */
 case H5G_TYPE:
  
  /* Search and replace names through datatype IDs */
  names.obj_type = H5I_DATATYPE;
  H5I_search(H5I_DATATYPE, (H5I_search_func_t)H5G_replace_ent, &names );
  
  break;
  
  /* We pass H5G_UNKNOWN as object type for the H5Fmount case; search all IDs */
 case H5G_UNKNOWN:
  
  /* Search through group IDs */
  if ( H5I_nmembers(H5I_GROUP) ) {
   names.obj_type = H5I_GROUP;
   H5I_search(H5I_GROUP, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  /* Search through dataset IDs */
  if ( H5I_nmembers(H5I_DATASET) ) {
   names.obj_type = H5I_DATASET;
   H5I_search(H5I_DATASET, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  /* Search through datatype IDs */
  if ( H5I_nmembers(H5I_DATATYPE) ) {
   names.obj_type = H5I_DATATYPE;
   H5I_search(H5I_DATATYPE, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  
  break;
  
  /* Object is a symbolic link */
  
 case H5G_LINK:
  
  /* Search through group IDs */
  if ( H5I_nmembers(H5I_GROUP) ) {
   names.obj_type = H5I_GROUP;
   H5I_search(H5I_GROUP, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  /* Search through dataset IDs */
  if ( H5I_nmembers(H5I_DATASET) ) {
   names.obj_type = H5I_DATASET;
   H5I_search(H5I_DATASET, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  /* Search through datatype IDs */
  if ( H5I_nmembers(H5I_DATATYPE) ) {
   names.obj_type = H5I_DATATYPE;
   H5I_search(H5I_DATATYPE, (H5I_search_func_t)H5G_replace_ent, &names );
  }
  
  break;
  
 default:
  HRETURN_ERROR (H5E_DATATYPE, H5E_BADTYPE, FAIL,
   "not valid object type");
  
 };
 
done: 
 FUNC_LEAVE( SUCCEED );
}



/*-------------------------------------------------------------------------
 * Function: H5G_rest
 *
 * Purpose: Get the last component of the name
 *
 * Return: SUCCEED
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 5, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

 static herr_t
  H5G_rest( const char *name, const char **rest/*out*/ )  
 {
  
  size_t  nchars;
  herr_t  ret_value = SUCCEED;       
  
  FUNC_ENTER_NOAPI(H5G_rest, FAIL);
  
  /* traverse the name */
  while ((name = H5G_component(name, &nchars)) && *name) 
  {
   if (rest) *rest = name;
   
   /* next component */
   name += nchars;
  }
  
  
done:
  FUNC_LEAVE(SUCCEED);
  
 }

/*-------------------------------------------------------------------------
 * Function: H5G_replace_ent
 *
 * Purpose: H5I_search callback function to replace group entry names
 *
 * Return: Success: 0, Failure: -1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: June 5, 2002
 *
 * Comments: 
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
 static herr_t
  H5G_replace_ent(void *obj_ptr, hid_t obj_id, void *key)
 {
  H5G_names_t *names = key;
  H5G_entry_t *ent = NULL;
  size_t      len1, len2;
  char        *tmp_buf;
  H5F_t       *parent_file;
  H5F_t       *child_file;
  H5F_mount_t *child_mnt;
  H5G_entry_t *child_grp_ent;
  unsigned    i;
  size_t      len;
  const char  *rest = NULL;  /*the base name  */
  int         cmp;
  char        *new_src_name;
  char        *new_dst_name;
  herr_t      ret_value = FAIL;       
  
  FUNC_ENTER_NOAPI(H5G_replace_ent, FAIL);
  
  assert(obj_ptr);
 
  /* avoid no named datatypes */
  if( names->obj_type==H5I_DATATYPE && !H5T_is_named((H5T_t*)obj_ptr)) 
  {
   /* Do not exit loop */
   ret_value = SUCCEED;
   goto done;
  }
 
  /* Get the symbol table entry */
  switch(names->obj_type) {
  case H5I_GROUP:
   ent = H5G_entof((H5G_t*)obj_ptr);
   break;
  case H5I_DATASET:
   ent = H5D_entof((H5D_t*)obj_ptr);
   break;
  case H5I_DATATYPE:
   ent = H5T_entof((H5T_t*)obj_ptr);
   break;
  default:
   ret_value = FAIL;
   HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
    "unknown data object");
  }

		assert( ent );
 
  
  /* Check if is a mounted file */
  if(ent->file->mtab.parent) {
   
   if(ent->file->mtab.parent->shared != names->loc->file->shared ) 
   {
    /* Do not exit loop */
    ret_value = SUCCEED;
    goto done;
   }
   
   
   if ( names->op==OP_UNLINK || names->op==OP_MOVE ) {
    parent_file  = ent->file->mtab.parent;
    for ( i = 0; i < parent_file->mtab.nmounts; i++ )
    {
     child_mnt = &parent_file->mtab.child[i];
     child_file  = child_mnt->file;
     
     /* we found the right file */
     if( ent->file->shared == child_file->shared ) 
     {
      child_grp_ent = &child_mnt->group->ent;
      len = HDstrlen( child_grp_ent->name );
      
      /* Find the prefix of the name */
      cmp = HDstrncmp( child_grp_ent->name, names->src_name, len);
      if ( cmp == 0 )
      {
       new_src_name=H5MM_strdup(names->src_name+len);
       
       if (names->op==OP_UNLINK)
        new_dst_name=H5MM_strdup(names->dst_name);
       else if (names->op==OP_MOVE)
        new_dst_name=H5MM_strdup(names->dst_name+len);
       
       /* Search the symbol table entry list and replace names through group IDs */
       if (H5G_replace_name( H5G_UNKNOWN, 
        child_grp_ent, 
        new_src_name, 
        new_dst_name, 
        names->op )<0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "unable to replace name ");
       
       H5MM_xfree(new_dst_name);

							H5MM_xfree(new_src_name);
       
      }/* if */
      
     }/* if */
     
    } /* for */

   }/* if */
   
  } /* if */
  
  /* Verify if file IDs refer to the same file */
  else {
   if( ent->file->shared != names->loc->file->shared ) 
   {
    /* Do not exit loop */
    ret_value = SUCCEED;
    goto done;
   }
  }
  
  /* Get the type of call we are doing */
  switch(names->op) {
   
  /*-------------------------------------------------------------------------
  * OP_MOUNT
  *-------------------------------------------------------------------------
   */
   
  case OP_MOUNT:
   
   /* Find entries that might contain part of this name */
   if ( HDstrstr( ent->name, names->src_name ))
   {
    
    /* Store the old name for unmount case */
    /*ent->old_name = ent->name;*/
    ent->old_name = HDstrdup(ent->name); 
    
    if (names->dst_name) {
     len1 = HDstrlen( names->dst_name );
     ent->name = H5MM_malloc( len1+1 );
     HDstrcpy( ent->name, names->dst_name );
    }
    else
     HDstrcpy( ent->name, "" );
    
   }
   
   /* Do not exit loop */
   ret_value = 0;
   goto done;
   break;
   
   /*-------------------------------------------------------------------------
   * OP_UNMOUNT
   *-------------------------------------------------------------------------
   */
   
  case OP_UNMOUNT:
   
   if(ent->name){
    /* Find entries that might contain part of this name */
    if ( HDstrstr( ent->name, names->src_name ))
    {
     
     /* Delete the old name  */
     ent->name=H5MM_xfree(ent->name);
     
     if (names->dst_name){
      len1 = HDstrlen( names->dst_name );
      ent->name = H5MM_malloc( len1+1 );
      HDstrcpy( ent->name, names->dst_name );
     }
    }
   }/*if*/
   
   
   /* See if the entry old name matches  */
   if ( ent->old_name ) {
    if ( HDstrstr( ent->old_name, names->src_name ))
    {
     
     /* Delete the old name  */
     if (ent->name )
      H5MM_xfree(ent->name);
     
     /* Copy the old name to the entry */
     ent->name = ent->old_name;
     ent->old_name = NULL;
    }
   }
   
   
   /* Do not exit loop */
   ret_value = 0;
   
   
   goto done;
   break;
   
   /*-------------------------------------------------------------------------
   * OP_UNLINK
   *-------------------------------------------------------------------------
   */
   
  case OP_UNLINK:
   
   /* H5Gunlink case */
   
   if (ent->name ){
    cmp = HDstrcmp( ent->name, names->src_name );
    
    /* Found the correct entry, just replace the name */
    if ( cmp==0 )
    {
     ent->name=H5MM_xfree(ent->name);
     
     if (names->dst_name){
      len1 = HDstrlen( names->dst_name );
      ent->name = H5MM_malloc( len1+1 );
      HDstrcpy( ent->name, names->dst_name );
     }
     
    }
    
    /* Find other entries that might contain part of this name */
    else if ( cmp>0 )
    {
     
     if ( HDstrstr( ent->name, names->src_name ))
     {
      /* Delete the old name and clear the entry */
      H5MM_xfree(ent->name);
      ent->name=NULL;
     }
     
    }
   }/*if*/
   
   
   /* Do not exit loop */
   ret_value = 0;
   goto done;
   break;
   
   /*-------------------------------------------------------------------------
   * OP_MOVE
   *-------------------------------------------------------------------------
   */
   
  case OP_MOVE:
   
   /* H5Gmove case, check for relative names case */
   
   /* Verify if we have the wanted entry */
   if ( HDstrcmp( ent->name, names->src_name )!=0 )
   {
    
    /* Do not exit loop */
    ret_value = SUCCEED;
    
    /* Get the last component of the name */
    H5G_rest( ent->name, &rest );
    
    /* Relative names case, build the full pathname */
    if ( rest && HDstrcmp( rest, names->src_name )==0 )
    {
     
     len1 = HDstrlen(ent->name);
     len2 = HDstrlen(rest);
     ent->name[len1-len2-1] = '\0';
     
     tmp_buf = HDstrdup(ent->name); 
     
     len1 = HDstrlen(ent->name);
     len2 = HDstrlen(names->dst_name);
     assert(len1>0 && len2>0);
     
     H5MM_xfree(ent->name);
     ent->name = H5MM_malloc(len1+1+len2+1);
     HDstrcpy(ent->name, tmp_buf);
     HDstrcpy(ent->name+len1, "/");
     HDstrcpy(ent->name+len1+1, names->dst_name);
     
     HDfree(tmp_buf);
     
     /* Found entry, exit loop */
     ret_value = 1;
     
    } /* Relative names case */
    
    goto done;
    
   } /* wanted entry */
   
   
   break;
   
   /*-------------------------------------------------------------------------
   * OP_LINK
   *-------------------------------------------------------------------------
   */
   
  case OP_LINK:
   
   /* H5Glink case */
   cmp = HDstrcmp( ent->name, names->src_name );
   
   /* Found the correct entry, just replace the name */
   if ( cmp==0 )
   {
    
    if (ent->name )
     H5MM_xfree(ent->name);
    
    len1 = HDstrlen( names->dst_name );
    ent->name = H5MM_malloc( len1+1 );
    HDstrcpy( ent->name, names->dst_name );
   }
   
   
   /* Do not exit loop */
   ret_value = 0;
   goto done;
   break;
   
   
   
  default:
   ret_value = FAIL;
   HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL,
    "invalid call");
 }
 
 
 
 /* Delete the old name  */
 if (ent->name )
  H5MM_xfree(ent->name);
 
 /* Add the new name */
 len1 = HDstrlen( names->dst_name );
 ent->name = H5MM_malloc( len1+1 );
 HDstrcpy( ent->name, names->dst_name );
 
 /* Found entry, exit loop */
 ret_value = 1;
 
done:
 FUNC_LEAVE(ret_value); 
}

