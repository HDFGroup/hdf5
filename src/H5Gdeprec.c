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
 * Created:	H5Gdeprec.c
 *		June 21 2006
 *		James Laird <jlaird@ncsa.uiuc.edu>
 *
 * Purpose:	Deprecated functions from the H5G interface.  These
 *              functions are here for compatibility purposes and may be
 *              removed in the future.  Applications should switch to the
 *              newer APIs.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5G_PACKAGE		/*suppress error about including H5Gpkg   */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5G_init_deprec_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Lprivate.h"         /* Links                                */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* User data for path traversal routine for getting object info */
typedef struct {
    H5G_stat_t  *statbuf;			/* Stat buffer about object */
    hbool_t follow_link;                        /* Whether we are following a link or not */
    H5F_t *loc_file;                            /* Pointer to the file the location is in */
    hid_t dxpl_id;                              /* Dataset transfer property list */
} H5G_trav_goi_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5G_link_hard(hid_t cur_loc_id, const char *cur_name,
    hid_t new_loc_id, const char *new_name);
static herr_t H5G_move(hid_t src_loc_id, const char *src_name,
    hid_t dst_loc_id, const char *dst_name);
static herr_t H5G_set_comment(H5G_loc_t *loc, const char *name,
    const char *buf, hid_t dxpl_id);
static int H5G_get_comment(H5G_loc_t *loc, const char *name,
    size_t bufsize, char *buf, hid_t dxpl_id);
static herr_t H5G_get_objinfo_cb(H5G_loc_t *grp_loc/*in*/, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata/*in,out*/,
    H5G_own_loc_t *own_loc/*out*/);
static herr_t H5G_get_objinfo(const H5G_loc_t *loc, const char *name,
    hbool_t follow_link, H5G_stat_t *statbuf/*out*/, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*--------------------------------------------------------------------------
NAME
   H5G_init_deprec_interface -- Initialize interface-specific information
USAGE
    herr_t H5G_init_deprec_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5G_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5G_init_deprec_interface(void)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5G_init_deprec_interface)

    FUNC_LEAVE_NOAPI(H5G_init())
} /* H5G_init_deprec_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Glink
 *
 * Purpose:	Creates a link between two existing objects.  The new
 *              APIs to do this are H5Lcreate_hard and H5Lcreate_soft.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink(hid_t cur_loc_id, H5L_type_t type, const char *cur_name, const char *new_name)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Glink, FAIL)
    H5TRACE4("e", "iLlss", cur_loc_id, type, cur_name, new_name);

    /* Check arguments */
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!new_name || !*new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(type == H5L_TYPE_HARD) {
        if((ret_value = H5G_link_hard(cur_loc_id, cur_name, H5L_SAME_LOC, new_name)) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "couldn't create link")
    } /* end if */
    else if(type == H5L_TYPE_SOFT) {
        H5G_loc_t	cur_loc;                /* Group location for new link */

        /* Finish checking arguments */
        if(H5G_loc(cur_loc_id, &cur_loc) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

        /* Create the link */
        if(H5L_create_soft(cur_name, &cur_loc, new_name, H5P_DEFAULT, H5P_DEFAULT, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
    } /* end else if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Not a valid link type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Glink() */


/*-------------------------------------------------------------------------
 * Function:	H5Glink2
 *
 * Purpose:	Creates a link between two existing objects.  The new
 *              APIs to do this are H5Lcreate_hard and H5Lcreate_soft.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Glink2(hid_t cur_loc_id, const char *cur_name, H5L_type_t type,
    hid_t new_loc_id, const char *new_name)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Glink2, FAIL)
    H5TRACE5("e", "isLlis", cur_loc_id, cur_name, type, new_loc_id, new_name);

    /* Check arguments */
    if(!cur_name || !*cur_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!new_name || !*new_name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no new name specified")

    if(type == H5L_TYPE_HARD) {
        if((ret_value = H5G_link_hard(cur_loc_id, cur_name, new_loc_id, new_name)) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "couldn't create link")
    } /* end if */
    else if(type == H5L_TYPE_SOFT) {
        H5G_loc_t	new_loc;                /* Group location for new link */

        /* Soft links only need one location, the new_loc_id, but it's possible that
         * new_loc_id is H5L_SAME_LOC */
        if(new_loc_id == H5L_SAME_LOC)
            new_loc_id = cur_loc_id;

        /* Finish checking arguments */
        if(H5G_loc(new_loc_id, &new_loc) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

        /* Create the link */
        if(H5L_create_soft(cur_name, &new_loc, new_name, H5P_DEFAULT, H5P_DEFAULT, H5AC_dxpl_id) < 0)
            HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")
    } /* end else if */
    else
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid link type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Glink2() */


/*-------------------------------------------------------------------------
 * Function:	H5G_link_hard
 *
 * Purpose:	Creates a hard link from NEW_NAME to CUR_NAME.
 *
 *		CUR_NAME must name an existing object.  CUR_NAME and
 *              NEW_NAME are interpreted relative to CUR_LOC_ID and
 *              NEW_LOC_ID, which are either file IDs or group IDs.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_link_hard(hid_t cur_loc_id, const char *cur_name, hid_t new_loc_id,
    const char *new_name)
{
    H5G_loc_t	cur_loc, *cur_loc_p;    /* Information about current link's group */
    H5G_loc_t	new_loc, *new_loc_p;    /* Information about new link's group */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_link_hard)

    /* Finish checking arguments */
    if(cur_loc_id == H5L_SAME_LOC && new_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not be both H5L_SAME_LOC")
    if(cur_loc_id != H5L_SAME_LOC && H5G_loc(cur_loc_id, &cur_loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(new_loc_id != H5L_SAME_LOC && H5G_loc(new_loc_id, &new_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")

    /* Set up current & new location pointers */
    cur_loc_p = &cur_loc;
    new_loc_p = &new_loc;
    if(cur_loc_id == H5L_SAME_LOC)
        cur_loc_p = new_loc_p;
    else if(new_loc_id == H5L_SAME_LOC)
   	new_loc_p = cur_loc_p;
    else if(cur_loc_p->oloc->file != new_loc_p->oloc->file)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should be in the same file.")

    /* Create the link */
    if(H5L_create_hard(cur_loc_p, cur_name, new_loc_p, new_name,
                H5P_DEFAULT, H5P_DEFAULT, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "unable to create link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_link_hard() */


/*-------------------------------------------------------------------------
 * Function:	H5Gmove
 *
 * Purpose:	Moves and renames a link.  The new API to do this is H5Lmove.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gmove(hid_t src_loc_id, const char *src_name, const char *dst_name)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gmove, FAIL)
    H5TRACE3("e", "iss", src_loc_id, src_name, dst_name);

    /* Call common routine to move the link */
    if(H5G_move(src_loc_id, src_name, H5L_SAME_LOC, dst_name) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "couldn't move link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gmove() */


/*-------------------------------------------------------------------------
 * Function:	H5Gmove2
 *
 * Purpose:	Moves and renames a link.  The new API to do this is H5Lmove.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gmove2(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Gmove2, FAIL)
    H5TRACE4("e", "isis", src_loc_id, src_name, dst_loc_id, dst_name);

    /* Call common routine to move the link */
    if(H5G_move(src_loc_id, src_name, dst_loc_id, dst_name) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTINIT, FAIL, "couldn't move link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gmove2() */


/*-------------------------------------------------------------------------
 * Function:	H5G_move
 *
 * Purpose:	Renames an object within an HDF5 file and moves it to a new
 *              group.  The original name SRC is unlinked from the group graph
 *              and then inserted with the new name DST (which can specify a
 *              new path for the object) as an atomic operation. The names
 *              are interpreted relative to SRC_LOC_ID and
 *              DST_LOC_ID, which are either file IDs or group ID.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, November  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_move(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
    const char *dst_name)
{
    H5G_loc_t	src_loc, *src_loc_p;    /* Group info for source location */
    H5G_loc_t	dst_loc, *dst_loc_p;    /* Group info for destination location */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_move)

    /* Check arguments */
    if(src_loc_id == H5L_SAME_LOC && dst_loc_id == H5L_SAME_LOC)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "source and destination should not both be H5L_SAME_LOC")
    if(src_loc_id != H5L_SAME_LOC && H5G_loc(src_loc_id, &src_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(dst_loc_id != H5L_SAME_LOC && H5G_loc(dst_loc_id, &dst_loc) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!src_name || !*src_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no current name specified")
    if(!dst_name || !*dst_name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no destination name specified")

    /* Set up src & dst location pointers */
    src_loc_p = &src_loc;
    dst_loc_p = &dst_loc;
    if(src_loc_id == H5L_SAME_LOC)
        src_loc_p = dst_loc_p;
    else if(dst_loc_id == H5L_SAME_LOC)
        dst_loc_p = src_loc_p;

    /* Move the link */
    if(H5L_move(src_loc_p, src_name, dst_loc_p, dst_name, FALSE, H5P_DEFAULT,
            H5P_DEFAULT, H5AC_dxpl_id) < 0)
	HGOTO_ERROR(H5E_LINK, H5E_CANTMOVE, FAIL, "unable to move link")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_move() */


/*-------------------------------------------------------------------------
 * Function:	H5Gunlink
 *
 * Purpose:	Removes a link.  The new API is H5Ldelete/H5Ldelete_by_idx.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gunlink(hid_t loc_id, const char *name)
{
    H5G_loc_t	loc;                    /* Group's location */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Gunlink, FAIL)
    H5TRACE2("e", "is", loc_id, name);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name")

    /* Call H5L routine... */
    if(H5L_delete(&loc, name, H5P_DEFAULT, H5AC_dxpl_id) < 0)
      HGOTO_ERROR(H5E_LINK, H5E_CANTDELETE, FAIL, "couldn't delete link")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gunlink() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_linkval
 *
 * Purpose:	Retrieve's a soft link's data.  The new API is
 *              H5Lget_val/H5Lget_val_by_idx.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Gget_linkval(hid_t loc_id, const char *name, size_t size, char *buf/*out*/)
{
    H5G_loc_t	loc;                    /* Group's location */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Gget_linkval, FAIL)
    H5TRACE4("e", "iszx", loc_id, name, size, buf);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Call the new link routine which provides this capability */
    if(H5L_get_val(&loc, name, buf, size, H5P_DEFAULT, H5AC_ind_dxpl_id) < 0)
      HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "couldn't get link info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_linkval() */


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
 * Note:	Deprecated in favor of H5Lget_name_by_idx
 *
 * Return:	Success:        Non-negative
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
    H5O_type_t          obj_type;       /* Type of object at location */
    ssize_t		ret_value;

    FUNC_ENTER_API(H5Gget_objname_by_idx, FAIL)
    H5TRACE4("Zs", "ihsz", loc_id, idx, name, size);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5O_obj_type(loc.oloc, &obj_type, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get object type")
    if(obj_type != H5O_TYPE_GROUP)
        HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "not a group")

    /* Call internal function */
    if((ret_value = H5G_obj_get_name_by_idx(loc.oloc, H5_INDEX_NAME, H5_ITER_INC, idx, name, size, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "can't get object name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objname_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Gset_comment
 *
 * Purpose:     Gives the specified object a comment.  The COMMENT string
 *		should be a null terminated string.  An object can have only
 *		one comment at a time.  Passing NULL for the COMMENT argument
 *		will remove the comment property from the object.
 *
 * Note:	Deprecated in favor of using attributes on group
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
    H5TRACE3("e", "iss", loc_id, name, comment);

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
 * Note:	Deprecated in favor of using attributes on group
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
    H5TRACE4("Is", "iszs", loc_id, name, bufsize, buf);

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
    H5G_loc_t   obj_loc;                  /* Object's location */
    H5G_name_t	path;
    H5O_loc_t	oloc;
    hbool_t     loc_valid = FALSE;
    H5O_name_t	comment;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_set_comment)

    /* Get the symbol table entry for the object */
    obj_loc.path = &path;
    obj_loc.oloc = &oloc;
    H5G_loc_reset(&obj_loc);
    if(H5G_loc_find(loc, name, &obj_loc/*out*/, H5P_DEFAULT, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
    loc_valid = TRUE;

    /* Remove the previous comment message if any */
    if(H5O_msg_remove(obj_loc.oloc, H5O_NAME_ID, 0, TRUE, dxpl_id) < 0)
        H5E_clear_stack(NULL);

    /* Add the new message */
    if(buf && *buf) {
        /* Casting away const OK -QAK */
	comment.s = (char *)buf;
	if(H5O_msg_create(obj_loc.oloc, H5O_NAME_ID, 0, H5O_UPDATE_TIME, &comment, dxpl_id) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to set comment object header message")
    } /* end if */

done:
    /* Release obj_loc */
    if(loc_valid && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location")

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
    H5G_loc_t	obj_loc;               /* Object's location */
    H5G_name_t	path;
    H5O_loc_t	oloc;
    hbool_t     loc_valid = FALSE;
    int	ret_value;                     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_comment)

    /* Get the symbol table entry for the object */
    obj_loc.path = &path;
    obj_loc.oloc = &oloc;
    H5G_loc_reset(&obj_loc);
    if(H5G_loc_find(loc, name, &obj_loc/*out*/, H5P_DEFAULT, dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")
    loc_valid = TRUE;

    /* Get the message */
    comment.s = NULL;
    if(NULL == H5O_msg_read(obj_loc.oloc, H5O_NAME_ID, &comment, dxpl_id)) {
	if(buf && bufsize > 0)
            buf[0] = '\0';
	ret_value = 0;
    } else {
        if(buf && bufsize)
	   HDstrncpy(buf, comment.s, bufsize);
	ret_value = (int)HDstrlen(comment.s);
	H5O_msg_reset(H5O_NAME_ID, &comment);
    } /* end else */

done:
    /* Release obj_loc */
    if(loc_valid && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_comment() */


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
 * Note:	Deprecated in favor of H5Literate
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
H5Giterate(hid_t loc_id, const char *name, int *idx_p, H5G_iterate_t op,
    void *op_data)
{
    H5G_link_iterate_t lnk_op;          /* Link operator */
    hsize_t     last_obj;               /* Index of last object looked at */
    hsize_t	idx;                    /* Internal location to hold index */
    herr_t	ret_value;

    FUNC_ENTER_API(H5Giterate, FAIL)
    H5TRACE5("e", "is*Isxx", loc_id, name, idx_p, op, op_data);

    /* Check args */
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")
    if(idx_p && *idx_p < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid index specified")
    if(!op)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no operator specified")

    /* Set number of objects looked at to zero */
    last_obj = 0;
    idx = (hsize_t)(idx_p == NULL ? 0 : *idx_p);

    /* Build link operator info */
    lnk_op.op_type = H5G_LINK_OP_OLD;
    lnk_op.u.old_op = op;

    /* Call private function. */
    if((ret_value = H5G_obj_iterate(loc_id, name, H5_INDEX_NAME, H5_ITER_INC, idx, &last_obj, &lnk_op, op_data, H5AC_ind_dxpl_id)) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_BADITER, FAIL, "group iteration failed")

    /* Set the index we stopped at */
    if(idx_p)
        *idx_p = (int)last_obj;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Giterate() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_objtype_by_idx
 *
 * Purpose:     Returns the type of objects in the group by giving index.
 *
 * Note:	Deprecated in favor of H5Lget_info/H5Oget_info
 *
 * Return:	Success:        H5G_GROUP(1), H5G_DATASET(2), H5G_TYPE(3)
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
    H5O_type_t          obj_type;       /* Type of object at location */
    H5G_obj_t		ret_value;

    FUNC_ENTER_API(H5Gget_objtype_by_idx, H5G_UNKNOWN)
    H5TRACE2("Go", "ih", loc_id, idx);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5G_UNKNOWN, "not a location ID")
    if(H5O_obj_type(loc.oloc, &obj_type, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get object type")
    if(obj_type != H5O_TYPE_GROUP)
        HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "not a group")

    /* Call internal function*/
    if((ret_value = H5G_obj_get_type_by_idx(loc.oloc, idx, H5AC_ind_dxpl_id)) == H5G_UNKNOWN)
	HGOTO_ERROR(H5E_SYM, H5E_BADTYPE, FAIL, "can't get object type")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_objtype_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_objinfo
 *
 * Purpose:	Returns information about an object.  If FOLLOW_LINK is
 *		non-zero then all symbolic links are followed; otherwise all
 *		links except the last component of the name are followed.
 *
 * Note:	Deprecated in favor of H5Lget_info/H5Oget_info
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
    H5TRACE4("e", "isbx", loc_id, name, follow_link, statbuf);

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
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5G_trav_goi_t *udata = (H5G_trav_goi_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_objinfo_cb)

    /* Check if the name in this group resolved to a valid link */
    if(lnk == NULL && obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Only modify user's buffer if it's available */
    if(udata->statbuf) {
        H5G_stat_t *statbuf = udata->statbuf;   /* Convenience pointer for statbuf */

        /* Common code to retrieve the file's fileno */
        /* (Use the object location's file info, if it's available) */
        if(H5F_get_fileno((obj_loc ? obj_loc : grp_loc)->oloc->file, &statbuf->fileno[0]) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "unable to read fileno")

        /* Info for soft and UD links is gotten by H5L_get_info. If we have
         *      a hard link, follow it and get info on the object
         */
        if(udata->follow_link || !lnk || (lnk->type == H5L_TYPE_HARD)) {
            H5O_info_t oinfo;           /* Object information */

            /* Go retrieve the object information */
            if(H5O_get_info(obj_loc->oloc, &oinfo, udata->dxpl_id) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to get object info")

            /* Get mapped object type */
            statbuf->type = H5G_map_obj_type(oinfo.type);

	    /* Get object number (i.e. address) for object */
	    statbuf->objno[0] = (unsigned long)(oinfo.addr);
#if H5_SIZEOF_UINT64_T > H5_SIZEOF_LONG
	    statbuf->objno[1] = (unsigned long)(oinfo.addr >> 8 * sizeof(long));
#else
	    statbuf->objno[1] = 0;
#endif
            /* Get # of hard links pointing to object */
	    statbuf->nlink = oinfo.rc;

            /* Get modification time for object */
            statbuf->mtime = oinfo.ctime;

            /* Retrieve the object header information */
            statbuf->ohdr.size = oinfo.hdr.hdr_size;
            statbuf->ohdr.free = oinfo.hdr.free_space;
            statbuf->ohdr.nmesgs = oinfo.hdr.nmesgs;
            statbuf->ohdr.nchunks = oinfo.hdr.nchunks;
        } /* end if */
    } /* end if */

done:
    /* Indicate that this callback didn't take ownership of the group *
     * location for the object */
    *own_loc = H5G_OWN_NONE;

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
static herr_t
H5G_get_objinfo(const H5G_loc_t *loc, const char *name, hbool_t follow_link,
    H5G_stat_t *statbuf/*out*/, hid_t dxpl_id)
{
    H5G_trav_goi_t udata;           /* User data for callback */
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_get_objinfo)

    HDassert(loc);
    HDassert(name && *name);

    /* Reset stat buffer, if one was given */
    if(statbuf)
        HDmemset(statbuf, 0, sizeof(H5G_stat_t));

    /* Set up user data for retrieving information */
    udata.statbuf = statbuf;
    udata.follow_link = follow_link;
    udata.loc_file = loc->oloc->file;
    udata.dxpl_id = dxpl_id;

    /* Traverse the group hierarchy to locate the object to get info about */
    if(H5G_traverse(loc, name, (unsigned)(follow_link ? H5G_TARGET_NORMAL : H5G_TARGET_SLINK|H5G_TARGET_UDLINK),
            H5G_get_objinfo_cb, &udata, H5P_DEFAULT, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_EXISTS, FAIL, "name doesn't exist")

    /* If we're pointing at a soft or UD link, get the real link length and type */
    if(statbuf && follow_link == 0) {
        H5L_info_t linfo;           /* Link information buffer */
        herr_t ret;

        /* Get information about link to the object. If this fails, e.g.
         * because the object is ".", just treat the object as a hard link. */
        H5E_BEGIN_TRY {
            ret = H5L_get_info(loc, name, &linfo, H5P_DEFAULT, dxpl_id);
        } H5E_END_TRY

        if(ret >= 0 && linfo.type != H5L_TYPE_HARD) {
            statbuf->linklen = linfo.u.val_size;
            if(linfo.type == H5L_TYPE_SOFT)
                statbuf->type = H5G_LINK;
            else {  /* UD link. H5L_get_info checked for invalid link classes */
                HDassert(linfo.type >= H5L_TYPE_UD_MIN && linfo.type <= H5L_TYPE_MAX);
                statbuf->type = H5G_UDLINK;
            } /* end else */
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_objinfo() */


/*-------------------------------------------------------------------------
 * Function:	H5Gget_num_objs
 *
 * Purpose:     Returns the number of objects in the group.  It iterates
 *              all B-tree leaves and sum up total number of group members.
 *
 * Note:	Deprecated in favor of H5Gget_info
 *
 * Return:	Success:        Non-negative
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
    H5G_info_t          grp_info;       /* Group information */
    H5O_type_t          obj_type;       /* Type of object at location */
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_API(H5Gget_num_objs, FAIL)
    H5TRACE2("e", "i*h", loc_id, num_objs);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5O_obj_type(loc.oloc, &obj_type, H5AC_ind_dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't get object type")
    if(obj_type != H5O_TYPE_GROUP)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")
    if(!num_objs)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "bad pointer to # of objects")

    /* Retrieve information about the group */
    if(H5G_obj_info(loc.oloc, &grp_info, H5AC_ind_dxpl_id) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTCOUNT, FAIL, "can't determine")

    /* Set the number of objects [sic: links] in the group */
    *num_objs = grp_info.nlinks;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Gget_num_objs() */

