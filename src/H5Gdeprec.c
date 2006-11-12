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
    H5TRACE4("e","iLlss",cur_loc_id,type,cur_name,new_name);

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
    H5TRACE5("e","isLlis",cur_loc_id,cur_name,type,new_loc_id,new_name);

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
    H5TRACE3("e","iss",src_loc_id,src_name,dst_name);

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
    H5TRACE4("e","isis",src_loc_id,src_name,dst_loc_id,dst_name);

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
    H5TRACE2("e","is",loc_id,name);

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
    H5TRACE4("e","iszx",loc_id,name,size,buf);

    /* Check arguments */
    if(H5G_loc(loc_id, &loc))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
    if(!name || !*name)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no name specified")

    /* Call the new link routine which provides this capability */
    if(H5L_get_val(&loc, name, size, buf, H5P_DEFAULT, H5AC_ind_dxpl_id) < 0)
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
    ssize_t		ret_value;

    FUNC_ENTER_API(H5Gget_objname_by_idx, FAIL)
    H5TRACE4("Zs","ihsz",loc_id,idx,name,size);

    /* Check args */
    if(H5G_loc(loc_id, &loc) < 0)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location ID")
    if(H5O_obj_type(loc.oloc, H5AC_ind_dxpl_id) != H5G_GROUP)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a group")

    /* Call internal function */
    if((ret_value = H5G_obj_get_name_by_idx(loc.oloc, H5L_INDEX_NAME, H5_ITER_INC, idx, name, size, H5AC_ind_dxpl_id)) < 0)
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
    if(H5O_remove(obj_loc.oloc, H5O_NAME_ID, 0, TRUE, dxpl_id) < 0)
        H5E_clear_stack(NULL);

    /* Add the new message */
    if(buf && *buf) {
        /* Casting away const OK -QAK */
	comment.s = (char *)buf;
	if(H5O_modify(obj_loc.oloc, H5O_NAME_ID, H5O_NEW_MESG, 0, H5O_UPDATE_TIME, &comment, dxpl_id) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to set comment object header message")
    } /* end if */

done:
    /* Release obj_loc */
    if(loc_valid)
        if(H5G_loc_free(&obj_loc) < 0)
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
    if(NULL == H5O_read(obj_loc.oloc, H5O_NAME_ID, 0, &comment, dxpl_id)) {
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
    /* Release obj_loc */
    if(loc_valid)
        if(H5G_loc_free(&obj_loc) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_get_comment() */

