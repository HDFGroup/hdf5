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
 * Created:		H5Gloc.c
 *			Sep 13 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Functions for working with group "locations"
 *
 *-------------------------------------------------------------------------
 */
#define H5G_PACKAGE		/*suppress error about including H5Gpkg	  */


/* Packages needed by this file... */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Gpkg.h"		/* Groups		  		*/
#include "H5Iprivate.h"		/* IDs			  		*/

/* Private typedefs */

/* User data for looking up an object in a group */
typedef struct {
    H5G_loc_t  *loc;            /* Group location to set */
} H5G_loc_ud1_t;

/* Private macros */

/* Local variables */

/* PRIVATE PROTOTYPES */
static herr_t H5G_loc_find_cb(H5G_loc_t *grp_loc, const char *name,
    const H5O_link_t *lnk, H5G_loc_t *obj_loc, void *_udata,
    H5G_own_loc_t *own_loc/*out*/);


/*-------------------------------------------------------------------------
 * Function:	H5G_loc
 *
 * Purpose:	Given an object ID return a location for the object.
 *
 * Return:	Success:	Group pointer.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc(hid_t loc_id, H5G_loc_t *loc)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc, FAIL)

    switch(H5I_get_type(loc_id)) {
        case H5I_FILE:
            {
                H5F_t	*f;

                if(NULL == (f = H5I_object(loc_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file ID")
                if(NULL == (loc->oloc = H5G_oloc(H5G_rootof(f))))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location for root group")
                if(NULL == (loc->path = H5G_nameof(H5G_rootof(f))))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path for root group")

                /* Patch up root group's object location to reflect this file */
                /* (Since the root group info is only stored once for files which
                 *  share an underlying low-level file)
                 */
                /* (but only for non-mounted files) */
                if(!H5F_is_mount(f))
                {
                    loc->oloc->file = f;
                    loc->oloc->holding_file = FALSE;
                }
            } /* end case */
            break;

        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get group location of property list")

        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get group location of error class, message or stack")

        case H5I_GROUP:
            {
                H5G_t	*group;

                if(NULL == (group = H5I_object(loc_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid group ID")
                if(NULL == (loc->oloc = H5G_oloc(group)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of group")
                if(NULL == (loc->path = H5G_nameof(group)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of group")
            } /* end case */
            break;

        case H5I_DATATYPE:
            {
                H5T_t	*dt;

                if(NULL == (dt = H5I_object(loc_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid type ID")
                if(NULL == (loc->oloc = H5T_oloc(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of datatype")
                if(NULL == (loc->path = H5T_nameof(dt)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of datatype")
            } /* end case */
            break;

        case H5I_DATASPACE:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get group location of dataspace")

        case H5I_DATASET:
            {
                H5D_t	*dset;

                if(NULL == (dset = H5I_object(loc_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid data ID")
                if(NULL == (loc->oloc = H5D_oloc(dset)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of dataset")
                if(NULL == (loc->path = H5D_nameof(dset)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of dataset")
            } /* end case */
            break;

        case H5I_ATTR:
            {
                H5A_t	*attr;

                if(NULL == (attr = H5I_object(loc_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid attribute ID")
                if(NULL == (loc->oloc = H5A_oloc(attr)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get object location of attribute")
                if(NULL == (loc->path = H5A_nameof(attr)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get path of attribute")
            } /* end case */
            break;

        case H5I_REFERENCE:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unable to get group location of reference")

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid object ID")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_copy
 *
 * Purpose:	Copy over information for a location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_copy(H5G_loc_t *dst, H5G_loc_t *src, H5_copy_depth_t depth)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_copy, FAIL)

    /* Check args. */
    HDassert(dst);
    HDassert(src);

    /* Copy components of the location */
    if(H5O_loc_copy(dst->oloc, src->oloc, depth) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")
    if(H5G_name_copy(dst->path, src->path, depth) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to copy path")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_reset
 *
 * Purpose:	Reset information for a location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_reset(H5G_loc_t *loc)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_reset, FAIL)

    /* Check args. */
    HDassert(loc);

    /* Reset components of the location */
    if(H5O_loc_reset(loc->oloc) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to reset entry")
    if(H5G_name_reset(loc->path) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to reset path")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_free
 *
 * Purpose:	Free information for a location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_free(H5G_loc_t *loc)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_free, FAIL)

    /* Check args. */
    HDassert(loc);

    /* Reset components of the location */
#ifdef NOT_YET
    if(H5G_ent_free(loc->ent) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTOPENOBJ, FAIL, "unable to free entry")
#endif /* NOT_YET */
    if(H5G_name_free(loc->path) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTRELEASE, FAIL, "unable to free path")
    if(H5O_loc_free(loc->oloc) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to free object header location")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_free() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_find_cb
 *
 * Purpose:	Callback for retrieving object location for an object in a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, October 17, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G_loc_find_cb(H5G_loc_t UNUSED *grp_loc/*in*/, const char UNUSED *name, const H5O_link_t UNUSED *lnk,
    H5G_loc_t *obj_loc, void *_udata/*in,out*/, H5G_own_loc_t *own_loc/*out*/)
{
    H5G_loc_ud1_t *udata = (H5G_loc_ud1_t *)_udata;   /* User data passed in */
    herr_t ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5G_loc_find_cb)

    /* Check if the name in this group resolved to a valid link */
    if(obj_loc == NULL)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "name doesn't exist")

    /* Take ownership of the object's group location */
    /* (Group traversal callbacks are responsible for either taking ownership
     *  of the group location for the object, or freeing it. - QAK)
     */
    H5G_loc_copy(udata->loc, obj_loc, H5_COPY_SHALLOW);
    *own_loc = H5G_OWN_OBJ_LOC;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_find_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_find
 *
 * Purpose:	Find a symbol from a location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_find(H5G_loc_t *loc, const char *name, H5G_loc_t *obj_loc/*out*/,
    hid_t lapl_id, hid_t dxpl_id)
{
    H5G_loc_ud1_t udata;                /* User data for traversal callback */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_find, FAIL)

    /* Check args. */
    HDassert(loc);
    HDassert(name && *name);
    HDassert(obj_loc);

    /* Set up user data for locating object */
    udata.loc = obj_loc;

    /* Traverse group hierarchy to locate object */
    if(H5G_traverse(loc, name, H5G_TARGET_NORMAL, H5G_loc_find_cb, &udata, lapl_id, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "can't find object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_find() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_insert
 *
 * Purpose:	Insert an object at a location
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, September 13, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_insert(H5G_loc_t *grp_loc, const char *name, H5G_loc_t *obj_loc,
    hid_t dxpl_id)
{
    H5O_link_t  lnk;                    /* Link for object to insert */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_insert, FAIL)

    /* Check args. */
    HDassert(grp_loc);
    HDassert(name && *name);
    HDassert(obj_loc);

    /* Create link object for the object location */
    lnk.type = H5L_TYPE_HARD;
    lnk.cset = H5F_DEFAULT_CSET;
    lnk.corder = 0;     /* Will be reset if the group is tracking creation order */
    lnk.corder_valid = FALSE;   /* Indicate that the creation order isn't valid (yet) */
    /* Casting away const OK -QAK */
    lnk.name = (char *)name;
    lnk.u.hard.addr = obj_loc->oloc->addr;

    /* Insert new group into current group's symbol table */
    if(H5G_obj_insert(grp_loc->oloc, name, &lnk, TRUE, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, FAIL, "unable to insert object")

    /* Set the name of the object location */
    if(H5G_name_set(grp_loc->path, obj_loc->path, name) < 0)
       HGOTO_ERROR(H5E_SYM, H5E_CANTINIT, FAIL, "cannot set name")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_exists
 *
 * Purpose:	Check if a symbol exists in a location
 *
 * Return:	Non-negative if object exists/Negative if object doesn't exist
 *
 * Programmer:	Quincey Koziol
 *		Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_exists(const H5G_loc_t *loc, const char *name, hid_t dxpl_id)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_exists, FAIL)

    /* Check args. */
    HDassert(loc);
    HDassert(name && *name);

    /* Get information for object in current group */
    if(H5G_obj_lookup(loc->oloc, name, NULL, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_exists() */


/*-------------------------------------------------------------------------
 * Function:	H5G_loc_remove
 *
 * Purpose:	Remove a link from a group
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, September 19, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5G_loc_remove(H5G_loc_t *grp_loc, const char *link_name, H5G_loc_t *obj_loc, hid_t dxpl_id)
{
    H5G_obj_t   obj_type;               /* Type of object removed */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5G_loc_remove, FAIL)

    /* Check args. */
    HDassert(grp_loc);
    HDassert(link_name && *link_name);

    /* Remove object from group */
    if(H5G_obj_remove(grp_loc->oloc, link_name, &obj_type, dxpl_id) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "component not found")

    /* Search the open IDs and replace names for unlinked object */
    if(H5G_name_replace(obj_type, obj_loc, NULL, NULL, H5G_NAME_DELETE) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTDELETE, FAIL, "unable to replace name")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G_loc_remove() */

