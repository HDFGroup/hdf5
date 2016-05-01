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
 * Created:     H5Oflush.c
 *              Aug 19, 2010
 *              Mike McGreevy <mamcgree@hdfgroup.org>
 *
 * Purpose:     Object flush/refresh routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Omodule.h"          /* This source code file is part of the H5O module */

/***********/
/* Headers */
/***********/

#include "H5private.h"      /* Generic Functions */
#include "H5Dprivate.h"     /* Datasets */
#include "H5Eprivate.h"     /* Errors   */
#include "H5Fprivate.h"     /* Files    */
#include "H5Gprivate.h"        /* Groups    */
#include "H5Iprivate.h"        /* IDs    */
#include "H5Opkg.h"         /* Objects  */

/********************/
/* Local Prototypes */
/********************/
static herr_t H5O_oh_tag(const H5O_loc_t *oloc, hid_t dxpl_id, haddr_t *tag);

/*************/
/* Functions */
/*************/



/*-------------------------------------------------------------------------
 * Function:    H5O_flush_common
 *
 * Purpose:    	Flushes the object's metadata
 *		Invokes the user-defined callback if there is one.
 *
 * Return:  	Non-negative on success, negative on failure
 *
 * Programmer:  Vailin Choi; Dec 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_flush_common(H5O_loc_t *oloc, hid_t obj_id, hid_t dxpl_id)
{
    haddr_t 	tag = 0;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Retrieve tag for object */
    if(H5O_oh_tag(oloc, dxpl_id, &tag) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "unable to flush object metadata")

    /* Flush metadata based on tag value of the object */
    if(H5F_flush_tagged_metadata(oloc->file, tag, dxpl_id) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush tagged metadata")

    /* Check to invoke callback */
    if(H5F_object_flush_cb(oloc->file, obj_id) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTFLUSH, FAIL, "unable to do object flush callback")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_flush_common() */


/*-------------------------------------------------------------------------
 * Function:    H5O_oh_tag
 *
 * Purpose:     Get object header's address--tag value for the object
 *
 * Return:  	Success:    Non-negative
 *          	Failure:    Negative
 *
 * Programmer: Mike McGreevy
 *             May 19, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_oh_tag(const H5O_loc_t *oloc, hid_t dxpl_id, haddr_t *tag)
{
    H5O_t       *oh = NULL;             /* Object header */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(oloc);

    /* Get object header for object */
    if(NULL == (oh = H5O_protect(oloc, dxpl_id, H5AC__READ_ONLY_FLAG)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to protect object's object header")

    /* Get object header's address (i.e. the tag value for this object) */
    if(HADDR_UNDEF == (*tag = H5O_OH_GET_ADDR(oh)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTGET, FAIL, "unable to get address of object header")

done:
    /* Unprotect object header on failure */
    if(oh && H5O_unprotect(oloc, dxpl_id, oh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_oh_tag() */

