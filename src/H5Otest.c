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

/* Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Monday, December 4, 2006
 *
 * Purpose:	Attribute testing functions.
 */

/****************/
/* Module Setup */
/****************/

#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */
#define H5O_TESTING		/*suppress warning about H5O testing funcs*/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Opkg.h"             /* Object headers			*/


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
    H5O_is_attr_dense_test
 PURPOSE
    Determine whether attributes for an object are stored "densely"
 USAGE
    htri_t H5O_is_dense_test(oid)
        hid_t oid;              IN: object to check
 RETURNS
    Non-negative TRUE/FALSE on success, negative on failure
 DESCRIPTION
    Checks to see if the object is storing attributes in the "dense" or
    "compact" form.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
    DO NOT USE THIS FUNCTION FOR ANYTHING EXCEPT TESTING
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
htri_t
H5O_is_attr_dense_test(hid_t oid)
{
    H5O_t *oh = NULL;           /* Object header */
    H5O_loc_t *oloc;            /* Pointer to object's location */
    htri_t ret_value;           /* Return value */

    FUNC_ENTER_NOAPI(H5O_is_attr_dense_test, FAIL)

    /* Get object location for object */
    if(NULL == (oloc = H5O_get_loc(oid)))
        HGOTO_ERROR(H5E_SYM, H5E_NOTFOUND, FAIL, "object not found")

    /* Get the object header */
    if(NULL == (oh = H5AC_protect(oloc->file, H5AC_ind_dxpl_id, H5AC_OHDR, oloc->addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unable to load object header")

    /* Check if dense storage is being used */
    if(H5F_addr_defined(oh->attr_fheap_addr))
        ret_value = TRUE;
    else
        ret_value = FALSE;

done:
    if(oh && H5AC_unprotect(oloc->file, H5AC_ind_dxpl_id, H5AC_OHDR, oloc->addr, oh, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_OHDR, H5E_PROTECT, FAIL, "unable to release object header")

    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5O_is_attr_dense_test() */


