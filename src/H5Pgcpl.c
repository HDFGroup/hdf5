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

#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */

/* Private header files */
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Ppkg.h"		/* Property lists		  	*/

/* Local datatypes */

/* Static function prototypes */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_local_heap_size_hint
 *
 * Purpose:     Set the "size hint" for creating local heaps for a group.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 29, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_local_heap_size_hint(hid_t plist_id, size_t size_hint)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5O_ginfo_t ginfo;          /* Group information structure */
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API(H5Pset_local_heap_size_hint, FAIL)

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

    /* Update field */
    ginfo.lheap_size_hint = size_hint;

    /* Set value */
    if(H5P_set(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set group info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_local_heap_size_hint() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_local_heap_size_hint
 *
 * Purpose:     Returns the local heap size hint, which is used for creating
 *              groups
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 29, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_local_heap_size_hint(hid_t plist_id, size_t *size_hint /*out*/)
{
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_local_heap_size_hint, FAIL)

    if(size_hint) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5O_ginfo_t ginfo;          /* Group information structure */

        /* Get the plist structure */
        if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Get value */
        if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

        /* Update field */
        *size_hint = ginfo.lheap_size_hint;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_local_heap_size_hint() */

