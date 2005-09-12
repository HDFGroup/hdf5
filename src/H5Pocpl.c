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
 * Function:    H5Pset_create_intermediate_group
 *
 * Purpose:     set crt_intmd_group so that H5Gcreate(), H5Dcreate, etc.
 *              will create missing groups along the given path "name"
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 08, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_create_intermediate_group(hid_t plist_id, unsigned crt_intmd_group)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(H5Pset_create_intermediate_group, FAIL);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set value */
    crt_intmd_group = crt_intmd_group > 0 ? 1 : 0;
    if(H5P_set(plist, H5G_CRT_INTERMEDIATE_GROUP_NAME, &crt_intmd_group) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set intermediate group creation flag")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_create_intermediate_group() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_create_intermediate_group
 *
 * Purpose:     Returns the crt_intmd_group, which is set at H5Gcreate(hid_t loc_id,
 *              const char* name, ... ) for create missing groups
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              May 08, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_create_intermediate_group(hid_t plist_id, unsigned *crt_intmd_group /*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(H5Pget_create_intermediate_group, FAIL);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get values */
    if(crt_intmd_group)
        if(H5P_get(plist, H5G_CRT_INTERMEDIATE_GROUP_NAME, crt_intmd_group) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get intermediate group creation flag")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_create_intermediate_group() */

