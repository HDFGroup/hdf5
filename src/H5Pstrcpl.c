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
#include "H5Ppkg.h"		/* Property lists		  	*/

/* Local datatypes */

/* Static function prototypes */

#ifdef H5_GROUP_REVISION
/*-------------------------------------------------------------------------
 * Function:  H5Pset_char_encoding
 *
 * Purpose:   Sets the character encoding of the string.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * Programmer:        James Laird
 *            Wednesday, October 26, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_char_encoding(hid_t plist_id, H5T_cset_t encoding)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value=SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pset_char_encoding, FAIL);
    H5TRACE2("e","iTc",plist_id,encoding);

    /* Check arguments */
    if (encoding <= H5T_CSET_ERROR || encoding >= H5T_NCSET)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "character encoding is not valid")

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id,H5P_STRING_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set the character encoding */
    if(H5P_set(plist, H5P_CHAR_ENCODING_NAME, &encoding) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set character encoding")

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5P_set_char_encoding() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_char_encoding
 *
 * Purpose:     Gets the character encoding of the string.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  James Laird
 *              November 1, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_char_encoding(hid_t plist_id, H5T_cset_t *encoding /*out*/)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(H5Pget_char_encoding, FAIL);
    H5TRACE2("e","ix",plist_id,encoding);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_STRING_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get value */
    if(encoding)
        if(H5P_get(plist, H5P_CHAR_ENCODING_NAME, encoding) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get character encoding flag")

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Pget_char_encoding() */

#endif /* H5_GROUP_REVISION */

