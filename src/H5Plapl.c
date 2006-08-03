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
#include "H5Lprivate.h"		/* Links		  		*/
#include "H5Ppkg.h"		/* Property lists		  	*/

/* Local datatypes */

/* Static function prototypes */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_nlinks
 *
 * Purpose:     Set the number of soft or UD link traversals allowed before
 *              the library assumes it has found a cycle and aborts the
 *              traversal.
 *
 *              The limit on soft or UD link traversals is designed to
 *              terminate link traversal if one or more links form a cycle.
 *              However, users may have a file with a legitimate path
 *              formed of a large number of soft or user-defined links.
 *              This property can be used to allow traversal of as many
 *              links as desired.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_nlinks(hid_t plist_id, size_t nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_nlinks, FAIL)
    H5TRACE2("e","iz",plist_id,nlinks);

    if(nlinks <= 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "number of links must be positive");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set number of links */
    if(H5P_set(plist, H5L_NLINKS_NAME, &nlinks) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set nlink info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_nlinks() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_nlinks
 *
 * Purpose:	Gets the number of soft or user-defined links that can be
 *              traversed before a failure occurs.
 *
 *              Retrieves the current setting for the nlinks property on
 *              the given property list.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Friday, July 14, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_nlinks(hid_t plist_id, size_t *nlinks)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pget_nlinks, FAIL)
    H5TRACE2("e","i*z",plist_id,nlinks);

    if(!nlinks)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer passed in");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current number of links */
    if(H5P_get(plist, H5L_NLINKS_NAME, nlinks) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get number of links")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Pset_elink_prefix
 *
 * Purpose:     Set a prefix to be applied to the path of any external links
 *              traversed.  The prefix is appended to the filename stored
 *              in the external link.
 * 
 *              The prefix is supplied by giving a pointer to a user-
 *              allocated buffer.  This buffer should not be freed
 *              until this property list has been closed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_elink_prefix(hid_t plist_id, const char *prefix)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_elink_prefix, FAIL)
    H5TRACE2("e","iz",plist_id,nlinks);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set prefix */
    if(H5P_set(plist, H5L_ELINK_PREFIX_NAME, &prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set prefix info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_elink_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_elink_prefix
 *
 * Purpose:	Gets the prefix to be applied to any external link
 *              traversals made using this property list.
 *
 *              If the pointer is not NULL, it points to a user-allocated
 *              buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	James Laird
 *              Thursday, August 3, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_elink_prefix(hid_t plist_id, char **prefix)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pget_elink_prefix, FAIL)
    H5TRACE2("e","i*z",plist_id,nlinks);

    if(!prefix)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid pointer passed in");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_LINK_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current prefix */
    if(H5P_get(plist, H5L_ELINK_PREFIX_NAME, prefix) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get external link prefix")

done:
    FUNC_LEAVE_API(ret_value)
}

