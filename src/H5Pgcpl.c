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

#ifdef H5_GROUP_REVISION

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
    H5TRACE2("e","iz",plist_id,size_hint);

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
    H5TRACE2("e","ix",plist_id,size_hint);

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


/*-------------------------------------------------------------------------
 * Function:    H5Pset_link_phase_change
 *
 * Purpose:     Set the maximum # of links to store "compactly" and the
 *              minimum # of links to store "densely".  (These should
 *              overlap).
 *
 * Note:        Currently both of these must be updated at the same time.
 *
 * Note:        Come up with better name & description! -QAK
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 29, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_link_phase_change(hid_t plist_id, unsigned max_compact, unsigned min_dense)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5O_ginfo_t ginfo;                  /* Group information structure */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_link_phase_change, FAIL)
    H5TRACE3("e","iIuIu",plist_id,max_compact,min_dense);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get group info */
    if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

    /* Update fields */
    ginfo.max_compact = max_compact;
    ginfo.min_dense = min_dense;

    /* Set group info */
    if(H5P_set(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set group info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_link_phase_change() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_link_phase_change
 *
 * Purpose:     Returns the max. # of compact links & the min. # of dense
 *              links, which are used for storing groups
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 29, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_link_phase_change(hid_t plist_id, unsigned *max_compact /*out*/, unsigned *min_dense /*out*/)
{
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_link_phase_change, FAIL)
    H5TRACE3("e","ixx",plist_id,max_compact,min_dense);

    /* Get values */
    if(max_compact || min_dense) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5O_ginfo_t ginfo;          /* Group information structure */

        /* Get the plist structure */
        if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Get group info */
        if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

        if(max_compact)
            *max_compact = ginfo.max_compact;
        if(min_dense)
            *min_dense = ginfo.min_dense;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_link_phase_change() */


/*-------------------------------------------------------------------------
 * Function:    H5Pset_est_link_info
 *
 * Purpose:     Set the estimates for the number of entries and length of each
 *              entry name in a group.
 *
 * Note:        Currently both of these must be updated at the same time.
 *
 * Note:        EST_NUM_ENTRIES applies only when the number of entries is less
 *              than the MAX_COMPACT # of entries (from H5Pset_link_phase_change).
 *
 * Note:        Come up with better name & description? -QAK
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September  6, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_est_link_info(hid_t plist_id, unsigned est_num_entries, unsigned est_name_len)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5O_ginfo_t ginfo;                  /* Group information structure */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(H5Pset_est_link_info, FAIL)
    H5TRACE3("e","iIuIu",plist_id,est_num_entries,est_name_len);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get group info */
    if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

    /* Update fields */
    ginfo.est_num_entries = est_num_entries;
    ginfo.est_name_len = est_name_len;

    /* Set group info */
    if(H5P_set(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set group info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_est_link_info() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_est_link_info
 *
 * Purpose:     Returns the est. # of links in a group & the est. length of
 *              the name of each link.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              September  6, 2005
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_est_link_info(hid_t plist_id, unsigned *est_num_entries /*out*/, unsigned *est_name_len /*out*/)
{
    herr_t ret_value = SUCCEED;   /* return value */

    FUNC_ENTER_API(H5Pget_est_link_info, FAIL)
    H5TRACE3("e","ixx",plist_id,est_num_entries,est_name_len);

    /* Get values */
    if(est_num_entries || est_name_len) {
        H5P_genplist_t *plist;      /* Property list pointer */
        H5O_ginfo_t ginfo;          /* Group information structure */

        /* Get the plist structure */
        if(NULL == (plist = H5P_object_verify(plist_id, H5P_GROUP_CREATE)))
            HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

        /* Get group info */
        if(H5P_get(plist, H5G_CRT_GROUP_INFO_NAME, &ginfo) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get group info")

        if(est_num_entries)
            *est_num_entries = ginfo.est_num_entries;
        if(est_name_len)
            *est_name_len = ginfo.est_name_len;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_est_link_info() */


#endif /* H5_GROUP_REVISION */

