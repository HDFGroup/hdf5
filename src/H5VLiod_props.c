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

/*
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              June, 2014
 *
 * Purpose:	The IOD VOL plugin property list functions.
 */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FDprivate.h"        /* file drivers            		*/
#include "H5FFprivate.h"        /* Fast Forward            		*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5Ppkg.h"		/* Property lists			*/
#include "H5VLiod.h"            /* Iod VOL plugin			*/

#ifdef H5_HAVE_EFF


/*-------------------------------------------------------------------------
 * Function:	H5Pset_ocpl_enable_checksum
 *
 * Purpose:     Set a boolean flag on the object creation property list 
 *              to indicate to the VOL plugin to enable checksum on the 
 *              object to be created.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_ocpl_enable_checksum(hid_t ocpl_id, hbool_t flag)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", ocpl_id, flag);

    if(ocpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(ocpl_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a ocpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5O_CRT_ENABLE_CHECKSUM_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_ocpl_enable_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_ocpl_enable_checksum
 *
 * Purpose:     Retrieve a boolean flag on the object creation property 
 *              list that indicates whether checksuming on this object 
 *              is enabled or not.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              January, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_ocpl_enable_checksum(hid_t ocpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", ocpl_id, flag);

    if(NULL == (plist = H5P_object_verify(ocpl_id, H5P_OBJECT_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a ocpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5O_CRT_ENABLE_CHECKSUM_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_ocpl_enable_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_replica
 *
 * Purpose:     Set the replica ID to be used when accessing an object 
 *              using this transfer plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_replica(hid_t dxpl_id, hrpl_t replica_id)
{
    H5P_genplist_t *plist = NULL;    /* Property list pointer */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iRp", dxpl_id, replica_id);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5O_XFER_REPLICA_ID_NAME, &replica_id) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_replica() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_replica
 *
 * Purpose:     Retrieve the replica ID from this access plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              February, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_replica(hid_t dxpl_id, hrpl_t *replica_id/*out*/)
{
    H5P_genplist_t *plist = NULL;       /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, replica_id);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(replica_id)
        if(H5P_get(plist, H5O_XFER_REPLICA_ID_NAME, replica_id) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_replica() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_prefetch_layout
 *
 * Purpose:     Set the prefetch layout to be used when accessing an object 
 *              using this transfer plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_prefetch_layout(hid_t dxpl_id, H5FF_layout_t layout)
{
    H5P_genplist_t *plist = NULL;    /* Property list pointer */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5O_XFER_LAYOUT_TYPE_NAME, &layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_prefetch_layout() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_prefetch_layout
 *
 * Purpose:     Retrieve the prefetch layout from this access plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_prefetch_layout(hid_t dxpl_id, H5FF_layout_t *layout/*out*/)
{
    H5P_genplist_t *plist = NULL;       /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, layout);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(layout)
        if(H5P_get(plist, H5O_XFER_LAYOUT_TYPE_NAME, layout) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_prefetch_layout() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_prefetch_selection
 *
 * Purpose:     Set the prefetch selection to be used when accessing an object 
 *              using this transfer plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_prefetch_selection(hid_t dxpl_id, hid_t selection)
{
    H5P_genplist_t *plist = NULL;    /* Property list pointer */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", dxpl_id, selection);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5O_XFER_SELECTION_NAME, &selection) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_prefetch_selection() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_prefetch_range
 *
 * Purpose:     Set the prefetch range to be used when accessing an object 
 *              using this transfer plist.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              May, 2014
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_prefetch_range(hid_t dxpl_id, hid_t keymem_type, const void *low_key, const void *high_key)
{
    H5P_genplist_t *plist = NULL;    /* Property list pointer */
    herr_t ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "ii*x*x", dxpl_id, keymem_type, low_key, high_key);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5O_XFER_KEY_TYPE_NAME, &keymem_type) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
    if(H5P_set(plist, H5O_XFER_LOW_KEY_BUF_NAME, &low_key) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
    if(H5P_set(plist, H5O_XFER_HIGH_KEY_BUF_NAME, &high_key) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_prefetch_range() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum
 *
 * Purpose:     Modify the dataset transfer property list to set a
 *              checksum value for the data to be transfered. 
 *              This is used with write operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum(hid_t dxpl_id, uint64_t cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIl", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum
 *
 * Purpose:     Retrieve the checksum value that was set using 
 *              H5Pset_dxpl_checksum.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum(hid_t dxpl_id, uint64_t *cs/*out*/)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_NAME, cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_checksum_ptr
 *
 * Purpose:     Set a pointer to tell the library where to insert the
 *              checksum that is received from a remote location. 
 *              This is used with read operations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t *cs)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Il", dxpl_id, cs);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_CHECKSUM_PTR_NAME, &cs) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_checksum_ptr
 *
 * Purpose:     Retrieve the checksum pointer value that was set using 
 *              H5Pset_dxpl_checksum_ptr.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t **cs/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, cs);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(cs)
        if(H5P_get(plist, H5D_XFER_CHECKSUM_PTR_NAME, *cs) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get checksum_ptr value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_checksum_ptr() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_metadata_integrity_scope
 *
 * Purpose: Set the scope of checksum generation and verification for
 * metadata in the FF stack. This is a file access property so the
 * property is set on a particular container. Changing the property
 * would require closing the file and reopening it. Possible values
 * for this property are:
 * H5_CHECKSUM_NONE      = No metadata checksuming and verification 
                           is done at any part of the stack.
 * H5_CHECKSUM_TRANSFER  = Metadata is verified after being transfered 
                           through Mercury.
 * H5_CHECKSUM_IOD       = Metadata is checksumed and the checksum is 
                           given to IOD when written, and verified when read.
 * H5_CHECKSUM_MEMORY    = Metadata is verified when moved in memory 
                           (Not currently supported).
 * H5_CHECKSUM_ALL       = Metadata is checksumed and verified on all levels.
 *
 * Note that the property value is a bitflag so any combination can be
 * set for individual values using OR operation.  
 *
 * Return: Non-negative
 * on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_metadata_integrity_scope(hid_t fapl_id, uint32_t scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", fapl_id, scope);

    if(scope > H5_CHECKSUM_ALL)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid scope for Data Integrity");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set property */
    if(H5P_set(plist, H5VL_CS_BITFLAG_NAME, &scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data integrity scope");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_metadata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_metadata_integrity_scope
 *
 * Purpose:	Get the current bit flag indicating the data integrity scope.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_metadata_integrity_scope(hid_t fapl_id, uint32_t *scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", fapl_id, scope);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(scope) {
        /* Get property */
        if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, scope) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_metadata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_rawdata_integrity_scope
 *
 * Purpose: Set the scope of checksum generation and verification for
 * rawdata in the FF stack. This is a data transfer property so the
 * property is set on a particular I/O operation (H5Dread/write, 
 * H5Mset/get, etc ...). Possible values for this property are:
 * H5_CHECKSUM_NONE      = No checksuming and verification 
                           is done at any part of the stack.
 * H5_CHECKSUM_TRANSFER  = Data is verified after being transfered 
                           through Mercury.
 * H5_CHECKSUM_IOD       = Data is checksumed and the checksum is 
                           given to IOD when written, and verified when read.
 * H5_CHECKSUM_MEMORY    = Data is verified when moved in memory 
 * H5_CHECKSUM_ALL       = Data is checksumed and verified on all levels.
 *
 * Note that the property value is a bitflag so any combination can be
 * set for individual values using OR operation. 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_rawdata_integrity_scope(hid_t dxpl_id, uint32_t scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iIu", dxpl_id, scope);

    if(scope > H5_CHECKSUM_ALL)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "Invalid scope for Data Integrity");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set property */
    if(H5P_set(plist, H5VL_CS_BITFLAG_NAME, &scope) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data integrity scope");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_rawdata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_rawdata_integrity_scope
 *
 * Purpose:	Get the current bit flag indicating the data integrity scope.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              September 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_rawdata_integrity_scope(hid_t dxpl_id, uint32_t *scope)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", dxpl_id, scope);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    if(scope) {
        /* Get property */
        if(H5P_get(plist, H5VL_CS_BITFLAG_NAME, scope) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get scope for data integrity checks");
    }

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_rawdata_integrity_scope() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to set a boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dxpl_inject_corruption(hid_t dxpl_id, hbool_t flag)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dxpl_id, flag);

    if(dxpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_XFER_INJECT_CORRUPTION_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dxpl_inject_corruption
 *
 * Purpose:     Temporary routine to retrieve the boolean flag that tells the 
 *              library to inject corruption in the stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dxpl_inject_corruption(hid_t dxpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dxpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dxpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_XFER_INJECT_CORRUPTION_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dxpl_inject_corruption() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_dcpl_append_only
 *
 * Purpose:     Set a boolean flag on the dataset creation property list 
 *              to indicate to the VOL plugin that access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_dcpl_append_only(hid_t dcpl_id, hbool_t flag)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ib", dcpl_id, flag);

    if(dcpl_id == H5P_DEFAULT)
        HGOTO_ERROR(H5E_PLIST, H5E_BADVALUE, FAIL, "can't set values in default property list")

    /* Check arguments */
    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Set the transfer mode */
    if(H5P_set(plist, H5D_CRT_APPEND_ONLY_NAME, &flag) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_dcpl_append_only() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_dcpl_append_only
 *
 * Purpose:     Retrieve a boolean flag on the dataset creation property list 
 *              that indicates whether access to this dataset 
 *              will always be in an append/sequence only manner.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mohamad Chaarawi
 *              June, 2013
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_dcpl_append_only(hid_t dcpl_id, hbool_t *flag/*out*/)
{
    H5P_genplist_t *plist = NULL;              /* Property list pointer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ix", dcpl_id, flag);

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    /* Get the transfer mode */
    if(flag)
        if(H5P_get(plist, H5D_CRT_APPEND_ONLY_NAME, flag) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_dcpl_append_only() */

herr_t 
H5Pset_dcpl_dim_layout(hid_t dcpl_id, H5FF_dset_dim_layout_t layout)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(H5P_set(plist, H5D_CRT_DIM_ORDER_NAME, &layout) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
done:
    FUNC_LEAVE_API(ret_value)
}

herr_t 
H5Pget_dcpl_dim_layout(hid_t dcpl_id, H5FF_dset_dim_layout_t *layout)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(layout)
        if(H5P_get(plist, H5D_CRT_DIM_ORDER_NAME, layout) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
done:
    FUNC_LEAVE_API(ret_value)
}

herr_t 
H5Pset_dcpl_stripe_count(hid_t dcpl_id, size_t stripe_count)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(H5P_set(plist, H5D_CRT_STRIPE_COUNT_NAME, &stripe_count) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
done:
    FUNC_LEAVE_API(ret_value)
}

herr_t 
H5Pget_dcpl_stripe_count(hid_t dcpl_id, size_t *stripe_count)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(stripe_count)
        if(H5P_get(plist, H5D_CRT_STRIPE_COUNT_NAME, stripe_count) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
done:
    FUNC_LEAVE_API(ret_value)
}

herr_t 
H5Pset_dcpl_stripe_size(hid_t dcpl_id, size_t stripe_size)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(H5P_set(plist, H5D_CRT_STRIPE_SIZE_NAME, &stripe_size) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to set value")
done:
    FUNC_LEAVE_API(ret_value)
}

herr_t 
H5Pget_dcpl_stripe_size(hid_t dcpl_id, size_t *stripe_size)
{
    H5P_genplist_t *plist = NULL;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)

    if(NULL == (plist = H5P_object_verify(dcpl_id, H5P_DATASET_CREATE)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "not a dcpl")

    if(stripe_size)
        if(H5P_get(plist, H5D_CRT_STRIPE_SIZE_NAME, stripe_size) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "unable to get value")
done:
    FUNC_LEAVE_API(ret_value)
}

#endif /* H5_HAVE_EFF */
