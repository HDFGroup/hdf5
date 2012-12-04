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

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "H5DOprivate.h"

/*-------------------------------------------------------------------------
 * Function:	H5DOwrite_chunk
 *
 * Purpose:     Writes an entire chunk to the file directly.	
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		30 July 2012
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
herr_t
H5DOwrite_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset, 
         size_t data_size, const void *buf)
{
    hbool_t created_dxpl = FALSE;
    herr_t  ret_value = SUCCEED;  /* Return value */
    
    if(dset_id < 0) {
        ret_value = FAIL;
        goto done;
    }

    if(!buf) {
        ret_value = FAIL;
        goto done;
    }

    if(!offset) {
        ret_value = FAIL;
        goto done;
    }

    if(!data_size) {
        ret_value = FAIL;
        goto done;
    }

    if(H5P_DEFAULT == dxpl_id) {
	if((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        created_dxpl = TRUE;
    }

    if(H5DO_write_chunk(dset_id, dxpl_id, filters, offset, data_size, buf) < 0) {
        ret_value = FAIL;
        goto done;
    }

done:
    if(created_dxpl) {
        if(H5Pclose(dxpl_id) < 0)
            ret_value = FAIL;
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:	H5DO_write_chunk
 *
 * Purpose:     Private function for H5DOwrite_chunk
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *		30 July 2012
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
herr_t
H5DO_write_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset, 
         size_t data_size, const void *buf)
{
    hbool_t do_direct_write = TRUE;
    herr_t  ret_value = SUCCEED;  /* Return value */

    if(H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &do_direct_write) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if(H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME, &filters) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if(H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME, &offset) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if(H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME, &data_size) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if(H5Dwrite(dset_id, 0, H5S_ALL, H5S_ALL, dxpl_id, buf) < 0) {
        ret_value = FAIL;
        goto done;
    }

done:
    do_direct_write = FALSE;
    if(H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &do_direct_write) < 0)
        ret_value = FAIL;

    return ret_value;
}
