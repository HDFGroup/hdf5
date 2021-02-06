/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* High-level library internal header file */
#include "H5HLprivate2.h"

/* public LT prototypes			*/
#include "H5DOpublic.h"

/*-------------------------------------------------------------------------
 * Function:    H5DOwrite_chunk
 *
 * Purpose:     Writes an entire chunk to the file directly.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5DOwrite_chunk(hid_t dset_id, hid_t dxpl_id, uint32_t filters, const hsize_t *offset, size_t data_size,
                const void *buf)
{
    hbool_t  created_dxpl    = FALSE; /* Whether we created a DXPL */
    hbool_t  do_direct_write = TRUE;  /* Flag for direct writes */
    uint32_t data_size_32;            /* Chunk data size (limited to 32-bits currently) */
    herr_t   ret_value = FAIL;        /* Return value */

    /* Check arguments */
    if (dset_id < 0)
        goto done;
    if (!buf)
        goto done;
    if (!offset)
        goto done;
    if (!data_size)
        goto done;
    data_size_32 = (uint32_t)data_size;
    if (data_size != (size_t)data_size_32)
        goto done;

    /* If the user passed in a default DXPL, create one to pass to H5Dwrite() */
    if (H5P_DEFAULT == dxpl_id) {
        if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
            goto done;
        created_dxpl = TRUE;
    } /* end if */

    /* Set direct write parameters */
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &do_direct_write) < 0)
        goto done;
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FILTERS_NAME, &filters) < 0)
        goto done;
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_OFFSET_NAME, &offset) < 0)
        goto done;
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_DATASIZE_NAME, &data_size_32) < 0)
        goto done;

    /* Write chunk */
    if (H5Dwrite(dset_id, 0, H5S_ALL, H5S_ALL, dxpl_id, buf) < 0)
        goto done;

    /* Indicate success */
    ret_value = SUCCEED;

done:
    if (created_dxpl) {
        if (H5Pclose(dxpl_id) < 0)
            ret_value = FAIL;
    } /* end if */
    else {
        /* Reset the direct write flag on user DXPL */
        do_direct_write = FALSE;
        if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_WRITE_FLAG_NAME, &do_direct_write) < 0)
            ret_value = FAIL;
    }

    return (ret_value);
} /* end H5DOwrite_chunk() */

/*-------------------------------------------------------------------------
 * Function:    H5DOread_chunk
 *
 * Purpose:     Reads an entire chunk from the file directly.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *---------------------------------------------------------------------------
 */
herr_t
H5DOread_chunk(hid_t dset_id, hid_t dxpl_id, const hsize_t *offset, uint32_t *filters, void *buf)
{
    hbool_t created_dxpl   = FALSE; /* Whether we created a DXPL */
    hbool_t do_direct_read = TRUE;  /* Flag for direct writes */
    herr_t  ret_value      = FAIL;  /* Return value */

    /* Check arguments */
    if (dset_id < 0)
        goto done;
    if (!buf)
        goto done;
    if (!offset)
        goto done;
    if (!filters)
        goto done;

    /* If the user passed in a default DXPL, create one to pass to H5Dwrite() */
    if (H5P_DEFAULT == dxpl_id) {
        if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
            goto done;
        created_dxpl = TRUE;
    } /* end if */

    /* Set direct write parameters */
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_READ_FLAG_NAME, &do_direct_read) < 0)
        goto done;
    if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_READ_OFFSET_NAME, &offset) < 0)
        goto done;

    /* Read chunk */
    if (H5Dread(dset_id, 0, H5S_ALL, H5S_ALL, dxpl_id, buf) < 0)
        goto done;
    /* Get the filter mask */
    if (H5Pget(dxpl_id, H5D_XFER_DIRECT_CHUNK_READ_FILTERS_NAME, filters) < 0)
        goto done;

    /* Indicate success */
    ret_value = SUCCEED;

done:
    if (created_dxpl) {
        if (H5Pclose(dxpl_id) < 0)
            ret_value = FAIL;
    } /* end if */
    else {
        /* Reset the direct read flag on user DXPL */
        do_direct_read = FALSE;
        if (H5Pset(dxpl_id, H5D_XFER_DIRECT_CHUNK_READ_FLAG_NAME, &do_direct_read) < 0)
            ret_value = FAIL;
    }

    return (ret_value);
} /* end H5DOread_chunk() */
