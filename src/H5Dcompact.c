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
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              August 5, 2002
 *
 * Purpose:     Compact dataset I/O functions.  These routines are similar
 *              H5D_contig_* and H5D_istore_*.
 */

#define H5D_PACKAGE             /*suppress error about including H5Dpkg   */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

/* Declare a free list to manage blocks of type conversion data */
H5FL_BLK_EXTERN(type_conv);


/*-------------------------------------------------------------------------
 * Function:	H5D_compact_fill
 *
 * Purpose:	Write fill values to a compactly stored dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		May 6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_compact_fill(H5D_t *dset, hid_t dxpl_id)
{
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_fill, FAIL)

    /* Check args */
    HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
    HDassert(dset && H5D_COMPACT == dset->shared->layout.type);
    HDassert(dset->shared->layout.u.compact.buf);
    HDassert(dset->shared->type);
    HDassert(dset->shared->space);

    /* If the fill value is defined, initialize the data buffer with it */
    if(dset->shared->fill.buf) {
        hssize_t snpoints;      /* Number of points in space (for error checking) */
        size_t npoints;         /* Number of points in space */

        /* Get the number of elements in the dataset's dataspace */
        snpoints = H5S_GET_EXTENT_NPOINTS(dset->shared->space);
        HDassert(snpoints >= 0);
        H5_ASSIGN_OVERFLOW(npoints, snpoints, hssize_t, size_t);

        /* If necessary, convert fill value datatypes (which copies VL components, etc.) */
        if(H5T_detect_class(dset->shared->type, H5T_VLEN) > 0) {
            H5T_path_t *tpath;      /* Datatype conversion path */
            uint8_t *bkg_buf = NULL;    /* Background conversion buffer */
            H5T_t *mem_type;            /* Pointer to memory datatype */
            size_t mem_type_size, file_type_size;       /* Size of datatype in memory and on disk */
            hid_t mem_tid;              /* Memory version of disk datatype */

            /* Create temporary datatype for conversion operation */
            if(NULL == (mem_type = H5T_copy(dset->shared->type, H5T_COPY_REOPEN)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, FAIL, "unable to copy file datatype")
            if((mem_tid = H5I_register(H5I_DATATYPE, mem_type)) < 0) {
                H5T_close(mem_type);
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register memory datatype")
            } /* end if */

            /* Retrieve sizes of memory & file datatypes */
            mem_type_size = H5T_get_size(mem_type);
            HDassert(mem_type_size > 0);
            file_type_size = H5T_get_size(dset->shared->type);
            HDassert(file_type_size == dset->shared->fill.size);

            /* Get the datatype conversion path for this operation */
            if(NULL == (tpath = H5T_path_find(dset->shared->type, mem_type, NULL, NULL, dxpl_id))) {
                H5I_dec_ref(mem_tid);
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and dst datatypes")
            } /* end if */

            /* Allocate a background buffer, if necessary */
            if(H5T_path_bkg(tpath) && NULL == (bkg_buf = H5FL_BLK_CALLOC(type_conv, (npoints * MAX(mem_type_size, file_type_size))))) {
                H5I_dec_ref(mem_tid);
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
            } /* end if */

            /* Make a copy of the (disk-based) fill value into the compact buffer */
            HDmemcpy(dset->shared->layout.u.compact.buf, dset->shared->fill.buf, file_type_size);

            /* Type convert the compact dataset buffer, to copy any VL components */
            if(H5T_convert(tpath, dset->shared->type_id, mem_tid, (size_t)1, (size_t)0, (size_t)0, dset->shared->layout.u.compact.buf, bkg_buf, dxpl_id) < 0) {
                if(bkg_buf)
                    H5FL_BLK_FREE(type_conv, bkg_buf);
                H5I_dec_ref(mem_tid);
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed")
            } /* end if */

            /* Replicate the fill value into the cached buffer */
            H5V_array_fill(dset->shared->layout.u.compact.buf, dset->shared->layout.u.compact.buf, mem_type_size, npoints);

            /* Get the inverse datatype conversion path for this operation */
            if(NULL == (tpath = H5T_path_find(mem_type, dset->shared->type, NULL, NULL, dxpl_id))) {
                if(bkg_buf)
                    H5FL_BLK_FREE(type_conv, bkg_buf);
                H5I_dec_ref(mem_tid);
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and dst datatypes")
            } /* end if */

            /* Allocate or reset the background buffer, if necessary */
            if(H5T_path_bkg(tpath)) {
                if(bkg_buf)
                    HDmemset(bkg_buf, 0, MAX(mem_type_size, file_type_size));
                else {
                    if(NULL == (bkg_buf = H5FL_BLK_CALLOC(type_conv, (npoints * MAX(mem_type_size, file_type_size))))) {
                        H5I_dec_ref(mem_tid);
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
                    } /* end if */
                } /* end else */
            } /* end if */

            /* Type convert the compact dataset buffer, to copy any VL components */
            if(H5T_convert(tpath, mem_tid, dset->shared->type_id, npoints, (size_t)0, (size_t)0, dset->shared->layout.u.compact.buf, bkg_buf, dxpl_id) < 0) {
                if(bkg_buf)
                    H5FL_BLK_FREE(type_conv, bkg_buf);
                H5I_dec_ref(mem_tid);
                HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "data type conversion failed")
            } /* end if */

            /* Release resources used */
            if(bkg_buf)
                H5FL_BLK_FREE(type_conv, bkg_buf);
            H5I_dec_ref(mem_tid);
        } /* end if */
        else
            /* Replicate the fill value into the cached buffer */
            H5V_array_fill(dset->shared->layout.u.compact.buf, dset->shared->fill.buf, dset->shared->fill.size, npoints);

    } /* end if */
    else /* If the fill value is default, zero set data buf. */
        HDmemset(dset->shared->layout.u.compact.buf, 0, dset->shared->layout.u.compact.size);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_compact_fill() */


/*-------------------------------------------------------------------------
 * Function:    H5D_compact_readvv
 *
 * Purpose:     Reads some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative
 *              to the beginning address of the dataset.  The offsets and
 *              sequence lengths are in bytes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 7, 2003
 *
 * Notes:
 *              Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_compact_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    void *buf)
{
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_readvv, FAIL)

    assert(io_info->dset);

    /* Use the vectorized memory copy routine to do actual work */
    if((ret_value=H5V_memcpyvv(buf,mem_max_nseq,mem_curr_seq,mem_size_arr,mem_offset_arr,io_info->dset->shared->layout.u.compact.buf,dset_max_nseq,dset_curr_seq,dset_size_arr,dset_offset_arr))<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vectorized memcpy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_compact_readvv() */


/*-------------------------------------------------------------------------
 * Function:    H5D_compact_writevv
 *
 * Purpose:     Writes some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative
 *              to the beginning address for the file.  The offsets and
 *              sequence lengths are in bytes.  This function only copies
 *              data into the buffer in the LAYOUT struct and mark it
 *              as DIRTY.  Later in H5D_close, the data is copied into
 *              header message in memory.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 2, 2003
 *
 * Notes:
 *              Offsets in the sequences must be monotonically increasing
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_compact_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    const void *buf)
{
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_writevv, FAIL)

    assert(io_info->dset);

    /* Use the vectorized memory copy routine to do actual work */
    if((ret_value=H5V_memcpyvv(io_info->dset->shared->layout.u.compact.buf,dset_max_nseq,dset_curr_seq,dset_size_arr,dset_offset_arr,buf,mem_max_nseq,mem_curr_seq,mem_size_arr,mem_offset_arr))<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vectorized memcpy failed")

    io_info->dset->shared->layout.u.compact.dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_compact_writevv() */
