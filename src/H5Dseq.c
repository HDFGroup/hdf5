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

/*
 * Programmer: 	Quincey Koziol <koziol@ncsa.uiuc.edu>
 *	       	Thursday, September 28, 2000
 *
 * Purpose:	Provides I/O facilities for sequences of bytes stored with various 
 *      layout policies.  These routines are similar to the H5Farray.c routines,
 *      these deal in terms of byte offsets and lengths, not coordinates and
 *      hyperslab sizes.
 *
 */

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */

/* Pablo information */
/* (Put before include files to avoid problems with inline functions) */
#define PABLO_MASK	H5Dseq_mask

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File space management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Pprivate.h"         /* Property lists                       */
#include "H5Vprivate.h"		/* Vector and array functions		*/


/*-------------------------------------------------------------------------
 * Function:	H5D_seq_readvv
 *
 * Purpose:	Reads in a vector of byte sequences from a file dataset into a
 *      buffer in in memory.  The data is read from file F and the array's size
 *      and storage information is in LAYOUT.  External files are described
 *      according to the external file list, EFL.  The vector of byte sequences
 *      offsets is in the DSET_OFFSET array into the dataset (offsets are in
 *      terms of bytes) and the size of each sequence is in the SEQ_LEN array.
 *      The total size of the file array is implied in the LAYOUT argument.
 *      Bytes read into BUF are sequentially stored in the buffer, each sequence
 *      from the vector stored directly after the previous.  The number of
 *      sequences is NSEQ.
 * Purpose:	Reads a vector of byte sequences from a vector of byte
 *      sequences in a file dataset into a buffer in memory.  The data is
 *      read from file F and the array's size and storage information is in
 *      LAYOUT.  External files and chunks are described according to the
 *      storage information, STORE.  The vector of byte sequences offsets for
 *      the file is in the DSET_OFFSET_ARR array into the dataset (offsets are
 *      in terms of bytes) and the size of each sequence is in the DSET_LEN_ARR
 *      array.  The vector of byte sequences offsets for memory is in the
 *      MEM_OFFSET_ARR array into the dataset (offsets are in terms of bytes)
 *      and the size of each sequence is in the MEM_LEN_ARR array.  The total
 *      size of the file array is implied in the LAYOUT argument.  The maximum
 *      number of sequences in the file dataset and the memory buffer are
 *      DSET_MAX_NSEQ & MEM_MAX_NSEQ respectively.  The current sequence being
 *      operated on in the file dataset and the memory buffer are DSET_CURR_SEQ
 *      & MEM_CURR_SEQ respectively.  The current sequence being operated on
 *      will be updated as a result of the operation, as will the offsets and
 *      lengths of the file dataset and memory buffer sequences.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 7, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_seq_readvv(H5F_t *f, const struct H5D_dxpl_cache_t *dxpl_cache, hid_t dxpl_id,
    H5D_t *dset, const H5D_storage_t *store, 
    size_t dset_max_nseq, size_t *dset_curr_seq,  size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    void *buf/*out*/)
{
    ssize_t ret_value;            /* Return value */
   
    FUNC_ENTER_NOAPI(H5D_seq_readvv, FAIL);

    /* Check args */
    assert(f);
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER)); /* Make certain we have the correct type of property list */
    assert(dset);
    assert(dset_curr_seq);
    assert(*dset_curr_seq<dset_max_nseq);
    assert(dset_len_arr);
    assert(dset_offset_arr);
    assert(mem_curr_seq);
    assert(*mem_curr_seq<mem_max_nseq);
    assert(mem_len_arr);
    assert(mem_offset_arr);
    assert(buf);

    switch (dset->layout.type) {
        case H5D_CONTIGUOUS:
            /* Read directly from file if the dataset is in an external file */
            if (store && store->efl.nused>0) {
                /* Note: We can't use data sieve buffers for datasets in external files
                 *  because the 'addr' of all external files is set to 0 (above) and
                 *  all datasets in external files would alias to the same set of
                 *  file offsets, totally mixing up the data sieve buffer information. -QAK
                 */
                if((ret_value=H5O_efl_readvv(&(store->efl),
                        dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                        mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                        buf))<0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "external data read failed");
            } else {
                /* Pass along the vector of sequences to read */
                if((ret_value=H5D_contig_readvv(f, dxpl_id, dset,
                        dset->layout.u.contig.addr, dset->layout.u.contig.size,
                        dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                        mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                        buf))<0)
                    HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "block read failed");
            } /* end else */
            break;

        case H5D_CHUNKED:
            assert(store);
            if((ret_value=H5D_istore_readvv(f, dxpl_cache, dxpl_id, dset, store,
                    dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                    mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                    buf))<0)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "istore read failed");
            break;

        case H5D_COMPACT:
            /* Pass along the vector of sequences to read */
            if((ret_value=H5D_compact_readvv(f, dxpl_id, dset,
                    dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                    mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                    buf))<0)
                HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "compact read failed");
            break;
                                                        
        default:
            assert("not implemented yet" && 0);
            HGOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "unsupported storage layout");
    }   /* end switch() */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* H5D_seq_readvv() */


/*-------------------------------------------------------------------------
 * Function:	H5D_seq_writevv
 *
 * Purpose:	Writes a vector of byte sequences from a buffer in memory into
 *      a vector of byte sequences in a file dataset.  The data is written to
 *      file F and the array's size and storage information is in LAYOUT.
 *      External files and chunks are described according to the storage
 *      information, STORE.  The vector of byte sequences offsets for the file
 *      is in the DSET_OFFSET_ARR array into the dataset (offsets are in
 *      terms of bytes) and the size of each sequence is in the DSET_LEN_ARR
 *      array.  The vector of byte sequences offsets for memory is in the
 *      MEM_OFFSET_ARR array into the dataset (offsets are in terms of bytes)
 *      and the size of each sequence is in the MEM_LEN_ARR array.  The total
 *      size of the file array is implied in the LAYOUT argument.  The maximum
 *      number of sequences in the file dataset and the memory buffer are
 *      DSET_MAX_NSEQ & MEM_MAX_NSEQ respectively.  The current sequence being
 *      operated on in the file dataset and the memory buffer are DSET_CURR_SEQ
 *      & MEM_CURR_SEQ respectively.  The current sequence being operated on
 *      will be updated as a result of the operation, as will the offsets and
 *      lengths of the file dataset and memory buffer sequences.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 2, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_seq_writevv(H5F_t *f, const struct H5D_dxpl_cache_t *dxpl_cache,
    hid_t dxpl_id, struct H5D_t *dset, const H5D_storage_t *store, 
    size_t dset_max_nseq, size_t *dset_curr_seq,  size_t dset_len_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_len_arr[], hsize_t mem_offset_arr[],
    const void *buf)
{
    ssize_t     ret_value;              /* Return value */
   
    FUNC_ENTER_NOAPI(H5D_seq_writevv, FAIL);

    /* Check args */
    assert(f);
    assert(TRUE==H5P_isa_class(dxpl_id,H5P_DATASET_XFER)); /* Make certain we have the correct type of property list */
    assert(dset);
    assert(dset_curr_seq);
    assert(*dset_curr_seq<dset_max_nseq);
    assert(dset_len_arr);
    assert(dset_offset_arr);
    assert(mem_curr_seq);
    assert(*mem_curr_seq<mem_max_nseq);
    assert(mem_len_arr);
    assert(mem_offset_arr);
    assert(buf);

    switch (dset->layout.type) {
        case H5D_CONTIGUOUS:
            /* Write directly to file if the dataset is in an external file */
            if (store && store->efl.nused>0) {
                /* Note: We can't use data sieve buffers for datasets in external files
                 *  because the 'addr' of all external files is set to 0 (above) and
                 *  all datasets in external files would alias to the same set of
                 *  file offsets, totally mixing up the data sieve buffer information. -QAK
                 */
                if ((ret_value=H5O_efl_writevv(&(store->efl),
                        dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                        mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                        buf))<0)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "external data write failed");
            } else {
                /* Pass along the vector of sequences to write */
                if ((ret_value=H5D_contig_writevv(f, dxpl_id, dset,
                        dset->layout.u.contig.addr, dset->layout.u.contig.size,
                        dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                        mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                        buf))<0)
                    HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "block write failed");
            } /* end else */
            break;

        case H5D_CHUNKED:
            assert(store);
            if((ret_value=H5D_istore_writevv(f, dxpl_cache, dxpl_id, dset, store,
                    dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                    mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                    buf))<0)
                HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "istore write failed");
            break;

        case H5D_COMPACT:       
            /* Pass along the vector of sequences to write */
            if((ret_value=H5D_compact_writevv(f, dxpl_id, dset,
                    dset_max_nseq, dset_curr_seq, dset_len_arr, dset_offset_arr,
                    mem_max_nseq, mem_curr_seq, mem_len_arr, mem_offset_arr,
                    buf))<0)
                 HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "compact write failed");
            break;

        default:
            assert("not implemented yet" && 0);
            HGOTO_ERROR(H5E_IO, H5E_UNSUPPORTED, FAIL, "unsupported storage layout");
    }   /* end switch() */

done:
    FUNC_LEAVE_NOAPI(ret_value);
}   /* H5D_seq_writevv() */
