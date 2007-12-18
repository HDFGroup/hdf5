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

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE             /*suppress error about including H5Dpkg   */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Vprivate.h"		/* Vector and array functions		*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/*********************/
/* Package Variables */
/*********************/

/*******************/
/* Local Variables */
/*******************/

/* Declare extern the free list to manage blocks of type conversion data */
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
    H5D_fill_buf_info_t fb_info;        /* Dataset's fill buffer info */
    hbool_t     fb_info_init = FALSE;   /* Whether the fill value buffer has been initialized */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_fill, FAIL)

    /* Check args */
    HDassert(TRUE == H5P_isa_class(dxpl_id, H5P_DATASET_XFER));
    HDassert(dset && H5D_COMPACT == dset->shared->layout.type);
    HDassert(dset->shared->layout.u.compact.buf);
    HDassert(dset->shared->type);
    HDassert(dset->shared->space);

    /* Initialize the fill value buffer */
    /* (use the compact dataset storage buffer as the fill value buffer) */
    if(H5D_fill_init(&fb_info, dset->shared->layout.u.compact.buf, FALSE,
            NULL, NULL, NULL, NULL,
            &dset->shared->dcpl_cache.fill, dset->shared->type,
            dset->shared->type_id, (size_t)0, dset->shared->layout.u.compact.size, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize fill buffer info")
    fb_info_init = TRUE;

    /* Check for VL datatype & non-default fill value */
    if(fb_info.has_vlen_fill_type)
        /* Fill the buffer with VL datatype fill values */
        if(H5D_fill_refill_vl(&fb_info, fb_info.elmts_per_buf, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "can't refill fill value buffer")

done:
    /* Release the fill buffer info, if it's been initialized */
    if(fb_info_init && H5D_fill_term(&fb_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release fill buffer info")

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
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_compact_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, void *buf)
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
 *-------------------------------------------------------------------------
 */
ssize_t
H5D_compact_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[],
    haddr_t UNUSED addr, void UNUSED *pointer/*in*/, const void *buf)
{
    ssize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_writevv, FAIL)

    assert(io_info->dset);

    /* Use the vectorized memory copy routine to do actual work */
    if((ret_value=H5V_memcpyvv(io_info->dset->shared->layout.u.compact.buf,dset_max_nseq,dset_curr_seq,dset_size_arr,dset_offset_arr,buf,mem_max_nseq,mem_curr_seq,mem_size_arr,mem_offset_arr))<0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vectorized memcpy failed")

    /* Mark the compact dataset's buffer as dirty */
    io_info->dset->shared->layout.u.compact.dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D_compact_writevv() */


/*-------------------------------------------------------------------------
 * Function:    H5D_compact_copy
 *
 * Purpose:     Copy compact storage raw data from SRC file to DST file.
 *
 * Return:      Non-negative on success, negative on failure.
 *
 * Programmer:  Peter Cao
 *              December 11, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_compact_copy(H5F_t *f_src, H5O_layout_t *layout_src, H5F_t *f_dst, 
    H5O_layout_t *layout_dst, H5T_t *dt_src, H5O_copy_t *cpy_info, hid_t dxpl_id)
{
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    hid_t       tid_mem = -1;           /* Datatype ID for memory datatype */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *bkg = NULL;             /* Temporary buffer for copying data */
    void       *reclaim_buf = NULL;     /* Buffer for reclaiming data */
    hid_t       buf_sid = -1;           /* ID for buffer dataspace */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_copy, FAIL)

    /* Check args */
    HDassert(layout_src && H5D_COMPACT == layout_src->type);
    HDassert(f_src);
    HDassert(f_dst);
    HDassert(layout_dst && H5D_COMPACT == layout_dst->type);
    HDassert(dt_src);

    /* Create datatype ID for src datatype, so it gets freed */
    if((tid_src = H5I_register(H5I_DATATYPE, dt_src)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register source file datatype")

    /* If there's a VLEN source datatype, do type conversion information */
    if(H5T_detect_class(dt_src, H5T_VLEN) > 0) {
        H5T_path_t  *tpath_src_mem, *tpath_mem_dst;   /* Datatype conversion paths */
        H5T_t *dt_dst;              /* Destination datatype */
        H5T_t *dt_mem;              /* Memory datatype */
        H5S_t *buf_space;           /* Dataspace describing buffer */
        size_t buf_size;            /* Size of copy buffer */
        size_t nelmts;              /* Number of elements in buffer */
        size_t src_dt_size;         /* Source datatype size */
        size_t tmp_dt_size;         /* Temporary datatype size */
        size_t max_dt_size;         /* Max atatype size */
        hsize_t buf_dim;            /* Dimension for buffer */

        /* create a memory copy of the variable-length datatype */
        if(NULL == (dt_mem = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if((tid_mem = H5I_register(H5I_DATATYPE, dt_mem)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register memory datatype")

        /* create variable-length datatype at the destinaton file */
        if(NULL == (dt_dst = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if(H5T_set_loc(dt_dst, f_dst, H5T_LOC_DISK) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "cannot mark datatype on disk")
        if((tid_dst = H5I_register(H5I_DATATYPE, dt_dst)) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register destination file datatype")

        /* Set up the conversion functions */
        if(NULL == (tpath_src_mem = H5T_path_find(dt_src, dt_mem, NULL, NULL, dxpl_id, FALSE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and mem datatypes")
        if(NULL == (tpath_mem_dst = H5T_path_find(dt_mem, dt_dst, NULL, NULL, dxpl_id, FALSE)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between mem and dst datatypes")

        /* Determine largest datatype size */
        if(0 == (src_dt_size = H5T_get_size(dt_src)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        if(0 == (tmp_dt_size = H5T_get_size(dt_mem)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(src_dt_size, tmp_dt_size);
        if(0 == (tmp_dt_size = H5T_get_size(dt_dst)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(max_dt_size, tmp_dt_size);

        /* Set number of whole elements that fit in buffer */
        if(0 == (nelmts = layout_src->u.compact.size / src_dt_size))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "element size too large")

        /* Set up number of bytes to copy, and initial buffer size */
        buf_size = nelmts * max_dt_size;

        /* Create dataspace for number of elements in buffer */
        buf_dim = nelmts;

        /* Create the space and set the initial extent */
        if(NULL == (buf_space = H5S_create_simple((unsigned)1, &buf_dim, NULL)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace")

        /* Atomize */
        if((buf_sid = H5I_register(H5I_DATASPACE, buf_space)) < 0) {
            H5S_close(buf_space);
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")
        } /* end if */

        /* Allocate memory for recclaim buf */
        if(NULL == (reclaim_buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Allocate memory for copying the chunk */
        if(NULL == (buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        HDmemcpy(buf, layout_src->u.compact.buf, layout_src->u.compact.size);

        /* Convert from source file to memory */
        if(H5T_convert(tpath_src_mem, tid_src, tid_mem, nelmts, (size_t)0, (size_t)0, buf, NULL, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        HDmemcpy(reclaim_buf, buf, buf_size);

        /* allocate temporary bkg buff for data conversion */
        if(NULL == (bkg = H5FL_BLK_CALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Convert from memory to destination file */
        if(H5T_convert(tpath_mem_dst, tid_mem, tid_dst, nelmts, (size_t)0, (size_t)0, buf, bkg, dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        HDmemcpy(layout_dst->u.compact.buf, buf, layout_dst->u.compact.size);

        if(H5D_vlen_reclaim(tid_mem, buf_space, H5P_DATASET_XFER_DEFAULT, reclaim_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADITER, FAIL, "unable to reclaim variable-length data")
    } /* end if */
    else if(H5T_get_class(dt_src, FALSE) == H5T_REFERENCE) {
        if(f_src != f_dst) {
            /* Check for expanding references */
            if(cpy_info->expand_ref) {
                size_t ref_count;

                /* Determine # of reference elements to copy */
                ref_count = layout_src->u.compact.size / H5T_get_size(dt_src);

                /* Copy objects referenced in source buffer to destination file and set destination elements */
                if(H5O_copy_expand_ref(f_src, layout_src->u.compact.buf, dxpl_id, f_dst,
                        layout_dst->u.compact.buf, ref_count, H5T_get_ref_type(dt_src), cpy_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "unable to copy reference attribute")
            } /* end if */
            else
                /* Reset value to zero */
                HDmemset(layout_dst->u.compact.buf, 0, layout_src->u.compact.size);
        } /* end if */
        else
            /* Type conversion not necessary */
            HDmemcpy(layout_dst->u.compact.buf, layout_src->u.compact.buf, layout_src->u.compact.size);
    } /* end if */
    else 
        /* Type conversion not necessary */
        HDmemcpy(layout_dst->u.compact.buf, layout_src->u.compact.buf, layout_src->u.compact.size);

done:
    if(buf_sid > 0)
        if(H5I_dec_ref(buf_sid) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary dataspace ID")
    if(tid_src > 0)
        if(H5I_dec_ref(tid_src) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_dst > 0)
        if(H5I_dec_ref(tid_dst) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_mem > 0)
        if(H5I_dec_ref(tid_mem) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(buf)
        H5FL_BLK_FREE(type_conv, buf);
    if(reclaim_buf)
        H5FL_BLK_FREE(type_conv, reclaim_buf);
    if(bkg)
        H5FL_BLK_FREE(type_conv, bkg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_compact_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5D_compact_copy_conv
 *
 * Purpose:	Copies compact data from SRC dataset to DST dataset
 *              and does data conversion.  This function is similar to
 *              H5D_compact_copy and is mainly used by H5Dmodify_dtype.  
 *
 * Return:	Non-negative on success. 
 *		Negative on failure.
 *
 * Programmer:  Raymond Lu
 *	        8 October 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D_compact_copy_conv(const H5D_t *dset_src, const H5D_t *dset_dst, 
    H5O_copy_t UNUSED *cpy_info, hid_t dxpl_id)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    H5T_path_t  *tpath = NULL;          /* Datatype conversion paths */
    H5T_t       *dt_src = NULL;         /* Source datatype */
    H5T_t       *dt_dst = NULL;         /* Destination datatype */
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    size_t      src_dt_size = 0;        /* Source datatype size */
    size_t      dst_dt_size = 0;        /* Destination datatype size */
    size_t      max_dt_size;            /* Max. datatype size */
    hsize_t     nelmts = 0;             /* Number of elements in buffer */
    hssize_t    snelmts = 0;            /* Number of elements in buffer */
    hsize_t     total_nbytes;           /* Total number of bytes to copy */
    hsize_t     total_src_nbytes;       /* Total number of bytes of the source */
    hsize_t     total_dst_nbytes;       /* Total number of bytes of the destination */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *bkg = NULL;             /* Temporary buffer for copying data */
    hbool_t     is_vlen = FALSE;        /* Flag to indicate VL type */
    hbool_t     vlen_conv = TRUE;       /* Transfer property to indicate no conversion for vlen */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5D_compact_copy_conv, FAIL)

    /* Check args */
    HDassert(dset_src && H5D_COMPACT == dset_src->shared->layout.type);
    HDassert(dset_dst && H5D_COMPACT == dset_dst->shared->layout.type);

    /* dataset pointers and IDs */
    dt_src = dset_src->shared->type;
    dt_dst = dset_dst->shared->type;
    tid_src = dset_src->shared->type_id;
    tid_dst = dset_dst->shared->type_id;

    /* Set up number of bytes to copy, and initial buffer size */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(dset_src->shared->space)) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "src dataspace has invalid selection")
    H5_ASSIGN_OVERFLOW(nelmts,snelmts,hssize_t,hsize_t);

    if(nelmts==0)
        HGOTO_DONE(SUCCEED)

    /* Compute element sizes and other parameters */
    src_dt_size = H5T_get_size(dt_src);
    dst_dt_size = H5T_get_size(dt_dst);
    max_dt_size = MAX(src_dt_size, dst_dt_size);

    total_src_nbytes = nelmts*src_dt_size;
    total_dst_nbytes = nelmts*dst_dt_size;
    total_nbytes = MAX(total_src_nbytes, total_dst_nbytes);

    /* If the datatype is or contains vlen, set the property to indicate no conversion 
     * is needed. */
    if((is_vlen = H5T_detect_class(dt_src, H5T_VLEN)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to detect dtatypes")

    if(is_vlen ) {
        vlen_conv = FALSE;
        if(NULL == (plist = H5P_object_verify(dxpl_id, H5P_DATASET_XFER)))
            HGOTO_ERROR(H5E_PLIST, H5E_BADATOM, FAIL, "can't find object for ID")

        if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
    }

    /* Set up the conversion functions */
    if(NULL == (tpath = H5T_path_find(dt_src, dt_dst, NULL, NULL, dxpl_id, FALSE)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to convert between src and mem datatypes")

    /* Allocate memory for copying the chunk */
    if(NULL == (buf = H5FL_BLK_MALLOC(type_conv, (size_t)total_nbytes)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* allocate temporary bkg buff for data conversion */
    if(NULL == (bkg = H5FL_BLK_CALLOC(type_conv, (size_t)total_nbytes)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Copy the data into the conversion buffer */
    HDmemcpy(buf, dset_src->shared->layout.u.compact.buf, (size_t)total_src_nbytes);

    /* Convert from source file to destination file */
    if(H5T_convert(tpath, tid_src, tid_dst, (size_t)nelmts, (size_t)0, (size_t)0, buf, bkg, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

    /* Set the property of vlen conversion back to normal */
    if(is_vlen ) {
        vlen_conv = TRUE;

        if(H5P_set(plist, H5D_XFER_VLEN_CONV_NAME, &vlen_conv) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "Error setting vlen conv flag")
    }

    /* Copy the data from the conversion buffer to the destination */
    dset_dst->shared->layout.u.compact.size = total_dst_nbytes;
    H5MM_xfree(dset_dst->shared->layout.u.compact.buf);
    dset_dst->shared->layout.u.compact.buf = (void*)H5MM_malloc((size_t)total_dst_nbytes);
    HDmemcpy(dset_dst->shared->layout.u.compact.buf, buf, (size_t)total_dst_nbytes);

done:
    if(buf)
        H5FL_BLK_FREE(type_conv, buf);
    if(bkg)
        H5FL_BLK_FREE(type_conv, bkg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_compact_copy_conv() */
