/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Purpose: Abstract indexed (structured chunk) I/O functions.  The logical
 *          multi-dimensional dataspace is regularly partitioned into
 *          same-sized "chunks", the first of which is aligned with the
 *          logical origin.  The chunks are indexed by different methods,
 *          that map a chunk index to disk address.  Each chunk can be
 *          compressed independently and the chunks may move around in the
 *          file as their storage requirements change.
 *
 * Cache:   Disk I/O is performed in units of chunks and H5MF_alloc()
 *          contains code to optionally align chunks on disk block
 *          boundaries for performance.
 *
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h" /* This source code file is part of the H5D module */

/***********/
/* Headers */
/***********/
#include "H5private.h" /* Generic Functions            */
#ifdef H5_HAVE_PARALLEL
#include "H5ACprivate.h" /* Metadata cache            */
#endif                   /* H5_HAVE_PARALLEL */
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Dpkg.h"      /* Dataset functions            */
#include "H5SCprivate.h" /* Shared chunk cache functions            */
#include "H5Eprivate.h"  /* Error handling              */
#include "H5Fprivate.h"  /* File functions            */
#include "H5FLprivate.h" /* Free Lists                           */
#include "H5Iprivate.h"  /* IDs                      */
#include "H5MMprivate.h" /* Memory management            */
#include "H5MFprivate.h" /* File memory management               */
#include "H5PBprivate.h" /* Page Buffer	                         */
#include "H5SLprivate.h" /* Skip Lists                               */
#include "H5VMprivate.h" /* Vector and array functions        */

/****************/
/* Local Macros */
/****************/

/* Length of sequence lists requested from dataspace selections */
#define SEQ_LIST_LEN 128

/* Length of allocated arrays for building vector I/O operations */
#define VECTOR_LEN 8

/******************/
/* Local Typedefs */
/******************/

typedef struct H5D_struct_chunk_common_ud_t {
    const H5O_layout_struct_chunk_t  *layout;  /* Chunk layout description */
    const H5O_storage_struct_chunk_t *storage; /* Chunk storage description */
    const hsize_t             *scaled;  /* Scaled coordinates for a chunk */
} H5D_struct_chunk_common_ud_t;

typedef struct H5D_stc_ud_t {
    /* Downward */
    H5D_struct_chunk_common_ud_t common; /* Common info for B-tree user data (must be first) */

    hsize_t chunk_idx;      /* Chunk index for EA, FA indexing */
    haddr_t  addr;          /* Address of chunk */
    uint64_t nbytes;        /* Size of chunk (in file) */
    uint64_t *offset;       /* Array of offsets for n sections */
    uint64_t *unfilt_size;  /* Array of unfiltered size for n sections */
    uint32_t *filt_mask;    /* Array of filtered mask for n sections */
} H5D_stc_ud_t;

/* Idx info : H5Dpkg.h?? */
typedef struct H5D_stc_idx_info_t {
    H5F_t               *f;       /* File pointer for operation */
    const H5O_pline_t   *pline;   /* I/O pipeline info */
    H5O_layout_struct_chunk_t *layout;
    H5O_storage_struct_chunk_t *storage;
} H5D_stc_idx_info_t;


/* Intermediate struct for the chunk's memory cache format */
typedef struct H5D_chunk_mem_cache_t {
    void *data_buf;    /* Buffer pointer to the data values */
    void *sel_buf;      /* Buffer pointer to the encoded selection */
    H5S_t *sel_ds;      /* Dataspace for encoded selection */
    size_t sel_nbytes;  /* nbytes for selection */
    size_t sel_alloc_size;  /* alloc_size for selection */
    size_t data_nbytes; /* nbytes for data values */
    size_t data_alloc_size; /* alloc_size for data values */
} H5D_chunk_mem_cache_t;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5D__struct_chunk_lookup(H5D_t *dset, size_t count, hsize_t *scaled[] /*in*/, 
    haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/, hsize_t *defined_values_size[] /*out*/, 
    size_t *size_hint[] /*out*/, size_t *defined_values_size_hint[] /*out*/, 
    void **udata[] /*out*/);

static herr_t H5D__struct_chunk_decode(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/, 
    void **chunk /*in,out*/, void *udata);

static herr_t H5D__struct_chunk_decode_defined_values(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
    void **chunk /*in,out*/, void *udata);

static herr_t H5D__struct_chunk_new_chunk(H5D_t *dset, bool fill, size_t *nbytes /*out*/, size_t *buf_size /*out*/,
    void **chunk /*chunk*/);

static herr_t H5D__struct_chunk_vector_read(H5D_t *dset, haddr_t addr, H5S_t *file_space_in, void *chunk /*in*/, 
    size_t *vec_count /*out*/, haddr_t **offsets /*out*/, size_t **sizes /*out*/, 
    bool *vector_possible /*out*/, void *udata);

static herr_t H5D__struct_chunk_vector_write(H5D_t *dset, haddr_t addr, H5S_t *file_space_in, void *chunk /*in*/, 
    size_t *vec_count /*out*/, haddr_t **offsets /*out*/, size_t **sizes /*out*/, 
    bool *vector_possible /*out*/, void *udata);


/*********************/
/* Package Variables */
/*********************/

/* Shared Chunk Cache callbacks for legacy chunked storage layout */
const H5SC_layout_ops_t H5SC_LOPS_STRUCT_CHUNK[1] = {{
    H5D__struct_chunk_lookup,                   /* lookup */
    H5D__struct_chunk_decode,                   /* decode */
    H5D__struct_chunk_decode_defined_values,    /* decode_defined_values */
    H5D__struct_chunk_new_chunk,                /* new_chunk */
    NULL,           /* condense */
    NULL,           /* encode */
    NULL,           /* evict */
    NULL,           /* encode_in_place */
    NULL,           /* insert */
    NULL,                                       /* selection_read */
    H5D__struct_chunk_vector_read,              /* vector_read */
    NULL,                                       /* selection_write */
    H5D__struct_chunk_vector_write,             /* vector_write */
    NULL,           /* scatter_mem */
    NULL,           /* gather_mem */
    NULL,           /* fill */
    NULL,           /* defined_values */
    NULL,           /* erase_values */
    NULL,           /* evict_values */
    NULL            /* delete */
}};

/*******************/
/* Local Variables */
/*******************/



/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_lookup
 *
 * Purpose:     Looks up chunk address and size on disk. 
 *
 *              defined_values_size is the number of bytes to read if only 
 *              the list of defined values is needed. 
 *
 *              size_hint is the suggested allocation size for the chunk 
 *              (could be larger if the chunk might expand when decoded). 
 *             
 *              defined_values_size_hint is the suggested allocation size if only 
 *              the list of defined values is needed. 
 *              If *defined_values_size is returned as 0, then all values 
 *              are defined for the chunk. 
 *              In this case, the chunk may still be decoded without reading 
 *              from disk, by allocating a buffer of size defined_valued_size_hint and 
 *              passing it to H5SC_chunk_decode_t with *nbytes_used set to 0. 
 *
 *              *udata can be set to anything and will be passed through to 
 *              H5SC_chunk_decode_t and/or the selection or vector I/O routines, 
 *              then freed with free() 
 *              (we will create an H5SC_free_udata_t callback if necessary).
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5D__struct_chunk_lookup(H5D_t *dset, size_t count, hsize_t *scaled[] /*in*/, 
    haddr_t *addr[] /*out*/, hsize_t *size[] /*out*/, hsize_t *defined_values_size[] /*out*/, 
    size_t *size_hint[] /*out*/, size_t *defined_values_size_hint[] /*out*/, 
    void **_udata[] /*out*/)
{
    H5D_stc_ud_t *udata;
    H5O_storage_struct_chunk_t *stc  = &(dset->shared->layout.storage.u.struct_chunk);
    H5O_layout_struct_chunk_t *layout = &dset->shared->layout.u.struct_chunk;
    H5D_stc_idx_info_t idx_info;    /* Chunked index info */
    H5O_pline_t   *pline;           /* I/O pipeline info */
    hbool_t filtered = false;
    size_t tot_unfilt_size = 0;
    size_t i;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(dset->shared->layout.type == H5D_STRUCT_CHUNK);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused)
        filtered = true;

    /* Compose chunked index info struct */
    idx_info.f       = dset->oloc.file;
    idx_info.pline   = pline;
    idx_info.layout  = layout;
    idx_info.storage = stc;

    for (i = 0; i < count; i++) {

        /* Allocate udata */
        udata = (H5D_stc_ud_t *)H5MM_malloc(sizeof(H5D_stc_ud_t));
        if (udata == NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_CANTALLOC, FAIL, "could not malloc space for udata");

        /* Set up udata */
        udata->common.layout  = &(dset->shared->layout.u.struct_chunk);
        udata->common.storage = stc;
        udata->common.scaled  = scaled[i];

        /* Reset information about the chunk we are looking for */
        udata->addr = HADDR_UNDEF;
        udata->nbytes = 0;

        /* TBD: need to replace with idx_info and udata for structured chunk */
        {
            H5D_chunk_ud_t TMP_udata;
            H5D_chk_idx_info_t TMP_idx_info;

            /* chunk_idx is calculated in get_addr callback */
            if ((stc->ops->get_addr)(&TMP_idx_info, &TMP_udata) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query chunk address");
        }

    
        if (addr[i])
            *addr[i] = udata->addr;
        if (size[i])
            *size[i] = udata->nbytes;

        if (filtered)
            /* For now: assume two sections for fixed data */
            tot_unfilt_size = udata->unfilt_size[0] + udata->unfilt_size[1];

        if (size_hint[i])
            *size_hint[i] = filtered ? tot_unfilt_size : *size[i];

        /* Size of defined values */
        if (defined_values_size[i])
            *defined_values_size[i] = filtered ? udata->unfilt_size[0] : udata->offset[1];

        if (defined_values_size_hint[i])
            *defined_values_size_hint[i] = filtered ? udata->unfilt_size[0] : *defined_values_size[i];

        if (_udata[i])
            *_udata[i] = udata;

    } /* end count */

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5D__struct_chunk_lookup() */


/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_decode
 *
 * Purpose:    Decompresses/decodes the chunk from file format to memory cache format if necessary. 
 *             Reallocs chunk buffer if necessary. 
 *
 *             On entry, nbytes is the number of bytes used in the chunk buffer. 
 *             On exit, it shall be set to the total number of bytes used (not allocated) 
 *             across all buffers for this chunk. 
 *
 *             On entry, alloc_size is the size of the chunk buffer. 
 *             On exit, it shall be set to the total number of bytes allocated across all 
 *             buffers for this chunk. 
 *
 *             Optional, if not present, chunk is the same in cache as on disk.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] contains the pointer to the on disk file format chunk buffer
 *                 [udata] is the pointer to the udata returned from chunk_lookup()
 *       On exit: [chunk] contains pointer to the chunk intermediate struct
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_decode(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
    void **chunk /*in,out*/, void *_udata)
{
    H5D_stc_ud_t *udata = (H5D_stc_ud_t *)_udata;

    H5D_chunk_mem_cache_t *chk; /* Chunk's intermediate struct */

    size_t sel_nbytes = 0;       /* nbytes for encoded selection */
    size_t sel_alloc_size = 0;   /* alloc_size for encoded selection */
    void *sel_buf = NULL;        /* Buffer pointer to encoded selection */

    size_t data_nbytes = 0;     /* nbytes for data values */
    size_t data_alloc_size = 0; /* alloc_size for data values */
    void *data_buf = NULL;      /* Buffer pointer to data values */

    H5S_t *sel_ds = NULL;    /* The dataspace for the encoded selection */

    H5O_pline_t   *pline;           /* I/O pipeline info */
    hbool_t filtered = false;
    H5Z_EDC_t err_detect;       /* Error detection info */
    H5Z_cb_t  filter_cb;        /* I/O filter callback function */

    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused)
        filtered = true;

    /* nbytes and alloc_size for encoded selection */
    sel_nbytes = udata->offset[1];
    sel_alloc_size = filtered ? udata->unfilt_size[0] : sel_nbytes;

    /* nbytes and alloc_size for data values */
    data_nbytes = *nbytes - sel_nbytes;
    data_alloc_size = *alloc_size - sel_alloc_size;

    /* Allocate a buffer for the encoded selection */
    if (NULL == (sel_buf = H5D__chunk_mem_alloc(sel_alloc_size, pline)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "memory allocation failed for encoded selection buffer");

    /* Copy over the encoded selection */
    H5MM_memcpy(sel_buf, *chunk, sel_nbytes);

    /* Allocate a buffer for the data values */
    if (NULL == (data_buf = H5D__chunk_mem_alloc(data_alloc_size, pline)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "memory allocation failed for data buffer");

    /* Copy over the data values */
    H5MM_memcpy(data_buf, (uint8_t *)(*chunk) + sel_nbytes, data_nbytes);

    /* Decompress the encoded selection */
    if (filtered) {

        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

        /* Decompress the encoded selection */
        if (H5Z_pipeline(pline, H5Z_FLAG_REVERSE, &(udata->filt_mask[0]), err_detect,
                         filter_cb, &sel_nbytes, &sel_alloc_size, &sel_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "data pipeline read failed");

        /* Decompress the data values */
        if (H5Z_pipeline(pline, H5Z_FLAG_REVERSE, &(udata->filt_mask[1]), err_detect,
                         filter_cb, &data_nbytes, &data_alloc_size, &data_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "data pipeline read failed");
    }

    /* Decode the encoded selecton to dataspace sel_ds */
    if (NULL == (sel_ds = H5S_decode((const unsigned char **)&sel_buf)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

    /* Allocate the chunk's intermediate struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_mem_cache_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for intermediate chunk struct");

    /* Set up info in intermediate chunk struct */
    chk->data_buf = data_buf;   /* Data buffer */
    chk->sel_buf = sel_buf;     /* Selection buffer */

    chk->sel_ds = sel_ds;       /* H5S_t for encoded selection */
    chk->sel_nbytes = sel_nbytes;
    chk->sel_alloc_size = sel_nbytes;
    chk->data_nbytes = data_nbytes;
    chk->data_alloc_size = data_nbytes;

    /* Return values on exit */
    *nbytes = chk->sel_nbytes + chk->data_nbytes;
    *alloc_size = *nbytes;
    *chunk = chk;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_decode_defined_values
 *
 * Purpose:     The same as H5SC_chunk_decode_t but only decodes the defined values. 
 *
 *              Optional, if not present, all values are defined.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On entry: [chunk] contains pointer to the on disk file format chunk buffer
 *                 [udata] is the pointer to the udata returned from chunk_lookup()
 *       On exit: [chunk] contains pointer to the chunk intermediate struct
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_decode_defined_values(H5D_t *dset, size_t *nbytes /*in,out*/, size_t *alloc_size /*in,out*/,
    void **chunk /*in,out*/, void *_udata)
{
    H5D_stc_ud_t *udata = (H5D_stc_ud_t *)_udata;
    H5D_chunk_mem_cache_t *chk;   /* Chunk's intermediate struct */

    size_t sel_nbytes = 0;       /* nbytes for encoded selection */
    size_t sel_alloc_size = 0;   /* alloc_size for encoded selection */
    void *sel_buf = NULL;        /* Buffer pointer to encoded selection */
    H5S_t *sel_ds = NULL;    /* The dataspace for the encoded selection */

    H5O_pline_t   *pline;           /* I/O pipeline info */
    hbool_t filtered = false;

    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused)
        filtered = true;

    /* nbytes and alloc_size for encoded selection */
    sel_nbytes = udata->offset[1];
    sel_alloc_size = filtered ? udata->unfilt_size[0] : sel_nbytes;

    /* Allocate a buffer for the encoded selection */
    if (NULL == (sel_buf = H5D__chunk_mem_alloc(sel_alloc_size, pline)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL,
                    "memory allocation failed for raw data chunk");

    /* Copy over the encoded selection */
    H5MM_memcpy(sel_buf, *chunk, sel_nbytes);

    /* Decompress the encoded selection */
    if (pline && pline->nused) {
        H5Z_EDC_t err_detect;       /* Error detection info */
        H5Z_cb_t  filter_cb;        /* I/O filter callback function */

        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info");
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function");

        if (H5Z_pipeline(pline, H5Z_FLAG_REVERSE, &(udata->filt_mask[0]), err_detect,
                         filter_cb, &sel_nbytes, &sel_alloc_size, &sel_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "data pipeline read failed");
    }

    /* Decode the encoded selecton to dataspace select_ds */
    if (NULL == (sel_ds = H5S_decode((const unsigned char **)&sel_buf)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, FAIL, "unable to decode dataspace");

    /* Allocate the intermediate chunk struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_mem_cache_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for chunk's memory cache info");

    /* Set up info in the intermediate chunk struct */
    chk->sel_buf = sel_buf; /* Buffer pointer for encoded selection */
    chk->sel_ds = sel_ds;   /* H5S_t for encoded selection */
    chk->sel_nbytes = sel_nbytes;
    chk->sel_alloc_size = sel_alloc_size;

    *nbytes = chk->sel_nbytes;
    *alloc_size = chk->sel_alloc_size;
    *chunk = chk;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_decode_defined_values() */

/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_new_chunk
 *
 * Purpose:    Creates a new empty chunk. 
 *             Does not insert into on disk chunk index. 
 *
 *             If fill is true, writes the fill value to the chunk 
 *             (unless this is a sparse chunk). 
 *
 *             The number of bytes used is returned in *nbytes 
 *             The size of the chunk buffer is returned in *buf_size
 *
 * Return:    Non-negative on success/Negative on failure
 *
 * NOTE: On exit: [chunk] contains pointer to the chunk intermediate struct
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_new_chunk(H5D_t *dset, bool fill, size_t *nbytes /*out*/, size_t *buf_size /*out*/, 
    void **chunk /*out*/)
{
    H5D_chunk_mem_cache_t *chk;          /* Chunk's intermediate struct */

    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(dset->shared->layout.u.struct_chunk.stc_type == H5D_SPARSE_CHUNK);

    /* Allocate the chunk's intermediate struct */
    if (NULL == (chk = H5MM_malloc(sizeof(H5D_chunk_mem_cache_t))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for intermediate chunk struct");

    chk->data_buf = NULL;

    chk->sel_buf = NULL;
    chk->sel_ds = H5S_SEL_NONE;

    chk->sel_nbytes = 0;
    chk->sel_alloc_size = 0;
    chk->data_nbytes = 0;
    chk->data_alloc_size = 0;

    *nbytes = *buf_size = 0;  
    *chunk = chk;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__struct_chunk_new_chunk() */


/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_vector_read
 *
 * Purpose:     For when the chunk cache wants to read data directly from the 
 *              disk to the user buffer, using vector I/O. 
 *              If not possible due to compression, etc, returns vector_possible=false. 
 *              Otherwise returns the vector of selected elements in offsets 
 *              (within the file, not the chunk, this is why addr is passed in) 
 *              and sizes, with the number of vectors returned in vec_count. 
 *
 *              chunk may be passed as NULL, and may also be an in-cache chunk 
 *              that only contains information on selected elements. 
 *
 *              Optional, if not present, chunk I/O is only performed on entire chunks 
 *              or with selection I/O. 
 *              The H5SC code checks for type conversion before calling this.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_vector_read(H5D_t *dset, haddr_t addr, H5S_t *file_space_in, void *chunk /*in*/, 
    size_t *vec_count /*out*/, haddr_t **offsets /*out*/, size_t **sizes /*out*/, 
    bool *vector_possible /*out*/, void *udata)
{
    H5O_pline_t *pline;     /* I/O pipeline info */
    size_t          elmt_size = 0;
    haddr_t        *vec_addrs = NULL;
    size_t         *vec_sizes = NULL;
    hsize_t         file_off[SEQ_LIST_LEN];
    size_t          file_len[SEQ_LIST_LEN];
    size_t          file_seq_i;
    size_t          file_nseq;
    size_t          io_len;
    size_t          nelmts;
    hssize_t        hss_nelmts;
    size_t          seq_nelem;
    H5S_sel_iter_t *file_iter      = NULL;
    bool            file_iter_init = false;
    size_t vec_arr_nused = 0;
    size_t vec_arr_nalloc = VECTOR_LEN;
    herr_t          ret_value      = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(file_space_in);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused) {
        *vector_possible = false;
        HGOTO_DONE(SUCCEED);
    }
    *vector_possible = true;

    if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate file selection iterator");

    if (0 == (elmt_size = H5T_get_size(dset->shared->type)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    /* Initialize sequence lists for file space */
    if (H5S_select_iter_init(file_iter, file_space_in, elmt_size, 0) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space");
    file_iter_init = true;

    /* Get the number of elements in selection */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_space_in)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(nelmts, size_t, hss_nelmts, hssize_t);

    /* Initialize values so sequence lists are retrieved on the first
     * iteration */
    file_seq_i = SEQ_LIST_LEN;
    file_nseq  = 0;

    if (NULL == (vec_addrs = H5MM_malloc(VECTOR_LEN * sizeof(haddr_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector addrs");

    if (NULL == (vec_sizes = H5MM_malloc(VECTOR_LEN * sizeof(size_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector sizes");

    /* Loop until all elements are processed */
    while (file_seq_i < file_nseq || nelmts > 0) {
        /* Fill/refill file sequence list if necessary */
        if (file_seq_i == SEQ_LIST_LEN) {
            if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                             &seq_nelem, file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");
            assert(file_nseq > 0);

            nelmts -= seq_nelem;
            file_seq_i = 0;
        }
        assert(file_seq_i < file_nseq);

        /* Calculate length of this IO */
        io_len = file_len[file_seq_i];

        if (vec_arr_nused == vec_arr_nalloc) {
            void *tmp_ptr;

            /* Reallocate arrays */
            if (NULL == (tmp_ptr = H5MM_realloc(vec_addrs, vec_arr_nalloc * 2 * sizeof(*vec_addrs) )))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory reallocation failed for address list");
            vec_addrs = tmp_ptr;

            if (NULL == (vec_sizes = H5MM_realloc(vec_sizes, vec_arr_nalloc * 2 * sizeof(*vec_sizes) * 2)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory reallocation failed for size list");
            vec_sizes = tmp_ptr;

            /* Record that we've doubled the array sizes */
            vec_arr_nalloc *= 2; 
        }

        /* Add this segment to vector read list */
        vec_addrs[vec_arr_nused]    = addr + file_off[file_seq_i];
        vec_sizes[vec_arr_nused]    = io_len;

        vec_arr_nused++;

        /* Update file sequence */
        if (io_len == file_len[file_seq_i])
            file_seq_i++;
        else {
            file_off[file_seq_i] += io_len;
            file_len[file_seq_i] -= io_len;
        }
    }

    *vec_count = vec_arr_nused;
    *offsets = vec_addrs;
    *sizes = vec_sizes;

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator");
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_vector_read() */

                 
/*-------------------------------------------------------------------------
 * Function:    H5D__struct_chunk_vector_write
 *
 * Purpose:     For when the chunk cache wants to write data directly from the 
 *              user buffer to the cache. 
 *              If not possible due to compression, etc, returns vector_possible=false. 
 *              Otherwise returns the vector of selected elements in offsets 
 *              (within the file, not the chunk, this is why addr is passed in) 
 *              and sizes, with the number of vectors returned in vec_count. 
 *
 *              chunk may be passed as NULL, and may also be an in-cache chunk 
 *              that only contains information on selected elements. 
 *
 *              Optional, if not present, chunk I/O is only performed on entire chunks 
 *              or with selection I/O. 
 *              The H5SC code checks for type conversion before calling this.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__struct_chunk_vector_write(H5D_t *dset, haddr_t addr, H5S_t *file_space_in, void *chunk /*in*/, 
    size_t *vec_count /*out*/, haddr_t **offsets /*out*/, size_t **sizes /*out*/, 
    bool *vector_possible /*out*/, void *udata)
{
    size_t          elmt_size = 0;
    haddr_t        *vec_addrs = NULL;
    size_t         *vec_sizes = NULL;
    hsize_t         file_off[SEQ_LIST_LEN];
    size_t          file_len[SEQ_LIST_LEN];
    size_t          file_seq_i;
    size_t          file_nseq;
    size_t          io_len;
    size_t          nelmts;
    hssize_t        hss_nelmts;
    size_t          seq_nelem;
    H5S_sel_iter_t *file_iter      = NULL;
    bool            file_iter_init = false;
    size_t vec_arr_nused = 0;
    size_t vec_arr_nalloc = VECTOR_LEN;
    H5O_pline_t *pline;     /* I/O pipeline info */
    herr_t          ret_value      = SUCCEED;

    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    assert(dset);
    assert(file_space_in);

    pline = &(dset->shared->dcpl_cache.pline);
    if (pline && pline->nused) {
        *vector_possible = false;
        HGOTO_DONE(SUCCEED);
    }
    *vector_possible = true;

    if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_VFL, H5E_CANTALLOC, FAIL, "couldn't allocate file selection iterator");

    /* Create selection iterator for memory selection */
    if (0 == (elmt_size = H5T_get_size(dset->shared->type)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_BADSIZE, FAIL, "datatype size invalid");

    /* Initialize sequence lists for memory and file spaces */
    if (H5S_select_iter_init(file_iter, file_space_in, elmt_size, 0) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't initialize sequence list for file space");
    file_iter_init = true;

    /* Get the number of elements in selection */
    if ((hss_nelmts = (hssize_t)H5S_GET_SELECT_NPOINTS(file_space_in)) < 0)
        HGOTO_ERROR(H5E_VFL, H5E_CANTCOUNT, FAIL, "can't get number of elements selected");
    H5_CHECKED_ASSIGN(nelmts, size_t, hss_nelmts, hssize_t);

    /* Initialize values so sequence lists are retrieved on the first
     * iteration */
    file_seq_i = SEQ_LIST_LEN;
    file_nseq  = 0;

    if (NULL == (vec_addrs = H5MM_malloc(VECTOR_LEN * sizeof(haddr_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector addrs");

    if (NULL == (vec_sizes = H5MM_malloc(VECTOR_LEN * sizeof(size_t *))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "memory allocation failed for vector sizes");


    /* Loop until all elements are processed */
    while (file_seq_i < file_nseq || nelmts > 0) {
        /* Fill/refill file sequence list if necessary */
        if (file_seq_i == SEQ_LIST_LEN) {
            if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, SEQ_LIST_LEN, SIZE_MAX, &file_nseq,
                                             &seq_nelem, file_off, file_len) < 0)
                HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed");
            assert(file_nseq > 0);

            nelmts -= seq_nelem;
            file_seq_i = 0;
        }
        assert(file_seq_i < file_nseq);

        /* Calculate length of this IO */
        io_len = file_len[file_seq_i];

        if (vec_arr_nused == vec_arr_nalloc) {
            void *tmp_ptr;

            /* Reallocate arrays */
            if (NULL == (tmp_ptr = H5MM_realloc(vec_addrs, vec_arr_nalloc * 2 * sizeof(*vec_addrs) )))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory reallocation failed for address list");
            vec_addrs = tmp_ptr;

            if (NULL == (vec_sizes = H5MM_realloc(vec_sizes, vec_arr_nalloc * 2 * sizeof(*vec_sizes) * 2)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory reallocation failed for size list");
            vec_sizes = tmp_ptr;

            /* Record that we've doubled the array sizes */
            vec_arr_nalloc *= 2; 
        }

        /* Add this segment to vector read list */
        vec_addrs[vec_arr_nused]    = addr + file_off[file_seq_i];
        vec_sizes[vec_arr_nused]    = io_len;

        vec_arr_nused++;

        /* Update file sequence */
        if (io_len == file_len[file_seq_i])
            file_seq_i++;
        else {
            file_off[file_seq_i] += io_len;
            file_len[file_seq_i] -= io_len;
        }
    }

    *vec_count = vec_arr_nused;
    *offsets = vec_addrs;
    *sizes = vec_sizes;

done:
    /* Terminate and free iterators */
    if (file_iter) {
        if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_CANTFREE, FAIL, "can't release file selection iterator");
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    }

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5D__struct_chunk_vector_write() */
