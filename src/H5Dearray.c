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

/* Programmer: 	Quincey Koziol <koziol@hdfgroup.org>
 *	       	Tuesday, January 27, 2009
 *
 * Purpose:	Extensible array indexed (chunked) I/O functions.  The chunks
 *              are given a single-dimensional index which is used as the
 *              offset in an extensible array that maps a chunk coordinate to
 *              a disk address.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EAprivate.h"	/* Extensible arrays		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"	/* File space management		*/
#include "H5Vprivate.h"         /* Vector functions			*/


/****************/
/* Local Macros */
/****************/

/* Value to fill unset array elements with */
#define H5D_EARRAY_FILL         HADDR_UNDEF
#define H5D_EARRAY_FILT_FILL    {HADDR_UNDEF, 0, 0}

/* Extensible array creation values */
#define H5D_EARRAY_MAX_NELMTS_BITS         32                      /* i.e. 4 giga-elements */
#define H5D_EARRAY_IDX_BLK_ELMTS           4
#define H5D_EARRAY_SUP_BLK_MIN_DATA_PTRS   4
#define H5D_EARRAY_DATA_BLK_MIN_ELMTS      16
#define H5D_EARRAY_MAX_DBLOCK_PAGE_NELMTS_BITS     10              /* i.e. 1024 elements per data block page */


/******************/
/* Local Typedefs */
/******************/

/* Extensible array create/open user data */
typedef struct H5D_earray_ctx_ud_t {
    const H5F_t *f;             /* Pointer to file info */
    const H5O_layout_t *layout; /* Pointer to layout info */
} H5D_earray_ctx_ud_t;

/* Extensible array callback context */
typedef struct H5D_earray_ctx_t {
    size_t file_addr_len;       /* Size of addresses in the file (bytes) */
    size_t chunk_size_len;      /* Size of chunk sizes in the file (bytes) */
} H5D_earray_ctx_t;

/* User data for chunk callbacks */
typedef struct H5D_earray_ud_t {
    H5F_t *f;                   /* File pointer for operation */
    hid_t dxpl_id;              /* DXPL ID for operation */
} H5D_earray_ud_t;

/* Native extensible array element for chunks w/filters */
typedef struct H5D_earray_filt_elmt_t {
    haddr_t addr;               /* Address of chunk */
    uint32_t nbytes;            /* Size of chunk (in file) */
    uint32_t filter_mask;       /* Excluded filters for chunk */
} H5D_earray_filt_elmt_t;


/********************/
/* Local Prototypes */
/********************/

/* Extensible array class callbacks for chunks w/o filters */
static void *H5D_earray_crt_context(void *udata);
static herr_t H5D_earray_dst_context(void *ctx);
static herr_t H5D_earray_fill(void *nat_blk, size_t nelmts);
static herr_t H5D_earray_encode(void *raw, const void *elmt, size_t nelmts,
    void *ctx);
static herr_t H5D_earray_decode(const void *raw, void *elmt, size_t nelmts,
    void *ctx);
static herr_t H5D_earray_debug(FILE *stream, int indent, int fwidth,
    hsize_t idx, const void *elmt);

/* Extensible array class callbacks for chunks w/filters */
/* (some shared with callbacks for chunks w/o filters) */
static herr_t H5D_earray_filt_fill(void *nat_blk, size_t nelmts);
static herr_t H5D_earray_filt_encode(void *raw, const void *elmt, size_t nelmts,
    void *ctx);
static herr_t H5D_earray_filt_decode(const void *raw, void *elmt, size_t nelmts,
    void *ctx);
static herr_t H5D_earray_filt_debug(FILE *stream, int indent, int fwidth,
    hsize_t idx, const void *elmt);

/* Chunked layout indexing callbacks */
static herr_t H5D_earray_idx_init(const H5D_chk_idx_info_t *idx_info,
    const H5S_t *space, haddr_t dset_ohdr_addr);
static herr_t H5D_earray_idx_create(const H5D_chk_idx_info_t *idx_info);
static hbool_t H5D_earray_idx_is_space_alloc(const H5O_layout_t *layout);
static herr_t H5D_earray_idx_insert(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static herr_t H5D_earray_idx_get_addr(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static herr_t H5D_earray_idx_resize(H5O_layout_t *layout);
static int H5D_earray_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata);
static herr_t H5D_earray_idx_remove(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata);
static herr_t H5D_earray_idx_delete(const H5D_chk_idx_info_t *idx_info);
static herr_t H5D_earray_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst);
static herr_t H5D_earray_idx_copy_shutdown(H5O_layout_t *layout_src,
    H5O_layout_t *layout_dst, hid_t dxpl_id);
static herr_t H5D_earray_idx_size(const H5D_chk_idx_info_t *idx_info,
    hsize_t *size);
static herr_t H5D_earray_idx_reset(H5O_layout_t *layout, hbool_t reset_addr);
static herr_t H5D_earray_idx_support(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata, H5AC_info_t *child_entry);
static herr_t H5D_earray_idx_unsupport(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata, H5AC_info_t *child_entry);
static herr_t H5D_earray_idx_dump(const H5D_chk_idx_info_t *idx_info,
    FILE *stream);
static herr_t H5D_earray_idx_dest(const H5D_chk_idx_info_t *idx_info);


/*********************/
/* Package Variables */
/*********************/

/* Extensible array indexed chunk I/O ops */
const H5D_chunk_ops_t H5D_COPS_EARRAY[1] = {{
    TRUE,                               /* Extensible array indices support SWMR access */
    H5D_earray_idx_init,
    H5D_earray_idx_create,
    H5D_earray_idx_is_space_alloc,
    H5D_earray_idx_insert,
    H5D_earray_idx_get_addr,
    H5D_earray_idx_resize,
    H5D_earray_idx_iterate,
    H5D_earray_idx_remove,
    H5D_earray_idx_delete,
    H5D_earray_idx_copy_setup,
    H5D_earray_idx_copy_shutdown,
    H5D_earray_idx_size,
    H5D_earray_idx_reset,
    H5D_earray_idx_support,
    H5D_earray_idx_unsupport,
    H5D_earray_idx_dump,
    H5D_earray_idx_dest
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Extensible array class callbacks for dataset chunks w/o filters */
const H5EA_class_t H5EA_CLS_CHUNK[1]={{
    H5EA_CLS_CHUNK_ID,          /* Type of extensible array */
    sizeof(haddr_t),            /* Size of native element */
    H5D_earray_crt_context,     /* Create context */
    H5D_earray_dst_context,     /* Destroy context */
    H5D_earray_fill,            /* Fill block of missing elements callback */
    H5D_earray_encode,          /* Element encoding callback */
    H5D_earray_decode,          /* Element decoding callback */
    H5D_earray_debug            /* Element debugging callback */
}};

/* Extensible array class callbacks for dataset chunks w/filters */
const H5EA_class_t H5EA_CLS_FILT_CHUNK[1]={{
    H5EA_CLS_FILT_CHUNK_ID,     /* Type of extensible array */
    sizeof(H5D_earray_filt_elmt_t), /* Size of native element */
    H5D_earray_crt_context,     /* Create context */
    H5D_earray_dst_context,     /* Destroy context */
    H5D_earray_filt_fill,       /* Fill block of missing elements callback */
    H5D_earray_filt_encode,     /* Element encoding callback */
    H5D_earray_filt_decode,     /* Element decoding callback */
    H5D_earray_filt_debug       /* Element debugging callback */
}};

/* Declare a free list to manage the H5D_earray_ctx_t struct */
H5FL_DEFINE_STATIC(H5D_earray_ctx_t);



/*-------------------------------------------------------------------------
 * Function:	H5D_earray_crt_context
 *
 * Purpose:	Create context for callbacks
 *
 * Return:	Success:	non-NULL
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5D_earray_crt_context(void *_udata)
{
    H5D_earray_ctx_t *ctx;      /* Extensible array callback context */
    H5D_earray_ctx_ud_t *udata = (H5D_earray_ctx_ud_t *)_udata; /* User data for extensible array context */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_crt_context)

    /* Sanity checks */
    HDassert(udata);
    HDassert(udata->f);
    HDassert(udata->layout);

    /* Allocate new context structure */
    if(NULL == (ctx = H5FL_MALLOC(H5D_earray_ctx_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "can't allocate extensible array client callback context")

    /* Initialize the context */
    ctx->file_addr_len = H5F_SIZEOF_ADDR(udata->f);

    /* Compute the size required for encoding the size of a chunk, allowing
     *      for an extra byte, in case the filter makes the chunk larger.
     */
    ctx->chunk_size_len = 1 + ((H5V_log2_gen(udata->layout->u.chunk.size) + 8) / 8);
    if(ctx->chunk_size_len > 8)
        ctx->chunk_size_len = 8;

    /* Set return value */
    ret_value = ctx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_crt_context() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_dst_context
 *
 * Purpose:	Destroy context for callbacks
 *
 * Return:	Success:	non-NULL
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_dst_context(void *_ctx)
{
    H5D_earray_ctx_t *ctx = (H5D_earray_ctx_t *)_ctx;   /* Extensible array callback context */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_dst_context)

    /* Sanity checks */
    HDassert(ctx);

    /* Release context structure */
    ctx = H5FL_FREE(H5D_earray_ctx_t, ctx);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_dst_context() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_fill
 *
 * Purpose:	Fill "missing elements" in block of elements
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, January 27, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_fill(void *nat_blk, size_t nelmts)
{
    haddr_t fill_val = H5D_EARRAY_FILL;          /* Value to fill elements with */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_fill)

    /* Sanity checks */
    HDassert(nat_blk);
    HDassert(nelmts);

    H5V_array_fill(nat_blk, &fill_val, H5EA_CLS_CHUNK->nat_elmt_size, nelmts);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_encode
 *
 * Purpose:	Encode an element from "native" to "raw" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, January 27, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_encode(void *raw, const void *_elmt, size_t nelmts, void *_ctx)
{
    H5D_earray_ctx_t *ctx = (H5D_earray_ctx_t *)_ctx;   /* Extensible array callback context */
    const haddr_t *elmt = (const haddr_t *)_elmt;     /* Convenience pointer to native elements */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_encode)

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);
    HDassert(ctx);

    /* Encode native elements into raw elements */
    while(nelmts) {
        /* Encode element */
        /* (advances 'raw' pointer) */
        H5F_addr_encode_len(ctx->file_addr_len, (uint8_t **)&raw, *elmt);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to encode */
        nelmts--;
    } /* end while */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_decode
 *
 * Purpose:	Decode an element from "raw" to "native" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_decode(const void *_raw, void *_elmt, size_t nelmts, void *_ctx)
{
    H5D_earray_ctx_t *ctx = (H5D_earray_ctx_t *)_ctx;   /* Extensible array callback context */
    haddr_t *elmt = (haddr_t *)_elmt;           /* Convenience pointer to native elements */
    const uint8_t *raw = (const uint8_t *)_raw; /* Convenience pointer to raw elements */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_decode)

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);

    /* Decode raw elements into native elements */
    while(nelmts) {
        /* Decode element */
        /* (advances 'raw' pointer) */
        H5F_addr_decode_len(ctx->file_addr_len, &raw, elmt);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to decode */
        nelmts--;
    } /* end while */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_debug
 *
 * Purpose:	Display an element for debugging
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_debug(FILE *stream, int indent, int fwidth, hsize_t idx,
    const void *elmt)
{
    char temp_str[128];     /* Temporary string, for formatting */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_debug)

    /* Sanity checks */
    HDassert(stream);
    HDassert(elmt);

    /* Print element */
    sprintf(temp_str, "Element #%llu:", (unsigned long long)idx);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth, temp_str,
        *(const haddr_t *)elmt);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_filt_fill
 *
 * Purpose:	Fill "missing elements" in block of elements
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_filt_fill(void *nat_blk, size_t nelmts)
{
    H5D_earray_filt_elmt_t fill_val = H5D_EARRAY_FILT_FILL;     /* Value to fill elements with */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_filt_fill)

    /* Sanity checks */
    HDassert(nat_blk);
    HDassert(nelmts);
    HDassert(sizeof(fill_val) == H5EA_CLS_FILT_CHUNK->nat_elmt_size);

    H5V_array_fill(nat_blk, &fill_val, H5EA_CLS_FILT_CHUNK->nat_elmt_size, nelmts);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_filt_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_filt_encode
 *
 * Purpose:	Encode an element from "native" to "raw" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_filt_encode(void *_raw, const void *_elmt, size_t nelmts, void *_ctx)
{
    H5D_earray_ctx_t *ctx = (H5D_earray_ctx_t *)_ctx;   /* Extensible array callback context */
    uint8_t *raw = (uint8_t *)_raw;             /* Convenience pointer to raw elements */
    const H5D_earray_filt_elmt_t *elmt = (const H5D_earray_filt_elmt_t *)_elmt;     /* Convenience pointer to native elements */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_filt_encode)

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);
    HDassert(ctx);

    /* Encode native elements into raw elements */
    while(nelmts) {
        /* Encode element */
        /* (advances 'raw' pointer) */
        H5F_addr_encode_len(ctx->file_addr_len, &raw, elmt->addr);
        UINT64ENCODE_VAR(raw, elmt->nbytes, ctx->chunk_size_len);
        UINT32ENCODE(raw, elmt->filter_mask);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to encode */
        nelmts--;
    } /* end while */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_filt_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_filt_decode
 *
 * Purpose:	Decode an element from "raw" to "native" form
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_filt_decode(const void *_raw, void *_elmt, size_t nelmts, void *_ctx)
{
    H5D_earray_ctx_t *ctx = (H5D_earray_ctx_t *)_ctx;   /* Extensible array callback context */
    H5D_earray_filt_elmt_t *elmt = (H5D_earray_filt_elmt_t *)_elmt;           /* Convenience pointer to native elements */
    const uint8_t *raw = (const uint8_t *)_raw; /* Convenience pointer to raw elements */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_filt_decode)

    /* Sanity checks */
    HDassert(raw);
    HDassert(elmt);
    HDassert(nelmts);

    /* Decode raw elements into native elements */
    while(nelmts) {
        /* Decode element */
        /* (advances 'raw' pointer) */
        H5F_addr_decode_len(ctx->file_addr_len, &raw, &elmt->addr);
        UINT64DECODE_VAR(raw, elmt->nbytes, ctx->chunk_size_len);
        UINT32DECODE(raw, elmt->filter_mask);

        /* Advance native element pointer */
        elmt++;

        /* Decrement # of elements to decode */
        nelmts--;
    } /* end while */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_filt_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_filt_debug
 *
 * Purpose:	Display an element for debugging
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_filt_debug(FILE *stream, int indent, int fwidth, hsize_t idx,
    const void *_elmt)
{
    const H5D_earray_filt_elmt_t *elmt = (const H5D_earray_filt_elmt_t *)_elmt;           /* Convenience pointer to native elements */
    char temp_str[128];     /* Temporary string, for formatting */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_filt_debug)

    /* Sanity checks */
    HDassert(stream);
    HDassert(elmt);

    /* Print element */
    sprintf(temp_str, "Element #%llu:", (unsigned long long)idx);
    HDfprintf(stream, "%*s%-*s {%a, %u, %0x}\n", indent, "", fwidth, temp_str,
        elmt->addr, elmt->nbytes, elmt->filter_mask);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_filt_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_depend
 *
 * Purpose:	Create flush dependency between extensible array and dataset's
 *              object header.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, June  2, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_depend(const H5D_chk_idx_info_t *idx_info)
{
    H5O_loc_t oloc;         /* Temporary object header location for dataset */
    H5O_t *oh = NULL;       /* Dataset's object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_depend)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5D_CHUNK_IDX_EARRAY == idx_info->layout->u.chunk.idx_type);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(idx_info->layout->u.chunk.u.earray.ea);

    /* Set up object header location for dataset */
    H5O_loc_reset(&oloc);
    oloc.file = idx_info->f;
    oloc.addr = idx_info->layout->u.chunk.u.earray.dset_ohdr_addr;

    /* Pin the dataset's object header */
    if(NULL == (oh = H5O_pin(&oloc, idx_info->dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTPIN, FAIL, "unable to pin dataset object header")

    /* Make the extensible array a child flush dependency of the dataset's object header */
    if(H5EA_depend((H5AC_info_t *)oh, idx_info->layout->u.chunk.u.earray.ea) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency on object header")

done:
    /* Unpin the dataset's object header */
    if(oh && H5O_unpin(&oloc, oh) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTUNPIN, FAIL, "unable to unpin dataset object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_depend() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_undepend
 *
 * Purpose:	Remove flush dependency between extensible array and dataset's
 *              object header.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, June  2, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_undepend(const H5D_chk_idx_info_t *idx_info)
{
    H5O_loc_t oloc;         /* Temporary object header location for dataset */
    H5O_t *oh = NULL;       /* Dataset's object header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_undepend)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5D_CHUNK_IDX_EARRAY == idx_info->layout->u.chunk.idx_type);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(idx_info->layout->u.chunk.u.earray.ea);

    /* Set up object header location for dataset */
    H5O_loc_reset(&oloc);
    oloc.file = idx_info->f;
    oloc.addr = idx_info->layout->u.chunk.u.earray.dset_ohdr_addr;

    /* Pin the dataset's object header */
    if(NULL == (oh = H5O_pin(&oloc, idx_info->dxpl_id)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTPIN, FAIL, "unable to pin dataset object header")

    /* Remove the extensible array as a child flush dependency of the dataset's object header */
    if(H5EA_undepend((H5AC_info_t *)oh, idx_info->layout->u.chunk.u.earray.ea) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on object header")

done:
    /* Unpin the dataset's object header */
    if(oh && H5O_unpin(&oloc, oh) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTUNPIN, FAIL, "unable to unpin dataset object header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_undepend() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_open
 *
 * Purpose:	Opens an existing extensible array.
 *
 * Note:	This information is passively initialized from each index
 *              operation callback because those abstract chunk index operations
 *              are designed to work with the v1 B-tree chunk indices also,
 *              which don't require an 'open' for the data structure.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_open(const H5D_chk_idx_info_t *idx_info)
{
    const H5EA_class_t *cls;            /* Extensible array class to use */
    H5D_earray_ctx_ud_t udata;          /* User data for extensible array open call */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_open)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5D_CHUNK_IDX_EARRAY == idx_info->layout->u.chunk.idx_type);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(NULL == idx_info->layout->u.chunk.u.earray.ea);

    /* Set up the user data */
    udata.f = idx_info->f;
    udata.layout = idx_info->layout;

    /* Open the extensible array for the chunk index */
    cls = (idx_info->pline->nused > 0) ?  H5EA_CLS_FILT_CHUNK : H5EA_CLS_CHUNK;
    if(NULL == (idx_info->layout->u.chunk.u.earray.ea = H5EA_open(idx_info->f, idx_info->dxpl_id, idx_info->layout->u.chunk.u.earray.addr, cls, &udata)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't open extensible array")

    /* Check for SWMR writes to the file */
    if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
        if(H5D_earray_idx_depend(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency on object header")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_open() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_init
 *
 * Purpose:	Initialize the indexing information for a dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, May 27, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_init(const H5D_chk_idx_info_t *idx_info, const H5S_t *space,
    haddr_t dset_ohdr_addr)
{
    hsize_t max_dims[H5O_LAYOUT_NDIMS];    /* Max. size of dataset dimensions */
    int unlim_dim;              /* Rank of the dataset's unlimited dimension */
    int sndims;                 /* Rank of dataspace */
    unsigned ndims;             /* Rank of dataspace */
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_init)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(space);
    HDassert(H5F_addr_defined(dset_ohdr_addr));

    /* Get the dim info for dataset */
    if((sndims = H5S_get_simple_extent_dims(space, NULL, max_dims)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get dataspace dimensions")
    H5_ASSIGN_OVERFLOW(ndims, sndims, int, unsigned);

    /* Find the rank of the unlimited dimension */
    unlim_dim = (-1);
    for(u = 0; u < ndims; u++) {
        /* Check for unlimited dimension */
        if(H5S_UNLIMITED == max_dims[u]) {
            /* Check if we've already found an unlimited dimension */
            if(unlim_dim >= 0)
                HGOTO_ERROR(H5E_DATASET, H5E_ALREADYINIT, FAIL, "already found unlimited dimension")

            /* Set the unlimited dimension */
            unlim_dim = (int)u;
        } /* end if */
    } /* end for */

    /* Check if we didn't find an unlimited dimension */
    if(unlim_dim < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_UNINITIALIZED, FAIL, "didn't find unlimited dimension")

    /* Set the unlimited dimension for the layout's future use */
    idx_info->layout->u.chunk.u.earray.unlim_dim = (unsigned)unlim_dim;

    /* Store the dataset's object header address for later */
    idx_info->layout->u.chunk.u.earray.dset_ohdr_addr = dset_ohdr_addr;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_init() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_create
 *
 * Purpose:	Creates a new indexed-storage extensible array and initializes
 *              the layout struct with information about the storage.  The
 *		struct should be immediately written to the object header.
 *
 *		This function must be called before passing LAYOUT to any of
 *		the other indexed storage functions!
 *
 * Return:	Non-negative on success (with the LAYOUT argument initialized
 *		and ready to write to an object header). Negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, January 27, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_create(const H5D_chk_idx_info_t *idx_info)
{
    H5EA_create_t cparam;               /* Extensible array creation parameters */
    H5D_earray_ctx_ud_t udata;          /* User data for extensible array create call */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_create)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(!H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(NULL == idx_info->layout->u.chunk.u.earray.ea);

    /* General parameters */
    if(idx_info->pline->nused > 0) {
        unsigned chunk_size_len;        /* Size of encoded chunk size */
        
        /* Compute the size required for encoding the size of a chunk, allowing
         *      for an extra byte, in case the filter makes the chunk larger.
         */
        chunk_size_len = 1 + ((H5V_log2_gen(idx_info->layout->u.chunk.size) + 8) / 8);
        if(chunk_size_len > 8)
            chunk_size_len = 8;

        cparam.cls = H5EA_CLS_FILT_CHUNK;
        cparam.raw_elmt_size = (uint8_t)(H5F_SIZEOF_ADDR(idx_info->f) + chunk_size_len + 4);
    } /* end if */
    else {
        cparam.cls = H5EA_CLS_CHUNK;
        cparam.raw_elmt_size = (uint8_t)H5F_SIZEOF_ADDR(idx_info->f);
    } /* end else */
    cparam.max_nelmts_bits = H5D_EARRAY_MAX_NELMTS_BITS;
    cparam.idx_blk_elmts = H5D_EARRAY_IDX_BLK_ELMTS;
    cparam.sup_blk_min_data_ptrs = H5D_EARRAY_SUP_BLK_MIN_DATA_PTRS;
    cparam.data_blk_min_elmts = H5D_EARRAY_DATA_BLK_MIN_ELMTS;
    cparam.max_dblk_page_nelmts_bits = H5D_EARRAY_MAX_DBLOCK_PAGE_NELMTS_BITS;

    /* Set up the user data */
    udata.f = idx_info->f;
    udata.layout = idx_info->layout;

    /* Create the extensible array for the chunk index */
    if(NULL == (idx_info->layout->u.chunk.u.earray.ea = H5EA_create(idx_info->f, idx_info->dxpl_id, &cparam, &udata)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't create extensible array")

    /* Get the address of the extensible array in file */
    if(H5EA_get_addr(idx_info->layout->u.chunk.u.earray.ea, &(idx_info->layout->u.chunk.u.earray.addr)) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query extensible array address")

    /* Check for SWMR writes to the file */
    if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
        if(H5D_earray_idx_depend(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency on object header")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_is_space_alloc
 *
 * Purpose:	Query if space is allocated for index method
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5D_earray_idx_is_space_alloc(const H5O_layout_t *layout)
{
    hbool_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_idx_is_space_alloc)

    /* Check args */
    HDassert(layout);

    /* Set return value */
    ret_value = (hbool_t)H5F_addr_defined(layout->u.chunk.u.earray.addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_insert
 *
 * Purpose:	Create the chunk it if it doesn't exist, or reallocate the
 *              chunk if its size changed.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_insert(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    hsize_t     idx;                    /* Array index of chunk */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_insert)

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(udata);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Check for unlimited dim. not being the slowest-changing dim. */
    if(idx_info->layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_coords[H5O_LAYOUT_NDIMS];	/* swizzled chunk coordinates */
        unsigned ndims = (idx_info->layout->u.chunk.ndims - 1); /* Number of dimensions */

        /* Set up the swizzled chunk coordinates */
        HDmemcpy(swizzled_coords, udata->common.offset, ndims * sizeof(udata->common.offset[0]));
        H5V_swizzle_coords(swizzled_coords, idx_info->layout->u.chunk.u.earray.unlim_dim);

        /* Calculate the index of this chunk */
        if(H5V_chunk_index(ndims, swizzled_coords, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.u.earray.swizzled_down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end if */
    else {
        /* Calculate the index of this chunk */
        if(H5V_chunk_index((idx_info->layout->u.chunk.ndims - 1), udata->common.offset, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end else */

    /* Check for filters on chunks */
    if(idx_info->pline->nused > 0) {
        H5D_earray_filt_elmt_t elmt;            /* Extensible array element */
        unsigned allow_chunk_size_len;          /* Allowed size of encoded chunk size */
        unsigned new_chunk_size_len;            /* Size of encoded chunk size */
        hbool_t alloc_chunk = FALSE;            /* Whether to allocate chunk */

        /* Compute the size required for encoding the size of a chunk, allowing
         *      for an extra byte, in case the filter makes the chunk larger.
         */
        allow_chunk_size_len = 1 + ((H5V_log2_gen(idx_info->layout->u.chunk.size) + 8) / 8);
        if(allow_chunk_size_len > 8)
            allow_chunk_size_len = 8;

        /* Compute encoded size of chunk */
        new_chunk_size_len = (H5V_log2_gen(udata->nbytes) + 8) / 8;
        if(new_chunk_size_len > 8)
            HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "encoded chunk size is more than 8 bytes?!?")

        /* Check if the chunk became too large to be encoded */
        if(new_chunk_size_len > allow_chunk_size_len)
            HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "chunk size can't be encoded")

        /* Get the information for the chunk */
        if(H5EA_get(ea, idx_info->dxpl_id, idx, &elmt) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk info")

        /* Check for previous chunk */
        if(H5F_addr_defined(elmt.addr)) {
            /* Sanity check */
            HDassert(!H5F_addr_defined(udata->addr) || H5F_addr_eq(udata->addr, elmt.addr));

            /* Check for chunk being same size */
            if(udata->nbytes != elmt.nbytes) {
                /* Release previous chunk */
                H5_CHECK_OVERFLOW(elmt.nbytes, uint32_t, hsize_t);
                if(H5MF_xfree(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, elmt.addr, (hsize_t)elmt.nbytes) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk")
                elmt.addr = HADDR_UNDEF;
                alloc_chunk = TRUE;
            } /* end if */
            else {
                /* Don't need to reallocate chunk, but send its address back up */
                if(!H5F_addr_defined(udata->addr))
                    udata->addr = elmt.addr;
            } /* end else */
        } /* end if */
        else
            alloc_chunk = TRUE;

        /* Check if we need to allocate the chunk */
        if(alloc_chunk) {
            H5_CHECK_OVERFLOW(udata->nbytes, uint32_t, hsize_t);
            udata->addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, (hsize_t)udata->nbytes);
            if(!H5F_addr_defined(udata->addr))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")

            /* Update the element information */
            elmt.addr = udata->addr;
            elmt.nbytes = udata->nbytes;
            elmt.filter_mask = udata->filter_mask;

            /* Set the info for the chunk */
            if(H5EA_set(ea, idx_info->dxpl_id, idx, &elmt) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set chunk info")
        } /* end if */
    } /* end if */
    else {
        HDassert(!H5F_addr_defined(udata->addr));
        HDassert(udata->nbytes == idx_info->layout->u.chunk.size);

#ifndef NDEBUG
{
    haddr_t     addr = HADDR_UNDEF;     /* Address of chunk in file */

    /* Get the address for the chunk */
    if(H5EA_get(ea, idx_info->dxpl_id, idx, &addr) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")
    HDassert(!H5F_addr_defined(addr));
}
#endif /* NDEBUG */

        /*
         * Allocate storage for the new chunk
         */
        H5_CHECK_OVERFLOW(udata->nbytes, /*From: */uint32_t, /*To: */hsize_t);
        udata->addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, (hsize_t)udata->nbytes);
        if(!H5F_addr_defined(udata->addr))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "file allocation failed")

        /* Set the address for the chunk */
        if(H5EA_set(ea, idx_info->dxpl_id, idx, &udata->addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set chunk address")
    } /* end else */
    HDassert(H5F_addr_defined(udata->addr));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_earray_idx_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_get_addr
 *
 * Purpose:	Get the file address of a chunk if file space has been
 *		assigned.  Save the retrieved information in the udata
 *		supplied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_get_addr(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    hsize_t     idx;                    /* Array index of chunk */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_get_addr)

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(udata);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Check for unlimited dim. not being the slowest-changing dim. */
    if(idx_info->layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_coords[H5O_LAYOUT_NDIMS];	/* swizzled chunk coordinates */
        unsigned ndims = (idx_info->layout->u.chunk.ndims - 1); /* Number of dimensions */

        /* Set up the swizzled chunk coordinates */
        HDmemcpy(swizzled_coords, udata->common.offset, ndims * sizeof(udata->common.offset[0]));
        H5V_swizzle_coords(swizzled_coords, idx_info->layout->u.chunk.u.earray.unlim_dim);

        /* Calculate the index of this chunk */
        if(H5V_chunk_index(ndims, swizzled_coords, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.u.earray.swizzled_down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end if */
    else {
        /* Calculate the index of this chunk */
        if(H5V_chunk_index((idx_info->layout->u.chunk.ndims - 1), udata->common.offset, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end else */

    /* Check for filters on chunks */
    if(idx_info->pline->nused > 0) {
        H5D_earray_filt_elmt_t elmt;            /* Extensible array element */

        /* Get the information for the chunk */
        if(H5EA_get(ea, idx_info->dxpl_id, idx, &elmt) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk info")

        /* Set the info for the chunk */
        udata->addr = elmt.addr;
        udata->nbytes = elmt.nbytes;
        udata->filter_mask = elmt.filter_mask;
    } /* end if */
    else {
        /* Get the address for the chunk */
        if(H5EA_get(ea, idx_info->dxpl_id, idx, &udata->addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")

        /* Update the other (constant) information for the chunk */
        udata->nbytes = idx_info->layout->u.chunk.size;
        udata->filter_mask = 0;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_earray_idx_get_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_resize
 *
 * Purpose:	Calculate/setup the swizzled down chunk array, used for chunk
 *              index calculations.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 23, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_resize(H5O_layout_t *layout)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_resize)

    /* Check args */
    HDassert(layout);

    /* Set up the swizzled "down" chunk information */
    if(layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_chunks[H5O_LAYOUT_NDIMS];    /* Swizzled form of # of chunks in each dimension */

        HDmemcpy(swizzled_chunks, layout->u.chunk.chunks, (layout->u.chunk.ndims - 1) * sizeof(swizzled_chunks[0]));
        H5V_swizzle_coords(swizzled_chunks, layout->u.chunk.u.earray.unlim_dim);

        /* Get the swizzled "down" sizes for each dimension */
        if(H5V_array_down((layout->u.chunk.ndims - 1), swizzled_chunks, layout->u.chunk.u.earray.swizzled_down_chunks) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't compute swizzled 'down' chunk size value")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_resize() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_iterate
 *
 * Purpose:	Iterate over the chunks in an index, making a callback
 *              for each one.
 *
 * Note:	This implementation is slow, particularly for sparse
 *              extensible arrays, replace it with call to H5EA_iterate()
 *              when that's available.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
H5D_earray_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata)
{
    H5EA_t      *ea;            /* Pointer to extensible array structure */
    H5EA_stat_t ea_stat;        /* Extensible array statistics */
    int ret_value = H5_ITER_CONT;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_iterate)

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(chunk_cb);
    HDassert(chunk_udata);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Get the extensible array statistics */
    if(H5EA_get_stats(ea, &ea_stat) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query extensible array statistics")

    /* Check if there are any array elements */
    if(ea_stat.stored.max_idx_set > 0) {
        H5D_chunk_rec_t chunk_rec;  /* Generic chunk record for callback */
        hsize_t u;              /* Local index variable */

        /* Check for filters on chunks */
        if(idx_info->pline->nused > 0) {
            /* Prepare common fields of chunk record for callback */
            HDmemset(&chunk_rec, 0, sizeof(chunk_rec));

            /* Loop over array elements */
            /* (Note: this may be too simple for datasets with >1 dimension) */
            for(u = 0; u < ea_stat.stored.max_idx_set; u++, chunk_rec.offset[0] += idx_info->layout->u.chunk.dim[0]) {
                H5D_earray_filt_elmt_t elmt;            /* Extensible array element */

                /* Get the info about the chunk for the index */
                if(H5EA_get(ea, idx_info->dxpl_id, u, &elmt) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk info")

                /* Check if chunk exists */
                if(H5F_addr_defined(elmt.addr)) {
                    /* Set chunk info for callback record */
                    chunk_rec.chunk_addr = elmt.addr;
                    chunk_rec.nbytes = elmt.nbytes;
                    chunk_rec.filter_mask = elmt.filter_mask;

                    /* Make chunk callback */
                    if((ret_value = (*chunk_cb)(&chunk_rec, chunk_udata)) < 0)
                        HERROR(H5E_DATASET, H5E_CALLBACK, "failure in generic chunk iterator callback");
                } /* end if */
            } /* end for */
        } /* end if */
        else {
            /* Prepare common fields of chunk record for callback */
            HDmemset(&chunk_rec, 0, sizeof(chunk_rec));
            chunk_rec.nbytes = idx_info->layout->u.chunk.size;
            chunk_rec.filter_mask = 0;

            /* Loop over array elements */
            /* (Note: this may be too simple for datasets with >1 dimension) */
            for(u = 0; u < ea_stat.stored.max_idx_set; u++, chunk_rec.offset[0] += idx_info->layout->u.chunk.dim[0]) {
                haddr_t addr;       /* Chunk address */

                /* Get the address of the chunk for the index */
                if(H5EA_get(ea, idx_info->dxpl_id, u, &addr) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")

                /* Check if chunk exists */
                if(H5F_addr_defined(addr)) {
                    /* Set chunk address for callback record */
                    chunk_rec.chunk_addr = addr;

                    /* Make chunk callback */
                    if((ret_value = (*chunk_cb)(&chunk_rec, chunk_udata)) < 0)
                        HERROR(H5E_DATASET, H5E_CALLBACK, "failure in generic chunk iterator callback");
                } /* end if */
            } /* end for */
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_remove
 *
 * Purpose:	Remove chunk from index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_remove(const H5D_chk_idx_info_t *idx_info, H5D_chunk_common_ud_t *udata)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    hsize_t     idx;                    /* Array index of chunk */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_remove)

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(udata);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Check for unlimited dim. not being the slowest-changing dim. */
    if(idx_info->layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_coords[H5O_LAYOUT_NDIMS];	/* swizzled chunk coordinates */
        unsigned ndims = (idx_info->layout->u.chunk.ndims - 1); /* Number of dimensions */

        /* Set up the swizzled chunk coordinates */
        HDmemcpy(swizzled_coords, udata->offset, ndims * sizeof(udata->offset[0]));
        H5V_swizzle_coords(swizzled_coords, idx_info->layout->u.chunk.u.earray.unlim_dim);

        /* Calculate the index of this chunk */
        if(H5V_chunk_index(ndims, swizzled_coords, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.u.earray.swizzled_down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end if */
    else {
        /* Calculate the index of this chunk */
        if(H5V_chunk_index((idx_info->layout->u.chunk.ndims - 1), udata->offset, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end else */

    /* Check for filters on chunks */
    if(idx_info->pline->nused > 0) {
        H5D_earray_filt_elmt_t elmt;            /* Extensible array element */

        /* Get the info about the chunk for the index */
        if(H5EA_get(ea, idx_info->dxpl_id, idx, &elmt) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk info")

        /* Remove raw data chunk from file */
        HDassert(H5F_addr_defined(elmt.addr));
        H5_CHECK_OVERFLOW(elmt.nbytes, /*From: */uint32_t, /*To: */hsize_t);
        if(H5MF_xfree(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, elmt.addr, (hsize_t)elmt.nbytes) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk")

        /* Reset the info about the chunk for the index */
        elmt.addr = HADDR_UNDEF;
        elmt.nbytes = 0;
        elmt.filter_mask = 0;
        if(H5EA_set(ea, idx_info->dxpl_id, idx, &elmt) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to reset chunk info")
    } /* end if */
    else {
        haddr_t     addr = HADDR_UNDEF;     /* Chunk address */

        /* Get the address of the chunk for the index */
        if(H5EA_get(ea, idx_info->dxpl_id, idx, &addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")

        /* Remove raw data chunk from file */
        HDassert(H5F_addr_defined(addr));
        H5_CHECK_OVERFLOW(idx_info->layout->u.chunk.size, /*From: */uint32_t, /*To: */hsize_t);
        if(H5MF_xfree(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, addr, (hsize_t)idx_info->layout->u.chunk.size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk")

        /* Reset the address of the chunk for the index */
        addr = HADDR_UNDEF;
        if(H5EA_set(ea, idx_info->dxpl_id, idx, &addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to reset chunk address")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_earray_idx_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_delete_cb
 *
 * Purpose:	Delete space for chunk in file
 *
 * Return:	Success:	Non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static int
H5D_earray_idx_delete_cb(const H5D_chunk_rec_t *chunk_rec, void *_udata)
{
    H5D_earray_ud_t *udata = (H5D_earray_ud_t *)_udata;         /* User data for callback */
    int ret_value = H5_ITER_CONT;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_delete_cb)

    /* Sanity checks */
    HDassert(chunk_rec);
    HDassert(H5F_addr_defined(chunk_rec->chunk_addr));
    HDassert(chunk_rec->nbytes > 0);
    HDassert(udata);
    HDassert(udata->f);

    /* Remove raw data chunk from file */
    H5_CHECK_OVERFLOW(chunk_rec->nbytes, /*From: */uint32_t, /*To: */hsize_t);
    if(H5MF_xfree(udata->f, H5FD_MEM_DRAW, udata->dxpl_id, chunk_rec->chunk_addr, (hsize_t)chunk_rec->nbytes) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, H5_ITER_ERROR, "unable to free chunk")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_delete_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_delete
 *
 * Purpose:	Delete index and raw data storage for entire dataset
 *              (i.e. all chunks)
 *
 * Note:	This implementation is slow, particularly for sparse
 *              extensible arrays, replace it with call to H5EA_iterate()
 *              when that's available.
 *
 * Return:	Success:	Non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_delete(const H5D_chk_idx_info_t *idx_info)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_delete)

    /* Sanity checks */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);

    /* Check if the index data structure has been allocated */
    if(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr)) {
        H5D_earray_ud_t udata;         /* User data for callback */

        /* Initialize user data for callback */
        udata.f = idx_info->f;
        udata.dxpl_id = idx_info->dxpl_id;

        /* Iterate over the chunk addresses in the extensible array, deleting each chunk */
        if(H5D_earray_idx_iterate(idx_info, H5D_earray_idx_delete_cb, &udata) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADITER, FAIL, "unable to iterate over chunk addresses")

        /* Close extensible array */
        if(H5EA_close(idx_info->layout->u.chunk.u.earray.ea, idx_info->dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close extensible array")
        idx_info->layout->u.chunk.u.earray.ea = NULL;

        /* Delete extensible array */
        if(H5EA_delete(idx_info->f, idx_info->dxpl_id, idx_info->layout->u.chunk.u.earray.addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDELETE, FAIL, "unable to delete chunk extensible array")
        idx_info->layout->u.chunk.u.earray.addr = HADDR_UNDEF;
    } /* end if */
    else
        HDassert(NULL == idx_info->layout->u.chunk.u.earray.ea);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_copy_setup
 *
 * Purpose:	Set up any necessary information for copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst)
{
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_copy_setup)

    /* Check args */
    HDassert(idx_info_src);
    HDassert(idx_info_src->f);
    HDassert(idx_info_src->pline);
    HDassert(idx_info_src->layout);
    HDassert(idx_info_dst);
    HDassert(idx_info_dst->f);
    HDassert(idx_info_dst->pline);
    HDassert(idx_info_dst->layout);
    HDassert(!H5F_addr_defined(idx_info_dst->layout->u.chunk.u.earray.addr));

    /* Check if the source extensible array is open yet */
    if(NULL == idx_info_src->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info_src) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Create the extensible array that describes chunked storage in the dest. file */
    if(H5D_earray_idx_create(idx_info_dst) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize chunked storage")
    HDassert(H5F_addr_defined(idx_info_dst->layout->u.chunk.u.earray.addr));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_copy_setup() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_copy_shutdown
 *
 * Purpose:	Shutdown any information from copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_copy_shutdown(H5O_layout_t *layout_src, H5O_layout_t *layout_dst,
    hid_t dxpl_id)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_copy_shutdown)

    /* Check args */
    HDassert(layout_src);
    HDassert(layout_src->u.chunk.u.earray.ea);
    HDassert(layout_dst);
    HDassert(layout_dst->u.chunk.u.earray.ea);

    /* Close extensible arrays */
    if(H5EA_close(layout_src->u.chunk.u.earray.ea, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close extensible array")
    layout_src->u.chunk.u.earray.ea = NULL;
    if(H5EA_close(layout_dst->u.chunk.u.earray.ea, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close extensible array")
    layout_dst->u.chunk.u.earray.ea = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_copy_shutdown() */


/*-------------------------------------------------------------------------
 * Function:    H5D_earray_idx_size
 *
 * Purpose:     Retrieve the amount of index storage for chunked dataset
 *
 * Return:      Success:        Non-negative
 *              Failure:        negative
 *
 * Programmer:  Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_size(const H5D_chk_idx_info_t *idx_info, hsize_t *index_size)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    H5EA_stat_t ea_stat;                /* Extensible array statistics */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5D_earray_idx_size, FAIL)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.addr));
    HDassert(index_size);

    /* Open the extensible array in file */
    if(H5D_earray_idx_open(idx_info) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Get the extensible array statistics */
    if(H5EA_get_stats(ea, &ea_stat) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't query extensible array statistics")

    /* Set the size of the extensible array */
    *index_size = ea_stat.computed.hdr_size + ea_stat.computed.index_blk_size
            + ea_stat.stored.super_blk_size + ea_stat.stored.data_blk_size;

done:
    if(idx_info->layout->u.chunk.u.earray.ea) {
        if(H5EA_close(idx_info->layout->u.chunk.u.earray.ea, idx_info->dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close extensible array")
        idx_info->layout->u.chunk.u.earray.ea = NULL;
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_size() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_reset
 *
 * Purpose:	Reset indexing information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 * Modifications:
 *      Vailin Choi; April 2009
 *      Reset address of the chunked storage index if RESET_ADDR is set
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_reset(H5O_layout_t *layout, hbool_t reset_addr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_idx_reset)

    /* Check args */
    HDassert(layout);

    /* Reset index info */
    if(reset_addr) {
	layout->u.chunk.u.earray.addr = HADDR_UNDEF;
        layout->u.chunk.u.earray.dset_ohdr_addr = HADDR_UNDEF;
    } /* end if */
    layout->u.chunk.u.earray.ea = NULL;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_idx_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_support
 *
 * Purpose:	Create a dependency between a chunk [proxy] and the index
 *              metadata that contains the record for the chunk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 21, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_support(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata, H5AC_info_t *child_entry)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    hsize_t     idx;                    /* Array index of chunk */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_support)

    /* Check args */
    HDassert(idx_info);
    HDassert(udata);
    HDassert(child_entry);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Check for unlimited dim. not being the slowest-changing dim. */
    if(idx_info->layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_coords[H5O_LAYOUT_NDIMS];	/* swizzled chunk coordinates */
        unsigned ndims = (idx_info->layout->u.chunk.ndims - 1); /* Number of dimensions */

        /* Set up the swizzled chunk coordinates */
        HDmemcpy(swizzled_coords, udata->offset, ndims * sizeof(udata->offset[0]));
        H5V_swizzle_coords(swizzled_coords, idx_info->layout->u.chunk.u.earray.unlim_dim);

        /* Calculate the index of this chunk */
        if(H5V_chunk_index(ndims, swizzled_coords, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.u.earray.swizzled_down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end if */
    else {
        /* Calculate the index of this chunk */
        if(H5V_chunk_index((idx_info->layout->u.chunk.ndims - 1), udata->offset, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end else */

    /* Create flush dependency between the child_entry and the piece of metadata
     *  in the extensible array that contains the entry for this chunk.
     */
    if(H5EA_support(ea, idx_info->dxpl_id, idx, child_entry) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency on extensible array metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_support() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_unsupport
 *
 * Purpose:	Remove a dependency between a chunk [proxy] and the index
 *              metadata that contains the record for the chunk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 21, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_unsupport(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata, H5AC_info_t *child_entry)
{
    H5EA_t      *ea;                    /* Pointer to extensible array structure */
    hsize_t     idx;                    /* Array index of chunk */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_unsupport)

    /* Check args */
    HDassert(idx_info);
    HDassert(udata);
    HDassert(child_entry);

    /* Check if the extensible array is open yet */
    if(NULL == idx_info->layout->u.chunk.u.earray.ea) {
        /* Open the extensible array in file */
        if(H5D_earray_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open extensible array")
    } /* end if */

    /* Set convenience pointer to extensible array structure */
    ea = idx_info->layout->u.chunk.u.earray.ea;

    /* Check for unlimited dim. not being the slowest-changing dim. */
    if(idx_info->layout->u.chunk.u.earray.unlim_dim > 0) {
        hsize_t swizzled_coords[H5O_LAYOUT_NDIMS];	/* swizzled chunk coordinates */
        unsigned ndims = (idx_info->layout->u.chunk.ndims - 1); /* Number of dimensions */

        /* Set up the swizzled chunk coordinates */
        HDmemcpy(swizzled_coords, udata->offset, ndims * sizeof(udata->offset[0]));
        H5V_swizzle_coords(swizzled_coords, idx_info->layout->u.chunk.u.earray.unlim_dim);

        /* Calculate the index of this chunk */
        if(H5V_chunk_index(ndims, swizzled_coords, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.u.earray.swizzled_down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end if */
    else {
        /* Calculate the index of this chunk */
        if(H5V_chunk_index((idx_info->layout->u.chunk.ndims - 1), udata->offset, idx_info->layout->u.chunk.dim, idx_info->layout->u.chunk.down_chunks, &idx) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "can't get chunk index")
    } /* end else */

    /* Remove flush dependency between the child_entry and the piece of metadata
     *  in the extensible array that contains the entry for this chunk.
     */
    if(H5EA_unsupport(ea, idx_info->dxpl_id, idx, child_entry) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on extensible array metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_unsupport() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_dump
 *
 * Purpose:	Dump indexing information to a stream.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_dump(const H5D_chk_idx_info_t *idx_info, FILE *stream)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5D_earray_idx_dump)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->layout);
    HDassert(stream);

    HDfprintf(stream, "    Address: %a\n", idx_info->layout->u.chunk.u.earray.addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_earray_idx_dump() */


/*-------------------------------------------------------------------------
 * Function:	H5D_earray_idx_dest
 *
 * Purpose:	Release indexing information in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Saturday, January 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_earray_idx_dest(const H5D_chk_idx_info_t *idx_info)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5D_earray_idx_dest)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->layout);

    /* Check if the extensible array is open */
    if(idx_info->layout->u.chunk.u.earray.ea) {
        /* Check for SWMR writes to the file */
        if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
            /* Sanity check */
            HDassert(H5F_addr_defined(idx_info->layout->u.chunk.u.earray.dset_ohdr_addr));

            /* Remove flush dependency between extensible array and dataset' object header */
            if(H5D_earray_idx_undepend(idx_info) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTUNDEPEND, FAIL, "unable to remove flush dependency on object header")
        } /* end if */

        /* Close extensible array */
        if(H5EA_close(idx_info->layout->u.chunk.u.earray.ea, idx_info->dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close extensible array")
        idx_info->layout->u.chunk.u.earray.ea = NULL;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_earray_idx_dest() */

