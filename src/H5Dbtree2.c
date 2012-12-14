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
 *
 * Purpose: v2 B-tree indexing for chunked datasets with > 1 unlimited dimensions.
 *   Each dataset chunk in the b-tree is identified by its dimensional offset.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                    */
#include "H5Dpkg.h"		/* Datasets				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MFprivate.h"     	/* File space management                */
#include "H5Vprivate.h"		/* Vector and array functions		*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/
/* Typedef for non-filtered object's records in v2 B-tree */
typedef struct H5D_bt2_rec_t {
    haddr_t addr;       		/* Address of the chunk in the file */
    hsize_t offset[H5O_LAYOUT_NDIMS];   /* logical offset to start*/
} H5D_bt2_rec_t;

/* Typedef for filtered object's records in v2 B-tree */
typedef struct H5D_bt2_filt_rec_t {
    haddr_t addr;       	/* Address of the filtered object in the file */
    uint32_t nbytes;        	/* Length of the filtered object in the file */
    uint32_t filter_mask;   	/* I/O pipeline filter mask for filtered object in the file */
    hsize_t offset[H5O_LAYOUT_NDIMS];       /* logical offset to start*/
} H5D_bt2_filt_rec_t;

/* User data for creating callback context */
typedef struct H5D_bt2_ctx_ud_t {
    const H5F_t *f;             /* Pointer to file info */
    uint32_t chunk_size;        /* Size of chunk (bytes; for filtered object) */
    unsigned ndims;		/* Number of dimensions */
} H5D_bt2_ctx_ud_t;

/* The callback context */
typedef struct H5D_bt2_ctx_t {
    size_t sizeof_addr;       	/* Size of file addresses in the file (bytes) */
    size_t chunk_size_len;      /* Size of chunk sizes in the file (bytes) */
    unsigned ndims;		/* Number of dimensions */
} H5D_bt2_ctx_t;

/* User data for the chunk's removal callback routine */
typedef struct H5D_bt2_remove_ud_t {
    H5F_t *f;                   /* File pointer for operation */
    hid_t dxpl_id;              /* DXPL ID for operation */
    uint32_t unfilt_size;       /* Size of unfiltered chunk in bytes */
} H5D_bt2_remove_ud_t;

/* Callback info for iteration over chunks in v2 B-tree */
typedef struct H5D_bt2_it_ud_t {
    H5D_chunk_common_ud_t common;       /* Common info for v2 B-tree user data (must be first) */
    H5D_chunk_rec_t     chunk_rec;      /* Generic chunk record for callback */
    hbool_t		filtered;	/* The chunk is filtered or not */
    H5D_chunk_cb_func_t cb;             /* Callback routine for the chunk */
    void                *udata;         /* User data for the chunk's callback routine */
} H5D_bt2_it_ud_t;

/* User data for compare callback */
typedef struct H5D_bt2_find_ud_t {
    void *rec;    	/* The record to search for */
    unsigned ndims;	/* Number of dimensions for the chunked dataset */
    const H5O_layout_chunk_t *layout; /* Chunk layout description */
    const struct H5D_rdcc_t *rdcc; /* Chunk cache.  Only necessary if the index
                                    * may be modified, and if any chunks in the
                                    * dset may be cached */
} H5D_bt2_find_ud_t;


/********************/
/* Local Prototypes */
/********************/

/* Shared v2 B-tree methods for indexing filtered and non-filtered chunked datasets */
static void *H5D_bt2_crt_context(void *udata);
static herr_t H5D_bt2_dst_context(void *ctx);
static void *H5D_bt2_crt_dbg_context(H5F_t *f, hid_t dxpl_id, haddr_t obj_addr);
static herr_t H5D_bt2_dst_dbg_context(void *_u_ctx);

/* v2 B-tree class for indexing non-filtered chunked datasets */
static herr_t H5D_bt2_store(void *native, const void *udata);
static herr_t H5D_bt2_compare(const void *rec1, const void *rec2);
static herr_t H5D_bt2_encode(uint8_t *raw, const void *native, void *ctx);
static herr_t H5D_bt2_decode(const uint8_t *raw, void *native, void *ctx);
static herr_t H5D_bt2_crt_flush_dep(void *_record, void *_udata, void *parent);
static herr_t H5D_bt2_upd_flush_dep(void *_record, void *_udata,
    void *old_parent, void *new_parent);
static herr_t H5D_bt2_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *u_ctx);

/* v2 B-tree class for indexing filtered chunked datasets */
static herr_t H5D_bt2_filt_store(void *native, const void *udata);
static herr_t H5D_bt2_filt_compare(const void *rec1, const void *rec2);
static herr_t H5D_bt2_filt_encode(uint8_t *raw, const void *native, void *ctx);
static herr_t H5D_bt2_filt_decode(const uint8_t *raw, void *native, void *ctx);
static herr_t H5D_bt2_filt_crt_flush_dep(void *_record, void *_udata,
    void *parent);
static herr_t H5D_bt2_filt_upd_flush_dep(void *_record, void *_udata,
    void *old_parent, void *new_parent);
static herr_t H5D_bt2_filt_debug(FILE *stream, const H5F_t *f, hid_t dxpl_id,
    int indent, int fwidth, const void *record, const void *u_ctx);

/* Helper routine */
static herr_t H5D_bt2_idx_open(const H5D_chk_idx_info_t *idx_info);

/* Callback for H5B2_iterate() which is called in H5D_bt2_idx_iterate() */
static int H5D_bt2_idx_iterate_cb(const void *_record, void *_udata);

/* Callbacks for H5B2_find() which are called in H5D_bt2_idx_get_addr() and H5D_bt2_idx_insert() */
static herr_t H5D_bt2_found_cb(const void *nrecord, void *op_data);
static herr_t H5D_bt2_filt_found_cb(const void *nrecord, void *op_data);

/*  
 * Callbacks for H5B2_remove() and H5B2_delete() which are called
 * in H5D_bt2_idx_remove() and H5D_bt2_idx_delete().
 */
static herr_t H5D_bt2_remove_cb(const void *nrecord, void *_udata);
static herr_t H5D_bt2_filt_remove_cb(const void *nrecord, void *_udata);

/* Callback for H5B2_modify() which is called in H5D_bt2_idx_insert() */
static herr_t H5D_bt2_mod_filt_cb(void *_record, void *_op_data, hbool_t *changed);

/* Chunked layout indexing callbacks for v2 B-tree indexing */
static herr_t H5D_bt2_idx_init(const H5D_chk_idx_info_t *idx_info,
    const H5S_t *space, haddr_t dset_ohdr_addr);
static herr_t H5D_bt2_idx_create(const H5D_chk_idx_info_t *idx_info);
static hbool_t H5D_bt2_idx_is_space_alloc(const H5O_storage_chunk_t *storage);
static herr_t H5D_bt2_idx_insert(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static herr_t H5D_bt2_idx_get_addr(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata);
static int H5D_bt2_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata);
static herr_t H5D_bt2_idx_remove(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_common_ud_t *udata);
static herr_t H5D_bt2_idx_delete(const H5D_chk_idx_info_t *idx_info);
static herr_t H5D_bt2_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst);
static herr_t H5D_bt2_idx_copy_shutdown(H5O_storage_chunk_t *storage_src,
    H5O_storage_chunk_t *storage_dst, hid_t dxpl_id);
static herr_t H5D_bt2_idx_size(const H5D_chk_idx_info_t *idx_info, hsize_t *size);
static herr_t H5D_bt2_idx_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr);
static htri_t H5D_bt2_idx_support(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata, H5AC_info_t *child_entry);
static herr_t H5D_bt2_idx_unsupport(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_ud_t *udata, H5AC_info_t *child_entry);
static herr_t H5D_bt2_idx_dump(const H5O_storage_chunk_t *storage,
    FILE *stream);
static herr_t H5D_bt2_idx_dest(const H5D_chk_idx_info_t *idx_info);


/*********************/
/* Package Variables */
/*********************/

/* Chunked dataset I/O ops for v2 B-tree indexing */
const H5D_chunk_ops_t H5D_COPS_BT2[1] = {{
    TRUE,
    H5D_bt2_idx_init, 
    H5D_bt2_idx_create,
    H5D_bt2_idx_is_space_alloc,
    H5D_bt2_idx_insert,
    H5D_bt2_idx_get_addr,
    NULL,
    H5D_bt2_idx_iterate,
    H5D_bt2_idx_remove,
    H5D_bt2_idx_delete,
    H5D_bt2_idx_copy_setup,
    H5D_bt2_idx_copy_shutdown,
    H5D_bt2_idx_size,
    H5D_bt2_idx_reset,
    H5D_bt2_idx_support,
    H5D_bt2_idx_unsupport,
    H5D_bt2_idx_dump, 
    H5D_bt2_idx_dest 
}};


/*****************************/
/* Library Private Variables */
/*****************************/

/* v2 B-tree class for indexing non-filtered chunked datasets */
const H5B2_class_t H5D_BT2[1] = {{  	/* B-tree class information */
    H5B2_CDSET_ID,             	/* Type of B-tree */
    "H5B2_CDSET_ID",           	/* Name of B-tree class */
    sizeof(H5D_bt2_rec_t),	/* Size of native record */
    H5D_bt2_crt_context,      	/* Create client callback context */
    H5D_bt2_dst_context,       	/* Destroy client callback context */
    H5D_bt2_store,    		/* Record storage callback */
    H5D_bt2_compare,   		/* Record comparison callback */
    H5D_bt2_encode,    		/* Record encoding callback */
    H5D_bt2_decode,    		/* Record decoding callback */
    H5D_bt2_crt_flush_dep,      /* Create flush dependency */
    H5D_bt2_upd_flush_dep,      /* Update flush dependency */
    H5D_bt2_debug,  		/* Record debugging callback */
    H5D_bt2_crt_dbg_context,  	/* Create debugging context */
    H5D_bt2_dst_dbg_context      /* Destroy debugging context */
}};

/* v2 B-tree class for indexing filtered chunked datasets */
const H5B2_class_t H5D_BT2_FILT[1] = {{	/* B-tree class information */
    H5B2_CDSET_FILT_ID, 	/* Type of B-tree */
    "H5B2_CDSET_FILT_ID",      	/* Name of B-tree class */
    sizeof(H5D_bt2_filt_rec_t), 	/* Size of native record */
    H5D_bt2_crt_context,       	/* Create client callback context */
    H5D_bt2_dst_context,       	/* Destroy client callback context */
    H5D_bt2_filt_store,        	/* Record storage callback */
    H5D_bt2_filt_compare,      	/* Record comparison callback */
    H5D_bt2_filt_encode,       	/* Record encoding callback */
    H5D_bt2_filt_decode,       	/* Record decoding callback */
    H5D_bt2_filt_crt_flush_dep, /* Create flush dependency */
    H5D_bt2_filt_upd_flush_dep, /* Update flush dependency */
    H5D_bt2_filt_debug,        	/* Record debugging callback */
    H5D_bt2_crt_dbg_context,   	/* Create debugging context */
    H5D_bt2_dst_dbg_context   	/* Destroy debugging context */
}};


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5D_bt2_ctx_t struct */
H5FL_DEFINE_STATIC(H5D_bt2_ctx_t);
/* Declare a free list to manage the H5D_bt2_ctx_ud_t struct */
H5FL_DEFINE_STATIC(H5D_bt2_ctx_ud_t);



/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_crt_context
 *
 * Purpose:     Create client callback context
 *
 * Return:      Success:        non-NULL
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static void *
H5D_bt2_crt_context(void *_udata)
{
    H5D_bt2_ctx_ud_t *udata = (H5D_bt2_ctx_ud_t *)_udata; /* User data for building callback context */
    H5D_bt2_ctx_t *ctx;   	/* Callback context structure */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(udata);
    HDassert(udata->f);
    HDassert(udata->ndims > 0 && udata->ndims < H5O_LAYOUT_NDIMS);

    /* Allocate callback context */
    if(NULL == (ctx = H5FL_MALLOC(H5D_bt2_ctx_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, NULL, "can't allocate callback context")

    /* Determine the size of addresses and # of dimensions for the dataset */
    ctx->sizeof_addr = H5F_SIZEOF_ADDR(udata->f);
    ctx->ndims = udata->ndims;

    /* 
     * Compute the size required for encoding the size of a chunk,
     * allowing for an extra byte, in case the filter makes the chunk larger.
     */
    ctx->chunk_size_len = 1 + ((H5V_log2_gen((uint64_t)udata->chunk_size) + 8) / 8);
    if(ctx->chunk_size_len > 8)
        ctx->chunk_size_len = 8;

    /* Set return value */
    ret_value = ctx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_crt_context() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_dst_context
 *
 * Purpose:     Destroy client callback context
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_dst_context(void *_ctx)
{
    H5D_bt2_ctx_t *ctx = (H5D_bt2_ctx_t *)_ctx;       /* Callback context structure */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ctx);

    /* Release callback context */
    ctx = H5FL_FREE(H5D_bt2_ctx_t, ctx);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_dst_context() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_store
 *
 * Purpose:     Store native information into record for v2 B-tree
 *		(non-filtered)
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_store(void *record, const void *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata;	/* User data */

    *(H5D_bt2_rec_t *)record = *(const H5D_bt2_rec_t *)udata->rec;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_store() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_compare
 *
 * Purpose:     Compare two native information records, according to some key
 *		(non-filtered)
 *
 * Return:      <0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_compare(const void *_udata, const void *_rec2)
{
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata;	/* User data */

    const H5D_bt2_rec_t *rec1 = (const H5D_bt2_rec_t *)udata->rec;	/* The native record */
    const H5D_bt2_rec_t *rec2 = (const H5D_bt2_rec_t *)_rec2;		/* The native record */
    herr_t ret_value; 	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(rec1);
    HDassert(rec2);

    /* Compare the offsets but ignore the other fields */
    ret_value = H5V_vector_cmp_u(udata->ndims, rec1->offset, rec2->offset);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_compare() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_encode
 *
 * Purpose:     Encode native information into raw form for storing on disk
 *		(non-filtered)
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_encode(uint8_t *raw, const void *_record, void *_ctx)
{
    H5D_bt2_ctx_t *ctx = (H5D_bt2_ctx_t *)_ctx;	/* Callback context structure */
    const H5D_bt2_rec_t *record = (const H5D_bt2_rec_t *)_record; /* The native record */
    unsigned	i;	/* Local index varible */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ctx);

    /* Encode the record's fields */
    H5F_addr_encode_len(ctx->sizeof_addr, &raw, record->addr);
    for(i = 0; i < ctx->ndims; i++)
	UINT64ENCODE(raw, record->offset[i]);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_decode
 *
 * Purpose:     Decode raw disk form of record into native form
 *		(non-filtered)
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_decode(const uint8_t *raw, void *_record, void *_ctx)
{
    H5D_bt2_ctx_t *ctx = (H5D_bt2_ctx_t *)_ctx;       	/* Callback context structure */
    H5D_bt2_rec_t *record = (H5D_bt2_rec_t *)_record;	/* The native record */
    unsigned	i;	/* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ctx);

    /* Decode the record's fields */
    H5F_addr_decode_len(ctx->sizeof_addr, &raw, &record->addr);
    for(i = 0; i < ctx->ndims; i++)
	UINT64DECODE(raw, record->offset[i]);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_crt_flush_dep
 *
 * Purpose:     Creates a flush dependency between the specified chunk
 *              (child) and parent.
 *
 * Return:      Success:        0
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 1, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_crt_flush_dep(void *_record, void *_udata, void *parent)
{
    H5D_bt2_rec_t *record = (H5D_bt2_rec_t *)_record;   /* The native record */
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata; /* User data */
    int         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(record);
    HDassert(udata);
    HDassert(udata->layout->ndims > 0 && udata->layout->ndims <= H5O_LAYOUT_NDIMS);
    HDassert(parent);

    /* If there is no rdcc, then there are no cached chunks to create
     * dependencies on.  This should only happen when copying */
    if(udata->rdcc)
        /* Delegate to chunk routine */
        if(H5D__chunk_create_flush_dep(udata->rdcc, udata->layout, record->offset, parent) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_crt_flush_dep() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_upd_flush_dep
 *
 * Purpose:     Updates the flush dependency of the specified chunk from
 *              old_parent to new_parent, but only if the current parent
 *              is cached.  If the chunk is not cached, does nothing.
 *
 * Return:      Success:        0
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 1, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_upd_flush_dep(void *_record, void *_udata, void *old_parent,
    void *new_parent)
{
    H5D_bt2_rec_t *record = (H5D_bt2_rec_t *)_record;   /* The native record */
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata; /* User data */
    int         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(record);
    HDassert(udata);
    HDassert(udata->layout->ndims > 0 && udata->layout->ndims <= H5O_LAYOUT_NDIMS);
    HDassert(old_parent);
    HDassert(new_parent);

    /* If there is no rdcc, then there are no cached chunks to update
     * dependencies.  This should only happen when copying */
    if(udata->rdcc)
        /* Delegate to chunk routine */
        if(H5D__chunk_update_flush_dep(udata->rdcc, udata->layout, record->offset, old_parent, new_parent) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to update flush dependency")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_upd_flush_dep() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_debug
 *
 * Purpose:	Debug native form of record (non-filtered)
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_record, const void *_u_ctx)
{
    const H5D_bt2_rec_t *record = (const H5D_bt2_rec_t *)_record; /* The native record */
    const H5D_bt2_ctx_ud_t *u_ctx = (const H5D_bt2_ctx_ud_t *)_u_ctx; 	  /* User data for creating callback context */
    unsigned u;		/* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(record);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth, "Chunk address:", (unsigned)record->addr);
    HDfprintf(stream, "%*s%-*s {", indent, "", fwidth, "Logical offset:");
    for(u = 0; u < u_ctx->ndims; u++)
        HDfprintf(stream, "%s%Hd", u?", ":"", record->offset[u]);
    HDfputs("}\n", stream);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_debug() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_filt_store
 *
 * Purpose:     Store native information into record for v2 B-tree
 *		(filtered)
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_store(void *record, const void *_udata)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata;	/* User data */

    *(H5D_bt2_filt_rec_t *)record = *(const H5D_bt2_filt_rec_t *)udata->rec;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_filt_store() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_filt_compare
 *
 * Purpose:     Compare two native information records, according to some key
 *		(filtered)
 *
 * Return:      <0 if rec1 < rec2
 *              =0 if rec1 == rec2
 *              >0 if rec1 > rec2
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_compare(const void *_udata, const void *_rec2)
{
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata;	/* User data */

    const H5D_bt2_filt_rec_t *rec1 = (const H5D_bt2_filt_rec_t *)udata->rec;	/* The native record */
    const H5D_bt2_filt_rec_t *rec2 = (const H5D_bt2_filt_rec_t *)_rec2;		/* The native record */
    herr_t ret_value;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(rec1);
    HDassert(rec2);

    /* Compare the offsets but ignore the other fields */
    ret_value = H5V_vector_cmp_u(udata->ndims, rec1->offset, rec2->offset);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_filt_compare() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_filt_encode
 *
 * Purpose:     Encode native information into raw form for storing on disk
 *		(filtered)
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_encode(uint8_t *raw, const void *_record, void *_ctx)
{
    H5D_bt2_ctx_t *ctx = (H5D_bt2_ctx_t *)_ctx;	/* Callback context structure */
    const H5D_bt2_filt_rec_t *record = (const H5D_bt2_filt_rec_t *)_record;  /* The native record */
    unsigned i;	/* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ctx);

    /* Encode the record's fields */
    H5F_addr_encode_len(ctx->sizeof_addr, &raw, record->addr);
    UINT64ENCODE_VAR(raw, record->nbytes, ctx->chunk_size_len);
    UINT32ENCODE(raw, record->filter_mask);
    for(i = 0; i < ctx->ndims; i++)
	UINT64ENCODE(raw, record->offset[i]);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_filt_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_filt_decode
 *
 * Purpose:	Decode raw disk form of record into native form
 *		(filtered)
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_decode(const uint8_t *raw, void *_record, void *_ctx)
{
    H5D_bt2_ctx_t *ctx = (H5D_bt2_ctx_t *)_ctx;       		/* Callback context structure */
    H5D_bt2_filt_rec_t *record = (H5D_bt2_filt_rec_t *)_record;	/* The native record */
    unsigned i;	/* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(ctx);

    /* Decode the record's fields */
    H5F_addr_decode_len(ctx->sizeof_addr, &raw, &record->addr);
    UINT64DECODE_VAR(raw, record->nbytes, ctx->chunk_size_len);
    UINT32DECODE(raw, record->filter_mask);
    for(i = 0; i < ctx->ndims; i++)
	UINT64DECODE(raw, record->offset[i]);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_filt_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_filt_crt_flush_dep
 *
 * Purpose:     Creates a flush dependency between the specified chunk
 *              (child) and parent.
 *
 * Return:      Success:        0
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 1, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_crt_flush_dep(void *_record, void *_udata, void *parent)
{
    H5D_bt2_filt_rec_t *record = (H5D_bt2_filt_rec_t *)_record; /* The native record */
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata; /* User data */
    int         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(record);
    HDassert(udata);
    HDassert(udata->layout->ndims > 0 && udata->layout->ndims <= H5O_LAYOUT_NDIMS);
    HDassert(parent);

    /* If there is no rdcc, then there are no cached chunks to create
     * dependencies on.  This should only happen when copying */
    if(udata->rdcc)
        /* Delegate to chunk routine */
        if(H5D__chunk_create_flush_dep(udata->rdcc, udata->layout, record->offset, parent) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_filt_crt_flush_dep() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_filt_upd_flush_dep
 *
 * Purpose:     Updates the flush dependency of the specified chunk from
 *              old_parent to new_parent, but only if the current parent
 *              is cached.  If the chunk is not cached, does nothing.
 *
 * Return:      Success:        0
 *              Failure:        FAIL
 *
 * Programmer:  Neil Fortner
 *              Tuesday, May 1, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_upd_flush_dep(void *_record, void *_udata, void *old_parent,
    void *new_parent)
{
    H5D_bt2_filt_rec_t *record = (H5D_bt2_filt_rec_t *)_record; /* The native record */
    const H5D_bt2_find_ud_t *udata = (const H5D_bt2_find_ud_t *)_udata; /* User data */
    int         ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(record);
    HDassert(udata);
    HDassert(udata->layout->ndims > 0 && udata->layout->ndims <= H5O_LAYOUT_NDIMS);
    HDassert(old_parent);
    HDassert(new_parent);

    /* If there is no rdcc, then there are no cached chunks to update
     * dependencies.  This should only happen when copying */
    if(udata->rdcc)
        /* Delegate to chunk routine */
        if(H5D__chunk_update_flush_dep(udata->rdcc, udata->layout, record->offset, old_parent, new_parent) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to update flush dependency")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_btree_filt_upd_flush_dep() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_filt_debug
 *
 * Purpose:	Debug native form of record (filterd)
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_debug(FILE *stream, const H5F_t UNUSED *f, hid_t UNUSED dxpl_id,
    int indent, int fwidth, const void *_record, const void *_u_ctx)
{
    const H5D_bt2_filt_rec_t *record = (const H5D_bt2_filt_rec_t *)_record; /* The native record */
    const H5D_bt2_ctx_ud_t *u_ctx = (const H5D_bt2_ctx_ud_t *)_u_ctx; /* User data for creating callback context */
    unsigned u;		/* Local index variable */
 
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(record);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth, "Chunk address:", (unsigned)record->addr);
    HDfprintf(stream, "%*s%-*s %u bytes\n", indent, "", fwidth, "Chunk size:", (unsigned)record->nbytes);
    HDfprintf(stream, "%*s%-*s 0x%08x\n", indent, "", fwidth, "Filter mask:", record->filter_mask);

    HDfprintf(stream, "%*s%-*s {", indent, "", fwidth, "Logical offset:");
    for(u = 0; u < u_ctx->ndims; u++)
        HDfprintf(stream, "%s%Hd", u?", ":"", record->offset[u]);
    HDfputs("}\n", stream);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_filt_debug() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_crt_dbg_context
 *
 * Purpose:     Create user data for debugged callback context 
 *
 * Return:      Success:        non-NULL
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static void *
H5D_bt2_crt_dbg_context(H5F_t *f, hid_t UNUSED dxpl_id, haddr_t obj_addr)
{
    H5D_bt2_ctx_ud_t *u_ctx;   	/* User data for creating callback context */
    H5O_loc_t obj_loc;          /* Pointer to an object's location */
    hbool_t obj_opened = FALSE; /* Flag to indicate that the object header was opened */
    H5O_layout_t layout;        /* Layout message */
    void *ret_value;      	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(obj_addr));

    /* Set up the object header location info */
    H5O_loc_reset(&obj_loc);
    obj_loc.file = f;
    obj_loc.addr = obj_addr;

    /* Open the object header where the layout message resides */
    if(H5O_open(&obj_loc) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, NULL, "can't open object header")
    obj_opened = TRUE;

    /* Read the layout message */
    if(NULL == H5O_msg_read(&obj_loc, H5O_LAYOUT_ID, &layout, dxpl_id))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, NULL, "can't get layout info")

    /* close the object header */
    if(H5O_close(&obj_loc) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, NULL, "can't close object header")

    /* Allocate structure for storing user data to create callback context */
    if(NULL == (u_ctx = H5FL_MALLOC(H5D_bt2_ctx_ud_t)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate user data context structure ")

    /* Set information for context structure */
    u_ctx->f = f;
    u_ctx->chunk_size = layout.u.chunk.size;
    u_ctx->ndims = layout.u.chunk.ndims - 1;

    /* Set return value */
    ret_value = u_ctx;

done:
     /* Cleanup on error */
    if(ret_value == NULL) {
        /* Release context structure */
        if(u_ctx)
            u_ctx = H5FL_FREE(H5D_bt2_ctx_ud_t, u_ctx);

        /* Close object header */
        if(obj_opened) {
            if(H5O_close(&obj_loc) < 0)
                HDONE_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, NULL, "can't close object header")
        } /* end if */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_crt_dbg_context() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_dst_dbg_context
 *
 * Purpose:     Destroy client callback context
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_dst_dbg_context(void *_u_ctx)
{
    H5D_bt2_ctx_ud_t *u_ctx = (H5D_bt2_ctx_ud_t *)_u_ctx; /* User data for creating callback context */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(u_ctx);

    /* Release user data for creating callback context */
    u_ctx = H5FL_FREE(H5D_bt2_ctx_ud_t, u_ctx);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_dst_dbg_context() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_idx_init
 *
 * Purpose:     Initialize the indexing information for a dataset.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Wednensday, May 23, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_init(const H5D_chk_idx_info_t UNUSED *idx_info,
    const H5S_t UNUSED *space, haddr_t dset_ohdr_addr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(H5F_addr_defined(dset_ohdr_addr));

    idx_info->storage->u.btree2.dset_ohdr_addr = dset_ohdr_addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_bt2_idx_init() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_open()
 *
 * Purpose:	Opens an existing v2 B-tree.
 *
 * Note:	This information is passively initialized from each index
 *              operation callback because those abstract chunk index operations
 *              are designed to work with the v2 B-tree chunk indices also,
 *              which don't require an 'open' for the data structure.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_open(const H5D_chk_idx_info_t *idx_info)
{
    H5D_bt2_ctx_ud_t u_ctx;	/* user data for creating context */
    H5O_loc_t oloc;             /* Temporary object header location for dataset */
    H5O_proxy_t *oh_proxy = NULL; /* Dataset's object header proxy */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(H5D_CHUNK_IDX_BT2 == idx_info->layout->idx_type);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(NULL == idx_info->storage->u.btree2.bt2);

    /* Set up the user data */
    u_ctx.f = idx_info->f;
    u_ctx.ndims = idx_info->layout->ndims - 1;
    u_ctx.chunk_size = idx_info->layout->size;

    /* Check for SWMR writes to the file */
    if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
        /* Set up object header location for dataset */
        H5O_loc_reset(&oloc);
        oloc.file = idx_info->f;
        oloc.addr = idx_info->storage->u.btree.dset_ohdr_addr;

        /* Pin the dataset's object header proxy */
        if(NULL == (oh_proxy = H5O_pin_flush_dep_proxy(&oloc, idx_info->dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTPIN, FAIL, "unable to pin dataset object header proxy")
    } /* end if */

    /* Open v2 B-tree for the chunk index */
    if(NULL == (idx_info->storage->u.btree2.bt2 = H5B2_open(idx_info->f, idx_info->dxpl_id, idx_info->storage->idx_addr , &u_ctx, oh_proxy)))
	HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't open v2 B-tree for tracking chunked dataset")

done:
    if(oh_proxy && H5O_unpin_flush_dep_proxy(oh_proxy) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTUNPIN, FAIL, "unable to unpin dataset object header proxy")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_open() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_create 
 *
 * Purpose:	Create the v2 B-tree for tracking dataset chunks 
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_create(const H5D_chk_idx_info_t *idx_info)
{
    H5B2_create_t bt2_cparam;           /* v2 B-tree creation parameters */
    H5D_bt2_ctx_ud_t u_ctx;		/* data for context call */
    H5O_loc_t oloc;                     /* Temporary object header location for dataset */
    H5O_proxy_t *oh_proxy = NULL;       /* Dataset's object header proxy */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(!H5F_addr_defined(idx_info->storage->idx_addr));

    bt2_cparam.rrec_size = H5F_SIZEOF_ADDR(idx_info->f)	/* Address of chunk */
		  + (idx_info->layout->ndims - 1) * 8;	/* # of dimensions x 64-bit chunk offsets */

    /* General parameters */
    if(idx_info->pline->nused > 0) {
	unsigned chunk_size_len;        /* Size of encoded chunk size */

        /* 
	 * Compute the size required for encoding the size of a chunk,
         * allowing for an extra byte, in case the filter makes the chunk larger.
         */
        chunk_size_len = 1 + ((H5V_log2_gen((uint64_t)idx_info->layout->size) + 8) / 8);
        if(chunk_size_len > 8)
            chunk_size_len = 8;

	bt2_cparam.rrec_size  += chunk_size_len	+ 4;	/* Size of encoded chunk size & filter mask */
	bt2_cparam.cls = H5D_BT2_FILT;
    } /* end if */
    else
	bt2_cparam.cls = H5D_BT2;

    bt2_cparam.node_size = idx_info->layout->u.btree2.cparam.node_size;
    bt2_cparam.split_percent = idx_info->layout->u.btree2.cparam.split_percent;
    bt2_cparam.merge_percent = idx_info->layout->u.btree2.cparam.merge_percent;

    u_ctx.f = idx_info->f;
    u_ctx.ndims = idx_info->layout->ndims - 1;
    u_ctx.chunk_size = idx_info->layout->size;

    /* Check for SWMR writes to the file */
    if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
        /* Set up object header location for dataset */
        H5O_loc_reset(&oloc);
        oloc.file = idx_info->f;
        oloc.addr = idx_info->storage->u.btree.dset_ohdr_addr;

        /* Pin the dataset's object header proxy */
        if(NULL == (oh_proxy = H5O_pin_flush_dep_proxy(&oloc, idx_info->dxpl_id)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTPIN, FAIL, "unable to pin dataset object header proxy")
    } /* end if */

    /* Create the v2 B-tree for the chunked dataset */
    if(NULL == (idx_info->storage->u.btree2.bt2 = H5B2_create(idx_info->f, idx_info->dxpl_id, &bt2_cparam, &u_ctx, oh_proxy)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCREATE, FAIL, "can't create v2 B-tree for tracking chunked dataset")

    /* Retrieve the v2 B-tree's address in the file */
    if(H5B2_get_addr(idx_info->storage->u.btree2.bt2, &(idx_info->storage->idx_addr)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get v2 B-tree address for tracking chunked dataset")

done:
    if(oh_proxy && H5O_unpin_flush_dep_proxy(oh_proxy) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTUNPIN, FAIL, "unable to unpin dataset object header proxy")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_create() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_is_space_alloc
 *
 * Purpose:	Query if space is allocated for index method
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5D_bt2_idx_is_space_alloc(const H5O_storage_chunk_t *storage)
{
    hbool_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(storage);

    /* Set return value */
    ret_value = (hbool_t)H5F_addr_defined(storage->idx_addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_mod_filt_cb
 *
 * Purpose:	Modify record (filtered) for dataset chunk when it is found 
 *		in the v2 B-tree.
 * 		This is the callback for H5B2_modify() which is called in 
 *		H5D_bt2_idx_insert().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_mod_filt_cb(void *_record, void *_op_data, hbool_t *changed)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    *(H5D_bt2_filt_rec_t *)_record = *(H5D_bt2_filt_rec_t *)_op_data;

    /* Note that the record changed */
    *changed = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_bt2_mod_filt_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_insert
 *
 * Purpose:	A non-filtered chunk: 
 *		  Should not exist
 *		  Allocate the chunk and pass chunk address back up
 *		A filtered chunk:
 *		  If it was not found, create the chunk and pass chunk address back up
 *		  If it was found but its size changed, reallocate the chunk and pass chunk address back up
 *		  If it was found but its size was the same, pass chunk address back up
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_insert(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    H5B2_t *bt2;                        /* v2 B-tree handle for indexing chunks */
    H5D_bt2_find_ud_t bt2_udata;        /* User data for v2 B-tree calls */
    unsigned u;				/* Local index variable */
    herr_t ret_value = SUCCEED;		/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    /* Check if the v2 B-tree is open yet */
    if(NULL == idx_info->storage->u.btree2.bt2) {
	/* Open existing v2 B-tree */
        if(H5D_bt2_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")
    } /* end if */

    /* Set convenience pointer to v2 B-tree structure */
    bt2 = idx_info->storage->u.btree2.bt2;

    /* Check for filters on chunks */
    if(idx_info->pline->nused > 0) { /* filtered chunk */
	H5D_bt2_filt_rec_t rec;  	/* Record for searching object */
	H5D_bt2_filt_rec_t found_rec;  	/* Record found from searching for object */
	unsigned allow_chunk_size_len;	/* Allowed size of encoded chunk size */
        unsigned new_chunk_size_len;  	/* Size of encoded chunk size */

	/* 
	 * Compute the size required for encoding the size of a chunk,
         * allowing for an extra byte, in case the filter makes the chunk larger.
         */
        allow_chunk_size_len = 1 + ((H5V_log2_gen((uint64_t)idx_info->layout->size) + 8) / 8);
        if(allow_chunk_size_len > 8)
            allow_chunk_size_len = 8;

        /* Compute encoded size of chunk */
        new_chunk_size_len = (H5V_log2_gen((uint64_t)udata->nbytes) + 8) / 8;
        if(new_chunk_size_len > 8)
            HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "encoded chunk size is more than 8 bytes?!?")

        /* Check if the chunk became too large to be encoded */
        if(new_chunk_size_len > allow_chunk_size_len)
            HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "chunk size can't be encoded")

	/* Initialize record information */
        rec.nbytes = udata->nbytes;
        rec.filter_mask = udata->filter_mask;
	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    rec.offset[u] = udata->common.offset[u];

	found_rec.addr = HADDR_UNDEF;
	found_rec.nbytes = 0;
	found_rec.filter_mask = 0;

	/* Prepare user data for compare callback */
	bt2_udata.rec = &rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
	bt2_udata.layout = idx_info->layout;
	bt2_udata.rdcc = udata->common.rdcc;

	/* Try to find the chunked record */
	if(H5B2_find(bt2, idx_info->dxpl_id, &bt2_udata, H5D_bt2_filt_found_cb, &found_rec) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_NOTFOUND, FAIL, "can't find object in v2 B-tree")

        /* Check for previous chunk */
        if(H5F_addr_defined(found_rec.addr)) { /* Found it */
            /* Sanity check */
            HDassert(!H5F_addr_defined(udata->addr) || H5F_addr_eq(udata->addr, found_rec.addr));

            /* Check for chunk being same size */
            if(udata->nbytes != found_rec.nbytes) {
		/* Free the original chunk if not doing SWMR writes */
		if(!(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE)) {
                    H5_CHECK_OVERFLOW(found_rec.nbytes, uint32_t, hsize_t);
                    if(H5MF_xfree(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, found_rec.addr, (hsize_t)found_rec.nbytes) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk")
                } /* end if */

		/* Allocate a new chunk */
		H5_CHECK_OVERFLOW(udata->nbytes, uint32_t, hsize_t);
                udata->addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, (hsize_t)udata->nbytes);
                if(!H5F_addr_defined(udata->addr))
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")
                rec.addr = udata->addr;

		/* Modify record for object in v2 B-tree */
                if(H5B2_modify(bt2, idx_info->dxpl_id, &bt2_udata, H5D_bt2_mod_filt_cb, &rec) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "unable to modify record in v2 B-tree")
            } /* end if */
            else
                /* Don't need to reallocate chunk, but send its address back up */
		udata->addr = found_rec.addr;
        } /* end if */
        else { /* Not found */
            H5_CHECK_OVERFLOW(udata->nbytes, uint32_t, hsize_t);

	    /* Allocate a new chunk */
            udata->addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, (hsize_t)udata->nbytes);
            if(!H5F_addr_defined(udata->addr))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")
            rec.addr = udata->addr;

	    /* Insert record for object in v2 B-tree */
            if(H5B2_insert(bt2, idx_info->dxpl_id, &bt2_udata) < 0)                
		HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "couldn't insert record in v2 B-tree")
	} /* end else */
    } /* end if */
    else { /* non-filtered chunk */
	H5D_bt2_rec_t rec;  	

	/* The record should not be there */
	HDassert(!H5F_addr_defined(udata->addr));
        HDassert(udata->nbytes == idx_info->layout->size);

	/* Prepare user data for compare callback */
	bt2_udata.rec = &rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;

	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    rec.offset[u] = udata->common.offset[u];

	/* Allocate storage for the new chunk */
	H5_CHECK_OVERFLOW(udata->nbytes, uint32_t, hsize_t);
        udata->addr = H5MF_alloc(idx_info->f, H5FD_MEM_DRAW, idx_info->dxpl_id, (hsize_t)udata->nbytes);
	if(!H5F_addr_defined(udata->addr))
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")
        rec.addr = udata->addr;

	/* Insert record for object in v2 B-tree */
	if(H5B2_insert(bt2, idx_info->dxpl_id, &bt2_udata) < 0)                
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL, "couldn't insert record in v2 B-tree")
    } /* end else */
    HDassert(H5F_addr_defined(udata->addr));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_idx_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_found_cb
 *
 * Purpose:	Retrieve record (non-filtered) for dataset chunk when it is found 
 *		in the v2 B-tree.
 * 		This is the callback for H5B2_find() which is called in 
 *		H5D_bt2_idx_get_addr() and H5D_bt2_idx_insert().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_found_cb(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    *(H5D_bt2_rec_t *)op_data = *(const H5D_bt2_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_found_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_filt_found_cb
 *
 * Purpose:	Retrieve record (filtered) for dataset chunk when it is found 
 *		in the v2 B-tree.
 * 		This is the callback for H5B2_find() which is called in 
 *		H5D_bt2_idx_get_addr() and H5D_bt2_idx_insert().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_found_cb(const void *nrecord, void *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    *(H5D_bt2_filt_rec_t *)op_data = *(const H5D_bt2_filt_rec_t *)nrecord;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5D_bt2_filt_found_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_get_addr
 *
 * Purpose:	Get the file address of a chunk if file space has been
 *		assigned.  Save the retrieved information in the udata
 *		supplied.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_get_addr(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata)
{
    H5B2_t 	*bt2;                   /* v2 B-tree handle for indexing chunks */
    H5D_bt2_find_ud_t bt2_udata;        /* User data for v2 B-tree calls */
    unsigned	u;			/* Local index variable */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->layout->ndims > 0);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    /* Check if the v2 B-tree is open yet */
    if(NULL == idx_info->storage->u.btree2.bt2) {
	/* Open existing v2 B-tree */
        if(H5D_bt2_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")
    } /* end if */

    /* Set convenience pointer to v2 B-tree structure */
    bt2 = idx_info->storage->u.btree2.bt2;

    /* Check for filters on chunks */
    if(idx_info->pline->nused > 0) { /* filtered chunk */
	H5D_bt2_filt_rec_t search_rec;  /* Record for searching object */
	H5D_bt2_filt_rec_t found_rec;  	/* Record found from searching for object */

	/* Set the chunk offset to be searched for */
	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    search_rec.offset[u] = udata->common.offset[u];

	found_rec.addr = HADDR_UNDEF;
	found_rec.nbytes = 0;
	found_rec.filter_mask = 0;

	/* Prepare user data for compare callback */
	bt2_udata.rec = &search_rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;

	/* Go get chunk information from v2 B-tree */
	if(H5B2_find(bt2, idx_info->dxpl_id, &bt2_udata, H5D_bt2_filt_found_cb, &found_rec) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "can't find object in v2 B-tree")

	/* Set info for the chunk */
        udata->addr = found_rec.addr;
        udata->nbytes = found_rec.nbytes;
        udata->filter_mask = found_rec.filter_mask;
    } /* end if */
    else { /* non-filtered chunk */
	H5D_bt2_rec_t   search_rec; /* Record for searching object */
	H5D_bt2_rec_t   found_rec;  /* Record found from searching for object */

	/* Set the chunk offset to be searched for */
	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    search_rec.offset[u] = udata->common.offset[u];

	search_rec.addr = HADDR_UNDEF;
	found_rec.addr = HADDR_UNDEF;

	/* Prepare user data for compare callback */
	bt2_udata.rec = &search_rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;

	/* Go get chunk information from v2 B-tree */
	if(H5B2_find(bt2, idx_info->dxpl_id, &bt2_udata, H5D_bt2_found_cb, &found_rec) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "can't find object in v2 B-tree")

	udata->addr = found_rec.addr;

	/* Update the other (constant) information for the chunk */
        udata->nbytes = idx_info->layout->size;
        udata->filter_mask = 0;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_idx_get_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_iterate_cb
 *
 * Purpose:	Translate the B-tree specific chunk record into a generic
 *              form and make the callback to the generic chunk callback
 *              routine.
 * 		This is the callback for H5B2_iterate() which is called in 
 *		H5D_bt2_idx_iterate().
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5D_bt2_idx_iterate_cb(const void *_record, void *_udata)
{
    H5D_bt2_it_ud_t *udata = (H5D_bt2_it_ud_t *)_udata; /* User data */
    unsigned u;		        /* Local index variable */
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOERR

    /* Compose generic chunk record for callback */
    if(udata->filtered) { /* filtered record */
        const H5D_bt2_filt_rec_t *filt_rec = (const H5D_bt2_filt_rec_t *)_record;

        udata->chunk_rec.chunk_addr = filt_rec->addr;
        udata->chunk_rec.nbytes = filt_rec->nbytes;
        udata->chunk_rec.filter_mask = filt_rec->filter_mask;
	for(u = 0; u < (udata->common.layout->ndims - 1); u++)
	    udata->chunk_rec.offset[u] = filt_rec->offset[u];
    } /* end if */
    else { /* non-filtered record */
        const H5D_bt2_rec_t *rec = (const H5D_bt2_rec_t *)_record;

        udata->chunk_rec.chunk_addr = rec->addr;
        udata->chunk_rec.nbytes = udata->common.layout->size;
        udata->chunk_rec.filter_mask = 0;
	for(u = 0; u < (udata->common.layout->ndims - 1); u++)
	    udata->chunk_rec.offset[u] = rec->offset[u];
    } /* end else */

    /* Make "generic chunk" callback */
    if((ret_value = (udata->cb)(&udata->chunk_rec, udata->udata)) < 0)
        HERROR(H5E_DATASET, H5E_CALLBACK, "failure in generic chunk iterator callback");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_idx_iterate_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_iterate
 *
 * Purpose:	Iterate over the chunks in an index, making a callback
 *              for each one.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5D_bt2_idx_iterate(const H5D_chk_idx_info_t *idx_info,
    H5D_chunk_cb_func_t chunk_cb, void *chunk_udata)
{
    H5B2_t *bt2;	        /* v2 B-tree handle for indexing chunks */
    H5D_bt2_it_ud_t udata; 	/* User data for B-tree iterator callback */
    int ret_value;		/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(chunk_cb);
    HDassert(chunk_udata);

    /* Initialize user data */
    HDmemset(&udata, 0, sizeof udata);
    udata.common.layout = idx_info->layout;
    udata.common.storage = idx_info->storage;
    udata.common.rdcc = NULL;
    udata.filtered = idx_info->pline->nused > 0;
    HDmemset(&udata.chunk_rec, 0, sizeof(udata.chunk_rec));

    /* Check if the v2 B-tree is open yet */
    if(NULL == idx_info->storage->u.btree2.bt2) {
	/* Open existing v2 B-tree */
        if(H5D_bt2_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")
    } /* end if */

    /* Set convenience pointer to v2 B-tree structure */
    bt2 = idx_info->storage->u.btree2.bt2;

    /* Prepare user data for iterate callback */
    udata.cb = chunk_cb;
    udata.udata = chunk_udata;

    /* Iterate over the records in the v2 B-tree */
    if((ret_value = H5B2_iterate(bt2, idx_info->dxpl_id, H5D_bt2_idx_iterate_cb, &udata)) < 0)
	HERROR(H5E_DATASET, H5E_BADITER, "unable to iterate over v2 B-tree for dataset chunks");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_remove_cb()
 *
 * Purpose:	Free space for 'dataset chunk' object as v2 B-tree
 *             	is being deleted or v2 B-tree node is removed.
 * 		This is the callback for H5B2_remove() and H5B2_delete() which 
 *		which are called in H5D_bt2_idx_remove() and H5D_bt2_idx_delete().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_remove_cb(const void *record, void *_udata)
{
    H5D_bt2_remove_ud_t *udata = (H5D_bt2_remove_ud_t *)_udata;	/* User data for removal callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Free the space in the file for the object being removed */
    H5_CHECK_OVERFLOW(udata->unfilt_size, uint32_t, hsize_t);
    if(H5MF_xfree(udata->f, H5FD_MEM_DRAW, udata->dxpl_id, ((const H5D_bt2_rec_t *)record)->addr, (hsize_t)udata->unfilt_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "unable to free chunk")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_filt_remove_cb
 *
 * Purpose:	Free space for 'filtered dataset chunk' object as v2 B-tree
 *              is being deleted or v2 B-tree node is removed
 * 		This is the callback for H5B2_remove() and H5B2_delete() which 
 *		which are called in H5D_bt2_idx_remove() and H5D_bt2_idx_delete().
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_filt_remove_cb(const void *_record, void *_udata)
{
    H5D_bt2_remove_ud_t *udata = (H5D_bt2_remove_ud_t *)_udata;			/* User data for removal callback */
    const H5D_bt2_filt_rec_t *record = (const H5D_bt2_filt_rec_t *)_record;	/* The native record */
    herr_t ret_value = SUCCEED;  	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Free the space in the file for the object being removed */
    H5_CHECK_OVERFLOW(record->nbytes, uint32_t, hsize_t);
    if(H5MF_xfree(udata->f, H5FD_MEM_DRAW, udata->dxpl_id, record->addr, (hsize_t)record->nbytes) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free chunk")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_filt_remove_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_remove
 *
 * Purpose:	Remove chunk from index.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_remove(const H5D_chk_idx_info_t *idx_info, H5D_chunk_common_ud_t *udata)
{
    H5B2_t 	*bt2;                   /* v2 B-tree handle for indexing chunks */
    H5D_bt2_remove_ud_t remove_udata;	/* User data for removal callback */
    H5D_bt2_find_ud_t bt2_udata;        /* User data for v2 B-tree find call */
    unsigned 	u;			/* Local index variable */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);

    /* Check if the v2 B-tree is open yet */
    if(NULL == idx_info->storage->u.btree2.bt2) {
	/* Open existing v2 B-tree */
        if(H5D_bt2_idx_open(idx_info) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")
    } /* end if */

    /* Set convenience pointer to v2 B-tree structure */
    bt2 = idx_info->storage->u.btree2.bt2;

    /* Initialize user data for removal callback */
    remove_udata.f = idx_info->f;
    remove_udata.dxpl_id = idx_info->dxpl_id;

    /* Remove the record for the "dataset chunk" object from the v2 B-tree */
    /* (space in the file for the object is freed in the 'remove' callback) */
    if(idx_info->pline->nused > 0) { /* filtered chunk */
	H5D_bt2_filt_rec_t   search_rec;	/* Record for searching for object */

	/* Initialize the record to search for */
	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    search_rec.offset[u] = udata->offset[u];

	bt2_udata.rec = &search_rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->rdcc;

	if(H5B2_remove(bt2, idx_info->dxpl_id, &bt2_udata, (H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) ? NULL : H5D_bt2_filt_remove_cb, &remove_udata) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREMOVE, FAIL, "can't remove object from B-tree")
    } /* end if */
    else { /* non-filtered chunk */
	H5D_bt2_rec_t   search_rec; 	/* Record for searching for object */

	remove_udata.unfilt_size = idx_info->layout->size;

	/* Initialize the record to search for */
	for(u = 0; u < (idx_info->layout->ndims - 1); u++)
	    search_rec.offset[u] = udata->offset[u];

	bt2_udata.rec = &search_rec;
	bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->rdcc;

	if(H5B2_remove(bt2, idx_info->dxpl_id, &bt2_udata, (H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) ? NULL : H5D_bt2_remove_cb, &remove_udata) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTREMOVE, FAIL, "can't remove object from B-tree")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D_bt2_idx_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_delete
 *
 * Purpose:	Delete index and raw data storage for entire dataset
 *              (i.e. all chunks)
 *
 * Return:	Success:	Non-negative
 *		Failure:	negative
 *
 * Programmer:	Vailin Choi; June 2010
 *
 * Modifications:
 *	Vailin Choi; March 2011
 *	Initialize size of an unfiltered chunk.
 *	This is a fix for for the assertion failure in:
 *	[src/H5FSsection.c:968: H5FS_sect_link_size: Assertion `bin < sinfo->nbins' failed.]
 *	which is uncovered by test_unlink_chunked_dataset() in test/unlink.c
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_delete(const H5D_chk_idx_info_t *idx_info)
{
    H5D_bt2_remove_ud_t remove_udata;	/* User data for removal callback */
    H5B2_remove_t remove_op;		/* The removal callback */
    H5D_bt2_ctx_ud_t u_ctx;		/* data for context call */
    H5O_loc_t oloc;                     /* Temporary object header location for dataset */
    H5O_proxy_t *oh_proxy = NULL;       /* Dataset's object header proxy */
    herr_t ret_value = SUCCEED;     	/* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);

    /* Check if the index data structure has been allocated */
    if(H5F_addr_defined(idx_info->storage->idx_addr)) {
	/* Set up user data for creating context */
	u_ctx.f = idx_info->f;
	u_ctx.ndims = idx_info->layout->ndims - 1;
	u_ctx.chunk_size = idx_info->layout->size;

	/* Initialize user data for removal callback */
	remove_udata.f = idx_info->f;
	remove_udata.dxpl_id = idx_info->dxpl_id;

	/* Set remove operation.  Do not remove chunks in SWMR_WRITE mode */
        if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE)
            remove_op = NULL;
        else {
            if(idx_info->pline->nused > 0) /* filtered */
                remove_op = H5D_bt2_filt_remove_cb;
            else { /* non-filtered */
                remove_op = H5D_bt2_remove_cb;
                remove_udata.unfilt_size = idx_info->layout->size;
            } /* end else */
        } /* end else */

        /* Check for SWMR writes to the file */
        if(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE) {
            /* Set up object header location for dataset */
            H5O_loc_reset(&oloc);
            oloc.file = idx_info->f;
            oloc.addr = idx_info->storage->u.btree.dset_ohdr_addr;

            /* Pin the dataset's object header proxy */
            if(NULL == (oh_proxy = H5O_pin_flush_dep_proxy(&oloc, idx_info->dxpl_id)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTPIN, FAIL, "unable to pin dataset object header proxy")
        } /* end if */

	/* Delete the v2 B-tree */
	/*(space in the file for each object is freed in the 'remove' callback) */
	if(H5B2_delete(idx_info->f, idx_info->dxpl_id, idx_info->storage->idx_addr, &u_ctx, oh_proxy, remove_op, &remove_udata) < 0)
	    HGOTO_ERROR(H5E_DATASET, H5E_CANTDELETE, FAIL, "can't delete v2 B-tree")

	idx_info->storage->idx_addr = HADDR_UNDEF;
    } /* end if */

done:
    if(oh_proxy && H5O_unpin_flush_dep_proxy(oh_proxy) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTUNPIN, FAIL, "unable to unpin dataset object header proxy")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_copy_setup
 *
 * Purpose:	Set up any necessary information for copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_copy_setup(const H5D_chk_idx_info_t *idx_info_src,
    const H5D_chk_idx_info_t *idx_info_dst)
{
    herr_t      ret_value = SUCCEED;        /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Source file */
    HDassert(idx_info_src);
    HDassert(idx_info_src->f);
    HDassert(idx_info_src->pline);
    HDassert(idx_info_src->layout);
    HDassert(idx_info_src->storage);

    /* Destination file */
    HDassert(idx_info_dst);
    HDassert(idx_info_dst->f);
    HDassert(idx_info_dst->pline);
    HDassert(idx_info_dst->layout);
    HDassert(idx_info_dst->storage);
    HDassert(!H5F_addr_defined(idx_info_dst->storage->idx_addr));

    /* Check if the source v2 B-tree is open yet */
    if(NULL == idx_info_src->storage->u.btree2.bt2) {
        if(H5D_bt2_idx_open(idx_info_src) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")
    } /* end if */

    /* Set copied metadata tag */
    H5_BEGIN_TAG(idx_info_dst->dxpl_id, H5AC__COPIED_TAG, FAIL);

    /* Create v2 B-tree that describes the chunked dataset in the destination file */
    if(H5D_bt2_idx_create(idx_info_dst) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to initialize chunked storage")
    HDassert(H5F_addr_defined(idx_info_dst->storage->idx_addr));

    /* Reset metadata tag */
    H5_END_TAG(FAIL);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_copy_setup() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_copy_shutdown
 *
 * Purpose:	Shutdown any information from copying chunks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_copy_shutdown(H5O_storage_chunk_t *storage_src,
    H5O_storage_chunk_t *storage_dst, hid_t UNUSED dxpl_id)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(storage_src);
    HDassert(storage_src->u.btree2.bt2);
    HDassert(storage_dst);
    HDassert(storage_dst->u.btree2.bt2);

    /* Close v2 B-tree for source file */
    if(H5B2_close(storage_src->u.btree2.bt2, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close v2 B-tree")
    storage_src->u.btree2.bt2 = NULL;

    /* Close v2 B-tree for destination file */
    if(H5B2_close(storage_dst->u.btree2.bt2, dxpl_id) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "unable to close v2 B-tree")
    storage_dst->u.btree2.bt2 = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_copy_shutdown() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_idx_size
 *
 * Purpose:     Retrieve the amount of index storage for chunked dataset
 *
 * Return:      Success:        Non-negative
 *              Failure:        negative
 *
 * Programmer:  Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_size(const H5D_chk_idx_info_t *idx_info, hsize_t *index_size)
{
    H5B2_t *bt2_cdset = NULL;		/* Pointer to v2 B-tree structure */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(index_size);

    /* Open v2 B-tree */
    if(H5D_bt2_idx_open(idx_info) < 0)
	HGOTO_ERROR(H5E_DATASET, H5E_CANTOPENOBJ, FAIL, "can't open v2 B-tree")

    /* Set convenience pointer to v2 B-tree structure */
    bt2_cdset = idx_info->storage->u.btree2.bt2;

    /* Get v2 B-tree size for indexing chunked dataset */
    if(H5B2_size(bt2_cdset, idx_info->dxpl_id, index_size) < 0)
	HGOTO_ERROR(H5E_SYM, H5E_CANTGET, FAIL, "can't retrieve v2 B-tree storage info for chunked dataset")
done:
    /* Close v2 B-tree index */
    if(bt2_cdset && H5B2_close(bt2_cdset, idx_info->dxpl_id) < 0)
        HDONE_ERROR(H5E_SYM, H5E_CLOSEERROR, FAIL, "can't close v2 B-tree for tracking chunked dataset")
    idx_info->storage->u.btree2.bt2 = NULL;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_size() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_reset
 *
 * Purpose:	Reset indexing information.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_reset(H5O_storage_chunk_t *storage, hbool_t reset_addr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(storage);

    /* Reset index info */
    if(reset_addr)
	storage->idx_addr = HADDR_UNDEF;
    storage->u.btree2.bt2 = NULL;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_bt2_idx_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_idx_support
 *
 * Purpose:     Create a dependency between a chunk [proxy] and the index
 *              metadata that contains the record for the chunk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Monday, May 14, 2012
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5D_bt2_idx_support(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata,
    H5AC_info_t *child_entry)
{
    H5D_bt2_find_ud_t bt2_udata;        /* User data for v2 B-tree calls */
    unsigned u;                         /* Local index variable */
    htri_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);
    HDassert(child_entry);
    HDassert(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE);

    if(idx_info->pline->nused > 0) { /* filtered chunk */
        H5D_bt2_filt_rec_t   search_rec;        /* Record for searching for object */

        /* Set the chunk offset to be searched for */
        for(u = 0; u < (idx_info->layout->ndims - 1); u++)
            search_rec.offset[u] = udata->common.offset[u];

        /* Prepare user data for support callback */
        bt2_udata.rec = &search_rec;
        bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;
    } /* end if */
    else { /* non-filtered chunk */
        H5D_bt2_rec_t   search_rec;     /* Record for searching for object */

        /* Set the chunk offset to be searched for */
        for(u = 0; u < (idx_info->layout->ndims - 1); u++)
            search_rec.offset[u] = udata->common.offset[u];

        /* Prepare user data for support callback */
        bt2_udata.rec = &search_rec;
        bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;
    } /* end else */

    /* Add the flush dependency on the chunk */
    if((ret_value = H5B2_support(idx_info->storage->u.btree2.bt2, idx_info->dxpl_id, &bt2_udata, child_entry)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTDEPEND, FAIL, "unable to create flush dependency on b-tree array metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_support() */


/*-------------------------------------------------------------------------
 * Function:    H5D_bt2_idx_unsupport
 *
 * Purpose:     Destroy a dependency between a chunk [proxy] and the index
 *              metadata that contains the record for the chunk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Monday, May 14, 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_unsupport(const H5D_chk_idx_info_t *idx_info, H5D_chunk_ud_t *udata,
    H5AC_info_t *child_entry)
{
    H5D_bt2_find_ud_t bt2_udata;        /* User data for v2 B-tree calls */
    unsigned u;                         /* Local index variable */
    herr_t ret_value;                   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->pline);
    HDassert(idx_info->layout);
    HDassert(idx_info->storage);
    HDassert(H5F_addr_defined(idx_info->storage->idx_addr));
    HDassert(udata);
    HDassert(child_entry);
    HDassert(H5F_INTENT(idx_info->f) & H5F_ACC_SWMR_WRITE);

    if(idx_info->pline->nused > 0) { /* filtered chunk */
        H5D_bt2_filt_rec_t   search_rec;        /* Record for searching for object */

        /* Set the chunk offset to be searched for */
        for(u = 0; u < (idx_info->layout->ndims - 1); u++)
            search_rec.offset[u] = udata->common.offset[u];

        /* Prepare user data for support callback */
        bt2_udata.rec = &search_rec;
        bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;
    } /* end if */
    else { /* non-filtered chunk */
        H5D_bt2_rec_t   search_rec;     /* Record for searching for object */

        /* Set the chunk offset to be searched for */
        for(u = 0; u < (idx_info->layout->ndims - 1); u++)
            search_rec.offset[u] = udata->common.offset[u];

        /* Prepare user data for support callback */
        bt2_udata.rec = &search_rec;
        bt2_udata.ndims = idx_info->layout->ndims - 1;
        bt2_udata.layout = idx_info->layout;
        bt2_udata.rdcc = udata->common.rdcc;
    } /* end else */

    /* Add the flush dependency on the chunk */
    if((ret_value = H5B2_unsupport(idx_info->storage->u.btree2.bt2, idx_info->dxpl_id, &bt2_udata, child_entry)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency on b-tree array metadata")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_unsupport() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_dump
 *
 * Purpose:	Dump indexing information to a stream.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_dump(const H5O_storage_chunk_t *storage, FILE *stream)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(storage);
    HDassert(stream);

    HDfprintf(stream, "    Address: %a\n", storage->idx_addr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D_bt2_idx_dump() */


/*-------------------------------------------------------------------------
 * Function:	H5D_bt2_idx_dest
 *
 * Purpose:	Release indexing information in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; June 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D_bt2_idx_dest(const H5D_chk_idx_info_t *idx_info)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check args */
    HDassert(idx_info);
    HDassert(idx_info->f);
    HDassert(idx_info->storage);

    /* Check if the v2-btree is open */
    if(idx_info->storage->u.btree2.bt2) {
        /* Close v2 B-tree */
	if(H5B2_close(idx_info->storage->u.btree2.bt2, idx_info->dxpl_id) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCLOSEOBJ, FAIL, "can't close v2 B-tree")
        idx_info->storage->u.btree2.bt2 = NULL;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D_bt2_idx_dest() */

