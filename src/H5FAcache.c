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

/*-------------------------------------------------------------------------
 *
 * Created:     H5FAcache.c
 *
 * Purpose:     Implement fixed array metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5FA_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions                            */
#include "H5Eprivate.h"     /* Error handling                               */
#include "H5FApkg.h"        /* Fixed Arrays                                 */
#include "H5MFprivate.h"    /* File memory management                       */
#include "H5Vprivate.h"     /* Vectors and arrays                           */
#include "H5WBprivate.h"    /* Wrapped Buffers                              */


/****************/
/* Local Macros */
/****************/

/* Fixed Array format version #'s */
#define H5FA_HDR_VERSION        0               /* Header */
#define H5FA_DBLOCK_VERSION     0               /* Data block */

/* Size of stack buffer for serialization buffers */
#define H5FA_HDR_BUF_SIZE       512
#define H5FA_DBLOCK_BUF_SIZE    512
#define H5FA_DBLK_PAGE_BUF_SIZE 512


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static H5FA_hdr_t *H5FA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5FA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FA_hdr_t *hdr, unsigned * flags_ptr);
static herr_t H5FA__cache_hdr_dest(H5F_t *f, H5FA_hdr_t *hdr);
static herr_t H5FA__cache_hdr_clear(H5F_t *f, H5FA_hdr_t *hdr, hbool_t destroy);
static herr_t H5FA__cache_hdr_size(const H5F_t *f, const H5FA_hdr_t *hdr, size_t *size_ptr);

static H5FA_dblock_t *H5FA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5FA__cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FA_dblock_t *dblock, unsigned * flags_ptr);
static herr_t H5FA__cache_dblock_dest(H5F_t *f, H5FA_dblock_t *dblock);
static herr_t H5FA__cache_dblock_clear(H5F_t *f, H5FA_dblock_t *dblock, hbool_t destroy);
static herr_t H5FA__cache_dblock_notify(H5AC_notify_action_t action, H5FA_dblock_t *dblock);
static herr_t H5FA__cache_dblock_size(const H5F_t *f, const H5FA_dblock_t *dblock, size_t *size_ptr);

static H5FA_dblk_page_t *H5FA__cache_dblk_page_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5FA__cache_dblk_page_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FA_dblk_page_t *dblk_page, unsigned * flags_ptr);
static herr_t H5FA__cache_dblk_page_dest(H5F_t *f, H5FA_dblk_page_t *dblk_page);
static herr_t H5FA__cache_dblk_page_clear(H5F_t *f, H5FA_dblk_page_t *dblk_page, hbool_t destroy);
static herr_t H5FA__cache_dblk_page_size(const H5F_t *f, const H5FA_dblk_page_t *dblk_page, size_t *size_ptr);


/*********************/
/* Package Variables */
/*********************/

/* H5FA header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_HDR[1] = {{
    H5AC_FARRAY_HDR_ID,
    (H5AC_load_func_t)H5FA__cache_hdr_load,
    (H5AC_flush_func_t)H5FA__cache_hdr_flush,
    (H5AC_dest_func_t)H5FA__cache_hdr_dest,
    (H5AC_clear_func_t)H5FA__cache_hdr_clear,
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)H5FA__cache_hdr_size,
}};


/* H5FA data block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_DBLOCK[1] = {{
    H5AC_FARRAY_DBLOCK_ID,
    (H5AC_load_func_t)H5FA__cache_dblock_load,
    (H5AC_flush_func_t)H5FA__cache_dblock_flush,
    (H5AC_dest_func_t)H5FA__cache_dblock_dest,
    (H5AC_clear_func_t)H5FA__cache_dblock_clear,
    (H5AC_notify_func_t)H5FA__cache_dblock_notify,
    (H5AC_size_func_t)H5FA__cache_dblock_size,
}};

/* H5FA data block page inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_DBLK_PAGE[1] = {{
    H5AC_FARRAY_DBLK_PAGE_ID,
    (H5AC_load_func_t)H5FA__cache_dblk_page_load,
    (H5AC_flush_func_t)H5FA__cache_dblk_page_flush,
    (H5AC_dest_func_t)H5FA__cache_dblk_page_dest,
    (H5AC_clear_func_t)H5FA__cache_dblk_page_clear,
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)H5FA__cache_dblk_page_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_load
 *
 * Purpose:     Loads a fixed array header from the disk.
 *
 * Return:      Success:    Pointer to a new fixed array
 *              Failure:    NULL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5FA_hdr_t *, NULL, NULL,
H5FA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata))

    /* Local variables */
    H5FA_cls_id_t       id;             /* ID of fixed array class, as found in file */
    H5FA_hdr_t          *hdr = NULL;    /* Fixed array info */
    size_t              size;           /* Header size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for header data */
    uint8_t             hdr_buf[H5FA_HDR_BUF_SIZE]; /* Buffer for header */
    uint8_t             *buf;           /* Pointer to header buffer */
    const uint8_t       *p;             /* Pointer into raw data buffer */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the fixed array data structure */
    if(NULL == (hdr = H5FA__hdr_alloc(f)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array shared header")

    /* Set the fixed array header's address */
    hdr->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(hdr_buf, sizeof(hdr_buf))))
        H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the 'base' size of the fixed array header on disk */
    size = H5FA_HEADER_SIZE(hdr);

    /* Get a pointer to a buffer that's large enough for serialized header */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
        H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read and vaildate header from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_FARRAY_HDR, H5AC_FARRAY_HDR_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for fixed array header")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5FA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array header signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FA_HDR_VERSION)
        H5E_THROW(H5E_VERSION, "wrong fixed array header version")

    /* Fixed array class */
    id = (H5FA_cls_id_t)*p++;
    if(id >= H5FA_NUM_CLS_ID)
        H5E_THROW(H5E_BADTYPE, "incorrect fixed array class")
    hdr->cparam.cls = H5FA_client_class_g[id];

    /* General array creation/configuration information */
    hdr->cparam.raw_elmt_size = *p++;               /* Element size in file (in bytes) */
    hdr->cparam.max_dblk_page_nelmts_bits = *p++;   /* Log2(Max. # of elements in data block page) -
                                                     * i.e. # of bits needed to store max. # of
                                                     * elements in data block page.
                                                     */

    /* Array statistics */
    H5F_DECODE_LENGTH(f, p, hdr->cparam.nelmts);    /* Number of elements */

    /* Internal information */
    H5F_addr_decode(f, &p, &hdr->dblk_addr);        /* Address of index block */

    /* Check for data block */
    if(H5F_addr_defined(hdr->dblk_addr)) {
        H5FA_dblock_t  dblock;  	/* Fake data block for computing size */
        size_t	dblk_page_nelmts;	/* # of elements per data block page */

        /* Set up fake data block for computing size on disk */
        dblock.hdr = hdr;
        dblock.dblk_page_init_size = 0;
        dblock.npages = 0;
        dblk_page_nelmts = (size_t)1 << hdr->cparam.max_dblk_page_nelmts_bits;
        if(hdr->cparam.nelmts > dblk_page_nelmts) {
            dblock.npages = (size_t)(((hdr->cparam.nelmts + dblk_page_nelmts) - 1) / dblk_page_nelmts);
            dblock.dblk_page_init_size = (dblock.npages + 7) / 8;
        } /* end if */

        /* Compute Fixed Array data block size for hdr statistics */
        hdr->stats.dblk_size = (size_t)H5FA_DBLOCK_SIZE(&dblock);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5FA_SIZEOF_CHKSUM));

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (size - H5_SIZEOF_CHKSUM));

    /* Finish initializing fixed array header */
    if(H5FA__hdr_init(hdr, udata) < 0)
        H5E_THROW(H5E_CANTINIT, "initialization failed for fixed array header")
    HDassert(hdr->size == size);

    /* Set return value */
    ret_value = hdr;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(hdr && H5FA__hdr_dest(hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array header")

END_FUNC(STATIC)   /* end H5FA__cache_hdr_load() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_flush
 *
 * Purpose:     Flushes a dirty fixed array header to disk.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5FA_hdr_t *hdr, unsigned UNUSED * flags_ptr))

    H5WB_t *wb = NULL;                  /* Wrapped buffer for header data */
    uint8_t hdr_buf[H5FA_HDR_BUF_SIZE]; /* Buffer for header */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);

    if(hdr->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Header size on disk */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

        /* Wrap the local buffer for serialized header info */
        if(NULL == (wb = H5WB_wrap(hdr_buf, sizeof(hdr_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

        /* Compute the size of the array header on disk */
        size = hdr->size;

        /* Get a pointer to a buffer that's large enough for serialized header */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized header */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5FA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FA_HDR_VERSION;

        /* Fixed array type */
        *p++ = hdr->cparam.cls->id;

        /* General array creation/configuration information */
        *p++ = hdr->cparam.raw_elmt_size;          /* Element size in file (in bytes) */
        *p++ = hdr->cparam.max_dblk_page_nelmts_bits;  /* Log2(Max. # of elements in data block page) - i.e. # of bits needed to store max. # of elements in data block page */

        /* Array statistics */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.nelmts);       /* Number of elements for the fixed array */

        /* Internal information */
        H5F_addr_encode(f, &p, hdr->dblk_addr);  /* Address of fixed array data block */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

        /* Write the array header. */
        HDassert((size_t)(p - buf) == size);
        if(H5F_block_write(f, H5FD_MEM_FARRAY_HDR, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save fixed array header to disk")

        hdr->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array header")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5FA__cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_dest
 *
 * Purpose:     Destroys a fixed array header in memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_hdr_dest(H5F_t *f, H5FA_hdr_t *hdr))

    /* Check arguments */
    HDassert(f);
    HDassert(hdr);

    /* Verify that header is clean */
    HDassert(hdr->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!hdr->cache_info.free_file_space_on_destroy || H5F_addr_defined(hdr->cache_info.addr));

    /* Check for freeing file space for fixed array header */
    if(hdr->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(hdr->addr, hdr->cache_info.addr));

        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_FARRAY_HDR, H5AC_dxpl_id, hdr->cache_info.addr, (hsize_t)hdr->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free fixed array header")
    } /* end if */

    /* Release the fixed array header */
    if(H5FA__hdr_dest(hdr) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array header")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_clear
 *
 * Purpose:     Mark a fixed array header in memory as non-dirty.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_hdr_clear(H5F_t *f, H5FA_hdr_t *hdr, hbool_t destroy))

    /* Sanity check */
    HDassert(hdr);

    /* Reset the dirty flag.  */
    hdr->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array header")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_size
 *
 * Purpose:     Compute the size in bytes of a fixed array header
 *              on disk, and return it in *size_ptr.  On failure,
 *              the value of *size_ptr is undefined.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_hdr_size(const H5F_t UNUSED *f, const H5FA_hdr_t *hdr,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = hdr->size;

END_FUNC(STATIC)   /* end H5FA__cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_load
 *
 * Purpose:     Loads a fixed array data block from the disk.
 *
 * Return:      Success:    Pointer to a new fixed array data block
 *              Failure:    NULL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5FA_dblock_t *, NULL, NULL,
H5FA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5FA_dblock_t  *dblock = NULL;  /* Data block info */
    H5FA_dblock_cache_ud_t *udata = (H5FA_dblock_cache_ud_t *)_udata; /* User data for loading data block */
    size_t	   size;            /* Data block size */
    H5WB_t         *wb = NULL;      /* Wrapped buffer for data block data */
    uint8_t        dblock_buf[H5FA_DBLOCK_BUF_SIZE]; /* Buffer for data block */
    uint8_t	   *buf;             /* Pointer to data block buffer */
    const uint8_t  *p;              /* Pointer into raw data buffer */
    haddr_t        arr_addr;        /* Address of array header in the file */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->hdr && udata->nelmts > 0);

    /* Allocate the fixed array data block */
    if(NULL == (dblock = H5FA__dblock_alloc(udata->hdr, udata->nelmts)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array data block")

    /* Set the fixed array data block's information */
    dblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(dblock_buf, sizeof(dblock_buf))))
        H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the fixed array data block on disk */
    if(!dblock->npages)
        size = (size_t)H5FA_DBLOCK_SIZE(dblock);
    else
        size = H5FA_DBLOCK_PREFIX_SIZE(dblock);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
        H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read and validate data block from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_FARRAY_DBLOCK, H5AC_FARRAY_DBLOCK_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for fixed array data block")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5FA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array data block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FA_DBLOCK_VERSION)
        H5E_THROW(H5E_VERSION, "wrong fixed array data block version")

    /* Fixed array type */
    if(*p++ != (uint8_t)udata->hdr->cparam.cls->id)
        H5E_THROW(H5E_BADTYPE, "incorrect fixed array class")

    /* Address of header for array that owns this block (just for file integrity checks) */
    H5F_addr_decode(f, &p, &arr_addr);
    if(H5F_addr_ne(arr_addr, udata->hdr->addr))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array header address")

    /* Page initialization flags */
    if(dblock->npages > 0) {
        HDmemcpy(dblock->dblk_page_init, p, dblock->dblk_page_init_size);
        p += dblock->dblk_page_init_size;
    } /* end if */

    /* Only decode elements if the data block is not paged */
    if(!dblock->npages) {
        /* Decode elements in data block */
        /* Convert from raw elements on disk into native elements in memory */
        if((udata->hdr->cparam.cls->decode)(p, dblock->elmts, (size_t)udata->nelmts, udata->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTDECODE, "can't decode fixed array data elements")
        p += (udata->nelmts * udata->hdr->cparam.raw_elmt_size);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5FA_SIZEOF_CHKSUM));

    /* Set the data block's size */
    dblock->size = H5FA_DBLOCK_SIZE(dblock);

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (size - H5_SIZEOF_CHKSUM));

    /* Set return value */
    ret_value = dblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(dblock && H5FA__dblock_dest(dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block")

END_FUNC(STATIC)   /* end H5FA__cache_dblock_load() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_flush
 *
 * Purpose:     Flushes a dirty fixed array data block to disk.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5FA_dblock_t *dblock, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                     /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5FA_DBLOCK_BUF_SIZE]; /* Serialization buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblock);
    HDassert(dblock->hdr);

    if(dblock->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Index block size on disk */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

        /* Wrap the local buffer for serialized info */
        if(NULL == (wb = H5WB_wrap(ser_buf, sizeof(ser_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

        /* Compute the size of the data block on disk */
        if(!dblock->npages)
            size = (size_t)dblock->size;
        else
            size = H5FA_DBLOCK_PREFIX_SIZE(dblock);

        /* Get a pointer to a buffer that's large enough for serialized info */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized info */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5FA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FA_DBLOCK_VERSION;

        /* Fixed array type */
        *p++ = dblock->hdr->cparam.cls->id;

        /* Address of array header for array which owns this block */
        H5F_addr_encode(f, &p, dblock->hdr->addr);

        /* Page init flags */
        if(dblock->npages > 0) {
            /* Store the 'page init' bitmasks */
            HDmemcpy(p, dblock->dblk_page_init, dblock->dblk_page_init_size);
            p += dblock->dblk_page_init_size;
        } /* end if */

        /* Only encode elements if the data block is not paged */
        if(!dblock->npages) {
            /* Encode elements in data block */

            /* Convert from native elements in memory into raw elements on disk */
            H5_CHECK_OVERFLOW(dblock->hdr->cparam.nelmts, /* From: */hsize_t, /* To: */size_t);
            if((dblock->hdr->cparam.cls->encode)(p, dblock->elmts, (size_t)dblock->hdr->cparam.nelmts, dblock->hdr->cb_ctx) < 0)
                H5E_THROW(H5E_CANTENCODE, "can't encode fixed array data elements")
            p += (dblock->hdr->cparam.nelmts * dblock->hdr->cparam.raw_elmt_size);
        } /* end if */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

        /* Write the data block */
        HDassert((size_t)(p - buf) == size);
        if(H5F_block_write(f, H5FD_MEM_FARRAY_DBLOCK, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save fixed array data block to disk")

        dblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FA__cache_dblock_dest(f, dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5FA__cache_dblock_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_dest
 *
 * Purpose:     Destroys a fixed array data block in memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_dest(H5F_t *f, H5FA_dblock_t *dblock))

    /* Sanity check */
    HDassert(f);
    HDassert(dblock);

    /* Verify that data block is clean */
    HDassert(dblock->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!dblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(dblock->cache_info.addr));

    /* Check for freeing file space for fixed array data block */
    if(dblock->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(dblock->addr, dblock->cache_info.addr));

        /* Release the space on disk */
        /* (Includes space for pages!) */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_FARRAY_DBLOCK, H5AC_dxpl_id, dblock->cache_info.addr, (hsize_t)dblock->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free fixed array data block")
    } /* end if */

    /* Release the data block */
    if(H5FA__dblock_dest(dblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array data block")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_clear
 *
 * Purpose:     Mark a fixed array data block in memory as non-dirty.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_clear(H5F_t *f, H5FA_dblock_t *dblock, hbool_t destroy))

    /* Sanity check */
    HDassert(dblock);

    /* Reset the dirty flag */
    dblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FA__cache_dblock_dest(f, dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2012
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_notify(H5AC_notify_action_t action, H5FA_dblock_t *dblock))

    /* Sanity check */
    HDassert(dblock);

    /* Check if the file was opened with SWMR-write access */
    if(dblock->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on extensible array header */
                if(H5FA__create_flush_depend((H5AC_info_t *)dblock->hdr, (H5AC_info_t *)dblock) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between data block and header, address = %llu", (unsigned long long)dblock->addr)
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* Nothing to do */
                break;

            default:
#ifdef NDEBUG
                H5E_THROW(H5E_BADVALUE, "unknown action from metadata cache")
#else /* NDEBUG */
                HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
        } /* end switch */
    } /* end if */

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_notify() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_size
 *
 * Purpose:     Compute the size in bytes of a fixed array data block
 *              on disk, and return it in *size_ptr.
 *
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblock_size(const H5F_t UNUSED *f, const H5FA_dblock_t *dblock,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(dblock);
    HDassert(size_ptr);

    /* Set size value */
    if(!dblock->npages)
        *size_ptr = (size_t)dblock->size;
    else
        *size_ptr = H5FA_DBLOCK_PREFIX_SIZE(dblock);

END_FUNC(STATIC)   /* end H5FA__cache_dblock_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_load
 *
 * Purpose:     Loads a fixed array data block page from the disk.
 *
 * Return:      Success:    Pointer to a new fixed array data block page
 *              Failure:    NULL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5FA_dblk_page_t *, NULL, NULL,
H5FA__cache_dblk_page_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5FA_dblk_page_t    *dblk_page = NULL; /* Data block page info */
    H5FA_dblk_page_cache_ud_t *udata = (H5FA_dblk_page_cache_ud_t *)_udata; /* User data for loading data block page */
    size_t              size;           /* Data block page size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for data block page data */
    uint8_t             dblk_page_buf[H5FA_DBLK_PAGE_BUF_SIZE]; /* Buffer for data block page */
    uint8_t             *buf;           /* Pointer to data block page buffer */
    const uint8_t       *p;             /* Pointer into raw data buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->hdr && udata->nelmts > 0);
#ifdef QAK
HDfprintf(stderr, "%s: addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Allocate the fixed array data block page */
    if(NULL == (dblk_page = H5FA__dblk_page_alloc(udata->hdr, udata->nelmts)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array data block page")

    /* Set the fixed array data block's information */
    dblk_page->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(dblk_page_buf, sizeof(dblk_page_buf))))
        H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the fixed array data block page on disk */
    size = H5FA_DBLK_PAGE_SIZE(dblk_page, udata->nelmts);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
        H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read and validate data block page from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_FARRAY_DBLK_PAGE, H5AC_FARRAY_DBLK_PAGE_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for fixed array data block page")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Internal information */

    /* Decode elements in data block page */
    /* Convert from raw elements on disk into native elements in memory */
    if((udata->hdr->cparam.cls->decode)(p, dblk_page->elmts, udata->nelmts, udata->hdr->cb_ctx) < 0)
        H5E_THROW(H5E_CANTDECODE, "can't decode fixed array data elements")
    p += (udata->nelmts * udata->hdr->cparam.raw_elmt_size);

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5FA_SIZEOF_CHKSUM));

    /* Set the data block page's size */
    dblk_page->size = size;

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (dblk_page->size - H5_SIZEOF_CHKSUM));

    /* Set return value */
    ret_value = dblk_page;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(dblk_page && H5FA__dblk_page_dest(dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block page")

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_load() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_flush
 *
 * Purpose:     Flushes a dirty fixed array data block page to disk.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5FA_dblk_page_t *dblk_page, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5FA_DBLK_PAGE_BUF_SIZE]; /* Serialization buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblk_page);
    HDassert(dblk_page->hdr);

    if(dblk_page->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Index block size on disk */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

        /* Wrap the local buffer for serialized info */
        if(NULL == (wb = H5WB_wrap(ser_buf, sizeof(ser_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

        /* Compute the size of the data block on disk */
        size = dblk_page->size;

        /* Get a pointer to a buffer that's large enough for serialized info */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized info */
        p = buf;

        /* Internal information */

        /* Encode elements in data block page */

        /* Convert from native elements in memory into raw elements on disk */
        if((dblk_page->hdr->cparam.cls->encode)(p, dblk_page->elmts, dblk_page->nelmts, dblk_page->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTENCODE, "can't encode fixed array data elements")
        p += (dblk_page->nelmts * dblk_page->hdr->cparam.raw_elmt_size);

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

        /* Write the data block */
        HDassert((size_t)(p - buf) == size);
        if(H5F_block_write(f, H5FD_MEM_FARRAY_DBLK_PAGE, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save fixed array data block page to disk")

        dblk_page->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FA__cache_dblk_page_dest(f, dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block page")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_dest
 *
 * Purpose:     Destroys a fixed array data block page in memory.
 *
 * Note:        Does _not_ free the space for the page on disk, that is
 *              handled through the data block that "owns" the page.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_dest(H5F_t UNUSED *f, H5FA_dblk_page_t *dblk_page))

    /* Sanity check */
    HDassert(f);
    HDassert(dblk_page);

    /* Verify that data block page is clean */
    HDassert(dblk_page->cache_info.is_dirty == FALSE);

    /* Release the data block page */
    if(H5FA__dblk_page_dest(dblk_page) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array data block page")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_clear
 *
 * Purpose:     Mark a fixed array data block page in memory as non-dirty.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_clear(H5F_t *f, H5FA_dblk_page_t *dblk_page, hbool_t destroy))

    /* Sanity check */
    HDassert(dblk_page);

    /* Reset the dirty flag */
    dblk_page->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FA__cache_dblk_page_dest(f, dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block page")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_size
 *
 * Purpose:     Compute the size in bytes of a fixed array data block page
 *              on disk, and return it in *size_ptr.  On failure,
 *              the value of *size_ptr is undefined.
 *
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  Vailin Choi
 *              Thursday, April 30, 2009
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblk_page_size(const H5F_t UNUSED *f, const H5FA_dblk_page_t *dblk_page,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(dblk_page);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblk_page->size;

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_size() */
