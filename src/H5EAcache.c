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
 * Created:		H5EAcache.c
 *			Aug 26 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement extensible array metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#define H5EA_MODULE


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5EApkg.h"		/* Extensible Arrays			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5EA_HDR_VERSION        0               /* Header */
#define H5EA_IBLOCK_VERSION     0               /* Index block */

/* Size of stack buffer for serialization buffers */
#define H5EA_HDR_BUF_SIZE       512
#define H5EA_IBLOCK_BUF_SIZE    512


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
static H5EA_hdr_t *H5EA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5EA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_hdr_t *hdr, unsigned UNUSED * flags_ptr);
static herr_t H5EA__cache_hdr_clear(H5F_t *f, H5EA_hdr_t *hdr, hbool_t destroy);
static herr_t H5EA__cache_hdr_size(const H5F_t *f, const H5EA_hdr_t *hdr, size_t *size_ptr);
static H5EA_iblock_t *H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5EA__cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_iblock_t *iblock, unsigned UNUSED * flags_ptr);
static herr_t H5EA__cache_iblock_clear(H5F_t *f, H5EA_iblock_t *iblock, hbool_t destroy);
static herr_t H5EA__cache_iblock_size(const H5F_t *f, const H5EA_iblock_t *iblock, size_t *size_ptr);


/*********************/
/* Package Variables */
/*********************/

/* H5EA header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_HDR[1] = {{
    H5AC_EARRAY_HDR_ID,
    (H5AC_load_func_t)H5EA__cache_hdr_load,
    (H5AC_flush_func_t)H5EA__cache_hdr_flush,
    (H5AC_dest_func_t)H5EA__cache_hdr_dest,
    (H5AC_clear_func_t)H5EA__cache_hdr_clear,
    (H5AC_size_func_t)H5EA__cache_hdr_size,
}};

/* H5EA index block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_IBLOCK[1] = {{
    H5AC_EARRAY_IBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_iblock_load,
    (H5AC_flush_func_t)H5EA__cache_iblock_flush,
    (H5AC_dest_func_t)H5EA__cache_iblock_dest,
    (H5AC_clear_func_t)H5EA__cache_iblock_clear,
    (H5AC_size_func_t)H5EA__cache_iblock_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_load
 *
 * Purpose:	Loads an extensible array header from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5EA_hdr_t *, NULL, NULL,
H5EA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_cls, void UNUSED *udata2))

    /* Local variables */
    const H5EA_class_t  *cls = (const H5EA_class_t *)_cls;      /* Extensible array class */
    H5EA_hdr_t		*hdr = NULL;    /* Extensible array info */
    size_t		size;           /* Header size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for header data */
    uint8_t             hdr_buf[H5EA_HDR_BUF_SIZE]; /* Buffer for header */
    uint8_t		*buf;           /* Pointer to header buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the extensible array data structure */
    if(NULL == (hdr = H5EA__hdr_alloc(f, cls)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Set the extensible array header's address */
    hdr->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(hdr_buf, sizeof(hdr_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the 'base' size of the extensible array header on disk */
    size = H5EA_HEADER_SIZE(hdr);

    /* Get a pointer to a buffer that's large enough for serialized header */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_EARRAY_HDR, addr, size, dxpl_id, buf) < 0)
	H5E_THROW(H5E_READERROR, "can't read extensible array header")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_HDR_MAGIC, (size_t)H5EA_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header signature")
    p += H5EA_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_HDR_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array header version")

    /* General array creation/configuration information */
    hdr->raw_elmt_size = *p++;          /* Element size in file (in bytes) */
    hdr->max_nelmts_bits = *p++;        /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
    hdr->idx_blk_elmts = *p++;          /* # of elements to store in index block */
    hdr->data_blk_min_elmts = *p++;     /* Min. # of elements per data block */
    hdr->sup_blk_min_data_ptrs = *p++;  /* Min. # of data block pointers for a super block */

    /* Internal information */
    H5F_addr_decode(f, &p, &hdr->idx_blk_addr); /* Address of index block */
    H5F_DECODE_LENGTH(f, p, hdr->max_idx_set);  /* Max. index set (+1) */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Set the array header's size */
    hdr->size = size;

    /* Compute checksum on entire header */
    /* (including the filter information, if present) */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == hdr->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array header")

    /* Set return value */
    ret_value = hdr;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(hdr && H5EA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array header")

END_FUNC(STATIC)   /* end H5EA__cache_hdr_load() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_flush
 *
 * Purpose:	Flushes a dirty extensible array header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_hdr_t *hdr, unsigned UNUSED * flags_ptr))

    H5WB_t *wb = NULL;                  /* Wrapped buffer for header data */
    uint8_t hdr_buf[H5EA_HDR_BUF_SIZE]; /* Buffer for header */

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
        HDmemcpy(p, H5EA_HDR_MAGIC, (size_t)H5EA_SIZEOF_MAGIC);
        p += H5EA_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_HDR_VERSION;

        /* General array creation/configuration information */
        *p++ = hdr->raw_elmt_size;          /* Element size in file (in bytes) */
        *p++ = hdr->max_nelmts_bits;        /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
        *p++ = hdr->idx_blk_elmts;          /* # of elements to store in index block */
        *p++ = hdr->data_blk_min_elmts;     /* Min. # of elements per data block */
        *p++ = hdr->sup_blk_min_data_ptrs;  /* Min. # of data block pointers for a super block */

        /* Internal information */
        H5F_addr_encode(f, &p, hdr->idx_blk_addr);  /* Address of index block */
        H5F_ENCODE_LENGTH(f, p, hdr->max_idx_set);  /* Max. index set (+1) */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the array header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_EARRAY_HDR, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save extensible array header to disk")

	hdr->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5EA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array header")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5EA__cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_dest
 *
 * Purpose:	Destroys a extensible array header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(PKG, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_hdr_dest(H5F_t UNUSED *f, H5EA_hdr_t *hdr))

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(hdr->rc == 0);

    /* Free the shared info itself */
    (void)H5FL_FREE(H5EA_hdr_t, hdr);

END_FUNC(PKG)   /* end H5EA__cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_clear
 *
 * Purpose:	Mark a extensible array header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_hdr_clear(H5F_t *f, H5EA_hdr_t *hdr, hbool_t destroy))

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Reset the dirty flag.  */
    hdr->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5EA__cache_hdr_dest(f, hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array header")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a extensible array header
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 26 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_hdr_size(const H5F_t UNUSED *f, const H5EA_hdr_t *hdr, size_t *size_ptr))

    /* check arguments */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = hdr->size;

END_FUNC(STATIC)   /* end H5EA__cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_load
 *
 * Purpose:	Loads an extensible array index block from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array index block
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5EA_iblock_t *, NULL, NULL,
H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void *_hdr))

    /* Local variables */
    H5EA_hdr_t    *hdr = (H5EA_hdr_t *)_hdr;      /* Shared extensible array information */
    H5EA_iblock_t	*iblock = NULL; /* Index block info */
    size_t		size;           /* Index block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for index block data */
    uint8_t             iblock_buf[H5EA_IBLOCK_BUF_SIZE]; /* Buffer for index block */
    uint8_t		*buf;           /* Pointer to index block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);

    /* Allocate the extensible array index block */
    if(NULL == (iblock = H5EA__iblock_alloc(hdr)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array shared header")

    /* Share common array information */
    iblock->hdr = hdr;
    if(H5EA__hdr_incr(hdr) < 0)
	H5E_THROW(H5E_CANTINC, "can't increment reference count on shared array header")

    /* Set the extensible array index block's address */
    iblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(iblock_buf, sizeof(iblock_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array index block on disk */
    size = H5EA_IBLOCK_SIZE(hdr);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read index block from disk */
    if(H5F_block_read(f, H5FD_MEM_EARRAY_IBLOCK, addr, size, dxpl_id, buf) < 0)
	H5E_THROW(H5E_READERROR, "can't read extensible array index block")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_IBLOCK_MAGIC, (size_t)H5EA_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array index block signature")
    p += H5EA_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_IBLOCK_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array index block version")

    /* Internal information */

    /* Decode elements in index block */
    if(hdr->idx_blk_elmts > 0) {
        /* Convert from raw elements on disk into native elements in memory */
        if((hdr->cls->decode)(p, iblock->elmts, (size_t)hdr->idx_blk_elmts) < 0)
            H5E_THROW(H5E_CANTDECODE, "can't decode extensible array index elements")
        p += (hdr->idx_blk_elmts * hdr->raw_elmt_size);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Save the index block's size */
    iblock->size = size;

    /* Compute checksum on index block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == iblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array index block")

    /* Set return value */
    ret_value = iblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(iblock && H5EA__cache_iblock_dest(f, iblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array index block")

END_FUNC(STATIC)   /* end H5EA__cache_iblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_flush
 *
 * Purpose:	Flushes a dirty extensible array index block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_iblock_t *iblock, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5EA_HDR_BUF_SIZE]; /* Serialization buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);
    HDassert(iblock->hdr);

    if(iblock->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Index block size on disk */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

        /* Wrap the local buffer for serialized info */
        if(NULL == (wb = H5WB_wrap(ser_buf, sizeof(ser_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

        /* Compute the size of the index block on disk */
        size = iblock->size;

        /* Get a pointer to a buffer that's large enough for serialized info */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized info */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5EA_IBLOCK_MAGIC, (size_t)H5EA_SIZEOF_MAGIC);
        p += H5EA_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_IBLOCK_VERSION;

        /* Internal information */

        /* Encode elements in index block */
        if(iblock->hdr->idx_blk_elmts > 0) {
            /* Convert from native elements in memory into raw elements on disk */
            if((iblock->hdr->cls->encode)(p, iblock->elmts, (size_t)iblock->hdr->idx_blk_elmts) < 0)
                H5E_THROW(H5E_CANTENCODE, "can't encode extensible array index elements")
            p += (iblock->hdr->idx_blk_elmts * iblock->hdr->raw_elmt_size);
        } /* end if */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the index block */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_EARRAY_IBLOCK, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save extensible array index block to disk")

	iblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5EA__cache_iblock_dest(f, iblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array index block")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5EA__cache_iblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_dest
 *
 * Purpose:	Destroys a extensible array index block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep  9 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_iblock_dest(H5F_t UNUSED *f, H5EA_iblock_t *iblock))

    /* Sanity check */
    HDassert(iblock);
    HDassert(iblock->rc == 0);

    /* Set the shared array header's file context for this operation */
    iblock->hdr->f = f;

    /* Check if we've got elements in the index block */
    if(iblock->hdr->idx_blk_elmts > 0) {
        /* Free buffer for index block elements */
        HDassert(iblock->elmts);
        (void)H5FL_BLK_FREE(elmt_buf, iblock->elmts);
    } /* end if */

    /* Decrement reference count on shared info */
    if(H5EA__hdr_decr(iblock->hdr) < 0)
        H5E_THROW(H5E_CANTDEC, "can't decrement reference count on shared array header")

    /* Free the index block itself */
    (void)H5FL_FREE(H5EA_iblock_t, iblock);

CATCH

END_FUNC(PKG)   /* end H5EA__cache_iblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_clear
 *
 * Purpose:	Mark a extensible array index block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 9 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_iblock_clear(H5F_t *f, H5EA_iblock_t *iblock, hbool_t destroy))

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Reset the dirty flag */
    iblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5EA__cache_iblock_dest(f, iblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array index block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_iblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_size
 *
 * Purpose:	Compute the size in bytes of a extensible array index block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 9 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_iblock_size(const H5F_t UNUSED *f, const H5EA_iblock_t *iblock, size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(iblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = iblock->size;

END_FUNC(STATIC)   /* end H5EA__cache_iblock_size() */

