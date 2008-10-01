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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5EA_HDR_VERSION        0               /* Header */
#define H5EA_IBLOCK_VERSION     0               /* Index block */
#define H5EA_SBLOCK_VERSION     0               /* Super block */
#define H5EA_DBLOCK_VERSION     0               /* Data block */

/* Size of stack buffer for serialization buffers */
#define H5EA_HDR_BUF_SIZE       512
#define H5EA_IBLOCK_BUF_SIZE    512
#define H5EA_SBLOCK_BUF_SIZE    512
#define H5EA_DBLOCK_BUF_SIZE    512


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
static herr_t H5EA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_hdr_t *hdr, unsigned * flags_ptr);
static herr_t H5EA__cache_hdr_clear(H5F_t *f, H5EA_hdr_t *hdr, hbool_t destroy);
static herr_t H5EA__cache_hdr_size(const H5F_t *f, const H5EA_hdr_t *hdr, size_t *size_ptr);
static herr_t H5EA__cache_hdr_dest(H5F_t *f, H5EA_hdr_t *hdr);
static H5EA_iblock_t *H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5EA__cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_iblock_t *iblock, unsigned * flags_ptr);
static herr_t H5EA__cache_iblock_clear(H5F_t *f, H5EA_iblock_t *iblock, hbool_t destroy);
static herr_t H5EA__cache_iblock_size(const H5F_t *f, const H5EA_iblock_t *iblock, size_t *size_ptr);
static herr_t H5EA__cache_iblock_dest(H5F_t *f, H5EA_iblock_t *iblock);
static H5EA_sblock_t *H5EA__cache_sblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5EA__cache_sblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_sblock_t *sblock, unsigned * flags_ptr);
static herr_t H5EA__cache_sblock_clear(H5F_t *f, H5EA_sblock_t *sblock, hbool_t destroy);
static herr_t H5EA__cache_sblock_size(const H5F_t *f, const H5EA_sblock_t *sblock, size_t *size_ptr);
static herr_t H5EA__cache_sblock_dest(H5F_t *f, H5EA_sblock_t *sblock);
static H5EA_dblock_t *H5EA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5EA__cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_dblock_t *dblock, unsigned * flags_ptr);
static herr_t H5EA__cache_dblock_clear(H5F_t *f, H5EA_dblock_t *dblock, hbool_t destroy);
static herr_t H5EA__cache_dblock_size(const H5F_t *f, const H5EA_dblock_t *dblock, size_t *size_ptr);
static herr_t H5EA__cache_dblock_dest(H5F_t *f, H5EA_dblock_t *dblock);


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

/* H5EA super block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_SBLOCK[1] = {{
    H5AC_EARRAY_SBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_sblock_load,
    (H5AC_flush_func_t)H5EA__cache_sblock_flush,
    (H5AC_dest_func_t)H5EA__cache_sblock_dest,
    (H5AC_clear_func_t)H5EA__cache_sblock_clear,
    (H5AC_size_func_t)H5EA__cache_sblock_size,
}};

/* H5EA data block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_DBLOCK[1] = {{
    H5AC_EARRAY_DBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_dblock_load,
    (H5AC_flush_func_t)H5EA__cache_dblock_flush,
    (H5AC_dest_func_t)H5EA__cache_dblock_dest,
    (H5AC_clear_func_t)H5EA__cache_dblock_clear,
    (H5AC_size_func_t)H5EA__cache_dblock_size,
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
H5EA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_cls,
    void UNUSED *udata2))

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
    if(HDmemcmp(p, H5EA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_HDR_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array header version")

    /* Extensible array type */
    if(*p++ != (uint8_t)cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* General array creation/configuration information */
    hdr->cparam.raw_elmt_size = *p++;          /* Element size in file (in bytes) */
    hdr->cparam.max_nelmts_bits = *p++;        /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
    hdr->cparam.idx_blk_elmts = *p++;          /* # of elements to store in index block */
    hdr->cparam.data_blk_min_elmts = *p++;     /* Min. # of elements per data block */
    hdr->cparam.sup_blk_min_data_ptrs = *p++;  /* Min. # of data block pointers for a super block */

    /* Internal information */
    H5F_addr_decode(f, &p, &hdr->idx_blk_addr); /* Address of index block */
    H5F_DECODE_LENGTH(f, p, hdr->stats.max_idx_set);    /* Max. index set (+1) */
    H5F_DECODE_LENGTH(f, p, hdr->stats.nsuper_blks);    /* Number of super blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.ndata_blks);     /* Number of data blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.nelmts);         /* Number of elements 'realized' */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Compute checksum on entire header */
    /* (including the filter information, if present) */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array header")

    /* Finish initializing extensible array header */
    if(H5EA__hdr_init(hdr) < 0)
	H5E_THROW(H5E_CANTINIT, "initialization failed for extensible array header")
    HDassert(hdr->size == size);

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
H5EA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5EA_hdr_t *hdr, unsigned UNUSED * flags_ptr))

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
        HDmemcpy(p, H5EA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_HDR_VERSION;

        /* Extensible array type */
        *p++ = hdr->cparam.cls->id;

        /* General array creation/configuration information */
        *p++ = hdr->cparam.raw_elmt_size;          /* Element size in file (in bytes) */
        *p++ = hdr->cparam.max_nelmts_bits;        /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
        *p++ = hdr->cparam.idx_blk_elmts;          /* # of elements to store in index block */
        *p++ = hdr->cparam.data_blk_min_elmts;     /* Min. # of elements per data block */
        *p++ = hdr->cparam.sup_blk_min_data_ptrs;  /* Min. # of data block pointers for a super block */

        /* Internal information */
        H5F_addr_encode(f, &p, hdr->idx_blk_addr);  /* Address of index block */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.max_idx_set);    /* Max. index set (+1) */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.nsuper_blks);    /* Number of super blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.ndata_blks);     /* Number of data blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.nelmts);         /* Number of elements 'realized' */

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

    /* Sanity check */
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
H5EA__cache_hdr_size(const H5F_t UNUSED *f, const H5EA_hdr_t *hdr,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = hdr->size;

END_FUNC(STATIC)   /* end H5EA__cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_hdr_dest
 *
 * Purpose:	Destroys an extensible array header in memory.
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
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_hdr_dest(H5F_t UNUSED *f, H5EA_hdr_t *hdr))

    /* Check arguments */
    HDassert(hdr);

    /* Release the extensible array header */
    if(H5EA__hdr_dest(hdr) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array header")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_hdr_dest() */


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
H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    const void UNUSED *udata1, void *_hdr))

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
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array index block")

    /* Set the extensible array index block's address */
    iblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(iblock_buf, sizeof(iblock_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array index block on disk */
    size = H5EA_IBLOCK_SIZE(iblock);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read index block from disk */
    if(H5F_block_read(f, H5FD_MEM_EARRAY_IBLOCK, addr, size, dxpl_id, buf) < 0)
	H5E_THROW(H5E_READERROR, "can't read extensible array index block")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array index block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_IBLOCK_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array index block version")

    /* Extensible array type */
    if(*p++ != (uint8_t)hdr->cparam.cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* Internal information */

    /* Decode elements in index block */
    if(hdr->cparam.idx_blk_elmts > 0) {
        /* Convert from raw elements on disk into native elements in memory */
        if((hdr->cparam.cls->decode)(p, iblock->elmts, (size_t)hdr->cparam.idx_blk_elmts) < 0)
            H5E_THROW(H5E_CANTDECODE, "can't decode extensible array index elements")
        p += (hdr->cparam.idx_blk_elmts * hdr->cparam.raw_elmt_size);
    } /* end if */

    /* Decode data block addresses in index block */
    if(iblock->ndblk_addrs > 0) {
        size_t u;               /* Local index variable */

        /* Decode addresses of data blocks in index block */
        for(u = 0; u < iblock->ndblk_addrs; u++)
            H5F_addr_decode(f, &p, &iblock->dblk_addrs[u]);
    } /* end if */

    /* Decode super block addresses in index block */
    if(iblock->nsblk_addrs > 0) {
        size_t u;               /* Local index variable */

        /* Decode addresses of super blocks in index block */
        for(u = 0; u < iblock->nsblk_addrs; u++)
            H5F_addr_decode(f, &p, &iblock->sblk_addrs[u]);
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
H5EA__cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5EA_iblock_t *iblock, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5EA_IBLOCK_BUF_SIZE]; /* Serialization buffer */

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
        HDmemcpy(p, H5EA_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_IBLOCK_VERSION;

        /* Extensible array type */
        *p++ = iblock->hdr->cparam.cls->id;

        /* Internal information */

        /* Encode elements in index block */
        if(iblock->hdr->cparam.idx_blk_elmts > 0) {
            /* Convert from native elements in memory into raw elements on disk */
            if((iblock->hdr->cparam.cls->encode)(p, iblock->elmts, (size_t)iblock->hdr->cparam.idx_blk_elmts) < 0)
                H5E_THROW(H5E_CANTENCODE, "can't encode extensible array index elements")
            p += (iblock->hdr->cparam.idx_blk_elmts * iblock->hdr->cparam.raw_elmt_size);
        } /* end if */

        /* Encode data block addresses in index block */
        if(iblock->ndblk_addrs > 0) {
            size_t u;               /* Local index variable */

            /* Encode addresses of data blocks in index block */
            for(u = 0; u < iblock->ndblk_addrs; u++)
                H5F_addr_encode(f, &p, iblock->dblk_addrs[u]);
        } /* end if */

        /* Encode data block addresses in index block */
        if(iblock->nsblk_addrs > 0) {
            size_t u;               /* Local index variable */

            /* Encode addresses of super blocks in index block */
            for(u = 0; u < iblock->nsblk_addrs; u++)
                H5F_addr_encode(f, &p, iblock->sblk_addrs[u]);
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

    /* Sanity check */
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
H5EA__cache_iblock_size(const H5F_t UNUSED *f, const H5EA_iblock_t *iblock,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(iblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = iblock->size;

END_FUNC(STATIC)   /* end H5EA__cache_iblock_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_iblock_dest
 *
 * Purpose:	Destroys an extensible array index block in memory.
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
H5EA__cache_iblock_dest(H5F_t *f, H5EA_iblock_t *iblock))

    /* Sanity check */
    HDassert(iblock);

    /* Release the index block */
    if(H5EA__iblock_dest(f, iblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array index block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_iblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_sblock_load
 *
 * Purpose:	Loads an extensible array super block from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array super block
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5EA_sblock_t *, NULL, NULL,
H5EA__cache_sblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    const void *_sblk_idx, void *_hdr))

    /* Local variables */
    H5EA_hdr_t    *hdr = (H5EA_hdr_t *)_hdr;      /* Shared extensible array information */
    const unsigned      *sblk_idx = (const unsigned *)_sblk_idx;  /* Index of this super block */
    H5EA_sblock_t	*sblock = NULL; /* Super block info */
    size_t		size;           /* Super block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for super block data */
    uint8_t             sblock_buf[H5EA_IBLOCK_BUF_SIZE]; /* Buffer for super block */
    uint8_t		*buf;           /* Pointer to super block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sblk_idx && *sblk_idx > 0);
    HDassert(hdr);

    /* Allocate the extensible array super block */
    if(NULL == (sblock = H5EA__sblock_alloc(hdr, *sblk_idx)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array super block")

    /* Set the extensible array super block's address */
    sblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(sblock_buf, sizeof(sblock_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array super block on disk */
    size = H5EA_SBLOCK_SIZE(sblock);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read super block from disk */
    if(H5F_block_read(f, H5FD_MEM_EARRAY_SBLOCK, addr, size, dxpl_id, buf) < 0)
	H5E_THROW(H5E_READERROR, "can't read extensible array super block")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_SBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array super block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_SBLOCK_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array super block version")

    /* Extensible array type */
    if(*p++ != (uint8_t)hdr->cparam.cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* Internal information */

    /* Decode data block addresses */
    if(sblock->ndblks > 0) {
        size_t u;               /* Local index variable */

        /* Decode addresses of data blocks in super block */
        for(u = 0; u < sblock->ndblks; u++)
            H5F_addr_decode(f, &p, &sblock->dblk_addrs[u]);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Save the super block's size */
    sblock->size = size;

    /* Compute checksum on super block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == sblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array super block")

    /* Set return value */
    ret_value = sblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(sblock && H5EA__cache_sblock_dest(f, sblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array super block")

END_FUNC(STATIC)   /* end H5EA__cache_sblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_sblock_flush
 *
 * Purpose:	Flushes a dirty extensible array super block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_sblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5EA_sblock_t *sblock, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5EA_SBLOCK_BUF_SIZE]; /* Serialization buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sblock);
    HDassert(sblock->hdr);

    if(sblock->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Index block size on disk */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

        /* Wrap the local buffer for serialized info */
        if(NULL == (wb = H5WB_wrap(ser_buf, sizeof(ser_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

        /* Compute the size of the super block on disk */
        size = sblock->size;

        /* Get a pointer to a buffer that's large enough for serialized info */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized info */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5EA_SBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_SBLOCK_VERSION;

        /* Extensible array type */
        *p++ = sblock->hdr->cparam.cls->id;

        /* Internal information */

        /* Encode data block addresses */
        if(sblock->ndblks > 0) {
            size_t u;               /* Local index variable */

            /* Encode addresses of data blocks in super block */
            for(u = 0; u < sblock->ndblks; u++)
                H5F_addr_encode(f, &p, sblock->dblk_addrs[u]);
        } /* end if */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the super block */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_EARRAY_SBLOCK, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save extensible array super block to disk")

	sblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5EA__cache_sblock_dest(f, sblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array super block")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5EA__cache_sblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_sblock_clear
 *
 * Purpose:	Mark a extensible array super block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_sblock_clear(H5F_t *f, H5EA_sblock_t *sblock, hbool_t destroy))

    /* Sanity check */
    HDassert(sblock);

    /* Reset the dirty flag */
    sblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5EA__cache_sblock_dest(f, sblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array super block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_sblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_sblock_size
 *
 * Purpose:	Compute the size in bytes of a extensible array super block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 30 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_sblock_size(const H5F_t UNUSED *f, const H5EA_sblock_t *sblock,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(sblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = sblock->size;

END_FUNC(STATIC)   /* end H5EA__cache_sblock_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_sblock_dest
 *
 * Purpose:	Destroys an extensible array super block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 30 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_sblock_dest(H5F_t *f, H5EA_sblock_t *sblock))

    /* Sanity check */
    HDassert(sblock);

    /* Release the super block */
    if(H5EA__sblock_dest(f, sblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array super block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_sblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblock_load
 *
 * Purpose:	Loads an extensible array data block from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array data block
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 16 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5EA_dblock_t *, NULL, NULL,
H5EA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr,
    const void *_nelmts, void *_hdr))

    /* Local variables */
    H5EA_hdr_t          *hdr = (H5EA_hdr_t *)_hdr;  /* Shared extensible array information */
    const size_t        *nelmts = (const size_t *)_nelmts;  /* Number of elements in data block */
    H5EA_dblock_t	*dblock = NULL; /* Data block info */
    size_t		size;           /* Data block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for data block data */
    uint8_t             dblock_buf[H5EA_DBLOCK_BUF_SIZE]; /* Buffer for data block */
    uint8_t		*buf;           /* Pointer to data block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(nelmts && *nelmts > 0);
    HDassert(hdr);

    /* Allocate the extensible array data block */
    if(NULL == (dblock = H5EA__dblock_alloc(hdr, *nelmts)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Set the extensible array data block's information */
    dblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(dblock_buf, sizeof(dblock_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array data block on disk */
    size = H5EA_DBLOCK_SIZE(dblock);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read data block from disk */
    if(H5F_block_read(f, H5FD_MEM_EARRAY_DBLOCK, addr, size, dxpl_id, buf) < 0)
	H5E_THROW(H5E_READERROR, "can't read extensible array data block")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array data block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_DBLOCK_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array data block version")

    /* Extensible array type */
    if(*p++ != (uint8_t)hdr->cparam.cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* Internal information */

    /* Decode elements in data block */
    /* Convert from raw elements on disk into native elements in memory */
    if((hdr->cparam.cls->decode)(p, dblock->elmts, *nelmts) < 0)
        H5E_THROW(H5E_CANTDECODE, "can't decode extensible array data elements")
    p += (*nelmts * hdr->cparam.raw_elmt_size);

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Save the data block's size */
    dblock->size = size;

    /* Compute checksum on data block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - buf) == dblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array data block")

    /* Set return value */
    ret_value = dblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(dblock && H5EA__cache_dblock_dest(f, dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block")

END_FUNC(STATIC)   /* end H5EA__cache_dblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblock_flush
 *
 * Purpose:	Flushes a dirty extensible array data block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 18 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5EA_dblock_t *dblock, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5EA_DBLOCK_BUF_SIZE]; /* Serialization buffer */

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
        size = dblock->size;

        /* Get a pointer to a buffer that's large enough for serialized info */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            H5E_THROW(H5E_CANTGET, "can't get actual buffer")

        /* Get temporary pointer to serialized info */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5EA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5EA_DBLOCK_VERSION;

        /* Extensible array type */
        *p++ = dblock->hdr->cparam.cls->id;

        /* Internal information */

        /* Encode elements in data block */

        /* Convert from native elements in memory into raw elements on disk */
        if((dblock->hdr->cparam.cls->encode)(p, dblock->elmts, dblock->nelmts) < 0)
            H5E_THROW(H5E_CANTENCODE, "can't encode extensible array data elements")
        p += (dblock->nelmts * dblock->hdr->cparam.raw_elmt_size);

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the index block */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_EARRAY_DBLOCK, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save extensible array data block to disk")

	dblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5EA__cache_dblock_dest(f, dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5EA__cache_dblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblock_clear
 *
 * Purpose:	Mark a extensible array data block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 18 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblock_clear(H5F_t *f, H5EA_dblock_t *dblock, hbool_t destroy))

    /* Sanity check */
    HDassert(dblock);

    /* Reset the dirty flag */
    dblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5EA__cache_dblock_dest(f, dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_dblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblock_size
 *
 * Purpose:	Compute the size in bytes of a extensible array data block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sept 18 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_dblock_size(const H5F_t UNUSED *f, const H5EA_dblock_t *dblock,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(dblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblock->size;

END_FUNC(STATIC)   /* end H5EA__cache_dblock_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblock_dest
 *
 * Purpose:	Destroys an extensible array data block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Sep 18 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblock_dest(H5F_t *f, H5EA_dblock_t *dblock))

    /* Sanity check */
    HDassert(dblock);

    /* Release the data block */
    if(H5EA__dblock_dest(f, dblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array data block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_dblock_dest() */

