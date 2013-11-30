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
#define H5EA_DBLK_PAGE_BUF_SIZE 512


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
static H5EA_hdr_t *H5EA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5EA__cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_hdr_t *hdr, unsigned * flags_ptr);
static herr_t H5EA__cache_hdr_clear(H5F_t *f, H5EA_hdr_t *hdr, hbool_t destroy);
static herr_t H5EA__cache_hdr_size(const H5F_t *f, const H5EA_hdr_t *hdr, size_t *size_ptr);
static herr_t H5EA__cache_hdr_dest(H5F_t *f, H5EA_hdr_t *hdr);
static H5EA_iblock_t *H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5EA__cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_iblock_t *iblock, unsigned * flags_ptr);
static herr_t H5EA__cache_iblock_clear(H5F_t *f, H5EA_iblock_t *iblock, hbool_t destroy);
static herr_t H5EA__cache_iblock_notify(H5AC_notify_action_t action, H5EA_iblock_t *iblock);
static herr_t H5EA__cache_iblock_size(const H5F_t *f, const H5EA_iblock_t *iblock, size_t *size_ptr);
static herr_t H5EA__cache_iblock_dest(H5F_t *f, H5EA_iblock_t *iblock);
static H5EA_sblock_t *H5EA__cache_sblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5EA__cache_sblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_sblock_t *sblock, unsigned * flags_ptr);
static herr_t H5EA__cache_sblock_clear(H5F_t *f, H5EA_sblock_t *sblock, hbool_t destroy);
static herr_t H5EA__cache_sblock_size(const H5F_t *f, const H5EA_sblock_t *sblock, size_t *size_ptr);
static herr_t H5EA__cache_sblock_notify(H5AC_notify_action_t action, H5EA_sblock_t *sblock);
static herr_t H5EA__cache_sblock_dest(H5F_t *f, H5EA_sblock_t *sblock);
static H5EA_dblock_t *H5EA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5EA__cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_dblock_t *dblock, unsigned * flags_ptr);
static herr_t H5EA__cache_dblock_clear(H5F_t *f, H5EA_dblock_t *dblock, hbool_t destroy);
static herr_t H5EA__cache_dblock_size(const H5F_t *f, const H5EA_dblock_t *dblock, size_t *size_ptr);
static herr_t H5EA__cache_dblock_notify(H5AC_notify_action_t action, H5EA_dblock_t *dblock);
static herr_t H5EA__cache_dblock_dest(H5F_t *f, H5EA_dblock_t *dblock);
static H5EA_dblk_page_t *H5EA__cache_dblk_page_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5EA__cache_dblk_page_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5EA_dblk_page_t *dblk_page, unsigned * flags_ptr);
static herr_t H5EA__cache_dblk_page_clear(H5F_t *f, H5EA_dblk_page_t *dblk_page, hbool_t destroy);
static herr_t H5EA__cache_dblk_page_size(const H5F_t *f, const H5EA_dblk_page_t *dblk_page, size_t *size_ptr);
static herr_t H5EA__cache_dblk_page_notify(H5AC_notify_action_t action, H5EA_dblk_page_t *dblk_page);
static herr_t H5EA__cache_dblk_page_dest(H5F_t *f, H5EA_dblk_page_t *dblk_page);


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
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)H5EA__cache_hdr_size,
}};

/* H5EA index block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_IBLOCK[1] = {{
    H5AC_EARRAY_IBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_iblock_load,
    (H5AC_flush_func_t)H5EA__cache_iblock_flush,
    (H5AC_dest_func_t)H5EA__cache_iblock_dest,
    (H5AC_clear_func_t)H5EA__cache_iblock_clear,
    (H5AC_notify_func_t)H5EA__cache_iblock_notify,
    (H5AC_size_func_t)H5EA__cache_iblock_size,
}};

/* H5EA super block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_SBLOCK[1] = {{
    H5AC_EARRAY_SBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_sblock_load,
    (H5AC_flush_func_t)H5EA__cache_sblock_flush,
    (H5AC_dest_func_t)H5EA__cache_sblock_dest,
    (H5AC_clear_func_t)H5EA__cache_sblock_clear,
    (H5AC_notify_func_t)H5EA__cache_sblock_notify,
    (H5AC_size_func_t)H5EA__cache_sblock_size,
}};

/* H5EA data block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_DBLOCK[1] = {{
    H5AC_EARRAY_DBLOCK_ID,
    (H5AC_load_func_t)H5EA__cache_dblock_load,
    (H5AC_flush_func_t)H5EA__cache_dblock_flush,
    (H5AC_dest_func_t)H5EA__cache_dblock_dest,
    (H5AC_clear_func_t)H5EA__cache_dblock_clear,
    (H5AC_notify_func_t)H5EA__cache_dblock_notify,
    (H5AC_size_func_t)H5EA__cache_dblock_size,
}};

/* H5EA data block page inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_EARRAY_DBLK_PAGE[1] = {{
    H5AC_EARRAY_DBLK_PAGE_ID,
    (H5AC_load_func_t)H5EA__cache_dblk_page_load,
    (H5AC_flush_func_t)H5EA__cache_dblk_page_flush,
    (H5AC_dest_func_t)H5EA__cache_dblk_page_dest,
    (H5AC_clear_func_t)H5EA__cache_dblk_page_clear,
    (H5AC_notify_func_t)H5EA__cache_dblk_page_notify,
    (H5AC_size_func_t)H5EA__cache_dblk_page_size,
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
H5EA__cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata))

    /* Local variables */
    H5EA_cls_id_t       id;		/* ID of extensible array class, as found in file */
    H5EA_hdr_t		*hdr = NULL;    /* Extensible array info */
    size_t		size;           /* Header size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for header data */
    uint8_t             hdr_buf[H5EA_HDR_BUF_SIZE]; /* Buffer for header */
    uint8_t		*buf;           /* Pointer to header buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the extensible array data structure */
    if(NULL == (hdr = H5EA__hdr_alloc(f)))
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

    /* Read and validate header from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_EARRAY_HDR, H5AC_EARRAY_HDR_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array header")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5EA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5EA_HDR_VERSION)
	H5E_THROW(H5E_VERSION, "wrong extensible array header version")

    /* Extensible array class */
    id = (H5EA_cls_id_t)*p++;
    if(id >= H5EA_NUM_CLS_ID)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")
    hdr->cparam.cls = H5EA_client_class_g[id];

    /* General array creation/configuration information */
    hdr->cparam.raw_elmt_size = *p++;          /* Element size in file (in bytes) */
    hdr->cparam.max_nelmts_bits = *p++;        /* Log2(Max. # of elements in array) - i.e. # of bits needed to store max. # of elements */
    hdr->cparam.idx_blk_elmts = *p++;          /* # of elements to store in index block */
    hdr->cparam.data_blk_min_elmts = *p++;     /* Min. # of elements per data block */
    hdr->cparam.sup_blk_min_data_ptrs = *p++;  /* Min. # of data block pointers for a super block */
    hdr->cparam.max_dblk_page_nelmts_bits = *p++;  /* Log2(Max. # of elements in data block page) - i.e. # of bits needed to store max. # of elements in data block page */

    /* Array statistics */
    hdr->stats.computed.hdr_size = size;                        /* Size of header in file */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.nsuper_blks);    /* Number of super blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.super_blk_size); /* Size of super blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.ndata_blks);     /* Number of data blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.data_blk_size);  /* Size of data blocks created */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.max_idx_set);    /* Max. index set (+1) */
    H5F_DECODE_LENGTH(f, p, hdr->stats.stored.nelmts);         /* Number of elements 'realized' */

    /* Internal information */
    H5F_addr_decode(f, &p, &hdr->idx_blk_addr); /* Address of index block */

    /* Index block statistics */
    if(H5F_addr_defined(hdr->idx_blk_addr)) {
        H5EA_iblock_t iblock;           /* Fake index block for computing size */

        /* Set index block count for file */
        hdr->stats.computed.nindex_blks = 1;

        /* Set up fake index block for computing size on disk */
        iblock.hdr = hdr;
        iblock.nsblks = H5EA_SBLK_FIRST_IDX(hdr->cparam.sup_blk_min_data_ptrs);
        iblock.ndblk_addrs = 2 * ((size_t)hdr->cparam.sup_blk_min_data_ptrs - 1);
        iblock.nsblk_addrs = hdr->nsblks - iblock.nsblks;

        /* Compute size of index block in file */
        hdr->stats.computed.index_blk_size = H5EA_IBLOCK_SIZE(&iblock);
    } /* end if */
    else {
        hdr->stats.computed.nindex_blks = 0;       /* Number of index blocks in file */
        hdr->stats.computed.index_blk_size = 0;    /* Size of index blocks in file */
    } /* end else */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (size - H5_SIZEOF_CHKSUM));

    /* Finish initializing extensible array header */
    if(H5EA__hdr_init(hdr, udata) < 0)
	H5E_THROW(H5E_CANTINIT, "initialization failed for extensible array header")
    HDassert(hdr->size == size);

    /* Set return value */
    ret_value = hdr;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(hdr && H5EA__hdr_dest(hdr) < 0)
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
        *p++ = hdr->cparam.max_dblk_page_nelmts_bits;  /* Log2(Max. # of elements in data block page) - i.e. # of bits needed to store max. # of elements in data block page */

        /* Array statistics */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.nsuper_blks);    /* Number of super blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.super_blk_size); /* Size of super blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.ndata_blks);     /* Number of data blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.data_blk_size);  /* Size of data blocks created */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.max_idx_set);    /* Max. index set (+1) */
        H5F_ENCODE_LENGTH(f, p, hdr->stats.stored.nelmts);         /* Number of elements 'realized' */

        /* Internal information */
        H5F_addr_encode(f, &p, hdr->idx_blk_addr);  /* Address of index block */

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
H5EA__cache_hdr_dest(H5F_t *f, H5EA_hdr_t *hdr))

    /* Check arguments */
    HDassert(f);
    HDassert(hdr);

    /* Verify that header is clean */
    HDassert(hdr->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!hdr->cache_info.free_file_space_on_destroy || H5F_addr_defined(hdr->cache_info.addr));

    /* Check for freeing file space for extensible array header */
    if(hdr->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(hdr->addr, hdr->cache_info.addr));

        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_EARRAY_HDR, H5AC_dxpl_id, hdr->cache_info.addr, (hsize_t)hdr->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free extensible array header")
    } /* end if */

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
H5EA__cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5EA_hdr_t    *hdr = (H5EA_hdr_t *)_udata;   /* Shared extensible array information */
    H5EA_iblock_t	*iblock = NULL; /* Index block info */
    size_t		size;           /* Index block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for index block data */
    uint8_t             iblock_buf[H5EA_IBLOCK_BUF_SIZE]; /* Buffer for index block */
    uint8_t		*buf;           /* Pointer to index block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             arr_addr;       /* Address of array header in the file */

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

    /* Read and validate index block from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_EARRAY_IBLOCK, H5AC_EARRAY_IBLOCK_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array index block")

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

    /* Address of header for array that owns this block (just for file integrity checks) */
    H5F_addr_decode(f, &p, &arr_addr);
    if(H5F_addr_ne(arr_addr, hdr->addr))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header address")

    /* Internal information */

    /* Decode elements in index block */
    if(hdr->cparam.idx_blk_elmts > 0) {
        /* Convert from raw elements on disk into native elements in memory */
        if((hdr->cparam.cls->decode)(p, iblock->elmts, (size_t)hdr->cparam.idx_blk_elmts, hdr->cb_ctx) < 0)
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

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (iblock->size - H5_SIZEOF_CHKSUM));

    /* Set return value */
    ret_value = iblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(iblock && H5EA__iblock_dest(iblock) < 0)
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

        /* Address of array header for array which owns this block */
        H5F_addr_encode(f, &p, iblock->hdr->addr);

        /* Internal information */

        /* Encode elements in index block */
        if(iblock->hdr->cparam.idx_blk_elmts > 0) {
            /* Convert from native elements in memory into raw elements on disk */
            if((iblock->hdr->cparam.cls->encode)(p, iblock->elmts, (size_t)iblock->hdr->cparam.idx_blk_elmts, iblock->hdr->cb_ctx) < 0)
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
 * Function:	H5EA__cache_iblock_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 31 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_iblock_notify(H5AC_notify_action_t action, H5EA_iblock_t *iblock))

    /* Sanity check */
    HDassert(iblock);

    /* Check if the file was opened with SWMR-write access */
    if(iblock->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on extensible array header */
                if(H5EA__create_flush_depend((H5AC_info_t *)iblock->hdr, (H5AC_info_t *)iblock) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between index block and header, address = %llu", (unsigned long long)iblock->addr)
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

END_FUNC(STATIC)   /* end H5EA__cache_iblock_notify() */


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
    HDassert(f);
    HDassert(iblock);

    /* Verify that index block is clean */
    HDassert(iblock->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!iblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(iblock->cache_info.addr));

    /* Check for freeing file space for extensible array index block */
    if(iblock->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(iblock->addr, iblock->cache_info.addr));

        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_EARRAY_IBLOCK, H5AC_dxpl_id, iblock->cache_info.addr, (hsize_t)iblock->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free extensible array index block")
    } /* end if */

    /* Release the index block */
    if(H5EA__iblock_dest(iblock) < 0)
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
H5EA__cache_sblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5EA_sblock_t	*sblock = NULL; /* Super block info */
    H5EA_sblock_cache_ud_t *udata = (H5EA_sblock_cache_ud_t *)_udata;      /* User data for loading super block */
    size_t		size;           /* Super block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for super block data */
    uint8_t             sblock_buf[H5EA_IBLOCK_BUF_SIZE]; /* Buffer for super block */
    uint8_t		*buf;           /* Pointer to super block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             arr_addr;       /* Address of array header in the file */
    size_t              u;              /* Local index variable */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->hdr && udata->parent && udata->sblk_idx > 0);

    /* Allocate the extensible array super block */
    if(NULL == (sblock = H5EA__sblock_alloc(udata->hdr, udata->parent, udata->sblk_idx)))
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

    /* Read and validate super block from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_EARRAY_SBLOCK, H5AC_EARRAY_SBLOCK_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array super block")

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
    if(*p++ != (uint8_t)udata->hdr->cparam.cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* Address of header for array that owns this block (just for file integrity checks) */
    H5F_addr_decode(f, &p, &arr_addr);
    if(H5F_addr_ne(arr_addr, udata->hdr->addr))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header address")

    /* Offset of block within the array's address space */
    UINT64DECODE_VAR(p, sblock->block_off, udata->hdr->arr_off_size);

    /* Internal information */

    /* Check for 'page init' bitmasks for this super block */
    if(sblock->dblk_npages > 0) {
        size_t tot_page_init_size = sblock->ndblks * sblock->dblk_page_init_size;        /* Compute total size of 'page init' buffer */

        /* Retrieve the 'page init' bitmasks */
        HDmemcpy(sblock->page_init, p, tot_page_init_size);
        p += tot_page_init_size;
    } /* end if */

    /* Decode data block addresses */
    for(u = 0; u < sblock->ndblks; u++)
        H5F_addr_decode(f, &p, &sblock->dblk_addrs[u]);

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Save the super block's size */
    sblock->size = size;

    /* Already decoded and compared stored checksum earlier, when reading */

    /* Sanity check */
    HDassert((size_t)(p - buf) == (sblock->size - H5_SIZEOF_CHKSUM));

    /* Set return value */
    ret_value = sblock;

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")
    if(!ret_value)
        if(sblock && H5EA__sblock_dest(sblock) < 0)
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
        size_t u;               /* Local index variable */

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

        /* Address of array header for array which owns this block */
        H5F_addr_encode(f, &p, sblock->hdr->addr);

        /* Offset of block in array */
        UINT64ENCODE_VAR(p, sblock->block_off, sblock->hdr->arr_off_size);

        /* Internal information */

        /* Check for 'page init' bitmasks for this super block */
        if(sblock->dblk_npages > 0) {
            size_t tot_page_init_size = sblock->ndblks * sblock->dblk_page_init_size;        /* Compute total size of 'page init' buffer */

            /* Store the 'page init' bitmasks */
            HDmemcpy(p, sblock->page_init, tot_page_init_size);
            p += tot_page_init_size;
        } /* end if */

        /* Encode addresses of data blocks in super block */
        for(u = 0; u < sblock->ndblks; u++)
            H5F_addr_encode(f, &p, sblock->dblk_addrs[u]);

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
 * Function:	H5EA__cache_sblock_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 31 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_sblock_notify(H5AC_notify_action_t action, H5EA_sblock_t *sblock))

    /* Sanity check */
    HDassert(sblock);

    /* Check if the file was opened with SWMR-write access */
    if(sblock->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on index block */
                if(H5EA__create_flush_depend((H5AC_info_t *)sblock->parent, (H5AC_info_t *)sblock) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between super block and index block, address = %llu", (unsigned long long)sblock->addr)
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

END_FUNC(STATIC)   /* end H5EA__cache_sblock_notify() */


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
    HDassert(f);
    HDassert(sblock);

    /* Verify that super block is clean */
    HDassert(sblock->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!sblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(sblock->cache_info.addr));

    /* Check for freeing file space for extensible array super block */
    if(sblock->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(sblock->addr, sblock->cache_info.addr));

        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_EARRAY_SBLOCK, H5AC_dxpl_id, sblock->cache_info.addr, (hsize_t)sblock->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free extensible array super block")
    } /* end if */

    /* Release the super block */
    if(H5EA__sblock_dest(sblock) < 0)
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
H5EA__cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5EA_dblock_t	*dblock = NULL; /* Data block info */
    H5EA_dblock_cache_ud_t *udata = (H5EA_dblock_cache_ud_t *)_udata;      /* User data for loading data block */
    size_t		size;           /* Data block size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for data block data */
    uint8_t             dblock_buf[H5EA_DBLOCK_BUF_SIZE]; /* Buffer for data block */
    uint8_t		*buf;           /* Pointer to data block buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             arr_addr;       /* Address of array header in the file */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->hdr && udata->parent && udata->nelmts > 0);

    /* Allocate the extensible array data block */
    if(NULL == (dblock = H5EA__dblock_alloc(udata->hdr, udata->parent, udata->nelmts)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block")

    /* Set the extensible array data block's information */
    dblock->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(dblock_buf, sizeof(dblock_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array data block on disk */
    if(!dblock->npages)
        size = H5EA_DBLOCK_SIZE(dblock);
    else
        size = H5EA_DBLOCK_PREFIX_SIZE(dblock);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read and validate data block from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_EARRAY_DBLOCK, H5AC_EARRAY_DBLOCK_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible data block")

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
    if(*p++ != (uint8_t)udata->hdr->cparam.cls->id)
	H5E_THROW(H5E_BADTYPE, "incorrect extensible array class")

    /* Address of header for array that owns this block (just for file integrity checks) */
    H5F_addr_decode(f, &p, &arr_addr);
    if(H5F_addr_ne(arr_addr, udata->hdr->addr))
	H5E_THROW(H5E_BADVALUE, "wrong extensible array header address")

    /* Offset of block within the array's address space */
    UINT64DECODE_VAR(p, dblock->block_off, udata->hdr->arr_off_size);

    /* Internal information */

    /* Only decode elements if the data block is not paged */
    if(!dblock->npages) {
        /* Decode elements in data block */
        /* Convert from raw elements on disk into native elements in memory */
        if((udata->hdr->cparam.cls->decode)(p, dblock->elmts, udata->nelmts, udata->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTDECODE, "can't decode extensible array data elements")
        p += (udata->nelmts * udata->hdr->cparam.raw_elmt_size);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

    /* Set the data block's size */
    dblock->size = H5EA_DBLOCK_SIZE(dblock);

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
        if(dblock && H5EA__dblock_dest(dblock) < 0)
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
        if(!dblock->npages)
            size = dblock->size;
        else
            size = H5EA_DBLOCK_PREFIX_SIZE(dblock);

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

        /* Address of array header for array which owns this block */
        H5F_addr_encode(f, &p, dblock->hdr->addr);

        /* Offset of block in array */
        UINT64ENCODE_VAR(p, dblock->block_off, dblock->hdr->arr_off_size);

        /* Internal information */

        /* Only encode elements if the data block is not paged */
        if(!dblock->npages) {
            /* Encode elements in data block */

            /* Convert from native elements in memory into raw elements on disk */
            if((dblock->hdr->cparam.cls->encode)(p, dblock->elmts, dblock->nelmts, dblock->hdr->cb_ctx) < 0)
                H5E_THROW(H5E_CANTENCODE, "can't encode extensible array data elements")
            p += (dblock->nelmts * dblock->hdr->cparam.raw_elmt_size);
        } /* end if */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the data block */
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
 * Function:	H5EA__cache_dblock_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 31 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblock_notify(H5AC_notify_action_t action, H5EA_dblock_t *dblock))

    /* Sanity check */
    HDassert(dblock);

    /* Check if the file was opened with SWMR-write access */
    if(dblock->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on parent */
                if(H5EA__create_flush_depend((H5AC_info_t *)dblock->parent, (H5AC_info_t *)dblock) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between data block and parent, address = %llu", (unsigned long long)dblock->addr)
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

END_FUNC(STATIC)   /* end H5EA__cache_dblock_notify() */


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
    if(!dblock->npages)
        *size_ptr = dblock->size;
    else
        *size_ptr = H5EA_DBLOCK_PREFIX_SIZE(dblock);

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
    HDassert(f);
    HDassert(dblock);

    /* Verify that data block is clean */
    HDassert(dblock->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!dblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(dblock->cache_info.addr));

    /* Check for freeing file space for extensible array data block */
    if(dblock->cache_info.free_file_space_on_destroy) {
        /* Sanity check address */
        HDassert(H5F_addr_eq(dblock->addr, dblock->cache_info.addr));

        /* Release the space on disk */
        /* (Includes space for pages!) */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_EARRAY_DBLOCK, H5AC_dxpl_id, dblock->cache_info.addr, (hsize_t)dblock->size) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to free extensible array data block")
    } /* end if */

    /* Release the data block */
    if(H5EA__dblock_dest(dblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array data block")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_dblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_load
 *
 * Purpose:	Loads an extensible array data block page from the disk.
 *
 * Return:	Success:	Pointer to a new extensible array data block page
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5EA_dblk_page_t *, NULL, NULL,
H5EA__cache_dblk_page_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    /* Local variables */
    H5EA_dblk_page_t	*dblk_page = NULL; /* Data block page info */
    H5EA_dblk_page_cache_ud_t *udata = (H5EA_dblk_page_cache_ud_t *)_udata;      /* User data for loading data block page */
    size_t		size;           /* Data block page size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for data block page data */
    uint8_t             dblk_page_buf[H5EA_DBLK_PAGE_BUF_SIZE]; /* Buffer for data block page */
    uint8_t		*buf;           /* Pointer to data block page buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */

    /* Sanity check */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata && udata->hdr && udata->parent);
#ifdef QAK
HDfprintf(stderr, "%s: addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Allocate the extensible array data block page */
    if(NULL == (dblk_page = H5EA__dblk_page_alloc(udata->hdr, udata->parent)))
	H5E_THROW(H5E_CANTALLOC, "memory allocation failed for extensible array data block page")

    /* Set the extensible array data block's information */
    dblk_page->addr = addr;

    /* Wrap the local buffer for serialized info */
    if(NULL == (wb = H5WB_wrap(dblk_page_buf, sizeof(dblk_page_buf))))
	H5E_THROW(H5E_CANTINIT, "can't wrap buffer")

    /* Compute the size of the extensible array data block page on disk */
    size = H5EA_DBLK_PAGE_SIZE(dblk_page);

    /* Get a pointer to a buffer that's large enough for serialized info */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
	H5E_THROW(H5E_CANTGET, "can't get actual buffer")

    /* Read and validate data block page from disk */
    if(H5F_read_check_metadata(f, dxpl_id, H5FD_MEM_EARRAY_DBLK_PAGE, H5AC_EARRAY_DBLK_PAGE_ID, addr, size, size, buf) < 0)
        H5E_THROW(H5E_BADVALUE, "incorrect metadata checksum for extensible array data block page")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Internal information */

    /* Decode elements in data block page */
    /* Convert from raw elements on disk into native elements in memory */
    if((udata->hdr->cparam.cls->decode)(p, dblk_page->elmts, udata->hdr->dblk_page_nelmts, udata->hdr->cb_ctx) < 0)
        H5E_THROW(H5E_CANTDECODE, "can't decode extensible array data elements")
    p += (udata->hdr->dblk_page_nelmts * udata->hdr->cparam.raw_elmt_size);

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - buf) == (size - H5EA_SIZEOF_CHKSUM));

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
        if(dblk_page && H5EA__dblk_page_dest(dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block page")

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_load() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_flush
 *
 * Purpose:	Flushes a dirty extensible array data block page to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblk_page_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5EA_dblk_page_t *dblk_page, unsigned UNUSED * flags_ptr))

    /* Local variables */
    H5WB_t *wb = NULL;                  /* Wrapped buffer for serializing data */
    uint8_t ser_buf[H5EA_DBLK_PAGE_BUF_SIZE]; /* Serialization buffer */

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
        if((dblk_page->hdr->cparam.cls->encode)(p, dblk_page->elmts, dblk_page->hdr->dblk_page_nelmts, dblk_page->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTENCODE, "can't encode extensible array data elements")
        p += (dblk_page->hdr->dblk_page_nelmts * dblk_page->hdr->cparam.raw_elmt_size);

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the data block */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_EARRAY_DBLK_PAGE, addr, size, dxpl_id, buf) < 0)
            H5E_THROW(H5E_WRITEERROR, "unable to save extensible array data block page to disk")

	dblk_page->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5EA__cache_dblk_page_dest(f, dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block page")

CATCH

    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
	H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer")

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_clear
 *
 * Purpose:	Mark a extensible array data block page in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblk_page_clear(H5F_t *f, H5EA_dblk_page_t *dblk_page, hbool_t destroy))

    /* Sanity check */
    HDassert(dblk_page);

    /* Reset the dirty flag */
    dblk_page->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5EA__cache_dblk_page_dest(f, dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy extensible array data block page")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Mar 31 2009
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblk_page_notify(H5AC_notify_action_t action, H5EA_dblk_page_t *dblk_page))

    /* Sanity check */
    HDassert(dblk_page);

    /* Check if the file was opened with SWMR-write access */
    if(dblk_page->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on parent */
                if(H5EA__create_flush_depend((H5AC_info_t *)dblk_page->parent, (H5AC_info_t *)dblk_page) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between data block page and parent, address = %llu", (unsigned long long)dblk_page->addr)
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

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_size
 *
 * Purpose:	Compute the size in bytes of a extensible array data block page
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5EA__cache_dblk_page_size(const H5F_t UNUSED *f, const H5EA_dblk_page_t *dblk_page,
    size_t *size_ptr))

    /* Sanity check */
    HDassert(f);
    HDassert(dblk_page);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblk_page->size;

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_size() */


/*-------------------------------------------------------------------------
 * Function:	H5EA__cache_dblk_page_dest
 *
 * Purpose:	Destroys an extensible array data block page in memory.
 *
 * Note:	Does _not_ free the space for the page on disk, that is
 *              handled through the data block that "owns" the page.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 20 2008
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5EA__cache_dblk_page_dest(H5F_t UNUSED *f, H5EA_dblk_page_t *dblk_page))

    /* Sanity check */
    HDassert(f);
    HDassert(dblk_page);

    /* Verify that data block page is clean */
    HDassert(dblk_page->cache_info.is_dirty == FALSE);

    /* Release the data block page */
    if(H5EA__dblk_page_dest(dblk_page) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free extensible array data block page")

CATCH

END_FUNC(STATIC)   /* end H5EA__cache_dblk_page_dest() */

