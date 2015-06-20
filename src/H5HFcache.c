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
 * Created:		H5HFcache.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement fractal heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HF_PACKAGE		/*suppress error about including H5HFpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VMprivate.h"	/* Vectors and arrays 			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5HF_HDR_VERSION        0               /* Header */
#define H5HF_DBLOCK_VERSION     0               /* Direct block */
#define H5HF_IBLOCK_VERSION     0               /* Indirect block */

/* Size of stack buffer for serialized headers */
#define H5HF_HDR_BUF_SIZE       512

/* Size of stack buffer for serialized indirect blocks */
#define H5HF_IBLOCK_BUF_SIZE    4096


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Local encode/decode routines */
static herr_t H5HF__dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable);
static herr_t H5HF__dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable);

/* Metadata cache (H5AC) callbacks */
static H5HF_hdr_t *H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_hdr_t *hdr, unsigned H5_ATTR_UNUSED * flags_ptr);
static herr_t H5HF_cache_hdr_dest(H5F_t *f, H5HF_hdr_t *hdr);
static herr_t H5HF_cache_hdr_clear(H5F_t *f, H5HF_hdr_t *hdr, hbool_t destroy);
static herr_t H5HF_cache_hdr_size(const H5F_t *f, const H5HF_hdr_t *hdr, size_t *size_ptr);
static H5HF_indirect_t *H5HF_cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5HF_cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_indirect_t *iblock, unsigned H5_ATTR_UNUSED * flags_ptr);
static herr_t H5HF_cache_iblock_dest(H5F_t *f, H5HF_indirect_t *iblock);
static herr_t H5HF_cache_iblock_clear(H5F_t *f, H5HF_indirect_t *iblock, hbool_t destroy);
static herr_t H5HF_cache_iblock_notify(H5C_notify_action_t action, H5HF_indirect_t *iblock);
static herr_t H5HF_cache_iblock_size(const H5F_t *f, const H5HF_indirect_t *iblock, size_t *size_ptr);
static H5HF_direct_t *H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock, unsigned H5_ATTR_UNUSED * flags_ptr);
static herr_t H5HF_cache_dblock_dest(H5F_t *f, H5HF_direct_t *dblock);
static herr_t H5HF_cache_dblock_clear(H5F_t *f, H5HF_direct_t *dblock, hbool_t destroy);
static herr_t H5HF_cache_dblock_notify(H5C_notify_action_t action, H5HF_direct_t *dblock);
static herr_t H5HF_cache_dblock_size(const H5F_t *f, const H5HF_direct_t *dblock, size_t *size_ptr);


/* Debugging Function Prototypes */
#ifndef NDEBUG
static herr_t H5HF__cache_verify_hdr_descendants_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_hdr_t *hdr, hbool_t *clean);
static herr_t H5HF__cache_verify_iblock_descendants_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_indirect_t *iblock, unsigned *iblock_status, hbool_t *clean);
static herr_t H5HF__cache_verify_iblocks_dblocks_clean(H5F_t *f,
    H5HF_indirect_t *iblock, hbool_t *clean, hbool_t *has_dblocks);
static herr_t H5HF__cache_verify_descendant_iblocks_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_indirect_t *iblock, hbool_t *clean, hbool_t *has_iblocks);
#endif /* NDEBUG */


/*********************/
/* Package Variables */
/*********************/

/* H5HF header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_HDR[1] = {{
    H5AC_FHEAP_HDR_ID,
    (H5AC_load_func_t)H5HF_cache_hdr_load,
    (H5AC_flush_func_t)H5HF_cache_hdr_flush,
    (H5AC_dest_func_t)H5HF_cache_hdr_dest,
    (H5AC_clear_func_t)H5HF_cache_hdr_clear,
    (H5AC_notify_func_t)NULL,
    (H5AC_size_func_t)H5HF_cache_hdr_size,
}};

/* H5HF indirect block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_IBLOCK[1] = {{
    H5AC_FHEAP_IBLOCK_ID,
    (H5AC_load_func_t)H5HF_cache_iblock_load,
    (H5AC_flush_func_t)H5HF_cache_iblock_flush,
    (H5AC_dest_func_t)H5HF_cache_iblock_dest,
    (H5AC_clear_func_t)H5HF_cache_iblock_clear,
    (H5AC_notify_func_t)H5HF_cache_iblock_notify,
    (H5AC_size_func_t)H5HF_cache_iblock_size,
}};

/* H5HF direct block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_DBLOCK[1] = {{
    H5AC_FHEAP_DBLOCK_ID,
    (H5AC_load_func_t)H5HF_cache_dblock_load,
    (H5AC_flush_func_t)H5HF_cache_dblock_flush,
    (H5AC_dest_func_t)H5HF_cache_dblock_dest,
    (H5AC_clear_func_t)H5HF_cache_dblock_clear,
    (H5AC_notify_func_t)H5HF_cache_dblock_notify,
    (H5AC_size_func_t)H5HF_cache_dblock_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_DEFINE(direct_block);



/*-------------------------------------------------------------------------
 * Function:	H5HF__dtable_decode
 *
 * Purpose:	Decodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF__dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16DECODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16DECODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->cparam.start_root_rows);

    /* Address of table */
    H5F_addr_decode(f, pp, &(dtable->table_addr));

    /* Current # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF__dtable_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF__dtable_encode
 *
 * Purpose:	Encodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF__dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16ENCODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16ENCODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->cparam.start_root_rows);

    /* Address of root direct/indirect block */
    H5F_addr_encode(f, pp, dtable->table_addr);

    /* Current # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF__dtable_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_load
 *
 * Purpose:	Loads a fractal heap header from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_hdr_t *
H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata)
{
    H5HF_hdr_t		*hdr = NULL;     /* Fractal heap info */
    H5HF_hdr_cache_ud_t *udata = (H5HF_hdr_cache_ud_t *)_udata;
    size_t		size;           /* Header size */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for header data */
    uint8_t             hdr_buf[H5HF_HDR_BUF_SIZE]; /* Buffer for header */
    uint8_t		*buf;           /* Pointer to header buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    uint8_t             heap_flags;     /* Status flags for heap */
    H5HF_hdr_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);

    /* Allocate space for the fractal heap data structure */
    if(NULL == (hdr = H5HF_hdr_alloc(udata->f)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Wrap the local buffer for serialized header info */
    if(NULL == (wb = H5WB_wrap(hdr_buf, sizeof(hdr_buf))))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute the 'base' size of the fractal heap header on disk */
    size = (size_t)H5HF_HEADER_SIZE(hdr);

    /* Get a pointer to a buffer that's large enough for serialized header */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap header")

    /* Get temporary pointer to serialized header */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_HDR_VERSION)
        HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap header version")

    /* General heap information */
    UINT16DECODE(p, hdr->id_len);               /* Heap ID length */
    UINT16DECODE(p, hdr->filter_len);           /* I/O filters' encoded length */

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    /* (bit 1: checksum direct blocks) */
    heap_flags = *p++;
    hdr->huge_ids_wrapped = heap_flags & H5HF_HDR_FLAGS_HUGE_ID_WRAPPED;
    hdr->checksum_dblocks = heap_flags & H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS;

    /* "Huge" object information */
    UINT32DECODE(p, hdr->max_man_size);         /* Max. size of "managed" objects */
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_next_id); /* Next ID to use for "huge" object */
    H5F_addr_decode(udata->f, &p, &hdr->huge_bt2_addr); /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_DECODE_LENGTH(udata->f, p, hdr->total_man_free); /* Internal free space in managed direct blocks */
    H5F_addr_decode(udata->f, &p, &hdr->fs_addr);      /* Address of free section header */

    /* Heap statistics */
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_alloc_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_iter_off);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF__dtable_decode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, NULL, "unable to encode managed obj. doubling table info")

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - (const uint8_t *)buf) == (size - H5HF_SIZEOF_CHKSUM));

    /* Check for I/O filter information to decode */
    if(hdr->filter_len > 0) {
        size_t filter_info_off;     /* Offset in header of filter information */
        size_t filter_info_size;    /* Size of filter information */
        H5O_pline_t *pline;         /* Pipeline information from the header on disk */

        /* Compute the offset of the filter info in the header */
        filter_info_off = (size_t)(p - (const uint8_t *)buf);

        /* Compute the size of the extra filter information */
        filter_info_size = (size_t)(hdr->sizeof_size     /* Size of size for filtered root direct block */
            + (unsigned)4                       /* Size of filter mask for filtered root direct block */
            + hdr->filter_len);                 /* Size of encoded I/O filter info */

        /* Compute the heap header's size */
        hdr->heap_size = size + filter_info_size;

        /* Re-size current buffer */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, hdr->heap_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "can't get actual buffer")

        /* Read in I/O filter information */
        /* (and the checksum) */
        if(H5F_block_read(f, H5FD_MEM_FHEAP_HDR, (addr + filter_info_off), (filter_info_size + H5HF_SIZEOF_CHKSUM), dxpl_id, (buf + filter_info_off)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap header's I/O pipeline filter info")

        /* Point at correct offset in header for the filter information */
        p = buf + filter_info_off;

        /* Decode the size of a filtered root direct block */
        H5F_DECODE_LENGTH(udata->f, p, hdr->pline_root_direct_size);

        /* Decode the filter mask for a filtered root direct block */
        UINT32DECODE(p, hdr->pline_root_direct_filter_mask);

        /* Decode I/O filter information */
        if(NULL == (pline = (H5O_pline_t *)H5O_msg_decode(hdr->f, udata->dxpl_id, NULL, H5O_PLINE_ID, p)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, NULL, "can't decode I/O pipeline filters")
        p += hdr->filter_len;

        /* Copy the information into the header's I/O pipeline structure */
        if(NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &(hdr->pline)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOPY, NULL, "can't copy I/O filter pipeline")

        /* Release the space allocated for the I/O pipeline filters */
        H5O_msg_free(H5O_PLINE_ID, pline);
    } /* end if */
    else
        /* Set the heap header's size */
        hdr->heap_size = size;

    /* Compute checksum on entire header */
    /* (including the filter information, if present) */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - (const uint8_t *)buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)buf) == hdr->heap_size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap header")

    /* Finish initialization of heap header */
    if(H5HF_hdr_finish_init(hdr) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "can't finish initializing shared fractal heap header")

    /* Set return value */
    ret_value = hdr;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value && hdr)
        if(H5HF_hdr_free(hdr) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, NULL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_flush
 *
 * Purpose:	Flushes a dirty fractal heap header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_hdr_t *hdr, unsigned H5_ATTR_UNUSED * flags_ptr)
{
    H5WB_t *wb = NULL;                  /* Wrapped buffer for header data */
    uint8_t hdr_buf[H5HF_HDR_BUF_SIZE]; /* Buffer for header */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);

    if(hdr->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Header size on disk */
        uint8_t heap_flags;     /* Status flags for heap */
        uint32_t metadata_chksum; /* Computed metadata checksum value */

#ifndef NDEBUG
{
        /* Verify that flush dependencies are working correctly.  Do this
	 * by verifying that either:
         *
         * 1) the header has a root iblock, and that the root iblock and all
 	 *    of its children are clean, or
         *
         * 2) The header has a root dblock, which is clean, or
         *
         * 3) The heap is empty, and thus the header has neither a root
         *    iblock no a root dblock.  In this case, the flush ordering
         *    constraint is met by default.
	 *
 	 * Do this with a call to H5HF__cache_verify_hdr_descendants_clean().
	 */
	hbool_t descendants_clean = TRUE;

	if(H5HF__cache_verify_hdr_descendants_clean(f, dxpl_id, hdr, &descendants_clean) < 0)
	     HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify hdr descendants clean.")
	HDassert(descendants_clean);
}
#endif /* NDEBUG */

        /* Set the shared heap header's file context for this operation */
        hdr->f = f;

        /* Wrap the local buffer for serialized header info */
        if(NULL == (wb = H5WB_wrap(hdr_buf, sizeof(hdr_buf))))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't wrap buffer")

        /* Compute the size of the heap header on disk */
        size = hdr->heap_size;

        /* Get a pointer to a buffer that's large enough for serialized header */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Get temporary pointer to serialized header */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_HDR_VERSION;

        /* General heap information */
        UINT16ENCODE(p, hdr->id_len);           /* Heap ID length */
        UINT16ENCODE(p, hdr->filter_len);       /* I/O filters' encoded length */

        /* Heap status flags */
        /* (bit 0: "huge" object IDs have wrapped) */
        /* (bit 1: checksum direct blocks) */
        heap_flags = 0;
        heap_flags = (uint8_t)(heap_flags | (hdr->huge_ids_wrapped ? H5HF_HDR_FLAGS_HUGE_ID_WRAPPED : 0));
        heap_flags = (uint8_t)(heap_flags | (hdr->checksum_dblocks ? H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS : 0));
        *p++ = heap_flags;

        /* "Huge" object information */
        UINT32ENCODE(p, hdr->max_man_size);             /* Max. size of "managed" objects */
        H5F_ENCODE_LENGTH(f, p, hdr->huge_next_id);     /* Next ID to use for "huge" object */
        H5F_addr_encode(f, &p, hdr->huge_bt2_addr);     /* Address of "huge" object tracker B-tree */

        /* "Managed" object free space information */
        H5F_ENCODE_LENGTH(f, p, hdr->total_man_free);   /* Internal free space in managed direct blocks */
        H5F_addr_encode(f, &p, hdr->fs_addr);           /* Address of free section header */

        /* Heap statistics */
        H5F_ENCODE_LENGTH(f, p, hdr->man_size);
        H5F_ENCODE_LENGTH(f, p, hdr->man_alloc_size);
        H5F_ENCODE_LENGTH(f, p, hdr->man_iter_off);
        H5F_ENCODE_LENGTH(f, p, hdr->man_nobjs);
        H5F_ENCODE_LENGTH(f, p, hdr->huge_size);
        H5F_ENCODE_LENGTH(f, p, hdr->huge_nobjs);
        H5F_ENCODE_LENGTH(f, p, hdr->tiny_size);
        H5F_ENCODE_LENGTH(f, p, hdr->tiny_nobjs);

        /* Managed objects' doubling-table info */
        if(H5HF__dtable_encode(hdr->f, &p, &(hdr->man_dtable)) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "unable to encode managed obj. doubling table info")

        /* Check for I/O filter information to encode */
        if(hdr->filter_len > 0) {
            /* Encode the size of a filtered root direct block */
            H5F_ENCODE_LENGTH(f, p, hdr->pline_root_direct_size);

            /* Encode the filter mask for a filtered root direct block */
            UINT32ENCODE(p, hdr->pline_root_direct_filter_mask);

            /* Encode I/O filter information */
            if(H5O_msg_encode(hdr->f, H5O_PLINE_ID, FALSE, p, &(hdr->pline)) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "can't encode I/O pipeline fiters")
            p += hdr->filter_len;
        } /* end if */

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

	/* Write the heap header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap header to disk")

	hdr->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_hdr_dest(f, hdr) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap header")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_dest
 *
 * Purpose:	Destroys a fractal heap header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_dest(H5F_t *f, H5HF_hdr_t *hdr)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(hdr->rc == 0);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!hdr->cache_info.free_file_space_on_destroy || H5F_addr_defined(hdr->cache_info.addr));

    /* Check for freeing file space for heap header */
    if(hdr->cache_info.free_file_space_on_destroy) {
        /* Release the space on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(H5MF_xfree(f, H5FD_MEM_FHEAP_HDR, H5AC_dxpl_id, hdr->cache_info.addr, (hsize_t)hdr->heap_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap header")
    } /* end if */

    /* Free the shared info itself */
    if(H5HF_hdr_free(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "unable to release fractal heap header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_clear
 *
 * Purpose:	Mark a fractal heap header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_clear(H5F_t *f, H5HF_hdr_t *hdr, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Reset the dirty flag.  */
    hdr->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HF_cache_hdr_dest(f, hdr) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a fractal heap header
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_size(const H5F_t H5_ATTR_UNUSED *f, const H5HF_hdr_t *hdr, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = hdr->heap_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_load
 *
 * Purpose:	Loads a fractal heap indirect block from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap indirect block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_indirect_t *
H5HF_cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata)
{
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_iblock_cache_ud_t *udata = (H5HF_iblock_cache_ud_t *)_udata; /* user data for callback */
    H5HF_indirect_t	*iblock = NULL; /* Indirect block info */
    H5WB_t              *wb = NULL;     /* Wrapped buffer for indirect block data */
    uint8_t             iblock_buf[H5HF_IBLOCK_BUF_SIZE]; /* Buffer for indirect block */
    uint8_t		*buf;           /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    unsigned            u;              /* Local index variable */
    H5HF_indirect_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);

    /* Allocate space for the fractal heap indirect block */
    if(NULL == (iblock = H5FL_CALLOC(H5HF_indirect_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Get the pointer to the shared heap header */
    hdr = udata->par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = udata->f;

    /* Share common heap information */
    iblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    iblock->rc = 0;
    iblock->nrows = *udata->nrows;
    iblock->nchildren = 0;

    /* Wrap the local buffer for serialized indirect block */
    if(NULL == (wb = H5WB_wrap(iblock_buf, sizeof(iblock_buf))))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't wrap buffer")

    /* Compute size of indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock->nrows);

    /* Get a pointer to a buffer that's large enough for serialized indirect block */
    if(NULL == (buf = (uint8_t *)H5WB_actual(wb, iblock->size)))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "can't get actual buffer")

    /* Read indirect block from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap indirect block")

    /* Get temporary pointer to serialized indirect block */
    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap indirect block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_IBLOCK_VERSION)
        HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block */
    H5F_addr_decode(udata->f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    iblock->parent = udata->par_info->iblock;
    /* this copy of the parent pointer is needed by the notify callback so */
    /* that it can take down flush dependencies on eviction even if        */
    /* the parent pointer has been nulled out.             JRM -- 5/18/14  */
    iblock->fd_parent = udata->par_info->iblock;
    iblock->par_entry = udata->par_info->entry;
    if(iblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

        /* Set max. # of rows in this block */
        iblock->max_rows = iblock->nrows;
    } /* end if */
    else {
        /* Set max. # of rows in this block */
        iblock->max_rows = hdr->man_dtable.max_root_rows;
    } /* end else */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, iblock->block_off, hdr->heap_off_size);

    /* Allocate & decode child block entry tables */
    HDassert(iblock->nrows > 0);
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (size_t)(iblock->nrows * hdr->man_dtable.cparam.width))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")

    if(hdr->filter_len > 0) {
        unsigned dir_rows;   /* Number of direct rows in this indirect block */

        /* Compute the number of direct rows for this indirect block */
        dir_rows = MIN(iblock->nrows, hdr->man_dtable.max_direct_rows);

        /* Allocate indirect block filtered entry array */
        if(NULL == (iblock->filt_ents = H5FL_SEQ_MALLOC(H5HF_indirect_filt_ent_t, (size_t)(dir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->filt_ents = NULL;

    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        /* Decode child block address */
        H5F_addr_decode(udata->f, &p, &(iblock->ents[u].addr));

        /* Check for heap with I/O filters */
        if(hdr->filter_len > 0) {
            /* Sanity check */
            HDassert(iblock->filt_ents);

            /* Decode extra information for direct blocks */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
                /* Size of filtered direct block */
                H5F_DECODE_LENGTH(udata->f, p, iblock->filt_ents[u].size);

                /* Sanity check */
                /* (either both the address & size are defined or both are
                 *  not defined)
                 */
                HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                    || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                /* I/O filter mask for filtered direct block */
                UINT32DECODE(p, iblock->filt_ents[u].filter_mask);
            } /* end if */
        } /* end if */

        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            iblock->nchildren++;
            iblock->max_child = u;
        } /* end if */
    } /* end for */

    /* Sanity check */
    HDassert(iblock->nchildren);   /* indirect blocks w/no children should have been deleted */

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(buf, (size_t)(p - (const uint8_t *)buf), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)buf) == iblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
        HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Check if we have any indirect block children */
    if(iblock->nrows > hdr->man_dtable.max_direct_rows) {
        unsigned indir_rows;      /* Number of indirect rows in this indirect block */

        /* Compute the number of indirect rows for this indirect block */
        indir_rows = iblock->nrows - hdr->man_dtable.max_direct_rows;

        /* Allocate & initialize child indirect block pointer array */
        if(NULL == (iblock->child_iblocks = H5FL_SEQ_CALLOC(H5HF_indirect_ptr_t, (size_t)(indir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->child_iblocks = NULL;

    /* Set return value */
    ret_value = iblock;

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, NULL, "can't close wrapped buffer")
    if(!ret_value && iblock)
        if(H5HF_man_iblock_dest(iblock) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy fractal heap indirect block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_flush
 *
 * Purpose:	Flushes a dirty fractal heap indirect block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_indirect_t *iblock, unsigned H5_ATTR_UNUSED * flags_ptr)
{
    H5WB_t      *wb = NULL;             /* Wrapped buffer for indirect block data */
    uint8_t     iblock_buf[H5HF_IBLOCK_BUF_SIZE]; /* Buffer for indirect block */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);

    if(iblock->cache_info.is_dirty) {
        H5HF_hdr_t *hdr;                /* Shared fractal heap information */
        uint8_t	*buf;                   /* Temporary buffer */
        uint8_t *p;                     /* Pointer into raw data buffer */
#ifndef NDEBUG
        unsigned nchildren = 0;         /* Track # of children */
        unsigned max_child = 0;         /* Track max. child entry used */
#endif /* NDEBUG */
        uint32_t metadata_chksum;       /* Computed metadata checksum value */
        size_t u;                       /* Local index variable */

#ifndef NDEBUG
{
        /* Verify that flush dependencies are working correctly.  Do this 
	 * by verifying that all children of this iblock are clean.
	 */
	hbool_t descendants_clean = TRUE;
	unsigned iblock_status;

        if(H5AC_get_entry_status(f, iblock->addr, &iblock_status) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get iblock status")

	/* since the current iblock is the guest of honor in a flush, we know
         * that it is locked into the cache for the duration of the call.  Hence
         * there is no need to check to see if it is pinned or protected, or to
         * protect it if it is not.
         */
	if(H5HF__cache_verify_iblock_descendants_clean(f, dxpl_id, iblock, &iblock_status, &descendants_clean) < 0)
	     HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify descendants clean.")

	HDassert(descendants_clean);
}
#endif /* NDEBUG */

        /* Get the pointer to the shared heap header */
        hdr = iblock->hdr;

        /* Set the shared heap header's file context for this operation */
        hdr->f = f;

        /* Wrap the local buffer for serialized indirect block */
        if(NULL == (wb = H5WB_wrap(iblock_buf, sizeof(iblock_buf))))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't wrap buffer")

        /* Get a pointer to a buffer that's large enough for serialized indirect block */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, iblock->size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "can't get actual buffer")

        /* Get temporary pointer to buffer for serialized indirect block */
        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_IBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_IBLOCK_VERSION;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, hdr->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, iblock->block_off, hdr->heap_off_size);

        /* Encode indirect block-specific fields */
        for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
            /* Encode child block address */
            H5F_addr_encode(f, &p, iblock->ents[u].addr);

            /* Check for heap with I/O filters */
            if(hdr->filter_len > 0) {
                /* Sanity check */
                HDassert(iblock->filt_ents);

                /* Encode extra information for direct blocks */
                if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
                    /* Sanity check */
                    /* (either both the address & size are defined or both are
                     *  not defined)
                     */
                    HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                        || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                    /* Size of filtered direct block */
                    H5F_ENCODE_LENGTH(f, p, iblock->filt_ents[u].size);

                    /* I/O filter mask for filtered direct block */
                    UINT32ENCODE(p, iblock->filt_ents[u].filter_mask);
                } /* end if */
            } /* end if */

#ifndef NDEBUG
            /* Count child blocks */
            if(H5F_addr_defined(iblock->ents[u].addr)) {
                nchildren++;
                if(u > max_child)
                    max_child = u;
            } /* end if */
#endif /* NDEBUG */
        } /* end for */

        /* Compute checksum */
        metadata_chksum = H5_checksum_metadata(buf, (size_t)(p - buf), 0);

        /* Metadata checksum */
        UINT32ENCODE(p, metadata_chksum);

        /* Sanity check */
        HDassert((size_t)(p - buf) == iblock->size);
#ifndef NDEBUG
        HDassert(nchildren == iblock->nchildren);
        HDassert(max_child == iblock->max_child);
#endif /* NDEBUG */

        /* Check for needing to re-allocate indirect block from 'temp.' to 'normal' file space */
        if(H5F_IS_TMP_ADDR(f, addr)) {
            /* Sanity check */
            HDassert(H5F_addr_eq(iblock->addr, addr));

            /* Allocate 'normal' space for the new indirect block on disk */
            if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")

            /* Sanity check */
            HDassert(!H5F_addr_eq(iblock->addr, addr));

            /* Let the metadata cache know the block moved */
            if(H5AC_move_entry(f, H5AC_FHEAP_IBLOCK, iblock->addr, addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTMOVE, FAIL, "unable to move indirect block")

            /* Update the internal address for the block */
            iblock->addr = addr;

            /* Check for root indirect block */
            if(NULL == iblock->parent) {
                /* Update information about indirect block's location */
                hdr->man_dtable.table_addr = addr;

                /* Mark that heap header was modified */
                if(H5HF_hdr_dirty(hdr) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end if */
            else {
                H5HF_indirect_t *par_iblock;    /* Parent indirect block */
                unsigned par_entry;             /* Entry in parent indirect block */

                /* Get parent information */
                par_iblock = iblock->parent;
                par_entry = iblock->par_entry;

                /* Update information about indirect block's location */
                par_iblock->ents[par_entry].addr = addr;

                /* Mark that parent was modified */
                if(H5HF_iblock_dirty(par_iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end if */
        } /* end if */

        /* Indirect block must be in 'normal' file space now */
        HDassert(!H5F_IS_TMP_ADDR(f, addr));

	/* Write the indirect block */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap indirect block to disk")

        /* Reset dirty flags */
	iblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_iblock_dest(f, iblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap indirect block")

done:
    /* Release resources */
    if(wb && H5WB_unwrap(wb) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CLOSEERROR, FAIL, "can't close wrapped buffer")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_iblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_dest
 *
 * Purpose:	Destroys a fractal heap indirect block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_dest(H5F_t *f, H5HF_indirect_t *iblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(iblock);
    HDassert(iblock->rc == 0);
    HDassert(iblock->hdr);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!iblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(iblock->cache_info.addr));

    /* Check for freeing file space for indirect block */
    if(iblock->cache_info.free_file_space_on_destroy) {
        /* Check if the indirect block is NOT currently allocated in temp. file space */
        /* (temp. file space does not need to be freed) */
        if(!H5F_IS_TMP_ADDR(f, iblock->cache_info.addr)) {
            /* Release the space on disk */
            /* (XXX: Nasty usage of internal DXPL value! -QAK) */
            if(H5MF_xfree(f, H5FD_MEM_FHEAP_IBLOCK, H5AC_dxpl_id, iblock->cache_info.addr, (hsize_t)iblock->size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap indirect block")
        } /* end if */
    } /* end if */

    /* Destroy fractal heap indirect block */
    if(H5HF_man_iblock_dest(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_clear
 *
 * Purpose:	Mark a fractal heap indirect block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_clear(H5F_t *f, H5HF_indirect_t *iblock, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Reset the dirty flag.  */
    iblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HF_cache_iblock_dest(f, iblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_notify
 *
 * Purpose:	Setup / takedown flush dependencies as indirect blocks
 *		are loaded / inserted and evicted from the metadata cache.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/17/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_notify(H5C_notify_action_t action, H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(iblock);
    HDassert(iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(iblock->hdr);

    if(action == H5AC_NOTIFY_ACTION_BEFORE_EVICT)
        HDassert((iblock->parent == iblock->fd_parent) || ((NULL == iblock->parent) && (iblock->fd_parent)));
    else
        HDassert(iblock->parent == iblock->fd_parent);

    /* further sanity checks */
    if(iblock->parent == NULL) {
        /* Either this is the root iblock, or the parent pointer is     */
        /* invalid.  Since we save a copy of the parent pointer on      */
        /* the insertion event, it doesn't matter if the parent pointer */
        /* is invalid just before eviction.  However, we will not be    */
        /* able to function if it is invalid on the insertion event.    */
        /* Scream and die if this is the case.                          */
        HDassert((action == H5C_NOTIFY_ACTION_BEFORE_EVICT) || (iblock->block_off == 0));

        /* pointer from hdr to root iblock will not be set up unless */
        /* the fractal heap has already pinned the hdr.  Do what     */
        /* sanity checking we can.                                   */
        if((iblock->block_off == 0) && (iblock->hdr->root_iblock_flags & H5HF_ROOT_IBLOCK_PINNED))
           HDassert(iblock->hdr->root_iblock == iblock);
    } /* end if */
    else {
        /* if this is a child iblock, verify that the pointers are */
        /* either uninitialized or set up correctly.               */
        H5HF_indirect_t *par_iblock = iblock->parent;
        unsigned indir_idx;  /* Index in parent's child iblock pointer array */

        /* Sanity check */
        HDassert(par_iblock->child_iblocks);
        HDassert(iblock->par_entry >= (iblock->hdr->man_dtable.max_direct_rows * iblock->hdr->man_dtable.cparam.width));

        /* Compute index in parent's child iblock pointer array */
        indir_idx = iblock->par_entry - (iblock->hdr->man_dtable.max_direct_rows * iblock->hdr->man_dtable.cparam.width);

        /* The pointer to iblock in the parent may not be set yet -- */
        /* verify that it is either NULL, or that it has been set to */
        /* iblock.                                                   */
        HDassert((NULL == par_iblock->child_iblocks[indir_idx]) || (par_iblock->child_iblocks[indir_idx] == iblock));
    } /* end else */

    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            if(iblock->parent) {        /* this is a child iblock */
                /* create flush dependency with parent iblock */
                if(H5AC_create_flush_dependency(iblock->parent, iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
            } /* end if */
            else {      /* this is the root iblock */
                /* create flush dependency with header */
                if(H5AC_create_flush_dependency(iblock->hdr, iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
            } /* end else */
            break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            if(iblock->fd_parent) {     /* this is a child iblock */
                /* destroy flush dependency with parent iblock */
                if(H5AC_destroy_flush_dependency(iblock->fd_parent, iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
            } /* end if */
            else {      /* this is the root iblock */
                /* destroy flush dependency with header */
                if(H5AC_destroy_flush_dependency(iblock->hdr, iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
            } /* end else */
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown action from metadata cache")
            break;
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_size
 *
 * Purpose:	Compute the size in bytes of a fractal heap indirect block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_size(const H5F_t H5_ATTR_UNUSED *f, const H5HF_indirect_t *iblock, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(iblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = iblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_iblock_size() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_load
 *
 * Purpose:	Loads a fractal heap direct block from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap direct block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_direct_t *
H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata)
{
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_dblock_cache_ud_t *udata = (H5HF_dblock_cache_ud_t *)_udata;   /* User data for callback */
    H5HF_parent_t       *par_info;      /* Pointer to parent information */
    H5HF_direct_t	*dblock = NULL; /* Direct block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    H5HF_direct_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata != NULL);
    HDassert(udata->f != NULL);
    HDassert(udata->dblock_size > 0);

    /* Allocate space for the fractal heap direct block */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Get the pointer to the shared heap header */
    par_info = (H5HF_parent_t *)(&(udata->par_info));
    hdr = par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = udata->f;

    /* Share common heap information */
    dblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    dblock->size = udata->dblock_size;
    dblock->file_size = 0;

    /* Allocate block buffer */
/* XXX: Change to using free-list factories */
    if(NULL == (dblock->blk = H5FL_BLK_MALLOC(direct_block, (size_t)dblock->size)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Check for I/O filters on this heap */
    if(hdr->filter_len > 0) {
        H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
        size_t nbytes;          /* Number of bytes used in buffer, after applying reverse filters */
        void *read_buf;         /* Pointer to buffer to read in */
        size_t read_size;       /* Size of filtered direct block to read */
        unsigned filter_mask;   /* Excluded filters for direct block */

        /* Check for root direct block */
        if(par_info->iblock == NULL) {
            /* Sanity check */
            HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));

            /* Set up parameters to read filtered direct block */
            read_size = hdr->pline_root_direct_size;
        } /* end if */
        else {
            /* Sanity check */
            HDassert(H5F_addr_eq(par_info->iblock->ents[par_info->entry].addr, addr));

            /* Set up parameters to read filtered direct block */
            read_size = par_info->iblock->filt_ents[par_info->entry].size;
        } /* end else */

        /* Allocate buffer to perform I/O filtering on */
        if(NULL == (read_buf = H5MM_malloc(read_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "memory allocation failed for pipeline buffer")

        /* Read filtered direct block from disk */
        if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, read_size, dxpl_id, read_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")

        /* Push direct block data through I/O filter pipeline */
        nbytes = read_size;
        filter_mask = udata->filter_mask;
        if(H5Z_pipeline(&(hdr->pline), H5Z_FLAG_REVERSE, &filter_mask, H5Z_ENABLE_EDC, filter_cb, &nbytes, &read_size, &read_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFILTER, NULL, "output pipeline failed")

        /* Sanity check */
        HDassert(nbytes == dblock->size);

        /* Copy un-filtered data into block's buffer */
        HDmemcpy(dblock->blk, read_buf, dblock->size);

        /* Release the read buffer */
        H5MM_xfree(read_buf);
    } /* end if */
    else {
        /* Read direct block from disk */
        if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, dblock->size, dxpl_id, dblock->blk) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")
    } /* end else */

    /* Start decoding direct block */
    p = dblock->blk;

    /* Magic number */
    if(HDmemcmp(p, H5HF_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_DBLOCK_VERSION)
        HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block (just for file integrity checks) */
    H5F_addr_decode(udata->f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    dblock->parent = par_info->iblock;
    dblock->fd_parent = par_info->iblock;
    dblock->par_entry = par_info->entry;
    if(dblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
    } /* end if */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, dblock->block_off, hdr->heap_off_size);

    /* Decode checksum on direct block, if requested */
    if(hdr->checksum_dblocks) {
        uint32_t stored_chksum;         /* Metadata checksum value */
        uint32_t computed_chksum;       /* Computed metadata checksum value */

        /* Metadata checksum */
        UINT32DECODE(p, stored_chksum);

        /* Reset checksum field, for computing the checksum */
        /* (Casting away const OK - QAK) */
        HDmemset((uint8_t *)p - H5HF_SIZEOF_CHKSUM, 0, (size_t)H5HF_SIZEOF_CHKSUM);

        /* Compute checksum on entire direct block */
        computed_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

        /* Verify checksum */
        if(stored_chksum != computed_chksum)
            HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap direct block")
    } /* end if */

    /* Sanity check */
    HDassert((size_t)(p - dblock->blk) == (size_t)H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

    /* Set return value */
    ret_value = dblock;

done:
    if(!ret_value && dblock)
        if(H5HF_man_dblock_dest(dblock) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_flush
 *
 * Purpose:	Flushes a dirty fractal heap direct block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock, unsigned H5_ATTR_UNUSED * flags_ptr)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblock);

    if(dblock->cache_info.is_dirty) {
        H5HF_hdr_t *hdr;        /* Shared fractal heap information */
        hbool_t at_tmp_addr = H5F_IS_TMP_ADDR(f, addr);     /* Flag to indicate direct block is at temporary address */
        void *write_buf;        /* Pointer to buffer to write out */
        size_t write_size;      /* Size of buffer to write out */
        uint8_t *p;             /* Pointer into raw data buffer */

        /* Get the pointer to the shared heap header */
        hdr = dblock->hdr;

        /* Set the shared heap header's file context for this operation */
        hdr->f = f;

        HDassert(dblock->blk);
        p = dblock->blk;

        /* Magic number */
        HDmemcpy(p, H5HF_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_DBLOCK_VERSION;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, hdr->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, dblock->block_off, hdr->heap_off_size);

        /* Metadata checksum */
        if(hdr->checksum_dblocks) {
            uint32_t metadata_chksum;       /* Computed metadata checksum value */

            /* Clear the checksum field, to compute the checksum */
            HDmemset(p, 0, (size_t)H5HF_SIZEOF_CHKSUM);

            /* Compute checksum on entire direct block */
            metadata_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

            /* Metadata checksum */
            UINT32ENCODE(p, metadata_chksum);
        } /* end if */

        /* Sanity check */
        HDassert((size_t)(p - dblock->blk) == (size_t)H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

        /* Check for I/O filters on this heap */
        if(hdr->filter_len > 0) {
            H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
            size_t nbytes;                      /* Number of bytes used */
            unsigned filter_mask = 0;           /* Filter mask for block */

            /* Allocate buffer to perform I/O filtering on */
            write_size = dblock->size;
            if(NULL == (write_buf = H5MM_malloc(write_size)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "memory allocation failed for pipeline buffer")
            HDmemcpy(write_buf, dblock->blk, write_size);

            /* Push direct block data through I/O filter pipeline */
            nbytes = write_size;
            if(H5Z_pipeline(&(hdr->pline), 0, &filter_mask, H5Z_ENABLE_EDC, filter_cb, &nbytes, &write_size, &write_buf) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "output pipeline failed")

            /* Use the compressed number of bytes as the size to write */
            write_size = nbytes;

            /* Check for root direct block */
            if(dblock->parent == NULL) {
                hbool_t hdr_changed = FALSE;    /* Whether the header information changed */

                /* Sanity check */
                HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));
                HDassert(hdr->pline_root_direct_size > 0);

                /* Check if the filter mask changed */
                if(hdr->pline_root_direct_filter_mask != filter_mask) {
                    hdr->pline_root_direct_filter_mask = filter_mask;
                    hdr_changed = TRUE;
                } /* end if */

                /* Check if we need to re-size the block on disk */
                if(hdr->pline_root_direct_size != write_size || at_tmp_addr) {
                    /* Check if the direct block is NOT currently allocated in temp. file space */
                    /* (temp. file space does not need to be freed) */
                    if(!at_tmp_addr) {
                        /* Release direct block's current disk space */
                        if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)hdr->pline_root_direct_size) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")
                    } /* end if */

                    /* Allocate space for the compressed direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Let the metadata cache know, if the block moved */
                    if(!H5F_addr_eq(hdr->man_dtable.table_addr, addr))
                        if(H5AC_move_entry(f, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, addr) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTMOVE, FAIL, "unable to move direct block")

                    /* Update information about compressed direct block's location & size */
                    hdr->man_dtable.table_addr = addr;
                    hdr->pline_root_direct_size = write_size;

                    /* Note that heap header was modified */
                    hdr_changed = TRUE;
                } /* end if */

                /* Check if heap header was modified */
                if(hdr_changed)
                    if(H5HF_hdr_dirty(hdr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end if */
            else {
                hbool_t par_changed = FALSE;    /* Whether the parent's information changed */
                H5HF_indirect_t *par_iblock;    /* Parent indirect block */
                unsigned par_entry;             /* Entry in parent indirect block */

                /* Get parent information */
                par_iblock = dblock->parent;
                par_entry = dblock->par_entry;

                /* Sanity check */
                HDassert(H5F_addr_eq(par_iblock->ents[par_entry].addr, addr));
                HDassert(par_iblock->filt_ents[par_entry].size > 0);

                /* Check if the filter mask changed */
                if(par_iblock->filt_ents[par_entry].filter_mask != filter_mask) {
                    par_iblock->filt_ents[par_entry].filter_mask = filter_mask;
                    par_changed = TRUE;
                } /* end if */

                /* Check if we need to re-size the block on disk */
                if(par_iblock->filt_ents[par_entry].size != write_size || at_tmp_addr) {
                    /* Check if the direct block is NOT currently allocated in temp. file space */
                    /* (temp. file space does not need to be freed) */
                    if(!at_tmp_addr) {
                        /* Release direct block's current disk space */
                        if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)par_iblock->filt_ents[par_entry].size) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")
                    } /* end if */

                    /* Allocate space for the compressed direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Let the metadata cache know, if the block moved */
                    if(!H5F_addr_eq(par_iblock->ents[par_entry].addr, addr))
                        if(H5AC_move_entry(f, H5AC_FHEAP_DBLOCK, par_iblock->ents[par_entry].addr, addr) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTMOVE, FAIL, "unable to move direct block")

                    /* Update information about compressed direct block's location & size */
                    par_iblock->ents[par_entry].addr = addr;
                    par_iblock->filt_ents[par_entry].size = write_size;

                    /* Note that parent was modified */
                    par_changed = TRUE;
                } /* end if */

                /* Check if parent was modified */
                if(par_changed)
                    if(H5HF_iblock_dirty(par_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end else */
        } /* end if */
        else {
            write_buf = dblock->blk;
            write_size = dblock->size;

            /* Check for needing to re-allocate direct block from 'temp.' to 'normal' file space */
            if(at_tmp_addr) {
                /* Check for root direct block */
                if(NULL == dblock->parent) {
                    /* Sanity check */
                    HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));

                    /* Allocate 'normal' space for the direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Sanity check */
                    HDassert(!H5F_addr_eq(hdr->man_dtable.table_addr, addr));

                    /* Let the metadata cache know the block moved */
                    if(H5AC_move_entry(f, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTMOVE, FAIL, "unable to move direct block")

                    /* Update information about direct block's location */
                    hdr->man_dtable.table_addr = addr;

                    /* Mark that heap header was modified */
                    if(H5HF_hdr_dirty(hdr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
                } /* end if */
                else {
                    H5HF_indirect_t *par_iblock;    /* Parent indirect block */
                    unsigned par_entry;             /* Entry in parent indirect block */

                    /* Get parent information */
                    par_iblock = dblock->parent;
                    par_entry = dblock->par_entry;

                    /* Sanity check */
                    HDassert(H5F_addr_eq(par_iblock->ents[par_entry].addr, addr));

                    /* Allocate 'normal' space for the direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Sanity check */
                    HDassert(!H5F_addr_eq(par_iblock->ents[par_entry].addr, addr));

                    /* Let the metadata cache know the block moved */
                    if(H5AC_move_entry(f, H5AC_FHEAP_DBLOCK, par_iblock->ents[par_entry].addr, addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTMOVE, FAIL, "unable to move direct block")

                    /* Update information about direct block's location */
                    par_iblock->ents[par_entry].addr = addr;

                    /* Mark that parent was modified */
                    if(H5HF_iblock_dirty(par_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
                } /* end else */
            } /* end if */
        } /* end else */

        /* Direct block must be in 'normal' file space now */
        HDassert(!H5F_IS_TMP_ADDR(f, addr));

	/* Write the direct block */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_DBLOCK, addr, write_size, dxpl_id, write_buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap direct block to disk")

        /* Release the write buffer, if it was allocated */
        if(write_buf != dblock->blk)
            H5MM_xfree(write_buf);

	dblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_dblock_dest(f, dblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_dblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_dest
 *
 * Purpose:	Destroys a fractal heap direct block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_dest(H5F_t *f, H5HF_direct_t *dblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!dblock->cache_info.free_file_space_on_destroy || H5F_addr_defined(dblock->cache_info.addr));

    /* Check for freeing file space for direct block */
    if(dblock->cache_info.free_file_space_on_destroy) {
        /* Sanity check */
        HDassert(dblock->file_size > 0);

        /* Check if the direct block is NOT currently allocated in temp. file space */
        /* (temp. file space does not need to be freed) */
        if(!H5F_IS_TMP_ADDR(f, dblock->cache_info.addr)) {
            /* Release the space on disk */
            /* (XXX: Nasty usage of internal DXPL value! -QAK) */
            if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, H5AC_dxpl_id, dblock->cache_info.addr, dblock->file_size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")
        } /* end if */
    } /* end if */

    /* Destroy fractal heap direct block */
    if(H5HF_man_dblock_dest(dblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_clear
 *
 * Purpose:	Mark a fractal heap direct block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_clear(H5F_t *f, H5HF_direct_t *dblock, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Reset the dirty flag.  */
    dblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HF_cache_dblock_dest(f, dblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_notify
 *
 * Purpose:	Setup / takedown flush dependencies as direct blocks
 *		are loaded / inserted and evicted from the metadata cache.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/17/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_notify(H5C_notify_action_t action, H5HF_direct_t *dblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(dblock);
    HDassert(dblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(dblock->hdr);
    HDassert((dblock->fd_parent) ||
             ((dblock->hdr->man_dtable.curr_root_rows == 0) && (dblock->block_off == (hsize_t)0)));

    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            HDassert(dblock->parent == dblock->fd_parent);
            if(dblock->parent) {        /* this is a leaf dblock */
                /* create flush dependency with parent iblock */
                if(H5AC_create_flush_dependency(dblock->parent, dblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
            } /* end if */
            else {      /* this is a root dblock */
                /* create flush dependency with header */
                if(H5AC_create_flush_dependency(dblock->hdr, dblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")
            } /* end else */
            break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            HDassert((dblock->parent == dblock->fd_parent) ||
                     ((NULL == dblock->parent) && (dblock->fd_parent)));
            if(dblock->fd_parent) {     /* this is a leaf dblock */
                /* destroy flush dependency with parent iblock */
                if(H5AC_destroy_flush_dependency(dblock->fd_parent, dblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
            } /* end if */
            else {      /* this is a root dblock */
                /* destroy flush dependency with header */
                if(H5AC_destroy_flush_dependency(dblock->hdr, dblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
            } /* end else */
            break;

        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "unknown action from metadata cache")
            break;
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_size
 *
 * Purpose:	Compute the size in bytes of a fractal heap direct block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_size(const H5F_t H5_ATTR_UNUSED *f, const H5HF_direct_t *dblock, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check arguments */
    HDassert(dblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_dblock_size() */



/*------------------------------------------------------------------------
 * Function:	H5HF__cache_verify_hdr_descendants_clean
 *
 * Purpose:	Sanity checking routine that verifies that all indirect 
 *		and direct blocks that are descendants of the supplied 
 *		instance of H5HF_hdr_t are clean.  Set *clean to 
 *		TRUE if this is the case, and to FALSE otherwise.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/25/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
static herr_t
H5HF__cache_verify_hdr_descendants_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_hdr_t * hdr, hbool_t *clean)
{
    haddr_t	hdr_addr;               /* Address of header */
    unsigned	hdr_status = 0;         /* Header cache entry status */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(hdr);
    HDassert(hdr->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(hdr->cache_info.type == H5AC_FHEAP_HDR);
    HDassert(clean);
    hdr_addr = hdr->cache_info.addr;
    HDassert(hdr_addr == hdr->heap_addr);

    if(H5AC_get_entry_status(f, hdr_addr, &hdr_status) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get hdr status")
    HDassert(hdr_status & H5AC_ES__IN_CACHE);

    /* We have three basic scenarios we have to deal with:
     *
     * The first, and most common case, is that there is a root iblock.  
     * In this case we need to verify that the root iblock and all its 
     * children are clean.
     *
     * The second, and much less common case, is that in which the 
     * the fractal heap contains only one direct block, which is 
     * pointed to by hdr->man_dtable.table_addr.  In this case, all we 
     * need to do is verify that the root direct block is clean.
     *
     * Finally, it is possible that the fractal heap is empty, and 
     * has neither a root indirect block nor a root direct block.
     * In this case, we have nothing to do.
     */

    /* There are two ways in which we can arrive at the first scenario.
     *
     * By far the most common is when hdr->root_iblock contains a pointer
     * to the root iblock -- in this case the root iblock is almost certainly 
     * pinned, although we can't count on that.
     *
     * However, it is also possible that there is a root iblock that 
     * is no longer pointed to by the header.  In this case, the on 
     * disk address of the iblock will be in hdr->man_dtable.table_addr
     * and hdr->man_dtable.curr_root_rows will contain a positive value.
     *
     * Since the former case is far and away the most common, we don't 
     * worry too much about efficiency in the second case.
     */
    if(hdr->root_iblock ||
             ((hdr->man_dtable.curr_root_rows > 0) &&
               (HADDR_UNDEF != hdr->man_dtable.table_addr))) {
        H5HF_indirect_t *root_iblock = hdr->root_iblock;
        haddr_t		root_iblock_addr;
        unsigned	root_iblock_status = 0;
        hbool_t		root_iblock_in_cache;

        /* make note of the on disk address of the root iblock */
        if(root_iblock == NULL)
	    /* hdr->man_dtable.table_addr must contain address of root
             * iblock.  Check to see if it is in cache.  If it is, 
             * protect it and put its address in root_iblock.
             */
	    root_iblock_addr = hdr->man_dtable.table_addr;
        else
	    root_iblock_addr = root_iblock->addr;

	/* get the status of the root iblock */
	HDassert(root_iblock_addr != HADDR_UNDEF);
        if(H5AC_get_entry_status(f, root_iblock_addr, &root_iblock_status) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get root iblock status")

	root_iblock_in_cache = ( (root_iblock_status & H5AC_ES__IN_CACHE) != 0);
	HDassert(root_iblock_in_cache || (root_iblock == NULL));

	if(!root_iblock_in_cache) /* we are done */
	    *clean = TRUE;
	else if(root_iblock_status & H5AC_ES__IS_DIRTY)
	    *clean = FALSE;
	else { /* must examine children */
            hbool_t	unprotect_root_iblock = FALSE;

	    /* At this point, the root iblock may be pinned, protected,
	     * both, or neither, and we may or may not have a pointer
	     * to root iblock in memory.  
	     *
	     * Before we call H5HF__cache_verify_iblock_descendants_clean(),
	     * we must ensure that the root iblock is either pinned or 
	     * protected or both, and that we have a pointer to it.  
	     * Do this as follows:
	     */
	    if(root_iblock == NULL) {   /* we don't have ptr to root iblock */
		if(0 == (root_iblock_status & H5AC_ES__IS_PROTECTED)) {
		    /* just protect the root iblock -- this will give us
		     * the pointer we need to proceed, and ensure that 
		     * it is locked into the metadata cache for the 
		     * duration.
		     *
		     * Note that the udata is only used in the load callback.
                     * While the fractal heap makes heavy use of the udata
                     * in this case, since we know that the entry is in cache,
                     * we can pass NULL udata.
		     */
                    if(NULL == (root_iblock = (H5HF_indirect_t *)H5AC_protect(f, dxpl_id, H5AC_FHEAP_IBLOCK, root_iblock_addr, NULL, H5AC_READ)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "H5AC_protect() faild.")
                    unprotect_root_iblock = TRUE;
		} /* end if */
                else {
		    /* the root iblock is protected, and we have no
		     * legitimate way of getting a pointer to it.
		     *
		     * We square this circle by using the 
		     * H5AC_get_entry_ptr_from_addr() to get the needed
		     * pointer.
		     *
		     * WARNING: This call should be used only in debugging
                     *          routines, and it should be avoided there when
                     *          possible.
                     *
                     *          Further, if we ever multi-thread the cache,
                     *          this routine will have to be either discarded
                     *          or heavily re-worked.
                     *
                     *          Finally, keep in mind that the entry whose
                     *          pointer is obtained in this fashion may not
                     *          be in a stable state.
                     *
                     * Assuming that the flush dependency code is working
                     * as it should, the only reason for the root iblock to
                     * be unpinned is if none of its children are in cache.
                     * This unfortunately means that if it is protected and
                     * not pinned, the fractal heap is in the process of loading
                     * or inserting one of its children.  The obvious implication
                     * is that there is a significant chance that the root
                     * iblock is in an unstable state.
                     *
                     * All this suggests that using H5AC_get_entry_ptr_from_addr()
		     * to obtain the pointer to the protected root iblock is 
		     * questionable here.  However, since this is test/debugging 
		     * code, I expect that we will use this approach until it 
		     * causes problems, or we think of a better way.
                     */
                    if(H5AC_get_entry_ptr_from_addr(f, root_iblock_addr, (void **)(&root_iblock)) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "H5AC_get_entry_ptr_from_addr() failed.")
                    HDassert(root_iblock);
		} /* end else */
	    } /* end if */
            else {      /* root_iblock != NULL */
		/* we have the pointer to the root iblock.  Protect it 
		 * if it is neither pinned nor protected -- otherwise we 
		 * are ready to go.
		 */
                H5HF_indirect_t *   iblock = NULL;

                if(((root_iblock_status & H5AC_ES__IS_PINNED) == 0) &&
                        ((root_iblock_status & H5AC_ES__IS_PROTECTED) == 0)) {
                    /* the root iblock is neither pinned nor protected -- hence
                     * we must protect it before we proceed
                     *
                     * Note that the udata is only used in the load callback.
                     * While the fractal heap makes heavy use of the udata
                     * in this case, since we know that the entry is in cache,
                     * we can pass NULL udata.
                     */
                    if(NULL == (iblock = (H5HF_indirect_t *)H5AC_protect(f, dxpl_id, H5AC_FHEAP_IBLOCK, root_iblock_addr, NULL, H5AC_READ)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "H5AC_protect() faild.")
                    unprotect_root_iblock = TRUE;
                    HDassert(iblock == root_iblock);
		} /* end if */
	    } /* end else */

            /* at this point, one way or another, the root iblock is locked
             * in memory for the duration of the call.  Do some sanity checks,
	     * and then call H5HF__cache_verify_iblock_descendants_clean().
             */
            HDassert(hdr->root_iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            HDassert(hdr->root_iblock->cache_info.type == H5AC_FHEAP_IBLOCK);

            if(H5HF__cache_verify_iblock_descendants_clean(f, dxpl_id, root_iblock, &root_iblock_status, clean) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify root iblock & descendants clean.")

            /* unprotect the root indirect block if required */
            if(unprotect_root_iblock) {
                HDassert(root_iblock);
                if(H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_IBLOCK, root_iblock_addr, root_iblock, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "H5AC_unprotect() faild.")
            } /* end if */
        } /* end else */
    } /* end if */
    else if((hdr->man_dtable.curr_root_rows == 0) &&
		(HADDR_UNDEF != hdr->man_dtable.table_addr)) {
        haddr_t		root_dblock_addr;
        unsigned	root_dblock_status = 0;
        hbool_t		in_cache;
        hbool_t		type_ok;

	/* this is scenario 2 -- we have a root dblock */
	root_dblock_addr = hdr->man_dtable.table_addr;
        if(H5AC_get_entry_status(f, root_dblock_addr, &root_dblock_status) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get root dblock status")

	if(root_dblock_status & H5AC_ES__IN_CACHE) {
	    if(H5AC_verify_entry_type(f, root_dblock_addr, &H5AC_FHEAP_DBLOCK[0], &in_cache, &type_ok) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't check dblock type")
	    HDassert(in_cache);
	    if(!type_ok)
		HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "root dblock addr doesn't refer to a dblock?!?")

            /* If a root dblock is in cache, it must have a flush
             * dependency relationship with the header, and it
             * may not be the parent in any flush dependency
             * relationship.
             *
             * We don't test this fully, but we will verify that
             * the root iblock is a child in some flush dependency
             * relationship.
             */
            if(0 == (root_dblock_status & H5AC_ES__IS_FLUSH_DEP_CHILD))
                HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "root dblock in cache and not a flush dep child.")
            if(0 != (root_dblock_status & H5AC_ES__IS_FLUSH_DEP_PARENT))
                HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "root dblock in cache and is a flush dep parent.")

	    *clean = ! (root_dblock_status & H5AC_ES__IS_DIRTY);
	} /* end if */
        else    /* root dblock not in cache */
	    *clean = TRUE;
    } /* end else-if */
    else
	/* this is scenario 3 -- the fractal heap is empty, and we 
	 * have nothing to do. 
	 */
	*clean = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF__cache_verify_hdr_descendants_clean() */
#endif /* NDEBUG */


/*------------------------------------------------------------------------
 * Function:	H5HF__cache_verify_iblock_descendants_clean
 *
 * Purpose:	Sanity checking routine that verifies that all indirect 
 *		and direct blocks that are decendents of the supplied 
 *		instance of H5HF_indirect_t are clean.  Set *clean 
 *		to TRUE if this is the case, and to FALSE otherwise.
 *
 *		In passing, the function also does a cursory check to 
 *		spot any obvious errors in the flush dependency setup.  
 *		If any problems are found, the function returns failure.  
 *		Note that these checks are not exhaustive, thus passing 
 *		them does not mean that the flush dependencies are 
 *		correct -- only that there is nothing obviously wrong
 *		with them.
 *
 *		WARNING:  At its top level call, this function is 
 *		intended to be called from H5HF_cache_iblock_flush(), 
 *		and thus presumes that the supplied indirect block 
 *		is in cache.  Any other use of this function and 
 *		its descendants must insure that this assumption is 
 *		met.
 *
 *		Note that this function and 
 *		H5HF__cache_verify_descendant_iblocks_clean() are 
 *		recursive co-routines.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/25/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
static herr_t
H5HF__cache_verify_iblock_descendants_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_indirect_t *iblock, unsigned *iblock_status, hbool_t *clean)
{
    hbool_t	has_dblocks = FALSE;
    hbool_t	has_iblocks = FALSE;
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(iblock);
    HDassert(iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(iblock->cache_info.type == H5AC_FHEAP_IBLOCK);
    HDassert(iblock_status);
    HDassert(clean);
    HDassert(*clean);

    if((*clean) && H5HF__cache_verify_iblocks_dblocks_clean(f, iblock, clean, &has_dblocks) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify dblocks clean.")

    if((*clean) && H5HF__cache_verify_descendant_iblocks_clean(f, dxpl_id, iblock, clean, &has_iblocks) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify iblocks clean.")

    if((NULL == iblock_status) && H5AC_get_entry_status(f, iblock->addr, iblock_status) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get iblock status")

    /* verify that flush dependency setup is plausible */
    if(0 == (*iblock_status & H5AC_ES__IS_FLUSH_DEP_CHILD))
	HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "iblock is not a flush dep child.")
    if(((has_dblocks || has_iblocks)) && (0 == (*iblock_status & H5AC_ES__IS_FLUSH_DEP_PARENT)))
	HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "iblock has children and is not a flush dep parent.")
    if(((has_dblocks || has_iblocks)) && (0 == (*iblock_status & H5AC_ES__IS_PINNED)))
	HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "iblock has children and is not pinned.")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF__cache_verify_iblock_descendants_clean() */
#endif /* NDEBUG */


/*------------------------------------------------------------------------
 * Function:	H5HF__cache_verify_iblocks_dblocks_clean
 *
 * Purpose:	Sanity checking routine that attempts to verify that all
 *		direct blocks pointed to by the supplied indirect block
 *		are either clean, or not in the cache.
 *
 *		In passing, the function also does a cursory check to 
 *		spot any obvious errors in the flush dependency setup.  
 *		If any problems are found, the function returns failure.  
 *		Note that these checks are not exhaustive, thus passing 
 *		them does not mean that the flush dependencies are 
 *		correct -- only that there is nothing obviously wrong
 *		with them.
 *
 *		WARNING:  This function presumes that the supplied 
 *		iblock is in the cache, and will not be removed 
 *		during the call.  Caller must ensure that this is 
 *		the case before the call.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/25/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
static herr_t
H5HF__cache_verify_iblocks_dblocks_clean(H5F_t *f, H5HF_indirect_t *iblock, 
    hbool_t *clean, hbool_t *has_dblocks)
{
    unsigned	num_direct_rows;
    unsigned	max_dblock_index;
    unsigned    i;
    herr_t      ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(iblock);
    HDassert(iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(iblock->cache_info.type == H5AC_FHEAP_IBLOCK);
    HDassert(clean);
    HDassert(*clean);
    HDassert(has_dblocks);

    i = 0;
    num_direct_rows = MIN(iblock->nrows, iblock->hdr->man_dtable.max_direct_rows);
    HDassert(num_direct_rows <= iblock->nrows);
    max_dblock_index = (num_direct_rows * iblock->hdr->man_dtable.cparam.width) - 1;
    while((*clean) && (i <= max_dblock_index)) {
        haddr_t     dblock_addr;

        dblock_addr = iblock->ents[i].addr;
	if(H5F_addr_defined(dblock_addr)) {
            hbool_t	in_cache;
            hbool_t	type_ok;

	    if(H5AC_verify_entry_type(f, dblock_addr, &H5AC_FHEAP_DBLOCK[0], &in_cache, &type_ok) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't check dblock type")

	    if(in_cache) { /* dblock is in cache */
                unsigned 	dblock_status = 0;

		if(!type_ok)
		    HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "dblock addr doesn't refer to a dblock?!?")

                if(H5AC_get_entry_status(f, dblock_addr, &dblock_status) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get dblock status")
                HDassert(dblock_status & H5AC_ES__IN_CACHE);

	        *has_dblocks = TRUE;
                if(dblock_status & H5AC_ES__IS_DIRTY)
		    *clean = FALSE;

	        /* If a child dblock is in cache, it must have a flush 
                 * dependency relationship with this iblock, and it 
                 * may not be the parent in any flush dependency 
                 * relationship.  
                 * 
                 * We don't test this fully, but we will verify that 
                 * the child iblock is a child in some flush dependency 
                 * relationship.
                 */
	        if(0 == (dblock_status & H5AC_ES__IS_FLUSH_DEP_CHILD))
		    HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "dblock in cache and not a flush dep child.")
	        
                if(0 != (dblock_status & H5AC_ES__IS_FLUSH_DEP_PARENT)) 
		    HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "dblock in cache and is a flush dep parent.")
   
            } /* end if */
        } /* end if */

        i++;
    } /* end while */
    
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF__cache_verify_iblocks_dblocks_clean() */
#endif /* NDEBUG */


/*------------------------------------------------------------------------
 * Function:	H5HF__cache_verify_descendant_iblocks_clean
 *
 * Purpose:	Sanity checking routine that attempts to verify that all
 *		direct blocks pointed to by the supplied indirect block
 *		are either clean, or not in the cache.
 *
 *		In passing, the function also does a cursory check to 
 *		spot any obvious errors in the flush dependency setup.  
 *		If any problems are found, the function returns failure.  
 *		Note that these checks are not exhaustive, thus passing 
 *		them does not mean that the flush dependencies are 
 *		correct -- only that there is nothing obviously wrong
 *		with them.
 *
 *		WARNING:  This function presumes that the supplied 
 *		iblock is in the cache, and will not be removed 
 *		during the call.  Caller must ensure that this is 
 *		the case before the call.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/25/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
static herr_t
H5HF__cache_verify_descendant_iblocks_clean(H5F_t *f, hid_t dxpl_id,
    H5HF_indirect_t *iblock, hbool_t *clean, hbool_t *has_iblocks)
{
    unsigned	      first_iblock_index;
    unsigned	      last_iblock_index;
    unsigned	      num_direct_rows;
    unsigned	      i;
    herr_t            ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(iblock);
    HDassert(iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(iblock->cache_info.type == H5AC_FHEAP_IBLOCK);
    HDassert(clean);
    HDassert(*clean);
    HDassert(has_iblocks);
    num_direct_rows = MIN(iblock->nrows, iblock->hdr->man_dtable.max_direct_rows);
    HDassert(num_direct_rows <= iblock->nrows);

    first_iblock_index = num_direct_rows * iblock->hdr->man_dtable.cparam.width;
    last_iblock_index = (iblock->nrows * iblock->hdr->man_dtable.cparam.width) - 1;

    i = first_iblock_index;
    while((*clean) && (i <= last_iblock_index)) {
        haddr_t           child_iblock_addr = iblock->ents[i].addr;

	if(H5F_addr_defined(child_iblock_addr)) {
            unsigned 	      child_iblock_status = 0;

            if(H5AC_get_entry_status(f, child_iblock_addr, &child_iblock_status) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "can't get iblock status")

	    if(child_iblock_status & H5AC_ES__IN_CACHE) {
	        *has_iblocks = TRUE;
                if(child_iblock_status & H5AC_ES__IS_DIRTY)
		    *clean = FALSE;

                /* if the child iblock is in cache and *clean is TRUE, 
                 * we must continue to explore down the fractal heap tree
                 * structure to verify that all descendant blocks are either
                 * clean, or not in the metadata cache.  We do this with a 
                 * recursive call to 
		 * H5HF__cache_verify_iblock_descendants_clean().
		 * However, we can't make this call unless the child iblock
                 * is somehow locked into the cache -- typically via either 
 		 * pinning or protecting.
                 *
                 * If the child iblock is pinned, we can look up its pointer
                 * on the current iblock's pinned child iblock list, and 
                 * and use that pointer in the recursive call.
                 *
                 * If the entry is unprotected and unpinned, we simply
                 * protect it.
                 *
		 * If, however, the the child iblock is already protected, 
                 * but not pinned, we have a bit of a problem, as we have 
		 * no legitimate way of looking up its pointer in memory.
		 *
		 * To solve this problem, I have added a new metadata cache
		 * call to obtain the pointer.  
		 *
		 * WARNING: This call should be used only in debugging 
		 * 	    routines, and it should be avoided there when 
		 *	    possible.  
		 *
		 *          Further, if we ever multi-thread the cache, 
		 *	    this routine will have to be either discarded 
		 *	    or heavily re-worked.
		 *
		 *	    Finally, keep in mind that the entry whose 
		 *	    pointer is obtained in this fashion may not 
		 *          be in a stable state.  
		 *
		 * Assuming that the flush dependency code is working 
		 * as it should, the only reason for the child entry to 
		 * be unpinned is if none of its children are in cache.
		 * This unfortunately means that if it is protected and 
		 * not pinned, the fractal heap is in the process of loading
		 * or inserting one of its children.  The obvious implication
		 * is that there is a significant chance that the child 
		 * iblock is in an unstable state.
		 *
		 * All this suggests that using the new call to obtain the 
		 * pointer to the protected child iblock is questionable 
		 * here.  However, since this is test/debugging code, I
		 * expect that we will use this approach until it causes
		 * problems, or we think of a better way.
                 */
                if(*clean) {
                    H5HF_indirect_t *child_iblock = NULL;
                    hbool_t unprotect_child_iblock = FALSE;

		    if(0 == (child_iblock_status & H5AC_ES__IS_PINNED)) {
			/* child iblock is not pinned */
			if(0 == (child_iblock_status & H5AC_ES__IS_PROTECTED)) {
			    /* child iblock is unprotected, and unpinned */
			    /* protect it.  Note that the udata is only  */
			    /* used in the load callback.  While the     */
			    /* fractal heap makes heavy use of the udata */
			    /* in this case, since we know that the      */
			    /* entry is in cache, we can pass NULL udata */

			    if(NULL == (child_iblock = (H5HF_indirect_t *) H5AC_protect(f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, NULL, H5AC_READ)))
                                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "H5AC_protect() faild.")
			    unprotect_child_iblock = TRUE;
			} /* end if */
                        else {
			    /* child iblock is protected -- use             */
			    /* H5AC_get_entry_ptr_from_addr() to get a      */
			    /* pointer to the entry.  This is very slimy -- */
			    /* come up with a better solution.              */
			    if(H5AC_get_entry_ptr_from_addr(f, child_iblock_addr, (void **)(&child_iblock)) < 0)
                                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "H5AC_get_entry_ptr_from_addr() faild.")
			    HDassert(child_iblock);
			} /* end else */
		    } /* end if */
                    else {
			/* child iblock is pinned -- look it up in the */
			/* parent iblocks child_iblocks array.         */
			HDassert(iblock->child_iblocks);
			child_iblock = iblock->child_iblocks[i - first_iblock_index];
		    } /* end else */

		    /* At this point, one way or another we should have 
                     * a pointer to the child iblock.  Verify that we 
                     * that we have the correct one.
                     */
		    HDassert(child_iblock);
    		    HDassert(child_iblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    		    HDassert(child_iblock->cache_info.type == H5AC_FHEAP_IBLOCK);
		    HDassert(child_iblock->addr == child_iblock_addr);

		    /* now make the recursive call */
		    if(H5HF__cache_verify_iblock_descendants_clean(f, dxpl_id, child_iblock, &child_iblock_status, clean) < 0)
        		HGOTO_ERROR(H5E_HEAP, H5E_SYSTEM, FAIL, "can't verify child iblock clean.")

		    /* if we protected the child iblock, unprotect it now */
		    if(unprotect_child_iblock) {
			if(H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, child_iblock, H5AC__NO_FLAGS_SET) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "H5AC_unprotect() faild.")

                    } /* end if */
                } /* end if */
            } /* end if */
        } /* end if */

        i++;
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF__cache_verify_descendant_iblocks_clean() */
#endif /* NDEBUG */

