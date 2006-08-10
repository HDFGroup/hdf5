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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5HF_HDR_VERSION        0               /* Header */
#define H5HF_DBLOCK_VERSION     0               /* Direct block */
#define H5HF_IBLOCK_VERSION     0               /* Indirect block */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static H5HF_hdr_t *H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_hdr_t *hdr);
static herr_t H5HF_cache_hdr_clear(H5F_t *f, H5HF_hdr_t *hdr, hbool_t destroy);
static herr_t H5HF_cache_hdr_size(const H5F_t *f, const H5HF_hdr_t *hdr, size_t *size_ptr);
static H5HF_direct_t *H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock);
static herr_t H5HF_cache_dblock_clear(H5F_t *f, H5HF_direct_t *dblock, hbool_t destroy);
static herr_t H5HF_cache_dblock_size(const H5F_t *f, const H5HF_direct_t *dblock, size_t *size_ptr);
static H5HF_indirect_t *H5HF_cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5HF_cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_indirect_t *iblock);
static herr_t H5HF_cache_iblock_clear(H5F_t *f, H5HF_indirect_t *iblock, hbool_t destroy);
static herr_t H5HF_cache_iblock_size(const H5F_t *f, const H5HF_indirect_t *iblock, size_t *size_ptr);

/* Local encode/decode routines */
static herr_t H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable);
static herr_t H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable);

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
    (H5AC_size_func_t)H5HF_cache_hdr_size,
}};

/* H5HF direct block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_DBLOCK[1] = {{
    H5AC_FHEAP_DBLOCK_ID,
    (H5AC_load_func_t)H5HF_cache_dblock_load,
    (H5AC_flush_func_t)H5HF_cache_dblock_flush,
    (H5AC_dest_func_t)H5HF_cache_dblock_dest,
    (H5AC_clear_func_t)H5HF_cache_dblock_clear,
    (H5AC_size_func_t)H5HF_cache_dblock_size,
}};

/* H5HF indirect block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_IBLOCK[1] = {{
    H5AC_FHEAP_IBLOCK_ID,
    (H5AC_load_func_t)H5HF_cache_iblock_load,
    (H5AC_flush_func_t)H5HF_cache_iblock_flush,
    (H5AC_dest_func_t)H5HF_cache_iblock_dest,
    (H5AC_clear_func_t)H5HF_cache_iblock_clear,
    (H5AC_size_func_t)H5HF_cache_iblock_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage heap header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_DEFINE(direct_block);

/* Declare a free list to manage heap indirect block data to/from disk */
H5FL_BLK_DEFINE_STATIC(indirect_block);



/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_decode
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
H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_decode)

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
} /* end H5HF_dtable_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_encode
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
H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_encode)

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

    /* Address of table */
    H5F_addr_encode(f, pp, dtable->table_addr);

    /* Current # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_load
 *
 * Purpose:	Loads a fractal heap header from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_hdr_t *
H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5HF_hdr_t		*hdr = NULL;     /* Fractal heap info */
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    uint8_t             heap_flags;     /* Status flags for heap */
    H5HF_hdr_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_load)
#ifdef QAK
HDfprintf(stderr, "%s: Load heap header, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the fractal heap data structure */
    if(NULL == (hdr = H5HF_hdr_alloc(f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set the heap header's address */
    hdr->heap_addr = addr;

    /* Compute the size of the fractal heap header on disk */
    size = H5HF_HEADER_SIZE(hdr);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap header")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_HDR_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header version")

    /* Metadata flags (unused, currently) */
/* XXX: Plan out metadata flags (including "read-only duplicate" feature) */
    if(*p++ != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unknown metadata flag in fractal heap header")

    /* Metadata checksum (unused, currently) */
    UINT32DECODE(p, metadata_chksum);
/* XXX: Verify checksum */
    if(metadata_chksum != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect metadata checksum for fractal heap header")

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    heap_flags = *p++;
    hdr->huge_ids_wrapped = heap_flags & H5HF_HDR_FLAGS_HUGE_ID_WRAPPED;

    /* "Huge" object information */
    UINT32DECODE(p, hdr->max_man_size);         /* Max. size of "managed" objects */
    H5F_DECODE_LENGTH(f, p, hdr->huge_next_id); /* Next ID to use for "huge" object */
    H5F_addr_decode(f, &p, &hdr->huge_bt2_addr); /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_DECODE_LENGTH(f, p, hdr->total_man_free); /* Internal free space in managed direct blocks */
    H5F_addr_decode(f, &p, &hdr->fs_addr);      /* Address of free section header */

    /* Heap statistics */
    H5F_DECODE_LENGTH(f, p, hdr->man_size);
    H5F_DECODE_LENGTH(f, p, hdr->man_alloc_size);
    H5F_DECODE_LENGTH(f, p, hdr->man_iter_off);
    H5F_DECODE_LENGTH(f, p, hdr->man_nobjs);
    H5F_DECODE_LENGTH(f, p, hdr->huge_size);
    H5F_DECODE_LENGTH(f, p, hdr->huge_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_decode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, NULL, "unable to encode managed obj. doubling table info")

    HDassert((size_t)(p - buf) == size);

    /* Finish initialization of heap header */
    if(H5HF_hdr_finish_init(hdr) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "can't finish initializing shared fractal heap header")
#ifdef QAK
HDfprintf(stderr, "%s: hdr->fspace = %p\n", FUNC, hdr->fspace);
#endif /* QAK */

    /* Set return value */
    ret_value = hdr;

done:
    if(buf)
        H5FL_BLK_FREE(header_block, buf);
    if(!ret_value && hdr)
        (void)H5HF_cache_hdr_dest(f, hdr);

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
H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_flush)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing heap header, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);

    if(hdr->cache_info.is_dirty) {
        uint8_t	*buf;           /* Temporary raw data buffer */
        uint8_t *p;             /* Pointer into raw data buffer */
        size_t	size;           /* Header size on disk */
        uint8_t heap_flags;     /* Status flags for heap */

        /* Sanity check */
        HDassert(hdr->dirty);

        /* Compute the size of the heap header on disk */
        size = H5HF_HEADER_SIZE(hdr);

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_HDR_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, (size_t)4);
        p += 4;

        /* Heap status flags */
        /* (bit 0: "huge" object IDs have wrapped) */
        heap_flags = 0;
        heap_flags |= (hdr->huge_ids_wrapped ?  H5HF_HDR_FLAGS_HUGE_ID_WRAPPED : 0);
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

        /* Managed objects' doubling-table info */
        if(H5HF_dtable_encode(hdr->f, &p, &(hdr->man_dtable)) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "unable to encode managed obj. doubling table info")

	/* Write the heap header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap header to disk")

        H5FL_BLK_FREE(header_block, buf);

	hdr->dirty = FALSE;
	hdr->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_hdr_dest(f, hdr) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap header")

done:
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
/* ARGSUSED */
herr_t
H5HF_cache_hdr_dest(H5F_t UNUSED *f, H5HF_hdr_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(hdr->rc == 0);

    /* Free the block size lookup table for the doubling table */
    H5HF_dtable_dest(&hdr->man_dtable);

    /* Free the shared info itself */
    H5FL_FREE(H5HF_hdr_t, hdr);

    FUNC_LEAVE_NOAPI(SUCCEED)
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

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_clear)

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
H5HF_cache_hdr_size(const H5F_t UNUSED *f, const H5HF_hdr_t *hdr, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5HF_HEADER_SIZE(hdr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_hdr_size() */


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
H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_size, void *_par_info)
{
    const size_t        *size = (const size_t *)_size;         /* Size of block */
    H5HF_parent_t       *par_info = (H5HF_parent_t *)_par_info; /* Pointer to parent information */
    H5HF_direct_t	*dblock = NULL; /* Direct block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    H5HF_direct_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_load)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(par_info);

    /* Allocate space for the fractal heap direct block */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    dblock->hdr = par_info->hdr;
    if(H5HF_hdr_incr(dblock->hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    dblock->size = *size;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dblock->size);

    /* Allocate block buffer */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, (size_t)dblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read direct block from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, (size_t)dblock->size, dxpl_id, dblock->blk) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")

    p = dblock->blk;

    /* Magic number */
    if(HDmemcmp(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_DBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block version")

    /* Metadata flags (unused, currently) */
/* XXX: Plan out metadata flags (including "read-only duplicate" feature) */
    if(*p++ != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unknown metadata flag in fractal heap direct block")

    /* Metadata checksum (unused, currently) */
    UINT32DECODE(p, metadata_chksum);
/* XXX: Verify checksum */
    if(metadata_chksum != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect metadata checksum for fractal heap direct block")

    /* Address of heap that owns this block (skip) */
    H5F_addr_decode(f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, dblock->hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    dblock->parent = par_info->iblock;
    dblock->par_entry = par_info->entry;
    if(dblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
    } /* end if */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, dblock->block_off, dblock->hdr->heap_off_size);

    /* Set return value */
    ret_value = dblock;

done:
    if(!ret_value && dblock)
        (void)H5HF_cache_dblock_dest(f, dblock);

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
H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_flush)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblock);

    if(dblock->cache_info.is_dirty) {
        H5HF_hdr_t *hdr;        /* Shared fractal heap information */
        uint8_t *p;             /* Pointer into raw data buffer */

        /* Get the pointer to the shared heap header */
        hdr = dblock->hdr;

        HDassert(dblock->blk);
        p = dblock->blk;

        /* Magic number */
        HDmemcpy(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_DBLOCK_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, (size_t)4);
        p += 4;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, hdr->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, dblock->block_off, hdr->heap_off_size);

        /* Sanity check */
        HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

	/* Write the direct block */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_DBLOCK, addr, (size_t)dblock->size, dxpl_id, dblock->blk) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap direct block to disk")

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
/* ARGSUSED */
herr_t
H5HF_cache_dblock_dest(H5F_t UNUSED *f, H5HF_direct_t *dblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_dest)

    /*
     * Check arguments.
     */
    HDassert(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: Destroying direct block, dblock = %p\n", FUNC, dblock);
#endif /* QAK */

    /* Decrement reference count on shared fractal heap info */
    HDassert(dblock->hdr);
    if(H5HF_hdr_decr(dblock->hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared heap header")
    if(dblock->parent)
        if(H5HF_iblock_decr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Free block's buffer */
    H5FL_BLK_FREE(direct_block, dblock->blk);

    /* Free fractal heap direct block info */
    H5FL_FREE(H5HF_direct_t, dblock);

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

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_clear)

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
H5HF_cache_dblock_size(const H5F_t UNUSED *f, const H5HF_direct_t *dblock, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_dblock_size)

    /* check arguments */
    HDassert(dblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_dblock_size() */


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
H5HF_cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrows, void *_par_info)
{
    const unsigned      *nrows = (const unsigned *)_nrows;     /* # of rows in indirect block */
    H5HF_parent_t       *par_info = (H5HF_parent_t *)_par_info;     /* Shared parent information */
    H5HF_indirect_t	*iblock = NULL; /* Indirect block info */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    size_t              u;              /* Local index variable */
    H5HF_indirect_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_load)
#ifdef QAK
HDfprintf(stderr, "%s: Load indirect block, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(par_info);

    /* Allocate space for the fractal heap indirect block */
    if(NULL == (iblock = H5FL_MALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    iblock->hdr = par_info->hdr;
    if(H5HF_hdr_incr(iblock->hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    iblock->rc = 0;
    iblock->nrows = *nrows;
    iblock->addr = addr;
    iblock->nchildren = 0;

    /* Compute size of indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(iblock->hdr, iblock);

    /* Allocate buffer to decode block */
/* XXX: Use free list factories? */
    if((buf = H5FL_BLK_MALLOC(indirect_block, iblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read indirect block from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap indirect block")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap indirect block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_IBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block version")

    /* Metadata flags (unused, currently) */
/* XXX: Plan out metadata flags (including "read-only duplicate" feature) */
    if(*p++ != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unknown metadata flag in fractal heap direct block")

    /* Metadata checksum (unused, currently) */
    UINT32DECODE(p, metadata_chksum);
/* XXX: Verify checksum */
    if(metadata_chksum != 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect metadata checksum for fractal heap direct block")

    /* Address of heap that owns this block */
    H5F_addr_decode(f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, iblock->hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    iblock->parent = par_info->iblock;
    iblock->par_entry = par_info->entry;
    if(iblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

        /* Set max. # of rows in this block */
        iblock->max_rows = iblock->nrows;
    } /* end if */
    else {
        /* Set max. # of rows in this block */
        iblock->max_rows = iblock->hdr->man_dtable.max_root_rows;
    } /* end else */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, iblock->block_off, iblock->hdr->heap_off_size);

    /* Allocate & decode indirect block entry tables */
    HDassert(iblock->nrows > 0);
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (size_t)(iblock->nrows * iblock->hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")
    for(u = 0; u < (iblock->nrows * iblock->hdr->man_dtable.cparam.width); u++) {
        /* Decode child block address */
        H5F_addr_decode(f, &p, &(iblock->ents[u].addr));

        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            iblock->nchildren++;
            iblock->max_child = u;
        } /* end if */

#ifdef LATER
        /* Decode direct & indirect blocks differently (later, when direct blocks can be compressed) */
        if(u < (iblock->hdr->man_dtable.max_direct_rows * iblock->hdr->man_dtable.cparam.width))
            UINT32DECODE_VAR(p, iblock->ents[u].free_space, iblock->hdr->man_dtable.max_dir_blk_off_size)
#endif /* LATER */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->ents[%Zu] = {%a}\n", FUNC, u, iblock->ents[u].addr);
#endif /* QAK */
    } /* end for */

    /* Sanity checks */
    HDassert((size_t)(p - buf) == iblock->size);
    HDassert(iblock->nchildren);        /* indirect blocks w/no children should have been deleted */

    /* Set return value */
    ret_value = iblock;

done:
    /* Free buffer */
/* XXX: Keep buffer around? */
    H5FL_BLK_FREE(indirect_block, buf);

    if(!ret_value && iblock)
        (void)H5HF_cache_iblock_dest(f, iblock);

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
H5HF_cache_iblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_indirect_t *iblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_flush)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing indirect block, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);

    if(iblock->cache_info.is_dirty) {
        H5HF_hdr_t *hdr;                /* Shared fractal heap information */
        uint8_t	*buf = NULL;            /* Temporary buffer */
        uint8_t *p;                     /* Pointer into raw data buffer */
#ifndef NDEBUG
        unsigned nchildren = 0;         /* Track # of children */
        unsigned max_child = 0;         /* Track max. child entry used */
#endif /* NDEBUG */
        size_t u;                       /* Local index variable */

        /* Get the pointer to the shared heap header */
        hdr = iblock->hdr;

        /* Allocate buffer to encode block */
/* XXX: Use free list factories? */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: iblock->size = %Zu\n", FUNC, iblock->size);
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: hdr->man_dtable.cparam.width = %u\n", FUNC, hdr->man_dtable.cparam.width);
#endif /* QAK */
        if((buf = H5FL_BLK_MALLOC(indirect_block, iblock->size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_IBLOCK_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, (size_t)4);
        p += 4;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, hdr->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, iblock->block_off, hdr->heap_off_size);

        /* Encode indirect block-specific fields */
        for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->ents[%Zu] = {%a}\n", FUNC, u, iblock->ents[u].addr);
#endif /* QAK */
            /* Encode child block address */
            H5F_addr_encode(f, &p, iblock->ents[u].addr);

#ifndef NDEBUG
            /* Count child blocks */
            if(H5F_addr_defined(iblock->ents[u].addr)) {
                nchildren++;
                if(u > max_child)
                    max_child = u;
            } /* end if */
#endif /* NDEBUG */

#ifdef LATER
            /* Encode direct & indirect blocks differently (when direct blocks can be compressed) */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width))
                UINT32ENCODE_VAR(p, iblock->ents[u].free_space, hdr->man_dtable.max_dir_blk_off_size)
#endif /* LATER */
        } /* end for */

        /* Sanity check */
        HDassert((size_t)(p - buf) == iblock->size);
#ifndef NDEBUG
        HDassert(nchildren == iblock->nchildren);
        HDassert(max_child == iblock->max_child);
#endif /* NDEBUG */

	/* Write the indirect block */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap indirect block to disk")

        /* Free buffer */
        H5FL_BLK_FREE(indirect_block, buf);

        /* Reset dirty flags */
	iblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_iblock_dest(f, iblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap indirect block")

done:
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
/* ARGSUSED */
herr_t
H5HF_cache_iblock_dest(H5F_t UNUSED *f, H5HF_indirect_t *iblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_dest)

    /*
     * Check arguments.
     */
    HDassert(iblock);
    HDassert(iblock->rc == 0);
#ifdef QAK
HDfprintf(stderr, "%s: Destroying indirect block\n", FUNC);
#endif /* QAK */

    /* Decrement reference count on shared info */
    HDassert(iblock->hdr);
    if(H5HF_hdr_decr(iblock->hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared heap header")
    if(iblock->parent)
        if(H5HF_iblock_decr(iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Release entry tables */
    if(iblock->ents)
        H5FL_SEQ_FREE(H5HF_indirect_ent_t, iblock->ents);

    /* Free fractal heap indirect block info */
    H5FL_FREE(H5HF_indirect_t, iblock);

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

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_clear)

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
H5HF_cache_iblock_size(const H5F_t UNUSED *f, const H5HF_indirect_t *iblock, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_iblock_size)

    /* check arguments */
    HDassert(iblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = iblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_iblock_size() */

