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
static H5HF_t *H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_t *fh);
static herr_t H5HF_cache_hdr_clear(H5F_t *f, H5HF_t *fh, hbool_t destroy);
static herr_t H5HF_cache_hdr_size(const H5F_t *f, const H5HF_t *fh, size_t *size_ptr);
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

    /* Next direct block's heap offset */
    H5F_DECODE_LENGTH(f, *pp, dtable->next_dir_block);

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

    /* Next direct block's heap offset */
    H5F_ENCODE_LENGTH(f, *pp, dtable->next_dir_block);

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
static H5HF_t *
H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5HF_t		*fh = NULL;     /* Fractal heap info */
    H5HF_shared_t       *shared = NULL; /* Shared fractal heap information */
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    H5HF_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5HF_cache_hdr_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the fractal heap data structure */
    if(NULL == (fh = H5FL_MALLOC(H5HF_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&fh->cache_info, 0, sizeof(H5AC_info_t));

    /* Allocate & basic initialization for the shared info struct */
    if(NULL == (shared = H5HF_shared_alloc(f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate space for shared heap info")
    shared->heap_addr = addr;

    /* Compute the size of the fractal heap header on disk */
    size = H5HF_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap header")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, H5HF_SIZEOF_MAGIC))
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

    /* Heap address mapping */
    shared->addrmap = *p++;
    HDassert(H5HF_ABSOLUTE == 0);
    if(shared->addrmap > H5HF_MAPPED)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect fractal heap address mapping")

    /* Min. size of standalone objects */
    UINT32DECODE(p, shared->standalone_size);

    /* Size of ref. count for objects in heap */
    shared->ref_count_size = *p++;

    /* Internal management information */
    H5F_DECODE_LENGTH(f, p, shared->total_man_free);
    H5F_DECODE_LENGTH(f, p, shared->total_std_free);

    /* Statistics information */
    H5F_DECODE_LENGTH(f, p, shared->total_size);
    H5F_DECODE_LENGTH(f, p, shared->man_size);
    H5F_DECODE_LENGTH(f, p, shared->std_size);
    H5F_DECODE_LENGTH(f, p, shared->nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_decode(shared->f, &p, &(shared->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, NULL, "unable to encode managed obj. doubling table info")

    HDassert((size_t)(p - buf) == size);

    /* Make shared heap info reference counted */
    if(H5HF_shared_own(fh, shared) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create ref-count wrapper for shared fractal heap info")

    /* Set return value */
    ret_value = fh;

done:
    if(buf)
        H5FL_BLK_FREE(header_block, buf);
    if(!ret_value && fh)
        (void)H5HF_cache_hdr_dest(f, fh);

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
H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_t *fh)
{
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HF_cache_hdr_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fh);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    if(fh->cache_info.is_dirty) {
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */

        /* Sanity check */
        HDassert(shared->dirty);

        /* Compute the size of the heap header on disk */
        size = H5HF_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_HDR_MAGIC, H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_HDR_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, 4);
        p += 4;

        /* Heap address mapping */
        *p++ = shared->addrmap;

        /* Min. size of standalone objects */
        UINT32ENCODE(p, shared->standalone_size);

        /* Size of ref. count for objects in heap */
        *p++ = shared->ref_count_size;

        /* Internal management information */
        H5F_ENCODE_LENGTH(f, p, shared->total_man_free);
        H5F_ENCODE_LENGTH(f, p, shared->total_std_free);

        /* Statistics information */
        H5F_ENCODE_LENGTH(f, p, shared->total_size);
        H5F_ENCODE_LENGTH(f, p, shared->man_size);
        H5F_ENCODE_LENGTH(f, p, shared->std_size);
        H5F_ENCODE_LENGTH(f, p, shared->nobjs);

        /* Managed objects' doubling-table info */
        if(H5HF_dtable_encode(shared->f, &p, &(shared->man_dtable)) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "unable to encode managed obj. doubling table info")

	/* Write the heap header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap header to disk")

        H5FL_BLK_FREE(header_block, buf);

	shared->dirty = FALSE;
	fh->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_hdr_dest(f, fh) < 0)
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
H5HF_cache_hdr_dest(H5F_t UNUSED *f, H5HF_t *fh)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(fh);

    /* Decrement reference count on shared fractal heap info */
    if(fh->shared)
        H5RC_DEC(fh->shared);

    /* Free fractal heap header info */
    H5FL_FREE(H5HF_t, fh);

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
H5HF_cache_hdr_clear(H5F_t *f, H5HF_t *fh, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(fh);

    /* Reset the dirty flag.  */
    fh->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HF_cache_hdr_dest(f, fh) < 0)
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
H5HF_cache_hdr_size(const H5F_t *f, const H5HF_t UNUSED *fh, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5HF_HEADER_SIZE(f);

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
H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_size, void *_par_shared)
{
    const size_t        *size = (const size_t *)_size;         /* Size of block */
    H5HF_parent_shared_t *par_shared = (H5HF_parent_shared_t *)_par_shared;     /* Shared direct block information */
    H5HF_shared_t       *shared = NULL; /* Shared fractal heap information */
    H5HF_direct_t	*dblock = NULL; /* Direct block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    H5HF_direct_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5HF_cache_dblock_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(par_shared);

    /* Allocate space for the fractal heap direct block */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    dblock->shared = par_shared->shared;
    H5RC_INC(dblock->shared);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(dblock->shared);
    HDassert(shared);

    /* Share common parent [indirect] block information */
    dblock->parent = par_shared->parent;
    if(dblock->parent)
        H5RC_INC(dblock->parent);
    dblock->parent_entry = par_shared->parent_entry;
    dblock->blk_free_space = (size_t)par_shared->parent_free_space;

    /* Set block's internal information */
    dblock->size = *size;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dblock->size);
    dblock->free_list = NULL;

    /* Allocate block buffer */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, (size_t)dblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read direct block from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, (size_t)dblock->size, dxpl_id, dblock->blk) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")

    p = dblock->blk;

    /* Magic number */
    if(HDmemcmp(p, H5HF_DBLOCK_MAGIC, H5HF_SIZEOF_MAGIC))
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
    if(H5F_addr_ne(heap_addr, shared->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")
    
    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, dblock->block_off, shared->heap_off_size);

    /* Offset of free list head */
    /* (Defer deserializing the whole free list until we actually need to modify it) */
    UINT64DECODE_VAR(p, dblock->free_list_head, dblock->blk_off_size);

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

    FUNC_ENTER_NOAPI(H5HF_cache_dblock_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblock);

    if(dblock->cache_info.is_dirty) {
        H5HF_shared_t *shared;      /* Shared fractal heap information */
        uint8_t *p;                 /* Pointer into raw data buffer */

        /* Get the pointer to the shared heap info */
        shared = H5RC_GET_OBJ(dblock->shared);
        HDassert(shared);

        p = dblock->blk;

        /* Magic number */
        HDmemcpy(p, H5HF_DBLOCK_MAGIC, H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_DBLOCK_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, 4);
        p += 4;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, shared->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, dblock->block_off, shared->heap_off_size);

        /* Check for (currently) unsupported address mapping */
        if(shared->addrmap != H5HF_ABSOLUTE)
            HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "encoding mapped direct blocks not supported currently")

        /* Offset of free list head */
        UINT64ENCODE_VAR(p, dblock->free_list_head, dblock->blk_off_size);

        /* Sanity check */
        HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(shared, dblock));

        /* Check for dirty free list */
        if(dblock->free_list && dblock->free_list->dirty) {
            H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */

            /* Loop over all free list blocks, updating their data */
            node = dblock->free_list->first;
            while(node) {
                /* Find first node which has enough room to describe free space */
                while(node && node->size < H5HF_MAN_ABS_DIRECT_FREE_NODE_SIZE(dblock))
                    node = node->next;

                /* Check for free space node to encode */
                if(node) {
                    H5HF_direct_free_node_t *next_node; /* Pointer to next free list node for block */

                    /* Probe ahead for next node that is large enough to encode free space description */
                    next_node = node->next;
                    while(next_node && next_node->size < H5HF_MAN_ABS_DIRECT_FREE_NODE_SIZE(dblock))
                        next_node = next_node->next;

                    /* Encode information for this node on free list */
                    p = dblock->blk + node->my_offset;
                    UINT64ENCODE_VAR(p, node->size, dblock->blk_off_size);
                    UINT64ENCODE_VAR(p, (next_node ? next_node->my_offset : 0), dblock->blk_off_size);

                    /* Advance to next node */
                    node = node->next;
                } /* end if */

            } /* end while */

            /* Reset the free list dirty flag */
            dblock->free_list->dirty = FALSE;
        } /* end if */

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
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_dblock_dest)

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Decrement reference count on shared fractal heap info */
    if(dblock->shared)
        H5RC_DEC(dblock->shared);
    if(dblock->parent)
        H5RC_DEC(dblock->parent);

    /* Check for free list & free it, if necessary */
    if(dblock->free_list) {
        H5HF_direct_free_node_t *node; /* Pointer to free list node for block */

        /* Walk through list, freeing the nodes */
        node = dblock->free_list->first;
        while(node) {
            H5HF_direct_free_node_t *last_node; /* Pointer to last free list node for block */

            /* Advance to next node */
            last_node = node;
            node = node->next;

            /* Release the last node */
            H5FL_FREE(H5HF_direct_free_node_t, last_node);
        } /* end while */

        /* Release the free list head */
        H5FL_FREE(H5HF_direct_free_head_t, dblock->free_list);
    } /* end if */

    /* Free block's buffer */
    H5FL_BLK_FREE(direct_block, dblock->blk);

    /* Free fractal heap direct block info */
    H5FL_FREE(H5HF_direct_t, dblock);

    FUNC_LEAVE_NOAPI(SUCCEED)
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

    /* Reset the free list dirty flag */
    dblock->free_list->dirty = FALSE;

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
H5HF_cache_iblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrows, void *_par_shared)
{
    const unsigned      *nrows = (const unsigned *)_nrows;     /* # of rows in indirect block */
    H5HF_parent_shared_t *par_shared = (H5HF_parent_shared_t *)_par_shared;     /* Shared direct block information */
    H5HF_shared_t       *shared = NULL; /* Shared fractal heap information */
    H5HF_indirect_t	*iblock = NULL; /* Indirect block info */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    size_t              u;              /* Local index variable */
    H5HF_indirect_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5HF_cache_iblock_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(par_shared);

    /* Allocate space for the fractal heap indirect block */
    if(NULL == (iblock = H5FL_MALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    iblock->shared = par_shared->shared;
    H5RC_INC(iblock->shared);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(iblock->shared);
    HDassert(shared);

    /* Share common parent [indirect] block information */
    iblock->parent = par_shared->parent;
    if(iblock->parent)
        H5RC_INC(iblock->parent);
    iblock->parent_entry = par_shared->parent_entry;
    iblock->child_free_space = par_shared->parent_free_space;

    /* Make a reference to this block */
    if(NULL == (iblock->self = H5RC_create(iblock, H5HF_iblock_free)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create ref-count wrapper for indirect fractal heap block")

    /* Set block's internal information */
    iblock->nrows = *nrows;
    if(iblock->nrows > shared->man_dtable.max_direct_rows) {
        iblock->ndir_rows = shared->man_dtable.max_direct_rows;
        iblock->nindir_rows = iblock->nrows - iblock->ndir_rows;
    } /* end if */
    else {
        iblock->ndir_rows = iblock->nrows;
        iblock->nindir_rows = 0;
    } /* end else */

    /* Compute size of indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(shared, iblock);

    /* Allocate buffer to decode block */
/* XXX: Use free list factories? */
    if((buf = H5FL_BLK_MALLOC(indirect_block, iblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read indirect block from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap indirect block")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5HF_IBLOCK_MAGIC, H5HF_SIZEOF_MAGIC))
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

    /* Address of heap that owns this block (skip) */
    H5F_addr_decode(f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, shared->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")
    
    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, iblock->block_off, shared->heap_off_size);

    /* Allocate & decode indirect block entry tables */
    HDassert(iblock->ndir_rows > 0);
    if(NULL == (iblock->dblock_ents = H5FL_SEQ_MALLOC(H5HF_indirect_dblock_ent_t, (iblock->ndir_rows * shared->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")
    iblock->dir_full = TRUE;
    for(u = 0; u < (iblock->ndir_rows * shared->man_dtable.cparam.width); u++) {
        H5F_addr_decode(f, &p, &(iblock->dblock_ents[u].addr));
        UINT32DECODE_VAR(p, iblock->dblock_ents[u].free_space, shared->man_dtable.max_dir_blk_off_size);

        /* Check for direct block being undefined and use that for the next one to allocate from this indirect block */
        if(iblock->dir_full) {
            if(!H5F_addr_defined(iblock->dblock_ents[u].addr)) {
                iblock->next_dir_col = u % shared->man_dtable.cparam.width;
                iblock->next_dir_row = u / shared->man_dtable.cparam.width;
                iblock->next_dir_entry = u;
                if(iblock->next_dir_row == 0)
                    iblock->next_dir_size = shared->man_dtable.cparam.start_block_size;
                else
                    iblock->next_dir_size = shared->man_dtable.cparam.start_block_size * (1 << (iblock->next_dir_row - 1));
                if(iblock->block_off == 0)
                    iblock->max_direct_rows = shared->man_dtable.max_direct_rows;
                else
                    HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, NULL, "computing max direct rows for non-root indirect block not supported yet")

                /* Set the flag to indicate the direct blocks aren't full */
                iblock->dir_full = FALSE;
            } /* end if */
        } /* end if */
    } /* end for */
    if(iblock->nindir_rows > 0) {
        if(NULL == (iblock->iblock_ents = H5FL_SEQ_MALLOC(H5HF_indirect_iblock_ent_t, (iblock->nindir_rows * shared->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for indirect entries")

        /* Decode indirect block-specific fields */
        for(u = 0; u < (iblock->nindir_rows * shared->man_dtable.cparam.width); u++) {
            H5F_addr_decode(f, &p, &(iblock->iblock_ents[u].addr));
            UINT64DECODE_VAR(p, iblock->iblock_ents[u].free_space, shared->heap_off_size);
        } /* end for */
    } /* end if */
    else
        iblock->iblock_ents = NULL;

    /* Sanity check */
    HDassert((size_t)(p - buf) == iblock->size);

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

    FUNC_ENTER_NOAPI(H5HF_cache_iblock_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);

    if(iblock->cache_info.is_dirty) {
        H5HF_shared_t *shared;      /* Shared fractal heap information */
        uint8_t	*buf = NULL;        /* Temporary buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t u;                   /* Local index variable */

        /* Sanity check */
        HDassert(iblock->dirty);
#ifdef QAK
HDfprintf(stderr, "%s: Flushing indirect block\n", FUNC);
#endif /* QAK */

        /* Get the pointer to the shared heap info */
        shared = H5RC_GET_OBJ(iblock->shared);
        HDassert(shared);

        /* Allocate buffer to encode block */
/* XXX: Use free list factories? */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: iblock->ndir_rows = %u\n", FUNC, iblock->ndir_rows);
HDfprintf(stderr, "%s: iblock->nindir_rows = %u\n", FUNC, iblock->nindir_rows);
HDfprintf(stderr, "%s: iblock->size = %Zu\n", FUNC, iblock->size);
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: shared->man_dtable.cparam.width = %u\n", FUNC, shared->man_dtable.cparam.width);
#endif /* QAK */
        if((buf = H5FL_BLK_MALLOC(indirect_block, iblock->size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5HF_IBLOCK_MAGIC, H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_IBLOCK_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, 4);
        p += 4;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, shared->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, iblock->block_off, shared->heap_off_size);
/* XXX: Fix this when we start writing recursive indirect blocks */
if(iblock->block_off != 0)
    HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "fix computing max direct rows for non-root indirect block for loading")

        /* Encode indirect block-specific fields */
        HDassert(iblock->ndir_rows > 0);
        for(u = 0; u < (iblock->ndir_rows * shared->man_dtable.cparam.width); u++) {
            H5F_addr_encode(f, &p, iblock->dblock_ents[u].addr);
            UINT32ENCODE_VAR(p, iblock->dblock_ents[u].free_space, shared->man_dtable.max_dir_blk_off_size);
        } /* end for */
        if(iblock->nindir_rows > 0)
            for(u = 0; u < (iblock->nindir_rows * shared->man_dtable.cparam.width); u++) {
                H5F_addr_encode(f, &p, iblock->iblock_ents[u].addr);
                UINT64ENCODE_VAR(p, iblock->iblock_ents[u].free_space, shared->heap_off_size);
            } /* end for */

        /* Sanity check */
        HDassert((size_t)(p - buf) == iblock->size);

	/* Write the indirect block */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_IBLOCK, addr, iblock->size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap indirect block to disk")

        /* Free buffer */
        H5FL_BLK_FREE(indirect_block, buf);

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
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_iblock_dest)

    /*
     * Check arguments.
     */
    HDassert(iblock);
#ifdef QAK
HDfprintf(stderr, "%s: Destroying indirect block\n", "H5HF_cache_iblock_dest");
#endif /* QAK */

    /* Decrement reference count on self */
    /* (let ref-counting API's callback for the block perform all cleanup) */
/* XXX: This should actually free all the indirect block info, since this
 *      routine should not get called until the block has no dependents (once
 *      the "un-evictable" flag is in the metadata cache)
 */
/* XXX: Once the "un-evictable" flag is working in the metadata cache, can
 *      get rid of the "self" field and make a wrapper when creating the parent
 *      info for each "child" block, which will make the indirect block 
 *      un-evictable as well as create the initial ref-counted object.
 */
/* XXX: Once the "un-evictable" flag is working in the metadata cache, the
 *      ref-counted 'free' callback should just make the indirect block
 *      evictable again.
 */
    H5RC_DEC(iblock->self);

    FUNC_LEAVE_NOAPI(SUCCEED)
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
    H5HF_shared_t *shared;      /* Shared fractal heap information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_iblock_size)

    /* check arguments */
    HDassert(iblock);
    HDassert(size_ptr);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(iblock->shared);
    HDassert(shared);

    /* Set size value */
    *size_ptr = iblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_iblock_size() */

