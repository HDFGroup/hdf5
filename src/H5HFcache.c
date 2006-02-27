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
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5HF_HDR_VERSION 0              /* Header */

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static H5HF_t *H5HF_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5HF_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_t *fh);
static herr_t H5HF_cache_hdr_clear(H5F_t *f, H5HF_t *fh, hbool_t destroy);
static herr_t H5HF_cache_hdr_size(const H5F_t *f, const H5HF_t *fh, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

/* H5HF inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_HDR[1] = {{
    H5AC_FHEAP_HDR_ID,
    (H5AC_load_func_t)H5HF_cache_hdr_load,
    (H5AC_flush_func_t)H5HF_cache_hdr_flush,
    (H5AC_dest_func_t)H5HF_cache_hdr_dest,
    (H5AC_clear_func_t)H5HF_cache_hdr_clear,
    (H5AC_size_func_t)H5HF_cache_hdr_size,
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage B-tree header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);



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
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5HF_type_t         heap_type;      /* Type of heap */
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

    /* Compute the size of the fractal heap header on disk */
    size = H5HF_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap header")

    p = buf;

    /* magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header signature")
    p += H5HF_SIZEOF_MAGIC;

    /* version */
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

    /* Fractal heap type */
    heap_type = *p++;
    HDassert(H5HF_ABSOLUTE == 0);
    if(heap_type > H5HF_MAPPED)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect fractal heap type")

    /* Initialize shared fractal heap info */
    if(H5HF_shared_init(f, fh, heap_type) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared fractal heap info")

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
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HF_cache_hdr_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(fh);

    if(fh->cache_info.is_dirty) {
        H5HF_shared_t *shared;      /* Shared fractal heap information */
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */

        /* Get the pointer to the shared B-tree info */
        shared = H5RC_GET_OBJ(fh->shared);
        HDassert(shared);

        /* Compute the size of the B-tree header on disk */
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

        /* Fractal heap type */
        *p++ = shared->type;

	/* Write the B-tree header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap header to disk")

        H5FL_BLK_FREE(header_block, buf);

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

