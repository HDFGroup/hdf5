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
 * Created:		H5FScache.c
 *			May  2 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement file free space metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5FS_PACKAGE		/*suppress error about including H5FSpkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FSpkg.h"		/* File free space			*/

/****************/
/* Local Macros */
/****************/

/* File free space format version #'s */
#define H5FS_HDR_VERSION        0               /* Header */


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
static H5FS_hdr_t *H5FS_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5FS_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_hdr_t *hdr);
static herr_t H5FS_cache_hdr_clear(H5F_t *f, H5FS_hdr_t *hdr, hbool_t destroy);
static herr_t H5FS_cache_hdr_size(const H5F_t *f, const H5FS_hdr_t *hdr, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

/* H5FS header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FSPACE_HDR[1] = {{
    H5AC_FSPACE_HDR_ID,
    (H5AC_load_func_t)H5FS_cache_hdr_load,
    (H5AC_flush_func_t)H5FS_cache_hdr_flush,
    (H5AC_dest_func_t)H5FS_cache_hdr_dest,
    (H5AC_clear_func_t)H5FS_cache_hdr_clear,
    (H5AC_size_func_t)H5FS_cache_hdr_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage free space header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);



/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_load
 *
 * Purpose:	Loads a free space header from the disk.
 *
 * Return:	Success:	Pointer to a new free space header
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static H5FS_hdr_t *
H5FS_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5FS_hdr_t		*hdr = NULL;    /* Free space header info */
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            metadata_chksum;        /* Metadata checksum value */
    H5FS_hdr_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_load)
#ifdef QAK
HDfprintf(stderr, "%s: Load free space header, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Allocate space for the free space header */
    if(NULL == (hdr = H5FL_MALLOC(H5FS_hdr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&hdr->cache_info, 0, sizeof(H5AC_info_t));

    /* Compute the size of the free space header on disk */
    size = H5FS_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_FSPACE_HDR, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_READERROR, NULL, "can't read free space header")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5FS_HDR_MAGIC, H5FS_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space header signature")
    p += H5FS_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5FS_HDR_VERSION)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "wrong free space header version")

    /* Metadata flags (unused, currently) */
/* XXX: Plan out metadata flags (including "read-only duplicate" feature) */
    if(*p++ != 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "unknown metadata flag in free space header")

    /* Metadata checksum (unused, currently) */
    UINT32DECODE(p, metadata_chksum);
/* XXX: Verify checksum */
    if(metadata_chksum != 0)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "incorrect metadata checksum for free space header")

    /* Client ID */
    hdr->client = *p++;
    if(hdr->client >= H5FS_NUM_CLIENT_ID)
	HGOTO_ERROR(H5E_FSPACE, H5E_CANTLOAD, NULL, "unknown client ID in free space header")

    /* Total space tracked */
    H5F_DECODE_LENGTH(f, p, hdr->tot_space);

    /* Total # of free space sections tracked */
    H5F_DECODE_LENGTH(f, p, hdr->tot_sect_count);

    /* # of serializable free space sections tracked */
    H5F_DECODE_LENGTH(f, p, hdr->serial_sect_count);

    /* # of ghost free space sections tracked */
    H5F_DECODE_LENGTH(f, p, hdr->ghost_sect_count);

    /* # of section classes */
    UINT16DECODE(p, hdr->nclasses);

    /* Shrink percent */
    UINT16DECODE(p, hdr->shrink_percent);

    /* Expand percent */
    UINT16DECODE(p, hdr->expand_percent);

    /* Size of address space free space sections are within (log2 of actual value) */
    UINT16DECODE(p, hdr->max_sect_addr);

    /* Max. size of section to track */
    H5F_DECODE_LENGTH(f, p, hdr->max_sect_size);

    /* Address of serialized free space sections */
    H5F_addr_decode(f, &p, &hdr->sect_addr);

    /* Size of serialized free space sections */
    H5F_DECODE_LENGTH(f, p, hdr->sect_size);

    /* Allocated size of serialized free space sections */
    H5F_DECODE_LENGTH(f, p, hdr->alloc_sect_size);

    HDassert((size_t)(p - buf) == size);

    /* Set return value */
    ret_value = hdr;

done:
    if(buf)
        H5FL_BLK_FREE(header_block, buf);
    if(!ret_value && hdr)
        (void)H5FS_cache_hdr_dest(f, hdr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_flush
 *
 * Purpose:	Flushes a dirty free space header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5FS_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_flush)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing free space header, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);

    if(hdr->cache_info.is_dirty) {
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */

        /* Compute the size of the free space header on disk */
        size = H5FS_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5FS_HDR_MAGIC, H5FS_SIZEOF_MAGIC);
        p += H5FS_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5FS_HDR_VERSION;

        /* Metadata status flags */
/* XXX: Set this? */
        *p++ = 0;

        /* Metadata checksum */
/* XXX: Set this!  (After all the metadata is in the buffer) */
        HDmemset(p, 0, 4);
        p += 4;

        /* Client ID */
        *p++ = hdr->client;

        /* Total space tracked */
        H5F_ENCODE_LENGTH(f, p, hdr->tot_space);

        /* Total # of free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, hdr->tot_sect_count);

        /* # of serializable free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, hdr->serial_sect_count);

        /* # of ghost free space sections tracked */
        H5F_ENCODE_LENGTH(f, p, hdr->ghost_sect_count);

        /* # of section classes */
        UINT16ENCODE(p, hdr->nclasses);

        /* Shrink percent */
        UINT16ENCODE(p, hdr->shrink_percent);

        /* Expand percent */
        UINT16ENCODE(p, hdr->expand_percent);

        /* Size of address space free space sections are within (log2 of actual value) */
        UINT16ENCODE(p, hdr->max_sect_addr);

        /* Max. size of section to track */
        H5F_ENCODE_LENGTH(f, p, hdr->max_sect_size);

        /* Address of serialized free space sections */
        H5F_addr_encode(f, &p, hdr->sect_addr);

        /* Size of serialized free space sections */
        H5F_ENCODE_LENGTH(f, p, hdr->sect_size);

        /* Allocated size of serialized free space sections */
        H5F_ENCODE_LENGTH(f, p, hdr->alloc_sect_size);

	/* Write the free space header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_FSPACE_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFLUSH, FAIL, "unable to save free space header to disk")

        H5FL_BLK_FREE(header_block, buf);

	hdr->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5FS_cache_hdr_dest(f, hdr) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_dest
 *
 * Purpose:	Destroys a free space header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5FS_cache_hdr_dest(H5F_t UNUSED *f, H5FS_hdr_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Free the shared info itself */
    H5FL_FREE(H5FS_hdr_t, hdr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5FS_cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_clear
 *
 * Purpose:	Mark a free space header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_clear(H5F_t *f, H5FS_hdr_t *hdr, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5FS_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Reset the dirty flag.  */
    hdr->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5FS_cache_hdr_dest(f, hdr) < 0)
	    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to destroy free space header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5FS_cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5FS_cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a free space header
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_cache_hdr_size(const H5F_t *f, const H5FS_hdr_t *hdr, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5FS_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(hdr);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5FS_HEADER_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_cache_hdr_size() */

