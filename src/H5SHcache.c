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
 * Created:		H5SHcache.c
 *			Mar 10 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement segmented heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

#define H5SH_PACKAGE		/*suppress error about including H5SHpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5SHpkg.h"		/* Segmented heap			*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/* Local macros */

/* Segmented heap format version #'s */
#define H5SH_VERSION 0


/* Local typedefs */

/* Local prototypes */

/* Metadata cache callbacks */
static H5SH_t *H5SH_cache_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5SH_cache_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SH_t *s);
static herr_t H5SH_cache_clear(H5F_t *f, H5SH_t *s, hbool_t destroy);
static herr_t H5SH_cache_size(const H5F_t *f, const H5SH_t *s, size_t *size_ptr);

/* Package variables */

/* H5SH inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_SGHP[1] = {{
    H5AC_SHEAP_ID,
    (H5AC_load_func_t)H5SH_cache_load,
    (H5AC_flush_func_t)H5SH_cache_flush,
    (H5AC_dest_func_t)H5SH_cache_dest,
    (H5AC_clear_func_t)H5SH_cache_clear,
    (H5AC_size_func_t)H5SH_cache_size,
}};

/* Static variables */

/* Declare a free list to manage segmented heap data to/from disk */
H5FL_BLK_DEFINE_STATIC(info_block);



/*-------------------------------------------------------------------------
 * Function:	H5SH_cache_load
 *
 * Purpose:	Loads segmented heap info from the disk.
 *
 * Return:	Success:	Pointer to a new segmented heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 *-------------------------------------------------------------------------
 */
static H5SH_t *
H5SH_cache_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5SH_t		*sh = NULL;
    size_t		size;
    uint8_t		*buf = NULL;
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5SH_t		*ret_value;

    FUNC_ENTER_NOAPI(H5SH_cache_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    if (NULL==(sh = H5FL_MALLOC(H5SH_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&sh->cache_info,0,sizeof(H5AC_info_t));

    /* Compute the size of the segmented heap info on disk */
    size = H5SH_SIZE(f);

    /* Allocate temporary buffer */
    if ((buf=H5FL_BLK_MALLOC(info_block,size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Read info from disk */
    if (H5F_block_read(f, H5FD_MEM_SHEAP_HDR, addr, size, dxpl_id, buf)<0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read segmented heap info")

    p = buf;

    /* magic number */
    if (HDmemcmp(p, H5SH_MAGIC, H5SH_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong segmented heap info signature")
    p += H5SH_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5SH_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong segmented heap info version")

    /* Type of data in heap blocks */
    sh->heap_type = (H5SH_data_type_t)*p++;
    if(sh->heap_type == H5SH_RAW)
        sh->file_mem_type = H5FD_MEM_DRAW;
    else
        sh->file_mem_type = H5FD_MEM_SHEAP_BLOCK;

    /* Total heap block minimum size */
    H5F_DECODE_LENGTH(f, p, sh->min_size);

    /* Total heap block max. expand size */
    H5F_DECODE_LENGTH(f, p, sh->max_extend_size);

    /* Address of block tracker for heap blocks */
    H5F_addr_decode(f, (const uint8_t **)&p, &(sh->bt_heap_addr));

    /* Address of block tracker for free space */
    H5F_addr_decode(f, (const uint8_t **)&p, &(sh->bt_free_addr));

    /* Set return value */
    ret_value = sh;

done:
    if(buf)
        H5FL_BLK_FREE(info_block,buf);
    if (!ret_value && sh)
        (void)H5SH_cache_dest(f,sh);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_cache_load() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_cache_flush
 *
 * Purpose:	Flushes dirty segmented heap info to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SH_cache_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5SH_t *sh)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5SH_cache_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(sh);

    if (sh->cache_info.is_dirty) {
        uint8_t	*buf = NULL;
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;

        /* Compute the size of the segmented heap info on disk */
        size = H5SH_SIZE(f);

        /* Allocate temporary buffer */
        if ((buf=H5FL_BLK_MALLOC(info_block,size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        p = buf;

        /* magic number */
        HDmemcpy(p, H5SH_MAGIC, H5SH_SIZEOF_MAGIC);
        p += H5SH_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5SH_VERSION;

        /* Type of data in heap blocks */
        *p++ = (uint8_t)sh->heap_type;

        /* Min. size of heap block */
        H5F_ENCODE_LENGTH(f, p, sh->min_size);

        /* Max. size to expand heap block */
        H5F_ENCODE_LENGTH(f, p, sh->max_extend_size);

        /* Address of block tracker for heap blocks */
        H5F_addr_encode(f, &p, sh->bt_heap_addr);

        /* Address of block tracker for free space */
        H5F_addr_encode(f, &p, sh->bt_free_addr);

	/* Write the segmented heap info. */
	if (H5F_block_write(f, H5FD_MEM_SHEAP_HDR, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save segmented heap info to disk")

        H5FL_BLK_FREE(info_block,buf);

	sh->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5SH_cache_dest(f,sh) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy segmented heap info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5SH_cache_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_cache_dest
 *
 * Purpose:	Destroys a segmented heap in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5SH_cache_dest(H5F_t UNUSED *f, H5SH_t *sh)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SH_cache_dest)

    /*
     * Check arguments.
     */
    HDassert(sh);

    /* Free segmented heap info */
    H5FL_FREE(H5SH_t,sh);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5SH_cache_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_cache_clear
 *
 * Purpose:	Mark a segmented heap info in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SH_cache_clear(H5F_t *f, H5SH_t *sh, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5SH_cache_clear)

    /*
     * Check arguments.
     */
    HDassert(sh);

    /* Reset the dirty flag.  */
    sh->cache_info.is_dirty = FALSE;
 
    if (destroy)
        if (H5SH_cache_dest(f, sh) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy segmented heap info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_cache_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_cache_size
 *
 * Purpose:	Compute the size in bytes of a segmented heap info
 *		on disk, and return it in *size_ptr.  On failure, 
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5SH_cache_size(const H5F_t *f, const H5SH_t UNUSED *sh, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5SH_cache_size)

    /* check arguments */
    HDassert(f);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5SH_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5SH_cache_size() */


