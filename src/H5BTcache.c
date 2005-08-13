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
 * Created:		H5BTcache.c
 *			Mar 10 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement block tracker metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

#define H5BT_PACKAGE		/*suppress error about including H5SHpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTpkg.h"		/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/* Local macros */

/* Block tracker format version #'s */
#define H5BT_VERSION 0


/* Local typedefs */

/* Local prototypes */

/* Metadata cache callbacks */
static H5BT_t *H5BT_cache_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5BT_cache_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BT_t *b);
static herr_t H5BT_cache_clear(H5F_t *f, H5BT_t *b, hbool_t destroy);
static herr_t H5BT_cache_size(const H5F_t *f, const H5BT_t *bt, size_t *size_ptr);

/* Package variables */

/* H5BT inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BLTR[1] = {{
    H5AC_BLTR_ID,
    (H5AC_load_func_t)H5BT_cache_load,
    (H5AC_flush_func_t)H5BT_cache_flush,
    (H5AC_dest_func_t)H5BT_cache_dest,
    (H5AC_clear_func_t)H5BT_cache_clear,
    (H5AC_size_func_t)H5BT_cache_size,
}};

/* Static variables */

/* Declare a free list to manage block tracker data to/from disk */
H5FL_BLK_DEFINE_STATIC(info_block);



/*-------------------------------------------------------------------------
 * Function:	H5BT_cache_load
 *
 * Purpose:	Loads block tracker info from the disk.
 *
 * Return:	Success:	Pointer to a new block tracker
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
static H5BT_t *
H5BT_cache_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED *udata1, void UNUSED *udata2)
{
    H5BT_t		*bt = NULL;
    size_t		size;
    uint8_t		*buf = NULL;
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5BT_t		*ret_value;

    FUNC_ENTER_NOAPI(H5BT_cache_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    if (NULL==(bt = H5FL_MALLOC(H5BT_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bt->cache_info,0,sizeof(H5AC_info_t));

    /* Compute the size of the block tracker on disk */
    size = H5BT_SIZE(f);

    /* Allocate temporary buffer */
    if ((buf=H5FL_BLK_MALLOC(info_block,size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Read header from disk */
    if (H5F_block_read(f, H5FD_MEM_BLKTRK, addr, size, dxpl_id, buf)<0)
	HGOTO_ERROR(H5E_BLKTRK, H5E_READERROR, NULL, "can't read block tracker info")

    p = buf;

    /* magic number */
    if (HDmemcmp(p, H5BT_MAGIC, H5BT_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTLOAD, NULL, "wrong block tracker info signature")
    p += H5BT_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5BT_VERSION)
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTLOAD, NULL, "wrong block tracker info version")

    /* Size status information */
    bt->status = *p++;

    /* Max. block size info */
    H5F_DECODE_LENGTH(f, p, bt->max_block_size);
    UINT32DECODE(p, bt->max_block_cnt);

    /* Min. block size info */
    H5F_DECODE_LENGTH(f, p, bt->min_block_size);
    UINT32DECODE(p, bt->min_block_cnt);

    /* Total size of all blocks tracked */
    H5F_DECODE_LENGTH(f, p, bt->tot_block_size);

    /* Address of B-tree for blocks */
    H5F_addr_decode(f, (const uint8_t **)&p, &(bt->bt2_addr));

    /* Set return value */
    ret_value = bt;

done:
    if(buf)
        H5FL_BLK_FREE(info_block,buf);
    if (!ret_value && bt)
        (void)H5BT_cache_dest(f,bt);
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BT_cache_load() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_cache_flush
 *
 * Purpose:	Flushes dirty block tracker info to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_cache_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BT_t *bt)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5BT_cache_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt);

    if (bt->cache_info.is_dirty) {
        uint8_t	*buf = NULL;
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;

        /* Compute the size of the block tracker info on disk */
        size = H5BT_SIZE(f);

        /* Allocate temporary buffer */
        if ((buf=H5FL_BLK_MALLOC(info_block,size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        p = buf;

        /* magic number */
        HDmemcpy(p, H5BT_MAGIC, H5BT_SIZEOF_MAGIC);
        p += H5BT_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5BT_VERSION;

        /* Size status information */
        *p++ = bt->status;

        /* Max. block size info */
        H5F_ENCODE_LENGTH(f, p, bt->max_block_size);
        UINT32ENCODE(p, bt->max_block_cnt);

        /* Min. block size info */
        H5F_ENCODE_LENGTH(f, p, bt->min_block_size);
        UINT32ENCODE(p, bt->min_block_cnt);

        /* Total size of all blocks tracked */
        H5F_ENCODE_LENGTH(f, p, bt->tot_block_size);

        /* Address of B-tree for blocks */
        H5F_addr_encode(f, &p, bt->bt2_addr);

	/* Write the block tracker info. */
	if (H5F_block_write(f, H5FD_MEM_BLKTRK, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_BLKTRK, H5E_CANTFLUSH, FAIL, "unable to save block tracker info to disk")

        H5FL_BLK_FREE(info_block,buf);

	bt->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5BT_cache_dest(f,bt) < 0)
	    HGOTO_ERROR(H5E_BLKTRK, H5E_CANTFREE, FAIL, "unable to destroy block tracker info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BT_cache_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_cache_dest
 *
 * Purpose:	Destroys a block tracker in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5BT_cache_dest(H5F_t UNUSED *f, H5BT_t *bt)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_cache_dest)

    /*
     * Check arguments.
     */
    HDassert(bt);

    /* Free block tracker info */
    H5FL_FREE(H5BT_t,bt);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5BT_cache_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_cache_clear
 *
 * Purpose:	Mark a block tracker info in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_cache_clear(H5F_t *f, H5BT_t *bt, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BT_cache_clear)

    /*
     * Check arguments.
     */
    HDassert(bt);

    /* Reset the dirty flag.  */
    bt->cache_info.is_dirty = FALSE;

    if (destroy)
        if (H5BT_cache_dest(f, bt) < 0)
	    HGOTO_ERROR(H5E_BLKTRK, H5E_CANTFREE, FAIL, "unable to destroy block tracker info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BT_cache_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_cache_size
 *
 * Purpose:	Compute the size in bytes of a block tracker info
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BT_cache_size(const H5F_t *f, const H5BT_t UNUSED *bt, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_cache_size)

    /* check arguments */
    HDassert(f);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5BT_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5BT_cache_size() */

