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
 * Created:		H5HLcache.c
 *			Feb  5 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement local heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HL_PACKAGE		/* Suppress error about including H5HLpkg */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5HLpkg.h"		/* Local Heaps				*/
#include "H5MFprivate.h"	/* File memory management		*/


/****************/
/* Local Macros */
/****************/

#define H5HL_VERSION	0               /* Local heap collection version    */
#define H5HL_FREE_NULL	1		/* End of free list on disk	    */


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
static herr_t H5HL_serialize(H5F_t *f, H5HL_t *heap, uint8_t *buf);

/* Metadata cache callbacks */
static H5HL_t *H5HL_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata1,
			 void *udata2);
static herr_t H5HL_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, H5HL_t *heap, unsigned UNUSED * flags_ptr);
static herr_t H5HL_clear(H5F_t *f, H5HL_t *heap, hbool_t destroy);


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* H5HL inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_LHEAP[1] = {{
    H5AC_LHEAP_ID,
    (H5AC_load_func_t)H5HL_load,
    (H5AC_flush_func_t)H5HL_flush,
    (H5AC_dest_func_t)H5HL_dest,
    (H5AC_clear_func_t)H5HL_clear,
    (H5AC_size_func_t)H5HL_size,
}};


/*-------------------------------------------------------------------------
 * Function:    H5HL_serialize
 *
 * Purpose:     Serialize the heap. This function will eliminate free
 *              blocks at the tail of the heap and also split the block
 *              if it needs to be split for the file. This is so that we
 *              can serialize it correctly.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 16, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_serialize(H5F_t *f, H5HL_t *heap, uint8_t *buf)
{
    H5HL_free_t	   *fl;
    uint8_t        *p;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_serialize)

    /* check args */
    assert(buf);
    assert(heap);

    /* serialize the header */
    p = buf;
    fl = heap->freelist;
    HDmemcpy(p, H5HL_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    p += H5_SIZEOF_MAGIC;
    *p++ = H5HL_VERSION;
    *p++ = 0;	/*reserved*/
    *p++ = 0;	/*reserved*/
    *p++ = 0;	/*reserved*/
    H5F_ENCODE_LENGTH(f, p, heap->heap_alloc);
    H5F_ENCODE_LENGTH(f, p, fl ? fl->offset : H5HL_FREE_NULL);
    H5F_addr_encode(f, &p, heap->addr);

    /* serialize the free list */
    for (; fl; fl = fl->next) {
        assert (fl->offset == H5HL_ALIGN (fl->offset));
        p = heap->chunk + H5HL_SIZEOF_HDR(f) + fl->offset;

        if (fl->next) {
            H5F_ENCODE_LENGTH(f, p, fl->next->offset);
        } else {
            H5F_ENCODE_LENGTH(f, p, H5HL_FREE_NULL);
        }

        H5F_ENCODE_LENGTH(f, p, fl->size);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_load
 *
 * Purpose:	Loads a heap from disk.
 *
 * Return:	Success:	Ptr to a local heap memory data structure.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
static H5HL_t *
H5HL_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void UNUSED * udata1,
	  void UNUSED * udata2)
{
    uint8_t		hdr[52];
    size_t              sizeof_hdr;     /* Cache H5HL header size for file */
    const uint8_t	*p = NULL;
    H5HL_t		*heap = NULL;
    H5HL_free_t		*fl = NULL, *tail = NULL;
    size_t		free_block = H5HL_FREE_NULL;
    H5HL_t		*ret_value;

    FUNC_ENTER_NOAPI(H5HL_load, NULL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(!udata1);
    HDassert(!udata2);

    /* Cache this for later */
    sizeof_hdr = H5HL_SIZEOF_HDR(f);
    HDassert(sizeof_hdr <= sizeof(hdr));

    /* Get the local heap's header */
    if(H5F_block_read(f, H5FD_MEM_LHEAP, addr, sizeof_hdr, dxpl_id, hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "unable to read heap header")
    p = hdr;

    /* Check magic number */
    if(HDmemcmp(hdr, H5HL_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad heap signature")
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(H5HL_VERSION != *p++)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong version number in global heap")

    /* Reserved */
    p += 3;

    /* Allocate space in memory for the heap */
    if(NULL == (heap = H5FL_CALLOC(H5HL_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* heap data size */
    H5F_DECODE_LENGTH(f, p, heap->heap_alloc);

    /* free list head */
    H5F_DECODE_LENGTH(f, p, free_block);
    if(free_block != H5HL_FREE_NULL && free_block >= heap->heap_alloc)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad heap free list")

    /* data */
    H5F_addr_decode(f, &p, &(heap->addr));
    if(NULL == (heap->chunk = H5FL_BLK_CALLOC(lheap_chunk, (sizeof_hdr + heap->heap_alloc))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(heap->heap_alloc &&
            H5F_block_read(f, H5FD_MEM_LHEAP, heap->addr, heap->heap_alloc, dxpl_id, heap->chunk + sizeof_hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unable to read heap data")

    /* Build free list */
    while(H5HL_FREE_NULL != free_block) {
	if(free_block >= heap->heap_alloc)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad heap free list")
	if(NULL == (fl = H5FL_MALLOC(H5HL_free_t)))
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
	fl->offset = free_block;
	fl->prev = tail;
	fl->next = NULL;
	if(tail)
            tail->next = fl;
	tail = fl;
	if(!heap->freelist)
            heap->freelist = fl;

	p = heap->chunk + sizeof_hdr + free_block;

	H5F_DECODE_LENGTH(f, p, free_block);
	if(free_block == 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "free block size is zero?")

	H5F_DECODE_LENGTH(f, p, fl->size);
	if(fl->offset + fl->size > heap->heap_alloc)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad heap free list")
    } /* end while */

    /* Set return value */
    ret_value = heap;

done:
    if(!ret_value && heap)
        if(H5HL_dest(f,heap) < 0)
	    HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy local heap collection")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_load() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_flush
 *
 * Purpose:	Flushes a heap from memory to disk if it's dirty.  Optionally
 *		deletes the heap from memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HL_t *heap, unsigned UNUSED * flags_ptr)
{
    herr_t  ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HL_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(heap);

    if(heap->cache_info.is_dirty) {
        haddr_t hdr_end_addr;
        size_t  sizeof_hdr = H5HL_SIZEOF_HDR(f);    /* cache H5HL header size for file */

	/* Write the header */
        if(H5HL_serialize(f, heap, heap->chunk) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSERIALIZE, FAIL, "unable to serialize local heap")

	/* Copy buffer to disk */
	hdr_end_addr = addr + (hsize_t)sizeof_hdr;

	if(H5F_addr_eq(heap->addr, hdr_end_addr)) {
	    /* The header and data are contiguous */
	    if(H5F_block_write(f, H5FD_MEM_LHEAP, addr, (sizeof_hdr + heap->heap_alloc), dxpl_id, heap->chunk) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap header and data to file")
	} /* end if */
        else {
	    if(H5F_block_write(f, H5FD_MEM_LHEAP, addr, sizeof_hdr, dxpl_id, heap->chunk) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap header to file")

	    if(H5F_block_write(f, H5FD_MEM_LHEAP, heap->addr, heap->heap_alloc, dxpl_id, heap->chunk + sizeof_hdr) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap data to file")
	} /* end else */

	heap->cache_info.is_dirty = FALSE;
    } /* end if */

    /* Should we destroy the memory version? */
    if(destroy)
        if(H5HL_dest(f, heap) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy local heap collection")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dest
 *
 * Purpose:	Destroys a heap in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_dest(H5F_t *f, H5HL_t *heap)
{
    H5HL_free_t	*fl;                    /* Heap object free list */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_dest)

    /* check arguments */
    HDassert(heap);

    /* Verify that node is clean */
    HDassert(heap->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!heap->cache_info.free_file_space_on_destroy || H5F_addr_defined(heap->cache_info.addr));

    /* Check for freeing file space for local heap */
    if(heap->cache_info.free_file_space_on_destroy) {
        size_t sizeof_hdr;      /* H5HL header size for file */
        haddr_t hdr_addr;       /* Address of heap header in file */

        /* Compute this for later */
        sizeof_hdr = H5HL_SIZEOF_HDR(f);
        hdr_addr = heap->cache_info.addr;

        /* Check if the heap is contiguous on disk */
        HDassert(!H5F_addr_overflow(hdr_addr, sizeof_hdr));
        if(H5F_addr_eq(heap->addr, hdr_addr + sizeof_hdr)) {
            /* Free the contiguous local heap in one call */
            /* (XXX: Nasty usage of internal DXPL value! -QAK) */
            H5_CHECK_OVERFLOW(sizeof_hdr + heap->heap_alloc, size_t, hsize_t);
            if(H5MF_xfree(f, H5FD_MEM_LHEAP, H5AC_dxpl_id, hdr_addr, (hsize_t)(sizeof_hdr + heap->heap_alloc)) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free contiguous local heap")
        } /* end if */
        else {
            /* Free the local heap's header */
            /* (XXX: Nasty usage of internal DXPL value! -QAK) */
            H5_CHECK_OVERFLOW(sizeof_hdr, size_t, hsize_t);
            if(H5MF_xfree(f, H5FD_MEM_LHEAP, H5AC_dxpl_id, hdr_addr, (hsize_t)sizeof_hdr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free local heap header")

            /* Free the local heap's data */
            /* (XXX: Nasty usage of internal DXPL value! -QAK) */
            H5_CHECK_OVERFLOW(heap->heap_alloc, size_t, hsize_t);
            if(H5MF_xfree(f, H5FD_MEM_LHEAP, H5AC_dxpl_id, heap->addr, (hsize_t)heap->heap_alloc) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free local heap data")
        } /* end else */
    } /* end if */

    /* Release resources */
    if(heap->chunk)
        heap->chunk = H5FL_BLK_FREE(lheap_chunk, heap->chunk);
    while(heap->freelist) {
        fl = heap->freelist;
        heap->freelist = fl->next;
        (void)H5FL_FREE(H5HL_free_t, fl);
    } /* end while */
    (void)H5FL_FREE(H5HL_t, heap);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_clear
 *
 * Purpose:	Mark a local heap in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 20 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_clear(H5F_t *f, H5HL_t *heap, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_clear)

    /* check arguments */
    HDassert(heap);

    /* Mark heap as clean */
    heap->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HL_dest(f, heap) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy local heap collection")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_size
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *              H5HL_t on disk, and return it in *len_ptr.  On failure,
 *              the value of *len_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *		5/13/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_size(const H5F_t *f, const H5HL_t *heap, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_size)

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(size_ptr);

    *size_ptr = H5HL_SIZEOF_HDR(f) + heap->heap_alloc;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HL_size() */

