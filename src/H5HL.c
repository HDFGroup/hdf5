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
 * Created:		H5HL.c
 *			Jul 16 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Heap functions for the local heaps used by symbol
 *			tables to store names (among other things).
 *
 * Modifications:
 *
 *	Robb Matzke, 5 Aug 1997
 *	Added calls to H5E.
 *
 *-------------------------------------------------------------------------
 */
#define H5F_PACKAGE		/* Suppress error about including H5Fpkg  */
#define H5HL_PACKAGE		/* Suppress error about including H5HLpkg */


#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"	/* Metadata cache			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5HLpkg.h"		/* Local Heaps				*/
#include "H5MFprivate.h"	/* File memory management		*/

/* Private macros */
#define H5HL_FREE_NULL	1		/*end of free list on disk	*/
#define H5HL_MIN_HEAP   128             /* Minimum size to reduce heap buffer to */

/*
 * Local heap collection version.
 */
#define H5HL_VERSION	0

/* Private typedefs */

/* PRIVATE PROTOTYPES */
static herr_t H5HL_serialize(H5F_t *f, H5HL_t *heap, uint8_t *buf);
static H5HL_free_t *H5HL_remove_free(H5HL_t *heap, H5HL_free_t *fl);
static herr_t H5HL_minimize_heap_space(H5F_t *f, hid_t dxpl_id, H5HL_t *heap);

/* Metadata cache callbacks */
static H5HL_t *H5HL_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata1,
			 void *udata2);
static herr_t H5HL_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr, H5HL_t *heap, unsigned UNUSED * flags_ptr);
static herr_t H5HL_dest(H5F_t *f, H5HL_t *heap);
static herr_t H5HL_clear(H5F_t *f, H5HL_t *heap, hbool_t destroy);
static herr_t H5HL_size(const H5F_t *f, const H5HL_t *heap, size_t *size_ptr);

/*
 * H5HL inherits cache-like properties from H5AC
 */
const H5AC_class_t H5AC_LHEAP[1] = {{
    H5AC_LHEAP_ID,
    (H5AC_load_func_t)H5HL_load,
    (H5AC_flush_func_t)H5HL_flush,
    (H5AC_dest_func_t)H5HL_dest,
    (H5AC_clear_func_t)H5HL_clear,
    (H5AC_size_func_t)H5HL_size,
}};

/* Declare a free list to manage the H5HL_free_t struct */
H5FL_DEFINE_STATIC(H5HL_free_t);

/* Declare a free list to manage the H5HL_t struct */
H5FL_DEFINE_STATIC(H5HL_t);

/* Declare a PQ free list to manage the heap chunk information */
H5FL_BLK_DEFINE_STATIC(heap_chunk);


/*-------------------------------------------------------------------------
 * Function:	H5HL_create
 *
 * Purpose:	Creates a new heap data structure on disk and caches it
 *		in memory.  SIZE_HINT is a hint for the initial size of the
 *		data area of the heap.	If size hint is invalid then a
 *		reasonable (but probably not optimal) size will be chosen.
 *		If the heap ever has to grow, then REALLOC_HINT is the
 *		minimum amount by which the heap will grow.
 *
 * Return:	Success:	Non-negative. The file address of new heap is
 *				returned through the ADDR argument.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 16 1997
 *
 * Modifications:
 *
 *	Robb Matzke, 5 Aug 1997
 *	Takes a flag that determines the type of heap that is
 *	created.
 *
 *	John Mainzer, 6/7/05
 *	Removed code modifying the is_dirty field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_create(H5F_t *f, hid_t dxpl_id, size_t size_hint, haddr_t *addr_p/*out*/)
{
    H5HL_t	*heap = NULL;
    hsize_t	total_size;		/*total heap size on disk	*/
    size_t      sizeof_hdr;             /* Cache H5HL header size for file */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HL_create, FAIL);

    /* check arguments */
    assert(f);
    assert(addr_p);

    if (size_hint && size_hint < H5HL_SIZEOF_FREE(f))
	size_hint = H5HL_SIZEOF_FREE(f);
    size_hint = H5HL_ALIGN(size_hint);

    /* Cache this for later */
    sizeof_hdr= H5HL_SIZEOF_HDR(f);

    /* allocate file version */
    total_size = sizeof_hdr + size_hint;
    if (HADDR_UNDEF==(*addr_p=H5MF_alloc(f, H5FD_MEM_LHEAP, dxpl_id, total_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate file memory");

    /* allocate memory version */
    if (NULL==(heap = H5FL_CALLOC(H5HL_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
    heap->addr = *addr_p + (hsize_t)sizeof_hdr;
    heap->heap_alloc = size_hint;
    if (NULL==(heap->chunk = H5FL_BLK_CALLOC(heap_chunk,(sizeof_hdr + size_hint))))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

    /* free list */
    if (size_hint) {
	if (NULL==(heap->freelist = H5FL_MALLOC(H5HL_free_t)))
	    HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");
	heap->freelist->offset = 0;
	heap->freelist->size = size_hint;
	heap->freelist->prev = heap->freelist->next = NULL;
    } else {
	heap->freelist = NULL;
    }

    /* add to cache */
    if (H5AC_set(f, dxpl_id, H5AC_LHEAP, *addr_p, heap, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "unable to cache heap");

done:
    if (ret_value<0) {
	if (H5F_addr_defined(*addr_p))
	    H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, *addr_p, total_size);
	if (heap) {
            if(H5HL_dest(f,heap)<0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy local heap collection");
	}
    }
    FUNC_LEAVE_NOAPI(ret_value);
}


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
 * Modifications:
 *		Robb Matzke, 1999-07-28
 *		The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
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
    if(HDmemcmp(hdr, H5HL_MAGIC, (size_t)H5HL_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad heap signature")
    p += H5HL_SIZEOF_MAGIC;

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
    if(NULL == (heap->chunk = H5FL_BLK_CALLOC(heap_chunk, (sizeof_hdr + heap->heap_alloc))))
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
	if (fl->offset + fl->size > heap->heap_alloc)
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
 * Function:    H5HL_minimize_heap_space
 *
 * Purpose:     Go through the heap's freelist and determine if we can
 *              eliminate the free blocks at the tail of the buffer.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 16, 2003
 *
 * Modifications:
 *
 *		John Mainzer, 8/10/05
 *		Reworked this function for a different role.
 *
 *		It used to be called during cache eviction, where it
 *		attempted to size the disk space allocation for the
 *		actual size of the heap.  However, this causes problems
 *		in the parallel case, as the reuslting disk allocations
 *		may not be synchronized.
 *
 *		It is now called from H5HL_remove(), where it is used to
 *		reduce heap size in response to an entry deletion.  This
 *		means that the function should either do nothing, or
 *		reduce the size of the disk allocation.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_minimize_heap_space(H5F_t *f, hid_t dxpl_id, H5HL_t *heap)
{
    size_t new_heap_size = heap->heap_alloc; /* New size of heap */
    size_t sizeof_hdr;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HL_minimize_heap_space, FAIL)

    /* check args */
    HDassert(f);
    HDassert(heap);

    sizeof_hdr = H5HL_SIZEOF_HDR(f);    /* cache H5HL header size for file */

    /*
     * Check to see if we can reduce the size of the heap in memory by
     * eliminating free blocks at the tail of the buffer before flushing the
     * buffer out.
     */
    if(heap->freelist) {
        H5HL_free_t    *tmp_fl;
        H5HL_free_t    *last_fl = NULL;

        /* Search for a free block at the end of the buffer */
        for(tmp_fl = heap->freelist; tmp_fl; tmp_fl = tmp_fl->next)
            /* Check if the end of this free block is at the end of the buffer */
            if(tmp_fl->offset + tmp_fl->size == heap->heap_alloc) {
                last_fl = tmp_fl;
                break;
            } /* end if */

        /*
         * Found free block at the end of the buffer, decide what to do
         * about it
         */
        if(last_fl) {
            /*
             * If the last free block's size is more than half the memory
             * buffer size (and the memory buffer is larger than the
             * minimum size), reduce or eliminate it.
             */
            if(last_fl->size >= (heap->heap_alloc / 2) && heap->heap_alloc > H5HL_MIN_HEAP) {
                /*
                 * Reduce size of buffer until it's too small or would
                 * eliminate the free block
                 */
                while(new_heap_size > H5HL_MIN_HEAP &&
                        new_heap_size >= (last_fl->offset + H5HL_SIZEOF_FREE(f)))
                    new_heap_size /= 2;

                /*
                 * Check if reducing the memory buffer size would
                 * eliminate the free block
                 */
                if(new_heap_size < (last_fl->offset + H5HL_SIZEOF_FREE(f))) {
                    /* Check if this is the only block on the free list */
                    if(last_fl->prev == NULL && last_fl->next == NULL) {
                        /* Double the new memory size */
                        new_heap_size *= 2;

                        /* Truncate the free block */
                        last_fl->size = H5HL_ALIGN(new_heap_size - last_fl->offset);
                        new_heap_size = last_fl->offset + last_fl->size;
                        assert(last_fl->size >= H5HL_SIZEOF_FREE(f));
                    } else {
                        /*
                         * Set the size of the memory buffer to the start
                         * of the free list
                         */
                        new_heap_size = last_fl->offset;

                        /* Eliminate the free block from the list */
                        last_fl = H5HL_remove_free(heap, last_fl);
                    } /* end else */
                } else {
                    /* Truncate the free block */
                    last_fl->size = H5HL_ALIGN(new_heap_size - last_fl->offset);
                    new_heap_size = last_fl->offset + last_fl->size;
                    assert(last_fl->size >= H5HL_SIZEOF_FREE(f));
                    assert(last_fl->size == H5HL_ALIGN(last_fl->size));
                } /* end else */
            } /* end if */
        } /* end if */
    } /* end if */

    /*
     * If the heap grew smaller than disk storage then move the
     * data segment of the heap to another contiguous block of disk
     * storage.
     */
    if(new_heap_size != heap->heap_alloc) {
        haddr_t old_addr = heap->addr,
            new_addr;

	HDassert(new_heap_size < heap->heap_alloc);

        /* Resize the memory buffer */
        heap->chunk = H5FL_BLK_REALLOC(heap_chunk, heap->chunk, (sizeof_hdr + new_heap_size));
        if(!heap->chunk)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Release old space on disk */
        /* (Should be safe to free old heap space first, since it's shrinking -QAK) */
        H5_CHECK_OVERFLOW(heap->heap_alloc, size_t, hsize_t);
        H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, old_addr, (hsize_t)heap->heap_alloc);
        H5E_clear_stack(NULL);    /* don't really care if the free failed */

        /* Allocate new space on disk */
        H5_CHECK_OVERFLOW(new_heap_size, size_t, hsize_t);
        if(HADDR_UNDEF == (new_addr = H5MF_alloc(f, H5FD_MEM_LHEAP, dxpl_id, (hsize_t)new_heap_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate file space for heap")

        /* Update heap info*/
        heap->addr = new_addr;
        heap->heap_alloc = new_heap_size;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HL_minimize_heap_space() */


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
 * Modifications:
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
    HDmemcpy(p, H5HL_MAGIC, (size_t)H5HL_SIZEOF_MAGIC);
    p += H5HL_SIZEOF_MAGIC;
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
}


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
 * Modifications:
 *      rky, 1998-08-28
 *      Only p0 writes metadata to disk.
 *
 *      Robb Matzke, 1999-07-28
 *      The ADDR argument is passed by value.
 *
 *	Quincey Koziol, 2002-7-180
 *	Added dxpl parameter to allow more control over I/O from metadata
 *      cache.
 *
 *      Bill Wendling, 2003-09-16
 *      Separated out the bit that serializes the heap.
 *
 *	John Mainzer, 2005-08-10
 *	Removed call to H5HL_minimize_heap_space().  It does disk space
 *	allocation, which can cause problems if done at flush time.
 *	Instead, disk space allocation/deallocation is now done at
 *	insert/remove time.
 *
 *	John Mainzer, 2006-08-21
 *	Added the flags_ptr parameter.  This parameter exists to
 *	allow the flush routine to report to the cache if the
 *	entry is resized or renamed as a result of the flush.
 *	*flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HL_t *heap, unsigned UNUSED * flags_ptr)
{
    herr_t  ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HL_flush, FAIL);

    /* check arguments */
    HDassert( f );
    HDassert( H5F_addr_defined(addr) );
    HDassert( heap );

    if (heap->cache_info.is_dirty) {
        haddr_t hdr_end_addr;
        size_t  sizeof_hdr = H5HL_SIZEOF_HDR(f);    /* cache H5HL header size for file */

	/* Write the header */
        if (H5HL_serialize(f, heap, heap->chunk) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSERIALIZE, FAIL, "unable to serialize local heap")

	/* Copy buffer to disk */
	hdr_end_addr = addr + (hsize_t)sizeof_hdr;

	if (H5F_addr_eq(heap->addr, hdr_end_addr)) {
	    /* The header and data are contiguous */
	    if (H5F_block_write(f, H5FD_MEM_LHEAP, addr, (sizeof_hdr + heap->heap_alloc),
				dxpl_id, heap->chunk) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap header and data to file")
	} else {
	    if (H5F_block_write(f, H5FD_MEM_LHEAP, addr, sizeof_hdr, dxpl_id, heap->chunk) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap header to file")

	    if (H5F_block_write(f, H5FD_MEM_LHEAP, heap->addr, heap->heap_alloc,
				dxpl_id, heap->chunk + sizeof_hdr) < 0)
		HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "unable to write heap data to file")
	}

	heap->cache_info.is_dirty = FALSE;
    }

    /* Should we destroy the memory version? */
    if (destroy) {
        if (H5HL_dest(f,heap) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy local heap collection")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_dest(H5F_t UNUSED *f, H5HL_t *heap)
{
    H5HL_free_t	*fl;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_dest);

    /* check arguments */
    assert(heap);

    /* Verify that node is clean */
    assert (heap->cache_info.is_dirty==FALSE);

    if(heap->chunk)
        heap->chunk = H5FL_BLK_FREE(heap_chunk,heap->chunk);
    while (heap->freelist) {
        fl = heap->freelist;
        heap->freelist = fl->next;
        H5FL_FREE(H5HL_free_t,fl);
    }
    H5FL_FREE(H5HL_t,heap);

    FUNC_LEAVE_NOAPI(SUCCEED);
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
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_clear(H5F_t *f, H5HL_t *heap, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5HL_clear);

    /* check arguments */
    assert(heap);

    /* Mark heap as clean */
    heap->cache_info.is_dirty = FALSE;

    if (destroy)
        if (H5HL_dest(f, heap) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy local heap collection");

done:
    FUNC_LEAVE_NOAPI(ret_value);
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
 * Modifications:
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_size(const H5F_t *f, const H5HL_t *heap, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_size);

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(size_ptr);

    *size_ptr = H5HL_SIZEOF_HDR(f) + heap->heap_alloc;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HL_size() */


/*-------------------------------------------------------------------------
 * Function:    H5HL_protect
 *
 * Purpose:     This function is a wrapper for the H5AC_protect call. The
 *              old H5HL_peek call (which this once was) wasn't "safe"
 *              for FPHDF5. (It'd get a read lock on an object but once
 *              it got that object, it'd release it keeping the old
 *              pointer value, which is no longer valid. This won't work
 *              since the pointer into some metdata block can become
 *              invalid.)
 *
 *              N.B.: This function is always called in conjunction with
 *              the H5HL_offset_into function. The return from that
 *              function is the proper pointer to the heap's object. This
 *              is done so that the return from this function can be sent
 *              to H5HL_unprotect.
 *
 * Return:      Success:    Ptr to the object. The pointer points to a
 *                          chunk of memory large enough to hold the
 *                          object from the specified offset (usually the
 *                          beginning of the object) to the end of the
 *                          object. Do not attempt to read past the end
 *                          of the object.
 *              Failure:    NULL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 17, 2003
 *
 *-------------------------------------------------------------------------
 */
H5HL_t *
H5HL_protect(H5F_t *f, hid_t dxpl_id, haddr_t addr, H5AC_protect_t rw)
{
    H5HL_t *ret_value;

    FUNC_ENTER_NOAPI(H5HL_protect, NULL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    if(NULL == (ret_value = H5AC_protect(f, dxpl_id, H5AC_LHEAP, addr, NULL, NULL, rw)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unable to load heap")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_protect() */


/*-------------------------------------------------------------------------
 * Function:    H5HL_offset_into
 *
 * Purpose:     Called directly after the call to H5HL_protect so that
 *              a pointer to the object in the heap can be got.
 *
 * Return:      Success:    Valid pointer.
 *              Failure:    NULL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 17, 2003
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
void *
H5HL_offset_into(H5F_t *f, const H5HL_t *heap, size_t offset)
{
    /*
     * We need to have called some other function before this to get a
     * valid heap pointer. So, this can remain "FUNC_ENTER_NOAPI_NOINIT"
     */
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_offset_into)
    assert(f);
    assert(heap);
    assert(offset < heap->heap_alloc);
    FUNC_LEAVE_NOAPI(heap->chunk + H5HL_SIZEOF_HDR(f) + offset)
}


/*-------------------------------------------------------------------------
 * Function:    H5HL_unprotect
 *
 * Purpose:     Unprotect the data retrieved by the H5HL_protect call.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:  Bill Wendling
 *              wendling@ncsa.uiuc.edu
 *              Sept. 17, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_unprotect(H5F_t *f, hid_t dxpl_id, H5HL_t *heap, haddr_t addr)
{
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HL_unprotect, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(H5F_addr_defined(addr));

    if(H5AC_unprotect(f, dxpl_id, H5AC_LHEAP, addr, (void *)heap, H5AC__NO_FLAGS_SET) != SUCCEED)
        HGOTO_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_unprotect() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_remove_free
 *
 * Purpose:	Removes free list element FL from the specified heap and
 *		frees it.
 *
 * Return:	NULL
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 17 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static H5HL_free_t *
H5HL_remove_free(H5HL_t *heap, H5HL_free_t *fl)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_remove_free);

    if (fl->prev) fl->prev->next = fl->next;
    if (fl->next) fl->next->prev = fl->prev;

    if (!fl->prev) heap->freelist = fl->next;

    FUNC_LEAVE_NOAPI(H5FL_FREE(H5HL_free_t,fl));
}


/*-------------------------------------------------------------------------
 * Function:	H5HL_insert
 *
 * Purpose:	Inserts a new item into the heap.
 *
 * Return:	Success:	Offset of new item within heap.
 *
 *		Failure:	(size_t)(-1)
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
size_t
H5HL_insert(H5F_t *f, hid_t dxpl_id, H5HL_t *heap, size_t buf_size, const void *buf)
{
    H5HL_free_t	*fl = NULL, *last_fl = NULL;
    size_t	offset = 0;
    size_t	need_size;
    hbool_t	found;
    size_t      sizeof_hdr;     /* Cache H5HL header size for file */
    size_t	ret_value;      /* Return value */

    FUNC_ENTER_NOAPI(H5HL_insert, (size_t)(-1))

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(buf_size > 0);
    HDassert(buf);

    /* Mark heap as dirty in cache */
    /* (A bit early in the process, but it's difficult to determine in the
     *  code below where to mark the heap as dirty, especially in error cases,
     *  so we just accept that an extra flush of the heap info could occur
     *  if an error occurs -QAK)
     */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, heap) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, (size_t)(-1), "unable to mark heap as dirty")

    /* Cache this for later */
    sizeof_hdr = H5HL_SIZEOF_HDR(f);

    /*
     * In order to keep the free list descriptors aligned on word boundaries,
     * whatever that might mean, we round the size up to the next multiple of
     * a word.
     */
    need_size = H5HL_ALIGN(buf_size);

    /*
     * Look for a free slot large enough for this object and which would
     * leave zero or at least H5G_SIZEOF_FREE bytes left over.
     */
    for(fl = heap->freelist, found = FALSE; fl; fl = fl->next) {
	if(fl->size > need_size &&
                fl->size - need_size >= H5HL_SIZEOF_FREE(f)) {
	    /* a big enough free block was found */
	    offset = fl->offset;
	    fl->offset += need_size;
	    fl->size -= need_size;
	    HDassert(fl->offset == H5HL_ALIGN(fl->offset));
	    HDassert(fl->size == H5HL_ALIGN(fl->size));
	    found = TRUE;
	    break;
	} else if(fl->size == need_size) {
	    /* free block of exact size found */
	    offset = fl->offset;
	    fl = H5HL_remove_free(heap, fl);
	    found = TRUE;
	    break;
	} else if(!last_fl || last_fl->offset < fl->offset) {
	    /* track free space that's closest to end of heap */
	    last_fl = fl;
	}
    } /* end for */

    /*
     * If no free chunk was large enough, then allocate more space and
     * add it to the free list.	 If the heap ends with a free chunk, we
     * can extend that free chunk.  Otherwise we'll have to make another
     * free chunk.  If the heap must expand, we double its size.
     */
    if(found == FALSE) {
        size_t	need_more;              /* How much more space we need */
        size_t	new_heap_alloc;         /* Final size of space allocated for heap */
        htri_t	can_extend;             /* Whether the local heap's data segment on disk can be extended */

        /* At least double the heap's size, making certain there's enough room
         * for the new object */
	need_more = MAX(need_size, heap->heap_alloc);

        /* If there is no last free block or it's not at the end of the heap,
         * and the amount of space to allocate is not big enough to include at
         * least the new object and a free-list info, trim down the amount of
         * space requested to just the amount of space needed.  (Generally
         * speaking, this only occurs when the heap is small -QAK)
         */
	if(!(last_fl && last_fl->offset + last_fl->size == heap->heap_alloc)
                && (need_more < (need_size + H5HL_SIZEOF_FREE(f))))
            need_more = need_size;

	new_heap_alloc = heap->heap_alloc + need_more;
/*
 * XXX: This is a _total_ hack, a real kludge. :-/  The metadata cache currently
 *      responds very poorly when an object is inserted into the cache (or
 *      resized) that is larger than the current cache size.  It waits through
 *      an entire 'epoch' of cache operations to resize the cache larger (getting
 *      _very_ poor performance), instead of immediately accommodating the large
 *      object by increasing the cache size.
 *
 *      So, what we are doing here is to look at the current cache size, check
 *      if the new local heap will overwhelm the cache and, if so, resize the
 *      cache to be large enough to hold the new local heap block along with
 *      leaving room for other objects in the cache.
 *
 *      John will be working on a fix inside the cache itself, so this special
 *      case code here can be removed when he's finished.  - QAK, 2007/12/21
 */
{
	H5AC_cache_config_t mdc_config;

        /* Retrieve the current cache information */
	mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
	if(H5AC_get_cache_auto_resize_config(f->shared->cache, &mdc_config) < 0)
	    HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (size_t)-1, "H5AC_get_cache_auto_resize_config() failed.")

        /* Check if the current cache will get blown out by adding this heap
         * block and resize it if so.
         */
	if((2 * new_heap_alloc) >= mdc_config.initial_size) {
	    mdc_config.set_initial_size = TRUE;
	    mdc_config.initial_size = 2 * new_heap_alloc;

	    if(H5AC_set_cache_auto_resize_config(f->shared->cache, &mdc_config) < 0)
	        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, (size_t)-1, "H5AC_set_cache_auto_resize_config() failed.")
        } /* end if */
}
	HDassert(heap->heap_alloc < new_heap_alloc);
	H5_CHECK_OVERFLOW(heap->heap_alloc, size_t, hsize_t);
	H5_CHECK_OVERFLOW(new_heap_alloc, size_t, hsize_t);

        /* Check if current heap is extendible */
	can_extend = H5MF_can_extend(f, H5FD_MEM_LHEAP, heap->addr, (hsize_t)(heap->heap_alloc), (hsize_t)need_more);
        if(can_extend < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (size_t)(-1), "unable to check whether heap can be extended")

	/* extend the current heap if we can... */
	if(can_extend == TRUE) {
            if(H5MF_extend(f, H5FD_MEM_LHEAP, heap->addr, (hsize_t)(heap->heap_alloc), (hsize_t)need_more) < 0)
		HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (size_t)(-1), "can't extend heap on disk")
	} /* end if */
        else { /* ...if we can't, allocate a new chunk & release the old */
	    haddr_t new_addr;

	    /* The new allocation may fail -- to avoid the possiblity of
	     * file corruption, allocate the new heap first, and then
	     * deallocate the old.
	     */

	    /* allocate new disk space for the heap */
	    if((new_addr = H5MF_alloc(f, H5FD_MEM_LHEAP, dxpl_id, (hsize_t)new_heap_alloc)) == HADDR_UNDEF)
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (size_t)(-1), "unable to allocate file space for heap")

            /* Release old space on disk */
            H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, heap->addr, (hsize_t)heap->heap_alloc);
            H5E_clear_stack(NULL);    /* don't really care if the free failed */

	    heap->addr = new_addr;
	} /* end else */

        /* If the last free list in the heap is at the end of the heap, extend it */
	if(last_fl && last_fl->offset + last_fl->size == heap->heap_alloc) {
	    /*
	     * Increase the size of the last free block.
	     */
	    offset = last_fl->offset;
	    last_fl->offset += need_size;
	    last_fl->size += need_more - need_size;
	    HDassert(last_fl->offset == H5HL_ALIGN(last_fl->offset));
	    HDassert(last_fl->size == H5HL_ALIGN(last_fl->size));

	    if (last_fl->size < H5HL_SIZEOF_FREE(f)) {
#ifdef H5HL_DEBUG
		if (H5DEBUG(HL) && last_fl->size) {
		    fprintf(H5DEBUG(HL), "H5HL: lost %lu bytes at line %d\n",
			    (unsigned long)(last_fl->size), __LINE__);
		}
#endif
		last_fl = H5HL_remove_free(heap, last_fl);
	    }
	} /* end if */
        else {
	    /*
	     * Create a new free list element large enough that we can
	     * take some space out of it right away.
	     */
	    offset = heap->heap_alloc;
	    if(need_more - need_size >= H5HL_SIZEOF_FREE(f)) {
		if(NULL == (fl = H5FL_MALLOC(H5HL_free_t)))
		    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (size_t)(-1), "memory allocation failed")
		fl->offset = heap->heap_alloc + need_size;
		fl->size = need_more - need_size;
		HDassert(fl->offset == H5HL_ALIGN(fl->offset));
		HDassert(fl->size == H5HL_ALIGN(fl->size));
		fl->prev = NULL;
		fl->next = heap->freelist;
		if (heap->freelist) heap->freelist->prev = fl;
		heap->freelist = fl;
#ifdef H5HL_DEBUG
	    } else if (H5DEBUG(HL) && need_more > need_size) {
		fprintf(H5DEBUG(HL),
			"H5HL_insert: lost %lu bytes at line %d\n",
			(unsigned long)(need_more - need_size), __LINE__);
#endif
	    }
	} /* end else */

#ifdef H5HL_DEBUG
	if (H5DEBUG(HL)) {
	    fprintf(H5DEBUG(HL),
		    "H5HL: resize mem buf from %lu to %lu bytes\n",
		    (unsigned long)(heap->heap_alloc),
		    (unsigned long)(heap->heap_alloc + need_more));
	}
#endif
        heap->heap_alloc = new_heap_alloc;
	heap->chunk = H5FL_BLK_REALLOC(heap_chunk, heap->chunk, (sizeof_hdr + heap->heap_alloc));
	if(NULL == heap->chunk)
	    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, (size_t)(-1), "memory allocation failed")

	/* Clear new section so junk doesn't appear in the file */
        /* (Avoid clearing section which will be overwritten with newly inserted data) */
	HDmemset(heap->chunk + sizeof_hdr + offset + buf_size, 0, (new_heap_alloc - (offset + buf_size)));
    } /* end if */

    /*
     * Copy the data into the heap
     */
    HDmemcpy(heap->chunk + sizeof_hdr + offset, buf, buf_size);

    /* Set return value */
    ret_value = offset;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HL_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_remove
 *
 * Purpose:	Removes an object or part of an object from the heap at
 *		address ADDR of file F.	 The object (or part) to remove
 *		begins at byte OFFSET from the beginning of the heap and
 *		continues for SIZE bytes.
 *
 *		Once part of an object is removed, one must not attempt
 *		to access that part.  Removing the beginning of an object
 *		results in the object OFFSET increasing by the amount
 *		truncated.  Removing the end of an object results in
 *		object truncation.  Removing the middle of an object results
 *		in two separate objects, one at the original offset and
 *		one at the first offset past the removed portion.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 16 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_remove(H5F_t *f, hid_t dxpl_id, H5HL_t *heap, size_t offset, size_t size)
{
    H5HL_free_t		*fl = NULL;
    herr_t      	ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5HL_remove, FAIL);

    /* check arguments */
    HDassert(f);
    HDassert(heap);
    HDassert(size > 0);
    HDassert(offset == H5HL_ALIGN(offset));

    size = H5HL_ALIGN(size);

    HDassert(offset < heap->heap_alloc);
    HDassert(offset + size <= heap->heap_alloc);

    /* Mark heap as dirty in cache */
    /* (A bit early in the process, but it's difficult to determine in the
     *  code below where to mark the heap as dirty, especially in error cases,
     *  so we just accept that an extra flush of the heap info could occur
     *  if an error occurs -QAK)
     */
    if(H5AC_mark_pinned_or_protected_entry_dirty(f, heap) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, FAIL, "unable to mark heap as dirty")

    /*
     * Check if this chunk can be prepended or appended to an already
     * free chunk.  It might also fall between two chunks in such a way
     * that all three chunks can be combined into one.
     */
    fl = heap->freelist;
    while(fl) {
        H5HL_free_t *fl2 = NULL;

	if((offset + size) == fl->offset) {
	    fl->offset = offset;
	    fl->size += size;
	    HDassert(fl->offset==H5HL_ALIGN (fl->offset));
	    HDassert(fl->size==H5HL_ALIGN (fl->size));
	    fl2 = fl->next;
	    while(fl2) {
		if((fl2->offset + fl2->size) == fl->offset) {
		    fl->offset = fl2->offset;
		    fl->size += fl2->size;
		    HDassert(fl->offset == H5HL_ALIGN (fl->offset));
		    HDassert(fl->size == H5HL_ALIGN (fl->size));
		    fl2 = H5HL_remove_free(heap, fl2);
	            if(((fl->offset + fl->size) == heap->heap_alloc ) &&
                             ((2 * fl->size) > heap->heap_alloc )) {
                        if(H5HL_minimize_heap_space(f, dxpl_id, heap) < 0)
	                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "heap size minimization failed")
                    }
		    HGOTO_DONE(SUCCEED);
		}
		fl2 = fl2->next;
	    }
	    if(((fl->offset + fl->size) == heap->heap_alloc) &&
                     ((2 * fl->size) > heap->heap_alloc)) {
                if(H5HL_minimize_heap_space(f, dxpl_id, heap) < 0)
	            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "heap size minimization failed")
            }
	    HGOTO_DONE(SUCCEED);
	} else if(fl->offset + fl->size == offset) {
	    fl->size += size;
	    fl2 = fl->next;
	    HDassert(fl->size==H5HL_ALIGN (fl->size));
	    while(fl2) {
		if(fl->offset + fl->size == fl2->offset) {
		    fl->size += fl2->size;
		    HDassert(fl->size==H5HL_ALIGN (fl->size));
		    fl2 = H5HL_remove_free(heap, fl2);
	            if(((fl->offset + fl->size) == heap->heap_alloc) &&
                            ((2 * fl->size) > heap->heap_alloc)) {
                        if(H5HL_minimize_heap_space(f, dxpl_id, heap) < 0)
	                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "heap size minimization failed")
                    }
		    HGOTO_DONE(SUCCEED);
		}
		fl2 = fl2->next;
	    }
	    if(((fl->offset + fl->size) == heap->heap_alloc) &&
                    ((2 * fl->size) > heap->heap_alloc)) {
                if(H5HL_minimize_heap_space(f, dxpl_id, heap) < 0)
	            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "heap size minimization failed")
            }
	    HGOTO_DONE(SUCCEED);
	}
	fl = fl->next;
    } /* end while */

    /*
     * The amount which is being removed must be large enough to
     * hold the free list data.	 If not, the freed chunk is forever
     * lost.
     */
    if(size < H5HL_SIZEOF_FREE(f)) {
#ifdef H5HL_DEBUG
	if(H5DEBUG(HL)) {
	    fprintf(H5DEBUG(HL), "H5HL: lost %lu bytes\n",
		    (unsigned long) size);
	}
#endif
	HGOTO_DONE(SUCCEED);
    } /* end if */

    /*
     * Add an entry to the free list.
     */
    if(NULL == (fl = H5FL_MALLOC(H5HL_free_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    fl->offset = offset;
    fl->size = size;
    HDassert(fl->offset == H5HL_ALIGN(fl->offset));
    HDassert(fl->size == H5HL_ALIGN(fl->size));
    fl->prev = NULL;
    fl->next = heap->freelist;
    if(heap->freelist)
        heap->freelist->prev = fl;
    heap->freelist = fl;

    if(((fl->offset + fl->size) == heap->heap_alloc) &&
            ((2 * fl->size) > heap->heap_alloc)) {
        if(H5HL_minimize_heap_space(f, dxpl_id, heap) < 0)
            HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "heap size minimization failed")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5HL_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_delete
 *
 * Purpose:	Deletes a local heap from disk, freeing disk space used.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 22 2003
 *
 * Modifications:
 *
 *		John Mainzer - 6/17/05
 *		Modified function to use the new dirtied parmeter of
 *		H5AC_unprotect(), which allows management of the is_dirty
 *		field of the cache info to be moved into the cache code.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_delete(H5F_t *f, hid_t dxpl_id, haddr_t addr)
{
    H5HL_t	*heap = NULL;
    size_t      sizeof_hdr;     /* Cache H5HL header size for file */
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI(H5HL_delete, FAIL);

    /* check arguments */
    assert(f);
    assert(H5F_addr_defined(addr));

    /* Cache this for later */
    sizeof_hdr= H5HL_SIZEOF_HDR(f);

    /* Get heap pointer */
    if (NULL == (heap = H5AC_protect(f, dxpl_id, H5AC_LHEAP, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load heap");

    /* Check if the heap is contiguous on disk */
    assert(!H5F_addr_overflow(addr,sizeof_hdr));
    if(H5F_addr_eq(heap->addr,addr+sizeof_hdr)) {
        /* Free the contiguous local heap in one call */
        H5_CHECK_OVERFLOW(sizeof_hdr+heap->heap_alloc,size_t,hsize_t);
        if (H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, addr, (hsize_t)(sizeof_hdr+heap->heap_alloc))<0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free contiguous local heap");
    } /* end if */
    else {
        /* Free the local heap's header */
        H5_CHECK_OVERFLOW(sizeof_hdr,size_t,hsize_t);
        if (H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, addr, (hsize_t)sizeof_hdr)<0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free local heap header");

        /* Free the local heap's data */
        H5_CHECK_OVERFLOW(heap->heap_alloc,size_t,hsize_t);
        if (H5MF_xfree(f, H5FD_MEM_LHEAP, dxpl_id, heap->addr, (hsize_t)heap->heap_alloc)<0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free local heap data");
    } /* end else */

    /* Release the local heap metadata from the cache */
    if (H5AC_unprotect(f, dxpl_id, H5AC_LHEAP, addr, heap, H5AC__DIRTIED_FLAG | H5C__DELETED_FLAG)<0) {
        heap = NULL;
        HGOTO_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release local heap");
    }
    heap = NULL;

done:
    if (heap && H5AC_unprotect(f, dxpl_id, H5AC_LHEAP, addr, heap, H5AC__NO_FLAGS_SET)<0)
	HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release local heap");

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5HL_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_get_size
 *
 * Purpose:	Retrieves the current size of a heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Nov  7 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_get_size(H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t *size)
{
    H5HL_t	*heap = NULL;           /* Heap to query */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HL_get_size, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(size);

    /* Get heap pointer */
    if(NULL == (heap = H5AC_protect(f, dxpl_id, H5AC_LHEAP, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load heap")

    /* Set the size to return */
    *size = heap->heap_alloc;

done:
    if(heap && H5AC_unprotect(f, dxpl_id, H5AC_LHEAP, addr, heap, H5AC__NO_FLAGS_SET) < 0)
	HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release local heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_get_size() */


/*-------------------------------------------------------------------------
 * Function:    H5HL_heapsize
 *
 * Purpose:     Compute the size in bytes of the specified instance of
 *              H5HL_t via H5HL_size()
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi
 *              June 19 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_heapsize(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t *heap_size)
{
    H5HL_t      *heap = NULL;           /* Heap to query */
    size_t	local_heap_size = 0;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HL_heapsize, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(heap_size);

    /* Get heap pointer */
    if(NULL == (heap = H5AC_protect(f, dxpl_id, H5AC_LHEAP, addr, NULL, NULL, H5AC_READ)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load heap")

    /* Get the total size of the local heap */
    if(H5HL_size(f, heap, &local_heap_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to compute size of local heap")

    /* Accumulate the size of the local heap */
    *heap_size += (hsize_t)local_heap_size;

done:
    if(heap && H5AC_unprotect(f, dxpl_id, H5AC_LHEAP, addr, heap, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release local heap")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_heapsize() */

