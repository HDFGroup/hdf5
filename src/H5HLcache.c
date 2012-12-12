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
 * Created:     H5HLcache.c
 *              Feb  5 2008
 *              Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:     Implement local heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5HL_PACKAGE        /* Suppress error about including H5HLpkg */


/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions            */
#include "H5Eprivate.h"     /* Error handling               */
#include "H5HLpkg.h"        /* Local Heaps                  */
#include "H5MFprivate.h"    /* File memory management       */
#include "H5WBprivate.h"    /* Wrapped Buffers              */


/****************/
/* Local Macros */
/****************/

#define H5HL_VERSION    0               /* Local heap collection version    */

/* Set the local heap size to speculatively read in
 *      (needs to be more than the local heap prefix size to work at all and
 *      should be larger than the default local heap size to save the
 *      extra I/O operations)
 */
#define H5HL_SPEC_READ_SIZE     512


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
/* Local heap prefix */
static H5HL_prfx_t *H5HL__prefix_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5HL__prefix_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr,
    H5HL_prfx_t *prfx, unsigned *flags_ptr);
static herr_t H5HL__prefix_dest(H5F_t *f, H5HL_prfx_t *prfx);
static herr_t H5HL__prefix_clear(H5F_t *f, H5HL_prfx_t *prfx, hbool_t destroy);
static herr_t H5HL__prefix_size(const H5F_t *f, H5HL_prfx_t *prfx, size_t *size_ptr);

/* Local heap data block */
static H5HL_dblk_t *H5HL__datablock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *udata);
static herr_t H5HL__datablock_flush(H5F_t *f, hid_t dxpl_id, hbool_t dest, haddr_t addr,
    H5HL_dblk_t *dblk, unsigned *flags_ptr);
static herr_t H5HL__datablock_dest(H5F_t *f, H5HL_dblk_t *dblk);
static herr_t H5HL__datablock_clear(H5F_t *f, H5HL_dblk_t *dblk, hbool_t destroy);
static herr_t H5HL__datablock_notify(H5AC_notify_action_t action, H5HL_dblk_t *dblk);
static herr_t H5HL__datablock_size(const H5F_t *f, H5HL_dblk_t *dblk, size_t *size_ptr);

/* Free list de/serialization */
static herr_t H5HL__fl_deserialize(H5HL_t *heap);
static void H5HL__fl_serialize(const H5HL_t *heap);

/*********************/
/* Package Variables */
/*********************/

/* H5HL inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_LHEAP_PRFX[1] = {{
    H5AC_LHEAP_PRFX_ID,
    (H5AC_load_func_t)      H5HL__prefix_load,
    (H5AC_flush_func_t)     H5HL__prefix_flush,
    (H5AC_dest_func_t)      H5HL__prefix_dest,
    (H5AC_clear_func_t)     H5HL__prefix_clear,
    (H5AC_notify_func_t)    NULL,
    (H5AC_size_func_t)      H5HL__prefix_size,
}};

const H5AC_class_t H5AC_LHEAP_DBLK[1] = {{
    H5AC_LHEAP_DBLK_ID,
    (H5AC_load_func_t)      H5HL__datablock_load,
    (H5AC_flush_func_t)     H5HL__datablock_flush,
    (H5AC_dest_func_t)      H5HL__datablock_dest,
    (H5AC_clear_func_t)     H5HL__datablock_clear,
    (H5AC_notify_func_t)    H5HL__datablock_notify,
    (H5AC_size_func_t)      H5HL__datablock_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5HL__fl_deserialize
 *
 * Purpose:     Deserialize the free list for a heap data block
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__fl_deserialize(H5HL_t *heap))
    H5HL_free_t *fl = NULL, *tail = NULL;   /* Heap free block nodes */
    hsize_t free_block;                     /* Offset of free block */

    /* check arguments */
    HDassert(heap);
    HDassert(!heap->freelist);

    /* Build free list */
    free_block = heap->free_block;
    while(H5HL_FREE_NULL != free_block) {
        const uint8_t *p;               /* Pointer into image buffer */

        /* Sanity check */
        if(free_block >= heap->dblk_size)
            H5E_THROW(H5E_BADRANGE, "bad heap free list");

        /* Allocate & initialize free list node */
        if(NULL == (fl = H5FL_MALLOC(H5HL_free_t)))
            H5E_THROW(H5E_CANTALLOC, "memory allocation failed");
        fl->offset = (size_t)free_block;
        fl->prev = tail;
        fl->next = NULL;

        /* Decode offset of next free block */
        p = heap->dblk_image + free_block;
        H5F_DECODE_LENGTH_LEN(p, free_block, heap->sizeof_size);
        if(0 == free_block)
            H5E_THROW(H5E_BADVALUE, "free block size is zero?");

        /* Decode length of this free block */
        H5F_DECODE_LENGTH_LEN(p, fl->size, heap->sizeof_size);
        if((fl->offset + fl->size) > heap->dblk_size)
            H5E_THROW(H5E_BADRANGE, "bad heap free list");

        /* Append node onto list */
        if(tail)
            tail->next = fl;
        else
            heap->freelist = fl;
        tail = fl;
        fl = NULL;
    } /* end while */

CATCH
    if(ret_value < 0)
        if(fl)
            /* H5FL_FREE always returns NULL so we can't check for errors */
            fl = (H5HL_free_t *)H5FL_FREE(H5HL_free_t, fl);

END_FUNC(STATIC) /* end H5HL__fl_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__fl_serialize
 *
 * Purpose:     Serialize the free list for a heap data block
 *
 * Return:      Nothing (void)
 *
 * Programmer:  Quincey Koziol
 *              Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC_VOID(STATIC, NOERR,
H5HL__fl_serialize(const H5HL_t *heap))

    H5HL_free_t *fl;                    /* Pointer to heap free list node */

    /* check arguments */
    HDassert(heap);

    /* Serialize the free list into the heap data's image */
    for(fl = heap->freelist; fl; fl = fl->next) {
        uint8_t     *p;                     /* Pointer into raw data buffer */

        HDassert(fl->offset == H5HL_ALIGN(fl->offset));
        p = heap->dblk_image + fl->offset;

        if(fl->next)
            H5F_ENCODE_LENGTH_LEN(p, fl->next->offset, heap->sizeof_size)
        else
            H5F_ENCODE_LENGTH_LEN(p, H5HL_FREE_NULL, heap->sizeof_size)

        H5F_ENCODE_LENGTH_LEN(p, fl->size, heap->sizeof_size)
    } /* end for */

END_FUNC_VOID(STATIC) /* end H5HL__fl_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__prefix_load
 *
 * Purpose:     Loads a local heap prefix from disk.
 *
 * Return:      Success:    Pointer to a local heap prefix structure.
 *              Failure:    NULL
 *
 * Programmer:  Robb Matzke
 *              Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5HL_prfx_t *, NULL, NULL,
H5HL__prefix_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    H5HL_t                  *heap = NULL;                               /* Local heap                               */
    H5HL_prfx_t             *prfx = NULL;                               /* Heap prefix deserialized                 */
    H5HL_cache_prfx_ud_t    *udata = (H5HL_cache_prfx_ud_t *)_udata;    /* User data for callback                   */
    uint8_t                 buf[H5HL_SPEC_READ_SIZE];                   /* Buffer for decoding                      */
    size_t                  spec_read_size;                             /* Size of buffer to speculatively read in  */
    const uint8_t           *p;                                         /* Pointer into decoding buffer             */
    haddr_t                 eoa;                                        /* Relative end of file address             */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);
    HDassert(udata->sizeof_size > 0);
    HDassert(udata->sizeof_addr > 0);
    HDassert(udata->sizeof_prfx > 0);
    HDassert(udata->sizeof_prfx <= sizeof(buf));

    /* Make certain we don't speculatively read off the end of the file */
    if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, H5FD_MEM_LHEAP)))
        H5E_THROW(H5E_CANTGET, "unable to determine file size");

    /* Compute the size of the speculative local heap prefix buffer */
    H5_ASSIGN_OVERFLOW(spec_read_size, MIN(eoa - addr, H5HL_SPEC_READ_SIZE), /* From: */ hsize_t, /* To: */ size_t);
    HDassert(spec_read_size >= udata->sizeof_prfx);

    /* Attempt to speculatively read both local heap prefix and heap data */
    if(FAIL == H5F_block_read(f, H5FD_MEM_LHEAP, addr, spec_read_size, dxpl_id, buf))
        H5E_THROW(H5E_READERROR, "unable to read local heap prefix");
    p = buf;

    /* Check magic number */
    if(HDmemcmp(p, H5HL_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        H5E_THROW(H5E_BADVALUE, "bad local heap signature");
    p += H5_SIZEOF_MAGIC;

    /* Version */
    if(H5HL_VERSION != *p++)
        H5E_THROW(H5E_BADVALUE, "wrong version number in local heap");

    /* Reserved */
    p += 3;

    /* Allocate space in memory for the heap */
    if(NULL == (heap = H5HL__new(udata->sizeof_size, udata->sizeof_addr, udata->sizeof_prfx)))
        H5E_THROW(H5E_CANTALLOC, "can't allocate local heap structure");

    /* Allocate the heap prefix */
    if(NULL == (prfx = H5HL__prfx_new(heap)))
        H5E_THROW(H5E_CANTALLOC, "can't allocate local heap prefix");

    /* Store the prefix's address & length */
    heap->prfx_addr = udata->prfx_addr;
    heap->prfx_size = udata->sizeof_prfx;

    /* Heap data size */
    H5F_DECODE_LENGTH_LEN(p, heap->dblk_size, udata->sizeof_size);

    /* Free list head */
    H5F_DECODE_LENGTH_LEN(p, heap->free_block, udata->sizeof_size);
    if(heap->free_block != H5HL_FREE_NULL && heap->free_block >= heap->dblk_size)
        H5E_THROW(H5E_BADVALUE, "bad heap free list");

    /* Heap data address */
    H5F_addr_decode_len(udata->sizeof_addr, &p, &(heap->dblk_addr));

    /* Check if heap block exists */
    if(heap->dblk_size) {
        /* Check if heap data block is contiguous with header */
        if(H5F_addr_eq((heap->prfx_addr + heap->prfx_size), heap->dblk_addr)) {
            /* Note that the heap should be a single object in the cache */
            heap->single_cache_obj = TRUE;

            /* Allocate space for the heap data image */
            if(NULL == (heap->dblk_image = H5FL_BLK_MALLOC(lheap_chunk, heap->dblk_size)))
                H5E_THROW(H5E_CANTALLOC, "memory allocation failed");

            /* Check if the current buffer from the speculative read already has the heap data */
            if(spec_read_size >= (heap->prfx_size + heap->dblk_size)) {
                /* Set p to the start of the data block.  This is necessary
                 * because there may be a gap between the used portion of the
                 * prefix and the data block due to alignment constraints. */
                p = buf + heap->prfx_size;

                /* Copy the heap data from the speculative read buffer */
                HDmemcpy(heap->dblk_image, p, heap->dblk_size);
            } /* end if */
            else {
                /* Read the local heap data block directly into buffer */
                if(FAIL == H5F_block_read(f, H5FD_MEM_LHEAP, heap->dblk_addr, heap->dblk_size, dxpl_id, heap->dblk_image))
                    H5E_THROW(H5E_READERROR, "unable to read heap data");
            } /* end else */

            /* Build free list */
            if(FAIL == H5HL__fl_deserialize(heap))
                H5E_THROW(H5E_CANTINIT, "can't initialize free list");
        } /* end if */
        else
            /* Note that the heap should _NOT_ be a single object in the cache */
            heap->single_cache_obj = FALSE;
    } /* end if */

    /* Set return value */
    ret_value = prfx;

CATCH
    /* Release the [possibly partially initialized] local heap on errors */
    if(!ret_value) {
        if(prfx) {
            if(FAIL == H5HL__prfx_dest(prfx))
                H5E_THROW(H5E_CANTRELEASE, "unable to destroy local heap prefix");
        } /* end if */
        else {
            if(heap && FAIL == H5HL__dest(heap))
                H5E_THROW(H5E_CANTRELEASE, "unable to destroy local heap");
        } /* end else */
    } /* end if */

END_FUNC(STATIC) /* end H5HL__prefix_load() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__prefix_flush
 *
 * Purpose:     Flushes a heap from memory to disk if it's dirty.  Optionally
 *              deletes the heap from memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Robb Matzke
 *              Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__prefix_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5HL_prfx_t *prfx, unsigned UNUSED *flags_ptr))

    H5WB_t      *wb = NULL;                     /* Wrapped buffer for heap data */
    uint8_t heap_buf[H5HL_SPEC_READ_SIZE];      /* Buffer for heap              */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(prfx);

    if(prfx->cache_info.is_dirty) {
        H5HL_t *heap = prfx->heap;  /* Pointer to the local heap */
        uint8_t *buf;               /* Pointer to heap buffer */
        size_t buf_size;            /* Size of buffer for encoding & writing heap info */
        uint8_t *p;                 /* Pointer into raw data buffer */

        /* Wrap the local buffer for serialized heap info */
        if(NULL == (wb = H5WB_wrap(heap_buf, sizeof(heap_buf))))
            H5E_THROW(H5E_CANTINIT, "can't wrap buffer");


        /* Compute the size of the buffer to encode & write */
        buf_size = heap->prfx_size;
        if(heap->single_cache_obj)
            buf_size += heap->dblk_size;

        /* Get a pointer to a buffer that's large enough for serialized heap */
        if(NULL == (buf = (uint8_t *)H5WB_actual(wb, buf_size)))
            H5E_THROW(H5E_NOSPACE, "can't get actual buffer");

        /* Update the free block value from the free list */
        heap->free_block = heap->freelist ? heap->freelist->offset : H5HL_FREE_NULL;

        /* Serialize the heap prefix */
        p = buf;
        HDmemcpy(p, H5HL_MAGIC, (size_t)H5_SIZEOF_MAGIC);
        p += H5_SIZEOF_MAGIC;
        *p++ = H5HL_VERSION;
        *p++ = 0;   /*reserved*/
        *p++ = 0;   /*reserved*/
        *p++ = 0;   /*reserved*/
        H5F_ENCODE_LENGTH_LEN(p, heap->dblk_size, heap->sizeof_size);
        H5F_ENCODE_LENGTH_LEN(p, heap->free_block, heap->sizeof_size);
        H5F_addr_encode_len(heap->sizeof_addr, &p, heap->dblk_addr);

        /* Check if the local heap is a single object in cache */
        if(heap->single_cache_obj) {
            if((size_t)(p - buf) < heap->prfx_size) {
                size_t gap;         /* Size of gap between prefix and data block */

                /* Set p to the start of the data block.  This is necessary because
                 * there may be a gap between the used portion of the prefix and the
                 * data block due to alignment constraints. */
                gap = heap->prfx_size - (size_t)(p - buf);
                HDmemset(p, 0, gap);
                p += gap;
            } /* end if */

            /* Serialize the free list into the heap data's image */
            H5HL__fl_serialize(heap);

            /* Copy the heap data block into the cache image */
            HDmemcpy(p, heap->dblk_image, heap->dblk_size);
        } /* end if */

        /* Write the prefix [and possibly the data block] to the file */
        if(FAIL == H5F_block_write(f, H5FD_MEM_LHEAP, addr, buf_size, dxpl_id, buf))
            H5E_THROW(H5E_WRITEERROR, "unable to write heap header and data to file");

        prfx->cache_info.is_dirty = FALSE;
    } /* end if */

    /* Should we destroy the memory version? */
    if(destroy)
        if(FAIL == H5HL__prefix_dest(f, prfx))
            H5E_THROW(H5E_CANTFREE, "unable to destroy local heap prefix");

CATCH
    /* Release resources */
    if(wb && FAIL == H5WB_unwrap(wb))
        H5E_THROW(H5E_CLOSEERROR, "can't close wrapped buffer");

END_FUNC(STATIC) /* end H5HL__prefix_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__prefix_dest
 *
 * Purpose:     Destroys a heap prefix in memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__prefix_dest(H5F_t *f, H5HL_prfx_t *prfx))

    /* check arguments */
    HDassert(prfx);
    HDassert(prfx->heap);
    HDassert(H5F_addr_eq(prfx->cache_info.addr, prfx->heap->prfx_addr));

    /* Verify that entry is clean */
    HDassert(prfx->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!prfx->cache_info.free_file_space_on_destroy || H5F_addr_defined(prfx->cache_info.addr));

    /* Check for freeing file space for local heap prefix */
    if(prfx->cache_info.free_file_space_on_destroy) {
        hsize_t free_size;       /* Size of region to free in file */

        /* Compute size to free for later */
        free_size = prfx->heap->prfx_size;
        if(prfx->heap->single_cache_obj)
            free_size += prfx->heap->dblk_size;

        /* Free the local heap prefix [and possible the data block] on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(FAIL == H5MF_xfree(f, H5FD_MEM_LHEAP, H5AC_dxpl_id, prfx->cache_info.addr, free_size))
            H5E_THROW(H5E_CANTFREE, "unable to free local heap prefix");
    } /* end if */

    /* Destroy local heap prefix */
    if(FAIL == H5HL__prfx_dest(prfx))
        H5E_THROW(H5E_CANTRELEASE, "can't destroy local heap prefix");

CATCH
    /* No special processing on errors */
    
END_FUNC(STATIC) /* end H5HL__prefix_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__prefix_clear
 *
 * Purpose:     Mark a local heap prefix in memory as non-dirty.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Mar 20 2003
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__prefix_clear(H5F_t UNUSED *f, H5HL_prfx_t *prfx, hbool_t destroy))

    /* check arguments */
    HDassert(prfx);

    /* Mark heap prefix as clean */
    prfx->cache_info.is_dirty = FALSE;

    if(destroy)
        if(FAIL == H5HL__prefix_dest(f, prfx))
            H5E_THROW(H5E_CANTFREE, "unable to destroy local heap prefix");
            
CATCH
    /* No special processing on errors */

END_FUNC(STATIC) /* end H5HL__prefix_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__prefix_size
 *
 * Purpose:     Compute the size in bytes of the heap prefix on disk,
 *              and return it in *len_ptr.  On failure, the value of *len_ptr
 *              is undefined.
 *
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  John Mainzer
 *              5/13/04
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5HL__prefix_size(const H5F_t UNUSED *f, H5HL_prfx_t *prfx, size_t *size_ptr))

    /* check arguments */
    HDassert(prfx);
    HDassert(prfx->heap);
    HDassert(size_ptr);

    /* Calculate size of prefix in cache */
    *size_ptr = prfx->heap->prfx_size;

    /* If the heap is stored as a single object, add in the data block size also */
    if(prfx->heap->single_cache_obj)
        *size_ptr += prfx->heap->dblk_size;

END_FUNC(STATIC) /* H5HL__prefix_size() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_load
 *
 * Purpose:     Loads a local heap data block from disk.
 *
 * Return:      Success:    Pointer to a local heap data block memory data
 *                          structure.
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Jan  5 2010
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
H5HL_dblk_t *, NULL, NULL,
H5HL__datablock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, void *_udata))

    H5HL_dblk_t *dblk = NULL;       /* Local heap data block deserialized */
    H5HL_cache_dblk_ud_t *udata = (H5HL_cache_dblk_ud_t *)_udata;       /* User data for callback */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(udata);
    HDassert(udata->heap);
    HDassert(!udata->heap->single_cache_obj);
    HDassert(NULL == udata->heap->dblk);

    /* Allocate space in memory for the heap data block */
    if(NULL == (dblk = H5HL__dblk_new(udata->heap)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed");

    /* Check for heap still retaining image */
    if(NULL == udata->heap->dblk_image) {
        /* Allocate space for the heap data image */
        if(NULL == (udata->heap->dblk_image = H5FL_BLK_MALLOC(lheap_chunk, udata->heap->dblk_size)))
            H5E_THROW(H5E_CANTALLOC, "can't allocate data block image buffer");

        /* Read local heap data block */
        if(FAIL == H5F_block_read(f, H5FD_MEM_LHEAP, udata->heap->dblk_addr, udata->heap->dblk_size, dxpl_id, udata->heap->dblk_image))
            H5E_THROW(H5E_READERROR, "unable to read local heap data block");

        /* Build free list */
        if(FAIL == H5HL__fl_deserialize(udata->heap))
            H5E_THROW(H5E_CANTINIT, "can't initialize free list");
    } /* end if */

    /* Set flag to indicate data block from loaded from file */
    udata->loaded = TRUE;

    /* Set return value */
    ret_value = dblk;

CATCH
    /* Release the [possibly partially initialized] local heap on errors */
    if(!ret_value && dblk)
        if(FAIL == H5HL__dblk_dest(dblk))
            H5E_THROW(H5E_CANTRELEASE, "unable to destroy local heap data block");

END_FUNC(STATIC) /* end H5HL__datablock_load() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_flush
 *
 * Purpose:     Flushes a heap's data block from memory to disk if it's dirty.
 *              Optionally deletes the heap data block from memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Robb Matzke
 *              Jul 17 1997
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__datablock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr,
    H5HL_dblk_t *dblk, unsigned UNUSED * flags_ptr))

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblk);
    HDassert(dblk->heap);
    HDassert(!dblk->heap->single_cache_obj);

    if(dblk->cache_info.is_dirty) {
        H5HL_t *heap = dblk->heap;      /* Pointer to the local heap */

        /* Update the free block value from the free list */
        heap->free_block = heap->freelist ? heap->freelist->offset : H5HL_FREE_NULL;

        /* Serialize the free list into the heap data's image */
        H5HL__fl_serialize(heap);

        /* Write the data block to the file */
        if(FAIL == H5F_block_write(f, H5FD_MEM_LHEAP, heap->dblk_addr, heap->dblk_size, dxpl_id, heap->dblk_image))
            H5E_THROW(H5E_WRITEERROR, "unable to write heap data block to file");

        dblk->cache_info.is_dirty = FALSE;
    } /* end if */

    /* Should we destroy the memory version? */
    if(destroy)
        if(FAIL == H5HL__datablock_dest(f, dblk))
            H5E_THROW(H5E_CANTFREE, "unable to destroy local heap data block");

CATCH
    /* No special processing on errors */

END_FUNC(STATIC) /* end H5HL__datablock_flush() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_dest
 *
 * Purpose:     Destroys a local heap data block in memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__datablock_dest(H5F_t *f, H5HL_dblk_t *dblk))

    /* check arguments */
    HDassert(dblk);
    HDassert(dblk->heap);
    HDassert(!dblk->heap->single_cache_obj);
    HDassert(H5F_addr_eq(dblk->cache_info.addr, dblk->heap->dblk_addr));

    /* Verify that entry is clean */
    HDassert(dblk->cache_info.is_dirty == FALSE);

    /* If we're going to free the space on disk, the address must be valid */
    HDassert(!dblk->cache_info.free_file_space_on_destroy || H5F_addr_defined(dblk->cache_info.addr));

    /* Check for freeing file space for local heap data block */
    if(dblk->cache_info.free_file_space_on_destroy) {
        /* Free the local heap data block on disk */
        /* (XXX: Nasty usage of internal DXPL value! -QAK) */
        if(FAIL == H5MF_xfree(f, H5FD_MEM_LHEAP, H5AC_dxpl_id, dblk->cache_info.addr, (hsize_t)dblk->heap->dblk_size))
            H5E_THROW(H5E_CANTFREE, "unable to free local heap data block");
    } /* end if */

    /* Destroy local heap data block */
    if(FAIL == H5HL__dblk_dest(dblk))
        H5E_THROW(H5E_CANTRELEASE, "can't destroy local heap data block");

CATCH
    /* No special processing on errors */
    
END_FUNC(STATIC) /* end H5HL__datablock_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_clear
 *
 * Purpose:     Mark a local heap data block in memory as non-dirty.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Mar 20 2003
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__datablock_clear(H5F_t *f, H5HL_dblk_t *dblk, hbool_t destroy))

    /* check arguments */
    HDassert(dblk);

    /* Mark local heap data block as clean */
    dblk->cache_info.is_dirty = FALSE;

    if(destroy)
        if(FAIL == H5HL__datablock_dest(f, dblk))
            H5E_THROW(H5E_CANTFREE, "unable to destroy local heap data block");

CATCH

END_FUNC(STATIC) /* end H5HL__datablock_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2011
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5HL__datablock_notify(H5AC_notify_action_t action, H5HL_dblk_t *dblk))
    
    /* Sanity check */
    HDassert(dblk);

    /* Check if the file was opened with SWMR-write access */
    if(dblk->heap->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
                /* Create flush dependency on parent */
                if(FAIL == H5HL__create_flush_depend((H5AC_info_t *)dblk->heap->prfx, (H5AC_info_t *)dblk))
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between data block and parent, address = %llu", (unsigned long long)dblk->heap->dblk_addr);
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* Nothing to do */
                break;

            default:
#ifdef NDEBUG
                H5E_THROW(H5E_BADVALUE, "unknown action from metadata cache");
#else /* NDEBUG */
                HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
        } /* end switch */
    } /* end if */

CATCH
    /* No special processing on errors */

END_FUNC(STATIC) /* end H5HL__datablock_notify() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__datablock_size
 *
 * Purpose:     Compute the size in bytes of the local heap data block on disk,
 *              and return it in *len_ptr.  On failure, the value of *len_ptr
 *              is undefined.
 *
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  John Mainzer
 *              5/13/04
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5HL__datablock_size(const H5F_t UNUSED *f, H5HL_dblk_t *dblk, size_t *size_ptr))

    /* check arguments */
    HDassert(dblk);
    HDassert(dblk->heap);
    HDassert(size_ptr);

    /* Set size of data block in cache */
    *size_ptr = dblk->heap->dblk_size;

END_FUNC(STATIC) /* H5HL__datablock_size() */

