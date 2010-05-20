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
 *			Jul 23 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Local heap metadata cache callbacks.
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


/****************/
/* Local Macros */
/****************/

/* Local heap format version */
#define H5HL_VERSION	0

/* Value indicating end of free list on disk */
#define H5HL_FREE_NULL	1

/* Set the local heap size to speculatively read in */
/* (needs to be more than the local heap prefix size to work at all and
 *      should be larger than the default local heap size to save the
 *      extra I/O operations) */
#define H5HL_SPEC_READ_SIZE 512


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
static herr_t H5HL_prfx_get_load_size(const void *_udata, size_t *image_len);
static void *H5HL_prfx_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5HL_prfx_image_len(const void *thing, size_t *image_len_ptr);
static herr_t H5HL_prfx_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5HL_prfx_free_icr(void *thing);

static herr_t H5HL_dblk_get_load_size(const void *_udata, size_t *image_len);
static void *H5HL_dblk_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5HL_dblk_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5HL_dblk_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/*
 * H5HL prefix inherits cache-like properties from H5AC
 */
const H5AC_class_t H5AC_LHEAP_PRFX[1] = {{
    H5AC_LHEAP_PRFX_ID,
    "local heap prefix",
    H5FD_MEM_LHEAP,
    H5HL_prfx_get_load_size,
    H5HL_prfx_deserialize,
    H5HL_prfx_image_len,
    H5HL_prfx_serialize,
    H5HL_prfx_free_icr,
}};

/*
 * H5HL data block inherits cache-like properties from H5AC
 */
const H5AC_class_t H5AC_LHEAP_DBLK[1] = {{
    H5AC_LHEAP_DBLK_ID,
    "local heap data block",
    H5FD_MEM_LHEAP,
    H5HL_dblk_get_load_size,
    H5HL_dblk_deserialize,
    NULL,
    H5HL_dblk_serialize,
    H5HL_dblk_free_icr,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HL_fl_deserialize
 *
 * Purpose:	Deserialize the free list for a heap data block
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_fl_deserialize(H5HL_t *heap, hsize_t free_block)
{
    H5HL_free_t *fl, *tail = NULL;      /* Heap free block nodes */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_fl_deserialize)

    /* check arguments */
    HDassert(heap);

    /* Build free list */
    while(H5HL_FREE_NULL != free_block) {
        const uint8_t *p;               /* Pointer into image buffer */

        /* Sanity check */
        if(free_block >= heap->dblk_size)
            HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "bad heap free list")

        /* Allocate & initialize free list node */
        if(NULL == (fl = H5FL_MALLOC(H5HL_free_t)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "memory allocation failed")
        fl->offset = (size_t)free_block;
        fl->prev = tail;
        fl->next = NULL;

        /* Insert node into list */
        if(tail)
            tail->next = fl;
        tail = fl;
        if(!heap->freelist)
            heap->freelist = fl;

        /* Decode offset of next free block */
        p = heap->dblk_image + free_block;
        H5F_DECODE_LENGTH_LEN(p, free_block, heap->sizeof_size);
        if(free_block == 0)
            HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "free block size is zero?")

        /* Decode length of this free block */
        H5F_DECODE_LENGTH_LEN(p, fl->size, heap->sizeof_size);
        if((fl->offset + fl->size) > heap->dblk_size)
            HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "bad heap free list")
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_fl_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_fl_serialize
 *
 * Purpose:	Serialize the free list for a heap data block
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static void
H5HL_fl_serialize(const H5HL_t *heap)
{
    H5HL_free_t	*fl;                    /* Pointer to heap free list node */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_fl_serialize)

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

    FUNC_LEAVE_NOAPI_VOID
} /* end H5HL_fl_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HL_prfx_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_prfx_get_load_size(const void *_udata, size_t *image_len)
{
    const H5HL_cache_prfx_ud_t *udata = (const H5HL_cache_prfx_ud_t *)_udata;       /* User data for callback */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_prfx_get_load_size)

    /* Check arguments */
    HDassert(udata);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = H5HL_SPEC_READ_SIZE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HL_prfx_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 11 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HL_prfx_deserialize(const void *image, size_t len, void *_udata,
    hbool_t UNUSED *dirty)
{
    H5HL_t *heap = NULL;            /* Local heap */
    H5HL_prfx_t *prfx = NULL;       /* Heap prefix deserialized */
    H5HL_cache_prfx_ud_t *udata = (H5HL_cache_prfx_ud_t *)_udata;       /* User data for callback */
    const uint8_t *p;               /* Pointer into image buffer */
    H5HL_prfx_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_prfx_deserialize)

    /* check arguments */
    HDassert(len > 0);
    HDassert(image);
    HDassert(udata);
    HDassert(udata->sizeof_size > 0);
    HDassert(udata->sizeof_addr > 0);
    HDassert(udata->sizeof_prfx > 0);

    /* Point to beginning of image buffer */
    p = (const uint8_t *)image;

    /* Check magic number */
    if(HDmemcmp(p, H5HL_MAGIC, (size_t)H5HL_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "bad local heap signature")
    p += H5HL_SIZEOF_MAGIC;

    /* Version */
    if(H5HL_VERSION != *p++)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "wrong version number in local heap")

    /* Reserved */
    p += 3;

    /* Allocate space in memory for the heap */
    if(NULL == (heap = H5HL_new(udata->sizeof_size, udata->sizeof_addr, udata->sizeof_prfx)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Allocate the heap prefix */
    if(NULL == (prfx = H5HL_prfx_new(heap)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Store the prefix's address & length */
    heap->prfx_addr = udata->prfx_addr;
    heap->prfx_size = udata->sizeof_prfx;

    /* Heap data size */
    H5F_DECODE_LENGTH_LEN(p, heap->dblk_size, udata->sizeof_size);

    /* Free list head */
    H5F_DECODE_LENGTH_LEN(p, udata->free_block, udata->sizeof_size);
    if(udata->free_block != H5HL_FREE_NULL && udata->free_block >= heap->dblk_size)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "bad heap free list")

    /* Heap data address */
    H5F_addr_decode_len(udata->sizeof_addr, &p, &(heap->dblk_addr));

    /* Check if heap block exists */
    if(heap->dblk_size) {
        /* Check if heap data block is contiguous with header */
        if(H5F_addr_eq((heap->prfx_addr + heap->prfx_size), heap->dblk_addr)) {
            /* Note that the heap should be a single object in the cache */
            heap->single_cache_obj = TRUE;

            /* Check if the current image from the cache is big enough to hold the heap data */
            if(len >= (heap->prfx_size + heap->dblk_size)) {
                /* Allocate space for the heap data image */
                if(NULL == (heap->dblk_image = H5FL_BLK_MALLOC(lheap_chunk, heap->dblk_size)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

                /* Copy the heap data from the image */
                HDmemcpy(heap->dblk_image, p, heap->dblk_size);

                /* Build free list */
                if(H5HL_fl_deserialize(heap, udata->free_block) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't initialize free list")
            } /* end if */
            else
                /* Make certain the length was OK on the retry */
                HDassert(!udata->made_attempt);

            /* Note that we've made one attempt at decoding the local heap already */
            /* (useful when the length is incorrect and the cache will retry with a larger one) */
            udata->made_attempt = TRUE;
        } /* end if */
        else
            /* Note that the heap should _NOT_ be a single object in the cache */
            heap->single_cache_obj = FALSE;
    } /* end if */

    /* Set flag to indicate prefix from loaded from file */
    udata->loaded = TRUE;

    /* Set return value */
    ret_value = prfx;

done:
    /* Release the [possibly partially initialized] local heap on errors */
    if(!ret_value) {
        if(prfx) {
            if(H5HL_prfx_dest(prfx) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, NULL, "unable to destroy local heap prefix")
        } /* end if */
        else {
            if(heap && H5HL_dest(heap) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, NULL, "unable to destroy local heap")
        } /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_prfx_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_image_len
 *
 * Purpose:	Tell the metadata cache about the actual size of the object
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 11 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_prfx_image_len(const void *thing, size_t *image_len_ptr)
{
    const H5HL_prfx_t *prfx = (const H5HL_prfx_t *)thing;     /* The local heap prefix */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_prfx_image_len)

    /* Check arguments */
    HDassert(prfx);
    HDassert(prfx->heap);
    HDassert(image_len_ptr);

    /* Report the local heap's size, including the data block, if it's contiguous w/prefix */
    if(prfx->heap->single_cache_obj)
        *image_len_ptr = prfx->heap->prfx_size + prfx->heap->dblk_size;
    else
        *image_len_ptr = prfx->heap->prfx_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HL_prfx_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_serialize
 *
 * Purpose:	Serializes a 'in core' representation of data structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 11 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_prfx_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5HL_prfx_t *prfx = (H5HL_prfx_t *)_thing; /* Pointer to the local heap prefix */
    H5HL_t *heap;                       /* Pointer to the local heap */
    uint8_t     *p;                     /* Pointer into raw data buffer */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_prfx_serialize)

    /* check arguments */
    HDassert(image);
    HDassert(prfx);
    HDassert(prfx->heap);
    HDassert(flags);

    /* Get the pointer to the heap */
    heap = prfx->heap;

    /* Point to the cache image */
    p = (uint8_t *)image;

    /* Serialize the header */
    HDmemcpy(p, H5HL_MAGIC, (size_t)H5HL_SIZEOF_MAGIC);
    p += H5HL_SIZEOF_MAGIC;
    *p++ = H5HL_VERSION;
    *p++ = 0;	/*reserved*/
    *p++ = 0;	/*reserved*/
    *p++ = 0;	/*reserved*/
    H5F_ENCODE_LENGTH_LEN(p, heap->dblk_size, heap->sizeof_size);
    H5F_ENCODE_LENGTH_LEN(p, heap->freelist ? heap->freelist->offset : H5HL_FREE_NULL, heap->sizeof_size);
    H5F_addr_encode_len(heap->sizeof_addr, &p, heap->dblk_addr);

    /* Check if the local heap is a single object in cache */
    if(heap->single_cache_obj) {
        /* Serialize the free list into the heap data's image */
        H5HL_fl_serialize(heap);

        /* Copy the heap data block into the cache image */
        HDmemcpy(p, heap->dblk_image, heap->dblk_size);
    } /* end if */

    /* Reset the cache flags for this operation */
    *flags = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HL_prfx_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              October 11, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_prfx_free_icr(void *thing)
{
    H5HL_prfx_t *prfx = (H5HL_prfx_t *)thing;   /* Local heap prefix to destroy */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_prfx_free_icr)

    /* Check arguments */
    HDassert(prfx);

    /* Destroy local heap prefix */
    if(H5HL_prfx_dest(prfx) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't destroy local heap prefix")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HL_prfx_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5HL_dblk_get_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_dblk_get_load_size(const void *_udata, size_t *image_len)
{
    const H5HL_cache_dblk_ud_t *udata = (const H5HL_cache_dblk_ud_t *)_udata;       /* User data for callback */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_dblk_get_load_size)

    /* Check arguments */
    HDassert(udata);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = udata->heap->dblk_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HL_dblk_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dblk_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HL_dblk_deserialize(const void *image, size_t UNUSED len,
    void *_udata, hbool_t UNUSED *dirty)
{
    H5HL_dblk_t *dblk = NULL;       /* Local heap data block deserialized */
    H5HL_cache_dblk_ud_t *udata = (H5HL_cache_dblk_ud_t *)_udata;       /* User data for callback */
    H5HL_dblk_t *ret_value = NULL;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_dblk_deserialize)

    /* check arguments */
    HDassert(image);
    HDassert(udata);
    HDassert(udata->heap);
    HDassert(!udata->heap->single_cache_obj);
    HDassert(NULL == udata->heap->dblk);

    /* Allocate space in memory for the heap data block */
    if(NULL == (dblk = H5HL_dblk_new(udata->heap)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Check for heap still retaining image */
    if(NULL == udata->heap->dblk_image) {
        /* Allocate space for the heap data image */
        if(NULL == (udata->heap->dblk_image = H5FL_BLK_MALLOC(lheap_chunk, udata->heap->dblk_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

        /* Copy the cache image into the heap's data image */
        HDmemcpy(udata->heap->dblk_image, image, udata->heap->dblk_size);

        /* Build free list */
        if(H5HL_fl_deserialize(udata->heap, udata->free_block) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't initialize free list")
    } /* end if */

    /* Set flag to indicate data block from loaded from file */
    udata->loaded = TRUE;

    /* Set return value */
    ret_value = dblk;

done:
    /* Release the [possibly partially initialized] local heap on errors */
    if(!ret_value && dblk)
        if(H5HL_dblk_dest(dblk) < 0)
	    HDONE_ERROR(H5E_HEAP, H5E_CANTRELEASE, NULL, "unable to destroy local heap data block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_dblk_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dblk_serialize
 *
 * Purpose:	Serializes a 'in core' representation of data structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_dblk_serialize(const H5F_t UNUSED *f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5HL_dblk_t *dblk = (H5HL_dblk_t *)_thing;  /* Pointer to the local heap data block */
    H5HL_t      *heap = dblk->heap;             /* Pointer to the local heap */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_dblk_serialize)

    /* check arguments */
    HDassert(image);
    HDassert(dblk);
    HDassert(dblk->heap);
    HDassert(flags);

    /* Serialize the free list into the heap data's image */
    H5HL_fl_serialize(heap);

    /* Copy the heap's data block into the cache's image */
    HDmemcpy(image, heap->dblk_image, heap->dblk_size);

    /* Reset the cache flags for this operation */
    *flags = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HL_dblk_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dblk_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              October 12, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_dblk_free_icr(void *thing)
{
    H5HL_dblk_t *dblk = (H5HL_dblk_t *)thing;   /* Local heap data block to destroy */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HL_dblk_free_icr)

    /* Check arguments */
    HDassert(dblk);

    /* Destroy local heap data block */
    if(H5HL_dblk_dest(dblk) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "unable to destroy local heap data block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HL_dblk_free_icr() */

