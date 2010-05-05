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
 * Created:		H5HGcache.c
 *			October 6, 2008
 *                      Mike McGreevy <mamcgree@hdfgroup.org>
 *
 * Purpose:		Implement gloabl heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */
#define H5HG_PACKAGE		/*suppress error about including H5HGpkg  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5HGpkg.h"		/* Global heaps				*/
#include "H5MMprivate.h"	/* Memory management			*/


/****************/
/* Local Macros */
/****************/


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
static void *H5HG_deserialize(const void *image, size_t len, void *udata,
    hbool_t *dirty);
static herr_t H5HG_image_len(const void *thing, size_t *image_len_ptr);
static herr_t H5HG_serialize(const H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5HG_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/*
 * H5HG inherits cache-like properties from H5AC
 */
const H5AC_class_t H5AC_GHEAP[1] = {{
    H5AC_GHEAP_ID,
    "global heap",
    H5FD_MEM_GHEAP,
    H5HG_deserialize,
    H5HG_image_len,
    H5HG_serialize,
    H5HG_free_icr,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HG_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HG_deserialize(const void *image, size_t UNUSED len, void *_udata,
    hbool_t UNUSED *dirty)
{
    H5HG_heap_t	*heap = NULL;
    H5F_t *f = (H5F_t *)_udata;
    uint8_t	*p;
    size_t	nalloc, need;
    size_t      max_idx = 0;            /* The maximum index seen */
    H5HG_heap_t	*ret_value;             /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HG_deserialize)

    /* check arguments */
    HDassert(image);

    /* Allocate space for heap */
    if(NULL == (heap = H5FL_CALLOC(H5HG_heap_t)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")
    heap->shared = f->shared;

    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5HG_MAGIC, (size_t)H5HG_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad global heap collection signature")
    p += H5HG_SIZEOF_MAGIC;

    /* Version */
    if(H5HG_VERSION != *p++)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong version number in global heap")

    /* Reserved */
    p += 3;

    /* Size */
    H5F_DECODE_LENGTH(f, p, heap->size);
    HDassert(heap->size >= H5HG_MINSIZE);

    /* if heap->size is more than the allocated image size, then we need to do nothing and wait for correctly sized image to come in */
    if(heap->size <= len) {
        /* Allocate space for the heap->chunk */
        if(NULL == (heap->chunk = H5FL_BLK_MALLOC(gheap_chunk, (size_t)heap->size)))
    	    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

        /* Copy image into chunk */
        HDmemcpy(heap->chunk, image, heap->size);

        /* Decode each object */
        p = heap->chunk + H5HG_SIZEOF_HDR(f);
        nalloc = H5HG_NOBJS(f, heap->size);
        /* Calloc the obj array because the file format spec makes no guarantee
         * about the order of the objects, and unused slots must be set to zero.
         */
        if(NULL == (heap->obj = H5FL_SEQ_CALLOC(H5HG_obj_t, nalloc)))
    	    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")
        heap->nalloc = nalloc;
        while(p < (heap->chunk + heap->size)) {
	    if((p + H5HG_SIZEOF_OBJHDR(f)) > (heap->chunk + heap->size)) {
	        /*
	         * The last bit of space is too tiny for an object header, so we
	         * assume that it's free space.
	         */
	        HDassert(NULL == heap->obj[0].begin);
	        heap->obj[0].size = ((const uint8_t *)heap->chunk + heap->size) - p;
	        heap->obj[0].begin = p;
	        p += heap->obj[0].size;
	    } /* end if */
            else {
	        unsigned idx;
	        uint8_t *begin = p;

	        UINT16DECODE(p, idx);

                /* Check if we need more room to store heap objects */
                if(idx >= heap->nalloc) {
                    size_t new_alloc;       /* New allocation number */
                    H5HG_obj_t *new_obj;	/* New array of object descriptions */

                    /* Determine the new number of objects to index */
                    new_alloc = MAX(heap->nalloc * 2, (idx + 1));
                    HDassert(idx < new_alloc);

                    /* Reallocate array of objects */
                    if(NULL == (new_obj = H5FL_SEQ_REALLOC(H5HG_obj_t, heap->obj, new_alloc)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

                    /* Clear newly allocated space */
                    HDmemset(&new_obj[heap->nalloc], 0, (new_alloc - heap->nalloc) * sizeof(heap->obj[0]));

                    /* Update heap information */
                    heap->nalloc = new_alloc;
                    heap->obj = new_obj;
                    HDassert(heap->nalloc > heap->nused);
                } /* end if */

	        UINT16DECODE(p, heap->obj[idx].nrefs);
	        p += 4; /*reserved*/
	        H5F_DECODE_LENGTH(f, p, heap->obj[idx].size);
	        heap->obj[idx].begin = begin;
	        /*
	         * The total storage size includes the size of the object header
	         * and is zero padded so the next object header is properly
	         * aligned. The entire obj array was calloc'ed, so no need to zero
	         * the space here. The last bit of space is the free space object
	         * whose size is never padded and already includes the object
	         * header.
	         */
	        if(idx > 0) {
		    need = H5HG_SIZEOF_OBJHDR(f) + H5HG_ALIGN(heap->obj[idx].size);

                    if(idx > max_idx)
                        max_idx = idx;
	        } /* end if */
                else
		    need = heap->obj[idx].size;
	        p = begin + need;
	    } /* end else */
        } /* end while */
        HDassert(p == heap->chunk + heap->size);
        HDassert(H5HG_ISALIGNED(heap->obj[0].size));

        /* Set the next index value to use */
        if(max_idx > 0)
            heap->nused = max_idx + 1;
        else
            heap->nused = 1;

        HDassert(max_idx < heap->nused);

        /*
         * Add the new heap to the CWFS list, removing some other entry if
         * necessary to make room. We remove the right-most entry that has less
         * free space than this heap.
         */
        if(!f->shared->cwfs) {
            if(NULL == (f->shared->cwfs = (H5HG_heap_t **)H5MM_malloc(H5HG_NCWFS * sizeof(H5HG_heap_t *))))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")
            f->shared->ncwfs = 1;
            f->shared->cwfs[0] = heap;
        } else if(H5HG_NCWFS == f->shared->ncwfs) {
            int i;          /* Local index variable */

            for(i = H5HG_NCWFS - 1; i >= 0; --i)
                if(f->shared->cwfs[i]->obj[0].size < heap->obj[0].size) {
                    HDmemmove(f->shared->cwfs + 1, f->shared->cwfs, i * sizeof(H5HG_heap_t *));
                    f->shared->cwfs[0] = heap;
                    break;
                } /* end if */
        } else {
            HDmemmove(f->shared->cwfs + 1, f->shared->cwfs, f->shared->ncwfs * sizeof(H5HG_heap_t *));
            f->shared->ncwfs += 1;
            f->shared->cwfs[0] = heap;
        } /* end else */

        /* Sanity check */
        HDassert((size_t)((const uint8_t *)p - (const uint8_t *)heap->chunk) <= len);
    } /* end if heap->size <= len */

    ret_value = heap;

done:
    if(!ret_value && heap)
        if(H5HG_free(heap) < 0)
	    HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy global heap collection")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HG_serialize
 *
 * Purpose:	Serialize the data structure for writing to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, March 27, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG_serialize(const H5F_t *f, hid_t UNUSED dxpl_id, haddr_t UNUSED addr,
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5HG_heap_t *heap = (H5HG_heap_t *)_thing;
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HG_serialize)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(H5F_addr_eq(addr, heap->addr));
    HDassert(heap);

    /* Need to increase image size if we need to copy a bigger thing into it */
    if(heap->size > len) {
        /* free old image buffer */
        H5MM_free(image);

        /* allocate new image buffer */
        if(NULL == (*new_image = H5MM_malloc(heap->size)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "new image null after H5MM_realloc()\n")

        /* copy the heap->chunk into the new image buffer */
        HDmemcpy(*new_image, heap->chunk, heap->size);

        /* set new length of image */
        *new_len = heap->size;

        /* specify in flags that image has been resized */
        *flags = H5AC__SERIALIZE_RESIZED_FLAG;
    } /* end if */
    else {
        /* copy the heap->chunk into the image buffer */
        HDmemcpy(image, heap->chunk, heap->size);

        /* Reset the cache flags for this operation */
        *flags = 0;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HG_image_len
 *
 * Purpose:     Tell the metadata cache about the actual size
 *              of the global heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG_image_len(const void *thing, size_t *image_len_ptr)
{
    const H5HG_heap_t    *heap = (const H5HG_heap_t *)thing;    /* Global heap */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HG_image_len)

    /* Check arguments */
    HDassert(heap);
    HDassert(image_len_ptr);

    /* Report the global heap's total size */
    *image_len_ptr = heap->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HG_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HG_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mike McGreevy
 *		mcgreevy@hdfgroup.org
 *		July 28, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG_free_icr(void *thing)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HG_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy global heap collection */
    if(H5HG_free(thing) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy global heap")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG_free_icr() */

