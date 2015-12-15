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
 *			Feb  5 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Implement global heap metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5HGmodule.h"         /* This source code file is part of the H5HG module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* File access				*/
#include "H5HGpkg.h"		/* Global heaps				*/
#include "H5MFprivate.h"	/* File memory management		*/
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
static herr_t H5HG__cache_heap_get_load_size(const void *_image, void *udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static void *H5HG__cache_heap_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty); 
static herr_t H5HG__cache_heap_image_len(const void *thing, size_t *image_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static herr_t H5HG__cache_heap_serialize(const H5F_t *f, void *image,
    size_t len, void *thing); 
static herr_t H5HG__cache_heap_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5HG inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_GHEAP[1] = {{
    H5AC_GHEAP_ID,                      /* Metadata client ID */
    "global heap",                      /* Metadata client name (for debugging) */
    H5FD_MEM_GHEAP,                     /* File space memory type for client */
    H5AC__CLASS_SPECULATIVE_LOAD_FLAG,  /* Client class behavior flags */
    H5HG__cache_heap_get_load_size,     /* 'get_load_size' callback */
    NULL, 				/* 'verify_chksum' callback */
    H5HG__cache_heap_deserialize,       /* 'deserialize' callback */
    H5HG__cache_heap_image_len,         /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5HG__cache_heap_serialize,         /* 'serialize' callback */
    NULL,                               /* 'notify' callback */
    H5HG__cache_heap_free_icr,          /* 'free_icr' callback */
    NULL,                               /* 'clear' callback */
    NULL,                               /* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5HG__cache_heap_get_load_size()
 *
 * Purpose:	Return the initial speculative read size to the metadata 
 *		cache.  This size will be used in the initial attempt to read 
 *		the global heap.  If this read is too small, the cache will 
 *		try again with the correct value obtained from 
 *		H5HG__cache_heap_image_len().
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/27/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG__cache_heap_get_load_size(const void *_image, void *_udata, size_t *image_len, size_t *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const uint8_t *image = (const uint8_t *)_image;   	/* Pointer into raw data buffer */
    H5F_t *f = (H5F_t *)_udata;                         /* File pointer -- obtained from user data */
    size_t heap_size;                                   /* Total size of collection      */
    htri_t ret_value = SUCCEED;                         /* Return value */

    FUNC_ENTER_STATIC

    HDassert(image_len);

    if(image == NULL) {
	*image_len = (size_t)H5HG_MINSIZE;

    } else { /* compute actual_len */
	HDassert(f);
	HDassert(actual_len);
	HDassert(*actual_len == *image_len);

	/* Magic number */
	if(HDmemcmp(image, H5HG_MAGIC, (size_t)H5_SIZEOF_MAGIC))
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "bad global heap collection signature")
	image += H5_SIZEOF_MAGIC;

	/* Version */
	if(H5HG_VERSION != *image++)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "wrong version number in global heap")

	/* Reserved */
	image += 3;

	/* Size */
	H5F_DECODE_LENGTH(f, image, heap_size);
	HDassert(heap_size >= H5HG_MINSIZE);
	HDassert(*image_len == H5HG_MINSIZE);

	*actual_len = heap_size;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG__cache_heap_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:    H5HG__cache_heap_deserialize
 *
 * Purpose:	Given a buffer containing the on disk image of the global 
 *		heap, deserialize it, load its contents into a newly allocated
 *		instance of H5HG_heap_t, and return a pointer to the new instance.
 *
 *		Note that this heap client uses speculative reads.  If the supplied
 *		buffer is too small, we simply make note of the correct size, and 
 *		wait for the metadata cache to try again.
 *
 * Return:      Success:        Pointer to in core representation
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              7/27/14
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HG__cache_heap_deserialize(const void *_image, size_t len, void *_udata,
    hbool_t H5_ATTR_UNUSED *dirty)
{
    H5F_t       *f = (H5F_t *)_udata;   /* File pointer -- obtained from user data */
    H5HG_heap_t *heap = NULL;   /* New global heap */
    uint8_t     *image;         /* Pointer to image to decode */
    void        *ret_value = NULL;      /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(_image);
    HDassert(len >= (size_t)H5HG_MINSIZE);
    HDassert(f);
    HDassert(dirty);

    /* Allocate a new global heap */
    if(NULL == (heap = H5FL_CALLOC(H5HG_heap_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    heap->shared = H5F_SHARED(f);
    if(NULL == (heap->chunk = H5FL_BLK_MALLOC(gheap_chunk, len)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy the image buffer into the newly allocate chunk */
    HDmemcpy(heap->chunk, _image, len);

    image = heap->chunk;

    /* Magic number */
    if(HDmemcmp(image, H5HG_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "bad global heap collection signature")
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if(H5HG_VERSION != *image++) 
        HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong version number in global heap")

    /* Reserved */
    image += 3;

    /* Size */
    H5F_DECODE_LENGTH(f, image, heap->size);
    HDassert(heap->size >= H5HG_MINSIZE);
    HDassert((len == H5HG_MINSIZE) /* first try */ || 
             ((len == heap->size) && (len > H5HG_MINSIZE))); /* second try */
    
    if(len == heap->size) {     /* proceed with the deserialize */
        size_t       max_idx = 0;
        size_t       nalloc;

        /* Decode each object */
        image = heap->chunk + H5HG_SIZEOF_HDR(f);
        nalloc = H5HG_NOBJS(f, heap->size);

        /* Calloc the obj array because the file format spec makes no guarantee
         * about the order of the objects, and unused slots must be set to zero.
         */
        if(NULL == (heap->obj = H5FL_SEQ_CALLOC(H5HG_obj_t, nalloc)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        heap->nalloc = nalloc;

        while(image < (heap->chunk + heap->size)) {
            if((image + H5HG_SIZEOF_OBJHDR(f)) > (heap->chunk + heap->size)) {
                /*
                 * The last bit of space is too tiny for an object header, so
                 * we assume that it's free space.
                 */
                HDassert(NULL == heap->obj[0].begin);
                heap->obj[0].size = (size_t)(((const uint8_t *)heap->chunk + heap->size) - image);
                heap->obj[0].begin = image;
                image += heap->obj[0].size;
            } /* end if */
            else {
                size_t need;
                unsigned idx;
                uint8_t *begin = image;

                UINT16DECODE(image, idx);

                /* Check if we need more room to store heap objects */
                if(idx >= heap->nalloc) {
                    size_t new_alloc;       /* New allocation number */
                    H5HG_obj_t *new_obj;    /* New array of object   descriptions          */

                    /* Determine the new number of objects to index */
                    new_alloc = MAX(heap->nalloc * 2, (idx + 1));
                    HDassert(idx < new_alloc);

                    /* Reallocate array of objects */
                    if(NULL == (new_obj = H5FL_SEQ_REALLOC(H5HG_obj_t, heap->obj, new_alloc)))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

                    /* Clear newly allocated space */
                    HDmemset(&new_obj[heap->nalloc], 0, (new_alloc - heap->nalloc) * sizeof(heap->obj[0]));

                    /* Update heap information */
                    heap->nalloc = new_alloc;
                    heap->obj = new_obj;
                    HDassert(heap->nalloc > heap->nused);
                } /* end if */

                UINT16DECODE(image, heap->obj[idx].nrefs);
                image += 4; /*reserved*/
                H5F_DECODE_LENGTH(f, image, heap->obj[idx].size);
                heap->obj[idx].begin = begin;

                /*
                 * The total storage size includes the size of the object 
                 * header and is zero padded so the next object header is 
                 * properly aligned. The entire obj array was calloc'ed, 
                 * so no need to zero the space here. The last bit of space 
                 * is the free space object whose size is never padded and 
                 * already includes the object header.
                 */
                if(idx > 0) {
                    need = H5HG_SIZEOF_OBJHDR(f) + H5HG_ALIGN(heap->obj[idx].size);
                    if(idx > max_idx)
                        max_idx = idx;
                } /* end if */
                else
                    need = heap->obj[idx].size;

                image = begin + need;
            } /* end else */
        } /* end while */

        HDassert(image == heap->chunk + heap->size);
        HDassert(H5HG_ISALIGNED(heap->obj[0].size));

        /* Set the next index value to use */
        if(max_idx > 0)
            heap->nused = max_idx + 1;
        else
            heap->nused = 1;

        HDassert(max_idx < heap->nused);

        /* Add the new heap to the CWFS list for the file */
        if(H5F_cwfs_add(f, heap) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "unable to add global heap collection to file's CWFS")
    } /* end if ( len == heap->size ) */
    else
        /* if len is less than heap size, then the initial speculative 
         * read was too small.  In this case we return without reporting
         * failure.  H5C_load_entry() will call H5HG__cache_heap_image_len()
         * to get the actual read size, and then repeat the read with the 
         * correct size, and call this function a second time.
         */
        HDassert(len < heap->size);

    ret_value = heap;

done:
    if(!ret_value && heap)
        if(H5HG_free(heap) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTFREE, NULL, "unable to destroy global heap collection")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG__cache_heap_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HG__cache_heap_image_len
 *
 * Purpose:	Return the on disk image size of the global heap to the 
 *		metadata cache via the image_len.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/27/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG__cache_heap_image_len(const void *_thing, size_t *image_len, 
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const H5HG_heap_t *heap = (const H5HG_heap_t *)_thing;

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(heap);
    HDassert(heap->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(heap->cache_info.type == H5AC_GHEAP);
    HDassert(heap->size >= H5HG_MINSIZE);
    HDassert(image_len);

    *image_len = heap->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HG__cache_heap_image_len() */

/**************************************/
/* no H5HG_cache_heap_pre_serialize() */
/**************************************/


/*-------------------------------------------------------------------------
 * Function:    H5HG__cache_heap_serialize
 *
 * Purpose:	Given an appropriately sized buffer and an instance of 
 *		H5HG_heap_t, serialize the global heap for writing to file,
 *		and copy the serialized verion into the buffer.
 *
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/27/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG__cache_heap_serialize(const H5F_t *f, void *image, size_t len,
    void *_thing)
{
    H5HG_heap_t *heap = (H5HG_heap_t *)_thing;

    FUNC_ENTER_STATIC_NOERR

    HDassert(f);
    HDassert(image);
    HDassert(heap);
    HDassert(heap->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(heap->cache_info.type == H5AC_GHEAP);
    HDassert(heap->size == len);
    HDassert(heap->chunk);

    /* copy the image into the buffer */
    HDmemcpy(image, heap->chunk, len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HG__cache_heap_serialize() */

/****************************************/
/* no H5HG_cache_heap_notify() function */
/****************************************/


/*-------------------------------------------------------------------------
 * Function:    H5HG__cache_heap_free_icr
 *
 * Purpose:	Free the in memory representation of the supplied global heap.
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/27/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HG__cache_heap_free_icr(void *_thing)
{
    H5HG_heap_t *heap = (H5HG_heap_t *)_thing;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(heap);
    HDassert(heap->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(heap->cache_info.type == H5AC_GHEAP);

    /* Destroy global heap collection */
    if(H5HG_free(heap) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy global heap collection")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HG__cache_heap_free_icr() */

