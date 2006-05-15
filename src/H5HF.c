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
 * Created:		H5HF.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implements a "fractal heap" for storing variable-
 *                      length objects in a file.
 *
 *                      Please see the documentation in:
 *                      doc/html/TechNotes/FractalHeap.html for a full description
 *                      of how they work, etc.
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
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FOprivate.h"        /* File objects                         */
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MFprivate.h"	/* File memory management		*/

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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5HF_t struct */
H5FL_DEFINE_STATIC(H5HF_t);



/*-------------------------------------------------------------------------
 * Function:	H5HF_create
 *
 * Purpose:	Creates a new empty fractal heap in the file.
 *
 * Return:	Pointer to heap wrapper on success
 *              NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_t *
H5HF_create(H5F_t *f, hid_t dxpl_id, H5HF_create_t *cparam)
{
    H5HF_t *fh = NULL;          /* Pointer to new fractal heap */
    H5HF_hdr_t *hdr = NULL;     /* The new fractal heap header information */
    haddr_t hdr_addr;           /* Heap header address */
    H5HF_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5HF_create, NULL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(cparam);

    /* Allocate & basic initialization for the shared header */
    if(NULL == (hdr = H5HF_hdr_alloc(f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't allocate space for shared heap info")

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (hdr_addr = H5MF_alloc(f, H5FD_MEM_FHEAP_HDR, dxpl_id, (hsize_t)H5HF_HEADER_SIZE(hdr))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "file allocation failed for fractal heap header")

    /* Initialize shared fractal heap header */
    /* (This routine is only called for newly created heaps) */
    if(H5HF_hdr_init(hdr, hdr_addr, cparam) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't initialize shared fractal heap header")

#ifdef QAK
HDfprintf(stderr, "%s: hdr->id_len = %Zu\n", FUNC, hdr->id_len);
#endif /* QAK */

    /* Cache the new fractal heap header */
    if(H5AC_set(f, dxpl_id, H5AC_FHEAP_HDR, hdr_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't add fractal heap header to cache")

    /* Create fractal heap wrapper */
    if(NULL == (fh = H5FL_MALLOC(H5HF_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fractal heap info")

    /* Lock the heap header into memory */
    if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, hdr_addr, NULL, NULL, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to load fractal heap header")

    /* Point fractal heap wrapper at header and bump it's ref count */
    fh->hdr = hdr;
    if(H5HF_hdr_incr(fh->hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Unlock heap header, now pinned */
    if(H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, hdr_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap header")
    hdr = NULL;

    /* Add heap to list of open objects in file */
    if(H5FO_insert(f, fh->hdr->heap_addr, fh) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert heap into list of open objects")

    /* Set open object count */
    fh->fo_count = 1;

    /* Set the return value */
    ret_value = fh;

done:
    if(!ret_value) {
        if(fh)
            (void)H5HF_close(fh, dxpl_id);
	else if(hdr)
            (void)H5HF_cache_hdr_dest(f, hdr);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_open
 *
 * Purpose:	Opens an existing fractal heap in the file.
 *
 * Return:	Pointer to heap wrapper on success
 *              NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 18 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_t *
H5HF_open(H5F_t *f, hid_t dxpl_id, haddr_t fh_addr)
{
    H5HF_t *fh = NULL;          /* Pointer to new fractal heap */
    H5HF_hdr_t *hdr = NULL;     /* The new fractal heap header information */
    H5HF_t *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI(H5HF_open, NULL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(fh_addr));

    /* Check if group was already open */
    if((fh = H5FO_opened(f, fh_addr)) == NULL) {

        /* Clear any errors from H5FO_opened() */
        H5E_clear_stack(NULL);

        /* Load the heap header into memory */
#ifdef QAK
HDfprintf(stderr, "%s: fh_addr = %a\n", FUNC, fh_addr);
#endif /* QAK */
        if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, NULL, NULL, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "unable to load fractal heap header")
#ifdef QAK
HDfprintf(stderr, "%s: hdr->rc = %u\n", FUNC, hdr->rc);
#endif /* QAK */

        /* Create fractal heap info */
        if(NULL == (fh = H5FL_MALLOC(H5HF_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fractal heap info")

        /* Point fractal heap wrapper at header */
        fh->hdr = hdr;
        if(H5HF_hdr_incr(fh->hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

        /* Add heap to list of open objects in file */
        if(H5FO_insert(f, fh->hdr->heap_addr, fh) < 0)
            HGOTO_ERROR(H5E_SYM, H5E_CANTINSERT, NULL, "can't insert heap into list of open objects")

        /* Set open object count */
        fh->fo_count = 1;
    } /* end if */
    else {
        /* Increment shared reference count */
        fh->fo_count++;
    } /* end else */

    /* Set the return value */
    ret_value = fh;

done:
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, fh_addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, NULL, "unable to release fractal heap header")
    if(!ret_value) {
        if(fh)
            (void)H5HF_close(fh, dxpl_id);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_open() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_id_len
 *
 * Purpose:	Get the size of IDs for entries in a fractal heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_id_len(H5HF_t *fh, size_t *id_len_p)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5HF_get_id_len)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(id_len_p);

    /* Retrieve the ID length for entries in this heap */
    *id_len_p = fh->hdr->id_len;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_get_id_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_heap_addr
 *
 * Purpose:	Get the address of a fractal heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 18 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_heap_addr(H5HF_t *fh, haddr_t *heap_addr_p)
{
    FUNC_ENTER_NOAPI_NOFUNC(H5HF_get_heap_addr)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(heap_addr_p);

    /* Retrieve the heap header address for this heap */
    *heap_addr_p = fh->hdr->heap_addr;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_get_heap_addr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_insert
 *
 * Purpose:	Insert a new object into a fractal heap.
 *
 * Return:	Non-negative on success (with heap ID of new object
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_insert(H5HF_t *fh, hid_t dxpl_id, size_t size, const void *obj,
    void *id/*out*/)
{
    H5HF_hdr_t *hdr = NULL;                  /* The fractal heap header information */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_insert, FAIL)
#ifdef QAK
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(size > 0);
    HDassert(obj);
    HDassert(id);

    /* Get the fractal heap header */
    hdr = fh->hdr;

    /* Check if object is large enough to be standalone */
    if(size >= hdr->standalone_size) {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "standalone blocks not supported yet")
    } /* end if */
    else {
        /* Check if we are in "append only" mode, or if there's enough room for the object */
        if(hdr->write_once) {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "'write once' managed blocks not supported yet")
        } /* end if */
        else {
            H5HF_free_section_t *sec_node;         /* Pointer to free space section */

            /* Find free space in heap */
            if(H5HF_man_find(hdr, dxpl_id, size, &sec_node) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate space in fractal heap")

            /* Use free space for allocating object */
            if(H5HF_man_insert(hdr, dxpl_id, sec_node, size, obj, id) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate space for object in fractal heap")
        } /* end else */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_get_obj_len
 *
 * Purpose:	Get the size of an entry in a fractal heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  9 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_get_obj_len(H5HF_t *fh, const void *_id, size_t *obj_len_p)
{
    const uint8_t *id = (const uint8_t *)_id;   /* Object ID */

    FUNC_ENTER_NOAPI_NOFUNC(H5HF_get_obj_len)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(id);
    HDassert(obj_len_p);

    /* Skip over object offset */
    id += fh->hdr->heap_off_size;

    /* Retrieve the entry length */
    UINT64DECODE_VAR(id, *obj_len_p, fh->hdr->id_len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_get_obj_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_read
 *
 * Purpose:	Read an object from a fractal heap into a buffer
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 18 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_read(H5HF_t *fh, hid_t dxpl_id, const void *_id, void *obj/*out*/)
{
    H5HF_hdr_t *hdr = NULL;                  /* The fractal heap header information */
    const uint8_t *id = (const uint8_t *)_id;   /* Object ID */
    hsize_t obj_off;                    /* Object's offset in heap */
    size_t obj_len;                     /* Object's length in heap */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_read, FAIL)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(id);
    HDassert(obj);

    /* Get the fractal heap header */
    hdr = fh->hdr;

    /* Decode the object offset within the heap & it's length */
    UINT64DECODE_VAR(id, obj_off, hdr->heap_off_size);
    UINT64DECODE_VAR(id, obj_len, hdr->id_len);
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu, obj_len = %Zu\n", FUNC, obj_off, obj_len);
#endif /* QAK */

    /* Check for standalone object */
    if(obj_off & 0) {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "standalone blocks not supported yet")
    } /* end if */
    else {
        /* Read object from managed heap blocks */
        if(H5HF_man_read(hdr, dxpl_id, obj_off, obj_len, obj) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't read object from fractal heap")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_read() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_close
 *
 * Purpose:	Close a fractal heap wrapper
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_close(H5HF_t *fh, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_close, FAIL)

    /*
     * Check arguments.
     */
    HDassert(fh);

    /* Decrement shared object count */
    --fh->fo_count;

    /* Check if this is the last reference to the shared heap wrapper */
    if(0 == fh->fo_count) {
        /* Remove the heap from the list of opened objects in the file */
        if(H5FO_delete(fh->hdr->f, H5AC_dxpl_id, fh->hdr->heap_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't remove group from list of open objects")

        /* Close the free space information */
        if(H5HF_space_close(fh->hdr, dxpl_id) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't release free space info")

        /* Reset the block iterator */
        if(H5HF_man_iter_reset(&fh->hdr->next_block) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't reset block iterator")

        /* Decrement the reference count on the heap header */
        if(H5HF_hdr_decr(fh->hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared heap header")

        /* Release the fractal heap wrapper */
        H5FL_FREE(H5HF_t, fh);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_close() */

