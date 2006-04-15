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

/* Declare a free list to manage the H5HF_t struct */
H5FL_DEFINE(H5HF_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_create
 *
 * Purpose:	Creates a new empty fractal heap in the file.
 *
 * Return:	Non-negative on success (with address of new fractal heap
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_create(H5F_t *f, hid_t dxpl_id, H5HF_create_t *cparam, haddr_t *addr_p,
    size_t *id_len_p)
{
    H5HF_t *hdr = NULL;                  /* The new fractal heap header information */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5HF_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(cparam);
    HDassert(addr_p);
    HDassert(id_len_p);

    /* Allocate & basic initialization for the shared header */
    if(NULL == (hdr = H5HF_hdr_alloc(f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate space for shared heap info")

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(f, H5FD_MEM_FHEAP_HDR, dxpl_id, (hsize_t)H5HF_HEADER_SIZE(hdr))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap header")

    /* Initialize shared fractal heap header */
    if(H5HF_hdr_init(hdr, *addr_p, cparam) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't initialize shared fractal heap header")

    /* Set the length of heap IDs */
    *id_len_p = hdr->id_len;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->id_len = %Zu\n", FUNC, hdr->id_len);
#endif /* QAK */

    /* Cache the new fractal heap header */
    if(H5AC_set(f, dxpl_id, H5AC_FHEAP_HDR, *addr_p, hdr, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap header to cache")

done:
    if(ret_value < 0) {
	if(hdr)
            (void)H5HF_cache_hdr_dest(f, hdr);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_create() */


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
H5HF_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, size_t size,
    const void *obj, void *id/*out*/)
{
    H5HF_t *hdr = NULL;                  /* The fractal heap header information */
    unsigned hdr_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for header */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_insert, FAIL)
#ifdef QAK
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(size > 0);
    HDassert(obj);
    HDassert(id);

    /*
     * Load the fractal heap header.
     */
    if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

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

    /* Check for making header dirty */
    if(hdr->dirty)
        hdr_flags |= H5AC__DIRTIED_FLAG;

done:
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, addr, hdr, hdr_flags) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_insert() */


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
H5HF_read(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_id,
    void *obj/*out*/)
{
    H5HF_t *hdr = NULL;                  /* The fractal heap header information */
    const uint8_t *id = (const uint8_t *)_id;   /* Object ID */
    hsize_t obj_off;                    /* Object's offset in heap */
    size_t obj_len;                     /* Object's length in heap */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_read, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(id);
    HDassert(obj);

    /*
     * Load the fractal heap header.
     */
    if(NULL == (hdr = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

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
    if(hdr && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, addr, hdr, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_read() */

