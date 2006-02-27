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
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


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
H5HF_create(H5F_t *f, hid_t dxpl_id, H5HF_create_t *cparam, haddr_t *addr_p)
{
    H5HF_t *fh = NULL;                  /* The new fractal heap header information */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(addr_p);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (fh = H5FL_MALLOC(H5HF_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap header")

    /* Assign internal information */
    HDmemset(&fh->cache_info, 0, sizeof(H5AC_info_t));

    /* Initialize shared fractal heap info */
    if(H5HF_shared_init(fh, cparam) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create shared fractal heap info")

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(f, H5FD_MEM_FHEAP_HDR, dxpl_id, (hsize_t)H5HF_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap header")

    /* Cache the new fractal heap node */
    if(H5AC_set(f, dxpl_id, H5AC_FHEAP_HDR, *addr_p, fh, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap header to cache")

done:
    if(ret_value < 0) {
	if(fh)
            (void)H5HF_cache_hdr_dest(f, fh);
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
    H5HF_t *fh = NULL;                  /* The new fractal heap header information */
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    unsigned hdr_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for header */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5HF_insert, FAIL)

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
    if(NULL == (fh = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

    /* Get the pointer to the shared fractal heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    /* Check if object is large enough to be standalone */
    if(size >= shared->standalone_size) {
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "standalone blocks not supported yet")
    } /* end if */
    else {
        /* Check if we are in "append only" mode, or if there's enough room for the object */
        if(shared->write_once) {
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "'write once' managed blocks not supported yet")
        } /* end if */
        else if(size <= shared->total_man_free) {
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "allocating internal managed blocks not supported yet")
        } /* end if */
        else {
            /* Allocate space at end of heap */
            if(H5HF_man_alloc_end(shared, size, obj, id) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate space at end of managed blocks")
        } /* end else */
    } /* end else */

done:
    if(fh && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, addr, fh, hdr_flags) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_insert() */

