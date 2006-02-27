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
 * Created:		H5HFint.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		"Internal" routines for fractal heaps.
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

static herr_t H5HF_shared_free(void *_shared);

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5HF_shared_t struct */
H5FL_DEFINE_STATIC(H5HF_shared_t);



/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_init
 *
 * Purpose:	Allocate & initialize shared fractal heap info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_shared_init(H5HF_t *fh, H5HF_create_t *cparam)
{
    H5HF_shared_t *shared = NULL;       /* Shared fractal heap information */
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_init)

    /* Allocate space for the shared information */
    if(NULL == (shared = H5FL_CALLOC(H5HF_shared_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap shared information")

    /* Set the creation parameters for the heap */
    shared->addrmap = cparam->addrmap;
    shared->standalone_size = cparam->standalone_size;
    shared->fixed_len_size = cparam->fixed_len_size;

    /* Make shared B-tree info reference counted */
    if(NULL == (fh->shared = H5RC_create(shared, H5HF_shared_free)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap info")

done:
    if(ret_value < 0)
        if(shared)
            H5HF_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_shared_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_free
 *
 * Purpose:	Free shared fractal heap info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_shared_free(void *_shared)
{
    H5HF_shared_t *shared = (H5HF_shared_t *)_shared;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_shared_free)

    /* Sanity check */
    HDassert(shared);

    /* Free the shared info itself */
    H5FL_FREE(H5HF_shared_t, shared);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_alloc_end
 *
 * Purpose:	Allocate space for an object at the end of the heap
 *
 * Return:	Non-negative on success (with heap ID of new object
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_alloc_end(H5HF_shared_t *shared, size_t size, const void *obj,
    void *id/*out*/)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_alloc_end)

    /*
     * Check arguments.
     */
    HDassert(shared);
    HDassert(size > 0);
    HDassert(obj);
    HDassert(id);

    /* Check if this is the first object in the heap */
    if(shared->next_man_block == 0) {
        if(size /* + H5HF_DIRECT_OVERHEAD */ <= shared->man_dtable_info.start_block_size) {
        } /* end if */
        else {
        } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "allocating first object not supported yet")
#endif /* QAK */
    } /* end if */
    else {
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "allocating objects at end of heap not supported yet")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_alloc_end() */

