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
 * Created:		H5HLint.c
 *			Oct 12 2008
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Local heap internal routines.
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
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5HLpkg.h"		/* Local Heaps				*/

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

/* Declare a free list to manage the H5HL_dblk_t struct */
H5FL_DEFINE(H5HL_dblk_t);

/* Declare a free list to manage the H5HL_prfx_t struct */
H5FL_DEFINE(H5HL_prfx_t);



/*-------------------------------------------------------------------------
 * Function:	H5HL_inc_rc
 *
 * Purpose:	Increment ref. count on heap
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_inc_rc(H5HL_t *heap)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_inc_rc)

    /* check arguments */
    HDassert(heap);

    /* Increment heap's ref. count */
    heap->rc++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HL_inc_rc() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dec_rc
 *
 * Purpose:	Decrement ref. count on heap
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HL_dec_rc(H5HL_t *heap)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HL_dec_rc)

    /* check arguments */
    HDassert(heap);

    /* Decrement heap's ref. count */
    heap->rc--;

    /* Check if we should destroy the heap */
    if(heap->rc == 0)
        H5HL_dest(heap);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HL_dec_rc() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_new
 *
 * Purpose:	Create a new local heap prefix object
 *
 * Return:	Success:	non-NULL pointer to new local heap prefix
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
H5HL_prfx_t *
H5HL_prfx_new(H5HL_t *heap)
{
    H5HL_prfx_t *prfx = NULL;       /* New local heap prefix */
    H5HL_prfx_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI(H5HL_prfx_new, NULL)

    /* check arguments */
    HDassert(heap);

    /* Allocate new local heap prefix */
    if(NULL == (prfx = H5FL_CALLOC(H5HL_prfx_t)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Increment ref. count on heap data structure */
    if(H5HL_inc_rc(heap) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment heap ref. count")

    /* Link the heap & the prefix */
    prfx->heap = heap;
    heap->prfx = prfx;

    /* Set the return value */
    ret_value = prfx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_prfx_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_prfx_dest
 *
 * Purpose:	Destroy a local heap prefix object
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_prfx_dest(H5HL_prfx_t *prfx)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5HL_prfx_dest, FAIL)

    /* check arguments */
    HDassert(prfx);

    /* Unlink prefix from heap */
    prfx->heap->prfx = NULL;

    /* Decrement ref. count on heap data structure */
    if(H5HL_dec_rc(prfx->heap) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement heap ref. count")

    /* Unlink heap from prefix */
    prfx->heap = NULL;

    /* Free local heap prefix */
    H5FL_FREE(H5HL_prfx_t, prfx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_prfx_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dblk_new
 *
 * Purpose:	Create a new local heap data block object
 *
 * Return:	Success:	non-NULL pointer to new local heap data block
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
H5HL_dblk_t *
H5HL_dblk_new(H5HL_t *heap)
{
    H5HL_dblk_t *dblk = NULL;       /* New local heap data block */
    H5HL_dblk_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI(H5HL_dblk_new, NULL)

    /* check arguments */
    HDassert(heap);

    /* Allocate new local heap data block */
    if(NULL == (dblk = H5FL_CALLOC(H5HL_dblk_t)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* Increment ref. count on heap data structure */
    if(H5HL_inc_rc(heap) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment heap ref. count")

    /* Link the heap & the data block */
    dblk->heap = heap;
    heap->dblk = dblk;

    /* Set the return value */
    ret_value = dblk;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_dblk_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HL_dblk_dest
 *
 * Purpose:	Destroy a local heap data block object
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HL_dblk_dest(H5HL_dblk_t *dblk)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(H5HL_dblk_dest, FAIL)

    /* check arguments */
    HDassert(dblk);

    /* Check if data block was initialized */
    if(dblk->heap) {
        /* Unlink data block from heap */
        dblk->heap->dblk = NULL;

        /* Unpin the local heap prefix */
        if(H5AC2_unpin_entry(dblk->heap->prfx) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPIN, FAIL, "can't unpin local heap prefix")

        /* Decrement ref. count on heap data structure */
        if(H5HL_dec_rc(dblk->heap) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement heap ref. count")

        /* Unlink heap from data block */
        dblk->heap = NULL;
    } /* end if */

    /* Free local heap data block */
    H5FL_FREE(H5HL_dblk_t, dblk);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HL_dblk_dest() */

