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
 * Created:     H5HLint.c
 *              Oct 12 2008
 *              Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:     Local heap internal routines.
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
#include "H5FLprivate.h"    /* Free lists                   */
#include "H5HLpkg.h"        /* Local Heaps                  */


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

/* Declare a free list to manage the H5HL_t struct */
H5FL_DEFINE_STATIC(H5HL_t);



/*-------------------------------------------------------------------------
 * Function:    H5HL__new
 *
 * Purpose:     Create a new local heap object
 *
 * Return:      Success:    non-NULL pointer to new local heap
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Jan  5 2010
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
H5HL_t *, NULL, NULL,
H5HL__new(size_t sizeof_size, size_t sizeof_addr, size_t prfx_size))

    H5HL_t *heap = NULL;        /* New local heap */

    /* check arguments */
    HDassert(sizeof_size > 0);
    HDassert(sizeof_addr > 0);
    HDassert(prfx_size > 0);

    /* Allocate new local heap structure */
    if(NULL == (heap = H5FL_CALLOC(H5HL_t)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed");

    /* Initialize non-zero fields */
    heap->sizeof_size = sizeof_size;
    heap->sizeof_addr = sizeof_addr;
    heap->prfx_size = prfx_size;

    /* Set the return value */
    ret_value = heap;

CATCH
    if(!ret_value && heap != NULL)
        if (NULL == (heap = H5FL_FREE(H5HL_t, heap)))
            H5E_THROW(H5E_CANTFREE, "can't free heap memory");

END_FUNC(PKG) /* end H5HL__new() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__inc_rc
 *
 * Purpose:     Increment ref. count on heap
 *
 * Return:      SUCCEED (Can't fail)
 *
 * Programmer:  Quincey Koziol
 *              Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, NOERR,
herr_t, SUCCEED, -,
H5HL__inc_rc(H5HL_t *heap))

    /* check arguments */
    HDassert(heap);

    /* Increment heap's ref. count */
    heap->rc++;

END_FUNC(PKG) /* end H5HL__inc_rc() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__dec_rc
 *
 * Purpose:     Decrement ref. count on heap
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Oct 12 2008
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5HL__dec_rc(H5HL_t *heap))

    /* check arguments */
    HDassert(heap);

    /* Decrement heap's ref. count */
    heap->rc--;

CATCH
    /* Check if we should destroy the heap */
    if(heap->rc == 0 && FAIL == H5HL__dest(heap))
        H5E_THROW(H5E_CANTFREE, "unable to destroy local heap");

END_FUNC(PKG) /* end H5HL__dec_rc() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__dest
 *
 * Purpose:     Destroys a heap in memory.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Jan 15 2003
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5HL__dest(H5HL_t *heap))

    /* check arguments */
    HDassert(heap);

    /* Verify that node is unused */
    HDassert(heap->prots == 0);
    HDassert(heap->rc == 0);
    HDassert(heap->prfx == NULL);
    HDassert(heap->dblk == NULL);

CATCH
    if(heap->dblk_image)
        if(NULL != (heap->dblk_image = H5FL_BLK_FREE(lheap_chunk, heap->dblk_image)))
            H5E_THROW(H5E_CANTFREE, "unable to free local heap data block image");
    while(heap->freelist) {
        H5HL_free_t	*fl;

        fl = heap->freelist;
        heap->freelist = fl->next;
        if(NULL != (fl = H5FL_FREE(H5HL_free_t, fl)))
            H5E_THROW(H5E_CANTFREE, "unable to free local heap free list");
    } /* end while */
    
    if(NULL != (heap = H5FL_FREE(H5HL_t, heap)))
        H5E_THROW(H5E_CANTFREE, "unable to free local heap");

END_FUNC(PKG) /* end H5HL__dest() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__create_flush_depend
 *
 * Purpose:     Create a flush dependency between two data structure components
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2011
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5HL__create_flush_depend(H5AC_info_t *parent_entry, H5AC_info_t *child_entry))

    /* Sanity check */
    HDassert(parent_entry);
    HDassert(child_entry);

    /* Create a flush dependency between parent and child entry */
    if(FAIL == H5AC_create_flush_dependency(parent_entry, child_entry))
        H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency");

CATCH
    /* No special processing on errors */

END_FUNC(PKG) /* end H5HL__create_flush_depend() */


/*-------------------------------------------------------------------------
 * Function:    H5HL__destroy_flush_depend
 *
 * Purpose:     Destroy a flush dependency between two data structure components
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2011
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(PKG, ERR,
herr_t, SUCCEED, FAIL,
H5HL__destroy_flush_depend(H5AC_info_t *parent_entry, H5AC_info_t *child_entry))

    /* Sanity check */
    HDassert(parent_entry);
    HDassert(child_entry);

    /* Destroy a flush dependency between parent and child entry */
    if(FAIL == H5AC_destroy_flush_dependency(parent_entry, child_entry))
        H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency");

CATCH
    /* No special processing on errors */

END_FUNC(PKG) /* end H5HL__destroy_flush_depend() */
