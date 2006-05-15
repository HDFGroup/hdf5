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
 * Created:		H5HFhdr.c
 *			Apr 10 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Heap header routines for fractal heaps.
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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Limit on the size of the max. direct block size */
/* (This is limited to 32-bits currently, because I think it's unlikely to
 *      need to be larger, the 32-bit limit for H5V_log2_of2(n), and
 *      some offsets/sizes are encoded with a maxiumum of 32-bits  - QAK)
 */
#define H5HF_MAX_DIRECT_SIZE_LIMIT ((hsize_t)2 * 1024 * 1024 * 1024)

/* Limit on the width of the doubling table */
/* (This is limited to 16-bits currently, because I think it's unlikely to
 *      need to be larger, and its encoded with a maxiumum of 16-bits  - QAK)
 */
#define H5HF_WIDTH_LIMIT (64 * 1024)

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

/* Declare a free list to manage the H5HF_hdr_t struct */
H5FL_DEFINE(H5HF_hdr_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_alloc
 *
 * Purpose:	Allocate shared fractal heap header 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_hdr_t *
H5HF_hdr_alloc(H5F_t *f)
{
    H5HF_hdr_t *hdr = NULL;          /* Shared fractal heap header */
    H5HF_hdr_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_alloc)

    /*
     * Check arguments.
     */
    HDassert(f);

    /* Allocate space for the shared information */
    if(NULL == (hdr = H5FL_CALLOC(H5HF_hdr_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fractal heap shared header")

    /* Set the internal parameters for the heap */
    hdr->f = f;
    hdr->sizeof_size = H5F_SIZEOF_SIZE(f);
    hdr->sizeof_addr = H5F_SIZEOF_ADDR(f);

    /* Set the return value */
    ret_value = hdr;

done:
    if(!ret_value)
        if(hdr)
            (void)H5HF_cache_hdr_dest(f, hdr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_free_space
 *
 * Purpose:	Compute direct block free space, for indirect blocks of
 *              different sizes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5HF_hdr_compute_free_space(H5HF_hdr_t *hdr, hsize_t iblock_size)
{
    hsize_t acc_heap_size;      /* Accumumated heap space */
    hsize_t acc_dblock_free;    /* Accumumated direct block free space */
    unsigned curr_row;          /* Current row in block */
    hsize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_compute_free_space)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock_size > hdr->man_dtable.cparam.max_direct_size);

    /* Set the free space in direct blocks */
    acc_heap_size = 0;
    acc_dblock_free = 0;
    curr_row = 0;
    while(acc_heap_size < iblock_size) {
        acc_heap_size += hdr->man_dtable.row_block_size[curr_row] *
                hdr->man_dtable.cparam.width;
        acc_dblock_free += hdr->man_dtable.row_dblock_free[curr_row] *
                hdr->man_dtable.cparam.width;
        curr_row++;
    } /* end while */

    /* Set return value */
    ret_value = acc_dblock_free;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_compute_free_space() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_finish_init
 *
 * Purpose:	Finish initializing info in shared heap header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_finish_init(H5HF_hdr_t *hdr)
{
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_finish_init)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Compute/cache some values */
    hdr->heap_off_size = H5HF_SIZEOF_OFFSET_BITS(hdr->man_dtable.cparam.max_index);
    if(H5HF_dtable_init(&hdr->man_dtable) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize doubling table info")

    /* Set the size of heap IDs */
    hdr->id_len = hdr->heap_off_size + MIN(hdr->man_dtable.max_dir_blk_off_size,
        ((H5V_log2_gen((hsize_t)hdr->standalone_size) + 7) / 8));

    /* Set the free space in direct blocks */
    for(u = 0; u < hdr->man_dtable.max_root_rows; u++) {
        if(u < hdr->man_dtable.max_direct_rows)
            hdr->man_dtable.row_dblock_free[u] = hdr->man_dtable.row_block_size[u] -
                    H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
        else
            hdr->man_dtable.row_dblock_free[u] = H5HF_hdr_compute_free_space(hdr, hdr->man_dtable.row_block_size[u]);
#ifdef QAK
HDfprintf(stderr, "%s: row_block_size[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_block_size[u]);
HDfprintf(stderr, "%s: row_block_off[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_block_off[u]);
HDfprintf(stderr, "%s: row_dblock_free[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_dblock_free[u]);
#endif /* QAK */
    } /* end for */

    /* Initialize the block iterator for searching for free space */
    if(H5HF_man_iter_init(&hdr->next_block) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize space search block iterator")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_finish_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_init
 *
 * Purpose:	Initialize shared fractal heap header for new heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_init(H5HF_hdr_t *hdr, haddr_t fh_addr, H5HF_create_t *cparam)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_init)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(cparam);

#ifndef NDEBUG
    /* Check for valid parameters */
    if(cparam->managed.width == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "width must be greater than zero")
    if(cparam->managed.width > H5HF_WIDTH_LIMIT)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "width too large")
    if(!POWER_OF_TWO(cparam->managed.width))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "width not power of two")
    if(cparam->managed.start_block_size == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "starting block size must be greater than zero")
    if(!POWER_OF_TWO(cparam->managed.start_block_size))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "starting block size not power of two")
    if(cparam->managed.max_direct_size == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size must be greater than zero")
    if(cparam->managed.max_direct_size > H5HF_MAX_DIRECT_SIZE_LIMIT)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size too large")
    if(!POWER_OF_TWO(cparam->managed.max_direct_size))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not power of two")
    if(cparam->managed.max_direct_size < cparam->standalone_size)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not large enough to hold all managed blocks")
    if(cparam->managed.max_index == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. heap size must be greater than zero")
    if(cparam->managed.max_index > (8 * hdr->sizeof_size))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. heap size too large for file")
#endif /* NDEBUG */

    /* Set the creation parameters for the heap */
    hdr->heap_addr = fh_addr;
    hdr->addrmap = cparam->addrmap;
    hdr->standalone_size = cparam->standalone_size;
    HDmemcpy(&(hdr->man_dtable.cparam), &(cparam->managed), sizeof(H5HF_dtable_cparam_t));

    /* Set root table address to indicate that the heap is empty currently */
    hdr->man_dtable.table_addr = HADDR_UNDEF;

    /* Set free list header address to indicate that the heap is empty currently */
    hdr->fs_addr = HADDR_UNDEF;

    /* Note that the shared info is dirty (it's not written to the file yet) */
    hdr->dirty = TRUE;

    /* Make shared heap info reference counted */
    if(H5HF_hdr_finish_init(hdr) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap header")

done:
    if(ret_value < 0)
        if(hdr)
            (void)H5HF_cache_hdr_dest(NULL, hdr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_incr
 *
 * Purpose:	Increment reference count on shared heap header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_incr(H5HF_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_incr)

    /* Sanity check */
    HDassert(hdr);

/* XXX: When "un-evictable" feature is finished, mark the header as
 *      unevictable on the first block to share it.  - QAK
 */
    /* Mark header as un-evictable when a block is depending on it */
    if(hdr->rc == 0)
        if(H5AC_pin_protected_entry(hdr->f, hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPIN, FAIL, "unable to pin fractal heap header")

    /* Increment reference count on shared header */
    hdr->rc++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_decr
 *
 * Purpose:	Decrement reference count on shared heap header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_decr(H5HF_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_decr)

    /* Sanity check */
    HDassert(hdr);

    /* Decrement reference count on shared header */
    hdr->rc--;

    /* Mark header as evictable again when no child blocks depend on it */
    if(hdr->rc == 0)
        if(H5AC_unpin_entry(hdr->f, hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPIN, FAIL, "unable to unpin fractal heap header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_dirty
 *
 * Purpose:	Mark heap header as dirty
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_dirty(H5HF_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_dirty)
#ifdef QAK
HDfprintf(stderr, "%s: Marking heap header as dirty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Mark header as dirty in cache */
    if(H5AC_mark_pinned_entry_dirty(hdr->f, hdr, FALSE, 0) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, FAIL, "unable to mark fractal heap header as dirty")

    /* Set the dirty flags for the heap header */
    hdr->dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_dirty() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_adj_free
 *
 * Purpose:	Adjust the free space for a heap
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
H5HF_hdr_adj_free(H5HF_hdr_t *hdr, ssize_t amt)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_adj_free)
#ifdef QAK
HDfprintf(stderr, "%s: amt = %Zd\n", FUNC, amt);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Update heap header */
    HDassert(amt > 0 || hdr->total_man_free >= (hsize_t)-amt);
    hdr->total_man_free += amt;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->total_man_free = %Hu\n", FUNC, hdr->total_man_free);
#endif /* QAK */

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_adj_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_extend_heap
 *
 * Purpose:	Extend heap to cover more space
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 10 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_extend_heap(H5HF_hdr_t *hdr, hsize_t new_size, hsize_t extra_free)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_extend_heap)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(new_size >= hdr->total_size);

    /* Set the total space in heap */
    hdr->total_size = new_size;
    hdr->man_size = new_size;

    /* Increment the free space in direct blocks */
    hdr->total_man_free += extra_free;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_hdr_extend_heap() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_inc_alloc
 *
 * Purpose:	Increase allocated size of heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_inc_alloc(H5HF_hdr_t *hdr, hsize_t new_alloc_size, unsigned nentries)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_inc_alloc)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(new_alloc_size >= hdr->man_alloc_size);

    /* Advance the iterator for the current location with in the indirect block */
    if(hdr->next_block.curr)
        if(H5HF_man_iter_next(hdr, &hdr->next_block, nentries) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

    /* Update the "allocated" size within the heap */
    hdr->man_alloc_size = new_alloc_size;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_alloc_size = %Hu\n", FUNC, hdr->man_alloc_size);
#endif /* QAK */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_inc_alloc() */

