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

/* Free space section routines */
static herr_t H5HF_hdr_skip_ranges(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_indirect_t *iblock, unsigned start_entry, unsigned nentries);


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
    hdr->heap_len_size = MIN(hdr->man_dtable.max_dir_blk_off_size,
            ((H5V_log2_gen((hsize_t)hdr->standalone_size) + 7) / 8));
    hdr->id_len = hdr->heap_off_size + hdr->heap_len_size;

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
    HDassert(hdr->rc);

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
    if(H5AC_mark_pinned_or_protected_entry_dirty(hdr->f, hdr) < 0)
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
 * Function:	H5HF_hdr_adjust_heap
 *
 * Purpose:	Adjust heap space
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
H5HF_hdr_adjust_heap(H5HF_hdr_t *hdr, hsize_t new_size, hssize_t extra_free)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_adjust_heap)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Set the total space in heap */
    hdr->total_size = new_size;
    hdr->man_size = new_size;

    /* Adjust the free space in direct blocks */
    hdr->total_man_free += extra_free;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_adjust_heap() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_inc_alloc
 *
 * Purpose:	Increase allocated size of heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 23 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_inc_alloc(H5HF_hdr_t *hdr, size_t alloc_size)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_inc_alloc)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(alloc_size);

    /* Update the "allocated" size within the heap */
    hdr->man_alloc_size += alloc_size;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_alloc_size = %Hu\n", "H5HF_hdr_inc_alloc", hdr->man_alloc_size);
#endif /* QAK */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_hdr_inc_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_start_iter
 *
 * Purpose:	Start "next block" iterator at an offset/entry in the heap
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 30 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_start_iter(H5HF_hdr_t *hdr, H5HF_indirect_t *iblock, hsize_t curr_off, unsigned curr_entry)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_start_iter)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);

    /* Set up "next block" iterator at correct location */
    if(H5HF_man_iter_start_entry(hdr, &hdr->next_block, iblock, curr_entry) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize block iterator")

    /* Set the offset of the iterator in the heap */
    hdr->man_iter_off = curr_off;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_start_iter() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_reset_iter
 *
 * Purpose:	Reset "next block" iterator
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_reset_iter(H5HF_hdr_t *hdr, hsize_t curr_off)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_reset_iter)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Reset "next block" iterator */
    if(H5HF_man_iter_reset(&hdr->next_block) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't reset block iterator")

    /* Set the offset of the iterator in the heap */
    hdr->man_iter_off = curr_off;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_reset_iter() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_skip_blocks
 *
 * Purpose:	Add skipped direct blocks to free space for heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr  3 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_skip_blocks(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_indirect_t *iblock,
    unsigned start_entry, unsigned nentries)
{
    hsize_t sect_off;                   /* Offset of free section in heap */
    unsigned curr_row;                  /* Current row in indirect block */
    unsigned curr_col;                  /* Current column in indirect block */
    unsigned u;                         /* Local index variables */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_skip_blocks)
#ifdef QAK
HDfprintf(stderr, "%s: start_entry = %u, nentries = %u\n", FUNC, start_entry, nentries);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(nentries);

    /* Compute starting column & row */
    curr_row = start_entry / hdr->man_dtable.cparam.width;
    curr_col = start_entry % hdr->man_dtable.cparam.width;

    /* Initialize information for rows skipped over */
    sect_off = iblock->block_off;
    for(u = 0; u < curr_row; u++)
        sect_off += hdr->man_dtable.row_block_size[u] * hdr->man_dtable.cparam.width;
    sect_off += hdr->man_dtable.row_block_size[curr_row] * curr_col;
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Zu\n", FUNC, sect_off);
#endif /* QAK */

    /* Loop over the blocks to skip */
    for(u = start_entry; u < (start_entry + nentries); /* u is advanced in loop */) {
        unsigned row_entries;           /* Number of entries in a particular row */

        /* Compute number of entries in (possible partial) current row */
        row_entries = MIN(hdr->man_dtable.cparam.width - curr_col, (start_entry + nentries) - u);
#ifdef QAK
HDfprintf(stderr, "%s: u = %u\n", FUNC, u);
HDfprintf(stderr, "%s: curr_col = %u, curr_row = %u\n", FUNC, curr_col, curr_row);
HDfprintf(stderr, "%s: row_entries = %u, hdr->man_dtable.row_dblock_free[%u] = %Hu\n", FUNC, row_entries, curr_row, hdr->man_dtable.row_dblock_free[curr_row]);
#endif /* QAK */

        /* Add 'range' section for blocks skipped in this row */
        if(H5HF_sect_range_add(hdr, dxpl_id, sect_off, hdr->man_dtable.row_dblock_free[curr_row],
                iblock, curr_row, curr_col, row_entries) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create range section for indirect block's free space")

        /* Advance row & column position */
        sect_off += row_entries * hdr->man_dtable.row_block_size[curr_row];
        curr_row++;
        curr_col = 0;           /* (first partial row aligns this) */

        /* Increment index variable */
        u += row_entries;
    } /* end for */
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Zu\n", FUNC, sect_off);
#endif /* QAK */

    /* Advance the new block iterator */
    if(H5HF_hdr_inc_iter(hdr, (sect_off - hdr->man_iter_off), nentries) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_skip_blocks() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_skip_ranges
 *
 * Purpose:	Add skipped indirect ranges to free space for heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr  4 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_hdr_skip_ranges(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_indirect_t *iblock,
    unsigned start_entry, unsigned nentries)
{
    hsize_t sect_off;                   /* Offset of free section in heap */
    size_t row_dblock_free_space;       /* Size of free space for row of direct blocks in a row */
    size_t acc_row_dblock_free_space;   /* Accumulated size of free space for row of direct blocks in a row */
    unsigned curr_row;                  /* Current row in indirect block */
    unsigned curr_col;                  /* Current column in indirect block */
    unsigned u, w;                      /* Local index variables */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_skip_ranges)
#ifdef QAK
HDfprintf(stderr, "%s: start_entry = %u, nentries = %u\n", FUNC, start_entry, nentries);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(nentries);

    /* Compute starting column & row */
    curr_row = start_entry / hdr->man_dtable.cparam.width;
    curr_col = start_entry % hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: curr_col = %u, curr_row = %u\n", FUNC, curr_col, curr_row);
#endif /* QAK */

    /* Initialize information for rows skipped over */
    sect_off = iblock->block_off;
    for(u = 0; u < curr_row; u++)
        sect_off += hdr->man_dtable.row_block_size[u] * hdr->man_dtable.cparam.width;
    sect_off += hdr->man_dtable.row_block_size[curr_row] * curr_col;
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Zu\n", FUNC, sect_off);
#endif /* QAK */

    /* Loop over the blocks to skip */
    for(u = start_entry; u < (start_entry + nentries); /* u is advanced in loop */) {
        unsigned row_entries;       /* Number of entries in a particular row */
        unsigned num_rows;          /* Number of rows in indirect blocks referenced */

        /* Compute number of rows in indirect blocks covered by entry */
        num_rows = (H5V_log2_of2((uint32_t)hdr->man_dtable.row_block_size[curr_row]) -
                H5V_log2_of2(hdr->man_dtable.cparam.start_block_size)) - 1;

        /* Compute number of entries in (possible partial) current row */
        row_entries = MIN(hdr->man_dtable.cparam.width - curr_col, (start_entry + nentries) - u);
#ifdef QAK
HDfprintf(stderr, "%s: u = %u\n", FUNC, u);
HDfprintf(stderr, "%s: curr_col = %u, curr_row = %u\n", FUNC, curr_col, curr_row);
HDfprintf(stderr, "%s: row_entries = %u, num_rows = %u\n", FUNC, row_entries, num_rows);
#endif /* QAK */

        /* Loop over rows in indirect blocks covered */
        acc_row_dblock_free_space = 0;
        for(w = 0; w < num_rows; w++) {

            /* Compute free space in direct blocks for this row */
            row_dblock_free_space = hdr->man_dtable.cparam.width * hdr->man_dtable.row_dblock_free[w];
            acc_row_dblock_free_space += row_dblock_free_space;
#ifdef QAK
HDfprintf(stderr, "%s: w = %u\n", FUNC, w);
HDfprintf(stderr, "%s: hdr->man_dtable.row_dblock_free[%u] = %Zu\n", FUNC, w, hdr->man_dtable.row_dblock_free[w]);
#endif /* QAK */

            /* Add "indirect" free space section for blocks in this row */

            /* Create free list section node for blocks skipped over */
            if(H5HF_sect_indirect_add(hdr, dxpl_id, (sect_off + hdr->man_dtable.row_block_off[w]),
                    hdr->man_dtable.row_dblock_free[w], iblock, curr_row, curr_col, row_entries, w, num_rows) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create indirect section for indirect block's free space")
        } /* end for */
#ifdef QAK
HDfprintf(stderr, "%s: acc_row_dblock_free_space = %Zu\n", FUNC, acc_row_dblock_free_space);
#endif /* QAK */

        /* Advance row & column position */
        sect_off += row_entries * hdr->man_dtable.row_block_size[curr_row];
        curr_row++;
        curr_col = 0;           /* (first partial row aligns this) */

        /* Advance outer loop index */
        u += row_entries;
    } /* end for */
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Zu\n", FUNC, sect_off);
#endif /* QAK */

    /* Advance the new block iterator */
    if(H5HF_hdr_inc_iter(hdr, (sect_off - hdr->man_iter_off), nentries) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_skip_ranges() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_update_iter
 *
 * Purpose:	Update state of heap to account for current iterator
 *              position.
 *
 * Note:	Creates necessary indirect blocks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_update_iter(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t min_dblock_size)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_update_iter)
#ifdef QAK
HDfprintf(stderr, "%s: min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(min_dblock_size > 0);

    /* Check for creating first indirect block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        if(H5HF_man_iblock_root_create(hdr, dxpl_id, min_dblock_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "unable to create root indirect block")
    } /* end if */
    else {
        H5HF_indirect_t *iblock;        /* Pointer to indirect block */
        hbool_t walked_up, walked_down; /* Condition variables for finding direct block location */
        unsigned next_row;              /* Iterator's next block row */
        unsigned next_entry;            /* Iterator's next block entry */
        unsigned min_dblock_row;        /* Minimum row for direct block size request */

#ifdef QAK
HDfprintf(stderr, "%s: searching root indirect block\n", FUNC);
#endif /* QAK */
        /* Compute min. row for direct block requested */
        min_dblock_row = H5HF_dtable_size_to_row(&hdr->man_dtable, min_dblock_size);
#ifdef QAK
HDfprintf(stderr, "%s: min_dblock_size = %Zu, min_dblock_row = %u\n", FUNC, min_dblock_size, min_dblock_row);
#endif /* QAK */

        /* Initialize block iterator, if necessary */
        if(!H5HF_man_iter_ready(&hdr->next_block)) {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
#endif /* QAK */
            /* Start iterator with previous offset of iterator */
            if(H5HF_man_iter_start_offset(hdr, dxpl_id, &hdr->next_block, hdr->man_iter_off) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "unable to set block iterator location")
        } /* end if */

        /* Get information about current iterator location */
        if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")

#ifdef QAK
HDfprintf(stderr, "%s: Check 1.0\n", FUNC);
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: next_row = %u\n", FUNC, next_row);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */
        /* Check for skipping over blocks in the current block */
        if(min_dblock_row > next_row && next_row < iblock->nrows) {
            unsigned min_entry;         /* Min entry for direct block requested */
            unsigned skip_entries;      /* Number of entries to skip in the current block */

            /* Compute the number of entries to skip in the current block */
            min_entry = min_dblock_row * hdr->man_dtable.cparam.width;
            if(min_dblock_row >= iblock->nrows)
                skip_entries = (iblock->nrows * hdr->man_dtable.cparam.width) - next_entry;
            else
                skip_entries = min_entry - next_entry;
#ifdef QAK
HDfprintf(stderr, "%s: min_entry = %u, skip_entries = %u\n", FUNC, min_entry, skip_entries);
#endif /* QAK */

            /* Add skipped direct blocks to heap's free space */
            if(H5HF_hdr_skip_blocks(hdr, dxpl_id, iblock, next_entry, skip_entries) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't add skipped blocks to heap's free space")

            /* Get information about new iterator location */
            if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")
        } /* end if */

        do {
            /* Reset conditions for leaving loop */
            walked_up = walked_down = FALSE;

#ifdef QAK
HDfprintf(stderr, "%s: Check 2.0\n", FUNC);
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: next_row = %u\n", FUNC, next_row);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */

            /* Check for walking off end of indirect block */
            /* (walk up iterator) */
            while(next_row >= iblock->nrows) {
#ifdef QAK
HDfprintf(stderr, "%s: Off the end of a block\n", FUNC);
#endif /* QAK */
                /* Check for needing to expand root indirect block */
                if(iblock->parent == NULL) {
#ifdef QAK
HDfprintf(stderr, "%s: Doubling root block\n", FUNC);
#endif /* QAK */
                    if(H5HF_man_iblock_root_double(hdr, dxpl_id, min_dblock_size) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "unable to double root indirect block")
                } /* end if */
                else {
#ifdef QAK
HDfprintf(stderr, "%s: Walking up a level\n", FUNC);
#endif /* QAK */
                    /* Move iterator up one level */
                    if(H5HF_man_iter_up(&hdr->next_block) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

                    /* Increment location of next block at this level */
                    if(H5HF_man_iter_next(hdr, &hdr->next_block, 1) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")
                } /* end else */

                /* Get information about new iterator location */
                if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")

                /* Indicate that we walked up */
                walked_up = TRUE;
            } /* end while */
#ifdef QAK
HDfprintf(stderr, "%s: Check 3.0\n", FUNC);
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: next_row = %u\n", FUNC, next_row);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */

            /* Check for walking into child indirect block */
            /* (walk down iterator) */
            if(next_row >= hdr->man_dtable.max_direct_rows) {
                unsigned child_nrows;   /* Number of rows in new indirect block */

#ifdef QAK
HDfprintf(stderr, "%s: Walking down into child indirect block\n", FUNC);
#endif /* QAK */
#ifdef QAK
HDfprintf(stderr, "%s: Check 3.1\n", FUNC);
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: next_row = %u\n", FUNC, next_row);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */
                HDassert(!H5F_addr_defined(iblock->ents[next_entry].addr));

                /* Compute # of rows in next child indirect block to use */
                child_nrows = H5HF_dtable_size_to_rows(&hdr->man_dtable, hdr->man_dtable.row_block_size[next_row]);
#ifdef QAK
HDfprintf(stderr, "%s: child_nrows = %u\n", FUNC, child_nrows);
#endif /* QAK */

                /* Check for skipping over indirect blocks */
                /* (that don't have direct blocks large enough to hold direct block size requested) */
                if(hdr->man_dtable.row_block_size[child_nrows - 1] < min_dblock_size) {
                    unsigned child_rows_needed;     /* Number of rows needed to hold direct block */
                    unsigned child_entry;           /* Entry of child indirect block */

#ifdef QAK
HDfprintf(stderr, "%s: Skipping indirect block row that is too small\n", FUNC);
#endif /* QAK */
                    /* Compute # of rows needed in child indirect block */
                    child_rows_needed = (H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size)) + 2;
                    HDassert(child_rows_needed > child_nrows);
                    child_entry = (next_row + (child_rows_needed - child_nrows)) * hdr->man_dtable.cparam.width;
                    if(child_entry > (iblock->nrows * hdr->man_dtable.cparam.width))
                        child_entry = iblock->nrows * hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: child_rows_needed = %u\n", FUNC, child_rows_needed);
HDfprintf(stderr, "%s: child_entry = %u\n", FUNC, child_entry);
#endif /* QAK */

                    /* Add skipped indirect ranges to heap's free space */
                    if(H5HF_hdr_skip_ranges(hdr, dxpl_id, iblock, next_entry, (child_entry - next_entry)) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't add skipped blocks to heap's free space")
                } /* end if */
                else {
                    H5HF_indirect_t *new_iblock;    /* Pointer to new indirect block */
                    haddr_t new_iblock_addr;        /* New indirect block's address */

#ifdef QAK
HDfprintf(stderr, "%s: Allocating new child indirect block\n", FUNC);
#endif /* QAK */
                    /* Allocate new indirect block */
                    if(H5HF_man_iblock_create(hdr, dxpl_id, iblock, next_entry, child_nrows, child_nrows, &new_iblock_addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap indirect block")

                    /* Lock new indirect block */
                    if(NULL == (new_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, new_iblock_addr, child_nrows, iblock, next_entry, H5AC_WRITE)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

                    /* Move iterator down one level */
                    if(H5HF_man_iter_down(&hdr->next_block, new_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

                    /* Check for skipping over rows and add free section for skipped rows */
                    if(min_dblock_size > hdr->man_dtable.cparam.start_block_size) {
                        unsigned new_entry;        /* Entry of direct block which is large enough */

                        /* Compute entry for direct block size requested */
                        new_entry = hdr->man_dtable.cparam.width * min_dblock_row;
#ifdef QAK
HDfprintf(stderr, "%s: Skipping rows in new child indirect block - new_entry = %u\n", FUNC, new_entry);
#endif /* QAK */

                        /* Add skipped blocks to heap's free space */
                        if(H5HF_hdr_skip_blocks(hdr, dxpl_id, new_iblock, 0, new_entry) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't add skipped blocks to heap's free space")
                    } /* end if */

                    /* Unprotect child indirect block */
                    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock->addr, new_iblock, H5AC__NO_FLAGS_SET) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
                } /* end else */

                /* Get information about new iterator location */
                if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")

                /* Indicate that we walked down */
                walked_down = TRUE;
            } /* end if */
        } while(walked_down || walked_up);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_update_iter() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_inc_iter
 *
 * Purpose:	Advance "next block" iterator
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 23 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_inc_iter(H5HF_hdr_t *hdr, hsize_t adv_size, unsigned nentries)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_inc_iter)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(nentries);
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu, adv_size = %Hu\n", FUNC, hdr->man_iter_off, adv_size);
#endif /* QAK */

    /* Advance the iterator for the current location within the indirect block */
    if(hdr->next_block.curr)
        if(H5HF_man_iter_next(hdr, &hdr->next_block, nentries) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

    /* Increment the offset of the iterator in the heap */
    hdr->man_iter_off += adv_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_inc_iter() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_reverse_iter
 *
 * Purpose:	Walk "next block" iterator backwards until the correct
 *              location to allocate next block from is found
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_reverse_iter(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    H5HF_indirect_t *iblock;            /* Indirect block where iterator is located */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_reverse_iter)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Initialize block iterator, if necessary */
    if(!H5HF_man_iter_ready(&hdr->next_block))
        /* Start iterator with previous offset of iterator */
        if(H5HF_man_iter_start_offset(hdr, dxpl_id, &hdr->next_block, hdr->man_iter_off) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "unable to set block iterator location")

    /* Get information about current iterator location */
    if(H5HF_man_iter_curr(&hdr->next_block, NULL, NULL, NULL, &iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator information")
#ifdef QAK
HDfprintf(stderr, "%s: iblock->nchildren = %u\n", FUNC, iblock->nchildren);
HDfprintf(stderr, "%s: iblock->parent = %p\n", FUNC, iblock->parent);
#endif /* QAK */

    /* Walk backwards through heap, looking for direct block to place iterator after */

    /* Walk up the levels in the heap until we find an indirect block with children */
    while(iblock->nchildren == 0 && iblock->parent) {
        /* Move iterator to parent of current block */
        if(H5HF_man_iter_up(&hdr->next_block) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

        /* Detach from parent indirect block */
        if(H5HF_man_iblock_detach(iblock->parent, dxpl_id, iblock->par_entry) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't detach from parent indirect block")
        iblock->parent = NULL;
        iblock->par_entry = 0;
    } /* end while */

    /* If there's children in this indirect block, walk backwards to find last child */
    if(iblock->nchildren) {
        unsigned curr_entry;    /* Current entry for iterator */
        unsigned row;           /* Row for entry */
        hbool_t walked_down;    /* Loop flag */

        /* Get information about current iterator location */
        if(H5HF_man_iter_curr(&hdr->next_block, NULL, NULL, &curr_entry, NULL) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator information")
#ifdef QAK
HDfprintf(stderr, "%s: curr_entry = %u\n", FUNC, curr_entry);
#endif /* QAK */

        /* Move current iterator position backwards once */
        curr_entry--;

        /* Search backwards for direct block to latch onto */
        do {
            int tmp_entry;     /* Temp. entry for iterator (use signed value to detect errors) */

            /* Reset loop flag */
            walked_down = FALSE;

            /* Walk backwards through entries, until we find one that has a child */
            /* (Account for root indirect block shrinking) */
            tmp_entry = MIN(curr_entry, ((iblock->nrows * hdr->man_dtable.cparam.width) - 1));
#ifdef QAK
HDfprintf(stderr, "%s: tmp_entry = %d\n", FUNC, tmp_entry);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
#endif /* QAK */
            while(!H5F_addr_defined(iblock->ents[tmp_entry].addr)) {
                tmp_entry--;
                HDassert(tmp_entry >= 0);
            } /* end while */
#ifdef QAK
HDfprintf(stderr, "%s: check 2.0 - tmp_entry = %d\n", FUNC, tmp_entry);
#endif /* QAK */
            curr_entry = tmp_entry;

            /* Check if entry is for a direct block */
            row = curr_entry / hdr->man_dtable.cparam.width;
            if(row < hdr->man_dtable.max_direct_rows) {
                /* Increment entry to empty location */
                curr_entry++;

                /* Set the current location of the iterator to next entry after the existing direct block */
                if(H5HF_man_iter_set_entry(hdr, &hdr->next_block, curr_entry) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTSET, FAIL, "unable to set current block iterator location")

                /* Update iterator offset */
                hdr->man_iter_off = iblock->block_off;
                hdr->man_iter_off += hdr->man_dtable.row_block_off[curr_entry / hdr->man_dtable.cparam.width];
                hdr->man_iter_off += hdr->man_dtable.row_block_size[curr_entry / hdr->man_dtable.cparam.width] * (curr_entry % hdr->man_dtable.cparam.width);
            } /* end if */
            else {
                H5HF_indirect_t *child_iblock;      /* Pointer to child indirect block */
                unsigned child_nrows;               /* # of rows in child block */

                /* Compute # of rows in next child indirect block to use */
                child_nrows = H5HF_dtable_size_to_rows(&hdr->man_dtable, hdr->man_dtable.row_block_size[row]);

                /* Lock child indirect block */
                if(NULL == (child_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, iblock->ents[curr_entry].addr, child_nrows, iblock, curr_entry, H5AC_WRITE)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

                /* Set the current location of the iterator */
                if(H5HF_man_iter_set_entry(hdr, &hdr->next_block, curr_entry) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTSET, FAIL, "unable to set current block iterator location")

                /* Walk down into child indirect block (pins child block) */
                if(H5HF_man_iter_down(&hdr->next_block, child_iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, FAIL, "unable to advance current block iterator location")

                /* Update iterator location */
                iblock = child_iblock;
                curr_entry = (child_iblock->nrows * hdr->man_dtable.cparam.width) - 1;

                /* Unprotect child indirect block */
                if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock->addr, child_iblock, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

                /* Note that we walked down */
                walked_down = TRUE;
            } /* end else */
        } while(walked_down);
    } /* end if */
    else {
        /* Reset header information back to "empty heap" state */
        if(H5HF_hdr_empty(hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't make heap empty")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_reverse_iter() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_empty
 *
 * Purpose:	Reset heap header to 'empty heap' state
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_empty(H5HF_hdr_t *hdr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_empty)
#ifdef QAK
HDfprintf(stderr, "%s: Reseting heap header to empty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

    /* Shrink heap size */
    hdr->total_size = hdr->std_size;
    hdr->man_size = 0;
    hdr->man_alloc_size = 0;

    /* Reset the free space in direct blocks */
    hdr->total_man_free = 0;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_empty() */

