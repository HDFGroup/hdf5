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
 * Created:		H5HFiblock.c
 *			Apr 10 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Indirect block routines for fractal heaps.
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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

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
static herr_t H5HF_man_iblock_root_halve(H5HF_indirect_t *root_iblock, hid_t dxpl_id);
static herr_t H5HF_man_iblock_root_revert(H5HF_indirect_t *root_iblock, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5HF_indirect_t struct */
H5FL_DEFINE(H5HF_indirect_t);

/* Declare a free list to manage the H5HF_indirect_ent_t sequence information */
H5FL_SEQ_DEFINE(H5HF_indirect_ent_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_incr
 *
 * Purpose:	Increment reference count on shared indirect block
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
H5HF_iblock_incr(H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_iblock_incr)

    /* Sanity check */
    HDassert(iblock);

    /* Mark block as un-evictable when a child block is depending on it */
    if(iblock->rc == 0)
        if(H5AC_pin_protected_entry(iblock->hdr->f, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPIN, FAIL, "unable to pin fractal heap indirect block")

    /* Increment reference count on shared indirect block */
    iblock->rc++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_iblock_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_decr
 *
 * Purpose:	Decrement reference count on shared indirect block
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
H5HF_iblock_decr(H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_iblock_decr)

    /* Sanity check */
    HDassert(iblock);

    /* Decrement reference count on shared indirect block */
    iblock->rc--;

    /* Mark block as evictable again when no child blocks depend on it */
    if(iblock->rc == 0) {
        H5HF_indirect_t *tmp_iblock;    /* Temporary pointer to indirect block */

#ifdef QAK
HDfprintf(stderr, "%s: indirect block ref. count at zero, iblock->addr = %a\n", FUNC, iblock->addr);
#endif /* QAK */
        /* Lock indirect block */
        if(iblock->nchildren == 0) {
            if(NULL == (tmp_iblock = H5HF_man_iblock_protect(iblock->hdr, H5AC_dxpl_id, iblock->addr, iblock->nrows, NULL, 0, H5AC_WRITE)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
            HDassert(tmp_iblock == iblock);
        } /* end if */

        if(H5AC_unpin_entry(iblock->hdr->f, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPIN, FAIL, "unable to unpin fractal heap indirect block")

/* XXX: If the indirect block has no children, delete indirect block's entry
 *      from cache.
 */
        if(iblock->nchildren == 0) {
            /* Check for deleting root indirect block (and no root direct block) */
            if(iblock->block_off == 0 && iblock->hdr->man_dtable.curr_root_rows > 0) {
                /* Reset root pointer information */
                iblock->hdr->man_dtable.curr_root_rows = 0;
                iblock->hdr->man_dtable.table_addr = HADDR_UNDEF;
            } /* end if */

            /* Unlock indirect block with delete flag */
            if(H5AC_unprotect(iblock->hdr->f, H5AC_dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, tmp_iblock, H5AC__DIRTIED_FLAG|H5AC__DELETED_FLAG) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
            tmp_iblock = NULL;
        } /* end if */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_iblock_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_dirty
 *
 * Purpose:	Mark indirect block as dirty
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
H5HF_iblock_dirty(H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_iblock_dirty)
#ifdef QAK
HDfprintf(stderr, "%s: Marking indirect block as dirty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);

    /* Mark indirect block as dirty in cache */
    if(H5AC_mark_pinned_or_protected_entry_dirty(iblock->hdr->f, iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, FAIL, "unable to mark fractal heap indirect block as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_iblock_dirty() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_root_create
 *
 * Purpose:	Create root indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  2 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_root_create(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t min_dblock_size)
{
    H5HF_indirect_t *iblock;        /* Pointer to indirect block */
    haddr_t iblock_addr;            /* Indirect block's address */
    hsize_t acc_dblock_free;        /* Accumulated free space in direct blocks */
    hbool_t have_direct_block;      /* Flag to indicate a direct block already exists */
    unsigned nrows;                 /* Number of rows for root indirect block */
    unsigned u;                     /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_root_create)

#ifdef QAK
HDfprintf(stderr, "%s: Creating root indirect block\n", FUNC);
#endif /* QAK */

    /* Check for allocating entire root indirect block initially */
    if(hdr->man_dtable.cparam.start_root_rows == 0)
        nrows = hdr->man_dtable.max_root_rows;
    else {
        unsigned rows_needed;   /* Number of rows needed to get to direct block size */
        unsigned block_row_off; /* Row offset from larger block sizes */

        nrows = hdr->man_dtable.cparam.start_root_rows;

        block_row_off = H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size);
        if(block_row_off > 0)
            block_row_off++;        /* Account for the pair of initial rows of the initial block size */
        rows_needed = 1 + block_row_off;
        if(nrows < rows_needed)
            nrows = rows_needed;
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u\n", FUNC, nrows);
#endif /* QAK */

    /* Allocate root indirect block */
    if(H5HF_man_iblock_create(hdr, dxpl_id, NULL, 0, nrows, hdr->man_dtable.max_root_rows, &iblock_addr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
#endif /* QAK */

    /* Move current direct block (used as root) into new indirect block */

    /* Lock new indirect block */
    if(NULL == (iblock = H5HF_man_iblock_protect(hdr, dxpl_id, iblock_addr, nrows, NULL, 0, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

    /* Check if there's already a direct block as root) */
    have_direct_block = H5F_addr_defined(hdr->man_dtable.table_addr);
#ifdef QAK
HDfprintf(stderr, "%s: have_direct_block = %u\n", FUNC, (unsigned)have_direct_block);
#endif /* QAK */
    if(have_direct_block) {
        H5HF_direct_t *dblock;          /* Pointer to direct block to query */

        /* Lock first (root) direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, hdr->man_dtable.table_addr, hdr->man_dtable.cparam.start_block_size, iblock, 0, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Attach direct block to new root indirect block */
        dblock->parent = iblock;
        dblock->par_entry = 0;
        if(H5HF_man_iblock_attach(iblock, 0, hdr->man_dtable.table_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't attach root direct block to parent indirect block")

        /* Unlock first (previously the root) direct block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
        dblock = NULL;
    } /* end if */

    /* Start iterator at correct location */
    if(H5HF_hdr_start_iter(hdr, iblock, (hsize_t)(have_direct_block ? hdr->man_dtable.cparam.start_block_size : 0), have_direct_block) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize block iterator")

    /* Check for skipping over direct blocks, in order to get to large enough block */
    if(min_dblock_size > hdr->man_dtable.cparam.start_block_size) {
        /* Add skipped blocks to heap's free space */
        if(H5HF_hdr_skip_blocks(hdr, dxpl_id, iblock, have_direct_block,
                ((nrows - 1) * hdr->man_dtable.cparam.width) - have_direct_block) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't add skipped blocks to heap's free space")
    } /* end if */

    /* Mark indirect block as modified */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

    /* Unprotect root indirect block (it's pinned by the iterator though) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    iblock = NULL;

    /* Point heap header at new indirect block */
    hdr->man_dtable.curr_root_rows = nrows;
    hdr->man_dtable.table_addr = iblock_addr;

    /* Compute free space in direct blocks referenced from entries in root indirect block */
    acc_dblock_free = 0;
    for(u = 0; u < nrows; u++)
        acc_dblock_free += hdr->man_dtable.row_dblock_free[u] * hdr->man_dtable.cparam.width;

    /* Account for potential initial direct block */
    if(have_direct_block)
        acc_dblock_free -= hdr->man_dtable.row_dblock_free[0];

    /* Extend heap to cover new root indirect block */
    if(H5HF_hdr_adjust_heap(hdr, hdr->man_dtable.row_block_off[nrows], (hssize_t)acc_dblock_free) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_root_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_root_double
 *
 * Purpose:	Double size of root indirect block
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
H5HF_man_iblock_root_double(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t min_dblock_size)
{
    H5HF_indirect_t *iblock;    /* Pointer to root indirect block */
    haddr_t new_addr;           /* New address of indirect block */
    hsize_t acc_dblock_free;    /* Accumulated free space in direct blocks */
    hsize_t next_size;          /* The previous value of the "next size" for the new block iterator */
    unsigned next_row;          /* The next row to allocate block in */
    unsigned next_entry;        /* The previous value of the "next entry" for the new block iterator */
    unsigned new_next_entry;    /* The new value of the "next entry" for the new block iterator */
    unsigned min_nrows = 0;     /* Min. # of direct rows */
    unsigned old_nrows;         /* Old # of rows */
    unsigned new_nrows;         /* New # of rows */
    hbool_t skip_direct_rows = FALSE;   /* Whether we are skipping direct rows */
    size_t u;                   /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_root_double)

#ifdef QAK
HDfprintf(stderr, "%s: Extending root indirect block\n", FUNC);
#endif /* QAK */

    /* Get "new block" iterator information */
    if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")
    next_size = hdr->man_dtable.row_block_size[next_row];

    /* Make certain the iterator is at the root indirect block */
    HDassert(iblock->parent == NULL);

    /* Keep this for later */
    old_nrows = iblock->nrows;

    /* Check for skipping over direct block rows */
    if(iblock->nrows < hdr->man_dtable.max_direct_rows && min_dblock_size > next_size) {
        /* Sanity check */
        HDassert(min_dblock_size > hdr->man_dtable.cparam.start_block_size);

        /* Set flag */
        skip_direct_rows = TRUE;

        /* Make certain we allocate at least the required row for the block requested */
        min_nrows = 1 + H5HF_dtable_size_to_row(&hdr->man_dtable, min_dblock_size);

        /* Set the information for the next block, of the appropriate size */
        new_next_entry = (min_nrows - 1) * hdr->man_dtable.cparam.width;
    } /* end if */

    /* Compute new # of rows in indirect block */
    new_nrows = MAX(min_nrows, MIN(2 * iblock->nrows, iblock->max_rows));
#ifdef QAK
HDfprintf(stderr, "%s: min_nrows = %u, new_nrows = %u\n", FUNC, min_nrows, new_nrows);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: new_next_entry = %u\n", FUNC, new_next_entry);
#endif /* QAK */

/* Currently, the old block data is "thrown away" after the space is reallocated,
* to avoid data copy in H5MF_realloc() call by just free'ing the space and
* allocating new space.
*
* This also keeps the file smaller, by freeing the space and then
* allocating new space, instead of vice versa (in H5MF_realloc).
*
* QAK - 3/14/2006
*/
    /* Free previous indirect block disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock->addr, (hsize_t)iblock->size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap indirect block file space")

    /* Compute size of buffer needed for new indirect block */
    iblock->nrows = new_nrows;
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

    /* Allocate space for the new indirect block on disk */
    if(HADDR_UNDEF == (new_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: new_addr = %a\n", FUNC, new_addr);
#endif /* QAK */

    /* Re-allocate direct block entry table */
    if(NULL == (iblock->ents = H5FL_SEQ_REALLOC(H5HF_indirect_ent_t, iblock->ents, (iblock->nrows * hdr->man_dtable.cparam.width))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct entries")

    /* Check for skipping over rows and add free section for skipped rows */
    if(skip_direct_rows) {
        /* Add skipped blocks to heap's free space */
        if(H5HF_hdr_skip_blocks(hdr, dxpl_id, iblock, next_entry,
                (new_next_entry - next_entry)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't add skipped blocks to heap's free space")
    } /* end if */

    /* Initialize new direct block entries in rows added */
    acc_dblock_free = 0;
    for(u = (old_nrows * hdr->man_dtable.cparam.width); u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        unsigned row = u / hdr->man_dtable.cparam.width;        /* Row for current entry */

        iblock->ents[u].addr = HADDR_UNDEF;
        acc_dblock_free += hdr->man_dtable.row_dblock_free[row];
    } /* end for */

    /* Mark indirect block as dirty */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

    /* Move object in cache, if it actually was relocated */
    if(H5F_addr_ne(iblock->addr, new_addr)) {
        if(H5AC_rename(hdr->f, H5AC_FHEAP_IBLOCK, iblock->addr, new_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, FAIL, "unable to move fractal heap root indirect block")
        iblock->addr = new_addr;
    } /* end if */

    /* Update other shared header info */
    hdr->man_dtable.curr_root_rows = new_nrows;
    hdr->man_dtable.table_addr = new_addr;

    /* Extend heap to cover new root indirect block */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows - 1] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows - 1]);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows]);
HDfprintf(stderr, "%s: acc_dblock_free = %Hu\n", FUNC, acc_dblock_free);
#endif /* QAK */
    if(H5HF_hdr_adjust_heap(hdr, 2 * hdr->man_dtable.row_block_off[new_nrows - 1], (hssize_t)acc_dblock_free) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_root_double() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_root_halve
 *
 * Purpose:	Halve size of root indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jun 12 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_root_halve(H5HF_indirect_t *iblock, hid_t dxpl_id)
{
    H5HF_hdr_t *hdr = iblock->hdr;      /* Pointer to heap header */
    haddr_t new_addr;                   /* New address of indirect block */
    hsize_t acc_dblock_free;            /* Accumulated free space in direct blocks */
    unsigned max_child_row;             /* Row for max. child entry */
    unsigned new_nrows;                 /* New # of rows */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_root_halve)

    /* Sanity check */
    HDassert(iblock);
    HDassert(iblock->parent == NULL);
    HDassert(hdr);

#ifdef QAK
HDfprintf(stderr, "%s: Reducing root indirect block\n", FUNC);
#endif /* QAK */

    /* Compute maximum row used by child of indirect block */
    max_child_row = iblock->max_child / hdr->man_dtable.cparam.width;

    /* Compute new # of rows in root indirect block */
    new_nrows = 1 << (1 + H5V_log2_gen((hsize_t)max_child_row));
#ifdef QAK
HDfprintf(stderr, "%s: new_nrows = %u\n", FUNC, new_nrows);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
#endif /* QAK */

/* Currently, the old block data is "thrown away" after the space is reallocated,
* to avoid data copy in H5MF_realloc() call by just free'ing the space and
* allocating new space.
*
* This also keeps the file smaller, by freeing the space and then
* allocating new space, instead of vice versa (in H5MF_realloc).
*
* QAK - 6/12/2006
*/
    /* Free previous indirect block disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock->addr, (hsize_t)iblock->size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap indirect block file space")

    /* Compute free space in rows to delete */
    acc_dblock_free = 0;
    for(u = new_nrows; u < iblock->nrows; u++)
        acc_dblock_free += hdr->man_dtable.row_dblock_free[u] * hdr->man_dtable.cparam.width;

    /* Compute size of buffer needed for new indirect block */
    iblock->nrows = new_nrows;
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

    /* Allocate space for the new indirect block on disk */
    if(HADDR_UNDEF == (new_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: new_addr = %a\n", FUNC, new_addr);
#endif /* QAK */

    /* Re-allocate direct block entry table */
    if(NULL == (iblock->ents = H5FL_SEQ_REALLOC(H5HF_indirect_ent_t, iblock->ents, (iblock->nrows * hdr->man_dtable.cparam.width))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct entries")

    /* Move object in cache, if it actually was relocated */
    if(H5F_addr_ne(iblock->addr, new_addr)) {
        if(H5AC_rename(hdr->f, H5AC_FHEAP_IBLOCK, iblock->addr, new_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, FAIL, "unable to move fractal heap root indirect block")
        iblock->addr = new_addr;
    } /* end if */

    /* Mark indirect block as dirty */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

    /* Update other shared header info */
    hdr->man_dtable.curr_root_rows = new_nrows;
    hdr->man_dtable.table_addr = new_addr;

    /* Shrink heap to only cover new root indirect block */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows - 1] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows - 1]);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows]);
HDfprintf(stderr, "%s: acc_dblock_free = %Hu\n", FUNC, acc_dblock_free);
#endif /* QAK */
    if(H5HF_hdr_adjust_heap(hdr, 2 * hdr->man_dtable.row_block_off[new_nrows - 1], -(hssize_t)acc_dblock_free) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce space to cover root direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_root_halve() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_root_revert
 *
 * Purpose:	Revert root indirect block back to root direct block
 *
 * Note:	Any sections left pointing to the  old root indirect block
 *              will be cleaned up by the free space manager
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_root_revert(H5HF_indirect_t *root_iblock, hid_t dxpl_id)
{
    H5HF_hdr_t *hdr;                    /* Pointer to heap's header */
    H5HF_direct_t *dblock = NULL;       /* Pointer to new root indirect block */
    haddr_t dblock_addr;                /* Direct block's address in the file */
    size_t dblock_size;                 /* Direct block's size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_root_revert)

    /*
     * Check arguments.
     */
    HDassert(root_iblock);

#ifdef QAK
HDfprintf(stderr, "%s: Reverting root indirect block\n", FUNC);
#endif /* QAK */

    /* Set up local convenience variables */
    hdr = root_iblock->hdr;
    dblock_addr = root_iblock->ents[0].addr;
    dblock_size = hdr->man_dtable.cparam.start_block_size;

    /* Get pointer to last direct block */
    if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, root_iblock, 0, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

    /* Detach direct block from parent */
    if(H5HF_man_iblock_detach(dblock->parent, dxpl_id, dblock->par_entry) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't detach direct block from parent indirect block")
    dblock->parent = NULL;
    dblock->par_entry = 0;

    /* Point root at direct block */
    hdr->man_dtable.curr_root_rows = 0;
    hdr->man_dtable.table_addr = dblock_addr;

    /* Reset 'next block' iterator */
    if(H5HF_hdr_reset_iter(hdr, (hsize_t)dblock_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't reset block iterator")

done:
    if(dblock && H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_root_revert() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_alloc_range
 *
 * Purpose:	Allocate a "single" section for an object, out of a "range"
 *              section
 *
 * Note:	Creates necessary direct & indirect blocks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 28 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_alloc_range(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node)
{
    H5HF_indirect_t *iblock = NULL;     /* Pointer to indirect block */
    haddr_t dblock_addr;                /* Direct block's address */
    H5HF_free_section_t *dblock_sec_node = NULL;     /* Pointer to direct block's section node */
    H5HF_free_section_t *old_sec_node = *sec_node;     /* Pointer to old section node */
    unsigned cur_entry;                 /* Current entry in indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_alloc_range)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sec_node && *sec_node);

    /* Check for serialized section */
    if(old_sec_node->sect_info.state == H5FS_SECT_SERIALIZED) {
        /* Revive range section */
        if(H5HF_sect_range_revive(hdr, dxpl_id, old_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREVIVE, FAIL, "can't revive range section")
    } /* end if */

    /* Compute info about range */
    cur_entry = (old_sec_node->u.range.row * hdr->man_dtable.cparam.width) + old_sec_node->u.range.col;

    /* Get a pointer to the indirect block covering the range */
    iblock = old_sec_node->u.range.iblock;
    HDassert(iblock);

#ifdef QAK
HDfprintf(stderr, "%s: cur_entry = %u\n", FUNC, cur_entry);
HDfprintf(stderr, "%s: old_sec_node->u.range.num_entries = %u\n", FUNC, old_sec_node->u.range.num_entries);
#endif /* QAK */
    /* Create direct block of appropriate size */
    if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, cur_entry, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Reduce (& possibly re-add) 'range' section */
    if(H5HF_sect_range_reduce(hdr, dxpl_id, old_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce indirect section node")

    /* Point 'sec_node' at new direct block section node */
    *sec_node = dblock_sec_node;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_alloc_range() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_alloc_indirect
 *
 * Purpose:	Allocate a "single" section for an object, out of an
 *              "indirect" section, possibly creating "range" section as a
 *              byproduct.
 *
 * Note:	Creates necessary direct & indirect blocks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr  4 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_alloc_indirect(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    H5HF_indirect_t *child_iblock;      /* Pointer to child indirect block */
    haddr_t child_iblock_addr;          /* Address of child indirect block */
    haddr_t dblock_addr;                /* New direct block's address */
    H5HF_free_section_t *dblock_sec_node = NULL;   /* Pointer to direct block's section node */
    H5HF_free_section_t *old_sec_node = *sec_node; /* Pointer to old indirect section node */
    unsigned curr_entry;                /* Current entry in indirect block */
    unsigned dblock_entry;              /* Entry of direct block in child indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_alloc_indirect)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sec_node && *sec_node);

    /* Check for serialized section */
    if(old_sec_node->sect_info.state == H5FS_SECT_SERIALIZED) {
        /* Revive indirect section */
        if(H5HF_sect_indirect_revive(hdr, dxpl_id, old_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREVIVE, FAIL, "can't revive indirect section")
    } /* end if */

    /* Compute info about range */
    curr_entry = (old_sec_node->u.indirect.row * hdr->man_dtable.cparam.width) + old_sec_node->u.indirect.col;

    /* Get a pointer to the indirect block covering the section */
    iblock = old_sec_node->u.indirect.iblock;

#ifdef QAK
HDfprintf(stderr, "%s: curr_entry = %u\n", FUNC, curr_entry);
HDfprintf(stderr, "%s: iblock->addr = %a\n", FUNC, iblock->addr);
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: iblock->parent = %p\n", FUNC, iblock->parent);
HDfprintf(stderr, "%s: iblock->ents[curr_entry].addr = %a\n", FUNC, iblock->ents[curr_entry].addr);
HDfprintf(stderr, "%s: old_sec_node->u.indirect.indir_nrows = %u\n", FUNC, old_sec_node->u.indirect.indir_nrows);
HDfprintf(stderr, "%s: old_sec_node->u.indirect.num_entries = %u\n", FUNC, old_sec_node->u.indirect.num_entries);
#endif /* QAK */

    /* Check if indirect block for this indirect section has already been created */
    if(H5F_addr_defined(iblock->ents[curr_entry].addr)) {
        /* Look up existing child indirect block */
        child_iblock_addr = iblock->ents[curr_entry].addr;
        if(NULL == (child_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, child_iblock_addr, old_sec_node->u.indirect.indir_nrows, iblock, curr_entry, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
    } /* end if */
    else {
        /* Create child indirect block */
        if(H5HF_man_iblock_create(hdr, dxpl_id, iblock, curr_entry, old_sec_node->u.indirect.indir_nrows, old_sec_node->u.indirect.indir_nrows, &child_iblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap indirect block")

        /* Lock new child indirect block */
        if(NULL == (child_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, child_iblock_addr, old_sec_node->u.indirect.indir_nrows, iblock, curr_entry, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: child_iblock->child_free_space = %Hu\n", FUNC, child_iblock->child_free_space);
#endif /* QAK */
    } /* end else */

    /* Compute entry for new direct block in child indirect block */
    dblock_entry = old_sec_node->u.indirect.indir_row * hdr->man_dtable.cparam.width;

    /* Create direct block of correct size */
    /* (creates a 'single' free space section also) */
#ifdef QAK
HDfprintf(stderr, "%s: old_sec_node->u.indirect.indir_row = %u\n", FUNC, old_sec_node->u.indirect.indir_row);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row] = %Hu\n", FUNC, hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row]);
HDfprintf(stderr, "%s: old_sec_node->sect_addr = %a\n", FUNC, old_sec_node->sect_addr);
#endif /* QAK */
    if(H5HF_man_dblock_create(dxpl_id, hdr, child_iblock, dblock_entry, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Create "range" section for other direct blocks in row of child indirect block */
    if(H5HF_sect_range_add(hdr, dxpl_id, (child_iblock->block_off + hdr->man_dtable.row_block_off[old_sec_node->u.indirect.indir_row]
                + hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row]),
            old_sec_node->sect_info.size, child_iblock, old_sec_node->u.indirect.indir_row,
            1, hdr->man_dtable.cparam.width - 1) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create range section for indirect block's free space")

    /* Reduce (& possibly re-add) 'indirect' section */
    if(H5HF_sect_indirect_reduce(hdr, dxpl_id, old_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce indirect section node")

    /* Release the child indirect block (marked as dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, child_iblock, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

    /* Point 'sec_node' at new direct block section node */
    *sec_node = dblock_sec_node;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_alloc_indirect() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_create
 *
 * Purpose:	Allocate & initialize a managed indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_create(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_indirect_t *par_iblock,
    unsigned par_entry, unsigned nrows, unsigned max_rows, haddr_t *addr_p)
{
    H5HF_indirect_t *iblock = NULL;     /* Pointer to indirect block */
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_create)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(nrows > 0);
    HDassert(addr_p);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (iblock = H5FL_MALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap indirect block")

    /* Reset the metadata cache info for the heap header */
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    iblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared heap header")

#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u, max_rows = %u\n", FUNC, nrows, max_rows);
#endif /* QAK */
    /* Set info for direct block */
    iblock->rc = 0;
    iblock->nrows = nrows;
    iblock->max_rows = max_rows;

    /* Compute size of buffer needed for indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

    /* Allocate indirect block entry tables */
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (iblock->nrows * hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for block entries")

    /* Initialize indirect block entry tables */
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++)
        iblock->ents[u].addr = HADDR_UNDEF;

    /* Allocate space for the indirect block on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")
    iblock->addr = *addr_p;

    /* Attach to parent indirect block, if there is one */
    iblock->parent = par_iblock;
    iblock->par_entry = par_entry;
    if(iblock->parent) {
        /* Attach new block to parent */
        if(H5HF_man_iblock_attach(iblock->parent, par_entry, *addr_p) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't attach indirect block to parent indirect block")

        /* Compute the indirect block's offset in the heap's address space */
        /* (based on parent's block offset) */
        iblock->block_off = par_iblock->block_off;
        iblock->block_off += hdr->man_dtable.row_block_off[par_entry / hdr->man_dtable.cparam.width];
        iblock->block_off += hdr->man_dtable.row_block_size[par_entry / hdr->man_dtable.cparam.width] * (par_entry % hdr->man_dtable.cparam.width);
    } /* end if */
    else
        iblock->block_off = 0;  /* Must be the root indirect block... */

    /* Update indirect block's statistics */
    iblock->nchildren = 0;
    iblock->max_child = 0;

    /* Cache the new fractal heap header */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, *addr_p, iblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap indirect block to cache")

done:
    if(ret_value < 0)
        if(iblock)
            (void)H5HF_cache_iblock_dest(hdr->f, iblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_protect
 *
 * Purpose:	Convenience wrapper around H5AC_protect on a indirect block
 *              (Use H5AC_unprotect to unprotect it for now)
 *
 * Return:	Pointer to indirect block on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 17 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_indirect_t *
H5HF_man_iblock_protect(H5HF_hdr_t *hdr, hid_t dxpl_id, haddr_t iblock_addr,
    unsigned iblock_nrows, H5HF_indirect_t *par_iblock, unsigned par_entry,
    H5AC_protect_t rw)
{
    H5HF_parent_t par_info;             /* Parent info for loading block */
    H5HF_indirect_t *iblock;            /* Indirect block from cache */
    H5HF_indirect_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_protect)
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a, iblock_nrows = %u\n", FUNC, iblock_addr, iblock_nrows);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(H5F_addr_defined(iblock_addr));
    HDassert(iblock_nrows > 0);

    /* Set up parent info */
    par_info.hdr = hdr;
    par_info.iblock = par_iblock;
    par_info.entry = par_entry;

    /* Protect the indirect block */
    if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &iblock_nrows, &par_info, rw)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

    /* Set the return value */
    ret_value = iblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_attach
 *
 * Purpose:	Attach a block to an indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 30 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_attach(H5HF_indirect_t *iblock, unsigned entry, haddr_t child_addr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_attach)
#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p, entry = %u, child_addr = %a, iblock_nrows = %u\n", FUNC, iblock, entry, child_addr);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(iblock);
    HDassert(H5F_addr_defined(child_addr));

    /* Increment the reference count on this indirect block */
    if(H5HF_iblock_incr(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")

    /* Point at the direct block */
    iblock->ents[entry].addr = child_addr;

    /* Check for max. entry used */
    if(entry > iblock->max_child)
        iblock->max_child = entry;

    /* Increment the # of child blocks */
    iblock->nchildren++;

    /* Mark indirect block as modified */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_attach() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_detach
 *
 * Purpose:	Detach a block from an indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 31 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_iblock_detach(H5HF_indirect_t *iblock, hid_t dxpl_id, unsigned entry)
{
    unsigned start_children;            /* # of children of iblock when routine was entered */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_detach)
#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p, entry = %u, dblock_addr = %a, iblock_nrows = %u\n", FUNC, iblock, entry, dblock_addr);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(iblock);
    HDassert(iblock->nchildren);

    /* Reset address of entry */
    iblock->ents[entry].addr = HADDR_UNDEF;

    /* Decrement the # of child blocks */
    /* (If the number of children drop to 0, the indirect block will be
     *  removed from the heap when it's ref. count drops to zero and the
     *  metadata cache calls the indirect block destructor)
     */
    /* (Track the initial # of children before the block gets modified, because
     *  this routine is called recursively)
     */
    start_children = iblock->nchildren;
    iblock->nchildren--;

    /* Reduce the max. entry used, if necessary */
    if(entry == iblock->max_child) {
        if(iblock->nchildren > 0)
            while(!H5F_addr_defined(iblock->ents[iblock->max_child].addr))
                iblock->max_child--;
        else
            iblock->max_child = 0;
    } /* end if */

    /* If this is the root indirect block handle some special cases */
    if(iblock->block_off == 0) {
        /* If the number of children drops to 1, and that child is the first
         *      direct block in the heap, convert the heap back to using a root
         *      direct block
         */
        if(iblock->nchildren == 1 && H5F_addr_defined(iblock->ents[0].addr))
            if(H5HF_man_iblock_root_revert(iblock, dxpl_id) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't convert root indirect block back to root direct block")

        /* Check for reducing size of root indirect block */
        if(iblock->nchildren > 0 && iblock->hdr->man_dtable.cparam.start_root_rows != 0
                && entry > iblock->max_child) {
            unsigned max_child_row;         /* Row for max. child entry */

            /* Compute information needed for determining whether to reduce size of root indirect block */
            max_child_row = iblock->max_child / iblock->hdr->man_dtable.cparam.width;

            /* Check if the root indirect block should be reduced */
            if(iblock->nrows > 1 && max_child_row <= (iblock->nrows / 2))
                if(H5HF_man_iblock_root_halve(iblock, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce size of root indirect block")
        } /* end if */
    } /* end if */

    /* Free indirect block disk space, if it has no children (i.e. it's been deleted) */
    if(start_children == 1) {
        HDassert(iblock->nchildren == 0);
        if(H5MF_xfree(iblock->hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock->addr, (hsize_t)iblock->size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap indirect block disk space")
    } /* end if */

    /* Mark indirect block as modified */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

    /* Decrement the reference count on this indirect block */
    /* (should be last, so that potential 'unpin' on this indirect block
     *  doesn't invalidate the 'iblock' variable)
     */
    if(H5HF_iblock_decr(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't decrement reference count on shared indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_detach() */

