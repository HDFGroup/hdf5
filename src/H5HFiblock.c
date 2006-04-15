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

/* Indirect block routines */
static herr_t H5HF_man_iblock_inc_loc(H5HF_indirect_t *iblock);
static herr_t H5HF_man_iblock_skip_blocks(H5HF_t *hdr, 
    H5HF_indirect_t *iblock, haddr_t iblock_addr,
    unsigned start_entry, unsigned nentries);
static herr_t H5HF_man_iblock_skip_ranges(H5HF_t *hdr,
    H5HF_indirect_t *iblock, haddr_t iblock_addr,
    unsigned start_entry, unsigned nentries);
static herr_t H5HF_man_iblock_create(H5HF_t *fh, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p);

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
 * Function:	H5HF_man_iblock_inc_loc
 *
 * Purpose:	Increment location of next direct block in indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_inc_loc(H5HF_indirect_t *iblock)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_inc_loc)

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Increment block entry */
    iblock->next_entry++;

    /* Increment column */
    iblock->next_col++;

    /* Check for walking off end of column */
    if(iblock->next_col == iblock->shared->man_dtable.cparam.width) {
        /* Reset column */
        iblock->next_col = 0;

        /* Increment row & block size */
        iblock->next_row++;
        if(iblock->next_row > 1)
            iblock->next_size *= 2;

        /* Check for filling up indirect block */
        if(iblock->next_row == iblock->max_rows) {
            /* Check for "full" heap */
            if(iblock->parent == NULL)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")

            /* Increment location for parent indirect block */
            if(H5HF_man_iblock_inc_loc(iblock->parent) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")
        } /* end if */
    } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
#endif /* QAK */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_inc_loc() */


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
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_iblock_incr)

    /* Sanity check */
    HDassert(iblock);

/* XXX: When "un-evictable" feature is finished, mark the block as
 *      unevictable on the first block to share it.  - QAK
 */

    /* Increment reference count on shared indirect block */
    iblock->rc++;

    FUNC_LEAVE_NOAPI(SUCCEED)
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
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_iblock_decr)

    /* Sanity check */
    HDassert(iblock);

    /* Decrement reference count on shared indirect block */
    iblock->rc--;

/* XXX: When "un-evictable" feature is finished, mark the block as
 *      evictable when the ref. count drops to zero.  - QAK
 */
/* XXX: Take this call out after "un-evictable" flag is working */
    H5HF_cache_iblock_dest_real(iblock);


    FUNC_LEAVE_NOAPI(SUCCEED)
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
H5HF_iblock_dirty(hid_t dxpl_id, H5HF_indirect_t *iblock)
{
    H5HF_indirect_t *tmp_iblock;        /* Temporary pointer to indirect block */
    hbool_t is_protected;               /* Whether the indirect block is protected */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_iblock_dirty)
#ifdef QAK
HDfprintf(stderr, "%s: Marking indirect block as dirty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);

/* XXX: When "un-evictable" feature is finished, just mark the block as dirty
 *      in the cache, instead of this protect -> unprotect kludge - QAK
 */
    /* Protect the indirect block */
    is_protected = iblock->cache_info.is_protected;
    if(!is_protected) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->addr = %a\n", FUNC, iblock->addr);
#endif /* QAK */
        if(NULL == (tmp_iblock = H5AC_protect(iblock->shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, &iblock->nrows, iblock->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
        HDassert(iblock == tmp_iblock);
    } /* end if */

    /* Set the dirty flags for the indirect block */
    iblock->dirty = TRUE;

    /* Release the indirect block (marked as dirty) */
    if(!is_protected) {
        if(H5AC_unprotect(iblock->shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, tmp_iblock, H5AC__DIRTIED_FLAG) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_iblock_dirty() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_skip_blocks
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
static herr_t
H5HF_man_iblock_skip_blocks(H5HF_t *hdr, H5HF_indirect_t *iblock,
    haddr_t iblock_addr, unsigned start_entry, unsigned nentries)
{
    H5HF_free_section_t *sec_node; /* Pointer to free list section for range */
    hsize_t sect_off;                   /* Offset of free section in heap */
    unsigned curr_row;                  /* Current row in indirect block */
    unsigned curr_col;                  /* Current column in indirect block */
    unsigned u;                         /* Local index variables */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_skip_blocks)
#ifdef QAK
HDfprintf(stderr, "%s: start_entry = %u, nentries = %u\n", FUNC, start_entry, nentries);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(H5F_addr_defined(iblock_addr));

    /* Compute starting column & row */
    curr_row = start_entry / hdr->man_dtable.cparam.width;
    curr_col = start_entry % hdr->man_dtable.cparam.width;

    /* Initialize information for rows skipped over */
    sect_off = iblock->block_off;
    for(u = 0; u < curr_row; u++)
        sect_off += hdr->man_dtable.row_block_size[u] * hdr->man_dtable.cparam.width;
    for(u = 0; u < curr_col; u++)
        sect_off += hdr->man_dtable.row_block_size[curr_row];
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

        /* Add free space section for blocks in this row */

        /* Create free list section node for blocks skipped over */
        if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

        /* Set section's information */
        sec_node->sect_addr = sect_off;
        sec_node->sect_size = hdr->man_dtable.row_dblock_free[curr_row];
        sec_node->type = H5HF_SECT_RANGE;
        sec_node->u.range.iblock_addr = iblock_addr;
        sec_node->u.range.iblock_nrows = iblock->nrows;
        sec_node->u.range.row = curr_row;
        sec_node->u.range.col = curr_col;
        sec_node->u.range.num_entries = row_entries;

        /* Add new free space to the global list of space */
        if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")

        /* Advance row & column position */
        sect_off += row_entries * hdr->man_dtable.row_block_size[curr_row];
        curr_row++;
        curr_col = 0;           /* (first partial row aligns this) */
        u += row_entries;
    } /* end for */

    /* Re-compute row information for next empty block */
#ifdef QAK
HDfprintf(stderr, "%s: u = %u\n", FUNC, u);
#endif /* QAK */
    curr_row = u / hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: curr_row = %u\n", FUNC, curr_row);
#endif /* QAK */

    /* Set indirect block's "next entry" information */
    iblock->next_col = 0;
    iblock->next_row = curr_row;
    iblock->next_size = hdr->man_dtable.row_block_size[curr_row];
    iblock->next_entry = u;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_skip_blocks() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_skip_ranges
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
H5HF_man_iblock_skip_ranges(H5HF_t *hdr, H5HF_indirect_t *iblock,
    haddr_t iblock_addr, unsigned start_entry, unsigned nentries)
{
    H5HF_free_section_t *sec_node; /* Pointer to free list section for range */
    hsize_t sect_off;                   /* Offset of free section in heap */
    size_t row_dblock_free_space;       /* Size of free space for row of direct blocks in a row */
    size_t acc_row_dblock_free_space;   /* Accumulated size of free space for row of direct blocks in a row */
    unsigned curr_row;                  /* Current row in indirect block */
    unsigned curr_col;                  /* Current column in indirect block */
    unsigned u, w;                      /* Local index variables */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_skip_ranges)
#ifdef QAK
HDfprintf(stderr, "%s: start_entry = %u, nentries = %u\n", FUNC, start_entry, nentries);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock);
    HDassert(H5F_addr_defined(iblock_addr));

    /* Compute starting column & row */
    curr_row = start_entry / hdr->man_dtable.cparam.width;
    curr_col = start_entry % hdr->man_dtable.cparam.width;

    /* Initialize information for rows skipped over */
    sect_off = iblock->block_off;
    for(u = 0; u < curr_row; u++)
        sect_off += hdr->man_dtable.row_block_size[u] * hdr->man_dtable.cparam.width;
    for(u = 0; u < curr_col; u++)
        sect_off += hdr->man_dtable.row_block_size[curr_row];
#ifdef QAK
HDfprintf(stderr, "%s: sect_off = %Zu\n", FUNC, sect_off);
#endif /* QAK */

    /* Loop over the blocks to skip */
    for(u = start_entry; u < (start_entry + nentries); /* u is advanced in inner loop */) {
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
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

            /* Set section's information */
            sec_node->sect_addr = sect_off + hdr->man_dtable.row_block_off[w];
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
#endif /* QAK */
            sec_node->sect_size = hdr->man_dtable.row_dblock_free[w];
            sec_node->type = H5HF_SECT_INDIRECT;
            sec_node->u.indirect.iblock_addr = iblock_addr;
            sec_node->u.indirect.iblock_nrows = iblock->nrows;
            sec_node->u.indirect.row = curr_row;
            sec_node->u.indirect.col = curr_col;
            sec_node->u.indirect.num_entries = row_entries;
            sec_node->u.indirect.indir_row = w;
            sec_node->u.indirect.indir_nrows = num_rows;

            /* Add new free space to the global list of space */
            if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
        } /* end for */
#ifdef QAK
HDfprintf(stderr, "%s: acc_row_dblock_free_space = %Zu\n", FUNC, acc_row_dblock_free_space);
#endif /* QAK */

        /* Advance row & column position */
        sect_off += row_entries * hdr->man_dtable.row_block_size[curr_row];
        curr_row++;
        curr_col = 0;           /* (first partial row aligns this) */
        u += row_entries;
    } /* end for */

    /* Re-compute row information for next empty block */
#ifdef QAK
HDfprintf(stderr, "%s: u = %u\n", FUNC, u);
#endif /* QAK */
    curr_row = u / hdr->man_dtable.cparam.width;
#ifdef QAK
HDfprintf(stderr, "%s: curr_row = %u\n", FUNC, curr_row);
#endif /* QAK */

    /* Set indirect block's "next entry" information */
    iblock->next_col = 0;
    iblock->next_row = curr_row;
    iblock->next_size = hdr->man_dtable.row_block_size[curr_row];
    iblock->next_entry = u;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_skip_ranges() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_place_dblock
 *
 * Purpose:	Find indirect block with location for placing a direct block
 *
 * Note:	Creates necessary indirect blocks
 *
 * Return:	Pointer to indirect block on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_indirect_t *
H5HF_man_iblock_place_dblock(H5HF_t *hdr, hid_t dxpl_id, size_t min_dblock_size,
    haddr_t *addr_p, size_t *entry_p, size_t *dblock_size)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    H5HF_indirect_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_place_dblock)
#ifdef QAK
HDfprintf(stderr, "%s: min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(min_dblock_size > 0);
    HDassert(addr_p);

    /* Check for creating first indirect block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        H5HF_direct_t *dblock;          /* Pointer to direct block to query */
        hsize_t acc_dblock_free;        /* Accumulated free space in direct blocks */
        hbool_t have_direct_block;      /* Flag to indicate a direct block already exists */
        unsigned nrows;                 /* Number of rows for root indirect block */
        unsigned u;                     /* Local index variable */

#ifdef QAK
HDfprintf(stderr, "%s: creating first indirect block\n", FUNC);
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
        if(H5HF_man_iblock_create(hdr, dxpl_id, (hsize_t)0, nrows, hdr->man_dtable.max_root_rows, &iblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
#endif /* QAK */

        /* Move current direct block (used as root) into new indirect block */

        /* Lock new indirect block */
        if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &nrows, hdr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

        /* Check if there's already a direct block as root) */
        have_direct_block = H5F_addr_defined(hdr->man_dtable.table_addr);
#ifdef QAK
HDfprintf(stderr, "%s: have_direct_block = %u\n", FUNC, (unsigned)have_direct_block);
#endif /* QAK */
        if(have_direct_block) {
            /* Lock first (root) direct block */
            if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, &hdr->man_dtable.cparam.start_block_size, hdr, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap direct block")

            /* Point indirect block at direct block to add */
            iblock->child_free_space += (hssize_t)(dblock->blk_free_space - iblock->ents[0].free_space);
            iblock->ents[0].addr = hdr->man_dtable.table_addr;
            iblock->ents[0].free_space = dblock->blk_free_space;

            /* Make direct block share parent indirect block */
            dblock->parent = iblock;
            dblock->par_entry = 0;
            dblock->par_addr = iblock->addr;
            dblock->par_nrows = iblock->nrows;
            if(H5HF_iblock_incr(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

            /* Unlock first (root) direct block */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap direct block")
            dblock = NULL;

            /* Increment size of next block from this indirect block */
            /* (account for the already existing direct block */
            if(H5HF_man_iblock_inc_loc(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")
        } /* end if */

        /* Check for skipping over direct blocks, in order to get to large enough block */
        if(min_dblock_size > iblock->next_size) {
            /* Add skipped blocks to heap's free space */
            if(H5HF_man_iblock_skip_blocks(hdr, iblock, iblock_addr,
                    have_direct_block, ((nrows - 1) * hdr->man_dtable.cparam.width) - have_direct_block) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")
            HDassert(iblock->next_size == min_dblock_size);
        } /* end if */

        /* Mark indirect block as modified */
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

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
        if(H5HF_hdr_extend_heap(hdr, hdr->man_dtable.row_block_off[nrows], acc_dblock_free) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, NULL, "can't increase space to cover root direct block")

        /* Mark heap header as modified */
        hdr->dirty = TRUE;
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: searching root indirect block\n", FUNC);
#endif /* QAK */

        /* Lock root indirect block */
        iblock_addr = hdr->man_dtable.table_addr;
        if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &hdr->man_dtable.curr_root_rows, hdr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u, iblock->nrows = %u\n", FUNC, iblock->next_row, iblock->nrows);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
#endif /* QAK */
        /* Check if we need a block past current allocation */
        /* (i.e. extend the root indirect block) */
        if(iblock->next_row == iblock->nrows || 
                /* Don't try to extend the root indirect block if the requested
                 *  direct block is too large, but the
                 *  next direct block is in a child indirect block.
                 */
                (iblock->nrows < hdr->man_dtable.max_direct_rows &&
                    min_dblock_size > iblock->next_size)) {
            haddr_t new_addr;           /* New address of indirect block */
            hsize_t acc_dblock_free;    /* Accumulated free space in direct blocks */
            unsigned old_next_entry;    /* The previous value of the "next entry" for the indirect block */
            unsigned min_nrows = 0;     /* Min. # of direct rows */
            unsigned old_nrows;         /* Old # of rows */
            unsigned new_nrows;         /* New # of rows */
            size_t u;                   /* Local index variable */

#ifdef QAK
HDfprintf(stderr, "%s: Extending root indirect block\n", FUNC);
#endif /* QAK */

            /* Keep this for later */
            old_next_entry = iblock->next_entry;
            old_nrows = iblock->nrows;

            /* Check for skipping over rows */
            if(iblock->nrows < hdr->man_dtable.max_direct_rows && min_dblock_size > iblock->next_size) {
                /* Sanity check */
                HDassert(min_dblock_size > hdr->man_dtable.cparam.start_block_size);

                /* Make certain we allocate at least the required row for the block requested */
                min_nrows = 2 + (H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size));

                /* Set the information for the next block, of the appropriate size */
                iblock->next_entry = (min_nrows - 1) * hdr->man_dtable.cparam.width;
            } /* end if */
            /* Check for special case of second row, which has blocks the same size as first row */
            else if(iblock->next_row == 1)
                iblock->next_size = hdr->man_dtable.cparam.start_block_size;

            /* Compute new # of rows in indirect block */
            new_nrows = MAX(min_nrows, MIN(2 * iblock->nrows, iblock->max_rows));
#ifdef QAK
HDfprintf(stderr, "%s: min_nrows = %u, new_nrows = %u\n", FUNC, min_nrows, new_nrows);
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: old_next_entry = %u, iblock->next_entry = %u\n", FUNC, old_next_entry, iblock->next_entry);
#endif /* QAK */

/* Currently, the old block data is "thrown away" after the space is reallocated,
* so avoid data copy in H5MF_realloc() call by just free'ing the space and
* allocating new space.
*
* This also keeps the file smaller, by freeing the space and then
* allocating new space, instead of vice versa (in H5MF_realloc).
*
* QAK - 3/14/2006
*/
            /* Free previous indirect block disk space */
            if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock_addr, (hsize_t)iblock->size)<0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, NULL, "unable to free fractal heap indirect block")

            /* Compute size of buffer needed for new indirect block */
            iblock->nrows = new_nrows;
            iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

            /* Allocate space for the new indirect block on disk */
            if(HADDR_UNDEF == (new_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
                HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, NULL, "file allocation failed for fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: new_addr = %a\n", FUNC, new_addr);
#endif /* QAK */

            /* Re-allocate direct block entry table */
            if(NULL == (iblock->ents = H5FL_SEQ_REALLOC(H5HF_indirect_ent_t, iblock->ents, (iblock->nrows * hdr->man_dtable.cparam.width))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")

            /* Check for skipping over rows and add free section for skipped rows */
            if(iblock->nrows < hdr->man_dtable.max_direct_rows && min_dblock_size > iblock->next_size) {
                /* Add skipped blocks to heap's free space */
                if(H5HF_man_iblock_skip_blocks(hdr, iblock, new_addr,
                        old_next_entry, (iblock->next_entry - old_next_entry)) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")
                HDassert(iblock->next_size == min_dblock_size);
            } /* end if */

            /* Initialize new direct block entries in rows added */
            acc_dblock_free = 0;
            for(u = (old_nrows * hdr->man_dtable.cparam.width); u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
                unsigned row = u / hdr->man_dtable.cparam.width;        /* Row for current entry */

                iblock->ents[u].addr = HADDR_UNDEF;
                iblock->ents[u].free_space = hdr->man_dtable.row_dblock_free[row];
                iblock->child_free_space += iblock->ents[u].free_space;
                acc_dblock_free += iblock->ents[u].free_space;
            } /* end for */

            /* Mark indirect block as dirty */
            if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

            /* Release the indirect block (marked as dirty) */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")
            iblock = NULL;

            /* Move object in cache */
            if(H5AC_rename(hdr->f, H5AC_FHEAP_IBLOCK, iblock_addr, new_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, NULL, "unable to move fractal heap root indirect block")

            /* Update other shared header info */
            hdr->man_dtable.curr_root_rows = new_nrows;
            hdr->man_dtable.table_addr = iblock_addr = new_addr;

            /* Extend heap to cover new root indirect block */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows - 1] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows - 1]);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows]);
HDfprintf(stderr, "%s: acc_dblock_free = %Hu\n", FUNC, acc_dblock_free);
#endif /* QAK */
            if(H5HF_hdr_extend_heap(hdr, 2 * hdr->man_dtable.row_block_off[new_nrows - 1], acc_dblock_free) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, NULL, "can't increase space to cover root direct block")

            /* Mark heap header as modified */
            hdr->dirty = TRUE;

            /* Lock root indirect block (again) */
            if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &hdr->man_dtable.curr_root_rows, hdr, H5AC_WRITE)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
            iblock->addr = iblock_addr;
        } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: Check 1.0\n", FUNC);
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
#endif /* QAK */
        /* Check for full direct block entries in current indirect block */
        while(iblock->next_row >= hdr->man_dtable.max_direct_rows) {
            haddr_t new_iblock_addr;       /* New indirect block's address */
            H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
            unsigned hdr_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for indirect block */
            unsigned nrows;                /* Number of rows in new indirect block */

            /* Compute # of rows in child indirect block */
            nrows = (H5V_log2_of2((uint32_t)iblock->next_size) - hdr->man_dtable.first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "%s: Check 2.0\n", FUNC);
HDfprintf(stderr, "%s: iblock->next_size = %Hu, nrows = %u\n", FUNC, iblock->next_size, nrows);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->max_rows = %u\n", FUNC, iblock->max_rows);
#endif /* QAK */

            /* Check for skipping over indirect block rows */
            if(hdr->man_dtable.row_block_size[nrows - 1] < min_dblock_size) {
                unsigned child_rows_needed;           /* Number of rows needed to hold direct block */

                /* Compute # of rows needed in child indirect block */
                child_rows_needed = (H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size)) + 2;
                HDassert(child_rows_needed > nrows);
#ifdef QAK
HDfprintf(stderr, "%s: child_rows_needed = %u\n", FUNC, child_rows_needed);
#endif /* QAK */

                /* Add skipped indirect ranges to heap's free space */
                if(H5HF_man_iblock_skip_ranges(hdr, iblock, iblock->addr, iblock->next_entry, (child_rows_needed - nrows) * hdr->man_dtable.cparam.width) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")

                /* Update the number of rows in requested child indirect block */
                nrows = child_rows_needed;
#ifdef QAK
HDfprintf(stderr, "%s: (new) nrows = %u\n", FUNC, nrows);
#endif /* QAK */
            } /* end if */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_size = %Hu, nrows = %u\n", FUNC, iblock->next_size, nrows);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->max_rows = %u\n", FUNC, iblock->max_rows);
#endif /* QAK */

            /* Check for walking off indirect block rows */
            if(iblock->next_row >= iblock->max_rows) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->parent->nrows = %u\n", FUNC, iblock->parent->nrows);
HDfprintf(stderr, "%s: iblock->parent->next_entry = %u\n", FUNC, iblock->parent->next_entry);
HDfprintf(stderr, "%s: iblock->parent->next_size = %Hu\n", FUNC, iblock->parent->next_size);
HDfprintf(stderr, "%s: iblock->parent->next_row = %u\n", FUNC, iblock->parent->next_row);
HDfprintf(stderr, "%s: iblock->parent->next_col = %u\n", FUNC, iblock->parent->next_col);
#endif /* QAK */
                    /* Locate parent indirect block */
                    new_iblock_addr = iblock->parent->addr;
                    nrows = iblock->parent->nrows;

                    /* Lock parent indirect block */
                    if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_WRITE)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

                    /* Advance location in parent lock */
                    if(H5HF_man_iblock_inc_loc(new_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")
            } /* end if */
            else {
                /* Check for allocating new indirect block */
                if(!H5F_addr_defined(iblock->ents[iblock->next_entry].addr)) {
                    hsize_t new_iblock_off;         /* Direct block offset in heap address space */
#ifdef QAK
    HDfprintf(stderr, "%s: Allocating new indirect block\n", FUNC);
#endif /* QAK */
                    /* Compute the direct block's offset in the heap's address space */
                    new_iblock_off = iblock->block_off;
                    new_iblock_off += hdr->man_dtable.row_block_off[iblock->next_entry / hdr->man_dtable.cparam.width];
                    new_iblock_off += hdr->man_dtable.row_block_size[iblock->next_entry / hdr->man_dtable.cparam.width] * (iblock->next_entry % hdr->man_dtable.cparam.width);

                    /* Allocate new indirect block */
                    if(H5HF_man_iblock_create(hdr, dxpl_id, new_iblock_off, nrows, nrows, &new_iblock_addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")

                    /* Lock new indirect block */
                    if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_WRITE)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

                    /* Set parent information */
                    HDassert(new_iblock->parent == NULL);
                    new_iblock->parent = iblock;
                    new_iblock->par_entry = iblock->next_entry;
                    new_iblock->par_nrows = iblock->nrows;
                    new_iblock->par_addr = iblock->addr;
                    if(H5HF_iblock_incr(iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

                    /* Point current indirect block at new indirect block */
                    iblock->ents[iblock->next_entry].addr = new_iblock_addr;
#ifdef QAK
    HDfprintf(stderr, "%s: new_iblock->next_row = %u\n", FUNC, new_iblock->next_row);
    HDfprintf(stderr, "%s: new_iblock->next_col = %u\n", FUNC, new_iblock->next_col);
    HDfprintf(stderr, "%s: new_iblock->next_size = %Zu\n", FUNC, new_iblock->next_size);
    HDfprintf(stderr, "%s: new_iblock->next_entry = %u\n", FUNC, new_iblock->next_entry);
#endif /* QAK */

                    /* Check for skipping over rows and add free section for skipped rows */
                    if(min_dblock_size > new_iblock->next_size) {
                        unsigned new_entry;        /* Entry of direct block which is large enough */

                        /* Compute entry for direct block size requested */
                        new_entry = hdr->man_dtable.cparam.width *
                            (1 + (H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size)));
#ifdef QAK
    HDfprintf(stderr, "%s: new_entry = %u\n", FUNC, new_entry);
#endif /* QAK */

                        /* Add skipped blocks to heap's free space */
                        if(H5HF_man_iblock_skip_blocks(hdr, new_iblock, new_iblock->addr, 0, new_entry) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")
                        HDassert(new_iblock->next_size == min_dblock_size);
                    } /* end if */

                    /* Mark current indirect block as modified */
                    if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

                    /* Set dirty flag for the current indirect block */
                    hdr_flags |= H5AC__DIRTIED_FLAG;
                } /* end if */
                else {
#ifdef QAK
    HDfprintf(stderr, "%s: Descending existing indirect block\n", FUNC);
#endif /* QAK */
                    /* Locate child indirect block */
                    new_iblock_addr = iblock->ents[iblock->next_entry].addr;

                    /* Lock new indirect block */
                    if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_WRITE)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
                 } /* end else */
             } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: new_iblock->next_row = %u\n", FUNC, new_iblock->next_row);
HDfprintf(stderr, "%s: new_iblock->next_col = %u\n", FUNC, new_iblock->next_col);
HDfprintf(stderr, "%s: new_iblock->next_size = %Zu\n", FUNC, new_iblock->next_size);
HDfprintf(stderr, "%s: new_iblock->next_entry = %u\n", FUNC, new_iblock->next_entry);
#endif /* QAK */

            /* Release the current indirect block (possibly marked as dirty) */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, hdr_flags) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")

            /* Switch variables to use new indirect block */
            iblock = new_iblock;
            iblock_addr = new_iblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: new_iblock_addr = %a\n", FUNC, new_iblock_addr);
#endif /* QAK */
        } /* end while */
    } /* end else */

    /* Check for skipping over blocks */
    if(min_dblock_size > iblock->next_size) {
HDfprintf(stderr, "%s: Skipping direct block sizes not supported, iblock->next_size = %Zu\n", FUNC, iblock->next_size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, NULL, "skipping direct block sizes not supported yet")
    } /* end if */

    /* Set address of indirect block that's the immediate parent of new direct block */
    *addr_p = iblock_addr;

    /* Set entry for new direct block to use */
    *entry_p = iblock->next_entry;

    /* Set size of direct block to create */
    *dblock_size = iblock->next_size;

    /* Increment location of next block from this indirect block */
    if(H5HF_man_iblock_inc_loc(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")

    /* Set return value */
    ret_value = iblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_place_dblock() */


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
H5HF_man_iblock_alloc_range(H5HF_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node, size_t obj_size)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    haddr_t dblock_addr;                /* Direct block's address */
    unsigned iblock_nrows;              /* Indirect block's number of rows */
    H5HF_free_section_t *dblock_sec_node = NULL;     /* Pointer to direct block's section node */
    H5HF_free_section_t *old_sec_node = *sec_node;     /* Pointer to old section node */
    size_t full_obj_size;               /* Size of object including metadata */
    unsigned cur_entry;                 /* Current entry in indirect block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_alloc_range)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sec_node && *sec_node);
    HDassert(obj_size > 0);

    /* Compute info about range */
    cur_entry = (old_sec_node->u.range.row * hdr->man_dtable.cparam.width) + old_sec_node->u.range.col;

    /* Check for range covering indirect blocks */
    if(old_sec_node->u.range.row >= hdr->man_dtable.max_direct_rows) {
HDfprintf(stderr, "%s: Can't handle range sections over indirect blocks yet\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "'range' free space sections over indirect blocks not supported yet")
    } /* end if */

    /* Get information about indirect block covering section */
    /* (Allow for root indirect block being resized) */
    iblock_addr = old_sec_node->u.range.iblock_addr;
    if(H5F_addr_eq(iblock_addr, hdr->man_dtable.table_addr))
        iblock_nrows = hdr->man_dtable.curr_root_rows;
    else
        iblock_nrows = old_sec_node->u.range.iblock_nrows;

    /* Get a pointer to the indirect block covering the range */
    if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &iblock_nrows, hdr, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

    /* Compute size of object, with metadata overhead */
    full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);

#ifdef QAK
HDfprintf(stderr, "%s: cur_entry = %u\n", FUNC, cur_entry);
HDfprintf(stderr, "%s: old_sec_node->u.range.num_entries = %u\n", FUNC, old_sec_node->u.range.num_entries);
#endif /* QAK */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_size[old_sec_node->u.range.row] = %Hu\n", FUNC, hdr->man_dtable.row_block_size[old_sec_node->u.range.row]);
HDfprintf(stderr, "%s: old_sec_node->sect_addr = %a\n", FUNC, old_sec_node->sect_addr);
#endif /* QAK */
    /* Create direct block of appropriate size */
    if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, cur_entry, (size_t)hdr->man_dtable.row_block_size[old_sec_node->u.range.row], (hsize_t)old_sec_node->sect_addr, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Hook direct block up to indirect block */
    iblock->ents[cur_entry].addr = dblock_addr;

    /* Check for only single block covered in range section */
    if(old_sec_node->u.range.num_entries == 1)
        H5FL_FREE(H5HF_free_section_t, old_sec_node);
    else {
        /* Adjust section information */
        old_sec_node->sect_addr += hdr->man_dtable.row_block_size[old_sec_node->u.range.row];

        /* Adjust range information */
        old_sec_node->u.range.col++;
        old_sec_node->u.range.num_entries--;

        /* Add section back to free space list */
        if(H5HF_flist_add(hdr->flist, old_sec_node, &old_sec_node->sect_size, &old_sec_node->sect_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
    } /* end else */

    /* Release the indirect block (marked as dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

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
H5HF_man_iblock_alloc_indirect(H5HF_t *hdr, hid_t dxpl_id,
    H5HF_free_section_t **sec_node, size_t obj_size)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    H5HF_indirect_t *child_iblock;      /* Pointer to child indirect block */
    unsigned iblock_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for parent indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    haddr_t child_iblock_addr;          /* Address of child indirect block */
    haddr_t dblock_addr;                /* New direct block's address */
    unsigned iblock_nrows;              /* Indirect block's number of rows */
    H5HF_free_section_t *dblock_sec_node = NULL;   /* Pointer to direct block's section node */
    H5HF_free_section_t *range_sec_node = NULL;    /* Pointer to new range section node */
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
    HDassert(obj_size > 0);

    /* Compute info about range */
    curr_entry = (old_sec_node->u.indirect.row * hdr->man_dtable.cparam.width) + old_sec_node->u.indirect.col;

    /* Get information about indirect block covering section */
    /* (Allow for root indirect block being resized) */
    iblock_addr = old_sec_node->u.indirect.iblock_addr;
    if(H5F_addr_eq(iblock_addr, hdr->man_dtable.table_addr))
        iblock_nrows = hdr->man_dtable.curr_root_rows;
    else
        iblock_nrows = old_sec_node->u.indirect.iblock_nrows;

    /* Get a pointer to the indirect block covering the range */
    if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &iblock_nrows, hdr, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

#ifdef QAK
HDfprintf(stderr, "%s: curr_entry = %u\n", FUNC, curr_entry);
HDfprintf(stderr, "%s: iblock->ents[curr_entry].addr = %a\n", FUNC, iblock->ents[curr_entry].addr);
HDfprintf(stderr, "%s: iblock->ents[curr_entry].free_space = %Hu\n", FUNC, iblock->ents[curr_entry].free_space);
HDfprintf(stderr, "%s: old_sec_node->u.indirect.indir_nrows = %u\n", FUNC, old_sec_node->u.indirect.indir_nrows);
HDfprintf(stderr, "%s: old_sec_node->u.indirect.num_entries = %u\n", FUNC, old_sec_node->u.indirect.num_entries);
#endif /* QAK */

    /* Check if indirect block for this indirect section has already been created */
    if(H5F_addr_defined(iblock->ents[curr_entry].addr)) {
        /* Look up existing child indirect block */
        child_iblock_addr = iblock->ents[curr_entry].addr;
        if(NULL == (child_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, &old_sec_node->u.indirect.indir_nrows, hdr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
    } /* end if */
    else {
        hsize_t new_iblock_off;         /* Offset of new indirect block */

        /* Compute heap offset of new indirect block */
        new_iblock_off = iblock->block_off + 
            hdr->man_dtable.row_block_off[old_sec_node->u.indirect.row] +
            (old_sec_node->u.indirect.col * 
                hdr->man_dtable.row_block_size[old_sec_node->u.indirect.row]);
#ifdef QAK
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: new_iblock_off = %Hu\n", FUNC, new_iblock_off);
#endif /* QAK */

        /* Create child indirect block */
        if(H5HF_man_iblock_create(hdr, dxpl_id, new_iblock_off, old_sec_node->u.indirect.indir_nrows, old_sec_node->u.indirect.indir_nrows, &child_iblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap indirect block")

        /* Lock new child indirect block */
        if(NULL == (child_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, &old_sec_node->u.indirect.indir_nrows, hdr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Set parent information */
        HDassert(child_iblock->parent == NULL);
        child_iblock->parent = iblock;
        child_iblock->par_entry = curr_entry;
        child_iblock->par_nrows = iblock->nrows;
        child_iblock->par_addr = iblock->addr;
        if(H5HF_iblock_incr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: child_iblock->child_free_space = %Hu\n", FUNC, child_iblock->child_free_space);
#endif /* QAK */

        /* Hook child up to parent indirect block */
        iblock->ents[curr_entry].addr = child_iblock_addr;
        HDassert(iblock->ents[curr_entry].free_space == child_iblock->child_free_space);

        /* Mark parent indirect block as modified */
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

        /* Set dirty flag for the parent indirect block */
        iblock_flags |= H5AC__DIRTIED_FLAG;
    } /* end else */

    /* Compute entry for new direct block in child indirect block */
    dblock_entry = old_sec_node->u.indirect.indir_row * hdr->man_dtable.cparam.width;

    /* Create direct block of correct size */
#ifdef QAK
HDfprintf(stderr, "%s: old_sec_node->u.indirect.indir_row = %u\n", FUNC, old_sec_node->u.indirect.indir_row);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row] = %Hu\n", FUNC, hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row]);
HDfprintf(stderr, "%s: old_sec_node->sect_addr = %a\n", FUNC, old_sec_node->sect_addr);
#endif /* QAK */
    if(H5HF_man_dblock_create(dxpl_id, hdr, child_iblock, dblock_entry, (size_t)hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row], (hsize_t)old_sec_node->sect_addr, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Hook direct block up to child indirect block */
    child_iblock->ents[dblock_entry].addr = dblock_addr;


    /* Create "range" section for other direct blocks in row of child indirect block */

    /* Create free list section node for blocks skipped over */
    if(NULL == (range_sec_node = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set section's information */
    range_sec_node->sect_addr = child_iblock->block_off + hdr->man_dtable.row_block_off[old_sec_node->u.indirect.indir_row]
            + hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row];
    range_sec_node->sect_size = old_sec_node->sect_size;
    range_sec_node->type = H5HF_SECT_RANGE;
    range_sec_node->u.range.iblock_addr = child_iblock_addr;
    range_sec_node->u.range.iblock_nrows = child_iblock->nrows;
    range_sec_node->u.range.row = old_sec_node->u.indirect.indir_row;
    range_sec_node->u.range.col = 1;
    range_sec_node->u.range.num_entries = hdr->man_dtable.cparam.width - 1;

    /* Add new free space to the global list of space */
    if(H5HF_flist_add(hdr->flist, range_sec_node, &range_sec_node->sect_size, &range_sec_node->sect_addr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")


    /* Reduce "indirect" section */

    /* Check for only single block covered in range section */
    if(old_sec_node->u.indirect.num_entries == 1)
        H5FL_FREE(H5HF_free_section_t, old_sec_node);
    else {
        /* Adjust section information */
        old_sec_node->sect_addr += hdr->man_dtable.row_block_size[old_sec_node->u.indirect.row];

        /* Adjust range information */
        old_sec_node->u.indirect.col++;
        old_sec_node->u.indirect.num_entries--;

        /* Add section back to free space list */
        if(H5HF_flist_add(hdr->flist, old_sec_node, &old_sec_node->sect_size, &old_sec_node->sect_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
    } /* end else */

    /* Release the child indirect block (marked as dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, child_iblock_addr, child_iblock, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

    /* Release the parent indirect block (possibly dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, iblock_flags) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

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
static herr_t
H5HF_man_iblock_create(H5HF_t *hdr, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p)
{
    H5HF_indirect_t *iblock = NULL;     /* Pointer to indirect block */
    size_t curr_row;                    /* Current row within indirect block */
    size_t u, v;                        /* Local index variable */
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
    iblock->shared = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared heap header")

#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u, max_nrows = %u\n", FUNC, nrows, max_nrows);
#endif /* QAK */
    /* Set info for direct block */
    iblock->rc = 0;
    iblock->parent = NULL;              /* Temporary, except for root indirect block */
    iblock->par_entry = 0;
    iblock->par_nrows = 0;
    iblock->par_addr = HADDR_UNDEF;
    iblock->block_off = block_off;
    iblock->nrows = nrows;
    iblock->max_rows = max_rows;
    iblock->next_col = 0;
    iblock->next_row = 0;
    iblock->next_entry = 0;
    iblock->next_size = hdr->man_dtable.cparam.start_block_size;
    iblock->dirty = TRUE;
    iblock->evicted = FALSE;

    /* Compute size of buffer needed for indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

    /* Allocate indirect block entry tables */
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (iblock->nrows * hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for block entries")

    /* Initialize indirect block entry tables */
    curr_row = 0;
    iblock->child_free_space = 0;
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); /* u advanced in inner loop */) {
        for(v = 0; v < hdr->man_dtable.cparam.width; v++, u++) {
            iblock->ents[u].addr = HADDR_UNDEF;
            iblock->ents[u].free_space = hdr->man_dtable.row_dblock_free[curr_row];
            iblock->child_free_space += iblock->ents[u].free_space;
        } /* end for */
        curr_row++;
    } /* end for */

    /* Allocate space for the indirect block on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")
    iblock->addr = *addr_p;

/* XXX: Update indirect statistics when they are added */

    /* Cache the new fractal heap header */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, *addr_p, iblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap indirect block to cache")

done:
    if(ret_value < 0)
        if(iblock)
            (void)H5HF_cache_iblock_dest(hdr->f, iblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_create() */

