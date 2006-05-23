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

/* Free space section routines */
static herr_t H5HF_man_iblock_skip_blocks(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_indirect_t *iblock, haddr_t iblock_addr,
    unsigned start_entry, unsigned nentries);
static herr_t H5HF_man_iblock_skip_ranges(H5HF_hdr_t *hdr, hid_t dxpl_id,
    H5HF_indirect_t *iblock, haddr_t iblock_addr,
    unsigned start_entry, unsigned nentries);

/* Root indirect block routines */
static herr_t H5HF_man_iblock_root_create(H5HF_hdr_t *hdr, hid_t dxpl_id,
    size_t min_dblock_size);
static herr_t H5HF_man_iblock_root_double(H5HF_hdr_t *hdr, hid_t dxpl_id,
    size_t min_dblock_size);

/* Misc. indirect block routines */
static herr_t H5HF_man_iblock_create(H5HF_hdr_t *hdr, hid_t dxpl_id,
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
    if(iblock->rc == 0)
        if(H5AC_unpin_entry(iblock->hdr->f, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPIN, FAIL, "unable to unpin fractal heap indirect block")

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

/* XXX: Need to mark a protected block as dirty eventually also... */
{
    unsigned entry_status;              /* Indirect block entry status */

    if(H5AC_get_entry_status(iblock->hdr->f, iblock->addr, &entry_status) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, FAIL, "unable to query fractal heap indirect block status")
    HDassert(entry_status & H5AC_ES__IN_CACHE);

    if(!(entry_status & H5AC_ES__IS_PROTECTED)) {
        /* Mark indirect block as dirty in cache */
        if(H5AC_mark_pinned_entry_dirty(iblock->hdr->f, iblock, FALSE, 0) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTMARKDIRTY, FAIL, "unable to mark fractal heap indirect block as dirty")
    } /* end if */
}

    /* Set the dirty flag for the indirect block */
    iblock->dirty = TRUE;

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
H5HF_man_iblock_skip_blocks(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_indirect_t *iblock,
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

        /* Add free space section for blocks in this row */

        /* Create free list section node for blocks skipped over */
        if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

        /* Set section's information */
        sec_node->sect_info.addr = sect_off;
        sec_node->sect_info.size = hdr->man_dtable.row_dblock_free[curr_row];
        sec_node->sect_info.cls = &hdr->sect_cls[H5FS_SECT_FHEAP_RANGE];
        sec_node->sect_info.state = H5FS_SECT_LIVE;
        sec_node->u.range.iblock = iblock;
        if(H5HF_iblock_incr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
        sec_node->u.range.row = curr_row;
        sec_node->u.range.col = curr_col;
        sec_node->u.range.num_entries = row_entries;

        /* Add new free space to the global list of space */
        if(H5HF_space_add(hdr, dxpl_id, sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")

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

    /* Advance the allocated heap size/new block iterator */
    if(H5HF_hdr_inc_alloc(hdr, sect_off, nentries) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

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
H5HF_man_iblock_skip_ranges(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_indirect_t *iblock,
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
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

            /* Set section's information */
            sec_node->sect_info.addr = sect_off + hdr->man_dtable.row_block_off[w];
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_info.addr = %a\n", FUNC, sec_node->sect_info.addr);
#endif /* QAK */
            sec_node->sect_info.size = hdr->man_dtable.row_dblock_free[w];
            sec_node->sect_info.cls = &hdr->sect_cls[H5FS_SECT_FHEAP_INDIRECT];
            sec_node->sect_info.state = H5FS_SECT_LIVE;
            sec_node->u.indirect.iblock = iblock;
            if(H5HF_iblock_incr(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
            sec_node->u.indirect.row = curr_row;
            sec_node->u.indirect.col = curr_col;
            sec_node->u.indirect.num_entries = row_entries;
            sec_node->u.indirect.indir_row = w;
            sec_node->u.indirect.indir_nrows = num_rows;

            /* Add new free space to the global list of space */
            if(H5HF_space_add(hdr, dxpl_id, sec_node) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
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

    /* Advance the allocated heap size/new block iterator */
    if(H5HF_hdr_inc_alloc(hdr, sect_off, nentries) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_skip_ranges() */


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
static herr_t
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
    if(H5HF_man_iblock_create(hdr, dxpl_id, (hsize_t)0, nrows, hdr->man_dtable.max_root_rows, &iblock_addr) < 0)
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

        /* Point indirect block at direct block to add */
        iblock->ents[0].addr = hdr->man_dtable.table_addr;

        /* Make direct block share parent indirect block */
        dblock->parent = iblock;
        dblock->par_entry = 0;
        if(H5HF_iblock_incr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")

        /* Unlock first (root) direct block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
        dblock = NULL;
    } /* end if */

    /* Set up iterator at correct location */
    if(H5HF_man_iter_start_entry(hdr, &hdr->next_block, iblock, have_direct_block) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize block iterator")

    /* Check for skipping over direct blocks, in order to get to large enough block */
    if(min_dblock_size > hdr->man_dtable.cparam.start_block_size) {
        /* Add skipped blocks to heap's free space */
        if(H5HF_man_iblock_skip_blocks(hdr, dxpl_id, iblock, iblock_addr,
                have_direct_block, ((nrows - 1) * hdr->man_dtable.cparam.width) - have_direct_block) < 0)
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
    if(H5HF_hdr_extend_heap(hdr, hdr->man_dtable.row_block_off[nrows], acc_dblock_free) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark header as dirty")

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
static herr_t
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
* so avoid data copy in H5MF_realloc() call by just free'ing the space and
* allocating new space.
*
* This also keeps the file smaller, by freeing the space and then
* allocating new space, instead of vice versa (in H5MF_realloc).
*
* QAK - 3/14/2006
*/
    /* Free previous indirect block disk space */
    if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock->addr, (hsize_t)iblock->size)<0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap indirect block")

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
        if(H5HF_man_iblock_skip_blocks(hdr, dxpl_id, iblock, new_addr,
                next_entry, (new_next_entry - next_entry)) < 0)
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
    if(H5F_addr_ne(iblock->addr, new_addr))
        if(H5AC_rename(hdr->f, H5AC_FHEAP_IBLOCK, iblock->addr, new_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, FAIL, "unable to move fractal heap root indirect block")

    /* Update other shared header info */
    hdr->man_dtable.curr_root_rows = new_nrows;
    hdr->man_dtable.table_addr = new_addr;

    /* Extend heap to cover new root indirect block */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows - 1] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows - 1]);
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_off[new_nrows] = %Hu\n", FUNC, hdr->man_dtable.row_block_off[new_nrows]);
HDfprintf(stderr, "%s: acc_dblock_free = %Hu\n", FUNC, acc_dblock_free);
#endif /* QAK */
    if(H5HF_hdr_extend_heap(hdr, 2 * hdr->man_dtable.row_block_off[new_nrows - 1], acc_dblock_free) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark header as dirty")

/* XXX: Sanity check until can rename pinned entry in metadata cache */
#ifndef NDEBUG
{
H5HF_indirect_t *old_root_iblock = iblock;
#endif /* NDEBUG */

    /* Lock root indirect block (again) */
    if(NULL == (iblock = H5HF_man_iblock_protect(hdr, dxpl_id, new_addr, hdr->man_dtable.curr_root_rows, NULL, 0, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
    iblock->addr = new_addr;

#ifndef NDEBUG
    HDassert(old_root_iblock == iblock);
}
#endif /* NDEBUG */

    /* Update the indirect block pointer in iterator */
    /* (pins the indirect block after it's in the new location) */
    if(H5HF_man_iter_update_iblock(&hdr->next_block, iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTMODIFY, FAIL, "unable to update indirect block for block iterator")

    /* Release the indirect block (marked as dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_root_double() */


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
H5HF_man_iblock_place_dblock(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t min_dblock_size,
    size_t *entry_p, size_t *dblock_size)
{
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

    /* Check for creating first indirect block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        if(H5HF_man_iblock_root_create(hdr, dxpl_id, min_dblock_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, NULL, "unable to create root indirect block")
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
HDfprintf(stderr, "%s: hdr->man_alloc_size = %Hu\n", FUNC, hdr->man_alloc_size);
#endif /* QAK */
            /* Start iterator with offset of allocated space */
            if(H5HF_man_iter_start_offset(hdr, dxpl_id, &hdr->next_block, hdr->man_alloc_size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "unable to set block iterator location")
        } /* end if */

        /* Get information about current iterator location */
        if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
                &next_entry, &iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")

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
            if(H5HF_man_iblock_skip_blocks(hdr, dxpl_id, iblock, iblock->addr, next_entry, skip_entries) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")

            /* Get information about new iterator location */
            if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
                    &next_entry, &iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")
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
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, NULL, "unable to double root indirect block")
                } /* end if */
                else {
#ifdef QAK
HDfprintf(stderr, "%s: Walking up a level\n", FUNC);
#endif /* QAK */
                    /* Move iterator up one level */
                    if(H5HF_man_iter_up(&hdr->next_block) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, NULL, "unable to advance current block iterator location")

                    /* Increment location of next block at this level */
                    if(H5HF_man_iter_next(hdr, &hdr->next_block, 1) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")
                } /* end else */

                /* Get information about new iterator location */
                if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
                        &next_entry, &iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")

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
                hsize_t next_size;      /* Size of next direct block to create */
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
                next_size = hdr->man_dtable.row_block_size[next_row];
                child_nrows = (H5V_log2_gen(next_size) - hdr->man_dtable.first_row_bits) + 1;
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
                    if(H5HF_man_iblock_skip_ranges(hdr, dxpl_id, iblock, iblock->addr, next_entry, (child_entry - next_entry)) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")
                } /* end if */
                else {
                    H5HF_indirect_t *new_iblock;    /* Pointer to new indirect block */
                    hsize_t new_iblock_off;         /* Direct block offset in heap address space */
                    haddr_t new_iblock_addr;        /* New indirect block's address */

#ifdef QAK
HDfprintf(stderr, "%s: Allocating new child indirect block\n", FUNC);
#endif /* QAK */
                    /* Compute the direct block's offset in the heap's address space */
                    new_iblock_off = iblock->block_off;
                    new_iblock_off += hdr->man_dtable.row_block_off[next_entry / hdr->man_dtable.cparam.width];
                    new_iblock_off += hdr->man_dtable.row_block_size[next_entry / hdr->man_dtable.cparam.width] * (next_entry % hdr->man_dtable.cparam.width);

                    /* Allocate new indirect block */
                    if(H5HF_man_iblock_create(hdr, dxpl_id, new_iblock_off, child_nrows, child_nrows, &new_iblock_addr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")

                    /* Lock new indirect block */
                    if(NULL == (new_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, new_iblock_addr, child_nrows, iblock, next_entry, H5AC_WRITE)))
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

                    /* Set parent information */
                    HDassert(new_iblock->parent == NULL);
                    new_iblock->parent = iblock;
                    new_iblock->par_entry = next_entry;
                    if(H5HF_iblock_incr(iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

                    /* Point current indirect block at new indirect block */
                    iblock->ents[next_entry].addr = new_iblock_addr;

                    /* Move iterator down one level */
                    if(H5HF_man_iter_down(&hdr->next_block, new_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTNEXT, NULL, "unable to advance current block iterator location")

                    /* Get information about new iterator location */
                    if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
                            &next_entry, NULL) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")
                    next_size = hdr->man_dtable.row_block_size[next_row];
#ifdef QAK
HDfprintf(stderr, "%s: next_row = %u\n", FUNC, next_row);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */

                    /* Check for skipping over rows and add free section for skipped rows */
                    if(min_dblock_size > next_size) {
                        unsigned new_entry;        /* Entry of direct block which is large enough */

                        /* Compute entry for direct block size requested */
                        new_entry = hdr->man_dtable.cparam.width * min_dblock_row;
#ifdef QAK
HDfprintf(stderr, "%s: Skipping rows in new child indirect block - new_entry = %u\n", FUNC, new_entry);
#endif /* QAK */

                        /* Add skipped blocks to heap's free space */
                        if(H5HF_man_iblock_skip_blocks(hdr, dxpl_id, new_iblock, new_iblock->addr, 0, new_entry) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't add skipped blocks to heap's free space")
                    } /* end if */

                    /* Mark new indirect block as modified */
                    if(H5HF_iblock_dirty(new_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

                    /* Mark current indirect block as modified */
                    if(H5HF_iblock_dirty(iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

                    /* Unprotect child indirect block */
                    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock->addr, new_iblock, H5AC__DIRTIED_FLAG) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")
                } /* end else */

                /* Get information about new iterator location */
                if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
                        &next_entry, &iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")

                /* Indicate that we walked down */
                walked_down = TRUE;
            } /* end if */
        } while(walked_down || walked_up);
    } /* end else */

    /* Get information about iterator location */
{
    H5HF_indirect_t *iblock;    /* Pointer to indirect block */
    unsigned next_row;          /* Iterator's next block row */
    unsigned next_entry;        /* Iterator's next block entry */
    size_t next_size;           /* Size of next direct block to create */

    if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL,
            &next_entry, &iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, NULL, "unable to retrieve current block iterator location")
    HDassert(next_row < iblock->nrows);
    next_size = hdr->man_dtable.row_block_size[next_row];

    /* Check for skipping over blocks */
    if(min_dblock_size > next_size) {
HDfprintf(stderr, "%s: Skipping direct block sizes not supported, min_dblock_size = %Zu, next_size = %Zu\n", FUNC, min_dblock_size, next_size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, NULL, "skipping direct block sizes not supported yet")
    } /* end if */

    /* Set entry for new direct block to use */
    *entry_p = next_entry;

    /* Set size of direct block to create */
    *dblock_size = next_size;

    /* Set return value */
    ret_value = iblock;
}

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

    /* Check for range covering indirect blocks */
    if(old_sec_node->u.range.row >= hdr->man_dtable.max_direct_rows) {
HDfprintf(stderr, "%s: Can't handle range sections over indirect blocks yet\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "'range' free space sections over indirect blocks not supported yet")
    } /* end if */

    /* Get a pointer to the indirect block covering the range */
    iblock = old_sec_node->u.range.iblock;
    HDassert(iblock);

#ifdef QAK
HDfprintf(stderr, "%s: cur_entry = %u\n", FUNC, cur_entry);
HDfprintf(stderr, "%s: old_sec_node->u.range.num_entries = %u\n", FUNC, old_sec_node->u.range.num_entries);
#endif /* QAK */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->man_dtable.row_block_size[old_sec_node->u.range.row] = %Hu\n", FUNC, hdr->man_dtable.row_block_size[old_sec_node->u.range.row]);
HDfprintf(stderr, "%s: old_sec_node->sect_addr = %a\n", FUNC, old_sec_node->sect_addr);
#endif /* QAK */
    /* Create direct block of appropriate size */
    if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, cur_entry, (size_t)hdr->man_dtable.row_block_size[old_sec_node->u.range.row], (hsize_t)old_sec_node->sect_info.addr, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Hook direct block up to indirect block */
    iblock->ents[cur_entry].addr = dblock_addr;

    /* Mark indirect block as dirty */
    if(H5HF_iblock_dirty(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

    /* Check for only single block covered in range section */
    if(old_sec_node->u.range.num_entries == 1) {
        /* Drop reference count on indirect block that free section is in */
        if(H5HF_iblock_decr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

        /* Free section structure */
        H5FL_FREE(H5HF_free_section_t, old_sec_node);
    } /* end if */
    else {
        /* Adjust section information */
        old_sec_node->sect_info.addr += hdr->man_dtable.row_block_size[old_sec_node->u.range.row];

        /* Adjust range information */
        old_sec_node->u.range.col++;
        old_sec_node->u.range.num_entries--;

        /* Add section back to free space list */
        if(H5HF_space_add(hdr, dxpl_id, old_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
    } /* end else */

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
HDfprintf(stderr, "%s: iblock->ents[curr_entry].addr = %a\n", FUNC, iblock->ents[curr_entry].addr);
HDfprintf(stderr, "%s: iblock->ents[curr_entry].free_space = %Hu\n", FUNC, iblock->ents[curr_entry].free_space);
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
        if(NULL == (child_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, child_iblock_addr, old_sec_node->u.indirect.indir_nrows, iblock, curr_entry, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Set parent information */
        HDassert(child_iblock->parent == NULL);
        child_iblock->parent = iblock;
        child_iblock->par_entry = curr_entry;
        if(H5HF_iblock_incr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: child_iblock->child_free_space = %Hu\n", FUNC, child_iblock->child_free_space);
#endif /* QAK */

        /* Hook child up to parent indirect block */
        iblock->ents[curr_entry].addr = child_iblock_addr;

        /* Mark parent indirect block as modified */
        if(H5HF_iblock_dirty(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")
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
    if(H5HF_man_dblock_create(dxpl_id, hdr, child_iblock, dblock_entry, (size_t)hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row], (hsize_t)old_sec_node->sect_info.addr, &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Hook direct block up to child indirect block */
    child_iblock->ents[dblock_entry].addr = dblock_addr;

    /* Mark child indirect block as modified */
    if(H5HF_iblock_dirty(child_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")


    /* Create "range" section for other direct blocks in row of child indirect block */

    /* Create free list section node for blocks skipped over */
    if(NULL == (range_sec_node = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set section's information */
    range_sec_node->sect_info.addr = child_iblock->block_off + hdr->man_dtable.row_block_off[old_sec_node->u.indirect.indir_row]
            + hdr->man_dtable.row_block_size[old_sec_node->u.indirect.indir_row];
    range_sec_node->sect_info.size = old_sec_node->sect_info.size;
    range_sec_node->sect_info.cls = &hdr->sect_cls[H5FS_SECT_FHEAP_RANGE];
    range_sec_node->sect_info.state = H5FS_SECT_LIVE;
    range_sec_node->u.range.iblock = child_iblock;
    if(H5HF_iblock_incr(child_iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
    range_sec_node->u.range.row = old_sec_node->u.indirect.indir_row;
    range_sec_node->u.range.col = 1;
    range_sec_node->u.range.num_entries = hdr->man_dtable.cparam.width - 1;

    /* Add new free space to the global list of space */
    if(H5HF_space_add(hdr, dxpl_id, range_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")


    /* Reduce "indirect" section */

    /* Check for only single block covered in range section */
    if(old_sec_node->u.indirect.num_entries == 1) {
        /* Drop reference count on indirect block that free section is in */
        if(H5HF_iblock_decr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

        /* Free section structure */
        H5FL_FREE(H5HF_free_section_t, old_sec_node);
    } /* end if */
    else {
        /* Adjust section information */
        old_sec_node->sect_info.addr += hdr->man_dtable.row_block_size[old_sec_node->u.indirect.row];

        /* Adjust range information */
        old_sec_node->u.indirect.col++;
        old_sec_node->u.indirect.num_entries--;

        /* Add section back to free space list */
        if(H5HF_space_add(hdr, dxpl_id, old_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
    } /* end else */

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
static herr_t
H5HF_man_iblock_create(H5HF_hdr_t *hdr, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p)
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
HDfprintf(stderr, "%s: nrows = %u, max_nrows = %u\n", FUNC, nrows, max_nrows);
#endif /* QAK */
    /* Set info for direct block */
    iblock->rc = 0;
    iblock->parent = NULL;              /* Temporary, except for root indirect block */
    iblock->par_entry = 0;
    iblock->block_off = block_off;
    iblock->nrows = nrows;
    iblock->max_rows = max_rows;
    iblock->dirty = TRUE;

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

