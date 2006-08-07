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
 * Created:		H5HFdblock.c
 *			Apr 10 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Direct block routines for fractal heaps.
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


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5HF_direct_t struct */
H5FL_DEFINE(H5HF_direct_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_create
 *
 * Purpose:	Allocate & initialize a managed direct block
 *
 * Return:	Pointer to new direct block on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_create(hid_t dxpl_id, H5HF_hdr_t *hdr, H5HF_indirect_t *par_iblock,
    unsigned par_entry, haddr_t *addr_p, H5HF_free_section_t **ret_sec_node)
{
    H5HF_free_section_t *sec_node; /* Pointer to free space section for block */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block */
    haddr_t dblock_addr;                /* Direct block's address */
    size_t free_space;                  /* Free space in new block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_create)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap direct block")

    /* Reset the metadata cache info for the heap header */
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    dblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared heap header")

    /* Set info for direct block */
    if(par_iblock) {
        unsigned par_row = par_entry / hdr->man_dtable.cparam.width;    /* Row for block */

        /* Compute offset & size, based on parent's information */
        dblock->block_off = par_iblock->block_off;
        dblock->block_off += hdr->man_dtable.row_block_off[par_row];
        dblock->block_off += hdr->man_dtable.row_block_size[par_row] * (par_entry % hdr->man_dtable.cparam.width);
        dblock->size = hdr->man_dtable.row_block_size[par_row];
    } /* end if */
    else {
        /* Must be the root direct block */
        dblock->block_off = 0;
        dblock->size = hdr->man_dtable.cparam.start_block_size;
    } /* end else */
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dblock->size);
    free_space = dblock->size - H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);

    /* Allocate buffer for block */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, dblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_USING_PURIFY
HDmemset(dblock->blk, 0, dblock->size);
#endif /* H5_USING_PURIFY */

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (dblock_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)dblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")
#ifdef QAK
HDfprintf(stderr, "%s: direct block address = %a\n", FUNC, dblock_addr);
#endif /* QAK */

    /* Attach to parent indirect block, if there is one */
    dblock->parent = par_iblock;
    if(dblock->parent)
        if(H5HF_man_iblock_attach(dblock->parent, par_entry, dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't attach direct block to parent indirect block")
    dblock->par_entry = par_entry;

    /* Create a new 'single' section for the free space in the block */
    if(NULL == (sec_node = H5HF_sect_single_new((dblock->block_off + H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr)),
            free_space, dblock->parent, dblock->par_entry, dblock_addr, dblock->size)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create section for new direct block's free space")

    /* Check what to do with section node */
    if(ret_sec_node)
        /* Pass back the pointer to the section instead of adding it to the free list */
        *ret_sec_node = sec_node;
    else {
        /* Add new free space to the heap's list of space */
        if(H5HF_space_add(hdr, dxpl_id, sec_node, 0) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")
    } /* end else */

    /* Cache the new fractal heap direct block */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap direct block to cache")

    /* Increase the allocated heap size */
    if(H5HF_hdr_inc_alloc(hdr, dblock->size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

    /* Set the address of of direct block, if requested */
    if(addr_p)
        *addr_p = dblock_addr;

done:
    if(ret_value < 0)
        if(dblock)
            (void)H5HF_cache_dblock_dest(hdr->f, dblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_destroy
 *
 * Purpose:	Destroy a managed direct block
 *
 * Note:	This routine does _not_ insert a range section for the
 *              destroyed direct block, that must be handled by the
 *              caller.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_destroy(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_direct_t *dblock,
    haddr_t dblock_addr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_destroy)
#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
HDfprintf(stderr, "%s: dblock->size = %Zu\n", FUNC, dblock->size);
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(dblock);

    /* Check for root direct block */
    if(hdr->man_dtable.curr_root_rows == 0) {
#ifdef QAK
HDfprintf(stderr, "%s: root direct block\n", FUNC);
#endif /* QAK */
        /* Sanity check */
        HDassert(hdr->man_dtable.table_addr == dblock_addr);
        HDassert(hdr->man_dtable.cparam.start_block_size == dblock->size);

        /* Sanity check block iterator */
        HDassert(!H5HF_man_iter_ready(&hdr->next_block));

        /* Reset root pointer information */
        hdr->man_dtable.table_addr = HADDR_UNDEF;

        /* Reset header information back to "empty heap" state */
        if(H5HF_hdr_empty(hdr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't make heap empty")
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: root indirect block\n", FUNC);
#endif /* QAK */

        /* Adjust heap statistics */
        hdr->man_alloc_size -= dblock->size;

#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
HDfprintf(stderr, "%s: dblock->size = %Zu\n", FUNC, dblock->size);
HDfprintf(stderr, "%s: dblock->parent->nchildren = %u\n", FUNC, dblock->parent->nchildren);
HDfprintf(stderr, "%s: dblock->par_entry = %u\n", FUNC, dblock->par_entry);
HDfprintf(stderr, "%s: hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
#endif /* QAK */
        /* Check for this direct block being the highest in the heap */
        if((dblock->block_off + dblock->size) == hdr->man_iter_off) {
#ifdef QAK
HDfprintf(stderr, "%s: Reversing iterator\n", FUNC);
#endif /* QAK */
            /* Move 'next block' iterator backwards (may shrink heap) */
            if(H5HF_hdr_reverse_iter(hdr, dxpl_id, dblock_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't reverse 'next block' iterator")
        } /* end if */
#if 0
        else {
            unsigned par_row, par_col;  /* Row & column in parent indirect block */

            /* Compute row & column in parent indirect block */
            par_row = dblock->par_entry / hdr->man_dtable.cparam.width;
            par_col = dblock->par_entry % hdr->man_dtable.cparam.width;

            /* Add a 'range' section for the space in the destroyed block */
            if(H5HF_sect_range_add(hdr, dxpl_id, dblock->block_off, hdr->man_dtable.row_tot_dblock_free[par_row],
                    dblock->parent, par_row, par_col, 1) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "can't create range section for direct block being destroyed")
        } /* end else */
#endif /* 0 */

        /* Detach from parent indirect block */
        if(H5HF_man_iblock_detach(dblock->parent, dxpl_id, dblock->par_entry) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTATTACH, FAIL, "can't detach from parent indirect block")
        dblock->parent = NULL;
        dblock->par_entry = 0;
    } /* end else */

    /* Release direct block's disk space */
#ifdef QAK
HDfprintf(stderr, "%s: Before releasing direct block's space, dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
    if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, dblock_addr, (hsize_t)dblock->size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

    /* Remove direct block from metadata cache */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__DIRTIED_FLAG|H5AC__DELETED_FLAG) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    dblock = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_destroy() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_new
 *
 * Purpose:	Create a direct block large enough to hold an object of
 *              the requested size
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 13 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_new(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t request,
    H5HF_free_section_t **ret_sec_node)
{
    haddr_t dblock_addr;            /* Address of new direct block */
    size_t min_dblock_size;         /* Min. size of direct block to allocate */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_new)
#ifdef QAK
HDfprintf(stderr, "%s: request = %Zu\n", FUNC, request);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(request > 0);

    /* Compute the min. size of the direct block needed to fulfill the request */
    if(request < hdr->man_dtable.cparam.start_block_size)
        min_dblock_size = hdr->man_dtable.cparam.start_block_size;
    else {
        min_dblock_size = 1 << (1 + H5V_log2_gen((hsize_t)request));
        HDassert(min_dblock_size <= hdr->man_dtable.cparam.max_direct_size);
    } /* end else */

    /* Adjust the size of block needed to fulfill request, with overhead */
#ifdef QAK
HDfprintf(stderr, "%s: Check 1 - min_dblock_size = %Zu\n", FUNC, min_dblock_size);
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OVERHEAD= %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));
#endif /* QAK */
    if((min_dblock_size - request) < H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr))
        min_dblock_size *= 2;
#ifdef QAK
HDfprintf(stderr, "%s: Check 2 - min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /* Check if this is the first block in the heap */
    if(!H5F_addr_defined(hdr->man_dtable.table_addr) &&
            min_dblock_size == hdr->man_dtable.cparam.start_block_size) {
        /* Create new direct block at starting offset */
        if(H5HF_man_dblock_create(dxpl_id, hdr, NULL, 0, &dblock_addr, ret_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

#ifdef QAK
HDfprintf(stderr, "%s: root direct block, dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */

        /* Point root at new direct block */
        hdr->man_dtable.curr_root_rows = 0;
        hdr->man_dtable.table_addr = dblock_addr;

        /* Extend heap to cover new direct block */
        if(H5HF_hdr_adjust_heap(hdr, (hsize_t)hdr->man_dtable.cparam.start_block_size, (hssize_t)hdr->man_dtable.row_tot_dblock_free[0]) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")
    } /* end if */
    /* Root entry already exists, allocate direct block from root indirect block */
    else {
        H5HF_indirect_t *iblock;    /* Pointer to indirect block to create */
        unsigned next_row;          /* Iterator's next block row */
        unsigned next_entry;        /* Iterator's next block entry */
        size_t next_size;           /* Size of next direct block to create */

#ifdef QAK
HDfprintf(stderr, "%s: before updating iterator, hdr->man_iter_off = %Hu, hdr->man_size = %Hu\n", FUNC, hdr->man_iter_off, hdr->man_size);
#endif /* QAK */
        /* Update iterator to reflect any previous increments as well as allow for requested direct block size */
        if(H5HF_hdr_update_iter(hdr, dxpl_id, min_dblock_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUPDATE, FAIL, "unable to update block iterator")

#ifdef QAK
HDfprintf(stderr, "%s: after updating iterator, hdr->man_iter_off = %Hu\n", FUNC, hdr->man_iter_off);
#endif /* QAK */
        /* Retrieve information about current iterator position */
        if(H5HF_man_iter_curr(&hdr->next_block, &next_row, NULL, &next_entry, &iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to retrieve current block iterator location")
        HDassert(next_row < iblock->nrows);
        next_size = hdr->man_dtable.row_block_size[next_row];

        /* Check for skipping over blocks */
        if(min_dblock_size > next_size) {
HDfprintf(stderr, "%s: Skipping direct block sizes not supported, min_dblock_size = %Zu, next_size = %Zu\n", FUNC, min_dblock_size, next_size);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "skipping direct block sizes not supported yet")
        } /* end if */

        /* Advance "next block" iterator to next direct block entry */
        if(H5HF_hdr_inc_iter(hdr, (hsize_t)next_size, 1) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment 'next block' iterator")

#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->addr = %a\n", FUNC, iblock->addr);
HDfprintf(stderr, "%s: next_entry = %u\n", FUNC, next_entry);
#endif /* QAK */

        /* Create new direct block at current location*/
        if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, next_entry, &dblock_addr, ret_sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_new() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_protect
 *
 * Purpose:	Convenience wrapper around H5AC_protect on a direct block
 *              (Use H5AC_unprotect to unprotect it for now)
 *
 * Return:	Pointer to direct block on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 17 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_direct_t *
H5HF_man_dblock_protect(H5HF_hdr_t *hdr, hid_t dxpl_id, haddr_t dblock_addr,
    size_t dblock_size, H5HF_indirect_t *par_iblock, unsigned par_entry,
    H5AC_protect_t rw)
{
    H5HF_parent_t par_info;         /* Parent info for loading block */
    H5HF_direct_t *dblock;          /* Direct block from cache */
    H5HF_direct_t *ret_value;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_protect)
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a, dblock_size = %Zu\n", FUNC, dblock_addr, dblock_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(H5F_addr_defined(dblock_addr));
    HDassert(dblock_size > 0);

    /* Set up parent info */
    par_info.hdr = hdr;
    par_info.iblock = par_iblock;
    par_info.entry = par_entry;

    /* Protect the direct block */
    if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &dblock_size, &par_info, rw)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap direct block")

    /* Set the return value */
    ret_value = dblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_protect() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_delete
 *
 * Purpose:	Delete a managed direct block
 *
 * Note:	This routine does _not_ modify any indirect block that points
 *              to this direct block, it is assumed that the whole heap is
 *              being deleted.  (H5HF_man_dblock_destroy modifies the indirect
 *              block)
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug  7 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_delete(H5F_t *f, hid_t dxpl_id, haddr_t dblock_addr,
    hsize_t dblock_size)
{
    unsigned dblock_status = 0;         /* Direct block's status in the metadata cache */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_delete)
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a, dblock_size = %Hu\n", FUNC, dblock_addr, dblock_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(dblock_addr));

    /* Check the direct block's status in the metadata cache */
    if(H5AC_get_entry_status(f, dblock_addr, &dblock_status) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to check metadata cache status for direct block")

    /* If the direct block is in the cache, expunge it now */
    if(dblock_status & H5AC_ES__IN_CACHE) {
        /* Sanity checks on direct block */
        HDassert(!(dblock_status & H5AC_ES__IS_PINNED));
        HDassert(!(dblock_status & H5AC_ES__IS_PROTECTED));

#ifdef QAK
HDfprintf(stderr, "%s: Expunging direct block from cache\n", FUNC);
#endif /* QAK */
        /* Evict the direct block from the metadata cache */
        if(H5AC_expunge_entry(f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTREMOVE, FAIL, "unable to remove direct block from cache")
#ifdef QAK
HDfprintf(stderr, "%s: Done expunging direct block from cache\n", FUNC);
#endif /* QAK */
    } /* end if */

    /* Release direct block's disk space */
    if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, dblock_addr, dblock_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_delete() */

