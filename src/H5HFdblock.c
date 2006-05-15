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
    unsigned par_entry, size_t block_size, hsize_t block_off, haddr_t *addr_p,
    H5HF_free_section_t **ret_sec_node)
{
    H5HF_free_section_t *sec_node; /* Pointer to free list section for block */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block */
    size_t free_space;                  /* Free space in new block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_create)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(block_size > 0);
    HDassert(addr_p);

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
#ifdef QAK
HDfprintf(stderr, "%s: size = %Zu, block_off = %Hu\n", FUNC, block_size, block_off);
#endif /* QAK */
    dblock->parent = par_iblock;
    if(dblock->parent) {
        if(H5HF_iblock_incr(par_iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
    } /* end if */
    dblock->par_entry = par_entry;
    dblock->size = block_size;
    dblock->block_off = block_off;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(block_size);
    free_space = block_size - H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);

    /* Allocate buffer for block */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, block_size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_USING_PURIFY
HDmemset(dblock->blk, 0, dblock->size);
#endif /* H5_USING_PURIFY */

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)block_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

    /* Create free space section node */
    if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set section's information */
    sec_node->sect_info.addr = block_off + H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr);
    sec_node->sect_info.size = free_space;
    sec_node->sect_info.cls = &hdr->sect_cls[H5FS_SECT_FHEAP_SINGLE];
    sec_node->sect_info.state = H5FS_SECT_LIVE;
    sec_node->u.single.parent = dblock->parent;
    if(dblock->parent) {
        if(H5HF_iblock_incr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared indirect block")
    } /* end if */
    sec_node->u.single.par_entry = dblock->par_entry;
    sec_node->u.single.dblock_addr = *addr_p;
    sec_node->u.single.dblock_size = block_size;

    /* Check what to do with section node */
    if(ret_sec_node)
        /* Pass back the pointer to the section instead of adding it to the free list */
        *ret_sec_node = sec_node;
    else {
        /* Add new free space to the global list of space */
        if(H5HF_space_add(hdr, dxpl_id, sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")
    } /* end else */

    /* Cache the new fractal heap direct block */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, *addr_p, dblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap direct block to cache")

done:
    if(ret_value < 0)
        if(dblock)
            (void)H5HF_cache_dblock_dest(hdr->f, dblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_create() */


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
H5HF_man_dblock_new(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t request)
{
    haddr_t dblock_addr;            /* Address of new direct block */
    hsize_t dblock_off;             /* Direct block offset in heap address space */
    size_t dblock_size;             /* Size of new direct block */
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
        dblock_size = hdr->man_dtable.cparam.start_block_size;
        dblock_off = 0;
        if(H5HF_man_dblock_create(dxpl_id, hdr, NULL, 0, dblock_size, dblock_off, &dblock_addr, NULL) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */

        /* Point root at new direct block */
        hdr->man_dtable.curr_root_rows = 0;
        hdr->man_dtable.table_addr = dblock_addr;

        /* Extend heap to cover new direct block */
        if(H5HF_hdr_extend_heap(hdr, (hsize_t)dblock_size, hdr->man_dtable.row_dblock_free[0]) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTEXTEND, FAIL, "can't increase space to cover root direct block")
    } /* end if */
    /* Root entry already exists, go get indirect block for new direct block */
    else {
        H5HF_indirect_t *iblock;    /* Pointer to indirect block to create */
        size_t dblock_entry;        /* Direct entry for new direct block */

        /* Find indirect block with room for block of correct size */
        if(NULL == (iblock = H5HF_man_iblock_place_dblock(hdr, dxpl_id, min_dblock_size, &dblock_entry, &dblock_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to locate indirect block with space for direct block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock = %p\n", FUNC, iblock);
HDfprintf(stderr, "%s: iblock->addr = %a\n", FUNC, iblock->addr);
HDfprintf(stderr, "%s: dblock_entry = %Zu\n", FUNC, dblock_entry);
HDfprintf(stderr, "%s: dblock_size = %Zu\n", FUNC, dblock_size);
#endif /* QAK */

        /* Compute the direct block's offset in the heap's address space */
        dblock_off = iblock->block_off;
        dblock_off += hdr->man_dtable.row_block_off[dblock_entry / hdr->man_dtable.cparam.width];
        dblock_off += hdr->man_dtable.row_block_size[dblock_entry / hdr->man_dtable.cparam.width] * (dblock_entry % hdr->man_dtable.cparam.width);

        /* Create new direct block at current location*/
        if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, dblock_entry, dblock_size, dblock_off, &dblock_addr, NULL) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */

        /* Point indirect block at new direct block */
        iblock->ents[dblock_entry].addr = dblock_addr;

        /* Mark indirect block as modified */
        if(H5HF_iblock_dirty(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")
    } /* end else */

    /* Advance the allocated heap size/new block iterator */
    if(H5HF_hdr_inc_alloc(hdr, dblock_off + dblock_size, 1) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTRELEASE, FAIL, "can't increase allocated heap size")

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

