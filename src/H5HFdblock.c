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

/* Declare a free list to manage the H5HF_direct_free_head_t struct */
H5FL_DEFINE(H5HF_direct_free_head_t);

/* Declare a free list to manage the H5HF_direct_free_node_t struct */
H5FL_DEFINE(H5HF_direct_free_node_t);


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
H5HF_man_dblock_create(hid_t dxpl_id, H5HF_t *hdr, H5HF_indirect_t *par_iblock,
    unsigned par_entry, size_t block_size, hsize_t block_off, haddr_t *addr_p,
    H5HF_free_section_t **ret_sec_node)
{
    H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
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
    dblock->shared = hdr;
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
        dblock->par_addr = par_iblock->addr;
        dblock->par_nrows = par_iblock->nrows;
    } /* end if */
    else {
        dblock->par_addr = HADDR_UNDEF;
        dblock->par_nrows = 0;
    } /* end else */
    dblock->par_entry = par_entry;
    dblock->size = block_size;
    dblock->block_off = block_off;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(block_size);
    dblock->free_list_head = H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(hdr, dblock);
    dblock->blk_free_space = block_size - 
            (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, block_size) + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));
    free_space = block_size - dblock->free_list_head;

    /* Allocate buffer for block */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, block_size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_USING_PURIFY
HDmemset(dblock->blk, 0, dblock->size);
#endif /* H5_USING_PURIFY */

    /* Set up free list head */
    if(NULL == (dblock->free_list = H5FL_MALLOC(H5HF_direct_free_head_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list head")
    dblock->free_list->dirty = TRUE;

    /* Set up free list node for all unused space in block */
    if(NULL == (node = H5FL_MALLOC(H5HF_direct_free_node_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list node")

    /* Set node's information */
    node->size = free_space;
    node->my_offset = dblock->free_list_head;
    node->next_offset = 0;
    node->prev = node->next = NULL;

    /* Attach to free list head */
/* XXX: Convert this list to a skip list? */
    dblock->free_list->first = node;

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)block_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

    /* Create free list section node */
    if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set section's information */
    sec_node->sect_addr = block_off + node->my_offset;
    /* (section size is "object size", without the metadata overhead) */
    sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);
    sec_node->type = H5HF_SECT_SINGLE;
    sec_node->u.single.dblock_addr = *addr_p;
    sec_node->u.single.dblock_size = block_size;

    /* Check what to do with section node */
    if(ret_sec_node)
        /* Pass back the pointer to the section instead of adding it to the free list */
        *ret_sec_node = sec_node;
    else {
        /* Add new free space to the global list of space */
        if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
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
 * Function:	H5HF_man_dblock_build_freelist
 *
 * Purpose:	Parse the free list information for a direct block and build
 *              block's free list
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 28 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_build_freelist(H5HF_direct_t *dblock, haddr_t dblock_addr)
{
    H5HF_direct_free_head_t *head = NULL;       /* Pointer to free list head for block */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_build_freelist)

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Allocate head of list */
    if(NULL == (head = H5FL_MALLOC(H5HF_direct_free_head_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list head")
    head->dirty = FALSE;

    /* Check for any nodes on free list */
    if(dblock->free_list_head == 0)
        head->first = NULL;
    else {
        H5HF_t *hdr;                            /* Pointer to shared heap header */
        H5HF_free_section_t *sec_node;     /* Pointer to free list section for block */
        H5HF_direct_free_node_t *node = NULL;   /* Pointer to free list node for block */
        H5HF_direct_free_node_t *prev_node;     /* Pointer to previous free list node for block */
        hsize_t free_len;       /* Length of free list info */
        hsize_t next_off;       /* Next node offset in block */
        hsize_t prev_off;       /* Prev node offset in block */
        uint8_t *p;             /* Temporary pointer to free node info */

        /* Get the pointer to the shared heap info */
        hdr = dblock->shared;

        /* Point to first node in free list */
        p = dblock->blk + dblock->free_list_head;

        /* Decode information for first node on free list */
        UINT64DECODE_VAR(p, free_len, dblock->blk_off_size);
        UINT64DECODE_VAR(p, next_off, dblock->blk_off_size);

        /* Allocate node on list */
        if(NULL == (node = H5FL_MALLOC(H5HF_direct_free_node_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list node")

        /* Set node's information */
        node->size = free_len;
        node->my_offset = dblock->free_list_head;
        node->next_offset = next_off;
        node->prev = node->next = NULL;

        /* Attach to free list head */
        head->first = node;

        /* Set block's free space */
        dblock->blk_free_space = free_len;

        /* Create free list section node */
        if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

        /* Set section's information */
        sec_node->sect_addr = dblock->block_off + node->my_offset;
        /* (section size is "object size", without the metadata overhead) */
        sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);
        sec_node->type = H5HF_SECT_SINGLE;
        sec_node->u.single.dblock_addr = dblock_addr;
        sec_node->u.single.dblock_size = dblock->size;

        /* Add new free space to the global list of space */
        if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")

        /* Set up trailing node pointer */
        prev_node = node;
        prev_off = next_off;

        /* Bring in rest of node on free list */
        while(next_off != 0) {
            /* Point to first node in free list */
            p = dblock->blk + next_off;

            /* Decode information for first node on free list */
            UINT64DECODE_VAR(p, free_len, dblock->blk_off_size);
            UINT64DECODE_VAR(p, next_off, dblock->blk_off_size);

            /* Allocate node on list */
            if(NULL == (node = H5FL_MALLOC(H5HF_direct_free_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list node")

            /* Set node's information */
            node->size = free_len;
            node->my_offset = prev_off;
            node->next_offset = next_off;
            node->prev = prev_node;
            node->next = NULL;

            /* Adjust block's free space */
            dblock->blk_free_space += free_len;

            /* Create free list section node */
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_free_section_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

            /* Set section's information */
            sec_node->sect_addr = dblock->block_off + node->my_offset;
            /* (section size is "object size", without the metadata overhead) */
            sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);
            sec_node->type = H5HF_SECT_SINGLE;
            sec_node->u.single.dblock_addr = dblock_addr;
            sec_node->u.single.dblock_size = dblock->size;

            /* Add new free space to the global list of space */
            if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")

            /* Update trailing info */
            prev_node->next = node;
            prev_off = next_off;

            /* Advance to next node */
            prev_node = node;
        } /* end while */
    } /* end else */

    /* Assign free list head to block */
    dblock->free_list = head;

done:
/* XXX: cleanup on failure? */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_build_freelist() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_dblock_adj_free
 *
 * Purpose:	Adjust the free space for a direct block, and it's parents
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_dblock_adj_free(hid_t dxpl_id, H5HF_direct_t *dblock, ssize_t amt)
{
    H5HF_t *hdr;                        /* Shared heap information */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_adj_free)
#ifdef QAK
HDfprintf(stderr, "%s: amt = %Zd\n", FUNC, amt);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Get the pointer to the shared heap header */
    hdr = dblock->shared;

    /* Adjust space available in block */
    HDassert(amt > 0 || dblock->blk_free_space >= (size_t)-amt);
    dblock->blk_free_space += amt;

    /* Check if the parent info is set */
    if(dblock->parent) {
        H5HF_indirect_t *iblock;    /* Block's parent */

        /* Get the pointer to the shared parent indirect block */
        iblock = dblock->parent;

        /* Adjust this indirect block's child free space */
#ifdef QAK
HDfprintf(stderr, "%s: dblock->par_entry = %u\n", FUNC, dblock->par_entry);
HDfprintf(stderr, "%s: iblock->block_off = %a\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: iblock->child_free_space = %Hu\n", FUNC, iblock->child_free_space);
HDfprintf(stderr, "%s: iblock->ents[dblock->par_entry].free_space = %Hu\n", FUNC, iblock->ents[dblock->par_entry].free_space);
#endif /* QAK */
        HDassert(amt > 0 || iblock->ents[dblock->par_entry].free_space >= (hsize_t)-amt);
        iblock->ents[dblock->par_entry].free_space += amt;
        HDassert(amt > 0 || iblock->child_free_space >= (hsize_t)-amt);
        iblock->child_free_space += amt;

        /* Mark indirect block as dirty */
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

        /* Modify the free space in parent block(s) */
        while(iblock->parent) {
            size_t par_entry;            /* Entry in parent */

            /* Get the pointer to the shared parent indirect block */
            par_entry = iblock->par_entry;
            iblock = iblock->parent;
            HDassert(iblock);

            /* Adjust this indirect block's child free space */
            HDassert(amt > 0 || iblock->ents[par_entry].free_space >= (hsize_t)-amt);
            iblock->ents[par_entry].free_space += amt;
            HDassert(amt > 0 || iblock->child_free_space >= (hsize_t)-amt);
            iblock->child_free_space += amt;

            /* Mark indirect block as dirty */
            if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")
        } /* end while */
    } /* end if */

    /* Update shared heap free space header */
    HDassert(amt > 0 || hdr->total_man_free >= (hsize_t)-amt);
    hdr->total_man_free += amt;
#ifdef QAK
HDfprintf(stderr, "%s: hdr->total_man_free = %Hu\n", FUNC, hdr->total_man_free);
#endif /* QAK */

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(dxpl_id, hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_adj_free() */


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
H5HF_man_dblock_new(H5HF_t *hdr, hid_t dxpl_id, size_t request)
{
    haddr_t dblock_addr;                /* Address of new direct block */
    size_t dblock_size;                 /* Size of new direct block */
    size_t min_dblock_size;             /* Min. size of direct block to allocate */
    herr_t ret_value = SUCCEED;         /* Return value */

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
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, hdr->man_dtable.cparam.start_block_size));
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));
#endif /* QAK */
    if((min_dblock_size - request) < (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, min_dblock_size)
            + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr)))
        min_dblock_size *= 2;
#ifdef QAK
HDfprintf(stderr, "%s: Check 2 - min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /* Check if this is the first block in the heap */
    if(!H5F_addr_defined(hdr->man_dtable.table_addr) &&
            min_dblock_size == hdr->man_dtable.cparam.start_block_size) {
        /* Create new direct block at starting offset */
        dblock_size = hdr->man_dtable.cparam.start_block_size;
        if(H5HF_man_dblock_create(dxpl_id, hdr, NULL, 0, dblock_size, (hsize_t)0, &dblock_addr, NULL) < 0)
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
        haddr_t iblock_addr;        /* Indirect block's address */
        size_t dblock_entry;        /* Direct entry for new direct block */
        hsize_t dblock_off;         /* Direct block offset in heap address space */

        /* Find indirect block with room for block of correct size */
        if(NULL == (iblock = H5HF_man_iblock_place_dblock(hdr, dxpl_id, min_dblock_size, &iblock_addr, &dblock_entry, &dblock_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to locate indirect block with space for direct block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
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
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")

        /* Release the indirect block (marked as dirty) */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_new() */

