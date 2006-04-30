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
 * Created:		H5HFint.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		"Internal" routines for fractal heaps.
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
#include "H5MMprivate.h"	/* Memory management			*/
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
static herr_t H5HF_man_start_freelist(H5HF_hdr_t *hdr, hid_t dxpl_id);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5HF_free_section_t struct */
H5FL_DEFINE(H5HF_free_section_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_free_section_free_cb
 *
 * Purpose:	Free a free section node
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 13, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_free_section_free_cb(void *_sect, void UNUSED *key, void UNUSED *op_data)
{
    H5HF_free_section_t *sect = (H5HF_free_section_t *)_sect;
    H5HF_indirect_t *iblock = NULL;     /* Indirect block referenced */
    herr_t ret_value = 0;               /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_free_section_free_cb)

    HDassert(sect);

    /* Find indirect block that free section references */
    switch(sect->type) {
        case H5HF_SECT_SINGLE:
            iblock = sect->u.single.parent;
            break;

        case H5HF_SECT_OPAQUE:
            iblock = sect->u.opaque.iblock;
            break;

        case H5HF_SECT_RANGE:
            iblock = sect->u.range.iblock;
            break;

         case H5HF_SECT_INDIRECT:
            iblock = sect->u.indirect.iblock;
            break;
    } /* end switch */

    /* Release indirect block, if there was one */
    if(iblock)
        if(H5HF_iblock_decr(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Release the sections */
    H5FL_FREE(H5HF_free_section_t, sect);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* H5HF_free_section_free_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_start_freelist
 *
 * Purpose:	Start free list for existing heap, by bringing in root direct
 *              or indirect block and it's free space sections.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 25 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_start_freelist(H5HF_hdr_t *hdr, hid_t dxpl_id)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_start_freelist)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Check for empty heap */
    if(!H5F_addr_defined(hdr->man_dtable.table_addr)) {
#ifdef QAK
HDfprintf(stderr, "%s: Empty heap to [not] start\n", FUNC);
#endif /* QAK */
        /* Nothing to do */
        ;
    } /* end if */
    /* Check for root direct block */
    else if(hdr->man_dtable.curr_root_rows == 0) {
        H5HF_direct_t *dblock;              /* Pointer to direct block to query */
        haddr_t dblock_addr;                /* Direct block address */
        size_t dblock_size;                 /* Direct block size */

#ifdef QAK
HDfprintf(stderr, "%s: Root direct block to start\n", FUNC);
#endif /* QAK */
        /* Bring root direct block into memory */
        /* (which will parse it's free list) */
        /* (Could create an "opaque" section for it, but it doesn't seem worthwhile) */
        dblock_addr = hdr->man_dtable.table_addr;
        dblock_size = hdr->man_dtable.cparam.start_block_size;
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, NULL, 0, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Unlock direct block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
        dblock = NULL;
    } /* end if */
    /* Must be a root indirect block */
    else {
        H5HF_indirect_t *iblock;    /* Pointer to root indirect block to query */
        haddr_t iblock_addr;        /* Indirect block's address */

#ifdef QAK
HDfprintf(stderr, "%s: Root indirect block to start\n", FUNC);
#endif /* QAK */
        /* Get indirect block info */
        iblock_addr = hdr->man_dtable.table_addr;

        /* Lock root indirect block */
        /* (which will parse it's free space) */
        if(NULL == (iblock = H5HF_man_iblock_protect(hdr, dxpl_id, iblock_addr, hdr->man_dtable.curr_root_rows, NULL, 0, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Release the root indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        iblock = NULL;
    } /* end else */

    /* Mark heap as being in sync now */
    hdr->freelist_sync = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_start_freelist() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_find
 *
 * Purpose:	Find space for an object in a managed obj. heap
 *
 * Return:	Non-negative on success (with direct block info
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 13 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_find(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t request,
    H5HF_free_section_t **sec_node/*out*/)
{
    htri_t node_found;                  /* Whether an existing free list node was found */
    hbool_t search_again;               /* Whether to search again for another free section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_find)
#ifdef QAK
HDfprintf(stderr, "%s: request = %Zu\n", FUNC, request);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(request > 0);
    HDassert(sec_node);

    /* Search for free space in the globel free list, until a non-opaque node is found */
    do {
        /* Done searching, unless opaque node found */
        search_again = FALSE;

        /* Look for free space in global free list */
        if((node_found = H5HF_flist_find(hdr->flist, request, (void **)sec_node)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap direct block")

        /* Check for opaque section */
        if(node_found) {
            if((*sec_node)->type == H5HF_SECT_OPAQUE) {
                /* Break opaque section down into component sections */
                if(H5HF_man_iblock_alloc_opaque(hdr, dxpl_id, sec_node) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't break up opaque free section")

                /* Search again */
                search_again = TRUE;
            } /* end if */
        } /* end if */
        /* If we didn't find a node, check if we've looked in all the direct blocks in the heap for space */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->freelist_sync = %u\n", FUNC, (unsigned)hdr->freelist_sync);
#endif /* QAK */
            if(!hdr->freelist_sync) {
#ifdef QAK
HDfprintf(stderr, "%s: Start free list for existing blocks in heap\n", FUNC);
#endif /* QAK */
                /* Start freelist for existing heap */
                if(H5HF_man_start_freelist(hdr, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't start fractal heap free list")

                /* Search again */
                search_again = TRUE;
            } /* end if */
        } /* end else */
    } while(search_again);


    /* If we didn't find a node, go make one big enough to hold the requested block */
    if(!node_found) {
#ifdef QAK
HDfprintf(stderr, "%s: Allocate new direct block\n", FUNC);
#endif /* QAK */
        /* Allocate direct block big enough to hold requested size */
        if(H5HF_man_dblock_new(hdr, dxpl_id, request) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create fractal heap direct block")

        /* Request space from the free list */
        /* (Ought to be able to be filled, now) */
        if(H5HF_flist_find(hdr->flist, request, (void **)sec_node) <= 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap direct block")
    } /* end if */
    HDassert(*sec_node);
#ifdef QAK
HDfprintf(stderr, "%s: (*sec_node)->sect_addr = %a\n", FUNC, (*sec_node)->sect_addr);
HDfprintf(stderr, "%s: (*sec_node)->sect_size = %Zu\n", FUNC, (*sec_node)->sect_size);
HDfprintf(stderr, "%s: (*sec_node)->type = %u\n", FUNC, (unsigned)(*sec_node)->type);
#endif /* QAK */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_find() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_insert
 *
 * Purpose:	Insert an object in a managed direct block
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
H5HF_man_insert(H5HF_hdr_t *hdr, hid_t dxpl_id, H5HF_free_section_t *sec_node,
    size_t obj_size, const void *obj, void *id)
{
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block to modify */
    haddr_t dblock_addr;                /* Direct block address */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_insert)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(obj_size > 0);
    HDassert(obj);
    HDassert(id);

    /* Check for indirect section */
    if(sec_node->type == H5HF_SECT_INDIRECT) {
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
HDfprintf(stderr, "%s: sec_node->sect_size = %Zu\n", FUNC, sec_node->sect_size);
HDfprintf(stderr, "%s: sec_node->u.indirect.iblock = %p\n", FUNC, sec_node->u.indirect.iblock);
if(sec_node->u.indirect.iblock)
    HDfprintf(stderr, "%s: sec_node->u.indirect.iblock->addr = %a\n", FUNC, sec_node->u.indirect.iblock->addr);
HDfprintf(stderr, "%s: sec_node->u.indirect.row = %u\n", FUNC, sec_node->u.indirect.row);
HDfprintf(stderr, "%s: sec_node->u.indirect.col = %u\n", FUNC, sec_node->u.indirect.col);
HDfprintf(stderr, "%s: sec_node->u.indirect.num_entries = %u\n", FUNC, sec_node->u.indirect.num_entries);
HDfprintf(stderr, "%s: sec_node->u.indirect.indir_row = %u\n", FUNC, sec_node->u.indirect.indir_row);
HDfprintf(stderr, "%s: sec_node->u.indirect.indir_nrows = %u\n", FUNC, sec_node->u.indirect.indir_nrows);
#endif /* QAK */

        /* Allocate 'single' selection out of 'indirect' selection */
        if(H5HF_man_iblock_alloc_indirect(hdr, dxpl_id, &sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't break up indirect free section")
    } /* end if */
    /* Check for range section */
    else if(sec_node->type == H5HF_SECT_RANGE) {
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
HDfprintf(stderr, "%s: sec_node->sect_size = %Zu\n", FUNC, sec_node->sect_size);
HDfprintf(stderr, "%s: sec_node->u.range.iblock = %p\n", FUNC, sec_node->u.range.iblock);
if(sec_node->u.range.iblock)
    HDfprintf(stderr, "%s: sec_node->u.range.iblock->addr = %a\n", FUNC, sec_node->u.range.iblock->addr);
HDfprintf(stderr, "%s: sec_node->u.range.row = %u\n", FUNC, sec_node->u.range.row);
HDfprintf(stderr, "%s: sec_node->u.range.col = %u\n", FUNC, sec_node->u.range.col);
HDfprintf(stderr, "%s: sec_node->u.range.num_entries = %u\n", FUNC, sec_node->u.range.num_entries);
#endif /* QAK */
        /* Allocate 'single' selection out of 'range' selection */
        if(H5HF_man_iblock_alloc_range(hdr, dxpl_id, &sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't break up range free section")
    } /* end if */

    /* Lock direct block */
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
HDfprintf(stderr, "%s: sec_node->sect_size = %Zu\n", FUNC, sec_node->sect_size);
HDfprintf(stderr, "%s: sec_node->u.single.parent = %p\n", FUNC, sec_node->u.single.parent);
if(sec_node->u.single.parent)
    HDfprintf(stderr, "%s: sec_node->u.single.parent->addr = %a\n", FUNC, sec_node->u.single.parent->addr);
HDfprintf(stderr, "%s: sec_node->u.single.par_entry = %u\n", FUNC, sec_node->u.single.par_entry);
HDfprintf(stderr, "%s: sec_node->u.single.dblock_addr = %a\n", FUNC, sec_node->u.single.dblock_addr);
HDfprintf(stderr, "%s: sec_node->u.single.dblock_size = %Zu\n", FUNC, sec_node->u.single.dblock_size);
#endif /* QAK */
    dblock_addr = sec_node->u.single.dblock_addr;
    if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, sec_node->u.single.dblock_size, sec_node->u.single.parent, sec_node->u.single.par_entry, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")

    /* Insert object into block */

    /* Check for address mapping type */
    if(hdr->addrmap == H5HF_ABSOLUTE) {
        H5HF_direct_free_node_t *node;  /* Block's free list node */
        uint8_t *p;                     /* Temporary pointer to obj info in block */
        size_t obj_off;                 /* Offset of object within block */
        size_t full_obj_size;           /* Size of object including metadata */
        size_t alloc_obj_size;          /* Size of object including metadata & any free space fragment */
        size_t free_obj_size;           /* Size of space to free for object */
        hbool_t whole_node = FALSE;     /* Whether we've used the whole node or not */
        unsigned char free_frag_size;   /* Size of free space fragment */

        /* Locate "local" free list node for section */
/* XXX: Change to using skip list? */
        obj_off = sec_node->sect_addr - dblock->block_off;
        node = dblock->free_list->first;
        while(node->my_offset != obj_off)
            node = node->next;

        /* Compute full object size, with metadata for object */
        full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);

        /* Sanity checks */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->total_man_free = %Hu\n", FUNC, hdr->total_man_free);
HDfprintf(stderr, "%s: dblock->blk_free_space = %Zu\n", FUNC, dblock->blk_free_space);
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */
        HDassert(dblock->blk_free_space >= obj_size);
        HDassert(dblock->free_list);
        HDassert(node->size >= full_obj_size);

        /* Check for using entire node */
        free_frag_size = 0;
#ifdef QAK
HDfprintf(stderr, "%s: node->size = %Zu\n", FUNC, node->size);
#endif /* QAK */
        if(node->size <= (full_obj_size + H5HF_MAN_ABS_DIRECT_FREE_NODE_SIZE(dblock))) {
            /* Set the offset of the object within the block */
            obj_off = node->my_offset;

            /* Check for allocating from first node in list */
            if(node->prev == NULL) {
                /* Make the next node in the free list the list head */
                dblock->free_list->first = node->next;
                dblock->free_list_head = node->next_offset;
            } /* end if */
            else {
                H5HF_direct_free_node_t *prev_node;          /* Pointer to previous free list node for block */

                /* Excise node from list */
                prev_node = node->prev;
                prev_node->next = node->next;
                if(node->next) {
                    H5HF_direct_free_node_t *next_node;          /* Pointer to next free list node for block */

                    next_node = node->next;
                    next_node->prev = prev_node;
                    prev_node->next_offset = next_node->my_offset;
                } /* end if */
                else
                    prev_node->next_offset = 0;
            } /* end if */

            /* Set the free fragment size */
            free_frag_size = (unsigned char )(node->size - full_obj_size);
            whole_node = TRUE;

            /* Drop reference count on parent indirect block of direct block that free section is in */
            HDassert(dblock->parent == NULL ||
                    sec_node->u.single.parent == NULL ||
                    dblock->parent == sec_node->u.single.parent);
            if(sec_node->u.single.parent)
                if(H5HF_iblock_decr(sec_node->u.single.parent) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

            /* Release the memory for the free list node & section */
            H5FL_FREE(H5HF_direct_free_node_t, node);
            H5FL_FREE(H5HF_free_section_t, sec_node);
        } /* end if */
        else {
            /* Allocate object from end of free space node */
            /* (so we don't have to adjust with any other node's info */
            obj_off = (node->my_offset + node->size) - full_obj_size;
            node->size -= full_obj_size;

            /* Adjust information for section node */
            sec_node->sect_size -= full_obj_size;

            /* Re-insert section node onto global list */
            if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")
        } /* end else */

        /* Mark free list as dirty */
        dblock->free_list->dirty = TRUE;

        /* Compute the size of the space to actually allocate */
        /* (includes the metadata for the object & the free space fragment) */
        alloc_obj_size = full_obj_size + free_frag_size;

        /* Compute the size of the free space to reduce */
        /* (does not include the object prefix if this object uses a whole node) */
        free_obj_size = obj_size + (whole_node ? free_frag_size : H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));

#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Zu\n", FUNC, obj_off);
HDfprintf(stderr, "%s: free_frag_size = %Zu\n", FUNC, free_frag_size);
HDfprintf(stderr, "%s: full_obj_size = %Zu\n", FUNC, full_obj_size);
HDfprintf(stderr, "%s: alloc_obj_size = %Zu\n", FUNC, alloc_obj_size);
#endif /* QAK */
        /* Reduce space available in parent block(s) */
        if(H5HF_man_dblock_adj_free(dblock, -(ssize_t)free_obj_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for direct block & parents")

        /* Encode the object in the block */

        /* Point to location for object */
        p = dblock->blk + obj_off;

        /* Encode the free fragment size */
        *p++ = free_frag_size;

        /* Copy the object's data into the heap */
        HDmemcpy(p, obj, obj_size);
        p += obj_size;

#ifdef H5_USING_PURIFY
        /* Zero out the free space fragment */
        HDmemset(p, 0, free_frag_size);
#endif /* H5_USING_PURIFY */
#ifndef NDEBUG
        p += free_frag_size;
#endif /* NDEBUG */

        /* Sanity check */
        HDassert((size_t)(p - (dblock->blk  + obj_off)) == alloc_obj_size);

        /* Set the heap ID for the new object (heap offset & obj length) */
#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */
        UINT64ENCODE_VAR(id, (dblock->block_off + obj_off), hdr->heap_off_size);
        UINT64ENCODE_VAR(id, obj_size, hdr->id_len);
    } /* end if */
    else {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "inserting within mapped managed blocks not supported yet")
    } /* end else */

    /* Update statistics about heap */
    hdr->nobjs++;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

done:
    /* Release the direct block (marked as dirty) */
    if(dblock && H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_read
 *
 * Purpose:	Read an object from a managed heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 17 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_read(H5HF_hdr_t *hdr, hid_t dxpl_id, hsize_t obj_off, size_t obj_len, void *obj)
{
    H5HF_direct_t *dblock;              /* Pointer to direct block to query */
    size_t blk_off;                     /* Offset of object in block */
    uint8_t *p;                         /* Temporary pointer to obj info in block */
    haddr_t dblock_addr;                /* Direct block address */
    size_t dblock_size;                 /* Direct block size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_read)
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu, obj_len = %Zu\n", FUNC, obj_off, obj_len);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(obj_off > 0);
    HDassert(obj);

    /* Check for root direct block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        /* Set direct block info */
        dblock_addr = hdr->man_dtable.table_addr;
        dblock_size = hdr->man_dtable.cparam.start_block_size;

        /* Lock direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, NULL, 0, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")
    } /* end if */
    else {
        haddr_t iblock_addr;            /* Indirect block's address */
        H5HF_indirect_t *iblock;        /* Pointer to indirect block */
        unsigned row, col;              /* Row & column for object's block */
        size_t entry;                   /* Entry of block */

        /* Look up row & column for object */
        if(H5HF_dtable_lookup(&hdr->man_dtable, obj_off, &row, &col) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of object")
#ifdef QAK
HDfprintf(stderr, "%s: row = %u, col = %u\n", FUNC, row, col);
#endif /* QAK */

        /* Set initial indirect block info */
        iblock_addr = hdr->man_dtable.table_addr;
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
#endif /* QAK */

        /* Lock root indirect block */
        if(NULL == (iblock = H5HF_man_iblock_protect(hdr, dxpl_id, iblock_addr, hdr->man_dtable.curr_root_rows, NULL, 0, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Check for indirect block row */
        while(row >= hdr->man_dtable.max_direct_rows) {
            haddr_t new_iblock_addr;       /* New indirect block's address */
            H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
            unsigned nrows;                /* Number of rows in new indirect block */

            /* Compute # of rows in child indirect block */
            nrows = (H5V_log2_gen(hdr->man_dtable.row_block_size[row]) - hdr->man_dtable.first_row_bits) + 1;

            /* Compute indirect block's entry */
            entry = (row * hdr->man_dtable.cparam.width) + col;
#ifdef QAK
HDfprintf(stderr, "%s: entry = %Zu\n", FUNC, entry);
#endif /* QAK */

            /* Locate child indirect block */
            new_iblock_addr = iblock->ents[entry].addr;

            /* Lock new indirect block */
            if(NULL == (new_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, new_iblock_addr, nrows, iblock, entry, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

            /* Release the current indirect block */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

            /* Switch variables to use new indirect block */
            iblock = new_iblock;
            iblock_addr = new_iblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
#endif /* QAK */

            /* Look up row & column in new indirect block for object */
            if(H5HF_dtable_lookup(&hdr->man_dtable, (obj_off - iblock->block_off), &row, &col) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of object")

#ifdef QAK
HDfprintf(stderr, "%s: row = %u, col = %u\n", FUNC, row, col);
#endif /* QAK */
        } /* end while */

        /* Compute direct block's entry */
        entry = (row * hdr->man_dtable.cparam.width) + col;
#ifdef QAK
HDfprintf(stderr, "%s: entry address = %a\n", FUNC, iblock->ents[entry].addr);
#endif /* QAK */

        /* Set direct block info */
        dblock_addr =  iblock->ents[entry].addr;
        dblock_size =  hdr->man_dtable.row_block_size[row];

        /* Lock direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, iblock, entry, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Unlock indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        iblock = NULL;
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a, dblock_size = %Zu\n", FUNC, dblock_addr, dblock_size);
#endif /* QAK */

    /* Compute offset of object within block */
    HDassert((obj_off - dblock->block_off) < (hsize_t)dblock_size);
    blk_off = (size_t)(obj_off - dblock->block_off);

    /* Point to location for object */
    p = dblock->blk + blk_off;

    /* Skip over the free fragment size */
    p++;

    /* Copy the object's data into the heap */
    HDmemcpy(obj, p, obj_len);

    /* Unlock direct block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    dblock = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_read() */

