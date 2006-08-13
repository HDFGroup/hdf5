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
 * Created:		H5HFman.c
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


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5HF_man_locate_block
 *
 * Purpose:	Locate a block in a managed heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May  8 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_locate_block(H5HF_hdr_t *hdr, hid_t dxpl_id, hsize_t obj_off,
    hbool_t locate_indirect,
    H5HF_indirect_t **ret_iblock, unsigned *ret_entry,
    H5AC_protect_t rw)
{
    haddr_t iblock_addr;            /* Indirect block's address */
    H5HF_indirect_t *iblock;        /* Pointer to indirect block */
    unsigned bot_row, top_row;      /* Bottom & top acceptable rows */
    unsigned row, col;              /* Row & column for object's block */
    unsigned entry;                 /* Entry of block */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_locate_block)
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu\n", FUNC, obj_off);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(hdr->man_dtable.curr_root_rows);   /* Only works for heaps with indirect root block */

    /* Set up target bottom & top rows */
    if(locate_indirect) {
        bot_row = hdr->man_dtable.max_direct_rows;
        top_row = hdr->man_dtable.max_direct_rows + H5V_log2_of2(hdr->man_dtable.cparam.width);
    } /* end if */
    else {
        bot_row = 0;
        top_row = hdr->man_dtable.max_direct_rows - 1;
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: bot_row = %u, top_row = %u\n", FUNC, bot_row, top_row);
#endif /* QAK */

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
    if(NULL == (iblock = H5HF_man_iblock_protect(hdr, dxpl_id, iblock_addr, hdr->man_dtable.curr_root_rows, NULL, 0, rw)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

    /* Check for indirect block row */
    while(row < bot_row || row > top_row) {
        haddr_t new_iblock_addr;       /* New indirect block's address */
        H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
        unsigned nrows;                /* Number of rows in new indirect block */
        unsigned cache_flags = H5AC__NO_FLAGS_SET;      /* Flags for unprotecting parent indirect block */

        /* Compute # of rows in child indirect block */
        nrows = (H5V_log2_gen(hdr->man_dtable.row_block_size[row]) - hdr->man_dtable.first_row_bits) + 1;

        /* Compute indirect block's entry */
        entry = (row * hdr->man_dtable.cparam.width) + col;
#ifdef QAK
HDfprintf(stderr, "%s: entry = %Zu\n", FUNC, entry);
#endif /* QAK */

        /* Locate child indirect block */
        new_iblock_addr = iblock->ents[entry].addr;

        /* Check if we need to (re-)create the child indirect block */
        if(!H5F_addr_defined(new_iblock_addr)) {
            if(H5HF_man_iblock_create(hdr, dxpl_id, iblock, entry, nrows, nrows, &new_iblock_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap indirect block")

            /* Indicate that the parent indirect block was modified */
            cache_flags |= H5AC__DIRTIED_FLAG;
        } /* end if */

        /* Lock new indirect block */
        if(NULL == (new_iblock = H5HF_man_iblock_protect(hdr, dxpl_id, new_iblock_addr, nrows, iblock, entry, rw)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Release the current indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, cache_flags) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

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

    /* Set return parameters */
    if(ret_entry)
        *ret_entry = (row * hdr->man_dtable.cparam.width) + col;
    *ret_iblock = iblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_locate_block() */


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
H5HF_man_insert(H5HF_hdr_t *hdr, hid_t dxpl_id, size_t obj_size, const void *obj,
    void *_id)
{
    H5HF_free_section_t *sec_node;      /* Pointer to free space section */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block to modify */
    haddr_t dblock_addr = HADDR_UNDEF;  /* Direct block address */
    uint8_t *id = (uint8_t *)_id;       /* Pointer to ID buffer */
    size_t blk_off;                     /* Offset of object within block */
    htri_t node_found;                  /* Whether an existing free list node was found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_insert)
#ifdef QAK
HDfprintf(stderr, "%s: obj_size = %Zu\n", FUNC, obj_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(obj_size > 0);
    HDassert(obj);
    HDassert(id);

    /* Look for free space */
    if((node_found = H5HF_space_find(hdr, dxpl_id, (hsize_t)obj_size, &sec_node)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap")
#ifdef QAK
HDfprintf(stderr, "%s: After H5HF_space_find(), node_found = %t\n", FUNC, node_found);
#endif /* QAK */

    /* If we didn't find a node, go create a direct block big enough to hold the requested block */
    if(!node_found)
        /* Allocate direct block big enough to hold requested size */
        if(H5HF_man_dblock_new(hdr, dxpl_id, obj_size, &sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create fractal heap direct block")

    /* Check for row section */
    if(sec_node->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ||
            sec_node->sect_info.type == H5HF_FSPACE_SECT_NORMAL_ROW) {
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_info.addr = %a\n", FUNC, sec_node->sect_info.addr);
HDfprintf(stderr, "%s: sec_node->sect_info.size = %Hu\n", FUNC, sec_node->sect_info.size);
HDfprintf(stderr, "%s: sec_node->sect_info.type = %s\n", FUNC, (sec_node->sect_info.type == H5HF_FSPACE_SECT_FIRST_ROW ? "H5HF_FSPACE_SECT_FIRST_ROW" : "H5HF_FSPACE_SECT_NORMAL_ROW"));
HDfprintf(stderr, "%s: sec_node->u.row.under = %p\n", FUNC, sec_node->u.row.under);
HDfprintf(stderr, "%s: sec_node->u.row.row = %u\n", FUNC, sec_node->u.row.row);
HDfprintf(stderr, "%s: sec_node->u.row.col = %u\n", FUNC, sec_node->u.row.col);
HDfprintf(stderr, "%s: sec_node->u.row.num_entries = %u\n", FUNC, sec_node->u.row.num_entries);
#endif /* QAK */

        /* Allocate 'single' selection out of 'row' selection */
        if(H5HF_man_iblock_alloc_row(hdr, dxpl_id, &sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't break up row section")
    } /* end if */
    HDassert(sec_node->sect_info.type == H5HF_FSPACE_SECT_SINGLE);

    /* Check for 'single' section being serialized */
    if(sec_node->sect_info.state == H5FS_SECT_SERIALIZED) {
        if(H5HF_sect_single_revive(hdr, dxpl_id, sec_node) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't revive single free section")
    } /* end if */
    HDassert(sec_node->sect_info.state == H5FS_SECT_LIVE);

    /* Lock direct block */
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_info.addr = %a\n", FUNC, sec_node->sect_info.addr);
HDfprintf(stderr, "%s: sec_node->sect_info.size = %Hu\n", FUNC, sec_node->sect_info.size);
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

    /* Get the offset of the object within the block */
    blk_off = sec_node->sect_info.addr - dblock->block_off;
#ifdef QAK
HDfprintf(stderr, "%s: blk_off = %Zu\n", FUNC, blk_off);
#endif /* QAK */

    /* Sanity checks */
#ifdef QAK
HDfprintf(stderr, "%s: hdr->total_man_free = %Hu\n", FUNC, hdr->total_man_free);
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */
    HDassert(sec_node->sect_info.size >= obj_size);

    /* Reduce (& possibly re-add) single section */
    if(H5HF_sect_single_reduce(hdr, dxpl_id, sec_node, obj_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTSHRINK, FAIL, "can't reduce single section node")

    /* Reduce space available in heap */
    if(H5HF_hdr_adj_free(hdr, -(ssize_t)obj_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for heap")

    /* Encode the object in the block */
    {
        uint8_t *p;                         /* Temporary pointer to obj info in block */

        /* Point to location for object */
        p = dblock->blk + blk_off;

        /* Copy the object's data into the heap */
        HDmemcpy(p, obj, obj_size);
        p += obj_size;

        /* Sanity check */
        HDassert((size_t)(p - (dblock->blk + blk_off)) == obj_size);
    } /* end block */

    /* Set the heap ID for the new object (heap offset & obj length) */
#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */
    H5HF_MAN_ID_ENCODE(id, hdr, (dblock->block_off + blk_off), obj_size);
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu, obj_len = %Zu\n", FUNC, (dblock->block_off + blk_off), obj_size);
#endif /* QAK */

    /* Update statistics about heap */
    hdr->man_nobjs++;

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
H5HF_man_read(H5HF_hdr_t *hdr, hid_t dxpl_id, const uint8_t *id, void *obj)
{
    H5HF_direct_t *dblock;              /* Pointer to direct block to query */
    hsize_t obj_off;                    /* Object's offset in heap */
    size_t obj_len;                     /* Object's length in heap */
    size_t blk_off;                     /* Offset of object in block */
    uint8_t *p;                         /* Temporary pointer to obj info in block */
    haddr_t dblock_addr;                /* Direct block address */
    size_t dblock_size;                 /* Direct block size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_read)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);
    HDassert(obj);

    /* Decode the object offset within the heap & it's length */
    UINT64DECODE_VAR(id, obj_off, hdr->heap_off_size);
    UINT64DECODE_VAR(id, obj_len, hdr->heap_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu, obj_len = %Zu\n", FUNC, obj_off, obj_len);
#endif /* QAK */
    HDassert(obj_off > 0);
    HDassert(obj_len > 0);

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
        H5HF_indirect_t *iblock;        /* Pointer to indirect block */
        unsigned entry;                 /* Entry of block */

        /* Look up indirect block containing direct block */
        if(H5HF_man_locate_block(hdr, dxpl_id, obj_off, FALSE, &iblock, &entry, H5AC_READ) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")
#ifdef QAK
HDfprintf(stderr, "%s: entry address = %a\n", FUNC, iblock->ents[entry].addr);
#endif /* QAK */

        /* Set direct block info */
        dblock_addr =  iblock->ents[entry].addr;
        dblock_size =  hdr->man_dtable.row_block_size[entry / hdr->man_dtable.cparam.width];

        /* Lock direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, iblock, entry, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Unlock indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
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

    /* Copy the object's data into the heap */
    HDmemcpy(obj, p, obj_len);

    /* Unlock direct block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    dblock = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_read() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_remove
 *
 * Purpose:	Remove an object from a managed heap
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		May 15 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_remove(H5HF_hdr_t *hdr, hid_t dxpl_id, const uint8_t *id)
{
    H5HF_free_section_t *sec_node;      /* Pointer to free space section for block */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block to query */
    hsize_t obj_off;                    /* Object's offset in heap */
    size_t obj_len;                     /* Object's length in heap */
    haddr_t dblock_addr;                /* Direct block address */
    size_t dblock_size;                 /* Direct block size */
    size_t blk_off;                     /* Offset of object in block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_remove)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(id);

    /* Decode the object offset within the heap & it's length */
#ifdef QAK
HDfprintf(stderr, "%s: fh->hdr->heap_off_size = %u, fh->hdr->heap_len_size = %u\n", FUNC, (unsigned)fh->hdr->heap_off_size, (unsigned)fh->hdr->heap_len_size);
#endif /* QAK */
    UINT64DECODE_VAR(id, obj_off, hdr->heap_off_size);
    UINT64DECODE_VAR(id, obj_len, hdr->heap_len_size);
#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Hu, obj_len = %Zu\n", FUNC, obj_off, obj_len);
#endif /* QAK */
    HDassert(obj_off > 0);
    HDassert(obj_len > 0);

    /* Check for bad offset or length */
    if(obj_off > hdr->man_size)
        HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "fractal heap object offset too large")
    if(obj_len > hdr->man_dtable.cparam.max_direct_size)
        HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "fractal heap object size too large for direct block")
    if(obj_len > hdr->max_man_size)
        HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "fractal heap object should be standalone")

    /* Check for root direct block */
    if(hdr->man_dtable.curr_root_rows == 0) {
#ifdef QAK
HDfprintf(stderr, "%s: direct root block\n", FUNC);
#endif /* QAK */
        /* Set direct block info */
        dblock_addr = hdr->man_dtable.table_addr;
        dblock_size = hdr->man_dtable.cparam.start_block_size;

        /* Lock direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, NULL, 0, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")
    } /* end if */
    else {
        H5HF_indirect_t *iblock;        /* Pointer to indirect block */
        unsigned entry;                 /* Entry of block */

#ifdef QAK
HDfprintf(stderr, "%s: indirect root block\n", FUNC);
#endif /* QAK */
        /* Look up indirect block containing direct block */
        if(H5HF_man_locate_block(hdr, dxpl_id, obj_off, FALSE, &iblock, &entry, H5AC_WRITE) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of section")
#ifdef QAK
HDfprintf(stderr, "%s: entry address = %a\n", FUNC, iblock->ents[entry].addr);
#endif /* QAK */

        /* Set direct block info */
        dblock_addr =  iblock->ents[entry].addr;
        dblock_size =  hdr->man_dtable.row_block_size[entry / hdr->man_dtable.cparam.width];

        /* Check for offset of invalid direct block */
        if(!H5F_addr_defined(dblock_addr)) {
            /* Unlock indirect block */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

            HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "fractal heap ID not in allocated direct block")
        } /* end if */

        /* Lock direct block */
        if(NULL == (dblock = H5HF_man_dblock_protect(hdr, dxpl_id, dblock_addr, dblock_size, iblock, entry, H5AC_WRITE))) {
            /* Unlock indirect block */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")
        } /* end if */

        /* Unlock indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        iblock = NULL;
    } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a, dblock_size = %Zu\n", FUNC, dblock_addr, dblock_size);
#endif /* QAK */

    /* Compute offset of object within block */
    HDassert((obj_off - dblock->block_off) < (hsize_t)dblock_size);
    blk_off = (size_t)(obj_off - dblock->block_off);
#ifdef QAK
HDfprintf(stderr, "%s: blk_off = %Zu\n", FUNC, blk_off);
#endif /* QAK */

    /* Check for object's offset in the direct block prefix information */
    if(blk_off < H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr))
        HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "object located in prefix of direct block")

    /* Check for object's length overrunning the end of the direct block */
    if((blk_off + obj_len) > dblock_size)
        HGOTO_ERROR(H5E_HEAP, H5E_BADRANGE, FAIL, "object overruns end of direct block")

    /* Create free space section node */
    if(NULL == (sec_node = H5HF_sect_single_new(obj_off, obj_len,
            dblock->parent, dblock->par_entry, dblock_addr, dblock_size)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create section for direct block's free space")

    /* Unlock direct block */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    dblock = NULL;

    /* Update statistics about heap */
    hdr->man_nobjs--;

    /* Reduce space available in heap */
    if(H5HF_hdr_adj_free(hdr, (ssize_t)obj_len) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for heap")

    /* Return free space to the heap's list of space */
    if(H5HF_space_add(hdr, dxpl_id, sec_node, H5FS_ADD_RETURNED_SPACE) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")

done:
    if(ret_value < 0) {
        /* Unlock direct block */
        if(dblock && H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_remove() */

