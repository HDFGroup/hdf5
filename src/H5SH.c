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
 * Created:		H5SH.c
 *			Mar  9 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement "segmented heaps".  These heaps are
 *
 *-------------------------------------------------------------------------
 */

#define H5SH_PACKAGE		/*suppress error about including H5SHpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTprivate.h"	/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5SHpkg.h"		/* Segmented heap			*/

/* Local macros */

/* Local typedefs */

/* Information needed for extending a heap block during an allocation */
typedef struct {
    H5F_t *f;                   /* File to search for space within */
    hid_t dxpl_id;              /* DXPL for allocation operation */
    haddr_t bt_free_addr;       /* Location of free space B-tree */
    H5FD_mem_t file_mem_type;   /* Type of file memory being used */
    hsize_t max_extend_size;    /* Maximum size of a block to consider for extention */
    hsize_t space_needed;       /* Amount of space to allocate */
    haddr_t loc;                /* Location of space allocated */
    haddr_t extend_addr;        /* Extended space address */
    hsize_t extend_size;        /* Extended space size */
} H5SH_extend_t;

/* Local prototypes */

/* Package variables */

/* Declare a free list to manage the H5SH_t struct */
H5FL_DEFINE(H5SH_t);

/* Static variables */


/*-------------------------------------------------------------------------
 * Function:	H5SH_create
 *
 * Purpose:	Creates a new empty segmented heap in the file.
 *
 * Return:	Non-negative on success (with address of new segmented heap
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 23 2005
 *
 * Changes:	John Mainzer -- 6/7/05
 *		Removed code modifying the is_dirty field of the cache
 *		info.  Management of this field is in the process of
 *		being moved to the H5C code.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SH_create(H5F_t *f, hid_t dxpl_id, haddr_t *addr_p, H5SH_data_type_t heap_type,
    hsize_t min_size, hsize_t max_extend_size)
{
    H5SH_t *sh = NULL;          /* Segmented heap info */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5SH_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(min_size > 0);
    HDassert(max_extend_size >= min_size);

    /*
     * Allocate file and memory data structures.
     */
    if (NULL == (sh = H5FL_CALLOC(H5SH_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for segmented heap info")

    /* Assign internal information */
    sh->heap_type = heap_type;
    if(sh->heap_type == H5SH_RAW)
        sh->file_mem_type = H5FD_MEM_DRAW;
    else
        sh->file_mem_type = H5FD_MEM_SHEAP_BLOCK;
    sh->min_size = min_size;
    sh->max_extend_size = max_extend_size;

    /* Allocate space for the header on disk */
    if (HADDR_UNDEF == (*addr_p = H5MF_alloc(f, H5FD_MEM_SHEAP_HDR, dxpl_id, (hsize_t)H5SH_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for segmented heap info")

    /* Create a block tracker for storing heap block info */
    if (H5BT_create(f, dxpl_id, &sh->bt_heap_addr/*out*/) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create block tracker for storing heap block info")

    /* Create a block tracker for storing free space info */
    if (H5BT_create(f, dxpl_id, &sh->bt_free_addr/*out*/) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't create block tracker for storing free space info")

    /* Cache the new segmented heap node */
    if (H5AC_set(f, dxpl_id, H5AC_SGHP, *addr_p, sh, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add segmented heap info to cache")

done:
    if (ret_value<0) {
	if (sh)
            (void)H5SH_cache_dest(f, sh);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_create() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_alloc_extend_cb
 *
 * Purpose:	Extend an existing heap block if possible
 *
 * Return:	Non-negative on success (setting flag to indicate block
 *              was extended), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 25 2005
 *
 *-------------------------------------------------------------------------
 */
static int
H5SH_alloc_extend_cb(const H5BT_blk_info_t *record, void *_op_data)
{
    H5SH_extend_t *ex_info = (H5SH_extend_t *)_op_data;
    htri_t extendable;          /* Is  block extendable */
    int ret_value;              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5SH_alloc_extend_cb)

    /* Check if we are allowed to extend this heap block */
    if(record->len < ex_info->max_extend_size) {
        H5BT_blk_info_t free_block;     /* Block info for free space */
        hsize_t space_needed;           /* Space needed to extend block */
        hbool_t use_free_space = FALSE; /* Flag to indicate that free space should be used */

        /* Set the amount of space needed */
        space_needed = ex_info->space_needed;

        /* Find first free space block before end of extendable block */
        free_block.addr = HADDR_UNDEF;
        if(H5BT_neighbor(ex_info->f, ex_info->dxpl_id, ex_info->bt_free_addr,
                H5BT_COMPARE_LESS, (record->addr + record->len + 1),
                &free_block) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, H5BT_ITER_ERROR, "can't search for free space")

        /* Check if we can use free space for part of block to allocate */
        if(H5F_addr_defined(free_block.addr) &&
                (free_block.addr + free_block.len) == (record->addr + record->len)) {
            use_free_space = TRUE;
            space_needed -= free_block.len;
        } /* end if */

        /* Check if this heap block can be extended */
        if((extendable = H5MF_can_extend(ex_info->f, ex_info->file_mem_type,
                record->addr, record->len, space_needed)) == TRUE) {

            /* Extend heap block in the file */
            if(H5MF_extend(ex_info->f, ex_info->file_mem_type,
                    record->addr, record->len, space_needed) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, H5BT_ITER_ERROR, "can't extend extendable heap block?")

            /* Set information to return from iterator */
            if(use_free_space) {
                /* Remove block from free space tracker */
                if(H5BT_remove(ex_info->f, ex_info->dxpl_id, ex_info->bt_free_addr,
                        free_block.addr, free_block.len) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDELETE, FAIL, "can't remove free space block")

                ex_info->loc = free_block.addr;
            } /* end if */
            else
                ex_info->loc = record->addr + record->len;

            /* Set information about extension to block */
            ex_info->extend_addr = record->addr + record->len;
            ex_info->extend_size = space_needed;

            ret_value = H5BT_ITER_STOP;
        } /* end if */
        else if(extendable == FALSE)
            ret_value = H5BT_ITER_CONT;
        else
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPARE, H5BT_ITER_ERROR, "can't check if heap block is extendable")
    } /* end if */
    else
        ret_value = H5BT_ITER_CONT;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_alloc_extend_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5SH_alloc
 *
 * Purpose:	Allocate space for a new object in a segmented heap
 *
 * Return:	Non-negative on success (with address of new object
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 25 2005
 *
 * Modifications:
 *
 *              John Mainzer, 6/16/05
 *              Modified the function to use the new dirtied parameter of
 *              of H5AC_unprotect() instead of modifying the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5SH_alloc(H5F_t *f, hid_t dxpl_id, haddr_t addr, hsize_t size, haddr_t *obj_addr_p)
{
    H5SH_t *sh = NULL;          /* Segmented heap info */
    haddr_t free_addr;          /* Address of free block */
    hsize_t free_len;           /* Address of free block */
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(H5SH_alloc, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);

    /* Look up the segmented heap */
    if (NULL == (sh = H5AC_protect(f, dxpl_id, H5AC_SGHP, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load segmented heap info")

    /* Check for block of at least 'size' bytes in free list */
    free_addr = HADDR_UNDEF;
    if (H5BT_locate(f, dxpl_id, sh->bt_free_addr, size, &free_addr, &free_len) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "error searching segmented heap free list")

    /* Check for finding large enough free block */
    if (H5F_addr_defined(free_addr)) {
        /* Remove block from free list */
        if (H5BT_remove(f, dxpl_id, sh->bt_free_addr, free_addr, size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDELETE, FAIL, "can't remove block")

        /* Set address to return */
        *obj_addr_p = free_addr;
    } /* end if */
    else {
        H5SH_extend_t ex_info;

        /* Iterate over the existing heap blocks, to check for extending one */
        ex_info.f = f;
        ex_info.dxpl_id = dxpl_id;
        ex_info.file_mem_type = sh->file_mem_type;
        ex_info.bt_free_addr = sh->bt_free_addr;
        ex_info.max_extend_size = sh->max_extend_size;
        ex_info.space_needed = size;
        ex_info.loc = HADDR_UNDEF;
        if(H5BT_iterate(f, dxpl_id, sh->bt_heap_addr, H5SH_alloc_extend_cb, &ex_info) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_NOTFOUND, FAIL, "can't iterate over heap blocks")

        /* Check if we managed to extend a heap block */
        if(H5F_addr_defined(ex_info.loc)) {
            if(H5BT_insert(f, dxpl_id, sh->bt_heap_addr, ex_info.extend_addr, ex_info.extend_size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "can't extend tracked block")

            /* Set address to return */
            *obj_addr_p = ex_info.loc;
        } /* end if */
        else {
            haddr_t block_addr;         /* Address of new heap block */
            hsize_t block_size;         /* Size of heap block */

            /* Determine how large to make the heap block initially */
            if(size <= sh->min_size)
                block_size = sh->min_size;
            else
                block_size = size;

            /* There's no internal or external space available, allocate a new heap block */
            if(HADDR_UNDEF == (block_addr = H5MF_alloc(f, sh->file_mem_type, dxpl_id, sh->min_size)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "failed to allocate space for new heap block")

            /* Add heap block to tracker */
            if(H5BT_insert(f, dxpl_id, sh->bt_heap_addr, block_addr, block_size) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "can't insert heap block")

            /* Check if there is free space */
            if(size < sh->min_size) {
                /* Add free space to tracker */
                if(H5BT_insert(f, dxpl_id, sh->bt_free_addr, block_addr + size, sh->min_size - size) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINSERT, FAIL, "can't insert free space")
            } /* end if */

            /* Set address to return */
            *obj_addr_p = block_addr;
        } /* end else */
    } /* end else */

done:
    /* Release the block tracker info */
    if (sh && H5AC_unprotect(f, dxpl_id, H5AC_SGHP, addr, sh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release segmented heap info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5SH_alloc() */

