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
 * Created:		H5HFdbg.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Dump debugging information about a fractal heap
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
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5MMprivate.h"	/* Memory management			*/

/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5HF_dtable_debug(H5HF_dtable_param_t *dt_param, FILE *stream,
    int indent, int fwidth);


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
 * Function:	H5HF_dtable_debug
 *
 * Purpose:	Prints debugging info about a doubling table
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 28 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_debug(H5HF_dtable_param_t *dt_param, FILE *stream, int indent, int fwidth)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_debug)

    /*
     * Check arguments.
     */
    HDassert(dt_param);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Doubling table width:",
	      dt_param->cparam.width);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Starting block size:",
	      dt_param->cparam.start_block_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Max. direct block size:",
	      dt_param->cparam.max_direct_size);
    HDfprintf(stream, "%*s%-*s %u (bits)\n", indent, "", fwidth,
	      "Max. index size:",
	      dt_param->cparam.max_index);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Starting # of rows in root indirect block:",
	      dt_param->cparam.start_root_rows);
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Table's root address:",
	      dt_param->table_addr);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Current # of rows in root indirect block:",
	      dt_param->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_debug
 *
 * Purpose:	Prints debugging info about a fractal heap header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream, int indent, int fwidth)
{
    H5HF_t	*fh = NULL;             /* Fractal heap header info */
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HF_hdr_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    /*
     * Load the fractal heap header.
     */
    if(NULL == (fh = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

    /* Get the pointer to the shared fractal heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    /* Print opening message */
    HDfprintf(stream, "%*sFractal Heap Header...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
	      "Heap address mapping method:",
	      ((shared->addrmap) == H5HF_ABSOLUTE ? "Absolute" :
	      ((shared->addrmap) == H5HF_MAPPED ? "Mapped" :
              "Unknown!")));
    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Min. size of standalone object:",
	      (unsigned long)shared->standalone_size);
    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
	      "Fixed length object size:",
	      (unsigned long)shared->fixed_len_size);
    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
	      "Ref. count size:",
	      (unsigned)shared->ref_count_size);

    HDfprintf(stream, "%*sManaged Objects Doubling-Table Info...\n", indent, "");
    H5HF_dtable_debug(&shared->man_dtable_info, stream, indent + 3, MAX(0, fwidth -3));

done:
    if(fh && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, addr, fh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap header")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_debug() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dblock_debug
 *
 * Purpose:	Prints debugging info about a fractal heap direct block.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 28 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_dblock_debug(H5F_t *f, hid_t dxpl_id, haddr_t addr, FILE *stream,
    int indent, int fwidth, haddr_t hdr_addr, size_t block_size)
{
    H5HF_t	*fh = NULL;             /* Fractal heap header info */
    H5HF_direct_t *dblock = NULL;       /* Fractal heap direct block info */
    H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
    H5HF_shared_t *shared;              /* Shared fractal heap information */
    size_t	blk_prefix_size;        /* Size of prefix for block */
    unsigned    node_count = 0;         /* Number of free space nodes */
    size_t	amount_free = 0;        /* Amount of free space in block */
    uint8_t	*marker = NULL;         /* Track free space for block */
    size_t	overlap;                /* Number of free space overlaps */
    size_t	u, v;                   /* Local index variable */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5HF_dblock_debug, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);
    HDassert(H5F_addr_defined(hdr_addr));
    HDassert(block_size > 0);

    /*
     * Load the fractal heap header.
     */
    if(NULL == (fh = H5AC_protect(f, dxpl_id, H5AC_FHEAP_HDR, hdr_addr, NULL, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap header")

    /* Get the pointer to the shared fractal heap info */
    shared = H5RC_GET_OBJ(fh->shared);
    HDassert(shared);

    /*
     * Load the heap direct block
     */
    if(NULL == (dblock = H5AC_protect(f, dxpl_id, H5AC_FHEAP_DBLOCK, addr, &block_size, fh->shared, H5AC_READ)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, FAIL, "unable to load fractal heap direct block")

    /* Release the heap header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_HDR, hdr_addr, fh, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap header")
    fh = NULL;

    /* Print opening message */
    HDfprintf(stream, "%*sFractal Heap Direct Block...\n", indent, "");

    /*
     * Print the values.
     */
    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
	      "Address of fractal heap that owns this block:",
	      shared->heap_addr);
    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
	      "Offset of direct block in heap:",
	      dblock->block_off);
    blk_prefix_size = H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(shared, dblock);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of prefix:",
              blk_prefix_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Size of block offsets:",
	      dblock->blk_off_size);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Total free space in block:",
	      dblock->blk_free_space);
    HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
	      "Offset of free list head:",
	      dblock->free_list_head);

    /* Check for valid free list */
    if(!dblock->free_list)
        if(H5HF_man_dblock_build_freelist(dblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, FAIL, "can't decode free list for block")
    HDassert(dblock->free_list);

    /*
     * Traverse the free list and check that all free blocks fall within
     * the block and that no two free blocks point to the same region of
     * the block.  */
    if(NULL == (marker = H5MM_calloc(dblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    HDfprintf(stream, "%*sFree Blocks (offset, next offset, size):\n", indent, "");
    node = dblock->free_list->first;
    while(node) {
        char temp_str[32];

        sprintf(temp_str, "Block #%u:", node_count);
	HDfprintf(stream, "%*s%-*s %8Zu, %8Zu, %8Zu\n", indent + 3, "", MAX(0, fwidth - 9),
		temp_str,
		node->my_offset, node->next_offset, node->size);

	if (node->my_offset + node->size > dblock->size)
	    fprintf(stream, "***THAT FREE BLOCK IS OUT OF BOUNDS!\n");
        else {
            /* Mark this node's free space & check for overlaps w/other free space */
	    for(u = overlap = 0; u < node->size; u++) {
		if(marker[node->my_offset + u])
		    overlap++;
		marker[node->my_offset + u] = 1;
	    } /* end for */

	    if (overlap)
		fprintf(stream, "***THAT FREE BLOCK OVERLAPPED A PREVIOUS ONE!\n");
	    else
		amount_free += node->size;
	} /* end else */

        /* Avance to next node */
        node = node->next;
        node_count++;
    } /* end while */

    HDfprintf(stream, "%*s%-*s %.2f%%\n", indent, "", fwidth,
            "Percent of block used:",
            (100.0 * (double)(dblock->size - (blk_prefix_size + amount_free)) / (double)dblock->size));

    /*
     * Print the data in a VMS-style octal dump.
     */
    H5_buffer_dump(stream, indent, dblock->blk, marker, 0, dblock->size);

done:
    if(dblock && H5AC_unprotect(f, dxpl_id, H5AC_FHEAP_DBLOCK, addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_PROTECT, FAIL, "unable to release fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_dblock_debug() */

