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
 * Created:		H5BT.c
 *			Mar  9 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Tracks blocks of bytes in a file
 *
 *-------------------------------------------------------------------------
 */

#define H5BT_PACKAGE		/*suppress error about including H5BTpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BTpkg.h"		/* Block tracker			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/

/* Local macros */

/* v2 B-tree settings */
#define H5BT_BT2_NODE_SIZE      512
#define H5BT_BT2_RREC_SIZE(f)   (H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f))    /* Offset & length of block tracked */
#define H5BT_BT2_SPLIT_PERC     100
#define H5BT_BT2_MERGE_PERC     40

/* Bit flags for block tracker size status */
#define H5BT_STATUS_MAX_VALID   0x01    /* Maximum block size valid over all blocks tracked */
    /* If this flag is not set, then only part of the blocks have been */
    /* searched to determine the current maximum block size.  This can happen */
    /* during block shrinks or removals */
#define H5BT_STATUS_MIN_VALID   0x01    /* Minimum block size valid over all blocks tracked */
    /* If this flag is not set, then only part of the blocks have been */
    /* searched to determine the current minimum block size.  This can happen */
    /* during block expansions or removals */

/* Local typedefs */

/* Local prototypes */

/* Package variables */

/* Declare a free list to manage the H5BT_t struct */
H5FL_DEFINE(H5BT_t);

/* Static variables */


/*-------------------------------------------------------------------------
 * Function:	H5BT_create
 *
 * Purpose:	Creates a new empty block tracker in the file.
 *
 * Return:	Non-negative on success (with address of new block tracker
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BT_create(H5F_t *f, hid_t dxpl_id, haddr_t *addr_p)
{
    H5BT_t *bt = NULL;                 /* The new B-tree header information */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5BT_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);

    /*
     * Allocate file and memory data structures.
     */
    if (NULL==(bt = H5FL_CALLOC(H5BT_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for block tracker info")

    /* Assign internal information */
    bt->cache_info.is_dirty = TRUE;
    bt->max_block_size = 0;             /* Indicate that the value is invalid */
    bt->min_block_size = HSIZET_MAX;    /* Indicate that the value is invalid */

    /* Allocate space for the header on disk */
    if (HADDR_UNDEF==(*addr_p=H5MF_alloc(f, H5FD_MEM_BLKTRK, dxpl_id, (hsize_t)H5BT_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for block tracker info")

    /* Create a B-tree for storing the block info records */
    if (H5B2_create(f, dxpl_id, H5B2_BLKTRK, H5BT_BT2_NODE_SIZE, H5BT_BT2_RREC_SIZE(f), H5BT_BT2_SPLIT_PERC, H5BT_BT2_MERGE_PERC, &bt->bt2_addr/*out*/)<0)
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTINIT, FAIL, "can't create B-tree for storing block info")

    /* Cache the new B-tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BLTR, *addr_p, bt, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTINIT, FAIL, "can't add block tracker info to cache")

done:
    if (ret_value<0) {
	if (bt)
            (void)H5BT_cache_dest(f,bt);
    } /* end if */
    
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BT_create() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_insert_neighbor_cb
 *
 * Purpose:	v2 B-tree neighbor callback for H5BT_insert()
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 10, 2005
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static int
H5BT_insert_neighbor_cb(const void *_record, void *_op_data)
{
    const H5BT_blk_info_t *record = (const H5BT_blk_info_t *)_record;
    H5BT_blk_info_t *search = (H5BT_blk_info_t *)_op_data;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BT_insert_neighbor_cb)

    *search = *record;

    FUNC_LEAVE_NOAPI(0)
} /* end H5BT_insert_neighbor_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5BT_insert
 *
 * Purpose:	Insert new block (offset/length) into a block tracker.
 *              Duplicate and overlapping blocks are rejected.
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BT_insert(H5F_t *f, hid_t dxpl_id, haddr_t addr, haddr_t offset, hsize_t length)
{
    H5BT_t *bt = NULL;                  /* The new B-tree header information */
    H5BT_blk_info_t lower, upper;       /* Info for blocks less than & greater than new block */
    hbool_t lower_valid = FALSE, upper_valid = FALSE;   /* Lower & upper blocks valid? */
    H5BT_blk_info_t new_block;          /* Info for new block */
    hsize_t nblks;                      /* Number of blocks tracked */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5BT_insert, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));

    /* Look up the block tracker header */
    if (NULL == (bt = H5AC_protect(f, dxpl_id, H5AC_BLTR, addr, NULL, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BLKTRK, H5E_CANTPROTECT, FAIL, "unable to load block tracker info")

    /* Check for block at this address already */
    if (H5B2_find(f, dxpl_id, H5B2_BLKTRK, bt->bt2_addr, &offset, NULL, NULL) >= 0)
	HGOTO_ERROR(H5E_BLKTRK, H5E_EXISTS, FAIL, "block at address already exists")
    /* Clear any errors from H5B2_find() */
    H5E_clear_stack(NULL);

    /* Find next block lower than the new block */
    if ( H5B2_neighbor(f, dxpl_id, H5B2_BLKTRK, bt->bt2_addr, H5B2_COMPARE_LESS, &offset, H5BT_insert_neighbor_cb, &lower) >= 0) {
        if ( H5F_addr_overlap(lower.addr, lower.len, offset, length) )
            HGOTO_ERROR(H5E_BLKTRK, H5E_OVERLAPS, FAIL, "new block overlaps existing block")

        /* Set flag to indicate lower bound found */
        lower_valid = TRUE;
    } /* end if */
    /* Clear any errors from H5B2_neighbor() */
    H5E_clear_stack(NULL);

    /* Find next block higher than the new block */
    if ( H5B2_neighbor(f, dxpl_id, H5B2_BLKTRK, bt->bt2_addr, H5B2_COMPARE_GREATER, &offset, H5BT_insert_neighbor_cb, &upper) >= 0) {
        if ( H5F_addr_overlap(upper.addr, upper.len, offset, length) )
            HGOTO_ERROR(H5E_BLKTRK, H5E_OVERLAPS, FAIL, "new block overlaps existing block")

        /* Set flag to indicate upper bound found */
        upper_valid = TRUE;
    } /* end if */
    /* Clear any errors from H5B2_neighbor() */
    H5E_clear_stack(NULL);

#ifdef QAK
    /* Check for merged blocks */
    if(lower_valid || upper_valid) {
HDfprintf(stderr,"%s: Lower & upper block merging not supported yet!\n",FUNC);
HGOTO_ERROR(H5E_BLKTRK, H5E_UNSUPPORTED, FAIL, "lower or upper block found!")
    } /* end if */
#endif /* QAK */

    /* Insert new block into B-tree */
    new_block.addr = offset;
    new_block.len = length;
    if(H5B2_insert(f, dxpl_id, H5B2_BLKTRK, bt->bt2_addr, &new_block) < 0)
        HDONE_ERROR(H5E_BLKTRK, H5E_CANTINSERT, FAIL, "unable to insert block")

/* Update block tracker metadata */

    /* Determine the number of blocks being tracked */
    if(H5B2_get_nrec(f, dxpl_id, H5B2_BLKTRK, bt->bt2_addr, &nblks) < 0)
        HDONE_ERROR(H5E_BLKTRK, H5E_CANTINSERT, FAIL, "unable to determine # of blocks")

    /* This is the only block tracked so far */
    if(nblks == 1) {
        bt->max_block_size = length;
        bt->max_block_cnt = 1;
        bt->status |= H5BT_STATUS_MAX_VALID;
        bt->min_block_size = length;
        bt->min_block_cnt = 1;
        bt->status |= H5BT_STATUS_MAX_VALID;
    } /* end if */
    else {
        /* Update maximum block size */
        if (length > bt->max_block_size) {
            bt->max_block_size = length;
            bt->max_block_cnt = 1;
        } /* end if */
        else if (length == bt->max_block_size)
            bt->max_block_cnt++;

        /* Update minimum block size */
        if (length < bt->min_block_size) {
            bt->min_block_size = length;
            bt->min_block_cnt = 1;
        } /* end if */
        else if (length == bt->min_block_size)
            bt->min_block_cnt++;
    } /* end if */

    /* Increment total number of bytes tracked in all blocks */
    bt->tot_block_size += length;

done:
    /* Release the block tracker info */
    if (bt && H5AC_unprotect(f, dxpl_id, H5AC_BLTR, addr, bt, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BLKTRK, H5E_CANTUNPROTECT, FAIL, "unable to release block tracker info")
    
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BT_insert() */

