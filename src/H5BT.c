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

/* v2 B-tree info */
#define H5BT_BT2_NODE_SIZE      512
#define H5BT_BT2_RREC_SIZE(f)   (H5F_SIZEOF_ADDR(f) + H5F_SIZEOF_SIZE(f))    /* Offset & length of block tracked */
#define H5BT_BT2_SPLIT_PERC     100
#define H5BT_BT2_MERGE_PERC     40

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

