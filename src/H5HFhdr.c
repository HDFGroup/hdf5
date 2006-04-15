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
 * Created:		H5HFhdr.c
 *			Apr 10 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Heap header routines for fractal heaps.
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
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Limit on the size of the max. direct block size */
/* (This is limited to 32-bits currently, because I think it's unlikely to
 *      need to be larger, the 32-bit limit for H5V_log2_of2(n), and
 *      some offsets/sizes are encoded with a maxiumum of 32-bits  - QAK)
 */
#define H5HL_MAX_DIRECT_SIZE_LIMIT ((hsize_t)2 * 1024 * 1024 * 1024)

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
 * Function:	H5HF_hdr_alloc
 *
 * Purpose:	Allocate shared fractal heap header 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_t *
H5HF_hdr_alloc(H5F_t *f)
{
    H5HF_t *fh = NULL;          /* Shared fractal heap header */
    H5HF_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_alloc)

    /*
     * Check arguments.
     */
    HDassert(f);

    /* Allocate space for the shared information */
    if(NULL == (fh = H5FL_CALLOC(H5HF_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fractal heap shared header")

    /* Set the internal parameters for the heap */
    fh->f = f;
    fh->sizeof_size = H5F_SIZEOF_SIZE(f);
    fh->sizeof_addr = H5F_SIZEOF_ADDR(f);

    /* Set the return value */
    ret_value = fh;

done:
    if(!ret_value)
        if(fh)
            (void)H5HF_cache_hdr_dest(f, fh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_free_space
 *
 * Purpose:	Compute direct block free space, for indirect blocks of
 *              different sizes.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5HF_hdr_compute_free_space(H5HF_t *hdr, hsize_t iblock_size)
{
    hsize_t acc_heap_size;      /* Accumumated heap space */
    hsize_t acc_dblock_free;    /* Accumumated direct block free space */
    unsigned curr_row;          /* Current row in block */
    hsize_t ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_compute_free_space)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(iblock_size > hdr->man_dtable.cparam.max_direct_size);

    /* Set the free space in direct blocks */
    acc_heap_size = 0;
    acc_dblock_free = 0;
    curr_row = 0;
    while(acc_heap_size < iblock_size) {
        acc_heap_size += hdr->man_dtable.row_block_size[curr_row] *
                hdr->man_dtable.cparam.width;
        acc_dblock_free += hdr->man_dtable.row_dblock_free[curr_row] *
                hdr->man_dtable.cparam.width;
        curr_row++;
    } /* end while */

    /* Set return value */
    ret_value = acc_dblock_free;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_compute_free_space() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_finish_init
 *
 * Purpose:	Finish initializing info in shared heap header
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
H5HF_hdr_finish_init(H5HF_t *hdr)
{
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_finish_init)

    /*
     * Check arguments.
     */
    HDassert(hdr);

    /* Compute/cache some values */
    hdr->heap_off_size = H5HF_SIZEOF_OFFSET_BITS(hdr->man_dtable.cparam.max_index);
    if(H5HF_dtable_init(&hdr->man_dtable) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize doubling table info")

    /* Create the free-list structure for the heap */
    if(NULL == (hdr->flist = H5HF_flist_create(hdr->man_dtable.cparam.max_direct_size, H5HF_free_section_free_cb)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize free list info")

    /* Set the size of heap IDs */
    hdr->id_len = hdr->heap_off_size + MIN(hdr->man_dtable.max_dir_blk_off_size,
        ((H5V_log2_gen((hsize_t)hdr->standalone_size) + 7) / 8));

    /* Set the free space in direct blocks */
    for(u = 0; u < hdr->man_dtable.max_root_rows; u++) {
        if(u < hdr->man_dtable.max_direct_rows)
            hdr->man_dtable.row_dblock_free[u] = hdr->man_dtable.row_block_size[u] -
                (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, hdr->man_dtable.row_block_size[u])
                    + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));
        else
            hdr->man_dtable.row_dblock_free[u] = H5HF_hdr_compute_free_space(hdr, hdr->man_dtable.row_block_size[u]);
#ifdef QAK
HDfprintf(stderr, "%s: row_block_size[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_block_size[u]);
HDfprintf(stderr, "%s: row_block_off[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_block_off[u]);
HDfprintf(stderr, "%s: row_dblock_free[%Zu] = %Hu\n", FUNC, u, hdr->man_dtable.row_dblock_free[u]);
#endif /* QAK */
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_finish_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_init
 *
 * Purpose:	Initialize shared fractal heap header for new heap
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
H5HF_hdr_init(H5HF_t *fh, haddr_t fh_addr, H5HF_create_t *cparam)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_init)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(cparam);

#ifndef NDEBUG
    /* Check for valid parameters */
    if(!POWER_OF_TWO(cparam->managed.width) || cparam->managed.width == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "width not power of two")
    if(!POWER_OF_TWO(cparam->managed.start_block_size) || cparam->managed.start_block_size == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "starting block size not power of two")
    if(!POWER_OF_TWO(cparam->managed.max_direct_size) ||
            (cparam->managed.max_direct_size == 0 || cparam->managed.max_direct_size > H5HL_MAX_DIRECT_SIZE_LIMIT))
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not power of two")
    if(cparam->managed.max_direct_size < cparam->standalone_size)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not large enough to hold all managed blocks")
    if(cparam->managed.max_index > (8 * fh->sizeof_size) || cparam->managed.max_index == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not power of two")
#endif /* NDEBUG */

    /* Set the creation parameters for the heap */
    fh->heap_addr = fh_addr;
    fh->addrmap = cparam->addrmap;
    fh->standalone_size = cparam->standalone_size;
    HDmemcpy(&(fh->man_dtable.cparam), &(cparam->managed), sizeof(H5HF_dtable_cparam_t));

    /* Set root table address */
    fh->man_dtable.table_addr = HADDR_UNDEF;

    /* Note that the shared info is dirty (it's not written to the file yet) */
    fh->dirty = TRUE;

    /* Make shared heap info reference counted */
    if(H5HF_hdr_finish_init(fh) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap header")

done:
    if(ret_value < 0)
        if(fh)
            (void)H5HF_cache_hdr_dest(NULL, fh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_incr
 *
 * Purpose:	Increment reference count on shared heap header
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
H5HF_hdr_incr(H5HF_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_incr)

    /* Sanity check */
    HDassert(hdr);

/* XXX: When "un-evictable" feature is finished, mark the header as
 *      unevictable on the first block to share it.  - QAK
 */

    /* Increment reference count on shared header */
    hdr->rc++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_hdr_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_decr
 *
 * Purpose:	Decrement reference count on shared heap header
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
H5HF_hdr_decr(H5HF_t *hdr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_decr)

    /* Sanity check */
    HDassert(hdr);

    /* Decrement reference count on shared header */
    hdr->rc--;

/* XXX: When "un-evictable" feature is finished, mark the header as
 *      evictable when the ref. count drops to zero.  - QAK
 */
/* XXX: Take this call out after "un-evictable" flag is working */
    H5HF_cache_hdr_dest_real(hdr);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_hdr_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_dirty
 *
 * Purpose:	Mark heap header as dirty
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
H5HF_hdr_dirty(hid_t dxpl_id, H5HF_t *hdr)
{
    H5HF_t *tmp_hdr;                    /* Temporary pointer to heap header */
    hbool_t is_protected;               /* Whether the indirect block is protected */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_hdr_dirty)
#ifdef QAK
HDfprintf(stderr, "%s: Marking heap header as dirty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(hdr);

/* XXX: When "un-evictable" feature is finished, just mark the header as dirty
 *      in the cache, instead of this protect -> unprotect kludge - QAK
 */
    /* Protect the header */
    is_protected = hdr->cache_info.is_protected;
    if(!is_protected) {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->heap_addr = %a\n", FUNC, hdr->heap_addr);
#endif /* QAK */
        if(NULL == (tmp_hdr = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_HDR, hdr->heap_addr, NULL, NULL, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
        HDassert(hdr == tmp_hdr);
    } /* end if */

    /* Set the dirty flags for the heap header */
    hdr->dirty = TRUE;

    /* Release the heap header (marked as dirty) */
    if(!is_protected) {
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_HDR, hdr->heap_addr, tmp_hdr, H5AC__DIRTIED_FLAG) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_hdr_dirty() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_hdr_extend_heap
 *
 * Purpose:	Extend heap to cover more space
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 10 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_hdr_extend_heap(H5HF_t *hdr, hsize_t new_size, hsize_t extra_free)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_hdr_extend_heap)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(new_size >= hdr->total_size);

    /* Set the total space in heap */
    hdr->total_size = new_size;
    hdr->man_size = new_size;

    /* Increment the free space in direct blocks */
    hdr->total_man_free += extra_free;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_hdr_extend_heap() */

