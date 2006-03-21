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

/* Direct block free list section node */
struct H5HF_section_free_node_t {
    haddr_t     sect_addr;                      /* Address of free list section in the file */
                                                /* (Not actually used as address, used as unique ID for free list node) */
    haddr_t     block_addr;                     /* Address of direct block for free section */
    size_t      sect_size;                      /* Size of free space section */
                                                /* (section size is "object size", without the metadata overhead, since metadata overhead varies from block to block) */
    size_t      block_size;                     /* Size of direct block */
                                                /* (Needed to retrieve direct block) */
};


/********************/
/* Local Prototypes */
/********************/

/* Doubling table routines */
static herr_t H5HF_dtable_init(const H5HF_shared_t *shared, H5HF_dtable_t *dtable);
static herr_t H5HF_dtable_lookup(const H5HF_dtable_t *dtable, hsize_t off,
    unsigned *row, unsigned *col);

/* Shared heap header routines */
static herr_t H5HF_shared_free(void *_shared);

/* Direct block routines */
static herr_t H5HF_dblock_section_node_free_cb(void *item, void UNUSED *key,
    void UNUSED *op_data);
static herr_t H5HF_man_dblock_create(H5HF_parent_shared_t *par_shared,
    hid_t dxpl_id, size_t block_size, hsize_t block_off, haddr_t *addr_p);
static herr_t H5HF_man_dblock_new(H5RC_t *fh_shared, hid_t dxpl_id,
    size_t request);
static herr_t H5HF_man_dblock_adj_free(H5HF_direct_t *dblock, ssize_t amt);

/* Indirect block routines */
static herr_t H5HF_man_iblock_inc_loc(H5HF_indirect_t *iblock);
static H5HF_indirect_t * H5HF_man_iblock_place_dblock(H5RC_t *fh_shared, hid_t dxpl_id,
    size_t min_dblock_size, haddr_t *addr_p, size_t *entry_p,
    size_t *dblock_size);
static herr_t H5HF_man_iblock_create(H5RC_t *fh_shared, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p);

/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5HF_direct_t struct */
H5FL_DEFINE(H5HF_direct_t);

/* Declare a free list to manage the H5HF_direct_free_head_t struct */
H5FL_DEFINE(H5HF_direct_free_head_t);

/* Declare a free list to manage the H5HF_direct_free_node_t struct */
H5FL_DEFINE(H5HF_direct_free_node_t);

/* Declare a free list to manage the H5HF_section_free_node_t struct */
H5FL_DEFINE(H5HF_section_free_node_t);

/* Declare a free list to manage the H5HF_indirect_t struct */
H5FL_DEFINE(H5HF_indirect_t);

/* Declare a free list to manage the H5HF_indirect_ent_t sequence information */
H5FL_SEQ_DEFINE(H5HF_indirect_ent_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5HF_shared_t struct */
H5FL_DEFINE_STATIC(H5HF_shared_t);



/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_init
 *
 * Purpose:	Initialize values for doubling table
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_init(const H5HF_shared_t *shared, H5HF_dtable_t *dtable)
{
    hsize_t tmp_block_size;             /* Temporary block size */
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_dtable_init)

    /*
     * Check arguments.
     */
    HDassert(shared);
    HDassert(dtable);

    /* Compute/cache some values */
    dtable->first_row_bits = H5V_log2_of2(dtable->cparam.start_block_size) +
            H5V_log2_of2(dtable->cparam.width);
    dtable->max_root_rows = (dtable->cparam.max_index - dtable->first_row_bits) + 1;
    dtable->max_direct_rows = (H5V_log2_of2(dtable->cparam.max_direct_size) -
            H5V_log2_of2(dtable->cparam.start_block_size)) + 2;
    dtable->num_id_first_row = dtable->cparam.start_block_size * dtable->cparam.width;
    dtable->max_dir_blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dtable->cparam.max_direct_size);

    /* Build table of block sizes for each row */
    if(NULL == (dtable->row_block_size = H5MM_malloc(dtable->max_root_rows * sizeof(hsize_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create doubling table block size table")
    tmp_block_size = dtable->cparam.start_block_size;
    dtable->row_block_size[0] = dtable->cparam.start_block_size;
    for(u = 1; u < dtable->max_root_rows; u++) {
        dtable->row_block_size[u] = tmp_block_size;
        tmp_block_size *= 2;
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_dtable_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_lookup
 *
 * Purpose:	Compute the row & col of an offset in a doubling-table
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_lookup(const H5HF_dtable_t *dtable, hsize_t off, unsigned *row, unsigned *col)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_lookup)

    /*
     * Check arguments.
     */
    HDassert(dtable);
    HDassert(row);
    HDassert(col);

    /* Check for offset in first row */
    if(off < dtable->num_id_first_row) {
        *row = 0;
        *col = off / dtable->cparam.start_block_size;
    } /* end if */
    else {
        unsigned high_bit = H5V_log2_gen(off);  /* Determine the high bit in the offset */
        hsize_t off_mask = 1 << high_bit;       /* Compute mask for determining column */

        *row = (high_bit - dtable->first_row_bits) + 1;
        *col = (off - off_mask) / dtable->row_block_size[*row];
    } /* end else */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_lookup() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_alloc
 *
 * Purpose:	Allocate shared fractal heap info 
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
H5HF_shared_t *
H5HF_shared_alloc(H5F_t *f)
{
    H5HF_shared_t *shared = NULL;       /* Shared fractal heap information */
    H5HF_shared_t *ret_value = NULL;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_alloc)

    /*
     * Check arguments.
     */
    HDassert(f);

    /* Allocate space for the shared information */
    if(NULL == (shared = H5FL_CALLOC(H5HF_shared_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for fractal heap shared information")

    /* Set the internal parameters for the heap */
    shared->f = f;
    shared->sizeof_size = H5F_SIZEOF_SIZE(f);
    shared->sizeof_addr = H5F_SIZEOF_ADDR(f);

    /* Set the return value */
    ret_value = shared;

done:
    if(!ret_value)
        if(shared)
            H5HF_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_shared_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_own
 *
 * Purpose:	Have heap take ownership of the shared info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_shared_own(H5HF_t *fh, H5HF_shared_t *shared)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_own)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(shared);

    /* Compute/cache some values */
    shared->ref_count_obj = (shared->ref_count_size > 0);
    shared->heap_off_size = H5HF_SIZEOF_OFFSET_BITS(shared->man_dtable.cparam.max_index);
    if(H5HF_dtable_init(shared, &shared->man_dtable) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize doubling table info")

    /* Create the free-list structure for the heap */
    if(NULL == (shared->flist = H5HF_flist_create(shared->man_dtable.cparam.max_direct_size, H5HF_dblock_section_node_free_cb)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize free list info")

    /* Make shared heap info reference counted */
    if(NULL == (fh->shared = H5RC_create(shared, H5HF_shared_free)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_shared_own() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_create
 *
 * Purpose:	Allocate & create shared fractal heap info for new heap
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
H5HF_shared_init(H5HF_shared_t *shared, H5HF_t *fh, haddr_t fh_addr, H5HF_create_t *cparam)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_init)

    /*
     * Check arguments.
     */
    HDassert(shared);
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
    if(cparam->managed.max_index > (8 * shared->sizeof_size) || cparam->managed.max_index == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not power of two")
#endif /* NDEBUG */

    /* Set the creation parameters for the heap */
    shared->heap_addr = fh_addr;
    shared->addrmap = cparam->addrmap;
    shared->standalone_size = cparam->standalone_size;
    shared->ref_count_size = cparam->ref_count_size;
    HDmemcpy(&(shared->man_dtable.cparam), &(cparam->managed), sizeof(H5HF_dtable_cparam_t));

    /* Set root table address */
    shared->man_dtable.table_addr = HADDR_UNDEF;

    /* Note that the shared info is dirty (it's not written to the file yet) */
    shared->dirty = TRUE;

    /* Make shared heap info reference counted */
    if(H5HF_shared_own(fh, shared) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap info")

done:
    if(ret_value < 0)
        if(shared)
            H5HF_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_shared_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_shared_free
 *
 * Purpose:	Free shared fractal heap info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_shared_free(void *_shared)
{
    H5HF_shared_t *shared = (H5HF_shared_t *)_shared;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_shared_free)

    /* Sanity check */
    HDassert(shared);

    /* Free the free list information for the heap */
    if(shared->flist)
        H5HF_flist_free(shared->flist);

    /* Free the block size lookup table for the doubling table */
    H5MM_xfree(shared->man_dtable.row_block_size);

    /* Free the shared info itself */
    H5FL_FREE(H5HF_shared_t, shared);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_inc_loc
 *
 * Purpose:	Increment location of next direct block in indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_inc_loc(H5HF_indirect_t *iblock)
{
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_inc_loc)

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(iblock->shared);
    HDassert(shared);

    /* Increment block entry */
    iblock->next_entry++;

    /* Increment column */
    iblock->next_col++;

    /* Check for walking off end of column */
    if(iblock->next_col == shared->man_dtable.cparam.width) {
        /* Reset column */
        iblock->next_col = 0;

        /* Increment row & block size */
        iblock->next_row++;
        iblock->next_size *= 2;

        /* Check for filling up indirect block */
        if(iblock->next_row == iblock->max_rows) {
            /* Check for "full" heap */
            if(iblock->parent == NULL)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")

            /* Increment location for parent indirect block */
            iblock = H5RC_GET_OBJ(iblock->parent);
            if(H5HF_man_iblock_inc_loc(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")
        } /* end if */
    } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
#endif /* QAK */
    /* Mark heap header as modified */
    shared->dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_inc_loc() */


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
static herr_t
H5HF_man_dblock_create(H5HF_parent_shared_t *par_shared, hid_t dxpl_id, 
    size_t block_size, hsize_t block_off, haddr_t *addr_p)
{
    H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
    H5HF_section_free_node_t *sec_node; /* Pointer to free list section for block */
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block */
    size_t free_space;                  /* Free space in new block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_create)

    /*
     * Check arguments.
     */
    HDassert(par_shared);
    HDassert(par_shared->shared);
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
    dblock->shared = par_shared->shared;
    H5RC_INC(dblock->shared);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(dblock->shared);
    HDassert(shared);

    /* Set info for direct block */
#ifdef QAK
HDfprintf(stderr, "%s: size = %Zu, block_off = %Hu\n", FUNC, block_size, block_off);
#endif /* QAK */
    dblock->parent = par_shared->parent;
    if(dblock->parent)
        H5RC_INC(dblock->parent);
    dblock->parent_entry = par_shared->parent_entry;
    dblock->size = block_size;
    dblock->block_off = block_off;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(block_size);
    dblock->free_list_head = H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(shared, dblock);
    dblock->blk_free_space = par_shared->parent_free_space;
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
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(shared->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)block_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

    /* Create free list section node */
    if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

    /* Set section's information */
    sec_node->block_addr = *addr_p;
    sec_node->block_size = block_size;
    sec_node->sect_addr = *addr_p + node->my_offset;
    /* (section size is "object size", without the metadata overhead) */
    sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(shared, dblock);

    /* Add new free space to the global list of space */
    if(H5HF_flist_add(shared->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")

    /* Adjust free space to include new block's space */
    if(H5HF_man_dblock_adj_free(dblock, (ssize_t)free_space) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for direct block & parents")

    /* Update shared heap info */
    shared->total_size += dblock->size;
    shared->man_size += dblock->size;

    /* Mark heap header as modified */
    shared->dirty = TRUE;

    /* Cache the new fractal heap direct block */
    if(H5AC_set(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, *addr_p, dblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap direct block to cache")

done:
    if(ret_value < 0)
        if(dblock)
            (void)H5HF_cache_dblock_dest(shared->f, dblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_create() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dblock_section_node_free_cb
 *
 * Purpose:	Free a section node for a block
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
static herr_t
H5HF_dblock_section_node_free_cb(void *item, void UNUSED *key, void UNUSED *op_data)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dblock_section_node_free_cb)

    HDassert(item);

    /* Release the sections */
    H5FL_FREE(H5HF_section_free_node_t, item);

    FUNC_LEAVE_NOAPI(0)
}   /* H5HF_dblock_section_node_free_cb() */


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
    H5HF_shared_t *shared;                      /* Pointer to shared heap info */
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
        H5HF_section_free_node_t *sec_node;     /* Pointer to free list section for block */
        H5HF_direct_free_node_t *node = NULL;   /* Pointer to free list node for block */
        H5HF_direct_free_node_t *prev_node;     /* Pointer to previous free list node for block */
        hsize_t free_len;       /* Length of free list info */
        hsize_t next_off;       /* Next node offset in block */
        hsize_t prev_off;       /* Prev node offset in block */
        uint8_t *p;             /* Temporary pointer to free node info */

        /* Get the pointer to the shared heap info */
        shared = H5RC_GET_OBJ(dblock->shared);
        HDassert(shared);

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
        if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

        /* Set section's information */
        sec_node->block_addr = dblock_addr;
        sec_node->block_size = dblock->size;
        sec_node->sect_addr = dblock_addr + node->my_offset;
        /* (section size is "object size", without the metadata overhead) */
        sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(shared, dblock);

        /* Add new free space to the global list of space */
        if(H5HF_flist_add(shared->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
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
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for direct block free list section")

            /* Set section's information */
            sec_node->block_addr = dblock_addr;
            sec_node->block_size = dblock->size;
            sec_node->sect_addr = dblock_addr + node->my_offset;
            /* (section size is "object size", without the metadata overhead) */
            sec_node->sect_size = node->size - H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(shared, dblock);

            /* Add new free space to the global list of space */
            if(H5HF_flist_add(shared->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
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
static herr_t
H5HF_man_dblock_adj_free(H5HF_direct_t *dblock, ssize_t amt)
{
    H5HF_shared_t *shared;              /* Shared heap information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_man_dblock_adj_free)
#ifdef QAK
HDfprintf(stderr, "%s: amt = %Zd\n", "H5HF_man_dblock_adj_free", amt);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(dblock->shared);
    HDassert(shared);

    /* Adjust space available in block */
    dblock->blk_free_space += amt;

    /* Check if the parent info is set */
    if(dblock->parent) {
        H5HF_indirect_t *iblock;    /* Block's parent */

        /* Get the pointer to the shared parent indirect block */
        iblock = H5RC_GET_OBJ(dblock->parent);
        HDassert(iblock);

        /* Adjust this indirect block's child free space */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->child_free_space = %Hu\n", "H5HF_man_dblock_adj_free", iblock->child_free_space);
#endif /* QAK */
        iblock->ents[dblock->parent_entry].free_space += amt;
        iblock->child_free_space += amt;

        /* Mark indirect block as dirty */
        iblock->dirty = TRUE;

        /* Modify the free space in parent block(s) */
        while(iblock->parent) {
            size_t parent_entry;            /* Entry in parent */

            /* Get the pointer to the shared parent indirect block */
            parent_entry = iblock->parent_entry;
            iblock = H5RC_GET_OBJ(iblock->parent);
            HDassert(iblock);

            /* Adjust this indirect block's child free space */
            iblock->ents[parent_entry].free_space += amt;
            iblock->child_free_space += amt;

            /* Mark indirect block as dirty */
            iblock->dirty = TRUE;
        } /* end while */
    } /* end if */

    /* Update shared heap free space info */
    shared->total_man_free += amt;

    /* Mark heap header as modified */
    shared->dirty = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
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
static herr_t
H5HF_man_dblock_new(H5RC_t *fh_shared, hid_t dxpl_id, size_t request)
{
    H5HF_parent_shared_t par_shared;    /* Parent shared information */
    H5HF_shared_t *shared;              /* Shared heap information */
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
    HDassert(fh_shared);
    HDassert(request > 0);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Compute the min. size of the direct block needed to fulfill the request */
    if(request < shared->man_dtable.cparam.start_block_size)
        min_dblock_size = shared->man_dtable.cparam.start_block_size;
    else {
        min_dblock_size = 2 * H5V_log2_gen((hsize_t)request);
        HDassert(min_dblock_size <= shared->man_dtable.cparam.max_direct_size);
    } /* end else */

    /* Adjust the size of block needed to fulfill request, with overhead */
#ifdef QAK
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(shared, shared->man_dtable.cparam.start_block_size));
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE(shared, shared->man_dtable.cparam.start_block_size));
#endif /* QAK */
    if((min_dblock_size - request) < (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(shared, min_dblock_size)
            + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE(shared, min_dblock_size)))
        min_dblock_size *= 2;
#ifdef QAK
HDfprintf(stderr, "%s: min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /* Check if this is the first block in the heap */
    if(!H5F_addr_defined(shared->man_dtable.table_addr)) {
        /* Create new direct block at corrent location*/
        dblock_size = shared->man_dtable.cparam.start_block_size;
        par_shared.shared = fh_shared;
        par_shared.parent = NULL;
        par_shared.parent_entry = 0;
        par_shared.parent_free_space = 0;
        if(H5HF_man_dblock_create(&par_shared, dxpl_id, dblock_size, shared->man_dtable.next_dir_block, &dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
HDfprintf(stderr, "%s: shared->man_dtable.next_dir_block = %Hu\n", FUNC, shared->man_dtable.next_dir_block);
#endif /* QAK */

        /* Point root at new direct block */
        shared->man_dtable.curr_root_rows = 0;
        shared->man_dtable.table_addr = dblock_addr;
    } /* end if */
    /* Root entry already exists, go get indirect block for new direct block */
    else {
        H5HF_indirect_t *iblock;    /* Pointer to indirect block to create */
        haddr_t iblock_addr;        /* Indirect block's address */
        size_t dblock_entry;        /* Direct entry for new direct block */

        /* Find indirect block with room for block of correct size */
        if(NULL == (iblock = H5HF_man_iblock_place_dblock(fh_shared, dxpl_id, min_dblock_size, &iblock_addr, &dblock_entry, &dblock_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTGET, FAIL, "unable to locate indirect block with space for direct block")
#ifdef QAK
HDfprintf(stderr, "%s: dblock_entry = %Zu\n", FUNC, dblock_entry);
HDfprintf(stderr, "%s: dblock_size = %Zu\n", FUNC, dblock_size);
#endif /* QAK */

        /* Create new direct block at corrent location*/
        par_shared.shared = fh_shared;
        par_shared.parent = iblock->self;
        par_shared.parent_entry = dblock_entry;
        par_shared.parent_free_space = 0;
        if(H5HF_man_dblock_create(&par_shared, dxpl_id, dblock_size, shared->man_dtable.next_dir_block, &dblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

#ifdef QAK
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);
#endif /* QAK */

        /* Point indirect block at new direct block */
        iblock->ents[dblock_entry].addr = dblock_addr;

        /* Mark indirect block as modified */
        iblock->dirty = TRUE;

        /* Release the indirect block (marked as dirty) */
        if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    } /* end else */

    /* Update shared info */
/* XXX: This is going to cause problems when we support skipping blocks */
    shared->man_dtable.next_dir_block += dblock_size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_new() */


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
H5HF_man_find(H5RC_t *fh_shared, hid_t dxpl_id, size_t request,
    H5HF_section_free_node_t **sec_node/*out*/)
{
    H5HF_shared_t *shared;              /* Shared heap information */
    htri_t node_found;                  /* Whether an existing free list node was found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_find)
#ifdef QAK
HDfprintf(stderr, "%s: request = %Zu\n", FUNC, request);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(request > 0);
    HDassert(sec_node);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Look for free space in global free list */
    if((node_found = H5HF_flist_find(shared->flist, request, (void **)sec_node)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap direct block")

/* XXX: Make certain we've loaded all the direct blocks in the heap */

    /* If we didn't find a node, go make one big enough to hold the requested block */
    if(!node_found) {
        /* Allocate direct block big enough to hold requested size */
        if(H5HF_man_dblock_new(fh_shared, dxpl_id, request) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create fractal heap direct block")

        /* Request space from the free list */
        /* (Ought to be able to be filled, now) */
        if(H5HF_flist_find(shared->flist, request, (void **)sec_node) <= 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap direct block")
    } /* end if */
    HDassert(*sec_node);
#ifdef QAK
HDfprintf(stderr, "%s: (*sec_node)->block_addr = %a\n", FUNC, (*sec_node)->block_addr);
HDfprintf(stderr, "%s: (*sec_node)->block_size = %Zu\n", FUNC, (*sec_node)->block_size);
HDfprintf(stderr, "%s: (*sec_node)->sect_addr = %a\n", FUNC, (*sec_node)->sect_addr);
HDfprintf(stderr, "%s: (*sec_node)->sect_size = %Zu\n", FUNC, (*sec_node)->sect_size);
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
H5HF_man_insert(H5RC_t *fh_shared, hid_t dxpl_id, H5HF_section_free_node_t *sec_node,
    size_t obj_size, const void *obj, void *id)
{
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block to modify */
    haddr_t dblock_addr;                /* Direct block address */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_insert)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(obj_size > 0);
    HDassert(obj);
    HDassert(id);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Lock direct block */
    dblock_addr = sec_node->block_addr;
    if(NULL == (dblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &sec_node->block_size, fh_shared, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")

    /* Insert object into block */

    /* Check for address mapping type */
    if(shared->addrmap == H5HF_ABSOLUTE) {
        H5HF_direct_free_node_t *node;  /* Block's free list node */
        uint8_t *p;                     /* Temporary pointer to obj info in block */
        size_t obj_off;                 /* Offset of object within block */
        size_t full_obj_size;           /* Size of object including metadata */
        size_t alloc_obj_size;          /* Size of object including metadata & any free space fragment */
        unsigned char free_frag_size;   /* Size of free space fragment */

        /* Locate "local" free list node for section */
/* XXX: Change to using skip list */
        obj_off = sec_node->sect_addr - sec_node->block_addr;
        node = dblock->free_list->first;
        while(node->my_offset != obj_off)
            node = node->next;

        /* Compute full object size, with metadata for object */
        full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(shared, dblock);

        /* Sanity checks */
        HDassert(dblock->blk_free_space >= full_obj_size);
        HDassert(dblock->free_list);
        HDassert(node->size >= full_obj_size);

        /* Check for using entire node */
        free_frag_size = 0;
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

            /* Release the memory for the free list node & section */
            H5FL_FREE(H5HF_direct_free_node_t, node);
            H5FL_FREE(H5HF_section_free_node_t, sec_node);
        } /* end if */
        else {
            /* Allocate object from end of free space node */
            /* (so we don't have to adjust with any other node's info */
            obj_off = (node->my_offset + node->size) - full_obj_size;
            node->size -= full_obj_size;

            /* Adjust information for section node */
            sec_node->sect_size -= full_obj_size;

            /* Re-insert section node onto global list */
            if(H5HF_flist_add(shared->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add direct block free space to global list")
        } /* end else */

        /* Mark free list as dirty */
        dblock->free_list->dirty = TRUE;

        /* Compute the size of the space to actually allocate */
        /* (includes the metadata for the object & the free space fragment) */
        alloc_obj_size = full_obj_size + free_frag_size;

#ifdef QAK
HDfprintf(stderr, "%s: obj_off = %Zu\n", FUNC, obj_off);
HDfprintf(stderr, "%s: free_frag_size = %Zu\n", FUNC, free_frag_size);
HDfprintf(stderr, "%s: full_obj_size = %Zu\n", FUNC, full_obj_size);
HDfprintf(stderr, "%s: alloc_obj_size = %Zu\n", FUNC, alloc_obj_size);
#endif /* QAK */
        /* Reduce space available in parent block(s) */
        if(H5HF_man_dblock_adj_free(dblock, -(ssize_t)alloc_obj_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for direct block & parents")

        /* Encode the object in the block */

        /* Point to location for object */
        p = dblock->blk + obj_off;

        /* Encode the length */
        UINT64ENCODE_VAR(p, obj_size, dblock->blk_off_size);

        /* Encode the free fragment size */
        *p++ = free_frag_size;

        /* Encode a ref. count (of 1), if required */
        if(shared->ref_count_obj)
            UINT64ENCODE_VAR(p, 1, shared->ref_count_size);

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

        /* Set the heap ID for the new object */
#ifdef QAK
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
#endif /* QAK */
        UINT64ENCODE_VAR(id, (dblock->block_off + obj_off), shared->heap_off_size);
    } /* end if */
    else {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "inserting within mapped managed blocks not supported yet")
    } /* end else */

    /* Update statistics about heap */
    shared->nobjs++;

    /* Mark heap header as modified */
    shared->dirty = TRUE;

done:
    /* Release the direct block (marked as dirty) */
    if(dblock && H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__DIRTIED_FLAG) < 0)
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
H5HF_man_read(H5RC_t *fh_shared, hid_t dxpl_id, hsize_t obj_off, void *obj)
{
    H5HF_parent_shared_t par_shared;    /* Parent shared information */
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_direct_t *dblock;              /* Pointer to direct block to query */
    size_t blk_off;                     /* Offset of object in block */
    uint8_t *p;                         /* Temporary pointer to obj info in block */
    size_t obj_size;                    /* Size of object */
    haddr_t dblock_addr;                /* Direct block address */
    size_t dblock_size;                 /* Direct block size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_read)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(obj_off > 0);
    HDassert(obj);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Check for root direct block */
    if(shared->man_dtable.curr_root_rows == 0) {
        /* Set direct block info */
        par_shared.shared = fh_shared;
        par_shared.parent = NULL;
        par_shared.parent_entry = 0;
        par_shared.parent_free_space = shared->total_man_free;

        dblock_addr =  shared->man_dtable.table_addr;
        dblock_size =  shared->man_dtable.cparam.start_block_size;

        /* Lock direct block */
        if(NULL == (dblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &dblock_size, &par_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")
    } /* end if */
    else {
        haddr_t iblock_addr;            /* Indirect block's address */
        H5HF_indirect_t *iblock;        /* Pointer to indirect block */
        unsigned row, col;              /* Row & column for object's block */
        size_t entry;                   /* Entry of block */

        /* Look up row & column for object */
        if(H5HF_dtable_lookup(&shared->man_dtable, obj_off, &row, &col) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of object")
#ifdef QAK
HDfprintf(stderr, "%s: row = %u, col = %u\n", FUNC, row, col);
#endif /* QAK */

        /* Set initial indirect block info */
        iblock_addr = shared->man_dtable.table_addr;

        /* Lock indirect block */
        par_shared.shared = fh_shared;
        par_shared.parent = NULL;
        par_shared.parent_entry = 0;
        par_shared.parent_free_space = shared->total_man_free;
        if(NULL == (iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &shared->man_dtable.curr_root_rows, &par_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

        /* Check for indirect block row */
        while(row >= shared->man_dtable.max_direct_rows) {
            haddr_t new_iblock_addr;       /* New indirect block's address */
            H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
            unsigned nrows;                /* Number of rows in new indirect block */

            /* Compute # of rows in child indirect block */
            nrows = (H5V_log2_gen(shared->man_dtable.row_block_size[row]) - shared->man_dtable.first_row_bits) + 1;

            /* Compute indirect block's entry */
            entry = (row * shared->man_dtable.cparam.width) + col;

            /* Locate child indirect block */
            new_iblock_addr = iblock->ents[entry].addr;

            /* Lock new indirect block */
            par_shared.shared = fh_shared;
            par_shared.parent = iblock->self;
            par_shared.parent_entry = entry;
            par_shared.parent_free_space = iblock->ents[entry].free_space;
            if(NULL == (new_iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, &par_shared, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

            /* Release the current indirect block (possibly marked as dirty) */
            if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

            /* Switch variables to use new indirect block */
            iblock = new_iblock;
            iblock_addr = new_iblock_addr;

            /* Look up row & column in new indirect block for object */
            if(H5HF_dtable_lookup(&shared->man_dtable, (obj_off - iblock->block_off), &row, &col) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTCOMPUTE, FAIL, "can't compute row & column of object")
        } /* end while */

        /* Compute direct block's entry */
        entry = (row * shared->man_dtable.cparam.width) + col;
#ifdef QAK
HDfprintf(stderr, "%s: entry address = %a\n", FUNC, iblock->dblock_ents[entry].addr);
#endif /* QAK */

        /* Set direct block info */
        par_shared.shared = fh_shared;
        par_shared.parent = iblock->self;
        par_shared.parent_entry = entry;
        par_shared.parent_free_space = iblock->ents[entry].free_space;

        dblock_addr =  iblock->ents[entry].addr;
        dblock_size =  shared->man_dtable.row_block_size[row];

        /* Lock direct block */
        if(NULL == (dblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &dblock_size, &par_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Unlock indirect block */
        if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        iblock = NULL;
    } /* end else */

    /* Compute offset of object within block */
    HDassert((obj_off - dblock->block_off) < (hsize_t)dblock_size);
    blk_off = (size_t)(obj_off - dblock->block_off);

    /* Point to location for object */
    p = dblock->blk + blk_off;

    /* Decode the length */
    UINT64DECODE_VAR(p, obj_size, dblock->blk_off_size);

    /* Skip over the free fragment size & ref count */
    p += 1 + (shared->ref_count_obj ?  shared->ref_count_size : 0);

    /* Copy the object's data into the heap */
    HDmemcpy(obj, p, obj_size);

    /* Unlock direct block */
    if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")
    dblock = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_read() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_free
 *
 * Purpose:	Free fractal heap indirect block
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 13 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_iblock_free(void *_iblock)
{
    H5HF_indirect_t *iblock = (H5HF_indirect_t *)_iblock;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_iblock_free)
#ifdef QAK
HDfprintf(stderr, "%s: Freeing indirect block\n", "H5HF_iblock_free");
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);

    /* Decrement reference count on shared fractal heap info */
    if(iblock->shared)
        H5RC_DEC(iblock->shared);
    if(iblock->parent)
        H5RC_DEC(iblock->parent);

    /* Release entry tables */
    if(iblock->ents)
        H5FL_SEQ_FREE(H5HF_indirect_ent_t, iblock->ents);

    /* Free fractal heap indirect block info */
    H5FL_FREE(H5HF_indirect_t, iblock);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_iblock_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_place_dblock
 *
 * Purpose:	Find indirect block with location for placing a direct block
 *
 * Note:	Creates necessary indirect blocks
 *
 * Return:	Pointer to indirect block on success, NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 14 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_indirect_t *
H5HF_man_iblock_place_dblock(H5RC_t *fh_shared, hid_t dxpl_id,
    size_t min_dblock_size, haddr_t *addr_p, size_t *entry_p,
    size_t *dblock_size)
{
    H5HF_parent_shared_t par_shared;    /* Parent shared information */
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    H5HF_indirect_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_place_dblock)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(min_dblock_size > 0);
    HDassert(addr_p);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Check for creating first indirect block */
    if(shared->man_dtable.curr_root_rows == 0) {
        H5HF_direct_t *dblock;          /* Pointer to direct block to query */
        unsigned nrows;                 /* Number of rows for root indirect block */

#ifdef QAK
HDfprintf(stderr, "%s: creating first indirect block\n", FUNC);
#endif /* QAK */
        /* Check for allocating entire root indirect block initially */
        if(shared->man_dtable.cparam.start_root_rows == 0)
            nrows = shared->man_dtable.max_root_rows;
        else
            nrows = shared->man_dtable.cparam.start_root_rows;

        /* Allocate root indirect block */
        if(H5HF_man_iblock_create(fh_shared, dxpl_id, (hsize_t)0, nrows, shared->man_dtable.max_root_rows, &iblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
#endif /* QAK */

        /* Move current direct block (used as root) into new indirect block */

        /* Lock new indirect block */
        par_shared.shared = fh_shared;
        par_shared.parent = NULL;
        par_shared.parent_entry = 0;
        par_shared.parent_free_space = 0;
        if(NULL == (iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &nrows, &par_shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

        /* Lock first (root) direct block */
        par_shared.parent_free_space = shared->total_man_free;
        if(NULL == (dblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, shared->man_dtable.table_addr, &shared->man_dtable.cparam.start_block_size, &par_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap direct block")

        /* Point indirect block at direct block to add */
        iblock->ents[0].addr = shared->man_dtable.table_addr;
        iblock->ents[0].free_space = dblock->blk_free_space;
        iblock->child_free_space += dblock->blk_free_space;

        /* Make direct block share parent indirect block */
        dblock->parent = iblock->self;
        dblock->parent_entry = 0;
        H5RC_INC(dblock->parent);

        /* Unlock first (root) direct block */
        if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, shared->man_dtable.table_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap direct block")
        dblock = NULL;

        /* Increment size of next block from this indirect block */
        /* (account for the already existing direct block */
        if(H5HF_man_iblock_inc_loc(iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")

        /* Mark indirect block as modified */
        iblock->dirty = TRUE;

        /* Point heap header at new indirect block */
        shared->man_dtable.curr_root_rows = nrows;
        shared->man_dtable.table_addr = iblock_addr;

        /* Mark heap header as modified */
        shared->dirty = TRUE;
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: searching root indirect block\n", FUNC);
#endif /* QAK */

        /* Lock root indirect block */
        par_shared.shared = fh_shared;
        par_shared.parent = NULL;
        par_shared.parent_entry = 0;
        par_shared.parent_free_space = shared->total_man_free;
        iblock_addr = shared->man_dtable.table_addr;
        if(NULL == (iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &shared->man_dtable.curr_root_rows, &par_shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

#ifdef QAK
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
#endif /* QAK */
        /* Check if we need a block past current allocation */
        if(iblock->next_row == iblock->nrows) {
            haddr_t new_addr;           /* New address of indirect block */
            unsigned new_nrows;         /* New # of direct rows */
            size_t u;                   /* Local index variable */

            /* Check for special case of second row, which has blocks the same size as first row */
            if(iblock->next_row == 1)
                iblock->next_size = shared->man_dtable.cparam.start_block_size;

            /* Compute new # of rows in indirect block */
            new_nrows = MIN(2 * iblock->nrows, iblock->max_rows);
#ifdef QAK
HDfprintf(stderr, "%s: new_nrows = %u\n", FUNC, new_nrows);
#endif /* QAK */

/* Currently, the old chunk data is "thrown away" after the space is reallocated,
* so avoid data copy in H5MF_realloc() call by just free'ing the space and
* allocating new space.
*
* This should keep the file smaller also, by freeing the space and then
* allocating new space, instead of vice versa (in H5MF_realloc).
*
* QAK - 3/14/2006
*/
            /* Free previous indirect block disk space */
            if(H5MF_xfree(shared->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock_addr, (hsize_t)iblock->size)<0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, NULL, "unable to free fractal heap indirect block")

            /* Compute size of buffer needed for new indirect block */
            iblock->nrows = new_nrows;
            iblock->size = H5HF_MAN_INDIRECT_SIZE(shared, iblock);

            /* Allocate space for the new indirect block on disk */
            if(HADDR_UNDEF == (new_addr = H5MF_alloc(shared->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
                HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, NULL, "file allocation failed for fractal heap indirect block")

            /* Re-allocate direct block entry table */
            if(NULL == (iblock->ents = H5FL_SEQ_REALLOC(H5HF_indirect_ent_t, iblock->ents, (iblock->nrows * shared->man_dtable.cparam.width))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")

            /* Initialize new direct block entries */
            for(u = (iblock->next_row * shared->man_dtable.cparam.width);
                    u < (iblock->nrows * shared->man_dtable.cparam.width);
                    u++) {
                iblock->ents[u].addr = HADDR_UNDEF;
                iblock->ents[u].free_space = 0;
            } /* end for */

            /* Mark indirect block as dirty */
            iblock->dirty = TRUE;

            /* Release the indirect block (marked as dirty) */
            if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")

            /* Move object in cache */
            if(H5AC_rename(shared->f, H5AC_FHEAP_IBLOCK, iblock_addr, new_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, NULL, "unable to move fractal heap root indirect block")

            /* Update other shared header info */
            shared->man_dtable.curr_root_rows = new_nrows;
            shared->man_dtable.table_addr = iblock_addr = new_addr;

            /* Mark heap header as modified */
            shared->dirty = TRUE;

            /* Lock root indirect block (again) */
            par_shared.shared = fh_shared;
            par_shared.parent = NULL;
            par_shared.parent_entry = 0;
            par_shared.parent_free_space = shared->total_man_free;
            if(NULL == (iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &shared->man_dtable.curr_root_rows, &par_shared, H5AC_WRITE)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
        } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
#endif /* QAK */
        /* Check for full direct block entries in current indirect block */
        while(iblock->next_row >= shared->man_dtable.max_direct_rows) {
            haddr_t new_iblock_addr;       /* New indirect block's address */
            H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
            unsigned hdr_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for header */
            unsigned nrows;                /* Number of rows in new indirect block */

            /* Compute # of rows in child indirect block */
            nrows = (H5V_log2_gen(iblock->next_size) - shared->man_dtable.first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_size = %Hu, nrows = %u\n", FUNC, iblock->next_size, nrows);
#endif /* QAK */

            /* Check for allocating new indirect block */
            if(!H5F_addr_defined(iblock->ents[iblock->next_entry].addr)) {
#ifdef QAK
HDfprintf(stderr, "%s: Allocating new indirect block\n", FUNC);
#endif /* QAK */
                /* Allocate new indirect block */
                if(H5HF_man_iblock_create(fh_shared, dxpl_id, shared->man_dtable.next_dir_block, nrows, nrows, &new_iblock_addr) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")

                /* Lock new indirect block */
                par_shared.shared = fh_shared;
                par_shared.parent = iblock->self;
                par_shared.parent_entry = iblock->next_entry;
                par_shared.parent_free_space = 0;
                if(NULL == (new_iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, &par_shared, H5AC_WRITE)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: new_iblock->next_row = %u\n", FUNC, new_iblock->next_row);
HDfprintf(stderr, "%s: new_iblock->next_col = %u\n", FUNC, new_iblock->next_col);
HDfprintf(stderr, "%s: new_iblock->next_size = %Zu\n", FUNC, new_iblock->next_size);
HDfprintf(stderr, "%s: new_iblock->next_entry = %u\n", FUNC, new_iblock->next_entry);
#endif /* QAK */

                /* Point current indirect block at new indirect block */
                iblock->ents[iblock->next_entry].addr = new_iblock_addr;

                /* Increment location of next block from current indirect block */
                if(H5HF_man_iblock_inc_loc(iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")

                /* Mark current indirect block as modified */
                iblock->dirty = TRUE;

                /* Release the current indirect block (marked as dirty) */
                hdr_flags |= H5AC__DIRTIED_FLAG;
            } /* end if */
            else {
#ifdef QAK
HDfprintf(stderr, "%s: Descending existing indirect block\n", FUNC);
#endif /* QAK */
                /* Locate child indirect block */
                new_iblock_addr = iblock->ents[iblock->next_entry].addr;

                /* Lock new indirect block */
                par_shared.shared = fh_shared;
                par_shared.parent = iblock->self;
                par_shared.parent_entry = iblock->next_entry;
                par_shared.parent_free_space = iblock->ents[iblock->next_entry].free_space;
                if(NULL == (new_iblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, &par_shared, H5AC_WRITE)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
             } /* end else */

            /* Release the current indirect block (possibly marked as dirty) */
            if(H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, hdr_flags) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")

            /* Switch variables to use new indirect block */
            iblock = new_iblock;
            iblock_addr = new_iblock_addr;
#ifdef QAK
HDfprintf(stderr, "%s: new_iblock_addr = %a\n", FUNC, new_iblock_addr);
#endif /* QAK */
        } /* end while */
    } /* end else */

    /* Check for skipping over blocks */
    if(min_dblock_size > iblock->next_size) {
HDfprintf(stderr, "%s: Skipping direct block sizes not supported\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, NULL, "skipping direct block sizes not supported yet")
    } /* end if */

    /* Set address of indirect block that's the immediate parent of new direct block */
    *addr_p = iblock_addr;

    /* Set entry for new direct block to use */
    *entry_p = iblock->next_entry;

    /* Set size of direct block to create */
    *dblock_size = iblock->next_size;

    /* Increment location of next block from this indirect block */
    if(H5HF_man_iblock_inc_loc(iblock) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")

    /* Set return value */
    ret_value = iblock;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_place_dblock() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_create
 *
 * Purpose:	Allocate & initialize a managed indirect block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_create(H5RC_t *fh_shared, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p)
{
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_indirect_t *iblock = NULL;     /* Pointer to indirect block */
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_create)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(nrows > 0);
    HDassert(addr_p);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (iblock = H5FL_MALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap indirect block")

    /* Reset the metadata cache info for the heap header */
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    iblock->shared = fh_shared;
    H5RC_INC(iblock->shared);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(iblock->shared);
    HDassert(shared);

    /* Make a reference to this block */
    if(NULL == (iblock->self = H5RC_create(iblock, H5HF_iblock_free)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for indirect fractal heap block")

#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u, max_nrows = %u\n", FUNC, nrows, max_nrows);
#endif /* QAK */
    /* Set info for direct block */
    iblock->parent = NULL;              /* Temporary, except for root indirect block */
    iblock->parent_entry = 0;
    iblock->block_off = block_off;
    iblock->child_free_space = 0;
    iblock->nrows = nrows;
    iblock->max_rows = max_rows;
    iblock->next_col = 0;
    iblock->next_row = 0;
    iblock->next_entry = 0;
    iblock->next_size = shared->man_dtable.cparam.start_block_size;

    /* Compute size of buffer needed for indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(shared, iblock);

    /* Allocate indirect block entry tables */
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (iblock->nrows * shared->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for block entries")

    /* Initialize indirect block entry tables */
    for(u = 0; u < (iblock->nrows * shared->man_dtable.cparam.width); u++) {
        iblock->ents[u].addr = HADDR_UNDEF;
        iblock->ents[u].free_space = 0;
    } /* end for */

    /* Allocate space for the indirect block on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(shared->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")

/* XXX: Update indirect statistics when they are added */
#ifdef LATER
    /* Update shared heap info */
    shared->total_man_free += dblock->blk_free_space;
    shared->total_size += dblock->size;
    shared->man_size += dblock->size;

    /* Mark heap header as modified */
    shared->dirty = TRUE;
#endif /* LATER */

    /* Cache the new fractal heap header */
    if(H5AC_set(shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, *addr_p, iblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap indirect block to cache")

done:
    if(ret_value < 0)
        if(iblock)
            (void)H5HF_cache_iblock_dest(shared->f, iblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_create() */

