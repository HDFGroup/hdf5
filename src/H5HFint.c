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
    size_t      sect_size;                      /* Size of free space section */
                                                /* (section size is "object size", without the metadata overhead, since metadata overhead varies from block to block) */
                                                /* (for range sections, this is the largest single section within the range) */
    enum {H5HF_SECT_SINGLE, H5HF_SECT_RANGE} type;    /* Type of free space section */
    union {
        struct {
            haddr_t     dblock_addr;            /* Address of direct block for free section */
            size_t      dblock_size;            /* Size of direct block */
                                                /* (Needed to retrieve direct block) */
        } single;
        struct {
            haddr_t     iblock_addr;            /* Address of indirect block for free section */
            unsigned    iblock_nrows;           /* Number of rows in indirect block */
                                                /* (Needed to retrieve indirect block) */
            unsigned    entry;                  /* Starting entry in indirect block */
            unsigned    num_entries;            /* Number of entries covered */
            hsize_t     range;                  /* Size of actual free section */
        } range;
    } u;
};


/********************/
/* Local Prototypes */
/********************/

/* Doubling table routines */
static herr_t H5HF_dtable_init(H5HF_dtable_t *dtable);
static herr_t H5HF_dtable_lookup(const H5HF_dtable_t *dtable, hsize_t off,
    unsigned *row, unsigned *col);

/* Shared heap header routines */
static herr_t H5HF_hdr_dirty(hid_t dxpl_id, H5HF_t *hdr);

/* Direct block routines */
static herr_t H5HF_dblock_section_node_free_cb(void *item, void UNUSED *key,
    void UNUSED *op_data);
static herr_t H5HF_man_dblock_create(hid_t dxpl_id, H5HF_t *hdr,
    H5HF_indirect_t *par_iblock, unsigned par_entry, size_t block_size,
    hsize_t block_off, haddr_t *addr_p, H5HF_section_free_node_t **ret_sec_node);
static herr_t H5HF_man_dblock_new(H5HF_t *fh, hid_t dxpl_id,
    size_t request);
static herr_t H5HF_man_dblock_adj_free(hid_t dxpl_id, H5HF_direct_t *dblock, ssize_t amt);

/* Indirect block routines */
static herr_t H5HF_man_iblock_inc_loc(H5HF_indirect_t *iblock);
static herr_t H5HF_iblock_dirty(hid_t dxpl_id, H5HF_indirect_t *iblock);
static herr_t H5HF_man_iblock_adj_free(hid_t dxpl_id, H5HF_indirect_t *iblock, ssize_t amt);
static H5HF_indirect_t * H5HF_man_iblock_place_dblock(H5HF_t *fh, hid_t dxpl_id,
    size_t min_dblock_size, haddr_t *addr_p, size_t *entry_p,
    size_t *dblock_size);
static herr_t H5HF_man_iblock_alloc_range(H5HF_t *hdr, hid_t dxpl_id,
    H5HF_section_free_node_t **sec_node, size_t obj_size);
static herr_t H5HF_man_iblock_create(H5HF_t *fh, hid_t dxpl_id,
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
H5HF_dtable_init(H5HF_dtable_t *dtable)
{
    hsize_t tmp_block_size;             /* Temporary block size */
    hsize_t acc_block_off;              /* Accumulated block offset */
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_dtable_init)

    /*
     * Check arguments.
     */
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
    if(NULL == (dtable->row_block_off = H5MM_malloc(dtable->max_root_rows * sizeof(hsize_t))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create doubling table block offset table")
    tmp_block_size = dtable->cparam.start_block_size;
    acc_block_off = dtable->cparam.start_block_size * dtable->cparam.width;
    dtable->row_block_size[0] = dtable->cparam.start_block_size;
    dtable->row_block_off[0] = 0;
    for(u = 1; u < dtable->max_root_rows; u++) {
        dtable->row_block_size[u] = tmp_block_size;
        dtable->row_block_off[u] = acc_block_off;
        tmp_block_size *= 2;
        acc_block_off *= 2;
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
 * Function:	H5HF_dtable_dest
 *
 * Purpose:	Release information for doubling table
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
H5HF_dtable_dest(H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_dest)

    /*
     * Check arguments.
     */
    HDassert(dtable);

    /* Free the block size lookup table for the doubling table */
    H5MM_xfree(dtable->row_block_size);

    /* Free the block offset lookup table for the doubling table */
    H5MM_xfree(dtable->row_block_off);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_alloc
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
H5HF_alloc(H5F_t *f)
{
    H5HF_t *fh = NULL;          /* Shared fractal heap header */
    H5HF_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_alloc)

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
} /* end H5HF_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_finish_init
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
H5HF_finish_init(H5HF_t *fh)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_finish_init)

    /*
     * Check arguments.
     */
    HDassert(fh);

    /* Compute/cache some values */
    fh->heap_off_size = H5HF_SIZEOF_OFFSET_BITS(fh->man_dtable.cparam.max_index);
    if(H5HF_dtable_init(&fh->man_dtable) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize doubling table info")

    /* Create the free-list structure for the heap */
    if(NULL == (fh->flist = H5HF_flist_create(fh->man_dtable.cparam.max_direct_size, H5HF_dblock_section_node_free_cb)))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't initialize free list info")

    /* Set the size of heap IDs */
    fh->id_len = fh->heap_off_size + MIN(fh->man_dtable.max_dir_blk_off_size,
        ((H5V_log2_gen((hsize_t)fh->standalone_size) + 7) / 8));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_finish_init() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_init
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
H5HF_init(H5HF_t *fh, haddr_t fh_addr, H5HF_create_t *cparam)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_init)

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
    if(H5HF_finish_init(fh) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap header")

done:
    if(ret_value < 0)
        if(fh)
            (void)H5HF_cache_hdr_dest(NULL, fh);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_init() */


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
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_inc_loc)

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Increment block entry */
    iblock->next_entry++;

    /* Increment column */
    iblock->next_col++;

    /* Check for walking off end of column */
    if(iblock->next_col == iblock->shared->man_dtable.cparam.width) {
        /* Reset column */
        iblock->next_col = 0;

        /* Increment row & block size */
        iblock->next_row++;
        if(iblock->next_row > 1)
            iblock->next_size *= 2;

        /* Check for filling up indirect block */
        if(iblock->next_row == iblock->max_rows) {
            /* Check for "full" heap */
            if(iblock->parent == NULL)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")

            /* Increment location for parent indirect block */
            if(H5HF_man_iblock_inc_loc(iblock->parent) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't advance fractal heap block location")
        } /* end if */
    } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
#endif /* QAK */

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
H5HF_man_dblock_create(hid_t dxpl_id, H5HF_t *hdr, H5HF_indirect_t *par_iblock,
    unsigned par_entry, size_t block_size, hsize_t block_off, haddr_t *addr_p,
    H5HF_section_free_node_t **ret_sec_node)
{
    H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
    H5HF_section_free_node_t *sec_node; /* Pointer to free list section for block */
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
    dblock->blk_free_space = 0;         /* temporarily, this is modified later */
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
    if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
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

    /* Adjust free space to include new block's space */
    if(H5HF_man_dblock_adj_free(dxpl_id, dblock, (ssize_t)sec_node->sect_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for direct block & parents")

    /* Update shared heap header */
    hdr->total_size += dblock->size;
    hdr->man_size += dblock->size;

    /* Mark heap header as modified */
    if(H5HF_hdr_dirty(dxpl_id, hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")

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
        H5HF_section_free_node_t *sec_node;     /* Pointer to free list section for block */
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
        if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
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
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
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
static herr_t
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
HDfprintf(stderr, "%s: iblock->child_free_space = %Hu\n", FUNC, iblock->child_free_space);
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
static herr_t
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
H5HF_man_find(H5HF_t *fh, hid_t dxpl_id, size_t request,
    H5HF_section_free_node_t **sec_node/*out*/)
{
    htri_t node_found;                  /* Whether an existing free list node was found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_find)
#ifdef QAK
HDfprintf(stderr, "%s: request = %Zu\n", FUNC, request);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(request > 0);
    HDassert(sec_node);

    /* Look for free space in global free list */
    if((node_found = H5HF_flist_find(fh->flist, request, (void **)sec_node)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't locate free space in fractal heap direct block")

/* XXX: Make certain we've loaded all the direct blocks in the heap */

    /* If we didn't find a node, go make one big enough to hold the requested block */
    if(!node_found) {
        /* Allocate direct block big enough to hold requested size */
        if(H5HF_man_dblock_new(fh, dxpl_id, request) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCREATE, FAIL, "can't create fractal heap direct block")

        /* Request space from the free list */
        /* (Ought to be able to be filled, now) */
        if(H5HF_flist_find(fh->flist, request, (void **)sec_node) <= 0)
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
H5HF_man_insert(H5HF_t *hdr, hid_t dxpl_id, H5HF_section_free_node_t *sec_node,
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

    /* Check for range section */
    if(sec_node->type == H5HF_SECT_RANGE) {
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
HDfprintf(stderr, "%s: sec_node->sect_size = %Zu\n", FUNC, sec_node->sect_size);
HDfprintf(stderr, "%s: sec_node->u.range.iblock_addr = %a\n", FUNC, sec_node->u.range.iblock_addr);
HDfprintf(stderr, "%s: sec_node->u.range.iblock_nrows = %u\n", FUNC, sec_node->u.range.iblock_nrows);
HDfprintf(stderr, "%s: sec_node->u.range.entry = %u\n", FUNC, sec_node->u.range.entry);
HDfprintf(stderr, "%s: sec_node->u.range.num_entries = %u\n", FUNC, sec_node->u.range.num_entries);
HDfprintf(stderr, "%s: sec_node->u.range.range = %Hu\n", FUNC, sec_node->u.range.range);
#endif /* QAK */
        /* Allocate 'single' selection out of range selection */
        if(H5HF_man_iblock_alloc_range(hdr, dxpl_id, &sec_node, obj_size) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't break up range free section")
    } /* end if */

    /* Lock direct block */
#ifdef QAK
HDfprintf(stderr, "%s: sec_node->sect_addr = %a\n", FUNC, sec_node->sect_addr);
HDfprintf(stderr, "%s: sec_node->sect_size = %Zu\n", FUNC, sec_node->sect_size);
HDfprintf(stderr, "%s: sec_node->u.single.dblock_addr = %a\n", FUNC, sec_node->u.single.dblock_addr);
HDfprintf(stderr, "%s: sec_node->u.single.dblock_size = %Zu\n", FUNC, sec_node->u.single.dblock_size);
#endif /* QAK */
    dblock_addr = sec_node->u.single.dblock_addr;
    if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &sec_node->u.single.dblock_size, hdr, H5AC_WRITE)))
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
/* XXX: Change to using skip list */
        obj_off = sec_node->sect_addr - dblock->block_off;
        node = dblock->free_list->first;
        while(node->my_offset != obj_off)
            node = node->next;

        /* Compute full object size, with metadata for object */
        full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);

        /* Sanity checks */
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
        if(H5HF_man_dblock_adj_free(dxpl_id, dblock, -(ssize_t)free_obj_size) < 0)
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
    if(H5HF_hdr_dirty(dxpl_id, hdr) < 0)
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
H5HF_man_read(H5HF_t *hdr, hid_t dxpl_id, hsize_t obj_off, size_t obj_len, void *obj)
{
    H5HF_direct_t *dblock;              /* Pointer to direct block to query */
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
    HDassert(obj_off > 0);
    HDassert(obj);

    /* Check for root direct block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        /* Set direct block info */
        dblock_addr =  hdr->man_dtable.table_addr;
        dblock_size =  hdr->man_dtable.cparam.start_block_size;

        /* Lock direct block */
        if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &dblock_size, hdr, H5AC_READ)))
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

        /* Lock indirect block */
        if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &hdr->man_dtable.curr_root_rows, hdr, H5AC_READ)))
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

            /* Locate child indirect block */
            new_iblock_addr = iblock->ents[entry].addr;

            /* Lock new indirect block */
            if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

            /* Release the current indirect block (possibly marked as dirty) */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

            /* Switch variables to use new indirect block */
            iblock = new_iblock;
            iblock_addr = new_iblock_addr;

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
        if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, dblock_addr, &dblock_size, hdr, H5AC_READ)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap direct block")

        /* Unlock indirect block */
        if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
        iblock = NULL;
    } /* end else */

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
static herr_t
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
 * Function:	H5HF_iblock_incr
 *
 * Purpose:	Increment reference count on shared indirect block
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
H5HF_iblock_incr(H5HF_indirect_t *iblock)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_iblock_incr)

    /* Sanity check */
    HDassert(iblock);

/* XXX: When "un-evictable" feature is finished, mark the block as
 *      unevictable on the first block to share it.  - QAK
 */

    /* Increment reference count on shared indirect block */
    iblock->rc++;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_iblock_incr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_decr
 *
 * Purpose:	Decrement reference count on shared indirect block
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
H5HF_iblock_decr(H5HF_indirect_t *iblock)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_iblock_decr)

    /* Sanity check */
    HDassert(iblock);

    /* Decrement reference count on shared indirect block */
    iblock->rc--;

/* XXX: When "un-evictable" feature is finished, mark the block as
 *      evictable when the ref. count drops to zero.  - QAK
 */
/* XXX: Take this call out after "un-evictable" flag is working */
    H5HF_cache_iblock_dest_real(iblock);


    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_iblock_decr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_iblock_dirty
 *
 * Purpose:	Mark indirect block as dirty
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 21 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_iblock_dirty(hid_t dxpl_id, H5HF_indirect_t *iblock)
{
    H5HF_indirect_t *tmp_iblock;        /* Temporary pointer to indirect block */
    hbool_t is_protected;               /* Whether the indirect block is protected */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_iblock_dirty)
#ifdef QAK
HDfprintf(stderr, "%s: Marking indirect block as dirty\n", FUNC);
#endif /* QAK */

    /* Sanity check */
    HDassert(iblock);

/* XXX: When "un-evictable" feature is finished, just mark the block as dirty
 *      in the cache, instead of this protect -> unprotect kludge - QAK
 */
    /* Protect the indirect block */
    is_protected = iblock->cache_info.is_protected;
    if(!is_protected) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->addr = %a\n", FUNC, iblock->addr);
#endif /* QAK */
        if(NULL == (tmp_iblock = H5AC_protect(iblock->shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, &iblock->nrows, iblock->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")
        HDassert(iblock == tmp_iblock);
    } /* end if */

    /* Set the dirty flags for the indirect block */
    iblock->dirty = TRUE;

    /* Release the indirect block (marked as dirty) */
    if(!is_protected) {
        if(H5AC_unprotect(iblock->shared->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock->addr, tmp_iblock, H5AC__DIRTIED_FLAG) < 0)
            HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_iblock_dirty() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_iblock_adj_free
 *
 * Purpose:	Adjust the free space for an indirect block, and it's parents
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 28 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_adj_free(hid_t dxpl_id, H5HF_indirect_t *iblock, ssize_t amt)
{
    H5HF_t *hdr;                        /* Shared heap information */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_adj_free)
#ifdef QAK
HDfprintf(stderr, "%s: amt = %Zd\n", FUNC, amt);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(iblock);

    /* Get the pointer to the shared heap header */
    hdr = iblock->shared;

    /* Adjust space available in block */
    HDassert(amt > 0 || iblock->child_free_space >= (hsize_t)-amt);
    iblock->child_free_space += amt;

    /* Check if the parent info is set */
    while(iblock->parent) {
        size_t par_entry;               /* Entry in parent */

        /* Get the pointer to the shared parent indirect block */
        par_entry = iblock->par_entry;
        iblock = iblock->parent;
        HDassert(iblock);

        /* Adjust this indirect block's child free space */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->child_free_space = %Hu\n", FUNC, iblock->child_free_space);
#endif /* QAK */
        HDassert(amt > 0 || iblock->ents[par_entry].free_space >= (hsize_t)-amt);
        iblock->ents[par_entry].free_space += amt;
        HDassert(amt > 0 || iblock->child_free_space >= (hsize_t)-amt);
        iblock->child_free_space += amt;

        /* Mark indirect block as dirty */
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark indirect block as dirty")
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
} /* end H5HF_man_iblock_adj_free() */


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
H5HF_man_iblock_place_dblock(H5HF_t *hdr, hid_t dxpl_id, size_t min_dblock_size,
    haddr_t *addr_p, size_t *entry_p, size_t *dblock_size)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    H5HF_indirect_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_place_dblock)
#ifdef QAK
HDfprintf(stderr, "%s: min_dblock_size = %Zu\n", FUNC, min_dblock_size);
#endif /* QAK */

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(min_dblock_size > 0);
    HDassert(addr_p);

    /* Check for creating first indirect block */
    if(hdr->man_dtable.curr_root_rows == 0) {
        H5HF_direct_t *dblock;          /* Pointer to direct block to query */
        hbool_t have_direct_block;      /* Flag to indicate a direct block already exists */
        unsigned nrows;                 /* Number of rows for root indirect block */

#ifdef QAK
HDfprintf(stderr, "%s: creating first indirect block\n", FUNC);
#endif /* QAK */
        /* Check for allocating entire root indirect block initially */
        if(hdr->man_dtable.cparam.start_root_rows == 0)
            nrows = hdr->man_dtable.max_root_rows;
        else {
            unsigned rows_needed;   /* Number of rows needed to get to direct block size */

            nrows = hdr->man_dtable.cparam.start_root_rows;
            rows_needed = 2 + (H5V_log2_of2(min_dblock_size) - H5V_log2_of2(hdr->man_dtable.cparam.start_block_size));
            if(nrows < rows_needed)
                nrows = rows_needed;
        } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u\n", FUNC, nrows);
#endif /* QAK */

        /* Allocate root indirect block */
        if(H5HF_man_iblock_create(hdr, dxpl_id, (hsize_t)0, nrows, hdr->man_dtable.max_root_rows, &iblock_addr) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")
#ifdef QAK
HDfprintf(stderr, "%s: iblock_addr = %a\n", FUNC, iblock_addr);
#endif /* QAK */

        /* Move current direct block (used as root) into new indirect block */

        /* Lock new indirect block */
        if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &nrows, hdr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

        /* Check if there's already a direct block as root) */
        have_direct_block = H5F_addr_defined(hdr->man_dtable.table_addr);
        if(have_direct_block) {
            /* Lock first (root) direct block */
            if(NULL == (dblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, &hdr->man_dtable.cparam.start_block_size, hdr, H5AC_READ)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap direct block")

            /* Point indirect block at direct block to add */
            iblock->ents[0].addr = hdr->man_dtable.table_addr;
            iblock->ents[0].free_space = dblock->blk_free_space;
            iblock->child_free_space += dblock->blk_free_space;

            /* Make direct block share parent indirect block */
            dblock->parent = iblock;
            dblock->par_entry = 0;
            dblock->par_addr = iblock->addr;
            dblock->par_nrows = iblock->nrows;
            if(H5HF_iblock_incr(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

            /* Unlock first (root) direct block */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, dblock, H5AC__NO_FLAGS_SET) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap direct block")
            dblock = NULL;

            /* Increment size of next block from this indirect block */
            /* (account for the already existing direct block */
            if(H5HF_man_iblock_inc_loc(iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't advance fractal heap block location")
        } /* end if */

        /* Check for skipping over direct blocks, in order to get to large enough block */
        if(min_dblock_size > iblock->next_size) {
            H5HF_section_free_node_t *sec_node; /* Pointer to free list section for range */
            size_t dblock_free_space;   /* Size of free space for direct block */
            hsize_t range;              /* Size range skipped over */
            unsigned cur_entry;         /* Current entry */
            unsigned u, v;              /* Local index variables */

            /* Initialize information for rows skipped over */
            cur_entry = (unsigned)have_direct_block;
            range = 0;
            for(u = 0; u < (nrows - 1); u++) {
                /* Compute free space in direct blocks for this row */
                dblock_free_space = hdr->man_dtable.row_block_size[u] - 
                        (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, hdr->man_dtable.row_block_size[u])
                            + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));

                /* Initialize information for this row */
                if(u == 0 && have_direct_block)
                    v = 1;
                else
                    v = 0;
                for(; v < hdr->man_dtable.cparam.width; v++) {
                    /* Initialize entry for this row */
                    iblock->ents[cur_entry].addr = HADDR_UNDEF;
                    iblock->ents[cur_entry].free_space = dblock_free_space;

                    /* Increment block and heap's free space */
                    if(H5HF_man_iblock_adj_free(dxpl_id, iblock, (ssize_t)dblock_free_space) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, NULL, "can't adjust free space for indirect block & parents")

                    /* Increment range skipped */
                    range += hdr->man_dtable.row_block_size[u];

                    /* Move to next entry */
                    cur_entry++;
                } /* end for */
            } /* end for */

            /* Set indirect block's "next entry" information */
            iblock->next_col = 0;
            iblock->next_row = u;
            iblock->next_size = hdr->man_dtable.row_block_size[u];
            iblock->next_entry = cur_entry;
            HDassert(iblock->next_size == min_dblock_size);

            /* Create free space section for blocks skipped over */

            /* Create free list section node for blocks skipped over */
            if(NULL == (sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct block free list section")

            /* Set section's information */
            sec_node->sect_addr = (hsize_t)have_direct_block * hdr->man_dtable.row_block_size[0];    /* Range starts at offset 0 in the heap */
            sec_node->sect_size = dblock_free_space;
            sec_node->type = H5HF_SECT_RANGE;
            sec_node->u.range.iblock_addr = iblock->addr;
            sec_node->u.range.iblock_nrows = iblock->nrows;
            sec_node->u.range.entry = (unsigned)have_direct_block;
            sec_node->u.range.num_entries = cur_entry - (unsigned)have_direct_block;
            sec_node->u.range.range = range;

            /* Add new free space to the global list of space */
            if(H5HF_flist_add(hdr->flist, sec_node, &sec_node->sect_size, &sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, NULL, "can't add indirect block free space to global list")
        } /* end else */

        /* Mark indirect block as modified */
        if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

        /* Point heap header at new indirect block */
        hdr->man_dtable.curr_root_rows = nrows;
        hdr->man_dtable.table_addr = iblock_addr;

        /* Mark heap header as modified */
        hdr->dirty = TRUE;
    } /* end if */
    else {
#ifdef QAK
HDfprintf(stderr, "%s: searching root indirect block\n", FUNC);
#endif /* QAK */

        /* Lock root indirect block */
        iblock_addr = hdr->man_dtable.table_addr;
        if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &hdr->man_dtable.curr_root_rows, hdr, H5AC_WRITE)))
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
                iblock->next_size = hdr->man_dtable.cparam.start_block_size;

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
            if(H5MF_xfree(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, iblock_addr, (hsize_t)iblock->size)<0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTFREE, NULL, "unable to free fractal heap indirect block")

            /* Compute size of buffer needed for new indirect block */
            iblock->nrows = new_nrows;
            iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

            /* Allocate space for the new indirect block on disk */
            if(HADDR_UNDEF == (new_addr = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
                HGOTO_ERROR(H5E_STORAGE, H5E_NOSPACE, NULL, "file allocation failed for fractal heap indirect block")

            /* Re-allocate direct block entry table */
            if(NULL == (iblock->ents = H5FL_SEQ_REALLOC(H5HF_indirect_ent_t, iblock->ents, (iblock->nrows * hdr->man_dtable.cparam.width))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")

            /* Initialize new direct block entries */
            for(u = (iblock->next_row * hdr->man_dtable.cparam.width);
                    u < (iblock->nrows * hdr->man_dtable.cparam.width);
                    u++) {
                iblock->ents[u].addr = HADDR_UNDEF;
                iblock->ents[u].free_space = 0;
            } /* end for */

            /* Mark indirect block as dirty */
            if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

            /* Release the indirect block (marked as dirty) */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
                HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, NULL, "unable to release fractal heap indirect block")
            iblock = NULL;

            /* Move object in cache */
            if(H5AC_rename(hdr->f, H5AC_FHEAP_IBLOCK, iblock_addr, new_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTSPLIT, NULL, "unable to move fractal heap root indirect block")

            /* Update other shared header info */
            hdr->man_dtable.curr_root_rows = new_nrows;
            hdr->man_dtable.table_addr = iblock_addr = new_addr;

            /* Mark heap header as modified */
            hdr->dirty = TRUE;

            /* Lock root indirect block (again) */
            if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &hdr->man_dtable.curr_root_rows, hdr, H5AC_WRITE)))
                HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
            iblock->addr = iblock_addr;
        } /* end if */

#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_row = %u\n", FUNC, iblock->next_row);
HDfprintf(stderr, "%s: iblock->next_col = %u\n", FUNC, iblock->next_col);
HDfprintf(stderr, "%s: iblock->next_size = %Zu\n", FUNC, iblock->next_size);
HDfprintf(stderr, "%s: iblock->next_entry = %u\n", FUNC, iblock->next_entry);
#endif /* QAK */
        /* Check for full direct block entries in current indirect block */
        while(iblock->next_row >= hdr->man_dtable.max_direct_rows) {
            haddr_t new_iblock_addr;       /* New indirect block's address */
            H5HF_indirect_t *new_iblock;   /* Pointer to new indirect block */
            unsigned hdr_flags = H5AC__NO_FLAGS_SET;    /* Metadata cache flags for header */
            unsigned nrows;                /* Number of rows in new indirect block */

            /* Compute # of rows in child indirect block */
            nrows = (H5V_log2_gen(iblock->next_size) - hdr->man_dtable.first_row_bits) + 1;
#ifdef QAK
HDfprintf(stderr, "%s: iblock->next_size = %Hu, nrows = %u\n", FUNC, iblock->next_size, nrows);
#endif /* QAK */

            /* Check for allocating new indirect block */
            if(!H5F_addr_defined(iblock->ents[iblock->next_entry].addr)) {
                hsize_t new_iblock_off;         /* Direct block offset in heap address space */
#ifdef QAK
HDfprintf(stderr, "%s: Allocating new indirect block\n", FUNC);
#endif /* QAK */
                /* Compute the direct block's offset in the heap's address space */
                new_iblock_off = iblock->block_off;
                new_iblock_off += hdr->man_dtable.row_block_off[iblock->next_entry / hdr->man_dtable.cparam.width];
                new_iblock_off += hdr->man_dtable.row_block_size[iblock->next_entry / hdr->man_dtable.cparam.width] * (iblock->next_entry % hdr->man_dtable.cparam.width);

                /* Allocate new indirect block */
                if(H5HF_man_iblock_create(hdr, dxpl_id, new_iblock_off, nrows, nrows, &new_iblock_addr) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, NULL, "can't allocate fractal heap indirect block")

                /* Lock new indirect block */
                if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_WRITE)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")

                /* Set parent information */
                HDassert(new_iblock->parent == NULL);
                new_iblock->parent = iblock;
                new_iblock->par_entry = iblock->next_entry;
                new_iblock->par_nrows = iblock->nrows;
                new_iblock->par_addr = iblock->addr;
                if(H5HF_iblock_incr(iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

                /* Point current indirect block at new indirect block */
                iblock->ents[iblock->next_entry].addr = new_iblock_addr;

                /* Mark current indirect block as modified */
                if(H5HF_iblock_dirty(dxpl_id, iblock) < 0)
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, NULL, "can't mark indirect block as dirty")

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
                if(NULL == (new_iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, new_iblock_addr, &nrows, hdr, H5AC_WRITE)))
                    HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, NULL, "unable to protect fractal heap indirect block")
             } /* end else */
#ifdef QAK
HDfprintf(stderr, "%s: new_iblock->next_row = %u\n", FUNC, new_iblock->next_row);
HDfprintf(stderr, "%s: new_iblock->next_col = %u\n", FUNC, new_iblock->next_col);
HDfprintf(stderr, "%s: new_iblock->next_size = %Zu\n", FUNC, new_iblock->next_size);
HDfprintf(stderr, "%s: new_iblock->next_entry = %u\n", FUNC, new_iblock->next_entry);
#endif /* QAK */

            /* Release the current indirect block (possibly marked as dirty) */
            if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, hdr_flags) < 0)
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
 * Function:	H5HF_man_iblock_alloc_range
 *
 * Purpose:	Allocate a "single" section for an object, out of a "range"
 *              section
 *
 * Note:	Creates necessary direct & indirect blocks
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 28 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_iblock_alloc_range(H5HF_t *hdr, hid_t dxpl_id,
    H5HF_section_free_node_t **sec_node, size_t obj_size)
{
    H5HF_indirect_t *iblock;            /* Pointer to indirect block */
    haddr_t iblock_addr;                /* Indirect block's address */
    haddr_t dblock_addr;                /* Direct block's address */
    unsigned iblock_nrows;              /* Indirect block's number of rows */
    H5HF_section_free_node_t *dblock_sec_node = NULL;     /* Pointer to direct block's section node */
    H5HF_section_free_node_t *old_sec_node = *sec_node;     /* Pointer to old section node */
    size_t full_obj_size;               /* Size of object including metadata */
    unsigned cur_entry;                 /* Current entry in indirect block */
    unsigned cur_row;                   /* Current row in indirect block */
    size_t row_size;                    /* Size of objects in current row */
    hsize_t range_covered;              /* Range covered while searching for block to use */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_alloc_range)

    /*
     * Check arguments.
     */
    HDassert(hdr);
    HDassert(sec_node && *sec_node);
    HDassert(obj_size > 0);

    /* Compute info about range */
    cur_entry = old_sec_node->u.range.entry; 

    /* Check for range covering indirect blocks */
    if((cur_entry / hdr->man_dtable.cparam.width) >= hdr->man_dtable.max_direct_rows ||
        ((cur_entry + old_sec_node->u.range.num_entries) / hdr->man_dtable.cparam.width)
            >= hdr->man_dtable.max_direct_rows) {
HDfprintf(stderr, "%s: Can't handle range sections over indirect blocks yet\n", FUNC);
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "'range' free space sections over indirect blocks not supported yet")
    } /* end if */

    /* Get information about indirect block covering section */
    /* (Allow for root indirect block being resized) */
    iblock_addr = old_sec_node->u.range.iblock_addr;
    if(H5F_addr_eq(iblock_addr, hdr->man_dtable.table_addr))
        iblock_nrows = hdr->man_dtable.curr_root_rows;
    else
        iblock_nrows = old_sec_node->u.range.iblock_nrows;

    /* Get a pointer to the indirect block covering the range */
    if(NULL == (iblock = H5AC_protect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, &iblock_nrows, hdr, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to protect fractal heap indirect block")

    /* Compute size of object, with metadata overhead */
    full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr);

    /* Look for first direct block that will fulfill request */
    range_covered = 0;
    for(u = 0; u < old_sec_node->u.range.num_entries; ) {
        unsigned skip_blocks;           /* # of blocks to skip over to next row */

        /* Compute informatio about current row */
        cur_row = cur_entry / hdr->man_dtable.cparam.width;
        row_size = hdr->man_dtable.row_block_size[cur_row];

        /* Check if first available block in this row will hold block */
        if((H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, row_size) + full_obj_size) <= row_size)
            break;

        /* Compute number of blocks to skip in row */
        skip_blocks = ((cur_row + 1) * hdr->man_dtable.cparam.width) - cur_entry;

        /* Advance values to beginning of next row */
        range_covered += row_size * skip_blocks;
        u += skip_blocks;
        cur_entry += skip_blocks;
    } /* end for */
#ifdef QAK
HDfprintf(stderr, "%s: cur_entry = %u, cur_row = %u, row_size = %Zu\n", FUNC, cur_entry, cur_row, row_size);
HDfprintf(stderr, "%s: range_covered = %Hu, u = %u\n", FUNC, range_covered, u);
HDfprintf(stderr, "%s: old_sec_node->u.range.num_entries = %u\n", FUNC, old_sec_node->u.range.num_entries);
#endif /* QAK */
    /* Must find direct block of useful size */
    HDassert(u < old_sec_node->u.range.num_entries);

    /* Create direct block of appropriate size */
    if(H5HF_man_dblock_create(dxpl_id, hdr, iblock, cur_entry, row_size, (hsize_t)(old_sec_node->sect_addr + range_covered), &dblock_addr, &dblock_sec_node) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")

    /* Hook direct block up to indirect block */
    iblock->ents[cur_entry].addr = dblock_addr;
    iblock->ents[cur_entry].free_space = dblock_sec_node->sect_size;

    /* Adjust free space in block & heap */
    if(H5HF_man_iblock_adj_free(dxpl_id, iblock, -(ssize_t)dblock_sec_node->sect_size) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't adjust free space for indirect block & parents")

    /* Break range section up into zero, one or two range sections and add them
     * back to the free sections for the file
     */

    /* Check for only single block covered in range section */
    if(old_sec_node->u.range.num_entries == 1)
        H5FL_FREE(H5HF_section_free_node_t, old_sec_node);
    else {
        size_t next_block_size;            /* Next entry after block used */

        /* Check for using first block in range section */
        if(u == 0) {
#ifdef QAK
HDfprintf(stderr, "%s: range type 1\n", FUNC);
#endif /* QAK */
            /* Adjust section information */
            old_sec_node->sect_addr += row_size;
            /* Section size stays the same, since we just grabbed the smallest block in the range */

            /* Adjust range information */
            old_sec_node->u.range.entry++;
            old_sec_node->u.range.num_entries--;
            old_sec_node->u.range.range -= row_size;

            /* Add section back to free space list */
            if(H5HF_flist_add(hdr->flist, old_sec_node, &old_sec_node->sect_size, &old_sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
        } /* end if */
        /* Check for using middle block from range section */
        else if(u < (old_sec_node->u.range.num_entries -1)) {
            H5HF_section_free_node_t *new_sec_node; /* Pointer to new free list section for block */
#ifdef QAK
HDfprintf(stderr, "%s: range type 2\n", FUNC);
#endif /* QAK */

            /* Create new section node for space after block used */
            if(NULL == (new_sec_node = H5FL_MALLOC(H5HF_section_free_node_t)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for indirect block free list section")

            /* Set section information for new node */
            new_sec_node->sect_addr = old_sec_node->sect_addr + range_covered + row_size;
            new_sec_node->sect_size = old_sec_node->sect_size;

            /* Set range information */
            new_sec_node->type = H5HF_SECT_RANGE;
            new_sec_node->u.range.iblock_addr = old_sec_node->u.range.iblock_addr;
            new_sec_node->u.range.iblock_nrows = old_sec_node->u.range.iblock_nrows;
            new_sec_node->u.range.entry = cur_entry + 1;
            new_sec_node->u.range.num_entries = old_sec_node->u.range.num_entries - (u + 1);
            new_sec_node->u.range.range = old_sec_node->u.range.range - (range_covered + row_size);

            /* Add new section to free space list */
            if(H5HF_flist_add(hdr->flist, new_sec_node, &new_sec_node->sect_size, &new_sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")


            /* Compute size of block before the one used */
            next_block_size = hdr->man_dtable.row_block_size[(cur_entry - 1) /
                hdr->man_dtable.cparam.width];

            /* Adjust current section information */
            old_sec_node->sect_size = next_block_size - 
                (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, row_size) + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));

            /* Adjust range information */
            old_sec_node->u.range.num_entries = u;
            old_sec_node->u.range.range = range_covered;

            /* Add section back to free space list */
            if(H5HF_flist_add(hdr->flist, old_sec_node, &old_sec_node->sect_size, &old_sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
        } /* end if */
        /* Must be using last block in range section */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: range type 3\n", FUNC);
#endif /* QAK */
            /* Compute size of next block after the one used */
            next_block_size = hdr->man_dtable.row_block_size[(cur_entry - 1) /
                hdr->man_dtable.cparam.width];

            /* Adjust section information */
            old_sec_node->sect_size = next_block_size - 
                (H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(hdr, row_size) + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN(hdr));

            /* Adjust range information */
            old_sec_node->u.range.num_entries--;
            old_sec_node->u.range.range -= row_size;

            /* Add section back to free space list */
            if(H5HF_flist_add(hdr->flist, old_sec_node, &old_sec_node->sect_size, &old_sec_node->sect_addr) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add indirect block free space to global list")
        } /* end else */
    } /* end if */

    /* Release the indirect block (marked as dirty) */
    if(H5AC_unprotect(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, iblock_addr, iblock, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap indirect block")

    /* Point 'sec_node' at new direct block section node */
    *sec_node = dblock_sec_node;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_alloc_range() */


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
H5HF_man_iblock_create(H5HF_t *hdr, hid_t dxpl_id,
    hsize_t block_off, unsigned nrows, unsigned max_rows, haddr_t *addr_p)
{
    H5HF_indirect_t *iblock = NULL;     /* Pointer to indirect block */
    size_t u;                           /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_iblock_create)

    /*
     * Check arguments.
     */
    HDassert(hdr);
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
    iblock->shared = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, FAIL, "can't increment reference count on shared heap header")

#ifdef QAK
HDfprintf(stderr, "%s: nrows = %u, max_nrows = %u\n", FUNC, nrows, max_nrows);
#endif /* QAK */
    /* Set info for direct block */
    iblock->rc = 0;
    iblock->parent = NULL;              /* Temporary, except for root indirect block */
    iblock->par_entry = 0;
    iblock->par_nrows = 0;
    iblock->par_addr = HADDR_UNDEF;
    iblock->block_off = block_off;
    iblock->child_free_space = 0;
    iblock->nrows = nrows;
    iblock->max_rows = max_rows;
    iblock->next_col = 0;
    iblock->next_row = 0;
    iblock->next_entry = 0;
    iblock->next_size = hdr->man_dtable.cparam.start_block_size;
    iblock->dirty = TRUE;
    iblock->evicted = FALSE;

    /* Compute size of buffer needed for indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);

    /* Allocate indirect block entry tables */
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (iblock->nrows * hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for block entries")

    /* Initialize indirect block entry tables */
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        iblock->ents[u].addr = HADDR_UNDEF;
        iblock->ents[u].free_space = 0;
    } /* end for */

    /* Allocate space for the indirect block on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(hdr->f, H5FD_MEM_FHEAP_IBLOCK, dxpl_id, (hsize_t)iblock->size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap indirect block")
    iblock->addr = *addr_p;

/* XXX: Update indirect statistics when they are added */

    /* Cache the new fractal heap header */
    if(H5AC_set(hdr->f, dxpl_id, H5AC_FHEAP_IBLOCK, *addr_p, iblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap indirect block to cache")

done:
    if(ret_value < 0)
        if(iblock)
            (void)H5HF_cache_iblock_dest(hdr->f, iblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_iblock_create() */

