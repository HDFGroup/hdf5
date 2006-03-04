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
#include "H5HFpkg.h"		/* Fractal heaps			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Limit on the size of the max. direct block size */
/* (This is limited to 32-bits currently, because I think it's unlikely to
 *      need to be larger, and the 32-bit limit for H5V_log2(n)  - QAK)
 */
#define H5HL_MAX_DIRECT_SIZE_LIMIT ((hsize_t)2 * 1024 * 1024 * 1024)

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5HF_shared_free(void *_shared);
static herr_t H5HF_man_dblock_create(H5RC_t *fh_shared, hid_t dxpl_id,
    size_t block_size, hsize_t block_off, haddr_t *addr_p);
static herr_t H5HF_man_dblock_insert(H5RC_t *fh_shared, hid_t dxpl_id,
    haddr_t block_addr, size_t block_size, size_t obj_size, const void *obj,
    void *id);

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

/* Declare a free list to manage the H5HF_shared_t struct */
H5FL_DEFINE_STATIC(H5HF_shared_t);



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
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_own)

    /*
     * Check arguments.
     */
    HDassert(fh);
    HDassert(shared);

    /* Compute/cache some values */
    shared->fixed_len_obj = (shared->fixed_len_size > 0);
    shared->ref_count_obj = (shared->ref_count_size > 0);
    shared->heap_off_size = H5HF_SIZEOF_OFFSET_BITS(shared->man_dtable_info.cparam.max_index);

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
H5HF_shared_create(H5F_t *f, H5HF_t *fh, haddr_t fh_addr, H5HF_create_t *cparam)
{
    H5HF_shared_t *shared = NULL;       /* Shared fractal heap information */
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_shared_create)

    /*
     * Check arguments.
     */
    HDassert(f);
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
    if(cparam->managed.max_index > (8 * H5F_SIZEOF_SIZE(f)) || cparam->managed.max_index == 0)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, FAIL, "max. direct block size not power of two")
#endif /* NDEBUG */

    /* Allocate & basic initialization for the shared info struct */
    if(NULL == (shared = H5HF_shared_alloc(f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't allocate space for shared heap info")

    /* Set the creation parameters for the heap */
    shared->heap_addr = fh_addr;
    shared->addrmap = cparam->addrmap;
    shared->standalone_size = cparam->standalone_size;
    shared->fixed_len_size = cparam->fixed_len_size;
    shared->ref_count_size = cparam->ref_count_size;
    HDmemcpy(&(shared->man_dtable_info.cparam), &(cparam->managed), sizeof(H5HF_dtable_cparam_t));

    /* Make shared heap info reference counted */
    if(H5HF_shared_own(fh, shared) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared fractal heap info")

done:
    if(ret_value < 0)
        if(shared)
            H5HF_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_shared_create() */


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

    /* Free the shared info itself */
    H5FL_FREE(H5HF_shared_t, shared);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_man_alloc_end
 *
 * Purpose:	Allocate space for an object at the end of the managed obj. heap
 *
 * Return:	Non-negative on success (with heap ID of new object
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5HF_man_alloc_end(H5RC_t *fh_shared, hid_t dxpl_id, unsigned *fh_flags_ptr,
    size_t size, const void *obj, void *id/*out*/)
{
    H5HF_shared_t *shared;              /* Shared heap information */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_alloc_end)
HDfprintf(stderr, "%s: size = %Zu\n", FUNC, size);

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(fh_flags_ptr);
    HDassert(size > 0);
    HDassert(obj);
    HDassert(id);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Check if this is the first object in the heap */
    if(shared->next_man_block == 0) {
        /* Check if the object can fit in a direct block of the starting block size */
#ifdef QAK
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(shared, shared->man_dtable_info.cparam.start_block_size));
HDfprintf(stderr, "%s: H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE = %u\n", FUNC, H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE(shared, shared->man_dtable_info.cparam.start_block_size));
#endif /* QAK */
        if((size + H5HF_MAN_ABS_DIRECT_OVERHEAD_SIZE(shared, shared->man_dtable_info.cparam.start_block_size)
                    + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_SIZE(shared, shared->man_dtable_info.cparam.start_block_size))
                <= shared->man_dtable_info.cparam.start_block_size) {
            haddr_t dblock_addr;        /* Address of new direct block */
            size_t dblock_size;         /* Size of new direct block */

            /* Allocate initial direct block */
            dblock_size = shared->man_dtable_info.cparam.start_block_size;
            if(H5HF_man_dblock_create(fh_shared, dxpl_id, dblock_size, (hsize_t)0, &dblock_addr) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate fractal heap direct block")
HDfprintf(stderr, "%s: dblock_addr = %a\n", FUNC, dblock_addr);

            /* Point root at new direct block */
            shared->next_man_block = dblock_size;
            shared->man_dtable_info.curr_root_rows = 0;
            shared->man_dtable_info.table_addr = dblock_addr;

            /* Mark heap header as modified */
            *fh_flags_ptr |= H5AC__DIRTIED_FLAG;

            /* Attempt to insert object into direct block */
            if(H5HF_man_dblock_insert(fh_shared, dxpl_id, dblock_addr, dblock_size, size, obj, id) < 0)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't insert object into fractal heap direct block")
        } /* end if */
        else {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "first object doesn't fit into starting direct block")
        } /* end else */
    } /* end if */
    else {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "allocating objects at end of heap not supported yet")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_alloc_end() */


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
H5HF_man_dblock_create(H5RC_t *fh_shared, hid_t dxpl_id, size_t block_size,
    hsize_t block_off, haddr_t *addr_p)
{
    H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_create)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(block_size > 0);
    HDassert(addr_p);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for fractal heap header")

    /* Reset the metadata cache info for the heap header */
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common heap information */
    dblock->shared = fh_shared;
    H5RC_INC(dblock->shared);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(dblock->shared);
    HDassert(shared);

    /* Set info for direct block */
HDfprintf(stderr, "%s: size = %Zu, block_off = %Hu\n", FUNC, block_size, block_off);
    dblock->size = block_size;
    dblock->block_off = block_off;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(block_size);
    dblock->free_list_head = H5HF_MAN_ABS_DIRECT_OVERHEAD_DBLOCK(shared, dblock);
    dblock->blk_free_space = block_size - dblock->free_list_head;

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
    node->size = dblock->blk_free_space;
    node->my_offset = dblock->free_list_head;
    node->next_offset = 0;
    node->prev = node->next = NULL;

    /* Attach to free list head */
    dblock->free_list->first = node;

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(shared->f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)block_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap header")

    /* Cache the new fractal heap header */
    if(H5AC_set(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, *addr_p, dblock, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINIT, FAIL, "can't add fractal heap header to cache")

done:
    if(ret_value < 0)
        if(dblock)
            (void)H5HF_cache_dblock_dest(shared->f, dblock);

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
H5HF_man_dblock_build_freelist(H5HF_direct_t *dblock)
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
        H5HF_direct_free_node_t *node = NULL;   /* Pointer to free list node for block */
        H5HF_direct_free_node_t *prev_node;     /* Pointer to previous free list node for block */
        hsize_t free_len;       /* Length of free list info */
        hsize_t next_off;       /* Next node offset in block */
        hsize_t prev_off;       /* Prev node offset in block */
        uint8_t *p;             /* Temporary pointer to free node info */

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
 * Function:	H5HF_man_dblock_insert
 *
 * Purpose:	Allocate space in a managed direct block
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 28 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_man_dblock_insert(H5RC_t *fh_shared, hid_t dxpl_id, haddr_t block_addr,
    size_t block_size, size_t obj_size, const void *obj, void *id)
{
    H5HF_direct_t *dblock = NULL;       /* Pointer to direct block to modify */
    H5HF_shared_t *shared;              /* Pointer to shared heap info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_man_dblock_insert)

    /*
     * Check arguments.
     */
    HDassert(fh_shared);
    HDassert(H5F_addr_defined(block_addr));
    HDassert(block_size > 0);
    HDassert(obj_size > 0);
    HDassert(obj);
    HDassert(id);

    /* Get the pointer to the shared heap info */
    shared = H5RC_GET_OBJ(fh_shared);
    HDassert(shared);

    /* Lock direct block */
    if(NULL == (dblock = H5AC_protect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, block_addr, &block_size, fh_shared, H5AC_WRITE)))
        HGOTO_ERROR(H5E_HEAP, H5E_CANTPROTECT, FAIL, "unable to load fractal heap direct block")

    /* Check for address mapping type */
    if(shared->addrmap == H5HF_ABSOLUTE) {
        H5HF_direct_free_node_t *node;      /* Pointer to free list node for block */
        size_t full_obj_size;               /* Size of object including metadata */
        size_t obj_off;                     /* Offset of object within block */
        hbool_t found;                      /* Flag to indicate we found a node with enough free space */

        /* Compute full object size, with metadata for object */
        full_obj_size = obj_size + H5HF_MAN_ABS_DIRECT_OBJ_PREFIX_LEN_DBLOCK(shared, dblock);

        /* Sanity check */
        HDassert(dblock->blk_free_space >= full_obj_size);

        /* Check for valid free list */
        if(!dblock->free_list)
            if(H5HF_man_dblock_build_freelist(dblock) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, FAIL, "can't decode free list for block")
        HDassert(dblock->free_list);

        /* Search free list for block large enough to fit object */
        node = dblock->free_list->first;
        found = FALSE;
        while(node) {
            /* Check if this node is large enough to hold object */
            if(node->size >= full_obj_size) {
                /* Check for using exact free space available */
                if(node->size == full_obj_size) {
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

                    /* Release the memory for the free list node */
                    H5FL_FREE(H5HF_direct_free_node_t, node);
                } /* end if */
                else {
                    /* Allocate object from end of free space node */
                    /* (so we don't have to play with the other node's info */
                    obj_off = (node->my_offset + node->size) - full_obj_size;
                    node->size -= full_obj_size;
                } /* end else */

                /* Reduce space available in block */
                dblock->blk_free_space -= full_obj_size;

                /* Mark free list as dirty */
                dblock->free_list->dirty = TRUE;
                found = TRUE;
            } /* end if */

            /* Avance to next node */
            node = node->next;
        } /* end while */

        /* Encode metadata & copy object into block */
        if(found) {
            uint8_t *p;             /* Temporary pointer to obj info */

            /* Point to location for object */
HDfprintf(stderr, "%s: obj_off = %Zu\n", FUNC, obj_off);
HDfprintf(stderr, "%s: full_obj_size = %Zu\n", FUNC, full_obj_size);
            p = dblock->blk + obj_off;

            /* Encode the length, if required */
            if(!shared->fixed_len_obj)
                UINT64ENCODE_VAR(p, obj_size, dblock->blk_off_size);

            /* Encode a ref. count (of 1), if required */
            if(shared->ref_count_obj)
                UINT64ENCODE_VAR(p, 1, shared->ref_count_size);

            /* Copy the object's data into the heap */
            HDmemcpy(p, obj, obj_size);
#ifndef NDEBUG
            p += obj_size;
#endif /* NDEBUG */

            /* Sanity check */
            HDassert((size_t)(p - (dblock->blk  + obj_off)) == full_obj_size);

            /* Set the head ID for the new object */
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
            UINT64ENCODE_VAR(id, (dblock->block_off + obj_off), shared->heap_off_size);
        } /* end if */
        else
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "no space for object in block")

    } /* end if */
    else {
HGOTO_ERROR(H5E_HEAP, H5E_UNSUPPORTED, FAIL, "inserting within mapped managed blocks not supported yet")
    } /* end else */

done:
    /* Release the direct block (marked as dirty) */
    if(dblock && H5AC_unprotect(shared->f, dxpl_id, H5AC_FHEAP_DBLOCK, block_addr, dblock, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_HEAP, H5E_CANTUNPROTECT, FAIL, "unable to release fractal heap direct block")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_man_dblock_insert() */

