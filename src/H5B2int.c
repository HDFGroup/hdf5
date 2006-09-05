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
 * Created:		H5B2int.c
 *			Feb 27 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Internal routines for managing v2 B-trees.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#define H5B2_PACKAGE		/*suppress error about including H5B2pkg  */

/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5B2pkg.h"		/* v2 B-trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/****************/
/* Local Macros */
/****************/

/* Number of records that fit into internal node */
/* (accounts for extra node pointer by counting it in with the prefix bytes) */
#define H5B2_NUM_INT_REC(f, s, d) \
    (((s)->node_size - (H5B2_INT_PREFIX_SIZE + H5B2_INT_POINTER_SIZE(f, s, d))) / ((s)->rrec_size + H5B2_INT_POINTER_SIZE(f, s, d)))

/* Number of records that fit into leaf node */
#define H5B2_NUM_LEAF_REC(n, r) \
    (((n) - H5B2_LEAF_PREFIX_SIZE) / (r))

/* Uncomment this macro to enable extra sanity checking */
/* #define H5B2_DEBUG */

/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Helper functions */
static herr_t H5B2_shared_free(void *_shared);
static herr_t H5B2_create_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *node_ptr, unsigned depth);
static herr_t H5B2_split1(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_node_ptr_t *curr_node_ptr, unsigned *parent_cache_info_flags_ptr,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx);
static herr_t H5B2_redistribute2(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_internal_t *internal, unsigned idx);
static herr_t H5B2_redistribute3(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx);
static herr_t H5B2_merge2(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_node_ptr_t *curr_node_ptr, unsigned *parent_cache_info_flags_ptr,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx);
static herr_t H5B2_merge3(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_node_ptr_t *curr_node_ptr, unsigned *parent_cache_info_flags_ptr,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx);
static herr_t H5B2_swap_leaf(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr,
    unsigned idx, void *swap_loc);
#ifdef H5B2_DEBUG
static herr_t H5B2_assert_leaf(H5B2_shared_t *shared, H5B2_leaf_t *leaf);
static herr_t H5B2_assert_leaf2(H5B2_shared_t *shared, H5B2_leaf_t *leaf, H5B2_leaf_t *leaf2);
static herr_t H5B2_assert_internal(hsize_t parent_all_nrec, H5B2_shared_t *shared, H5B2_internal_t *internal);
static herr_t H5B2_assert_internal2(hsize_t parent_all_nrec, H5B2_shared_t *shared, H5B2_internal_t *internal, H5B2_internal_t *internal2);
#endif /* H5B2_DEBUG */

/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5B2_internal_t struct */
H5FL_DEFINE(H5B2_internal_t);

/* Declare a free list to manage the H5B2_leaf_t struct */
H5FL_DEFINE(H5B2_leaf_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage B-tree node pages to/from disk */
H5FL_BLK_DEFINE_STATIC(node_page);

/* Declare a free list to manage the 'size_t' sequence information */
H5FL_SEQ_DEFINE_STATIC(size_t);

/* Declare a free list to manage the 'H5B2_node_info_t' sequence information */
H5FL_SEQ_DEFINE_STATIC(H5B2_node_info_t);

/* Declare a free list to manage the H5B2_shared_t struct */
H5FL_DEFINE_STATIC(H5B2_shared_t);



/*-------------------------------------------------------------------------
 * Function:	H5B2_shared_init
 *
 * Purpose:	Allocate & initialize shared B-tree info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  2 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_shared_init (H5F_t *f, H5B2_t *bt2, const H5B2_class_t *type,
    unsigned depth, size_t node_size, size_t rrec_size,
    unsigned split_percent, unsigned merge_percent)
{
    H5B2_shared_t *shared = NULL;       /* Shared B-tree information */
    unsigned u;                         /* Local index variable */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_shared_init)

    /* Allocate space for the shared information */
    if(NULL == (shared = H5FL_CALLOC(H5B2_shared_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree shared information")

    /* Assign dynamic information */
    shared->depth = depth;

    /* Assign user's information */
    shared->split_percent = split_percent;
    shared->merge_percent = merge_percent;
    shared->node_size = node_size;
    shared->rrec_size = rrec_size;

    /* Assign common type information */
    shared->type = type;

    /* Allocate "page" for node I/O */
    if((shared->page = H5FL_BLK_MALLOC(node_page, shared->node_size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_USING_PURIFY
HDmemset(shared->page,0,shared->node_size);
#endif /* H5_USING_PURIFY */

    /* Allocate array of node info structs */
    if((shared->node_info = H5FL_SEQ_MALLOC(H5B2_node_info_t, (size_t)(shared->depth + 1))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize leaf node info */
    shared->node_info[0].max_nrec = H5B2_NUM_LEAF_REC(shared->node_size, shared->rrec_size);
    shared->node_info[0].split_nrec = (shared->node_info[0].max_nrec * shared->split_percent) / 100;
    shared->node_info[0].merge_nrec = (shared->node_info[0].max_nrec * shared->merge_percent) / 100;
    shared->node_info[0].cum_max_nrec = shared->node_info[0].max_nrec;
    shared->node_info[0].cum_max_nrec_size = 0;
    if((shared->node_info[0].nat_rec_fac = H5FL_fac_init(type->nrec_size * shared->node_info[0].max_nrec)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't create node native key block factory")
    shared->node_info[0].node_ptr_fac = NULL;

    /* Allocate array of pointers to internal node native keys */
    /* (uses leaf # of records because its the largest) */
    if((shared->nat_off = H5FL_SEQ_MALLOC(size_t, (size_t)shared->node_info[0].max_nrec)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Initialize offsets in native key block */
    /* (uses leaf # of records because its the largest) */
    for(u = 0; u < shared->node_info[0].max_nrec; u++)
        shared->nat_off[u] = type->nrec_size * u;

    /* Compute size to store # of records in each node */
    /* (uses leaf # of records because its the largest) */
    shared->max_nrec_size = (H5V_log2_gen((hsize_t)shared->node_info[0].max_nrec) + 7) / 8;
    HDassert(shared->max_nrec_size <= H5B2_SIZEOF_RECORDS_PER_NODE);

    /* Initialize internal node info */
    if(depth > 0) {
        for(u = 1; u < (depth + 1); u++) {
            shared->node_info[u].max_nrec = H5B2_NUM_INT_REC(f, shared, u);
            HDassert(shared->node_info[u].max_nrec <= shared->node_info[u - 1].max_nrec);

            shared->node_info[u].split_nrec = (shared->node_info[u].max_nrec * shared->split_percent) / 100;
            shared->node_info[u].merge_nrec = (shared->node_info[u].max_nrec * shared->merge_percent) / 100;

            shared->node_info[u].cum_max_nrec = ((shared->node_info[u].max_nrec + 1) * 
                shared->node_info[u - 1].cum_max_nrec) + shared->node_info[u].max_nrec;
            shared->node_info[u].cum_max_nrec_size = (H5V_log2_gen((hsize_t)shared->node_info[u].cum_max_nrec) + 7) / 8;

            if((shared->node_info[u].nat_rec_fac = H5FL_fac_init(shared->type->nrec_size * shared->node_info[u].max_nrec)) == NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't create node native key block factory")
            if((shared->node_info[u].node_ptr_fac = H5FL_fac_init(sizeof(H5B2_node_ptr_t) * (shared->node_info[u].max_nrec + 1))) == NULL)
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't create internal 'branch' node node pointer block factory")
        } /* end for */
    } /* end if */

    /* Make shared B-tree info reference counted */
    if(NULL == (bt2->shared = H5RC_create(shared, H5B2_shared_free)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared B-tree info")

done:
    if(ret_value < 0)
        if(shared)
            H5B2_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_shared_init() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_shared_free
 *
 * Purpose:	Free shared B-tree info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  2 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_shared_free(void *_shared)
{
    H5B2_shared_t *shared = (H5B2_shared_t *)_shared;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_shared_free)

    /* Sanity check */
    HDassert(shared);

    /* Free the B-tree node buffer */
    if(shared->page)
        H5FL_BLK_FREE(node_page, shared->page);

    /* Free the array of offsets into the native key block */
    if(shared->nat_off)
        H5FL_SEQ_FREE(size_t, shared->nat_off);

    /* Release the node info */
    if(shared->node_info) {
        unsigned u;             /* Local index variable */

        /* Destroy free list factories */
        for(u = 0; u < (shared->depth + 1); u++) {
            if(shared->node_info[u].nat_rec_fac)
                if(H5FL_fac_term(shared->node_info[u].nat_rec_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's native record block factory")
            if(shared->node_info[u].node_ptr_fac)
                if(H5FL_fac_term(shared->node_info[u].node_ptr_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's node pointer block factory")
        } /* end for */

        /* Free the array of node info structs */
        H5FL_SEQ_FREE(H5B2_node_info_t, shared->node_info);
    } /* end if */

    /* Free the shared B-tree info itself */
    H5FL_FREE(H5B2_shared_t, shared);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_locate_record
 *
 * Purpose:	Performs a binary search to locate a record in a sorted
 *              array of records.
 *
 *              Sets *IDX to location of record greater than or equal to
 *              record to locate.
 *
 * Return:	Comparison value for insertion location.  Negative for record
 *              to locate being less than value in *IDX.  Zero for record to
 *              locate equal to value in *IDX.  Positive for record to locate
 *              being greater than value in *IDX (which should only happen when
 *              record to locate is greater than all records to search).
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  3 2005
 *
 *-------------------------------------------------------------------------
 */
int
H5B2_locate_record(const H5B2_class_t *type, unsigned nrec, size_t *rec_off,
    const uint8_t *native, const void *udata, unsigned *idx)
{
    unsigned	lo = 0, hi;     /* Low & high index values */
    unsigned    my_idx = 0;     /* Final index value */
    int         cmp = -1;       /* Key comparison value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_locate_record)

    hi = nrec;
    while(lo < hi && cmp) {
	my_idx = (lo + hi) / 2;
	if((cmp = (type->compare)(udata, native + rec_off[my_idx])) < 0)
	    hi = my_idx;
	else
	    lo = my_idx + 1;
    }

    *idx = my_idx;

    FUNC_LEAVE_NOAPI(cmp);
} /* end H5B2_locate_record */


/*-------------------------------------------------------------------------
 * Function:	H5B2_split1
 *
 * Purpose:	Perform a 1->2 node split
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 28 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_split1(H5F_t *f, hid_t dxpl_id, unsigned depth, H5B2_node_ptr_t *curr_node_ptr,
    unsigned *parent_cache_info_flags_ptr, H5B2_internal_t *internal,
    unsigned *internal_flags_ptr, unsigned idx)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t left_addr, right_addr;      /* Addresses of left & right child nodes */
    void *left_child, *right_child;     /* Pointers to child nodes */
    unsigned *left_nrec, *right_nrec;   /* Pointers to child # of records */
    uint8_t *left_native, *right_native;/* Pointers to childs' native records */
    H5B2_node_ptr_t *left_node_ptrs=NULL, *right_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    unsigned mid_record;                /* Index of "middle" record in current node */
    unsigned old_node_nrec;             /* Number of records in internal node split */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_split1)

    HDassert(f);
    HDassert(parent_cache_info_flags_ptr);
    HDassert(internal);
    HDassert(internal_flags_ptr);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Slide records in parent node up one space, to make room for promoted record */
    if(idx < internal->nrec) {
        HDmemmove(H5B2_INT_NREC(internal, shared, idx + 1), H5B2_INT_NREC(internal, shared, idx), shared->type->nrec_size * (internal->nrec - idx));
        HDmemmove(&(internal->node_ptrs[idx + 2]), &(internal->node_ptrs[idx + 1]), sizeof(H5B2_node_ptr_t) * (internal->nrec - idx));
    } /* end if */

    /* Check for the kind of B-tree node to split */
    if(depth > 1) {
        H5B2_internal_t *left_int = NULL, *right_int = NULL;       /* Pointers to old & new internal nodes */

        /* Create new internal node */
        internal->node_ptrs[idx + 1].all_nrec = internal->node_ptrs[idx + 1].node_nrec = 0;
        if(H5B2_create_internal(f, dxpl_id, internal->shared, &(internal->node_ptrs[idx + 1]), (depth - 1)) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create new internal node")

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_INT;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx + 1].addr;

        /* Protect both leafs */
        if(NULL == (left_int = H5B2_protect_internal(f, dxpl_id, internal->shared, left_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if(NULL == (right_int = H5B2_protect_internal(f, dxpl_id, internal->shared, right_addr, internal->node_ptrs[idx + 1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* More setup for child nodes */
        left_child = left_int;
        right_child = right_int;
        left_nrec = &(left_int->nrec);
        right_nrec = &(right_int->nrec);
        left_native = left_int->int_native;
        right_native = right_int->int_native;
        left_node_ptrs = left_int->node_ptrs;
        right_node_ptrs = right_int->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *left_leaf = NULL, *right_leaf = NULL;       /* Pointers to old & new leaf nodes */

        /* Create new leaf node */
        internal->node_ptrs[idx + 1].all_nrec = internal->node_ptrs[idx + 1].node_nrec = 0;
        if(H5B2_create_leaf(f, dxpl_id, internal->shared, &(internal->node_ptrs[idx + 1])) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create new leaf node")

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx + 1].addr;

        /* Protect both leafs */
        if(NULL == (left_leaf = H5AC_protect(f, dxpl_id, child_class, left_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if(NULL == (right_leaf = H5AC_protect(f, dxpl_id, child_class, right_addr, &(internal->node_ptrs[idx + 1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for child nodes */
        left_child = left_leaf;
        right_child = right_leaf;
        left_nrec = &(left_leaf->nrec);
        right_nrec = &(right_leaf->nrec);
        left_native = left_leaf->leaf_native;
        right_native = right_leaf->leaf_native;
    } /* end if */

    /* Get the number of records in node to split */
    old_node_nrec = internal->node_ptrs[idx].node_nrec;

    /* Determine "middle" record to promote to internal node */
    mid_record = old_node_nrec / 2;

    /* Copy "upper half" of records to new child */
    HDmemcpy(H5B2_NAT_NREC(right_native, shared, 0),
            H5B2_NAT_NREC(left_native, shared, mid_record + 1),
            shared->type->nrec_size * (old_node_nrec - (mid_record + 1)));

    /* Copy "upper half" of node pointers, if the node is an internal node */
    if(depth > 1)
        HDmemcpy(&(right_node_ptrs[0]), &(left_node_ptrs[mid_record + 1]),
                sizeof(H5B2_node_ptr_t) * (old_node_nrec - mid_record));

    /* Copy "middle" record to internal node */
    HDmemcpy(H5B2_INT_NREC(internal, shared, idx), H5B2_NAT_NREC(left_native, shared, mid_record), shared->type->nrec_size);

    /* Update record counts in child nodes */
    internal->node_ptrs[idx].node_nrec = *left_nrec = mid_record;
    internal->node_ptrs[idx + 1].node_nrec = *right_nrec = old_node_nrec - (mid_record + 1);

    /* Determine total number of records in new child nodes */
    if(depth > 1) {
        unsigned u;             /* Local index variable */
        hsize_t new_left_all_nrec;     /* New total number of records in left child */
        hsize_t new_right_all_nrec;    /* New total number of records in right child */

        /* Compute total of all records in each child node */
        new_left_all_nrec = internal->node_ptrs[idx].node_nrec;
        for(u = 0; u < (*left_nrec + 1); u++)
            new_left_all_nrec += left_node_ptrs[u].all_nrec;

        new_right_all_nrec = internal->node_ptrs[idx + 1].node_nrec;
        for(u = 0; u < (*right_nrec + 1); u++)
            new_right_all_nrec += right_node_ptrs[u].all_nrec;

        internal->node_ptrs[idx].all_nrec = new_left_all_nrec;
        internal->node_ptrs[idx + 1].all_nrec = new_right_all_nrec;
    } /* end if */
    else {
        internal->node_ptrs[idx].all_nrec = internal->node_ptrs[idx].node_nrec;
        internal->node_ptrs[idx + 1].all_nrec = internal->node_ptrs[idx + 1].node_nrec;
    } /* end else */

    /* Update # of records in parent node */
    internal->nrec++;

    /* Mark parent as dirty */
    *internal_flags_ptr |= H5AC__DIRTIED_FLAG;

    /* Update grandparent info */
    curr_node_ptr->node_nrec++;

    /* Mark grandparent as dirty */
    *parent_cache_info_flags_ptr |= H5AC__DIRTIED_FLAG;

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth > 1) {
        H5B2_assert_internal2(internal->node_ptrs[idx].all_nrec, shared, left_child, right_child);
        H5B2_assert_internal2(internal->node_ptrs[idx + 1].all_nrec, shared, right_child, left_child);
    } /* end if */
    else {
        H5B2_assert_leaf2(shared, left_child, right_child);
        H5B2_assert_leaf(shared, right_child);
    } /* end else */
#endif /* H5B2_DEBUG */

    /* Release child nodes (marked as dirty) */
    if(H5AC_unprotect(f, dxpl_id, child_class, left_addr, left_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree leaf node")
    if(H5AC_unprotect(f, dxpl_id, child_class, right_addr, right_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_split1 */


/*-------------------------------------------------------------------------
 * Function:	H5B2_split_root
 *
 * Purpose:	Split the root node
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  3 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_split_root(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, unsigned *bt2_flags_ptr)
{
    H5B2_internal_t *new_root;          /* Pointer to new root node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned new_root_flags = H5AC__NO_FLAGS_SET;   /* Cache flags for new root node */
    H5B2_node_ptr_t old_root_ptr;       /* Old node pointer to root node in B-tree */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_split_root)

    HDassert(f);
    HDassert(bt2);
    HDassert(bt2_flags_ptr);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Update depth of B-tree */
    shared->depth++;

    /* Re-allocate array of node info structs */
    if((shared->node_info = H5FL_SEQ_REALLOC(H5B2_node_info_t, shared->node_info, (size_t)(shared->depth + 1))) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Update node info for new depth of tree */
    shared->node_info[shared->depth].max_nrec = H5B2_NUM_INT_REC(f, shared, shared->depth);
    shared->node_info[shared->depth].split_nrec = (shared->node_info[shared->depth].max_nrec * shared->split_percent) / 100;
    shared->node_info[shared->depth].merge_nrec = (shared->node_info[shared->depth].max_nrec * shared->merge_percent) / 100;
    shared->node_info[shared->depth].cum_max_nrec = ((shared->node_info[shared->depth].max_nrec + 1) * 
        shared->node_info[shared->depth - 1].cum_max_nrec) + shared->node_info[shared->depth].max_nrec;
    shared->node_info[shared->depth].cum_max_nrec_size = (H5V_log2_gen((hsize_t)shared->node_info[shared->depth].cum_max_nrec) + 7) / 8;
    if((shared->node_info[shared->depth].nat_rec_fac = H5FL_fac_init(shared->type->nrec_size * shared->node_info[shared->depth].max_nrec)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't create node native key block factory")
    if((shared->node_info[shared->depth].node_ptr_fac = H5FL_fac_init(sizeof(H5B2_node_ptr_t) * (shared->node_info[shared->depth].max_nrec + 1))) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, FAIL, "can't create internal 'branch' node node pointer block factory")

    /* Keep old root node pointer info */
    old_root_ptr = bt2->root;

    /* Create new internal node to use as root */
    bt2->root.node_nrec = 0;
    if(H5B2_create_internal(f, dxpl_id, bt2->shared, &(bt2->root), shared->depth) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create new internal node")

    /* Protect new root node */
    if(NULL == (new_root = H5B2_protect_internal(f, dxpl_id, bt2->shared, bt2->root.addr, bt2->root.node_nrec, shared->depth, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

    /* Set first node pointer in root node to old root node pointer info */
    new_root->node_ptrs[0] = old_root_ptr;

    /* Split original root node */
    if(H5B2_split1(f, dxpl_id, shared->depth, &(bt2->root), bt2_flags_ptr, new_root, &new_root_flags, 0) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split old root node")

    /* Release new root node (marked as dirty) */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, bt2->root.addr, new_root, new_root_flags) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree internal node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_split_root */


/*-------------------------------------------------------------------------
 * Function:	H5B2_redistribute2
 *
 * Purpose:	Redistribute records between two nodes
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  9 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_redistribute2(H5F_t *f, hid_t dxpl_id, unsigned depth, H5B2_internal_t *internal, unsigned idx)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t left_addr, right_addr;      /* Addresses of left & right child nodes */
    void *left_child, *right_child;     /* Pointers to child nodes */
    unsigned *left_nrec, *right_nrec;   /* Pointers to child # of records */
    uint8_t *left_native, *right_native;    /* Pointers to childs' native records */
    H5B2_node_ptr_t *left_node_ptrs=NULL, *right_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    hssize_t left_moved_nrec=0, right_moved_nrec=0; /* Number of records moved, for internal redistrib */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_redistribute2)

    HDassert(f);
    HDassert(internal);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Check for the kind of B-tree node to redistribute */
    if(depth > 1) {
        H5B2_internal_t *left_internal;         /* Pointer to left internal node */
        H5B2_internal_t *right_internal;        /* Pointer to right internal node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_INT;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock left & right B-tree child nodes */
        if (NULL == (left_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, left_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (right_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, right_addr, internal->node_ptrs[idx+1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for child nodes */
        left_child = left_internal;
        right_child = right_internal;
        left_nrec = &(left_internal->nrec);
        right_nrec = &(right_internal->nrec);
        left_native = left_internal->int_native;
        right_native = right_internal->int_native;
        left_node_ptrs = left_internal->node_ptrs;
        right_node_ptrs = right_internal->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *left_leaf;         /* Pointer to left leaf node */
        H5B2_leaf_t *right_leaf;        /* Pointer to right leaf node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock left & right B-tree child nodes */
        if (NULL == (left_leaf = H5AC_protect(f, dxpl_id, child_class, left_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (right_leaf = H5AC_protect(f, dxpl_id, child_class, right_addr, &(internal->node_ptrs[idx+1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for child nodes */
        left_child = left_leaf;
        right_child = right_leaf;
        left_nrec = &(left_leaf->nrec);
        right_nrec = &(right_leaf->nrec);
        left_native = left_leaf->leaf_native;
        right_native = right_leaf->leaf_native;
    } /* end else */

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1) {
        H5B2_assert_internal2(internal->node_ptrs[idx].all_nrec,shared,left_child,right_child);
        H5B2_assert_internal2(internal->node_ptrs[idx+1].all_nrec,shared,right_child,left_child);
    } /* end if */
    else {
        H5B2_assert_leaf2(shared,left_child,right_child);
        H5B2_assert_leaf(shared,right_child);
    } /* end else */
#endif /* H5B2_DEBUG */

    /* Determine whether to shuffle records left or right */
    if(*left_nrec < *right_nrec) {
        /* Moving record from right node to left */

        unsigned new_right_nrec = (*left_nrec + *right_nrec) / 2;             /* New number of records for right child */
        unsigned move_nrec = *right_nrec - new_right_nrec;      /* Number of records to move from right node to left */

        /* Copy record from parent node down into left child */
        HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

        /* See if we need to move records from right node */
        if(move_nrec>1)
            HDmemcpy(H5B2_NAT_NREC(left_native,shared,(*left_nrec+1)),H5B2_NAT_NREC(right_native,shared,0),shared->type->nrec_size*(move_nrec-1));

        /* Move record from right node into parent node */
        HDmemcpy(H5B2_INT_NREC(internal,shared,idx),H5B2_NAT_NREC(right_native,shared,(move_nrec-1)),shared->type->nrec_size);

        /* Slide records in right node down */
        HDmemmove(H5B2_NAT_NREC(right_native,shared,0),H5B2_NAT_NREC(right_native,shared,move_nrec),shared->type->nrec_size*new_right_nrec);

        /* Handle node pointers, if we have an internal node */
        if(depth>1) {
            hsize_t moved_nrec=move_nrec;   /* Total number of records moved, for internal redistrib */
            unsigned u;             /* Local index variable */

            /* Count the number of records being moved */
            for(u=0; u<move_nrec; u++)
                moved_nrec += right_node_ptrs[u].all_nrec;
            left_moved_nrec = moved_nrec;
            right_moved_nrec -= moved_nrec;

            /* Copy node pointers from right node to left */
            HDmemcpy(&(left_node_ptrs[*left_nrec+1]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*move_nrec);

            /* Slide node pointers in right node down */
            HDmemmove(&(right_node_ptrs[0]),&(right_node_ptrs[move_nrec]),sizeof(H5B2_node_ptr_t)*(new_right_nrec+1));
        } /* end if */

        /* Update number of records in child nodes */
        *left_nrec += move_nrec;
        *right_nrec = new_right_nrec;
    } /* end if */
    else {
        /* Moving record from left node to right */

        unsigned new_left_nrec = (*left_nrec + *right_nrec) / 2;    /* New number of records for left child */
        unsigned move_nrec = *left_nrec - new_left_nrec;        /* Number of records to move from left node to right */

        /* Slide records in right node up */
        HDmemmove(H5B2_NAT_NREC(right_native,shared,move_nrec),
                H5B2_NAT_NREC(right_native,shared,0),
                shared->type->nrec_size*(*right_nrec));

        /* Copy record from parent node down into right child */
        HDmemcpy(H5B2_NAT_NREC(right_native,shared,(move_nrec-1)),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

        /* See if we need to move records from left node */
        if(move_nrec>1)
            HDmemcpy(H5B2_NAT_NREC(right_native,shared,0),H5B2_NAT_NREC(left_native,shared,((*left_nrec-move_nrec)+1)),shared->type->nrec_size*(move_nrec-1));

        /* Move record from left node into parent node */
        HDmemcpy(H5B2_INT_NREC(internal,shared,idx),H5B2_NAT_NREC(left_native,shared,(*left_nrec-move_nrec)),shared->type->nrec_size);

        /* Handle node pointers, if we have an internal node */
        if(depth>1) {
            hsize_t moved_nrec=move_nrec;   /* Total number of records moved, for internal redistrib */
            unsigned u;             /* Local index variable */

            /* Slide node pointers in right node up */
            HDmemmove(&(right_node_ptrs[move_nrec]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*(*right_nrec+1));

            /* Copy node pointers from left node to right */
            HDmemcpy(&(right_node_ptrs[0]),&(left_node_ptrs[new_left_nrec+1]),sizeof(H5B2_node_ptr_t)*move_nrec);

            /* Count the number of records being moved */
            for(u=0; u<move_nrec; u++)
                moved_nrec += right_node_ptrs[u].all_nrec;
            left_moved_nrec -= moved_nrec;
            right_moved_nrec = moved_nrec;
        } /* end if */

        /* Update number of records in child nodes */
        *left_nrec = new_left_nrec;
        *right_nrec += move_nrec;
    } /* end else */

    /* Update # of records in child nodes */
    internal->node_ptrs[idx].node_nrec = *left_nrec;
    internal->node_ptrs[idx+1].node_nrec = *right_nrec;

    /* Update total # of records in child B-trees */
    if(depth>1) {
        internal->node_ptrs[idx].all_nrec += left_moved_nrec;
        internal->node_ptrs[idx+1].all_nrec += right_moved_nrec;
    } /* end if */
    else {
        internal->node_ptrs[idx].all_nrec = internal->node_ptrs[idx].node_nrec;
        internal->node_ptrs[idx+1].all_nrec = internal->node_ptrs[idx+1].node_nrec;
    } /* end else */

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1) {
        H5B2_assert_internal2(internal->node_ptrs[idx].all_nrec,shared,left_child,right_child);
        H5B2_assert_internal2(internal->node_ptrs[idx+1].all_nrec,shared,right_child,left_child);
    } /* end if */
    else {
        H5B2_assert_leaf2(shared,left_child,right_child);
        H5B2_assert_leaf(shared,right_child);
    } /* end else */
#endif /* H5B2_DEBUG */

    /* Release child nodes (marked as dirty) */
    if (H5AC_unprotect(f, dxpl_id, child_class, left_addr, left_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")
    if (H5AC_unprotect(f, dxpl_id, child_class, right_addr, right_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_redistribute2 */


/*-------------------------------------------------------------------------
 * Function:	H5B2_redistribute3
 *
 * Purpose:	Redistribute records between three nodes
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  9 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_redistribute3(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t left_addr, right_addr;      /* Addresses of left & right child nodes */
    haddr_t middle_addr;                /* Address of middle child node */
    void *left_child, *right_child;     /* Pointers to child nodes */
    void *middle_child;                 /* Pointers to middle child node */
    unsigned *left_nrec, *right_nrec;   /* Pointers to child # of records */
    unsigned *middle_nrec;              /* Pointers to middle child # of records */
    uint8_t *left_native, *right_native;    /* Pointers to childs' native records */
    uint8_t *middle_native;             /* Pointers to middle child's native records */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    H5B2_node_ptr_t *left_node_ptrs=NULL, *right_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    H5B2_node_ptr_t *middle_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    hssize_t left_moved_nrec=0, right_moved_nrec=0; /* Number of records moved, for internal split */
    hssize_t middle_moved_nrec=0;       /* Number of records moved, for internal split */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_redistribute3)

    HDassert(f);
    HDassert(internal);
    HDassert(internal_flags_ptr);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Check for the kind of B-tree node to redistribute */
    if(depth > 1) {
        H5B2_internal_t *left_internal;         /* Pointer to left internal node */
        H5B2_internal_t *middle_internal;       /* Pointer to middle internal node */
        H5B2_internal_t *right_internal;        /* Pointer to right internal node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_INT;
        left_addr = internal->node_ptrs[idx-1].addr;
        middle_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock B-tree child nodes */
        if (NULL == (left_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, left_addr, internal->node_ptrs[idx-1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if (NULL == (middle_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, middle_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if (NULL == (right_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, right_addr, internal->node_ptrs[idx+1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* More setup for child nodes */
        left_child = left_internal;
        middle_child = middle_internal;
        right_child = right_internal;
        left_nrec = &(left_internal->nrec);
        middle_nrec = &(middle_internal->nrec);
        right_nrec = &(right_internal->nrec);
        left_native = left_internal->int_native;
        middle_native = middle_internal->int_native;
        right_native = right_internal->int_native;
        left_node_ptrs = left_internal->node_ptrs;
        middle_node_ptrs = middle_internal->node_ptrs;
        right_node_ptrs = right_internal->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *left_leaf;         /* Pointer to left leaf node */
        H5B2_leaf_t *middle_leaf;       /* Pointer to middle leaf node */
        H5B2_leaf_t *right_leaf;        /* Pointer to right leaf node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        left_addr = internal->node_ptrs[idx-1].addr;
        middle_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock B-tree child nodes */
        if (NULL == (left_leaf = H5AC_protect(f, dxpl_id, child_class, left_addr, &(internal->node_ptrs[idx-1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (middle_leaf = H5AC_protect(f, dxpl_id, child_class, middle_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (right_leaf = H5AC_protect(f, dxpl_id, child_class, right_addr, &(internal->node_ptrs[idx+1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for child nodes */
        left_child = left_leaf;
        middle_child = middle_leaf;
        right_child = right_leaf;
        left_nrec = &(left_leaf->nrec);
        middle_nrec = &(middle_leaf->nrec);
        right_nrec = &(right_leaf->nrec);
        left_native = left_leaf->leaf_native;
        middle_native = middle_leaf->leaf_native;
        right_native = right_leaf->leaf_native;
    } /* end else */

    /* Redistribute records */
    {
        /* Compute new # of records in each node */
        unsigned total_nrec = *left_nrec + *middle_nrec + *right_nrec + 2;
        unsigned new_middle_nrec = (total_nrec - 2) / 3;
        unsigned new_left_nrec = ((total_nrec - 2) - new_middle_nrec) / 2;
        unsigned new_right_nrec = (total_nrec - 2) - (new_left_nrec + new_middle_nrec);
        unsigned curr_middle_nrec = *middle_nrec;

        /* Sanity check rounding */
        HDassert(new_middle_nrec <= new_left_nrec);
        HDassert(new_middle_nrec <= new_right_nrec);

        /* Move records into left node */
        if(new_left_nrec > *left_nrec) {
            unsigned moved_middle_nrec = 0;      /* Number of records moved into left node */

            /* Move left parent record down to left node */
            HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec),H5B2_INT_NREC(internal,shared,idx-1),shared->type->nrec_size);

            /* Move records from middle node into left node */
            if((new_left_nrec - 1) > *left_nrec) {
                moved_middle_nrec = new_left_nrec-(*left_nrec + 1);
                HDmemcpy(H5B2_NAT_NREC(left_native, shared, *left_nrec + 1),H5B2_NAT_NREC(middle_native, shared, 0), shared->type->nrec_size * moved_middle_nrec);
            } /* end if */

            /* Move record from middle node up to parent node */
            HDmemcpy(H5B2_INT_NREC(internal,shared,idx-1),H5B2_NAT_NREC(middle_native,shared,moved_middle_nrec),shared->type->nrec_size);
            moved_middle_nrec++;

            /* Slide records in middle node down */
            HDmemmove(H5B2_NAT_NREC(middle_native,shared,0),H5B2_NAT_NREC(middle_native,shared,moved_middle_nrec),shared->type->nrec_size*(*middle_nrec-moved_middle_nrec));

            /* Move node pointers also if this is an internal node */
            if(depth>1) {
                hsize_t moved_nrec;         /* Total number of records moved, for internal redistrib */
                unsigned move_nptrs;    /* Number of node pointers to move */
                unsigned u;             /* Local index variable */

                /* Move middle node pointers into left node */
                move_nptrs = new_left_nrec - *left_nrec;
                HDmemcpy(&(left_node_ptrs[*left_nrec+1]),&(middle_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*move_nptrs);

                /* Count the number of records being moved into the left node */
                for(u=0, moved_nrec=0; u<move_nptrs; u++)
                    moved_nrec += middle_node_ptrs[u].all_nrec;
                left_moved_nrec = moved_nrec+move_nptrs;
                middle_moved_nrec -= moved_nrec+move_nptrs;

                /* Slide the node pointers in middle node down */
                HDmemmove(&(middle_node_ptrs[0]),&(middle_node_ptrs[move_nptrs]),sizeof(H5B2_node_ptr_t)*((*middle_nrec-move_nptrs)+1));
            } /* end if */

            /* Update the current number of records in middle node */
            curr_middle_nrec -= moved_middle_nrec;
        } /* end if */

        /* Move records into right node */
        if(new_right_nrec>*right_nrec) {
            unsigned right_nrec_move = new_right_nrec-*right_nrec; /* Number of records to move out of right node */

            /* Slide records in right node up */
            HDmemmove(H5B2_NAT_NREC(right_native,shared,right_nrec_move),H5B2_NAT_NREC(right_native,shared,0),shared->type->nrec_size*(*right_nrec));

            /* Move right parent record down to right node */
            HDmemcpy(H5B2_NAT_NREC(right_native,shared,right_nrec_move-1),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

            /* Move records from middle node into right node */
            if(right_nrec_move>1)
                HDmemcpy(H5B2_NAT_NREC(right_native,shared,0),H5B2_NAT_NREC(middle_native,shared,((curr_middle_nrec-right_nrec_move)+1)),shared->type->nrec_size*(right_nrec_move-1));

            /* Move record from middle node up to parent node */
            HDmemcpy(H5B2_INT_NREC(internal,shared,idx),H5B2_NAT_NREC(middle_native,shared,(curr_middle_nrec-right_nrec_move)),shared->type->nrec_size);

            /* Move node pointers also if this is an internal node */
            if(depth>1) {
                hsize_t moved_nrec;         /* Total number of records moved, for internal redistrib */
                unsigned u;             /* Local index variable */

                /* Slide the node pointers in right node up */
                HDmemmove(&(right_node_ptrs[right_nrec_move]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*(*right_nrec+1));

                /* Move middle node pointers into right node */
                HDmemcpy(&(right_node_ptrs[0]),&(middle_node_ptrs[(curr_middle_nrec-right_nrec_move)+1]),sizeof(H5B2_node_ptr_t)*right_nrec_move);

                /* Count the number of records being moved into the right node */
                for(u=0, moved_nrec=0; u<right_nrec_move; u++)
                    moved_nrec += right_node_ptrs[u].all_nrec;
                right_moved_nrec = moved_nrec+right_nrec_move;
                middle_moved_nrec -= moved_nrec+right_nrec_move;
            } /* end if */

            /* Update the current number of records in middle node */
            curr_middle_nrec -= right_nrec_move;
        } /* end if */

        /* Move records out of left node */
        if(new_left_nrec<*left_nrec) {
            unsigned left_nrec_move = *left_nrec-new_left_nrec; /* Number of records to move out of left node */

            /* Slide middle records up */
            HDmemmove(H5B2_NAT_NREC(middle_native,shared,left_nrec_move),H5B2_NAT_NREC(middle_native,shared,0),shared->type->nrec_size*curr_middle_nrec);

            /* Move left parent record down to middle node */
            HDmemcpy(H5B2_NAT_NREC(middle_native,shared,left_nrec_move-1),H5B2_INT_NREC(internal,shared,idx-1),shared->type->nrec_size);

            /* Move left records to middle node */
            if(left_nrec_move>1)
                HDmemmove(H5B2_NAT_NREC(middle_native,shared,0),H5B2_NAT_NREC(left_native,shared,new_left_nrec+1),shared->type->nrec_size*(left_nrec_move-1));

            /* Move left parent record up from left node */
            HDmemcpy(H5B2_INT_NREC(internal,shared,idx-1),H5B2_NAT_NREC(left_native,shared,new_left_nrec),shared->type->nrec_size);

            /* Move node pointers also if this is an internal node */
            if(depth>1) {
                hsize_t moved_nrec;         /* Total number of records moved, for internal redistrib */
                unsigned u;             /* Local index variable */

                /* Slide the node pointers in middle node up */
                HDmemmove(&(middle_node_ptrs[left_nrec_move]),&(middle_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*(curr_middle_nrec+1));

                /* Move left node pointers into middle node */
                HDmemcpy(&(middle_node_ptrs[0]),&(left_node_ptrs[new_left_nrec+1]),sizeof(H5B2_node_ptr_t)*left_nrec_move);

                /* Count the number of records being moved into the left node */
                for(u=0, moved_nrec=0; u<left_nrec_move; u++)
                    moved_nrec += middle_node_ptrs[u].all_nrec;
                left_moved_nrec -= moved_nrec+left_nrec_move;
                middle_moved_nrec += moved_nrec+left_nrec_move;
            } /* end if */

            /* Update the current number of records in middle node */
            curr_middle_nrec += left_nrec_move;
        } /* end if */

        /* Move records out of right node */
        if(new_right_nrec<*right_nrec) {
            unsigned right_nrec_move = *right_nrec-new_right_nrec; /* Number of records to move out of right node */

            /* Move right parent record down to middle node */
            HDmemcpy(H5B2_NAT_NREC(middle_native,shared,curr_middle_nrec),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

            /* Move right records to middle node */
            HDmemmove(H5B2_NAT_NREC(middle_native,shared,(curr_middle_nrec+1)),H5B2_NAT_NREC(right_native,shared,0),shared->type->nrec_size*(right_nrec_move-1));

            /* Move right parent record up from right node */
            HDmemcpy(H5B2_INT_NREC(internal,shared,idx),H5B2_NAT_NREC(right_native,shared,right_nrec_move-1),shared->type->nrec_size);

            /* Slide right records down */
            HDmemmove(H5B2_NAT_NREC(right_native,shared,0),H5B2_NAT_NREC(right_native,shared,right_nrec_move),shared->type->nrec_size*new_right_nrec);

            /* Move node pointers also if this is an internal node */
            if(depth>1) {
                hsize_t moved_nrec;         /* Total number of records moved, for internal redistrib */
                unsigned u;             /* Local index variable */

                /* Move right node pointers into middle node */
                HDmemcpy(&(middle_node_ptrs[curr_middle_nrec+1]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*right_nrec_move);

                /* Count the number of records being moved into the right node */
                for(u=0, moved_nrec=0; u<right_nrec_move; u++)
                    moved_nrec += right_node_ptrs[u].all_nrec;
                right_moved_nrec -= moved_nrec+right_nrec_move;
                middle_moved_nrec += moved_nrec+right_nrec_move;

                /* Slide the node pointers in right node down */
                HDmemmove(&(right_node_ptrs[0]),&(right_node_ptrs[right_nrec_move]),sizeof(H5B2_node_ptr_t)*(new_right_nrec+1));
            } /* end if */
        } /* end if */

        /* Update # of records in nodes */
        *left_nrec = new_left_nrec;
        *middle_nrec = new_middle_nrec;
        *right_nrec = new_right_nrec;
    } /* end block */

    /* Update # of records in child nodes */
    internal->node_ptrs[idx-1].node_nrec = *left_nrec;
    internal->node_ptrs[idx].node_nrec = *middle_nrec;
    internal->node_ptrs[idx+1].node_nrec = *right_nrec;

    /* Update total # of records in child B-trees */
    if(depth>1) {
        internal->node_ptrs[idx-1].all_nrec += left_moved_nrec;
        internal->node_ptrs[idx].all_nrec += middle_moved_nrec;
        internal->node_ptrs[idx+1].all_nrec += right_moved_nrec;
    } /* end if */
    else {
        internal->node_ptrs[idx-1].all_nrec = internal->node_ptrs[idx-1].node_nrec;
        internal->node_ptrs[idx].all_nrec = internal->node_ptrs[idx].node_nrec;
        internal->node_ptrs[idx+1].all_nrec = internal->node_ptrs[idx+1].node_nrec;
    } /* end else */

    /* Mark parent as dirty */
    *internal_flags_ptr |= H5AC__DIRTIED_FLAG;

#ifdef QAK
{
    unsigned u;

    HDfprintf(stderr,"%s: Internal records:\n",FUNC);
    for(u=0; u<internal->nrec; u++) {
        HDfprintf(stderr,"%s: u=%u\n",FUNC,u);
        (shared->type->debug)(stderr,f,dxpl_id,3,4,H5B2_INT_NREC(internal,shared,u),NULL);
    } /* end for */

    HDfprintf(stderr,"%s: Left Child records:\n",FUNC);
    for(u=0; u<*left_nrec; u++) {
        HDfprintf(stderr,"%s: u=%u\n",FUNC,u);
        (shared->type->debug)(stderr,f,dxpl_id,3,4,H5B2_NAT_NREC(left_native,shared,u),NULL);
    } /* end for */

    HDfprintf(stderr,"%s: Middle Child records:\n",FUNC);
    for(u=0; u<*middle_nrec; u++) {
        HDfprintf(stderr,"%s: u=%u\n",FUNC,u);
        (shared->type->debug)(stderr,f,dxpl_id,3,4,H5B2_NAT_NREC(middle_native,shared,u),NULL);
    } /* end for */

    HDfprintf(stderr,"%s: Right Child records:\n",FUNC);
    for(u=0; u<*right_nrec; u++) {
        HDfprintf(stderr,"%s: u=%u\n",FUNC,u);
        (shared->type->debug)(stderr,f,dxpl_id,3,4,H5B2_NAT_NREC(right_native,shared,u),NULL);
    } /* end for */

    for(u=0; u<internal->nrec+1; u++)
        HDfprintf(stderr,"%s: internal->node_ptrs[%u]=(%Hu/%u/%a)\n",FUNC,u,internal->node_ptrs[u].all_nrec,internal->node_ptrs[u].node_nrec,internal->node_ptrs[u].addr);
    if(depth>1) {
        for(u=0; u<*left_nrec+1; u++)
            HDfprintf(stderr,"%s: left_node_ptr[%u]=(%Hu/%u/%a)\n",FUNC,u,left_node_ptrs[u].all_nrec,left_node_ptrs[u].node_nrec,left_node_ptrs[u].addr);
        for(u=0; u<*middle_nrec+1; u++)
            HDfprintf(stderr,"%s: middle_node_ptr[%u]=(%Hu/%u/%a)\n",FUNC,u,middle_node_ptrs[u].all_nrec,middle_node_ptrs[u].node_nrec,middle_node_ptrs[u].addr);
        for(u=0; u<*right_nrec+1; u++)
            HDfprintf(stderr,"%s: right_node_ptr[%u]=(%Hu/%u/%a)\n",FUNC,u,right_node_ptrs[u].all_nrec,right_node_ptrs[u].node_nrec,right_node_ptrs[u].addr);
    } /* end if */
}
#endif /* QAK */
#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1) {
        H5B2_assert_internal2(internal->node_ptrs[idx-1].all_nrec,shared,left_child,middle_child);
        H5B2_assert_internal2(internal->node_ptrs[idx].all_nrec,shared,middle_child,left_child);
        H5B2_assert_internal2(internal->node_ptrs[idx].all_nrec,shared,middle_child,right_child);
        H5B2_assert_internal2(internal->node_ptrs[idx+1].all_nrec,shared,right_child,middle_child);
    } /* end if */
    else {
        H5B2_assert_leaf2(shared,left_child,middle_child);
        H5B2_assert_leaf2(shared,middle_child,right_child);
        H5B2_assert_leaf(shared,right_child);
    } /* end else */
#endif /* H5B2_DEBUG */

    /* Unlock child nodes (marked as dirty) */
    if (H5AC_unprotect(f, dxpl_id, child_class, left_addr, left_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")
    if (H5AC_unprotect(f, dxpl_id, child_class, middle_addr, middle_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")
    if (H5AC_unprotect(f, dxpl_id, child_class, right_addr, right_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_redistribute3 */


/*-------------------------------------------------------------------------
 * Function:	H5B2_merge2
 *
 * Purpose:	Perform a 2->1 node merge
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  4 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_merge2(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_node_ptr_t *curr_node_ptr, unsigned *parent_cache_info_flags_ptr,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t left_addr, right_addr;      /* Addresses of left & right child nodes */
    void *left_child, *right_child;     /* Pointers to left & right child nodes */
    unsigned *left_nrec, *right_nrec;   /* Pointers to left & right child # of records */
    uint8_t *left_native, *right_native;    /* Pointers to left & right children's native records */
    H5B2_node_ptr_t *left_node_ptrs=NULL, *right_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_merge2)

    HDassert(f);
    HDassert(curr_node_ptr);
    HDassert(parent_cache_info_flags_ptr);
    HDassert(internal);
    HDassert(internal_flags_ptr);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Check for the kind of B-tree node to split */
    if(depth > 1) {
        H5B2_internal_t *left_internal;         /* Pointer to left internal node */
        H5B2_internal_t *right_internal;        /* Pointer to right internal node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_INT;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock left & right B-tree child nodes */
        if(NULL == (left_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, left_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if(NULL == (right_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, right_addr, internal->node_ptrs[idx+1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* More setup for accessing child node information */
        left_child = left_internal;
        right_child = right_internal;
        left_nrec = &(left_internal->nrec);
        right_nrec = &(right_internal->nrec);
        left_native = left_internal->int_native;
        right_native = right_internal->int_native;
        left_node_ptrs = left_internal->node_ptrs;
        right_node_ptrs = right_internal->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *left_leaf;         /* Pointer to left leaf node */
        H5B2_leaf_t *right_leaf;        /* Pointer to right leaf node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        left_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock left & right B-tree child nodes */
        if(NULL == (left_leaf = H5AC_protect(f, dxpl_id, child_class, left_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if(NULL == (right_leaf = H5AC_protect(f, dxpl_id, child_class, right_addr, &(internal->node_ptrs[idx+1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for accessing child node information */
        left_child = left_leaf;
        right_child = right_leaf;
        left_nrec = &(left_leaf->nrec);
        right_nrec = &(right_leaf->nrec);
        left_native = left_leaf->leaf_native;
        right_native = right_leaf->leaf_native;
    } /* end else */

    /* Redistribute records into left node */
    {
        /* Copy record from parent node to proper location */
        HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

        /* Copy records from right node to left node */
        HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec+1),H5B2_NAT_NREC(right_native,shared,0),shared->type->nrec_size*(*right_nrec));

        /* Copy node pointers from right node into left node */
        if(depth>1)
            HDmemcpy(&(left_node_ptrs[*left_nrec+1]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*(*right_nrec+1));

        /* Update # of records in left node */
        *left_nrec += *right_nrec + 1;
    } /* end block */

    /* Update # of records in child nodes */
    internal->node_ptrs[idx].node_nrec = *left_nrec;

    /* Update total # of records in child B-trees */
    internal->node_ptrs[idx].all_nrec += internal->node_ptrs[idx+1].all_nrec + 1;

    /* Slide records in parent node down, to eliminate demoted record */
    if((idx+1) < internal->nrec) {
        HDmemmove(H5B2_INT_NREC(internal,shared,idx),H5B2_INT_NREC(internal,shared,idx+1),shared->type->nrec_size*(internal->nrec-(idx+1)));
        HDmemmove(&(internal->node_ptrs[idx+1]),&(internal->node_ptrs[idx+2]),sizeof(H5B2_node_ptr_t)*(internal->nrec-(idx+1)));
    } /* end if */

    /* Update # of records in parent node */
    internal->nrec--;

    /* Mark parent as dirty */
    *internal_flags_ptr |= H5AC__DIRTIED_FLAG;

    /* Update grandparent info */
    curr_node_ptr->node_nrec--;

    /* Mark grandparent as dirty */
    *parent_cache_info_flags_ptr |= H5AC__DIRTIED_FLAG;

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1)
        H5B2_assert_internal(internal->node_ptrs[idx].all_nrec,shared,left_child);
    else
        H5B2_assert_leaf(shared,left_child);
#endif /* H5B2_DEBUG */

    /* Unlock left node (marked as dirty) */
    if(H5AC_unprotect(f, dxpl_id, child_class, left_addr, left_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

    /* Delete right node & remove from cache (marked as dirty) */
    if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, right_addr, (hsize_t)shared->node_size)<0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree leaf node")
    if(H5AC_unprotect(f, dxpl_id, child_class, right_addr, right_child, H5AC__DIRTIED_FLAG|H5AC__DELETED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_merge2 */


/*-------------------------------------------------------------------------
 * Function:	H5B2_merge3
 *
 * Purpose:	Perform a 3->2 node merge
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  4 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_merge3(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_node_ptr_t *curr_node_ptr, unsigned *parent_cache_info_flags_ptr,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr, unsigned idx)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t left_addr, right_addr;      /* Addresses of left & right child nodes */
    haddr_t middle_addr;                /* Address of middle child node */
    void *left_child, *right_child;     /* Pointers to left & right child nodes */
    void *middle_child;                 /* Pointer to middle child node */
    unsigned *left_nrec, *right_nrec;   /* Pointers to left & right child # of records */
    unsigned *middle_nrec;              /* Pointer to middle child # of records */
    uint8_t *left_native, *right_native;    /* Pointers to left & right children's native records */
    uint8_t *middle_native;             /* Pointer to middle child's native records */
    H5B2_node_ptr_t *left_node_ptrs=NULL, *right_node_ptrs=NULL;/* Pointers to childs' node pointer info */
    H5B2_node_ptr_t *middle_node_ptrs=NULL;/* Pointer to child's node pointer info */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    hsize_t middle_moved_nrec;          /* Number of records moved, for internal split */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_merge3)

    HDassert(f);
    HDassert(curr_node_ptr);
    HDassert(parent_cache_info_flags_ptr);
    HDassert(internal);
    HDassert(internal_flags_ptr);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Check for the kind of B-tree node to split */
    if(depth > 1) {
        H5B2_internal_t *left_internal;         /* Pointer to left internal node */
        H5B2_internal_t *middle_internal;       /* Pointer to middle internal node */
        H5B2_internal_t *right_internal;        /* Pointer to right internal node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_INT;
        left_addr = internal->node_ptrs[idx-1].addr;
        middle_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock B-tree child nodes */
        if (NULL == (left_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, left_addr, internal->node_ptrs[idx-1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if (NULL == (middle_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, middle_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")
        if (NULL == (right_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, right_addr, internal->node_ptrs[idx+1].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* More setup for accessing child node information */
        left_child = left_internal;
        middle_child = middle_internal;
        right_child = right_internal;
        left_nrec = &(left_internal->nrec);
        middle_nrec = &(middle_internal->nrec);
        right_nrec = &(right_internal->nrec);
        left_native = left_internal->int_native;
        middle_native = middle_internal->int_native;
        right_native = right_internal->int_native;
        left_node_ptrs = left_internal->node_ptrs;
        middle_node_ptrs = middle_internal->node_ptrs;
        right_node_ptrs = right_internal->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *left_leaf;         /* Pointer to left leaf node */
        H5B2_leaf_t *middle_leaf;       /* Pointer to middle leaf node */
        H5B2_leaf_t *right_leaf;        /* Pointer to right leaf node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        left_addr = internal->node_ptrs[idx-1].addr;
        middle_addr = internal->node_ptrs[idx].addr;
        right_addr = internal->node_ptrs[idx+1].addr;

        /* Lock B-tree child nodes */
        if (NULL == (left_leaf = H5AC_protect(f, dxpl_id, child_class, left_addr, &(internal->node_ptrs[idx-1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (middle_leaf = H5AC_protect(f, dxpl_id, child_class, middle_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")
        if (NULL == (right_leaf = H5AC_protect(f, dxpl_id, child_class, right_addr, &(internal->node_ptrs[idx+1].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for accessing child node information */
        left_child = left_leaf;
        middle_child = middle_leaf;
        right_child = right_leaf;
        left_nrec = &(left_leaf->nrec);
        middle_nrec = &(middle_leaf->nrec);
        right_nrec = &(right_leaf->nrec);
        left_native = left_leaf->leaf_native;
        middle_native = middle_leaf->leaf_native;
        right_native = right_leaf->leaf_native;
    } /* end else */

    /* Redistribute records into left node */
    {
        unsigned total_nrec = *left_nrec + *middle_nrec + *right_nrec + 2;
        unsigned middle_nrec_move = ((total_nrec - 1) / 2) - *left_nrec;

        /* Set the base number of records moved from middle node */
        middle_moved_nrec = middle_nrec_move;

        /* Copy record from parent node to proper location in left node */
        HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec),H5B2_INT_NREC(internal,shared,idx-1),shared->type->nrec_size);

        /* Copy records from middle node to left node */
        HDmemcpy(H5B2_NAT_NREC(left_native,shared,*left_nrec+1),H5B2_NAT_NREC(middle_native,shared,0),shared->type->nrec_size*(middle_nrec_move-1));

        /* Copy record from middle node to proper location in parent node */
        HDmemcpy(H5B2_INT_NREC(internal,shared,idx-1),H5B2_NAT_NREC(middle_native,shared,(middle_nrec_move-1)),shared->type->nrec_size);

        /* Slide records in middle node down */
        HDmemmove(H5B2_NAT_NREC(middle_native,shared,0),H5B2_NAT_NREC(middle_native,shared,middle_nrec_move),shared->type->nrec_size*(*middle_nrec-middle_nrec_move));

        /* Move node pointers also if this is an internal node */
        if(depth>1) {
            unsigned u;         /* Local index variable */

            /* Copy node pointers from middle node into left node */
            HDmemcpy(&(left_node_ptrs[*left_nrec+1]),&(middle_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*middle_nrec_move);

            /* Count the number of records being moved into the left node */
            for(u=0; u<middle_nrec_move; u++)
                middle_moved_nrec += middle_node_ptrs[u].all_nrec;

            /* Slide the node pointers in right node down */
            HDmemmove(&(middle_node_ptrs[0]),&(middle_node_ptrs[middle_nrec_move]),sizeof(H5B2_node_ptr_t)*((*middle_nrec+1)-middle_nrec_move));
        } /* end if */

        /* Update # of records in left & middle nodes */
        *left_nrec += middle_nrec_move;
        *middle_nrec -= middle_nrec_move;
    } /* end block */

    /* Redistribute records into middle node */
    {
        /* Copy record from parent node to proper location in middle node */
        HDmemcpy(H5B2_NAT_NREC(middle_native,shared,*middle_nrec),H5B2_INT_NREC(internal,shared,idx),shared->type->nrec_size);

        /* Copy records from right node to middle node */
        HDmemcpy(H5B2_NAT_NREC(middle_native,shared,*middle_nrec+1),H5B2_NAT_NREC(right_native,shared,0),shared->type->nrec_size*(*right_nrec));

        /* Move node pointers also if this is an internal node */
        if(depth>1)
            /* Copy node pointers from middle node into left node */
            HDmemcpy(&(middle_node_ptrs[*middle_nrec+1]),&(right_node_ptrs[0]),sizeof(H5B2_node_ptr_t)*(*right_nrec+1));

        /* Update # of records in middle node */
        *middle_nrec += *right_nrec + 1;
    } /* end block */

    /* Update # of records in child nodes */
    internal->node_ptrs[idx-1].node_nrec = *left_nrec;
    internal->node_ptrs[idx].node_nrec = *middle_nrec;

    /* Update total # of records in child B-trees */
    internal->node_ptrs[idx-1].all_nrec += middle_moved_nrec;
    internal->node_ptrs[idx].all_nrec += (internal->node_ptrs[idx+1].all_nrec + 1) - middle_moved_nrec;

    /* Slide records in parent node down, to eliminate demoted record */
    if((idx+1) < internal->nrec) {
        HDmemmove(H5B2_INT_NREC(internal,shared,idx),H5B2_INT_NREC(internal,shared,idx+1),shared->type->nrec_size*(internal->nrec-(idx+1)));
        HDmemmove(&(internal->node_ptrs[idx+1]),&(internal->node_ptrs[idx+2]),sizeof(H5B2_node_ptr_t)*(internal->nrec-(idx+1)));
    } /* end if */

    /* Update # of records in parent node */
    internal->nrec--;

    /* Mark parent as dirty */
    *internal_flags_ptr |= H5AC__DIRTIED_FLAG;

    /* Update grandparent info */
    curr_node_ptr->node_nrec--;

    /* Mark grandparent as dirty */
    *parent_cache_info_flags_ptr |= H5AC__DIRTIED_FLAG;

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1) {
        H5B2_assert_internal2(internal->node_ptrs[idx-1].all_nrec,shared,left_child,middle_child);
        H5B2_assert_internal(internal->node_ptrs[idx].all_nrec,shared,middle_child);
    } /* end if */
    else {
        H5B2_assert_leaf2(shared,left_child,middle_child);
        H5B2_assert_leaf(shared,middle_child);
    } /* end else */
#endif /* H5B2_DEBUG */

    /* Unlock left & middle nodes (marked as dirty) */
    if (H5AC_unprotect(f, dxpl_id, child_class, left_addr, left_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")
    if (H5AC_unprotect(f, dxpl_id, child_class, middle_addr, middle_child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

    /* Delete right node & remove from cache (marked as dirty) */
    if (H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, right_addr, (hsize_t)shared->node_size)<0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree leaf node")
    if (H5AC_unprotect(f, dxpl_id, child_class, right_addr, right_child, H5AC__DIRTIED_FLAG|H5AC__DELETED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_merge3 */


/*-------------------------------------------------------------------------
 * Function:	H5B2_swap_leaf
 *
 * Purpose:	Swap a record in a node with a record in a leaf node
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  4 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_swap_leaf(H5F_t *f, hid_t dxpl_id, unsigned depth,
    H5B2_internal_t *internal, unsigned *internal_flags_ptr,
    unsigned idx, void *swap_loc)
{
    const H5AC_class_t *child_class;    /* Pointer to child node's class info */
    haddr_t child_addr;                 /* Address of child node */
    void *child;                        /* Pointer to child node */
    uint8_t *child_native;              /* Pointer to child's native records */
    H5B2_shared_t *shared;              /* B-tree's shared info */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_swap_leaf)

    HDassert(f);
    HDassert(internal);
    HDassert(internal_flags_ptr);
    HDassert(idx <= internal->nrec);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Check for the kind of B-tree node to swap */
    if(depth > 1) {
        H5B2_internal_t *child_internal;        /* Pointer to internal node */

        /* Setup information for unlocking child node */
        child_class = H5AC_BT2_INT;
        child_addr = internal->node_ptrs[idx].addr;

        /* Lock B-tree child nodes */
        if(NULL == (child_internal = H5B2_protect_internal(f, dxpl_id, internal->shared, child_addr, internal->node_ptrs[idx].node_nrec, (depth - 1), H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* More setup for accessing child node information */
        child = child_internal;
        child_native = child_internal->int_native;
    } /* end if */
    else {
        H5B2_leaf_t *child_leaf;        /* Pointer to leaf node */

        /* Setup information for unlocking child nodes */
        child_class = H5AC_BT2_LEAF;
        child_addr = internal->node_ptrs[idx].addr;

        /* Lock B-tree child node */
        if (NULL == (child_leaf = H5AC_protect(f, dxpl_id, child_class, child_addr, &(internal->node_ptrs[idx].node_nrec), internal->shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* More setup for accessing child node information */
        child = child_leaf;
        child_native = child_leaf->leaf_native;
    } /* end else */

    /* Swap records (use disk page as temporary buffer) */
    HDmemcpy(shared->page, H5B2_NAT_NREC(child_native,shared,0), shared->type->nrec_size);
    HDmemcpy(H5B2_NAT_NREC(child_native,shared,0), swap_loc, shared->type->nrec_size);
    HDmemcpy(swap_loc, shared->page, shared->type->nrec_size);

    /* Mark parent as dirty */
    *internal_flags_ptr |= H5AC__DIRTIED_FLAG;

#ifdef H5B2_DEBUG
    H5B2_assert_internal((hsize_t)0,shared,internal);
    if(depth>1)
        H5B2_assert_internal(internal->node_ptrs[idx].all_nrec,shared,child);
    else
        H5B2_assert_leaf(shared,child);
#endif /* H5B2_DEBUG */

    /* Unlock child node */
    if (H5AC_unprotect(f, dxpl_id, child_class, child_addr, child, H5AC__DIRTIED_FLAG) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree child node")

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_swap_leaf */


/*-------------------------------------------------------------------------
 * Function:	H5B2_insert_leaf
 *
 * Purpose:	Adds a new record to a B-tree leaf node.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  3 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_insert_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *udata)
{
    H5B2_leaf_t *leaf;                  /* Pointer to leaf node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    int         cmp;                    /* Comparison value of records */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_insert_leaf)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));

    /* Lock current B-tree node */
    if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr->addr, &(curr_node_ptr->node_nrec), bt2_shared, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Must have a leaf node with enough space to insert a record now */
    HDassert(curr_node_ptr->node_nrec < shared->node_info[0].max_nrec);

    /* Sanity check number of records */
    HDassert(curr_node_ptr->all_nrec == curr_node_ptr->node_nrec);
    HDassert(leaf->nrec == curr_node_ptr->node_nrec);

    /* Check for inserting into empty leaf */
    if(leaf->nrec==0)
        idx=0;
    else {
        /* Find correct location to insert this record */
        if((cmp = H5B2_locate_record(shared->type,leaf->nrec,shared->nat_off,leaf->leaf_native,udata,&idx)) == 0)
            HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B-tree")
        if(cmp > 0)
            idx++;

        /* Make room for new record */
        if(idx<leaf->nrec)
            HDmemmove(H5B2_LEAF_NREC(leaf,shared,idx+1),H5B2_LEAF_NREC(leaf,shared,idx),shared->type->nrec_size*(leaf->nrec-idx));
    } /* end else */

    /* Make callback to store record in native form */
    if((shared->type->store)(H5B2_LEAF_NREC(leaf,shared,idx), udata) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into leaf node")

    /* Update record count for node pointer to current node */
    curr_node_ptr->all_nrec++;
    curr_node_ptr->node_nrec++;

    /* Update record count for current node */
    leaf->nrec++;

done:
    /* Release the B-tree leaf node (marked as dirty) */
    if(leaf && H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr->addr, leaf, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release leaf B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_insert_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_insert_internal
 *
 * Purpose:	Adds a new record to a B-tree node.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  2 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_insert_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, unsigned *parent_cache_info_flags_ptr,
    H5B2_node_ptr_t *curr_node_ptr, void *udata)
{
    H5B2_internal_t *internal;          /* Pointer to internal node */
    unsigned internal_flags = H5AC__NO_FLAGS_SET;
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_insert_internal)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(depth > 0);
    HDassert(parent_cache_info_flags_ptr);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));

    /* Lock current B-tree node */
    if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr->addr, curr_node_ptr->node_nrec, depth, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

/* Split or redistribute child node pointers, if necessary */
    {
        int         cmp;        /* Comparison value of records */
        unsigned retries;       /* Number of times to attempt redistribution */
        size_t      split_nrec; /* Number of records to split node at */

        /* Locate node pointer for child */
        if((cmp=H5B2_locate_record(shared->type,internal->nrec,shared->nat_off,internal->int_native,udata,&idx)) == 0)
            HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B-tree")
        if(cmp>0)
            idx++;

        /* Set the number of redistribution retries */
        /* This takes care of the case where a B-tree node needs to be
         * redistributed, but redistributing the node causes the index
         * for insertion to move to another node, which also needs to be
         * redistributed.  Now, we loop trying to redistribute and then
         * eventually force a split */
        retries = 2;

        /* Determine the correct number of records to split child node at */
        split_nrec = shared->node_info[depth - 1].split_nrec;

        /* Preemptively split/redistribute a node we will enter */
        while(internal->node_ptrs[idx].node_nrec == split_nrec) {
            /* Attempt to redistribute records among children */
            if(idx == 0) {    /* Left-most child */
                if(retries > 0 && (internal->node_ptrs[idx+1].node_nrec < split_nrec)) {
                    if(H5B2_redistribute2(f, dxpl_id, depth, internal, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_split1(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split child node")
                } /* end else */
            } /* end if */
            else if(idx == internal->nrec) { /* Right-most child */
                if(retries > 0 && (internal->node_ptrs[idx - 1].node_nrec < split_nrec)) {
                    if(H5B2_redistribute2(f, dxpl_id, depth, internal, (idx - 1)) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_split1(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split child node")
                } /* end else */
            } /* end if */
            else { /* Middle child */
                if(retries > 0 && ((internal->node_ptrs[idx + 1].node_nrec < split_nrec) ||
                            (internal->node_ptrs[idx - 1].node_nrec < split_nrec))) {
                    if(H5B2_redistribute3(f, dxpl_id, depth, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_split1(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split child node")
                } /* end else */
            } /* end else */

            /* Locate node pointer for child (after split/redistribute) */
/* Actually, this can be easily updated (for 2-node redistrib.) and shouldn't require re-searching */
            if((cmp = H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx)) == 0)
                HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B-tree")
            if(cmp > 0)
                idx++;

            /* Decrement the number of redistribution retries left */
            retries--;
        } /* end while */
    } /* end block */

    /* Attempt to insert node */
    if(depth > 1) {
        if(H5B2_insert_internal(f, dxpl_id, bt2_shared, (depth - 1), &internal_flags, &internal->node_ptrs[idx], udata) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B-tree internal node")
    } /* end if */
    else {
        if(H5B2_insert_leaf(f, dxpl_id, bt2_shared, &internal->node_ptrs[idx], udata) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B-tree leaf node")
    } /* end else */

    /* Update record count for node pointer to current node */
    curr_node_ptr->all_nrec++;

    /* Mark node as dirty */
    internal_flags |= H5AC__DIRTIED_FLAG;

done:
    /* Release the B-tree internal node */
    if (internal && H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr->addr, internal, internal_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release internal B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_insert_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_create_leaf
 *
 * Purpose:	Creates empty leaf node of a B-tree and update node pointer
 *              to point to it.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  2 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_create_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared, H5B2_node_ptr_t *node_ptr)
{
    H5B2_leaf_t *leaf = NULL;     /* Pointer to new leaf node created */
    H5B2_shared_t *shared;      /* Shared B-tree information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_create_leaf)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(node_ptr);

    /* Allocate memory for leaf information */
    if(NULL == (leaf = H5FL_MALLOC(H5B2_leaf_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree leaf info")

    /* Set metadata cache info */
    HDmemset(&leaf->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common B-tree information */
    leaf->shared = bt2_shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Allocate space for the native keys in memory */
    if((leaf->leaf_native = H5FL_FAC_MALLOC(shared->node_info[0].nat_rec_fac)) == NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree leaf native keys")
#ifdef H5_USING_PURIFY
HDmemset(leaf->leaf_native, 0, shared->type->nrec_size * shared->node_info[0].max_nrec);
#endif /* H5_USING_PURIFY */

    /* Set number of records */
    leaf->nrec = 0;

    /* Allocate space on disk for the leaf */
    if(HADDR_UNDEF == (node_ptr->addr=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)shared->node_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree leaf node")

    /* Cache the new B-tree node */
    if(H5AC_set(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree leaf to cache")

done:
    if(ret_value < 0) {
	if(leaf)
            (void)H5B2_cache_leaf_dest(f,leaf);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_create_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_create_internal
 *
 * Purpose:	Creates empty internal node of a B-tree and update node pointer
 *              to point to it.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  3 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_create_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *node_ptr, unsigned depth)
{
    H5B2_internal_t *internal=NULL;     /* Pointer to new internal node created */
    H5B2_shared_t *shared;              /* Shared B-tree information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_create_internal)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(node_ptr);
    HDassert(depth > 0);

    /* Allocate memory for internal node information */
    if(NULL == (internal = H5FL_MALLOC(H5B2_internal_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal info")

    /* Set metadata cache info */
    HDmemset(&internal->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common B-tree information */
    internal->shared = bt2_shared;
    H5RC_INC(internal->shared);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Allocate space for the native keys in memory */
    if((internal->int_native = H5FL_FAC_MALLOC(shared->node_info[depth].nat_rec_fac)) == NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal native keys")
#ifdef H5_USING_PURIFY
HDmemset(internal->int_native, 0, shared->type->nrec_size * shared->node_info[depth].max_nrec);
#endif /* H5_USING_PURIFY */

    /* Allocate space for the node pointers in memory */
    if((internal->node_ptrs = H5FL_FAC_MALLOC(shared->node_info[depth].node_ptr_fac)) == NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal node pointers")
#ifdef H5_USING_PURIFY
HDmemset(internal->node_ptrs, 0, sizeof(H5B2_node_ptr_t) * (shared->node_info[depth].max_nrec + 1));
#endif /* H5_USING_PURIFY */

    /* Set number of records & depth of the node */
    internal->nrec = 0;
    internal->depth = depth;

    /* Allocate space on disk for the internal node */
    if(HADDR_UNDEF == (node_ptr->addr = H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)shared->node_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree internal node")

    /* Cache the new B-tree node */
    if(H5AC_set(f, dxpl_id, H5AC_BT2_INT, node_ptr->addr, internal, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree internal node to cache")

done:
    if(ret_value < 0) {
	if(internal)
            (void)H5B2_cache_internal_dest(f,internal);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_create_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_protect_internal
 *
 * Purpose:	"Protect" an internal node in the metadata cache
 *
 * Return:	Pointer to internal node on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Aug 25 2006
 *
 *-------------------------------------------------------------------------
 */
H5B2_internal_t *
H5B2_protect_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared, haddr_t addr,
    unsigned nrec, unsigned depth, H5AC_protect_t rw)
{
    H5B2_int_load_ud1_t udata;          /* User data to pass through to cache 'load' callback */
    H5B2_internal_t *ret_value;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_protect_internal)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(H5F_addr_defined(addr));
    HDassert(depth > 0);

    /* Set up user data for callback */
    udata.bt2_shared = bt2_shared;
    udata.nrec = nrec;
    udata.depth = depth;

    /* Protect the internal node */
    if(NULL == (ret_value = H5AC_protect(f, dxpl_id, H5AC_BT2_INT, addr, &udata, NULL, rw)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, NULL, "unable to load B-tree internal node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_protect_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_iterate_node
 *
 * Purpose:	Iterate over all the records from a B-tree node, in "in-order"
 *		order, making a callback for each record.
 *
 *              If the callback returns non-zero, the iteration breaks out
 *              without finishing all the records.
 *
 * Return:	Value from callback, non-negative on success, negative on error
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 11 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_iterate_node(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared, unsigned depth,
    const H5B2_node_ptr_t *curr_node, H5B2_operator_t op, void *op_data)
{
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    const H5AC_class_t *curr_node_class=NULL; /* Pointer to current node's class info */
    void *node=NULL;                    /* Pointers to current node */
    uint8_t *native;                    /* Pointers to node's native records */
    H5B2_node_ptr_t *node_ptrs=NULL;    /* Pointers to node's node pointers */
    unsigned u;                         /* Local index */
    herr_t	ret_value = H5B2_ITER_CONT;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_iterate_node)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(curr_node);
    HDassert(op);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    if(depth>0) {
        H5B2_internal_t *internal;     /* Pointer to internal node */

        /* Lock the current B-tree node */
        if (NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node->addr, curr_node->node_nrec, depth, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Set up information about current node */
        curr_node_class = H5AC_BT2_INT;
        node = internal;
        native = internal->int_native;
        node_ptrs = internal->node_ptrs;
    } /* end if */
    else {
        H5B2_leaf_t *leaf;             /* Pointer to leaf node */

        /* Lock the current B-tree node */
        if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node->addr, &(curr_node->node_nrec), bt2_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* Set up information about current node */
        curr_node_class = H5AC_BT2_LEAF;
        node = leaf;
        native = leaf->leaf_native;
    } /* end else */

    /* Iterate through records, in order */
    for(u = 0; u < curr_node->node_nrec && !ret_value; u++) {
        /* Descend into child node, if current node is an internal node */
        if(depth > 0) {
            if((ret_value = H5B2_iterate_node(f, dxpl_id, bt2_shared, depth-1, &(node_ptrs[u]), op, op_data)) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTLIST, FAIL, "node iteration failed")
        } /* end if */

        /* Make callback for current record */
        if((ret_value = (op)(H5B2_NAT_NREC(native, shared, u), op_data)) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLIST, FAIL, "iterator function failed")
    } /* end for */

    /* Descend into last child node, if current node is an internal node */
    if(!ret_value && depth>0) {
        if((ret_value = H5B2_iterate_node(f,dxpl_id,bt2_shared,depth-1,&(node_ptrs[u]),op,op_data))<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLIST, FAIL, "node iteration failed")
    } /* end if */

done:
    /* Unlock current node */
    if(node)
        if (H5AC_unprotect(f, dxpl_id, curr_node_class, curr_node->addr, node, H5AC__NO_FLAGS_SET) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_iterate_node() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_remove_leaf
 *
 * Purpose:	Removes a record from a B-tree leaf node.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  3 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_remove_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *udata, H5B2_remove_t op,
    void *op_data)
{
    H5B2_leaf_t *leaf;                  /* Pointer to leaf node */
    haddr_t     leaf_addr=HADDR_UNDEF;  /* Leaf address on disk */
    unsigned    leaf_flags=H5AC__NO_FLAGS_SET; /* Flags for unprotecting leaf node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_remove_leaf)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));

    /* Lock current B-tree node */
    leaf_addr = curr_node_ptr->addr;
    if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, leaf_addr, &(curr_node_ptr->node_nrec), bt2_shared, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Sanity check number of records */
    HDassert(curr_node_ptr->all_nrec == curr_node_ptr->node_nrec);
    HDassert(leaf->nrec == curr_node_ptr->node_nrec);

    /* Find correct location to remove this record */
    if(H5B2_locate_record(shared->type,leaf->nrec,shared->nat_off,leaf->leaf_native,udata,&idx) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "record is not in B-tree")

    /* Make 'remove' callback if there is one */
    if(op)
        if((op)(H5B2_LEAF_NREC(leaf,shared,idx), op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record into leaf node")

    /* Update number of records in node */
    leaf->nrec--;

    /* Mark leaf node as dirty also */
    leaf_flags |= H5AC__DIRTIED_FLAG;

    if(leaf->nrec > 0) {
        /* Pack record out of leaf */
        if(idx<leaf->nrec)
            HDmemmove(H5B2_LEAF_NREC(leaf,shared,idx),H5B2_LEAF_NREC(leaf,shared,idx+1),shared->type->nrec_size*(leaf->nrec-idx));
    } /* end if */
    else {
        /* Release space for B-tree node on disk */
        if (H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, leaf_addr, (hsize_t)shared->node_size)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree leaf node")

        /* Let the cache know that the object is deleted */
        leaf_flags |= H5AC__DELETED_FLAG;

        /* Reset address of parent node pointer */
        curr_node_ptr->addr = HADDR_UNDEF;
    } /* end else */

    /* Update record count for parent of leaf node */
    curr_node_ptr->node_nrec--;

done:
    /* Release the B-tree leaf node */
    if (leaf && H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, leaf_addr, leaf, leaf_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release leaf B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_remove_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_remove_internal
 *
 * Purpose:	Removes a record from a B-tree node.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  3 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_remove_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    hbool_t *depth_decreased, void *swap_loc, unsigned depth,
    H5AC_info_t *parent_cache_info, unsigned *parent_cache_info_flags_ptr,
    H5B2_node_ptr_t *curr_node_ptr, void *udata, H5B2_remove_t op,
    void *op_data)
{
    H5AC_info_t *new_cache_info;        /* Pointer to new cache info */
    unsigned *new_cache_info_flags_ptr = NULL;
    H5B2_node_ptr_t *new_node_ptr;      /* Pointer to new node pointer */
    H5B2_internal_t *internal;          /* Pointer to internal node */
    unsigned internal_flags = H5AC__NO_FLAGS_SET;
    haddr_t internal_addr;              /* Address of internal node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned    idx;                    /* Location of record which matches key */
    size_t      merge_nrec;             /* Number of records to merge node at */
    hbool_t     collapsed_root = FALSE; /* Whether the root was collapsed */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_remove_internal)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(depth>0);
    HDassert(parent_cache_info);
    HDassert(parent_cache_info_flags_ptr);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));

    /* Lock current B-tree node */
    internal_addr = curr_node_ptr->addr;
    if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, internal_addr, curr_node_ptr->node_nrec, depth, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Determine the correct number of records to merge at */
    merge_nrec = shared->node_info[depth - 1].merge_nrec;

    /* Check for needing to collapse the root node */
    /* (The root node is the only internal node allowed to have 1 record) */
    if(internal->nrec == 1 &&
            ((internal->node_ptrs[0].node_nrec + internal->node_ptrs[1].node_nrec) <= ((merge_nrec * 2) + 1))) {

        /* Merge children of root node */
        if(H5B2_merge2(f, dxpl_id, depth, curr_node_ptr,
                parent_cache_info_flags_ptr, internal, &internal_flags, 0) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to merge child node")

        /* Release space for root B-tree node on disk */
        if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, internal_addr, (hsize_t)shared->node_size) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree leaf node")

        /* Let the cache know that the object is deleted */
        internal_flags |= H5AC__DELETED_FLAG;

        /* Reset information in header's root node pointer */
        curr_node_ptr->addr = internal->node_ptrs[0].addr;
        curr_node_ptr->node_nrec = internal->node_ptrs[0].node_nrec;

        /* Indicate that the level of the B-tree decreased */
        *depth_decreased = TRUE;

        /* Set pointers for advancing to child node */
        new_cache_info = parent_cache_info;
        new_cache_info_flags_ptr = parent_cache_info_flags_ptr;
        new_node_ptr = curr_node_ptr;

        /* Set flag to indicate root was collapsed */
        collapsed_root = TRUE;
    } /* end if */
    /* Merge or redistribute child node pointers, if necessary */
    else {
        int cmp = 0;            /* Comparison value of records */
        unsigned retries;       /* Number of times to attempt redistribution */

        /* Locate node pointer for child */
        if(swap_loc)
            idx = 0;
        else {
            cmp = H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx);
            if(cmp >= 0)
                idx++;
        } /* end else */

        /* Set the number of redistribution retries */
        /* This takes care of the case where a B-tree node needs to be
         * redistributed, but redistributing the node causes the index
         * for removal to move to another node, which also needs to be
         * redistributed.  Now, we loop trying to redistribute and then
         * eventually force a merge */
        retries = 2;

        /* Preemptively merge/redistribute a node we will enter */
        while(internal->node_ptrs[idx].node_nrec == merge_nrec) {
            /* Attempt to redistribute records among children */
            /* (NOTE: These 2-node redistributions should actually get the
             *  record to promote from the node with more records. - QAK)
             */
            if(idx == 0) {    /* Left-most child */
                if(retries > 0 && (internal->node_ptrs[idx + 1].node_nrec > merge_nrec)) {
                    if(H5B2_redistribute2(f, dxpl_id, depth, internal, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_merge2(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to merge child node")
                } /* end else */
            } /* end if */
            else if(idx == internal->nrec) { /* Right-most child */
                if(retries > 0 && (internal->node_ptrs[idx - 1].node_nrec > merge_nrec)) {
                    if(H5B2_redistribute2(f, dxpl_id, depth, internal, (idx - 1)) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_merge2(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, (idx - 1)) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to merge child node")
                } /* end else */
            } /* end if */
            else { /* Middle child */
                if(retries > 0 && ((internal->node_ptrs[idx + 1].node_nrec > merge_nrec) ||
                            (internal->node_ptrs[idx - 1].node_nrec > merge_nrec))) {
                    if(H5B2_redistribute3(f, dxpl_id, depth, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTREDISTRIBUTE, FAIL, "unable to redistribute child node records")
                } /* end if */
                else {
                    if(H5B2_merge3(f, dxpl_id, depth, curr_node_ptr,
                           parent_cache_info_flags_ptr, internal, &internal_flags, idx) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to merge child node")
                } /* end else */
            } /* end else */

            /* Locate node pointer for child (after merge/redistribute) */
            if(swap_loc)
                idx = 0;
            else {
/* Actually, this can be easily updated (for 2-node redistrib.) and shouldn't require re-searching */
                cmp=H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx);
                if(cmp >= 0)
                    idx++;
            } /* end else */

            /* Decrement the number of redistribution retries left */
            retries--;
        } /* end while */

        /* Handle deleting a record from an internal node */
        if(!swap_loc && cmp == 0)
            swap_loc = H5B2_INT_NREC(internal, shared, idx - 1);

        /* Swap record to delete with record from leaf, if we are the last internal node */
        if(swap_loc && depth == 1)
            if(H5B2_swap_leaf(f, dxpl_id, depth, internal, &internal_flags, idx, swap_loc) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTSWAP, FAIL, "Can't swap records in B-tree")

        /* Set pointers for advancing to child node */
        new_cache_info_flags_ptr = &internal_flags;
        new_cache_info = &internal->cache_info;
        new_node_ptr = &internal->node_ptrs[idx];
    } /* end else */

    /* Attempt to remove record from child node */
    if(depth > 1) {
        if(H5B2_remove_internal(f, dxpl_id, bt2_shared, depth_decreased, swap_loc, depth - 1,
                new_cache_info, new_cache_info_flags_ptr, new_node_ptr, udata, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree internal node")
    } /* end if */
    else {
        if(H5B2_remove_leaf(f, dxpl_id, bt2_shared, new_node_ptr, udata, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree leaf node")
    } /* end else */

    /* Update record count for node pointer to current node */
    if(!collapsed_root)
        new_node_ptr->all_nrec--;

    /* Mark node as dirty */
    internal_flags |= H5AC__DIRTIED_FLAG;

#ifdef H5B2_DEBUG
    H5B2_assert_internal((!collapsed_root ? (curr_node_ptr->all_nrec-1) : new_node_ptr->all_nrec),shared,internal);
#endif /* H5B2_DEBUG */

done:
    /* Release the B-tree internal node */
    if(internal && H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, internal_addr, internal, internal_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release internal B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_remove_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_neighbor_leaf
 *
 * Purpose:	Locate a record relative to the specified information in a
 *              B-tree leaf node and return that information by filling in
 *              fields of the
 *              caller-supplied UDATA pointer depending on the type of leaf node
 *		requested.  The UDATA can point to additional data passed
 *		to the key comparison function.
 *
 *              The 'OP' routine is called with the record found and the
 *              OP_DATA pointer, to allow caller to return information about
 *              the record.
 *
 *              The RANGE indicates whether to search for records less than or
 *              equal to, or greater than or equal to the information passed
 *              in with UDATA.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  9 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_neighbor_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    H5B2_node_ptr_t *curr_node_ptr, void *neighbor_loc,
    H5B2_compare_t comp, void *udata, H5B2_found_t op, void *op_data)
{
    H5B2_leaf_t *leaf;                  /* Pointer to leaf node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned    idx;                    /* Location of record which matches key */
    int         cmp=0;                  /* Comparison value of records */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_neighbor_leaf)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));
    HDassert(op);

    /* Lock current B-tree node */
    if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr->addr, &(curr_node_ptr->node_nrec), bt2_shared, H5AC_READ)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);


    /* Locate node pointer for child */
    cmp = H5B2_locate_record(shared->type, leaf->nrec, shared->nat_off, leaf->leaf_native, udata, &idx);
    if(cmp > 0)
        idx++;
    else
        if(cmp == 0 && comp == H5B2_COMPARE_GREATER)
            idx++;

    /* Set the neighbor location, if appropriate */
    if(comp == H5B2_COMPARE_LESS) {
        if(idx > 0)
            neighbor_loc = H5B2_LEAF_NREC(leaf,shared,idx-1);
    } /* end if */
    else {
        HDassert(comp == H5B2_COMPARE_GREATER);

        if(idx < leaf->nrec)
            neighbor_loc = H5B2_LEAF_NREC(leaf,shared,idx);
    } /* end else */

    /* Make callback if neighbor record has been found */
    if(neighbor_loc) {
        /* Make callback for current record */
        if ((op)(neighbor_loc, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "'found' callback failed for B-tree neighbor operation")
    } /* end if */
    else
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "unable to find neighbor record in B-tree")

done:
    /* Release the B-tree internal node */
    if (leaf && H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree leaf node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_neighbor_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_neighbor_internal
 *
 * Purpose:	Locate a record relative to the specified information in a
 *              B-tree internal node and return that information by filling in
 *              fields of the
 *              caller-supplied UDATA pointer depending on the type of leaf node
 *		requested.  The UDATA can point to additional data passed
 *		to the key comparison function.
 *
 *              The 'OP' routine is called with the record found and the
 *              OP_DATA pointer, to allow caller to return information about
 *              the record.
 *
 *              The RANGE indicates whether to search for records less than or
 *              equal to, or greater than or equal to the information passed
 *              in with UDATA.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  9 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_neighbor_internal(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared,
    unsigned depth, H5B2_node_ptr_t *curr_node_ptr, void *neighbor_loc,
    H5B2_compare_t comp, void *udata, H5B2_found_t op, void *op_data)
{
    H5B2_internal_t *internal;          /* Pointer to internal node */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    unsigned    idx;                    /* Location of record which matches key */
    int         cmp=0;                  /* Comparison value of records */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_neighbor_internal)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(depth>0);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));
    HDassert(op);

    /* Lock current B-tree node */
    if (NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr->addr, curr_node_ptr->node_nrec, depth, H5AC_READ)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Locate node pointer for child */
    cmp = H5B2_locate_record(shared->type,internal->nrec,shared->nat_off,internal->int_native,udata,&idx);
    if(cmp > 0)
        idx++;

    /* Set the neighbor location, if appropriate */
    if(comp == H5B2_COMPARE_LESS) {
        if(idx > 0)
            neighbor_loc = H5B2_INT_NREC(internal,shared,idx-1);
    } /* end if */
    else {
        HDassert(comp == H5B2_COMPARE_GREATER);

        if(idx < internal->nrec)
            neighbor_loc = H5B2_INT_NREC(internal,shared,idx);
    } /* end else */

    /* Attempt to find neighboring record */
    if(depth>1) {
        if(H5B2_neighbor_internal(f, dxpl_id, bt2_shared, depth-1, &internal->node_ptrs[idx], neighbor_loc, comp, udata, op, op_data)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "unable to find neighbor record in B-tree internal node")
    } /* end if */
    else {
        if(H5B2_neighbor_leaf(f, dxpl_id, bt2_shared, &internal->node_ptrs[idx], neighbor_loc, comp, udata, op, op_data)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "unable to find neighbor record in B-tree leaf node")
    } /* end else */

done:
    /* Release the B-tree internal node */
    if (internal && H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr->addr, internal, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release internal B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_neighbor_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_delete_node
 *
 * Purpose:	Iterate over all the nodes in a B-tree node deleting them
 *		after they no longer have any children
 *
 * Return:	Value from callback, non-negative on success, negative on error
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  9 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_delete_node(H5F_t *f, hid_t dxpl_id, H5RC_t *bt2_shared, unsigned depth,
    const H5B2_node_ptr_t *curr_node, H5B2_remove_t op, void *op_data)
{
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    const H5AC_class_t *curr_node_class=NULL; /* Pointer to current node's class info */
    void *node=NULL;                    /* Pointers to current node */
    uint8_t *native;                    /* Pointers to node's native records */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_delete_node)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2_shared);
    HDassert(curr_node);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    if(depth>0) {
        H5B2_internal_t *internal;     /* Pointer to internal node */
        unsigned u;                    /* Local index */

        /* Lock the current B-tree node */
        if (NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node->addr, curr_node->node_nrec, depth, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Set up information about current node */
        curr_node_class = H5AC_BT2_INT;
        node = internal;
        native = internal->int_native;

        /* Descend into children */
        for(u=0; u<internal->nrec+1; u++)
            if(H5B2_delete_node(f, dxpl_id, bt2_shared, depth-1, &(internal->node_ptrs[u]), op, op_data)<0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTLIST, FAIL, "node descent failed")
    } /* end if */
    else {
        H5B2_leaf_t *leaf;             /* Pointer to leaf node */

        /* Lock the current B-tree node */
        if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node->addr, &(curr_node->node_nrec), bt2_shared, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree leaf node")

        /* Set up information about current node */
        curr_node_class = H5AC_BT2_LEAF;
        node = leaf;
        native = leaf->leaf_native;
    } /* end else */

    /* If there's a callback defined, iterate over the records in this node */
    if(op) {
        unsigned u;             /* Local index */

        /* Iterate through records in this node */
        for(u = 0; u < curr_node->node_nrec; u++) {
            /* Make callback for each record */
            if((op)(H5B2_NAT_NREC(native, shared, u), op_data) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTLIST, FAIL, "iterator function failed")
        } /* end for */
    } /* end if */

    /* Release space for current B-tree node on disk */
    if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, curr_node->addr, (hsize_t)shared->node_size)<0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree node")

done:
    /* Unlock & delete current node */
    if(node)
        if(H5AC_unprotect(f, dxpl_id, curr_node_class, curr_node->addr, node, H5AC__DELETED_FLAG) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_delete_node() */

#ifdef H5B2_DEBUG

/*-------------------------------------------------------------------------
 * Function:	H5B2_assert_leaf
 *
 * Purpose:	Verify than a leaf node is mostly sane
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 19 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_assert_leaf(H5B2_shared_t *shared, H5B2_leaf_t *leaf)
{
    unsigned u,v;       /* Local index variables */

    /* General sanity checking on node */
    HDassert(leaf->nrec<=shared->split_leaf_nrec);

    /* Sanity checking on records */
    for(u=0; u<leaf->nrec; u++)
        for(v=0; v<u; v++)
            HDassert((shared->type->compare)(H5B2_LEAF_NREC(leaf,shared,u), H5B2_LEAF_NREC(leaf,shared,v))>0);

    return(0);
} /* end H5B2_assert_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_assert_leaf2
 *
 * Purpose:	Verify than a leaf node is mostly sane
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 19 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_assert_leaf2(H5B2_shared_t *shared, H5B2_leaf_t *leaf, H5B2_leaf_t *leaf2)
{
    unsigned u,v;       /* Local index variables */

    /* General sanity checking on node */
    HDassert(leaf->nrec<=shared->split_leaf_nrec);

    /* Sanity checking on records */
    for(u=0; u<leaf->nrec; u++) {
        HDassert((shared->type->compare)(H5B2_LEAF_NREC(leaf2,shared,0), H5B2_LEAF_NREC(leaf,shared,u))>0);
        for(v=0; v<u; v++)
            HDassert((shared->type->compare)(H5B2_LEAF_NREC(leaf,shared,u), H5B2_LEAF_NREC(leaf,shared,v))>0);
    } /* end for */

    return(0);
} /* end H5B2_assert_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_assert_internal
 *
 * Purpose:	Verify than an internal node is mostly sane
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 19 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_assert_internal(hsize_t parent_all_nrec, H5B2_shared_t *shared, H5B2_internal_t *internal)
{
    hsize_t tot_all_nrec;       /* Total number of records at or below this node */
    unsigned u,v;               /* Local index variables */

    /* General sanity checking on node */
    HDassert(internal->nrec<=shared->split_int_nrec);

    /* Sanity checking on records */
    for(u=0; u<internal->nrec; u++)
        for(v=0; v<u; v++)
            HDassert((shared->type->compare)(H5B2_INT_NREC(internal,shared,u), H5B2_INT_NREC(internal,shared,v))>0);

    /* Sanity checking on node pointers */
    tot_all_nrec=internal->nrec;
    for(u=0; u<internal->nrec+1; u++) {
        tot_all_nrec += internal->node_ptrs[u].all_nrec;

        HDassert(H5F_addr_defined(internal->node_ptrs[u].addr));
        HDassert(internal->node_ptrs[u].addr>0);
        for(v=0; v<u; v++)
            HDassert(internal->node_ptrs[u].addr!=internal->node_ptrs[v].addr);
    } /* end for */

    /* Sanity check all_nrec total in parent */
    if(parent_all_nrec>0)
        HDassert(tot_all_nrec == parent_all_nrec);

    return(0);
} /* end H5B2_assert_internal() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_assert_internal2
 *
 * Purpose:	Verify than internal nodes are mostly sane
 *
 * Return:	Non-negative on success, negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 19 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_assert_internal2(hsize_t parent_all_nrec, H5B2_shared_t *shared, H5B2_internal_t *internal, H5B2_internal_t *internal2)
{
    hsize_t tot_all_nrec;       /* Total number of records at or below this node */
    unsigned u,v;       /* Local index variables */

    /* General sanity checking on node */
    HDassert(internal->nrec<=shared->split_int_nrec);

    /* Sanity checking on records */
    for(u=0; u<internal->nrec; u++)
        for(v=0; v<u; v++)
            HDassert((shared->type->compare)(H5B2_INT_NREC(internal,shared,u), H5B2_INT_NREC(internal,shared,v))>0);

    /* Sanity checking on node pointers */
    tot_all_nrec=internal->nrec;
    for(u=0; u<internal->nrec+1; u++) {
        tot_all_nrec += internal->node_ptrs[u].all_nrec;

        HDassert(H5F_addr_defined(internal->node_ptrs[u].addr));
        HDassert(internal->node_ptrs[u].addr>0);
        for(v=0; v<u; v++)
            HDassert(internal->node_ptrs[u].addr!=internal->node_ptrs[v].addr);
        for(v=0; v<internal2->nrec+1; v++)
            HDassert(internal->node_ptrs[u].addr!=internal2->node_ptrs[v].addr);
    } /* end for */

    /* Sanity check all_nrec total in parent */
    if(parent_all_nrec>0)
        HDassert(tot_all_nrec == parent_all_nrec);

    return(0);
} /* end H5B2_assert_internal2() */
#endif /* H5B2_DEBUG */

