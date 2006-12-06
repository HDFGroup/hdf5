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
 * Created:		H5B2.c
 *			Jan 31 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implements a B-tree, with several modifications from
 *                      the "standard" methods.
 *
 *                      Please see the documentation in:
 *                      doc/html/TechNotes/Btrees.html for a full description
 *                      of how they work, etc.
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

/* Declare a free list to manage the H5B2_t struct */
H5FL_DEFINE(H5B2_t);

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5B2_create
 *
 * Purpose:	Creates a new empty B-tree in the file.
 *
 * Return:	Non-negative on success (with address of new B-tree
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 31 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_create(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    size_t node_size, size_t rrec_size,
    unsigned split_percent, unsigned merge_percent, haddr_t *addr_p)
{
    H5B2_t *bt2 = NULL;                 /* The new B-tree header information */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(type);
    HDassert(node_size > 0);
    HDassert(rrec_size > 0);
    HDassert(merge_percent > 0 && merge_percent <= 100);
    HDassert(split_percent > 0 && split_percent <= 100);
    HDassert(merge_percent < (split_percent / 2));
    HDassert(addr_p);

    /*
     * Allocate file and memory data structures.
     */
    if(NULL == (bt2 = H5FL_MALLOC(H5B2_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree header")

    /* Assign internal information */
    HDmemset(&bt2->cache_info, 0, sizeof(H5AC_info_t));
    bt2->root.addr = HADDR_UNDEF;
    bt2->root.node_nrec = 0;
    bt2->root.all_nrec = 0;

    /* Initialize shared B-tree info */
    if(H5B2_shared_init(f, bt2, type, 0, node_size, rrec_size, split_percent, merge_percent) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create shared B-tree info")

    /* Allocate space for the header on disk */
    if(HADDR_UNDEF == (*addr_p = H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)H5B2_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree header")

    /* Cache the new B-tree node */
    if(H5AC_set(f, dxpl_id, H5AC_BT2_HDR, *addr_p, bt2, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree header to cache")

done:
    if(ret_value < 0) {
	if(bt2)
            (void)H5B2_cache_hdr_dest(f, bt2);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_create() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_insert
 *
 * Purpose:	Adds a new record to the B-tree.
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
H5B2_insert(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
           void *udata)
{
    H5B2_t	*bt2 = NULL;            /* Pointer to the B-tree header */
    unsigned	bt2_flags = H5AC__NO_FLAGS_SET; /* Metadata cache flags for B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_insert, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the b-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Check if the root node is allocated yet */
    if(!H5F_addr_defined(bt2->root.addr)) {
        /* Create root node as leaf node in B-tree */
        if(H5B2_create_leaf(f, dxpl_id, bt2->shared, &(bt2->root)) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create root node")

        /* Mark B-tree header as dirty, since we updated the address of the root node */
        bt2_flags |= H5AC__DIRTIED_FLAG;
    } /* end if */
    /* Check if we need to split the root node (equiv. to a 1->2 node split) */
    else if(bt2->root.node_nrec == shared->node_info[shared->depth].split_nrec) {
        /* Split root node */
        if(H5B2_split_root(f, dxpl_id, bt2, &bt2_flags) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split root node")
    } /* end if */

    /* Attempt to insert record into B-tree */
    if(shared->depth > 0) {
        if(H5B2_insert_internal(f, dxpl_id, bt2->shared, shared->depth, &bt2_flags, &bt2->root, udata) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B-tree internal node")
    } /* end if */
    else {
        if(H5B2_insert_leaf(f, dxpl_id, bt2->shared, &bt2->root, udata) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B-tree leaf node")
    } /* end else */

    /* Mark parent node as dirty */
    bt2_flags |= H5AC__DIRTIED_FLAG;

done:
    /* Release the B-tree header info */
    if(bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, bt2_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_insert() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_iterate
 *
 * Purpose:	Iterate over all the records in the B-tree, in "in-order"
 *		order, making a callback for each record.
 *
 *              If the callback returns non-zero, the iteration breaks out
 *              without finishing all the records.
 *
 * Return:	Value from callback: non-negative on success, negative on error
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 11 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_iterate(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    H5B2_operator_t op, void *op_data)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    H5RC_t      *bt2_shared=NULL;       /* Pointer to ref-counter for shared B-tree info */
    hbool_t     incr_rc=FALSE;          /* Flag to indicate that we've incremented the B-tree's shared info reference count */
    H5B2_node_ptr_t root_ptr;           /* Node pointer info for root node */
    unsigned    depth;                  /* Current depth of the tree */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_iterate, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(op);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Safely grab pointer to reference counted shared B-tree info, so we can release the B-tree header if necessary */
    bt2_shared = bt2->shared;
    H5RC_INC(bt2_shared);
    incr_rc = TRUE;

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Make copy of the root node pointer */
    root_ptr = bt2->root;

    /* Current depth of the tree */
    depth = shared->depth;

    /* Release header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")
    bt2 = NULL;

    /* Iterate through records */
    if(root_ptr.node_nrec > 0) {
        /* Iterate through nodes */
        if((ret_value = H5B2_iterate_node(f, dxpl_id, bt2_shared, depth, &root_ptr, op, op_data)) < 0)
            HERROR(H5E_BTREE, H5E_CANTLIST, "node iteration failed");
    } /* end if */

done:
    /* Check if we need to decrement the reference count for the B-tree's shared info */
    if(incr_rc)
        H5RC_DEC(bt2_shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_iterate() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_find
 *
 * Purpose:	Locate the specified information in a B-tree and return
 *		that information by calling the provided 'OP' routine with an
 *		OP_DATA pointer.  The UDATA parameter points to data passed
 *		to the key comparison function.
 *
 *              The 'OP' routine is called with the record found and the
 *              OP_DATA pointer, to allow caller to return information about
 *              the record.
 *
 *              If 'OP' is NULL, then this routine just returns "SUCCEED" when
 *              a record is present in the B-tree.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 23 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_find(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    void *udata, H5B2_found_t op, void *op_data)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5RC_t      *bt2_shared=NULL;       /* Pointer to ref-counter for shared B-tree info */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    hbool_t     incr_rc=FALSE;          /* Flag to indicate that we've incremented the B-tree's shared info reference count */
    H5B2_node_ptr_t curr_node_ptr;      /* Node pointer info for current node */
    unsigned    depth;                  /* Current depth of the tree */
    int         cmp;                    /* Comparison value of records */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_find, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the B-tree header */
    if (NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Safely grab pointer to reference counted shared B-tree info, so we can release the B-tree header if necessary */
    bt2_shared=bt2->shared;
    H5RC_INC(bt2_shared);
    incr_rc=TRUE;

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Make copy of the root node pointer to start search with */
    curr_node_ptr = bt2->root;

    /* Current depth of the tree */
    depth = shared->depth;

    /* Release header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")
    bt2 = NULL;

    /* Check for empty tree */
    if(curr_node_ptr.node_nrec==0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree has no records")

    /* Walk down B-tree to find record or leaf node where record is located */
    cmp = -1;
    while(depth > 0 && cmp != 0) {
        H5B2_internal_t *internal;          /* Pointer to internal node in B-tree */
        H5B2_node_ptr_t next_node_ptr;      /* Node pointer info for next node */

        /* Lock B-tree current node */
        if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr.addr, curr_node_ptr.node_nrec, depth, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate node pointer for child */
        cmp = H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx);
        if(cmp > 0)
            idx++;

        if(cmp != 0) {
            /* Get node pointer for next node to search */
            next_node_ptr=internal->node_ptrs[idx];

            /* Unlock current node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Set pointer to next node to load */
            curr_node_ptr = next_node_ptr;
        } /* end if */
        else {
            /* Make callback for current record */
            if(op && (op)(H5B2_INT_NREC(internal, shared, idx), op_data) < 0) {
                /* Unlock current node */
                if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "'found' callback failed for B-tree find operation")
            } /* end if */

            /* Unlock current node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            HGOTO_DONE(SUCCEED);
        } /* end else */

        /* Decrement depth we're at in B-tree */
        depth--;
    } /* end while */

    {
        H5B2_leaf_t *leaf;          /* Pointer to leaf node in B-tree */

        /* Lock B-tree leaf node */
        if(NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, &(curr_node_ptr.node_nrec), bt2_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate record */
        cmp = H5B2_locate_record(shared->type, leaf->nrec, shared->nat_off, leaf->leaf_native, udata, &idx);

        if(cmp != 0) {
            /* Unlock leaf node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Note: don't push error on stack, leave that to next higher level,
             *      since many times the B-tree is searched in order to determine
             *      if an object exists in the B-tree or not. -QAK
             */
#ifdef OLD_WAY
                HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "key not found in leaf node")
#else /* OLD_WAY */
                HGOTO_DONE(FAIL)
#endif /* OLD_WAY */
        } /* end if */
        else {
            /* Make callback for current record */
            if(op && (op)(H5B2_LEAF_NREC(leaf, shared, idx), op_data) < 0) {
                /* Unlock current node */
                if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "'found' callback failed for B-tree find operation")
            } /* end if */
        } /* end else */

        /* Unlock current node */
        if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")
    } /* end block */

done:
    /* Check if we need to decrement the reference count for the B-tree's shared info */
    if(incr_rc)
        H5RC_DEC(bt2_shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_find() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_index
 *
 * Purpose:	Locate the IDX'th record in a B-tree according to the
 *              ordering used by the B-tree.  The IDX values are 0-based.
 *
 *              The 'OP' routine is called with the record found and the
 *              OP_DATA pointer, to allow caller to return information about
 *              the record.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 23 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_index(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    H5_iter_order_t order, hsize_t idx, H5B2_found_t op, void *op_data)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5RC_t      *bt2_shared=NULL;       /* Pointer to ref-counter for shared B-tree info */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    hbool_t     incr_rc=FALSE;          /* Flag to indicate that we've incremented the B-tree's shared info reference count */
    H5B2_node_ptr_t curr_node_ptr;      /* Node pointer info for current node */
    unsigned    depth;                  /* Current depth of the tree */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_index, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(op);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Safely grab pointer to reference counted shared B-tree info, so we can release the B-tree header if necessary */
    bt2_shared = bt2->shared;
    H5RC_INC(bt2_shared);
    incr_rc = TRUE;

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Make copy of the root node pointer to start search with */
    curr_node_ptr = bt2->root;

    /* Current depth of the tree */
    depth = shared->depth;

    /* Release header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")
    bt2 = NULL;

    /* Check for empty tree */
    if(curr_node_ptr.node_nrec == 0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree has no records")

    /* Check for index greater than the number of records in the tree */
    if(idx >= curr_node_ptr.all_nrec)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree doesn't have that many records")

    /* Check for reverse indexing and map requested index to appropriate forward index */
    if(order == H5_ITER_DEC)
        idx = curr_node_ptr.all_nrec - (idx + 1);

    /* Walk down B-tree to find record or leaf node where record is located */
    while(depth > 0) {
        H5B2_internal_t *internal;          /* Pointer to internal node in B-tree */
        H5B2_node_ptr_t next_node_ptr;      /* Node pointer info for next node */
        unsigned u;                         /* Local index variable */

        /* Lock B-tree current node */
        if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr.addr, curr_node_ptr.node_nrec, depth, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Search for record with correct index */
        for(u = 0; u < internal->nrec; u++) {
            /* Check if record is in child node */
            if(internal->node_ptrs[u].all_nrec > idx) {
                /* Get node pointer for next node to search */
                next_node_ptr = internal->node_ptrs[u];

                /* Unlock current node */
                if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                /* Set pointer to next node to load */
                curr_node_ptr = next_node_ptr;

                /* Break out of for loop */
                break;
            } /* end if */

            /* Check if record is in this node */
            if(internal->node_ptrs[u].all_nrec == idx) {
                /* Make callback for current record */
                if((op)(H5B2_INT_NREC(internal, shared, u), op_data) < 0) {
                    /* Unlock current node */
                    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                    HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "'found' callback failed for B-tree find operation")
                } /* end if */

                /* Unlock current node */
                if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                HGOTO_DONE(SUCCEED);
            } /* end if */

            /* Decrement index we are looking for to account for the node we
             * just advanced past.
             */
            idx -= (internal->node_ptrs[u].all_nrec + 1);
        } /* end for */

        /* Check last node pointer */
        if(u == internal->nrec) {
            /* Check if record is in child node */
            if(internal->node_ptrs[u].all_nrec > idx) {
                /* Get node pointer for next node to search */
                next_node_ptr = internal->node_ptrs[u];

                /* Unlock current node */
                if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                /* Set pointer to next node to load */
                curr_node_ptr = next_node_ptr;
            } /* end if */
            else
                /* Index that is greater than the number of records in the tree? */
                HDassert("Index off end of tree??" && 0);
        } /* end if */

        /* Decrement depth we're at in B-tree */
        depth--;
    } /* end while */

    {
        H5B2_leaf_t *leaf;          /* Pointer to leaf node in B-tree */

        /* Lock B-tree leaf node */
        if(NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, &(curr_node_ptr.node_nrec), bt2_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Sanity check index */
        HDassert(idx < leaf->nrec);

        /* Make callback for correct record */
        if((op)(H5B2_LEAF_NREC(leaf, shared, idx), op_data) < 0) {
            /* Unlock current node */
            if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "'found' callback failed for B-tree find operation")
        } /* end if */

        /* Unlock current node */
        if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")
    } /* end block */

done:
    /* Check if we need to decrement the reference count for the B-tree's shared info */
    if(incr_rc)
        H5RC_DEC(bt2_shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_index() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_remove
 *
 * Purpose:	Removes a record from a B-tree.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 25 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_remove(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    void *udata, H5B2_remove_t op, void *op_data)
{
    H5B2_t	*bt2 = NULL;            /* Pointer to the B-tree header */
    unsigned    bt2_flags = H5AC__NO_FLAGS_SET;
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_remove, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the b-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Check for empty B-tree */
    if(bt2->root.all_nrec == 0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "record is not in B-tree")

    /* Attempt to remove record from B-tree */
    if(shared->depth > 0) {
        hbool_t depth_decreased = FALSE;  /* Flag to indicate whether the depth of the B-tree decreased */

        if(H5B2_remove_internal(f, dxpl_id, bt2->shared, &depth_decreased, NULL, shared->depth,
                &(bt2->cache_info), &bt2_flags, &bt2->root, udata, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree internal node")

        /* Check for decreasing the depth of the B-tree */
        if(depth_decreased) {
            /* Destroy free list factories for previous depth */
            if(shared->node_info[shared->depth].nat_rec_fac)
                if(H5FL_fac_term(shared->node_info[shared->depth].nat_rec_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's native record block factory")
            if(shared->node_info[shared->depth].node_ptr_fac)
                if(H5FL_fac_term(shared->node_info[shared->depth].node_ptr_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's node pointer block factory")

            shared->depth -= depth_decreased;
        } /* end for */
    } /* end if */
    else {
        if(H5B2_remove_leaf(f, dxpl_id, bt2->shared, &bt2->root, udata, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree leaf node")
    } /* end else */

    /* Decrement # of records in B-tree */
    bt2->root.all_nrec--;

    /* Mark parent node as dirty */
    bt2_flags |= H5AC__DIRTIED_FLAG;

done:
    /* Release the B-tree header info */
    if (bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, bt2_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_remove() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_remove_by_idx
 *
 * Purpose:	Removes the n'th record from a B-tree.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Nov 14 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_remove_by_idx(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    haddr_t addr, H5_iter_order_t order, hsize_t idx, H5B2_remove_t op,
    void *op_data)
{
    H5B2_t	*bt2 = NULL;              /* Pointer to the B-tree header */
    unsigned    bt2_flags = H5AC__NO_FLAGS_SET;
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_remove_by_idx, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the b-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Check for empty B-tree */
    if(bt2->root.all_nrec == 0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "record is not in B-tree")

    /* Check for index greater than the number of records in the tree */
    if(idx >= bt2->root.all_nrec)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree doesn't have that many records")

    /* Check for reverse indexing and map requested index to appropriate forward index */
    if(order == H5_ITER_DEC)
        idx = bt2->root.all_nrec - (idx + 1);

    /* Attempt to remove record from B-tree */
    if(shared->depth > 0) {
        hbool_t depth_decreased = FALSE;  /* Flag to indicate whether the depth of the B-tree decreased */

        if(H5B2_remove_internal_by_idx(f, dxpl_id, bt2->shared, &depth_decreased, NULL, shared->depth,
                &(bt2->cache_info), &bt2_flags, &bt2->root, idx, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree internal node")

        /* Check for decreasing the depth of the B-tree */
        if(depth_decreased) {
            /* Destroy free list factories for previous depth */
            if(shared->node_info[shared->depth].nat_rec_fac)
                if(H5FL_fac_term(shared->node_info[shared->depth].nat_rec_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's native record block factory")
            if(shared->node_info[shared->depth].node_ptr_fac)
                if(H5FL_fac_term(shared->node_info[shared->depth].node_ptr_fac) < 0)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy node's node pointer block factory")

            shared->depth -= depth_decreased;
        } /* end for */
    } /* end if */
    else {
        if(H5B2_remove_leaf_by_idx(f, dxpl_id, bt2->shared, &bt2->root, (unsigned)idx, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to remove record from B-tree leaf node")
    } /* end else */

    /* Decrement # of records in B-tree */
    bt2->root.all_nrec--;

    /* Mark parent node as dirty */
    bt2_flags |= H5AC__DIRTIED_FLAG;

done:
    /* Release the B-tree header info */
    if (bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, bt2_flags) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_remove_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_get_nrec
 *
 * Purpose:	Retrieves the number of records in a B-tree
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 25 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_get_nrec(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
           hsize_t *nrec)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_get_nrec, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(nrec);

    /* Look up the B-tree header */
    if (NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get B-tree number of records */
    *nrec = bt2->root.all_nrec;

done:
    /* Release B-tree header node */
    if (bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_get_nrec() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_neighbor
 *
 * Purpose:	Locate a record relative to the specified information in a
 *              B-tree and return that information by filling in fields of the
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
 *		Mar  8 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_neighbor(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    H5B2_compare_t range, void *udata, H5B2_found_t op, void *op_data)
{
    H5B2_t	*bt2 = NULL;              /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_neighbor, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(op);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Check for empty tree */
    if(!H5F_addr_defined(bt2->root.addr))
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree has no records")

    /* Attempt to find neighbor record in B-tree */
    if(shared->depth > 0) {
        if(H5B2_neighbor_internal(f, dxpl_id, bt2->shared, shared->depth, &bt2->root, NULL, range, udata, op, op_data)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "unable to find neighbor record in B-tree internal node")
    } /* end if */
    else {
        if(H5B2_neighbor_leaf(f, dxpl_id, bt2->shared, &bt2->root, NULL, range, udata, op, op_data)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "unable to find neighbor record in B-tree leaf node")
    } /* end else */

done:
    /* Release the B-tree header info */
    if (bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_neighbor() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_delete
 *
 * Purpose:	Delete an entire B-tree from a file.
 *
 *              The 'OP' routine is called for each record and the
 *              OP_DATA pointer, to allow caller to perform an operation as
 *              each record is removed from the B-tree.
 *
 *              If 'OP' is NULL, the records are just removed in the process
 *              of deleting the B-tree.
 *
 * Note:	The records are _not_ guaranteed to be visited in order.
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
H5B2_delete(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    H5B2_remove_t op, void *op_data)
{
    H5B2_t	*bt2 = NULL;              /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_delete, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Delete all nodes in B-tree */
    if(H5F_addr_defined(bt2->root.addr))
        if(H5B2_delete_node(f, dxpl_id, bt2->shared, shared->depth, &bt2->root, op, op_data) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDELETE, FAIL, "unable to delete B-tree nodes")

    /* Release space for B-tree node on disk */
    if(H5MF_xfree(f, H5FD_MEM_BTREE, dxpl_id, addr, (hsize_t)H5B2_HEADER_SIZE(f))<0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to free B-tree header info")

done:
    /* Release the B-tree header info */
    if(bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__DELETED_FLAG) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to delete B-tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_delete() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_modify
 *
 * Purpose:	Locate the specified information in a B-tree and modify it.
 *		The UDATA points to additional data passed
 *		to the key comparison function for locating the record to
 *              modify.
 *
 *              The 'OP' routine is called with the record found and the
 *              OP_DATA pointer, to allow caller to modify information about
 *              the record.
 *
 * Return:	Non-negative on success, negative on failure.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar 10 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_modify(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type, haddr_t addr,
    void *udata, H5B2_modify_t op, void *op_data)
{
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5RC_t      *bt2_shared=NULL;       /* Pointer to ref-counter for shared B-tree info */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    hbool_t     incr_rc=FALSE;          /* Flag to indicate that we've incremented the B-tree's shared info reference count */
    H5B2_node_ptr_t curr_node_ptr;      /* Node pointer info for current node */
    unsigned    depth;                  /* Current depth of the tree */
    int         cmp;                    /* Comparison value of records */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_modify, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));
    HDassert(op);

    /* Look up the B-tree header */
    if(NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_READ)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree header")

    /* Safely grab pointer to reference counted shared B-tree info, so we can release the B-tree header if necessary */
    bt2_shared = bt2->shared;
    H5RC_INC(bt2_shared);
    incr_rc = TRUE;

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2_shared);
    HDassert(shared);

    /* Make copy of the root node pointer to start search with */
    curr_node_ptr = bt2->root;

    /* Current depth of the tree */
    depth = shared->depth;

    /* Release header */
    if(H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree header info")
    bt2 = NULL;

    /* Check for empty tree */
    if(curr_node_ptr.node_nrec==0)
        HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "B-tree has no records")

    /* Walk down B-tree to find record or leaf node where record is located */
    cmp = -1;
    while(depth > 0 && cmp != 0) {
        unsigned internal_flags = H5AC__NO_FLAGS_SET;
        H5B2_internal_t *internal;          /* Pointer to internal node in B-tree */
        H5B2_node_ptr_t next_node_ptr;      /* Node pointer info for next node */

        /* Lock B-tree current node */
        if(NULL == (internal = H5B2_protect_internal(f, dxpl_id, bt2_shared, curr_node_ptr.addr, curr_node_ptr.node_nrec, depth, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate node pointer for child */
        cmp = H5B2_locate_record(shared->type, internal->nrec, shared->nat_off, internal->int_native, udata, &idx);
        if(cmp > 0)
            idx++;

        if(cmp != 0) {
            /* Get node pointer for next node to search */
            next_node_ptr=internal->node_ptrs[idx];

            /* Unlock current node */
            if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Set pointer to next node to load */
            curr_node_ptr = next_node_ptr;
        } /* end if */
        else {
            hbool_t changed;            /* Whether the 'modify' callback changed the record */

            /* Make callback for current record */
            if ( (op)(H5B2_INT_NREC(internal,shared,idx), op_data, &changed) <0) {
                /* Make certain that the callback didn't modify the value if it failed */
                HDassert(changed==FALSE);

                /* Unlock current node */
                if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                HGOTO_ERROR(H5E_BTREE, H5E_CANTMODIFY, FAIL, "'modify' callback failed for B-tree find operation")
            } /* end if */

            /* Mark the node as dirty if it changed */
            internal_flags |= changed ? H5AC__DIRTIED_FLAG : 0;

            /* Unlock current node */
            if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr.addr, internal, internal_flags) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            HGOTO_DONE(SUCCEED);
        } /* end else */

        /* Decrement depth we're at in B-tree */
        depth--;
    } /* end while */

    {
        unsigned leaf_flags = H5AC__NO_FLAGS_SET;
        H5B2_leaf_t *leaf;      /* Pointer to leaf node in B-tree */
        hbool_t changed;        /* Whether the 'modify' callback changed the record */

        /* Lock B-tree leaf node */
        if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, &(curr_node_ptr.node_nrec), bt2_shared, H5AC_READ)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B-tree internal node")

        /* Locate record */
        cmp = H5B2_locate_record(shared->type, leaf->nrec, shared->nat_off, leaf->leaf_native, udata, &idx);

        if(cmp != 0) {
            /* Unlock leaf node */
            if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

            /* Note: don't push error on stack, leave that to next higher level,
             *      since many times the B-tree is searched in order to determine
             *      if an object exists in the B-tree or not. -QAK
             */
#ifdef OLD_WAY
                HGOTO_ERROR(H5E_BTREE, H5E_NOTFOUND, FAIL, "key not found in leaf node")
#else /* OLD_WAY */
                HGOTO_DONE(FAIL)
#endif /* OLD_WAY */
        } /* end if */
        else {
            /* Make callback for current record */
            if ((op)(H5B2_LEAF_NREC(leaf,shared,idx), op_data, &changed) <0) {
                /* Make certain that the callback didn't modify the value if it failed */
                HDassert(changed==FALSE);

                /* Unlock current node */
                if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")

                HGOTO_ERROR(H5E_BTREE, H5E_CANTMODIFY, FAIL, "'modify' callback failed for B-tree find operation")
            } /* end if */
        } /* end else */

        /* Mark the node as dirty if it changed */
        leaf_flags |= changed ? H5AC__DIRTIED_FLAG : 0;

        /* Unlock current node */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, curr_node_ptr.addr, leaf, leaf_flags) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B-tree node")
    }

done:
    /* Check if we need to decrement the reference count for the B-tree's shared info */
    if(incr_rc)
        H5RC_DEC(bt2_shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_modify() */

