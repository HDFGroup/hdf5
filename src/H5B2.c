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

#define H5B2_PACKAGE		/*suppress error about including H5B2pkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5B2pkg.h"		/* B-trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/

/* Local macros */

/* Format overhead for each node (on disk) */
#define H5B2_OVERHEAD_SIZE              (H5B2_SIZEOF_MAGIC+1)   /* Signature + version # */

/* Number of records that fit into internal node */
#define H5B2_NUM_INT_REC(f,n,r)         (((n)-(H5B2_OVERHEAD_SIZE+H5B2_NODE_POINTER_SIZE(f)))/((r)+H5B2_NODE_POINTER_SIZE(f)))

/* Number of records that fit into leaf node */
#define H5B2_NUM_LEAF_REC(n,r)          (((n)-H5B2_OVERHEAD_SIZE)/(r))

/* Macro to retrieve pointer to i'th native key for leaf node */
#define H5B2_INT_NKEY(i,shared,idx)  ((i)->int_native+(shared)->nat_off[(idx)])
#define H5B2_LEAF_NKEY(l,shared,idx)  ((l)->leaf_native+(shared)->nat_off[(idx)])


/* Local typedefs */

/* Local prototypes */

/* Helper functions */
static herr_t H5B2_create_leaf(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, H5B2_node_ptr_t *node_ptr);
static int H5B2_locate_record(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    unsigned nrec, size_t *rec_off, const uint8_t *native,
    const void *udata);
static herr_t H5B2_split_root(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2,
    const H5B2_shared_t *shared);
static herr_t H5B2_create_internal(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2,
    H5B2_node_ptr_t *node_ptr);


/* Package variables */

/* Declare a free list to manage the H5B2_t struct */
H5FL_DEFINE(H5B2_t);

/* Declare a free list to manage the H5B2_internal_t struct */
H5FL_DEFINE(H5B2_internal_t);

/* Declare a free list to manage the H5B2_leaf_t struct */
H5FL_DEFINE(H5B2_leaf_t);


/* Static variables */

/* Declare a free list to manage B-tree node pages to/from disk */
H5FL_BLK_DEFINE_STATIC(node_page);

/* Declare a free list to manage the 'size_t' sequence information */
H5FL_SEQ_DEFINE_STATIC(size_t);

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
    size_t node_size, size_t rkey_size,
    unsigned split_percent, unsigned merge_percent)
{
    H5B2_shared_t *shared = NULL;       /* Shared B-tree information */
    unsigned u;                         /* Local index variable */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_shared_init)

    /* Allocate space for the shared information */
    if(NULL==(shared = H5FL_CALLOC(H5B2_shared_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree shared information")

    /* Assign user's information */
    shared->split_percent = split_percent;
    shared->merge_percent = merge_percent;
    shared->node_size = node_size;
    shared->rkey_size = rkey_size;

    /* Compute derived information */
    shared->internal_nrec = H5B2_NUM_INT_REC(f,shared->node_size,shared->rkey_size);
    shared->split_int_nrec = (shared->internal_nrec * shared->split_percent)/100;
    shared->merge_int_nrec = (shared->internal_nrec * shared->merge_percent)/100;

    shared->leaf_nrec = H5B2_NUM_LEAF_REC(shared->node_size,shared->rkey_size);
    shared->split_leaf_nrec = (shared->leaf_nrec * shared->split_percent)/100;
    shared->merge_leaf_nrec = (shared->leaf_nrec * shared->merge_percent)/100;

    /* Assign common type information */
    shared->type = type;

    /* Allocate "page" for node I/O */
    if((shared->page=H5FL_BLK_MALLOC(node_page,shared->node_size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Create factory for internal node native record storage */
    if((shared->int_fac=H5FL_fac_init(type->nkey_size*shared->internal_nrec))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_CANTINIT, NULL, "can't create internal node native key block factory")

    /* Create factory for leaf node native record storage */
    if((shared->leaf_fac=H5FL_fac_init(type->nkey_size*shared->leaf_nrec))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_CANTINIT, NULL, "can't create leaf node native key block factory")

    /* Create factory for internal node node pointer storage */
    if((shared->node_ptr_fac=H5FL_fac_init(sizeof(H5B2_node_ptr_t)*(shared->internal_nrec+1)))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_CANTINIT, NULL, "can't create internal node node pointer block factory")

    /* Allocate array of pointers to internal node native keys */
    if((shared->nat_off=H5FL_SEQ_MALLOC(size_t,MAX(shared->internal_nrec,shared->leaf_nrec)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Initialize offsets in native key block */
    for(u=0; u<MAX(shared->internal_nrec,shared->leaf_nrec); u++)
        shared->nat_off[u]=type->nkey_size*u;

    /* Make shared B-tree info reference counted */
    if(NULL==(bt2->shared=H5RC_create(shared,H5B2_shared_free)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create ref-count wrapper for shared B-tree info")

done:
    if(ret_value<0)
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
herr_t
H5B2_shared_free (void *_shared)
{
    H5B2_shared_t *shared = (H5B2_shared_t *)_shared;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_shared_free)

    /* Sanity check */
    HDassert(shared);

    /* Free the B-tree node buffer */
    if(shared->page)
        H5FL_BLK_FREE(node_page,shared->page);

    /* Destroy factory for internal node native record storage */
    if(shared->int_fac)
        if(H5FL_fac_term(shared->int_fac)<0)
            HGOTO_ERROR (H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy internal node native key block factory")

    /* Destroy factory for leaf node native record storage */
    if(shared->leaf_fac)
        if(H5FL_fac_term(shared->leaf_fac)<0)
            HGOTO_ERROR (H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy leaf node native key block factory")

    /* Destroy factory for internal node node pointer storage */
    if(shared->node_ptr_fac)
        if(H5FL_fac_term(shared->node_ptr_fac)<0)
            HGOTO_ERROR (H5E_RESOURCE, H5E_CANTRELEASE, FAIL, "can't destroy internal node node pointer block factory")

    /* Free the array of offsets into the native key block */
    if(shared->nat_off)
        H5FL_SEQ_FREE(size_t,shared->nat_off);

    /* Free the shared B-tree info itself */
    H5FL_FREE(H5B2_shared_t,shared);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_create
 *
 * Purpose:	Creates a new empty B-tree in the file.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Jan 31 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5B2_create(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    size_t node_size, size_t rkey_size,
    unsigned split_percent, unsigned merge_percent, haddr_t *addr_p)
{
    H5B2_t *bt2 = NULL;                 /* The new B-tree header information */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(type);
    HDassert(node_size>0);
    HDassert(rkey_size>0);
    HDassert(merge_percent>0 && merge_percent<=100);
    HDassert(split_percent>0 && split_percent<=100);
    HDassert(merge_percent<(split_percent/2));

    /*
     * Allocate file and memory data structures.
     */
    if (NULL==(bt2 = H5FL_MALLOC(H5B2_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree header")

    /* Assign internal information */
    HDmemset(&bt2->cache_info,0,sizeof(H5AC_info_t));
    bt2->cache_info.is_dirty = TRUE;
    bt2->depth = 0;
    bt2->root.addr = HADDR_UNDEF;
    bt2->root.node_nrec = bt2->root.all_nrec = 0;

    /* Initialize shared B-tree info */
    if(H5B2_shared_init(f, bt2, type, node_size, rkey_size, split_percent, merge_percent)<0)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared B-tree info")

    /* Allocate space for the header on disk */
    if (HADDR_UNDEF==(*addr_p=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)H5B2_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree header")

    /* Cache the new B-tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BT2_HDR, *addr_p, bt2, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree header to cache")

done:
    if (ret_value<0) {
	if (bt2)
            (void)H5B2_cache_hdr_dest(f,bt2);
    } /* end if */
    
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_create() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_locate_record
 *
 * Purpose:	Performs a binary search to locate a record in a sorted
 *              array of records.
 *
 * Return:	Success:	Non-negative array index where new record
 *                              should be inserted
 *
 *		Failure:	Negative, if record already is in array of
 *                              records.
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  3 2005
 *
 *-------------------------------------------------------------------------
 */
static int
H5B2_locate_record(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    unsigned nrec, size_t *rec_off, const uint8_t *native,
    const void *udata)
{
    unsigned	lo = 0, hi;     /* Low & high index values */
    int         idx = 0;        /* Final index value */
    int         cmp = -1;       /* Key comparison value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_locate_record)

    hi = nrec;
    while (lo < hi && cmp) {
	idx = (lo + hi) / 2;
	if ((cmp = (type->compare)(f, dxpl_id, udata, native+rec_off[idx])) < 0)
	    hi = idx;
	else
	    lo = idx + 1;
    }
    if(cmp==0)
        idx=(-1);
    else if(cmp>0)
        idx++;

    FUNC_LEAVE_NOAPI(idx);
} /* end H5B2_locate_record */


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
static herr_t
H5B2_split_root(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, const H5B2_shared_t *shared)
{
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_split_root)

    HDassert(f);
    HDassert(bt2);
    HDassert(shared);

    if(bt2->depth==0) {
        H5B2_internal_t *new_int=NULL;  /* Pointer to new internal node */
        H5B2_leaf_t *old_leaf=NULL, *new_leaf=NULL;       /* Pointers to old & new leaf nodes */
        H5B2_node_ptr_t new_int_ptr;    /* Node pointer to manage new internal node */
        H5B2_node_ptr_t new_leaf_ptr;   /* Node pointer to manage new leaf node */
        H5B2_node_ptr_t old_leaf_ptr;   /* Node pointer to manage old leaf node */
        unsigned mid_record;            /* Index of "middle" record in current node */

        /* Create new leaf node */
        new_leaf_ptr.all_nrec=new_leaf_ptr.node_nrec=0;
        if(H5B2_create_leaf(f, dxpl_id, bt2, &new_leaf_ptr)<0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create new leaf node")

        /* Determine "middle" record to promote to new root */
        mid_record = shared->split_leaf_nrec/2;

        /* Set "old" leaf pointer to root node */
        old_leaf_ptr = bt2->root;

        /* Protect both leafs */
        if (NULL == (old_leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, old_leaf_ptr.addr, shared->type, &old_leaf_ptr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree leaf node")
        if (NULL == (new_leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, new_leaf_ptr.addr, shared->type, &new_leaf_ptr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree leaf node")

        /* Copy "upper half" of records to new leaf */
        HDmemcpy(new_leaf->leaf_native,H5B2_LEAF_NKEY(old_leaf,shared,mid_record+1),shared->type->nkey_size*(shared->split_leaf_nrec-(mid_record+1)));

        /* Create new internal node */
        new_int_ptr.all_nrec=new_int_ptr.node_nrec=0;
        if(H5B2_create_internal(f, dxpl_id, bt2, &new_int_ptr)<0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create new internal node")

        /* Protect new internal node */
        if (NULL == (new_int = H5AC_protect(f, dxpl_id, H5AC_BT2_INT, new_int_ptr.addr, shared->type, &new_int_ptr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree internal node")

        /* Copy "middle" record to new internal node */
        HDmemcpy(new_int->int_native,H5B2_LEAF_NKEY(old_leaf,shared,mid_record),shared->type->nkey_size);

        /* Update record counts in leaf nodes */
        old_leaf_ptr.all_nrec = old_leaf_ptr.node_nrec = old_leaf->nrec = mid_record;
        new_leaf_ptr.all_nrec = new_leaf_ptr.node_nrec = new_leaf->nrec = shared->split_leaf_nrec-(mid_record+1);

        /* Mark leaf nodes as dirty */
        old_leaf->cache_info.is_dirty = TRUE;
        new_leaf->cache_info.is_dirty = TRUE;

        /* Release leaf nodes */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, old_leaf_ptr.addr, old_leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, new_leaf_ptr.addr, new_leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

        /* Set internal node pointers to leaf nodes */
        new_int->node_ptrs[0] = old_leaf_ptr;
        new_int->node_ptrs[1] = new_leaf_ptr;

        /* Update record count in new internal node */
        new_int->nrec = 1;

        /* Mark new internal node as dirty */
        new_int->cache_info.is_dirty = TRUE;

        /* Release new internal node */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_INT, new_int_ptr.addr, new_int, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree internal node")

        /* Update depth of B-tree */
        bt2->depth++;

        /* Update pointer to B-tree's root node to pointer to new internal node */
        bt2->root.addr = new_int_ptr.addr;
        bt2->root.node_nrec = 1;

        /* Mark B-tree header as dirty */
        bt2->cache_info.is_dirty = TRUE;
    } /* end if */
    else {
HDfprintf(stderr,"%s: Need to split internal root! shared->split_int_nrec=%Zu\n",FUNC,shared->split_int_nrec);
HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split internal root node yet")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5B2_split_root */


/*-------------------------------------------------------------------------
 * Function:	H5B2_insert
 *
 * Purpose:	Adds a new record to the B-tree.  If the root node of
 *		the B-tree splits then the B-tree header tracks the new
 *              root node created.
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
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    H5B2_node_ptr_t leaf_ptr;           /* Node pointer info for leaf node */
    H5B2_leaf_t *leaf=NULL;             /* Pointer to leaf node */
    int         idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_insert, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the b-tree header */
    if (NULL == (bt2 = H5AC_protect(f, dxpl_id, H5AC_BT2_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree header")

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Check if the root node is allocated yet */
    if(!H5F_addr_defined(bt2->root.addr)) {
        /* Create root node as leaf node in B-tree */
        if(H5B2_create_leaf(f, dxpl_id, bt2, &(bt2->root))<0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create root node")

        /* Mark B-tree header as dirty, since we updated the address of the root node */
        bt2->cache_info.is_dirty = TRUE;
    } /* end if */
    /* Check if we need to split the root node */
    else if((bt2->depth==0 && bt2->root.node_nrec==shared->split_leaf_nrec) ||
            (bt2->depth>0 && bt2->root.node_nrec==shared->split_int_nrec)) {
        /* Split root node */
        if(H5B2_split_root(f, dxpl_id, bt2, shared)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split root node")
    } /* end if */
        
    /* Check for non-trivial B-tree */
    if(bt2->depth>0) {
        H5B2_internal_t *internal;          /* Pointer to internal node in B-tree */
        H5AC_info_t *parent_cache_info;     /* Parent node's cache info */
        haddr_t     parent_addr;            /* Address of parent which contain's node pointer to current node */
        void        *parent_ptr;            /* Pointer to parent structure in memory */
        H5AC_info_t *curr_cache_info=NULL;  /* Current node's cache info */
        H5B2_node_ptr_t *curr_node_ptr;     /* Pointer to node pointer info for current node (in parent node) */
        haddr_t curr_addr=HADDR_UNDEF;      /* Address of current node */
        void        *curr_ptr=NULL;         /* Pointer to current structure in memory */
        H5B2_node_ptr_t *child_node_ptr=NULL;   /* Pointer to node pointer info for child node */
        unsigned    depth;                  /* Depth of current node */

        /* Track the depth of current node (leaves are depth 0) */
        depth=bt2->depth;

        /* Set initial "parent" information to the B-tree header */
        parent_cache_info=&(bt2->cache_info);
        parent_addr=addr;
        parent_ptr=bt2;

        /* Set initial node pointer for "current" node */
        curr_node_ptr=&(bt2->root);

        /* Find correct leaf to insert node into */
        while(depth>0) {
            /* Lock B-tree current node */
            if (NULL == (internal = H5AC_protect(f, dxpl_id, H5AC_BT2_INT, curr_node_ptr->addr, shared->type, curr_node_ptr, H5AC_WRITE)))
                HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree internal node")

            /* Set information for current (root) node */
            curr_cache_info=&(internal->cache_info);
            curr_addr=curr_node_ptr->addr;
            curr_ptr=internal;

            /* Locate node pointer for child */
            if((idx=H5B2_locate_record(f,dxpl_id,shared->type,internal->nrec,shared->nat_off,internal->int_native,udata))<0)
                HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B-tree")

            /* Get node pointer for child */
            child_node_ptr=&(internal->node_ptrs[idx]);

            /* Preemptively split/redistribute a node we will enter */
            if((depth==1 && child_node_ptr->node_nrec==shared->split_leaf_nrec) ||
                    (depth>1 && child_node_ptr->node_nrec==shared->split_int_nrec)) {
HDfprintf(stderr,"%s: need to split child node!, depth=%u\n",FUNC,depth);
HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split child node")
                /* Attempt to redistribute records among children */
                /* Split children, if can't redistribute */
                    /* Update record count info for current node to reflect new # of records (in parent) */
                /* Update child_node_ptr (to reflect change in location from split/redistribution) */
            } /* end if */

            /* Update record count (in parent) */
            curr_node_ptr->all_nrec++;

            /* Mark parent node as dirty */
            parent_cache_info->is_dirty = TRUE;

            /* Unlock parent node (possibly the B-tree header) */
            if (H5AC_unprotect(f, dxpl_id, parent_cache_info->type, parent_addr, parent_ptr, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree info")

            /* Transition "current" info into "parent" info */
            parent_cache_info = curr_cache_info;
            parent_addr = curr_addr;
            parent_ptr = curr_ptr;

            /* Transition "child" node pointer to "current" node pointer */
            curr_node_ptr = child_node_ptr;

            /* Decrement depth of current node */
            depth--;
        } /* end while */

        /* Update record count for leaf (in current node) */
        HDassert(curr_node_ptr);
        curr_node_ptr->all_nrec++;
        curr_node_ptr->node_nrec++;

        /* Mark parent node as dirty */
        curr_cache_info->is_dirty = TRUE;

        /* Copy node pointer info for leaf */
        leaf_ptr = *curr_node_ptr;

        /* Release current node */
        if (H5AC_unprotect(f, dxpl_id, curr_cache_info->type, curr_addr, curr_ptr, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree node")
    } /* end if */
    else {
        /* Update record count for root node */
        bt2->root.all_nrec++;
        bt2->root.node_nrec++;

        /* Mark parent node as dirty */
        bt2->cache_info.is_dirty = TRUE;

        /* Copy node pointer info for leaf */
        leaf_ptr = bt2->root;

        /* Release parent node */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree header info")
        bt2=NULL;
    } /* end else */

    /* Must have a leaf node with enough space to insert a record now */
    HDassert(H5F_addr_defined(leaf_ptr.addr));
    HDassert((leaf_ptr.node_nrec-1)<shared->split_leaf_nrec); /* node pointer to leaf has already been incremented */

    /* Look up the B-tree leaf node */
    if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, leaf_ptr.addr, shared->type, &leaf_ptr, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree leaf node")

    /* Sanity check number of records */
    HDassert(leaf_ptr.all_nrec==leaf_ptr.node_nrec);
    HDassert(leaf->nrec==(leaf_ptr.node_nrec-1)); /* node pointer to leaf has already been incremented */

    /* Check for inserting into empty leaf */
    if(leaf->nrec==0)
        idx=0;
    else {
        /* Sanity check for the leaf node being full */
        HDassert(leaf->nrec!=shared->split_leaf_nrec);

        /* Find correct location to insert this record and make room for it */
        if((idx=H5B2_locate_record(f,dxpl_id,shared->type,leaf->nrec,shared->nat_off,leaf->leaf_native,udata))<0) {
            /* Release the B-tree leaf node */
            if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, leaf_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

            HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B-tree")
        } /* end if */

        /* Make room for new record */
        if((unsigned)idx<leaf->nrec)
            HDmemmove(H5B2_LEAF_NKEY(leaf,shared,idx+1),H5B2_LEAF_NKEY(leaf,shared,idx),shared->type->nkey_size*(leaf->nrec-idx));
    } /* end else */

    /* Make callback to store record in native form */
    if((shared->type->store)(f,dxpl_id,udata,H5B2_LEAF_NKEY(leaf,shared,idx))<0) {
        /* Release the B-tree leaf node */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, leaf_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

        HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into leaf node")
    } /* end if */

    /* Update number of records in node */
    leaf->nrec++;

    /* Mark leaf node as dirty also */
    leaf->cache_info.is_dirty = TRUE;

    /* Release the B-tree leaf node */
    if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, leaf_ptr.addr, leaf, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_insert() */


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
static herr_t
H5B2_create_leaf(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, H5B2_node_ptr_t *node_ptr)
{
    H5B2_leaf_t *leaf=NULL;     /* Pointer to new leaf node created */
    H5B2_shared_t *shared;      /* Shared B-tree information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_create_leaf, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2);
    HDassert(node_ptr);

    /* Allocate memory for leaf information */
    if (NULL==(leaf = H5FL_MALLOC(H5B2_leaf_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree leaf info")

    /* Set metadata cache info */
    HDmemset(&leaf->cache_info,0,sizeof(H5AC_info_t));
    leaf->cache_info.is_dirty = TRUE;

    /* Share common B-tree information */
    leaf->shared = bt2->shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Allocate space for the native keys in memory */
    if((leaf->leaf_native=H5FL_FAC_MALLOC(shared->leaf_fac))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree leaf native keys")

    /* Set number of records */
    leaf->nrec=0;

    /* Allocate space on disk for the leaf */
    if (HADDR_UNDEF==(node_ptr->addr=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)shared->node_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree leaf node")

    /* Cache the new B-tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree leaf to cache")

done:
    if (ret_value<0) {
	if (leaf)
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
H5B2_create_internal(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, H5B2_node_ptr_t *node_ptr)
{
    H5B2_internal_t *internal=NULL;     /* Pointer to new internal node created */
    H5B2_shared_t *shared;              /* Shared B-tree information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5B2_create_internal, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(bt2);
    HDassert(node_ptr);

    /* Allocate memory for internal node information */
    if (NULL==(internal = H5FL_MALLOC(H5B2_internal_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal info")

    /* Set metadata cache info */
    HDmemset(&internal->cache_info,0,sizeof(H5AC_info_t));
    internal->cache_info.is_dirty = TRUE;

    /* Share common B-tree information */
    internal->shared = bt2->shared;
    H5RC_INC(internal->shared);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Allocate space for the native keys in memory */
    if((internal->int_native=H5FL_FAC_MALLOC(shared->int_fac))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal native keys")

    /* Allocate space for the node pointers in memory */
    if((internal->node_ptrs=H5FL_FAC_MALLOC(shared->node_ptr_fac))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree internal node pointers")

    /* Set number of records */
    internal->nrec=0;

    /* Allocate space on disk for the internal node */
    if (HADDR_UNDEF==(node_ptr->addr=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)shared->node_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree internal node")

    /* Cache the new B-tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BT2_INT, node_ptr->addr, internal, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B-tree internal node to cache")

done:
    if (ret_value<0) {
	if (internal)
            (void)H5B2_cache_internal_dest(f,internal);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_create_internal() */

