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
 * Created:		H5BP.c
 *			Apr 14 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implements a B+ tree, with several modifications from
 *                      the "standard" methods.
 *
 *                      Please see the documentation in:
 *                      doc/html/TechNotes/Btrees.html for a full description
 *                      of how they work, etc.
 *
 *-------------------------------------------------------------------------
 */

#define H5BP_PACKAGE		/*suppress error about including H5BPpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BPpkg.h"		/* B+ trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/

/* Local macros */

/* Uncomment this macro to enable extra sanity checking */
/* #define H5BP_DEBUG */


/* Local typedefs */

/* Local prototypes */

/* Helper functions */


/* Package variables */

/* Declare a free list to manage the H5BP_t struct */
H5FL_DEFINE(H5BP_t);

/* Declare a free list to manage the H5BP_branch_t struct */
H5FL_DEFINE(H5BP_branch_t);

/* Declare a free list to manage the H5BP_twig_t struct */
H5FL_DEFINE(H5BP_twig_t);

/* Declare a free list to manage the H5BP_leaf_t struct */
H5FL_DEFINE(H5BP_leaf_t);


/* Static variables */

/* Declare a free list to manage B-tree node pages to/from disk */
H5FL_BLK_DEFINE_STATIC(node_page);

/* Declare a free list to manage the H5BP_shared_t struct */
H5FL_DEFINE_STATIC(H5BP_shared_t);



/*-------------------------------------------------------------------------
 * Function:	H5BP_shared_init
 *
 * Purpose:	Allocate & initialize shared B-tree info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 14 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BP_shared_init (H5BP_t *bpt, const H5BP_class_t *type,
    size_t node_size, unsigned split_percent, unsigned merge_percent)
{
    H5BP_shared_t *shared = NULL;       /* Shared B+ tree information */
    herr_t ret_value=SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5BP_shared_init)

    /* Allocate space for the shared information */
    if(NULL==(shared = H5FL_CALLOC(H5BP_shared_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B+ tree shared information")

    /* Assign user's information */
    shared->split_percent = split_percent;
    shared->merge_percent = merge_percent;
    shared->node_size = node_size;

    /* Assign common type information */
    shared->type = type;

    /* Allocate "page" for node I/O */
    if((shared->page=H5FL_BLK_MALLOC(node_page,shared->node_size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#ifdef H5_USING_PURIFY
HDmemset(shared->page,0,shared->node_size);
#endif /* H5_USING_PURIFY */

    /* Make shared B-tree info reference counted */
    if(NULL==(bpt->shared=H5RC_create(shared,H5BP_shared_free)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create ref-count wrapper for shared B+ tree info")

done:
    if(ret_value<0)
        if(shared)
            H5BP_shared_free(shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_shared_init() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_shared_free
 *
 * Purpose:	Free shared B+ tree info
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 14 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BP_shared_free (void *_shared)
{
    H5BP_shared_t *shared = (H5BP_shared_t *)_shared;
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_shared_free)

    /* Sanity check */
    HDassert(shared);

    /* Free the B-tree node buffer */
    if(shared->page)
        H5FL_BLK_FREE(node_page,shared->page);

    /* Free the shared B-tree info itself */
    H5FL_FREE(H5BP_shared_t,shared);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_shared_free() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_create
 *
 * Purpose:	Creates a new empty B+ tree in the file.
 *
 * Return:	Non-negative on success (with address of new B-tree
 *              filled in), negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BP_create(H5F_t *f, hid_t dxpl_id, const H5BP_class_t *type,
    size_t node_size, unsigned split_percent, unsigned merge_percent,
    haddr_t *addr_p)
{
    H5BP_t *bpt = NULL;                 /* The new B+ tree header information */
    herr_t ret_value=SUCCEED;

    FUNC_ENTER_NOAPI(H5BP_create, FAIL)

    /*
     * Check arguments.
     */
    HDassert(f);
    HDassert(type);
    HDassert(node_size>0);
    HDassert(merge_percent>0 && merge_percent<=100);
    HDassert(split_percent>0 && split_percent<=100);
    HDassert(merge_percent<(split_percent/2));

    /*
     * Allocate file and memory data structures.
     */
    if (NULL==(bpt = H5FL_MALLOC(H5BP_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B-tree header")

    /* Assign internal information */
    HDmemset(&bpt->cache_info,0,sizeof(H5AC_info_t));
    bpt->cache_info.is_dirty = TRUE;
    bpt->depth = 0;
    bpt->root.addr = HADDR_UNDEF;
    bpt->root.all_nrec = 0;
    bpt->root.util = 0;

    /* Initialize shared B-tree info */
    if(H5BP_shared_init(bpt, type, node_size, split_percent, merge_percent)<0)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "can't create shared B+ tree info")

    /* Allocate space for the header on disk */
    if (HADDR_UNDEF==(*addr_p=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)H5BP_HEADER_SIZE(f))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree header")

    /* Cache the new B-tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BPT_HDR, *addr_p, bpt, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B+ tree header to cache")

done:
    if (ret_value<0) {
	if (bpt)
            (void)H5BP_cache_hdr_dest(f,bpt);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_create() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_create_leaf
 *
 * Purpose:	Creates empty leaf node of a B+ tree and updates node pointer
 *              to point to it.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_create_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bpt_shared, H5BP_node_ptr_t *node_ptr)
{
    H5BP_leaf_t *leaf=NULL;     /* Pointer to new leaf node created */
    H5BP_shared_t *shared;      /* Shared B+ tree information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5BP_create_leaf, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(bpt_shared);
    HDassert(node_ptr);

    /* Allocate memory for leaf information */
    if (NULL==(leaf = H5FL_MALLOC(H5BP_leaf_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for B+ tree leaf info")

    /* Set metadata cache info */
    HDmemset(&leaf->cache_info,0,sizeof(H5AC_info_t));
    leaf->cache_info.is_dirty = TRUE;

    /* Share common B+ tree information */
    leaf->shared = bpt_shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Create record pool for leaf node */
    if((leaf->rec_pool = H5MP_create(H5BP_LEAF_POOL_PAGE, H5MP_FLG_DEFAULT)) == NULL)
        HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate memory pool")

    /* Set number of records */
    leaf->nrec=0;
    leaf->flags=0;

    /* Allocate space on disk for the leaf */
    if (HADDR_UNDEF==(node_ptr->addr=H5MF_alloc(f, H5FD_MEM_BTREE, dxpl_id, (hsize_t)shared->node_size)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B+ tree leaf node")

    /* Cache the new B+ tree node */
    if (H5AC_set(f, dxpl_id, H5AC_BPT_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "can't add B+ tree leaf to cache")

done:
    if (ret_value<0) {
	if (leaf)
            (void)H5BP_cache_leaf_dest(f,leaf);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_create_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_insert_leaf
 *
 * Purpose:	Adds a new record to a B+ tree leaf node.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 22 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5BP_insert_leaf(H5F_t *f, hid_t dxpl_id, H5RC_t *bpt_shared,
    H5BP_node_ptr_t *curr_node_ptr, void *udata)
{
    H5BP_leaf_t *leaf;                  /* Pointer to leaf node */
    H5BP_shared_t *shared;              /* Pointer to B+ tree's shared information */
    int         cmp;                    /* Comparison value of records */
    unsigned    idx;                    /* Location of record which matches key */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BP_insert_leaf)

    /* Check arguments. */
    HDassert(f);
    HDassert(bpt_shared);
    HDassert(curr_node_ptr);
    HDassert(H5F_addr_defined(curr_node_ptr->addr));

if( H5BP_NODE_OVERSIZE(curr_node_ptr->util)) {
    HDfprintf(stderr,"%s: attempting to insert record into extent node\n",FUNC);
    HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, FAIL, "Can't handle extent node!")
}
    /* Lock B+ tree node */
    if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BPT_LEAF, curr_node_ptr->addr, &(curr_node_ptr->util), bpt_shared, H5AC_WRITE)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B+ tree leaf node")

    /* Get the pointer to the shared B+ tree info */
    shared=H5RC_GET_OBJ(bpt_shared);
    HDassert(shared);

#ifndef LATER
HDfprintf(stderr,"%s: finish record insertion code\n",FUNC);
HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, FAIL, "Can't handle record insertion!")
#else /* LATER */
    /* Check for inserting into empty leaf */
    if(leaf->nrec==0)
        idx=0;
    else {
        /* Find correct location to insert this record */
        if((cmp = H5BP_locate_record(shared->type,leaf->nrec,shared->nat_off,leaf->leaf_native,udata,&idx)) == 0)
            HGOTO_ERROR(H5E_BTREE, H5E_EXISTS, FAIL, "record is already in B+ tree")
        if(cmp > 0)
            idx++;

        /* Make room for new record */
        if(idx<leaf->nrec)
            HDmemmove(H5BP_LEAF_NREC(leaf,shared,idx+1),H5BP_LEAF_NREC(leaf,shared,idx),shared->type->nrec_size*(leaf->nrec-idx));
    } /* end else */

    /* Make callback to store record in native form */
    if((shared->type->store)(H5BP_LEAF_NREC(leaf,shared,idx),udata)<0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into leaf node")
#endif /* LATER */

    /* Update record count for node pointer to current node */
    curr_node_ptr->all_nrec++;

    /* Update record count for current node */
    leaf->nrec++;

done:
    /* Release the B+ tree leaf node */
    if (leaf && H5AC_unprotect(f, dxpl_id, H5AC_BPT_LEAF, curr_node_ptr->addr, leaf, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release leaf B+ tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_insert_leaf() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_insert
 *
 * Purpose:	Adds a new record to the B+ tree.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5BP_insert(H5F_t *f, hid_t dxpl_id, const H5BP_class_t *type, haddr_t addr,
           void *udata)
{
    H5BP_t	*bpt=NULL;              /* Pointer to the B+ tree header */
    H5BP_shared_t *shared;              /* Pointer to B+ tree's shared information */
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(H5BP_insert, FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(type);
    HDassert(H5F_addr_defined(addr));

    /* Look up the B+ tree header */
    if (NULL == (bpt = H5AC_protect(f, dxpl_id, H5AC_BPT_HDR, addr, type, NULL, H5AC_WRITE)))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTPROTECT, FAIL, "unable to load B+ tree header")

    /* Get the pointer to the shared B+ tree info */
    shared=H5RC_GET_OBJ(bpt->shared);
    HDassert(shared);

    /* Check if the root node is allocated yet */
    if(!H5F_addr_defined(bpt->root.addr)) {
        /* Create root node as leaf node in B+ tree */
        if(H5BP_create_leaf(f, dxpl_id, bpt->shared, &(bpt->root))<0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINIT, FAIL, "unable to create root node")

        /* Mark B+ tree header as dirty, since we updated the address of the root node */
        bpt->cache_info.is_dirty = TRUE;
    } /* end if */
    /* Check if we need to split the root node (equiv. to a 1->2 leaf node split) */
    else if(H5BP_NODE_OVERSIZE(bpt->root.util) ||
            (!H5BP_NODE_OVERSIZE(bpt->root.util) && bpt->root.util>=shared->split_percent)) {
        /* Split root node */
#ifdef LATER
        if(H5BP_split_root(f, dxpl_id, bpt, bpt->shared)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTSPLIT, FAIL, "unable to split root node")
#else /* LATER */
HDfprintf(stderr,"%s: attempting to split root node\n",FUNC);
HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, FAIL, "Can't split root yet!")
#endif /* LATER */
    } /* end if */

    /* Attempt to insert record into B+ tree */
    if(bpt->depth>0) {
#ifdef LATER
        if(H5BP_insert_internal(f,dxpl_id,bpt->shared,bpt->depth,&(bpt->cache_info),&bpt->root,udata)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B+ tree internal node")
#else /* LATER */
HDfprintf(stderr,"%s: attempting to insert record with internal node\n",FUNC);
HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, FAIL, "Can't insert records yet!")
#endif /* LATER */
    } /* end if */
    else {
        if(H5BP_insert_leaf(f,dxpl_id,bpt->shared,&bpt->root,udata)<0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into B+ tree leaf node")
    } /* end else */

done:
    /* Release the B+ tree header info */
    if (bpt && H5AC_unprotect(f, dxpl_id, H5AC_BPT_HDR, addr, bpt, H5AC__DIRTIED_FLAG) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_CANTUNPROTECT, FAIL, "unable to release B+ tree header info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_insert() */

