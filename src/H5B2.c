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

/* B-tree version #'s */
#define H5B2_HDR_VERSION 0              /* Header */
#define H5B2_LEAF_VERSION 0             /* Leaf node */

/* Size of storage for number of records per node (on disk) */
#define H5B2_SIZEOF_RECORDS_PER_NODE    2

/* Size of a "node pointer" (on disk) */
#define H5B2_NODE_POINTER_SIZE(f)       (H5F_SIZEOF_ADDR(f)+H5B2_SIZEOF_RECORDS_PER_NODE+H5F_SIZEOF_SIZE(f))

/* Format overhead for each node (on disk) */
#define H5B2_OVERHEAD_SIZE              (H5B2_SIZEOF_MAGIC+1)   /* Signature + version # */

/* Number of records that fit into internal node */
#define H5B2_NUM_INT_REC(f,n,r)         (((n)-(H5B2_OVERHEAD_SIZE+H5B2_NODE_POINTER_SIZE(f)))/((r)+H5B2_NODE_POINTER_SIZE(f)))

/* Number of records that fit into leaf node */
#define H5B2_NUM_LEAF_REC(n,r)          (((n)-H5B2_OVERHEAD_SIZE)/(r))

/* Size of the B-tree header on disk */
#define H5B2_HEADER_SIZE(f)     (                                             \
    4 + /* Signature */                                                       \
    1 + /* Version */                                                         \
    1 + /* Tree type */                                                       \
    4 + /* Node size, in bytes */                                             \
    2 + /* Key size, in bytes */                                              \
    2 + /* Depth of tree */                                                   \
    2 + /* Split % of full (as integer, ie. "98" means 98%) */                \
    2 + /* Merge % of full (as integer, ie. "98" means 98%) */                \
    H5B2_NODE_POINTER_SIZE(f))  /* Node pointer to root node in tree */

/* Macro to retrieve pointer to i'th native key for leaf node */
#define H5B2_INT_NKEY(i,shared,idx)  ((i)->int_native+(shared)->int_nat_off[(idx)])
#define H5B2_LEAF_NKEY(l,shared,idx)  ((l)->leaf_native+(shared)->leaf_nat_off[(idx)])


/* Local typedefs */

/* Local prototypes */

/* Helper functions */
static herr_t H5B2_shared_free (void *_shared);
static herr_t H5B2_shared_init (H5F_t *f, H5B2_t *bt2, const H5B2_class_t *type,
    size_t node_size, size_t rkey_size, unsigned split_percent, unsigned merge_percent);
static herr_t H5B2_create_leaf(H5F_t *f, hid_t dxpl_id, H5B2_t *bt2, H5B2_node_ptr_t *node_ptr);
static int H5B2_locate_record(H5F_t *f, hid_t dxpl_id, const H5B2_class_t *type,
    unsigned nrec, size_t *rec_off, const uint8_t *native,
    const void *udata);

/* Metadata cache callbacks */
static H5B2_t *H5B2_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5B2_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_t *b);
static herr_t H5B2_cache_hdr_dest(H5F_t *f, H5B2_t *b);
static herr_t H5B2_cache_hdr_clear(H5F_t *f, H5B2_t *b, hbool_t destroy);
static herr_t H5B2_cache_hdr_size(const H5F_t *f, const H5B2_t *bt, size_t *size_ptr);
static H5B2_leaf_t *H5B2_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5B2_cache_leaf_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_leaf_t *l);
static herr_t H5B2_cache_leaf_dest(H5F_t *f, H5B2_leaf_t *l);
static herr_t H5B2_cache_leaf_clear(H5F_t *f, H5B2_leaf_t *l, hbool_t destroy);
static herr_t H5B2_cache_leaf_size(const H5F_t *f, const H5B2_leaf_t *l, size_t *size_ptr);

/* Static variables */

/* H5B2 inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT2_HDR[1] = {{
    H5AC_BT2_HDR_ID,
    (H5AC_load_func_t)H5B2_cache_hdr_load,
    (H5AC_flush_func_t)H5B2_cache_hdr_flush,
    (H5AC_dest_func_t)H5B2_cache_hdr_dest,
    (H5AC_clear_func_t)H5B2_cache_hdr_clear,
    (H5AC_size_func_t)H5B2_cache_hdr_size,
}};

/* H5B2 inherits cache-like properties from H5AC */
static const H5AC_class_t H5AC_BT2_LEAF[1] = {{
    H5AC_BT2_LEAF_ID,
    (H5AC_load_func_t)H5B2_cache_leaf_load,
    (H5AC_flush_func_t)H5B2_cache_leaf_flush,
    (H5AC_dest_func_t)H5B2_cache_leaf_dest,
    (H5AC_clear_func_t)H5B2_cache_leaf_clear,
    (H5AC_size_func_t)H5B2_cache_leaf_size,
}};


/* Declare a free list to manage B-tree header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);

/* Declare a free list to manage B-tree node pages to/from disk */
H5FL_BLK_DEFINE_STATIC(node_page);

/* Declare a free list to manage the 'H5B2_node_ptr_t' sequence information */
H5FL_SEQ_DEFINE_STATIC(H5B2_node_ptr_t);

/* Declare a free list to manage the 'size_t' sequence information */
H5FL_SEQ_DEFINE_STATIC(size_t);

/* Declare a free list to manage the H5B2_t struct */
H5FL_DEFINE_STATIC(H5B2_t);

/* Declare a free list to manage the H5B2_shared_t struct */
H5FL_DEFINE_STATIC(H5B2_shared_t);

/* Declare a free list to manage the H5B2_leaf_t struct */
H5FL_DEFINE_STATIC(H5B2_leaf_t);


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
static herr_t
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
    if((shared->int_nat_off=H5FL_SEQ_MALLOC(size_t,shared->internal_nrec))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Allocate array of pointers to leaf node native keys */
    if((shared->leaf_nat_off=H5FL_SEQ_MALLOC(size_t,shared->leaf_nrec))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Allocate array of pointers to internal node node pointers */
    if((shared->node_ptr_off=H5FL_SEQ_MALLOC(H5B2_node_ptr_t,(shared->internal_nrec+1)))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Initialize offsets in internal node native key block */
    for(u=0; u<shared->internal_nrec; u++)
        shared->int_nat_off[u]=type->nkey_size*u;

    /* Initialize offsets in leaf node native key block */
    for(u=0; u<shared->leaf_nrec; u++)
        shared->leaf_nat_off[u]=type->nkey_size*u;

    /* Initialize offsets in internal node node pointer block */
    for(u=0; u<(shared->internal_nrec+1); u++)
        shared->node_ptr_off[u]=sizeof(H5B2_node_ptr_t)*u;

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
static herr_t
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

    /* Free the array of offsets into the internal node native key block */
    if(shared->int_nat_off)
        H5FL_SEQ_FREE(size_t,shared->int_nat_off);

    /* Free the array of offsets into the leaf node native key block */
    if(shared->leaf_nat_off)
        H5FL_SEQ_FREE(size_t,shared->leaf_nat_off);

    /* Free the array of offsets into the internal node node pointer block */
    if(shared->node_ptr_off)
        H5FL_SEQ_FREE(H5B2_node_ptr_t,shared->node_ptr_off);

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
 * Function:	H5B2_cache_hdr_load
 *
 * Purpose:	Loads a B-tree header from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 1 2005
 *
 *-------------------------------------------------------------------------
 */
static H5B2_t *
H5B2_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void UNUSED *udata)
{
    const H5B2_class_t	*type = (const H5B2_class_t *) _type;
    size_t node_size, rkey_size;        /* Size info for B-tree */
    unsigned split_percent, merge_percent;      /* Split & merge info for B-tree */
    H5B2_t		*bt2 = NULL;
    size_t		size;
    uint8_t		*buf = NULL;
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5B2_t		*ret_value;

    FUNC_ENTER_NOAPI(H5B2_cache_hdr_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(type);

    if (NULL==(bt2 = H5FL_MALLOC(H5B2_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bt2->cache_info,0,sizeof(H5AC_info_t));

    /* Compute the size of the B-tree header on disk */
    size = H5B2_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if ((buf=H5FL_BLK_MALLOC(header_block,size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Read header from disk */
    if (H5F_block_read(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree header")

    p = buf;

    /* magic number */
    if (HDmemcmp(p, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header signature")
    p += H5B2_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5B2_HDR_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header version")

    /* B-tree type */
    if (*p++ != (uint8_t)type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* node size (in bytes) */
    UINT32DECODE(p, node_size);

    /* raw key size (in bytes) */
    UINT16DECODE(p, rkey_size);

    /* depth of tree */
    UINT16DECODE(p, bt2->depth);

    /* split & merge %s */
    UINT16DECODE(p, split_percent);
    UINT16DECODE(p, merge_percent);

    /* root node pointer */
    H5F_addr_decode(f, (const uint8_t **)&p, &(bt2->root.addr));
    UINT16DECODE(p, bt2->root.node_nrec);
    H5F_DECODE_LENGTH(f, p, bt2->root.all_nrec);

    /* Initialize shared B-tree info */
    if(H5B2_shared_init(f, bt2, type, node_size, rkey_size, split_percent, merge_percent)<0)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared B-tree info")

    /* Set return value */
    ret_value = bt2;

done:
    if(buf)
        H5FL_BLK_FREE(header_block,buf);
    if (!ret_value && bt2)
        (void)H5B2_cache_hdr_dest(f,bt2);
    FUNC_LEAVE_NOAPI(ret_value)
} /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_flush
 *
 * Purpose:	Flushes a dirty B-tree header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 1 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_t *bt2)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_hdr_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt2);

    if (bt2->cache_info.is_dirty) {
        H5B2_shared_t *shared;      /* Shared B-tree information */
        uint8_t	*buf = NULL;
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;

        /* Get the pointer to the shared B-tree info */
        shared=H5RC_GET_OBJ(bt2->shared);
        HDassert(shared);

        /* Compute the size of the B-tree header on disk */
        size = H5B2_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if ((buf=H5FL_BLK_MALLOC(header_block,size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed");

        p = buf;

        /* magic number */
        HDmemcpy(p, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC);
        p += H5B2_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5B2_HDR_VERSION;

        /* b-tree type */
        *p++ = shared->type->id;

        /* node size (in bytes) */
        UINT32ENCODE(p, shared->node_size);

        /* raw key size (in bytes) */
        UINT16ENCODE(p, shared->rkey_size);

        /* depth of tree */
        UINT16ENCODE(p, bt2->depth);

        /* split & merge %s */
        UINT16ENCODE(p, shared->split_percent);
        UINT16ENCODE(p, shared->merge_percent);

        /* root node pointer */
        H5F_addr_encode(f, &p, bt2->root.addr);
        UINT16ENCODE(p, bt2->root.node_nrec);
        H5F_ENCODE_LENGTH(f, p, bt2->root.all_nrec);

	/* Write the B-tree header. */
	if (H5F_block_write(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree header to disk")

        H5FL_BLK_FREE(header_block,buf);

	bt2->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5B2_cache_hdr_dest(f,bt2) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B_cache_hdr_dest
 *
 * Purpose:	Destroys a B-tree header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 1 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5B2_cache_hdr_dest(H5F_t UNUSED *f, H5B2_t *bt2)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(bt2);

    /* Decrement reference count on shared B-tree info */
    if(bt2->shared)
        H5RC_DEC(bt2->shared);

    /* Free B-tree header info */
    H5FL_FREE(H5B2_t,bt2);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2_cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_clear
 *
 * Purpose:	Mark a B-tree header in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb  1 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_hdr_clear(H5F_t *f, H5B2_t *bt2, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(bt2);

    /* Reset the dirty flag.  */
    bt2->cache_info.is_dirty = FALSE;
 
    if (destroy)
        if (H5B2_cache_hdr_dest(f, bt2) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a B-tree header
 *		on disk, and return it in *size_ptr.  On failure, 
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 1 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_hdr_size(const H5F_t *f, const H5B2_t UNUSED *bt2, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5B2_HEADER_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_hdr_size() */


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
    H5AC_info_t *cache_info;            /* Parent node's cache info */
    H5B2_t	*bt2=NULL;              /* Pointer to the B-tree header */
    H5B2_shared_t *shared;              /* Pointer to B-tree's shared information */
    H5B2_node_ptr_t *node_ptr;          /* Pointer to node pointer info for current node */
    unsigned    depth;                  /* Current depth of node */
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
depth=bt2->depth;
node_ptr=&(bt2->root);
cache_info=&(bt2->cache_info);

    /* Check if we are inserting in internal or leaf node */
    if(depth==0) {
        H5B2_leaf_t *leaf=NULL;      /* Pointer to leaf node */
        int         idx;             /* Location of record which matches key */

        /* Look up the B-tree leaf node */
        if (NULL == (leaf = H5AC_protect(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, shared->type, node_ptr, H5AC_WRITE)))
            HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, FAIL, "unable to load B-tree leaf node")

        /* Check for inserting into empty leaf */
        if(node_ptr->node_nrec==0)
            idx=0;
        else {
            /* Find correct location to insert this record and make room for it */
            if((idx=H5B2_locate_record(f,dxpl_id,shared->type,leaf->nrec,shared->leaf_nat_off,leaf->leaf_native,udata))<0) {
                /* Release the B-tree leaf node */
                if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
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
            if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")

	    HGOTO_ERROR(H5E_BTREE, H5E_CANTINSERT, FAIL, "unable to insert record into leaf node")
        } /* end if */

        /* Update record # info in node pointer */
        node_ptr->all_nrec++;
        node_ptr->node_nrec++;

        /* Update number of records in node */
        leaf->nrec++;
        HDassert(node_ptr->node_nrec==leaf->nrec);

        /* Mark parent's node as dirty now */
        cache_info->is_dirty = TRUE;

        /* Mark leaf node as dirty also */
        leaf->cache_info.is_dirty = TRUE;

        /* Release the B-tree leaf node */
        if (H5AC_unprotect(f, dxpl_id, H5AC_BT2_LEAF, node_ptr->addr, leaf, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree leaf node")
    } /* end if */
    else {
    } /* end else */

done:
    if (bt2 && H5AC_unprotect(f, dxpl_id, H5AC_BT2_HDR, addr, bt2, H5AC__NO_FLAGS_SET) < 0)
        HDONE_ERROR(H5E_BTREE, H5E_PROTECT, FAIL, "unable to release B-tree header")

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
herr_t
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
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "file allocation failed for B-tree header")

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
 * Function:	H5B2_cache_leaf_load
 *
 * Purpose:	Loads a B-tree leaf from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree leaf node.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
static H5B2_leaf_t *
H5B2_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_bt2, void *_node_ptr)
{
    const H5B2_t	*bt2 = (const H5B2_t *)_bt2;
    H5B2_node_ptr_t     *node_ptr = (H5B2_node_ptr_t *)_node_ptr;
    H5B2_leaf_t		*leaf = NULL;
    H5B2_shared_t 	*shared;      /* Shared B-tree information */
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5B2_leaf_t		*ret_value;

    FUNC_ENTER_NOAPI(H5B2_cache_leaf_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt2);

    if (NULL==(leaf = H5FL_MALLOC(H5B2_leaf_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&leaf->cache_info,0,sizeof(H5AC_info_t));

    /* Share common B-tree information */
    leaf->shared = bt2->shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Read header from disk */
    if (H5F_block_read(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree leaf node")

    p = shared->page;

    /* magic number */
    if (HDmemcmp(p, H5B2_LEAF_MAGIC, H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree leaf node signature")
    p += H5B2_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5B2_LEAF_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header version")

    /* B-tree type */
    if (*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Allocate space for the native keys in memory */
    if((leaf->leaf_native=H5FL_FAC_MALLOC(shared->leaf_fac))==NULL)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree leaf native keys")

HDfprintf(stderr,"%s: Deserialize leaf node records here!, node_ptr->node_nrec=%u\n",FUNC,node_ptr->node_nrec);

    /* Set return value */
    ret_value = leaf;

done:
    if (!ret_value && leaf)
        (void)H5B2_cache_leaf_dest(f,leaf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_leaf_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_flush
 *
 * Purpose:	Flushes a dirty B-tree leaf node to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_leaf_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_leaf_t *leaf)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_leaf_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(leaf);

    if (leaf->cache_info.is_dirty) {
        H5B2_shared_t *shared;  /* Shared B-tree information */
        uint8_t *p;             /* Pointer into raw data buffer */
        uint8_t *native;        /* Pointer to native keys */
        unsigned u;             /* Local index variable */

        /* Get the pointer to the shared B-tree info */
        shared=H5RC_GET_OBJ(leaf->shared);
        HDassert(shared);

        p = shared->page;

        /* magic number */
        HDmemcpy(p, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC);
        p += H5B2_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5B2_LEAF_VERSION;

        /* b-tree type */
        *p++ = shared->type->id;

        /* Serialize records for leaf node */
        native=leaf->leaf_native;
        for(u=0; u<leaf->nrec; u++) {
            /* Encode record */
            if((shared->type->encode)(f,p,native)<0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "enable to encode B-tree record");

            /* Move to next record */
            p += shared->rkey_size;
            native += shared->type->nkey_size;
        } /* end for */

	/* Write the B-tree leaf node */
	if (H5F_block_write(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree leaf node to disk")

	leaf->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5B2_cache_leaf_dest(f,leaf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_leaf_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B_cache_leaf_dest
 *
 * Purpose:	Destroys a B-tree leaf node in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
static herr_t
H5B2_cache_leaf_dest(H5F_t UNUSED *f, H5B2_leaf_t *leaf)
{
    H5B2_shared_t *shared;      /* Shared B-tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_leaf_dest)

    /*
     * Check arguments.
     */
    HDassert(leaf);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Release leaf's native key buffer */
    if(leaf->leaf_native)
        H5FL_FAC_FREE(shared->leaf_fac,leaf->leaf_native);

    /* Decrement reference count on shared B-tree info */
    if(leaf->shared)
        H5RC_DEC(leaf->shared);

    /* Free B-tree leaf node info */
    H5FL_FREE(H5B2_leaf_t,leaf);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2_cache_leaf_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_clear
 *
 * Purpose:	Mark a B-tree leaf node in memory as non-dirty.
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
H5B2_cache_leaf_clear(H5F_t *f, H5B2_leaf_t *leaf, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_leaf_clear)

    /*
     * Check arguments.
     */
    HDassert(leaf);

    /* Reset the dirty flag.  */
    leaf->cache_info.is_dirty = FALSE;
 
    if (destroy)
        if (H5B2_cache_leaf_dest(f, leaf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_cache_leaf_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_size
 *
 * Purpose:	Compute the size in bytes of a B-tree leaf node
 *		on disk, and return it in *size_ptr.  On failure, 
 *		the value of *size_ptr is undefined.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_leaf_size(const H5F_t UNUSED *f, const H5B2_leaf_t *leaf, size_t *size_ptr)
{
    H5B2_shared_t *shared;      /* Shared B-tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_leaf_size)

    /* check arguments */
    HDassert(leaf);
    HDassert(size_ptr);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Set size value */
    *size_ptr = shared->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_leaf_size() */

