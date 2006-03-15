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
 * Created:		H5B2cache.c
 *			Jan 31 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement v2 B-tree metadata cache methods.
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

/****************/
/* Local Macros */
/****************/

/* B-tree format version #'s */
#define H5B2_HDR_VERSION 0              /* Header */
#define H5B2_INT_VERSION 0              /* Internal node */
#define H5B2_LEAF_VERSION 0             /* Leaf node */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static H5B2_t *H5B2_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5B2_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_t *b);
static herr_t H5B2_cache_hdr_clear(H5F_t *f, H5B2_t *b, hbool_t destroy);
static herr_t H5B2_cache_hdr_size(const H5F_t *f, const H5B2_t *bt, size_t *size_ptr);
static H5B2_leaf_t *H5B2_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_shared);
static herr_t H5B2_cache_leaf_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_leaf_t *l);
static herr_t H5B2_cache_leaf_clear(H5F_t *f, H5B2_leaf_t *l, hbool_t destroy);
static herr_t H5B2_cache_leaf_size(const H5F_t *f, const H5B2_leaf_t *l, size_t *size_ptr);
static H5B2_internal_t *H5B2_cache_internal_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_shared);
static herr_t H5B2_cache_internal_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_internal_t *i);
static herr_t H5B2_cache_internal_clear(H5F_t *f, H5B2_internal_t *i, hbool_t destroy);
static herr_t H5B2_cache_internal_size(const H5F_t *f, const H5B2_internal_t *i, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

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
const H5AC_class_t H5AC_BT2_LEAF[1] = {{
    H5AC_BT2_LEAF_ID,
    (H5AC_load_func_t)H5B2_cache_leaf_load,
    (H5AC_flush_func_t)H5B2_cache_leaf_flush,
    (H5AC_dest_func_t)H5B2_cache_leaf_dest,
    (H5AC_clear_func_t)H5B2_cache_leaf_clear,
    (H5AC_size_func_t)H5B2_cache_leaf_size,
}};

/* H5B2 inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT2_INT[1] = {{
    H5AC_BT2_INT_ID,
    (H5AC_load_func_t)H5B2_cache_internal_load,
    (H5AC_flush_func_t)H5B2_cache_internal_flush,
    (H5AC_dest_func_t)H5B2_cache_internal_dest,
    (H5AC_clear_func_t)H5B2_cache_internal_clear,
    (H5AC_size_func_t)H5B2_cache_internal_size,
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage B-tree header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);



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
    const H5B2_class_t	*type = (const H5B2_class_t *) _type;   /* Type of B-tree */
    size_t node_size, rrec_size;        /* Size info for B-tree */
    unsigned split_percent, merge_percent;      /* Split & merge info for B-tree */
    H5B2_t		*bt2 = NULL;    /* B-tree info */
    size_t		size;           /* Header size */
    uint8_t		*buf = NULL;    /* Temporary buffer */
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5B2_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_hdr_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(type);

    /* Allocate space for the B-tree data structure */
    if(NULL == (bt2 = H5FL_MALLOC(H5B2_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bt2->cache_info, 0, sizeof(H5AC_info_t));

    /* Compute the size of the B-tree header on disk */
    size = H5B2_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree header")

    p = buf;

    /* Magic number */
    if(HDmemcmp(p, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_HDR_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header version")

/* XXX: Add metadata flags & checksum to v2 B-tree metadata (like fractal heap) */

    /* B-tree type */
    if(*p++ != (uint8_t)type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Node size (in bytes) */
    UINT32DECODE(p, node_size);

    /* Raw key size (in bytes) */
    UINT16DECODE(p, rrec_size);

    /* Depth of tree */
    UINT16DECODE(p, bt2->depth);

    /* Split & merge %s */
    UINT16DECODE(p, split_percent);
    UINT16DECODE(p, merge_percent);

    /* Root node pointer */
    H5F_addr_decode(f, (const uint8_t **)&p, &(bt2->root.addr));
    UINT16DECODE(p, bt2->root.node_nrec);
    H5F_DECODE_LENGTH(f, p, bt2->root.all_nrec);

    /* Initialize shared B-tree info */
    HDassert((size_t)(p - buf) == size);
    if(H5B2_shared_init(f, bt2, type, node_size, rrec_size, split_percent, merge_percent) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared B-tree info")

    /* Set return value */
    ret_value = bt2;

done:
    if(buf)
        H5FL_BLK_FREE(header_block, buf);
    if(!ret_value && bt2)
        (void)H5B2_cache_hdr_dest(f, bt2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_cache_hdr_load() */ /*lint !e818 Can't make udata a pointer to const */


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
        uint8_t	*buf = NULL;        /* Temporary raw data buffer */
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;               /* Header size on disk */

        /* Get the pointer to the shared B-tree info */
        shared = H5RC_GET_OBJ(bt2->shared);
        HDassert(shared);

        /* Compute the size of the B-tree header on disk */
        size = H5B2_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if((buf = H5FL_BLK_MALLOC(header_block, size)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* Magic number */
        HDmemcpy(p, H5B2_HDR_MAGIC, H5B2_SIZEOF_MAGIC);
        p += H5B2_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5B2_HDR_VERSION;

        /* B-tree type */
        *p++ = shared->type->id;

        /* Node size (in bytes) */
        UINT32ENCODE(p, shared->node_size);

        /* Raw key size (in bytes) */
        UINT16ENCODE(p, shared->rrec_size);

        /* Depth of tree */
        UINT16ENCODE(p, bt2->depth);

        /* Split & merge %s */
        UINT16ENCODE(p, shared->split_percent);
        UINT16ENCODE(p, shared->merge_percent);

        /* Root node pointer */
        H5F_addr_encode(f, &p, bt2->root.addr);
        UINT16ENCODE(p, bt2->root.node_nrec);
        H5F_ENCODE_LENGTH(f, p, bt2->root.all_nrec);

	/* Write the B-tree header. */
        HDassert((size_t)(p - buf) == size);
	if(H5F_block_write(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree header to disk")

        H5FL_BLK_FREE(header_block, buf);

	bt2->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5B2_cache_hdr_dest(f, bt2) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_dest
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
herr_t
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
    H5FL_FREE(H5B2_t, bt2);

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
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(bt2);

    /* Reset the dirty flag.  */
    bt2->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5B2_cache_hdr_dest(f, bt2) < 0)
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
H5B2_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_bt2_shared)
{
    const unsigned      *nrec = (const unsigned *)_nrec;
    H5RC_t 		*bt2_shared = (H5RC_t *)_bt2_shared;        /* Shared B-tree information */
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    H5B2_leaf_t		*leaf = NULL;   /* Pointer to lead node loaded */
    uint8_t		*p;             /* Pointer into raw data buffer */
    uint8_t		*native;        /* Pointer to native keys */
    unsigned		u;              /* Local index variable */
    H5B2_leaf_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_leaf_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt2_shared);

    if(NULL == (leaf = H5FL_MALLOC(H5B2_leaf_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&leaf->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common B-tree information */
    leaf->shared = bt2_shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page) < 0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree leaf node")

    p = shared->page;

    /* Magic number */
    if(HDmemcmp(p, H5B2_LEAF_MAGIC, H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree leaf node signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_LEAF_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree leaf node version")

/* XXX: Add metadata flags & checksum to v2 B-tree metadata (like fractal heap) */

    /* B-tree type */
    if(*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Allocate space for the native keys in memory */
    if((leaf->leaf_native = H5FL_FAC_MALLOC(shared->leaf_fac)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree leaf native keys")

    /* Set the number of records in the leaf */
    leaf->nrec = *nrec;

    /* Deserialize records for leaf node */
    native = leaf->leaf_native;
    for(u = 0; u < leaf->nrec; u++) {
        /* Decode record */
        if((shared->type->decode)(f, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, NULL, "unable to decode B-tree record")

        /* Move to next record */
        p += shared->rrec_size;
        native += shared->type->nrec_size;
    } /* end for */

    /* Sanity check parsing */
    HDassert((size_t)(p - shared->page) <= shared->node_size);

    /* Set return value */
    ret_value = leaf;

done:
    if(!ret_value && leaf)
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

    if(leaf->cache_info.is_dirty) {
        H5B2_shared_t *shared;  /* Shared B-tree information */
        uint8_t *p;             /* Pointer into raw data buffer */
        uint8_t *native;        /* Pointer to native keys */
        unsigned u;             /* Local index variable */

        /* Get the pointer to the shared B-tree info */
        shared = H5RC_GET_OBJ(leaf->shared);
        HDassert(shared);

        p = shared->page;

        /* magic number */
        HDmemcpy(p, H5B2_LEAF_MAGIC, H5B2_SIZEOF_MAGIC);
        p += H5B2_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5B2_LEAF_VERSION;

        /* b-tree type */
        *p++ = shared->type->id;

        /* Serialize records for leaf node */
        native = leaf->leaf_native;
        for(u = 0; u < leaf->nrec; u++) {
            /* Encode record */
            if((shared->type->encode)(f, p, native) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree record")

            /* Move to next record */
            p += shared->rrec_size;
            native += shared->type->nrec_size;
        } /* end for */

	/* Write the B-tree leaf node */
        HDassert((size_t)(p - shared->page) <= shared->node_size);
	if(H5F_block_write(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree leaf node to disk")

	leaf->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5B2_cache_leaf_dest(f, leaf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_leaf_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_dest
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
herr_t
H5B2_cache_leaf_dest(H5F_t UNUSED *f, H5B2_leaf_t *leaf)
{
    H5B2_shared_t *shared;      /* Shared B-tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_leaf_dest)

    /*
     * Check arguments.
     */
    HDassert(leaf);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(leaf->shared);
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

    if(destroy)
        if(H5B2_cache_leaf_dest(f, leaf) < 0)
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
    shared = H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Set size value */
    *size_ptr = shared->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_leaf_size() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_load
 *
 * Purpose:	Loads a B-tree internal node from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree internal node.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 *-------------------------------------------------------------------------
 */
static H5B2_internal_t *
H5B2_cache_internal_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_bt2_shared)
{
    const unsigned      *nrec = (const unsigned *)_nrec;
    H5RC_t 		*bt2_shared = (H5RC_t *)_bt2_shared;       /* Ref counter for shared B-tree info */
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    H5B2_internal_t	*internal = NULL;       /* Internal node read */
    uint8_t		*p;             /* Pointer into raw data buffer */
    uint8_t		*native;        /* Pointer to native record info */
    H5B2_node_ptr_t	*int_node_ptr;  /* Pointer to node pointer info */
    unsigned		u;              /* Local index variable */
    H5B2_internal_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_internal_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bt2_shared);

    if(NULL == (internal = H5FL_MALLOC(H5B2_internal_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&internal->cache_info, 0, sizeof(H5AC_info_t));

    /* Share common B-tree information */
    internal->shared = bt2_shared;
    H5RC_INC(internal->shared);

    /* Get the pointer to the shared B-tree info */
    shared=H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Read header from disk */
    if(H5F_block_read(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B-tree internal node")

    p = shared->page;

    /* Magic number */
    if(HDmemcmp(p, H5B2_INT_MAGIC, H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree internal node signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_INT_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree internal node version")

/* XXX: Add metadata flags & checksum to v2 B-tree metadata (like fractal heap) */

    /* B-tree type */
    if (*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Allocate space for the native keys in memory */
    if((internal->int_native = H5FL_FAC_MALLOC(shared->int_fac)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree internal native keys")

    /* Allocate space for the node pointers in memory */
    if((internal->node_ptrs = H5FL_FAC_MALLOC(shared->node_ptr_fac)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree internal node pointers")

    /* Set the number of records in the leaf */
    internal->nrec = *nrec;

    /* Deserialize records for internal node */
    native = internal->int_native;
    for(u = 0; u < internal->nrec; u++) {
        /* Decode record */
        if((shared->type->decode)(f, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, NULL, "unable to decode B-tree record")

        /* Move to next record */
        p += shared->rrec_size;
        native += shared->type->nrec_size;
    } /* end for */

    /* Deserialize node pointers for internal node */
    int_node_ptr = internal->node_ptrs;
    for(u = 0; u < internal->nrec + 1; u++) {
        /* Decode node pointer */
        H5F_addr_decode(f, (const uint8_t **)&p, &(int_node_ptr->addr));
        UINT16DECODE(p, int_node_ptr->node_nrec);
        H5F_DECODE_LENGTH(f, p, int_node_ptr->all_nrec);

        /* Move to next node pointer */
        int_node_ptr++;
    } /* end for */

    /* Sanity check parsing */
    HDassert((size_t)(p - shared->page) <= shared->node_size);

    /* Set return value */
    ret_value = internal;

done:
    if(!ret_value && internal)
        (void)H5B2_cache_internal_dest(f, internal);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_internal_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_flush
 *
 * Purpose:	Flushes a dirty B-tree internal node to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 3 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_internal_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5B2_internal_t *internal)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5B2_cache_internal_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(internal);

    if(internal->cache_info.is_dirty) {
        H5B2_shared_t *shared;  /* Shared B-tree information */
        uint8_t *p;             /* Pointer into raw data buffer */
        uint8_t *native;        /* Pointer to native record info */
        H5B2_node_ptr_t *int_node_ptr;      /* Pointer to node pointer info */
        unsigned u;             /* Local index variable */

        /* Get the pointer to the shared B-tree info */
        shared = H5RC_GET_OBJ(internal->shared);
        HDassert(shared);

        p = shared->page;

        /* Magic number */
        HDmemcpy(p, H5B2_INT_MAGIC, H5B2_SIZEOF_MAGIC);
        p += H5B2_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5B2_INT_VERSION;

        /* B-tree type */
        *p++ = shared->type->id;

        /* Serialize records for internal node */
        native = internal->int_native;
        for(u = 0; u < internal->nrec; u++) {
            /* Encode record */
            if((shared->type->encode)(f, p, native) < 0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree record")

            /* Move to next record */
            p += shared->rrec_size;
            native += shared->type->nrec_size;
        } /* end for */

        /* Serialize node pointers for internal node */
        int_node_ptr = internal->node_ptrs;
        for(u = 0; u < internal->nrec + 1; u++) {
            /* Encode node pointer */
            H5F_addr_encode(f, &p, int_node_ptr->addr);
            UINT16ENCODE(p, int_node_ptr->node_nrec);
            H5F_ENCODE_LENGTH(f, p, int_node_ptr->all_nrec);

            /* Move to next node pointer */
            int_node_ptr++;
        } /* end for */

	/* Write the B-tree internal node */
        HDassert((size_t)(p - shared->page) <= shared->node_size);
	if(H5F_block_write(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B-tree internal node to disk")

	internal->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5B2_cache_internal_dest(f, internal) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree internal node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_internal_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_dest
 *
 * Purpose:	Destroys a B-tree internal node in memory.
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
herr_t
H5B2_cache_internal_dest(H5F_t UNUSED *f, H5B2_internal_t *internal)
{
    H5B2_shared_t *shared;      /* Shared B-tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_internal_dest)

    /*
     * Check arguments.
     */
    HDassert(internal);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Release internal node's native key buffer */
    if(internal->int_native)
        H5FL_FAC_FREE(shared->int_fac, internal->int_native);

    /* Release internal node's node pointer buffer */
    if(internal->node_ptrs)
        H5FL_FAC_FREE(shared->node_ptr_fac, internal->node_ptrs);

    /* Decrement reference count on shared B-tree info */
    if(internal->shared)
        H5RC_DEC(internal->shared);

    /* Free B-tree internal node info */
    H5FL_FREE(H5B2_internal_t, internal);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B2_cache_internal_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_clear
 *
 * Purpose:	Mark a B-tree internal node in memory as non-dirty.
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
H5B2_cache_internal_clear(H5F_t *f, H5B2_internal_t *internal, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_internal_clear)

    /*
     * Check arguments.
     */
    HDassert(internal);

    /* Reset the dirty flag.  */
    internal->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5B2_cache_internal_dest(f, internal) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree internal node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_cache_internal_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_size
 *
 * Purpose:	Compute the size in bytes of a B-tree internal node
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
H5B2_cache_internal_size(const H5F_t UNUSED *f, const H5B2_internal_t *internal, size_t *size_ptr)
{
    H5B2_shared_t *shared;      /* Shared B-tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_internal_size)

    /* check arguments */
    HDassert(internal);
    HDassert(size_ptr);

    /* Get the pointer to the shared B-tree info */
    shared = H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    /* Set size value */
    *size_ptr = shared->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_internal_size() */

