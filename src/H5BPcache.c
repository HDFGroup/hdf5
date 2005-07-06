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
 * Created:		H5BPcache.c
 *			Apr 19 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement B+ tree metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

#define H5BP_PACKAGE		/*suppress error about including H5BPpkg  */

/* Private headers */
#include "H5private.h"		/* Generic Functions			*/
#include "H5BPpkg.h"		/* B+ trees				*/
#include "H5Eprivate.h"		/* Error handling		  	*/

/* Local macros */

/* B+ tree format version #'s */
#define H5BP_HDR_VERSION 0              /* Header */
#define H5BP_LEAF_VERSION 0             /* Leaf nodes */


/* Local typedefs */

/* Local prototypes */

/* Metadata cache callbacks */
static H5BP_t *H5BP_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void *udata);
static herr_t H5BP_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BP_t *b);
static herr_t H5BP_cache_hdr_clear(H5F_t *f, H5BP_t *b, hbool_t destroy);
static herr_t H5BP_cache_hdr_size(const H5F_t *f, const H5BP_t *bt, size_t *size_ptr);
static H5BP_leaf_t *H5BP_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_bpt_shared);
static herr_t H5BP_cache_leaf_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BP_leaf_t *l);
static herr_t H5BP_cache_leaf_clear(H5F_t *f, H5BP_leaf_t *l, hbool_t destroy);
static herr_t H5BP_cache_leaf_size(const H5F_t *f, const H5BP_leaf_t *l, size_t *size_ptr);

/* Package variables */

/* H5BP headers inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_BPT_HDR[1] = {{
    H5AC_BPT_HDR_ID,
    (H5AC_load_func_t)H5BP_cache_hdr_load,
    (H5AC_flush_func_t)H5BP_cache_hdr_flush,
    (H5AC_dest_func_t)H5BP_cache_hdr_dest,
    (H5AC_clear_func_t)H5BP_cache_hdr_clear,
    (H5AC_size_func_t)H5BP_cache_hdr_size,
}};

/* H5BP leaves inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_BPT_LEAF[1] = {{
    H5AC_BPT_LEAF_ID,
    (H5AC_load_func_t)H5BP_cache_leaf_load,
    (H5AC_flush_func_t)H5BP_cache_leaf_flush,
    (H5AC_dest_func_t)H5BP_cache_leaf_dest,
    (H5AC_clear_func_t)H5BP_cache_leaf_clear,
    (H5AC_size_func_t)H5BP_cache_leaf_size,
}};

/* Static variables */

/* Declare a free list to manage B+ tree header data to/from disk */
H5FL_BLK_DEFINE_STATIC(header_block);

/* Declare a free list to manage 'void *' array data */
typedef void *voidp;
H5FL_SEQ_DEFINE_STATIC(voidp);



/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_hdr_load
 *
 * Purpose:	Loads a B+ tree header from the disk.
 *
 * Return:	Success:	Pointer to a new B+ tree.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
static H5BP_t *
H5BP_cache_hdr_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_type, void UNUSED *udata)
{
    const H5BP_class_t	*type = (const H5BP_class_t *) _type;
    size_t node_size;                           /* Size info for B+ tree */
    unsigned split_percent, merge_percent;      /* Split & merge info for B+ tree */
    H5BP_t		*bpt = NULL;
    size_t		size;
    uint8_t		*buf = NULL;
    uint8_t		*p;             /* Pointer into raw data buffer */
    H5BP_t		*ret_value;

    FUNC_ENTER_NOAPI(H5BP_cache_hdr_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(type);

    if (NULL==(bpt = H5FL_MALLOC(H5BP_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bpt->cache_info,0,sizeof(H5AC_info_t));

    /* Compute the size of the B+ tree header on disk */
    size = H5BP_HEADER_SIZE(f);

    /* Allocate temporary buffer */
    if ((buf=H5FL_BLK_MALLOC(header_block,size))==NULL)
        HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Read header from disk */
    if (H5F_block_read(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B+ tree header")

    p = buf;

    /* magic number */
    if (HDmemcmp(p, H5BP_HDR_MAGIC, H5BP_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B+ tree header signature")
    p += H5BP_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5BP_HDR_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B+ tree header version")

    /* B+ tree type */
    if (*p++ != (uint8_t)type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B+ tree type")

    /* node size (in bytes) */
    UINT32DECODE(p, node_size);

    /* depth of tree */
    UINT16DECODE(p, bpt->depth);

    /* split & merge %s */
    UINT16DECODE(p, split_percent);
    UINT16DECODE(p, merge_percent);

    /* root node pointer */
    H5F_addr_decode(f, (const uint8_t **)&p, &(bpt->root.addr));
    H5F_DECODE_LENGTH(f, p, bpt->root.all_nrec);
    bpt->root.util = *p++;

    /* Initialize shared B+ tree info */
    if(H5BP_shared_init(bpt, type, node_size, split_percent, merge_percent)<0)
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared B+ tree info")

    /* Set return value */
    ret_value = bpt;

done:
    if(buf)
        H5FL_BLK_FREE(header_block,buf);
    if (!ret_value && bpt)
        (void)H5BP_cache_hdr_dest(f,bpt);
    FUNC_LEAVE_NOAPI(ret_value)
} /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_hdr_flush
 *
 * Purpose:	Flushes a dirty B+ tree header to disk.
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
H5BP_cache_hdr_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BP_t *bpt)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5BP_cache_hdr_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bpt);

    if (bpt->cache_info.is_dirty) {
        H5BP_shared_t *shared;      /* Shared B+ tree information */
        uint8_t	*buf = NULL;
        uint8_t *p;                 /* Pointer into raw data buffer */
        size_t	size;

        /* Get the pointer to the shared B+ tree info */
        shared=H5RC_GET_OBJ(bpt->shared);
        HDassert(shared);

        /* Compute the size of the B+ tree header on disk */
        size = H5BP_HEADER_SIZE(f);

        /* Allocate temporary buffer */
        if ((buf=H5FL_BLK_MALLOC(header_block,size))==NULL)
            HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        p = buf;

        /* magic number */
        HDmemcpy(p, H5BP_HDR_MAGIC, H5BP_SIZEOF_MAGIC);
        p += H5BP_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5BP_HDR_VERSION;

        /* B+ tree type */
        *p++ = shared->type->id;

        /* node size (in bytes) */
        UINT32ENCODE(p, shared->node_size);

        /* depth of tree */
        UINT16ENCODE(p, bpt->depth);

        /* split & merge %s */
        UINT16ENCODE(p, shared->split_percent);
        UINT16ENCODE(p, shared->merge_percent);

        /* root node pointer */
        H5F_addr_encode(f, &p, bpt->root.addr);
        H5F_ENCODE_LENGTH(f, p, bpt->root.all_nrec);
        *p++ = bpt->root.util;

	/* Write the B+ tree header. */
	if (H5F_block_write(f, H5FD_MEM_BTREE, addr, size, dxpl_id, buf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B+ tree header to disk")

        H5FL_BLK_FREE(header_block,buf);

	bpt->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5BP_cache_hdr_dest(f, bpt) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B+ tree header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_cache_hdr_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_hdr_dest
 *
 * Purpose:	Destroys a B+ tree header in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5BP_cache_hdr_dest(H5F_t UNUSED *f, H5BP_t *bpt)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_cache_hdr_dest)

    /*
     * Check arguments.
     */
    HDassert(bpt);

    /* Decrement reference count on shared B+ tree info */
    if(bpt->shared)
        H5RC_DEC(bpt->shared);

    /* Free B+ tree header info */
    H5FL_FREE(H5BP_t,bpt);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5BP_cache_hdr_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_hdr_clear
 *
 * Purpose:	Mark a B+ tree header in memory as non-dirty.
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
H5BP_cache_hdr_clear(H5F_t *f, H5BP_t *bpt, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BP_cache_hdr_clear)

    /*
     * Check arguments.
     */
    HDassert(bpt);

    /* Reset the dirty flag.  */
    bpt->cache_info.is_dirty = FALSE;
 
    if (destroy)
        if (H5BP_cache_hdr_dest(f, bpt) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B+ tree header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_cache_hdr_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_hdr_size
 *
 * Purpose:	Compute the size in bytes of a B+ tree header
 *		on disk, and return it in *size_ptr.  On failure, 
 *		the value of *size_ptr is undefined.
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
H5BP_cache_hdr_size(const H5F_t *f, const H5BP_t UNUSED *bpt, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_cache_hdr_size)

    /* check arguments */
    HDassert(f);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = H5BP_HEADER_SIZE(f);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5BP_cache_hdr_size() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_leaf_load
 *
 * Purpose:	Loads a B+ tree leaf from the disk.
 *
 * Return:	Success:	Pointer to a new B+ tree leaf node.
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
static H5BP_leaf_t *
H5BP_cache_leaf_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_nrec, void *_bpt_shared)
{
    const unsigned char *nrec = (const unsigned char *)_nrec;
    H5RC_t 		*bpt_shared=(H5RC_t *)_bpt_shared;        /* Shared B+ tree information */
    H5BP_shared_t 	*shared;        /* Shared B+ tree information */
    H5BP_leaf_t		*leaf = NULL;   /* Pointer to new leaf */
    uint8_t		*p;             /* Pointer into raw data buffer */
    size_t              rec_size;       /* Size of records */
    unsigned		u;              /* Local index variable */
    H5BP_leaf_t		*ret_value;

    FUNC_ENTER_NOAPI(H5BP_cache_leaf_load, NULL)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(bpt_shared);

    if (NULL==(leaf = H5FL_MALLOC(H5BP_leaf_t)))
	HGOTO_ERROR (H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&leaf->cache_info,0,sizeof(H5AC_info_t));

    /* Share common B+ tree information */
    leaf->shared = bpt_shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B+ tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Check if we can use existing buffer for node */
    /* Read header from disk */
    if (H5F_block_read(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page)<0)
	HGOTO_ERROR(H5E_BTREE, H5E_READERROR, NULL, "can't read B+ tree leaf node")

    p = shared->page;

    /* magic number */
    if (HDmemcmp(p, H5BP_LEAF_MAGIC, H5BP_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B+ tree leaf node signature")
    p += H5BP_SIZEOF_MAGIC;

    /* version */
    if (*p++ != H5BP_LEAF_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B+ tree leaf node version")

    /* B+ tree type */
    if (*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B+ tree type")

    /* Set the number of records in the leaf */
    leaf->nrec = *nrec;

    /* Get the status flags for the leaf */
    leaf->flags = *p++;

    /* Create record pool for leaf node */
    if((leaf->rec_pool = H5MP_create(H5BP_LEAF_POOL_PAGE, H5MP_FLG_DEFAULT)) == NULL)
        HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate memory pool")

    /* Retrieve variable-length record information, if present */
    if(H5BP_LEAF_REC_VARLEN(leaf->flags)) {
        /* Allocate space for the record pointers */
        if((leaf->rec_ptr = H5FL_SEQ_MALLOC(voidp, leaf->nrec)) == NULL)
            HGOTO_ERROR(H5E_BTREE, H5E_NOSPACE, NULL, "can't allocate record pointers for node")

        /* Work through the records on disk and insert them into array for node */
        for(u = 0; u < leaf->nrec; u++) {
            size_t  rec_len;        /* Index of current record */

            /* Determine the length of the record */
            UINT16DECODE(p, rec_len);

            /* Decode the record */
            if((leaf->rec_ptr[u] = (shared->type->decode)(f, leaf->rec_pool, p, rec_len)) == NULL)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "can't decode B+ tree record")
            p += rec_len;
        } /* end for */
    } /* end if */
    else {
        /* Retrieve the records' size */
        UINT16DECODE(p, rec_size);

HDfprintf(stderr,"%s: attempting to decode fixed-size records from leaf node\n",FUNC);
HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, NULL, "Can't decode records yet!")
    } /* end else */

    /* Set return value */
    ret_value = leaf;

done:
    if (!ret_value && leaf)
        (void)H5BP_cache_leaf_dest(f,leaf);
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_cache_leaf_load() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_leaf_flush
 *
 * Purpose:	Flushes a dirty B+ tree leaf node to disk.
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
H5BP_cache_leaf_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5BP_leaf_t *leaf)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(H5BP_cache_leaf_flush, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(leaf);

    if (leaf->cache_info.is_dirty) {
        H5BP_shared_t *shared;  /* Shared B+ tree information */
        uint8_t *p;             /* Pointer into raw data buffer */
#ifdef LATER
        uint8_t *native;        /* Pointer to native keys */
        unsigned u;             /* Local index variable */
#endif /* LATER */

        /* Get the pointer to the shared B+ tree info */
        shared=H5RC_GET_OBJ(leaf->shared);
        HDassert(shared);

        p = shared->page;

        /* magic number */
        HDmemcpy(p, H5BP_LEAF_MAGIC, H5BP_SIZEOF_MAGIC);
        p += H5BP_SIZEOF_MAGIC;

        /* version # */
        *p++ = H5BP_LEAF_VERSION;

        /* B+ tree type */
        *p++ = shared->type->id;

        /* status flags */
        *p++ = leaf->flags;

        /* Serialize records */
        if(H5BP_LEAF_REC_VARLEN(leaf->flags)) {
            unsigned u;         /* Local index */

            /* Work through the records in memory and encode them to disk */
            for(u = 0; u < leaf->nrec; u++) {
                size_t  rec_len;        /* Index of current record */
                uint8_t *tp;            /* Temporary pointer into raw data buffer */

                /* Save the location for the record length */
                tp = p;

                /* Skip over the location for the record length */
                p += 2;

                /* Encode the record */
                if((shared->type->encode)(f, leaf->rec_ptr[u], p, &rec_len) < 0)
                    HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, NULL, "can't encode B+ tree record")
                assert(rec_len > 0);
                p += rec_len;

                /* Encode the record length */
                UINT16ENCODE(tp, rec_len);
            } /* end for */
        } /* end if */
        else {
HDfprintf(stderr,"%s: attempting to decode fixed-size records from leaf node\n",FUNC);
HGOTO_ERROR(H5E_BTREE, H5E_UNSUPPORTED, NULL, "Can't decode records yet!")
        } /* end else */

#ifdef LATER
        /* Serialize records for leaf node */
        native=leaf->leaf_native;
        for(u=0; u<leaf->nrec; u++) {
            /* Encode record */
            if((shared->type->encode)(f,p,native)<0)
                HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B+ tree record")

            /* Move to next record */
            p += shared->rrec_size;
            native += shared->type->nrec_size;
        } /* end for */
#endif /* LATER */

	/* Write the B+ tree leaf node */
	if (H5F_block_write(f, H5FD_MEM_BTREE, addr, shared->node_size, dxpl_id, shared->page) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFLUSH, FAIL, "unable to save B+ tree leaf node to disk")

	leaf->cache_info.is_dirty = FALSE;
    } /* end if */

    if (destroy)
        if (H5BP_cache_leaf_dest(f,leaf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B+ tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5BP_cache_leaf_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_leaf_dest
 *
 * Purpose:	Destroys a B+ tree leaf node in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Apr 19 2005
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5BP_cache_leaf_dest(H5F_t UNUSED *f, H5BP_leaf_t *leaf)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_cache_leaf_dest)

    /*
     * Check arguments.
     */
    HDassert(leaf);

    /* Decrement reference count on shared B+ tree info */
    if(leaf->shared)
        H5RC_DEC(leaf->shared);

    /* Release B+ tree leaf node record pointer array */
    H5FL_SEQ_FREE(voidp, leaf->rec_ptr);

    /* Release B+ tree leaf node record memory pool */
    H5MP_close(leaf->rec_pool);

    /* Free B+ tree leaf node info */
    H5FL_FREE(H5BP_leaf_t, leaf);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5BP_cache_leaf_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_leaf_clear
 *
 * Purpose:	Mark a B+ tree leaf node in memory as non-dirty.
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
H5BP_cache_leaf_clear(H5F_t *f, H5BP_leaf_t *leaf, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT(H5BP_cache_leaf_clear)

    /*
     * Check arguments.
     */
    HDassert(leaf);

    /* Reset the dirty flag.  */
    leaf->cache_info.is_dirty = FALSE;
 
    if (destroy)
        if (H5BP_cache_leaf_dest(f, leaf) < 0)
	    HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B+ tree leaf node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5BP_cache_leaf_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5BP_cache_leaf_size
 *
 * Purpose:	Compute the size in bytes of a B+ tree leaf node
 *		on disk, and return it in *size_ptr.  On failure, 
 *		the value of *size_ptr is undefined.
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
H5BP_cache_leaf_size(const H5F_t UNUSED *f, const H5BP_leaf_t *leaf, size_t *size_ptr)
{
    H5BP_shared_t *shared;      /* Shared B+ tree information */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5BP_cache_leaf_size)

    /* check arguments */
    HDassert(leaf);
    HDassert(size_ptr);

    /* Get the pointer to the shared B+ tree info */
    shared=H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    /* Set size value */
    *size_ptr = shared->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5BP_cache_leaf_size() */

