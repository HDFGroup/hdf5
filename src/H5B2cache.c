/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


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
static void *H5B2_cache_hdr_deserialize(haddr_t addr, size_t len,
    const void *image, void *udata, hbool_t *dirty);
static herr_t H5B2_cache_hdr_serialize(const H5F_t *f, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5B2_cache_hdr_free_icr(haddr_t addr, size_t len, void *thing);

static void *H5B2_cache_internal_deserialize(haddr_t addr, size_t len,
    const void *image, void *udata, hbool_t *dirty);
static herr_t H5B2_cache_internal_serialize(const H5F_t *f, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5B2_cache_internal_free_icr(haddr_t addr, size_t len, void *thing);

static void *H5B2_cache_leaf_deserialize(haddr_t addr, size_t len,
    const void *image, void *udata, hbool_t *dirty);
static herr_t H5B2_cache_leaf_serialize(const H5F_t *f, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr,
    size_t *new_len, void **new_image);
static herr_t H5B2_cache_leaf_free_icr(haddr_t addr, size_t len, void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5B2 inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_BT2_HDR[1] = {{
    H5AC2_BT2_HDR_ID,
    "v2 b-tree header",
    H5FD_MEM_BTREE,
    H5B2_cache_hdr_deserialize,
    NULL,
    H5B2_cache_hdr_serialize,
    H5B2_cache_hdr_free_icr,
    NULL,
}};

/* H5B2 inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_BT2_INT[1] = {{
    H5AC2_BT2_INT_ID,
    "v2 b-tree internal node",
    H5FD_MEM_BTREE,
    H5B2_cache_internal_deserialize,
    NULL,
    H5B2_cache_internal_serialize,
    H5B2_cache_internal_free_icr,
    NULL,
}};

/* H5B2 inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_BT2_LEAF[1] = {{
    H5AC2_BT2_LEAF_ID,
    "v2 b-tree leaf node",
    H5FD_MEM_BTREE,
    H5B2_cache_leaf_deserialize,
    NULL,
    H5B2_cache_leaf_serialize,
    H5B2_cache_leaf_free_icr,
    NULL,
}};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_deserialize
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
 * Changes:     Mike McGreevy
 *              mcgreevy@hdfgroup.prg
 *              June 18, 2008
 *              Converted from H5B2_cache_hdr_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2_cache_hdr_deserialize(haddr_t UNUSED addr, size_t UNUSED len, 
    const void *image, void *_udata, hbool_t UNUSED *dirty)
{
    H5B2_hdr_cache_ud_t *udata = (H5B2_hdr_cache_ud_t *)_udata;
    unsigned depth;                     /* Depth of B-tree */
    size_t node_size, rrec_size;        /* Size info for B-tree */
    uint8_t split_percent, merge_percent;      /* Split & merge %s for B-tree */
    H5B2_t		*bt2 = NULL;    /* B-tree info */
    size_t		size;           /* Header size */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    H5B2_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_hdr_deserialize)

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* Allocate space for the B-tree data structure */
    if(NULL == (bt2 = H5FL_MALLOC(H5B2_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&bt2->cache_info, 0, sizeof(H5AC2_info_t));

    /* Compute the size of the serialized B-tree header on disk */
    size = H5B2_HEADER_SIZE(udata->f);

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5B2_HDR_MAGIC, (size_t)H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_HDR_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree header version")

    /* B-tree type */
    if(*p++ != (uint8_t)udata->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Node size (in bytes) */
    UINT32DECODE(p, node_size);

    /* Raw key size (in bytes) */
    UINT16DECODE(p, rrec_size);

    /* Depth of tree */
    UINT16DECODE(p, depth);

    /* Split & merge %s */
    split_percent = *p++;
    merge_percent = *p++;

    /* Root node pointer */
    H5F_addr_decode(udata->f, (const uint8_t **)&p, &(bt2->root.addr));
    UINT16DECODE(p, bt2->root.node_nrec);
    H5F_DECODE_LENGTH(udata->f, p, bt2->root.all_nrec);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Compute checksum on entire header */
    computed_chksum = H5_checksum_metadata(image, (size - H5B2_SIZEOF_CHKSUM), 0);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "incorrect metadata checksum for v2 B-tree header")

    /* Initialize shared B-tree info */
    if(H5B2_shared_init(udata->f, bt2, udata->type, depth, node_size, rrec_size, split_percent, merge_percent) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "can't create shared B-tree info")

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Set return value */
    ret_value = bt2;

done:
    if(!ret_value && bt2)
        (void)H5B2_cache_hdr_dest(bt2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B2_cache_hdr_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_serialize
 *
 * Purpose:	Flushes a dirty B-tree header to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 1 2005
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *              Converted from H5B2_cache_hdr_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_hdr_serialize(const H5F_t *f, haddr_t UNUSED addr, size_t UNUSED len,
    void *image, void *_thing, unsigned *flags, haddr_t UNUSED *new_addr,
    size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5B2_t *bt2 = (H5B2_t *)_thing;      /* Pointer to the b-tree header */
    H5B2_shared_t *shared;  /* Shared B-tree information */
    uint8_t *p;             /* Pointer into raw data buffer */
    size_t	size;           /* Header size on disk */
    uint32_t metadata_chksum; /* Computed metadata checksum value */

    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_hdr_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(bt2);
    HDassert(flags);

    /* Get the pointer to the shared B-tree info */
    shared = (H5B2_shared_t *)H5RC_GET_OBJ(bt2->shared);
    HDassert(shared);

    /* Compute the size of the serialized B-tree header on disk */
    size = H5B2_HEADER_SIZE(f);

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    HDmemcpy(p, H5B2_HDR_MAGIC, (size_t)H5B2_SIZEOF_MAGIC);
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
    UINT16ENCODE(p, shared->depth);

    /* Split & merge %s */
    H5_CHECK_OVERFLOW(shared->split_percent, /* From: */ unsigned, /* To: */ uint8_t);
    *p++ = (uint8_t)shared->split_percent;
    H5_CHECK_OVERFLOW(shared->merge_percent, /* From: */ unsigned, /* To: */ uint8_t);
    *p++ = (uint8_t)shared->merge_percent;

    /* Root node pointer */
    H5F_addr_encode(f, &p, bt2->root.addr);
    UINT16ENCODE(p, bt2->root.node_nrec);
    H5F_ENCODE_LENGTH(f, p, bt2->root.all_nrec);

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(image, (size - H5B2_SIZEOF_CHKSUM), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_hdr_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_hdr_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_hdr_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_hdr_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy v2 b-tree header */
    H5B2_cache_hdr_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_hdr_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_deserialize
 *
 * Purpose:	Deserialize a B-tree internal node from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree internal node.
 *              Failure:        NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 * Changes:     Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *              Converted from H5B2_cache_internal_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2_cache_internal_deserialize(haddr_t UNUSED addr, size_t UNUSED len,
    const void *image, void *_udata, hbool_t UNUSED *dirty)
{
    H5B2_internal_cache_ud_t *udata = (H5B2_internal_cache_ud_t *)_udata;     /* Pointer to user data */
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    H5B2_internal_t	*internal = NULL;       /* Internal node read */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint8_t		*native;        /* Pointer to native record info */
    H5B2_node_ptr_t	*int_node_ptr;  /* Pointer to node pointer info */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    unsigned		u;              /* Local index variable */
    H5B2_internal_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_internal_deserialize)

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* Allocate new internal node and reset cache info */
    if(NULL == (internal = H5FL_MALLOC(H5B2_internal_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&internal->cache_info, 0, sizeof(H5AC2_info_t));

    /* Share common B-tree information */
    internal->shared = udata->bt2_shared;
    H5RC_INC(internal->shared);

    /* Get the pointer to the shared B-tree info */
    shared=(H5B2_shared_t *)H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5B2_INT_MAGIC, (size_t)H5B2_SIZEOF_MAGIC))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree internal node signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_INT_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree internal node version")

    /* B-tree type */
    if (*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Allocate space for the native keys in memory */
    if((internal->int_native = (uint8_t *)H5FL_FAC_MALLOC(shared->node_info[udata->depth].nat_rec_fac)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree internal native keys")

    /* Allocate space for the node pointers in memory */
    if((internal->node_ptrs = (H5B2_node_ptr_t *)H5FL_FAC_MALLOC(shared->node_info[udata->depth].node_ptr_fac)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree internal node pointers")

    /* Set the number of records in the leaf & it's depth */
    internal->nrec = udata->nrec;
    internal->depth = udata->depth;

    /* Deserialize records for internal node */
    native = internal->int_native;
    for(u = 0; u < internal->nrec; u++) {
        /* Decode record */
        if((shared->type->decode)(udata->f, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode B-tree record")

        /* Move to next record */
        p += shared->rrec_size;
        native += shared->type->nrec_size;
    } /* end for */

    /* Deserialize node pointers for internal node */
    int_node_ptr = internal->node_ptrs;
    for(u = 0; u < internal->nrec + 1; u++) {
        /* Decode node pointer */
        H5F_addr_decode(udata->f, (const uint8_t **)&p, &(int_node_ptr->addr));
        UINT64DECODE_VAR(p, int_node_ptr->node_nrec, shared->max_nrec_size);
        if(udata->depth > 1)
            UINT64DECODE_VAR(p, int_node_ptr->all_nrec, shared->node_info[udata->depth - 1].cum_max_nrec_size)
        else
            int_node_ptr->all_nrec = int_node_ptr->node_nrec;

        /* Move to next node pointer */
        int_node_ptr++;
    } /* end for */

    /* Compute checksum on internal node */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check parsing */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "incorrect metadata checksum for v2 internal node")

    /* Set return value */
    ret_value = internal;

done:
    if(!ret_value && internal)
        (void)H5B2_cache_internal_dest(internal);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_internal_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_serialize
 *
 * Purpose:	Serializes a B-tree internal node for writing to disk.
 *
 * Return:	Success:         SUCCEED
 *              Failure:         FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 3 2005
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *              Converted from H5B2_cache_internal_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_internal_serialize(const H5F_t *f, haddr_t UNUSED addr, 
    size_t UNUSED len, void *image, void *_thing, unsigned *flags,
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5B2_shared_t *shared;  /* Shared B-tree information */
    uint8_t *p;             /* Pointer into raw data buffer */
    uint8_t *native;        /* Pointer to native record info */
    H5B2_node_ptr_t *int_node_ptr;      /* Pointer to node pointer info */
    uint32_t metadata_chksum; /* Computed metadata checksum value */
    unsigned u;             /* Local index variable */
    H5B2_internal_t *internal = (H5B2_internal_t *)_thing;      /* Pointer to the b-tree internal node */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_internal_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(internal);
    HDassert(flags);

    /* Get the pointer to the shared B-tree info */
    shared = (H5B2_shared_t *)H5RC_GET_OBJ(internal->shared);
    HDassert(shared);

    p = image;

    /* Magic number */
    HDmemcpy(p, H5B2_INT_MAGIC, (size_t)H5B2_SIZEOF_MAGIC);
    p += H5B2_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5B2_INT_VERSION;

    /* B-tree type */
    *p++ = shared->type->id;
    HDassert((size_t)(p - (const uint8_t *)image) == (H5B2_INT_PREFIX_SIZE - H5B2_SIZEOF_CHKSUM));

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
        UINT64ENCODE_VAR(p, int_node_ptr->node_nrec, shared->max_nrec_size);
        if(internal->depth > 1)
            UINT64ENCODE_VAR(p, int_node_ptr->all_nrec, shared->node_info[internal->depth - 1].cum_max_nrec_size);

        /* Move to next node pointer */
        int_node_ptr++;
    } /* end for */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_internal_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_internal_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_internal_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_internal_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy v2 b-tree header */
    H5B2_cache_internal_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_internal_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_deserialize
 *
 * Purpose:	Deserialize a B-tree leaf from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree leaf node.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 * Changes:     Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *              Converted from H5B2_cache_leaf_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B2_cache_leaf_deserialize(haddr_t UNUSED addr, size_t UNUSED len,
    const void *image, void *_udata, hbool_t UNUSED *dirty)
{
    H5B2_leaf_cache_ud_t *udata = (H5B2_leaf_cache_ud_t *)_udata;
    H5B2_shared_t 	*shared;        /* Shared B-tree information */
    H5B2_leaf_t		*leaf = NULL;   /* Pointer to lead node loaded */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint8_t		*native;        /* Pointer to native keys */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    unsigned		u;              /* Local index variable */
    H5B2_leaf_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_leaf_deserialize)

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    if(NULL == (leaf = H5FL_MALLOC(H5B2_leaf_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&leaf->cache_info, 0, sizeof(H5AC2_info_t));

    /* Share common B-tree information */
    leaf->shared = udata->bt2_shared;
    H5RC_INC(leaf->shared);

    /* Get the pointer to the shared B-tree info */
    shared = (H5B2_shared_t *)H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5B2_LEAF_MAGIC, (size_t)H5B2_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree leaf node signature")
    p += H5B2_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5B2_LEAF_VERSION)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "wrong B-tree leaf node version")

    /* B-tree type */
    if(*p++ != (uint8_t)shared->type->id)
	HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree type")

    /* Allocate space for the native keys in memory */
    if((leaf->leaf_native = (uint8_t *)H5FL_FAC_MALLOC(shared->node_info[0].nat_rec_fac)) == NULL)
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for B-tree leaf native keys")

    /* Set the number of records in the leaf */
    leaf->nrec = *udata->nrec;

    /* Deserialize records for leaf node */
    native = leaf->leaf_native;
    for(u = 0; u < leaf->nrec; u++) {
        /* Decode record */
        if((shared->type->decode)(udata->f, p, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, NULL, "unable to decode B-tree record")

        /* Move to next record */
        p += shared->rrec_size;
        native += shared->type->nrec_size;
    } /* end for */

    /* Compute checksum on internal node */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check parsing */
    HDassert((size_t)(p - (const uint8_t *)image) <= shared->node_size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "incorrect metadata checksum for v2 leaf node")

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Set return value */
    ret_value = leaf;

done:
    if(!ret_value && leaf)
        (void)H5B2_cache_leaf_dest(leaf);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_leaf_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_serialize
 *
 * Purpose:	Serializes a B-tree leaf node for writing to disk.
 *
 * Return:	Success:                SUCCEED
 *              Failure:                FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 2 2005
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *              Converted from H5B2_cache_leaf_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_leaf_serialize(const H5F_t *f, haddr_t UNUSED addr, size_t UNUSED len,
    void *image, void *_thing, unsigned *flags, haddr_t UNUSED *new_addr,
    size_t UNUSED *new_len, void UNUSED **new_image)
{
    H5B2_shared_t *shared;  /* Shared B-tree information */
    uint8_t *p;             /* Pointer into raw data buffer */
    uint8_t *native;        /* Pointer to native keys */
    uint32_t metadata_chksum; /* Computed metadata checksum value */
    H5B2_leaf_t *leaf = (H5B2_leaf_t *)_thing;      /* Pointer to the b-tree leaf node  */
    unsigned u;             /* Local index variable */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5B2_cache_leaf_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(leaf);
    HDassert(flags);

    /* Get the pointer to the shared B-tree info */
    shared = (H5B2_shared_t *)H5RC_GET_OBJ(leaf->shared);
    HDassert(shared);

    p = image;

    /* magic number */
    HDmemcpy(p, H5B2_LEAF_MAGIC, (size_t)H5B2_SIZEOF_MAGIC);
    p += H5B2_SIZEOF_MAGIC;

    /* version # */
    *p++ = H5B2_LEAF_VERSION;

    /* b-tree type */
    *p++ = shared->type->id;
    HDassert((size_t)(p - (const uint8_t *)image) == (H5B2_LEAF_PREFIX_SIZE - H5B2_SIZEOF_CHKSUM));

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

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5B2_cache_leaf_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5B2_cache_leaf_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              June 18, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B2_cache_leaf_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5B2_cache_leaf_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy v2 b-tree header */
    H5B2_cache_leaf_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5B2_cache_leaf_free_icr() */

