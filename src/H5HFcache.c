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
 * Created:		H5HFcache.c
 *			Feb 24 2006
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Implement fractal heap metadata cache methods.
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
#include "H5WBprivate.h"        /* Wrapped Buffers                      */

/****************/
/* Local Macros */
/****************/

/* Fractal heap format version #'s */
#define H5HF_HDR_VERSION        0               /* Header */
#define H5HF_DBLOCK_VERSION     0               /* Direct block */
#define H5HF_IBLOCK_VERSION     0               /* Indirect block */

/* Size of stack buffer for serialized headers */
#define H5HF_HDR_BUF_SIZE       512

/* Size of stack buffer for serialized indirect blocks */
#define H5HF_IBLOCK_BUF_SIZE    4096


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Local encode/decode routines */
static herr_t H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable);
static herr_t H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable);

/* Metadata cache (H5AC2) callbacks */
static void *H5HF_cache_hdr_deserialize(haddr_t addr, size_t len, 
    const void *image, const void *udata, hbool_t *dirty);
static herr_t H5HF_cache_hdr_serialize(const H5F_t *f, haddr_t addr, size_t len,
    void *image, void *thing, unsigned *flags, haddr_t *new_addr, 
    size_t *new_len, void **new_image);
static herr_t H5HF_cache_hdr_free_icr(haddr_t addr, size_t len, void *thing);
static herr_t H5HF_cache_hdr_image_len(const void *thing, size_t *image_len_ptr);

static void *H5HF_cache_iblock_deserialize(haddr_t addr, size_t len, 
    const void *image, const void *udata, hbool_t *dirty);
static herr_t H5HF_cache_iblock_serialize(const H5F_t * f, haddr_t addr, 
    size_t len, void *image, void *_thing, unsigned *flags, haddr_t *new_addr, 
    size_t *new_len, void **new_image);
static herr_t H5HF_cache_iblock_free_icr(haddr_t addr, size_t len, void *thing);

static H5HF_direct_t *H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *udata, void *udata2);
static herr_t H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock, unsigned UNUSED * flags_ptr);
static herr_t H5HF_cache_dblock_clear(H5F_t *f, H5HF_direct_t *dblock, hbool_t destroy);
static herr_t H5HF_cache_dblock_size(const H5F_t *f, const H5HF_direct_t *dblock, size_t *size_ptr);

/*********************/
/* Package Variables */
/*********************/

/* H5HF header inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_FHEAP_HDR[1] = {{
    H5AC2_FHEAP_HDR_ID,
    "fractal heap header",
    H5FD_MEM_FHEAP_HDR,
    H5HF_cache_hdr_deserialize,
    H5HF_cache_hdr_image_len,
    H5HF_cache_hdr_serialize,
    H5HF_cache_hdr_free_icr,
    NULL,
}};

/* H5HF indirect block inherits cache-like properties from H5AC2 */
const H5AC2_class_t H5AC2_FHEAP_IBLOCK[1] = {{
    H5AC2_FHEAP_IBLOCK_ID,
    "fractal heap indirect block",
    H5FD_MEM_FHEAP_IBLOCK,
    H5HF_cache_iblock_deserialize,
    NULL,
    H5HF_cache_iblock_serialize,
    H5HF_cache_iblock_free_icr,
    NULL,
}};

/* H5HF direct block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FHEAP_DBLOCK[1] = {{
    H5AC_FHEAP_DBLOCK_ID,
    (H5AC_load_func_t)H5HF_cache_dblock_load,
    (H5AC_flush_func_t)H5HF_cache_dblock_flush,
    (H5AC_dest_func_t)H5HF_cache_dblock_dest,
    (H5AC_clear_func_t)H5HF_cache_dblock_clear,
    (H5AC_size_func_t)H5HF_cache_dblock_size,
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage heap direct block data to/from disk */
H5FL_BLK_DEFINE(direct_block);



/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_decode
 *
 * Purpose:	Decodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_decode(H5F_t *f, const uint8_t **pp, H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_decode)

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16DECODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_DECODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16DECODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->cparam.start_root_rows);

    /* Address of table */
    H5F_addr_decode(f, pp, &(dtable->table_addr));

    /* Current # of rows in root indirect block */
    UINT16DECODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_dtable_encode
 *
 * Purpose:	Encodes the metadata for a doubling table
 *
 * Return:	Success:	Pointer to a new fractal heap
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_dtable_encode(H5F_t *f, uint8_t **pp, const H5HF_dtable_t *dtable)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_dtable_encode)

    /* Check arguments */
    HDassert(f);
    HDassert(pp && *pp);
    HDassert(dtable);

    /* Table width */
    UINT16ENCODE(*pp, dtable->cparam.width);

    /* Starting block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.start_block_size);

    /* Maximum direct block size */
    H5F_ENCODE_LENGTH(f, *pp, dtable->cparam.max_direct_size);

    /* Maximum heap size (as # of bits) */
    UINT16ENCODE(*pp, dtable->cparam.max_index);

    /* Starting # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->cparam.start_root_rows);

    /* Address of root direct/indirect block */
    H5F_addr_encode(f, pp, dtable->table_addr);

    /* Current # of rows in root indirect block */
    UINT16ENCODE(*pp, dtable->curr_root_rows);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_dtable_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_deserialize
 *
 * Purpose:	Deserialize the data structure from disk.
 *
 * Return:	Success:	SUCCESS
 *		Failure:	FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 *
 * Changes:     Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 *              Converted from H5HF_cache_hdr_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HF_cache_hdr_deserialize(haddr_t addr, size_t UNUSED len, 
    const void *image, const void *_udata, hbool_t UNUSED *dirty)
{
    H5HF_hdr_t		*hdr = NULL;     /* Fractal heap info */
    H5HF_hdr_cache_ud_t *udata = (H5HF_hdr_cache_ud_t *)_udata;
    size_t		size;           /* Header size */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    uint8_t             heap_flags;     /* Status flags for heap */
    H5HF_hdr_t		*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_deserialize)

    /* Check arguments */
    HDassert(image);

    /* Allocate space for the fractal heap data structure */
    if(NULL == (hdr = H5HF_hdr_alloc(udata->f)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set the heap header's address */
    hdr->heap_addr = addr;

    /* Compute the 'base' size of the fractal heap header on disk */
    size = H5HF_HEADER_SIZE(hdr);

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap header signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_HDR_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap header version")

    /* General heap information */
    UINT16DECODE(p, hdr->id_len);               /* Heap ID length */
    UINT16DECODE(p, hdr->filter_len);           /* I/O filters' encoded length */

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    /* (bit 1: checksum direct blocks) */
    heap_flags = *p++;
    hdr->huge_ids_wrapped = heap_flags & H5HF_HDR_FLAGS_HUGE_ID_WRAPPED;
    hdr->checksum_dblocks = heap_flags & H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS;

    /* "Huge" object information */
    UINT32DECODE(p, hdr->max_man_size);         /* Max. size of "managed" objects */
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_next_id); /* Next ID to use for "huge" object */
    H5F_addr_decode(udata->f, &p, &hdr->huge_bt2_addr); /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_DECODE_LENGTH(udata->f, p, hdr->total_man_free); /* Internal free space in managed direct blocks */
    H5F_addr_decode(udata->f, &p, &hdr->fs_addr);      /* Address of free section header */

    /* Heap statistics */
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_alloc_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_iter_off);
    H5F_DECODE_LENGTH(udata->f, p, hdr->man_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->huge_nobjs);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_size);
    H5F_DECODE_LENGTH(udata->f, p, hdr->tiny_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_decode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, NULL, "unable to encode managed obj. doubling table info")

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(p - (const uint8_t *)image) == (size - H5HF_SIZEOF_CHKSUM));

    /* Check for I/O filter information to decode */
    if(hdr->filter_len > 0) {

        size_t filter_info_off;     /* Offset in header of filter information */
        size_t filter_info_size;    /* Size of filter information */
        H5O_pline_t *pline;         /* Pipeline information from the header on disk */

        /* Compute the offset of the filter info in the header */
        filter_info_off = p - (const uint8_t *)image;

        /* Compute the size of the extra filter information */
        filter_info_size = hdr->sizeof_size     /* Size of size for filtered root direct block */
            + 4                                 /* Size of filter mask for filtered root direct block */
            + hdr->filter_len;                  /* Size of encoded I/O filter info */

        /* Compute the heap header's size */
        hdr->heap_size = size + filter_info_size;

        /* Point at correct offset in header for the filter information */
        p = image + filter_info_off;

        /* Decode the size of a filtered root direct block */
        H5F_DECODE_LENGTH(udata->f, p, hdr->pline_root_direct_size);

        /* Decode the filter mask for a filtered root direct block */
        UINT32DECODE(p, hdr->pline_root_direct_filter_mask);

        /* Decode I/O filter information */
        if(NULL == (pline = (H5O_pline_t *)H5O_msg_decode(hdr->f, udata->dxpl_id, H5O_PLINE_ID, p)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDECODE, NULL, "can't decode I/O pipeline filters")
        p += hdr->filter_len;

        /* Copy the information into the header's I/O pipeline structure */
        if(NULL == H5O_msg_copy(H5O_PLINE_ID, pline, &(hdr->pline)))
            HGOTO_ERROR(H5E_HEAP, H5E_CANTCOPY, NULL, "can't copy I/O filter pipeline")

        /* Release the space allocated for the I/O pipeline filters */
        H5O_msg_free(H5O_PLINE_ID, pline);
    } /* end if */
    else 
        /* Set the heap header's size */
        hdr->heap_size = size;

    /* Compute checksum on entire header */
    /* (including the filter information, if present) */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == hdr->heap_size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap header")

    /* Finish initialization of heap header */
    if(H5HF_hdr_finish_init(hdr) < 0)
	HGOTO_ERROR(H5E_RESOURCE, H5E_CANTINIT, NULL, "can't finish initializing shared fractal heap header")
#ifdef QAK
HDfprintf(stderr, "%s: hdr->fspace = %p\n", FUNC, hdr->fspace);
#endif /* QAK */

    /* Set return value */
    ret_value = hdr;

done:
    /* Release resources */
    if(!ret_value && hdr)
        (void)H5HF_cache_hdr_dest(hdr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_hdr_deserialize() */ /*lint !e818 Can't make udata a pointer to const */


/*-------------------------------------------------------------------------
 * Function:    H5HF_cache_hdr_image_len
 *
 * Purpose:     Tell the metadata cache about the actual size 
 *              of the fractal heap header
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_image_len(const void *thing, size_t *image_len_ptr)
{

    const H5HF_hdr_t    *hdr = (const H5HF_hdr_t *)thing;    /* Fractal heap header */
    
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_image_len)

    /* Check arguments */
    HDassert(hdr);
    HDassert(image_len_ptr);
    
    /* Report the fractal heap header's prefix + I/O filter length */
    *image_len_ptr = H5HF_HEADER_SIZE(hdr) + hdr->filter_len;

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5HF_cache_hdr_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_serialize
 *
 * Purpose:	Serialize the data structure for writing to disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 24 2006
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 *              Converted from H5HF_cache_hdr_flush
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_serialize(const H5F_t *f, haddr_t UNUSED addr, size_t UNUSED len,
    void *image, void *_thing, unsigned *flags, haddr_t UNUSED *new_addr,
    size_t UNUSED *new_len, void UNUSED **new_image)
{
    herr_t ret_value = SUCCEED;         /* Return value */
    uint8_t *p;             /* Pointer into raw data buffer */
    size_t	size;           /* Header size on disk */
    uint8_t heap_flags;     /* Status flags for heap */
    uint32_t metadata_chksum; /* Computed metadata checksum value */
    H5HF_hdr_t *hdr = (H5HF_hdr_t *)_thing; /* fractal heap header */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_hdr_serialize)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(hdr);
    HDassert(image);
    HDassert(flags);

    /* Sanity check */
    HDassert(hdr->dirty);

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    /* Compute the size of the heap header on disk */
    size = hdr->heap_size;

    /* Get temporary pointer to serialized header */
    p = image;

    /* Magic number */
    HDmemcpy(p, H5HF_HDR_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
    p += H5HF_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5HF_HDR_VERSION;

    /* General heap information */
    UINT16ENCODE(p, hdr->id_len);           /* Heap ID length */
    UINT16ENCODE(p, hdr->filter_len);       /* I/O filters' encoded length */

    /* Heap status flags */
    /* (bit 0: "huge" object IDs have wrapped) */
    /* (bit 1: checksum direct blocks) */
    heap_flags = 0;
    heap_flags |= (hdr->huge_ids_wrapped ?  H5HF_HDR_FLAGS_HUGE_ID_WRAPPED : 0);
    heap_flags |= (hdr->checksum_dblocks ?  H5HF_HDR_FLAGS_CHECKSUM_DBLOCKS : 0);
    *p++ = heap_flags;

    /* "Huge" object information */
    UINT32ENCODE(p, hdr->max_man_size);             /* Max. size of "managed" objects */
    H5F_ENCODE_LENGTH(f, p, hdr->huge_next_id);     /* Next ID to use for "huge" object */
    H5F_addr_encode(f, &p, hdr->huge_bt2_addr);     /* Address of "huge" object tracker B-tree */

    /* "Managed" object free space information */
    H5F_ENCODE_LENGTH(f, p, hdr->total_man_free);   /* Internal free space in managed direct blocks */
    H5F_addr_encode(f, &p, hdr->fs_addr);           /* Address of free section header */

    /* Heap statistics */
    H5F_ENCODE_LENGTH(f, p, hdr->man_size);
    H5F_ENCODE_LENGTH(f, p, hdr->man_alloc_size);
    H5F_ENCODE_LENGTH(f, p, hdr->man_iter_off);
    H5F_ENCODE_LENGTH(f, p, hdr->man_nobjs);
    H5F_ENCODE_LENGTH(f, p, hdr->huge_size);
    H5F_ENCODE_LENGTH(f, p, hdr->huge_nobjs);
    H5F_ENCODE_LENGTH(f, p, hdr->tiny_size);
    H5F_ENCODE_LENGTH(f, p, hdr->tiny_nobjs);

    /* Managed objects' doubling-table info */
    if(H5HF_dtable_encode(hdr->f, &p, &(hdr->man_dtable)) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "unable to encode managed obj. doubling table info")

    /* Check for I/O filter information to encode */
    if(hdr->filter_len > 0) {
        /* Encode the size of a filtered root direct block */
        H5F_ENCODE_LENGTH(f, p, hdr->pline_root_direct_size);

        /* Encode the filter mask for a filtered root direct block */
        UINT32ENCODE(p, hdr->pline_root_direct_filter_mask);

        /* Encode I/O filter information */
        if(H5O_msg_encode(hdr->f, H5O_PLINE_ID, FALSE, p, &(hdr->pline)) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTENCODE, FAIL, "can't encode I/O pipeline fiters")
            p += hdr->filter_len;
    } /* end if */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == size);

    /* Reset the cache flags for this operation (metadata not resized or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_hdr_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_hdr_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data structure
 *
 * Return:	Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:	Mike McGreevy
 *		mcgreevy@hdfgroup.org
 *		July 25, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_hdr_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_hdr_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy B-tree node */
    H5HF_cache_hdr_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5HF_cache_hdr_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_deserialize
 *
 * Purpose:	Loads a fractal heap indirect block from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap indirect block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July  25, 2008
 *              Converted from H5HF_cache_iblock_load
 *
 *-------------------------------------------------------------------------
 */
static void *
H5HF_cache_iblock_deserialize(haddr_t UNUSED addr, size_t UNUSED len, 
    const void *image, const void *_udata, hbool_t UNUSED *dirty)
{
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_iblock_cache_ud_t *udata = (H5HF_iblock_cache_ud_t *)_udata; /* user data for callback */
    H5HF_indirect_t	*iblock = NULL; /* Indirect block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */
    uint32_t            computed_chksum; /* Computed metadata checksum value */
    size_t              u;              /* Local index variable */
    H5HF_indirect_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_deserialize)
#ifdef QAK
HDfprintf(stderr, "%s: Load indirect block, addr = %a\n", FUNC, addr);
#endif /* QAK */

    /* Check arguments */
    HDassert(image);

    /* Allocate space for the fractal heap indirect block */
    if(NULL == (iblock = H5FL_CALLOC(H5HF_indirect_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&iblock->cache_info, 0, sizeof(H5AC2_info_t));

    /* Get the pointer to the shared heap header */
    hdr = udata->par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = udata->f;

    /* Share common heap information */
    iblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    iblock->rc = 0;
    iblock->nrows = *udata->nrows;
    iblock->addr = addr;
    iblock->nchildren = 0;

    /* Compute size of indirect block */
    iblock->size = H5HF_MAN_INDIRECT_SIZE(hdr, iblock);
    /* Get temporary pointer to serialized indirect block */
    p = image;

    /* Magic number */
    if(HDmemcmp(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap indirect block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_IBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block */
    H5F_addr_decode(udata->f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    iblock->parent = udata->par_info->iblock;
    iblock->par_entry = udata->par_info->entry;
    if(iblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(iblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")

        /* Set max. # of rows in this block */
        iblock->max_rows = iblock->nrows;
    } /* end if */
    else {
        /* Set max. # of rows in this block */
        iblock->max_rows = hdr->man_dtable.max_root_rows;
    } /* end else */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, iblock->block_off, hdr->heap_off_size);

    /* Allocate & decode child block entry tables */
    HDassert(iblock->nrows > 0);
    if(NULL == (iblock->ents = H5FL_SEQ_MALLOC(H5HF_indirect_ent_t, (size_t)(iblock->nrows * hdr->man_dtable.cparam.width))))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for direct entries")
    if(hdr->filter_len > 0) {
        unsigned dir_rows;      /* Number of direct rows in this indirect block */

        /* Compute the number of direct rows for this indirect block */
        dir_rows = MIN(iblock->nrows, hdr->man_dtable.max_direct_rows);

        /* Allocate indirect block filtered entry array */
        if(NULL == (iblock->filt_ents = H5FL_SEQ_MALLOC(H5HF_indirect_filt_ent_t, (size_t)(dir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->filt_ents = NULL;
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
        /* Decode child block address */
        H5F_addr_decode(udata->f, &p, &(iblock->ents[u].addr));

        /* Check for heap with I/O filters */
        if(hdr->filter_len > 0) {
            /* Sanity check */
            HDassert(iblock->filt_ents);

            /* Decode extra information for direct blocks */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
                /* Size of filtered direct block */
                H5F_DECODE_LENGTH(udata->f, p, iblock->filt_ents[u].size);

                /* Sanity check */
                /* (either both the address & size are defined or both are
                 *  not defined)
                 */
                HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                    || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                /* I/O filter mask for filtered direct block */
                UINT32DECODE(p, iblock->filt_ents[u].filter_mask);
            } /* end if */
        } /* end if */

        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            iblock->nchildren++;
            iblock->max_child = u;
        } /* end if */
#ifdef QAK
HDfprintf(stderr, "%s: iblock->ents[%Zu] = {%a}\n", FUNC, u, iblock->ents[u].addr);
#endif /* QAK */
    } /* end for */

    /* Sanity check */
    HDassert(iblock->nchildren);        /* indirect blocks w/no children should have been deleted */

    /* Compute checksum on indirect block */
    computed_chksum = H5_checksum_metadata(image, (size_t)(p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32DECODE(p, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == iblock->size);

    /* Verify checksum */
    if(stored_chksum != computed_chksum)
	HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap indirect block")

    /* Check if we have any indirect block children */
    if(iblock->nrows > hdr->man_dtable.max_direct_rows) {
        unsigned indir_rows;      /* Number of indirect rows in this indirect block */

        /* Compute the number of indirect rows for this indirect block */
        indir_rows = iblock->nrows - hdr->man_dtable.max_direct_rows;

        /* Allocate & initialize child indirect block pointer array */
        if(NULL == (iblock->child_iblocks = H5FL_SEQ_CALLOC(H5HF_indirect_ptr_t, (size_t)(indir_rows * hdr->man_dtable.cparam.width))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for block entries")
    } /* end if */
    else
        iblock->child_iblocks = NULL;

    /* Set return value */
    ret_value = iblock;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);
done:
    /* Release resources */
    if(!ret_value && iblock)
        (void)H5HF_cache_iblock_dest(iblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_iblock_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_iblock_serialize
 *
 * Purpose:	Flushes a dirty fractal heap indirect block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Mar  6 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *              Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July  25, 2008
 *              Converted from H5HF_cache_iblock_flush
 * 
 *-------------------------------------------------------------------------
 */
static herr_t

H5HF_cache_iblock_serialize(const H5F_t *f, haddr_t UNUSED addr, 
    size_t UNUSED len, void *image, void *_thing, unsigned UNUSED *flags, 
    haddr_t UNUSED *new_addr, size_t UNUSED *new_len, 
    void UNUSED **new_image)
{
    herr_t      ret_value = SUCCEED;    /* Return value */
    H5HF_hdr_t *hdr;                /* Shared fractal heap information */
    H5HF_indirect_t *iblock = (H5HF_indirect_t *)_thing;
    uint8_t *p;                     /* Pointer into raw data buffer */
#ifndef NDEBUG
    unsigned nchildren = 0;         /* Track # of children */
    unsigned max_child = 0;         /* Track max. child entry used */
#endif /* NDEBUG */
    uint32_t metadata_chksum;       /* Computed metadata checksum value */
    size_t u;                       /* Local index variable */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_iblock_serialize)
#ifdef QAK
HDfprintf(stderr, "%s: Flushing indirect block, addr = %a, destroy = %u\n", FUNC, addr, (unsigned)destroy);
#endif /* QAK */

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(iblock);

    /* Get the pointer to the shared heap header */
    hdr = iblock->hdr;
#ifdef QAK
HDfprintf(stderr, "%s: iblock->nrows = %u\n", FUNC, iblock->nrows);
HDfprintf(stderr, "%s: iblock->size = %Zu\n", FUNC, iblock->size);
HDfprintf(stderr, "%s: iblock->block_off = %Hu\n", FUNC, iblock->block_off);
HDfprintf(stderr, "%s: hdr->man_dtable.cparam.width = %u\n", FUNC, hdr->man_dtable.cparam.width);
#endif /* QAK */

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    /* Get temporary pointer to buffer for serialized indirect block */
    p = image;

    /* Magic number */
    HDmemcpy(p, H5HF_IBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
    p += H5HF_SIZEOF_MAGIC;

    /* Version # */
    *p++ = H5HF_IBLOCK_VERSION;

    /* Address of heap header for heap which owns this block */
    H5F_addr_encode(f, &p, hdr->heap_addr);

    /* Offset of block in heap */
    UINT64ENCODE_VAR(p, iblock->block_off, hdr->heap_off_size);

    /* Encode indirect block-specific fields */
    for(u = 0; u < (iblock->nrows * hdr->man_dtable.cparam.width); u++) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->ents[%Zu] = {%a}\n", FUNC, u, iblock->ents[u].addr);
#endif /* QAK */
        /* Encode child block address */
        H5F_addr_encode(f, &p, iblock->ents[u].addr);

        /* Check for heap with I/O filters */
        if(hdr->filter_len > 0) {
            /* Sanity check */
            HDassert(iblock->filt_ents);

            /* Encode extra information for direct blocks */
            if(u < (hdr->man_dtable.max_direct_rows * hdr->man_dtable.cparam.width)) {
#ifdef QAK
HDfprintf(stderr, "%s: iblock->filt_ents[%Zu] = {%Zu, %x}\n", FUNC, u, iblock->filt_ents[u].size, iblock->filt_ents[u].filter_mask);
#endif /* QAK */
                /* Sanity check */
                /* (either both the address & size are defined or both are
                 *  not defined)
                 */
                HDassert((H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size)
                    || (!H5F_addr_defined(iblock->ents[u].addr) && iblock->filt_ents[u].size == 0));

                /* Size of filtered direct block */
                H5F_ENCODE_LENGTH(f, p, iblock->filt_ents[u].size);

                /* I/O filter mask for filtered direct block */
                UINT32ENCODE(p, iblock->filt_ents[u].filter_mask);
            } /* end if */
        } /* end if */

#ifndef NDEBUG
        /* Count child blocks */
        if(H5F_addr_defined(iblock->ents[u].addr)) {
            nchildren++;
            if(u > max_child)
                max_child = u;
        } /* end if */
#endif /* NDEBUG */
    } /* end for */

    /* Compute checksum */
    metadata_chksum = H5_checksum_metadata(image, (size_t)((const uint8_t *)p - (const uint8_t *)image), 0);

    /* Metadata checksum */
    UINT32ENCODE(p, metadata_chksum);

    /* Reset the cache flags for this operation (metadata not resized or renamed) */
    *flags = 0;

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)p - (const uint8_t *)image) <= len);

    /* Sanity check */
    HDassert((size_t)(p - (const uint8_t *)image) == iblock->size);
#ifndef NDEBUG
    HDassert(nchildren == iblock->nchildren);
    HDassert(max_child == iblock->max_child);
#endif /* NDEBUG */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_iblock_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5HF_cache_iblock_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Mike McGreevy
 *              mcgreevy@hdfgroup.org
 *              July 25, 2008
 * 
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_iblock_free_icr(haddr_t UNUSED addr, size_t UNUSED len, void *thing)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_iblock_free_icr)

    /* Check arguments */
    HDassert(thing);

    /* Destroy fractal heap indirect block */
    H5HF_cache_iblock_dest(thing);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_iblock_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_load
 *
 * Purpose:	Loads a fractal heap direct block from the disk.
 *
 * Return:	Success:	Pointer to a new fractal heap direct block
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static H5HF_direct_t *
H5HF_cache_dblock_load(H5F_t *f, hid_t dxpl_id, haddr_t addr, const void *_size, void *_par_info)
{
    const size_t        *size = (const size_t *)_size;         /* Size of block */
    H5HF_hdr_t          *hdr;           /* Shared fractal heap information */
    H5HF_parent_t       *par_info = (H5HF_parent_t *)_par_info; /* Pointer to parent information */
    H5HF_direct_t	*dblock = NULL; /* Direct block info */
    const uint8_t	*p;             /* Pointer into raw data buffer */
    haddr_t             heap_addr;      /* Address of heap header in the file */
    H5HF_direct_t	*ret_value;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_load)

    /* Check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(par_info);

    /* Allocate space for the fractal heap direct block */
    if(NULL == (dblock = H5FL_MALLOC(H5HF_direct_t)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDmemset(&dblock->cache_info, 0, sizeof(H5AC_info_t));

    /* Get the pointer to the shared heap header */
    hdr = par_info->hdr;

    /* Set the shared heap header's file context for this operation */
    hdr->f = f;

    /* Share common heap information */
    dblock->hdr = hdr;
    if(H5HF_hdr_incr(hdr) < 0)
	HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared heap header")

    /* Set block's internal information */
    dblock->size = *size;
    dblock->blk_off_size = H5HF_SIZEOF_OFFSET_LEN(dblock->size);

    /* Allocate block buffer */
/* XXX: Change to using free-list factories */
    if((dblock->blk = H5FL_BLK_MALLOC(direct_block, (size_t)dblock->size)) == NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Check for I/O filters on this heap */
    if(hdr->filter_len > 0) {
        H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
        size_t nbytes;          /* Number of bytes used in buffer, after applying reverse filters */
        void *read_buf;         /* Pointer to buffer to read in */
        size_t read_size;       /* Size of filtered direct block to read */
        unsigned filter_mask;	/* Excluded filters for direct block */

        /* Check for root direct block */
        if(par_info->iblock == NULL) {
#ifdef QAK
HDfprintf(stderr, "%s: hdr->pline_root_direct_size = %Zu, hdr->pline_root_direct_filter_mask = %x\n", FUNC, hdr->pline_root_direct_size, hdr->pline_root_direct_filter_mask);
HDfprintf(stderr, "%s: hdr->man_dtable.table_addr = %a, addr = %a\n", FUNC, hdr->man_dtable.table_addr, addr);
#endif /* QAK */
            /* Sanity check */
            HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));

            /* Set up parameters to read filtered direct block */
            read_size = hdr->pline_root_direct_size;
            filter_mask = hdr->pline_root_direct_filter_mask;
        } /* end if */
        else {
#ifdef QAK
HDfprintf(stderr, "%s: par_info->iblock = %p, par_info->entry = %u\n", FUNC, par_info->iblock, par_info->entry);
HDfprintf(stderr, "%s: par_info->iblock->filt_ents[%u].size = %Zu, par_info->iblock->filt_ents[%u].filter_mask = %x\n", FUNC, par_info->entry, par_info->iblock->filt_ents[par_info->entry].size, par_info->entry, par_info->iblock->filt_ents[par_info->entry].filter_mask);
HDfprintf(stderr, "%s: par_info->iblock->ents[%u].addr = %a, addr = %a\n", FUNC, par_info->entry, par_info->iblock->ents[par_info->entry].addr, addr);
#endif /* QAK */
            /* Sanity check */
            HDassert(H5F_addr_eq(par_info->iblock->ents[par_info->entry].addr, addr));

            /* Set up parameters to read filtered direct block */
            read_size = par_info->iblock->filt_ents[par_info->entry].size;
            filter_mask = par_info->iblock->filt_ents[par_info->entry].filter_mask;
        } /* end else */

        /* Allocate buffer to perform I/O filtering on */
        if(NULL == (read_buf = H5MM_malloc(read_size)))
            HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, NULL, "memory allocation failed for pipeline buffer")
#ifdef QAK
HDfprintf(stderr, "%s: read_size = %Zu, read_buf = %p\n", FUNC, read_size, read_buf);
#endif /* QAK */

        /* Read filtered direct block from disk */
        if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, read_size, dxpl_id, read_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")

        /* Push direct block data through I/O filter pipeline */
        nbytes = read_size;
        if(H5Z_pipeline(&(hdr->pline), H5Z_FLAG_REVERSE, &filter_mask, H5Z_ENABLE_EDC,
                 filter_cb, &nbytes, &read_size, &read_buf) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTFILTER, NULL, "output pipeline failed")
#ifdef QAK
HDfprintf(stderr, "%s: nbytes = %Zu, read_size = %Zu, read_buf = %p\n", FUNC, nbytes, read_size, read_buf);
#endif /* QAK */

        /* Sanity check */
        HDassert(nbytes == dblock->size);

        /* Copy un-filtered data into block's buffer */
        HDmemcpy(dblock->blk, read_buf, dblock->size);

        /* Release the read buffer */
        H5MM_xfree(read_buf);
    } /* end if */
    else {
        /* Read direct block from disk */
        if(H5F_block_read(f, H5FD_MEM_FHEAP_DBLOCK, addr, dblock->size, dxpl_id, dblock->blk) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_READERROR, NULL, "can't read fractal heap direct block")
    } /* end else */

    /* Start decoding direct block */
    p = dblock->blk;

    /* Magic number */
    if(HDmemcmp(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "wrong fractal heap direct block signature")
    p += H5HF_SIZEOF_MAGIC;

    /* Version */
    if(*p++ != H5HF_DBLOCK_VERSION)
	HGOTO_ERROR(H5E_HEAP, H5E_VERSION, NULL, "wrong fractal heap direct block version")

    /* Address of heap that owns this block (just for file integrity checks) */
    H5F_addr_decode(f, &p, &heap_addr);
    if(H5F_addr_ne(heap_addr, hdr->heap_addr))
	HGOTO_ERROR(H5E_HEAP, H5E_CANTLOAD, NULL, "incorrect heap header address for direct block")

    /* Address of parent block */
    dblock->parent = par_info->iblock;
    dblock->par_entry = par_info->entry;
    if(dblock->parent) {
        /* Share parent block */
        if(H5HF_iblock_incr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTINC, NULL, "can't increment reference count on shared indirect block")
    } /* end if */

    /* Offset of heap within the heap's address space */
    UINT64DECODE_VAR(p, dblock->block_off, hdr->heap_off_size);

    /* Encode checksum on direct block, if requested */
    if(hdr->checksum_dblocks) {
        uint32_t stored_chksum;         /* Metadata checksum value */
        uint32_t computed_chksum;       /* Computed metadata checksum value */

        /* Metadata checksum */
        UINT32DECODE(p, stored_chksum);

        /* Reset checksum field, for computing the checksum */
        /* (Casting away const OK - QAK) */
        HDmemset((uint8_t *)p - H5HF_SIZEOF_CHKSUM, 0, (size_t)H5HF_SIZEOF_CHKSUM);

        /* Compute checksum on entire direct block */
        computed_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

        /* Verify checksum */
        if(stored_chksum != computed_chksum)
            HGOTO_ERROR(H5E_HEAP, H5E_BADVALUE, NULL, "incorrect metadata checksum for fractal heap direct block")
    } /* end if */

    /* Sanity check */
    HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

    /* Set return value */
    ret_value = dblock;

done:
    if(!ret_value && dblock)
        (void)H5HF_cache_dblock_dest(f, dblock);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_load() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_flush
 *
 * Purpose:	Flushes a dirty fractal heap direct block to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 * Changes:     JRM -- 8/21/06
 *              Added the flags_ptr parameter.  This parameter exists to
 *              allow the flush routine to report to the cache if the
 *              entry is resized or renamed as a result of the flush.
 *              *flags_ptr is set to H5C_CALLBACK__NO_FLAGS_SET on entry.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_flush(H5F_t *f, hid_t dxpl_id, hbool_t destroy, haddr_t addr, H5HF_direct_t *dblock, unsigned UNUSED * flags_ptr)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_flush)

    /* check arguments */
    HDassert(f);
    HDassert(H5F_addr_defined(addr));
    HDassert(dblock);

    if(dblock->cache_info.is_dirty) {
        H5HF_hdr_t *hdr;        /* Shared fractal heap information */
        void *write_buf;        /* Pointer to buffer to write out */
        size_t write_size;      /* Size of buffer to write out */
        uint8_t *p;             /* Pointer into raw data buffer */

        /* Get the pointer to the shared heap header */
        hdr = dblock->hdr;

        /* Set the shared heap header's file context for this operation */
        hdr->f = f;

        HDassert(dblock->blk);
        p = dblock->blk;

        /* Magic number */
        HDmemcpy(p, H5HF_DBLOCK_MAGIC, (size_t)H5HF_SIZEOF_MAGIC);
        p += H5HF_SIZEOF_MAGIC;

        /* Version # */
        *p++ = H5HF_DBLOCK_VERSION;

        /* Address of heap header for heap which owns this block */
        H5F_addr_encode(f, &p, hdr->heap_addr);

        /* Offset of block in heap */
        UINT64ENCODE_VAR(p, dblock->block_off, hdr->heap_off_size);

        /* Metadata checksum */
        if(hdr->checksum_dblocks) {
            uint32_t metadata_chksum;       /* Computed metadata checksum value */

            /* Clear the checksum field, to compute the checksum */
            HDmemset(p, 0, (size_t)H5HF_SIZEOF_CHKSUM);

            /* Compute checksum on entire direct block */
            metadata_chksum = H5_checksum_metadata(dblock->blk, dblock->size, 0);

            /* Metadata checksum */
            UINT32ENCODE(p, metadata_chksum);
        } /* end if */

        /* Sanity check */
        HDassert((size_t)(p - dblock->blk) == H5HF_MAN_ABS_DIRECT_OVERHEAD(hdr));

        /* Check for I/O filters on this heap */
        if(hdr->filter_len > 0) {
            H5Z_cb_t filter_cb = {NULL, NULL};  /* Filter callback structure */
            size_t nbytes;                      /* Number of bytes used */
            unsigned filter_mask;               /* Filter mask for block */

            /* Allocate buffer to perform I/O filtering on */
            write_size = dblock->size;
            if(NULL == (write_buf = H5MM_malloc(write_size)))
                HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "memory allocation failed for pipeline buffer")
            HDmemcpy(write_buf, dblock->blk, write_size);

            /* Push direct block data through I/O filter pipeline */
            nbytes = write_size;
            if(H5Z_pipeline(&(hdr->pline), 0, &filter_mask, H5Z_ENABLE_EDC,
                     filter_cb, &nbytes, &write_size, &write_buf) < 0)
                HGOTO_ERROR(H5E_HEAP, H5E_WRITEERROR, FAIL, "output pipeline failed")
#ifdef QAK
HDfprintf(stderr, "%s: nbytes = %Zu, write_size = %Zu, write_buf = %p\n", FUNC, nbytes, write_size, write_buf);
HDfprintf(stderr, "%s: dblock->block_off = %Hu\n", FUNC, dblock->block_off);
HDfprintf(stderr, "%s: dblock->size = %Zu, dblock->blk = %p\n", FUNC, dblock->size, dblock->blk);
HDfprintf(stderr, "%s: dblock->parent = %p, dblock->par_entry = %u\n", FUNC, dblock->parent, dblock->par_entry);
#endif /* QAK */

            /* Use the compressed number of bytes as the size to write */
            write_size = nbytes;

            /* Check for root direct block */
            if(dblock->parent == NULL) {
                hbool_t hdr_changed = FALSE;    /* Whether the header information changed */

#ifdef QAK
HDfprintf(stderr, "%s: hdr->pline_root_direct_size = %Zu, hdr->pline_root_direct_filter_mask = %x\n", FUNC, hdr->pline_root_direct_size, hdr->pline_root_direct_filter_mask);
HDfprintf(stderr, "%s: hdr->man_dtable.table_addr = %a, addr = %a\n", FUNC, hdr->man_dtable.table_addr, addr);
#endif /* QAK */
                /* Sanity check */
                HDassert(H5F_addr_eq(hdr->man_dtable.table_addr, addr));
                HDassert(hdr->pline_root_direct_size > 0);

                /* Check if the filter mask changed */
                if(hdr->pline_root_direct_filter_mask != filter_mask) {
                    hdr->pline_root_direct_filter_mask = filter_mask;
                    hdr_changed = TRUE;
                } /* end if */

                /* Check if we need to re-size the block on disk */
                if(hdr->pline_root_direct_size != write_size) {
#ifdef QAK
HDfprintf(stderr, "%s: Need to re-allocate root direct block!\n", FUNC);
#endif /* QAK */
                    /* Release direct block's current disk space */
                    if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)hdr->pline_root_direct_size) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

                    /* Allocate space for the compressed direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Let the metadata cache know, if the block moved */
                    if(!H5F_addr_eq(hdr->man_dtable.table_addr, addr))
                        if(H5AC_rename(f, H5AC_FHEAP_DBLOCK, hdr->man_dtable.table_addr, addr) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTRENAME, FAIL, "unable to move direct block")

                    /* Update information about compressed direct block's location & size */
                    hdr->man_dtable.table_addr = addr;
                    hdr->pline_root_direct_size = write_size;

                    /* Note that heap header was modified */
                    hdr_changed = TRUE;
                } /* end if */

                /* Check if heap header was modified */
                if(hdr_changed)
                    if(H5HF_hdr_dirty(hdr) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end if */
            else {
                hbool_t par_changed = FALSE;    /* Whether the parent's information changed */
                H5HF_indirect_t *par_iblock;    /* Parent indirect block */
                unsigned par_entry;             /* Entry in parent indirect block */

                /* Get parent information */
                par_iblock = dblock->parent;
                par_entry = dblock->par_entry;

#ifdef QAK
HDfprintf(stderr, "%s: par_iblock->filt_ents[%u].size = %Zu, par_iblock->filt_ents[%u].filter_mask = %x\n", FUNC, par_entry, par_iblock->filt_ents[par_entry].size, par_entry, par_iblock->filt_ents[par_entry].filter_mask);
HDfprintf(stderr, "%s: par_iblock->ents[%u].addr = %a, addr = %a\n", FUNC, par_entry, par_iblock->ents[par_entry].addr, addr);
#endif /* QAK */
                /* Sanity check */
                HDassert(H5F_addr_eq(par_iblock->ents[par_entry].addr, addr));
                HDassert(par_iblock->filt_ents[par_entry].size > 0);

                /* Check if the filter mask changed */
                if(par_iblock->filt_ents[par_entry].filter_mask != filter_mask) {
                    par_iblock->filt_ents[par_entry].filter_mask = filter_mask;
                    par_changed = TRUE;
                } /* end if */

                /* Check if we need to re-size the block on disk */
                if(par_iblock->filt_ents[par_entry].size != write_size) {
#ifdef QAK
HDfprintf(stderr, "%s: Need to re-allocate non-root direct block!\n", FUNC);
#endif /* QAK */
                    /* Release direct block's current disk space */
                    if(H5MF_xfree(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, addr, (hsize_t)par_iblock->filt_ents[par_entry].size) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to free fractal heap direct block")

                    /* Allocate space for the compressed direct block */
                    if(HADDR_UNDEF == (addr = H5MF_alloc(f, H5FD_MEM_FHEAP_DBLOCK, dxpl_id, (hsize_t)write_size)))
                        HGOTO_ERROR(H5E_HEAP, H5E_NOSPACE, FAIL, "file allocation failed for fractal heap direct block")

                    /* Let the metadata cache know, if the block moved */
                    if(!H5F_addr_eq(par_iblock->ents[par_entry].addr, addr))
                        if(H5AC_rename(f, H5AC_FHEAP_DBLOCK, par_iblock->ents[par_entry].addr, addr) < 0)
                            HGOTO_ERROR(H5E_HEAP, H5E_CANTRENAME, FAIL, "unable to move direct block")

                    /* Update information about compressed direct block's location & size */
                    par_iblock->ents[par_entry].addr = addr;
                    par_iblock->filt_ents[par_entry].size = write_size;

                    /* Note that parent was modified */
                    par_changed = TRUE;
                } /* end if */

                /* Check if parent was modified */
                if(par_changed)
                    if(H5HF_iblock_dirty(par_iblock) < 0)
                        HGOTO_ERROR(H5E_HEAP, H5E_CANTDIRTY, FAIL, "can't mark heap header as dirty")
            } /* end else */
        } /* end if */
        else {
            write_buf = dblock->blk;
            write_size = dblock->size;
        } /* end else */

	/* Write the direct block */
#ifdef QAK
HDfprintf(stderr, "%s: addr = %a, write_size = %Zu\n", FUNC, addr, write_size);
#endif /* QAK */
	if(H5F_block_write(f, H5FD_MEM_FHEAP_DBLOCK, addr, write_size, dxpl_id, write_buf) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFLUSH, FAIL, "unable to save fractal heap direct block to disk")

        /* Release the write buffer, if it was allocated */
        if(write_buf != dblock->blk)
            H5MM_xfree(write_buf);

	dblock->cache_info.is_dirty = FALSE;
    } /* end if */

    if(destroy)
        if(H5HF_cache_dblock_dest(f, dblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5HF_cache_dblock_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_dest
 *
 * Purpose:	Destroys a fractal heap direct block in memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
/* ARGSUSED */
herr_t
H5HF_cache_dblock_dest(H5F_t UNUSED *f, H5HF_direct_t *dblock)
{
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_dest)

    /*
     * Check arguments.
     */
    HDassert(dblock);
#ifdef QAK
HDfprintf(stderr, "%s: Destroying direct block, dblock = %p\n", FUNC, dblock);
#endif /* QAK */

    /* Set the shared heap header's file context for this operation */
    dblock->hdr->f = f;

    /* Decrement reference count on shared fractal heap info */
    HDassert(dblock->hdr);
    if(H5HF_hdr_decr(dblock->hdr) < 0)
        HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared heap header")
    if(dblock->parent)
        if(H5HF_iblock_decr(dblock->parent) < 0)
            HGOTO_ERROR(H5E_HEAP, H5E_CANTDEC, FAIL, "can't decrement reference count on shared indirect block")

    /* Free block's buffer */
    dblock->blk = H5FL_BLK_FREE(direct_block, dblock->blk);

    /* Free fractal heap direct block info */
    H5FL_FREE(H5HF_direct_t, dblock);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_dest() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_clear
 *
 * Purpose:	Mark a fractal heap direct block in memory as non-dirty.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@ncsa.uiuc.edu
 *		Feb 27 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5HF_cache_dblock_clear(H5F_t *f, H5HF_direct_t *dblock, hbool_t destroy)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT(H5HF_cache_dblock_clear)

    /*
     * Check arguments.
     */
    HDassert(dblock);

    /* Reset the dirty flag.  */
    dblock->cache_info.is_dirty = FALSE;

    if(destroy)
        if(H5HF_cache_dblock_dest(f, dblock) < 0)
	    HGOTO_ERROR(H5E_HEAP, H5E_CANTFREE, FAIL, "unable to destroy fractal heap direct block")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5HF_cache_dblock_clear() */


/*-------------------------------------------------------------------------
 * Function:	H5HF_cache_dblock_size
 *
 * Purpose:	Compute the size in bytes of a fractal heap direct block
 *		on disk, and return it in *size_ptr.  On failure,
 *		the value of *size_ptr is undefined.
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
H5HF_cache_dblock_size(const H5F_t UNUSED *f, const H5HF_direct_t *dblock, size_t *size_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOFUNC(H5HF_cache_dblock_size)

    /* check arguments */
    HDassert(dblock);
    HDassert(size_ptr);

    /* Set size value */
    *size_ptr = dblock->size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5HF_cache_dblock_size() */

