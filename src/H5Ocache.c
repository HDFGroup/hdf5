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
 * Created:		H5Ocache.c
 *			Sep 28 2005
 *			Quincey Koziol <koziol@ncsa.uiuc.edu>
 *
 * Purpose:		Object header metadata cache virtual functions.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Omodule.h"          /* This source code file is part of the H5O module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"        /* Memory management                    */
#include "H5Opkg.h"             /* Object headers			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Set the object header size to speculatively read in */
/* (needs to be more than the object header prefix size to work at all and
 *      should be larger than the largest object type's default object header
 *      size to save the extra I/O operations) */
#define H5O_SPEC_READ_SIZE 512


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
static herr_t H5O__cache_get_load_size(const void *image_ptr, void *udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static htri_t H5O__cache_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5O__cache_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty); 
static herr_t H5O__cache_image_len(const void *thing, size_t *image_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static herr_t H5O__cache_serialize(const H5F_t *f, void *image, size_t len,
    void *thing); 
static herr_t H5O__cache_free_icr(void *thing);
static herr_t H5O__cache_clear(const H5F_t *f, void *thing, hbool_t about_to_destroy);

static herr_t H5O__cache_chk_get_load_size(const void *image_ptr, void *udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static htri_t H5O__cache_chk_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5O__cache_chk_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty); 
static herr_t H5O__cache_chk_image_len(const void *thing, size_t *image_len,
    hbool_t *compressed_ptr, size_t *compressed_image_len_ptr);
static herr_t H5O__cache_chk_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5O__cache_chk_notify(H5AC_notify_action_t action, void *_thing);
static herr_t H5O__cache_chk_free_icr(void *thing);
static herr_t H5O__cache_chk_clear(const H5F_t *f, void *thing, hbool_t about_to_destroy);

/* Chunk proxy routines */
static herr_t H5O__chunk_proxy_dest(H5O_chunk_proxy_t *chunk_proxy);

/* Chunk routines */
static herr_t H5O__chunk_deserialize(H5O_t *oh, haddr_t addr, size_t len,
    const uint8_t *image, H5O_common_cache_ud_t *udata, hbool_t *dirty);
static herr_t H5O__chunk_serialize(const H5F_t *f, H5O_t *oh, unsigned chunkno);

/* Misc. routines */
static herr_t H5O__add_cont_msg(H5O_cont_msgs_t *cont_msg_info,
    const H5O_cont_t *cont);
static herr_t H5O_decode_prefix(H5F_t *f, H5O_t *oh, const uint8_t *buf, void *_udata);


/*********************/
/* Package Variables */
/*********************/

/* H5O object header prefix inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_OHDR[1] = {{
    H5AC_OHDR_ID,                       /* Metadata client ID */
    "object header",                    /* Metadata client name (for debugging) */
    H5FD_MEM_OHDR,                      /* File space memory type for client */
    H5AC__CLASS_SPECULATIVE_LOAD_FLAG,  /* Client class behavior flags */
    H5O__cache_get_load_size,           /* 'get_load_size' callback */
    H5O__cache_verify_chksum, 		/* 'verify_chksum' callback */
    H5O__cache_deserialize,             /* 'deserialize' callback */
    H5O__cache_image_len,               /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5O__cache_serialize,               /* 'serialize' callback */
    NULL,                               /* 'notify' callback */
    H5O__cache_free_icr,                /* 'free_icr' callback */
    H5O__cache_clear,                   /* 'clear' callback */
    NULL,                               /* 'fsf_size' callback */
}};

/* H5O object header chunk inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_OHDR_CHK[1] = {{
    H5AC_OHDR_CHK_ID,                   /* Metadata client ID */
    "object header continuation chunk", /* Metadata client name (for debugging) */
    H5FD_MEM_OHDR,                      /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,           /* Client class behavior flags */
    H5O__cache_chk_get_load_size,       /* 'get_load_size' callback */
    H5O__cache_chk_verify_chksum,	/* 'verify_chksum' callback */
    H5O__cache_chk_deserialize,         /* 'deserialize' callback */
    H5O__cache_chk_image_len,           /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5O__cache_chk_serialize,           /* 'serialize' callback */
    H5O__cache_chk_notify,              /* 'notify' callback */
    H5O__cache_chk_free_icr,            /* 'free_icr' callback */
    H5O__cache_chk_clear,               /* 'clear' callback */
    NULL,                               /* 'fsf_size' callback */
}};

/* Declare external the free list for H5O_unknown_t's */
H5FL_EXTERN(H5O_unknown_t);

/* Declare extern the free list for H5O_chunk_proxy_t's */
H5FL_EXTERN(H5O_chunk_proxy_t);

/* Declare the free list for H5O_cont_t sequences */
H5FL_SEQ_DEFINE(H5O_cont_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/


/*-------------------------------------------------------------------------
 * Function:	H5O_decode_prefix
 *
 * Purpose:	To decode the object header prefix.
 *		The coding is extracted fromt H5O__cache_deserialize() to this routine.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_decode_prefix(H5F_t *f, H5O_t *oh, const uint8_t *buf, void *_udata)
{
    H5O_cache_ud_t *udata = (H5O_cache_ud_t *)_udata;       /* User data for callback */
    const uint8_t *p = buf;   	/* Pointer into buffer to decode */
    size_t prefix_size;    	/* Size of object header prefix */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(f);
    HDassert(oh);
    HDassert(buf);
    HDassert(udata);

    /* Check for presence of magic number */
    /* (indicates version 2 or later) */
    if(!HDmemcmp(p, H5O_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC)) {
        /* Magic number */
        p += H5_SIZEOF_MAGIC;

        /* Version */
        oh->version = *p++;
        if(H5O_VERSION_2 != oh->version)
            HGOTO_ERROR(H5E_OHDR, H5E_VERSION, FAIL, "bad object header version number")

        /* Flags */
        oh->flags = *p++;
        if(oh->flags & ~H5O_HDR_ALL_FLAGS)
            HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "unknown object header status flag(s)")

        /* Number of links to object (unless overridden by refcount message) */
        oh->nlink = 1;

        /* Time fields */
        if(oh->flags & H5O_HDR_STORE_TIMES) {
            uint32_t tmp;       /* Temporary value */

            UINT32DECODE(p, tmp);
            oh->atime = (time_t)tmp;
            UINT32DECODE(p, tmp);
            oh->mtime = (time_t)tmp;
            UINT32DECODE(p, tmp);
            oh->ctime = (time_t)tmp;
            UINT32DECODE(p, tmp);
            oh->btime = (time_t)tmp;
        } /* end if */
        else
            oh->atime = oh->mtime = oh->ctime = oh->btime = 0;

        /* Attribute fields */
        if(oh->flags & H5O_HDR_ATTR_STORE_PHASE_CHANGE) {
            UINT16DECODE(p, oh->max_compact);
            UINT16DECODE(p, oh->min_dense);
            if(oh->max_compact < oh->min_dense)
                HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "bad object header attribute phase change values")
        } /* end if */
        else {
            oh->max_compact = H5O_CRT_ATTR_MAX_COMPACT_DEF;
            oh->min_dense = H5O_CRT_ATTR_MIN_DENSE_DEF;
        } /* end else */

        /* First chunk size */
        switch(oh->flags & H5O_HDR_CHUNK0_SIZE) {
            case 0:     /* 1 byte size */
                oh->chunk0_size = *p++;
                break;

            case 1:     /* 2 byte size */
                UINT16DECODE(p, oh->chunk0_size);
                break;

            case 2:     /* 4 byte size */
                UINT32DECODE(p, oh->chunk0_size);
                break;

            case 3:     /* 8 byte size */
                UINT64DECODE(p, oh->chunk0_size);
                break;

            default:
                HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "bad size for chunk 0")
        } /* end switch */
        if(oh->chunk0_size > 0 && oh->chunk0_size < H5O_SIZEOF_MSGHDR_OH(oh))
            HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "bad object header chunk size")
    } /* end if */
    else {
        /* Version */
        oh->version = *p++;
        if(H5O_VERSION_1 != oh->version)
            HGOTO_ERROR(H5E_OHDR, H5E_VERSION, FAIL, "bad object header version number")

        /* Flags */
        oh->flags = H5O_CRT_OHDR_FLAGS_DEF;

        /* Reserved */
        p++;

        /* Number of messages */
        UINT16DECODE(p, udata->v1_pfx_nmesgs);

        /* Link count */
        UINT32DECODE(p, oh->nlink);

        /* Reset unused time fields */
        oh->atime = oh->mtime = oh->ctime = oh->btime = 0;

        /* Reset unused attribute fields */
        oh->max_compact = 0;
        oh->min_dense = 0;

        /* First chunk size */
        UINT32DECODE(p, oh->chunk0_size);
        if((udata->v1_pfx_nmesgs > 0 && oh->chunk0_size < H5O_SIZEOF_MSGHDR_OH(oh)) ||
                (udata->v1_pfx_nmesgs == 0 && oh->chunk0_size > 0))
            HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "bad object header chunk size")

        /* Reserved, in version 1 (for 8-byte alignment padding) */
        p += 4;
    } /* end else */

    /* Determine object header prefix length */
    prefix_size = (size_t)(p - buf);
    HDassert((size_t)prefix_size == (size_t)(H5O_SIZEOF_HDR(oh) - H5O_SIZEOF_CHKSUM_OH(oh)));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O_decode_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_get_load_size()
 *
 * Purpose:	Tell the metadata cache how much data to read from file in 
 *		the first speculative read for the object header.  Note that we do 
 *		not have to be concerned about reading past the end of file, as the 
 *		cache will clamp the read to avoid this if needed.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_get_load_size(const void *_image, void *_udata, size_t *image_len, size_t *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const uint8_t *image = (const uint8_t *)_image;   	/* Pointer into raw data buffer */
    H5O_cache_ud_t *udata = (H5O_cache_ud_t *)_udata;   /* User data for callback */
    H5O_t oh;                                           /* Object header read in */
    htri_t ret_value = SUCCEED;                         /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image_len);

    if(image == NULL)
	*image_len = H5O_SPEC_READ_SIZE;

    else { /* compute actual_len */
	HDassert(udata);
	HDassert(actual_len);
	HDassert(*actual_len == *image_len);

	/* Decode header prefix */
	if(H5O_decode_prefix(udata->common.f, &oh, image, udata) < 0)
	    HGOTO_DONE(FAIL)

	/* Save the version to be used in verify_chksum callback */
	udata->version = oh.version;
	*actual_len = oh.chunk0_size + (size_t)H5O_SIZEOF_HDR(&oh);
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_get_load_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O__cache_verify_chksum(const void *_image, size_t len, void *_udata)
{
    const uint8_t *image = (const uint8_t *)_image;    	/* Pointer into raw data buffer */
    H5O_cache_ud_t *udata = (H5O_cache_ud_t *)_udata;  	/* User data for callback */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    htri_t ret_value = TRUE;	/* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image);
    HDassert(udata);

    /* There is no checksum for version 1 */
    if(udata->version != H5O_VERSION_1) {

	/* Get stored and computed checksums */
	H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

	if(stored_chksum != computed_chksum)
	    ret_value = FALSE;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_deserialize
 *
 * Purpose:	Attempt to deserialize the object header contained in the 
 *		supplied buffer, load the data into an instance of H5O_t, and 
 *		return a pointer to the new instance.
 *
 *		Note that the object header is read with with a speculative read.  
 *		If the initial read is too small, make note of this fact and return 
 *     		without error.  H5C_load_entry() will note the size discrepency
 *		and retry the deserialize operation with the correct size read.
 *
 * Return:      Success:        Pointer to in core representation
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__cache_deserialize(const void *_image, size_t len, void *_udata,
    hbool_t *dirty)
{
    H5O_t          *oh = NULL;          /* Object header read in */
    H5O_cache_ud_t *udata = (H5O_cache_ud_t *)_udata;   /* User data for callback */
    const uint8_t  *image = (const uint8_t *)_image;    /* Pointer into buffer to decode */
    size_t          buf_size;           /* Size of prefix+chunk #0 buffer */
    void *          ret_value = NULL;   /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(image);
    HDassert(len > 0); 
    HDassert(udata);
    HDassert(udata->common.f);
    HDassert(udata->common.cont_msg_info);
    HDassert(dirty);

    /* Allocate space for the object header data structure */
    if(NULL == (oh = H5FL_CALLOC(H5O_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* File-specific, non-stored information */
    oh->sizeof_size = H5F_SIZEOF_SIZE(udata->common.f);
    oh->sizeof_addr = H5F_SIZEOF_ADDR(udata->common.f);

    /* Decode header prefix */
    if(H5O_decode_prefix(udata->common.f, oh, image, udata) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "can't deserialize object header prefix")

    /* Compute the size of the buffer used */
    buf_size = oh->chunk0_size + (size_t)H5O_SIZEOF_HDR(oh);

    oh->swmr_write = !!(H5F_INTENT(udata->common.f) & H5F_ACC_SWMR_WRITE);

    /* Create object header proxy if doing SWMR writes */
    HDassert(!oh->proxy_present);
    if(H5F_INTENT(udata->common.f) & H5F_ACC_SWMR_WRITE) {
	if(H5O__proxy_create(udata->common.f, udata->common.dxpl_id, oh) < 0)
	    HGOTO_ERROR(H5E_OHDR, H5E_CANTCREATE, NULL, "can't create object header proxy")
    } /* end if */
    else
	oh->proxy_addr = HADDR_UNDEF;

    /* Check to see if the buffer provided is large enough to contain both 
     * the prefix and the first chunk.  If it isn't, make note of the desired
     * size, but otherwise do nothing.  H5C_load_entry() will notice the 
     * discrepency, load the correct size buffer, and retry the deserialize.
     */
    if(len >= buf_size) {
        /* Parse the first chunk */
        if(H5O__chunk_deserialize(oh, udata->common.addr, oh->chunk0_size, (const uint8_t *)_image, &(udata->common), dirty) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "can't deserialize first object header chunk")
    } /* end if */
    else
        HDassert(!udata->made_attempt);

    /* Note that we've loaded the object header from the file */
    udata->made_attempt = TRUE;

    /* Set return value */
    ret_value = oh;

done:
    /* Release the [possibly partially initialized] object header on errors */
    if(!ret_value && oh)
        if(H5O_free(oh) < 0)
            HDONE_ERROR(H5E_OHDR, H5E_CANTRELEASE, NULL, "unable to destroy object header data")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_image_len
 *
 * Purpose:	Compute the size in bytes of the specified instance of
 *		H5O_t on disk, and return it in *image_len.  On failure,
 *		the value of *image_len is undefined.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_image_len(const void *_thing, size_t *image_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const H5O_t *oh = (const H5O_t *)_thing;    /* Object header to query */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(oh);
    HDassert(oh->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(oh->cache_info.type == H5AC_OHDR);
    HDassert(image_len);

    /* Report the object header's prefix+first chunk length */
    if(oh->chunk0_size)
       *image_len = (size_t)H5O_SIZEOF_HDR(oh) + oh->chunk0_size;
    else
       *image_len = oh->chunk[0].size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cache_image_len() */

/********************************/
/* no H5O_cache_pre_serialize() */
/********************************/


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_serialize
 *
 * Purpose:	Serialize the contents of the supplied object header, and
 *		load this data into the supplied buffer.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_serialize(const H5F_t *f, void *image, size_t len, void *_thing)
{
    H5O_t      *oh = (H5O_t *)_thing;   /* Object header to encode */
    uint8_t     *chunk_image;           /* Pointer to object header prefix buffer */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(oh);
    HDassert(oh->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(oh->cache_info.type == H5AC_OHDR);
    HDassert(oh->chunk[0].size == len);
#ifdef H5O_DEBUG
    H5O_assert(oh);
#endif /* H5O_DEBUG */

    /* Point to raw data 'image' for first chunk, which 
     * has room for the prefix 
     */
    chunk_image = oh->chunk[0].image;

    /* Later versions of object header prefix have different format and
     * also require that chunk 0 always be updated, since the checksum
     * on the entire block of memory needs to be updated if anything is
     * modified 
     */
    if(oh->version > H5O_VERSION_1) {
        uint64_t chunk0_size;       /* Size of chunk 0's data */

        HDassert(oh->chunk[0].size >= (size_t)H5O_SIZEOF_HDR(oh));
        chunk0_size = oh->chunk[0].size - (size_t)H5O_SIZEOF_HDR(oh);

        /* Verify magic number */
        HDassert(!HDmemcmp(chunk_image, H5O_HDR_MAGIC, H5_SIZEOF_MAGIC));
        chunk_image += H5_SIZEOF_MAGIC;

        /* Version */
        *chunk_image++ = oh->version;

        /* Flags */
        *chunk_image++ = oh->flags;

        /* Time fields */
        if(oh->flags & H5O_HDR_STORE_TIMES) {
            UINT32ENCODE(chunk_image, oh->atime);
            UINT32ENCODE(chunk_image, oh->mtime);
            UINT32ENCODE(chunk_image, oh->ctime);
            UINT32ENCODE(chunk_image, oh->btime);
        } /* end if */

        /* Attribute fields */
        if(oh->flags & H5O_HDR_ATTR_STORE_PHASE_CHANGE) {
            UINT16ENCODE(chunk_image, oh->max_compact);
            UINT16ENCODE(chunk_image, oh->min_dense);
        } /* end if */

        /* First chunk size */
        switch(oh->flags & H5O_HDR_CHUNK0_SIZE) {
            case 0:     /* 1 byte size */
                HDassert(chunk0_size < 256);
                *chunk_image++ = (uint8_t)chunk0_size;
                break;

            case 1:     /* 2 byte size */
                HDassert(chunk0_size < 65536);
                UINT16ENCODE(chunk_image, chunk0_size);
                break;

            case 2:     /* 4 byte size */
                /* use <= 2**32 -1 to stay within 4 bytes integer range */
                HDassert(chunk0_size <= 4294967295UL);
                UINT32ENCODE(chunk_image, chunk0_size);
                break;

            case 3:     /* 8 byte size */
                UINT64ENCODE(chunk_image, chunk0_size);
                break;

            default:
                HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "bad size for chunk 0")
        } /* end switch */
    } /* end if */
    else {
        /* Version */
        *chunk_image++ = oh->version;

        /* Reserved */
        *chunk_image++ = 0;

        /* Number of messages */
#ifdef H5O_ENABLE_BAD_MESG_COUNT
        if(oh->store_bad_mesg_count)
           UINT16ENCODE(chunk_image, (oh->nmesgs - 1))
        else
#endif /* H5O_ENABLE_BAD_MESG_COUNT */
            UINT16ENCODE(chunk_image, oh->nmesgs);

        /* Link count */
        UINT32ENCODE(chunk_image, oh->nlink);

        /* First chunk size */
        UINT32ENCODE(chunk_image, (oh->chunk[0].size - (size_t)H5O_SIZEOF_HDR(oh)));

        /* Zero to alignment */
        HDmemset(chunk_image, 0, (size_t)(H5O_SIZEOF_HDR(oh) - 12));
        chunk_image += (size_t)(H5O_SIZEOF_HDR(oh) - 12);
    } /* end else */

    HDassert((size_t)(chunk_image - oh->chunk[0].image) == (size_t)(H5O_SIZEOF_HDR(oh) - H5O_SIZEOF_CHKSUM_OH(oh)));

    /* Serialize messages for this chunk */
    if(H5O__chunk_serialize(f, oh, (unsigned)0) < 0) 
        HGOTO_ERROR(H5E_OHDR, H5E_CANTSERIALIZE, FAIL, "unable to serialize first object header chunk")

    /* copy the chunk into the image -- this is potentially expensive.  
     * Can we rework things so that the object header and the cache 
     * share a buffer?
     */
    HDmemcpy(image, oh->chunk[0].image, len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_serialize() */

/**********************************/
/* no H5O_cache_notify() function */
/**********************************/


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_free_icr
 *
 * Purpose:	Free the in core representation of the supplied object header.
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_free_icr(void *_thing)
{ 
    H5O_t      *oh = (H5O_t *)_thing;   /* Object header to destroy */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(oh);
    HDassert(oh->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(oh->cache_info.type == H5AC_OHDR);

    /* Destroy object header */
    if(H5O_free(oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "can't destroy object header")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_clear
 *
 * Purpose: 	Clear all dirty bits associated with this cache entry.
 *
 *		This is ncessary as the object header cache client maintains 
 *		its own dirty bits on individual messages.  These dirty bits 
 *		used to be cleared by the old V2 metadata cache flush callback,
 *		but now the metadata cache must clear them explicitly, as 
 *		the serialize callback does not imply that the data has been
 *		written to disk.
 *
 *		This callback is also necessary for the parallel case.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              9/22/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
#ifdef H5_HAVE_PARALLEL
H5O__cache_clear(const H5F_t *f, void *_thing, hbool_t H5_ATTR_UNUSED about_to_destroy)
#else
H5O__cache_clear(const H5F_t H5_ATTR_UNUSED *f, void *_thing, hbool_t H5_ATTR_UNUSED about_to_destroy)
#endif /* H5_HAVE_PARALLEL */
{ 
    H5O_t      *oh = (H5O_t *)_thing;   /* Object header to reset */
    unsigned    u;                      /* Local index variable */
    herr_t      ret_value = SUCCEED;    /* Return value */

#ifdef H5_HAVE_PARALLEL
    FUNC_ENTER_STATIC
#else
    FUNC_ENTER_STATIC_NOERR
#endif /* H5_HAVE_PARALLEL */

    /* Check arguments */
    HDassert(oh);
    HDassert(oh->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(oh->cache_info.type == H5AC_OHDR);

#ifdef H5_HAVE_PARALLEL
    if((oh->nchunks > 0) && (!about_to_destroy)) {
        /* Scan through chunk 0 (the chunk stored contiguously with this 
         * object header) and cause it to update its image of all entries 
         * currently marked dirty.  Must do this in the parallel case, as 
         * it is possible that this processor may clear this object header 
         * several times before flushing it -- thus causing undefined 
         * sections of the image to be written to disk overwriting valid data.
         */
        if(H5O__chunk_serialize(f, oh, 0) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTSERIALIZE, FAIL, "unable to serialize object header chunk")
    } /* end if */
#endif /* H5_HAVE_PARALLEL */

    /* Mark messages stored with the object header (i.e. messages in chunk 0) as clean */
    for(u = 0; u < oh->nmesgs; u++)
        if(oh->mesg[u].chunkno == 0)
            oh->mesg[u].dirty = FALSE;

#ifndef NDEBUG
    /* Reset the number of messages dirtied by decoding */
    oh->ndecode_dirtied = 0;
#endif /* NDEBUG */

#ifdef H5_HAVE_PARALLEL
done:
#endif /* H5_HAVE_PARALLEL */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_clear() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_get_load_size()
 *
 * Purpose:	Tell the metadata cache how large the on disk image of the 
 *		chunk proxy is, so it can load the image into a buffer for the 
 *		deserialize call.  In this case, we simply look up the size in 
 *		the user data, and return it in *image_len,
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_chk_get_load_size(const void *_image, void *_udata, 
    size_t *image_len, size_t *actual_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const uint8_t *image = (const uint8_t *)_image;       		  /* Pointer into raw data buffer */
    const H5O_chk_cache_ud_t *udata = (const H5O_chk_cache_ud_t *)_udata; /* User data for callback */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(udata);
    HDassert(udata->oh);
    HDassert(image_len);

    if(image == NULL)
	*image_len = udata->size;
    else {
	HDassert(actual_len);
        HDassert(*actual_len == *image_len);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cache_chk_get_load_size() */

/*-------------------------------------------------------------------------
 * Function:    H5B2__cache_chk_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5O__cache_chk_verify_chksum(const void *_image, size_t len, void *_udata)
{
    const uint8_t *image = (const uint8_t *)_image;       	/* Pointer into raw data buffer */
    H5O_chk_cache_ud_t *udata = (H5O_chk_cache_ud_t *)_udata;   /* User data for callback */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */
    htri_t ret_value = TRUE;	/* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(image);

    /* There is no checksum for version 1 */
    if(udata->oh->version != H5O_VERSION_1) {

	/* Get stored and computed checksums */
	H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

	if(stored_chksum != computed_chksum)
	    ret_value = FALSE;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_deserialize
 *
 * Purpose:	Attempt to deserialize the object header continuation chunk
 *		contained in the supplied buffer, load the data into an instance 
 *		of H5O_chunk_proxy_t, and return a pointer to the new instance.
 *
 * Return:      Success:        Pointer to in core representation
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__cache_chk_deserialize(const void *image, size_t len, void *_udata,
    hbool_t *dirty)
{
    H5O_chunk_proxy_t  *chk_proxy = NULL;       /* Chunk proxy object */
    H5O_chk_cache_ud_t *udata = (H5O_chk_cache_ud_t *)_udata;   /* User data for callback */
    void		*ret_value = NULL;      /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(image);
    HDassert(len > 0);
    HDassert(udata);
    HDassert(udata->oh);
    HDassert(dirty);

    /* Allocate space for the object header data structure */
    if(NULL == (chk_proxy = H5FL_CALLOC(H5O_chunk_proxy_t))) 
        HGOTO_ERROR(H5E_OHDR, H5E_CANTALLOC, NULL, "memory allocation failed")

    /* initialize the flush dependency parent fields.  If needed, they
     * will be set in the notify routine.
     */
    chk_proxy->fd_parent_addr = HADDR_UNDEF;
    chk_proxy->fd_parent_ptr = NULL;

    /* Check if we are still decoding the object header */
    /* (as opposed to bringing a piece of it back from the file) */
    if(udata->decoding) {
        /* Sanity check */
        HDassert(udata->common.f);
        HDassert(udata->common.cont_msg_info);

        /* Parse the chunk */
        if(H5O__chunk_deserialize(udata->oh, udata->common.addr, udata->size, (const uint8_t *)image, &(udata->common), dirty) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "can't deserialize object header chunk")

        /* Set the fields for the chunk proxy */
        chk_proxy->oh = udata->oh;
        chk_proxy->chunkno = udata->oh->nchunks - 1;
    } /* end if */
    else {
        /* Sanity check */
        HDassert(udata->chunkno < udata->oh->nchunks);

        /* Set the fields for the chunk proxy */
        chk_proxy->oh = udata->oh;
        chk_proxy->chunkno = udata->chunkno;

        /* Sanity check that the chunk representation we have in memory is 
         * the same as the one being brought in from disk.
         */
        HDassert(0 == HDmemcmp(image, chk_proxy->oh->chunk[chk_proxy->chunkno].image, chk_proxy->oh->chunk[chk_proxy->chunkno].size));
    } /* end else */

    /* Increment reference count of object header */
    if(H5O_inc_rc(udata->oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINC, NULL, "can't increment reference count on object header")

    /* Set return value */
    ret_value = chk_proxy;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_image_len
 *
 * Purpose:	Return the on disk image size of a object header chunk to the 
 *		metadata cache via the image_len.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_chk_image_len(const void *_thing, size_t *image_len,
    hbool_t H5_ATTR_UNUSED *compressed_ptr, size_t H5_ATTR_UNUSED *compressed_image_len_ptr)
{
    const H5O_chunk_proxy_t * chk_proxy = (const H5O_chunk_proxy_t *)_thing;    /* Chunk proxy to query */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(chk_proxy);
    HDassert(chk_proxy->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(chk_proxy->cache_info.type == H5AC_OHDR_CHK);
    HDassert(chk_proxy->oh);
    HDassert(image_len);

    *image_len = chk_proxy->oh->chunk[chk_proxy->chunkno].size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cache_chk_image_len() */

/************************************/
/* no H5O_cache_chk_pre_serialize() */
/************************************/


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_serialize
 *
 * Purpose:	Given a pointer to an instance of an object header chunk and an 
 *		appropriately sized buffer, serialize the contents of the 
 *		instance for writing to disk, and copy the serialized data 
 *		into the buffer.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_chk_serialize(const H5F_t *f, void *image, size_t len, void *_thing)
{
    H5O_chunk_proxy_t * chk_proxy = (H5O_chunk_proxy_t *)_thing;        /* Object header chunk to serialize */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(chk_proxy);
    HDassert(chk_proxy->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(chk_proxy->cache_info.type == H5AC_OHDR_CHK);
    HDassert(chk_proxy->oh);
    HDassert(chk_proxy->oh->chunk[chk_proxy->chunkno].size == len);

    /* Serialize messages for this chunk */
    if(H5O__chunk_serialize(f, chk_proxy->oh, chk_proxy->chunkno) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTSERIALIZE, FAIL, "unable to serialize object header continuation chunk")

    /* copy the chunk into the image -- this is potentially expensive.
     * Can we rework things so that the chunk and the cache share a buffer?
     */
    HDmemcpy(image, chk_proxy->oh->chunk[chk_proxy->chunkno].image, len);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Mar 20 2012
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_chk_notify(H5AC_notify_action_t action, void *_thing)
{
    H5O_chunk_proxy_t *chk_proxy = (H5O_chunk_proxy_t *)_thing;
    void *parent = NULL;                /* Chunk containing continuation message that points to this chunk */
    H5O_chunk_proxy_t *cont_chk_proxy = NULL; /* Proxy for chunk containing continuation message that points to this chunk, if not chunk 0 */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /*
     * Check arguments.
     */
    HDassert(chk_proxy);
    HDassert(chk_proxy->oh);

    if(chk_proxy->oh->swmr_write) {
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
	    case H5AC_NOTIFY_ACTION_AFTER_LOAD:
                /* Add flush dependency from chunk containing the continuation message
                 * that points to this chunk (either oh or another chunk proxy object)
                 */
                if(chk_proxy->cont_chunkno == 0)
                    parent = chk_proxy->oh;
                else {
                    if(NULL == (cont_chk_proxy = H5O_chunk_protect(chk_proxy->f, H5AC_ind_read_dxpl_id, chk_proxy->oh, chk_proxy->cont_chunkno)))
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTPROTECT, FAIL, "unable to load object header chunk")
                    parent = cont_chk_proxy;
                } /* end else */

                if(H5AC_create_flush_dependency(parent, chk_proxy) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTDEPEND, FAIL, "unable to create flush dependency")

                /* make note of the address and pointer of the flush 
                 * dependency parent so we can take the dependency down
                 * on eviction.
                 */
		HDassert(parent);
                HDassert(((H5C_cache_entry_t *)parent)->magic == 
                         H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(((H5C_cache_entry_t *)parent)->type);
                HDassert((((H5C_cache_entry_t *)(parent))->type->id
                          == H5AC_OHDR_ID) ||
                         (((H5C_cache_entry_t *)(parent))->type->id
                          == H5AC_OHDR_CHK_ID));

                chk_proxy->fd_parent_addr = ((H5C_cache_entry_t *)parent)->addr;
                chk_proxy->fd_parent_ptr = parent;


                /* Add flush dependency on object header proxy, if proxy exists */
                if(chk_proxy->oh->proxy_present)
                    if(H5O__proxy_depend(chk_proxy->f, H5AC_ind_read_dxpl_id, chk_proxy->oh, chk_proxy) < 0)
                        HGOTO_ERROR(H5E_OHDR, H5E_CANTDEPEND, FAIL, "can't create flush dependency on object header proxy")

	    case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                HDassert(chk_proxy->fd_parent_addr != HADDR_UNDEF);
                HDassert(chk_proxy->fd_parent_ptr != NULL);
                HDassert(((H5C_cache_entry_t *)(chk_proxy->fd_parent_ptr))->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
                HDassert(((H5C_cache_entry_t *)(chk_proxy->fd_parent_ptr))->type);
                HDassert((((H5C_cache_entry_t *)(chk_proxy->fd_parent_ptr))->type->id == H5AC_OHDR_ID) || (((H5C_cache_entry_t *)(chk_proxy->fd_parent_ptr))->type->id == H5AC_OHDR_CHK_ID));

                if(H5AC_destroy_flush_dependency(chk_proxy->fd_parent_ptr, chk_proxy) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTUNDEPEND, FAIL, "unable to destroy flush dependency")
                break;

            default:
#ifdef NDEBUG
                HGOTO_ERROR(H5E_OHDR, H5E_BADVALUE, FAIL, "unknown action from metadata cache")
#else /* NDEBUG */
                HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
        } /* end switch */
    } /* end if */

done:
    if(cont_chk_proxy)
        if(H5O_chunk_unprotect(chk_proxy->f, H5AC_ind_read_dxpl_id, cont_chk_proxy, FALSE) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTUNPROTECT, FAIL, "unable to unprotect object header chunk")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_notify() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_free_icr
 *
 * Purpose:	Free the in core memory associated with the supplied object
 *		header continuation chunk.
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/28/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cache_chk_free_icr(void *_thing)
{
    H5O_chunk_proxy_t * chk_proxy = (H5O_chunk_proxy_t *)_thing;        /* Object header chunk proxy to release */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(chk_proxy);
    HDassert(chk_proxy->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(chk_proxy->cache_info.type == H5AC_OHDR_CHK);

    /* Destroy object header chunk proxy */
    if(H5O__chunk_proxy_dest(chk_proxy) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTRELEASE, FAIL, "unable to destroy object header chunk proxy")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5O__cache_chk_clear
 *
 * Purpose: 	Clear all dirty bits associated with this cache entry.
 *
 *		This is ncessary as the object header cache client maintains 
 *		its own dirty bits on individual messages.  These dirty bits 
 *		used to be cleared by the old V2 metadata cache flush callback,
 *		but now the metadata cache must clear them explicitly, as 
 *		the serialize callback does not imply that the data has been
 *		written to disk.
 *
 *		This callback is also necessary for the parallel case.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              9/22/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
#ifdef H5_HAVE_PARALLEL
H5O__cache_chk_clear(const H5F_t *f, void *_thing, hbool_t about_to_destroy)
#else
H5O__cache_chk_clear(const H5F_t H5_ATTR_UNUSED *f, void *_thing, hbool_t H5_ATTR_UNUSED about_to_destroy)
#endif /* H5_HAVE_PARALLEL */
{ 
    H5O_chunk_proxy_t  *chk_proxy = (H5O_chunk_proxy_t *)_thing;        /* Object header chunk to reset */
    H5O_t              *oh;                     /* Object header for chunk */
    unsigned            u;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

#ifdef H5_HAVE_PARALLEL
    FUNC_ENTER_STATIC
#else
    FUNC_ENTER_STATIC_NOERR
#endif /* H5_HAVE_PARALLEL */

    /* Check arguments */
    HDassert(chk_proxy);
    HDassert(chk_proxy->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(chk_proxy->cache_info.type == H5AC_OHDR_CHK);
    oh = chk_proxy->oh;
    HDassert(oh);
    HDassert(oh->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(oh->cache_info.type == H5AC_OHDR);

#ifdef H5_HAVE_PARALLEL
    if((chk_proxy->oh->cache_info.is_dirty) && (!about_to_destroy))
        if(H5O__chunk_serialize(f, chk_proxy->oh, chk_proxy->chunkno) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTSERIALIZE, FAIL, "unable to serialize object header chunk")
#endif /* H5_HAVE_PARALLEL */

    /* Mark messages in chunk as clean */
    for(u = 0; u < chk_proxy->oh->nmesgs; u++)
        if(chk_proxy->oh->mesg[u].chunkno == chk_proxy->chunkno)
            chk_proxy->oh->mesg[u].dirty = FALSE;

#ifdef H5_HAVE_PARALLEL
done:
#endif /* H5_HAVE_PARALLEL */
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cache_chk_clear() */



/*-------------------------------------------------------------------------
 * Function:	H5O__add_cont_msg
 *
 * Purpose:	Add information from a continuation message to the list of
 *              continuation messages in the object header
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 12, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__add_cont_msg(H5O_cont_msgs_t *cont_msg_info, const H5O_cont_t *cont)
{
    size_t contno;              /* Continuation message index */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(cont_msg_info);
    HDassert(cont);

    /* Increase chunk array size, if necessary */
    if(cont_msg_info->nmsgs >= cont_msg_info->alloc_nmsgs) {
        size_t na = MAX(H5O_NCHUNKS, cont_msg_info->alloc_nmsgs * 2);        /* Double # of messages allocated */
        H5O_cont_t *x;

        if(NULL == (x = H5FL_SEQ_REALLOC(H5O_cont_t, cont_msg_info->msgs, na)))
            HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, FAIL, "memory allocation failed")
        cont_msg_info->alloc_nmsgs = na;
        cont_msg_info->msgs = x;
    } /* end if */

    /* Init the continuation message info */
    contno = cont_msg_info->nmsgs++;
    cont_msg_info->msgs[contno].addr = cont->addr;
    cont_msg_info->msgs[contno].size = cont->size;
    cont_msg_info->msgs[contno].chunkno = cont->chunkno;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__add_cont_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5O__chunk_deserialize
 *
 * Purpose:	Deserialize a chunk for an object header
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 12, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__chunk_deserialize(H5O_t *oh, haddr_t addr, size_t len, const uint8_t *image,
    H5O_common_cache_ud_t *udata, hbool_t *dirty)
{
    const uint8_t *chunk_image; /* Pointer into buffer to decode */
    uint8_t *eom_ptr;           /* Pointer to end of messages for a chunk */
    size_t curmesg;             /* Current message being decoded in object header */
    unsigned merged_null_msgs = 0;  /* Number of null messages merged together */
    unsigned chunkno;           /* Current chunk's index */
#ifndef NDEBUG
    unsigned nullcnt;           /* Count of null messages (for sanity checking gaps in chunks) */
#endif /* NDEBUG */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(oh);
    HDassert(H5F_addr_defined(addr));
    HDassert(image);
    HDassert(udata->f);
    HDassert(udata->cont_msg_info);

    /* Increase chunk array size, if necessary */
    if(oh->nchunks >= oh->alloc_nchunks) {
        size_t na = MAX(H5O_NCHUNKS, oh->alloc_nchunks * 2);        /* Double # of chunks allocated */
        H5O_chunk_t *x;

        if(NULL == (x = H5FL_SEQ_REALLOC(H5O_chunk_t, oh->chunk, na)))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTALLOC, FAIL, "memory allocation failed")
        oh->alloc_nchunks = na;
        oh->chunk = x;
    } /* end if */

    /* Init the chunk data info */
    chunkno = (unsigned)oh->nchunks++;
    oh->chunk[chunkno].gap = 0;
    if(chunkno == 0) {
        /* First chunk's 'image' includes room for the object header prefix */
        oh->chunk[0].addr = addr;
        oh->chunk[0].size = len + (size_t)H5O_SIZEOF_HDR(oh);
    } /* end if */
    else {
        oh->chunk[chunkno].addr = addr;
        oh->chunk[chunkno].size = len;
    } /* end else */
    if(NULL == (oh->chunk[chunkno].image = H5FL_BLK_MALLOC(chunk_image, oh->chunk[chunkno].size)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTALLOC, FAIL, "memory allocation failed")

    /* Copy disk image into chunk's image */
    HDmemcpy(oh->chunk[chunkno].image, image, oh->chunk[chunkno].size);

    /* Point into chunk image to decode */
    chunk_image = oh->chunk[chunkno].image;

    /* Handle chunk 0 as special case */
    if(chunkno == 0)
        /* Skip over [already decoded] prefix */
        chunk_image += (size_t)(H5O_SIZEOF_HDR(oh) - H5O_SIZEOF_CHKSUM_OH(oh));
    /* Check for magic # on chunks > 0 in later versions of the format */
    else if(chunkno > 0 && oh->version > H5O_VERSION_1) {
        /* Magic number */
        if(HDmemcmp(chunk_image, H5O_CHK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "wrong object header chunk signature")
        chunk_image += H5_SIZEOF_MAGIC;
    } /* end if */

    /* Save # of messages already inspected */
    curmesg = oh->nmesgs;

    /* Decode messages from this chunk */
    eom_ptr = oh->chunk[chunkno].image + (oh->chunk[chunkno].size - H5O_SIZEOF_CHKSUM_OH(oh));
#ifndef NDEBUG
    nullcnt = 0;
#endif /* NDEBUG */
    while(chunk_image < eom_ptr) {
        size_t mesgno;          /* Current message to operate on */
        size_t mesg_size;       /* Size of message read in */
        unsigned id;            /* ID (type) of current message */
        uint8_t	flags;          /* Flags for current message */
        H5O_msg_crt_idx_t crt_idx = 0;  /* Creation index for current message */

        /* Decode message prefix info */

        /* Version # */
        if(oh->version == H5O_VERSION_1)
            UINT16DECODE(chunk_image, id)
        else
            id = *chunk_image++;

        /* Message size */
        UINT16DECODE(chunk_image, mesg_size);
        HDassert(mesg_size == H5O_ALIGN_OH(oh, mesg_size));

        /* Message flags */
        flags = *chunk_image++;
        if(flags & ~H5O_MSG_FLAG_BITS)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "unknown flag for message")
        if((flags & H5O_MSG_FLAG_SHARED) && (flags & H5O_MSG_FLAG_DONTSHARE))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "bad flag combination for message")
        if((flags & H5O_MSG_FLAG_WAS_UNKNOWN) && (flags & H5O_MSG_FLAG_FAIL_IF_UNKNOWN_AND_OPEN_FOR_WRITE))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "bad flag combination for message")
        if((flags & H5O_MSG_FLAG_WAS_UNKNOWN) && !(flags & H5O_MSG_FLAG_MARK_IF_UNKNOWN))
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, FAIL, "bad flag combination for message")

        /* Reserved bytes/creation index */
        if(oh->version == H5O_VERSION_1)
            chunk_image += 3; /*reserved*/
        else {
            /* Only decode creation index if they are being tracked */
            if(oh->flags & H5O_HDR_ATTR_CRT_ORDER_TRACKED)
                UINT16DECODE(chunk_image, crt_idx);
        } /* end else */

        /* Try to detect invalidly formatted object header message that
         *  extends past end of chunk.
         */
        if(chunk_image + mesg_size > eom_ptr)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "corrupt object header")

#ifndef NDEBUG
        /* Increment count of null messages */
        if(H5O_NULL_ID == id)
            nullcnt++;
#endif /* NDEBUG */

        /* Check for combining two adjacent 'null' messages */
        if((udata->file_intent & H5F_ACC_RDWR) &&
                H5O_NULL_ID == id && oh->nmesgs > 0 &&
                H5O_NULL_ID == oh->mesg[oh->nmesgs - 1].type->id &&
                oh->mesg[oh->nmesgs - 1].chunkno == chunkno) {

            /* Combine adjacent null messages */
            mesgno = oh->nmesgs - 1;
            oh->mesg[mesgno].raw_size += (size_t)H5O_SIZEOF_MSGHDR_OH(oh) + mesg_size;
            oh->mesg[mesgno].dirty = TRUE;
            merged_null_msgs++;
            udata->merged_null_msgs++;
        } /* end if */
        else {
            /* Check if we need to extend message table to hold the new message */
            if(oh->nmesgs >= oh->alloc_nmesgs)
                if(H5O_alloc_msgs(oh, (size_t)1) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTALLOC, FAIL, "can't allocate more space for messages")

            /* Get index for message */
            mesgno = oh->nmesgs++;

            /* Initialize information about message */
            oh->mesg[mesgno].dirty = FALSE;
            oh->mesg[mesgno].flags = flags;
            oh->mesg[mesgno].crt_idx = crt_idx;
            oh->mesg[mesgno].native = NULL;
            oh->mesg[mesgno].raw = (uint8_t *)chunk_image;        /* Casting away const OK - QAK */
            oh->mesg[mesgno].raw_size = mesg_size;
            oh->mesg[mesgno].chunkno = chunkno;

            /* Point unknown messages at 'unknown' message class */
            /* (Usually from future versions of the library) */
	    if(id >= H5O_UNKNOWN_ID ||
#ifdef H5O_ENABLE_BOGUS
	       id == H5O_BOGUS_VALID_ID ||
#endif
	       NULL == H5O_msg_class_g[id]) {

		H5O_unknown_t *unknown;     /* Pointer to "unknown" message info */

                /* Allocate "unknown" message info */
                if(NULL == (unknown = H5FL_MALLOC(H5O_unknown_t)))
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTALLOC, FAIL, "memory allocation failed")

                /* Save the original message type ID */
                *unknown = id;

                /* Save 'native' form of unknown message */
                oh->mesg[mesgno].native = unknown;

                /* Set message to "unknown" class */
                oh->mesg[mesgno].type = H5O_msg_class_g[H5O_UNKNOWN_ID];

                /* Check for "fail if unknown" message flags */
                if(((udata->file_intent & H5F_ACC_RDWR) && 
                       (flags & H5O_MSG_FLAG_FAIL_IF_UNKNOWN_AND_OPEN_FOR_WRITE))
                        || (flags & H5O_MSG_FLAG_FAIL_IF_UNKNOWN_ALWAYS))
                    HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "unknown message with 'fail if unknown' flag found")
                /* Check for "mark if unknown" message flag, etc. */
                else if((flags & H5O_MSG_FLAG_MARK_IF_UNKNOWN) &&
                        !(flags & H5O_MSG_FLAG_WAS_UNKNOWN) &&
                        (udata->file_intent & H5F_ACC_RDWR)) {

                    /* Mark the message as "unknown" */
                    /* This is a bit aggressive, since the application may
                     * never change anything about the object (metadata or
                     * raw data), but we can sort out the finer details
                     * when/if we start using the flag - QAK
                     */
                    /* Also, it's possible that this functionality may not
                     * get invoked if the object header is brought into
                     * the metadata cache in some other "weird" way, like
                     * using H5Ocopy() - QAK
                     */
                    oh->mesg[mesgno].flags |= H5O_MSG_FLAG_WAS_UNKNOWN;

                    /* Mark the message and chunk as dirty */
                    oh->mesg[mesgno].dirty = TRUE;
                    udata->mesgs_modified = TRUE;
                    *dirty = TRUE;
                } /* end if */
            } /* end if */
            else
                /* Set message class for "known" messages */
                oh->mesg[mesgno].type = H5O_msg_class_g[id];
        } /* end else */

        /* Advance decode pointer past message */
        chunk_image += mesg_size;

        /* Check for 'gap' at end of chunk */
        if((eom_ptr - chunk_image) > 0 && (eom_ptr - chunk_image) < H5O_SIZEOF_MSGHDR_OH(oh)) {
            /* Gaps can only occur in later versions of the format */
            HDassert(oh->version > H5O_VERSION_1);

            /* Gaps should only occur in chunks with no null messages */
            HDassert(nullcnt == 0);

            /* Set gap information for chunk */
            oh->chunk[chunkno].gap = (size_t)(eom_ptr - chunk_image);

            /* Increment location in chunk */
            chunk_image += oh->chunk[chunkno].gap;
        } /* end if */
    } /* end while */

    /* Check for correct checksum on chunks, in later versions of the format */
    if(oh->version > H5O_VERSION_1) {
        uint32_t stored_chksum;     /* Checksum from file */

	/* checksum verification already done in verify_chksum cb */

        /* Metadata checksum */
        UINT32DECODE(chunk_image, stored_chksum);

    } /* end if */

    /* Sanity check */
    HDassert(chunk_image == oh->chunk[chunkno].image + oh->chunk[chunkno].size);

    /* Do some inspection/interpretation of new messages from this chunk */
    /* (detect continuation messages, ref. count messages, etc.) */
    while(curmesg < oh->nmesgs) {
        /* Check if next message to examine is a continuation message */
        if(H5O_CONT_ID == oh->mesg[curmesg].type->id) {
            H5O_cont_t *cont;
            unsigned ioflags = 0;   /* Flags for decode routine */

            /* Decode continuation message */
            cont = (H5O_cont_t *)(H5O_MSG_CONT->decode)(udata->f, udata->dxpl_id, NULL, 0, &ioflags, oh->mesg[curmesg].raw);
            cont->chunkno = udata->cont_msg_info->nmsgs + 1;	/*the next continuation message/chunk */

            /* Save 'native' form of continuation message */
            oh->mesg[curmesg].native = cont;

            /* Add to continuation messages left to interpret */
            if(H5O__add_cont_msg(udata->cont_msg_info, cont) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTSET, FAIL, "can't add continuation message")

            /* Mark the message & chunk as dirty if the message was changed by decoding */
            if((ioflags & H5O_DECODEIO_DIRTY) && (udata->file_intent & H5F_ACC_RDWR)) {
                oh->mesg[curmesg].dirty = TRUE;
                udata->mesgs_modified = TRUE;
                *dirty = TRUE;
            } /* end if */
        } /* end if */
        /* Check if next message to examine is a ref. count message */
        else if(H5O_REFCOUNT_ID == oh->mesg[curmesg].type->id) {
            H5O_refcount_t *refcount;
            unsigned ioflags = 0;   /* Flags for decode routine */

            /* Decode ref. count message */
            HDassert(oh->version > H5O_VERSION_1);
            refcount = (H5O_refcount_t *)(H5O_MSG_REFCOUNT->decode)(udata->f, udata->dxpl_id, NULL, 0, &ioflags, oh->mesg[curmesg].raw);

            /* Save 'native' form of ref. count message */
            oh->mesg[curmesg].native = refcount;

            /* Set object header values */
            oh->has_refcount_msg = TRUE;
            oh->nlink = *refcount;

            /* Mark the message & chunk as dirty if the message was changed by decoding */
            if((ioflags & H5O_DECODEIO_DIRTY) && (udata->file_intent & H5F_ACC_RDWR)) {
                oh->mesg[curmesg].dirty = TRUE;
                udata->mesgs_modified = TRUE;
                *dirty = TRUE;
            } /* end if */
        } /* end if */
        /* Check if next message to examine is a link message */
        else if(H5O_LINK_ID == oh->mesg[curmesg].type->id) {
            /* Increment the count of link messages */
            oh->link_msgs_seen++;
        } /* end if */
        /* Check if next message to examine is an attribute message */
        else if(H5O_ATTR_ID == oh->mesg[curmesg].type->id) {
            /* Increment the count of attribute messages */
            oh->attr_msgs_seen++;
        } /* end if */

        /* Advance to next message */
        curmesg++;
    } /* end while */

    /* Mark the chunk dirty if we've merged null messages */
    if(merged_null_msgs) {
        udata->mesgs_modified = TRUE;
	*dirty = TRUE;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__chunk_deserialize() */


/*-------------------------------------------------------------------------
 * Function:	H5O__chunk_serialize
 *
 * Purpose:	Serialize a chunk for an object header
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 12, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__chunk_serialize(const H5F_t *f, H5O_t *oh, unsigned chunkno)
{
    H5O_mesg_t *curr_msg;       /* Pointer to current message being operated on */
    unsigned	u;              /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(f);
    HDassert(oh);

    /* Encode any dirty messages in this chunk */
    for(u = 0, curr_msg = &oh->mesg[0]; u < oh->nmesgs; u++, curr_msg++)
        if(curr_msg->dirty && curr_msg->chunkno == chunkno)
            /* Casting away const OK -QAK */
            if(H5O_msg_flush((H5F_t *)f, oh, curr_msg) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "unable to encode object header message")

    /* Sanity checks */
    if(oh->version > H5O_VERSION_1)
        /* Make certain the magic # is present */
        HDassert(!HDmemcmp(oh->chunk[chunkno].image, (chunkno == 0 ? H5O_HDR_MAGIC : H5O_CHK_MAGIC), H5_SIZEOF_MAGIC));
    else
        /* Gaps should never occur in version 1 of the format */
        HDassert(oh->chunk[chunkno].gap == 0);

    /* Extra work, for later versions of the format */
    if(oh->version > H5O_VERSION_1) {
        uint32_t metadata_chksum;   /* Computed metadata checksum value */
        uint8_t	*chunk_image;       /* Pointer into object header chunk */

        /* Check for gap in chunk & zero it out */
        if(oh->chunk[chunkno].gap)
            HDmemset((oh->chunk[chunkno].image + oh->chunk[chunkno].size) -
                (H5O_SIZEOF_CHKSUM + oh->chunk[chunkno].gap), 0, oh->chunk[chunkno].gap);

        /* Compute metadata checksum */
        metadata_chksum = H5_checksum_metadata(oh->chunk[chunkno].image, (oh->chunk[chunkno].size - H5O_SIZEOF_CHKSUM), 0);

        /* Metadata checksum */
        chunk_image = oh->chunk[chunkno].image + (oh->chunk[chunkno].size - H5O_SIZEOF_CHKSUM);
        UINT32ENCODE(chunk_image, metadata_chksum);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__chunk_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5O__chunk_proxy_dest
 *
 * Purpose:	Destroy a chunk proxy object
 *
 * Return:	Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:	Quincey Koziol
 *              koziol@hdfgroup.org
 *              July 13, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__chunk_proxy_dest(H5O_chunk_proxy_t *chk_proxy)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(chk_proxy);

    /* Decrement reference count of object header */
    if(chk_proxy->oh && H5O_dec_rc(chk_proxy->oh) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDEC, FAIL, "can't decrement reference count on object header")

    /* Release the chunk proxy object */
    chk_proxy = H5FL_FREE(H5O_chunk_proxy_t, chk_proxy);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__chunk_proxy_dest() */
