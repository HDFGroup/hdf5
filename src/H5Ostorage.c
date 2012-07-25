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

/* Programmer:  Quincey Koziol <koziol@hdfgroup.org>
 *              Wednesday, July 29, 2009
 *
 * Purpose:     Message related to data storage.
 */

#define H5D_PACKAGE		/*suppress error about including H5Dpkg	  */
#define H5O_PACKAGE		/*suppress error about including H5Opkg	  */

#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Opkg.h"             /* Object headers			*/


/* Local macros */

/* Current version of storage information */
#define H5O_STORAGE_VERSION 	0

/* Flags for chunked storage feature encoding */
#ifdef NOT_YET
#define H5O_STORAGE_CHUNK_HAVE_ELEM_INDEX       0x02
#define H5O_STORAGE_ALL_CHUNK_FLAGS (                                         \
    H5O_STORAGE_CHUNK_HAVE_ELEM_INDEX                                         \
    | H5O_STORAGE_CHUNK_HAVE_CHUNK_INDEX                                      \
    )
#else /* NOT_YET */
#define H5O_STORAGE_CHUNK_HAVE_CHUNK_INDEX      0x01
#define H5O_STORAGE_ALL_CHUNK_FLAGS             H5O_STORAGE_CHUNK_HAVE_CHUNK_INDEX
#endif /* NOT_YET */


/* PRIVATE PROTOTYPES */
static void *H5O_storage_decode(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    unsigned mesg_flags, unsigned *ioflags, const uint8_t *p);
static herr_t H5O_storage_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static size_t H5O_storage_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O_storage_free(void *_mesg);
static herr_t H5O_storage_delete(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh,
    void *_mesg);
static void *H5O_storage_copy_file(H5F_t *file_src, void *mesg_src,
    H5F_t *file_dst, hbool_t *recompute_size, unsigned UNUSED *mesg_flags,
    H5O_copy_t *cpy_info, void *udata, hid_t dxpl_id);
static herr_t H5O_storage_debug(H5F_t *f, hid_t dxpl_id, const void *_mesg, FILE * stream,
			       int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_STORAGE[1] = {{
    H5O_STORAGE_ID,          	/*message id number             */
    "storage",               	/*message name for debugging    */
    sizeof(H5O_storage_t),   	/*native message size           */
    0,				/* messages are sharable?       */
    H5O_storage_decode,      	/*decode message                */
    H5O_storage_encode,      	/*encode message                */
    H5O_storage_copy,        	/*copy the native value         */
    H5O_storage_size,        	/*size of message on disk       */
    H5O_storage_reset,		/*reset method                  */
    H5O_storage_free,        	/*free the struct		*/
    H5O_storage_delete,	        /* file delete method		*/
    NULL,			/* link method			*/
    NULL,			/*set share method		*/
    NULL,		    	/*can share method		*/
    NULL,			/* pre copy native value to file */
    H5O_storage_copy_file,	/* copy native value to file    */
    NULL,		        /* post copy native value to file    */
    NULL,			/* get creation index		*/
    NULL,			/* set creation index		*/
    H5O_storage_debug       	/*debug the message             */
}};


/* Declare a free list to manage the H5O_storage_t struct */
H5FL_DEFINE_STATIC(H5O_storage_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_decode
 *
 * Purpose:     Decode a data storage message and return a pointer to a
 *              new one created with malloc().
 *
 * Return:      Success:        Ptr to new message in native order.
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_storage_decode(H5F_t *f, hid_t UNUSED dxpl_id, H5O_t UNUSED *open_oh,
    unsigned UNUSED mesg_flags, unsigned UNUSED *ioflags, const uint8_t *p)
{
    H5O_storage_t *mesg = NULL; /* Message decoded */
    uint8_t version;            /* Version of message decoded */
    void *ret_value;            /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Allocate new message */
    if(NULL == (mesg = H5FL_CALLOC(H5O_storage_t)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed")

    /* decode */
    version = *p++;
    if(H5O_STORAGE_VERSION != version)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad version number for message")

    /* Layout class */
    mesg->type = (H5D_layout_t)*p++;

    /* Interpret the rest of the message according to the layout class */
    switch(mesg->type) {
        case H5D_COMPACT:
            /* Compact data size */
            UINT16DECODE(p, mesg->u.compact.size);

            /* Check for any data stored */
            if(mesg->u.compact.size > 0) {
                /* Allocate space for compact data */
                if(NULL == (mesg->u.compact.buf = H5MM_malloc(mesg->u.compact.size)))
                    HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed for compact data buffer")

                /* Compact data */
                HDmemcpy(mesg->u.compact.buf, p, mesg->u.compact.size);
                p += mesg->u.compact.size;
            } /* end if */
            break;

        case H5D_CONTIGUOUS:
            /* Contiguous storage address */
            H5F_addr_decode(f, &p, &(mesg->u.contig.addr));

            /* Contiguous storage size */
            H5F_DECODE_LENGTH(f, p, mesg->u.contig.size);
            break;

        case H5D_CHUNKED:
            {
                uint8_t flags;          /* Flags for decoding chunk feature info */

                /* Get the chunked layout flags */
                flags = *p++;

                /* Check for valid flags */
                if(flags & ~H5O_STORAGE_ALL_CHUNK_FLAGS)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "bad flag value for message")

                /* Check for chunk index */
                if(flags & H5O_STORAGE_CHUNK_HAVE_CHUNK_INDEX) {
		    /* Chunk index type */
		    mesg->u.chunk.idx_type = (H5D_chunk_index_t)*p++;
                    switch(mesg->u.chunk.idx_type) {
                        case H5D_CHUNK_IDX_NONE:
                            /* Set the chunk operations */
			    mesg->u.chunk.ops = H5D_COPS_NONE;
                            break;

                        case H5D_CHUNK_IDX_FARRAY:
                            /* Set the chunk operations */
                            mesg->u.chunk.ops = H5D_COPS_FARRAY;
                            break;

                        case H5D_CHUNK_IDX_EARRAY:
                            /* Set the chunk operations */
                            mesg->u.chunk.ops = H5D_COPS_EARRAY;
                            break;

			case H5D_CHUNK_IDX_BT2:       /* v2 B-tree index */
                            /* Set the chunk operations */
                            mesg->u.chunk.ops = H5D_COPS_BT2;
                            break;

                        default:
                            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "Invalid chunk index type")
                    } /* end switch */

                    /* Chunk index address */
                    H5F_addr_decode(f, &p, &(mesg->u.chunk.idx_addr));
                } /* end if */
            } /* end block */
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "Invalid layout class")
    } /* end switch */

    /* Set return value */
    ret_value = mesg;

done:
    if(ret_value == NULL)
        if(mesg)
            (void)H5FL_FREE(H5O_storage_t, mesg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_storage_encode(H5F_t *f, hbool_t UNUSED disable_shared, uint8_t *p,
    const void *_mesg)
{
    const H5O_storage_t   *mesg = (const H5O_storage_t *) _mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(mesg);
    HDassert(p);

    /* Message version */
    *p++ = (uint8_t)H5O_STORAGE_VERSION;

    /* Layout class */
    *p++ = mesg->type;

    /* Write out layout class specific information */
    switch(mesg->type) {
        case H5D_COMPACT:
            /* Size of raw data */
            UINT16ENCODE(p, mesg->u.compact.size);

            /* Raw data */
            if(mesg->u.compact.size > 0) {
                if(mesg->u.compact.buf)
                    HDmemcpy(p, mesg->u.compact.buf, mesg->u.compact.size);
                else
                    HDmemset(p, 0, mesg->u.compact.size);
                p += mesg->u.compact.size;
            } /* end if */
            break;

        case H5D_CONTIGUOUS:
            /* Contiguous storage address */
            H5F_addr_encode(f, &p, mesg->u.contig.addr);

            /* Contiguous storage size */
            H5F_ENCODE_LENGTH(f, p, mesg->u.contig.size);
            break;

        case H5D_CHUNKED:
            {
		/* Chunk feature flags */
		*p++ = (uint8_t)H5O_STORAGE_CHUNK_HAVE_CHUNK_INDEX;

		/* Chunk index type */
		*p++ = (uint8_t)mesg->u.chunk.idx_type;

                /* Chunk index address */
                H5F_addr_encode(f, &p, mesg->u.chunk.idx_addr);
            } /* end block */
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, FAIL, "Invalid layout class")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
void *
H5O_storage_copy(const void *_mesg, void *_dest)
{
    const H5O_storage_t    *mesg = (const H5O_storage_t *) _mesg;
    H5O_storage_t          *dest = (H5O_storage_t *) _dest;
    void                   *ret_value;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(mesg);
    if(!dest && NULL == (dest = H5FL_MALLOC(H5O_storage_t)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *mesg;

    /* Deep copy the buffer for compact datasets also */
    if(mesg->type == H5D_COMPACT) {
        if(mesg->u.compact.size > 0) {
            /* Allocate memory for the raw data */
            if(NULL == (dest->u.compact.buf = H5MM_malloc(dest->u.compact.size)))
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "unable to allocate memory for compact dataset")

            /* Copy over the raw data */
            HDmemcpy(dest->u.compact.buf, mesg->u.compact.buf, dest->u.compact.size);
        } /* end if */
        else
            HDassert(dest->u.compact.buf == NULL);
    } /* end if */

    /* Reset the pointer of the chunked storage index but not the address */
    if(dest->type == H5D_CHUNKED && dest->u.chunk.ops)
	H5D_chunk_idx_reset(&dest->u.chunk, FALSE);

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_size
 *
 * Purpose:     Returns the size of the raw message in bytes.  If it's
 *              compact dataset, the data part is also included.
 *              This function doesn't take into account message alignment.
 *
 * Return:      Success:        Message data size in bytes
 *              Failure:        0
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_storage_size(const H5F_t *f, hbool_t UNUSED disable_shared, const void *_mesg)
{
    const H5O_storage_t     *mesg = (const H5O_storage_t *) _mesg;
    size_t                  ret_value;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(mesg);

    /* Compute the storage message's encoded size in the file */
    /* (including possible compact data) */
    ret_value = H5O_storage_meta_size(f, mesg, TRUE);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_size() */


/*-------------------------------------------------------------------------
 * Function:	H5O_storage_reset
 *
 * Purpose:	Frees resources within a message, but doesn't free
 *		the message itself.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_storage_reset(void *_mesg)
{
    H5O_storage_t     *mesg = (H5O_storage_t *)_mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(mesg) {
        /* Free the compact storage buffer */
        if(H5D_COMPACT == mesg->type)
            mesg->u.compact.buf = H5MM_xfree(mesg->u.compact.buf);

        /* Reset the message */
        mesg->type = H5D_CONTIGUOUS;
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_storage_reset() */


/*-------------------------------------------------------------------------
 * Function:	H5O_storage_free
 *
 * Purpose:	Free's the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_storage_free(void *_mesg)
{
    H5O_storage_t     *mesg = (H5O_storage_t *) _mesg;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(mesg);

    /* Free resources within the message */
    if(H5O_storage_reset(mesg) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free message resources")

    (void)H5FL_FREE(H5O_storage_t, mesg);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 30, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_storage_delete(H5F_t *f, hid_t dxpl_id, H5O_t *open_oh, void *_mesg)
{
    H5O_storage_t *mesg = (H5O_storage_t *) _mesg;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(open_oh);
    HDassert(mesg);

    /* Perform different actions, depending on the type of storage */
    switch(mesg->type) {
        case H5D_COMPACT:       /* Compact data storage */
            /* Nothing required */
            break;

        case H5D_CONTIGUOUS:    /* Contiguous block on disk */
            /* Free the file space for the raw data */
            if(H5D__contig_delete(f, dxpl_id, mesg) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free raw data")
            break;

        case H5D_CHUNKED:       /* Chunked blocks on disk */
            /* Free the file space for the index & chunk raw data */
            if(H5D__chunk_delete(f, dxpl_id, open_oh, mesg) < 0)
                HGOTO_ERROR(H5E_OHDR, H5E_CANTFREE, FAIL, "unable to free raw data")
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_BADTYPE, FAIL, "not valid storage type")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_delete() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 30, 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_storage_copy_file(H5F_t *file_src, void *mesg_src, H5F_t *file_dst,
    hbool_t UNUSED *recompute_size, unsigned UNUSED *mesg_flags,
    H5O_copy_t *cpy_info, void *_udata, hid_t dxpl_id)
{
    H5D_copy_file_ud_t *udata = (H5D_copy_file_ud_t *)_udata;   /* Dataset copying user data */
    H5O_storage_t      *storage_src = (H5O_storage_t *)mesg_src;
    H5O_storage_t      *storage_dst = NULL;
    void               *ret_value;                              /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(file_src);
    HDassert(storage_src);
    HDassert(file_dst);
    HDassert(cpy_info);
    HDassert(udata);
    HDassert(udata->src_layout);
    HDassert(udata->src_layout->version >= H5O_LAYOUT_VERSION_4 );

    /* Allocate space for the destination storage */
    if(NULL == (storage_dst = H5FL_MALLOC(H5O_storage_t)))
        HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the "top level" information */
    *storage_dst = *storage_src;

    /* Copy the storage type specific information */
    switch(storage_src->type) {
        case H5D_COMPACT:
	    if(storage_src->u.compact.buf) {
            	if(NULL == (storage_dst->u.compact.buf = H5MM_malloc(storage_src->u.compact.size)))
                    HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "unable to allocate memory for compact dataset")

                /* Copy compact raw data */
                if(H5D__compact_copy(file_src, &storage_src->u.compact, file_dst, &storage_dst->u.compact, udata->src_dtype, cpy_info, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy chunked storage")

            	storage_dst->u.compact.dirty = TRUE;
	    } /* end if */
            break;

        case H5D_CONTIGUOUS:
            if(H5D__contig_is_space_alloc(storage_src)) {
                /* Copy contiguous raw data */
                if(H5D__contig_copy(file_src, &storage_src->u.contig, file_dst, &storage_dst->u.contig, udata->src_dtype, cpy_info, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy contiguous storage")
            } /* end if */
            break;

        case H5D_CHUNKED:
            if(H5D__chunk_is_space_alloc(storage_src)) {
                /* Create chunked layout */
                if(H5D__chunk_copy(file_src, &storage_src->u.chunk, &udata->src_layout->u.chunk, file_dst, &storage_dst->u.chunk, udata->src_space_extent, udata->src_dtype, udata->common.src_pline, cpy_info, dxpl_id) < 0)
                    HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "unable to copy chunked storage")
            } /* end if */
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_CANTLOAD, NULL, "Invalid layout class")
    } /* end switch */

    /* Freed by copy routine */
    udata->src_dtype = NULL;

    /* Set return value */
    ret_value = storage_dst;

done:
    if(!ret_value)
	if(storage_dst)
	    (void)H5FL_FREE(H5O_storage_t, storage_dst);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Thursday, July 30, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_storage_debug(H5F_t UNUSED *f, hid_t UNUSED dxpl_id, const void *_mesg,
    FILE * stream, int indent, int fwidth)
{
    const H5O_storage_t     *mesg = (const H5O_storage_t *) _mesg;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(mesg);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
              "Version:", (unsigned)H5O_STORAGE_VERSION);
    switch(mesg->type) {
        case H5D_CHUNKED:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                      "Type:", "Chunked");

            /* Index information */
            switch(mesg->u.chunk.idx_type) {
                case H5D_CHUNK_IDX_BTREE:
                    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                              "Index Type:", "v1 B-tree");
                    break;

                case H5D_CHUNK_IDX_NONE:
                    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                              "Index Type:", "None");
                    break;

                case H5D_CHUNK_IDX_FARRAY:
                    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                              "Index Type:", "Fixed Array");
                    break;

                case H5D_CHUNK_IDX_EARRAY:
                    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                              "Index Type:", "Extensible Array");
                    break;

		case H5D_CHUNK_IDX_BT2:
                    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                              "Index Type:", "v2 B-tree");
                    break;

                default:
                    HDfprintf(stream, "%*s%-*s %s (%u)\n", indent, "", fwidth,
                              "Index Type:", "Unknown", (unsigned)mesg->u.chunk.idx_type);
                    break;
            } /* end switch */
            HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                      "Index address:", mesg->u.chunk.idx_addr);
            break;

        case H5D_CONTIGUOUS:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                      "Type:", "Contiguous");
            HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                      "Data address:", mesg->u.contig.addr);
            HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
                      "Data Size:", mesg->u.contig.size);
            break;

        case H5D_COMPACT:
            HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
                      "Type:", "Compact");
            HDfprintf(stream, "%*s%-*s %Zu\n", indent, "", fwidth,
                      "Data Size:", mesg->u.compact.size);
            break;

        default:
            HDfprintf(stream, "%*s%-*s %s (%u)\n", indent, "", fwidth,
                      "Type:", "Unknown", (unsigned)mesg->type);
            break;
    } /* end switch */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_storage_debug() */


/*-------------------------------------------------------------------------
 * Function:    H5O_storage_meta_size
 *
 * Purpose:     Returns the size of the raw message in bytes except raw data
 *              part for compact dataset.  This function doesn't take into
 *              account message alignment.
 *
 * Return:      Success:        Message data size in bytes
 *              Failure:        0
 *
 * Programmer:  Quincey Koziol
 *              July 29, 2009
 *
 *-------------------------------------------------------------------------
 */
size_t
H5O_storage_meta_size(const H5F_t *f, const H5O_storage_t *storage,
    hbool_t include_compact_data)
{
    size_t                  ret_value;

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(storage);

    ret_value = 1 +             /* Version number                       */
                1;              /* Layout class type                    */

    switch(storage->type) {
        case H5D_COMPACT:
            /* Size of raw data */
            ret_value += 2;
            if(include_compact_data)
                ret_value += storage->u.compact.size;/* data for compact dataset             */
            break;

        case H5D_CONTIGUOUS:
            ret_value += H5F_SIZEOF_ADDR(f);    /* Address of data */
            ret_value += H5F_SIZEOF_SIZE(f);    /* Length of data */
            break;

        case H5D_CHUNKED:
            ret_value += 1 +                    /* Chunk feature flags */
                         1;                     /* Chunk index type */
            ret_value += H5F_SIZEOF_ADDR(f);    /* Chunk index address or data address */
            break;

        default:
            HGOTO_ERROR(H5E_OHDR, H5E_CANTENCODE, 0, "Invalid layout class")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_storage_meta_size() */

