/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Data filter pipeline message
 */

#include "H5Omodule.h" /* This source code file is part of the H5O module */
#define H5Z_FRIEND     /*suppress error about including H5Zpkg      */

#include "H5private.h"   /* Generic Functions            */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5FLprivate.h" /* Free Lists                   */
#include "H5HGprivate.h" /* Global Heaps                 */
#include "H5MMprivate.h" /* Memory management            */
#include "H5Opkg.h"      /* Object headers               */
#include "H5Zpkg.h"      /* Data filters                 */

/* PRIVATE PROTOTYPES */
static herr_t H5O__pline_encode(H5F_t *f, uint8_t *p, const void *mesg);
static void  *H5O__pline_decode(H5F_t *f, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags,
                                size_t p_size, const uint8_t *p);
static void  *H5O__pline_copy(const void *_mesg, void *_dest);
static size_t H5O__pline_size(const H5F_t *f, const void *_mesg);
static herr_t H5O__pline_reset(void *_mesg);
static herr_t H5O__pline_free(void *_mesg);
static herr_t H5O__pline_delete(H5F_t *f, H5O_t *open_oh, void *_mesg);
static herr_t H5O__pline_pre_copy_file(H5F_t *file_src, const void *mesg_src, bool *deleted,
                                       const H5O_copy_t *cpy_info, void *_udata);
static void  *H5O__pline_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type, void *native_src,
                                   H5F_t *file_dst, bool *recompute_size, H5O_copy_t *cpy_info,
                                   void *udata);
static herr_t H5O__pline_debug(H5F_t *f, const void *_mesg, FILE *stream, int indent, int fwidth);

/* Set up & include shared message "interface" info */
#define H5O_SHARED_TYPE        H5O_MSG_PLINE
#define H5O_SHARED_DECODE      H5O__pline_shared_decode
#define H5O_SHARED_DECODE_REAL H5O__pline_decode
#define H5O_SHARED_ENCODE      H5O__pline_shared_encode
#define H5O_SHARED_ENCODE_REAL H5O__pline_encode
#define H5O_SHARED_SIZE        H5O__pline_shared_size
#define H5O_SHARED_SIZE_REAL   H5O__pline_size
#define H5O_SHARED_DELETE      H5O__pline_shared_delete
#define H5O_SHARED_DELETE_REAL H5O__pline_delete
#define H5O_SHARED_LINK        H5O__pline_shared_link
#undef H5O_SHARED_LINK_REAL
#define H5O_SHARED_COPY_FILE      H5O__pline_shared_copy_file
#define H5O_SHARED_COPY_FILE_REAL H5O__pline_copy_file
#define H5O_SHARED_POST_COPY_FILE H5O__pline_shared_post_copy_file
#undef H5O_SHARED_POST_COPY_FILE_REAL
#undef H5O_SHARED_POST_COPY_FILE_UPD
#define H5O_SHARED_DEBUG      H5O__pline_shared_debug
#define H5O_SHARED_DEBUG_REAL H5O__pline_debug
#include "H5Oshared.h" /* Shared Object Header Message Callbacks */

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_PLINE[1] = {{
    H5O_PLINE_ID,                              /* message id number            */
    "filter pipeline",                         /* message name for debugging   */
    sizeof(H5O_pline_t),                       /* native message size          */
    H5O_SHARE_IS_SHARABLE | H5O_SHARE_IN_OHDR, /* messages are shareable?       */
    H5O__pline_shared_decode,                  /* decode message               */
    H5O__pline_shared_encode,                  /* encode message               */
    H5O__pline_copy,                           /* copy the native value        */
    H5O__pline_shared_size,                    /* size of raw message          */
    H5O__pline_reset,                          /* reset method                 */
    H5O__pline_free,                           /* free method                  */
    H5O__pline_shared_delete,                  /* file delete method           */
    H5O__pline_shared_link,                    /* link method                  */
    NULL,                                      /* set share method             */
    NULL,                                      /*can share method              */
    H5O__pline_pre_copy_file,                  /* pre copy native value to file */
    H5O__pline_shared_copy_file,               /* copy native value to file    */
    H5O__pline_shared_post_copy_file,          /* post copy native value to file */
    NULL,                                      /* get creation index           */
    NULL,                                      /* set creation index           */
    H5O__pline_shared_debug                    /* debug the message            */
}};

/* Format version bounds for filter pipeline */
const unsigned H5O_pline_ver_bounds[] = {
    H5O_PLINE_VERSION_1,     /* H5F_LIBVER_EARLIEST */
    H5O_PLINE_VERSION_2,     /* H5F_LIBVER_V18 */
    H5O_PLINE_VERSION_2,     /* H5F_LIBVER_V110 */
    H5O_PLINE_VERSION_2,     /* H5F_LIBVER_V112 */
    H5O_PLINE_VERSION_2,     /* H5F_LIBVER_V114 */
    H5O_PLINE_VERSION_2,     /* H5F_LIBVER_V200 */
    H5O_PLINE_VERSION_3,     /* H5F_LIBVER_V300 */
    H5O_PLINE_VERSION_LATEST /* H5F_LIBVER_LATEST */
};

/* Declare a free list to manage the H5O_pline_t struct */
H5FL_DEFINE(H5O_pline_t);

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_decode
 *
 * Purpose:     Decodes a filter pipeline message.
 *
 * Return:      Success:    Pointer to a new pipeline message
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */

static void *
H5O__pline_decode(H5F_t *f, H5O_t H5_ATTR_UNUSED *open_oh, unsigned H5_ATTR_UNUSED mesg_flags,
                  unsigned H5_ATTR_UNUSED *ioflags, size_t p_size, const uint8_t *p)
{
    H5O_pline_t       *pline = NULL;               /* Pipeline message */
    H5Z_filter_info_t *filter;                     /* Filter to decode */
    size_t             name_length;                /* Length of filter name */
    size_t             i;                          /* Local index variable */
    const uint8_t     *p_end     = p + p_size - 1; /* End of the p buffer */
    void              *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(f);
    assert(p);

    /* Allocate space for I/O pipeline message */
    if (NULL == (pline = H5FL_CALLOC(H5O_pline_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Version */
    if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
    pline->version = *p++;
    if (pline->version < H5O_PLINE_VERSION_1 || pline->version > H5O_PLINE_VERSION_LATEST)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL, "bad version number for filter pipeline message");

    /* Number of filters */
    if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
    pline->nused = *p++;
    if (pline->nused > H5Z_MAX_NFILTERS) {

        /* Reset the number of filters used to avoid array traversal in error
         * handling code.
         */
        pline->nused = 0;

        HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL, "filter pipeline message has too many filters");
    }

    /* Reserved */
    if (pline->version == H5O_PLINE_VERSION_1) {
        if (H5_IS_BUFFER_OVERFLOW(p, 6, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        p += 6;
    }

    /* Allocate array for filters */
    pline->nalloc = pline->nused;
    if (NULL == (pline->filter = (H5Z_filter_info_t *)H5MM_calloc(pline->nalloc * sizeof(pline->filter[0]))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Decode filters */
    for (i = 0, filter = &pline->filter[0]; i < pline->nused; i++, filter++) {
        /* Filter ID */
        if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        UINT16DECODE(p, filter->id);

        /* Length of filter name */
        if (pline->version > H5O_PLINE_VERSION_1 && filter->id < H5Z_FILTER_RESERVED)
            name_length = 0;
        else {
            if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
                HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
            UINT16DECODE(p, name_length);
            if (pline->version == H5O_PLINE_VERSION_1 && name_length % 8)
                HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL, "filter name length is not a multiple of eight");
        }

        /* Filter flags */
        if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        UINT16DECODE(p, filter->flags);

        /* Number of filter parameters ("client data elements") */
        if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        UINT16DECODE(p, filter->cd_nelmts);

        /* Filter name, if there is one */
        if (name_length) {
            size_t actual_name_length;            /* Actual length of name */
            size_t max = (size_t)(p_end - p + 1); /* Max possible name length */

            /* Determine actual name length (without padding, but with null terminator) */
            actual_name_length = strnlen((const char *)p, max);
            if (actual_name_length == max)
                HGOTO_ERROR(H5E_OHDR, H5E_NOSPACE, NULL, "filter name not null terminated");
            actual_name_length += 1; /* include \0 byte */

            /* Allocate space for the filter name, or use the internal buffer */
            if (actual_name_length > H5Z_COMMON_NAME_LEN) {
                filter->name = (char *)H5MM_malloc(actual_name_length);
                if (NULL == filter->name)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for filter name");
            }
            else
                filter->name = filter->_name;

            strncpy(filter->name, (const char *)p, actual_name_length);

            if (H5_IS_BUFFER_OVERFLOW(p, name_length, p_end))
                HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
            p += name_length;
        }

        /* Filter parameters */
        if (filter->cd_nelmts) {

            /* Allocate space for the client data elements, or use the internal buffer */
            if (filter->cd_nelmts > H5Z_COMMON_CD_VALUES) {
                filter->cd_values = (unsigned *)H5MM_malloc(filter->cd_nelmts * sizeof(unsigned));
                if (NULL == filter->cd_values)
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for client data");
            }
            else
                filter->cd_values = filter->_cd_values;

            /* Read the client data values and the padding */
            for (size_t j = 0; j < filter->cd_nelmts; j++) {
                if (H5_IS_BUFFER_OVERFLOW(p, 4, p_end))
                    HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
                UINT32DECODE(p, filter->cd_values[j]);
            }

            if (pline->version == H5O_PLINE_VERSION_1)
                if (filter->cd_nelmts % 2) {
                    if (H5_IS_BUFFER_OVERFLOW(p, 4, p_end))
                        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL,
                                    "ran off end of input buffer while decoding");
                    p += 4; /* padding */
                }
        }

        /* Extension blocks, for version 3+.  Every block is skippable using
         * its length alone, so an unrecognised non-critical type costs the
         * decoder nothing; an unrecognised critical type is fatal. */
        filter->aux_loc.addr = HADDR_UNDEF;
        if (pline->version >= H5O_PLINE_VERSION_3) {
            unsigned ext_count;
            unsigned prev_type = 0; /* enforces ascending order & no dups */

            if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
                HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
            UINT16DECODE(p, ext_count);

            for (size_t k = 0; k < ext_count; k++) {
                unsigned ext_type;
                uint8_t  ext_flags;
                uint8_t  ext_reserved;
                uint32_t ext_length;

                if (H5_IS_BUFFER_OVERFLOW(p, H5O_PLINE_EXT_HDR_SIZE, p_end))
                    HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
                UINT16DECODE(p, ext_type);
                ext_flags    = *p++;
                ext_reserved = *p++;
                UINT32DECODE(p, ext_length);

                if (0 != ext_reserved)
                    HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL,
                                "filter extension block reserved byte is not zero");

                /* Blocks are written in ascending type order and a type
                 * appears at most once, so a non-increasing type means the
                 * message is malformed rather than merely unfamiliar. */
                if (ext_type <= prev_type)
                    HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL,
                                "filter extension blocks are out of order or duplicated "
                                "(type 0x%04x after 0x%04x)",
                                ext_type, prev_type);
                prev_type = ext_type;

                if (H5_IS_BUFFER_OVERFLOW(p, ext_length, p_end))
                    HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");

                switch (ext_type) {
                    case H5O_PLINE_EXT_CONFIG:
                        if (ext_length > H5Z_CONFIG_STRING_MAX)
                            HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL,
                                        "filter config string exceeds maximum length");
                        if (ext_length) {
                            /* Stored without a NUL terminator; add one here */
                            if (NULL == (filter->config = (char *)H5MM_malloc(ext_length + 1)))
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                            "memory allocation failed for filter config string");
                            H5MM_memcpy(filter->config, p, ext_length);
                            filter->config[ext_length] = '\0';
                        }
                        break;

                    case H5O_PLINE_EXT_BLOB: {
                        const uint8_t *bp = p; /* leave p to the common advance below */
                        uint32_t       idx;

                        if (ext_length != (uint32_t)(H5F_SIZEOF_ADDR(f) + 4))
                            HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL,
                                        "filter blob locator has unexpected length");
                        H5F_addr_decode(f, &bp, &filter->aux_loc.addr);
                        UINT32DECODE(bp, idx);
                        filter->aux_loc.idx           = (size_t)idx;
                        filter->blob_default_storage  = (ext_flags & H5O_PLINE_EXT_BLOB_FLAG_DEFAULT_STORAGE) != 0;
                        break;
                    }

                    default:
                        /* Unknown type.  Skipping is safe only if the writer
                         * said so; a critical block carries something this
                         * build cannot reconstruct, and proceeding would
                         * filter data with an incomplete configuration. */
                        if (ext_flags & H5O_PLINE_EXT_FLAG_CRITICAL)
                            HGOTO_ERROR(H5E_PLINE, H5E_CANTLOAD, NULL,
                                        "unsupported critical filter extension block "
                                        "(type 0x%04x on filter %u)",
                                        ext_type, (unsigned)filter->id);
                        break;
                }

                p += ext_length;
            }
        }
    }

    /* Set return value */
    ret_value = pline;

done:
    if (!ret_value && pline) {
        H5O__pline_reset(pline);
        H5O__pline_free(pline);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_encode
 *
 * Purpose:    Encodes message MESG into buffer P.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_encode(H5F_t *f, uint8_t *p /*out*/, const void *mesg)
{
    const H5O_pline_t       *pline = (const H5O_pline_t *)mesg; /* Pipeline message to encode */
    const H5Z_filter_info_t *filter;                            /* Filter to encode */
    size_t                   i, j;                              /* Local index variables */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check args */
    assert(p);
    assert(mesg);

    /* Message header */
    *p++ = (uint8_t)pline->version;
    *p++ = (uint8_t)(pline->nused);
    if (pline->version == H5O_PLINE_VERSION_1) {
        *p++ = 0; /*reserved 1*/
        *p++ = 0; /*reserved 2*/
        *p++ = 0; /*reserved 3*/
        *p++ = 0; /*reserved 4*/
        *p++ = 0; /*reserved 5*/
        *p++ = 0; /*reserved 6*/
    }             /* end if */

    /* Encode filters */
    for (i = 0, filter = &pline->filter[0]; i < pline->nused; i++, filter++) {
        const char *name;        /* Filter name */
        size_t      name_length; /* Length of filter name */

        /* Filter ID */
        UINT16ENCODE(p, filter->id);

        /* Skip writing the name length & name if the filter is an internal filter */
        if (pline->version > H5O_PLINE_VERSION_1 && filter->id < H5Z_FILTER_RESERVED) {
            name_length = 0;
            name        = NULL;
        } /* end if */
        else {
            /*
             * Get the filter name.  If the pipeline message has a name in it then
             * use that one.  Otherwise try to look up the filter and get the name
             * as it was registered.
             */
            if (NULL == (name = filter->name)) {
                H5Z_class2_t *cls; /* Filter class */

                H5Z_find(true, filter->id, &cls);
                if (cls)
                    name = cls->name;
            }
            name_length = name ? strlen(name) + 1 : 0;

            /* Filter name length */
            UINT16ENCODE(p, pline->version == H5O_PLINE_VERSION_1 ? H5O_ALIGN_OLD(name_length) : name_length);
        } /* end else */

        /* Filter flags */
        UINT16ENCODE(p, filter->flags);

        /* # of filter parameters */
        UINT16ENCODE(p, filter->cd_nelmts);

        /* Encode name, if there is one to encode */
        if (name_length > 0) {
            /* Store name, with null terminator */
            H5MM_memcpy(p, name, name_length);
            p += name_length;

            /* Pad out name to alignment, in older versions */
            if (pline->version == H5O_PLINE_VERSION_1)
                while (name_length++ % 8)
                    *p++ = 0;
        } /* end if */

        /* Filter parameters */
        for (j = 0; j < filter->cd_nelmts; j++)
            UINT32ENCODE(p, filter->cd_values[j]);

        /* Align the parameters for older versions of the format */
        if (pline->version == H5O_PLINE_VERSION_1)
            if (filter->cd_nelmts % 2)
                UINT32ENCODE(p, 0);

        /* Extension blocks, for version 3+.  Emitted in ascending type
         * order so that an entry encodes identically no matter what order
         * the library populated it in. */
        if (pline->version >= H5O_PLINE_VERSION_3) {
            size_t config_length = filter->config ? strlen(filter->config) : 0;
            bool   have_blob     = H5_addr_defined(filter->aux_loc.addr);

            UINT16ENCODE(p, (unsigned)((config_length ? 1 : 0) + (have_blob ? 1 : 0)));

            /* Config string: non-critical.  A reader that skips it loses
             * introspection fidelity, not correctness -- cd_values are
             * complete on their own.  Written without a NUL terminator. */
            if (config_length > 0) {
                UINT16ENCODE(p, H5O_PLINE_EXT_CONFIG);
                *p++ = 0; /* flags: not critical */
                *p++ = 0; /* reserved          */
                UINT32ENCODE(p, (uint32_t)config_length);
                H5MM_memcpy(p, filter->config, config_length);
                p += config_length;
            }

            /* Blob locator: critical.  The blob holds configuration the
             * filter cannot run without, so a reader that does not
             * understand this block must not fall back to cd_values.  The
             * locator is defined only once the blob has been written at
             * dataset-creation time. */
            if (have_blob) {
                UINT16ENCODE(p, H5O_PLINE_EXT_BLOB);
                *p++ = (uint8_t)(H5O_PLINE_EXT_FLAG_CRITICAL |
                                (filter->blob_default_storage ? H5O_PLINE_EXT_BLOB_FLAG_DEFAULT_STORAGE : 0));
                *p++ = 0; /* reserved */
                UINT32ENCODE(p, (uint32_t)(H5F_SIZEOF_ADDR(f) + 4));
                H5F_addr_encode(f, &p, filter->aux_loc.addr);
                UINT32ENCODE(p, (uint32_t)filter->aux_loc.idx);
            }
        }
    } /* end for */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__pline_encode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_copy
 *
 * Purpose:    Copies a filter pipeline message from SRC to DST allocating
 *        DST if necessary.  If DST is already allocated then we assume
 *        that it isn't initialized.
 *
 * Return:    Success:    Ptr to DST or allocated result.
 *
 *        Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__pline_copy(const void *_src, void *_dst /*out*/)
{
    const H5O_pline_t *src = (const H5O_pline_t *)_src; /* Source pipeline message */
    H5O_pline_t       *dst = (H5O_pline_t *)_dst;       /* Destination pipeline message */
    size_t             i;                               /* Local index variable */
    H5O_pline_t       *ret_value = NULL;                /* Return value */

    FUNC_ENTER_PACKAGE

    /* Allocate pipeline message, if not provided */
    if (!dst && NULL == (dst = H5FL_MALLOC(H5O_pline_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Shallow copy basic fields */
    *dst = *src;

    /* Copy over filters, if any */
    dst->nalloc = dst->nused;
    if (dst->nalloc) {
        /* Allocate array to hold filters */
        if (NULL == (dst->filter = (H5Z_filter_info_t *)H5MM_calloc(dst->nalloc * sizeof(dst->filter[0]))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

        /* Deep-copy filters */
        for (i = 0; i < src->nused; i++) {
            /* Basic filter information */
            dst->filter[i] = src->filter[i];

            /* The struct copy above just aliased src->filter[i].aux without
             * incrementing its refcount; null it out immediately so that if
             * any allocation below fails, done:'s H5O__pline_reset(dst) --
             * which releases every entry up to and including this one --
             * finds a NULL aux here and safely no-ops instead of decrementing
             * a reference this copy was never given (which, if it dropped
             * the count to zero, would free a buffer SRC still points at).
             * The real reference is taken further down, once every fallible
             * allocation for this entry has already succeeded. */
            dst->filter[i].aux = NULL;

            /* Filter name */
            if (src->filter[i].name) {
                size_t namelen; /* Length of source filter name, including null terminator  */

                namelen = strlen(src->filter[i].name) + 1;

                /* Allocate space for the filter name, or use the internal buffer */
                if (namelen > H5Z_COMMON_NAME_LEN) {
                    dst->filter[i].name = (char *)H5MM_strdup(src->filter[i].name);
                    if (NULL == dst->filter[i].name)
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                    "memory allocation failed for filter name");
                } /* end if */
                else
                    dst->filter[i].name = dst->filter[i]._name;
            } /* end if */

            /* Filter parameters */
            if (src->filter[i].cd_nelmts > 0) {
                /* Allocate space for the client data elements, or use the internal buffer */
                if (src->filter[i].cd_nelmts > H5Z_COMMON_CD_VALUES) {
                    if (NULL == (dst->filter[i].cd_values =
                                     (unsigned *)H5MM_malloc(src->filter[i].cd_nelmts * sizeof(unsigned))))
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

                    H5MM_memcpy(dst->filter[i].cd_values, src->filter[i].cd_values,
                                src->filter[i].cd_nelmts * sizeof(unsigned));
                } /* end if */
                else
                    dst->filter[i].cd_values = dst->filter[i]._cd_values;
            } /* end if */

            /* Verbatim configuration string */
            if (src->filter[i].config) {
                if (NULL == (dst->filter[i].config = (char *)H5MM_strdup(src->filter[i].config)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                "memory allocation failed for filter config string");
            } /* end if */

            /* Blob bytes: share the reference-counted buffer instead of
             * deep-copying it -- the buffer is immutable once created, so
             * sharing across this pipeline's copies is safe.  The on-disk
             * locator is copied verbatim but is only meaningful within the
             * originating file; dataset creation always writes the blob
             * afresh and assigns a new locator in the destination. */
            if (src->filter[i].aux) {
                H5Z_blob_buf_incref(src->filter[i].aux);
                dst->filter[i].aux = src->filter[i].aux;
            } /* end if */
        }     /* end for */
    }         /* end if */
    else
        dst->filter = NULL;

    /* Set return value */
    ret_value = dst;

done:
    if (!ret_value && dst) {
        H5O__pline_reset(dst);
        if (!_dst)
            H5O__pline_free(dst);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_copy_file
 *
 * Purpose:    Copies a filter pipeline message from one file to another,
 *        as part of H5Ocopy().  Every filter blob (H5Z_filter_info_t.aux)
 *        is re-persisted into FILE_DST and assigned a fresh locator there
 *        -- H5O__pline_copy()'s in-memory copy shares SRC's on-disk
 *        locator (aux_loc) verbatim, but that locator is only meaningful
 *        within the file it was written to, so it must never reach
 *        FILE_DST unmodified.  Reuses H5Z_blob_write(), the same routine
 *        H5D__create() calls for a newly created dataset, so a blob
 *        attached via a filter's custom write_blob callback is
 *        re-invoked against FILE_DST rather than the destination
 *        silently inheriting bytes that only make sense in the source
 *        file.
 *
 * Return:    Success:    Ptr to the newly allocated destination message.
 *
 *        Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__pline_copy_file(H5F_t H5_ATTR_UNUSED *file_src, const H5O_msg_class_t H5_ATTR_UNUSED *mesg_type,
                     void *native_src, H5F_t *file_dst, bool H5_ATTR_UNUSED *recompute_size,
                     H5O_copy_t H5_ATTR_UNUSED *cpy_info, void H5_ATTR_UNUSED *udata)
{
    H5O_pline_t *dst_pline = NULL;
    void        *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(native_src);
    assert(file_dst);

    if (NULL == (dst_pline = (H5O_pline_t *)H5O__pline_copy(native_src, NULL)))
        HGOTO_ERROR(H5E_PLINE, H5E_CANTCOPY, NULL, "unable to copy pipeline message");

    if (H5Z_blob_write(file_dst, dst_pline) < 0)
        HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, NULL, "unable to write filter blob to destination file");

    ret_value = dst_pline;

done:
    if (!ret_value && dst_pline) {
        H5O__pline_reset(dst_pline);
        H5O__pline_free(dst_pline);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_size
 *
 * Purpose:    Determines the size of a raw filter pipeline message.
 *
 * Return:    Success:    Size of message.
 *
 *        Failure:    zero
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O__pline_size(const H5F_t *f, const void *mesg)
{
    const H5O_pline_t *pline = (const H5O_pline_t *)mesg; /* Pipeline message */
    size_t             i;                                 /* Local index variable */
    size_t             ret_value = 0;                     /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* Message header */
    ret_value = (size_t)(1 +                                               /*version            */
                         1 +                                               /*number of filters        */
                         (pline->version == H5O_PLINE_VERSION_1 ? 6 : 0)); /*reserved            */

    /* Calculate size of each filter in pipeline */
    for (i = 0; i < pline->nused; i++) {
        size_t      name_len; /* Length of filter name */
        const char *name;     /* Filter name */

        /* Don't write the name length & name if the filter is an internal filter */
        if (pline->version > H5O_PLINE_VERSION_1 && pline->filter[i].id < H5Z_FILTER_RESERVED)
            name_len = 0;
        else {
            /* Get the name of the filter, same as done with H5O__pline_encode() */
            if (NULL == (name = pline->filter[i].name)) {
                H5Z_class2_t *cls; /* Filter class */

                H5Z_find(true, pline->filter[i].id, &cls);
                if (cls)
                    name = cls->name;
            }
            name_len = name ? strlen(name) + 1 : 0;
        } /* end else */

        ret_value +=
            2 + /*filter identification number    */
            (size_t)((pline->version == H5O_PLINE_VERSION_1 || pline->filter[i].id >= H5Z_FILTER_RESERVED)
                         ? 2
                         : 0) + /*name length            */
            2 +                 /*flags                */
            2 +                 /*number of client data values    */
            (pline->version == H5O_PLINE_VERSION_1 ? (size_t)H5O_ALIGN_OLD(name_len)
                                                   : name_len); /*length of the filter name    */

        ret_value += pline->filter[i].cd_nelmts * 4;
        if (pline->version == H5O_PLINE_VERSION_1)
            if (pline->filter[i].cd_nelmts % 2)
                ret_value += 4;

        /* Extension block list, for version 3+: 2-byte count, then framing
         * plus payload for each block actually written */
        if (pline->version >= H5O_PLINE_VERSION_3) {
            size_t config_length = pline->filter[i].config ? strlen(pline->filter[i].config) : 0;

            ret_value += 2; /* ext_count */
            if (config_length)
                ret_value += H5O_PLINE_EXT_HDR_SIZE + config_length;
            if (H5_addr_defined(pline->filter[i].aux_loc.addr))
                ret_value += H5O_PLINE_EXT_HDR_SIZE + (size_t)H5F_SIZEOF_ADDR(f) + 4;
        }
    } /* end for */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_size() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_reset
 *
 * Purpose:    Resets a filter pipeline message by clearing all filters.
 *        The MESG buffer is not freed.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_reset(void *mesg)
{
    H5O_pline_t *pline = (H5O_pline_t *)mesg; /* Pipeline message */
    size_t       i;                           /* Local index variable */

    FUNC_ENTER_PACKAGE_NOERR

    /* NOTE: This function can be called during error processing from
     *       other API calls so DO NOT ASSUME THAT ANY VALUES ARE SANE.
     */

    assert(pline);

    /* Free the filter information and array */
    if (pline->filter) {
        /* Free information for each filter */
        for (i = 0; i < pline->nused; i++) {
            if (pline->filter[i].name && pline->filter[i].name != pline->filter[i]._name)
                assert((strlen(pline->filter[i].name) + 1) > H5Z_COMMON_NAME_LEN);
            if (pline->filter[i].name != pline->filter[i]._name)
                pline->filter[i].name = (char *)H5MM_xfree(pline->filter[i].name);
            if (pline->filter[i].cd_values && pline->filter[i].cd_values != pline->filter[i]._cd_values)
                assert(pline->filter[i].cd_nelmts > H5Z_COMMON_CD_VALUES);
            if (pline->filter[i].cd_values != pline->filter[i]._cd_values)
                pline->filter[i].cd_values = (unsigned *)H5MM_xfree(pline->filter[i].cd_values);
            pline->filter[i].config = (char *)H5MM_xfree(pline->filter[i].config);

            H5Z_blob_release(&pline->filter[i]);
        } /* end for */

        /* Free filter array */
        pline->filter = (H5Z_filter_info_t *)H5MM_xfree(pline->filter);
    }

    /* Reset # of filters */
    pline->nused = pline->nalloc = 0;

    /* Reset version # of pipeline message */
    pline->version = H5O_PLINE_VERSION_1;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__pline_reset() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_free
 *
 * Purpose:    Frees the message
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_free(void *mesg)
{
    FUNC_ENTER_PACKAGE_NOERR

    assert(mesg);

    mesg = H5FL_FREE(H5O_pline_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__pline_free() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_delete
 *
 * Purpose:     Free file space referenced by the message: each blob-bearing
 *              filter's global-heap object is removed, analogous to how the
 *              fill-value message's delete handler frees vlen data.
 *
 *              Filters that implement a custom write_blob callback own
 *              their on-disk layout; the library cannot reclaim storage it
 *              did not allocate, so those locators are skipped.  Which case
 *              applies is read from H5Z_filter_info_t.blob_default_storage
 *              (persisted on disk in the BLOB extension block's flags byte
 *              and populated at decode time) rather than inferred from
 *              whether the filter happens to be registered right now: the
 *              owning filter's current registration state has nothing to
 *              do with which storage scheme actually wrote the bytes, and
 *              treating "not registered" as "must be default storage"
 *              risks calling H5HG_remove() on an opaque value a custom
 *              write_blob produced.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_delete(H5F_t *f, H5O_t H5_ATTR_UNUSED *open_oh, void *_mesg)
{
    H5O_pline_t *pline     = (H5O_pline_t *)_mesg; /* Pipeline message */
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    assert(f);
    assert(pline);

    for (size_t i = 0; i < pline->nused; i++) {
        H5HG_t hobj;

        if (!H5_addr_defined(pline->filter[i].aux_loc.addr))
            continue;

        /* Custom-storage filters own their on-disk layout; the library
         * cannot reclaim it. */
        if (!pline->filter[i].blob_default_storage)
            continue;

        hobj.addr = pline->filter[i].aux_loc.addr;
        hobj.idx  = pline->filter[i].aux_loc.idx;
        if (H5HG_remove(f, &hobj) < 0)
            HGOTO_ERROR(H5E_PLINE, H5E_CANTREMOVE, FAIL, "unable to remove filter blob from global heap");
        pline->filter[i].aux_loc.addr = HADDR_UNDEF;
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_delete() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_pre_copy_file
 *
 * Purpose:     Perform any necessary actions before copying message between
 *              files
 *
 * Return:      Success:        Non-negative
 *
 *              Failure:        Negative
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_pre_copy_file(H5F_t H5_ATTR_UNUSED *file_src, const void *mesg_src, bool H5_ATTR_UNUSED *deleted,
                         const H5O_copy_t *cpy_info, void *_udata)
{
    const H5O_pline_t         *pline_src = (const H5O_pline_t *)mesg_src;       /* Source pline */
    H5O_copy_file_ud_common_t *udata     = (H5O_copy_file_ud_common_t *)_udata; /* Object copying user data */
    herr_t                     ret_value = SUCCEED;                             /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    assert(pline_src);
    assert(cpy_info);
    assert(cpy_info->file_dst);

    /* Check to ensure that the version of the message to be copied does not exceed
       the message version allowed by the destination file's high bound */
    if (pline_src->version > H5O_pline_ver_bounds[H5F_HIGH_BOUND(cpy_info->file_dst)])
        HGOTO_ERROR(H5E_OHDR, H5E_BADRANGE, FAIL, "pline message version out of bounds");

    /* If the user data is non-NULL, assume we are copying a dataset or group
     * and make a copy of the filter pipeline for later in
     * the object copying process.
     */
    if (udata)
        if (NULL == (udata->src_pline = (H5O_pline_t *)H5O__pline_copy(pline_src, NULL)))
            HGOTO_ERROR(H5E_PLINE, H5E_CANTINIT, FAIL, "unable to copy");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__pline_pre_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__pline_debug
 *
 * Purpose:    Prints debugging information for filter pipeline message MESG
 *        on output stream STREAM.  Each line is indented INDENT
 *        characters and the field name takes up FWIDTH characters.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__pline_debug(H5F_t H5_ATTR_UNUSED *f, const void *mesg, FILE *stream, int indent, int fwidth)
{
    const H5O_pline_t *pline = (const H5O_pline_t *)mesg;

    FUNC_ENTER_PACKAGE_NOERR

    /* check args */
    assert(f);
    assert(pline);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    fprintf(stream, "%*s%-*s %zu/%zu\n", indent, "", fwidth, "Number of filters:", pline->nused,
            pline->nalloc);

    /* Loop over all the filters */
    for (size_t i = 0; i < pline->nused; i++) {
        /* 19 characters for text + 20 characters for largest 64-bit size_t +
         * terminal NUL = 40 characters.
         */
        char name[64];

        memset(name, 0, 64);
        snprintf(name, sizeof(name), "Filter at position %zu", i);

        fprintf(stream, "%*s%-*s\n", indent, "", fwidth, name);
        fprintf(stream, "%*s%-*s 0x%04x\n", indent + 3, "", MAX(0, fwidth - 3),
                "Filter identification:", (unsigned)(pline->filter[i].id));
        if (pline->filter[i].name)
            fprintf(stream, "%*s%-*s \"%s\"\n", indent + 3, "", MAX(0, fwidth - 3),
                    "Filter name:", pline->filter[i].name);
        else
            fprintf(stream, "%*s%-*s NONE\n", indent + 3, "", MAX(0, fwidth - 3), "Filter name:");
        fprintf(stream, "%*s%-*s 0x%04x\n", indent + 3, "", MAX(0, fwidth - 3),
                "Flags:", pline->filter[i].flags);
        fprintf(stream, "%*s%-*s %zu\n", indent + 3, "", MAX(0, fwidth - 3),
                "Num CD values:", pline->filter[i].cd_nelmts);

        /* Filter parameters */
        for (size_t j = 0; j < pline->filter[i].cd_nelmts; j++) {
            char field_name[32];

            snprintf(field_name, sizeof(field_name), "CD value %lu", (unsigned long)j);
            fprintf(stream, "%*s%-*s %u\n", indent + 6, "", MAX(0, fwidth - 6), field_name,
                    pline->filter[i].cd_values[j]);
        }
    }

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__pline_debug() */

/*-------------------------------------------------------------------------
 * Function:    H5O_pline_set_version
 *
 * Purpose:     Set the version to encode an I/O filter pipeline with.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_pline_set_version(H5F_t *f, H5O_pline_t *pline)
{
    unsigned version;             /* Message version */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    assert(f);
    assert(pline);

    /* Upgrade to the version indicated by the file's low bound if higher */
    version = MAX(pline->version, H5O_pline_ver_bounds[H5F_LOW_BOUND(f)]);

    /* A filter carrying a verbatim configuration string needs the version-3
     * encoding to persist it.  Request v3 only when the file's high bound
     * admits it; otherwise fall back silently to the current version -- the
     * strings are simply not written and introspection relies on get_config. */
    if (version < H5O_PLINE_VERSION_3) {
        bool have_config = false;

        for (size_t u = 0; u < pline->nused; u++)
            if (pline->filter[u].config) {
                have_config = true;
                break;
            }

        if (have_config && H5O_pline_ver_bounds[H5F_HIGH_BOUND(f)] >= H5O_PLINE_VERSION_3)
            version = H5O_PLINE_VERSION_3;
    }

    /* A blob-bearing filter requires the version-3 encoding for its locator.
     * Unlike the configuration string above, the blob bytes have nowhere
     * else to go, so this is not silently downgraded -- the version bounds
     * check below reports an error if the file's high bound doesn't admit it. */
    for (size_t u = 0; u < pline->nused; u++)
        if (pline->filter[u].aux != NULL) {
            version = MAX(version, H5O_PLINE_VERSION_3);
            break;
        }

    /* Version bounds check */
    if (version > H5O_pline_ver_bounds[H5F_HIGH_BOUND(f)])
        HGOTO_ERROR(H5E_PLINE, H5E_BADRANGE, FAIL, "Filter pipeline version out of bounds");

    /* Set the message version */
    pline->version = version;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_pline_set_version() */
