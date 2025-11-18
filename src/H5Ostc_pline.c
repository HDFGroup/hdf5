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
 * Purpose:     Data filter pipeline message for structured chunk
 */

#include "H5Omodule.h" /* This source code file is part of the H5O module */
#define H5Z_FRIEND     /*suppress error about including H5Zpkg      */

#include "H5private.h"   /* Generic Functions            */
#include "H5Eprivate.h"  /* Error handling               */
#include "H5FLprivate.h" /* Free Lists                   */
#include "H5MMprivate.h" /* Memory management            */
#include "H5Opkg.h"      /* Object headers               */
#include "H5Zpkg.h"      /* Data filters                 */

/* PRIVATE PROTOTYPES */
static herr_t H5O__stc_pline_encode(H5F_t *f, uint8_t *p, const void *mesg);
static void  *H5O__stc_pline_decode(H5F_t *f, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags,
                                size_t p_size, const uint8_t *p);
static void  *H5O__stc_pline_copy(const void *_mesg, void *_dest);
static size_t H5O__stc_pline_size(const H5F_t *f, const void *_mesg);
static herr_t H5O__stc_pline_reset(void *_mesg);
static herr_t H5O__stc_pline_free(void *_mesg);
static herr_t H5O__stc_pline_pre_copy_file(H5F_t *file_src, const void *mesg_src, bool *deleted,
                                       const H5O_copy_t *cpy_info, void *_udata);
static herr_t H5O__stc_pline_debug(H5F_t *f, const void *_mesg, FILE *stream, int indent, int fwidth);

/* Set up & include shared message "interface" info */
#define H5O_SHARED_TYPE        H5O_MSG_STC_PLINE
#define H5O_SHARED_DECODE      H5O__stc_pline_shared_decode
#define H5O_SHARED_DECODE_REAL H5O__stc_pline_decode
#define H5O_SHARED_ENCODE      H5O__stc_pline_shared_encode
#define H5O_SHARED_ENCODE_REAL H5O__stc_pline_encode
#define H5O_SHARED_SIZE        H5O__stc_pline_shared_size
#define H5O_SHARED_SIZE_REAL   H5O__stc_pline_size
#define H5O_SHARED_DELETE      H5O__stc_pline_shared_delete
#undef H5O_SHARED_DELETE_REAL
#define H5O_SHARED_LINK H5O__stc_pline_shared_link
#undef H5O_SHARED_LINK_REAL
#define H5O_SHARED_COPY_FILE H5O__stc_pline_shared_copy_file
#undef H5O_SHARED_COPY_FILE_REAL
#define H5O_SHARED_POST_COPY_FILE H5O__stc_pline_shared_post_copy_file
#undef H5O_SHARED_POST_COPY_FILE_REAL
#undef H5O_SHARED_POST_COPY_FILE_UPD
#define H5O_SHARED_DEBUG      H5O__stc_pline_shared_debug
#define H5O_SHARED_DEBUG_REAL H5O__stc_pline_debug
#include "H5Oshared.h" /* Shared Object Header Message Callbacks */

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_STC_PLINE[1] = {{
    H5O_STC_PLINE_ID,                          /* message id number            */
    "stc filter pipeline",                     /* message name for debugging   */
    sizeof(H5O_stc_pline_t),                   /* native message size          */
    H5O_SHARE_IS_SHARABLE | H5O_SHARE_IN_OHDR, /* messages are shareable?       */
    H5O__stc_pline_shared_decode,                  /* decode message               */
    H5O__stc_pline_shared_encode,                  /* encode message               */
    H5O__stc_pline_copy,                           /* copy the native value        */
    H5O__stc_pline_shared_size,                    /* size of raw message          */
    H5O__stc_pline_reset,                          /* reset method                 */
    H5O__stc_pline_free,                           /* free method                  */
    H5O__stc_pline_shared_delete,                  /* file delete method           */
    H5O__stc_pline_shared_link,                    /* link method                  */
    NULL,                                      /* set share method             */
    NULL,                                      /*can share method              */
    H5O__stc_pline_pre_copy_file,                  /* pre copy native value to file */
    H5O__stc_pline_shared_copy_file,               /* copy native value to file    */
    H5O__stc_pline_shared_post_copy_file,          /* post copy native value to file */
    NULL,                                      /* get creation index           */
    NULL,                                      /* set creation index           */
    H5O__stc_pline_shared_debug                    /* debug the message            */
}};

/* Format version bounds for filter pipeline */
const unsigned H5O_stc_pline_ver_bounds[] = {
    H5O_INVALID_VERSION,     /* H5F_LIBVER_EARLIEST */
    H5O_INVALID_VERSION,     /* H5F_LIBVER_V18 */
    H5O_INVALID_VERSION,     /* H5F_LIBVER_V110 */
    H5O_INVALID_VERSION,     /* H5F_LIBVER_V112 */
    H5O_INVALID_VERSION,     /* H5F_LIBVER_V114 */
    H5O_STC_PLINE_VERSION_1,     /* H5F_LIBVER_V200 */
    H5O_STC_PLINE_VERSION_LATEST /* H5F_LIBVER_LATEST */
};
#define N_STC_PLINE_VERSION_BOUNDS H5F_LIBVER_NBOUNDS

/* Declare a free list to manage the H5O_stc_pline_t struct */
H5FL_DEFINE(H5O_stc_pline_t);

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_decode
 *
 * Purpose:     Decodes a filter pipeline message for structured chunk.
 *
 * Return:      Success:    Pointer to a new pipeline message
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */

static void *
H5O__stc_pline_decode(H5F_t H5_ATTR_UNUSED *f, H5O_t H5_ATTR_UNUSED *open_oh, unsigned H5_ATTR_UNUSED mesg_flags,
                  unsigned H5_ATTR_UNUSED *ioflags, size_t p_size, const uint8_t *p)
{
    H5O_stc_pline_t   *pline = NULL;               /* Pipeline message */
    size_t         name_length;                /* Length of filter name */
    size_t         i, j, k;                    /* Local index variable */
    const uint8_t *p_end     = p + p_size - 1; /* End of the p buffer */
    H5O_stc_filter_sect_t *filt_sect;
    H5Z_filter_info_t     *filter;
    void          *ret_value = NULL;

    FUNC_ENTER_PACKAGE

    assert(f);
    assert(p);

    /* Allocate space for I/O pipeline message */
    if (NULL == (pline = H5FL_CALLOC(H5O_stc_pline_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Version */
    if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
    pline->version = *p++;

    if (pline->version < H5O_STC_PLINE_VERSION_1 || pline->version > H5O_STC_PLINE_VERSION_LATEST)
        HGOTO_ERROR(H5E_STC_PLINE, H5E_CANTLOAD, NULL, "bad version number for filter pipeline message");

    assert(pline->version == H5O_STC_PLINE_VERSION_1);

    /* Total number of filtered sections in the structured chunk */
    if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
    pline->tot_filt_nsects = *p++;

    if (pline->tot_filt_nsects > H5O_MAX_STC_NSECTS)
        HGOTO_ERROR(H5E_STC_PLINE, H5E_CANTLOAD, NULL,
                    "filter pipeline message has too many filtered sections");

    /* Decode array of filtered sections */
    for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {

        if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        filt_sect->seq_sect = *p++;

        if (H5_IS_BUFFER_OVERFLOW(p, 1, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        filt_sect->nused = *p++;

        if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
            HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
        UINT16DECODE(p, filt_sect->size_filt_descr);

        filt_sect->nalloc = filt_sect->nused;
        if (filt_sect->nused > H5Z_MAX_NFILTERS) {

            /* Reset the number of filters used to avoid array traversal in error
             * handling code.
             */
            filt_sect->nused = 0;

            HGOTO_ERROR(H5E_STC_PLINE, H5E_CANTLOAD, NULL, "filtered section has too many filters");
        }

        /* Allocate array of filter description for the ith filtered section */
        filt_sect->nalloc = filt_sect->nused;
        if (NULL == (filt_sect->filter = (H5Z_filter_info_t *)H5MM_calloc(filt_sect->nalloc *
                                                                          sizeof(filt_sect->filter[0]))))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

        /* Decode array of filter description */
        for (j = 0, filter = &filt_sect->filter[0]; j < filt_sect->nused; j++, filter++) {
            /* Filter ID */
            if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
                HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL, "ran off end of input buffer while decoding");
            UINT16DECODE(p, filter->id);

            /* Length of filter name */
            if (filter->id < H5Z_FILTER_RESERVED)
                    name_length = 0;
            else {
                if (H5_IS_BUFFER_OVERFLOW(p, 2, p_end))
                    HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL,
                                "ran off end of input buffer while decoding");
                UINT16DECODE(p, name_length);
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
                        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                    "memory allocation failed for filter name");
                }
                else
                    filter->name = filter->_name;

                strncpy(filter->name, (const char *)p, actual_name_length);

                if (H5_IS_BUFFER_OVERFLOW(p, name_length, p_end))
                    HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL,
                                "ran off end of input buffer while decoding");
                p += name_length;
            }

            /* Filter parameters */
            if (filter->cd_nelmts) {

                /* Allocate space for the client data elements, or use the internal buffer */
                if (filter->cd_nelmts > H5Z_COMMON_CD_VALUES) {
                        filter->cd_values = (unsigned *)H5MM_malloc(filter->cd_nelmts * sizeof(unsigned));
                        if (NULL == filter->cd_values)
                            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                        "memory allocation failed for client data");
                }
                else
                    filter->cd_values = filter->_cd_values;

                /* Read the client data values and the padding */
                for (k = 0; k < filter->cd_nelmts; k++) {
                    if (H5_IS_BUFFER_OVERFLOW(p, 4, p_end))
                        HGOTO_ERROR(H5E_OHDR, H5E_OVERFLOW, NULL,
                                    "ran off end of input buffer while decoding");
                    UINT32DECODE(p, filter->cd_values[k]);
                }
            }

        } /* end of array for filter description */

    } /* end of array for filtered sections */

    /* Set return value */
    ret_value = pline;

done:
    if (!ret_value && pline) {
        H5O__stc_pline_reset(pline);
        H5O__stc_pline_free(pline);
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__stc_pline_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_encode
 *
 * Purpose:    Encodes message MESG into buffer P.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__stc_pline_encode(H5F_t H5_ATTR_UNUSED *f, uint8_t *p /*out*/, const void *mesg)
{
    const H5O_stc_pline_t *pline = (const H5O_stc_pline_t *)mesg; /* Pipeline message to encode */
    size_t             i, j, k;                           /* Local index variables */

    FUNC_ENTER_PACKAGE_NOERR

    /* Check args */
    assert(p);
    assert(mesg);

    /* Message header */
    *p++ = (uint8_t)pline->version;

    assert(pline->version == H5O_PLINE_VERSION_1);

    *p++ = (uint8_t)pline->tot_filt_nsects;

    for (i = 0; i < pline->tot_filt_nsects; i++) {
        const H5Z_filter_info_t *filter; /* Filter to encode */
        size_t                   size_filt_descr = 0;
        uint8_t                 *save_p;

        *p++ = (uint8_t)(pline->filt_sects[i].seq_sect);
        *p++ = (uint8_t)(pline->filt_sects[i].nused);

        /* Save pointer to the field "size of ith filter description list" */
        /* It will be encoded later after the following for loop calculation */
        save_p = p;
        p += 2;

        for (j = 0, filter = &pline->filt_sects[i].filter[0]; j < pline->filt_sects[i].nused;
             j++, filter++) {
            const char *name;        /* Filter name */
            size_t      name_length; /* Length of filter name */

            /* Filter ID */
            UINT16ENCODE(p, filter->id);

            /* Skip writing the name length & name if the filter is an internal filter */
            if (filter->id < H5Z_FILTER_RESERVED) {
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
                    H5Z_class3_t *cls; /* Filter class */

                    H5Z_find(true, filter->id, &cls);
                    if (cls)
                        name = cls->name;
                }
                name_length = name ? strlen(name) + 1 : 0;

                /* Filter name length */
                UINT16ENCODE(p, name_length);
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

            } /* end if */

            /* Filter parameters */
            for (k = 0; k < filter->cd_nelmts; k++)
                UINT32ENCODE(p, filter->cd_values[k]);

            size_filt_descr += 2 + /* filter identification number */
                               (size_t)((filter->id >= H5Z_FILTER_RESERVED) ? 2 : 0) + /* name length */
                               2 +                                                     /* flags */
                               2 +                    /* number of client data values    */
                               name_length +          /* length of the filter name    */
                               filter->cd_nelmts * 4; /* Client data values */

        } /* end for */

        UINT16ENCODE(save_p, size_filt_descr);
    }

    FUNC_LEAVE_NOAPI(SUCCEED)

} /* end H5O__stc_pline_encode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_copy
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
H5O__stc_pline_copy(const void *_src, void *_dst /*out*/)
{
    const H5O_stc_pline_t *src = (const H5O_stc_pline_t *)_src; /* Source pipeline message */
    H5O_stc_pline_t       *dst = (H5O_stc_pline_t *)_dst;       /* Destination pipeline message */
    size_t             i, j;                            /* Local index variable */
    const H5O_stc_filter_sect_t *src_filt_sect;
    H5O_stc_filter_sect_t       *dst_filt_sect;
    H5O_stc_pline_t       *ret_value = NULL;                /* Return value */

    FUNC_ENTER_PACKAGE

    /* Allocate pipeline message, if not provided */
    if (!dst && NULL == (dst = H5FL_MALLOC(H5O_stc_pline_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Shallow copy basic fields */
    *dst = *src;

    assert(src->version == H5O_PLINE_VERSION_1);

    if (src->tot_filt_nsects) {
        for (i = 0, src_filt_sect = &src->filt_sects[0], dst_filt_sect = &dst->filt_sects[0];
             i < src->tot_filt_nsects; i++, src_filt_sect++, dst_filt_sect++) {

            /* Copy over filters, if any */
            dst_filt_sect->nalloc = dst_filt_sect->nused;
            if (dst_filt_sect->nalloc) {

                /* Allocate array to hold filters */
                if (NULL == (dst_filt_sect->filter = (H5Z_filter_info_t *)H5MM_calloc(
                             dst_filt_sect->nalloc * sizeof(dst_filt_sect->filter[0]))))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

                for (j = 0; j < src_filt_sect->nused; j++) {
                    /* Basic filter information */
                    dst_filt_sect->filter[j] = src_filt_sect->filter[j];

                    /* Filter name */
                    if (src_filt_sect->filter[j].name) {
                        size_t namelen; /* Length of source filter name, including null terminator  */

                        namelen = strlen(src_filt_sect->filter[j].name) + 1;

                        /* Allocate space for the filter name, or use the internal buffer */
                        if (namelen > H5Z_COMMON_NAME_LEN) {
                            dst_filt_sect->filter[j].name =
                                (char *)H5MM_strdup(src_filt_sect->filter[j].name);
                            if (NULL == dst_filt_sect->filter[j].name)
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL,
                                            "memory allocation failed for filter name");
                         } /* end if */
                         else
                            dst_filt_sect->filter[j].name = dst_filt_sect->filter[j]._name;
                    } /* end if */

                    /* Filter parameters */
                    if (src_filt_sect->filter[j].cd_nelmts > 0) {
                        /* Allocate space for the client data elements, or use the internal buffer */
                        if (src_filt_sect->filter[j].cd_nelmts > H5Z_COMMON_CD_VALUES) {
                            if (NULL == (dst_filt_sect->filter[j].cd_values = (unsigned *)H5MM_malloc(
                                         src_filt_sect->filter[j].cd_nelmts * sizeof(unsigned))))
                                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

                            H5MM_memcpy(dst_filt_sect->filter[j].cd_values,
                                        src_filt_sect->filter[j].cd_values,
                                        src_filt_sect->filter[j].cd_nelmts * sizeof(unsigned));
                        } /* end if */
                        else
                            dst_filt_sect->filter[j].cd_values = dst_filt_sect->filter[j]._cd_values;
                    } /* end if */
                } /* end for */
            }
            else
                dst_filt_sect->filter = NULL;

        } /* end for */

    } /* end if tot_filt_nsects */

    /* Set return value */
    ret_value = dst;

done:
    if (!ret_value && dst) {
        H5O__stc_pline_reset(dst);
        if (!_dst)
            H5O__stc_pline_free(dst);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__stc_pline_copy() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_size
 *
 * Purpose:    Determines the size of a raw filter pipeline message for structured chunk.
 *
 * Return:    Success:    Size of message.
 *
 *        Failure:    zero
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O__stc_pline_size(const H5F_t H5_ATTR_UNUSED *f, const void *mesg)
{
    const H5O_stc_pline_t *pline = (const H5O_stc_pline_t *)mesg; /* Pipeline message */
    size_t             i, j;                              /* Local index variable */
    const H5O_stc_filter_sect_t *filt_sect;
    size_t             ret_value = 0;                     /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    assert(pline->version == H5O_PLINE_VERSION_1);

    /* Message header */
    ret_value = (size_t)(1 + /*version            */
                         1); /*number of filtered sections */

    for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {
            ret_value += (1 + /* Sequence number of the ith filtered section */
                          1 + /* Number of filters for the ith filtered section */
                          2); /* Size of the ith filtered description list */

        /* Calculate size of each filter in pipeline for each filtered section */
        for (j = 0; j < filt_sect->nused; j++) {
            size_t      name_len; /* Length of filter name */
            const char *name;     /* Filter name */

            /* Don't write the name length & name if the filter is an internal filter */
            if (filt_sect->filter[j].id < H5Z_FILTER_RESERVED)
                name_len = 0;
            else {
                /* Get the name of the filter, same as done with H5O__pline_encode() */
                if (NULL == (name = filt_sect->filter[j].name)) {
                    H5Z_class3_t *cls; /* Filter class */

                    H5Z_find(true, filt_sect->filter[j].id, &cls);
                    if (cls)
                        name = cls->name;
                }
                name_len = name ? strlen(name) + 1 : 0;
            } /* end else */

            ret_value +=
                2 + /*filter identification number    */
                (size_t)((filt_sect->filter[j].id >= H5Z_FILTER_RESERVED) ? 2 : 0) + /*name length */
                2 +       /*flags                */
                2 +       /*number of client data values    */
                name_len; /*length of the filter name    */

            ret_value += filt_sect->filter[j].cd_nelmts * 4;
        } /* end for */
    }   /* end for */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__stc_pline_size() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_reset
 *
 * Purpose:    Resets a filter pipeline message by clearing all filters.
 *        The MESG buffer is not freed.
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__stc_pline_reset(void *mesg)
{
    H5O_stc_pline_t *pline = (H5O_stc_pline_t *)mesg; /* Pipeline message */
    size_t       i, j;                        /* Local index variable */
    H5O_stc_filter_sect_t *filt_sect;

    FUNC_ENTER_PACKAGE_NOERR

    /* NOTE: This function can be called during error processing from
     *       other API calls so DO NOT ASSUME THAT ANY VALUES ARE SANE.
     */

    assert(pline);

    assert(pline->version == H5O_PLINE_VERSION_1);

    for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {
        if (filt_sect->filter) {
            /* Free information for each filter */
            for (j = 0; j < filt_sect->nused; j++) {
                if (filt_sect->filter[j].name && filt_sect->filter[j].name != filt_sect->filter[j]._name)
                    assert((strlen(filt_sect->filter[j].name) + 1) > H5Z_COMMON_NAME_LEN);
                if (filt_sect->filter[j].name != filt_sect->filter[j]._name)
                    filt_sect->filter[j].name = (char *)H5MM_xfree(filt_sect->filter[j].name);

                if (filt_sect->filter[j].cd_values &&
                    filt_sect->filter[j].cd_values != filt_sect->filter[j]._cd_values)
                    assert(filt_sect->filter[j].cd_nelmts > H5Z_COMMON_CD_VALUES);
                if (filt_sect->filter[j].cd_values != filt_sect->filter[j]._cd_values)
                    filt_sect->filter[j].cd_values =
                    (unsigned *)H5MM_xfree(filt_sect->filter[j].cd_values);
            } /* end for */

            /* Free filter array for the section */
            filt_sect->filter = (H5Z_filter_info_t *)H5MM_xfree(filt_sect->filter);
        }
        filt_sect->nused = filt_sect->nalloc = 0;

    } /* end for */

    pline->tot_filt_nsects = 0;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__stc_pline_reset() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_free
 *
 * Purpose:    Frees the message
 *
 * Return:    Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__stc_pline_free(void *mesg)
{
    FUNC_ENTER_PACKAGE_NOERR

    assert(mesg);

    mesg = H5FL_FREE(H5O_stc_pline_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__stc_pline_free() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_pre_copy_file
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
H5O__stc_pline_pre_copy_file(H5F_t H5_ATTR_UNUSED *file_src, const void *mesg_src, bool H5_ATTR_UNUSED *deleted,
                         const H5O_copy_t *cpy_info, void *_udata)
{
    const H5O_stc_pline_t         *pline_src = (const H5O_stc_pline_t *)mesg_src;       /* Source pline */
    H5O_copy_file_ud_common_t *udata     = (H5O_copy_file_ud_common_t *)_udata; /* Object copying user data */
    herr_t                     ret_value = SUCCEED;                             /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    assert(pline_src);
    assert(cpy_info);
    assert(cpy_info->file_dst);

    /* Check to ensure that the version of the message to be copied does not exceed
       the message version allowed by the destination file's high bound */
    if (pline_src->version > H5O_stc_pline_ver_bounds[H5F_HIGH_BOUND(cpy_info->file_dst)])
        HGOTO_ERROR(H5E_OHDR, H5E_BADRANGE, FAIL, "pline message version out of bounds");

    /* If the user data is non-NULL, assume we are copying a dataset or group
     * and make a copy of the filter pipeline for later in
     * the object copying process.
     */
    if (udata)
        if (NULL == (udata->stc_src_pline = (H5O_stc_pline_t *)H5O__stc_pline_copy(pline_src, NULL)))
            HGOTO_ERROR(H5E_STC_PLINE, H5E_CANTINIT, FAIL, "unable to copy");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__stc_pline_pre_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__stc_pline_debug
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
H5O__stc_pline_debug(H5F_t H5_ATTR_UNUSED *f, const void *mesg, FILE *stream, int indent, int fwidth)
{
    const H5O_stc_pline_t *pline = (const H5O_stc_pline_t *)mesg;
    const H5O_stc_filter_sect_t *filt_sect;
    size_t                       i, j, k;

    FUNC_ENTER_PACKAGE_NOERR

    /* check args */
    assert(f);
    assert(pline);
    assert(stream);
    assert(indent >= 0);
    assert(fwidth >= 0);

    assert(pline->version == H5O_PLINE_VERSION_1);

    for (i = 0, filt_sect = &pline->filt_sects[0]; i < pline->tot_filt_nsects; i++, filt_sect++) {
            fprintf(stream, "%*s%-*s %zu\n", indent, "", fwidth,
                    "Sequence # of filtered section:", filt_sect->seq_sect);

            fprintf(stream, "%*s%-*s %zu/%zu\n", indent, "", fwidth, "Number of filters:", filt_sect->nused,
                    filt_sect->nalloc);

        /* Loop over all the filters */
        for (j = 0; j < filt_sect->nused; j++) {
            /* 19 characters for text + 20 characters for largest 64-bit size_t +
             * terminal NUL = 40 characters.
             */
            char name[64];

            memset(name, 0, 64);
            snprintf(name, sizeof(name), "Filter at position %zu", i);

            fprintf(stream, "%*s%-*s\n", indent, "", fwidth, name);
            fprintf(stream, "%*s%-*s 0x%04x\n", indent + 3, "", MAX(0, fwidth - 3),
                    "Filter identification:", (unsigned)(filt_sect->filter[j].id));
            if (filt_sect->filter[j].name)
                fprintf(stream, "%*s%-*s \"%s\"\n", indent + 3, "", MAX(0, fwidth - 3),
                        "Filter name:", filt_sect->filter[j].name);
            else
                fprintf(stream, "%*s%-*s NONE\n", indent + 3, "", MAX(0, fwidth - 3), "Filter name:");

            fprintf(stream, "%*s%-*s 0x%04x\n", indent + 3, "", MAX(0, fwidth - 3),
                    "Flags:", filt_sect->filter[i].flags);

            fprintf(stream, "%*s%-*s %zu\n", indent + 3, "", MAX(0, fwidth - 3),
                    "Num CD values:", filt_sect->filter[j].cd_nelmts);

            /* Filter parameters */
            for (k = 0; k < filt_sect->filter[j].cd_nelmts; k++) {
                char field_name[32];

                snprintf(field_name, sizeof(field_name), "CD value %lu", (unsigned long)k);
                fprintf(stream, "%*s%-*s %u\n", indent + 6, "", MAX(0, fwidth - 6), field_name,
                        filt_sect->filter[j].cd_values[k]);
            } /* for k */
        }   /* for j */
    }   /* for i */


    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__stc_pline_debug() */

/*-------------------------------------------------------------------------
 * Function:    H5O_stc_pline_set_version
 *
 * Purpose:     Set the version to encode an I/O filter pipeline with.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O_stc_pline_set_version(H5F_t *f, H5O_stc_pline_t *pline)
{
    unsigned version;             /* Message version */
    herr_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDcompile_assert(N_STC_PLINE_VERSION_BOUNDS == H5F_LIBVER_NBOUNDS);
    assert(f);
    assert(pline);

    version = H5O_STC_PLINE_VERSION_1;

    /* Upgrade to the version indicated by the file's low bound if higher */
    if (H5O_stc_pline_ver_bounds[H5F_LOW_BOUND(f)] != H5O_INVALID_VERSION)
        version = MAX(version, H5O_stc_pline_ver_bounds[H5F_LOW_BOUND(f)]);

    /* Version bounds check */
    if (H5O_stc_pline_ver_bounds[H5F_HIGH_BOUND(f)] == H5O_INVALID_VERSION || version > H5O_stc_pline_ver_bounds[H5F_HIGH_BOUND(f)])
        HGOTO_ERROR(H5E_OHDR, H5E_BADRANGE, FAIL, "STC Filter pipeline message's version out of bounds");

    /* Set the message version */
    pline->version = version;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_stc_pline_set_version() */
