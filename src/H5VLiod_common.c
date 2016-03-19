/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of h5netvol.  The full h5netvol copyright notice,       *
 * including terms governing use, modification, and redistribution, is       *
 * contained in the file COPYING at the root of the source code distribution *
 * tree.                                                                     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>
#include "H5VLiod_common.h"
#ifdef H5_HAVE_EFF
#include <mchecksum.h>          /* Mercury Checksum library             */
#endif /* H5_HAVE_EFF */

/*
 * Local typedefs
 */
/* Structure for a compound member type */
typedef struct H5VL_iod_cmpd_info_t {
    size_t offset;
    hid_t type_id;
} H5VL_iod_cmpd_info_t;

/* Macros for error handling */
#define SUCCEED 0
#define FAIL    -1
#define PRINT_ERROR(MSG)                                                       \
do {                                                                           \
    fprintf(stderr, "ERROR at %s:%d in %s()...\n%s\n", __FILE__, __LINE__, __FUNCTION__,MSG); \
} while(0)
#define ERROR(MSG)                                                             \
do {                                                                           \
    PRINT_ERROR(MSG);                                                          \
    goto error;                                                                \
} while(0)

#define TRUE 1
#define FALSE 0

/* Macros to encode and decode a uint64_t */
#  define UINT64ENCODE(p, n) \
    do { \
       uint64_t _n = (n); \
       size_t _i; \
       uint8_t *_p = (uint8_t*)(p); \
\
       for (_i = 0; _i < sizeof(uint64_t); _i++, _n >>= 8) \
          *_p++ = (uint8_t)(_n & 0xff); \
       for (/*void*/; _i < 8; _i++) \
          *_p++ = 0; \
    } while(0)

#  define UINT64DECODE(p, n) \
    do { \
       /* WE DON'T CHECK FOR OVERFLOW! */ \
       size_t _i; \
       uint8_t *_p = (uint8_t*)(p); \
\
       n = 0; \
       (_p) += 8; \
       for (_i = 0; _i < sizeof(uint64_t); _i++) \
          n = (n << 8) | *(--_p); \
    } while(0)

/*
 * Local functions
 */
static int H5VL_iod_cmpd_qsort_cb(const void *_memb1, const void *_memb2);
static int H5VL_iod_get_type_info_helper(hid_t type_id,
    H5VL_iod_type_info_t *type_info, size_t offset, size_t *vls_nalloc);
static int H5VL_iod_cs_send_helper(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    size_t *segments_nalloc, char **vl_lengths, size_t *vl_lengths_nused,
    size_t *vl_lengths_nalloc, void ***free_list, size_t *free_list_len,
    size_t *free_list_nalloc);
static int H5VL_iod_cs_recv_helper(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    size_t *segments_nalloc, char *vl_lengths, size_t *vl_lengths_loc,
    size_t vl_lengths_nused, void ***free_list, size_t *free_list_len,
    size_t *free_list_nalloc);


/*
 * Local macros
 */
#define H5VL_IOD_ARR_ADD(TYPE, ARR, NUSED, NALLOC, DEF_ALLOC) \
    do { \
        size_t _tmp_nalloc; \
        TYPE *_tmp_arr; \
\
        assert((NALLOC) >= (NUSED)); \
\
        if((NALLOC) == (NUSED)) { \
            _tmp_nalloc = (NALLOC) ? (NALLOC) * 2 : (DEF_ALLOC); \
\
            if(NULL == (_tmp_arr = (TYPE *)realloc(ARR, _tmp_nalloc * sizeof(TYPE)))) \
                ERROR("failed to reallocate array"); \
            (ARR) = _tmp_arr; \
            (NALLOC) = _tmp_nalloc; \
        } /* end if */ \
    } while(0)

/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_cmpd_qsort_cb
 *
 * Purpose:     qsort callback for sorting compound type members by
 *              offset.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL_iod_cmpd_qsort_cb(const void *_memb1, const void *_memb2)
{
    const H5VL_iod_cmpd_info_t *memb1 = (const H5VL_iod_cmpd_info_t *)_memb1;
    const H5VL_iod_cmpd_info_t *memb2 = (const H5VL_iod_cmpd_info_t *)_memb2;

    if(memb1->offset < memb2->offset)
        return -1;
    else if(memb1->offset > memb2->offset)
        return 1;
    else
        return 0;
} /* end H5VL_iod_cmpd_qsort_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_type_info_helper
 *
 * Purpose:     Recursively searches for variable-length datatypes in the
 *              provided type, filling in the fields in type_info.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 18, 2013
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL_iod_get_type_info_helper(hid_t type_id, H5VL_iod_type_info_t *type_info,
    size_t offset, size_t *vls_nalloc)
{
    hid_t super_type = -1;
    hsize_t *dims = NULL;
    size_t num_cmpd_membs = 0;
    H5VL_iod_cmpd_info_t *cmpd_membs = NULL;
    int i;

    assert(type_info);
    assert(vls_nalloc);

    /* Get type class */
    if(H5T_NO_CLASS == (type_info->type_class = H5Tget_class(type_id)))
        ERROR("failed to get datatype class");

    /* Take different actions depending on class */
    switch(type_info->type_class) {
        case H5T_COMPOUND:
            {
                int nmemb;

                /* Get number of members */
                if((nmemb = H5Tget_nmembers(type_id)) < 0)
                    ERROR("failed to get number of compound datatype members");

                /* Allocate array of members */
                if(NULL == (cmpd_membs = (H5VL_iod_cmpd_info_t *)malloc((size_t)nmemb * sizeof(H5VL_iod_cmpd_info_t))))
                    ERROR("failed ot allocate array of compound type members");

                /* Get offset and type for all members */
                for(i = 0; i < nmemb; i++) {
                    cmpd_membs[i].offset = H5Tget_member_offset(type_id, (unsigned)i);

                    if((cmpd_membs[i].type_id = H5Tget_member_type(type_id, (unsigned)i)) < 0)
                        ERROR("failed to get compound datatype member type");
                    num_cmpd_membs++;
                } /* end for */

                /* Sort members by offset */
                qsort(cmpd_membs, num_cmpd_membs, sizeof(cmpd_membs[0]), H5VL_iod_cmpd_qsort_cb);

                /* Get info for all members */
                for(i = 0; i < nmemb; i++)
                    if(H5VL_iod_get_type_info_helper(cmpd_membs[i].type_id, type_info, offset + cmpd_membs[i].offset, vls_nalloc) < 0)
                        ERROR("failed to get compound datatype member type info");

                /* Free cmpd_membs.  Run in opposite order so if a failure
                 * occurs the error code doesn't try to free types twice */
                for(i = nmemb - 1; i >= 0; i--) {
                    num_cmpd_membs--;
                    if(H5Tclose(cmpd_membs[i].type_id) < 0)
                        ERROR("failed to close compound member type");
                } /* end for */
                free(cmpd_membs);
                cmpd_membs = NULL;

                break;
            } /* end block */

        case H5T_ARRAY:
            {
                int ndims;
                size_t array_nelem = 1;
                size_t orig_num_vls = type_info->num_vls;
                size_t elem_num_vls;
                size_t super_type_size;
                size_t i_size, j_size;

                /* Get array element type */
                if((super_type = H5Tget_super(type_id)) < 0)
                    ERROR("failed to get array datatype element type");

                /* Get type info for element type (effectively only for the
                 * first element) */
                if(H5VL_iod_get_type_info_helper(super_type, type_info, offset, vls_nalloc) < 0)
                    ERROR("failed to get array datatype element type info");

                /* If the element type contains any vlens, we must copy the vlen
                 * info for each element in the array */
                if(type_info->num_vls != orig_num_vls) {
                    assert(type_info->num_vls > orig_num_vls);

                    /* Get number of dimensions in array */
                    if((ndims = H5Tget_array_ndims(type_id)) < 0)
                        ERROR("failed to get array datatype number of dimensions");

                    /* Allocate array of dimensions */
                    if(NULL == (dims = (hsize_t *)malloc((size_t)ndims * sizeof(hsize_t))))
                        ERROR("failed to allocate array of dimensions");

                    /* Get array dimensions */
                    if(H5Tget_array_dims2(type_id, dims) < 0)
                        ERROR("failed to get array datatype dimensions");

                    /* Calculate total number of elements */
                    for(i = 0; i < ndims; i++)
                        array_nelem *= (size_t)dims[i];

                    /* Get size of element type */
                    if(0 == (super_type_size = H5Tget_size(super_type)))
                        ERROR("failed to get array element type size");

                    /* Replicate all vls added during recursion into element
                     * type for each element in the array.  Increment ref count
                     * on base types instead of copying. */
                    elem_num_vls = type_info->num_vls;
                    for(i_size = 1; i_size < array_nelem; i_size++)
                        for(j_size = orig_num_vls; j_size < elem_num_vls;
                                j_size++) {
                            H5VL_IOD_ARR_ADD(H5VL_iod_vl_info_t, type_info->vls, type_info->num_vls, *vls_nalloc, 8);

                            type_info->vls[type_info->num_vls].offset = type_info->vls[j_size].offset + (i_size * super_type_size);
                            type_info->vls[type_info->num_vls].base_type = type_info->vls[j_size].base_type;
                            if(type_info->vls[type_info->num_vls].base_type)
                                type_info->vls[type_info->num_vls].base_type->rc++;
                            type_info->num_vls++;
                        } /* end for */

                    free(dims);
                    dims = NULL;
                } /* end if */

                if(H5Tclose(super_type) < 0)
                    ERROR("failed to close array datatype element type");
                super_type = -1;

                break;
            } /* end block */

        case H5T_VLEN:
            /* Add vlen to vls array */
            H5VL_IOD_ARR_ADD(H5VL_iod_vl_info_t, type_info->vls, type_info->num_vls, *vls_nalloc, 8);

            /* Get vlen base type */
            if((super_type = H5Tget_super(type_id)) < 0)
                ERROR("failed to get vlen datatype base type");

            /* Set vlen offset in type */
            type_info->vls[type_info->num_vls].offset = offset;

            /* Allocate type info for base type */
            if(NULL == (type_info->vls[type_info->num_vls].base_type = (H5VL_iod_type_info_t *)malloc(sizeof(H5VL_iod_type_info_t))))
                ERROR("failed to allocate vlen datatype base type info");
            type_info->num_vls++;

            /* Get type info for base type */
            if(H5VL_iod_get_type_info(super_type, type_info->vls[type_info->num_vls - 1].base_type) < 0)
                ERROR("failed to get vlen datatype base type info");

            if(H5Tclose(super_type) < 0)
                ERROR("failed to close vlen datatype base type");
            super_type = -1;

            break;

        case H5T_REFERENCE:
            H5VL_IOD_ARR_ADD(H5VL_iod_vl_info_t, type_info->vls, type_info->num_vls, *vls_nalloc, 8);

            type_info->vls[type_info->num_vls].offset = offset;
            type_info->vls[type_info->num_vls].base_type = NULL;
            type_info->num_vls++;

            if(H5Tequal(type_id, H5T_STD_REF_OBJ))
                type_info->ref_type = H5R_OBJECT;
            else if(H5Tequal(type_id, H5T_STD_REF_DSETREG))
                type_info->ref_type = H5R_DATASET_REGION;
            else
                assert("Ref Type not Supported for I/O" && 0);

            break;

        case H5T_STRING:
            {
                htri_t is_variable_str;

                /* Check if this string is variable length */
                if((is_variable_str = H5Tis_variable_str(type_id)) < 0)
                    ERROR("failed to determine if type is variable string");

                if(is_variable_str) {
                    /* Add variable-length string to vls array.  Set base_type
                     * to NULL. */
                    H5VL_IOD_ARR_ADD(H5VL_iod_vl_info_t, type_info->vls, type_info->num_vls, *vls_nalloc, 8);

                    type_info->vls[type_info->num_vls].offset = offset;
                    type_info->vls[type_info->num_vls].base_type = NULL;
                    type_info->num_vls++;

                    break;
                } /* end if */

                /* Fall through if not variable string */
            } /* end block */

        default:
            /* Nothing to do currently */
            break;
    } /* end switch */

    return(SUCCEED);

error:
    if(super_type >= 0)
        if(H5Tclose(super_type) < 0)
            PRINT_ERROR("failed to close super type");
    if(dims)
        free(dims);
    if(cmpd_membs) {
        for(i = 0; i < (int)num_cmpd_membs; i++)
            if(H5Tclose(cmpd_membs[i].type_id) < 0)
                PRINT_ERROR("failed to close compound member type");
        free(cmpd_membs);
    } /* end if */

    return(FAIL);
} /* end H5VL_iod_get_type_info_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_get_type_info
 *
 * Purpose:     Builds the type_info struct given type_id.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 18, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_get_type_info(hid_t type_id, H5VL_iod_type_info_t *type_info)
{
    htri_t found_class;

    /* Initialize struct first, so a call to reset() won't cause problems if
     * something in this function fails */
    type_info->num_fl_spans = 0;
    type_info->fl_spans = NULL;
    type_info->num_vls = 0;
    type_info->vls = NULL;
    type_info->rc = 1;
    if(0 == (type_info->size = H5Tget_size(type_id)))
        ERROR("H5Tget_size failed");
    if(H5T_REFERENCE == H5Tget_class(type_id))
        type_info->size = sizeof(href_ff_t);

    /* Check for a vl or string type */
    if((found_class = H5Tdetect_class(type_id, H5T_VLEN)) < 0)
        ERROR("failed to search for vlen type class");
    if(!found_class)
        if((found_class = H5Tdetect_class(type_id, H5T_STRING)) < 0)
            ERROR("failed to search for string type class");
    if(!found_class)
        if((found_class = H5Tdetect_class(type_id, H5T_REFERENCE)) < 0)
            ERROR("failed to search for reference type class");


    /* Only need to investigate further if the type contains a vlen, string, or reference */
    if(found_class) {
        size_t vls_nalloc = 0;

        if(H5VL_iod_get_type_info_helper(type_id, type_info, 0, &vls_nalloc) < 0)
            ERROR("failed to type info");
    } /* end if */

    /* If any vlens were found, build fixed-length span info */
    if(type_info->num_vls) {
        size_t cur_fl_offset = 0;
        size_t fl_spans_nalloc = 0;
        size_t i;

        /* Iterate over all vlens */
        for(i = 0; i < type_info->num_vls; i++) {
            /* Check if this vlen left a fixed-length gap before it */
            assert(type_info->vls[i].offset >= cur_fl_offset);
            if(type_info->vls[i].offset > cur_fl_offset) {
                /* Create fixed-length span before vl */
                H5VL_IOD_ARR_ADD(H5VL_iod_fl_span_t, type_info->fl_spans, type_info->num_fl_spans, 
                                 fl_spans_nalloc, 2);

                type_info->fl_spans[type_info->num_fl_spans].offset = cur_fl_offset;
                type_info->fl_spans[type_info->num_fl_spans].size = type_info->vls[i].offset - cur_fl_offset;
                type_info->num_fl_spans++;
            } /* end if */

            /* Update cur_fl_offset */
            if(H5T_REFERENCE == type_info->type_class)
                cur_fl_offset = type_info->vls[i].offset + sizeof(href_ff_t);
            else
                cur_fl_offset = type_info->vls[i].offset + 
                    (type_info->vls[i].base_type ? sizeof(hvl_t) : sizeof(char *));
        } /* end for */

        /* Add span at end */
        //assert(type_info->size >= cur_fl_offset);
        if(type_info->size > cur_fl_offset) {
            H5VL_IOD_ARR_ADD(H5VL_iod_fl_span_t, type_info->fl_spans, type_info->num_fl_spans, fl_spans_nalloc, 2);

            type_info->fl_spans[type_info->num_fl_spans].offset = cur_fl_offset;
            type_info->fl_spans[type_info->num_fl_spans].size = type_info->size - cur_fl_offset;
            type_info->num_fl_spans++;
        } /* end if */
    } /* end if */

    return(SUCCEED);

error:
    H5VL_iod_type_info_reset(type_info);

    return(FAIL);
} /* end H5VL_iod_get_type_info() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_type_info_reset
 *
 * Purpose:     Frees all fields fields in type_info, and anything
 *              referenced by those fields.  Does not free type_info.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_type_info_reset(H5VL_iod_type_info_t *type_info)
{
    size_t i;

    /* Free fl spans */
    if(type_info->fl_spans) {
        free(type_info->fl_spans);
        type_info->fl_spans = NULL;
    } /* end if */
    type_info->num_fl_spans = 0;

    /* Free vls */
    if(type_info->vls) {
        /* Recurse into each vl's base type, and free the base type if its rc
         * reaches 0 */
        for(i = 0; i < type_info->num_vls; i++)
            if(type_info->vls[i].base_type
                    && (--type_info->vls[i].base_type->rc == 0)) {
                H5VL_iod_type_info_reset(type_info->vls[i].base_type);
                free(type_info->vls[i].base_type);
            } /* end if */

        free(type_info->vls);
        type_info->vls = NULL;
    } /* end if */
    type_info->num_vls = 0;

    return;
} /* end H5VL_iod_type_info_reset() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_cs_send_helper
 *
 * Purpose:     Recursively builds the segments array and array of
 *              variable-length data lengths given buf, type_info and
 *              nelem.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL_iod_cs_send_helper(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    size_t *segments_nalloc, char **vl_lengths, size_t *vl_lengths_nused,
    size_t *vl_lengths_nalloc, void ***free_list, size_t *free_list_len,
    size_t *free_list_nalloc)
{
    _Bool wrapped = FALSE;
    _Bool wrappable;
    size_t span_size;
    size_t i;
    size_t j;

    assert(nelem > 0);

    if(type_info->vls) {
        /* Add segments for fixed-length spans */
        if(type_info->num_fl_spans > 0) {
            assert(type_info->fl_spans);

            /* Check if we can combine the last span in each element with the first
             * span in the next */
            wrappable = ((type_info->fl_spans[type_info->num_fl_spans - 1].offset
                    + type_info->fl_spans[type_info->num_fl_spans - 1].size)
                    == type_info->size) && (type_info->fl_spans[0].offset == 0);

            assert(!(wrappable && (type_info->num_fl_spans == 1)));

            /* Iterate over spans */
            for(i = 0; i < nelem; i++)
                for(j = 0; j < type_info->num_fl_spans; j++)
                    if(wrapped)
                        wrapped = FALSE;
                    else {
                        /* Add segment to segments array */
                        if(*segments_nalloc == *num_segments) {
                            size_t tmp_nalloc;

                            tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                            if(NULL == (*addrs = (void **)realloc
                                        (*addrs, tmp_nalloc * sizeof(void *))))
                                ERROR("failed to reallocate array");

                            if(NULL == (*sizes = (size_t *)realloc
                                        (*sizes, tmp_nalloc * sizeof(size_t))))
                                ERROR("failed to reallocate array");

                            *segments_nalloc = tmp_nalloc;
                        } /* end if */

                        /* Check if we can wrap this segment */
                        if((j == (type_info->num_fl_spans - 1)) && wrappable
                                && (i != (nelem - 1))) {
                            span_size = type_info->fl_spans[0].size + type_info->fl_spans[j].size;
                            wrapped = TRUE;
                        } /* end if */
                        else
                            span_size = type_info->fl_spans[j].size;

                        /* Add segment */
                        (*addrs)[*num_segments] = (void *)(buf + (i * type_info->size) + 
                                                           type_info->fl_spans[j].offset);
                        (*sizes)[*num_segments] = span_size;
                        (*num_segments)++;
                    } /* end else */
        } /* end if */

        /* Analyze vlens */
        for(i = 0; i < nelem; i++)
            for(j = 0; j < type_info->num_vls; j++)
                if(type_info->vls[j].base_type) {
                    /* Standard vlen */
                    hvl_t vl = *(hvl_t *)(buf + (i * type_info->size) + type_info->vls[j].offset);

                    /* Add vl segment length to vl_lengths array */
                    H5VL_IOD_ARR_ADD(char, *vl_lengths, *vl_lengths_nused, *vl_lengths_nalloc, 256);
                    assert(*vl_lengths_nused + 8 <= *vl_lengths_nalloc);

                    UINT64ENCODE(&(*vl_lengths)[*vl_lengths_nused], (uint64_t)vl.len);
                    *vl_lengths_nused += 8;

                    /* Handle vlen array if present */
                    if(vl.len > 0) {
                        /* Add vlen buffer to free list, if present */
                        if(free_list) {
                            H5VL_IOD_ARR_ADD(void *, *free_list, *free_list_len, *free_list_nalloc, 64);

                            (*free_list)[*free_list_len] = vl.p;
                            (*free_list_len)++;
                        } /* end if */

                        /* Recurse into vlen data */
                        if(H5VL_iod_cs_send_helper((char *)vl.p, type_info->vls[j].base_type, 
                                                   vl.len, addrs, sizes, num_segments, segments_nalloc, 
                                                   vl_lengths, vl_lengths_nused, vl_lengths_nalloc, 
                                                   free_list, free_list_len, free_list_nalloc) < 0)
                            ERROR("failed to build segments");
                    } /* end if */
                } /* end if */
                else if(H5T_REFERENCE == type_info->type_class){
                    href_ff_t ref = *(href_ff_t *)(buf + (i * type_info->size) + type_info->vls[j].offset);

                    //printf("Send Helper - ref: (%p, %zu) type = %d\n", ref.buf, ref.buf_size, ref.ref_type);
                    /* Add ref buffer size */
                    H5VL_IOD_ARR_ADD(char, *vl_lengths, *vl_lengths_nused, *vl_lengths_nalloc, 256);
                    assert(*vl_lengths_nused + 8 <= *vl_lengths_nalloc);
                    UINT64ENCODE(&(*vl_lengths)[*vl_lengths_nused], (uint64_t)ref.buf_size);
                    *vl_lengths_nused += 8;

                    /* Add segment to segments array */
                    if(*segments_nalloc == *num_segments) {
                        size_t tmp_nalloc;

                        tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                        if(NULL == (*addrs = (void **)realloc
                                    (*addrs, tmp_nalloc * sizeof(void *))))
                            ERROR("failed to reallocate array");

                        if(NULL == (*sizes = (size_t *)realloc
                                    (*sizes, tmp_nalloc * sizeof(size_t))))
                            ERROR("failed to reallocate array");

                        *segments_nalloc = tmp_nalloc;
                    } /* end if */

                    /* Add segment */
                    (*addrs)[*num_segments] = ref.buf;
                    (*sizes)[*num_segments] = ref.buf_size;
                    (*num_segments)++;
                }
                else {
                    /* Vlen string */
                    char *vl_s = *(char **)(buf + (i * type_info->size) + type_info->vls[j].offset);
                    uint64_t vl_s_len;

                    /* Get size */
                    if(vl_s)
                        vl_s_len = (uint64_t)strlen(vl_s) + 1;
                    else
                        vl_s_len = 0;

                    /* Add vl string length to vl_lengths array.  Include null
                     * terminator in length to distinguish between NULL pointer
                     * (len = 0) and null string (len = 1) */
                    H5VL_IOD_ARR_ADD(char, *vl_lengths, *vl_lengths_nused, *vl_lengths_nalloc, 256);
                    assert(*vl_lengths_nused + 8 <= *vl_lengths_nalloc);

                    UINT64ENCODE(&(*vl_lengths)[*vl_lengths_nused], vl_s_len);
                    *vl_lengths_nused += 8;

                    /* Handle the vl string buffer if present */
                    if(vl_s_len > 0) {
                        /* Add vl string buffer to free list, if present */
                        if(free_list) {
                            H5VL_IOD_ARR_ADD(void *, *free_list, *free_list_len, *free_list_nalloc, 64);

                            (*free_list)[*free_list_len] = vl_s;
                            (*free_list_len)++;
                        } /* end if */

                        /* Add vl string length to segments array, if length is
                         * greater than 1 (including null terminator).  Do not
                         * include null terminator in segment length so it is not
                         * transmitted. */
                        if(vl_s_len > 1) {
                            /* Add segment to segments array */
                            if(*segments_nalloc == *num_segments) {
                                size_t tmp_nalloc;

                                tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                                if(NULL == (*addrs = (void **)realloc
                                            (*addrs, tmp_nalloc * sizeof(void *))))
                                    ERROR("failed to reallocate array");

                                if(NULL == (*sizes = (size_t *)realloc
                                            (*sizes, tmp_nalloc * sizeof(size_t))))
                                    ERROR("failed to reallocate array");

                                *segments_nalloc = tmp_nalloc;
                            } /* end if */

                            /* Add segment */
                            (*addrs)[*num_segments] = (void *)vl_s;
                            (*sizes)[*num_segments] = vl_s_len - 1;
                            (*num_segments)++;
                        } /* end if */
                    } /* end if */
                } /* end else */
    } /* end if */
    else {
        /* No vlens, just add entire buffer to segments array */
        if(*segments_nalloc == *num_segments) {
            size_t tmp_nalloc;

            tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

            if(NULL == (*addrs = (void **)realloc
                        (*addrs, tmp_nalloc * sizeof(void *))))
                ERROR("failed to reallocate array");

            if(NULL == (*sizes = (size_t *)realloc
                        (*sizes, tmp_nalloc * sizeof(size_t))))
                ERROR("failed to reallocate array");

            *segments_nalloc = tmp_nalloc;
        } /* end if */

        /* Add segment */
        (*addrs)[*num_segments] = (void *)buf;
        (*sizes)[*num_segments] = nelem * type_info->size;
        (*num_segments)++;
    } /* end else */

    return(SUCCEED);

error:
    return(FAIL);
} /* end H5VL_iod_cs_send_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_create_segments_send
 *
 * Purpose:     Builds the segments array and array of variable-length
 *              data lengths given buf, type_info and nelem.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_create_segments_send(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    char **vl_lengths, size_t *vl_lengths_size, void ***free_list,
    size_t *free_list_len)
{
    size_t segments_nalloc = 0;
    size_t vl_lengths_nalloc = 0;
    size_t free_list_nalloc = 0;

    assert(buf);
    assert(type_info);
    assert(type_info->vls);
    assert(nelem > 0);
    assert(num_segments);
    assert(*num_segments == 0);
    assert(vl_lengths);
    assert(!*vl_lengths);
    assert(vl_lengths_size);
    assert(*vl_lengths_size == 0);
    assert(!free_list == !free_list_len);
    assert(!free_list || !*free_list);
    assert(!free_list_len || (*free_list_len == 0));

    /* Call the real (recursive) function */
    if(H5VL_iod_cs_send_helper(buf, type_info, nelem, addrs, sizes, num_segments, &segments_nalloc, 
                               vl_lengths, vl_lengths_size, &vl_lengths_nalloc, 
                               free_list, free_list_len, &free_list_nalloc) < 0)
        ERROR("failed to build segments");

    return(SUCCEED);

error:
    return(FAIL);
} /* end H5VL_iod_create_segments_send() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_cs_recv_helper
 *
 * Purpose:     Recursively builds the segments array, allocates buffers
 *              for variable-length data, and writes vlen pointers given
 *              (empty) buf, type_info, nelem, and the vl_lengths array.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
static int
H5VL_iod_cs_recv_helper(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    size_t *segments_nalloc, char *vl_lengths, size_t *vl_lengths_loc,
    size_t vl_lengths_size, void ***free_list, size_t *free_list_len,
    size_t *free_list_nalloc)
{
    _Bool wrapped = FALSE;
    _Bool wrappable;
    size_t span_size;
    size_t i;
    size_t j;

    assert(nelem > 0);

    if(type_info->vls) {
        /* Add segments for fixed-length spans */
        if(type_info->num_fl_spans > 0) {
            assert(type_info->fl_spans);

            /* Check if we can combine the last span in each element with the first
             * span in the next */
            wrappable = ((type_info->fl_spans[type_info->num_fl_spans - 1].offset
                    + type_info->fl_spans[type_info->num_fl_spans - 1].size)
                    == type_info->size) && (type_info->fl_spans[0].offset == 0);

            assert(!(wrappable && (type_info->num_fl_spans == 1)));

            /* Iterate over spans */
            for(i = 0; i < nelem; i++)
                for(j = 0; j < type_info->num_fl_spans; j++)
                    if(wrapped)
                        wrapped = FALSE;
                    else {
                        /* Add fixed-length segment to segments array */
                        if(*segments_nalloc == *num_segments) {
                            size_t tmp_nalloc;

                            tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                            if(NULL == (*addrs = (void **)realloc
                                        (*addrs, tmp_nalloc * sizeof(void *))))
                                ERROR("failed to reallocate array");

                            if(NULL == (*sizes = (size_t *)realloc
                                        (*sizes, tmp_nalloc * sizeof(size_t))))
                                ERROR("failed to reallocate array");

                            *segments_nalloc = tmp_nalloc;
                        } /* end if */

                        /* Check if we can wrap this segment */
                        if((j == (type_info->num_fl_spans - 1)) && wrappable
                                && (i != (nelem - 1))) {
                            span_size = type_info->fl_spans[0].size + type_info->fl_spans[j].size;
                            wrapped = TRUE;
                        } /* end if */
                        else
                            span_size = type_info->fl_spans[j].size;

                        /* Add segment */
                        (*addrs)[*num_segments] = (void *)(buf + (i * type_info->size) + 
                                                           type_info->fl_spans[j].offset);
                        (*sizes)[*num_segments] = span_size;
                        (*num_segments)++;
                    } /* end else */
        } /* end if */

        /* Analyze vlens */
        for(i = 0; i < nelem; i++)
            for(j = 0; j < type_info->num_vls; j++)
                if(type_info->vls[j].base_type) {
                    /* Standard vlen */
                    hvl_t *vl = (hvl_t *)(buf + (i * type_info->size) + type_info->vls[j].offset);
                    uint64_t vl_length;

                    /* Retrieve vl segment length from vl_lengths array */
                    if(*vl_lengths_loc + 8 > vl_lengths_size)
                        ERROR("vl_lengths array is too short");
                    UINT64DECODE(&vl_lengths[*vl_lengths_loc], vl_length);
                    *vl_lengths_loc += 8;
                    vl->len = (size_t)vl_length;

                    if(vl_length == 0)
                        vl->p = NULL;
                    else {
                        /* Allocate buffer for vlen data */
                        if(NULL == (vl->p = malloc(vl_length * type_info->vls[j].base_type->size)))
                            ERROR("failed to allocate vlen buffer");

                        /* Add malloc'ed buffer to free list, if present */
                        if(free_list) {
                            H5VL_IOD_ARR_ADD(void *, *free_list, *free_list_len, *free_list_nalloc, 64);

                            (*free_list)[*free_list_len] = vl->p;
                            (*free_list_len)++;
                        } /* end if */

                        /* Recurse into vlen data */
                        if(H5VL_iod_cs_recv_helper(vl->p, type_info->vls[j].base_type, 
                                                   (size_t)vl_length, addrs, sizes, 
                                                   num_segments, segments_nalloc, 
                                                   vl_lengths, vl_lengths_loc, vl_lengths_size, 
                                                   free_list, free_list_len, free_list_nalloc) < 0)
                            ERROR("failed to build segments");
                    } /* end if */
                } /* end if */
                else if(H5T_REFERENCE == type_info->type_class) {
                    href_ff_t *ref = (href_ff_t *)(buf + (i * type_info->size) + type_info->vls[j].offset);
                    uint64_t vl_length;

                    /* Retrieve buf size */
                    if(*vl_lengths_loc + 8 > vl_lengths_size)
                        ERROR("vl_lengths array is too short");
                    UINT64DECODE(&vl_lengths[*vl_lengths_loc], vl_length);
                    *vl_lengths_loc += 8;
                    ref->buf_size = (size_t)vl_length;

                    ref->ref_type = type_info->ref_type;

                    /* Allocate buffer for ref buffer */
                    if(NULL == (ref->buf = malloc(ref->buf_size)))
                        ERROR("failed to allocate ref buffer");

                    /* Add malloc'ed buffer to free list, if present */
                    if(free_list) {
                        H5VL_IOD_ARR_ADD(void *, *free_list, *free_list_len, *free_list_nalloc, 64);
                        (*free_list)[*free_list_len] = ref->buf;
                        (*free_list_len)++;
                    } /* end if */

                    /* Add segment to segments array */
                    if(*segments_nalloc == *num_segments) {
                        size_t tmp_nalloc;

                        tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                        if(NULL == (*addrs = (void **)realloc
                                    (*addrs, tmp_nalloc * sizeof(void *))))
                            ERROR("failed to reallocate array");

                        if(NULL == (*sizes = (size_t *)realloc
                                    (*sizes, tmp_nalloc * sizeof(size_t))))
                            ERROR("failed to reallocate array");

                        *segments_nalloc = tmp_nalloc;
                    } /* end if */

                    /* Add segment */
                    (*addrs)[*num_segments] = ref->buf;
                    (*sizes)[*num_segments] = ref->buf_size;
                    (*num_segments)++;
                    //printf("Recv Helper - ref: (%p, %zu) type = %d\n", ref->buf, ref->buf_size, ref->ref_type);
                }
                else {
                    /* Vlen string */
                    char **vl_s = (char **)(buf + (i * type_info->size) + type_info->vls[j].offset);
                    uint64_t vl_s_len;

                    /* Retrieve vl string length from vl_lengths array */
                    if(*vl_lengths_loc + 8 > vl_lengths_size)
                        ERROR("vl_lengths array is too short");
                    UINT64DECODE(&vl_lengths[*vl_lengths_loc], vl_s_len);
                    *vl_lengths_loc += 8;

                    if(vl_s_len == 0)
                        *vl_s = NULL;
                    else {
                        /* Allocate buffer for vlen string data */
                        if(NULL == (*vl_s = malloc((size_t)vl_s_len)))
                            ERROR("failed to allocate vlen string buffer");

                        /* Add malloc'ed buffer to free list, if present */
                        if(free_list) {
                            H5VL_IOD_ARR_ADD(void *, *free_list, *free_list_len, *free_list_nalloc, 64);

                            (*free_list)[*free_list_len] = *vl_s;
                            (*free_list_len)++;
                        } /* end if */

                        /* Add vl string to segments array, if length is greater
                         * than 1 (including null terminator) */
                        if(vl_s_len > 1) {
                            /* Add segment to segments array */
                            if(*segments_nalloc == *num_segments) {
                                size_t tmp_nalloc;

                                tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

                                if(NULL == (*addrs = (void **)realloc
                                            (*addrs, tmp_nalloc * sizeof(void *))))
                                    ERROR("failed to reallocate array");

                                if(NULL == (*sizes = (size_t *)realloc
                                            (*sizes, tmp_nalloc * sizeof(size_t))))
                                    ERROR("failed to reallocate array");

                                *segments_nalloc = tmp_nalloc;
                            } /* end if */

                            /* Add segment */
                            (*addrs)[*num_segments] = (void *)*vl_s;
                            (*sizes)[*num_segments] = vl_s_len - 1;
                            (*num_segments)++;
                        } /* end if */

                        /* Add null terminator */
                        (*vl_s)[vl_s_len - 1] = '\0';
                    } /* end if */
                } /* end else */
    } /* end if */
    else {
        /* No vlens, just add entire buffer to segments array */
        if(*segments_nalloc == *num_segments) {
            size_t tmp_nalloc;

            tmp_nalloc = (*num_segments) ? (*num_segments) * 2 : (256);

            if(NULL == (*addrs = (void **)realloc
                        (*addrs, tmp_nalloc * sizeof(void *))))
                ERROR("failed to reallocate array");

            if(NULL == (*sizes = (size_t *)realloc
                        (*sizes, tmp_nalloc * sizeof(size_t))))
                ERROR("failed to reallocate array");

            *segments_nalloc = tmp_nalloc;
        } /* end if */

        /* Add segment */
        (*addrs)[*num_segments] = (void *)buf;
        (*sizes)[*num_segments] = nelem * type_info->size;
        (*num_segments)++;
    } /* end else */

    return(SUCCEED);

error:
    return(FAIL);
} /* end H5VL_iod_cs_recv_helper() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_create_segments_recv
 *
 * Purpose:     Builds the segments array, allocates buffers for
 *              variable-length data, and writes vlen pointers given
 *              (empty) buf, type_info, nelem, and the vl_lengths array.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              Nov 19, 2013
 *
 *-------------------------------------------------------------------------
 */
int
H5VL_iod_create_segments_recv(char *buf, H5VL_iod_type_info_t *type_info,
    size_t nelem, void ***addrs, size_t **sizes, size_t *num_segments,
    char *vl_lengths, size_t vl_lengths_size, void ***free_list,
    size_t *free_list_len)
{
    size_t segments_nalloc = 0;
    size_t vl_lengths_loc = 0;
    size_t free_list_nalloc = 0;

    assert(buf);
    assert(type_info);
    assert(type_info->vls);
    assert(nelem > 0);
    assert(num_segments);
    assert(*num_segments == 0);
    assert(vl_lengths);
    assert(vl_lengths_size > 0);
    assert(!free_list == !free_list_len);
    assert(!free_list || !*free_list);
    assert(!free_list_len || (*free_list_len == 0));

    /* Call the real (recursive) function */
    if(H5VL_iod_cs_recv_helper(buf, type_info, nelem, addrs, sizes, num_segments, &segments_nalloc, 
                               vl_lengths, &vl_lengths_loc, vl_lengths_size, 
                               free_list, free_list_len, &free_list_nalloc) < 0)
        ERROR("failed to build segments");

    return(SUCCEED);

error:
    return(FAIL);
} /* end H5VL_iod_create_segments_recv() */


/*-------------------------------------------------------------------------
 * Function:    H5VL_iod_free_list_free
 *
 * Purpose:     Frees all pointers on the provided free list and then
 *              frees the free list itself
 *
 * Return:      void
 *
 * Programmer:  Neil Fortner
 *              Dec 5, 2013
 *
 *-------------------------------------------------------------------------
 */
void
H5VL_iod_free_list_free(void **free_list, size_t free_list_len)
{
    size_t i;

    for(i = 0; i < free_list_len; i++)
        free(free_list[i]);
    free(free_list);
} /* end H5VL_iod_free_list_free() */

#ifdef H5_HAVE_EFF
uint64_t 
H5_checksum_crc64(const void *buf, size_t buf_size)
{
    const char *hash_method = "crc64";
    size_t hash_size;
    mchecksum_object_t checksum;
    uint64_t ret_value = 0;

    /* Initialize checksum */
    mchecksum_init(hash_method, &checksum);

    /* Update checksum */
    mchecksum_update(checksum, buf, buf_size);

    /* Get size of checksum */
    hash_size = mchecksum_get_size(checksum);

    assert(hash_size == sizeof(uint64_t));

    /* get checksum value */
    mchecksum_get(checksum, &ret_value, hash_size, 1);

    /* Destroy checksum */
    mchecksum_destroy(checksum);

    return ret_value;
}

uint64_t 
H5_checksum_crc64_segments(void **addrs, size_t *sizes, size_t count)
{
    const char *hash_method = "crc64";
    size_t hash_size;
    size_t i;
    mchecksum_object_t checksum;
    uint64_t ret_value;

    /* Initialize checksum */
    mchecksum_init(hash_method, &checksum);

    /* Update checksum */
    for (i = 0; i < count; i++) {
        mchecksum_update(checksum, addrs[i], sizes[i]);
    }

    /* Get size of checksum */
    hash_size = mchecksum_get_size(checksum);

    assert(hash_size == sizeof(uint64_t));

    /* get checksum value */
    mchecksum_get(checksum, &ret_value, hash_size, 1);

    /* Destroy checksum */
    mchecksum_destroy(checksum);

    return ret_value;
}

uint64_t 
H5_checksum_crc64_fragments(void **buf, size_t *buf_size, size_t count)
{
    const char *hash_method = "crc64";
    size_t hash_size;
    size_t i;
    mchecksum_object_t checksum;
    uint64_t ret_value;

    /* Initialize checksum */
    mchecksum_init(hash_method, &checksum);

    /* Update checksum */
    for (i = 0; i < count; i++) {
        mchecksum_update(checksum, buf[i], buf_size[i]);
    }

    /* Get size of checksum */
    hash_size = mchecksum_get_size(checksum);

    assert(hash_size == sizeof(uint64_t));

    /* get checksum value */
    mchecksum_get(checksum, &ret_value, hash_size, 1);

    /* Destroy checksum */
    mchecksum_destroy(checksum);

    return ret_value;
}

void 
EFF__mercury_register_callbacks(void)
{
    /* Register function and encoding/decoding functions */
    H5VL_EFF_INIT_ID     = MERCURY_REGISTER("eff_init", eff_init_in_t, ret_t,
                                            H5VL_iod_server_eff_init);
    H5VL_EFF_FINALIZE_ID = MERCURY_REGISTER("eff_finalize", ret_t, ret_t,
                                            H5VL_iod_server_eff_finalize);

    H5VL_ANALYSIS_INVOKE_ID = MERCURY_REGISTER("analysis_invoke", 
                                                analysis_invoke_in_t, 
                                                analysis_invoke_out_t,
                                                H5VL_iod_server_analysis_invoke);

    H5VL_FILE_CREATE_ID = MERCURY_REGISTER("file_create", file_create_in_t, file_create_out_t,
                                           H5VL_iod_server_file_create);
    H5VL_FILE_OPEN_ID   = MERCURY_REGISTER("file_open", file_open_in_t, file_open_out_t,
                                           H5VL_iod_server_file_open);
    H5VL_FILE_CLOSE_ID  = MERCURY_REGISTER("file_close", file_close_in_t, ret_t,
                                           H5VL_iod_server_file_close);

    H5VL_ATTR_CREATE_ID = MERCURY_REGISTER("attr_create", attr_create_in_t, attr_create_out_t,
                                           H5VL_iod_server_attr_create);
    H5VL_ATTR_OPEN_ID   = MERCURY_REGISTER("attr_open", attr_open_in_t, attr_open_out_t,
                                           H5VL_iod_server_attr_open);
    H5VL_ATTR_READ_ID   = MERCURY_REGISTER("attr_read", attr_io_in_t, ret_t,
                                           H5VL_iod_server_attr_read);
    H5VL_ATTR_WRITE_ID  = MERCURY_REGISTER("attr_write", attr_io_in_t, ret_t,
                                           H5VL_iod_server_attr_write);
    H5VL_ATTR_EXISTS_ID = MERCURY_REGISTER("attr_exists", attr_op_in_t, htri_t,
                                           H5VL_iod_server_attr_exists);
    H5VL_ATTR_ITERATE_ID = MERCURY_REGISTER("attr_iterate", attr_op_in_t, attr_iterate_t, 
                                            H5VL_iod_server_attr_iterate);
    H5VL_ATTR_RENAME_ID = MERCURY_REGISTER("attr_rename", attr_rename_in_t, ret_t,
                                           H5VL_iod_server_attr_rename);
    H5VL_ATTR_REMOVE_ID = MERCURY_REGISTER("attr_remove", attr_op_in_t, ret_t,
                                           H5VL_iod_server_attr_remove);
    H5VL_ATTR_CLOSE_ID  = MERCURY_REGISTER("attr_close", attr_close_in_t, ret_t,
                                           H5VL_iod_server_attr_close);

    H5VL_GROUP_CREATE_ID = MERCURY_REGISTER("group_create", group_create_in_t, group_create_out_t,
                                            H5VL_iod_server_group_create);
    H5VL_GROUP_OPEN_ID   = MERCURY_REGISTER("group_open", group_open_in_t, group_open_out_t,
                                            H5VL_iod_server_group_open);
    H5VL_GROUP_CLOSE_ID  = MERCURY_REGISTER("group_close", group_close_in_t, ret_t,
                                            H5VL_iod_server_group_close);

    H5VL_MAP_CREATE_ID = MERCURY_REGISTER("map_create", map_create_in_t, map_create_out_t,
                                          H5VL_iod_server_map_create);
    H5VL_MAP_OPEN_ID   = MERCURY_REGISTER("map_open", map_open_in_t, map_open_out_t,
                                          H5VL_iod_server_map_open);
    H5VL_MAP_SET_ID    = MERCURY_REGISTER("map_set", map_set_in_t, ret_t,
                                          H5VL_iod_server_map_set);
    H5VL_MAP_GET_ID    = MERCURY_REGISTER("map_get", map_get_in_t, map_get_out_t,
                                          H5VL_iod_server_map_get);
    H5VL_MAP_GET_COUNT_ID = MERCURY_REGISTER("map_get_count", map_get_count_in_t, int64_t,
                                             H5VL_iod_server_map_get_count);
    //H5VL_MAP_ITERATE_ID   = MERCURY_REGISTER("map_iterate", map_op_in_t, ret_t);
    H5VL_MAP_EXISTS_ID = MERCURY_REGISTER("map_exists", map_op_in_t, hbool_t,
                                          H5VL_iod_server_map_exists);
    H5VL_MAP_DELETE_ID = MERCURY_REGISTER("map_delete", map_op_in_t, ret_t,
                                          H5VL_iod_server_map_delete);
    H5VL_MAP_CLOSE_ID  = MERCURY_REGISTER("map_close", map_close_in_t, ret_t,
                                          H5VL_iod_server_map_close);

    H5VL_DSET_CREATE_ID = MERCURY_REGISTER("dset_create", dset_create_in_t, dset_create_out_t,
                                           H5VL_iod_server_dset_create);
    H5VL_DSET_OPEN_ID   = MERCURY_REGISTER("dset_open", dset_open_in_t, dset_open_out_t,
                                           H5VL_iod_server_dset_open);
    H5VL_DSET_READ_ID   = MERCURY_REGISTER("dset_read", dset_io_in_t, dset_read_out_t,
                                           H5VL_iod_server_dset_read);
    H5VL_DSET_MULTI_READ_ID   = MERCURY_REGISTER("dset_multi_read", dset_multi_io_in_t, ret_t,
                                           H5VL_iod_server_dset_multi_read);
    H5VL_DSET_GET_VL_SIZE_ID = MERCURY_REGISTER("dset_get_vl_size", dset_io_in_t, dset_read_out_t,
                                                H5VL_iod_server_dset_get_vl_size);
    H5VL_DSET_WRITE_ID  = MERCURY_REGISTER("dset_write", dset_io_in_t, ret_t,
                                           H5VL_iod_server_dset_write);
    H5VL_DSET_MULTI_WRITE_ID  = MERCURY_REGISTER("dset_multi_write", dset_multi_io_in_t, ret_t,
                                           H5VL_iod_server_dset_multi_write);
    H5VL_DSET_SET_EXTENT_ID = MERCURY_REGISTER("dset_set_extent", 
                                               dset_set_extent_in_t, ret_t,
                                               H5VL_iod_server_dset_set_extent);
    H5VL_DSET_CLOSE_ID  = MERCURY_REGISTER("dset_close", dset_close_in_t, ret_t,
                                           H5VL_iod_server_dset_close);

    H5VL_DTYPE_COMMIT_ID = MERCURY_REGISTER("dtype_commit", dtype_commit_in_t, dtype_commit_out_t,
                                            H5VL_iod_server_dtype_commit);
    H5VL_DTYPE_OPEN_ID   = MERCURY_REGISTER("dtype_open", dtype_open_in_t, dtype_open_out_t,
                                            H5VL_iod_server_dtype_open);
    H5VL_DTYPE_CLOSE_ID  = MERCURY_REGISTER("dtype_close", dtype_close_in_t, ret_t,
                                            H5VL_iod_server_dtype_close);

    H5VL_LINK_CREATE_ID  = MERCURY_REGISTER("link_create", link_create_in_t, ret_t,
                                            H5VL_iod_server_link_create);
    H5VL_LINK_MOVE_ID    = MERCURY_REGISTER("link_move", link_move_in_t, ret_t,
                                            H5VL_iod_server_link_move);
    H5VL_LINK_EXISTS_ID  = MERCURY_REGISTER("link_exists", link_op_in_t, htri_t,
                                            H5VL_iod_server_link_exists);
    H5VL_LINK_GET_INFO_ID = MERCURY_REGISTER("link_get_info", link_op_in_t, linfo_t,
                                             H5VL_iod_server_link_get_info);
    H5VL_LINK_GET_VAL_ID  = MERCURY_REGISTER("link_get_val", link_get_val_in_t, 
                                             link_get_val_out_t, H5VL_iod_server_link_get_val);
    H5VL_LINK_ITERATE_ID = MERCURY_REGISTER("link_iterate", link_op_in_t, link_iterate_t,  
                                            H5VL_iod_server_link_iterate);
    H5VL_LINK_REMOVE_ID  = MERCURY_REGISTER("link_remove", link_op_in_t, ret_t,
                                            H5VL_iod_server_link_remove);

    H5VL_OBJECT_OPEN_BY_TOKEN_ID = MERCURY_REGISTER("object_open_by_token", 
                                                    object_token_in_t, iod_handles_t,
                                                    H5VL_iod_server_object_open_by_token);
    H5VL_OBJECT_OPEN_ID   = MERCURY_REGISTER("object_open", object_op_in_t, object_open_out_t,
                                             H5VL_iod_server_object_open);
    H5VL_OBJECT_OPEN_BY_ADDR_ID = MERCURY_REGISTER("object_open_by_addr", object_op_in_t, object_open_out_t,
                                             H5VL_iod_server_object_open_by_addr);
    //H5VL_OBJECT_COPY_ID   = MERCURY_REGISTER("object_copy", object_copy_in_t, ret_t);
    H5VL_OBJECT_EXISTS_ID = MERCURY_REGISTER("object_exists", object_op_in_t, htri_t,
                                             H5VL_iod_server_object_exists);
    H5VL_OBJECT_VISIT_ID  = MERCURY_REGISTER("object_visit", object_op_in_t, obj_iterate_t,
                                             H5VL_iod_server_object_visit);
    H5VL_OBJECT_SET_COMMENT_ID = MERCURY_REGISTER("set_comment", object_set_comment_in_t, ret_t,
                                                  H5VL_iod_server_object_set_comment);
    H5VL_OBJECT_GET_COMMENT_ID = MERCURY_REGISTER("get_comment", object_get_comment_in_t, 
                                                  object_get_comment_out_t,
                                                  H5VL_iod_server_object_get_comment);
    H5VL_OBJECT_GET_INFO_ID = MERCURY_REGISTER("object_get_info", object_op_in_t, oinfo_t,
                                               H5VL_iod_server_object_get_info);

    H5VL_RC_ACQUIRE_ID      = MERCURY_REGISTER("read_context_acquire", 
                                               rc_acquire_in_t, rc_acquire_out_t,
                                               H5VL_iod_server_rcxt_acquire);
    H5VL_RC_RELEASE_ID      = MERCURY_REGISTER("read_context_release", rc_release_in_t, ret_t,
                                               H5VL_iod_server_rcxt_release);
    H5VL_RC_PERSIST_ID      = MERCURY_REGISTER("read_context_persist", rc_persist_in_t, ret_t,
                                               H5VL_iod_server_rcxt_persist);
    H5VL_RC_SNAPSHOT_ID     = MERCURY_REGISTER("read_context_snapshot", rc_snapshot_in_t, ret_t,
                                               H5VL_iod_server_rcxt_snapshot);

    H5VL_TR_START_ID        = MERCURY_REGISTER("transaction_start", tr_start_in_t, ret_t,
                                               H5VL_iod_server_trans_start);
    H5VL_TR_FINISH_ID       = MERCURY_REGISTER("transaction_finish", tr_finish_in_t, ret_t,
                                               H5VL_iod_server_trans_finish);
    H5VL_TR_SET_DEPEND_ID   = MERCURY_REGISTER("transaction_set_depend",tr_set_depend_in_t, ret_t,
                                               H5VL_iod_server_trans_set_dependency);
    H5VL_TR_SKIP_ID         = MERCURY_REGISTER("transaction_skip", tr_skip_in_t, ret_t,
                                               H5VL_iod_server_trans_skip);
    H5VL_TR_ABORT_ID        = MERCURY_REGISTER("transaction_abort",tr_abort_in_t, ret_t,
                                               H5VL_iod_server_trans_abort);

    H5VL_PREFETCH_ID = MERCURY_REGISTER("prefetch", prefetch_in_t, hrpl_t,
                                        H5VL_iod_server_prefetch);
    H5VL_EVICT_ID    = MERCURY_REGISTER("evict", evict_in_t, ret_t,
                                        H5VL_iod_server_evict);

    H5VL_VIEW_CREATE_ID = MERCURY_REGISTER("view_create", view_create_in_t, view_create_out_t,
                                           H5VL_iod_server_view_create);

#ifdef H5_HAVE_INDEXING
    H5VL_DSET_SET_INDEX_INFO_ID = MERCURY_REGISTER("dset_set_index_info",
                                                   dset_set_index_info_in_t, ret_t,
                                                   H5VL_iod_server_dset_set_index_info);
    H5VL_DSET_GET_INDEX_INFO_ID = MERCURY_REGISTER("dset_get_index_info",
                                                   dset_get_index_info_in_t, 
                                                   dset_get_index_info_out_t,
                                                   H5VL_iod_server_dset_get_index_info);
    H5VL_DSET_RM_INDEX_INFO_ID = MERCURY_REGISTER("dset_rm_index_info",
                                                  dset_rm_index_info_in_t, ret_t,
                                                  H5VL_iod_server_dset_remove_index_info);
#endif

    H5VL_CANCEL_OP_ID = MERCURY_REGISTER("cancel_op", uint64_t, uint8_t,
                                         H5VL_iod_server_cancel_op);
}

#endif /* H5_HAVE_EFF */
