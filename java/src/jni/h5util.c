/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    https://portal.hdfgroup.org/documentation/index.html
 *
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <jni.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5util.h"

/* size of hyperslab buffer when a dataset is bigger than H5TOOLS_MALLOCSIZE */
hsize_t H5TOOLS_BUFSIZE    = (32 * 1024 * 1024); /* 32 MB */
int     H5TOOLS_TEXT_BLOCK = 16;                 /* Number of elements on a line in a text export file */

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
JavaVM *jvm;

jobject copy_callback;
jobject close_callback;
jobject create_callback;
jobject compare_callback;
jobject get_callback;
jobject set_callback;
jobject delete_callback;

H5E_auto2_t efunc;
void       *edata;

/********************/
/* Local Prototypes */
/********************/

int h5str_old_region_dataset(JNIEnv *env, h5str_t *out_str, hid_t container, void *ref_buf, int expand_data);
int h5str_region_dataset(JNIEnv *env, h5str_t *out_str, H5R_ref_t *ref_vp, int expand_data);

static int    h5str_dump_region_blocks(JNIEnv *env, h5str_t *str, hid_t region, hid_t region_obj,
                                       int expand_data);
static int    h5str_dump_region_points(JNIEnv *env, h5str_t *str, hid_t region, hid_t region_obj,
                                       int expand_data);
static int    h5str_dump_region_attribute(JNIEnv *env, h5str_t *str, hid_t region_id);
static int    h5str_is_zero(const void *_mem, size_t size);
static hid_t  h5str_get_native_type(hid_t type);
static hid_t  h5str_get_little_endian_type(hid_t type);
static hid_t  h5str_get_big_endian_type(hid_t type);
static htri_t h5str_detect_vlen_str(hid_t tid);
static int    h5str_dump_simple_data(JNIEnv *env, FILE *stream, hid_t container, hid_t type, void *_mem,
                                     hsize_t nelmts);
static int    h5str_render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem,
                                      hsize_t block_nelmts);
static int    render_bin_output_region_data_blocks(FILE *stream, hid_t region_id, hid_t container, int ndims,
                                                   hid_t type_id, hssize_t nblocks, hsize_t *ptdata);
static int    render_bin_output_region_blocks(FILE *stream, hid_t region_space, hid_t region_id,
                                              hid_t container);
static int    render_bin_output_region_data_points(FILE *stream, hid_t region_space, hid_t region_id,
                                                   hid_t container, int ndims, hid_t type_id, hssize_t npoints,
                                                   hsize_t *ptdata);
static int    render_bin_output_region_points(FILE *stream, hid_t region_space, hid_t region_id,
                                              hid_t container);
jobject       translate_atomic_rbuf(JNIEnv *env, jlong mem_type_id, H5T_class_t type_class, void *raw_buf);
void          translate_atomic_wbuf(JNIEnv *env, jobject in_obj, jlong mem_type_id, H5T_class_t type_class,
                                    void *raw_buf);

/* Strings for output */
#define H5_TOOLS_GROUP    "GROUP"
#define H5_TOOLS_DATASET  "DATASET"
#define H5_TOOLS_DATATYPE "DATATYPE"
#define H5_TOOLS_MAP      "MAP"

/** frees memory held by array of strings */
void
h5str_array_free(char **strs, size_t len)
{
    size_t i;

    if (!strs || len <= 0)
        return;

    for (i = 0; i < len; i++) {
        if (strs[i])
            free(strs[i]);
    } /* for (i=0; i<n; i++)*/

    free(strs);
} /* end h5str_array_free */

/** allocate a new str with given length */
void
h5str_new(h5str_t *str, size_t len)
{
    if (str && len > 0) {
        str->s    = (char *)malloc(len);
        str->max  = len;
        str->s[0] = '\0';
    } /* end if */
} /* end h5str_new */

/** free string memory */
void
h5str_free(h5str_t *str)
{
    if (str && str->max > 0) {
        free(str->s);
        memset(str, 0, sizeof(h5str_t));
    } /* end if */
} /* end h5str_free */

/** reset the max size of the string */
/*
 * TODO: no error return. malloc can fail.
 */
void
h5str_resize(h5str_t *str, size_t new_len)
{
    char *new_str;

    if (!str || new_len <= 0 || str->max == new_len)
        return;

    if (NULL == (new_str = (char *)malloc(new_len)))
        return;

    if (new_len > str->max) /* increase memory */
        strcpy(new_str, str->s);
    else
        strncpy(new_str, str->s, new_len - 1);

    free(str->s);
    str->s   = new_str;
    str->max = new_len;
} /* end h5str_resize */

/* appends a copy of the string pointed to by cstr to the h5str.
 Return Value:
 the char string point to str->s
 */
char *
h5str_append(h5str_t *str, const char *cstr)
{
    size_t len;

    if (!str)
        return NULL;
    else if (!cstr)
        return str->s;

    len = strlen(str->s) + strlen(cstr);
    while (len >= str->max) /* not enough to hold the new string, double the space */
    {
        h5str_resize(str, str->max * 2);
    }

    return strcat(str->s, cstr);
} /* end h5str_append */

/*
 * Converts the given data point string into a real data point.
 *
 * Returns:
 *        SUCCESS: Length of string token processed
 *        FAILURE: 0
 */
size_t
h5str_convert(JNIEnv *env, char **in_str, hid_t container, hid_t tid, void *out_buf, size_t out_buf_offset)
{
    unsigned char *ucptr = NULL;
    char           fmt_llong[8], fmt_ullong[8];
    H5T_class_t    tclass      = H5T_NO_CLASS;
    const char     delimiter[] = " ," H5_COMPOUND_BEGIN_INDICATOR H5_COMPOUND_END_INDICATOR
        H5_ARRAY_BEGIN_INDICATOR H5_ARRAY_END_INDICATOR H5_VLEN_BEGIN_INDICATOR H5_VLEN_END_INDICATOR;

    size_t retVal   = 0;
    size_t typeSize = 0;
    hid_t  mtid     = H5I_INVALID_HID;
    char  *this_str = NULL;
    char  *cptr     = NULL;
    char  *token;
    int    n;

    if (!in_str)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "h5str_convert: in_str is NULL");
    if (!out_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "h5str_convert: out_buf is NULL");

    this_str = *in_str;
    cptr     = &(((char *)out_buf)[out_buf_offset]);
    ucptr    = &(((unsigned char *)out_buf)[out_buf_offset]);

    if (H5T_NO_CLASS == (tclass = H5Tget_class(tid)))
        H5_LIBRARY_ERROR(ENVONLY);
    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
        if (snprintf(fmt_llong, sizeof(fmt_llong), "%%lld") < 0)
            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_convert: snprintf failure");
        if (snprintf(fmt_ullong, sizeof(fmt_ullong), "%%llu") < 0)
            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_convert: snprintf failure");
    } /* end if */

    switch (tclass) {
        case H5T_FLOAT: {
            token = strtok(this_str, delimiter);

            switch (typeSize) {
                case sizeof(float): {
                    float tmp_float = 0.0f;

                    sscanf(token, "%f", &tmp_float);
                    memcpy(cptr, &tmp_float, sizeof(float));
                    break;
                }

                case sizeof(double): {
                    double tmp_double = 0.0;

                    sscanf(token, "%lf", &tmp_double);
                    memcpy(cptr, &tmp_double, sizeof(double));
                    break;
                }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
                case sizeof(long double): {
                    long double tmp_ldouble = 0.0L;

                    sscanf(token, "%Lg", &tmp_ldouble);
                    memcpy(cptr, &tmp_ldouble, sizeof(long double));
                    break;
                }
#endif

                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY,
                        "h5str_convert: floating-point datatype size didn't match any of expected sizes");
                    break;
            }

            retVal = typeSize;

            break;
        }

        case H5T_STRING: {
            size_t len = strlen(this_str);

            if (len > 0) {
                strncpy(cptr, this_str, typeSize);
                cptr[typeSize - 1] = '\0';

                retVal = typeSize;
            }
            else {
                cptr = NULL;
            }

            break;
        }

        case H5T_INTEGER: {
            H5T_sign_t nsign = H5T_SGN_ERROR;

            if (H5T_SGN_ERROR == (nsign = H5Tget_sign(tid)))
                H5_LIBRARY_ERROR(ENVONLY);

            token = strtok(this_str, delimiter);

            switch (typeSize) {
                case sizeof(char): {
                    unsigned char tmp_uchar = 0;
                    signed char   tmp_char  = 0;

                    if (H5T_SGN_NONE == nsign) {
                        sscanf(token, "%hhu", &tmp_uchar);
                        memcpy(cptr, &tmp_uchar, sizeof(unsigned char));
                    }
                    else {
                        sscanf(token, "%hhd", &tmp_char);
                        memcpy(cptr, &tmp_char, sizeof(char));
                    }

                    break;
                }

                case sizeof(short): {
                    unsigned short tmp_ushort = 0;
                    short          tmp_short  = 0;

                    if (H5T_SGN_NONE == nsign) {
                        sscanf(token, "%hu", &tmp_ushort);
                        memcpy(&tmp_ushort, cptr, sizeof(unsigned short));
                    }
                    else {
                        sscanf(token, "%hd", &tmp_short);
                        memcpy(&tmp_short, cptr, sizeof(short));
                    }

                    break;
                }

                case sizeof(int): {
                    unsigned int tmp_uint = 0;
                    int          tmp_int  = 0;

                    if (H5T_SGN_NONE == nsign) {
                        sscanf(token, "%u", &tmp_uint);
                        memcpy(cptr, &tmp_uint, sizeof(unsigned int));
                    }
                    else {
                        sscanf(token, "%d", &tmp_int);
                        memcpy(cptr, &tmp_int, sizeof(int));
                    }

                    break;
                }

#if H5_SIZEOF_LONG != H5_SIZEOF_INT
                case sizeof(long): {
                    unsigned long tmp_ulong = 0;
                    long          tmp_long  = 0;

                    if (H5T_SGN_NONE == nsign) {
                        sscanf(token, "%lu", &tmp_ulong);
                        memcpy(cptr, &tmp_ulong, sizeof(unsigned long));
                    }
                    else {
                        sscanf(token, "%ld", &tmp_long);
                        memcpy(cptr, &tmp_long, sizeof(long));
                    }

                    break;
                }
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
                case sizeof(long long): {
                    unsigned long long tmp_ullong = 0;
                    long long          tmp_llong  = 0;

                    if (H5T_SGN_NONE == nsign) {
                        sscanf(token, fmt_ullong, &tmp_ullong);
                        memcpy(cptr, &tmp_ullong, sizeof(unsigned long long));
                    }
                    else {
                        sscanf(token, fmt_llong, &tmp_llong);
                        memcpy(cptr, &tmp_llong, sizeof(long long));
                    }

                    break;
                }
#endif

                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY, "h5str_convert: integer datatype size didn't match any of expected sizes");
                    break;
            }

            retVal = typeSize;

            break;
        }

        case H5T_COMPOUND: {
            unsigned i;
            size_t   member_offset;

            if ((n = H5Tget_nmembers(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            /* Skip whitespace and compound indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == '{')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            for (i = 0; i < (unsigned)n; i++) {
                member_offset = H5Tget_member_offset(tid, i);

                if ((mtid = H5Tget_member_type(tid, i)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                if (!h5str_convert(ENVONLY, &this_str, container, mtid, out_buf, member_offset)) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    goto done;
                }

                /* Skip whitespace and commas */
                while (*this_str == ' ')
                    this_str++;
                if (*this_str == ',')
                    this_str++;
                while (*this_str == ' ')
                    this_str++;

                if (H5Tclose(mtid) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                mtid = H5I_INVALID_HID;
            }

            /* Skip whitespace and compound indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == '}')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            retVal = typeSize * (size_t)n;

            break;
        }

        /* TODO handle reference writing */
        case H5T_REFERENCE:
            cptr = NULL;
            break;

        case H5T_ENUM: {
            void          *value      = NULL;
            unsigned char  tmp_uchar  = 0;
            unsigned short tmp_ushort = 0;
            unsigned int   tmp_uint   = 0;
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
            unsigned long tmp_ulong = 0;
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
            unsigned long long tmp_ullong = 0;
#endif

            token = strtok(this_str, delimiter);

            switch (typeSize) {
                case sizeof(char): {
                    value = &tmp_uchar;
                    break;
                }

                case sizeof(short): {
                    value = &tmp_ushort;
                    break;
                }
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
                case sizeof(long): {
                    value = &tmp_ulong;
                    break;
                }
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
                case sizeof(long long): {
                    value = &tmp_ullong;
                    break;
                }
#endif
                default: {
                    value = &tmp_uint;
                    break;
                }
            }

            if (H5Tenum_valueof(tid, token, value) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            memcpy(ucptr, value, typeSize);

            retVal = typeSize;

            break;
        }

        case H5T_ARRAY: {
            hsize_t i, dims[H5S_MAX_RANK], total_elmts;
            size_t  baseTypeSize;
            int     rank = 0;

            /* Skip whitespace and array indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == '[')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            if ((mtid = H5Tget_super(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (!(baseTypeSize = H5Tget_size(mtid)))
                H5_LIBRARY_ERROR(ENVONLY);

            if ((rank = H5Tget_array_ndims(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (H5Tget_array_dims2(tid, dims) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            for (i = 0, total_elmts = 1; i < (hsize_t)rank; i++)
                total_elmts *= dims[i];

            if (NULL == (cptr = (char *)calloc((size_t)total_elmts, baseTypeSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_convert: failed to allocate array buffer");

            for (i = 0; i < total_elmts; i++) {
                if (!(h5str_convert(ENVONLY, &this_str, container, mtid, out_buf, i * baseTypeSize))) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    goto done;
                }

                /* Skip whitespace and commas */
                while (*this_str == ' ')
                    this_str++;
                if (*this_str == ',')
                    this_str++;
                while (*this_str == ' ')
                    this_str++;
            }

            if (H5Tclose(mtid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            mtid = H5I_INVALID_HID;

            /* Skip whitespace and array indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == ']')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            retVal = typeSize * total_elmts;

            break;
        }

        case H5T_VLEN: {
            size_t i, baseTypeSize;
            hvl_t *vl_buf = (hvl_t *)out_buf;
            char   cur_char;

            if ((mtid = H5Tget_super(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (!(baseTypeSize = H5Tget_size(mtid)))
                H5_LIBRARY_ERROR(ENVONLY);

            if (NULL == (vl_buf->p = malloc(baseTypeSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_convert: failed to allocate vlen buffer");
            vl_buf->len = 1;

            /* Skip whitespace and vlen indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == '(')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            cur_char = *this_str;
            for (i = 0; cur_char != ')' && cur_char != '\0'; i++) {
                if (i >= vl_buf->len) {
                    char *tmp_realloc;

                    if (NULL == (tmp_realloc = (char *)realloc(vl_buf->p, vl_buf->len * 2 * baseTypeSize)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_convert: failed to reallocate vlen buffer");
                    vl_buf->len *= 2;
                    vl_buf->p = tmp_realloc;
                }

                if (!(h5str_convert(ENVONLY, &this_str, container, mtid, vl_buf->p, i * baseTypeSize))) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    vl_buf->len = i;
                    goto done;
                }

                /* Skip whitespace and commas */
                while (*this_str == ' ')
                    this_str++;
                if (*this_str == ',')
                    this_str++;
                while (*this_str == ' ')
                    this_str++;
            }

            vl_buf->len = i;

            if (H5Tclose(mtid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            mtid = H5I_INVALID_HID;

            /* Skip whitespace and vlen indicators */
            while (*this_str == ' ')
                this_str++;
            if (*this_str == ')')
                this_str++;
            while (*this_str == ' ')
                this_str++;

            retVal = typeSize;

            break;
        }

        case H5T_NCLASSES:
        case H5T_NO_CLASS: {
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_convert: invalid datatype class");
            break;
        }

        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        default: {
            /* All other types get copied raw */
            memcpy(ucptr, this_str, typeSize);

            retVal = typeSize;

            break;
        }
    } /* end switch */

done:
    if (mtid >= 0)
        H5Tclose(mtid);

    return retVal;
} /* end h5str_convert */

/*-------------------------------------------------------------------------
 * Function:    h5str_sprint_old_reference
 *
 * Purpose: Object reference -- show the name of the 1.10 referenced object.
 *
 * Return:  SUCCEED or FAIL
 *-------------------------------------------------------------------------
 */
int
h5str_sprint_old_reference(JNIEnv *env, h5str_t *out_str, hid_t region_obj, void *ref_buf)
{
    char ref_name[1024];
    int  ret_value = FAIL;

    if ((H5Rget_name(region_obj, H5R_DATASET_REGION, ref_buf, (char *)ref_name, 1024)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    if (!h5str_append(out_str, ref_name))
        H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

    ret_value = SUCCEED;

done:
    return ret_value;
} /* h5str_sprint_reference */

/*-------------------------------------------------------------------------
 * Function:    h5str_sprint_reference
 *
 * Purpose: Object reference -- show the name of the referenced object.
 *
 * Return:  SUCCEED or FAIL
 *-------------------------------------------------------------------------
 */
int
h5str_sprint_reference(JNIEnv *env, h5str_t *out_str, void *ref_p)
{
    ssize_t    buf_size;
    char      *ref_name = NULL;
    H5R_ref_t *ref_vp   = (H5R_ref_t *)ref_p;

    int ret_value = FAIL;

    buf_size = H5Rget_file_name(ref_vp, NULL, 0);
    if (buf_size > 0) {
        ref_name = (char *)malloc(sizeof(char) * (size_t)buf_size + 1);
        if (H5Rget_file_name(ref_vp, ref_name, (size_t)buf_size + 1) >= 0) {
            ref_name[buf_size] = '\0';
            if (!h5str_append(out_str, ref_name))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
        }
        free(ref_name);
        ref_name = NULL;
    }

    buf_size = H5Rget_obj_name(ref_vp, H5P_DEFAULT, NULL, 0);
    if (buf_size > 0) {
        ref_name = (char *)malloc(sizeof(char) * (size_t)buf_size + 1);
        if (H5Rget_obj_name(ref_vp, H5P_DEFAULT, ref_name, (size_t)buf_size + 1) >= 0) {
            ref_name[buf_size] = '\0';
            if (!h5str_append(out_str, ref_name))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
        }
        free(ref_name);
        ref_name = NULL;
    }

    if (H5Rget_type(ref_vp) == H5R_ATTR) {
        buf_size = H5Rget_attr_name(ref_vp, NULL, 0);
        if (buf_size > 0) {
            ref_name = (char *)malloc(sizeof(char) * (size_t)buf_size + 1);
            if (H5Rget_attr_name(ref_vp, ref_name, (size_t)buf_size + 1) >= 0) {
                ref_name[buf_size] = '\0';
                if (!h5str_append(out_str, ref_name))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }
            free(ref_name);
            ref_name = NULL;
        }
    }

    ret_value = SUCCEED;
done:
    if (ref_name)
        free(ref_name);

    return ret_value;
} /* h5str_sprint_reference */

int
h5str_old_region_dataset(JNIEnv *env, h5str_t *out_str, hid_t container, void *ref_buf, int expand_data)
{
    H5S_sel_type region_type = H5S_SEL_ERROR;
    hid_t        region_obj  = H5I_INVALID_HID;
    hid_t        region_sid  = H5I_INVALID_HID;

    int ret_value = FAIL;

    if ((region_obj = H5Rdereference2(container, H5P_DEFAULT, H5R_DATASET_REGION, ref_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((region_sid = H5Rget_region(container, H5R_DATASET_REGION, ref_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (expand_data == 0)
        if (h5str_sprint_old_reference(ENVONLY, out_str, region_obj, ref_buf) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if ((region_type = H5Sget_select_type(region_sid)) > H5S_SEL_ERROR) {
        if (H5S_SEL_POINTS == region_type) {
            if (h5str_dump_region_points(ENVONLY, out_str, region_sid, region_obj, expand_data) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
        else if (H5S_SEL_HYPERSLABS == region_type) {
            if (h5str_dump_region_blocks(ENVONLY, out_str, region_sid, region_obj, expand_data) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
    }

    ret_value = SUCCEED;
done:
    if (region_sid >= 0)
        if (H5Sclose(region_sid) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    if (region_obj >= 0)
        if (H5Dclose(region_obj) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

    return ret_value;
}

int
h5str_region_dataset(JNIEnv *env, h5str_t *out_str, H5R_ref_t *ref_vp, int expand_data)
{
    hid_t        new_obj_id  = H5I_INVALID_HID;
    hid_t        new_obj_sid = H5I_INVALID_HID;
    H5S_sel_type region_type;

    int ret_value = FAIL;

    if ((new_obj_id = H5Ropen_object(ref_vp, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        ret_value = SUCCEED; /* An uncreated region is a valid state */
        goto done;
    }

    if ((new_obj_sid = H5Ropen_region(ref_vp, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (expand_data == 0)
        if (h5str_sprint_reference(ENVONLY, out_str, ref_vp) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if ((region_type = H5Sget_select_type(new_obj_sid)) > H5S_SEL_ERROR) {
        if (H5S_SEL_POINTS == region_type) {
            if (h5str_dump_region_points(ENVONLY, out_str, new_obj_sid, new_obj_id, expand_data) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
        else if (H5S_SEL_HYPERSLABS == region_type) {
            if (h5str_dump_region_blocks(ENVONLY, out_str, new_obj_sid, new_obj_id, expand_data) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
    }

    ret_value = SUCCEED;
done:
    if (new_obj_sid >= 0)
        if (H5Sclose(new_obj_sid) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    if (new_obj_id >= 0)
        if (H5Dclose(new_obj_id) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

    return ret_value;
}

/*
 * Prints the value of a data point into a string.
 *
 * Returns:
 *        SUCCESS: Total number of characters printed
 *        FAILURE: 0
 */
size_t
h5str_sprintf(JNIEnv *env, h5str_t *out_str, hid_t container, hid_t tid, void *in_buf, int expand_data)
{
    unsigned char *ucptr = (unsigned char *)in_buf;
    char           fmt_llong[8], fmt_ullong[8];
    H5T_class_t    tclass   = H5T_NO_CLASS;
    size_t         typeSize = 0;
    H5T_sign_t     nsign    = H5T_SGN_ERROR;
    hid_t          mtid     = H5I_INVALID_HID;
    char          *cptr     = (char *)in_buf;
    char          *this_str = NULL;
    int            n;
    size_t         retVal = 0;

    if (!out_str)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "h5str_sprintf: out_str is NULL");
    if (!in_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "h5str_sprintf: in_buf is NULL");

    if (H5T_NO_CLASS == (tclass = H5Tget_class(tid)))
        H5_LIBRARY_ERROR(ENVONLY);
    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    /* Build default formats for long long types */
    if (!fmt_llong[0]) {
        if (snprintf(fmt_llong, sizeof(fmt_llong), "%%lld") < 0)
            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
        if (snprintf(fmt_ullong, sizeof(fmt_ullong), "%%llu") < 0)
            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
    } /* end if */
    switch (tclass) {
        case H5T_FLOAT: {
            switch (typeSize) {
                case sizeof(float): {
                    float tmp_float = 0.0f;

                    memcpy(&tmp_float, cptr, sizeof(float));

                    size_t this_len = 25;
                    if (NULL == (this_str = (char *)malloc(this_len)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

                    if (snprintf(this_str, this_len, "%g", (double)tmp_float) < 0)
                        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");

                    break;
                }

                case sizeof(double): {
                    double tmp_double = 0.0;

                    memcpy(&tmp_double, cptr, sizeof(double));

                    size_t this_len = 25;
                    if (NULL == (this_str = (char *)malloc(this_len)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

                    if (snprintf(this_str, this_len, "%g", tmp_double) < 0)
                        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");

                    break;
                }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
                case sizeof(long double): {
                    long double tmp_ldouble = 0.0L;

                    memcpy(&tmp_ldouble, cptr, sizeof(long double));

                    size_t this_len = 27;
                    if (NULL == (this_str = (char *)malloc(this_len)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

                    if (snprintf(this_str, this_len, "%Lg", tmp_ldouble) < 0)
                        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");

                    break;
                }
#endif

                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY,
                        "h5str_sprintf: floating-point datatype size didn't match any of expected sizes");
                    break;
            }

            break;
        }

        case H5T_STRING: {
            htri_t is_variable;
            char  *tmp_str;

            typeSize = 0;

            if ((is_variable = H5Tis_variable_str(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (is_variable) {
                /* cp_vp is the pointer into the struct where a `char*' is stored. So we have
                 * to dereference the pointer to get the `char*' to pass to strlen(). */
                tmp_str = *(char **)in_buf;
                if (NULL != tmp_str)
                    typeSize = strlen(tmp_str);
            }
            else {
                tmp_str = cptr;
            }

            /* Check for NULL pointer for string */
            if (!tmp_str) {
                if (NULL == (this_str = (char *)malloc(5)))
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");
                strncpy(this_str, "NULL", 5);
            }
            else {
                if (typeSize > 0) {
                    if (NULL == (this_str = strdup(tmp_str)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");
                }
            }

            break;
        }

        case H5T_INTEGER: {

            if (H5T_SGN_ERROR == (nsign = H5Tget_sign(tid)))
                H5_LIBRARY_ERROR(ENVONLY);

            switch (typeSize) {
                case sizeof(char): {
                    unsigned char tmp_uchar = 0;
                    char          tmp_char  = 0;

                    size_t this_len = 7;
                    if (H5T_SGN_NONE == nsign) {
                        memcpy(&tmp_uchar, cptr, sizeof(unsigned char));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%hhu", tmp_uchar) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }
                    else {
                        memcpy(&tmp_char, cptr, sizeof(char));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%hhd", tmp_char) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }

                    break;
                }

                case sizeof(short): {
                    unsigned short tmp_ushort = 0;
                    short          tmp_short  = 0;

                    size_t this_len = 9;
                    if (H5T_SGN_NONE == nsign) {
                        memcpy(&tmp_ushort, cptr, sizeof(unsigned short));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%hu", tmp_ushort) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }
                    else {
                        memcpy(&tmp_short, cptr, sizeof(short));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%hd", tmp_short) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }

                    break;
                }

                case sizeof(int): {
                    unsigned int tmp_uint = 0;
                    int          tmp_int  = 0;

                    size_t this_len = 14;
                    if (H5T_SGN_NONE == nsign) {
                        memcpy(&tmp_uint, cptr, sizeof(unsigned int));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%u", tmp_uint) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }
                    else {
                        memcpy(&tmp_int, cptr, sizeof(int));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%d", tmp_int) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }

                    break;
                }
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
                case sizeof(long): {
                    unsigned long tmp_ulong = 0;
                    long          tmp_long  = 0;

                    size_t this_len = 23;
                    if (H5T_SGN_NONE == nsign) {
                        memcpy(&tmp_ulong, cptr, sizeof(unsigned long));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%lu", tmp_ulong) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }
                    else {
                        memcpy(&tmp_long, cptr, sizeof(long));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, "%ld", tmp_long) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }

                    break;
                }
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
                case sizeof(long long): {
                    unsigned long long tmp_ullong = 0;
                    long long          tmp_llong  = 0;

                    size_t this_len = 25;
                    if (H5T_SGN_NONE == nsign) {
                        memcpy(&tmp_ullong, cptr, sizeof(unsigned long long));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, fmt_ullong, tmp_ullong) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }
                    else {
                        memcpy(&tmp_llong, cptr, sizeof(long long));

                        if (NULL == (this_str = (char *)malloc(this_len)))
                            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                                   "h5str_sprintf: failed to allocate string buffer");

                        if (snprintf(this_str, this_len, fmt_llong, tmp_llong) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                    }

                    break;
                }
#endif

                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY, "h5str_sprintf: integer datatype size didn't match any of expected sizes");
                    break;
            }

            break;
        }

        case H5T_COMPOUND: {
            unsigned i;
            size_t   offset;

            if ((n = H5Tget_nmembers(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (!h5str_append(out_str, H5_COMPOUND_BEGIN_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            for (i = 0; i < (unsigned)n; i++) {
                offset = H5Tget_member_offset(tid, i);

                if ((mtid = H5Tget_member_type(tid, i)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                if (!h5str_sprintf(ENVONLY, out_str, container, mtid, &cptr[offset], expand_data))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                if ((i + 1) < (unsigned)n)
                    if (!h5str_append(out_str, ", "))
                        H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

                if (H5Tclose(mtid) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                mtid = H5I_INVALID_HID;
            }

            if (!h5str_append(out_str, H5_COMPOUND_END_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            break;
        }

        case H5T_ENUM: {
            char enum_name[1024];

            if (H5Tenum_nameof(tid, cptr, enum_name, sizeof enum_name) >= 0) {
                if (!h5str_append(out_str, enum_name))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }
            else {
                size_t i;

                size_t this_len = 4 * (typeSize + 1);
                if (NULL == (this_str = (char *)malloc(this_len)))
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

                if (1 == typeSize) {
                    if (snprintf(this_str, this_len, "%#02x", ucptr[0]) < 0)
                        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                }
                else {
                    for (i = 0; i < typeSize; i++)
                        if (snprintf(this_str, this_len, "%s%02x", i ? ":" : "", ucptr[i]) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                }
            }

            break;
        }

        case H5T_REFERENCE: {
            if (H5R_DSET_REG_REF_BUF_SIZE == typeSize) {
                if (h5str_old_region_dataset(ENVONLY, out_str, container, cptr, expand_data) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            }
            else if (H5R_OBJ_REF_BUF_SIZE == typeSize) {
                H5O_info2_t oi;
                hid_t       obj         = H5I_INVALID_HID;
                char       *obj_tok_str = NULL;

                /*
                 * Object references -- show the type and OID of the referenced
                 * object.
                 */

                const size_t size = 64;
                if (NULL == (this_str = (char *)malloc(size)))
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

                if ((obj = H5Rdereference2(container, H5P_DEFAULT, H5R_OBJECT, cptr)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                if (H5Oget_info3(obj, &oi, H5O_INFO_ALL) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                /* Print object data and close object */
                H5Otoken_to_str(obj, &oi.token, &obj_tok_str);

                switch (oi.type) {
                    case H5O_TYPE_GROUP:
                        if (snprintf(this_str, size, "%s %s", H5_TOOLS_GROUP, obj_tok_str) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                        break;

                    case H5O_TYPE_DATASET:
                        if (snprintf(this_str, size, "%s %s", H5_TOOLS_DATASET, obj_tok_str) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                        break;

                    case H5O_TYPE_NAMED_DATATYPE:
                        if (snprintf(this_str, size, "%s %s", H5_TOOLS_DATATYPE, obj_tok_str) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                        break;

                    case H5O_TYPE_MAP:
                        if (snprintf(this_str, size, "%s %s", H5_TOOLS_MAP, obj_tok_str) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                        break;

                    case H5O_TYPE_UNKNOWN:
                    case H5O_TYPE_NTYPES:
                    default:
                        if (snprintf(this_str, size, "%u-%s", (unsigned)oi.type, obj_tok_str) < 0)
                            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
                        break;
                }

                if (obj_tok_str) {
                    H5free_memory(obj_tok_str);
                    obj_tok_str = NULL;
                }

                if (H5Oclose(obj) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                obj = H5I_INVALID_HID;
            }
            else {
                /* H5T_STD_REF */
                hid_t      new_obj_id = H5I_INVALID_HID;
                H5O_type_t obj_type   = -1; /* Object type */
                H5R_type_t ref_type;        /* Reference type */

                H5R_ref_t loc_ref;

                memcpy(&loc_ref, cptr, sizeof(H5R_ref_t));

                ref_type = H5Rget_type(&loc_ref);
                if (!h5str_is_zero(&loc_ref, H5Tget_size(H5T_STD_REF))) {
                    switch (ref_type) {
                        case H5R_OBJECT1:
                            if (H5Rget_obj_type3(&loc_ref, H5P_DEFAULT, &obj_type) >= 0) {
                                switch (obj_type) {
                                    case H5O_TYPE_DATASET:
                                        if (h5str_region_dataset(ENVONLY, out_str, &loc_ref, expand_data) < 0)
                                            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                                        break;

                                    case H5O_TYPE_GROUP:
                                    case H5O_TYPE_NAMED_DATATYPE:
                                    case H5O_TYPE_MAP:
                                    case H5O_TYPE_UNKNOWN:
                                    case H5O_TYPE_NTYPES:
                                    default: {
                                        /* Object references -- show the type and
                                         * OID of the referenced object. */
                                        H5O_info2_t oi;
                                        char       *obj_tok_str = NULL;
                                        if ((new_obj_id =
                                                 H5Ropen_object(&loc_ref, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                            H5Oget_info3(new_obj_id, &oi, H5O_INFO_BASIC);
                                            H5Otoken_to_str(new_obj_id, &oi.token, &obj_tok_str);
                                            if (H5Dclose(new_obj_id) < 0)
                                                H5_LIBRARY_ERROR(ENVONLY);
                                        }
                                        else
                                            H5_LIBRARY_ERROR(ENVONLY);

                                        size_t this_len = 14;
                                        if (NULL == (this_str = (char *)malloc(this_len)))
                                            H5_OUT_OF_MEMORY_ERROR(
                                                ENVONLY, "h5str_sprintf: failed to allocate string buffer");
                                        switch (oi.type) {
                                            case H5O_TYPE_GROUP:
                                                if (snprintf(this_str, this_len, "%s ", H5_TOOLS_GROUP) < 0)
                                                    H5_JNI_FATAL_ERROR(ENVONLY,
                                                                       "h5str_sprintf: snprintf failure");
                                                break;

                                            case H5O_TYPE_DATASET:
                                                if (snprintf(this_str, this_len, "%s ", H5_TOOLS_DATASET) < 0)
                                                    H5_JNI_FATAL_ERROR(ENVONLY,
                                                                       "h5str_sprintf: snprintf failure");
                                                break;

                                            case H5O_TYPE_NAMED_DATATYPE:
                                                if (snprintf(this_str, this_len, "%s ", H5_TOOLS_DATATYPE) <
                                                    0)
                                                    H5_JNI_FATAL_ERROR(ENVONLY,
                                                                       "h5str_sprintf: snprintf failure");
                                                break;

                                            case H5O_TYPE_MAP:
                                                if (snprintf(this_str, this_len, "%s ", H5_TOOLS_MAP) < 0)
                                                    H5_JNI_FATAL_ERROR(ENVONLY,
                                                                       "h5str_sprintf: snprintf failure");
                                                break;

                                            case H5O_TYPE_UNKNOWN:
                                            case H5O_TYPE_NTYPES:
                                            default:
                                                if (snprintf(this_str, this_len, "%u-", (unsigned)oi.type) <
                                                    0)
                                                    H5_JNI_FATAL_ERROR(ENVONLY,
                                                                       "h5str_sprintf: snprintf failure");
                                                break;
                                        } /* end switch */
                                        if (!h5str_append(out_str, this_str))
                                            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
                                        free(this_str);
                                        this_str = NULL;

                                        /* Print OID */
                                        {
                                            char *token_str;

                                            H5Otoken_to_str(tid, &oi.token, &token_str);

                                            size_t that_len = 64 + strlen(token_str) + 1;
                                            if (NULL == (this_str = malloc(that_len)))
                                                H5_OUT_OF_MEMORY_ERROR(
                                                    ENVONLY,
                                                    "h5str_sprintf: failed to allocate string buffer");
                                            if (snprintf(this_str, that_len, "%lu:%s", oi.fileno, token_str) <
                                                0)
                                                H5_JNI_FATAL_ERROR(ENVONLY,
                                                                   "h5str_sprintf: snprintf failure");

                                            H5free_memory(token_str);
                                        }
                                    } break;
                                } /* end switch */
                            }
                            else
                                H5_LIBRARY_ERROR(ENVONLY);
                            break;
                        case H5R_DATASET_REGION1:
                            if (h5str_region_dataset(ENVONLY, out_str, &loc_ref, expand_data) < 0)
                                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                            break;
                        case H5R_OBJECT2:
                            if (H5Rget_obj_type3(&loc_ref, H5P_DEFAULT, &obj_type) >= 0) {
                                switch (obj_type) {
                                    case H5O_TYPE_GROUP:
                                        break;

                                    case H5O_TYPE_DATASET:
                                        if (h5str_region_dataset(ENVONLY, out_str, &loc_ref, expand_data) < 0)
                                            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                                        break;

                                    case H5O_TYPE_NAMED_DATATYPE:
                                        break;

                                    case H5O_TYPE_MAP:
                                    case H5O_TYPE_UNKNOWN:
                                    case H5O_TYPE_NTYPES:
                                    default:
                                        break;
                                } /* end switch */
                            }
                            else
                                H5_ASSERTION_ERROR(ENVONLY, "h5str_sprintf: H5R_OBJECT2 failed");
                            break;
                        case H5R_DATASET_REGION2:
                            if (h5str_region_dataset(ENVONLY, out_str, &loc_ref, expand_data) < 0)
                                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                            break;
                        case H5R_ATTR:
                            if ((new_obj_id = H5Ropen_attr(&loc_ref, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if (h5str_dump_region_attribute(ENVONLY, out_str, new_obj_id) < 0)
                                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                                if (H5Aclose(new_obj_id) < 0)
                                    H5_LIBRARY_ERROR(ENVONLY);
                            }
                            break;
                        case H5R_BADTYPE:
                        case H5R_MAXTYPE:
                        default:
                            break;
                    } /* end switch */
                }

                if (H5Rdestroy(&loc_ref) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
            }

            break;
        }

        case H5T_ARRAY: {
            hsize_t dims[H5S_MAX_RANK], i, total_elmts;
            size_t  baseSize;
            int     rank = 0;

            if (!h5str_append(out_str, H5_ARRAY_BEGIN_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            if ((mtid = H5Tget_super(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (!(baseSize = H5Tget_size(mtid)))
                H5_LIBRARY_ERROR(ENVONLY);

            if ((rank = H5Tget_array_ndims(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (H5Tget_array_dims2(tid, dims) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            for (i = 0, total_elmts = 1; i < (hsize_t)rank; i++)
                total_elmts *= dims[i];

            for (i = 0; i < total_elmts; i++) {
                if (!h5str_sprintf(ENVONLY, out_str, container, mtid, &(cptr[i * baseSize]), expand_data))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                if ((i + 1) < total_elmts)
                    if (!h5str_append(out_str, ", "))
                        H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }

            if (!h5str_append(out_str, H5_ARRAY_END_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            if (H5Tclose(mtid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            mtid = H5I_INVALID_HID;

            break;
        }

        case H5T_VLEN: {
            unsigned int i;
            size_t       baseSize;
            hvl_t       *vl_buf = (hvl_t *)in_buf;

            if ((mtid = H5Tget_super(tid)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (!(baseSize = H5Tget_size(mtid)))
                H5_LIBRARY_ERROR(ENVONLY);

            if (!h5str_append(out_str, H5_VLEN_BEGIN_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            for (i = 0; i < (unsigned)vl_buf->len; i++) {
                if (!h5str_sprintf(ENVONLY, out_str, container, mtid, &(((char *)vl_buf->p)[i * baseSize]),
                                   expand_data))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                if ((i + 1) < (unsigned)vl_buf->len)
                    if (!h5str_append(out_str, ", "))
                        H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }

            if (!h5str_append(out_str, H5_VLEN_END_INDICATOR))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            if (H5Tclose(mtid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            mtid = H5I_INVALID_HID;

            break;
        }

        case H5T_NO_CLASS:
        case H5T_NCLASSES: {
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_sprintf: invalid datatype class");
            break;
        }

        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        default: {
            size_t i;

            /* All other types get printed as hexadecimal */

            size_t this_len = 4 * (typeSize + 1);
            if (NULL == (this_str = (char *)malloc(this_len)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_sprintf: failed to allocate string buffer");

            if (1 == typeSize) {
                if (snprintf(this_str, this_len, "%#02x", ucptr[0]) < 0)
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
            }
            else {
                for (i = 0; i < typeSize; i++)
                    if (snprintf(this_str, this_len, "%s%02x", i ? ":" : "", ucptr[i]) < 0)
                        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_sprintf: snprintf failure");
            }

            break;
        }
    }

    if (this_str) {
        if (!h5str_append(out_str, this_str))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

        free(this_str);
        this_str = NULL;
    } /* end if */

    retVal = strlen(out_str->s);

done:
    if (mtid >= 0)
        H5Tclose(mtid);

    return retVal;
} /* end h5str_sprintf */

/*-------------------------------------------------------------------------
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCCEED
 *-------------------------------------------------------------------------
 */
static int
h5str_print_region_data_blocks(JNIEnv *env, hid_t region_id, h5str_t *str, int ndims, hid_t type_id,
                               hssize_t nblocks, hsize_t *ptdata)
{
    unsigned indx;
    hsize_t *dims1 = NULL;
    hsize_t *start = NULL;
    hsize_t *count = NULL;
    hsize_t  blkndx;
    hsize_t  total_size[H5S_MAX_RANK];
    hsize_t  numelem;
    hsize_t  numindex;
    size_t   jndx;
    size_t   type_size;
    hid_t    mem_space  = H5I_INVALID_HID;
    hid_t    sid1       = H5I_INVALID_HID;
    void    *region_buf = NULL;
    int      ret_value  = FAIL;

    if (ndims < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_print_region_data_blocks: ndims < 0");
    if (nblocks < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_print_region_data_blocks: nblocks < 0");

    /* Get the dataspace of the dataset */
    if ((sid1 = H5Dget_space(region_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Allocate space for the dimension array */
    if (NULL == (dims1 = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "h5str_print_region_data_blocks: failed to allocate dimension array buffer");

    /* Find the dimensions of each data space from the block coordinates */
    for (jndx = 0, numelem = 1; jndx < (size_t)ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + (size_t)ndims] - ptdata[jndx] + 1;
        numelem     = dims1[jndx] * numelem;
    } /* end for */

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple(ndims, dims1, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(type_size = H5Tget_size(type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (region_buf = malloc(type_size * (size_t)numelem)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_print_region_data_blocks: failed to allocate region buffer");

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*         1   2        n      1   2        n                                        */
    if (NULL == (start = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "h5str_print_region_data_blocks: failed to allocate hyperslab start buffer");

    if (NULL == (count = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "h5str_print_region_data_blocks: failed to allocate hyperslab count buffer");

    for (blkndx = 0; blkndx < (hsize_t)nblocks; blkndx++) {
        for (indx = 0; indx < (unsigned)ndims; indx++) {
            start[indx] = ptdata[indx + blkndx * (hsize_t)ndims * 2];
            count[indx] = dims1[indx];
        } /* end for */

        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        for (numindex = 0; numindex < numelem; numindex++) {
            if (!h5str_sprintf(ENVONLY, str, region_id, type_id, ((char *)region_buf + numindex * type_size),
                               1))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            if (numindex + 1 < numelem)
                if (!h5str_append(str, ", "))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
        } /* end for (jndx = 0; jndx < numelem; jndx++, region_elmtno++, ctx.cur_elmt++) */
    }     /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

    ret_value = SUCCEED;

done:
    if (count)
        free(count);
    if (start)
        free(start);
    if (region_buf)
        free(region_buf);
    if (mem_space >= 0)
        H5Sclose(mem_space);
    if (dims1)
        free(dims1);
    if (sid1 >= 0)
        H5Sclose(sid1);

    return ret_value;
} /* end h5str_print_region_data_blocks */

int
h5str_dump_region_blocks(JNIEnv *env, h5str_t *str, hid_t region_space, hid_t region_id, int expand_data)
{
    hssize_t nblocks;
    hsize_t  alloc_size;
    hsize_t *ptdata    = NULL;
    hid_t    dtype     = H5I_INVALID_HID;
    hid_t    type_id   = H5I_INVALID_HID;
    int      ndims     = -1;
    int      ret_value = FAIL;
    int      i;
    char     tmp_str[256];

    /*
     * This function fails if the region does not have blocks.
     */
    H5E_BEGIN_TRY
    {
        nblocks = H5Sget_select_hyper_nblocks(region_space);
    }
    H5E_END_TRY

    if (nblocks <= 0) {
        ret_value = SUCCEED;
        goto done;
    }
    if ((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Print block information */
    alloc_size = (hsize_t)nblocks * (hsize_t)ndims * 2 * (hsize_t)sizeof(ptdata[0]);
    if (NULL == (ptdata = (hsize_t *)malloc((size_t)alloc_size)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_region_blocks: failed to allocate region block buffer");

    if (H5Sget_select_hyper_blocklist(region_space, (hsize_t)0, (hsize_t)nblocks, ptdata) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (expand_data) {
        if ((dtype = H5Dget_type(region_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (h5str_print_region_data_blocks(ENVONLY, region_id, str, ndims, type_id, nblocks, ptdata) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else {
        if (!h5str_append(str, " REGION_TYPE BLOCK"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

        if (!h5str_append(str, " {"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

        for (i = 0; i < nblocks; i++) {
            int j;

            if (!h5str_append(str, " "))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            /* Start coordinates and opposite corner */
            for (j = 0; j < ndims; j++) {
                tmp_str[0] = '\0';

                if (snprintf(tmp_str, sizeof(tmp_str), "%s%lu", j ? "," : "(",
                             (unsigned long)ptdata[i * 2 * ndims + j]) < 0)
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_region_blocks: snprintf failure");

                if (!h5str_append(str, tmp_str))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }

            for (j = 0; j < ndims; j++) {
                tmp_str[0] = '\0';

                if (snprintf(tmp_str, sizeof(tmp_str), "%s%lu", j ? "," : ")-(",
                             (unsigned long)ptdata[i * 2 * ndims + j + ndims]) < 0)
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_region_blocks: snprintf failure");

                if (!h5str_append(str, tmp_str))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            }

            if (!h5str_append(str, ") "))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            tmp_str[0] = '\0';
        }

        if (!h5str_append(str, " }"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
    }
    ret_value = SUCCEED;

done:
    if (type_id >= 0)
        H5Tclose(type_id);
    if (dtype >= 0)
        H5Tclose(dtype);
    if (ptdata)
        free(ptdata);

    return ret_value;
} /* end h5str_dump_region_blocks */

/*-------------------------------------------------------------------------
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *-------------------------------------------------------------------------
 */
static int
h5str_print_region_data_points(JNIEnv *env, hid_t region_space, hid_t region_id, h5str_t *str, int ndims,
                               hid_t type_id, hssize_t npoints, hsize_t *ptdata)
{
    hsize_t *dims1 = NULL;
    hsize_t  total_size[H5S_MAX_RANK];
    size_t   jndx;
    size_t   type_size;
    hid_t    mem_space  = H5I_INVALID_HID;
    void    *region_buf = NULL;
    int      ret_value  = FAIL;

    UNUSED(ptdata);

    if (npoints < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_print_region_data_points: npoints < 0");

    /* Allocate space for the dimension array */
    if (NULL == (dims1 = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "h5str_print_region_data_points: failed to allocate dimension array buffer");

    dims1[0] = (hsize_t)npoints;

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple(1, dims1, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(type_size = H5Tget_size(type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (region_buf = malloc(type_size * (size_t)npoints)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_print_region_data_points: failed to allocate region buffer");

    if (H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (jndx = 0; jndx < (size_t)npoints; jndx++) {
        if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (!h5str_sprintf(ENVONLY, str, region_id, type_id, ((char *)region_buf + jndx * type_size), 1))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (jndx + 1 < (size_t)npoints)
            if (!h5str_append(str, ", "))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
    } /* end for (jndx = 0; jndx < npoints; jndx++, elmtno++) */

    ret_value = SUCCEED;

done:
    if (region_buf)
        free(region_buf);
    if (mem_space >= 0)
        H5Sclose(mem_space);
    if (dims1)
        free(dims1);

    return ret_value;
} /* end h5str_print_region_data_points */

int
h5str_dump_region_points(JNIEnv *env, h5str_t *str, hid_t region_space, hid_t region_id, int expand_data)
{
    hsize_t  alloc_size;
    hssize_t npoints   = -1;
    hsize_t *ptdata    = NULL;
    hid_t    dtype     = H5I_INVALID_HID;
    hid_t    type_id   = H5I_INVALID_HID;
    int      ndims     = -1;
    int      ret_value = FAIL;
    int      i;
    char     tmp_str[256];

    /*
     * This function fails if the region does not have points.
     */
    H5E_BEGIN_TRY
    {
        npoints = H5Sget_select_elem_npoints(region_space);
    }
    H5E_END_TRY

    if (npoints <= 0) {
        ret_value = SUCCEED;
        goto done;
    }
    if ((ndims = H5Sget_simple_extent_ndims(region_space)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Print point information */
    alloc_size = (hsize_t)npoints * (hsize_t)ndims * (hsize_t)sizeof(ptdata[0]);
    if (NULL == (ptdata = (hsize_t *)malloc((size_t)alloc_size)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "h5str_dump_region_points: failed to allocate region point data buffer");

    if (H5Sget_select_elem_pointlist(region_space, (hsize_t)0, (hsize_t)npoints, ptdata) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (expand_data) {
        if ((dtype = H5Dget_type(region_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (h5str_print_region_data_points(ENVONLY, region_space, region_id, str, ndims, type_id, npoints,
                                           ptdata) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else {
        if (!h5str_append(str, " REGION_TYPE POINT"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

        if (!h5str_append(str, " {"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

        for (i = 0; i < npoints; i++) {
            int j;

            if (!h5str_append(str, " "))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");

            for (j = 0; j < ndims; j++) {
                tmp_str[0] = '\0';

                if (snprintf(tmp_str, sizeof(tmp_str), "%s%lu", j ? "," : "(",
                             (unsigned long)(ptdata[i * ndims + j])) < 0)
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_region_points: snprintf failure");

                if (!h5str_append(str, tmp_str))
                    H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
            } /* end for (j = 0; j < ndims; j++) */

            if (!h5str_append(str, ") "))
                H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
        } /* end for (i = 0; i < npoints; i++) */

        if (!h5str_append(str, " }"))
            H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
    }
    ret_value = SUCCEED;

done:
    if (type_id >= 0)
        H5Tclose(type_id);
    if (dtype >= 0)
        H5Tclose(dtype);
    if (ptdata)
        free(ptdata);

    return ret_value;
} /* end h5str_dump_region_points */

static int
h5str_is_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *)_mem;

    while (size-- > 0)
        if (mem[size])
            return 0;

    return 1;
} /* end h5str_is_zero */

/*-------------------------------------------------------------------------
 * Function: h5str_detect_vlen
 *
 * Purpose: Recursive check for any variable length data in given type.
 *
 * Return:
 *    1 : type contains any variable length data
 *    0 : type doesn't contain any variable length data
 *    Negative value: error occurred
 *-------------------------------------------------------------------------
 */
htri_t
h5str_detect_vlen(hid_t tid)
{
    htri_t ret = FAIL;

    /* Recursively detect any vlen data values in type (compound, array ...) */
    ret = H5Tdetect_class(tid, H5T_VLEN);
    if ((ret == 1) || (ret < 0))
        goto done;

    /* Recursively detect any vlen string in type (compound, array ...) */
    ret = h5str_detect_vlen_str(tid);
    if ((ret == 1) || (ret < 0))
        goto done;

done:
    return ret;
} /* end h5str_detect_vlen */

/*-------------------------------------------------------------------------
 * Function: h5str_detect_vlen_str
 *
 * Purpose: Recursive check for variable length string of a datatype.
 *
 * Return:
 *    true : type contains any variable length string
 *    false : type doesn't contain any variable length string
 *    Negative value: error occur
 *
 *-------------------------------------------------------------------------
 */
static htri_t
h5str_detect_vlen_str(hid_t tid)
{
    H5T_class_t tclass = H5T_NO_CLASS;
    htri_t      ret    = 0;

    ret = H5Tis_variable_str(tid);
    if ((ret == 1) || (ret < 0))
        goto done;

    tclass = H5Tget_class(tid);
    if (tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t btid = H5Tget_super(tid);

        if (btid < 0) {
            ret = (htri_t)btid;
            goto done;
        } /* end if */
        ret = h5str_detect_vlen_str(btid);
        if ((ret == 1) || (ret < 0)) {
            H5Tclose(btid);
            goto done;
        } /* end if */
    }     /* end if */
    else if (tclass == H5T_COMPOUND) {
        unsigned i = 0;
        int      n = H5Tget_nmembers(tid);

        if (n < 0)
            goto done;

        for (i = 0; i < (unsigned)n; i++) {
            hid_t mtid = H5Tget_member_type(tid, i);

            ret = h5str_detect_vlen_str(mtid);
            if ((ret == 1) || (ret < 0)) {
                H5Tclose(mtid);
                goto done;
            }
            H5Tclose(mtid);
        } /* end for */
    }     /* end else */

done:
    return ret;
} /* end h5str_detect_vlen_str */

/*-------------------------------------------------------------------------
 * Function: h5str_get_native_type
 *
 * Purpose: Wrapper around H5Tget_native_type() to work around
 *          Problems with bitfields.
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static hid_t
h5str_get_native_type(hid_t type)
{
    H5T_class_t type_class;
    hid_t       p_type = H5I_INVALID_HID;

    if ((type_class = H5Tget_class(type)) < 0)
        goto done;

    if (type_class == H5T_BITFIELD)
        p_type = H5Tcopy(type);
    else
        p_type = H5Tget_native_type(type, H5T_DIR_DEFAULT);

done:
    return (p_type);
} /* end h5str_get_native_type */

/*-------------------------------------------------------------------------
 * Function: h5str_get_little_endian_type
 *
 * Purpose: Get a little endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static hid_t
h5str_get_little_endian_type(hid_t tid)
{
    H5T_class_t type_class;
    H5T_sign_t  sign;
    size_t      size;
    hid_t       p_type = H5I_INVALID_HID;

    if ((type_class = H5Tget_class(tid)) < 0)
        goto done;

    if (!(size = H5Tget_size(tid)))
        goto done;

    if ((sign = H5Tget_sign(tid)) < 0)
        goto done;

    switch (type_class) {
        case H5T_INTEGER: {
            if (size == 1 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I8LE);
            else if (size == 2 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I16LE);
            else if (size == 4 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I32LE);
            else if (size == 8 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I64LE);
            else if (size == 1 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U8LE);
            else if (size == 2 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U16LE);
            else if (size == 4 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U32LE);
            else if (size == 8 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U64LE);

            break;
        }

        case H5T_FLOAT: {
            if (size == 2)
                p_type = H5Tcopy(H5T_IEEE_F16LE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_IEEE_F32LE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_IEEE_F64LE);

            break;
        }

        case H5T_BITFIELD: {
            if (size == 1)
                p_type = H5Tcopy(H5T_STD_B8LE);
            else if (size == 2)
                p_type = H5Tcopy(H5T_STD_B16LE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_STD_B32LE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_STD_B64LE);

            break;
        }

        case H5T_NO_CLASS:
        case H5T_NCLASSES: {
            goto done;
            break;
        }

        case H5T_TIME:
        case H5T_OPAQUE:
        case H5T_STRING:
        case H5T_COMPOUND:
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
            break;

        default:
            break;
    }

done:
    return (p_type);
} /* end h5str_get_little_endian_type */

/*-------------------------------------------------------------------------
 * Function: h5str_get_big_endian_type
 *
 * Purpose: Get a big endian type from a file type
 *
 * Return: Success:    datatype ID
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static hid_t
h5str_get_big_endian_type(hid_t tid)
{
    H5T_class_t type_class;
    H5T_sign_t  sign;
    size_t      size;
    hid_t       p_type = H5I_INVALID_HID;

    if ((type_class = H5Tget_class(tid)) < 0)
        goto done;

    if (!(size = H5Tget_size(tid)))
        goto done;

    if ((sign = H5Tget_sign(tid)) < 0)
        goto done;

    switch (type_class) {
        case H5T_INTEGER: {
            if (size == 1 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I8BE);
            else if (size == 2 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I16BE);
            else if (size == 4 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I32BE);
            else if (size == 8 && sign == H5T_SGN_2)
                p_type = H5Tcopy(H5T_STD_I64BE);
            else if (size == 1 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U8BE);
            else if (size == 2 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U16BE);
            else if (size == 4 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U32BE);
            else if (size == 8 && sign == H5T_SGN_NONE)
                p_type = H5Tcopy(H5T_STD_U64BE);

            break;
        }

        case H5T_FLOAT: {
            if (size == 2)
                p_type = H5Tcopy(H5T_IEEE_F16BE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_IEEE_F32BE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_IEEE_F64BE);

            break;
        }

        case H5T_BITFIELD: {
            if (size == 1)
                p_type = H5Tcopy(H5T_STD_B8BE);
            else if (size == 2)
                p_type = H5Tcopy(H5T_STD_B16BE);
            else if (size == 4)
                p_type = H5Tcopy(H5T_STD_B32BE);
            else if (size == 8)
                p_type = H5Tcopy(H5T_STD_B64BE);

            break;
        }

        case H5T_NO_CLASS:
        case H5T_NCLASSES: {
            goto done;
            break;
        }

        case H5T_TIME:
        case H5T_OPAQUE:
        case H5T_STRING:
        case H5T_COMPOUND:
        case H5T_REFERENCE:
        case H5T_ENUM:
        case H5T_VLEN:
        case H5T_ARRAY:
            break;

        default:
            break;
    }

done:
    return (p_type);
} /* end h5str_get_big_endian_type */

/*-------------------------------------------------------------------------
 * Function: render_bin_output
 *
 * Purpose: Write one element of memory buffer to a binary file stream
 *
 * Return: Success:    SUCCEED
 *         Failure:    FAIL
 *-------------------------------------------------------------------------
 */
static int
h5str_render_bin_output(FILE *stream, hid_t container, hid_t tid, void *_mem, hsize_t block_nelmts)
{
    unsigned char *mem = (unsigned char *)_mem;
    H5T_class_t    type_class;
    hsize_t        block_index;
    size_t         size; /* datum size */
    int            ret_value = 0;

    if (!(size = H5Tget_size(tid))) {
        ret_value = FAIL;
        goto done;
    }

    if ((type_class = H5Tget_class(tid)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    switch (type_class) {
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_ENUM: {
            block_index = block_nelmts * size;
            while (block_index > 0) {
                size_t bytes_in    = 0; /* # of bytes to write  */
                size_t bytes_wrote = 0; /* # of bytes written   */

                if (block_index > sizeof(size_t))
                    bytes_in = sizeof(size_t);
                else
                    bytes_in = (size_t)block_index;

                bytes_wrote = fwrite(mem, 1, bytes_in, stream);

                if (bytes_wrote != bytes_in || (0 == bytes_wrote && ferror(stream))) {
                    ret_value = FAIL;
                    break;
                }

                block_index -= (hsize_t)bytes_wrote;
                mem = mem + bytes_wrote;
            }

            break;
        }

        case H5T_STRING: {
            unsigned int  i;
            H5T_str_t     pad;
            char         *s = NULL;
            unsigned char tempuchar;

            if ((pad = H5Tget_strpad(tid)) < 0) {
                ret_value = FAIL;
                goto done;
            }

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;

                if (H5Tis_variable_str(tid)) {
                    s = *(char **)((void *)mem);
                    if (s != NULL)
                        size = strlen(s);
                }
                else {
                    s = (char *)mem;
                }
                for (i = 0; i < size && (s[i] || pad != H5T_STR_NULLTERM); i++) {
                    memcpy(&tempuchar, &s[i], sizeof(unsigned char));
                    if (1 != fwrite(&tempuchar, sizeof(unsigned char), 1, stream)) {
                        ret_value = FAIL;
                        break;
                    }
                } /* i */
                if (ret_value < 0)
                    break;
            } /* for (block_index = 0; block_index < block_nelmts; block_index++) */

            break;
        }

        case H5T_COMPOUND: {
            unsigned j;
            size_t   offset;
            hid_t    memb = H5I_INVALID_HID;
            int      nmembs;

            if ((nmembs = H5Tget_nmembers(tid)) < 0) {
                ret_value = FAIL;
                goto done;
            }

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;
                for (j = 0; j < (unsigned)nmembs; j++) {
                    offset = H5Tget_member_offset(tid, j);
                    memb   = H5Tget_member_type(tid, j);

                    if (h5str_render_bin_output(stream, container, memb, mem + offset, 1) < 0) {
                        H5Tclose(memb);
                        ret_value = FAIL;
                        break;
                    }

                    H5Tclose(memb);
                }

                if (ret_value < 0)
                    break;
            }

            break;
        }

        case H5T_ARRAY: {
            hsize_t dims[H5S_MAX_RANK], temp_nelmts, nelmts;
            hid_t   memb;
            int     k, ndims;

            /* Get the array's base datatype for each element */
            if ((memb = H5Tget_super(tid)) < 0) {
                ret_value = FAIL;
                goto done;
            }

            if ((ndims = H5Tget_array_ndims(tid)) < 0) {
                ret_value = FAIL;
                goto done;
            }

            if (H5Tget_array_dims2(tid, dims) < 0) {
                ret_value = FAIL;
                goto done;
            }

            /* Calculate the number of array elements */
            for (k = 0, nelmts = 1; k < ndims; k++) {
                temp_nelmts = nelmts;
                temp_nelmts *= dims[k];
                nelmts = (size_t)temp_nelmts;
            }

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;

                /* dump the array element */
                if (h5str_render_bin_output(stream, container, memb, mem, nelmts) < 0) {
                    ret_value = FAIL;
                    break;
                }
            }

            H5Tclose(memb);

            break;
        }

        case H5T_VLEN: {
            hsize_t nelmts;
            hid_t   memb;

            /* Get the VL sequences's base datatype for each element */
            if ((memb = H5Tget_super(tid)) < 0) {
                ret_value = FAIL;
                goto done;
            }

            for (block_index = 0; block_index < block_nelmts; block_index++) {
                hvl_t vl_elem;

                memcpy(&vl_elem, ((unsigned char *)_mem) + block_index * size, sizeof(hvl_t));

                /* Get the number of sequence elements */
                nelmts = vl_elem.len;

                /* dump the array element */
                if (h5str_render_bin_output(stream, container, memb, ((char *)(vl_elem.p)), nelmts) < 0) {
                    ret_value = FAIL;
                    break;
                }
            }

            H5Tclose(memb);

            break;
        }

        case H5T_REFERENCE: {
            hid_t        region_id    = H5I_INVALID_HID;
            hid_t        region_space = H5I_INVALID_HID;
            H5S_sel_type region_type;

            /* Region data */
            for (block_index = 0; block_index < block_nelmts; block_index++) {
                H5R_ref_t loc_ref;

                memcpy(&loc_ref, ((unsigned char *)_mem) + block_index * size, sizeof(H5R_ref_t));

                if ((region_id = H5Ropen_object(&loc_ref, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                    continue;
                if ((region_space = H5Ropen_region(&loc_ref, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                    if (!h5str_is_zero(&loc_ref, H5Tget_size(H5T_STD_REF))) {
                        region_type = H5Sget_select_type(region_space);
                        if (region_type == H5S_SEL_POINTS)
                            ret_value =
                                render_bin_output_region_points(stream, region_space, region_id, container);
                        else if (region_type == H5S_SEL_HYPERSLABS)
                            ret_value =
                                render_bin_output_region_blocks(stream, region_space, region_id, container);
                    }
                    H5Sclose(region_space);
                } /* end if (region_space >= 0) */
                H5Dclose(region_id);

                if (ret_value < 0)
                    break;
            }

            break;
        }

        case H5T_NO_CLASS:
        case H5T_NCLASSES: {
            ret_value = FAIL;
            goto done;
            break;
        }

        case H5T_TIME:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        default:
            for (block_index = 0; block_index < block_nelmts; block_index++) {
                mem = ((unsigned char *)_mem) + block_index * size;
                if (size != fwrite(mem, sizeof(char), size, stream)) {
                    ret_value = FAIL;
                    break;
                }
            }
            break;
    }

done:
    return ret_value;
} /* end h5str_render_bin_output */

/*-------------------------------------------------------------------------
 * Purpose: Print the data values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type blocks.
 *
 * Return:
 *      The function returns FAIL if there was an error, otherwise SUCCEED
 *
 *-------------------------------------------------------------------------
 */
static int
render_bin_output_region_data_blocks(FILE *stream, hid_t region_id, hid_t container, int ndims, hid_t type_id,
                                     hssize_t nblocks, hsize_t *ptdata)
{
    hsize_t *dims1 = NULL;
    hsize_t *start = NULL;
    hsize_t *count = NULL;
    hsize_t  numelem;
    hsize_t  total_size[H5S_MAX_RANK];
    size_t   type_size;
    hid_t    sid1       = H5I_INVALID_HID;
    hid_t    mem_space  = H5I_INVALID_HID;
    void    *region_buf = NULL;
    int      blkndx;
    int      jndx;
    int      ret_value = SUCCEED;

    /* Get the dataspace of the dataset */
    if ((sid1 = H5Dget_space(region_id)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    /* Allocate space for the dimension array */
    if (NULL == (dims1 = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims))) {
        ret_value = FAIL;
        goto done;
    }

    /* Find the dimensions of each data space from the block coordinates */
    for (jndx = 0, numelem = 1; jndx < ndims; jndx++) {
        dims1[jndx] = ptdata[jndx + ndims] - ptdata[jndx] + 1;
        numelem     = dims1[jndx] * numelem;
    }

    /* Create dataspace for reading buffer */
    if ((mem_space = H5Screate_simple(ndims, dims1, NULL)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (!(type_size = H5Tget_size(type_id))) {
        ret_value = FAIL;
        goto done;
    }

    if (NULL == (region_buf = malloc(type_size * (size_t)numelem))) {
        ret_value = FAIL;
        goto done;
    }

    /* Select (x , x , ..., x ) x (y , y , ..., y ) hyperslab for reading memory dataset */
    /*         1   2        n      1   2        n                                        */
    if (NULL == (start = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims))) {
        ret_value = FAIL;
        goto done;
    }

    if (NULL == (count = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims))) {
        ret_value = FAIL;
        goto done;
    }

    for (blkndx = 0; blkndx < nblocks; blkndx++) {
        for (jndx = 0; jndx < ndims; jndx++) {
            start[jndx] = ptdata[jndx + blkndx * ndims * 2];
            count[jndx] = dims1[jndx];
        }

        if (H5Sselect_hyperslab(sid1, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (H5Dread(region_id, type_id, mem_space, sid1, H5P_DEFAULT, region_buf) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (H5Sget_simple_extent_dims(mem_space, total_size, NULL) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (h5str_render_bin_output(stream, container, type_id, (char *)region_buf, numelem) < 0) {
            ret_value = FAIL;
            goto done;
        }

        /* Render the region data element end */
    } /* end for (blkndx = 0; blkndx < nblocks; blkndx++) */

done:
    if (count)
        free(count);
    if (start)
        free(start);
    if (region_buf)
        free(region_buf);
    if (mem_space >= 0)
        H5Sclose(mem_space);
    if (dims1)
        free(dims1);
    if (sid1 >= 0)
        H5Sclose(sid1);

    return ret_value;
} /* end render_bin_output_region_data_blocks */

/*-------------------------------------------------------------------------
 * Purpose: Print some values from a dataset referenced by region blocks.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using blocks.
 *
 * Return:
 *      The function returns False if ERROR, otherwise True
 *
 *-------------------------------------------------------------------------
 */
static int
render_bin_output_region_blocks(FILE *stream, hid_t region_space, hid_t region_id, hid_t container)
{
    hssize_t nblocks;
    hsize_t  alloc_size;
    hsize_t *ptdata  = NULL;
    hid_t    dtype   = H5I_INVALID_HID;
    hid_t    type_id = H5I_INVALID_HID;
    int      ndims;
    int      ret_value = SUCCEED;

    if ((nblocks = H5Sget_select_hyper_nblocks(region_space)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (nblocks > 0) {
        /* Print block information */
        if ((ndims = H5Sget_simple_extent_ndims(region_space)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        alloc_size = (hsize_t)nblocks * (hsize_t)ndims * 2 * (hsize_t)sizeof(ptdata[0]);

        if (NULL == (ptdata = (hsize_t *)malloc((size_t)alloc_size))) {
            ret_value = FAIL;
            goto done;
        }

        if (H5Sget_select_hyper_blocklist(region_space, (hsize_t)0, (hsize_t)nblocks, ptdata) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if ((dtype = H5Dget_type(region_id)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (render_bin_output_region_data_blocks(stream, region_id, container, ndims, type_id, nblocks,
                                                 ptdata) < 0) {
            ret_value = FAIL;
            goto done;
        }
    }

done:
    if (type_id >= 0)
        H5Tclose(type_id);
    if (dtype >= 0)
        H5Tclose(dtype);
    if (ptdata)
        free(ptdata);

    return ret_value;
} /* end render_bin_output_region_blocks */

/*-------------------------------------------------------------------------
 * Purpose: Print the data values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to print the data in a region reference of type points.
 *
 * Return:
 *      The function returns FAIL on error, otherwise SUCCEED
 *
 *-------------------------------------------------------------------------
 */
static int
render_bin_output_region_data_points(FILE *stream, hid_t region_space, hid_t region_id, hid_t container,
                                     int ndims, hid_t type_id, hssize_t npoints, hsize_t *ptdata)
{
    hsize_t *dims1 = NULL;
    size_t   type_size;
    hid_t    mem_space  = H5I_INVALID_HID;
    void    *region_buf = NULL;
    int      ret_value  = SUCCEED;

    UNUSED(ptdata);

    if (!(type_size = H5Tget_size(type_id))) {
        ret_value = FAIL;
        goto done;
    }

    if (NULL == (region_buf = malloc(type_size * (size_t)npoints))) {
        ret_value = FAIL;
        goto done;
    }

    /* Allocate space for the dimension array */
    if (NULL == (dims1 = (hsize_t *)malloc(sizeof(hsize_t) * (size_t)ndims))) {
        ret_value = FAIL;
        goto done;
    }

    dims1[0] = (hsize_t)npoints;

    if ((mem_space = H5Screate_simple(1, dims1, NULL)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (H5Dread(region_id, type_id, mem_space, region_space, H5P_DEFAULT, region_buf) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (H5Sget_simple_extent_dims(region_space, dims1, NULL) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (h5str_render_bin_output(stream, container, type_id, (char *)region_buf, (hsize_t)npoints) < 0) {
        ret_value = FAIL;
        goto done;
    }

done:
    if (dims1)
        free(dims1);
    if (mem_space >= 0)
        H5Sclose(mem_space);
    if (region_buf)
        free(region_buf);

    return ret_value;
} /* end render_bin_output_region_data_points */

/*-------------------------------------------------------------------------
 * Purpose: Print some values from a dataset referenced by region points.
 *
 * Description:
 *      This is a special case subfunction to dump a region reference using points.
 *
 * Return:
 *      The function returns False if the last dimension has been reached, otherwise True
 *
 *-------------------------------------------------------------------------
 */
static int
render_bin_output_region_points(FILE *stream, hid_t region_space, hid_t region_id, hid_t container)
{
    hssize_t npoints;
    hsize_t  alloc_size;
    hsize_t *ptdata  = NULL;
    hid_t    dtype   = H5I_INVALID_HID;
    hid_t    type_id = H5I_INVALID_HID;
    int      ndims;
    int      ret_value = SUCCEED;

    if ((npoints = H5Sget_select_elem_npoints(region_space)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if (npoints > 0) {
        /* Allocate space for the dimension array */
        if ((ndims = H5Sget_simple_extent_ndims(region_space)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        alloc_size = (hsize_t)npoints * (hsize_t)ndims * (hsize_t)sizeof(ptdata[0]);

        if (NULL == (ptdata = (hsize_t *)malloc((size_t)alloc_size))) {
            ret_value = FAIL;
            goto done;
        }

        if (H5Sget_select_elem_pointlist(region_space, (hsize_t)0, (hsize_t)npoints, ptdata) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if ((dtype = H5Dget_type(region_id)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if ((type_id = H5Tget_native_type(dtype, H5T_DIR_DEFAULT)) < 0) {
            ret_value = FAIL;
            goto done;
        }

        if (render_bin_output_region_data_points(stream, region_space, region_id, container, ndims, type_id,
                                                 npoints, ptdata) < 0) {
            ret_value = FAIL;
            goto done;
        }
    }

done:
    if (type_id >= 0)
        H5Tclose(type_id);
    if (dtype >= 0)
        H5Tclose(dtype);
    if (ptdata)
        free(ptdata);

    return ret_value;
} /* end render_bin_output_region_points */

/*-------------------------------------------------------------------------
 * Purpose: Print some values from an attribute referenced by object reference.
 *
 * Parameters Description:
 *      FILE *buffer is the string into which to render
 *-------------------------------------------------------------------------
 */
static int
h5str_dump_region_attribute(JNIEnv *env, h5str_t *str, hid_t region_id)
{
    int     ret_value    = SUCCEED;
    hid_t   atype        = H5I_INVALID_HID;
    hid_t   type_id      = H5I_INVALID_HID;
    hid_t   region_space = H5I_INVALID_HID;
    hsize_t total_size[H5S_MAX_RANK]; /* total size of dataset*/
    size_t  i;                        /* counter  */
    size_t  size;                     /* datum size */
    int     sndims;                   /* rank of dataspace */
    hsize_t p_nelmts;                 /* total selected elmts */
    hsize_t alloc_size;

    unsigned char *buf = NULL; /* buffer for raw data */

    /* VL data special information */
    unsigned int vl_data = 0; /* contains VL datatypes */

    if ((region_space = H5Aget_space(region_id)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    if ((sndims = H5Sget_simple_extent_ndims(region_space)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    /* Assume entire data space to be read */
    H5Sget_simple_extent_dims(region_space, total_size, NULL);
    p_nelmts = 1;

    for (i = 0; i < (size_t)sndims; i++)
        p_nelmts *= total_size[i];

    if ((atype = H5Aget_type(region_id)) < 0) {
        ret_value = FAIL;
        goto done;
    }
    if ((type_id = H5Tget_native_type(atype, H5T_DIR_DEFAULT)) < 0) {
        ret_value = FAIL;
        goto done;
    }

    /* Check if we have VL data in the dataset's datatype */
    if (h5str_detect_vlen(type_id) == true)
        vl_data = true;

    if (!(size = H5Tget_size(type_id))) {
        ret_value = FAIL;
        goto done;
    }

    alloc_size = p_nelmts * size;
    assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/
    if (NULL != (buf = (unsigned char *)malloc((size_t)alloc_size))) {
        /* Read the data */
        if (H5Aread(region_id, type_id, buf) >= 0) {

            for (i = 0; i < p_nelmts; i++) {
                size_t bytes_in = 0; /* # of bytes to write  */
                void  *memref   = buf + i * size;

                if (!(bytes_in = h5str_sprintf(ENVONLY, str, region_id, type_id, memref, 1)))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                if ((i < p_nelmts - 1) && (bytes_in > 0)) {
                    if (!h5str_append(str, ", "))
                        H5_ASSERTION_ERROR(ENVONLY, "Unable to append string.");
                }
            }

            /* Reclaim any VL memory, if necessary */
            if (vl_data) {
                if (H5Treclaim(type_id, region_space, H5P_DEFAULT, buf) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
            }
        }
        else
            H5_LIBRARY_ERROR(ENVONLY);
    }

done:
    if (buf)
        free(buf);
    if (region_space >= 0)
        H5Sclose(region_space);
    if (type_id >= 0)
        H5Tclose(type_id);
    if (atype >= 0)
        H5Tclose(type_id);

    return ret_value;
}

int
h5str_dump_simple_dset(JNIEnv *env, FILE *stream, hid_t dset, int binary_order)
{
    hid_t   f_space = H5I_INVALID_HID; /* file data space */
    hsize_t elmtno;                    /* counter  */
    size_t  i = 0;                     /* counter  */
    int     sndims;                    /* rank of dataspace */
    int     carry;                     /* counter carry value */
    hsize_t zero[8];                   /* vector of zeros */
    hsize_t total_size[H5S_MAX_RANK];  /* total size of dataset*/

    /* Stripmine info */
    hsize_t        sm_size[H5S_MAX_RANK];      /* stripmine size */
    hsize_t        sm_nbytes;                  /* bytes per stripmine */
    hsize_t        sm_nelmts;                  /* elements per stripmine */
    unsigned char *sm_buf   = NULL;            /* buffer for raw data */
    hid_t          sm_space = H5I_INVALID_HID; /* stripmine data space */

    /* Hyperslab info */
    hsize_t hs_offset[H5S_MAX_RANK]; /* starting offset */
    hsize_t hs_size[H5S_MAX_RANK];   /* size this pass */
    hsize_t hs_nelmts;               /* elements in request */

    /* VL data special information */
    unsigned int vl_data   = 0; /* contains VL datatypes */
    int          ret_value = FAIL;

    hid_t p_type = H5I_INVALID_HID;
    hid_t f_type = H5I_INVALID_HID;

    if (dset < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_dump_simple_dset: dset ID < 0");

    if ((f_type = H5Dget_type(dset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((f_space = H5Dget_space(dset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((sndims = H5Sget_simple_extent_ndims(f_space)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Assume entire data space to be printed */
    if (H5Sget_simple_extent_dims(f_space, total_size, NULL) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5Tequal(f_type, H5T_STD_REF_DSETREG)) {
        hssize_t p_nelmts; /* total selected elmts */

        if ((p_nelmts = H5Sget_simple_extent_npoints(f_space)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (NULL ==
            (sm_buf = (unsigned char *)calloc(MAX(sizeof(unsigned), sizeof(H5R_ref_t)), (size_t)p_nelmts)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_simple_mem: failed to allocate sm_buf");

        /* Read the data */
        if (H5Dread(dset, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, sm_buf) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (binary_order == 99) {
            if (h5str_dump_simple_data(ENVONLY, stream, dset, H5T_STD_REF, sm_buf, (size_t)p_nelmts) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
        else {
            if (h5str_render_bin_output(stream, dset, H5T_STD_REF, sm_buf, (size_t)p_nelmts) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
    }
    else {
        /* Print info */
        size_t  p_type_nbytes; /* size of memory type */
        hsize_t p_nelmts;      /* total selected elmts */

        switch (binary_order) {
            case 1: {
                if ((p_type = h5str_get_native_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            case 2: {
                if ((p_type = h5str_get_little_endian_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            case 3: {
                if ((p_type = h5str_get_big_endian_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            default: {
                if ((p_type = H5Tcopy(f_type)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                break;
            }
        }

        /* Calculate the number of elements we're going to print */
        p_nelmts = 1;

        if (sndims > 0) {
            for (i = 0; i < (size_t)sndims; i++)
                p_nelmts *= total_size[i];
        } /* end if */

        if (p_nelmts > 0) {
            /* Check if we have VL data in the dataset's datatype */
            if (h5str_detect_vlen(p_type) != 0)
                vl_data = 1;

            /*
             * Determine the strip mine size and allocate a buffer. The strip mine is
             * a hyperslab whose size is manageable.
             */
            if (!(sm_nbytes = p_type_nbytes = H5Tget_size(p_type)))
                H5_LIBRARY_ERROR(ENVONLY);

            if (sndims > 0) {
                for (i = (size_t)sndims; i > 0; --i) {
                    hsize_t size = H5TOOLS_BUFSIZE / sm_nbytes;
                    if (size == 0) /* datum size > H5TOOLS_BUFSIZE */
                        size = 1;
                    sm_size[i - 1] = (((total_size[i - 1]) < (size)) ? (total_size[i - 1]) : (size));
                    sm_nbytes *= sm_size[i - 1];
                }
            }

            if (sm_nbytes > 0) {
                if (NULL == (sm_buf = (unsigned char *)malloc((size_t)sm_nbytes)))
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_simple_dset: failed to allocate sm_buf");

                sm_nelmts = sm_nbytes / p_type_nbytes;

                if ((sm_space = H5Screate_simple(1, &sm_nelmts, NULL)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                /* The stripmine loop */
                memset(hs_offset, 0, sizeof hs_offset);
                memset(zero, 0, sizeof zero);

                for (elmtno = 0; elmtno < (hsize_t)p_nelmts; elmtno += hs_nelmts) {
                    /* Calculate the hyperslab size */
                    if (sndims > 0) {
                        for (i = 0, hs_nelmts = 1; i < (size_t)sndims; i++) {
                            hs_size[i] = (((total_size[i] - hs_offset[i]) < (sm_size[i]))
                                              ? (total_size[i] - hs_offset[i])
                                              : (sm_size[i]));
                            hs_nelmts *= hs_size[i];
                        }

                        if (H5Sselect_hyperslab(f_space, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL) < 0)
                            H5_LIBRARY_ERROR(ENVONLY);

                        if (H5Sselect_hyperslab(sm_space, H5S_SELECT_SET, zero, NULL, &hs_nelmts, NULL) < 0)
                            H5_LIBRARY_ERROR(ENVONLY);
                    }
                    else {
                        if (H5Sselect_all(f_space) < 0)
                            H5_LIBRARY_ERROR(ENVONLY);

                        if (H5Sselect_all(sm_space) < 0)
                            H5_LIBRARY_ERROR(ENVONLY);

                        hs_nelmts = 1;
                    }

                    /* Read the data */
                    if (H5Dread(dset, p_type, sm_space, f_space, H5P_DEFAULT, sm_buf) < 0)
                        H5_LIBRARY_ERROR(ENVONLY);

                    if (binary_order == 99) {
                        if (h5str_dump_simple_data(ENVONLY, stream, dset, p_type, sm_buf, hs_nelmts) < 0)
                            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    }
                    else {
                        if (h5str_render_bin_output(stream, dset, p_type, sm_buf, hs_nelmts) < 0)
                            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    }

                    /* Reclaim any VL memory, if necessary */
                    if (vl_data) {
                        if (H5Treclaim(p_type, sm_space, H5P_DEFAULT, sm_buf) < 0)
                            H5_LIBRARY_ERROR(ENVONLY);
                    }

                    /* Calculate the next hyperslab offset */
                    for (i = (size_t)sndims, carry = 1; i > 0 && carry; --i) {
                        hs_offset[i - 1] += hs_size[i - 1];

                        if (hs_offset[i - 1] == total_size[i - 1])
                            hs_offset[i - 1] = 0;
                        else
                            carry = 0;
                    }
                }
            }
        }
    }

    ret_value = SUCCEED;

done:
    if (sm_space >= 0)
        H5Sclose(sm_space);
    if (sm_buf)
        free(sm_buf);
    if (f_space >= 0)
        H5Sclose(f_space);
    if (p_type >= 0)
        H5Tclose(p_type);
    if (f_type >= 0)
        H5Tclose(f_type);

    return ret_value;
} /* end h5str_dump_simple_dset */

int
h5str_dump_simple_mem(JNIEnv *env, FILE *stream, hid_t attr_id, int binary_order)
{
    hid_t    f_space = H5I_INVALID_HID; /* file data space */
    hsize_t  alloc_size;
    int      sndims;                   /* rank of dataspace */
    unsigned i;                        /* counters */
    hsize_t  total_size[H5S_MAX_RANK]; /* total size of dataset */
    int      ret_value = 0;

    unsigned char *sm_buf = NULL; /* buffer for raw data */

    /* VL data special information */
    unsigned int vl_data = 0; /* contains VL datatypes */
    hid_t        p_type  = H5I_INVALID_HID;
    hid_t        f_type  = H5I_INVALID_HID;

    if (attr_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "h5str_dump_simple_mem: attr ID < 0");

    if ((f_type = H5Aget_type(attr_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5I_INVALID_HID == (f_space = H5Aget_space(attr_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if ((sndims = H5Sget_simple_extent_ndims(f_space)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5Sget_simple_extent_dims(f_space, total_size, NULL) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5Tequal(f_type, H5T_STD_REF_DSETREG)) {
        hssize_t p_nelmts; /* total selected elmts */

        if ((p_nelmts = H5Sget_simple_extent_npoints(f_space)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (NULL ==
            (sm_buf = (unsigned char *)calloc(MAX(sizeof(unsigned), sizeof(H5R_ref_t)), (size_t)p_nelmts)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_simple_mem: failed to allocate sm_buf");

        /* Read the data */
        if (H5Aread(attr_id, H5T_STD_REF, sm_buf) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        if (binary_order == 99) {
            if (h5str_dump_simple_data(ENVONLY, stream, attr_id, H5T_STD_REF, sm_buf, (size_t)p_nelmts) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
        else {
            if (h5str_render_bin_output(stream, attr_id, H5T_STD_REF, sm_buf, (size_t)p_nelmts) < 0)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        }
    }
    else {
        hsize_t p_nelmts; /* total selected elmts */

        switch (binary_order) {
            case 1: {
                if ((p_type = h5str_get_native_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            case 2: {
                if ((p_type = h5str_get_little_endian_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            case 3: {
                if ((p_type = h5str_get_big_endian_type(f_type)) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                break;
            }

            default: {
                if ((p_type = H5Tcopy(f_type)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);

                break;
            }
        }

        /* Calculate the number of elements we're going to print */
        p_nelmts = 1;

        if (sndims > 0) {
            for (i = 0; i < (size_t)sndims; i++)
                p_nelmts *= total_size[i];
        } /* end if */

        if (p_nelmts > 0) {
            /* Check if we have VL data in the dataset's datatype */
            if (h5str_detect_vlen(p_type) != 0)
                vl_data = 1;

            alloc_size = (size_t)p_nelmts * H5Tget_size(p_type);
            if (NULL == (sm_buf = (unsigned char *)malloc((size_t)alloc_size)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_simple_mem: failed to allocate sm_buf");

            /* Read the data */
            if (H5Aread(attr_id, p_type, sm_buf) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (binary_order == 99) {
                if (h5str_dump_simple_data(ENVONLY, stream, attr_id, p_type, sm_buf, (size_t)p_nelmts) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            }
            else {
                if (h5str_render_bin_output(stream, attr_id, p_type, sm_buf, (size_t)p_nelmts) < 0)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            }

            /* Reclaim any VL memory, if necessary */
            if (vl_data) {
                if (H5Treclaim(p_type, f_space, H5P_DEFAULT, sm_buf) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
            }
        }
    }

    ret_value = SUCCEED;

done:
    if (sm_buf)
        free(sm_buf);
    if (f_space >= 0)
        H5Sclose(f_space);
    if (p_type >= 0)
        H5Tclose(p_type);
    if (f_type >= 0)
        H5Tclose(f_type);

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: H5Tdetect_variable_str
 *
 * Purpose:  Recursive check for variable length string of a datatype.
 *
 * Return:   true : type contains any variable length string
 *           false : type doesn't contain any variable length string
 *           Negative value: failed
 *-------------------------------------------------------------------------
 */
htri_t
H5Tdetect_variable_str(hid_t tid)
{
    H5T_class_t tclass = -1;
    htri_t      ret    = false;

    ret = H5Tis_variable_str(tid);
    if ((ret == true) || (ret < 0))
        goto done;

    tclass = H5Tget_class(tid);
    if (tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t btid = H5Tget_super(tid);

        if (btid < 0) {
            ret = (htri_t)btid;
            goto done;
        }
        ret = H5Tdetect_variable_str(btid);
        if ((ret == true) || (ret < 0)) {
            H5Tclose(btid);
            goto done;
        }
    }
    else if (tclass == H5T_COMPOUND) {
        unsigned nmembs;
        int      snmembs = H5Tget_nmembers(tid);
        unsigned u;

        if (snmembs < 0) {
            ret = FAIL;
            goto done;
        }
        nmembs = (unsigned)snmembs;

        for (u = 0; u < nmembs; u++) {
            hid_t mtid = H5Tget_member_type(tid, u);

            ret = H5Tdetect_variable_str(mtid);
            if ((ret == true) || (ret < 0)) {
                H5Tclose(mtid);
                goto done;
            }
            H5Tclose(mtid);
        }
    }

done:
    return ret;
} /* end H5Tdetect_variable_str */

static int
h5str_dump_simple_data(JNIEnv *env, FILE *stream, hid_t container, hid_t type, void *_mem, hsize_t nelmts)
{
    unsigned char *mem = (unsigned char *)_mem;
    h5str_t        buffer; /* string into which to render */
    hsize_t        i;      /* element counter  */
    size_t         size;   /* datum size */
    int            line_count;
    int            ret_value = 0;

    if (!(size = H5Tget_size(type)))
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0, line_count = 0; i < nelmts; i++, line_count++) {
        size_t bytes_in = 0; /* # of bytes to write  */
        void  *memref   = mem + i * size;

        /* Render the data element*/
        h5str_new(&buffer, 32 * size);

        if (!buffer.s)
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "h5str_dump_simple_data: failed to allocate buffer");

        if (!(bytes_in = h5str_sprintf(ENVONLY, &buffer, container, type, memref, 1)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if ((i > 0) && (bytes_in > 0)) {
            if (fprintf(stream, ", ") < 0)
                H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_simple_data: fprintf failure");

            if (line_count >= H5TOOLS_TEXT_BLOCK) {
                line_count = 0;

                if (fprintf(stream, "\n") < 0)
                    H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_simple_data: fprintf failure");
            }
        }
        if (fprintf(stream, "%s", buffer.s) < 0)
            H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_simple_data: fprintf failure");

        h5str_free(&buffer);
    } /* end for (i = 0; i < nelmts... */

    if (fprintf(stream, "\n") < 0)
        H5_JNI_FATAL_ERROR(ENVONLY, "h5str_dump_simple_data: fprintf failure");

done:
    if (buffer.s)
        h5str_free(&buffer);

    return ret_value;
} /* end h5str_dump_simple_data */

/*
 * Utility Java APIs
 * Functions designed to workaround issues with the Java-C interface
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadComplex
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AreadComplex(JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id,
                                   jobjectArray buf)
{
    h5str_t h5str;
    jstring jstr;
    size_t  size;
    size_t  i;
    hid_t   p_type = H5I_INVALID_HID;
    jsize   n;
    char   *readBuf = NULL;
    herr_t  status  = FAIL;

    UNUSED(clss);

    memset(&h5str, 0, sizeof(h5str_t));

    if ((p_type = H5Tget_native_type(mem_type_id, H5T_DIR_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    size = (((H5Tget_size(mem_type_id)) > (H5Tget_size(p_type))) ? (H5Tget_size(mem_type_id))
                                                                 : (H5Tget_size(p_type)));

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5AreadComplex: read buffer length <= 0");

    if (NULL == (readBuf = (char *)malloc((size_t)n * size)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5AreadComplex: failed to allocate read buffer");

    if ((status = H5Aread(attr_id, mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    h5str_new(&h5str, 4 * size);

    if (!h5str.s)
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5AreadComplex: failed to allocate string buffer");

    for (i = 0; i < (size_t)n; i++) {
        h5str.s[0] = '\0';

        if (!h5str_sprintf(ENVONLY, &h5str, attr_id, mem_type_id, readBuf + (i * size), 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, h5str.s)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, buf, (jsize)i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (h5str.s)
        h5str_free(&h5str);
    if (readBuf)
        free(readBuf);
    if (p_type >= 0)
        H5Tclose(p_type);

    return status;
}

/*
 * Copies the content of one attribute to another attribute
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Acopy(JNIEnv *env, jclass clss, jlong src_id, jlong dst_id)
{
    hssize_t npoints;
    hsize_t  total_size = 0;
    size_t   type_size;
    jbyte   *buf    = NULL;
    hid_t    tid    = H5I_INVALID_HID;
    hid_t    sid    = H5I_INVALID_HID;
    herr_t   retVal = FAIL;

    UNUSED(clss);

    if ((sid = H5Aget_space((hid_t)src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((tid = H5Aget_type((hid_t)src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((npoints = H5Sget_simple_extent_npoints(sid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    if (!(type_size = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);
    total_size = (hsize_t)npoints * (hsize_t)type_size;

    if (NULL == (buf = (jbyte *)malloc((size_t)total_size * sizeof(jbyte))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Acopy: failed to allocate buffer");

    if ((retVal = H5Aread((hid_t)src_id, tid, buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5Tclose(tid) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    tid = H5I_INVALID_HID;

    if ((tid = H5Aget_type((hid_t)dst_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((retVal = H5Awrite((hid_t)dst_id, tid, buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (buf)
        free(buf);
    if (tid >= 0)
        H5Tclose(tid);
    if (sid >= 0)
        H5Sclose(sid);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Acopy */

/*
 * Copies the content of one dataset to another dataset
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dcopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dcopy(JNIEnv *env, jclass clss, jlong src_id, jlong dst_id)
{
    hssize_t npoints;
    hsize_t  total_size = 0, total_allocated_size;
    size_t   type_size;
    jbyte   *buf    = NULL;
    hid_t    tid    = H5I_INVALID_HID;
    hid_t    sid    = H5I_INVALID_HID;
    herr_t   retVal = FAIL;

    UNUSED(clss);

    if (!(total_allocated_size = H5Dget_storage_size((hid_t)src_id)))
        return 0; /* nothing to write */

    if ((sid = H5Dget_space((hid_t)src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((tid = H5Dget_type((hid_t)src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((npoints = H5Sget_simple_extent_npoints(sid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    if (!(type_size = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);
    total_size = (hsize_t)npoints * (hsize_t)type_size;

    if (NULL == (buf = (jbyte *)malloc((size_t)total_size * sizeof(jbyte))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dcopy: failed to allocate buffer");

    if ((retVal = H5Dread((hid_t)src_id, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5Tclose(tid) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    tid = H5I_INVALID_HID;

    if ((tid = H5Dget_type((hid_t)dst_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((retVal = H5Dwrite((hid_t)dst_id, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (buf)
        free(buf);
    if (tid >= 0)
        H5Tclose(tid);
    if (sid >= 0)
        H5Sclose(sid);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Dcopy */

/*
 * /////////////////////////////////////////////////////////////////////////////////
 * //
 * //
 * // Add these methods so that we don't need to call H5Gget_objtype_by_idx
 * // in a loop to get information for all the objects in a group, which takes
 * // a lot of time to finish if the number of objects is more than 10,000
 * //
 * /////////////////////////////////////////////////////////////////////////////////
 */

#ifdef __cplusplus
herr_t obj_info_all(hid_t g_id, const char *name, const H5L_info2_t *linfo, void *op_data);
herr_t obj_info_max(hid_t g_id, const char *name, const H5L_info2_t *linfo, void *op_data);
int    H5Gget_obj_info_max(hid_t, char **, int *, int *, H5O_token_t *, long);
int    H5Gget_obj_info_full(hid_t loc_id, char **objname, int *otype, int *ltype, unsigned long *fno,
                            H5O_token_t *obj_token, int indexType, int indexOrder);
#else
static herr_t obj_info_all(hid_t g_id, const char *name, const H5L_info2_t *linfo, void *op_data);
static herr_t obj_info_max(hid_t g_id, const char *name, const H5L_info2_t *linfo, void *op_data);
static int    H5Gget_obj_info_max(hid_t, char **, int *, int *, H5O_token_t *, long);
static int    H5Gget_obj_info_full(hid_t loc_id, char **objname, int *otype, int *ltype, unsigned long *fno,
                                   H5O_token_t *obj_token, int indexType, int indexOrder);
#endif

typedef struct info_all {
    char         **objname;
    int           *otype;
    int           *ltype;
    H5O_token_t   *obj_token;
    unsigned long *fno;
    unsigned long  idxnum;
    int            count;
} info_all_t;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_full
 * Signature: (JLjava/lang/String;[Ljava/lang/String;[I[I[J[JIII)I
 */
/*
 * NOTE: This is a dangerous call! The caller can supply any value they'd like
 * for 'n' and if it exceeds the number of links in the group, we will most likely
 * end up overwriting memory heap-tracking info.
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1full(JNIEnv *env, jclass clss, jlong loc_id, jstring group_name,
                                            jobjectArray objName, jintArray oType, jintArray lType,
                                            jlongArray fNo, jobjectArray oToken, jint n, jint indx_type,
                                            jint indx_order)
{
    unsigned long *fnos   = NULL;
    H5O_token_t   *tokens = NULL;
    const char    *gName  = NULL;
    char         **oName  = NULL;
    jboolean       isCopy;
    jstring        str;
    jobject        token;
    jint          *otarr = NULL;
    jint          *ltarr = NULL;
    jlong         *fnoP  = NULL;
    hid_t          gid   = (hid_t)loc_id;
    int            i;
    int            indexType  = indx_type;
    int            indexOrder = indx_order;
    herr_t         ret_val    = FAIL;

    UNUSED(clss);

    if (NULL == oType)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_full: oType is NULL");
    if (NULL == lType)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_full: lType is NULL");
    if (NULL == oToken)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_full: oToken is NULL");
    if (NULL == fNo)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_full: fNo is NULL");

    PIN_INT_ARRAY(ENVONLY, oType, otarr, &isCopy, "H5Gget_obj_info_full: oType not pinned");
    PIN_INT_ARRAY(ENVONLY, lType, ltarr, &isCopy, "H5Gget_obj_info_full: lType not pinned");
    PIN_LONG_ARRAY(ENVONLY, fNo, fnoP, &isCopy, "H5Gget_obj_info_full: fNo not pinned");

    if (NULL == (oName = (char **)calloc((size_t)n, sizeof(*oName))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Gget_obj_info_full: failed to allocate buffer for object name");

    if (NULL == (tokens = (H5O_token_t *)calloc((size_t)n, sizeof(H5O_token_t))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Gget_obj_info_full: failed to allocate buffer for object tokens");

    if (NULL == (fnos = (unsigned long *)calloc((size_t)n, sizeof(unsigned long))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "H5Gget_obj_info_full: failed to allocate buffer for file number info");

    if (group_name) {
        PIN_JAVA_STRING(ENVONLY, group_name, gName, &isCopy, "H5Gget_obj_info_full: group_name not pinned");

        if ((gid = H5Gopen2((hid_t)loc_id, gName, H5P_DEFAULT)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }

    if ((ret_val = H5Gget_obj_info_full(gid, oName, (int *)otarr, (int *)ltarr, fnos, tokens, indexType,
                                        indexOrder)) < 0)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Gget_obj_info_full: retrieval of object info failed");

    for (i = 0; i < n; i++) {
        fnoP[i] = (jlong)fnos[i];

        if (oName[i]) {
            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, oName[i])))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, objName, i, (jobject)str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->DeleteLocalRef(ENVONLY, str);
        } /* end if */

        /* Create an H5O_token_t object */
        if (NULL == (token = create_H5O_token_t(ENVONLY, &tokens[i], true)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, oToken, i, token);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, token);
    }

done:
    if (gName) {
        H5Gclose(gid);
        UNPIN_JAVA_STRING(ENVONLY, group_name, gName);
    }
    if (fnos)
        free(fnos);
    if (tokens)
        free(tokens);
    if (oName)
        h5str_array_free(oName, (size_t)n);
    if (fnoP)
        UNPIN_LONG_ARRAY(ENVONLY, fNo, fnoP, (ret_val < 0) ? JNI_ABORT : 0);
    if (ltarr)
        UNPIN_INT_ARRAY(ENVONLY, lType, ltarr, (ret_val < 0) ? JNI_ABORT : 0);
    if (otarr)
        UNPIN_INT_ARRAY(ENVONLY, oType, otarr, (ret_val < 0) ? JNI_ABORT : 0);

    return ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1full */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_max
 * Signature: (J[Ljava/lang/String;[I[I[JJI)I
 */
/*
 * NOTE: This is a dangerous call! The caller can supply any value they'd like
 * for 'n' and if it exceeds the number of links reachable from the group, we
 * will most likely end up overwriting memory heap-tracking info.
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1max(JNIEnv *env, jclass clss, jlong loc_id, jobjectArray objName,
                                           jintArray oType, jintArray lType, jobjectArray oToken,
                                           jlong maxnum, jint n)
{
    H5O_token_t *tokens = NULL;
    jboolean     isCopy;
    jstring      str;
    jobject      token;
    char       **oName = NULL;
    jint        *otarr = NULL;
    jint        *ltarr = NULL;
    int          i;
    herr_t       ret_val = FAIL;

    UNUSED(clss);

    if (NULL == oType)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_max: oType is NULL");
    if (NULL == lType)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_max: lType is NULL");
    if (NULL == oToken)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_obj_info_max: oToken is NULL");

    PIN_INT_ARRAY(ENVONLY, oType, otarr, &isCopy, "H5Gget_obj_info_max: oType not pinned");
    PIN_INT_ARRAY(ENVONLY, lType, ltarr, &isCopy, "H5Gget_obj_info_max: lType not pinned");

    if (NULL == (oName = (char **)calloc((size_t)n, sizeof(*oName))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Gget_obj_info_max: failed to allocate buffer for object name");

    if (NULL == (tokens = (H5O_token_t *)calloc((size_t)n, sizeof(H5O_token_t))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Gget_obj_info_max: failed to allocate buffer for object tokens");

    if ((ret_val = H5Gget_obj_info_max((hid_t)loc_id, oName, (int *)otarr, (int *)ltarr, tokens, maxnum)) < 0)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Gget_obj_info_max: retrieval of object info failed");

    for (i = 0; i < n; i++) {
        if (oName[i]) {
            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, oName[i])))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, objName, i, (jobject)str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->DeleteLocalRef(ENVONLY, str);
        }

        /* Create an H5O_token_t object */
        if (NULL == (token = create_H5O_token_t(ENVONLY, &tokens[i], true)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, oToken, i, token);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, token);
    } /* end for */

done:
    if (tokens)
        free(tokens);
    if (oName)
        h5str_array_free(oName, (size_t)n);
    if (ltarr)
        UNPIN_INT_ARRAY(ENVONLY, lType, ltarr, (ret_val < 0) ? JNI_ABORT : 0);
    if (otarr)
        UNPIN_INT_ARRAY(ENVONLY, oType, otarr, (ret_val < 0) ? JNI_ABORT : 0);

    return ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1max */

int
H5Gget_obj_info_full(hid_t loc_id, char **objname, int *otype, int *ltype, unsigned long *fno,
                     H5O_token_t *obj_token, int indexType, int indexOrder)
{
    info_all_t info;

    info.objname   = objname;
    info.otype     = otype;
    info.ltype     = ltype;
    info.idxnum    = 0;
    info.fno       = fno;
    info.obj_token = obj_token;
    info.count     = 0;

    if (H5Literate2(loc_id, (H5_index_t)indexType, (H5_iter_order_t)indexOrder, NULL, obj_info_all,
                    (void *)&info) < 0) {
        /*
         * Reset info stats; most importantly, reset the count.
         */
        info.objname   = objname;
        info.otype     = otype;
        info.ltype     = ltype;
        info.idxnum    = 0;
        info.fno       = fno;
        info.obj_token = obj_token;
        info.count     = 0;

        /* Iteration failed, try normal alphabetical order */
        if (H5Literate2(loc_id, H5_INDEX_NAME, H5_ITER_INC, NULL, obj_info_all, (void *)&info) < 0)
            return -1;
    }

    return info.count;
} /* end H5Gget_obj_info_full */

int
H5Gget_obj_info_max(hid_t loc_id, char **objname, int *otype, int *ltype, H5O_token_t *obj_token, long maxnum)
{
    info_all_t info;

    info.objname   = objname;
    info.otype     = otype;
    info.ltype     = ltype;
    info.idxnum    = (unsigned long)maxnum;
    info.obj_token = obj_token;
    info.count     = 0;

    if (H5Lvisit2(loc_id, H5_INDEX_NAME, H5_ITER_NATIVE, obj_info_max, (void *)&info) < 0)
        return -1;

    return info.count;
} /* end H5Gget_obj_info_max */

herr_t
obj_info_all(hid_t loc_id, const char *name, const H5L_info2_t *info, void *op_data)
{
    info_all_t *datainfo = (info_all_t *)op_data;
    H5O_info2_t object_info;
    htri_t      object_exists;

    datainfo->otype[datainfo->count]     = -1;
    datainfo->ltype[datainfo->count]     = -1;
    datainfo->obj_token[datainfo->count] = H5O_TOKEN_UNDEF;

    if (NULL == (datainfo->objname[datainfo->count] = strdup(name)))
        goto done;

    if ((object_exists = H5Oexists_by_name(loc_id, name, H5P_DEFAULT)) < 0)
        goto done;

    if (object_exists) {
        if (H5Oget_info_by_name3(loc_id, name, &object_info, H5O_INFO_ALL, H5P_DEFAULT) < 0)
            goto done;

        datainfo->otype[datainfo->count] = object_info.type;
        datainfo->ltype[datainfo->count] = info->type;
        datainfo->fno[datainfo->count]   = object_info.fileno;

        memcpy(&datainfo->obj_token[datainfo->count], &object_info.token, sizeof(object_info.token));
    }

done:
    datainfo->count++;

    return SUCCEED;
} /* end obj_info_all */

herr_t
obj_info_max(hid_t loc_id, const char *name, const H5L_info2_t *info, void *op_data)
{
    info_all_t *datainfo = (info_all_t *)op_data;
    H5O_info2_t object_info;

    datainfo->otype[datainfo->count]     = -1;
    datainfo->ltype[datainfo->count]     = -1;
    datainfo->objname[datainfo->count]   = NULL;
    datainfo->obj_token[datainfo->count] = H5O_TOKEN_UNDEF;

    /* This will be freed by h5str_array_free(oName, n) */
    if (NULL == (datainfo->objname[datainfo->count] = strdup(name)))
        goto done;

    if (H5Oget_info3(loc_id, &object_info, H5O_INFO_ALL) < 0)
        goto done;

    datainfo->otype[datainfo->count] = object_info.type;
    datainfo->ltype[datainfo->count] = info->type;

    memcpy(&datainfo->obj_token[datainfo->count], &object_info.token, sizeof(object_info.token));

done:
    datainfo->count++;

    if (datainfo->count >= (int)datainfo->idxnum)
        return 1;
    else
        return SUCCEED;
} /* end obj_info_max */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5export_dataset
 * Signature: (Ljava/lang/String;JLjava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5export_1dataset(JNIEnv *env, jclass clss, jstring file_export_name, jlong file_id,
                                      jstring object_path, jint binary_order)
{
    const char *file_export = NULL;
    const char *object_name = NULL;
    jboolean    isCopy;
    herr_t      ret_val    = FAIL;
    hid_t       dataset_id = H5I_INVALID_HID;
    FILE       *stream     = NULL;

    UNUSED(clss);

    if (NULL == file_export_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5export_dataset: file_export_name is NULL");

    if (NULL == object_path)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5export_dataset: object_path is NULL");

    PIN_JAVA_STRING(ENVONLY, object_path, object_name, &isCopy, "H5export_dataset: object_path not pinned");

    if ((dataset_id = H5Dopen2(file_id, object_name, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    PIN_JAVA_STRING(ENVONLY, file_export_name, file_export, NULL,
                    "H5export_dataset: file_export name not pinned");

    if (NULL == (stream = fopen(file_export, "w+")))
        H5_JNI_FATAL_ERROR(ENVONLY, "fopen failed");

    if ((ret_val = h5str_dump_simple_dset(ENVONLY, stream, dataset_id, binary_order)) < 0)
        H5_ASSERTION_ERROR(ENVONLY, "h5str_dump_simple_dset failed");

    if (stream) {
        fclose(stream);
        stream = NULL;
    }

done:
    if (stream)
        fclose(stream);
    if (file_export)
        UNPIN_JAVA_STRING(ENVONLY, file_export_name, file_export);
    if (object_name)
        UNPIN_JAVA_STRING(ENVONLY, object_path, object_name);
    if (dataset_id >= 0)
        H5Dclose(dataset_id);
} /* end Java_hdf_hdf5lib_H5_H5export_1dataset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5export_attribute
 * Signature: (Ljava/lang/String;JLjava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5export_1attribute(JNIEnv *env, jclass clss, jstring file_export_name, jlong dset_id,
                                        jstring attribute_name, jint binary_order)
{
    const char *file_export = NULL;
    const char *object_name = NULL;
    jboolean    isCopy;
    herr_t      ret_val = FAIL;
    hid_t       attr_id = H5I_INVALID_HID;
    FILE       *stream  = NULL;

    UNUSED(clss);

    if (NULL == file_export_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5export_dataset: file_export_name is NULL");

    if (NULL == attribute_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5export_dataset: object_path is NULL");

    PIN_JAVA_STRING(ENVONLY, attribute_name, object_name, &isCopy,
                    "H5export_dataset: object_path not pinned");

    if ((attr_id = H5Aopen(dset_id, object_name, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    PIN_JAVA_STRING(ENVONLY, file_export_name, file_export, NULL,
                    "H5export_dataset: file_export name not pinned");

    if (NULL == (stream = fopen(file_export, "w+")))
        H5_JNI_FATAL_ERROR(ENVONLY, "fopen failed");

    if ((ret_val = h5str_dump_simple_mem(ENVONLY, stream, attr_id, binary_order)) < 0)
        H5_ASSERTION_ERROR(ENVONLY, "h5str_dump_simple_dset failed");

    if (stream) {
        fclose(stream);
        stream = NULL;
    }

done:
    if (stream)
        fclose(stream);
    if (file_export)
        UNPIN_JAVA_STRING(ENVONLY, file_export_name, file_export);
    if (object_name)
        UNPIN_JAVA_STRING(ENVONLY, attribute_name, object_name);
    if (attr_id >= 0)
        H5Aclose(attr_id);
} /* end Java_hdf_hdf5lib_H5_H5export_1attribute */

jobject
translate_atomic_rbuf(JNIEnv *env, jlong mem_type_id, H5T_class_t type_class, void *raw_buf)
{
    jobject      jobj  = NULL;
    hid_t        memb  = H5I_INVALID_HID;
    jobjectArray jList = NULL;
    H5T_class_t  vlClass;
    size_t       vlSize;
    size_t       i;
    size_t       typeSize;
    // raw_buf is normally bytes except when used for variable length strings
    char *char_buf = (char *)raw_buf;

    /* retrieve the java.util.ArrayList interface class */
    jclass    arrCList      = ENVPTR->FindClass(ENVONLY, "java/util/ArrayList");
    jmethodID arrListMethod = ENVPTR->GetMethodID(ENVONLY, arrCList, "<init>", "(I)V");

    /* Cache class types */
    /* jclass cBool   = ENVPTR->FindClass(ENVONLY, "java/lang/Boolean"); */
    jclass cByte   = ENVPTR->FindClass(ENVONLY, "java/lang/Byte");
    jclass cShort  = ENVPTR->FindClass(ENVONLY, "java/lang/Short");
    jclass cInt    = ENVPTR->FindClass(ENVONLY, "java/lang/Integer");
    jclass cLong   = ENVPTR->FindClass(ENVONLY, "java/lang/Long");
    jclass cFloat  = ENVPTR->FindClass(ENVONLY, "java/lang/Float");
    jclass cDouble = ENVPTR->FindClass(ENVONLY, "java/lang/Double");
    /*jobjectArray
     jmethodID boolValueMid =
     ENVPTR->GetStaticMethodID(ENVONLY, cBool, "valueOf", "(Z)Ljava/lang/Boolean;");
     */
    jmethodID byteValueMid  = ENVPTR->GetStaticMethodID(ENVONLY, cByte, "valueOf", "(B)Ljava/lang/Byte;");
    jmethodID shortValueMid = ENVPTR->GetStaticMethodID(ENVONLY, cShort, "valueOf", "(S)Ljava/lang/Short;");
    jmethodID intValueMid   = ENVPTR->GetStaticMethodID(ENVONLY, cInt, "valueOf", "(I)Ljava/lang/Integer;");
    jmethodID longValueMid  = ENVPTR->GetStaticMethodID(ENVONLY, cLong, "valueOf", "(J)Ljava/lang/Long;");
    jmethodID floatValueMid = ENVPTR->GetStaticMethodID(ENVONLY, cFloat, "valueOf", "(F)Ljava/lang/Float;");
    jmethodID doubleValueMid =
        ENVPTR->GetStaticMethodID(ENVONLY, cDouble, "valueOf", "(D)Ljava/lang/Double;");

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    switch (type_class) {
        case H5T_VLEN: {
            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert element to a list */
            hvl_t vl_elem;

            /* Get the number of sequence elements */
            memcpy(&vl_elem, char_buf, sizeof(hvl_t));
            jsize nelmts = (jsize)vl_elem.len;
            if (vl_elem.len != (size_t)nelmts)
                H5_JNI_FATAL_ERROR(ENVONLY, "translate_atomic_rbuf: overflow of number of VL elements");

            if (nelmts < 0)
                H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_atomic_rbuf: number of VL elements < 0");

            /* The list we're going to return: */
            if (NULL == (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: failed to allocate list read buffer");

            translate_rbuf(ENVONLY, jList, memb, vlClass, (jsize)nelmts, vl_elem.p);
            jobj = jList;
            break;
        } /* H5T_VLEN */
        case H5T_COMPOUND: {
            int nmembs = H5Tget_nmembers(mem_type_id);

            /* The list we're going to return: */
            if (NULL == (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: failed to allocate list read buffer");

            /* Convert each element to a compound object */
            for (i = 0; i < (size_t)nmembs; i++) {
                H5T_class_t memb_vlClass;
                size_t      memb_vlSize;
                size_t      memb_offset;

                if ((memb = H5Tget_member_type(mem_type_id, (unsigned int)i)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                memb_offset = H5Tget_member_offset(mem_type_id, (unsigned int)i);
                if ((memb_vlClass = H5Tget_class(memb)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                if (!(memb_vlSize = H5Tget_size(memb)))
                    H5_LIBRARY_ERROR(ENVONLY);

                translate_atomic_rbuf(ENVONLY, memb, memb_vlClass, char_buf + i * typeSize + memb_offset);
                H5Tclose(memb);
            }
            jobj = jList;
            break;
        } /* H5T_COMPOUND */
        case H5T_ARRAY: {
            void  *objBuf = NULL;
            size_t typeCount;

            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(typeSize = H5Tget_size(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);

            typeCount = typeSize / vlSize;

            if (NULL == (objBuf = malloc(typeSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: failed to allocate buffer");

            /* Convert each element */
            /* Get the object element */
            memcpy((char *)objBuf, char_buf, typeSize);

            /* The list we're going to return: */
            if (NULL == (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: failed to allocate list read buffer");

            translate_rbuf(ENVONLY, jList, memb, vlClass, (jsize)typeCount, objBuf);
            jobj = jList;

            if (objBuf)
                free(objBuf);

            break;
        } /* H5T_ARRAY */
        case H5T_ENUM:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_INTEGER: {
            /* Convert each element */
            switch (typeSize) {
                case sizeof(jbyte): {
                    jbyte byteValue;
                    memcpy(((char *)&byteValue), char_buf, typeSize);

                    if (NULL ==
                        (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cByte, byteValueMid, byteValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                case sizeof(jshort): {
                    jshort shortValue;
                    memcpy(((char *)&shortValue), char_buf, typeSize);

                    if (NULL ==
                        (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cShort, shortValueMid, shortValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                case sizeof(jint): {
                    jint intValue;

                    memcpy(((char *)&intValue), char_buf, typeSize);

                    if (NULL == (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cInt, intValueMid, intValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                case sizeof(jlong): {
                    jlong longValue;
                    memcpy(((char *)&longValue), char_buf, typeSize);

                    if (NULL ==
                        (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cLong, longValueMid, longValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                default:
                    H5_BAD_ARGUMENT_ERROR(ENVONLY,
                                          "translate_atomic_rbuf: no matching JNI type for integer type");
            }
            break;
        } /* H5T_INTEGER */
        case H5T_FLOAT: {
            /* Convert each element to a list */
            switch (typeSize) {
                case sizeof(jfloat): {
                    jfloat floatValue;
                    memcpy(((char *)&floatValue), char_buf, typeSize);

                    if (NULL == (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cFloat, floatValueMid,
                                                                       (double)floatValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                case sizeof(jdouble): {
                    jdouble doubleValue;
                    memcpy(((char *)&doubleValue), char_buf, typeSize);

                    if (NULL == (jobj = ENVPTR->CallStaticObjectMethod(ENVONLY, cDouble, doubleValueMid,
                                                                       doubleValue)))
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                    break;
                }
                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY, "translate_atomic_rbuf: no matching JNI type for floating-point type");
            }
            break;
        } /* H5T_FLOAT */
        case H5T_REFERENCE: {
            /* Convert each element to a list */
            jboolean bb;
            jbyte   *barray = NULL;

            jsize byteArraySize = (jsize)typeSize;
            if (typeSize != (size_t)byteArraySize)
                H5_JNI_FATAL_ERROR(ENVONLY, "translate_atomic_rbuf: overflow of byteArraySize");

            if (NULL == (jobj = ENVPTR->NewByteArray(ENVONLY, byteArraySize)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            PIN_BYTE_ARRAY(ENVONLY, (jbyteArray)jobj, barray, &bb,
                           "translate_atomic_rbuf reference: byte array not pinned");

            memcpy(barray, ((jbyte *)raw_buf), typeSize);
            if (barray)
                UNPIN_BYTE_ARRAY(ENVONLY, (jbyteArray)jobj, barray, jobj ? 0 : JNI_ABORT);

            break;
        } /* H5T_REFERENCE */
        case H5T_STRING: {
            htri_t is_variable = 0;

            if ((is_variable = H5Tis_variable_str(mem_type_id)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each element */
            if (is_variable) {
                char **var_str_buf = (char **)raw_buf;
                if (NULL == (jobj = ENVPTR->NewStringUTF(ENVONLY, *var_str_buf))) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: out of memory - unable to "
                                                    "construct string from UTF characters");
                }
            }
            else {
                if (NULL == (jobj = ENVPTR->NewStringUTF(ENVONLY, char_buf))) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_rbuf: out of memory - unable to "
                                                    "construct string from UTF characters");
                }
            }

            break;
        } /* H5T_STRING */
        case H5T_TIME:
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            H5_UNIMPLEMENTED(ENVONLY, "translate_atomic_rbuf: invalid class type");
            break;
    } /* switch(type_class) */

done:

    return jobj;
}

void
translate_atomic_wbuf(JNIEnv *env, jobject in_obj, jlong mem_type_id, H5T_class_t type_class, void *raw_buf)
{
    hid_t       memb = H5I_INVALID_HID;
    H5T_class_t vlClass;
    size_t      vlSize;
    size_t      i;
    size_t      typeSize;
    // raw_buf is normally bytes except when used for variable length strings
    char *char_buf = (char *)raw_buf;

    /* retrieve the java.util.ArrayList interface class */
    jclass arrCList = ENVPTR->FindClass(ENVONLY, "java/util/ArrayList");
    /* retrieve the toArray method */
    jmethodID mToArray = ENVPTR->GetMethodID(ENVONLY, arrCList, "toArray", "()[Ljava/lang/Object;");

    /* Cache class types */
    jclass cByte   = ENVPTR->FindClass(ENVONLY, "java/lang/Byte");
    jclass cShort  = ENVPTR->FindClass(ENVONLY, "java/lang/Short");
    jclass cInt    = ENVPTR->FindClass(ENVONLY, "java/lang/Integer");
    jclass cLong   = ENVPTR->FindClass(ENVONLY, "java/lang/Long");
    jclass cFloat  = ENVPTR->FindClass(ENVONLY, "java/lang/Float");
    jclass cDouble = ENVPTR->FindClass(ENVONLY, "java/lang/Double");

    jmethodID byteValueMid   = ENVPTR->GetMethodID(ENVONLY, cByte, "byteValue", "()B");
    jmethodID shortValueMid  = ENVPTR->GetMethodID(ENVONLY, cShort, "shortValue", "()S");
    jmethodID intValueMid    = ENVPTR->GetMethodID(ENVONLY, cInt, "intValue", "()I");
    jmethodID longValueMid   = ENVPTR->GetMethodID(ENVONLY, cLong, "longValue", "()J");
    jmethodID floatValueMid  = ENVPTR->GetMethodID(ENVONLY, cFloat, "floatValue", "()F");
    jmethodID doubleValueMid = ENVPTR->GetMethodID(ENVONLY, cDouble, "doubleValue", "()D");

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    switch (type_class) {
        case H5T_VLEN: {
            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class((hid_t)memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert element to a vlen element */
            hvl_t vl_elem;

            jsize jnelmts = ENVPTR->GetArrayLength(ENVONLY, in_obj);

            if (jnelmts < 0)
                H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_atomic_wbuf: number of VL elements < 0");

            vl_elem.len = (size_t)jnelmts;

            if (NULL == (vl_elem.p = malloc((size_t)jnelmts * vlSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_wbuf: failed to allocate vlen ptr buffer");

            translate_wbuf(ENVONLY, (jobjectArray)in_obj, memb, vlClass, (jsize)jnelmts, vl_elem.p);

            memcpy(char_buf, &vl_elem, sizeof(hvl_t));
            break;
        } /* H5T_VLEN */
        case H5T_COMPOUND: {
            /* Convert each compound element */
            int nmembs = H5Tget_nmembers(mem_type_id);

            /* invoke the toArray method */
            if (mToArray == NULL)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            jobjectArray array   = (jobjectArray)ENVPTR->CallObjectMethod(ENVONLY, in_obj, mToArray);
            jsize        jnelmts = ENVPTR->GetArrayLength(ENVONLY, array);

            if (jnelmts != nmembs)
                H5_BAD_ARGUMENT_ERROR(
                    ENVONLY, "translate_atomic_wbuf: number of elements not equal to number of members");

            /* Convert each compound object to an element */
            for (i = 0; i < (size_t)nmembs; i++) {
                H5T_class_t memb_vlClass;
                size_t      memb_vlSize;
                size_t      memb_offset;

                if ((memb = H5Tget_member_type(mem_type_id, (unsigned int)i)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                memb_offset = H5Tget_member_offset(mem_type_id, (unsigned int)i);
                if ((memb_vlClass = H5Tget_class(memb)) < 0)
                    H5_LIBRARY_ERROR(ENVONLY);
                if (!(memb_vlSize = H5Tget_size(memb)))
                    H5_LIBRARY_ERROR(ENVONLY);

                jobject arr_obj = ENVPTR->GetObjectArrayElement(ENVONLY, array, (jsize)i);
                translate_atomic_wbuf(ENVONLY, arr_obj, memb, memb_vlClass,
                                      char_buf + i * typeSize + memb_offset);
                ENVPTR->DeleteLocalRef(ENVONLY, arr_obj);
                H5Tclose(memb);
            }
            break;
        } /* H5T_COMPOUND */
        case H5T_ARRAY: {
            void *objBuf = NULL;

            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each array element */
            /* invoke the toArray method */
            if (mToArray == NULL)
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            jobjectArray array   = (jobjectArray)ENVPTR->CallObjectMethod(ENVONLY, in_obj, mToArray);
            jsize        jnelmts = ENVPTR->GetArrayLength(ENVONLY, array);

            if (jnelmts < 0)
                H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_atomic_wbuf: number of array elements < 0");

            if (NULL == (objBuf = malloc((size_t)jnelmts * vlSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_atomic_wbuf: failed to allocate buffer");

            translate_wbuf(ENVONLY, array, memb, vlClass, (jsize)jnelmts, objBuf);

            memcpy(char_buf, (char *)objBuf, vlSize * (size_t)jnelmts);
            break;
        } /* H5T_ARRAY */
        case H5T_ENUM:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_INTEGER: {
            /* Convert each element */
            switch (typeSize) {
                case sizeof(jbyte): {
                    jbyte byteValue = ENVPTR->CallByteMethod(ENVONLY, in_obj, byteValueMid);
                    memcpy(char_buf, ((char *)&byteValue), typeSize);
                    break;
                }
                case sizeof(jshort): {
                    jshort shortValue = ENVPTR->CallShortMethod(ENVONLY, in_obj, shortValueMid);
                    memcpy(char_buf, ((char *)&shortValue), typeSize);
                    break;
                }
                case sizeof(jint): {
                    jint intValue = ENVPTR->CallIntMethod(ENVONLY, in_obj, intValueMid);
                    memcpy(char_buf, ((char *)&intValue), typeSize);
                    break;
                }
                case sizeof(jlong): {
                    jlong longValue = ENVPTR->CallLongMethod(ENVONLY, in_obj, longValueMid);
                    memcpy(char_buf, ((char *)&longValue), typeSize);
                    break;
                }
                default:
                    H5_BAD_ARGUMENT_ERROR(ENVONLY,
                                          "translate_atomic_wbuf: no matching JNI type for integer type");
            }
            break;
        } /* H5T_INTEGER */
        case H5T_FLOAT: {
            /* Convert each element */
            switch (typeSize) {
                case sizeof(jfloat): {
                    jfloat floatValue = ENVPTR->CallFloatMethod(ENVONLY, in_obj, floatValueMid);
                    memcpy(char_buf, ((char *)&floatValue), typeSize);
                    break;
                }
                case sizeof(jdouble): {
                    jdouble doubleValue = ENVPTR->CallDoubleMethod(ENVONLY, in_obj, doubleValueMid);
                    memcpy(char_buf, ((char *)&doubleValue), typeSize);
                    break;
                }
                default:
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY, "translate_atomic_wbuf: no matching JNI type for floating-point type");
            }
            break;
        } /* H5T_FLOAT */
        case H5T_REFERENCE: {
            /* Convert each array element */
            jbyte *barray = (jbyte *)ENVPTR->GetByteArrayElements(ENVONLY, in_obj, 0);
            memcpy(char_buf, ((char *)barray), typeSize);
            ENVPTR->ReleaseByteArrayElements(ENVONLY, in_obj, barray, 0);
            break;
        } /* H5T_REFERENCE */
        case H5T_STRING: {
            htri_t is_variable = 0;

            if ((is_variable = H5Tis_variable_str(mem_type_id)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each pointer element */
            jsize       length;
            const char *utf8 = NULL;

            memset(char_buf, 0, typeSize);
            if (NULL != in_obj) {
                PIN_JAVA_STRING(ENVONLY, in_obj, utf8, NULL, "translate_atomic_wbuf jobj not pinned");
                length = ENVPTR->GetStringUTFLength(ENVONLY, in_obj);
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                if (is_variable) {
                    char *new_buf = (char *)calloc(1, (size_t)(length + 1));
                    memcpy(((char *)new_buf), utf8, (size_t)length);
                    memcpy(char_buf, &new_buf, typeSize);
                }
                else {
                    memcpy(char_buf, utf8, (size_t)length);
                }
                UNPIN_JAVA_STRING(ENVONLY, in_obj, utf8);
                utf8 = NULL;
            }
            break;
        } /* H5T_STRING */
        case H5T_TIME:
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            H5_UNIMPLEMENTED(ENVONLY, "translate_atomic_wbuf: invalid class type");
            break;
    } /* switch(type_class) */

done:

    return;
}

void
translate_rbuf(JNIEnv *env, jobjectArray ret_buf, jlong mem_type_id, H5T_class_t type_class, jsize count,
               void *raw_buf)
{
    hid_t        memb        = H5I_INVALID_HID;
    int          ret_buflen  = -1;
    jboolean     found_jList = JNI_TRUE;
    jobjectArray jList       = NULL;
    jobject      jobj        = NULL;
    H5T_class_t  vlClass;
    size_t       vlSize;
    size_t       i, x;
    size_t       typeSize;
    // raw_buf is normally bytes except when used for variable length strings
    char *char_buf = (char *)raw_buf;

    /* retrieve the java.util.ArrayList interface class */
    jclass    arrCList      = ENVPTR->FindClass(ENVONLY, "java/util/ArrayList");
    jmethodID arrListMethod = ENVPTR->GetMethodID(ENVONLY, arrCList, "<init>", "(I)V");
    jmethodID arrAddMethod  = ENVPTR->GetMethodID(ENVONLY, arrCList, "add", "(Ljava/lang/Object;)Z");

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    ret_buflen = ENVPTR->GetArrayLength(ENVONLY, ret_buf);
    if (ret_buflen < 0)
        H5_JNI_FATAL_ERROR(ENVONLY, "ret_buflen: Array length cannot be negative");

    switch (type_class) {
        case H5T_VLEN: {
            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each element to a list */
            for (i = 0; i < (size_t)count; i++) {
                hvl_t vl_elem;

                found_jList = JNI_TRUE;
                jList       = NULL;

                /* Get the number of sequence elements */
                memcpy(&vl_elem, char_buf + i * sizeof(hvl_t), sizeof(hvl_t));
                jsize nelmts = (jsize)vl_elem.len;
                if (vl_elem.len != (size_t)nelmts)
                    H5_JNI_FATAL_ERROR(ENVONLY, "translate_rbuf: overflow of number of VL elements");

                if (nelmts < 0)
                    H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_rbuf: number of VL elements < 0");

                /* The list we're going to return: */
                if (i < (size_t)ret_buflen) {
                    jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)ret_buf, (jsize)i);
                }
                if (NULL == jList) {
                    found_jList = JNI_FALSE;
                    if (NULL ==
                        (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                               "translate_rbuf: failed to allocate list read buffer");
                }

                translate_rbuf(ENVONLY, jList, memb, vlClass, (jsize)nelmts, vl_elem.p);
                if (found_jList == JNI_FALSE) {
                    jboolean addResult =
                        ENVPTR->CallBooleanMethod(ENVONLY, ret_buf, arrAddMethod, (jobject)jList);
                    if (!addResult)
                        H5_JNI_FATAL_ERROR(ENVONLY, "translate_rbuf: cannot add VL element");
                }
                else {
                    ENVPTR->SetObjectArrayElement(ENVONLY, ret_buf, (jsize)i, (jobject)jList);
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                }
                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            }
            break;
        } /* H5T_VLEN */
        case H5T_COMPOUND: {
            /* Convert each compound element to a list */
            for (i = 0; i < (size_t)count; i++) {
                found_jList = JNI_TRUE;
                jList       = NULL;

                /* The list we're going to return: */
                if (i < (size_t)ret_buflen) {
                    jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)ret_buf, (jsize)i);
                }
                if (NULL == jList) {
                    found_jList = JNI_FALSE;
                    if (NULL ==
                        (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                               "translate_rbuf: failed to allocate list read buffer");
                }
                int nmembs = H5Tget_nmembers(mem_type_id);
                /* Convert each element to a list */
                for (x = 0; x < (size_t)nmembs; x++) {
                    H5T_class_t memb_vlClass;
                    size_t      memb_vlSize;
                    size_t      memb_offset;

                    if ((memb = H5Tget_member_type(mem_type_id, (unsigned int)x)) < 0)
                        H5_LIBRARY_ERROR(ENVONLY);
                    memb_offset = H5Tget_member_offset(mem_type_id, (unsigned int)x);

                    if ((memb_vlClass = H5Tget_class(memb)) < 0)
                        H5_LIBRARY_ERROR(ENVONLY);
                    if (!(memb_vlSize = H5Tget_size(memb)))
                        H5_LIBRARY_ERROR(ENVONLY);

                    jobj = translate_atomic_rbuf(ENVONLY, memb, memb_vlClass,
                                                 char_buf + i * typeSize + memb_offset);
                    if (jobj) {
                        if (found_jList == JNI_FALSE)
                            ENVPTR->CallBooleanMethod(ENVONLY, jList, arrAddMethod, (jobject)jobj);
                        else
                            ENVPTR->SetObjectArrayElement(ENVONLY, jList, (jsize)i, (jobject)jobj);
                        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                        ENVPTR->DeleteLocalRef(ENVONLY, jobj);
                    }

                    H5Tclose(memb);
                }
                if (ret_buflen == 0)
                    ENVPTR->CallBooleanMethod(ENVONLY, ret_buf, arrAddMethod, jList);
                else
                    ENVPTR->SetObjectArrayElement(ENVONLY, ret_buf, (jsize)i, jList);
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            }
            break;
        } /* H5T_COMPOUND */
        case H5T_ARRAY: {
            void  *objBuf = NULL;
            size_t typeCount;

            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(typeSize = H5Tget_size(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);

            typeCount = typeSize / vlSize;

            if (NULL == (objBuf = malloc(typeSize)))
                H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_rbuf: failed to allocate buffer");

            /* Convert each element to a list */
            for (i = 0; i < (size_t)count; i++) {
                found_jList = JNI_TRUE;
                jList       = NULL;

                /* Get the object element */
                memcpy((char *)objBuf, char_buf + i * typeSize, typeSize);

                /* The list we're going to return: */
                if (i < (size_t)ret_buflen) {
                    if (NULL ==
                        (jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)ret_buf, (jsize)i)))
                        found_jList = JNI_FALSE;
                }
                if (NULL == jList) {
                    if (NULL ==
                        (jList = (jobjectArray)ENVPTR->NewObject(ENVONLY, arrCList, arrListMethod, 0)))
                        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                               "translate_rbuf: failed to allocate list read buffer");
                }

                translate_rbuf(ENVONLY, jList, memb, vlClass, (jsize)typeCount, objBuf);
                if (found_jList == JNI_FALSE)
                    ENVPTR->CallBooleanMethod(ENVONLY, ret_buf, arrAddMethod, jList);
                else
                    ENVPTR->SetObjectArrayElement(ENVONLY, ret_buf, (jsize)i, jList);
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            }

            if (objBuf)
                free(objBuf);

            break;
        } /* H5T_ARRAY */
        case H5T_ENUM:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_REFERENCE:
        case H5T_STRING: {
            /* Convert each element to a list */
            for (i = 0; i < (size_t)count; i++) {
                jobj = translate_atomic_rbuf(ENVONLY, mem_type_id, type_class, char_buf + i * typeSize);
                if (jobj) {
                    if (ret_buflen == 0)
                        ENVPTR->CallBooleanMethod(ENVONLY, ret_buf, arrAddMethod, (jobject)jobj);
                    else
                        ENVPTR->SetObjectArrayElement(ENVONLY, ret_buf, (jsize)i, (jobject)jobj);
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                    ENVPTR->DeleteLocalRef(ENVONLY, jobj);
                }
            }
            break;
        }
        case H5T_TIME:
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            H5_UNIMPLEMENTED(ENVONLY, "translate_rbuf: invalid class type");
            break;
    } /* switch(type_class) */

done:

    return;
}

void
translate_wbuf(JNIEnv *env, jobjectArray in_buf, jlong mem_type_id, H5T_class_t type_class, jsize count,
               void *raw_buf)
{
    hid_t        memb  = H5I_INVALID_HID;
    jobjectArray jList = NULL;
    jobject      jobj  = NULL;
    H5T_class_t  vlClass;
    size_t       vlSize;
    size_t       i, x;
    size_t       typeSize;
    // raw_buf is normally bytes except when used for variable length strings
    char *char_buf = (char *)raw_buf;

    /* retrieve the java.util.ArrayList interface class */
    jclass arrCList = ENVPTR->FindClass(ENVONLY, "java/util/ArrayList");
    /* retrieve the toArray method */
    jmethodID mToArray = ENVPTR->GetMethodID(ENVONLY, arrCList, "toArray", "()[Ljava/lang/Object;");

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    switch (type_class) {
        case H5T_VLEN: {
            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class((hid_t)memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each list to a vlen element */
            for (i = 0; i < (size_t)count; i++) {
                hvl_t vl_elem;

                if (NULL == (jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)in_buf, (jsize)i)))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                /* invoke the toArray method */
                if (mToArray == NULL)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                jobjectArray array   = (jobjectArray)ENVPTR->CallObjectMethod(ENVONLY, jList, mToArray);
                jsize        jnelmts = ENVPTR->GetArrayLength(ENVONLY, array);

                if (jnelmts < 0)
                    H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_wbuf: number of VL elements < 0");

                vl_elem.len = (size_t)jnelmts;

                if (NULL == (vl_elem.p = malloc((size_t)jnelmts * vlSize)))
                    H5_OUT_OF_MEMORY_ERROR(ENVONLY, "translate_wbuf: failed to allocate vlen ptr buffer");

                translate_wbuf(ENVONLY, array, memb, vlClass, (jsize)jnelmts, vl_elem.p);

                memcpy(char_buf + i * sizeof(hvl_t), &vl_elem, sizeof(hvl_t));

                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            } /* end for (i = 0; i < count; i++) */
            break;
        } /* H5T_VLEN */
        case H5T_COMPOUND: {
            /* Convert each list to a compound element */
            for (i = 0; i < (size_t)count; i++) {
                if (NULL == (jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)in_buf, (jsize)i)))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                int nmembs = H5Tget_nmembers(mem_type_id);

                /* invoke the toArray method */
                if (mToArray == NULL)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                jobjectArray array   = (jobjectArray)ENVPTR->CallObjectMethod(ENVONLY, jList, mToArray);
                jsize        jnelmts = ENVPTR->GetArrayLength(ENVONLY, array);

                if (jnelmts != nmembs)
                    H5_BAD_ARGUMENT_ERROR(
                        ENVONLY, "translate_wbuf: number of elements not equal to number of members");

                /* Convert each compound object to an element */
                for (x = 0; x < (size_t)nmembs; x++) {
                    H5T_class_t memb_vlClass;
                    size_t      memb_vlSize;
                    size_t      memb_offset;

                    if ((memb = H5Tget_member_type(mem_type_id, (unsigned int)x)) < 0)
                        H5_LIBRARY_ERROR(ENVONLY);
                    memb_offset = H5Tget_member_offset(mem_type_id, (unsigned int)x);

                    if ((memb_vlClass = H5Tget_class(memb)) < 0)
                        H5_LIBRARY_ERROR(ENVONLY);
                    if (!(memb_vlSize = H5Tget_size(memb)))
                        H5_LIBRARY_ERROR(ENVONLY);

                    jobject arr_obj = ENVPTR->GetObjectArrayElement(ENVONLY, array, (jsize)x);
                    translate_atomic_wbuf(ENVONLY, arr_obj, memb, memb_vlClass,
                                          char_buf + i * typeSize + memb_offset);
                    ENVPTR->DeleteLocalRef(ENVONLY, arr_obj);
                    H5Tclose(memb);
                }

                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            } /* end for (i = 0; i < count; i++) */
            break;
        } /* H5T_COMPOUND */
        case H5T_ARRAY: {
            if (!(memb = H5Tget_super(mem_type_id)))
                H5_LIBRARY_ERROR(ENVONLY);
            if ((vlClass = H5Tget_class(memb)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            if (!(vlSize = H5Tget_size(memb)))
                H5_LIBRARY_ERROR(ENVONLY);

            /* Convert each list to an array element */
            for (i = 0; i < (size_t)count; i++) {
                if (NULL == (jList = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)in_buf, (jsize)i)))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                /* invoke the toArray method */
                if (mToArray == NULL)
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                jobjectArray array   = (jobjectArray)ENVPTR->CallObjectMethod(ENVONLY, jList, mToArray);
                jsize        jnelmts = ENVPTR->GetArrayLength(ENVONLY, array);

                if (jnelmts < 0)
                    H5_BAD_ARGUMENT_ERROR(ENVONLY, "translate_wbuf: number of array elements < 0");

                translate_wbuf(ENVONLY, array, memb, vlClass, jnelmts,
                               char_buf + i * vlSize * (size_t)jnelmts);

                ENVPTR->DeleteLocalRef(ENVONLY, jList);
            } /* end for (i = 0; i < count; i++) */
            break;
        } /* H5T_ARRAY */
        case H5T_ENUM:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_REFERENCE:
        case H5T_STRING: {
            /* Convert each list to an array element */
            for (i = 0; i < (size_t)count; i++) {
                if (NULL == (jobj = ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)in_buf, (jsize)i)))
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
                translate_atomic_wbuf(ENVONLY, jobj, mem_type_id, type_class, char_buf + i * typeSize);
                ENVPTR->DeleteLocalRef(ENVONLY, jobj);
            }
            break;
        }
        case H5T_TIME:
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            H5_UNIMPLEMENTED(ENVONLY, "translate_wbuf: invalid class type");
            break;
    } /* switch(type_class) */

done:

    return;
}

#ifdef __cplusplus
}
#endif
