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

#include "H5private.h"
#include "h5tools.h"
#include "h5tools_utils.h"
#include "h5diff.h"
#include "ph5diff.h"

#define ATTR_NAME_MAX 255

/*-------------------------------------------------------------------------
 * printf formatting
 *-------------------------------------------------------------------------
 */

#define F_FORMAT    "%-15g %-15g %-15g\n"
#define LD_FORMAT   "%-15Lg %-15Lg %-15Lg\n"
#define I_FORMAT    "%-15d %-15d %-15d\n"
#define S_FORMAT    "%-16s %-17s\n"
#define UI_FORMAT   "%-15u %-15u %-15u\n"
#define LI_FORMAT   "%-15ld %-15ld %-15ld\n"
#define ULI_FORMAT  "%-15lu %-15lu %-15lu\n"
#define LLI_FORMAT  "%-15lld %-15lld %-15lld\n"
#define ULLI_FORMAT "%-15llu %-15llu %-15llu\n"

/* with -p option */
#define F_FORMAT_P    "%-15.10g %-15.10g %-15.10g %-14.10g\n"
#define LD_FORMAT_P   "%-15.10Lg %-15.10Lg %-15.10Lg %-14.10Lg\n"
#define I_FORMAT_P    "%-15d %-15d %-15d %-14f\n"
#define UI_FORMAT_P   "%-15u %-15u %-15u %-14f\n"
#define LI_FORMAT_P   "%-15ld %-15ld %-15ld %-14f\n"
#define ULI_FORMAT_P  "%-15lu %-15lu %-15lu %-14f\n"
#define LLI_FORMAT_P  "%-15lld %-15lld %-15lld %-14f\n"
#define ULLI_FORMAT_P "%-15llu %-15llu %-15lld %-14f\n"
#define SPACES        "          "

/* not comparable */
#define F_FORMAT_P_NOTCOMP    "%-15.10g %-15.10g %-15.10g not comparable\n"
#define LD_FORMAT_P_NOTCOMP   "%-15.10Lg %-15.10Lg %-15.10Lg not comparable\n"
#define I_FORMAT_P_NOTCOMP    "%-15d %-15d %-15d not comparable\n"
#define UI_FORMAT_P_NOTCOMP   "%-15u %-15u %-15u not comparable\n"
#define LI_FORMAT_P_NOTCOMP   "%-15ld %-15ld %-15ld not comparable\n"
#define ULI_FORMAT_P_NOTCOMP  "%-15lu %-15lu %-15lu not comparable\n"
#define LLI_FORMAT_P_NOTCOMP  "%-15lld %-15lld %-15lld not comparable\n"
#define ULLI_FORMAT_P_NOTCOMP "%-15llu %-15llu %-15lld not comparable\n"

/* if system EPSILON is defined, use the system EPSILON; otherwise, use
 constants that are close to most EPSILON values */

#ifndef FLT_EPSILON
#define FLT_EPSILON 1.19209E-07
#endif

#ifndef DBL_EPSILON
#define DBL_EPSILON 2.22045E-16
#endif

/*-------------------------------------------------------------------------
 * -p relative error formula
 *
 * We assume the true value of a quantity to be A (value in first dataset)
 *  and the measured or inferred value to be B (value in second dataset).
 *  The relative error is defined by
 *
 *  B - A
 * --------
 *    A
 *

 *-------------------------------------------------------------------------
 */

static bool not_comparable;

#define PER(A, B)                                                                                            \
    do {                                                                                                     \
        per            = -1;                                                                                 \
        not_comparable = false;                                                                              \
        both_zero      = false;                                                                              \
        if (H5_DBL_ABS_EQUAL(0, (double)(A)) && H5_DBL_ABS_EQUAL(0, (double)(B)))                            \
            both_zero = true;                                                                                \
        if (!H5_DBL_ABS_EQUAL(0, (double)(A)))                                                               \
            per = (double)ABS((double)((B) - (A)) / (double)(A));                                            \
        else                                                                                                 \
            not_comparable = true;                                                                           \
    } while (0)

#define PER_UNSIGN(TYPE, A, B)                                                                               \
    do {                                                                                                     \
        per            = -1;                                                                                 \
        not_comparable = false;                                                                              \
        both_zero      = false;                                                                              \
        if (H5_DBL_ABS_EQUAL(0, (double)(A)) && H5_DBL_ABS_EQUAL(0, (double)(B)))                            \
            both_zero = true;                                                                                \
        if (!H5_DBL_ABS_EQUAL(0, (double)(A)))                                                               \
            per = ABS((double)((TYPE)((B) - (A))) / (double)(A));                                            \
        else                                                                                                 \
            not_comparable = true;                                                                           \
    } while (0)

#define PDIFF(a, b) (((b) > (a)) ? ((b) - (a)) : ((a) - (b)))

typedef struct mcomp_t {
    unsigned         n;   /* number of members */
    hid_t           *ids; /* member type id */
    size_t          *offsets;
    struct mcomp_t **m; /* members */
} mcomp_t;

/*-------------------------------------------------------------------------
 * local prototypes
 *-------------------------------------------------------------------------
 */
static bool    all_zero(const void *_mem, size_t size);
static int     ull2float(unsigned long long ull_value, float *f_value);
static hsize_t character_compare(char *mem1, char *mem2, hsize_t elemtno, size_t u, diff_opt_t *opts);
static hsize_t character_compare_opt(unsigned char *mem1, unsigned char *mem2, hsize_t elemtno,
                                     diff_opt_t *opts);
static bool    equal_float(float value, float expected, diff_opt_t *opts);
static bool    equal_double(double value, double expected, diff_opt_t *opts);
static bool    equal_ldouble(long double value, long double expected, diff_opt_t *opts);

static int  print_data(diff_opt_t *opts);
static void print_pos(diff_opt_t *opts, hsize_t elemtno, size_t u);
static void h5diff_print_char(char ch);

static hsize_t diff_region(hid_t obj1_id, hid_t obj2_id, hid_t region1_id, hid_t region2_id,
                           diff_opt_t *opts);
static hsize_t diff_datum(void *_mem1, void *_mem2, hsize_t elemtno, diff_opt_t *opts, hid_t container1_id,
                          hid_t container2_id, mcomp_t *members);
/* element diffs */
static hsize_t diff_float_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_double_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                   diff_opt_t *opts);
static hsize_t diff_ldouble_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                    diff_opt_t *opts);
#ifdef H5_HAVE__FLOAT16
static hsize_t diff_float16_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                    diff_opt_t *opts);
#endif
static hsize_t diff_schar_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_uchar_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_short_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_ushort_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                   diff_opt_t *opts);
static hsize_t diff_int_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts);
static hsize_t diff_uint_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                 diff_opt_t *opts);
static hsize_t diff_long_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                 diff_opt_t *opts);
static hsize_t diff_ulong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_llong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                  diff_opt_t *opts);
static hsize_t diff_ullong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx,
                                   diff_opt_t *opts);

/*-------------------------------------------------------------------------
 * NaN detection
 *-------------------------------------------------------------------------
 */

typedef enum dtype_t { FLT_FLOAT, FLT_DOUBLE, FLT_LDOUBLE } dtype_t;

/*-------------------------------------------------------------------------
 * XCAO, 11/10/2010
 * added to improve performance for compound datasets
 */
static void get_member_types(hid_t tid, mcomp_t *members);
static void close_member_types(mcomp_t *members);

/*-------------------------------------------------------------------------
 * Function: diff_array
 *
 * Purpose: compare two memory buffers;
 *
 * Return: number of differences found
 *-------------------------------------------------------------------------
 */

hsize_t
diff_array(void *_mem1, void *_mem2, diff_opt_t *opts, hid_t container1_id, hid_t container2_id)
{
    hsize_t        nfound = 0; /* number of differences found */
    size_t         size;       /* size of datum */
    unsigned char *mem1 = (unsigned char *)_mem1;
    unsigned char *mem2 = (unsigned char *)_mem2;
    hsize_t        i;
    mcomp_t        members;
    H5T_class_t    type_class;

    H5TOOLS_START_DEBUG(" - rank:%d hs_nelmts:%" PRIuHSIZE " errstat:%d", opts->rank, opts->hs_nelmts,
                        opts->err_stat);
    opts->print_header = 1; /* enable print header  */

    /* get the size. */
    size       = H5Tget_size(opts->m_tid);
    type_class = H5Tget_class(opts->m_tid);

    /* Fast comparison first for atomic type by memcmp().
     * It is OK not to list non-atomic type here because it will not be caught
     * by the condition, but it gives more clarity for code planning
     */
    if (type_class != H5T_REFERENCE && type_class != H5T_COMPOUND && type_class != H5T_STRING &&
        type_class != H5T_VLEN && memcmp(mem1, mem2, size * opts->hs_nelmts) == 0) {
        H5TOOLS_ENDDEBUG(":Fast comparison - errstat:%d", opts->err_stat);
        return 0;
    }

    H5TOOLS_DEBUG("type_class:%d", type_class);
    switch (type_class) {
        case H5T_NO_CLASS:
        case H5T_TIME:
        case H5T_NCLASSES:
        default:
            H5TOOLS_DEBUG("type_class:INVALID");
            assert(0);
            break;

        /*-------------------------------------------------------------------------
         * float and integer atomic types
         *-------------------------------------------------------------------------
         */
        case H5T_FLOAT:
            H5TOOLS_DEBUG("type_class:H5T_FLOAT");
#ifdef H5_HAVE__FLOAT16
            if (H5Tequal(opts->m_tid, H5T_NATIVE_FLOAT16)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_float16_element(mem1, mem2, i, opts);

                    mem1 += sizeof(H5__Float16);
                    mem2 += sizeof(H5__Float16);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                }
            }
            else
#endif
                if (H5Tequal(opts->m_tid, H5T_NATIVE_FLOAT)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_float_element(mem1, mem2, i, opts);

                    mem1 += sizeof(float);
                    mem2 += sizeof(float);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_DOUBLE)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_double_element(mem1, mem2, i, opts);

                    mem1 += sizeof(double);
                    mem2 += sizeof(double);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_LDOUBLE)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_ldouble_element(mem1, mem2, i, opts);

                    mem1 += sizeof(long double);
                    mem2 += sizeof(long double);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }

            break;

        case H5T_INTEGER:
            H5TOOLS_DEBUG("type_class:H5T_INTEGER");
            if (H5Tequal(opts->m_tid, H5T_NATIVE_SCHAR)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_schar_element(mem1, mem2, i, opts);
                    mem1 += sizeof(char);
                    mem2 += sizeof(char);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_UCHAR)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_uchar_element(mem1, mem2, i, opts);

                    mem1 += sizeof(unsigned char);
                    mem2 += sizeof(unsigned char);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_SHORT)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_short_element(mem1, mem2, i, opts);

                    mem1 += sizeof(short);
                    mem2 += sizeof(short);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_USHORT)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_ushort_element(mem1, mem2, i, opts);

                    mem1 += sizeof(unsigned short);
                    mem2 += sizeof(unsigned short);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_INT)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_int_element(mem1, mem2, i, opts);

                    mem1 += sizeof(int);
                    mem2 += sizeof(int);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_UINT)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_int_element(mem1, mem2, i, opts);

                    mem1 += sizeof(unsigned int);
                    mem2 += sizeof(unsigned int);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_LONG)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_long_element(mem1, mem2, i, opts);

                    mem1 += sizeof(long);
                    mem2 += sizeof(long);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_ULONG)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_ulong_element(mem1, mem2, i, opts);

                    mem1 += sizeof(unsigned long);
                    mem2 += sizeof(unsigned long);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_LLONG)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_llong_element(mem1, mem2, i, opts);

                    mem1 += sizeof(long long);
                    mem2 += sizeof(long long);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            else if (H5Tequal(opts->m_tid, H5T_NATIVE_ULLONG)) {
                for (i = 0; i < opts->hs_nelmts; i++) {
                    nfound += diff_ullong_element(mem1, mem2, i, opts);

                    mem1 += sizeof(unsigned long long);
                    mem2 += sizeof(unsigned long long);
                    if (opts->count_bool && nfound >= opts->count)
                        return nfound;
                } /* nelmts */
            }
            break;

        /*-------------------------------------------------------------------------
         * Other types than float and integer
         *-------------------------------------------------------------------------
         */
        case H5T_COMPOUND:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_ENUM:
        case H5T_ARRAY:
        case H5T_VLEN:
        case H5T_REFERENCE:
            H5TOOLS_DEBUG("type_class:OTHER");
            memset(&members, 0, sizeof(mcomp_t));
            get_member_types(opts->m_tid, &members);
            for (i = 0; i < opts->hs_nelmts; i++) {
                H5TOOLS_DEBUG("opts->pos[%" PRIuHSIZE "]:%" PRIuHSIZE " - nelmts:%" PRIuHSIZE, i,
                              opts->pos[i], opts->hs_nelmts);
                nfound += diff_datum(mem1 + i * size, mem2 + i * size, i, opts, container1_id, container2_id,
                                     &members);
                if (opts->count_bool && nfound >= opts->count)
                    break;
            } /* i */
            close_member_types(&members);
    } /* switch */
    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_datum
 *
 * Purpose: compare the values pointed to in _MEM1 and _MEM2 of type M_TYPE
 *
 * Return: number of differences found
 *
 * The comparison of the 2 buffers read from the files is made datum by datum.
 *
 * H5T_INTEGER and H5T_FLOAT
 *  Copy the buffer into a compatible local datum and do a numerical
 *  compare of this datum
 * H5T_COMPOUND
 *  Recursively call this function for each member
 * H5T_ARRAY
 *  Recursively call this function for each element
 * H5T_VLEN
 *  Recursively call this function for each element
 * H5T_STRING
 *  compare byte by byte in a cycle from 0 to type_size. this type_size is the
 *  value obtained by the get_size function but it is the string length for
 *  variable sized strings
 * H5T_OPAQUE
 *  compare byte by byte in a cycle from 0 to type_size
 * H5T_BITFIELD
 *  compare byte by byte in a cycle from 0 to type_size
 * H5T_ENUM
 *  for each pair of elements being compared, both bit patterns are converted to
 *  their corresponding enumeration constant and a string comparison is made
 * H5T_REFERENCE
 *  Dereference the object and compare the type (basic object type).
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_datum(void *_mem1, void *_mem2, hsize_t elemtno, diff_opt_t *opts, hid_t container1_id,
           hid_t container2_id, mcomp_t *members)
{
    unsigned char *mem1 = (unsigned char *)_mem1;
    unsigned char *mem2 = (unsigned char *)_mem2;
    size_t         u;
    size_t         type_size;
    H5T_sign_t     type_sign;
    H5T_class_t    type_class;
    size_t         offset;
    unsigned       nmembs;
    unsigned       j;
    size_t         size = 0;
    bool           iszero1;
    bool           iszero2;
    hsize_t        nfound    = 0; /* differences found */
    diff_err_t     ret_value = opts->err_stat;

    H5TOOLS_START_DEBUG("ph:%d elemtno:%" PRIuHSIZE " - errstat:%d", opts->print_header, elemtno,
                        opts->err_stat);

    type_size  = H5Tget_size(opts->m_tid);
    type_class = H5Tget_class(opts->m_tid);

    /* Fast comparison first for atomic type by memcmp().
     * It is OK not to list non-atomic type here because it will not be caught
     * by the condition, but it gives more clarity for code planning
     */
    if (type_class != H5T_REFERENCE && type_class != H5T_COMPOUND && type_class != H5T_STRING &&
        type_class != H5T_VLEN && memcmp(mem1, mem2, type_size) == 0)
        H5TOOLS_GOTO_DONE(opts->err_stat);

    switch (H5Tget_class(opts->m_tid)) {
        case H5T_NO_CLASS:
        case H5T_TIME:
        case H5T_NCLASSES:
        default:
            H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Invalid type class");
            break;

        /*-------------------------------------------------------------------------
         * H5T_COMPOUND
         *-------------------------------------------------------------------------
         */
        case H5T_COMPOUND:
            H5TOOLS_DEBUG("H5T_COMPOUND");
            {
                diff_opt_t cmpd_opts;

                cmpd_opts = *opts;
                nmembs    = members->n;

                for (j = 0; j < nmembs; j++) {
                    offset          = members->offsets[j];
                    cmpd_opts.m_tid = members->ids[j];

                    nfound += diff_datum(mem1 + offset, mem2 + offset, elemtno, &cmpd_opts, container1_id,
                                         container2_id, members->m[j]);
                }
                opts->err_stat     = opts->err_stat | cmpd_opts.err_stat;
                opts->print_header = cmpd_opts.print_header;
                opts->not_cmp      = cmpd_opts.not_cmp;
            }
            break;

        /*-------------------------------------------------------------------------
         * H5T_STRING
         *-------------------------------------------------------------------------
         */
        case H5T_STRING:
            H5TOOLS_DEBUG("H5T_STRING");
            {
                char     *s  = NULL;
                char     *sx = NULL;
                char     *s1 = NULL;
                char     *s2 = NULL;
                size_t    size1;
                size_t    size2;
                size_t    sizex;
                size_t    size_mtype = H5Tget_size(opts->m_tid);
                H5T_str_t pad        = H5Tget_strpad(opts->m_tid);

                /* if variable length string */
                if (H5Tis_variable_str(opts->m_tid)) {
                    H5TOOLS_DEBUG("H5T_STRING variable");
                    /* Get pointer to first string */
                    s1 = *(char **)((void *)mem1);
                    if (s1)
                        size1 = strlen(s1);
                    else
                        size1 = 0;

                    /* Get pointer to second string */
                    s2 = *(char **)((void *)mem2);
                    if (s2)
                        size2 = strlen(s2);
                    else
                        size2 = 0;
                }
                else if (H5T_STR_NULLTERM == pad) {
                    H5TOOLS_DEBUG("H5T_STRING null term");
                    /* Get pointer to first string */
                    s1 = (char *)mem1;
                    if (s1)
                        size1 = strlen(s1);
                    else
                        size1 = 0;

                    if (size1 > size_mtype)
                        size1 = size_mtype;

                    /* Get pointer to second string */
                    s2 = (char *)mem2;
                    if (s2)
                        size2 = strlen(s2);
                    else
                        size2 = 0;

                    if (size2 > size_mtype)
                        size2 = size_mtype;
                }
                else {
                    /* Get pointer to first string */
                    s1    = (char *)mem1;
                    size1 = size_mtype;

                    /* Get pointer to second string */
                    s2    = (char *)mem2;
                    size2 = size_mtype;
                }

                /*
                 * compare for shorter string
                 * TODO: this code need to be improved to handle the difference
                 *       of length of strings.
                 *       For now mimic the previous way.
                 */
                H5TOOLS_DEBUG("string size:%ld", size1);
                H5TOOLS_DEBUG("string size:%ld", size2);
                if (size1 != size2) {
                    H5TOOLS_DEBUG("string sizes difference");
                    nfound++;
                }
                if (size1 < size2) {
                    size  = size1;
                    s     = s1;
                    sizex = size2;
                    sx    = s2;
                }
                else {
                    size  = size2;
                    s     = s2;
                    sizex = size1;
                    sx    = s1;
                }

                /* check for NULL pointer for string */
                if (s != NULL) {
                    /* try fast compare first */
                    if ((memcmp(s, sx, size) == 0) && (size1 != size2)) {
                        for (u = size; u < sizex; u++)
                            character_compare(s + u, sx + u, elemtno, u, opts);
                    }
                    else
                        for (u = 0; u < size; u++)
                            nfound += character_compare(s + u, sx + u, elemtno, u, opts);
                } /* end check for NULL pointer for string */
            }
            break;

        /*-------------------------------------------------------------------------
         * H5T_BITFIELD
         *-------------------------------------------------------------------------
         */
        case H5T_BITFIELD:
            H5TOOLS_DEBUG("H5T_BITFIELD");
            /* byte-by-byte comparison */
            for (u = 0; u < type_size; u++)
                nfound += character_compare_opt(mem1 + u, mem2 + u, elemtno, opts);
            break;

        /*-------------------------------------------------------------------------
         * H5T_OPAQUE
         *-------------------------------------------------------------------------
         */
        case H5T_OPAQUE:
            H5TOOLS_DEBUG("H5T_OPAQUE");
            /* byte-by-byte comparison */
            for (u = 0; u < type_size; u++)
                nfound += character_compare_opt(mem1 + u, mem2 + u, elemtno, opts);
            break;

        /*-------------------------------------------------------------------------
         * H5T_ENUM
         *-------------------------------------------------------------------------
         */
        case H5T_ENUM:
            /* For enumeration types we compare the names instead of the
             * integer values.  For each pair of elements being
             * compared, we convert both bit patterns to their corresponding
             * enumeration constant and do a string comparison
             */
            H5TOOLS_DEBUG("H5T_ENUM");
            {
                char   enum_name1[1024];
                char   enum_name2[1024];
                herr_t err1;
                herr_t err2;

                /* disable error reporting */
                H5E_BEGIN_TRY
                {
                    /* If the enum value cannot be converted to a string
                     * it is set to an error string for later output.
                     */
                    err1 = H5Tenum_nameof(opts->m_tid, mem1, enum_name1, sizeof enum_name1);
                    if (err1 < 0)
                        snprintf(enum_name1, sizeof(enum_name1), "**INVALID VALUE**");

                    err2 = H5Tenum_nameof(opts->m_tid, mem2, enum_name2, sizeof enum_name2);
                    if (err2 < 0)
                        snprintf(enum_name2, sizeof(enum_name2), "**INVALID VALUE**");

                    /* One or more bad enum values */
                    if (err1 < 0 || err2 < 0) {
                        /* If the two values cannot be converted to a string
                         * (probably due to them being invalid enum values),
                         * don't attempt to convert them - just report errors.
                         */
                        nfound += 1;
                        opts->print_percentage = 0;
                        print_pos(opts, elemtno, 0);
                        if (print_data(opts)) {
                            parallel_print(S_FORMAT, enum_name1, enum_name2);
                        }
                    }
                    else {
                        /* Both enum values were valid */
                        if (strcmp(enum_name1, enum_name2) != 0) {
                            nfound                 = 1;
                            opts->print_percentage = 0;
                            print_pos(opts, elemtno, 0);
                            if (print_data(opts)) {
                                parallel_print(S_FORMAT, enum_name1, enum_name2);
                            }
                        }
                        else {
                            for (u = 0; u < type_size; u++)
                                nfound += character_compare_opt(mem1 + u, mem2 + u, elemtno, opts);
                        }
                    }
                    /* enable error reporting */
                }
                H5E_END_TRY
            }
            break;

        /*-------------------------------------------------------------------------
         * H5T_ARRAY
         *-------------------------------------------------------------------------
         */
        case H5T_ARRAY: {
            hsize_t    adims[H5S_MAX_RANK];
            int        ndims;
            diff_opt_t arr_opts;

            H5TOOLS_DEBUG("H5T_ARRAY ph=%d", opts->print_header);

            arr_opts = *opts;
            H5TOOLS_DEBUG("Check opts: hs_nelmts:%" PRIuHSIZE " to %" PRIuHSIZE " rank:%d to %d",
                          opts->hs_nelmts, arr_opts.hs_nelmts, opts->rank, arr_opts.rank);
            /* get the array's base datatype for each element */
            arr_opts.m_tid = H5Tget_super(opts->m_tid);
            size           = H5Tget_size(arr_opts.m_tid);
            ndims          = H5Tget_array_ndims(opts->m_tid);
            H5Tget_array_dims2(opts->m_tid, adims);
            assert(ndims >= 1 && ndims <= H5S_MAX_RANK);
            H5TOOLS_DEBUG("attr ph=%d", arr_opts.print_header);

            /* calculate the number of array elements */
            for (u = 0, arr_opts.hs_nelmts = 1; u < (unsigned)ndims; u++)
                arr_opts.hs_nelmts *= adims[u];
            for (u = 0; u < arr_opts.hs_nelmts; u++) {
                nfound += diff_datum(mem1 + u * size, mem2 + u * size, elemtno, &arr_opts, container1_id,
                                     container2_id, members);
            }
            opts->err_stat     = opts->err_stat | arr_opts.err_stat;
            opts->print_header = arr_opts.print_header;
            opts->not_cmp      = arr_opts.not_cmp;
            H5Tclose(arr_opts.m_tid);
        } break;

        /*-------------------------------------------------------------------------
         * H5T_REFERENCE
         *-------------------------------------------------------------------------
         */
        case H5T_REFERENCE:
            H5TOOLS_DEBUG("H5T_REFERENCE");
            iszero1 = all_zero(_mem1, H5Tget_size(opts->m_tid));
            iszero2 = all_zero(_mem2, H5Tget_size(opts->m_tid));
            if (iszero1 != iszero2) {
                nfound++;
                H5TOOLS_GOTO_DONE(opts->err_stat);
            }
            else if (!iszero1 && !iszero2) {
                hid_t      obj1_id = H5I_INVALID_HID;
                hid_t      obj2_id = H5I_INVALID_HID;
                diff_opt_t ref_opts;

                /*-------------------------------------------------------------------------
                 * H5T_STD_REF
                 * Reference
                 *-------------------------------------------------------------------------
                 */
                ref_opts             = *opts;
                ref_opts.obj_name[0] = NULL;
                ref_opts.obj_name[1] = NULL;
                if (H5Tequal(ref_opts.m_tid, H5T_STD_REF)) {
                    /* if (type_size == H5R_STD_REF_SIZE) */
                    hid_t      region1_id = H5I_INVALID_HID;
                    hid_t      region2_id = H5I_INVALID_HID;
                    H5R_ref_t *ref1_buf   = (H5R_ref_t *)_mem1;
                    H5R_ref_t *ref2_buf   = (H5R_ref_t *)_mem2;
                    H5O_type_t obj1_type  = -1; /* Object type */
                    H5O_type_t obj2_type  = -1; /* Object type */
                    H5R_type_t ref_type;        /* Reference type */

                    H5TOOLS_DEBUG("H5T_REFERENCE - H5T_STD_REF");
                    ref_type = H5Rget_type(ref1_buf);
                    switch (ref_type) {
                        case H5R_OBJECT1:
                            H5TOOLS_DEBUG("ref_type is H5R_OBJECT1");
                            if (H5Rget_obj_type3(ref1_buf, H5P_DEFAULT, &obj1_type) >= 0) {
                                if (H5Rget_obj_type3(ref2_buf, H5P_DEFAULT, &obj2_type) >= 0) {
                                    /* check object type */
                                    if (obj1_type == obj2_type) {
                                        switch (obj1_type) {
                                            case H5O_TYPE_DATASET:
                                                if ((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT,
                                                                              H5P_DEFAULT)) >= 0) {
                                                    if ((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT,
                                                                                  H5P_DEFAULT)) >= 0) {
                                                        nfound = diff_datasetid(obj1_id, obj2_id,
                                                                                opts->obj_name[0],
                                                                                opts->obj_name[1], &ref_opts);
                                                        if (H5Dclose(obj2_id) < 0) {
                                                            ref_opts.err_stat = H5DIFF_ERR;
                                                            H5TOOLS_INFO("H5Dclose H5R_OBJECT1 failed");
                                                        }
                                                    }
                                                    else {
                                                        ref_opts.err_stat = H5DIFF_ERR;
                                                        H5TOOLS_INFO("H5Ropen_object object 2 failed");
                                                    }
                                                    if (H5Dclose(obj1_id) < 0) {
                                                        ref_opts.err_stat = H5DIFF_ERR;
                                                        H5TOOLS_INFO("H5Dclose H5R_OBJECT1 failed");
                                                    }
                                                }
                                                else {
                                                    ref_opts.err_stat = H5DIFF_ERR;
                                                    H5TOOLS_INFO("H5Ropen_object object 1 failed");
                                                }
                                                break;

                                            case H5O_TYPE_GROUP:
                                            case H5O_TYPE_NAMED_DATATYPE:
                                            case H5O_TYPE_MAP:
                                            case H5O_TYPE_UNKNOWN:
                                            case H5O_TYPE_NTYPES:
                                            default:
                                                if (ref_opts.mode_verbose)
                                                    parallel_print("Warning: Comparison not possible of "
                                                                   "object types referenced: <%s> and <%s>\n",
                                                                   opts->obj_name[0], opts->obj_name[1]);
                                                ref_opts.not_cmp = 1;
                                                break;
                                        } /* end switch */
                                    }
                                    else {
                                        parallel_print("Different object types referenced: <%s> and <%s>",
                                                       opts->obj_name[0], opts->obj_name[1]);
                                        ref_opts.not_cmp  = 1;
                                        ref_opts.err_stat = H5DIFF_ERR;
                                    }
                                }
                                else {
                                    ref_opts.err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Rget_obj_type3 object 2 failed");
                                }
                            }
                            else {
                                ref_opts.err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Rget_obj_type3 object 1 failed");
                            }
                            break;
                        case H5R_DATASET_REGION1:
                            H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION1");
                            if ((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if ((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                    if ((region1_id = H5Ropen_region(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >=
                                        0) {
                                        if ((region2_id =
                                                 H5Ropen_region(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                            nfound = diff_region(obj1_id, obj2_id, region1_id, region2_id,
                                                                 &ref_opts);
                                            if (H5Sclose(region2_id) < 0)
                                                H5TOOLS_INFO("H5Sclose H5R_DATASET_REGION1 failed");
                                        }
                                        if (H5Sclose(region1_id) < 0)
                                            H5TOOLS_INFO("H5Sclose H5R_DATASET_REGION1 failed");
                                    }
                                    if (H5Dclose(obj2_id) < 0)
                                        H5TOOLS_INFO("H5Oclose H5R_DATASET_REGION1 failed");
                                }
                                else {
                                    H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION1 failed");
                                }
                                if (H5Dclose(obj1_id) < 0)
                                    H5TOOLS_INFO("H5Oclose H5R_DATASET_REGION1 failed");
                            }
                            else {
                                H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION1 failed");
                            }
                            break;
                        case H5R_OBJECT2:
                            H5TOOLS_DEBUG("ref_type is H5R_OBJECT2");
                            if (H5Rget_obj_type3(ref1_buf, H5P_DEFAULT, &obj1_type) >= 0) {
                                if (H5Rget_obj_type3(ref2_buf, H5P_DEFAULT, &obj2_type) >= 0) {
                                    /* check object type */
                                    if (obj1_type == obj2_type) {
                                        if ((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >=
                                            0) {
                                            if ((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT,
                                                                          H5P_DEFAULT)) >= 0) {
                                                switch (obj1_type) {
                                                    case H5O_TYPE_DATASET:
                                                        H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : DATASET");
                                                        nfound = diff_datasetid(obj1_id, obj2_id,
                                                                                opts->obj_name[0],
                                                                                opts->obj_name[1], &ref_opts);
                                                        break;

                                                    case H5O_TYPE_GROUP:
                                                        H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : GROUP");
                                                        if (ref_opts.mode_verbose)
                                                            parallel_print(
                                                                "Warning: Comparison not possible of group "
                                                                "object types referenced: <%s> and <%s>\n",
                                                                opts->obj_name[0], opts->obj_name[1]);
                                                        ref_opts.not_cmp = 1;
                                                        break;

                                                    case H5O_TYPE_NAMED_DATATYPE:
                                                        H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : NAMED");
                                                        if (ref_opts.mode_verbose)
                                                            parallel_print("Warning: Comparison not possible "
                                                                           "of named datatypes object types "
                                                                           "referenced: <%s> and <%s>\n",
                                                                           opts->obj_name[0],
                                                                           opts->obj_name[1]);
                                                        ref_opts.not_cmp = 1;
                                                        break;

                                                    case H5O_TYPE_MAP:
                                                    case H5O_TYPE_UNKNOWN:
                                                    case H5O_TYPE_NTYPES:
                                                    default:
                                                        if (ref_opts.mode_verbose)
                                                            parallel_print(
                                                                "Warning: Comparison not possible of object "
                                                                "types referenced: <%s> and <%s>\n",
                                                                opts->obj_name[0], opts->obj_name[1]);
                                                        ref_opts.not_cmp = 1;
                                                        break;
                                                } /* end switch */
                                                if (H5Oclose(obj2_id) < 0) {
                                                    ref_opts.err_stat = H5DIFF_ERR;
                                                    H5TOOLS_INFO("H5Oclose H5R_OBJECT2 failed");
                                                }
                                            }
                                            else {
                                                ref_opts.err_stat = H5DIFF_ERR;
                                                H5TOOLS_INFO("H5Ropen_object object 2 failed");
                                            }
                                            if (H5Oclose(obj1_id) < 0) {
                                                ref_opts.err_stat = H5DIFF_ERR;
                                                H5TOOLS_INFO("H5Oclose H5R_OBJECT2 failed");
                                            }
                                        }
                                        else {
                                            ref_opts.err_stat = H5DIFF_ERR;
                                            H5TOOLS_INFO("H5Ropen_object object 1 failed");
                                        }
                                    }
                                    else {
                                        parallel_print("Different object types referenced: <%s> and <%s>",
                                                       opts->obj_name[0], opts->obj_name[1]);
                                        ref_opts.not_cmp  = 1;
                                        ref_opts.err_stat = H5DIFF_ERR;
                                    }
                                }
                                else {
                                    ref_opts.err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Rget_obj_type3 object 2 failed");
                                }
                            }
                            else {
                                ref_opts.err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Rget_obj_type3 object 1 failed");
                            }
                            break;
                        case H5R_DATASET_REGION2:
                            H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION2");

                            /* if (obj_id < 0) - could mean that no reference was written do not throw failure
                             */
                            if ((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                                H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION2 object 1 failed");
                            }
                            else {
                                if ((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                    H5TOOLS_DEBUG("open_region - H5R_DATASET_REGION2");
                                    if ((region1_id = H5Ropen_region(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >=
                                        0) {
                                        if (h5tools_is_zero(ref1_buf, H5Tget_size(H5T_STD_REF))) {
                                            H5TOOLS_DEBUG("NULL H5R_DATASET_REGION2");
                                        }
                                        else {
                                            if ((region2_id = H5Ropen_region(ref2_buf, H5P_DEFAULT,
                                                                             H5P_DEFAULT)) >= 0) {
                                                if (h5tools_is_zero(ref2_buf, H5Tget_size(H5T_STD_REF))) {
                                                    H5TOOLS_DEBUG("NULL H5R_DATASET_REGION2");
                                                }
                                                else {
                                                    nfound = diff_region(obj1_id, obj2_id, region1_id,
                                                                         region2_id, &ref_opts);
                                                }
                                                if (H5Sclose(region2_id) < 0)
                                                    H5TOOLS_INFO("H5Sclose H5R_DATASET_REGION2 failed");
                                            }
                                            else
                                                H5TOOLS_INFO("H5Ropen_region H5R_DATASET_REGION2 failed");
                                        } /* end else to if (h5tools_is_zero(... */
                                        if (H5Sclose(region1_id) < 0)
                                            H5TOOLS_INFO("H5Sclose H5R_DATASET_REGION2 failed");
                                    }
                                    else
                                        H5TOOLS_ERROR(H5DIFF_ERR,
                                                      "H5Ropen_region H5R_DATASET_REGION2 failed");
                                    if (H5Dclose(obj2_id) < 0) {
                                        ref_opts.err_stat = H5DIFF_ERR;
                                        H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION2 failed");
                                    }
                                }
                                else {
                                    H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION2 object 2 failed");
                                }
                                if (H5Dclose(obj1_id) < 0) {
                                    ref_opts.err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION2 failed");
                                }
                            }
                            break;
                        case H5R_ATTR: {
                            char name1[ATTR_NAME_MAX];
                            char name2[ATTR_NAME_MAX];

                            H5TOOLS_DEBUG("ref_type is H5R_ATTR");
                            if ((obj1_id = H5Ropen_attr(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if ((obj2_id = H5Ropen_attr(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                    /* get name */
                                    if (H5Aget_name(obj1_id, (size_t)ATTR_NAME_MAX, name1) >= 0) {
                                        /* get name */
                                        if (H5Aget_name(obj2_id, (size_t)ATTR_NAME_MAX, name2) >= 0) {
                                            H5TOOLS_DEBUG("H5R_ATTR diff_attr_data - name1=%s, name2=%s",
                                                          name1, name2);
                                            nfound = diff_attr_data(obj1_id, obj2_id, name1, name2,
                                                                    opts->obj_name[0], opts->obj_name[1],
                                                                    &ref_opts);
                                        }
                                        else {
                                            ref_opts.err_stat = H5DIFF_ERR;
                                            H5TOOLS_INFO("H5Aget_name second attribute failed");
                                        }
                                    }
                                    else {
                                        ref_opts.err_stat = H5DIFF_ERR;
                                        H5TOOLS_INFO("H5Aget_name first attribute failed");
                                    }

                                    if (H5Aclose(obj2_id) < 0) {
                                        ref_opts.err_stat = H5DIFF_ERR;
                                        H5TOOLS_INFO("H5Aclose H5R_ATTR failed");
                                    }
                                }
                                else {
                                    parallel_print("Warning: Cannot open referenced attribute2\n");
                                    H5TOOLS_INFO("H5Ropen_attr object 2 failed");
                                }
                                if (H5Aclose(obj1_id) < 0) {
                                    H5TOOLS_INFO("H5Aclose H5R_ATTR failed");
                                }
                            }
                            else {
                                parallel_print("Warning: Cannot open referenced attribute1\n");
                                H5TOOLS_INFO("H5Ropen_attr object 1 failed");
                            }
                        } break;
                        case H5R_BADTYPE:
                        case H5R_MAXTYPE:
                        default:
                            break;
                    } /* end switch */
                    if (H5Rdestroy(ref2_buf) < 0)
                        H5TOOLS_INFO("H5Rdestroy H5R_OBJECT1 failed");
                    if (H5Rdestroy(ref1_buf) < 0)
                        H5TOOLS_INFO("H5Rdestroy H5R_OBJECT1 failed");
                    H5TOOLS_DEBUG("H5T_REFERENCE - H5T_STD_REF complete nfound:%" PRIuHSIZE " - errstat:%d",
                                  nfound, ref_opts.err_stat);
                }
                /*-------------------------------------------------------------------------
                 * H5T_STD_REF_DSETREG
                 * Dataset region reference
                 *-------------------------------------------------------------------------
                 */
                else if (H5Tequal(ref_opts.m_tid, H5T_STD_REF_DSETREG)) {
                    /* if (type_size == H5R_DSET_REG_REF_BUF_SIZE) */
                    H5TOOLS_DEBUG("H5T_STD_REF_DSETREG");
                } /*dataset reference*/

                /*-------------------------------------------------------------------------
                 * H5T_STD_REF_OBJ
                 * Object references. get the type and OID of the referenced object
                 *-------------------------------------------------------------------------
                 */
                else if (H5Tequal(ref_opts.m_tid, H5T_STD_REF_OBJ)) {
                    /* if (type_size == H5R_OBJ_REF_BUF_SIZE) */
                    H5TOOLS_DEBUG("H5T_STD_REF_OBJ");
                } /*object reference*/
                opts->print_header = ref_opts.print_header;
                opts->not_cmp      = ref_opts.not_cmp;
                opts->err_stat     = ref_opts.err_stat | ret_value;
            } /*is zero*/
            H5TOOLS_DEBUG("H5T_REFERENCE complete");
            break;

        /*-------------------------------------------------------------------------
         * H5T_VLEN
         *-------------------------------------------------------------------------
         */
        case H5T_VLEN: {
            diff_opt_t vl_opts;

            H5TOOLS_DEBUG("H5T_VLEN");

            vl_opts = *opts;
            /* get the VL sequences's base datatype for each element */
            vl_opts.m_tid = H5Tget_super(opts->m_tid);
            size          = H5Tget_size(vl_opts.m_tid);

            /* get the number of sequence elements */
            vl_opts.hs_nelmts = ((hvl_t *)((void *)mem1))->len;

            for (j = 0; j < vl_opts.hs_nelmts; j++)
                nfound += diff_datum(((char *)(((hvl_t *)((void *)mem1))->p)) + j * size,
                                     ((char *)(((hvl_t *)((void *)mem2))->p)) + j * size,
                                     elemtno, /* Extra (void *) cast to quiet "cast to create alignment"
                                                 warning - 2019/07/05, QAK */
                                     &vl_opts, container1_id, container2_id, members);
            opts->print_header = vl_opts.print_header;
            opts->not_cmp      = vl_opts.not_cmp;
            opts->err_stat     = opts->err_stat | vl_opts.err_stat;

            H5Tclose(vl_opts.m_tid);
        } break;

        /*-------------------------------------------------------------------------
         * H5T_INTEGER
         *-------------------------------------------------------------------------
         */
        case H5T_INTEGER:
            H5TOOLS_DEBUG("H5T_INTEGER");
            type_sign = H5Tget_sign(opts->m_tid);
            /*-------------------------------------------------------------------------
             * H5T_NATIVE_SCHAR
             *-------------------------------------------------------------------------
             */
            if (type_size == 1 && type_sign != H5T_SGN_NONE) {
                if (type_size != sizeof(char))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not char size");
                nfound += diff_schar_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_SCHAR*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_UCHAR
             *-------------------------------------------------------------------------
             */
            else if (type_size == 1 && type_sign == H5T_SGN_NONE) {
                if (type_size != sizeof(unsigned char))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned char size");
                nfound += diff_uchar_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_UCHAR*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_SHORT
             *-------------------------------------------------------------------------
             */
            else if (type_size == 2 && type_sign != H5T_SGN_NONE) {
                if (type_size != sizeof(short))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not short size");
                nfound += diff_short_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_SHORT*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_USHORT
             *-------------------------------------------------------------------------
             */
            else if (type_size == 2 && type_sign == H5T_SGN_NONE) {
                if (type_size != sizeof(unsigned short))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned short size");
                nfound += diff_ushort_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_USHORT*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_INT
             *-------------------------------------------------------------------------
             */
            else if (type_size == 4 && type_sign != H5T_SGN_NONE) {
                if (type_size != sizeof(int))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not int size");
                nfound += diff_int_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_INT*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_UINT
             *-------------------------------------------------------------------------
             */
            else if (type_size == 4 && type_sign == H5T_SGN_NONE) {
                if (type_size != sizeof(unsigned int))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned int size");
                nfound += diff_uint_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_UINT*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_LONG
             *-------------------------------------------------------------------------
             */
            else if (type_size == 8 && type_sign != H5T_SGN_NONE) {
                if (type_size != sizeof(long))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not long size");
                nfound += diff_long_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_LONG*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_ULONG
             *-------------------------------------------------------------------------
             */
            else if (type_size == 8 && type_sign == H5T_SGN_NONE) {
                if (type_size != sizeof(unsigned long))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned long size");
                nfound += diff_ulong_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_ULONG*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_LLONG
             *-------------------------------------------------------------------------
             */
            else if (type_size == 16 && type_sign != H5T_SGN_NONE) {
                if (type_size != sizeof(long long))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not long long size");
                nfound += diff_llong_element(mem1, mem2, elemtno, opts);
            } /*H5T_NATIVE_LLONG*/

            /*-------------------------------------------------------------------------
             * H5T_NATIVE_ULLONG
             *-------------------------------------------------------------------------
             */
            else if (type_size == 16 && type_sign == H5T_SGN_NONE) {
                if (type_size != sizeof(unsigned long long))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned long long size");
                nfound += diff_ullong_element(mem1, mem2, elemtno, opts);
            }      /*H5T_NATIVE_ULLONG*/
            break; /* H5T_INTEGER class */

        /*-------------------------------------------------------------------------
         * H5T_FLOAT
         *-------------------------------------------------------------------------
         */
        case H5T_FLOAT:
            H5TOOLS_DEBUG("H5T_FLOAT");
#ifdef H5_HAVE__FLOAT16
            /*-------------------------------------------------------------------------
             * H5T_NATIVE_FLOAT16
             *-------------------------------------------------------------------------
             */
            if (type_size == H5_SIZEOF__FLOAT16) {
                if (type_size != sizeof(H5__Float16))
                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not _Float16 size");
                nfound += diff_float16_element(mem1, mem2, elemtno, opts);
            }
            else
#endif
                /*-------------------------------------------------------------------------
                 * H5T_NATIVE_FLOAT
                 *-------------------------------------------------------------------------
                 */
                if (type_size == 4) {
                    if (type_size != sizeof(float))
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not float size");
                    nfound += diff_float_element(mem1, mem2, elemtno, opts);
                }
                /*-------------------------------------------------------------------------
                 * H5T_NATIVE_DOUBLE
                 *-------------------------------------------------------------------------
                 */
                else if (type_size == 8) {
                    if (type_size != sizeof(double))
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not double size");
                    nfound += diff_double_element(mem1, mem2, elemtno, opts);
                }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE

                /*-------------------------------------------------------------------------
                 * H5T_NATIVE_LDOUBLE
                 *-------------------------------------------------------------------------
                 */
                else if (type_size == H5_SIZEOF_LONG_DOUBLE) {
                    if (type_size != sizeof(long double)) {
                        H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not long double size");
                    }
                    nfound += diff_ldouble_element(mem1, mem2, elemtno, opts);
                } /*H5T_NATIVE_LDOUBLE*/
#endif            /* H5_SIZEOF_LONG_DOUBLE */

            break; /* H5T_FLOAT class */

    } /* switch */

done:
    opts->err_stat = opts->err_stat | ret_value;

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: all_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return: true if all bytes are zero; false otherwise
 *-------------------------------------------------------------------------
 */

static bool
all_zero(const void *_mem, size_t size)
{
    const unsigned char *mem = (const unsigned char *)_mem;

    while (size-- > 0)
        if (mem[size])
            return false;

    return true;
}

/*-------------------------------------------------------------------------
 * Function: print_region_block
 *
 * Purpose: print start coordinates and opposite corner of a region block
 *
 * Return: void
 *-------------------------------------------------------------------------
 */

static void
print_region_block(int i, hsize_t *ptdata, int ndims)
{
    int j;

    parallel_print("        ");
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : "   (", (unsigned long)ptdata[i * 2 * ndims + j]);
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : ")-(", (unsigned long)ptdata[i * 2 * ndims + j + ndims]);
    parallel_print(")");
}

/*-------------------------------------------------------------------------
 * Function: print_points
 *
 * Purpose: print points of a region reference
 *
 * Return: void
 *-------------------------------------------------------------------------
 */

static void
print_points(int i, hsize_t *ptdata, int ndims)
{
    int j;

    parallel_print("              ");
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : "(", (unsigned long)(ptdata[i * ndims + j]));
    parallel_print(")");
}

/*-------------------------------------------------------------------------
 * Function: diff_region
 *
 * Purpose: diff a dataspace region
 *
 * Return: number of differences
 *-------------------------------------------------------------------------
 */

static hsize_t
diff_region(hid_t obj1_id, hid_t obj2_id, hid_t region1_id, hid_t region2_id, diff_opt_t *opts)

{
    hssize_t nblocks1, npoints1;
    hssize_t nblocks2, npoints2;
    hsize_t  alloc_size;
    hsize_t *ptdata1 = NULL;
    hsize_t *ptdata2 = NULL;
    int      ndims1;
    int      ndims2;
    int      i, j;
    hsize_t  nfound_b  = 0; /* block differences found */
    hsize_t  nfound_p  = 0; /* point differences found */
    hsize_t  ret_value = 0;

    H5TOOLS_START_DEBUG(" ");

    ndims1 = H5Sget_simple_extent_ndims(region1_id);
    ndims2 = H5Sget_simple_extent_ndims(region2_id);

    /*
     * These two functions fail if the region does not have blocks or points,
     * respectively. They do not currently know how to translate from one to
     * the other.
     */
    H5E_BEGIN_TRY
    {
        nblocks1 = H5Sget_select_hyper_nblocks(region1_id);
        nblocks2 = H5Sget_select_hyper_nblocks(region2_id);

        npoints1 = H5Sget_select_elem_npoints(region1_id);
        npoints2 = H5Sget_select_elem_npoints(region2_id);
    }
    H5E_END_TRY
    H5TOOLS_DEBUG("blocks: 1=%" PRIdHSIZE "-2=%" PRIdHSIZE, nblocks1, nblocks2);
    H5TOOLS_DEBUG("points: 1=%" PRIdHSIZE "-2=%" PRIdHSIZE, npoints1, npoints2);

    if (nblocks1 != nblocks2 || npoints1 != npoints2 || ndims1 != ndims2) {
        opts->not_cmp = 1;
        H5TOOLS_GOTO_DONE(0);
    }

    /*-------------------------------------------------------------------------
     * compare block information
     *-------------------------------------------------------------------------
     */
    if (nblocks1 > 0) {
        H5TOOLS_DEBUG("region compare blocks");
        assert(ndims1 > 0);
        alloc_size = (hsize_t)nblocks1 * (unsigned)ndims1 * 2 * sizeof(ptdata1[0]);
        assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

        if ((ptdata1 = (hsize_t *)malloc((size_t)alloc_size)) == NULL) {
            opts->err_stat = H5DIFF_ERR;
            H5TOOLS_INFO("Buffer allocation failed");
        }
        else {
            H5_CHECK_OVERFLOW(nblocks1, hssize_t, hsize_t);
            H5Sget_select_hyper_blocklist(region1_id, (hsize_t)0, (hsize_t)nblocks1, ptdata1);

            if ((ptdata2 = (hsize_t *)malloc((size_t)alloc_size)) == NULL) {
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_INFO("Buffer allocation failed");
            }
            else {
                H5_CHECK_OVERFLOW(nblocks2, hssize_t, hsize_t);
                H5Sget_select_hyper_blocklist(region2_id, (hsize_t)0, (hsize_t)nblocks2, ptdata2);

                for (i = 0; i < nblocks1; i++) {
                    /* start coordinates and opposite corner */
                    for (j = 0; j < ndims1; j++) {
                        hsize_t start1, start2, end1, end2;

                        start1 = ptdata1[i * 2 * ndims1 + j];
                        start2 = ptdata2[i * 2 * ndims1 + j];
                        end1   = ptdata1[i * 2 * ndims1 + j + ndims1];
                        end2   = ptdata2[i * 2 * ndims1 + j + ndims1];
                        if (start1 != start2 || end1 != end2)
                            nfound_b++;
                    }
                }

                /* print differences if found */
                if (nfound_b && opts->mode_verbose) {
                    H5O_info2_t oi1, oi2;
                    char       *obj1_str = NULL, *obj2_str = NULL;

                    H5Oget_info3(obj1_id, &oi1, H5O_INFO_BASIC);
                    H5Oget_info3(obj2_id, &oi2, H5O_INFO_BASIC);

                    /* Convert object tokens into printable output */
                    H5Otoken_to_str(obj1_id, &oi1.token, &obj1_str);
                    H5Otoken_to_str(obj2_id, &oi2.token, &obj2_str);

                    parallel_print("Referenced dataset      %s            %s\n", obj1_str, obj2_str);
                    parallel_print("------------------------------------------------------------\n");

                    H5free_memory(obj1_str);
                    H5free_memory(obj2_str);

                    parallel_print("Region blocks\n");
                    for (i = 0; i < nblocks1; i++) {
                        parallel_print("block #%d", i);
                        print_region_block(i, ptdata1, ndims1);
                        print_region_block(i, ptdata2, ndims1);
                        parallel_print("\n");
                    }
                }
                free(ptdata2);
            } /* else ptdata2 */

            free(ptdata1);
        } /* else ptdata1 */
    }

    /*-------------------------------------------------------------------------
     * compare point information
     *-------------------------------------------------------------------------
     */
    if (npoints1 > 0) {
        H5TOOLS_DEBUG("region compare points");
        alloc_size = (hsize_t)npoints1 * (unsigned)ndims1 * sizeof(ptdata1[0]);
        assert(alloc_size == (hsize_t)((size_t)alloc_size)); /*check for overflow*/

        if ((ptdata1 = (hsize_t *)malloc((size_t)alloc_size)) == NULL) {
            opts->err_stat = H5DIFF_ERR;
            H5TOOLS_INFO("Buffer allocation failed");
        }
        else {
            H5_CHECK_OVERFLOW(npoints1, hssize_t, hsize_t);
            H5Sget_select_elem_pointlist(region1_id, (hsize_t)0, (hsize_t)npoints1, ptdata1);

            if ((ptdata2 = (hsize_t *)malloc((size_t)alloc_size)) == NULL) {
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_INFO("Buffer allocation failed");
            }
            else {
                H5_CHECK_OVERFLOW(npoints1, hssize_t, hsize_t);
                H5Sget_select_elem_pointlist(region2_id, (hsize_t)0, (hsize_t)npoints2, ptdata2);

                for (i = 0; i < npoints1; i++) {
                    hsize_t pt1, pt2;

                    for (j = 0; j < ndims1; j++) {
                        pt1 = ptdata1[i * ndims1 + j];
                        pt2 = ptdata2[i * ndims1 + j];
                        if (pt1 != pt2)
                            nfound_p++;
                    }
                }

                if (nfound_p && opts->mode_verbose) {
                    parallel_print("Region points\n");
                    for (i = 0; i < npoints1; i++) {
                        hsize_t pt1, pt2;
                        int     diff_data = 0;

                        for (j = 0; j < ndims1; j++) {
                            pt1 = ptdata1[i * ndims1 + j];
                            pt2 = ptdata2[i * ndims1 + j];
                            if (pt1 != pt2) {
                                diff_data = 1;
                                break;
                            }
                        }
                        if (diff_data) {
                            parallel_print("point #%d", i);
                            print_points(i, ptdata1, ndims1);
                            print_points(i, ptdata2, ndims1);
                            parallel_print("\n");
                        }
                    }
                }
                free(ptdata2);
            } /* else ptdata2 */

#if defined(H5DIFF_DEBUG)
            for (i = 0; i < npoints1; i++) {
                parallel_print("%sPt%d: ", i ? "," : "", i);

                for (j = 0; j < ndims1; j++)
                    parallel_print("%s%" PRIuHSIZE, j ? "," : "(", ptdata1[i * ndims1 + j]);

                parallel_print(")");
            }
            parallel_print("\n");
#endif

            free(ptdata1);
        } /* else ptdata1 */
    }

    nfound_b = nfound_b / (unsigned)ndims1;
    nfound_p = nfound_p / (unsigned)ndims1;

    ret_value = nfound_p + nfound_b;

done:
    H5TOOLS_ENDDEBUG(" with diffs:%" PRIuHSIZE, ret_value);
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function: character_compare
 *
 * Purpose:  do a byte-by-byte comparison and print in char format
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */

static hsize_t
character_compare(char *mem1, char *mem2, hsize_t elemtno, size_t u, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* differences found */
    char    temp1_uchar;
    char    temp2_uchar;

    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
    H5TOOLS_START_DEBUG(" %d=%d", temp1_uchar, temp2_uchar);

    if (temp1_uchar != temp2_uchar) {
        if (print_data(opts)) {
            opts->print_percentage = 0;
            opts->print_dims       = 1;
            print_pos(opts, elemtno, u);
            parallel_print("  ");
            h5diff_print_char(temp1_uchar);
            parallel_print("            ");
            h5diff_print_char(temp2_uchar);
            parallel_print("\n");
        }
        nfound++;
    }
    H5TOOLS_ENDDEBUG(": %" PRIuHSIZE, nfound);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: character_compare_opt
 *
 * Purpose:  do a byte-by-byte comparison and print in numerical format
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */

static hsize_t
character_compare_opt(unsigned char *mem1, unsigned char *mem2, hsize_t elemtno, diff_opt_t *opts)
{
    hsize_t       nfound = 0; /* differences found */
    unsigned char temp1_uchar;
    unsigned char temp2_uchar;
    bool          both_zero = false;
    double        per;

    /* both_zero is set in the PER_UNSIGN macro but not used in this function */
    (void)both_zero;

    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
    H5TOOLS_START_DEBUG(" %d=%d", temp1_uchar, temp2_uchar);

    /* -d and !-p */

    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elemtno, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);
        if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elemtno, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);
        if (per > opts->percent && PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elemtno, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    else if (temp1_uchar != temp2_uchar) {
        opts->print_percentage = 0;
        print_pos(opts, elemtno, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(": %" PRIuHSIZE " zero:%d", nfound, both_zero);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_float_element
 *
 * Purpose:  diff a single H5T_NATIVE_FLOAT type
 *
 * Return:   number of differences found
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_float_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    float   temp1_float;
    float   temp2_float;
    double  per;
    bool    both_zero = false;
    bool    isnan1    = false;
    bool    isnan2    = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_float, mem1, sizeof(float));
    memcpy(&temp2_float, mem2, sizeof(float));

    /* logic for detecting NaNs is different with opts -d, -p and no opts */

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */
    if (opts->delta_bool && !opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float);
            isnan2 = isnan(temp2_float);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            if ((double)ABS(temp1_float - temp2_float) > opts->delta) {
                opts->print_percentage = 0;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT, (double)temp1_float, (double)temp2_float,
                                   (double)ABS(temp1_float - temp2_float));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float, (double)temp2_float,
                               (double)ABS(temp1_float - temp2_float));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float);
            isnan2 = isnan(temp2_float);
        }
        /* both not NaN, do the comparison */
        if ((!isnan1 && !isnan2)) {
            PER(temp1_float, temp2_float);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, (double)temp1_float, (double)temp2_float,
                                   (double)ABS(temp1_float - temp2_float));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, (double)temp1_float, (double)temp2_float,
                                   (double)ABS(temp1_float - temp2_float),
                                   (double)ABS(1 - temp2_float / temp1_float));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float, (double)temp2_float,
                               (double)ABS(temp1_float - temp2_float));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float);
            isnan2 = isnan(temp2_float);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_float, temp2_float);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, (double)temp1_float, (double)temp2_float,
                                   (double)ABS(temp1_float - temp2_float));
                }
                nfound++;
            }
            else if (per > opts->percent && (double)ABS(temp1_float - temp2_float) > opts->delta) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, (double)temp1_float, (double)temp2_float,
                                   (double)ABS(temp1_float - temp2_float),
                                   (double)ABS(1 - temp2_float / temp1_float));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float, (double)temp2_float,
                               (double)ABS(temp1_float - temp2_float));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        if (equal_float(temp1_float, temp2_float, opts) == false) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float, (double)temp2_float,
                               (double)ABS(temp1_float - temp2_float));
            }
            nfound++;
        }
    }

    H5TOOLS_ENDDEBUG(": %" PRIuHSIZE " zero:%d", nfound, both_zero);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_double_element
 *
 * Purpose:  diff a single H5T_NATIVE_DOUBLE type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_double_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    double  temp1_double;
    double  temp2_double;
    double  per;
    bool    both_zero = false;
    bool    isnan1    = false;
    bool    isnan2    = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_double, mem1, sizeof(double));
    memcpy(&temp2_double, mem2, sizeof(double));

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */
    if (opts->delta_bool && !opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            if (ABS(temp1_double - temp2_double) > opts->delta) {
                opts->print_percentage = 0;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }

    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }
        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_double, temp2_double);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double,
                                   ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double),
                                   ABS(1 - temp2_double / temp1_double));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_double, temp2_double);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double,
                                   ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            else if (per > opts->percent && ABS(temp1_double - temp2_double) > opts->delta) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double),
                                   ABS(1 - temp2_double / temp1_double));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        if (equal_double(temp1_double, temp2_double, opts) == false) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ldouble_element
 *
 * Purpose:  diff a single H5T_NATIVE_LDOUBLE type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */

static hsize_t
diff_ldouble_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t     nfound = 0; /* number of differences found */
    long double temp1_double;
    long double temp2_double;
    double      per;
    bool        both_zero = false;
    bool        isnan1    = false;
    bool        isnan2    = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_double, mem1, sizeof(long double));
    memcpy(&temp2_double, mem2, sizeof(long double));

    /* logic for detecting NaNs is different with options -d, -p and no options */

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */
    if (opts->delta_bool && !opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            if ((double)ABS(temp1_double - temp2_double) > opts->delta) {
                opts->print_percentage = 0;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
        } /* NaN */
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_double, temp2_double);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(LD_FORMAT_P_NOTCOMP, temp1_double, temp2_double,
                                   ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(LD_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double),
                                   ABS(1 - temp2_double / temp1_double));
                }
                nfound++;
            }
        } /* NaN */
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_double);
            isnan2 = isnan(temp2_double);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_double, temp2_double);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(LD_FORMAT_P_NOTCOMP, temp1_double, temp2_double,
                                   ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            else if (per > opts->percent && (double)ABS(temp1_double - temp2_double) > opts->delta) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(LD_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double),
                                   ABS(1 - temp2_double / temp1_double));
                }
                nfound++;
            }
        } /* NaN */
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else if (equal_ldouble(temp1_double, temp2_double, opts) == false) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

#ifdef H5_HAVE__FLOAT16
/*-------------------------------------------------------------------------
 * Function: diff_float16_element
 *
 * Purpose:  diff a single H5T_NATIVE_FLOAT16 type
 *
 * Return:   number of differences found
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_float16_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t     nfound = 0; /* number of differences found */
    H5__Float16 temp1_float16;
    H5__Float16 temp2_float16;
    double      per;
    bool        both_zero = false;
    bool        isnan1    = false;
    bool        isnan2    = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_float16, mem1, sizeof(H5__Float16));
    memcpy(&temp2_float16, mem2, sizeof(H5__Float16));

    /* logic for detecting NaNs is different with opts -d, -p and no opts */

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */
    if (opts->delta_bool && !opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float16);
            isnan2 = isnan(temp2_float16);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            if ((double)ABS(temp1_float16 - temp2_float16) > opts->delta) {
                opts->print_percentage = 0;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT, (double)temp1_float16, (double)temp2_float16,
                                   (double)ABS(temp1_float16 - temp2_float16));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float16, (double)temp2_float16,
                               (double)ABS(temp1_float16 - temp2_float16));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float16);
            isnan2 = isnan(temp2_float16);
        }
        /* both not NaN, do the comparison */
        if ((!isnan1 && !isnan2)) {
            PER(temp1_float16, temp2_float16);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, (double)temp1_float16, (double)temp2_float16,
                                   (double)ABS(temp1_float16 - temp2_float16));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, (double)temp1_float16, (double)temp2_float16,
                                   (double)ABS(temp1_float16 - temp2_float16),
                                   (double)ABS(1 - temp2_float16 / temp1_float16));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float16, (double)temp2_float16,
                               (double)ABS(temp1_float16 - temp2_float16));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->delta_bool && opts->percent_bool) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        if (opts->do_nans) {
            isnan1 = isnan(temp1_float16);
            isnan2 = isnan(temp2_float16);
        }

        /* both not NaN, do the comparison */
        if (!isnan1 && !isnan2) {
            PER(temp1_float16, temp2_float16);

            if (not_comparable && !both_zero) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P_NOTCOMP, (double)temp1_float16, (double)temp2_float16,
                                   (double)ABS(temp1_float16 - temp2_float16));
                }
                nfound++;
            }
            else if (per > opts->percent && (double)ABS(temp1_float16 - temp2_float16) > opts->delta) {
                opts->print_percentage = 1;
                print_pos(opts, elem_idx, 0);
                if (print_data(opts)) {
                    parallel_print(F_FORMAT_P, (double)temp1_float16, (double)temp2_float16,
                                   (double)ABS(temp1_float16 - temp2_float16),
                                   (double)ABS(1 - temp2_float16 / temp1_float16));
                }
                nfound++;
            }
        }
        /* only one is NaN, assume difference */
        else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float16, (double)temp2_float16,
                               (double)ABS(temp1_float16 - temp2_float16));
            }
            nfound++;
        }
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        if (equal_float((float)temp1_float16, (float)temp2_float16, opts) == false) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(F_FORMAT, (double)temp1_float16, (double)temp2_float16,
                               (double)ABS(temp1_float16 - temp2_float16));
            }
            nfound++;
        }
    }

    H5TOOLS_ENDDEBUG(": %" PRIuHSIZE " zero:%d", nfound, both_zero);
    return nfound;
}
#endif

/*-------------------------------------------------------------------------
 * Function: diff_schar_element
 *
 * Purpose:  diff a single H5T_NATIVE_SCHAR type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_schar_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    char    temp1_char;
    char    temp2_char;
    double  per;
    bool    both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);
    memcpy(&temp1_char, mem1, sizeof(char));
    memcpy(&temp2_char, mem2, sizeof(char));

    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (ABS(temp1_char - temp2_char) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER(temp1_char, temp2_char);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER(temp1_char, temp2_char);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
            }
            nfound++;
        }
        else if (per > opts->percent && ABS(temp1_char - temp2_char) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
            }
            nfound++;
        }
    }
    else if (temp1_char != temp2_char) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_uchar_element
 *
 * Purpose:  diff a single H5T_NATIVE_UCHAR type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_uchar_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t       nfound = 0; /* number of differences found */
    unsigned char temp1_uchar;
    unsigned char temp2_uchar;
    double        per;
    bool          both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    memcpy(&temp2_uchar, mem2, sizeof(unsigned char));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
            }
            nfound++;
        }
        else if (per > opts->percent && PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    else if (temp1_uchar != temp2_uchar) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_short_element
 *
 * Purpose:  diff a H5T_NATIVE_SHORT type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_short_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    short   temp1_short;
    short   temp2_short;
    double  per;
    bool    both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_short, mem1, sizeof(short));
    memcpy(&temp2_short, mem2, sizeof(short));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (ABS(temp1_short - temp2_short) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER(temp1_short, temp2_short);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER(temp1_short, temp2_short);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
            }
            nfound++;
        }
        else if (per > opts->percent && ABS(temp1_short - temp2_short) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
            }
            nfound++;
        }
    }
    else if (temp1_short != temp2_short) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ushort_element
 *
 * Purpose:  diff a single H5T_NATIVE_USHORT type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_ushort_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t        nfound = 0; /* number of differences found */
    unsigned short temp1_ushort;
    unsigned short temp2_ushort;
    double         per;
    bool           both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_ushort, mem1, sizeof(unsigned short));
    memcpy(&temp2_ushort, mem2, sizeof(unsigned short));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_ushort, temp2_ushort) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort,
                               PDIFF(temp1_ushort, temp2_ushort));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort),
                               per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort,
                               PDIFF(temp1_ushort, temp2_ushort));
            }
            nfound++;
        }
        else if (per > opts->percent && PDIFF(temp1_ushort, temp2_ushort) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort),
                               per);
            }
            nfound++;
        }
    }
    else if (temp1_ushort != temp2_ushort) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function:  diff_int_element
 *
 * Purpose:   diff a single H5T_NATIVE_INT type
 *
 * Return:    number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_int_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    int     temp1_int;
    int     temp2_int;
    double  per;
    bool    both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_int, mem1, sizeof(int));
    memcpy(&temp2_int, mem2, sizeof(int));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (ABS(temp1_int - temp2_int) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER(temp1_int, temp2_int);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER(temp1_int, temp2_int);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
            }
            nfound++;
        }
        else if (per > opts->percent && ABS(temp1_int - temp2_int) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
            }
            nfound++;
        }
    }
    else if (temp1_int != temp2_int) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_uint_element
 *
 * Purpose:  diff a single H5T_NATIVE_UINT type
 *
 * Return:  number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_uint_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t      nfound = 0; /* number of differences found */
    unsigned int temp1_uint;
    unsigned int temp2_uint;
    double       per;
    bool         both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_uint, mem1, sizeof(unsigned int));
    memcpy(&temp2_uint, mem2, sizeof(unsigned int));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_uint, temp2_uint) > opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(UI_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed int, temp1_uint, temp2_uint);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(UI_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(UI_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed int, temp1_uint, temp2_uint);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(UI_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
            }
            nfound++;
        }
        else if (per > opts->percent && PDIFF(temp1_uint, temp2_uint) > opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(UI_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
            }
            nfound++;
        }
    }
    else if (temp1_uint != temp2_uint) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(UI_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_long_element
 *
 * Purpose:  diff a single H5T_NATIVE_LONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_long_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t nfound = 0; /* number of differences found */
    long    temp1_long;
    long    temp2_long;
    double  per;
    bool    both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_long, mem1, sizeof(long));
    memcpy(&temp2_long, mem2, sizeof(long));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (ABS(temp1_long - temp2_long) > (long)opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER(temp1_long, temp2_long);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER(temp1_long, temp2_long);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
            }
            nfound++;
        }
        else if (per > opts->percent && ABS(temp1_long - temp2_long) > (long)opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
            }
            nfound++;
        }
    }
    else if (temp1_long != temp2_long) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ulong_element
 *
 * Purpose:  diff a single H5T_NATIVE_ULONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_ulong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t       nfound = 0; /* number of differences found */
    unsigned long temp1_ulong;
    unsigned long temp2_ulong;
    double        per;
    bool          both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_ulong, mem1, sizeof(unsigned long));
    memcpy(&temp2_ulong, mem2, sizeof(unsigned long));
    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_ulong, temp2_ulong) > (unsigned long)opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong,
                               PDIFF(temp1_ulong, temp2_ulong));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong,
                               PDIFF(temp1_ulong, temp2_ulong));
            }
            nfound++;
        }
        else if (per > opts->percent && PDIFF(temp1_ulong, temp2_ulong) > (unsigned long)opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
            }
            nfound++;
        }
    }
    else if (temp1_ulong != temp2_ulong) {
        opts->print_percentage = 0;
        print_pos(opts, elem_idx, 0);
        if (print_data(opts)) {
            parallel_print(ULI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
        }
        nfound++;
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_llong_element
 *
 * Purpose:  diff a single H5T_NATIVE_LLONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_llong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t   nfound = 0; /* number of differences found */
    long long temp1_llong;
    long long temp2_llong;
    double    per;
    bool      both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_llong, mem1, sizeof(long long));
    memcpy(&temp2_llong, mem2, sizeof(long long));

    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (ABS(temp1_llong - temp2_llong) > (long long)opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        PER(temp1_llong, temp2_llong);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT_P_NOTCOMP, temp1_llong, temp2_llong,
                               ABS(temp1_llong - temp2_llong));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT_P, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        PER(temp1_llong, temp2_llong);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT_P_NOTCOMP, temp1_llong, temp2_llong,
                               ABS(temp1_llong - temp2_llong));
            }
            nfound++;
        }
        else if (per > opts->percent && ABS(temp1_llong - temp2_llong) > (long long)opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT_P, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong), per);
            }
            nfound++;
        }
    }
    else {
        if (temp1_llong != temp2_llong) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(LLI_FORMAT, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
            }
            nfound++;
        }
    }

    H5TOOLS_ENDDEBUG(":%" PRIuHSIZE " - errstat:%d", nfound, opts->err_stat);

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ullong_element
 *
 * Purpose:  diff a single H5T_NATIVE_ULLONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t
diff_ullong_element(unsigned char *mem1, unsigned char *mem2, hsize_t elem_idx, diff_opt_t *opts)
{
    hsize_t            nfound = 0; /* number of differences found */
    unsigned long long temp1_ullong;
    unsigned long long temp2_ullong;
    float              f1, f2;
    double             per;
    bool               both_zero = false;

    H5TOOLS_START_DEBUG("delta_bool:%d - percent_bool:%d", opts->delta_bool, opts->percent_bool);

    memcpy(&temp1_ullong, mem1, sizeof(unsigned long long));
    memcpy(&temp2_ullong, mem2, sizeof(unsigned long long));

    /* -d and !-p */
    if (opts->delta_bool && !opts->percent_bool) {
        if (PDIFF(temp1_ullong, temp2_ullong) > (unsigned long long)opts->delta) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT, temp1_ullong, temp2_ullong, PDIFF(temp1_ullong, temp2_ullong));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->delta_bool && opts->percent_bool) {
        ull2float(temp1_ullong, &f1);
        ull2float(temp2_ullong, &f2);
        PER(f1, f2);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT_P_NOTCOMP, temp1_ullong, temp2_ullong,
                               PDIFF(temp1_ullong, temp2_ullong));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT_P, temp1_ullong, temp2_ullong, PDIFF(temp1_ullong, temp2_ullong),
                               per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->delta_bool && opts->percent_bool) {
        ull2float(temp1_ullong, &f1);
        ull2float(temp2_ullong, &f2);
        PER(f1, f2);

        if (not_comparable && !both_zero) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT_P_NOTCOMP, temp1_ullong, temp2_ullong,
                               PDIFF(temp1_ullong, temp2_ullong));
            }
            nfound++;
        }
        else if (per > opts->percent && PDIFF(temp1_ullong, temp2_ullong) > (unsigned long long)opts->delta) {
            opts->print_percentage = 1;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT_P, temp1_ullong, temp2_ullong, PDIFF(temp1_ullong, temp2_ullong),
                               per);
            }
            nfound++;
        }
    }
    else {
        if (temp1_ullong != temp2_ullong) {
            opts->print_percentage = 0;
            print_pos(opts, elem_idx, 0);
            if (print_data(opts)) {
                parallel_print(ULLI_FORMAT, temp1_ullong, temp2_ullong, PDIFF(temp1_ullong, temp2_ullong));
            }
            nfound++;
        }
    }

    H5TOOLS_ENDDEBUG(": %" PRIuHSIZE " zero:%d", nfound, both_zero);
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function:    ull2float
 *
 * Purpose:     convert unsigned long long to float
 *-------------------------------------------------------------------------
 */
static int
ull2float(unsigned long long ull_value, float *f_value)
{
    hid_t          dxpl_id = H5I_INVALID_HID;
    unsigned char *buf     = NULL;
    size_t         src_size;
    size_t         dst_size;
    int            ret_value = 0;

    H5TOOLS_START_DEBUG(" ");
    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Pcreate failed");

    src_size = H5Tget_size(H5T_NATIVE_ULLONG);
    dst_size = H5Tget_size(H5T_NATIVE_FLOAT);
    if ((buf = (unsigned char *)calloc((size_t)1, MAX(src_size, dst_size))) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate buffer for dims");

    memcpy(buf, &ull_value, src_size);

    /* do conversion */
    if (H5Tconvert(H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT, (size_t)1, buf, NULL, dxpl_id) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Tconvert failed");

    memcpy(f_value, buf, dst_size);

done:
    H5E_BEGIN_TRY
    {
        H5Pclose(dxpl_id);
    }
    H5E_END_TRY

    if (buf)
        free(buf);

    H5TOOLS_ENDDEBUG(" ");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    equal_double
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */
static bool
equal_double(double value, double expected, diff_opt_t *opts)
{
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        bool isnan1 = isnan(value);
        bool isnan2 = isnan(expected);

        /*-------------------------------------------------------------------------
         * we consider NaN == NaN to be true
         *-------------------------------------------------------------------------
         */
        if (isnan1 && isnan2)
            return true;

        /*-------------------------------------------------------------------------
         * one is a NaN, do not compare but assume difference
         *-------------------------------------------------------------------------
         */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return false;
    }

    if (opts->use_system_epsilon) {
        /* Check equality within some epsilon */
        if (H5_DBL_ABS_EQUAL(value, expected))
            return true;
    }
    else {
        /* Check bits */
        if (!memcmp(&value, &expected, sizeof(double)))
            return true;
    }

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    equal_ldouble
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */

static bool
equal_ldouble(long double value, long double expected, diff_opt_t *opts)
{
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        bool isnan1 = isnan(value);
        bool isnan2 = isnan(expected);

        /*-------------------------------------------------------------------------
         * we consider NaN == NaN to be true
         *-------------------------------------------------------------------------
         */
        if (isnan1 && isnan2)
            return true;

        /*-------------------------------------------------------------------------
         * one is a NaN, do not compare but assume difference
         *-------------------------------------------------------------------------
         */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return false;
    }

    if (opts->use_system_epsilon) {
        /* Check equality within some epsilon */
        if (H5_LDBL_ABS_EQUAL(value, expected))
            return true;
    }
    else {
        /* Check bits */
        if (!memcmp(&value, &expected, sizeof(long double)))
            return true;
    }

    return false;
}

/*-------------------------------------------------------------------------
 * Function:    equal_float
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */
static bool
equal_float(float value, float expected, diff_opt_t *opts)
{
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        bool isnan1 = isnan(value);
        bool isnan2 = isnan(expected);

        /*-------------------------------------------------------------------------
         * we consider NaN == NaN to be true
         *-------------------------------------------------------------------------
         */
        if (isnan1 && isnan2)
            return true;

        /*-------------------------------------------------------------------------
         * one is a NaN, do not compare but assume difference
         *-------------------------------------------------------------------------
         */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return false;
    }

    if (opts->use_system_epsilon) {
        /* Check equality within some epsilon */
        if (H5_FLT_ABS_EQUAL(value, expected))
            return true;
    }
    else {
        /* Check bits */
        if (!memcmp(&value, &expected, sizeof(float)))
            return true;
    }

    return false;
}

/*-------------------------------------------------------------------------
 *
 * Local functions
 *
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function: print_data
 *
 * Purpose:  print data only in report or verbose modes, and do not print in quiet mode
 *-------------------------------------------------------------------------
 */
static int
print_data(diff_opt_t *opts)
{
    return ((opts->mode_report || opts->mode_verbose) && !opts->mode_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: print_header
 *
 * Purpose:  print header for difference
 *-------------------------------------------------------------------------
 */
static void
print_header(diff_opt_t *opts)
{
    /* print header */
    parallel_print("%-16s", "size:");
    print_dimensions(opts->rank, opts->dims);
    parallel_print("%-11s", "");
    print_dimensions(opts->rank, opts->dims);
    parallel_print("\n");

    if (opts->print_percentage) {
        parallel_print("%-15s %-15s %-15s %-15s %-15s\n", "position", opts->obj_name[0], opts->obj_name[1],
                       "difference", "relative");
        parallel_print("------------------------------------------------------------------------\n");
    }
    else {
        parallel_print("%-15s %-15s %-15s %-20s\n", "position", opts->obj_name[0], opts->obj_name[1],
                       "difference");
        parallel_print("------------------------------------------------------------\n");
    }
}

/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose:  print in matrix notation, converting from an array index position
 *-------------------------------------------------------------------------
 */
static void
print_pos(diff_opt_t *opts, hsize_t idx, size_t u)
{
    H5TOOLS_START_DEBUG(" -- idx:%" PRIuHSIZE, idx);

    if (print_data(opts)) {
        hsize_t curr_pos = idx;
        /* print header */
        if (opts->print_header == 1) {
            opts->print_header = 0;
            print_header(opts);
        } /* end print header */

        H5TOOLS_DEBUG("rank=%d", opts->rank);
        if (opts->rank > 0) {
            parallel_print("[ ");
            H5TOOLS_DEBUG("do calc_acc_pos[%" PRIuHSIZE "] nelmts:%" PRIuHSIZE " - errstat:%d", idx,
                          opts->hs_nelmts, opts->err_stat);
            if (opts->sset[0] != NULL) {
                /* Subsetting is used - calculate total position */
                hsize_t curr_idx = 0; /* current pos in the selection space for each dimension */

                curr_pos = 0; /* current position in full space */
                if (curr_idx < idx) {
                    int     j;
                    hsize_t count;
                    hsize_t block;
                    hsize_t stride              = 0;
                    hsize_t tmp                 = 0;
                    hsize_t k0                  = 0; /* whole location beyond current dimension */
                    hsize_t k1                  = 0; /* partial location within dimension */
                    hsize_t dim_size            = 0; /* previous dim size */
                    hsize_t prev_dim_size       = 0; /* previous dim size */
                    hsize_t total_dim_size      = 1; /* current dim size */
                    hsize_t prev_total_dim_size = 1; /* current dim size */

                    prev_dim_size  = 1;
                    total_dim_size = 1;
                    curr_idx       = idx;
                    /* begin with fastest changing dimension */
                    for (int i = 0; i < opts->rank; i++) {
                        j = opts->rank - i - 1;
                        prev_total_dim_size *= prev_dim_size;
                        dim_size = opts->dims[j];
                        H5TOOLS_DEBUG("j=%d, dim_size=%" PRIuHSIZE ", prev_dim_size=%" PRIuHSIZE
                                      ", total_dim_size=%" PRIuHSIZE ", "
                                      "prev_total_dim_size=%" PRIuHSIZE,
                                      j, dim_size, prev_dim_size, total_dim_size, prev_total_dim_size);
                        count  = opts->sset[0]->count.data[j];
                        block  = opts->sset[0]->block.data[j];
                        stride = opts->sset[0]->stride.data[j];
                        H5TOOLS_DEBUG("stride=%" PRIuHSIZE ", count=%" PRIuHSIZE ", block=%" PRIuHSIZE,
                                      stride, count, block);
                        tmp = count * block;
                        k0  = curr_idx / tmp;
                        k1  = curr_idx % tmp;
                        curr_pos += k1 * stride * prev_total_dim_size;
                        H5TOOLS_DEBUG("curr_idx=%" PRIuHSIZE ", k0=%" PRIuHSIZE ", k1=%" PRIuHSIZE
                                      ", curr_pos=%" PRIuHSIZE,
                                      curr_idx, k0, k1, curr_pos);
                        if (k0 > 0)
                            curr_idx = k0 * total_dim_size;
                        H5TOOLS_DEBUG("curr_idx=%" PRIuHSIZE ", tmp=%" PRIuHSIZE, curr_idx, tmp);
                        total_dim_size *= dim_size;
                        /* if last calculation exists within in current dimension */
                        if (k0 == 0)
                            break;
                        H5TOOLS_DEBUG("j=%d, curr_pos=%" PRIuHSIZE, j, curr_pos);
                        prev_dim_size = dim_size;
                    }
                    /* check if there is a final calculation needed for slowest changing dimension */
                    if (k0 > 0)
                        curr_pos += k0 * stride * prev_total_dim_size;
                    H5TOOLS_DEBUG("4:curr_idx=%" PRIuHSIZE ", curr_pos=%" PRIuHSIZE, curr_idx, curr_pos);
                }
            }
            /*
             * Calculate the number of elements represented by a unit change in a
             * certain index position.
             */
            calc_acc_pos((unsigned)opts->rank, curr_pos, opts->acc, opts->pos);

            for (int i = 0; i < opts->rank; i++) {
                H5TOOLS_DEBUG("pos loop:%d with opts->pos=%" PRIuHSIZE " opts->sm_pos=%" PRIuHSIZE, i,
                              opts->pos[i], opts->sm_pos[i]);
                opts->pos[i] += (unsigned long)opts->sm_pos[i];
                H5TOOLS_DEBUG("pos loop:%d with opts->pos=%" PRIuHSIZE, i, opts->pos[i]);
                parallel_print("%" PRIuHSIZE, opts->pos[i]);
                parallel_print(" ");
            }
            parallel_print("]");
        }
        else {
            if (opts->print_dims) {
                parallel_print("[ ");
                parallel_print("%zu", u);
                parallel_print("]");
                opts->print_dims = 0;
            }
            else
                parallel_print("      ");
        }
        parallel_print(SPACES);
    }

    H5TOOLS_ENDDEBUG(" ");
}

/*-------------------------------------------------------------------------
 * Function: h5diff_print_char. Adapted from h5tools_print_char
 *
 * Purpose:  Print a char
 *-------------------------------------------------------------------------
 */
static void
h5diff_print_char(char ch)
{
    switch (ch) {
        case '"':
            parallel_print("\\\"");
            break;
        case '\\':
            parallel_print("\\\\");
            break;
        case '\b':
            parallel_print("\\b");
            break;
        case '\f':
            parallel_print("\\f");
            break;
        case '\n':
            parallel_print("\\n");
            break;
        case '\r':
            parallel_print("\\r");
            break;
        case '\t':
            parallel_print("\\t");
            break;
        default:
            if (isprint(ch))
                parallel_print("%c", ch);
            else
                parallel_print("\\%03o", ch);
            break;
    }
}

/*-------------------------------------------------------------------------
 * added to improve performance for compound datasets
 * set up compound datatype structures.
 *-------------------------------------------------------------------------
 */
static void
get_member_types(hid_t tid, mcomp_t *members)
{
    int      tclass;
    unsigned u;

    if (tid <= 0 || !members)
        return;

    tclass = H5Tget_class(tid);
    if (tclass == H5T_ARRAY || tclass == H5T_VLEN) {
        hid_t base_tid = H5Tget_super(tid);
        get_member_types(base_tid, members);
        H5Tclose(base_tid);
    }
    else if (tclass == H5T_COMPOUND) {
        int nmembs;

        if ((nmembs = H5Tget_nmembers(tid)) <= 0)
            return;
        members->n = (unsigned)nmembs;

        members->ids     = (hid_t *)calloc((size_t)members->n, sizeof(hid_t));
        members->offsets = (size_t *)calloc((size_t)members->n, sizeof(size_t));
        members->m       = (mcomp_t **)calloc((size_t)members->n, sizeof(mcomp_t *));

        for (u = 0; u < members->n; u++) {
            members->ids[u]     = H5Tget_member_type(tid, u);
            members->offsets[u] = H5Tget_member_offset(tid, u);
            members->m[u]       = (mcomp_t *)malloc(sizeof(mcomp_t));
            memset(members->m[u], 0, sizeof(mcomp_t));
            get_member_types(members->ids[u], members->m[u]);
        }
    }
}

/*-------------------------------------------------------------------------
 * added to improve performance for compound datasets
 * clean and close compound members.
 *-------------------------------------------------------------------------
 */
static void
close_member_types(mcomp_t *members)
{
    unsigned u;

    if (!members || members->n <= 0 || !members->ids)
        return;

    for (u = 0; u < members->n; u++) {
        if (members->m[u]) {
            close_member_types(members->m[u]);
            free(members->m[u]);
        }
        H5Tclose(members->ids[u]);
    }

    free(members->m);
    free(members->ids);
    free(members->offsets);
}
