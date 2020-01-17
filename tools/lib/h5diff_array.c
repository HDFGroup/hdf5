/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
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

#define F_FORMAT      "%-15g %-15g %-15g\n"

#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT     "%-15Lf %-15Lf %-15Lf\n"
#endif

#define I_FORMAT      "%-15d %-15d %-15d\n"
#define S_FORMAT      "%-16s %-17s\n"
#define UI_FORMAT     "%-15u %-15u %-15u\n"
#define LI_FORMAT     "%-15ld %-15ld %-15ld\n"
#define ULI_FORMAT    "%-15lu %-15lu %-15lu\n"
#define LLI_FORMAT    "%-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d\n"
#define ULLI_FORMAT   "%-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "u\n"

/* with -p option */
#define F_FORMAT_P    "%-15.10g %-15.10g %-15.10g %-14.10g\n"

#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT_P    "%-15.10Lf %-15.10Lf %-15.10Lf %-14.10Lf\n"
#endif

#define I_FORMAT_P    "%-15d %-15d %-15d %-14f\n"
#define UI_FORMAT_P   "%-15u %-15u %-15u %-14f\n"
#define LI_FORMAT_P   "%-15ld %-15ld %-15ld %-14f\n"
#define ULI_FORMAT_P  "%-15lu %-15lu %-15lu %-14f\n"
#define LLI_FORMAT_P  "%-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d %-14f\n"
#define ULLI_FORMAT_P "%-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "d %-14f\n"
#define SPACES        "          "

/* not comparable */
#define F_FORMAT_P_NOTCOMP  "%-15.10g %-15.10g %-15.10g not comparable\n"

#if H5_SIZEOF_LONG_DOUBLE !=0
#define LD_FORMAT_P_NOTCOMP  "%-15.10Lf %-15.10Lf %-15.10Lf not comparable\n"
#endif

#define I_FORMAT_P_NOTCOMP  "%-15d %-15d %-15d not comparable\n"
#define UI_FORMAT_P_NOTCOMP   "%-15u %-15u %-15u not comparable\n"
#define LI_FORMAT_P_NOTCOMP   "%-15ld %-15ld %-15ld not comparable\n"
#define ULI_FORMAT_P_NOTCOMP  "%-15lu %-15lu %-15lu not comparable\n"
#define LLI_FORMAT_P_NOTCOMP  "%-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d %-15" H5_PRINTF_LL_WIDTH "d not comparable\n"
#define ULLI_FORMAT_P_NOTCOMP "%-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "u %-15" H5_PRINTF_LL_WIDTH "d not comparable\n"

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

static hbool_t not_comparable;

#define PER(A,B) {                                                      \
    per = -1;                                                           \
    not_comparable = FALSE;                                             \
    both_zero = FALSE;                                                  \
    if(H5_DBL_ABS_EQUAL(0, (double)A) && H5_DBL_ABS_EQUAL(0, (double)B)) \
        both_zero = TRUE;                                               \
    if(!H5_DBL_ABS_EQUAL(0, (double)A))                                 \
        per = (double)ABS((double)((B) - (A)) / (double)(A));           \
    else                                                                \
        not_comparable = TRUE;                                          \
}

#define PER_UNSIGN(TYPE,A,B) {                                          \
    per = -1;                                                           \
    not_comparable = FALSE;                                             \
    both_zero = FALSE;                                                  \
    if(H5_DBL_ABS_EQUAL(0, (double)A) && H5_DBL_ABS_EQUAL(0, (double)B)) \
        both_zero = TRUE;                                               \
    if(!H5_DBL_ABS_EQUAL(0, (double)A))                                 \
        per = ABS((double)((TYPE)((B) - (A))) / (double)(A)) ;          \
    else                                                                \
        not_comparable = TRUE;                                          \
}

#define PDIFF(a,b)    (((b) > (a)) ? ((b) - (a)) : ((a) -(b)))

typedef struct mcomp_t {
    unsigned n; /* number of members */
    hid_t *ids; /* member type id */
    size_t *offsets;
    struct mcomp_t **m; /* members */
} mcomp_t;

/*-------------------------------------------------------------------------
 * local prototypes
 *-------------------------------------------------------------------------
 */
static hsize_t diff_region(hid_t obj1_id, hid_t obj2_id, hid_t region1_id,
        hid_t region2_id, diff_opt_t *opts);
static hbool_t all_zero(const void *_mem, size_t size);
static int ull2float(unsigned long long ull_value, float *f_value);
static hsize_t character_compare(char *mem1, char *mem2, hsize_t i, size_t u,
        int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos,
        diff_opt_t *opts, const char *obj1, const char *obj2, int *ph);
static hsize_t character_compare_opt(unsigned char *mem1, unsigned char *mem2,
        hsize_t i, int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos,
        diff_opt_t *opts, const char *obj1, const char *obj2, int *ph);
static hbool_t equal_float(float value, float expected, diff_opt_t *opts);
static hbool_t equal_double(double value, double expected, diff_opt_t *opts);
#if H5_SIZEOF_LONG_DOUBLE !=0
static hbool_t equal_ldouble(long double value, long double expected, diff_opt_t *opts);
#endif
static int print_data(diff_opt_t *opts);
static void print_pos(int *ph, int pp, hsize_t curr_pos, hsize_t *acc,
        hsize_t *pos, int rank, hsize_t *dims, const char *obj1,
        const char *obj2);
static void print_char_pos(int *ph, int pp, hsize_t curr_pos, size_t u,
        hsize_t *acc, hsize_t *pos, int rank, hsize_t *dims, const char *obj1,
        const char *obj2);
static void h5diff_print_char(char ch);
static hsize_t diff_datum(void *_mem1, void *_mem2, hid_t m_type, hsize_t index,
        int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos,
        diff_opt_t *opts, const char *obj1, const char *obj2,
        hid_t container1_id, hid_t container2_id, /*where the reference came from*/
        int *ph, /*print header */
        mcomp_t *members); /*compound members */
static hsize_t diff_float(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_double(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
#if H5_SIZEOF_LONG_DOUBLE !=0
static hsize_t diff_ldouble(unsigned char *mem1,
        unsigned char *mem2,
        hsize_t nelmts,
        hsize_t hyper_start,
        int rank,
        hsize_t *dims,
        hsize_t *acc,
        hsize_t *pos,
        diff_opt_t *opts,
        const char *obj1,
        const char *obj2,
        int *ph);
#endif
static hsize_t diff_schar(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_uchar(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_short(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_ushort(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_int(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_uint(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_long(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_ulong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_llong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);
static hsize_t diff_ullong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph);

/*-------------------------------------------------------------------------
 * NaN detection
 *-------------------------------------------------------------------------
 */

#if H5_SIZEOF_LONG_DOUBLE !=0
typedef enum dtype_t
{
    FLT_FLOAT,
    FLT_DOUBLE,
    FLT_LDOUBLE
}dtype_t;
#else

typedef enum dtype_t {
    FLT_FLOAT, FLT_DOUBLE
} dtype_t;
#endif

static hbool_t my_isnan(dtype_t type, void *val);

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

hsize_t diff_array(
        void *_mem1,
        void *_mem2,
        hsize_t nelmts,
        hsize_t hyper_start,
        int rank,
        hsize_t *dims,
        diff_opt_t *opts,
        const char *name1,
        const char *name2,
        hid_t m_type,
        hid_t container1_id,
        hid_t container2_id) /* dataset where the reference came from*/
{
    hsize_t         nfound = 0; /* number of differences found */
    size_t          size; /* size of datum */
    unsigned char  *mem1 = (unsigned char*) _mem1;
    unsigned char  *mem2 = (unsigned char*) _mem2;
    hsize_t         acc[32]; /* accumulator position */
    hsize_t         pos[32]; /* matrix position */
    int             ph = 1; /* print header  */
    hsize_t         i;
    int             j;
    mcomp_t         members;
    H5T_class_t     type_class;

    H5TOOLS_DEBUG("diff_array start - errstat:%d", opts->err_stat);
    /* get the size. */
    size = H5Tget_size(m_type);
    type_class = H5Tget_class(m_type);

    /* Fast comparison first for atomic type by memcmp().
     * It is OK not to list non-atomic type here because it will not be caught
     * by the condition, but it gives more clarity for code planning
     */
    if (type_class != H5T_REFERENCE &&
            type_class != H5T_COMPOUND &&
            type_class != H5T_STRING &&
            type_class != H5T_VLEN &&
            HDmemcmp(mem1, mem2, size*nelmts) == 0)
        return 0;

    if (rank > 0) {
        acc[rank - 1] = 1;
        for (j = (rank - 2); j >= 0; j--) {
            acc[j] = acc[j + 1] * dims[j + 1];
        }
        for (j = 0; j < rank; j++)
            pos[j] = 0;
    }

    H5TOOLS_DEBUG("diff_array type_class:%d", type_class);
    switch (type_class) {
    case H5T_NO_CLASS:
    case H5T_TIME:
    case H5T_NCLASSES:
    default:
        HDassert(0);
        break;

    /*-------------------------------------------------------------------------
     * float and integer atomic types
     *-------------------------------------------------------------------------
     */
    case H5T_FLOAT:
        if (H5Tequal(m_type, H5T_NATIVE_FLOAT))
            nfound = diff_float(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_DOUBLE))
            nfound = diff_double(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
#if H5_SIZEOF_LONG_DOUBLE != 0
        else if (H5Tequal(m_type, H5T_NATIVE_LDOUBLE))
        nfound = diff_ldouble(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
#endif
        break;

    case H5T_INTEGER:
        if (H5Tequal(m_type, H5T_NATIVE_SCHAR))
            nfound = diff_schar(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_UCHAR))
            nfound = diff_uchar(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_SHORT))
            nfound = diff_short(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_USHORT))
            nfound = diff_ushort(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_INT))
            nfound = diff_int(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_UINT))
            nfound = diff_uint(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_LONG))
            nfound = diff_long(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_ULONG))
            nfound = diff_ulong(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_LLONG))
            nfound = diff_llong(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
        else if (H5Tequal(m_type, H5T_NATIVE_ULLONG))
            nfound = diff_ullong(mem1, mem2, nelmts, hyper_start, rank, dims, acc, pos, opts, name1, name2, &ph);
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
        HDmemset(&members, 0, sizeof(mcomp_t));
        get_member_types(m_type, &members);
        H5TOOLS_DEBUG("call diff_datum nelmts:%d - errstat:%d", nelmts, opts->err_stat);
        for (i = 0; i < nelmts; i++) {
            nfound += diff_datum(mem1 + i * size, mem2 + i * size, m_type, i, rank, dims, acc, pos, opts,
                    name1, name2, container1_id, container2_id, &ph, &members);
            if (opts->n && nfound >= opts->count)
                break;
        } /* i */
        close_member_types(&members);
    } /* switch */
    H5TOOLS_DEBUG("diff_array finish:%d - errstat:%d", nfound, opts->err_stat);

    H5TOOLS_ENDDEBUG("exit: %d", nfound);
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
static hsize_t diff_datum(
        void *_mem1,
        void *_mem2,
        hid_t m_type,
        hsize_t index,
        int rank,
        hsize_t *dims,
        hsize_t *acc,
        hsize_t *pos,
        diff_opt_t *opts,
        const char *obj1,
        const char *obj2,
        hid_t container1_id,
        hid_t container2_id, /*where the reference came from*/
        int *ph,             /*print header */
        mcomp_t *members)    /*compound members */
{
    unsigned char  *mem1 = (unsigned char*) _mem1;
    unsigned char  *mem2 = (unsigned char*) _mem2;
    size_t          u;
    size_t          type_size;
    H5T_sign_t      type_sign;
    H5T_class_t     type_class;
    size_t          offset;
    unsigned        nmembs;
    unsigned        j;
    hsize_t         nelmts;
    size_t          size = 0;
    hbool_t         iszero1;
    hbool_t         iszero2;
    hsize_t         nfound = 0;    /* differences found */
    double          per;
    hbool_t         both_zero;
    diff_err_t      ret_value = opts->err_stat;

    H5TOOLS_DEBUG("diff_datum start - errstat:%d", opts->err_stat);

    type_size = H5Tget_size(m_type);
    type_class = H5Tget_class(m_type);

    /* Fast comparison first for atomic type by memcmp().
     * It is OK not to list non-atomic type here because it will not be caught
     * by the condition, but it gives more clarity for code planning
     */
    if (type_class != H5T_REFERENCE &&
            type_class != H5T_COMPOUND &&
            type_class != H5T_STRING &&
            type_class != H5T_VLEN &&
            HDmemcmp(mem1, mem2, type_size) == 0)
        H5TOOLS_GOTO_DONE(opts->err_stat);

    switch (H5Tget_class(m_type)) {
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
        H5TOOLS_DEBUG("diff_datum H5T_COMPOUND");
        {
            hid_t memb_type = H5I_INVALID_HID;
            nmembs = members->n;

            for (j = 0; j < nmembs; j++) {
                offset = members->offsets[j];
                memb_type = members->ids[j];

                nfound += diff_datum(mem1 + offset, mem2 + offset, memb_type, index,
                        rank, dims, acc, pos, opts, obj1, obj2, container1_id, container2_id, ph, members->m[j]);
            }
        }
        break;

    /*-------------------------------------------------------------------------
     * H5T_STRING
     *-------------------------------------------------------------------------
     */
    case H5T_STRING:
        H5TOOLS_DEBUG("diff_datum H5T_STRING");
        {
            char *s = NULL;
            char *sx = NULL;
            char *s1 = NULL;
            char *s2 = NULL;
            size_t size1;
            size_t size2;
            size_t sizex;
            size_t size_mtype = H5Tget_size(m_type);
            H5T_str_t pad = H5Tget_strpad(m_type);

            /* if variable length string */
            if (H5Tis_variable_str(m_type)) {
                H5TOOLS_DEBUG("diff_datum H5T_STRING variable");
                /* Get pointer to first string */
                s1 = *(char **)((void *)mem1);
                if (s1)
                    size1 = HDstrlen(s1);
                else
                    size1 = 0;

                /* Get pointer to second string */
                s2 = *(char **)((void *)mem2);
                if (s2)
                    size2 = HDstrlen(s2);
                else
                    size2 = 0;
            }
            else if (H5T_STR_NULLTERM == pad) {
                H5TOOLS_DEBUG("diff_datum H5T_STRING null term");
                /* Get pointer to first string */
                s1 = (char*) mem1;
                if (s1)
                    size1 = HDstrlen(s1);
                else
                    size1 = 0;

                if (size1 > size_mtype)
                    size1 = size_mtype;

                /* Get pointer to second string */
                s2 = (char*) mem2;
                if (s2)
                    size2 = HDstrlen(s2);
                else
                    size2 = 0;

                if (size2 > size_mtype)
                    size2 = size_mtype;
            }
            else {
                /* Get pointer to first string */
                s1 = (char *) mem1;
                size1 = size_mtype;

                /* Get pointer to second string */
                s2 = (char *) mem2;
                size2 = size_mtype;
            }

            /*
             * compare for shorter string
             * TODO: this code need to be improved to handle the difference
             *       of length of strings.
             *       For now mimic the previous way.
             */
            H5TOOLS_DEBUG("diff_datum string size:%d", size1);
            H5TOOLS_DEBUG("diff_datum string size:%d", size2);
            if (size1 != size2) {
                H5TOOLS_DEBUG("diff_datum string sizes difference");
                nfound++;
            }
            if (size1 < size2) {
                size = size1;
                s = s1;
                sizex = size2;
                sx = s2;
            }
            else {
                size = size2;
                s = s2;
                sizex = size1;
                sx = s1;
            }

            /* check for NULL pointer for string */
            if (s != NULL) {
                /* try fast compare first */
                if (HDmemcmp(s, sx, size) == 0) {
                    if (size1 != size2)
                        if (print_data(opts))
                            for (u = size; u < sizex; u++)
                                character_compare(s + u, sx + u, index, u, rank, dims, acc, pos, opts, obj1, obj2, ph);
                }
                else
                    for (u = 0; u < size; u++)
                        nfound += character_compare(s + u, sx + u, index, u, rank, dims, acc, pos, opts, obj1, obj2, ph);
            } /* end check for NULL pointer for string */
        }
        break;

    /*-------------------------------------------------------------------------
     * H5T_BITFIELD
     *-------------------------------------------------------------------------
     */
    case H5T_BITFIELD:
        H5TOOLS_DEBUG("diff_datum H5T_BITFIELD");
        /* byte-by-byte comparison */
        for (u = 0; u < type_size; u++)
            nfound += character_compare_opt(mem1 + u, mem2 + u, index, rank, dims, acc, pos, opts, obj1, obj2, ph);
        break;

    /*-------------------------------------------------------------------------
     * H5T_OPAQUE
     *-------------------------------------------------------------------------
     */
    case H5T_OPAQUE:
        H5TOOLS_DEBUG("diff_datum H5T_OPAQUE");
        /* byte-by-byte comparison */
        for (u = 0; u < type_size; u++)
            nfound += character_compare_opt(mem1 + u, mem2 + u, index, rank, dims, acc, pos, opts, obj1, obj2, ph);
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
        H5TOOLS_DEBUG("diff_datum H5T_ENUM");
        {
            char enum_name1[1024];
            char enum_name2[1024];
            herr_t err1;
            herr_t err2;

            /* disable error reporting */
            H5E_BEGIN_TRY {
                /* If the enum value cannot be converted to a string
                 * it is set to an error string for later output.
                 */
                err1 = H5Tenum_nameof(m_type, mem1, enum_name1, sizeof enum_name1);
                if (err1 < 0)
                    HDsnprintf(enum_name1, sizeof(enum_name1), "**INVALID VALUE**");

                err2 = H5Tenum_nameof(m_type, mem2, enum_name2, sizeof enum_name2);
                if (err2 < 0)
                    HDsnprintf(enum_name2, sizeof(enum_name2), "**INVALID VALUE**");

                /* One or more bad enum values */
                if (err1 < 0 || err2 < 0) {
                    /* If the two values cannot be converted to a string
                     * (probably due to them being invalid enum values),
                     * don't attempt to convert them - just report errors.
                     */
                    nfound += 1;
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(S_FORMAT, enum_name1, enum_name2);
                    }
                }
                else {
                    /* Both enum values were valid */
                    if (HDstrcmp(enum_name1, enum_name2) != 0) {
                        nfound = 1;
                        if (print_data(opts)) {
                            print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(S_FORMAT, enum_name1, enum_name2);
                        }
                    }
                    else {
                        for (u = 0; u < type_size; u++)
                            nfound += character_compare_opt(mem1 + u, mem2 + u, index, rank, dims, acc, pos, opts, obj1, obj2, ph);
                    }
                }
                /* enable error reporting */
            } H5E_END_TRY;
        }
        break;

    /*-------------------------------------------------------------------------
     * H5T_ARRAY
     *-------------------------------------------------------------------------
     */
    case H5T_ARRAY:
        {
            hid_t   memb_type = H5I_INVALID_HID;
            hsize_t adims[H5S_MAX_RANK];
            int     ndims;

            H5TOOLS_DEBUG("diff_datum H5T_ARRAY");
            /* get the array's base datatype for each element */
            memb_type = H5Tget_super(m_type);
            size = H5Tget_size(memb_type);
            ndims = H5Tget_array_ndims(m_type);
            H5Tget_array_dims2(m_type, adims);
            HDassert(ndims >= 1 && ndims <= H5S_MAX_RANK);

            /* calculate the number of array elements */
            for (u = 0, nelmts = 1; u < (unsigned) ndims; u++)
                nelmts *= adims[u];
            for (u = 0; u < nelmts; u++) {
                nfound += diff_datum(mem1 + u * size, mem2 + u * size, memb_type, index,
                        rank, dims, acc, pos, opts, obj1, obj2, container1_id, container2_id, ph, members);
            }
            H5Tclose(memb_type);
        }
        break;

    /*-------------------------------------------------------------------------
     * H5T_REFERENCE
     *-------------------------------------------------------------------------
     */
    case H5T_REFERENCE:
        H5TOOLS_DEBUG("diff_datum H5T_REFERENCE");
        iszero1 = all_zero(_mem1, H5Tget_size(m_type));
        iszero2 = all_zero(_mem2, H5Tget_size(m_type));
        if (iszero1 != iszero2) {
            nfound++;
            H5TOOLS_GOTO_DONE(opts->err_stat);
        }
        else if (!iszero1 && !iszero2) {
            hid_t obj1_id = H5I_INVALID_HID;
            hid_t obj2_id = H5I_INVALID_HID;

            /*-------------------------------------------------------------------------
             * H5T_STD_REF
             * Reference
             *-------------------------------------------------------------------------
             */
            if (H5Tequal(m_type, H5T_STD_REF)) {
                /* if (type_size == H5R_STD_REF_SIZE) */
                hid_t region1_id = H5I_INVALID_HID;
                hid_t region2_id = H5I_INVALID_HID;
                H5R_ref_t *ref1_buf = (H5R_ref_t *)_mem1;
                H5R_ref_t *ref2_buf = (H5R_ref_t *)_mem2;
                H5O_type_t obj1_type = -1;   /* Object type */
                H5O_type_t obj2_type = -1;   /* Object type */
                H5R_type_t ref_type;   /* Reference type */

                H5TOOLS_DEBUG("diff_datum H5T_REFERENCE - H5T_STD_REF");
                ref_type = H5Rget_type(ref1_buf);
                switch (ref_type) {
                    case H5R_OBJECT1:
                        H5TOOLS_DEBUG("ref_type is H5R_OBJECT1");
                        if (H5Rget_obj_type3(ref1_buf, H5P_DEFAULT, &obj1_type) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Rget_obj_type3 object 1 failed");
                        }
                        if (H5Rget_obj_type3(ref2_buf, H5P_DEFAULT, &obj2_type) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Rget_obj_type3 object 2 failed");
                        }

                        /* check object type */
                        if (obj1_type != obj2_type) {
                            parallel_print("Different object types referenced: <%s> and <%s>", obj1, obj2);
                            opts->not_cmp = 1;
                            H5TOOLS_GOTO_DONE(opts->err_stat);
                        }

                        if((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Ropen_object object 1 failed");
                        }
                        if((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Ropen_object object 2 failed");
                        }

                        switch (obj1_type) {
                            case H5O_TYPE_DATASET:
                                nfound = diff_datasetid(obj1_id, obj2_id, NULL, NULL, opts);
                                break;

                            case H5O_TYPE_GROUP:
                            case H5O_TYPE_NAMED_DATATYPE:
                            case H5O_TYPE_MAP:
                            case H5O_TYPE_UNKNOWN:
                            case H5O_TYPE_NTYPES:
                            default:
                                if (opts->m_verbose)
                                    parallel_print("Warning: Comparison not possible of object types referenced: <%s> and <%s>\n", obj1, obj2);
                                opts->not_cmp = 1;
                                break;
                        } /* end switch */
                        if(obj1_id >= 0)
                            if(H5Oclose(obj1_id) < 0) {
                                opts->err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Oclose H5R_OBJECT1 failed");
                            }
                        if(obj2_id >= 0)
                            if(H5Oclose(obj2_id) < 0) {
                                opts->err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Oclose H5R_OBJECT1 failed");
                            }
                        if(H5Rdestroy(ref2_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_OBJECT1 failed");
                        if(H5Rdestroy(ref1_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_OBJECT1 failed");
                        break;
                    case H5R_DATASET_REGION1:
                        H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION1");
                        if((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                            if((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if((region1_id = H5Ropen_region(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                    if((region2_id = H5Ropen_region(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                        nfound = diff_region(obj1_id, obj2_id, region1_id, region2_id, opts);
                                        if(H5Sclose(region2_id) < 0)
                                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Sclose H5R_DATASET_REGION1 failed");
                                    }
                                    if(H5Sclose(region1_id) < 0)
                                        H5TOOLS_ERROR(H5DIFF_ERR, "H5Sclose H5R_DATASET_REGION1 failed");
                                }
                                if(H5Oclose(obj2_id) < 0)
                                    H5TOOLS_ERROR(H5DIFF_ERR, "H5Oclose H5R_DATASET_REGION1 failed");
                            }
                            else {
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Ropen_object H5R_DATASET_REGION1 failed");
                            }
                            if(H5Oclose(obj1_id) < 0)
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Oclose H5R_DATASET_REGION1 failed");
                        }
                        else {
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Ropen_object H5R_DATASET_REGION1 failed");
                        }
                        if(H5Rdestroy(ref2_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_DATASET_REGION1 failed");
                        if(H5Rdestroy(ref1_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_DATASET_REGION1 failed");
                        break;
                    case H5R_OBJECT2:
                        H5TOOLS_DEBUG("ref_type is H5R_OBJECT2");
                        if (H5Rget_obj_type3(ref1_buf, H5P_DEFAULT, &obj1_type) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Rget_obj_type3 object 1 failed");
                        }
                        if (H5Rget_obj_type3(ref2_buf, H5P_DEFAULT, &obj2_type) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Rget_obj_type3 object 2 failed");
                        }

                        /* check object type */
                        if (obj1_type != obj2_type) {
                            parallel_print("Different object types referenced: <%s> and <%s>", obj1, obj2);
                            opts->not_cmp = 1;
                            H5TOOLS_GOTO_DONE(opts->err_stat);
                        }

                        if((obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Ropen_object object 1 failed");
                        }
                        if((obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                            opts->err_stat = H5DIFF_ERR;
                            H5TOOLS_INFO("H5Ropen_object object 2 failed");
                        }

                        switch (obj1_type) {
                            case H5O_TYPE_DATASET:
                                H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : DATASET");
                                nfound = diff_datasetid(obj1_id, obj2_id, NULL, NULL, opts);
                                break;

                            case H5O_TYPE_GROUP:
                                H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : GROUP");
                                if (opts->m_verbose)
                                    parallel_print("Warning: Comparison not possible of group object types referenced: <%s> and <%s>\n", obj1, obj2);
                                opts->not_cmp = 1;
                                break;

                            case H5O_TYPE_NAMED_DATATYPE:
                                H5TOOLS_DEBUG("ref_type is H5R_OBJECT2 : NAMED");
                                if (opts->m_verbose)
                                    parallel_print("Warning: Comparison not possible of named datatypes object types referenced: <%s> and <%s>\n", obj1, obj2);
                                opts->not_cmp = 1;
                                break;

                            case H5O_TYPE_MAP:
                            case H5O_TYPE_UNKNOWN:
                            case H5O_TYPE_NTYPES:
                            default:
                                if (opts->m_verbose)
                                    parallel_print("Warning: Comparison not possible of object types referenced: <%s> and <%s>\n", obj1, obj2);
                                opts->not_cmp = 1;
                                break;
                        } /* end switch */
                        if(obj1_id >= 0)
                            if(H5Oclose(obj1_id) < 0) {
                                opts->err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Oclose H5R_OBJECT2 failed");
                            }
                        if(obj2_id >= 0)
                            if(H5Oclose(obj2_id) < 0) {
                                opts->err_stat = H5DIFF_ERR;
                                H5TOOLS_INFO("H5Oclose H5R_OBJECT2 failed");
                            }
                        if(H5Rdestroy(ref2_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_OBJECT2 failed");
                        if(H5Rdestroy(ref1_buf) < 0)
                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_OBJECT2 failed");
                        break;
                    case H5R_DATASET_REGION2:
                        H5TOOLS_DEBUG("ref_type is H5R_DATASET_REGION2");

                        /* if (obj_id < 0) - could mean that no reference was written do not throw failure */
                        obj1_id = H5Ropen_object(ref1_buf, H5P_DEFAULT, H5P_DEFAULT);
                        obj2_id = H5Ropen_object(ref2_buf, H5P_DEFAULT, H5P_DEFAULT);
                        if((obj1_id  < 0) || (obj2_id  < 0)) {
                            H5TOOLS_INFO("H5Ropen_object H5R_DATASET_REGION2 failed");
                        }
                        else {
                            H5TOOLS_DEBUG("open_region - H5R_DATASET_REGION2");
                            if((region1_id = H5Ropen_region(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                if (h5tools_is_zero(ref1_buf, H5Tget_size(H5T_STD_REF))) {
                                    H5TOOLS_DEBUG("NULL H5R_DATASET_REGION2");
                                }
                                else {
                                    if((region2_id = H5Ropen_region(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) >= 0) {
                                        if (h5tools_is_zero(ref2_buf, H5Tget_size(H5T_STD_REF))) {
                                            H5TOOLS_DEBUG("NULL H5R_DATASET_REGION2");
                                        }
                                        else {
                                            nfound = diff_region(obj1_id, obj2_id, region1_id, region2_id, opts);
                                        }
                                        if(H5Sclose(region2_id) < 0)
                                            H5TOOLS_ERROR(H5DIFF_ERR, "H5Sclose H5R_DATASET_REGION2 failed");
                                    }
                                    else
                                        H5TOOLS_ERROR(H5DIFF_ERR, "H5Ropen_region H5R_DATASET_REGION2 failed");
                                } /* end else to if (h5tools_is_zero(... */
                                if(H5Sclose(region1_id) < 0)
                                    H5TOOLS_ERROR(H5DIFF_ERR, "H5Sclose H5R_DATASET_REGION2 failed");
                            }
                            else
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Ropen_region H5R_DATASET_REGION2 failed");
                            if (obj1_id >= 0)
                                if(H5Dclose(obj1_id) < 0) {
                                    opts->err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION2 failed");
                                }
                            if (obj2_id >= 0)
                                if(H5Dclose(obj2_id) < 0) {
                                    opts->err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Dclose H5R_DATASET_REGION2 failed");
                                }
                            if(H5Rdestroy(ref1_buf) < 0)
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_DATASET_REGION2 failed");
                            if(H5Rdestroy(ref2_buf) < 0)
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_DATASET_REGION2 failed");
                        }
                        break;
                    case H5R_ATTR:
                        {
                            char       name1[ATTR_NAME_MAX];
                            char       name2[ATTR_NAME_MAX];

                            H5TOOLS_DEBUG("ref_type is H5R_ATTR");
                            if((obj1_id = H5Ropen_attr(ref1_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                                parallel_print("Warning: Cannot open referenced attribute: attribute 1\n");
                                H5TOOLS_INFO("H5Ropen_attr object 1 failed");
                            }
                            if((obj2_id = H5Ropen_attr(ref2_buf, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                                parallel_print("Warning: Cannot open referenced attribute: attribute 2\n");
                                H5TOOLS_INFO("H5Ropen_attr object 2 failed");
                            }

                            if((obj1_id < 0) || (obj2_id < 0)) {
                                H5TOOLS_INFO("H5Ropen_attr H5R_ATTR failed");
                            }
                            else {
                                /* get name */
                                if(H5Aget_name(obj1_id, (size_t)ATTR_NAME_MAX, name1) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_name first attribute failed");
                                /* get name */
                                if(H5Aget_name(obj2_id, (size_t)ATTR_NAME_MAX, name2) < 0)
                                    H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "H5Aget_name second attribute failed");

                                H5TOOLS_DEBUG("H5R_ATTR diff_attr_data - name1=%s, name2=%s", name1, name2);
                                nfound = diff_attr_data(obj1_id, obj2_id, name1, name2, NULL, NULL, opts);
                            }

                            if(obj1_id >= 0)
                                if(H5Aclose(obj1_id) < 0) {
                                    opts->err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Aclose H5R_ATTR failed");
                                }
                            if(obj2_id >= 0)
                                if(H5Aclose(obj2_id) < 0) {
                                    opts->err_stat = H5DIFF_ERR;
                                    H5TOOLS_INFO("H5Aclose H5R_ATTR failed");
                                }
                            if(H5Rdestroy(ref2_buf) < 0)
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_ATTR failed");
                            if(H5Rdestroy(ref1_buf) < 0)
                                H5TOOLS_ERROR(H5DIFF_ERR, "H5Rdestroy H5R_ATTR failed");
                        }
                        break;
                    case H5R_BADTYPE:
                    case H5R_MAXTYPE:
                    default:
                        break;
                } /* end switch */
                H5TOOLS_DEBUG("diff_datum H5T_REFERENCE - H5T_STD_REF complete nfound:%d - errstat:%d", nfound, opts->err_stat);
            }
            /*-------------------------------------------------------------------------
             * H5T_STD_REF_DSETREG
             * Dataset region reference
             *-------------------------------------------------------------------------
             */
            else if (H5Tequal(m_type, H5T_STD_REF_DSETREG)) {
                /* if (type_size == H5R_DSET_REG_REF_BUF_SIZE) */
                hid_t region1_id = H5I_INVALID_HID;
                hid_t region2_id = H5I_INVALID_HID;

                H5TOOLS_INFO("H5T_STD_REF_DSETREG reference type");

                if ((obj1_id = H5Rdereference2(container1_id, H5P_DEFAULT, H5R_DATASET_REGION, _mem1)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rdereference2 object 1 failed");
                }
                if ((obj2_id = H5Rdereference2(container2_id, H5P_DEFAULT, H5R_DATASET_REGION, _mem2)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rdereference2 object 2 failed");
                }
                if ((region1_id = H5Rget_region(container1_id, H5R_DATASET_REGION, _mem1)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rget_region object 1 failed");
                }
                if ((region2_id = H5Rget_region(container2_id, H5R_DATASET_REGION, _mem2)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rget_region object 2 failed");
                }

                nfound = diff_region(obj1_id, obj2_id, region1_id, region2_id, opts);

                H5Oclose(obj1_id);
                H5Oclose(obj2_id);
                H5Sclose(region1_id);
                H5Sclose(region2_id);
            }/*dataset reference*/

            /*-------------------------------------------------------------------------
             * H5T_STD_REF_OBJ
             * Object references. get the type and OID of the referenced object
             *-------------------------------------------------------------------------
             */
            else if (H5Tequal(m_type, H5T_STD_REF_OBJ)) {
                /* if (type_size == H5R_OBJ_REF_BUF_SIZE) */
                H5O_type_t obj1_type;
                H5O_type_t obj2_type;

                H5TOOLS_INFO("H5T_STD_REF_OBJ reference type");

                if (H5Rget_obj_type2(container1_id, H5R_OBJECT, _mem1, &obj1_type) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rget_obj_type2 object 1 failed");
                }
                if (H5Rget_obj_type2(container2_id, H5R_OBJECT, _mem2, &obj2_type) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rget_obj_type2 object 2 failed");
                }

                /* check object type */
                if (obj1_type != obj2_type) {
                    parallel_print("Different object types referenced: <%s> and <%s>", obj1, obj2);
                    opts->not_cmp = 1;
                    H5TOOLS_GOTO_DONE(opts->err_stat);
                }

                if ((obj1_id = H5Rdereference2(container1_id, H5P_DEFAULT, H5R_OBJECT, _mem1)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rdereference2 object 1 failed");
                }
                if ((obj2_id = H5Rdereference2(container2_id, H5P_DEFAULT, H5R_OBJECT, _mem2)) < 0) {
                    opts->err_stat = H5DIFF_ERR;
                    H5TOOLS_INFO("H5Rdereference2 object 2 failed");
                }

                /* compare */
                if (obj1_type == H5O_TYPE_DATASET)
                    nfound = diff_datasetid(obj1_id, obj2_id, NULL, NULL, opts);
                else {
                    if (opts->m_verbose)
                        parallel_print("Warning: Comparison not possible of object types referenced: <%s> and <%s>\n", obj1, obj2);
                    opts->not_cmp = 1;
                }

                H5Oclose(obj1_id);
                H5Oclose(obj2_id);
            }/*object reference*/
        }/*is zero*/
        H5TOOLS_DEBUG("diff_datum H5T_REFERENCE complete");
        break;

    /*-------------------------------------------------------------------------
     * H5T_VLEN
     *-------------------------------------------------------------------------
     */
    case H5T_VLEN:
        {
            hid_t memb_type = H5I_INVALID_HID;

            H5TOOLS_DEBUG("diff_datum H5T_VLEN");
            /* get the VL sequences's base datatype for each element */
            memb_type = H5Tget_super(m_type);
            size = H5Tget_size(memb_type);

            /* get the number of sequence elements */
            nelmts = ((hvl_t *)((void *)mem1))->len;

            for (j = 0; j < nelmts; j++)
                nfound += diff_datum(((char *) (((hvl_t *)((void *)mem1))->p)) + j * size, ((char *) (((hvl_t *)((void *)mem2))->p)) + j * size, memb_type, index,      /* Extra (void *) cast to quiet "cast to create alignment" warning - 2019/07/05, QAK */
                        rank, dims, acc, pos, opts, obj1, obj2, container1_id, container2_id, ph, members);

            H5Tclose(memb_type);
        }
        break;

    /*-------------------------------------------------------------------------
     * H5T_INTEGER
     *-------------------------------------------------------------------------
     */
    case H5T_INTEGER:
        H5TOOLS_DEBUG("diff_datum H5T_INTEGER");
        type_sign = H5Tget_sign(m_type);
        /*-------------------------------------------------------------------------
         * H5T_NATIVE_SCHAR
         *-------------------------------------------------------------------------
         */
        if (type_size == 1 && type_sign != H5T_SGN_NONE) {
            char temp1_char;
            char temp2_char;

            if(type_size != sizeof(char))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not char size");
            HDmemcpy(&temp1_char, mem1, sizeof(char));
            HDmemcpy(&temp2_char, mem2, sizeof(char));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (ABS(temp1_char-temp2_char) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER(temp1_char, temp2_char);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER(temp1_char, temp2_char);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_char - temp2_char) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_char != temp2_char) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                }
                nfound++;
            }
        } /*H5T_NATIVE_SCHAR*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_UCHAR
         *-------------------------------------------------------------------------
         */
        else if (type_size == 1 && type_sign == H5T_SGN_NONE) {
            unsigned char temp1_uchar;
            unsigned char temp2_uchar;

            if(type_size != sizeof(unsigned char))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned char size");

            HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
            HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                    }
                    nfound++;
                }
                else if (per > opts->percent && PDIFF(temp1_uchar, temp2_uchar) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_uchar != temp2_uchar) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                }
                nfound++;
            }
        } /*H5T_NATIVE_UCHAR*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_SHORT
         *-------------------------------------------------------------------------
         */
        else if (type_size == 2 && type_sign != H5T_SGN_NONE) {
            short temp1_short;
            short temp2_short;

            if(type_size != sizeof(short))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not short size");

            HDmemcpy(&temp1_short, mem1, sizeof(short));
            HDmemcpy(&temp2_short, mem2, sizeof(short));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (ABS(temp1_short - temp2_short) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER(temp1_short, temp2_short);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER(temp1_short, temp2_short);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_short - temp2_short) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_short != temp2_short) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                }
                nfound++;
            }
        } /*H5T_NATIVE_SHORT*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_USHORT
         *-------------------------------------------------------------------------
         */
        else if (type_size == 2 && type_sign == H5T_SGN_NONE) {
            unsigned short temp1_ushort;
            unsigned short temp2_ushort;

            if(type_size != sizeof(unsigned short))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned short size");

            HDmemcpy(&temp1_ushort, mem1, sizeof(unsigned short));
            HDmemcpy(&temp2_ushort, mem2, sizeof(unsigned short));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (PDIFF(temp1_ushort, temp2_ushort) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                    }
                    nfound++;
                }
                else if (per > opts->percent && PDIFF(temp1_ushort, temp2_ushort) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_ushort != temp2_ushort) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                }
                nfound++;
            }
        } /*H5T_NATIVE_USHORT*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_INT
         *-------------------------------------------------------------------------
         */
        else if (type_size == 4 && type_sign != H5T_SGN_NONE) {
            int temp1_int;
            int temp2_int;

            if(type_size != sizeof(int))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not int size");

            HDmemcpy(&temp1_int, mem1, sizeof(int));
            HDmemcpy(&temp2_int, mem2, sizeof(int));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (ABS(temp1_int-temp2_int) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER(temp1_int, temp2_int);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER(temp1_int, temp2_int);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_int - temp2_int) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_int != temp2_int) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                }
                nfound++;
            }
        } /*H5T_NATIVE_INT*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_UINT
         *-------------------------------------------------------------------------
         */
        else if (type_size == 4 && type_sign == H5T_SGN_NONE) {
            unsigned int temp1_uint;
            unsigned int temp2_uint;

            if(type_size != sizeof(unsigned int))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned int size");

            HDmemcpy(&temp1_uint, mem1, sizeof(unsigned int));
            HDmemcpy(&temp2_uint, mem2, sizeof(unsigned int));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (PDIFF(temp1_uint, temp2_uint) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(UI_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER_UNSIGN(signed int, temp1_uint, temp2_uint);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(UI_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(UI_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER_UNSIGN(signed int, temp1_uint, temp2_uint);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(UI_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                    }
                    nfound++;
                }
                else if (per > opts->percent && PDIFF(temp1_uint,temp2_uint) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(UI_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_uint != temp2_uint) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(UI_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                }
                nfound++;
            }
        } /*H5T_NATIVE_UINT*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_LONG
         *-------------------------------------------------------------------------
         */
        else if (type_size == 8 && type_sign != H5T_SGN_NONE) {
            long temp1_long;
            long temp2_long;

            if(type_size != sizeof(long))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not long size");

            HDmemcpy(&temp1_long, mem1, sizeof(long));
            HDmemcpy(&temp2_long, mem2, sizeof(long));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (ABS(temp1_long-temp2_long) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER(temp1_long, temp2_long);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER(temp1_long, temp2_long);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_long-temp2_long) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_long != temp2_long) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                }
                nfound++;
            }
        } /*H5T_NATIVE_LONG*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_ULONG
         *-------------------------------------------------------------------------
         */
        else if (type_size == 8 && type_sign == H5T_SGN_NONE) {
            unsigned long temp1_ulong;
            unsigned long temp2_ulong;

            if(type_size != sizeof(unsigned long))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not unsigned long size");

            HDmemcpy(&temp1_ulong, mem1, sizeof(unsigned long));
            HDmemcpy(&temp2_ulong, mem2, sizeof(unsigned long));
            /* -d and !-p */
            if (opts->d && !opts->p) {
                if (PDIFF(temp1_ulong, temp2_ulong) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(ULI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                    }
                    nfound++;
                }
            }
            /* !-d and -p */
            else if (!opts->d && opts->p) {
                PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(ULI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
                    }
                    nfound++;
                }
            }
            /* -d and -p */
            else if (opts->d && opts->p) {
                PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                    }
                    nfound++;
                }
                else if (per > opts->percent && PDIFF(temp1_ulong,temp2_ulong) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(ULI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
                    }
                    nfound++;
                }
            }
            else if (temp1_ulong != temp2_ulong) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                }
                nfound++;
            }
        } /*H5T_NATIVE_ULONG*/
        break; /* H5T_INTEGER class */

    /*-------------------------------------------------------------------------
     * H5T_FLOAT
     *-------------------------------------------------------------------------
     */
    case H5T_FLOAT:
        /*-------------------------------------------------------------------------
         * H5T_NATIVE_FLOAT
         *-------------------------------------------------------------------------
         */
        H5TOOLS_DEBUG("diff_datum H5T_FLOAT");
        if (type_size == 4) {
            float temp1_float;
            float temp2_float;
            hbool_t isnan1 = FALSE;
            hbool_t isnan2 = FALSE;

            if(type_size != sizeof(float))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not float size");

            HDmemcpy(&temp1_float, mem1, sizeof(float));
            HDmemcpy(&temp2_float, mem2, sizeof(float));

            /* logic for detecting NaNs is different with opts -d, -p and no opts */

            /*-------------------------------------------------------------------------
             * -d and !-p
             *-------------------------------------------------------------------------
             */
            if (opts->d && !opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    if (ABS(temp1_float-temp2_float) > (float) opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * !-d and -p
             *-------------------------------------------------------------------------
             */
            else if (!opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_float, temp2_float);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P_NOTCOMP, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent && (double) ABS(temp1_float - temp2_float) > opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P, (double) temp1_float, (double) temp2_float,
                                    (double) ABS(temp1_float - temp2_float), (double) ABS(1 - temp2_float / temp1_float));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * -d and -p
             *-------------------------------------------------------------------------
             */
            else if (opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                    isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_float, temp2_float);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P_NOTCOMP, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P, (double) temp1_float, (double) temp2_float,
                                    (double) ABS(temp1_float - temp2_float), (double) ABS(1 - temp2_float / temp1_float));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * no -d and -p
             *-------------------------------------------------------------------------
             */
            else if (equal_float(temp1_float, temp2_float, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                }
                nfound++;
            }
        } /*H5T_NATIVE_FLOAT*/

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_DOUBLE
         *-------------------------------------------------------------------------
         */
        else if (type_size == 8) {
            double temp1_double;
            double temp2_double;
            hbool_t isnan1 = FALSE;
            hbool_t isnan2 = FALSE;

            if(type_size != sizeof(double))
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not double size");

            HDmemcpy(&temp1_double, mem1, sizeof(double));
            HDmemcpy(&temp2_double, mem2, sizeof(double));

            /* logic for detecting NaNs is different with opts -d, -p and no opts */
            /*-------------------------------------------------------------------------
             * -d and !-p
             *-------------------------------------------------------------------------
             */
            if (opts->d && !opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    if (ABS(temp1_double-temp2_double) > opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            } /* opts->d && !opts->p */
            /*-------------------------------------------------------------------------
             * !-d and -p
             *-------------------------------------------------------------------------
             */
            else if (!opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_double, temp2_double);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * -d and -p
             *-------------------------------------------------------------------------
             */
            else if (opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                * detect NaNs
                *-------------------------------------------------------------------------
                */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                    isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_double, temp2_double);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent &&
                    ABS(temp1_double-temp2_double) > opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(F_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                        }
                        nfound++;
                    }
                }
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * no -d and -p
             *-------------------------------------------------------------------------
             */
            else if (equal_double(temp1_double, temp2_double, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
        } /*H5T_NATIVE_DOUBLE*/

#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE

        /*-------------------------------------------------------------------------
         * H5T_NATIVE_LDOUBLE
         *-------------------------------------------------------------------------
         */
        else if (type_size == H5_SIZEOF_LONG_DOUBLE) {
            long double temp1_double;
            long double temp2_double;
            hbool_t     isnan1 = FALSE;
            hbool_t     isnan2 = FALSE;

            if(type_size != sizeof(long double)) {
                H5TOOLS_GOTO_ERROR(H5DIFF_ERR, "Type size is not long double size");
            }

            HDmemcpy(&temp1_double, mem1, sizeof(long double));
            HDmemcpy(&temp2_double, mem2, sizeof(long double));

            /* logic for detecting NaNs is different with options -d, -p and no options */

            /*-------------------------------------------------------------------------
             * -d and !-p
             *-------------------------------------------------------------------------
             */
            if (opts->d && !opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    if (ABS(temp1_double-temp2_double) > opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                } /* NaN */
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * !-d and -p
             *-------------------------------------------------------------------------
             */
            else if (!opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_LDOUBLE, &temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE, &temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_double,temp2_double);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(LD_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(LD_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                        }
                        nfound++;
                    }
                } /* NaN */
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * -d and -p
             *-------------------------------------------------------------------------
             */
            else if (opts->d && opts->p) {
                /*-------------------------------------------------------------------------
                 * detect NaNs
                 *-------------------------------------------------------------------------
                 */
                if (opts->do_nans) {
                    isnan1 = my_isnan(FLT_LDOUBLE, &temp1_double);
                    isnan2 = my_isnan(FLT_LDOUBLE, &temp2_double);
                }

                /* both not NaN, do the comparison */
                if (!isnan1 && !isnan2) {
                    PER(temp1_double,temp2_double);

                    if (not_comparable && !both_zero) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(LD_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                        }
                        nfound++;
                    }
                    else if (per > opts->percent && ABS(temp1_double-temp2_double) > opts->delta) {
                        if (print_data(opts)) {
                            print_pos(ph, 1, index, acc, pos, rank, dims, obj1, obj2);
                            parallel_print(SPACES);
                            parallel_print(LD_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                        }
                        nfound++;
                    }
                } /* NaN */
                /* only one is NaN, assume difference */
                else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /*-------------------------------------------------------------------------
             * no -d and -p
             *-------------------------------------------------------------------------
             */
            else if (equal_ldouble(temp1_double, temp2_double, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, index, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LD_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
        } /*H5T_NATIVE_LDOUBLE*/
#endif  /* H5_SIZEOF_LONG_DOUBLE */

        break; /* H5T_FLOAT class */

    } /* switch */

done:
    opts->err_stat = opts->err_stat | ret_value;

    H5TOOLS_DEBUG("diff_datum finish:%d - errstat:%d", nfound, opts->err_stat);

    H5TOOLS_ENDDEBUG("exit");
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: all_zero
 *
 * Purpose: Determines if memory is initialized to all zero bytes.
 *
 * Return: TRUE if all bytes are zero; FALSE otherwise
 *-------------------------------------------------------------------------
 */

static hbool_t all_zero(const void *_mem, size_t size) {
    const unsigned char *mem = (const unsigned char *) _mem;

    while (size-- > 0)
        if (mem[size])
            return FALSE;

    return TRUE;
}

/*-------------------------------------------------------------------------
 * Function: print_region_block
 *
 * Purpose: print start coordinates and opposite corner of a region block
 *
 * Return: void
 *-------------------------------------------------------------------------
 */

static
void print_region_block(int i, hsize_t *ptdata, int ndims) {
    int j;

    parallel_print("        ");
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : "   (", (unsigned long) ptdata[i * 2 * ndims + j]);
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : ")-(", (unsigned long) ptdata[i * 2 * ndims + j + ndims]);
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

static
void print_points(int i, hsize_t *ptdata, int ndims) {
    int j;

    parallel_print("              ");
    for (j = 0; j < ndims; j++)
        parallel_print("%s%lu", j ? "," : "(", (unsigned long) (ptdata[i * ndims + j]));
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

static hsize_t diff_region(hid_t obj1_id, hid_t obj2_id, hid_t region1_id, hid_t region2_id, diff_opt_t *opts)

{
    hssize_t nblocks1, npoints1;
    hssize_t nblocks2, npoints2;
    hsize_t  alloc_size;
    hsize_t *ptdata1 = NULL;
    hsize_t *ptdata2 = NULL;
    int      ndims1;
    int      ndims2;
    int      i, j;
    hsize_t  nfound_b = 0; /* block differences found */
    hsize_t  nfound_p = 0; /* point differences found */
    hsize_t  ret_value = 0;

    H5TOOLS_DEBUG("diff_region start");

    ndims1 = H5Sget_simple_extent_ndims(region1_id);
    ndims2 = H5Sget_simple_extent_ndims(region2_id);

    /*
     * These two functions fail if the region does not have blocks or points,
     * respectively. They do not currently know how to translate from one to
     * the other.
     */
    H5E_BEGIN_TRY {
        nblocks1 = H5Sget_select_hyper_nblocks(region1_id);
        nblocks2 = H5Sget_select_hyper_nblocks(region2_id);

        npoints1 = H5Sget_select_elem_npoints(region1_id);
        npoints2 = H5Sget_select_elem_npoints(region2_id);
    } H5E_END_TRY;
    H5TOOLS_DEBUG("diff_region blocks: 1=%ld-2=%ld", nblocks1, nblocks2);
    H5TOOLS_DEBUG("diff_region points: 1=%ld-2=%ld", npoints1, npoints2);

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
        HDassert(ndims1 > 0);
        alloc_size = (hsize_t) nblocks1 * (unsigned) ndims1 * 2 * sizeof(ptdata1[0]);
        HDassert(alloc_size == (hsize_t)((size_t )alloc_size)); /*check for overflow*/

        if((ptdata1 = (hsize_t *) HDmalloc((size_t )alloc_size)) == NULL) {
            opts->err_stat = H5DIFF_ERR;
            H5TOOLS_INFO("Buffer allocation failed");
        }
        else {
            H5_CHECK_OVERFLOW(nblocks1, hssize_t, hsize_t);
            H5Sget_select_hyper_blocklist(region1_id, (hsize_t) 0, (hsize_t) nblocks1, ptdata1);

            if((ptdata2 = (hsize_t *) HDmalloc((size_t )alloc_size)) == NULL) {
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_INFO("Buffer allocation failed");
            }
            else {
                H5_CHECK_OVERFLOW(nblocks2, hssize_t, hsize_t);
                H5Sget_select_hyper_blocklist(region2_id, (hsize_t) 0, (hsize_t) nblocks2, ptdata2);

                for (i = 0; i < nblocks1; i++) {
                    /* start coordinates and opposite corner */
                    for (j = 0; j < ndims1; j++) {
                        hsize_t start1, start2, end1, end2;

                        start1 = ptdata1[i * 2 * ndims1 + j];
                        start2 = ptdata2[i * 2 * ndims1 + j];
                        end1 = ptdata1[i * 2 * ndims1 + j + ndims1];
                        end2 = ptdata2[i * 2 * ndims1 + j + ndims1];
                        if (start1 != start2 || end1 != end2)
                            nfound_b++;
                    }
                }

                /* print differences if found */
                if (nfound_b && opts->m_verbose) {
                    H5O_info2_t oi1, oi2;
                    char *obj1_str = NULL, *obj2_str = NULL;

                    H5Oget_info3(obj1_id, &oi1, H5O_INFO_BASIC);
                    H5Oget_info3(obj2_id, &oi2, H5O_INFO_BASIC);

                    /* Convert object tokens into printable output */
                    H5Otoken_to_str(obj1_id, &oi1.token, &obj1_str);
                    H5Otoken_to_str(obj2_id, &oi2.token, &obj2_str);

                    parallel_print("Referenced dataset      %s            %s\n", obj1_str, obj2_str);
                    parallel_print( "------------------------------------------------------------\n");

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
                HDfree(ptdata2);
            } /* else ptdata2 */

            HDfree(ptdata1);
        } /* else ptdata1 */
    }

    /*-------------------------------------------------------------------------
     * compare point information
     *-------------------------------------------------------------------------
     */
    if (npoints1 > 0) {
        H5TOOLS_DEBUG("region compare points");
        alloc_size = (hsize_t) npoints1 * (unsigned) ndims1 * sizeof(ptdata1[0]);
        HDassert(alloc_size == (hsize_t)((size_t )alloc_size)); /*check for overflow*/

        if((ptdata1 = (hsize_t *) HDmalloc((size_t )alloc_size)) == NULL) {
            opts->err_stat = H5DIFF_ERR;
            H5TOOLS_INFO("Buffer allocation failed");
        }
        else {
            H5_CHECK_OVERFLOW(npoints1, hssize_t, hsize_t);
            H5Sget_select_elem_pointlist(region1_id, (hsize_t) 0, (hsize_t) npoints1, ptdata1);

            if((ptdata2 = (hsize_t *) HDmalloc((size_t )alloc_size)) == NULL) {
                opts->err_stat = H5DIFF_ERR;
                H5TOOLS_INFO("Buffer allocation failed");
            }
            else {
                H5_CHECK_OVERFLOW(npoints1, hssize_t, hsize_t);
                H5Sget_select_elem_pointlist(region2_id, (hsize_t) 0, (hsize_t) npoints2, ptdata2);

                for (i = 0; i < npoints1; i++) {
                    hsize_t pt1, pt2;

                    for (j = 0; j < ndims1; j++) {
                        pt1 = ptdata1[i * ndims1 + j];
                        pt2 = ptdata2[i * ndims1 + j];
                        if (pt1 != pt2)
                            nfound_p++;
                    }
                }

                if (nfound_p && opts->m_verbose) {
                    parallel_print("Region points\n");
                    for (i = 0; i < npoints1; i++) {
                        hsize_t pt1, pt2;
                        int diff_data = 0;

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
                HDfree(ptdata2);
            } /* else ptdata2 */

#if defined (H5DIFF_DEBUG)
            for (i = 0; i < npoints1; i++) {
                parallel_print("%sPt%lu: " , i ? "," : "", (unsigned long)i);

                for (j = 0; j < ndims1; j++)
                    parallel_print("%s%lu", j ? "," : "(", (unsigned long)(ptdata1[i * ndims1 + j]));

                parallel_print(")");
            }
            parallel_print("\n");
#endif

            HDfree(ptdata1);
        } /* else ptdata1 */
    }

    nfound_b = nfound_b / (unsigned) ndims1;
    nfound_p = nfound_p / (unsigned) ndims1;

    ret_value = nfound_p + nfound_b;

done:
    H5TOOLS_ENDDEBUG("exit with diffs:%d", ret_value);
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

static hsize_t character_compare(char *mem1, char *mem2, hsize_t i, size_t u,
        int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1, const char *obj2, int *ph)
{
    hsize_t nfound = 0; /* differences found */
    char    temp1_uchar;
    char    temp2_uchar;

    HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));
    H5TOOLS_DEBUG("character_compare start %d=%d",temp1_uchar,temp2_uchar);

    if (temp1_uchar != temp2_uchar) {
        if (print_data(opts)) {
            print_char_pos(ph, 0, i, u, acc, pos, rank, dims, obj1, obj2);
            parallel_print("            ");
            h5diff_print_char(temp1_uchar);
            parallel_print("            ");
            h5diff_print_char(temp2_uchar);
            parallel_print("\n");
        }
        nfound++;
    }
    H5TOOLS_DEBUG("character_compare finish");

    H5TOOLS_ENDDEBUG("exit: %d", nfound);
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

static hsize_t character_compare_opt(unsigned char *mem1, unsigned char *mem2,
        hsize_t i, int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1, const char *obj2, int *ph)
{
    hsize_t       nfound = 0; /* differences found */
    unsigned char temp1_uchar;
    unsigned char temp2_uchar;
    double        per;
    hbool_t       both_zero;

    HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
    HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));

    /* -d and !-p */

    if (opts->d && !opts->p) {
        if (PDIFF(temp1_uchar,temp2_uchar) > opts->delta) {
            if (print_data(opts)) {
                print_pos(ph, 0, i, acc, pos, rank, dims, obj1, obj2);
                parallel_print(SPACES);
                parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
            }
            nfound++;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);
        if (per > opts->percent) {
            if (print_data(opts)) {
                print_pos(ph, 1, i, acc, pos, rank, dims, obj1, obj2);
                parallel_print(SPACES);
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);
        if (per > opts->percent && PDIFF(temp1_uchar,temp2_uchar) > opts->delta) {
            if (print_data(opts)) {
                print_pos(ph, 1, i, acc, pos, rank, dims, obj1, obj2);
                parallel_print(SPACES);
                parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
            }
            nfound++;
        }
    }
    else if (temp1_uchar != temp2_uchar) {
        if (print_data(opts)) {
            print_pos(ph, 0, i, acc, pos, rank, dims, obj1, obj2);
            parallel_print(SPACES);
            parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
        }
        nfound++;
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_float
 *
 * Purpose:  diff a H5T_NATIVE_FLOAT type
 *
 * Return:   number of differences found
*
 *-------------------------------------------------------------------------
 */
static hsize_t diff_float(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims, hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)

{
    hsize_t nfound = 0; /* number of differences found */
    float   temp1_float;
    float   temp2_float;
    hsize_t i;
    double  per;
    hbool_t both_zero;
    hbool_t isnan1 = FALSE;
    hbool_t isnan2 = FALSE;

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */

    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_float, mem1, sizeof(float));
            HDmemcpy(&temp2_float, mem2, sizeof(float));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                if ((double) ABS(temp1_float - temp2_float) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                }
                nfound++;

            }
            mem1 += sizeof(float);
            mem2 += sizeof(float);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_float, mem1, sizeof(float));
            HDmemcpy(&temp2_float, mem2, sizeof(float));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
            }
            /* both not NaN, do the comparison */
            if ((!isnan1 && !isnan2)) {
                PER(temp1_float, temp2_float);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, (double) temp1_float, (double) temp2_float,
                                (double) ABS(temp1_float - temp2_float), (double) ABS(1 - temp2_float / temp1_float));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                }
                nfound++;

            }
            mem1 += sizeof(float);
            mem2 += sizeof(float);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_float, mem1, sizeof(float));
            HDmemcpy(&temp2_float, mem2, sizeof(float));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_FLOAT, &temp1_float);
                isnan2 = my_isnan(FLT_FLOAT, &temp2_float);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                PER(temp1_float, temp2_float);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                    }
                    nfound++;
                }
                else if (per > opts->percent && (double) ABS(temp1_float - temp2_float) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, (double) temp1_float, (double) temp2_float,
                                (double) ABS(temp1_float - temp2_float), (double) ABS(1 - temp2_float / temp1_float));
                    }
                    nfound++;
                }

            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                }
                nfound++;

            }
            mem1 += sizeof(float);
            mem2 += sizeof(float);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }

    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_float, mem1, sizeof(float));
            HDmemcpy(&temp2_float, mem2, sizeof(float));

            if (equal_float(temp1_float, temp2_float, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, (double) temp1_float, (double) temp2_float, (double) ABS(temp1_float - temp2_float));
                }
                nfound++;
            }

            mem1 += sizeof(float);
            mem2 += sizeof(float);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_double
 *
 * Purpose:  diff a H5T_NATIVE_DOUBLE type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_double(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)

{
    hsize_t nfound = 0; /* number of differences found */
    double  temp1_double;
    double  temp2_double;
    hsize_t i;
    double  per;
    hbool_t both_zero;
    hbool_t isnan1 = FALSE;
    hbool_t isnan2 = FALSE;

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */

    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(double));
            HDmemcpy(&temp2_double, mem2, sizeof(double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                if (ABS(temp1_double-temp2_double) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;

            }
            mem1 += sizeof(double);
            mem2 += sizeof(double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }

    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(double));
            HDmemcpy(&temp2_double, mem2, sizeof(double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
            }
            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                PER(temp1_double, temp2_double);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, temp1_double, temp2_double,
                                ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;

            }
            mem1 += sizeof(double);
            mem2 += sizeof(double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->d && opts->p) {

        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(double));
            HDmemcpy(&temp2_double, mem2, sizeof(double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_DOUBLE, &temp1_double);
                isnan2 = my_isnan(FLT_DOUBLE, &temp2_double);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                PER(temp1_double, temp2_double);

                if (not_comparable && !both_zero)  {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_double-temp2_double) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, temp1_double, temp2_double,
                                ABS(temp1_double - temp2_double), ABS(1 - temp2_double / temp1_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            mem1 += sizeof(double);
            mem2 += sizeof(double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(double));
            HDmemcpy(&temp2_double, mem2, sizeof(double));

            if (equal_double(temp1_double, temp2_double, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }

            mem1 += sizeof(double);
            mem2 += sizeof(double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ldouble
 *
 * Purpose:  diff a H5T_NATIVE_LDOUBLE type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
#if H5_SIZEOF_LONG_DOUBLE !=0

static hsize_t diff_ldouble(unsigned char *mem1,
        unsigned char *mem2,
        hsize_t nelmts,
        hsize_t hyper_start,
        int rank,
        hsize_t *dims,
        hsize_t *acc,
        hsize_t *pos,
        diff_opt_t *opts,
        const char *obj1,
        const char *obj2,
        int *ph)

{
    hsize_t     nfound = 0; /* number of differences found */
    long double temp1_double;
    long double temp2_double;
    hsize_t     i;
    double      per;
    hbool_t     both_zero;
    hbool_t     isnan1 = FALSE;
    hbool_t     isnan2 = FALSE;

    /*-------------------------------------------------------------------------
     * -d and !-p
     *-------------------------------------------------------------------------
     */

    if (opts->d && !opts->p) {
        for ( i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(long double));
            HDmemcpy(&temp2_double, mem2, sizeof(long double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_LDOUBLE,&temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE,&temp2_double);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                if (ABS(temp1_double-temp2_double) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;

            }
            mem1 += sizeof(long double);
            mem2 += sizeof(long double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }

    /*-------------------------------------------------------------------------
     * !-d and -p
     *-------------------------------------------------------------------------
     */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(long double));
            HDmemcpy(&temp2_double, mem2, sizeof(long double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_LDOUBLE, &temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE, &temp2_double);
            }
            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                PER(temp1_double, temp2_double);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
                else if (per > opts->percent) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, temp1_double, temp2_double,
                                ABS(temp1_double - temp2_double), ABS(1-temp2_double / temp1_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start+i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            mem1 += sizeof(long double);
            mem2 += sizeof(long double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * -d and -p
     *-------------------------------------------------------------------------
     */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_double, mem1, sizeof(long double));
            HDmemcpy(&temp2_double, mem2, sizeof(long double));

            /*-------------------------------------------------------------------------
             * detect NaNs
             *-------------------------------------------------------------------------
             */
            if (opts->do_nans) {
                isnan1 = my_isnan(FLT_LDOUBLE, &temp1_double);
                isnan2 = my_isnan(FLT_LDOUBLE, &temp2_double);
            }

            /* both not NaN, do the comparison */
            if (!isnan1 && !isnan2) {
                PER(temp1_double, temp2_double);

                if (not_comparable && !both_zero) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P_NOTCOMP, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                    }
                    nfound++;
                }
                else if (per > opts->percent && ABS(temp1_double - temp2_double) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(F_FORMAT_P, temp1_double, temp2_double, ABS(temp1_double - temp2_double), ABS(1-temp2_double / temp1_double));
                    }
                    nfound++;
                }
            }
            /* only one is NaN, assume difference */
            else if ((isnan1 && !isnan2) || (!isnan1 && isnan2)) {
                if (print_data(opts))  {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            mem1 += sizeof(long double);
            mem2 += sizeof(long double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* i */
    }
    /*-------------------------------------------------------------------------
     * no -d and -p
     *-------------------------------------------------------------------------
     */
    else {
        for (i = 0; i < nelmts; i++)  {
            HDmemcpy(&temp1_double, mem1, sizeof(long double));
            HDmemcpy(&temp2_double, mem2, sizeof(long double));

            if (equal_ldouble(temp1_double, temp2_double, opts) == FALSE) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(F_FORMAT, temp1_double, temp2_double, ABS(temp1_double - temp2_double));
                }
                nfound++;
            }
            mem1 += sizeof(long double);
            mem2 += sizeof(long double);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}
#endif /* H5_SIZEOF_LONG_DOUBLE */

/*-------------------------------------------------------------------------
 * Function: diff_schar
 *
 * Purpose:  diff a H5T_NATIVE_SCHAR type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_schar(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)

{
    hsize_t nfound = 0; /* number of differences found */
    char    temp1_char;
    char    temp2_char;
    hsize_t i;
    double  per;
    hbool_t both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_char, mem1, sizeof(char));
            HDmemcpy(&temp2_char, mem2, sizeof(char));

            if (ABS(temp1_char-temp2_char) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                }
                nfound++;
            }
            mem1 += sizeof(char);
            mem2 += sizeof(char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_char, mem1, sizeof(char));
            HDmemcpy(&temp2_char, mem2, sizeof(char));

            PER(temp1_char, temp2_char);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
                }
                nfound++;
            }
            mem1 += sizeof(char);
            mem2 += sizeof(char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_char, mem1, sizeof(char));
            HDmemcpy(&temp2_char, mem2, sizeof(char));

            PER(temp1_char, temp2_char);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                }
                nfound++;
            }
            else if (per > opts->percent && ABS(temp1_char-temp2_char) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_char, temp2_char, ABS(temp1_char - temp2_char), per);
                }
                nfound++;
            }
            mem1 += sizeof(char);
            mem2 += sizeof(char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_char, mem1, sizeof(char));
            HDmemcpy(&temp2_char, mem2, sizeof(char));

            if (temp1_char != temp2_char) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_char, temp2_char, ABS(temp1_char - temp2_char));
                }
                nfound++;
            }

            mem1 += sizeof(char);
            mem2 += sizeof(char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_uchar
 *
 * Purpose:  diff a H5T_NATIVE_UCHAR type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_uchar(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t       nfound = 0; /* number of differences found */
    unsigned char temp1_uchar;
    unsigned char temp2_uchar;
    hsize_t       i;
    double        per;
    hbool_t       both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
            HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));

            if (PDIFF(temp1_uchar,temp2_uchar) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                }
                nfound++;
            }
            mem1 += sizeof(unsigned char);
            mem2 += sizeof(unsigned char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
            HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));

            PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned char);
            mem2 += sizeof(unsigned char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
            HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));

            PER_UNSIGN(signed char, temp1_uchar, temp2_uchar);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                }
                nfound++;
            }
            else if (per > opts->percent && PDIFF(temp1_uchar,temp2_uchar) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned char);
            mem2 += sizeof(unsigned char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uchar, mem1, sizeof(unsigned char));
            HDmemcpy(&temp2_uchar, mem2, sizeof(unsigned char));

            if (temp1_uchar != temp2_uchar) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_uchar, temp2_uchar, PDIFF(temp1_uchar, temp2_uchar));
                }
                nfound++;
            }

            mem1 += sizeof(unsigned char);
            mem2 += sizeof(unsigned char);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_short
 *
 * Purpose:  diff a H5T_NATIVE_SHORT type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_short(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t nfound = 0; /* number of differences found */
    short   temp1_short;
    short   temp2_short;
    hsize_t i;
    double  per;
    hbool_t both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_short, mem1, sizeof(short));
            HDmemcpy(&temp2_short, mem2, sizeof(short));

            if (ABS(temp1_short-temp2_short) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                }
                nfound++;
            }
            mem1 += sizeof(short);
            mem2 += sizeof(short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_short, mem1, sizeof(short));
            HDmemcpy(&temp2_short, mem2, sizeof(short));

            PER(temp1_short, temp2_short);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
                }
                nfound++;
            }
            mem1 += sizeof(short);
            mem2 += sizeof(short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_short, mem1, sizeof(short));
            HDmemcpy(&temp2_short, mem2, sizeof(short));

            PER(temp1_short, temp2_short);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                }
                nfound++;
            }
            else if (per > opts->percent && ABS(temp1_short-temp2_short) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_short, temp2_short, ABS(temp1_short - temp2_short), per);
                }
                nfound++;
            }
            mem1 += sizeof(short);
            mem2 += sizeof(short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_short, mem1, sizeof(short));
            HDmemcpy(&temp2_short, mem2, sizeof(short));

            if (temp1_short != temp2_short) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_short, temp2_short, ABS(temp1_short - temp2_short));
                }
                nfound++;
            }

            mem1 += sizeof(short);
            mem2 += sizeof(short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ushort
 *
 * Purpose:  diff a H5T_NATIVE_USHORT type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_ushort(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)

{
    hsize_t        nfound = 0; /* number of differences found */
    unsigned short temp1_ushort;
    unsigned short temp2_ushort;
    hsize_t        i;
    double         per;
    hbool_t        both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ushort, mem1, sizeof(unsigned short));
            HDmemcpy(&temp2_ushort, mem2, sizeof(unsigned short));

            if (PDIFF(temp1_ushort,temp2_ushort) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                }
                nfound++;
            }
            mem1 += sizeof(unsigned short);
            mem2 += sizeof(unsigned short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ushort, mem1, sizeof(unsigned short));
            HDmemcpy(&temp2_ushort, mem2, sizeof(unsigned short));

            PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

            if (not_comparable && !both_zero)  {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned short);
            mem2 += sizeof(unsigned short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ushort, mem1, sizeof(unsigned short));
            HDmemcpy(&temp2_ushort, mem2, sizeof(unsigned short));

            PER_UNSIGN(signed short, temp1_ushort, temp2_ushort);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                }
                nfound++;
            }
            else if (per > opts->percent && PDIFF(temp1_ushort,temp2_ushort) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned short);
            mem2 += sizeof(unsigned short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ushort, mem1, sizeof(unsigned short));
            HDmemcpy(&temp2_ushort, mem2, sizeof(unsigned short));

            if (temp1_ushort != temp2_ushort) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_ushort, temp2_ushort, PDIFF(temp1_ushort, temp2_ushort));
                }
                nfound++;
            }

            mem1 += sizeof(unsigned short);
            mem2 += sizeof(unsigned short);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function:  diff_int
 *
 * Purpose:   diff a H5T_NATIVE_INT type
 *
 * Return:    number of differences found
  *-------------------------------------------------------------------------
 */
static hsize_t diff_int(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t nfound = 0; /* number of differences found */
    int     temp1_int;
    int     temp2_int;
    hsize_t i;
    double  per;
    hbool_t both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_int, mem1, sizeof(int));
            HDmemcpy(&temp2_int, mem2, sizeof(int));

            if (ABS(temp1_int-temp2_int) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                }
                nfound++;
            }
            mem1 += sizeof(int);
            mem2 += sizeof(int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_int, mem1, sizeof(int));
            HDmemcpy(&temp2_int, mem2, sizeof(int));

            PER(temp1_int, temp2_int);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
                }
                nfound++;
            }
            mem1 += sizeof(int);
            mem2 += sizeof(int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_int, mem1, sizeof(int));
            HDmemcpy(&temp2_int, mem2, sizeof(int));

            PER(temp1_int, temp2_int);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                }
                nfound++;
            }
            else if (per > opts->percent && ABS(temp1_int-temp2_int) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_int, temp2_int, ABS(temp1_int - temp2_int), per);
                }
                nfound++;
            }
            mem1 += sizeof(int);
            mem2 += sizeof(int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_int, mem1, sizeof(int));
            HDmemcpy(&temp2_int, mem2, sizeof(int));

            if (temp1_int != temp2_int) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_int, temp2_int, ABS(temp1_int - temp2_int));
                }
                nfound++;
            }

            mem1 += sizeof(int);
            mem2 += sizeof(int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */

    }
    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_uint
 *
 * Purpose:  diff a H5T_NATIVE_UINT type
 *
 * Return:  number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_uint(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t nfound = 0; /* number of differences found */
    unsigned int temp1_uint;
    unsigned int temp2_uint;
    hsize_t i;
    double per;
    hbool_t both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uint, mem1, sizeof(unsigned int));
            HDmemcpy(&temp2_uint, mem2, sizeof(unsigned int));

            if (PDIFF(temp1_uint,temp2_uint) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                }
                nfound++;
            }
            mem1 += sizeof(unsigned int);
            mem2 += sizeof(unsigned int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uint, mem1, sizeof(unsigned int));
            HDmemcpy(&temp2_uint, mem2, sizeof(unsigned int));

            PER_UNSIGN(signed int, temp1_uint, temp2_uint);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned int);
            mem2 += sizeof(unsigned int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uint, mem1, sizeof(unsigned int));
            HDmemcpy(&temp2_uint, mem2, sizeof(unsigned int));

            PER_UNSIGN(signed int, temp1_uint, temp2_uint);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P_NOTCOMP, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                }
                nfound++;
            }
            else if (per > opts->percent
                    && PDIFF(temp1_uint,temp2_uint) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT_P, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned int);
            mem2 += sizeof(unsigned int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_uint, mem1, sizeof(unsigned int));
            HDmemcpy(&temp2_uint, mem2, sizeof(unsigned int));

            if (temp1_uint != temp2_uint) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(I_FORMAT, temp1_uint, temp2_uint, PDIFF(temp1_uint, temp2_uint));
                }
                nfound++;
            }

            mem1 += sizeof(unsigned int);
            mem2 += sizeof(unsigned int);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_long
 *
 * Purpose:  diff a H5T_NATIVE_LONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_long(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t nfound = 0; /* number of differences found */
    long temp1_long;
    long temp2_long;
    hsize_t i;
    double per;
    hbool_t both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            for (i = 0; i < nelmts; i++) {
                HDmemcpy(&temp1_long, mem1, sizeof(long));
                HDmemcpy(&temp2_long, mem2, sizeof(long));

                if (ABS(temp1_long-temp2_long) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                    }
                    nfound++;
                }
                mem1 += sizeof(long);
                mem2 += sizeof(long);
                if (opts->n && nfound >= opts->count)
                    return nfound;
            }
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_long, mem1, sizeof(long));
            HDmemcpy(&temp2_long, mem2, sizeof(long));

            PER(temp1_long, temp2_long);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
                }
                nfound++;
            }
            mem1 += sizeof(long);
            mem2 += sizeof(long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_long, mem1, sizeof(long));
            HDmemcpy(&temp2_long, mem2, sizeof(long));

            PER(temp1_long, temp2_long);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P_NOTCOMP, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                }
                nfound++;
            }
            else if (per > opts->percent && ABS(temp1_long-temp2_long) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P, temp1_long, temp2_long, ABS(temp1_long - temp2_long), per);
                }
                nfound++;
            }
            mem1 += sizeof(long);
            mem2 += sizeof(long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_long, mem1, sizeof(long));
            HDmemcpy(&temp2_long, mem2, sizeof(long));

            if (temp1_long != temp2_long) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT, temp1_long, temp2_long, ABS(temp1_long - temp2_long));
                }
                nfound++;
            }

            mem1 += sizeof(long);
            mem2 += sizeof(long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ulong
 *
 * Purpose:  diff a H5T_NATIVE_ULONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_ulong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t       nfound = 0; /* number of differences found */
    unsigned long temp1_ulong;
    unsigned long temp2_ulong;
    hsize_t       i;
    double        per;
    hbool_t       both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            for (i = 0; i < nelmts; i++) {
                HDmemcpy(&temp1_ulong, mem1, sizeof(unsigned long));
                HDmemcpy(&temp2_ulong, mem2, sizeof(unsigned long));

                if (PDIFF(temp1_ulong,temp2_ulong) > opts->delta) {
                    if (print_data(opts)) {
                        print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                        parallel_print(SPACES);
                        parallel_print(LI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                    }
                    nfound++;
                }
                mem1 += sizeof(unsigned long);
                mem2 += sizeof(unsigned long);
                if (opts->n && nfound >= opts->count)
                    return nfound;
            }
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ulong, mem1, sizeof(unsigned long));
            HDmemcpy(&temp2_ulong, mem2, sizeof(unsigned long));

            PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned long);
            mem2 += sizeof(unsigned long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ulong, mem1, sizeof(unsigned long));
            HDmemcpy(&temp2_ulong, mem2, sizeof(unsigned long));

            PER_UNSIGN(signed long, temp1_ulong, temp2_ulong);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULI_FORMAT_P_NOTCOMP, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                }
                nfound++;
            }
            else if (per > opts->percent
                    && PDIFF(temp1_ulong,temp2_ulong) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT_P, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong), per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned long);
            mem2 += sizeof(unsigned long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ulong, mem1, sizeof(unsigned long));
            HDmemcpy(&temp2_ulong, mem2, sizeof(unsigned long));

            if (temp1_ulong != temp2_ulong) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LI_FORMAT, temp1_ulong, temp2_ulong, PDIFF(temp1_ulong, temp2_ulong));
                }
                nfound++;
            }

            mem1 += sizeof(unsigned long);
            mem2 += sizeof(unsigned long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_llong
 *
 * Purpose:  diff a H5T_NATIVE_LLONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_llong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims,
        hsize_t *acc, hsize_t *pos, diff_opt_t *opts, const char *obj1,
        const char *obj2, int *ph)
{
    hsize_t   nfound = 0; /* number of differences found */
    long long temp1_llong;
    long long temp2_llong;
    hsize_t   i;
    double    per;
    hbool_t   both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_llong, mem1, sizeof(long long));
            HDmemcpy(&temp2_llong, mem2, sizeof(long long));

            if (ABS( temp1_llong-temp2_llong) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LLI_FORMAT, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
                }
                nfound++;
            }
            mem1 += sizeof(long long);
            mem2 += sizeof(long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_llong, mem1, sizeof(long long));
            HDmemcpy(&temp2_llong, mem2, sizeof(long long));

            PER(temp1_llong, temp2_llong);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                parallel_print(LLI_FORMAT_P_NOTCOMP, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
            }
            nfound++;
        }
        else if (per > opts->percent) {
            if (print_data(opts)) {
                print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                parallel_print(SPACES);
                parallel_print(LLI_FORMAT_P, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong),per);
            }
            nfound++;
        }
        mem1 += sizeof(long long);
        mem2 += sizeof(long long);
        if (opts->n && nfound >= opts->count)
            return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_llong, mem1, sizeof(long long));
            HDmemcpy(&temp2_llong, mem2, sizeof(long long));

            PER(temp1_llong, temp2_llong);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LLI_FORMAT_P_NOTCOMP, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
                }
                nfound++;
            }
            else if (per > opts->percent
                    && ABS(temp1_llong-temp2_llong) > opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LLI_FORMAT_P, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong),per);
                }
                nfound++;
            }
            mem1 += sizeof(long long);
            mem2 += sizeof(long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_llong, mem1, sizeof(long long));
            HDmemcpy(&temp2_llong, mem2, sizeof(long long));

            if (temp1_llong != temp2_llong) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(LLI_FORMAT, temp1_llong, temp2_llong, ABS(temp1_llong - temp2_llong));
                }
                nfound++;
            }

            mem1 += sizeof(long long);
            mem2 += sizeof(long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function: diff_ullong
 *
 * Purpose:  diff a H5T_NATIVE_ULLONG type
 *
 * Return:   number of differences found
 *-------------------------------------------------------------------------
 */
static hsize_t diff_ullong(unsigned char *mem1, unsigned char *mem2,
        hsize_t nelmts, hsize_t hyper_start, int rank, hsize_t *dims, hsize_t *acc,
        hsize_t *pos, diff_opt_t *opts, const char *obj1, const char *obj2, int *ph)

{
    hsize_t            nfound = 0; /* number of differences found */
    unsigned long long temp1_ullong;
    unsigned long long temp2_ullong;
    hsize_t            i;
    float              f1, f2;
    double             per;
    hbool_t            both_zero;

    /* -d and !-p */
    if (opts->d && !opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ullong, mem1, sizeof(unsigned long long));
            HDmemcpy(&temp2_ullong, mem2, sizeof(unsigned long long));

            if (PDIFF(temp1_ullong,temp2_ullong) > (unsigned long long) opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
                }
                nfound++;
            }
            mem1 += sizeof(unsigned long long);
            mem2 += sizeof(unsigned long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* !-d and -p */
    else if (!opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ullong, mem1, sizeof(unsigned long long));
            HDmemcpy(&temp2_ullong, mem2, sizeof(unsigned long long));

            ull2float(temp1_ullong, &f1);
            ull2float(temp2_ullong, &f2);
            PER(f1, f2);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT_P_NOTCOMP,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
                }
                nfound++;
            }
            else if (per > opts->percent) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT_P,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong),per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned long long);
            mem2 += sizeof(unsigned long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    /* -d and -p */
    else if (opts->d && opts->p) {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ullong, mem1, sizeof(unsigned long long));
            HDmemcpy(&temp2_ullong, mem2, sizeof(unsigned long long));

            ull2float(temp1_ullong, &f1);
            ull2float(temp2_ullong, &f2);
            PER(f1, f2);

            if (not_comparable && !both_zero) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT_P_NOTCOMP,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
                }
                nfound++;
            }
            else if (per > opts->percent
                    && PDIFF(temp1_ullong,temp2_ullong) > (unsigned long long) opts->delta) {
                if (print_data(opts)) {
                    print_pos(ph, 1, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT_P,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong),per);
                }
                nfound++;
            }
            mem1 += sizeof(unsigned long long);
            mem2 += sizeof(unsigned long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        }
    }
    else {
        for (i = 0; i < nelmts; i++) {
            HDmemcpy(&temp1_ullong, mem1, sizeof(unsigned long long));
            HDmemcpy(&temp2_ullong, mem2, sizeof(unsigned long long));

            if (temp1_ullong != temp2_ullong) {
                if (print_data(opts)) {
                    print_pos(ph, 0, hyper_start + i, acc, pos, rank, dims, obj1, obj2);
                    parallel_print(SPACES);
                    parallel_print(ULLI_FORMAT,temp1_ullong,temp2_ullong,PDIFF(temp1_ullong,temp2_ullong));
                }
                nfound++;
            }

            mem1 += sizeof(unsigned long long);
            mem2 += sizeof(unsigned long long);
            if (opts->n && nfound >= opts->count)
                return nfound;
        } /* nelmts */
    }

    return nfound;
}

/*-------------------------------------------------------------------------
 * Function:    ull2float
 *
 * Purpose:     convert unsigned long long to float
 *-------------------------------------------------------------------------
 */
static
int ull2float(unsigned long long ull_value, float *f_value)
{
    hid_t          dxpl_id = H5I_INVALID_HID;
    unsigned char *buf = NULL;
    size_t         src_size;
    size_t         dst_size;
    int            ret_value = 0;

    H5TOOLS_DEBUG("ull2float start");
    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Pcreate failed");

    src_size = H5Tget_size(H5T_NATIVE_ULLONG);
    dst_size = H5Tget_size(H5T_NATIVE_FLOAT);
    if((buf = (unsigned char*) HDcalloc((size_t )1, MAX(src_size, dst_size))) == NULL)
        H5TOOLS_GOTO_ERROR(FAIL, "Could not allocate buffer for dims");

    HDmemcpy(buf, &ull_value, src_size);

    /* do conversion */
    if (H5Tconvert(H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT, (size_t) 1, buf, NULL, dxpl_id) < 0)
        H5TOOLS_GOTO_ERROR(FAIL, "H5Tconvert failed");

    HDmemcpy(f_value, buf, dst_size);

done:
    H5E_BEGIN_TRY {
        H5Pclose(dxpl_id);
    } H5E_END_TRY;

    if (buf)
        HDfree(buf);

    H5TOOLS_DEBUG("ull2float finish");

    H5TOOLS_ENDDEBUG("exit");
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    equal_double
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */
static hbool_t equal_double(double value, double expected, diff_opt_t *opts) {
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
        * detect NaNs
        *-------------------------------------------------------------------------
        */
        hbool_t isnan1 = my_isnan(FLT_DOUBLE, &value);
        hbool_t isnan2 = my_isnan(FLT_DOUBLE, &expected);

        /*-------------------------------------------------------------------------
        * we consider NaN == NaN to be true
        *-------------------------------------------------------------------------
        */
        if (isnan1 && isnan2)
            return TRUE;

        /*-------------------------------------------------------------------------
        * one is a NaN, do not compare but assume difference
        *-------------------------------------------------------------------------
        */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return FALSE;
    }

    if (value == expected)
        return TRUE;

    if (opts->use_system_epsilon)
        if (ABS((value-expected)) < DBL_EPSILON)
            return TRUE;

    return FALSE;
}

/*-------------------------------------------------------------------------
 * Function:    equal_ldouble
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */

#if H5_SIZEOF_LONG_DOUBLE !=0
static
hbool_t equal_ldouble(long double value, long double expected, diff_opt_t *opts)
{
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
         * detect NaNs
         *-------------------------------------------------------------------------
         */
        hbool_t isnan1 = my_isnan(FLT_LDOUBLE, &value);
        hbool_t isnan2 = my_isnan(FLT_LDOUBLE, &expected);

        /*-------------------------------------------------------------------------
         * we consider NaN == NaN to be true
         *-------------------------------------------------------------------------
         */
        if (isnan1 && isnan2)
            return TRUE;

        /*-------------------------------------------------------------------------
         * one is a NaN, do not compare but assume difference
         *-------------------------------------------------------------------------
         */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return FALSE;
    }

    if (value == expected)
        return TRUE;

    if (opts->use_system_epsilon)
        if (ABS((value-expected)) < DBL_EPSILON)
            return TRUE;

    return FALSE;
}

#endif /* #if H5_SIZEOF_LONG_DOUBLE !=0 */

/*-------------------------------------------------------------------------
 * Function:    equal_float
 *
 * Purpose:     use a absolute error formula to deal with floating point uncertainty
 *-------------------------------------------------------------------------
 */
static hbool_t equal_float(float value, float expected, diff_opt_t *opts) {
    if (opts->do_nans) {
        /*-------------------------------------------------------------------------
        * detect NaNs
        *-------------------------------------------------------------------------
        */
        hbool_t isnan1 = my_isnan(FLT_FLOAT, &value);
        hbool_t isnan2 = my_isnan(FLT_FLOAT, &expected);

        /*-------------------------------------------------------------------------
         * we consider NaN == NaN to be true
         *-------------------------------------------------------------------------
         */
        if (isnan1 && isnan2)
            return TRUE;

        /*-------------------------------------------------------------------------
         * one is a NaN, do not compare but assume difference
         *-------------------------------------------------------------------------
         */
        if ((isnan1 && !isnan2) || (!isnan1 && isnan2))
            return FALSE;
    }

    if (value == expected)
        return TRUE;

    if (opts->use_system_epsilon)
        if (ABS( (value-expected) ) < FLT_EPSILON)
            return TRUE;

    return FALSE;
}

/*-------------------------------------------------------------------------
 * Function:  my_isnan
 *
 * Purpose:   Determines whether VAL points to NaN.
 *
 * Return:    TRUE or FALSE
 *-------------------------------------------------------------------------
 */
static hbool_t my_isnan(dtype_t type, void *val) {
    hbool_t retval = FALSE;
    char s[256];

    if (FLT_FLOAT == type) {
        float x;

        HDmemcpy(&x, val, sizeof(float));
        retval = (x != x);
    }
    else if (FLT_DOUBLE == type) {
        double x;

        HDmemcpy(&x, val, sizeof(double));
        retval = (x != x);
    }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE != 0
    else if (FLT_LDOUBLE == type) {
        long double x;

        HDmemcpy(&x, val, sizeof(long double));
        retval = (x!=x);
    }
#endif
    else
        return FALSE;

    /*
    * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
    * the result contains a NaN string.
    */
    if (!retval) {
        if (FLT_FLOAT == type) {
            float x;

            HDmemcpy(&x, val, sizeof(float));
            HDsnprintf(s, sizeof(s), "%g", (double) x);
        }
        else if (FLT_DOUBLE == type) {
            double x;

            HDmemcpy(&x, val, sizeof(double));
            HDsnprintf(s, sizeof(s), "%g", x);
        }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE && H5_SIZEOF_LONG_DOUBLE != 0
        else if (FLT_LDOUBLE == type) {
            long double x;

            HDmemcpy(&x, val, sizeof(long double));
            HDsnprintf(s, sizeof(s), "%Lg", x);
        }
#endif
        else
            return FALSE;

        if (HDstrstr(s, "NaN") ||
                HDstrstr(s, "NAN") ||
                HDstrstr(s, "nan") ||
                HDstrstr(s, "-1.#IND")) {
            retval = TRUE;
        }
    }

    return retval;
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
static
int print_data(diff_opt_t *opts)
{
    return ((opts->m_report || opts->m_verbose) && !opts->m_quiet) ? 1 : 0;
}

/*-------------------------------------------------------------------------
 * Function: print_header
 *
 * Purpose:  print header for difference
 *-------------------------------------------------------------------------
 */
static
void print_header(int pp, /* print percentage */
        int rank, hsize_t *dims, const char *obj1, const char *obj2)
{
    /* print header */
    parallel_print("%-16s", "size:");
    print_dimensions(rank, dims);
    parallel_print("%-11s", "");
    print_dimensions(rank, dims);
    parallel_print("\n");

    if (pp) {
        parallel_print("%-15s %-15s %-15s %-15s %-15s\n", "position",
                (obj1 != NULL) ? obj1 : " ", (obj2 != NULL) ? obj2 : " ", "difference", "relative");
        parallel_print(
                "------------------------------------------------------------------------\n");
    }
    else {
        parallel_print("%-15s %-15s %-15s %-20s\n", "position",
                (obj1 != NULL) ? obj1 : " ", (obj2 != NULL) ? obj2 : " ", "difference");
        parallel_print(
                "------------------------------------------------------------\n");
    }
}

/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose:  print in matrix notation, converting from an array index position
 *-------------------------------------------------------------------------
 */
static
void print_pos(int *ph, /* print header */
        int pp, /* print percentage */
        hsize_t curr_pos, hsize_t *acc, hsize_t *pos, int rank, hsize_t *dims,
        const char *obj1, const char *obj2)
{
    int i;

    /* print header */
    if (*ph == 1) {
        *ph = 0;

        print_header(pp, rank, dims, obj1, obj2);
    } /* end print header */

    for (i = 0; i < rank; i++) {
        pos[i] = curr_pos / acc[i];
        curr_pos -= acc[i] * pos[i];
    }
    HDassert(curr_pos == 0);

    if (rank > 0) {
        parallel_print("[ ");
        for (i = 0; i < rank; i++) {
            parallel_print(HSIZE_T_FORMAT, (unsigned long long)pos[i]);
            parallel_print(" ");
        }
        parallel_print("]");
    }
    else
        parallel_print("      ");
}

/*-------------------------------------------------------------------------
 * Function: print_char_pos
 *
 * Purpose:  print character position in string
 *-------------------------------------------------------------------------
 */
static
void print_char_pos(int *ph, /* print header */
        int pp, /* print percentage */
        hsize_t curr_pos, size_t u, hsize_t *acc, hsize_t *pos, int rank, hsize_t *dims,
        const char *obj1, const char *obj2)
{
    int i;

    /* print header */
    if (*ph == 1) {
        *ph = 0;

        print_header(pp, rank, dims, obj1, obj2);
    } /* end print header */

    for (i = 0; i < rank; i++) {
        pos[i] = curr_pos / acc[i];
        curr_pos -= acc[i] * pos[i];
    }
    HDassert(curr_pos == 0);

    parallel_print("[ ");
    if (rank > 0) {
        for (i = 0; i < rank; i++) {
            parallel_print(HSIZE_T_FORMAT, (unsigned long long)pos[i]);
            parallel_print(" ");
        }

    }
    else
        parallel_print("%zu", u);

    parallel_print("]");
}

/*-------------------------------------------------------------------------
 * Function: h5diff_print_char. Adapted from h5tools_print_char
 *
 * Purpose:  Print a char
 *-------------------------------------------------------------------------
 */
static void h5diff_print_char(char ch)
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
static void get_member_types(hid_t tid, mcomp_t *members)
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
        members->n = (unsigned) nmembs;

        members->ids = (hid_t *) HDcalloc((size_t )members->n, sizeof(hid_t));
        members->offsets = (size_t *) HDcalloc((size_t )members->n, sizeof(size_t));
        members->m = (mcomp_t **) HDcalloc((size_t )members->n, sizeof(mcomp_t *));

        for (u = 0; u < members->n; u++) {
            members->ids[u] = H5Tget_member_type(tid, u);
            members->offsets[u] = H5Tget_member_offset(tid, u);
            members->m[u] = (mcomp_t *) HDmalloc(sizeof(mcomp_t));
            HDmemset(members->m[u], 0, sizeof(mcomp_t));
            get_member_types(members->ids[u], members->m[u]);
        }
    }

    return;
}

/*-------------------------------------------------------------------------
 * added to improve performance for compound datasets
 * clean and close compound members.
 *-------------------------------------------------------------------------
 */
static void close_member_types(mcomp_t *members)
{
    unsigned u;

    if (!members || members->n <= 0 || !members->ids)
        return;

    for (u = 0; u < members->n; u++) {
        if (members->m[u]) {
            close_member_types(members->m[u]);
            HDfree(members->m[u]);
        }
        H5Tclose(members->ids[u]);
    }

    HDfree(members->m);
    HDfree(members->ids);
    HDfree(members->offsets);
}

