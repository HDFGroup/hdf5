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
 * Purpose:     Tests the data type interface (H5T)
 */

#include "h5test.h"

/* Number of elements in each random test */
#define NTESTELEM 10000

/* Epsilon for floating-point comparisons */
#define FP_EPSILON 0.000001F

/*
 * Offset from aligned memory returned by malloc().  This can be used to test
 * that type conversions handle non-aligned buffers correctly.
 */
#define ALIGNMENT 1

/*
 * Define if you want to test alignment code on a machine that doesn't
 * normally require alignment. When set, all native data types must be aligned
 * on a byte boundary equal to the data size.
 */
#define TEST_ALIGNMENT

/* Alignment test stuff */
#ifdef TEST_ALIGNMENT
#define H5T_FRIEND /*suppress error about including H5Tpkg      */
#include "H5Tpkg.h"
#endif
#define SET_ALIGNMENT(TYPE, VAL) H5T_NATIVE_##TYPE##_ALIGN_g = MAX(H5T_NATIVE_##TYPE##_ALIGN_g, VAL)

static const char *FILENAME[] = {"dt_arith1", "dt_arith2", "dt_arith3", NULL};

/*
 * Count up or down depending on whether the machine is big endian or little
 * endian.  If local variable `endian' is H5T_ORDER_BE then the result will
 * be I, otherwise the result will be Z-(I+1).
 */
#define ENDIAN(Z, I, E) (H5T_ORDER_BE == (E) ? (I) : (Z) - ((I) + 1))

typedef enum dtype_t {
    INT_SCHAR,
    INT_UCHAR,
    INT_SHORT,
    INT_USHORT,
    INT_INT,
    INT_UINT,
    INT_LONG,
    INT_ULONG,
    INT_LLONG,
    INT_ULLONG,
    FLT_FLOAT16,
    FLT_FLOAT,
    FLT_DOUBLE,
    FLT_LDOUBLE,
    FLT_COMPLEX,
    DBL_COMPLEX,
    LDBL_COMPLEX,
    OTHER
} dtype_t;

typedef enum conv_func_ret_t {
    CONV_ERROR = -1, /* Failure during conversion */
    CONV_SUCCESS,    /* Converted without overflow or underflow */
    CONV_OVERFLOW,   /* Converted with overflow */
    CONV_UNDERFLOW,  /* Converted with underflow */

    /* Complex number overflow/underflow values */
    CONV_OVERFLOW_REAL,  /* Converted with overflow on real part */
    CONV_OVERFLOW_IMAG,  /* Converted with overflow on imaginary part */
    CONV_OVERFLOW_BOTH,  /* Converted with overflow on real and imaginary parts */
    CONV_UNDERFLOW_REAL, /* Converted with underflow on real part */
    CONV_UNDERFLOW_IMAG, /* Converted with underflow on imaginary part */
    CONV_UNDERFLOW_BOTH, /* Converted with underflow on real and imaginary parts */
    CONV_OVERUNDER,      /* Converted with overflow on real part and underflow on imaginary part */
    CONV_UNDEROVER,      /* Converted with underflow on real part and overflow on imaginary part */
} conv_func_ret_t;

/*
 * Although we check whether a floating point overflow generates a SIGFPE and
 * turn off overflow tests in that case, it might still be possible for an
 * overflow condition to occur.  Once a SIGFPE is raised the program cannot
 * be allowed to continue (cf. Posix signals) so in order to recover from a
 * SIGFPE we run tests that might generate one in a child process.
 */
#if defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)
#define HANDLE_SIGFPE
#endif

/*
 * Decide what values of floating-point number we want to test.  They are
 * 1 - normalized; 2 - denormalized; 3 - special.
 */
#define TEST_NOOP    0
#define TEST_NORMAL  1
#define TEST_DENORM  2
#define TEST_SPECIAL 3

/* Temporary buffer sizes */
#define TMP_BUF_DIM1 32
#define TMP_BUF_DIM2 100

/* Don't use hardware conversions if set */
static int without_hardware_g = 0;

/* Allocates memory aligned on a certain boundary. */
#define aligned_malloc(Z) ((void *)((char *)malloc(ALIGNMENT + Z) + ALIGNMENT))
#define aligned_free(M)   free((char *)(M)-ALIGNMENT)

/* Initialize source buffer of integer for integer->integer and integer->floating-point conversion test.
 * This algorithm is mainly to avoid any casting and comparison between source and destination types
 * for compiler, because we're testing conversions. */
#define INIT_INTEGER(TYPE, SRC_MAX, SRC_MIN, SRC_SIZE, DST_SIZE, SRC_PREC, BUF, SAVED, NELMTS)               \
    do {                                                                                                     \
        unsigned char *buf_p, *saved_p;                                                                      \
        unsigned int   n;                                                                                    \
        TYPE           value1 = 1;                                                                           \
        TYPE           value2 = 0;                                                                           \
                                                                                                             \
        /* Allocate buffers */                                                                               \
        NELMTS = SRC_PREC * 3;                                                                               \
        BUF    = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                          \
        SAVED  = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                          \
        memset(BUF, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                     \
        memset(SAVED, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                   \
                                                                                                             \
        buf_p   = BUF;                                                                                       \
        saved_p = SAVED;                                                                                     \
                                                                                                             \
        /*positive values, ascending order. VALUE1 starts from 00000001, to 00000010, until 10000000*/       \
        /*VALUE2 ascends from 00000000, to 00000011, 00000111,...,  until 11111111.*/                        \
        for (n = 0; n < SRC_PREC; n++) {                                                                     \
            {                                                                                                \
                memcpy(buf_p, &value1, SRC_SIZE);                                                            \
                memcpy(saved_p, &value1, SRC_SIZE);                                                          \
                buf_p += SRC_SIZE;                                                                           \
                saved_p += SRC_SIZE;                                                                         \
            }                                                                                                \
            {                                                                                                \
                memcpy(buf_p, &value2, SRC_SIZE);                                                            \
                memcpy(saved_p, &value2, SRC_SIZE);                                                          \
                buf_p += SRC_SIZE;                                                                           \
                saved_p += SRC_SIZE;                                                                         \
            }                                                                                                \
                                                                                                             \
            if (n < SRC_PREC - 2) {                                                                          \
                value1 = (TYPE)(value1 << 1);                                                                \
                value2 = (TYPE)((value1 - 1) | value1);                                                      \
            }                                                                                                \
            else if (n == SRC_PREC - 2) { /*to avoid overflow of negative values for signed integer*/        \
                value1 = (TYPE)(value1 << 1);                                                                \
                value2 = (TYPE)((~value1) | value1);                                                         \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        /* negative values for signed; descending positive values for unsigned */                            \
        /* VALUE2 descends from 11111111 to 11111110, 11111100, ..., until 10000000. */                      \
        for (n = 0; n < SRC_PREC - 1; n++) {                                                                 \
            {                                                                                                \
                memcpy(buf_p, &value2, SRC_SIZE);                                                            \
                memcpy(saved_p, &value2, SRC_SIZE);                                                          \
                buf_p += SRC_SIZE;                                                                           \
                saved_p += SRC_SIZE;                                                                         \
            }                                                                                                \
            if (n < SRC_PREC - 1)                                                                            \
                value2 = (TYPE)(value2 << 1);                                                                \
        }                                                                                                    \
    } while (0)

/* Change a buffer's byte order from big endian to little endian.  It's mainly for library's
 * bit operations which handle only little endian order.
 */
#define CHANGE_ORDER(EBUF, EORDER, ESIZE)                                                                    \
    do {                                                                                                     \
        unsigned int m;                                                                                      \
        if (H5T_ORDER_BE == EORDER) {                                                                        \
            unsigned char mediator;                                                                          \
            size_t        half_size = ESIZE / 2;                                                             \
            for (m = 0; m < half_size; m++) {                                                                \
                mediator              = EBUF[ESIZE - (m + 1)];                                               \
                EBUF[ESIZE - (m + 1)] = EBUF[m];                                                             \
                EBUF[m]               = mediator;                                                            \
            }                                                                                                \
        }                                                                                                    \
        else if (H5T_ORDER_VAX == EORDER) {                                                                  \
            unsigned char mediator1, mediator2;                                                              \
            for (m = 0; m < ESIZE; m += 4) {                                                                 \
                mediator1 = EBUF[m];                                                                         \
                mediator2 = EBUF[m + 1];                                                                     \
                                                                                                             \
                EBUF[m]     = EBUF[(ESIZE - 2) - m];                                                         \
                EBUF[m + 1] = EBUF[(ESIZE - 1) - m];                                                         \
                                                                                                             \
                EBUF[(ESIZE - 2) - m] = mediator1;                                                           \
                EBUF[(ESIZE - 1) - m] = mediator2;                                                           \
            }                                                                                                \
        }                                                                                                    \
    } while (0)

/* Allocate buffer and initialize it with floating-point normalized values.
 * It's for conversion test of floating-point as the source.
 */
#define INIT_FP_NORM(TYPE, SRC_MAX, SRC_MIN, SRC_MAX_10_EXP, SRC_MIN_10_EXP, SRC_SIZE, DST_SIZE, BUF, SAVED, \
                     NELMTS)                                                                                 \
    do {                                                                                                     \
        unsigned char *buf_p, *saved_p;                                                                      \
        size_t         num_norm, factor, n;                                                                  \
        TYPE           value1, value2;                                                                       \
        TYPE           multiply;                                                                             \
                                                                                                             \
        /*Determine the number of normalized values and increment pace.  The values start from               \
         *minimal normalized value and are multiplied by MULTIPLY each step until reach to maximal           \
         *normalized value.*/                                                                                \
        if (SRC_MAX_10_EXP < 100) { /*for float*/                                                            \
            factor   = 0;                                                                                    \
            multiply = 10;                                                                                   \
        }                                                                                                    \
        else if (SRC_MAX_10_EXP >= 100 && SRC_MAX_10_EXP < 400) { /*for double*/                             \
            factor   = 2;                                                                                    \
            multiply = 10000;                                                                                \
        }                                                                                                    \
        else { /*for long double*/                                                                           \
            factor   = 3;                                                                                    \
            multiply = 100000000;                                                                            \
        }                                                                                                    \
                                                                                                             \
        /*The number of values if multiplied by 10 for each step.*/                                          \
        num_norm = (SRC_MAX_10_EXP - SRC_MIN_10_EXP);                                                        \
        /*Reduce the number of values by 2^factor. MULTIPLY=10^(2^factor). Using this algorithm              \
         *instead of arithmetic operation to avoid any conversion*/                                          \
        num_norm >>= factor;                                                                                 \
                                                                                                             \
        /*Total number of values*/                                                                           \
        NELMTS = 2 *         /*both positive and negative*/                                                  \
                 (num_norm + /*number of normalized values*/                                                 \
                  1);        /*maximal normalized value*/                                                    \
                                                                                                             \
        /* Allocate buffers */                                                                               \
        BUF   = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        SAVED = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        memset(BUF, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                     \
        memset(SAVED, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                   \
                                                                                                             \
        buf_p   = BUF;                                                                                       \
        saved_p = SAVED;                                                                                     \
                                                                                                             \
        /*Normalized values*/                                                                                \
        value1 = SRC_MIN;                                                                                    \
        value2 = -SRC_MIN;                                                                                   \
        for (n = 0; n < num_norm; n++) {                                                                     \
            if (value1 < SRC_MAX) { /*positive*/                                                             \
                memcpy(buf_p, &value1, SRC_SIZE);                                                            \
                memcpy(saved_p, &value1, SRC_SIZE);                                                          \
                value1 *= multiply;                                                                          \
                buf_p += SRC_SIZE;                                                                           \
                saved_p += SRC_SIZE;                                                                         \
            }                                                                                                \
            if (value2 > -SRC_MAX) { /*negative*/                                                            \
                memcpy(buf_p, &value2, SRC_SIZE);                                                            \
                memcpy(saved_p, &value2, SRC_SIZE);                                                          \
                value2 *= multiply;                                                                          \
                buf_p += SRC_SIZE;                                                                           \
                saved_p += SRC_SIZE;                                                                         \
            }                                                                                                \
        }                                                                                                    \
                                                                                                             \
        value1 = SRC_MAX; /*maximal value*/                                                                  \
        memcpy(buf_p, &value1, SRC_SIZE);                                                                    \
        memcpy(saved_p, &value1, SRC_SIZE);                                                                  \
        buf_p += SRC_SIZE;                                                                                   \
        saved_p += SRC_SIZE;                                                                                 \
                                                                                                             \
        value2 = -SRC_MAX; /*negative value*/                                                                \
        memcpy(buf_p, &value2, SRC_SIZE);                                                                    \
        memcpy(saved_p, &value2, SRC_SIZE);                                                                  \
        buf_p += SRC_SIZE;                                                                                   \
        saved_p += SRC_SIZE;                                                                                 \
    } while (0)

/* Allocate buffer and initialize it with floating-point denormalized values.
 * It's for conversion test of floating-point as the source.
 */
#define INIT_FP_DENORM(TYPE, SRC_MANT_DIG, SRC_SIZE, SRC_PREC, SRC_ORDR, DST_SIZE, BUF, SAVED, NELMTS)       \
    do {                                                                                                     \
        unsigned char *buf_p, *saved_p;                                                                      \
        unsigned char *tmp1, *tmp2;                                                                          \
        size_t         n;                                                                                    \
                                                                                                             \
        /*Total number of values*/                                                                           \
        NELMTS = 2 *                 /*both positive and negative*/                                          \
                 (SRC_MANT_DIG - 1); /*number of denormalized values*/                                       \
                                                                                                             \
        /* Allocate buffers */                                                                               \
        BUF   = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        SAVED = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        memset(BUF, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                     \
        memset(SAVED, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                   \
                                                                                                             \
        tmp1 = (unsigned char *)calloc((size_t)1, (size_t)SRC_SIZE);                                         \
        tmp2 = (unsigned char *)calloc((size_t)1, (size_t)SRC_SIZE);                                         \
                                                                                                             \
        buf_p   = BUF;                                                                                       \
        saved_p = SAVED;                                                                                     \
                                                                                                             \
        /*Denormalized values. Exponent is 0. Let mantissa starts from 00000001, 00000011,                   \
         *00000111,..., until 11111111.*/                                                                    \
        memset(tmp1, 0, SRC_SIZE);                                                                           \
        memset(tmp2, 0, SRC_SIZE);                                                                           \
        H5T__bit_set(tmp2, SRC_PREC - 1, (size_t)1, true); /*the negative value*/                            \
        for (n = 0; n < SRC_MANT_DIG - 1; n++) {                                                             \
            H5T__bit_set(tmp1, n, (size_t)1, true); /*turn on 1 bit each time*/                              \
            CHANGE_ORDER(tmp1, SRC_ORDR, SRC_SIZE); /*change order for big endian*/                          \
            memcpy(buf_p, tmp1, SRC_SIZE);                                                                   \
            memcpy(saved_p, tmp1, SRC_SIZE);                                                                 \
            CHANGE_ORDER(tmp1, SRC_ORDR, SRC_SIZE); /*change back the order for bit operation*/              \
            buf_p += SRC_SIZE;                                                                               \
            saved_p += SRC_SIZE;                                                                             \
                                                                                                             \
            /*negative values*/                                                                              \
            H5T__bit_set(tmp2, n, (size_t)1, true);                                                          \
            CHANGE_ORDER(tmp2, SRC_ORDR, SRC_SIZE);                                                          \
            memcpy(buf_p, tmp2, SRC_SIZE);                                                                   \
            memcpy(saved_p, tmp2, SRC_SIZE);                                                                 \
            CHANGE_ORDER(tmp2, SRC_ORDR, SRC_SIZE);                                                          \
            buf_p += SRC_SIZE;                                                                               \
            saved_p += SRC_SIZE;                                                                             \
        }                                                                                                    \
        free(tmp1);                                                                                          \
        free(tmp2);                                                                                          \
    } while (0)

/* Allocate buffer and initialize it with floating-point special values, +/-0, +/-infinity,
 * +/-QNaN, +/-SNaN.  It's for conversion test of floating-point as the source.
 */
#define INIT_FP_SPECIAL(SRC_SIZE, SRC_PREC, SRC_ORDR, SRC_MANT_DIG, DST_SIZE, BUF, SAVED, NELMTS)            \
    do {                                                                                                     \
        unsigned char *buf_p;                                                                                \
        unsigned char *value;                                                                                \
        int            n;                                                                                    \
                                                                                                             \
        /*Total number of values*/                                                                           \
        NELMTS = 2 * /*both positive and negative*/                                                          \
                 4;  /*infinity, SNaN, QNaN      */                                                          \
                                                                                                             \
        /* Allocate buffers */                                                                               \
        BUF   = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        SAVED = (unsigned char *)aligned_malloc(NELMTS * MAX(SRC_SIZE, DST_SIZE));                           \
        memset(BUF, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                     \
        memset(SAVED, 0, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                   \
        value = (unsigned char *)calloc(SRC_SIZE, sizeof(unsigned char));                                    \
                                                                                                             \
        buf_p = BUF;                                                                                         \
                                                                                                             \
        /* +0 */                                                                                             \
        H5T__bit_set(value, (size_t)0, SRC_PREC, false);                                                     \
        memcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                              \
        buf_p += SRC_SIZE;                                                                                   \
                                                                                                             \
        for (n = 0; n < 2; n++) {                                                                            \
            if (n == 1) {                                                                                    \
                memset(value, 0, SRC_SIZE * sizeof(unsigned char));                                          \
                /* -0 */                                                                                     \
                H5T__bit_set(value, (size_t)(SRC_PREC - 1), (size_t)1, true);                                \
                CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change order for big endian*/                     \
                memcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                      \
                CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change back the order for bit operation*/         \
                buf_p += SRC_SIZE;                                                                           \
            }                                                                                                \
                                                                                                             \
            /* +/-infinity */                                                                                \
            H5T__bit_set(value, (size_t)(SRC_MANT_DIG - 1), SRC_PREC - SRC_MANT_DIG, true);                  \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change order for big endian*/                         \
            memcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                          \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change back the order for bit operation*/             \
            buf_p += SRC_SIZE;                                                                               \
                                                                                                             \
            /* +/-SNaN */                                                                                    \
            H5T__bit_set(value, (size_t)0, (size_t)1, true);                                                 \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change order for big endian*/                         \
            memcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                          \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change back the order for bit operation*/             \
            buf_p += SRC_SIZE;                                                                               \
                                                                                                             \
            /* +/-QNaN */                                                                                    \
            H5T__bit_set(value, (size_t)(SRC_MANT_DIG - 2), (size_t)1, true);                                \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change order for big endian*/                         \
            memcpy(buf_p, value, SRC_SIZE * sizeof(unsigned char));                                          \
            CHANGE_ORDER(value, SRC_ORDR, SRC_SIZE); /*change back the order for bit operation*/             \
            buf_p += SRC_SIZE;                                                                               \
        }                                                                                                    \
                                                                                                             \
        memcpy(SAVED, BUF, NELMTS *MAX(SRC_SIZE, DST_SIZE));                                                 \
        free(value);                                                                                         \
    } while (0)

static bool overflows(unsigned char *origin_bits, hid_t src_id, size_t dst_num_bits);
static int  my_isnan(dtype_t type, void *val);
static int my_isinf(int endian, const unsigned char *val, size_t size, size_t mpos, size_t msize, size_t epos,
                    size_t esize);

/*-------------------------------------------------------------------------
 * Function:    fpe_handler
 *
 * Purpose:    Exit with 255
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
static void
fpe_handler(int H5_ATTR_UNUSED signo)
{
    SKIPPED();
    puts("    Test skipped due to SIGFPE.");
#ifndef HANDLE_SIGFPE
    puts("    Remaining tests could not be run.");
    puts("    Please turn off SIGFPE on overflows and try again.");
#endif
    exit(255);
}

/*-------------------------------------------------------------------------
 * Function:    reset_hdf5
 *
 * Purpose:    Reset the hdf5 library.  This causes statistics to be printed
 *        and counters to be reset.
 *
 * Return:    void
 *
 *-------------------------------------------------------------------------
 */
static void
reset_hdf5(void)
{
    h5_test_init();

    if (without_hardware_g)
        h5_no_hwconv();
#ifdef TEST_ALIGNMENT
    SET_ALIGNMENT(SCHAR, H5_SIZEOF_CHAR);
    SET_ALIGNMENT(UCHAR, H5_SIZEOF_CHAR);
    SET_ALIGNMENT(SHORT, H5_SIZEOF_SHORT);
    SET_ALIGNMENT(USHORT, H5_SIZEOF_SHORT);
    SET_ALIGNMENT(INT, H5_SIZEOF_INT);
    SET_ALIGNMENT(UINT, H5_SIZEOF_INT);
    SET_ALIGNMENT(LONG, H5_SIZEOF_LONG);
    SET_ALIGNMENT(ULONG, H5_SIZEOF_LONG);
    SET_ALIGNMENT(LLONG, H5_SIZEOF_LONG_LONG);
    SET_ALIGNMENT(ULLONG, H5_SIZEOF_LONG_LONG);
    SET_ALIGNMENT(FLOAT, H5_SIZEOF_FLOAT);
    SET_ALIGNMENT(DOUBLE, H5_SIZEOF_DOUBLE);
    SET_ALIGNMENT(LDOUBLE, H5_SIZEOF_LONG_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    SET_ALIGNMENT(FLOAT16, H5_SIZEOF__FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    SET_ALIGNMENT(FLOAT_COMPLEX, H5_SIZEOF_FLOAT_COMPLEX);
    SET_ALIGNMENT(DOUBLE_COMPLEX, H5_SIZEOF_DOUBLE_COMPLEX);
    SET_ALIGNMENT(LDOUBLE_COMPLEX, H5_SIZEOF_LONG_DOUBLE_COMPLEX);
#endif
#endif
}

/*-------------------------------------------------------------------------
 * Function:    except_func
 *
 * Purpose:    Gets called for all data type conversion exceptions.
 *
 * Return:    H5T_CONV_ABORT:            -1
 *
 *              H5T_CONV_UNHANDLED      0
 *
 *              H5T_CONV_HANDLED        1
 *
 *-------------------------------------------------------------------------
 */
static H5T_conv_ret_t
except_func(H5T_conv_except_t except_type, hid_t H5_ATTR_UNUSED src_id, hid_t H5_ATTR_UNUSED dst_id,
            void H5_ATTR_UNUSED *src_buf, void *dst_buf, void *user_data)
{
    H5T_conv_ret_t ret = H5T_CONV_HANDLED;

    if (except_type == H5T_CONV_EXCEPT_RANGE_HI)
        /*only test integer case*/
        *(int *)dst_buf = *(int *)user_data;
    else if (except_type == H5T_CONV_EXCEPT_RANGE_LOW)
        /*only test integer case*/
        *(int *)dst_buf = *(int *)user_data;
    else if (except_type == H5T_CONV_EXCEPT_TRUNCATE)
        ret = H5T_CONV_UNHANDLED;
    else if (except_type == H5T_CONV_EXCEPT_PRECISION)
        ret = H5T_CONV_UNHANDLED;
    else if (except_type == H5T_CONV_EXCEPT_PINF)
        /*only test integer case*/
        *(int *)dst_buf = *(int *)user_data;
    else if (except_type == H5T_CONV_EXCEPT_NINF)
        /*only test integer case*/
        *(int *)dst_buf = *(int *)user_data;
    else if (except_type == H5T_CONV_EXCEPT_NAN)
        /*only test integer case*/
        *(int *)dst_buf = *(int *)user_data;

    return ret;
}

static herr_t
my_conv_int_float_func(hid_t H5_ATTR_UNUSED src_id, hid_t H5_ATTR_UNUSED dst_id,
                       H5T_cdata_t H5_ATTR_UNUSED *cdata, size_t H5_ATTR_UNUSED nelmts,
                       size_t H5_ATTR_UNUSED buf_stride, size_t H5_ATTR_UNUSED bkg_stride,
                       void H5_ATTR_UNUSED *buf, void H5_ATTR_UNUSED *bkg,
                       hid_t H5_ATTR_UNUSED dset_xfer_plist)
{
    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_hard_query
 *
 * Purpose:     Tests H5Tcompiler_conv() for querying whether a conversion is
 *              a hard one.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_hard_query(void)
{
    TESTING("query functions of compiler conversion");

    /* Verify the conversion from int to float is a hard conversion. */
    if (H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != true) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    /* Unregister all hard conversion paths */
    H5Tunregister(H5T_PERS_HARD, NULL, H5I_INVALID_HID, H5I_INVALID_HID, NULL);

    /* Verify the conversion is now a soft conversion */
    if (H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != false) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    /* Register our custom int to float conversion function */
    H5Tregister(H5T_PERS_HARD, "int_flt", H5T_NATIVE_INT, H5T_NATIVE_FLOAT,
                (H5T_conv_t)((void (*)(void))my_conv_int_float_func));

    /* Verify the conversion is now a hard conversion */
    if (H5Tcompiler_conv(H5T_NATIVE_INT, H5T_NATIVE_FLOAT) != true) {
        H5_FAILED();
        printf("Can't query conversion function\n");
        goto error;
    }

    PASSED();

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();

    return 0;

error:
    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    expt_handle
 *
 * Purpose:    Gets called from test_particular_fp_integer() for data type
 *              conversion exceptions.
 *
 * Return:    H5T_CONV_HANDLED        1
 *
 *-------------------------------------------------------------------------
 */
static H5T_conv_ret_t
expt_handle(H5T_conv_except_t except_type, hid_t H5_ATTR_UNUSED src_id, hid_t H5_ATTR_UNUSED dst_id,
            void H5_ATTR_UNUSED *src_buf, void *dst_buf, void *user_data)
{
    signed char fill_value1 = 7;
    int         fill_value2 = 13;
    short       fill_value3 = 25;

    if (except_type == H5T_CONV_EXCEPT_RANGE_HI || except_type == H5T_CONV_EXCEPT_RANGE_LOW ||
        except_type == H5T_CONV_EXCEPT_TRUNCATE) {
        if (*(int *)user_data == 0)
            *(int *)dst_buf = fill_value2;
        else if (*(int *)user_data == 1)
            *(signed char *)dst_buf = fill_value1;
        else
            *(short *)dst_buf = fill_value3;
    } /* end if */

    return H5T_CONV_HANDLED;
}

/*-------------------------------------------------------------------------
 * Function:    test_particular_fp_integer
 *
 * Purpose:     Tests hard conversions from floating numbers to integers in
 *              a special situation when the source is "float" and assigned
 *              the value of "INT_MAX".  A compiler may do roundup making
 *              this value "INT_MAX+1".  When this float value is casted to
 *              int, overflow happens.  This test makes sure the library
 *              returns exception in this situation.
 *
 *              Also verifies the library handles conversion from double to
 *              signed char correctly when the value of double is SCHAR_MAX.
 *              The test makes sure the signed char doesn't overflow.
 *
 *              This test is mainly for netCDF's request.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *-------------------------------------------------------------------------
 */
static int
test_particular_fp_integer(void)
{
    hid_t          dxpl_id;
    int            flag;
    double         src_d = (double)SCHAR_MAX;
    signed char    dst_c;
    unsigned char *buf1       = NULL;
    unsigned char *saved_buf1 = NULL;
    size_t         src_size1;
    size_t         dst_size1;
    int            endian; /*endianness            */
    unsigned int   fails_this_test = 0;
    size_t         j;
#ifdef H5_WANT_DCONV_EXCEPTION
    unsigned char *buf2       = NULL;
    unsigned char *saved_buf2 = NULL;
    size_t         src_size2;
    size_t         dst_size2;
    float          src_f      = (float)INT_MAX;
    int            fill_value = 13;
    int            dst_i;
#ifdef H5_HAVE__FLOAT16
    unsigned char *buf3       = NULL;
    unsigned char *saved_buf3 = NULL;
    H5__Float16    src_half   = (H5__Float16)SHRT_MAX;
    short          s_fill_val = 25;
    short          dst_s;
    size_t         src_size3;
    size_t         dst_size3;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    unsigned char *buf4       = NULL;
    unsigned char *saved_buf4 = NULL;
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
    H5_float_complex src_fc = (H5_float_complex)INT_MAX;
#else
    H5_float_complex src_fc = H5_CMPLXF(INT_MAX, 0.0F);
#endif
    size_t src_size4;
    size_t dst_size4;
#endif
#endif

    TESTING("hard particular floating number -> integer conversions");

    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    /* Test conversion from double (the value is SCHAR_MAX) to signed char. */
    endian     = H5Tget_order(H5T_NATIVE_DOUBLE);
    src_size1  = H5Tget_size(H5T_NATIVE_DOUBLE);
    dst_size1  = H5Tget_size(H5T_NATIVE_SCHAR);
    buf1       = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size1, dst_size1));
    saved_buf1 = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size1, dst_size1));

    memcpy(buf1, &src_d, src_size1);
    memcpy(saved_buf1, &src_d, src_size1);

    /* Register exception handling function and signal the destination is "signed char". */
    flag = 1;
    if (H5Pset_type_conv_cb(dxpl_id, expt_handle, &flag) < 0) {
        H5_FAILED();
        printf("Can't register conversion callback\n");
        goto error;
    }

    /* Do conversion */
    if (H5Tconvert(H5T_NATIVE_DOUBLE, H5T_NATIVE_SCHAR, (size_t)1, buf1, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_c, buf1, dst_size1);

    /* Print errors */
    if (dst_c != SCHAR_MAX) {
        double      x = 0.0;
        signed char y;

        if (0 == fails_this_test++)
            H5_FAILED();

        printf("    test double to signed char:\n");
        printf("        src = ");
        for (j = 0; j < src_size1; j++)
            printf(" %02x", saved_buf1[ENDIAN(src_size1, j, endian)]);

        memcpy(&x, saved_buf1, src_size1);
        printf(" %29.20e\n", x);

        printf("        dst = ");
        for (j = 0; j < dst_size1; j++)
            printf(" %02x", buf1[ENDIAN(dst_size1, j, endian)]);

        memcpy(&y, buf1, dst_size1);
        printf(" %29d\n", y);
    }

/* Only run this part of the test if conversion exceptions are enabled */
#ifdef H5_WANT_DCONV_EXCEPTION
    /* Test conversion from float (the value is INT_MAX) to int. */
    src_size2  = H5Tget_size(H5T_NATIVE_FLOAT);
    dst_size2  = H5Tget_size(H5T_NATIVE_INT);
    buf2       = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size2, dst_size2));
    saved_buf2 = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size2, dst_size2));
    memcpy(buf2, &src_f, src_size2);
    memcpy(saved_buf2, &src_f, src_size2);

    /* signal exception handling function that the destination is "int". */
    flag = 0;

    /* Do conversion */
    if (H5Tconvert(H5T_NATIVE_FLOAT, H5T_NATIVE_INT, (size_t)1, buf2, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_i, buf2, dst_size2);

    /* Print errors */
    if (dst_i != fill_value) {
        float x = 0.0F;
        int   y;

        if (0 == fails_this_test++)
            H5_FAILED();

        printf("    test float to int:\n");
        printf("        src = ");
        for (j = 0; j < src_size2; j++)
            printf(" %02x", saved_buf2[ENDIAN(src_size2, j, endian)]);

        memcpy(&x, saved_buf2, src_size2);
        printf(" %29.20e\n", (double)x);

        printf("        dst = ");
        for (j = 0; j < dst_size2; j++)
            printf(" %02x", buf2[ENDIAN(dst_size2, j, endian)]);

        memcpy(&y, buf2, dst_size2);
        printf(" %29d\n", y);
    }

#ifdef H5_HAVE__FLOAT16
    /* Test conversion from _Float16 (the value is SHRT_MAX) to short. */
    src_size3  = H5Tget_size(H5T_NATIVE_FLOAT16);
    dst_size3  = H5Tget_size(H5T_NATIVE_SHORT);
    buf3       = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size3, dst_size3));
    saved_buf3 = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size3, dst_size3));
    memcpy(buf3, &src_half, src_size3);
    memcpy(saved_buf3, &src_half, src_size3);

    /* Register exception handling function and signal the destination is "short". */
    flag = 2;

    /* Do conversion */
    if (H5Tconvert(H5T_NATIVE_FLOAT16, H5T_NATIVE_SHORT, (size_t)1, buf3, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_s, buf3, dst_size3);

    /* Print errors */
    if (dst_s != s_fill_val) {
        H5__Float16 x;
        short       y;

        if (0 == fails_this_test++)
            H5_FAILED();

        printf("    test _Float16 to short:\n");
        printf("        src = ");
        for (j = 0; j < src_size3; j++)
            printf(" %02x", saved_buf3[ENDIAN(src_size3, j, endian)]);

        memcpy(&x, saved_buf3, src_size3);
        printf(" %29.20e\n", (double)x);

        printf("        dst = ");
        for (j = 0; j < dst_size3; j++)
            printf(" %02x", buf3[ENDIAN(dst_size3, j, endian)]);

        memcpy(&y, buf3, dst_size3);
        printf(" %29d\n", (int)y);
    }
#endif /* H5_HAVE__FLOAT16 */

#ifdef H5_HAVE_COMPLEX_NUMBERS
    /* Test conversion from float complex (the value is INT_MAX) to int. */
    src_size4  = H5Tget_size(H5T_NATIVE_FLOAT_COMPLEX);
    dst_size4  = H5Tget_size(H5T_NATIVE_INT);
    buf4       = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size4, dst_size4));
    saved_buf4 = (unsigned char *)calloc((size_t)1, (size_t)MAX(src_size4, dst_size4));
    memcpy(buf4, &src_fc, src_size4);
    memcpy(saved_buf4, &src_fc, src_size4);

    /* signal exception handling function that the destination is "int". */
    flag = 0;

    /* Do conversion */
    if (H5Tconvert(H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_INT, (size_t)1, buf4, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    memcpy(&dst_i, buf4, dst_size4);

    /* Print errors */
    if (dst_i != fill_value) {
        H5_float_complex x = H5_CMPLXF(0.0F, 0.0F);
        unsigned char   *buf_ptr;
        int              y;

        if (0 == fails_this_test++)
            H5_FAILED();

        printf("    test float complex to int:\n");
        printf("        src = ");
        buf_ptr = saved_buf4;
        for (j = 0; j < src_size4 / 2; j++)
            printf(" %02x", buf_ptr[ENDIAN(src_size4 / 2, j, endian)]);
        buf_ptr += src_size4 / 2;
        for (j = 0; j < src_size4 / 2; j++)
            printf(" %02x", buf_ptr[ENDIAN(src_size4 / 2, j, endian)]);

        memcpy(&x, saved_buf4, src_size4);
        printf(" %29.20e%+29.20ei\n", (double)crealf(x), (double)cimagf(x));

        printf("        dst = ");
        for (j = 0; j < dst_size4; j++)
            printf(" %02x", buf4[ENDIAN(dst_size4, j, endian)]);

        memcpy(&y, buf4, dst_size4);
        printf(" %29d\n", y);
    }
#endif /* H5_HAVE_COMPLEX_NUMBERS */

#endif /* H5_WANT_DCONV_EXCEPTION */

    if (fails_this_test)
        goto error;

    if (H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if (buf1)
        free(buf1);

#ifdef H5_WANT_DCONV_EXCEPTION
    if (buf2)
        free(buf2);
#ifdef H5_HAVE__FLOAT16
    if (buf3)
        free(buf3);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    if (buf4)
        free(buf4);
#endif
#endif

    if (saved_buf1)
        free(saved_buf1);

#ifdef H5_WANT_DCONV_EXCEPTION
    if (saved_buf2)
        free(saved_buf2);
#ifdef H5_HAVE__FLOAT16
    if (saved_buf3)
        free(saved_buf3);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    if (saved_buf4)
        free(saved_buf4);
#endif
#endif

    PASSED();
    return 0;

error:
    fflush(stdout);
    H5E_BEGIN_TRY
    {
        H5Pclose(dxpl_id);
    }
    H5E_END_TRY
    if (buf1)
        free(buf1);

#ifdef H5_WANT_DCONV_EXCEPTION
    if (buf2)
        free(buf2);
#ifdef H5_HAVE__FLOAT16
    if (buf3)
        free(buf3);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    if (buf4)
        free(buf4);
#endif
#endif

    if (saved_buf1)
        free(saved_buf1);

#ifdef H5_WANT_DCONV_EXCEPTION
    if (saved_buf2)
        free(saved_buf2);
#ifdef H5_HAVE__FLOAT16
    if (saved_buf3)
        free(saved_buf3);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    if (saved_buf4)
        free(saved_buf4);
#endif
#endif

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return MAX((int)fails_this_test, 1);
}

/*-------------------------------------------------------------------------
 * Function:    test_derived_flt
 *
 * Purpose:     Tests user-defined and query functions of floating-point types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_derived_flt(void)
{
    hid_t          file = H5I_INVALID_HID, tid1 = H5I_INVALID_HID, tid2 = H5I_INVALID_HID;
    hid_t          dxpl_id = H5I_INVALID_HID;
    char           filename[1024];
    size_t         spos, epos, esize, mpos, msize, size;
    size_t         src_size, dst_size;
    unsigned char *buf = NULL, *saved_buf = NULL;
    int           *aligned = NULL;
    int            endian; /*endianness            */
    size_t         nelmts          = NTESTELEM;
    unsigned int   fails_this_test = 0;
    const size_t   max_fails       = 40; /*max number of failures*/
    char           str[256];             /*message string    */
    unsigned int   i, j;

    TESTING("user-defined and query functions of floating-point types");

    /* Create File */
    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't create file\n");
        goto error;
    }

    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    if ((tid1 = H5Tcopy(H5T_IEEE_F64LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    if ((tid2 = H5Tcopy(H5T_IEEE_F32LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    /*------------------------------------------------------------------------
     *                   1st floating-point type
     * size=7 byte, precision=42 bits, offset=3 bits, mantissa size=31 bits,
     * mantissa position=3, exponent size=10 bits, exponent position=34,
     * exponent bias=511.  It can be illustrated in little-endian order as
     *
     *          6       5       4       3       2       1       0
     *    ???????? ???SEEEE EEEEEEMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMM???
     *
     * To create a new floating-point type, the following properties must be
     * set in the order of
     *   set fields -> set offset -> set precision -> set size.
     * All these properties must be set before the type can function. Other
     * properties can be set anytime.  Derived type size cannot be expanded
     * bigger than original size but can be decreased.  There should be no
     * holes among the significant bits.  Exponent bias usually is set
     * 2^(n-1)-1, where n is the exponent size.
     *-----------------------------------------------------------------------*/
    if (H5Tset_fields(tid1, (size_t)44, (size_t)34, (size_t)10, (size_t)3, (size_t)31) < 0) {
        H5_FAILED();
        printf("Can't set fields\n");
        goto error;
    }
    if (H5Tset_offset(tid1, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }
    if (H5Tset_precision(tid1, (size_t)42) < 0) {
        H5_FAILED();
        printf("Can't set precision 1\n");
        goto error;
    }
    if (H5Tset_size(tid1, (size_t)7) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }

    if (H5Tset_ebias(tid1, (size_t)511) < 0) {
        H5_FAILED();
        printf("Can't set exponent bias\n");
        goto error;
    }
    if (H5Tset_pad(tid1, H5T_PAD_ZERO, H5T_PAD_ZERO) < 0) {
        H5_FAILED();
        printf("Can't set padding\n");
        goto error;
    }

    if (H5Tcommit2(file, "new float type 1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit datatype\n");
        goto error;
    }
    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((tid1 = H5Topen2(file, "new float type 1", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype");
    if (H5Tget_fields(tid1, &spos, &epos, &esize, &mpos, &msize) < 0) {
        H5_FAILED();
        printf("Can't get fields\n");
        goto error;
    }
    if (spos != 44 || epos != 34 || esize != 10 || mpos != 3 || msize != 31) {
        H5_FAILED();
        printf("Wrong field values\n");
        goto error;
    }

    if (H5Tget_precision(tid1) != 42) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if (H5Tget_offset(tid1) != 3) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if ((size = H5Tget_size(tid1)) != 7) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if (H5Tget_ebias(tid1) != 511) {
        H5_FAILED();
        printf("Can't get exponent bias or wrong bias\n");
        goto error;
    }

    /* Convert data from native integer to the 1st derived floating-point type.
     * Then convert data from the floating-point type back to native integer.
     * Compare the final data with the original data.
     */
    src_size  = H5Tget_size(H5T_NATIVE_INT);
    endian    = H5Tget_order(H5T_NATIVE_INT);
    buf       = (unsigned char *)malloc(nelmts * (MAX(src_size, size)));
    saved_buf = (unsigned char *)malloc(nelmts * src_size);
    memset(buf, 0, nelmts * MAX(src_size, size));
    memset(saved_buf, 0, nelmts * src_size);
    aligned = (int *)calloc((size_t)1, src_size);

    for (i = 0; i < nelmts * src_size; i++)
        buf[i] = saved_buf[i] = (unsigned char)rand();

    /* Convert data from native integer to derived floating-point type.
     * The mantissa is big enough to retain the integer's precision. */
    if (H5Tconvert(H5T_NATIVE_INT, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the derived floating-point type back to native integer. */
    if (H5Tconvert(tid1, H5T_NATIVE_INT, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for (i = 0; i < nelmts; i++) {
        for (j = 0; j < src_size; j++)
            if (buf[i * src_size + j] != saved_buf[i * src_size + j])
                break;
        if (j == src_size)
            continue; /*no error*/

        /* Print errors */
        if (0 == fails_this_test++) {
            snprintf(str, sizeof(str),
                     "\nTesting random sw derived floating-point -> derived floating-point conversions");
            printf("%-70s", str);
            fflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", saved_buf[i * src_size + ENDIAN(src_size, j, endian)]);

        memcpy(aligned, saved_buf + i * sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        printf("        dst = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", buf[i * src_size + ENDIAN(src_size, j, endian)]);

        memcpy(aligned, buf + i * sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        if (fails_this_test >= max_fails) {
            puts("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    fails_this_test = 0;
    free(buf);
    free(saved_buf);
    free(aligned);
    buf       = NULL;
    saved_buf = NULL;
    aligned   = NULL;

    /*--------------------------------------------------------------------------
     *                   2nd floating-point type
     * size=3 byte, precision=24 bits, offset=0 bits, mantissa size=16 bits,
     * mantissa position=0, exponent size=7 bits, exponent position=16, exponent
     * bias=63. It can be illustrated in little-endian order as
     *
     *          2       1       0
     *    SEEEEEEE MMMMMMMM MMMMMMMM
     *--------------------------------------------------------------------------*/
    if (H5Tset_fields(tid2, (size_t)23, (size_t)16, (size_t)7, (size_t)0, (size_t)16) < 0) {
        H5_FAILED();
        printf("Can't set fields\n");
        goto error;
    }
    if (H5Tset_offset(tid2, (size_t)0) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }
    if (H5Tset_precision(tid2, (size_t)24) < 0) {
        H5_FAILED();
        printf("Can't set precision 2\n");
        goto error;
    }
    if (H5Tset_size(tid2, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }
    if (H5Tset_ebias(tid2, (size_t)63) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }
    if (H5Tset_pad(tid2, H5T_PAD_ZERO, H5T_PAD_ZERO) < 0) {
        H5_FAILED();
        printf("Can't set padding\n");
        goto error;
    }

    if (H5Tcommit2(file, "new float type 2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't set inpad\n");
        goto error;
    }
    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((tid2 = H5Topen2(file, "new float type 2", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype");
    if (H5Tget_fields(tid2, &spos, &epos, &esize, &mpos, &msize) < 0) {
        H5_FAILED();
        printf("Can't get fields\n");
        goto error;
    }
    if (spos != 23 || epos != 16 || esize != 7 || mpos != 0 || msize != 16) {
        H5_FAILED();
        printf("Wrong field values\n");
        goto error;
    }

    if (H5Tget_precision(tid2) != 24) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if (H5Tget_offset(tid2) != 0) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if ((size = H5Tget_size(tid2)) != 3) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if (H5Tget_ebias(tid2) != 63) {
        H5_FAILED();
        printf("Can't get exponent bias or wrong bias\n");
        goto error;
    }

    /* Convert data from the 2nd to the 1st derived floating-point type.
     * Then convert data from the 1st type back to the 2nd type.
     * Compare the final data with the original data.
     */
    src_size  = H5Tget_size(tid2);
    dst_size  = H5Tget_size(tid1);
    endian    = H5Tget_order(tid2);
    buf       = (unsigned char *)malloc(nelmts * (MAX(src_size, dst_size)));
    saved_buf = (unsigned char *)malloc(nelmts * src_size);
    memset(buf, 0, nelmts * MAX(src_size, dst_size));
    memset(saved_buf, 0, nelmts * src_size);

    for (i = 0; i < nelmts * src_size; i++)
        buf[i] = saved_buf[i] = (unsigned char)rand();

    /* Convert data from the 2nd to the 1st derived floating-point type.
     * The mantissa and exponent of the 2nd type are big enough to retain
     * the precision and exponent power. */
    if (H5Tconvert(tid2, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the 1st back to the 2nd derived floating-point type. */
    if (H5Tconvert(tid1, tid2, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for (i = 0; i < nelmts; i++) {
        for (j = 0; j < src_size; j++)
            if (buf[i * src_size + j] != saved_buf[i * src_size + j])
                break;
        if (j == src_size)
            continue; /*no error*/

        /* If original value is NaN(exponent bits are all ones, 11..11),
         * the library simply sets all mantissa bits to ones.  So don't
         * compare values in this case.
         */
        if ((buf[i * src_size + 2] == 0x7f && saved_buf[i * src_size + 2] == 0x7f) ||
            (buf[i * src_size + 2] == 0xff && saved_buf[i * src_size + 2] == 0xff))
            continue;

        /* Print errors */
        if (0 == fails_this_test++) {
            snprintf(str, sizeof(str),
                     "\nTesting random sw derived floating-point -> derived floating-point conversions");
            printf("%-70s", str);
            fflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", saved_buf[i * src_size + ENDIAN(src_size, j, endian)]);
        printf("\n");

        printf("        dst = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", buf[i * src_size + ENDIAN(src_size, j, endian)]);
        printf("\n");

        if (fails_this_test >= max_fails) {
            puts("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    if (buf)
        free(buf);
    if (saved_buf)
        free(saved_buf);

    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if (H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    } /* end if */

    if (H5Fdelete(filename, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't delete file\n");
        goto error;
    }

    PASSED();

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return 0;

error:
    if (buf)
        free(buf);
    if (saved_buf)
        free(saved_buf);
    if (aligned)
        free(aligned);
    fflush(stdout);
    H5E_BEGIN_TRY
    {
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Pclose(dxpl_id);
        H5Fclose(file);
    }
    H5E_END_TRY

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return MAX((int)fails_this_test, 1);
}

/*-------------------------------------------------------------------------
 * Function:    test_derived_integer
 *
 * Purpose:     Tests user-defined and query functions of integer types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_derived_integer(void)
{
    hid_t          file = H5I_INVALID_HID, tid1 = H5I_INVALID_HID, tid2 = H5I_INVALID_HID;
    hid_t          dxpl_id = H5I_INVALID_HID;
    char           filename[1024];
    size_t         src_size, dst_size;
    unsigned char *buf = NULL, *saved_buf = NULL;
    int            endian; /*endianness            */
    size_t         nelmts          = NTESTELEM;
    unsigned int   fails_this_test = 0;
    const size_t   max_fails       = 40; /*max number of failures*/
    char           str[256];             /*message string    */
    unsigned int   i, j;

    TESTING("user-defined and query functions of integer types");

    /* Create File */
    h5_fixname(FILENAME[1], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't create file\n");
        goto error;
    }

    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    if ((tid1 = H5Tcopy(H5T_STD_I32LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    if ((tid2 = H5Tcopy(H5T_STD_U64LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }

    /*--------------------------------------------------------------------------
     *                   1st integer type
     * size=3 byte, precision=24 bits, offset=0 bits, order=big endian.
     * It can be illustrated in big-endian order as
     *
     *          0       1       2
     *    SIIIIIII IIIIIIII IIIIIIII
     *
     * There's no specific order for these functions to define the attributes
     * of a new integer type, H5Tset_precision, H5Tset_offset, H5Tset_size,
     * H5Tset_order, H5Tset_pad, H5Tset_sign.
     *--------------------------------------------------------------------------*/
    if (H5Tset_offset(tid1, (size_t)0) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if (H5Tset_size(tid1, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }

    if (H5Tset_precision(tid1, (size_t)24) < 0) {
        H5_FAILED();
        printf("Can't set precision\n");
        goto error;
    }

    if (H5Tset_order(tid1, H5T_ORDER_BE) < 0) {
        H5_FAILED();
        printf("Can't set order\n");
        goto error;
    }

    if (H5Tcommit2(file, "new integer type 1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit data type\n");
        goto error;
    }

    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((tid1 = H5Topen2(file, "new integer type 1", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype");
    if (H5Tget_precision(tid1) != 24) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if (H5Tget_offset(tid1) != 0) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if (H5Tget_size(tid1) != 3) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if (H5Tget_order(tid1) != H5T_ORDER_BE) {
        H5_FAILED();
        printf("Can't get order or wrong order\n");
        goto error;
    }

    /*--------------------------------------------------------------------------
     *                   2nd integer type
     * size=8 byte, precision=48 bits, offset=10 bits, order=little endian.
     * It can be illustrated in little-endian order as
     *
     *          7       6       5       4       3       2       1       0
     *   ??????SI IIIIIIII IIIIIIII IIIIIIII IIIIIIII IIIIIIII IIIIII?? ????????
     *--------------------------------------------------------------------------*/
    if (H5Tset_precision(tid2, (size_t)48) < 0) {
        H5_FAILED();
        printf("Can't set precision\n");
        goto error;
    }

    if (H5Tset_offset(tid2, (size_t)10) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if (H5Tset_sign(tid2, H5T_SGN_2) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }

    if (H5Tcommit2(file, "new integer type 2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit data type\n");
        goto error;
    }

    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((tid2 = H5Topen2(file, "new integer type 2", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype");
    if (H5Tget_precision(tid2) != 48) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if (H5Tget_offset(tid2) != 10) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if (H5Tget_size(tid2) != 8) {
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if (H5Tget_sign(tid2) != H5T_SGN_2) {
        H5_FAILED();
        printf("Can't get sign or wrong sign\n");
        goto error;
    }

    /* Convert data from the 1st to the 2nd derived integer type.
     * Then convert data from the 2nd type back to the 1st type.
     * Compare the final data with the original data.
     */
    src_size  = H5Tget_size(tid1);
    dst_size  = H5Tget_size(tid2);
    endian    = H5Tget_order(tid1);
    buf       = (unsigned char *)malloc(nelmts * (MAX(src_size, dst_size)));
    saved_buf = (unsigned char *)malloc(nelmts * src_size);
    memset(buf, 0, nelmts * MAX(src_size, dst_size));
    memset(saved_buf, 0, nelmts * src_size);

    for (i = 0; i < nelmts * src_size; i++)
        buf[i] = saved_buf[i] = (unsigned char)rand();

    /* Convert data from the 1st to the 2nd derived integer type.
     * The precision of the 2nd type are big enough to retain
     * the 1st type's precision. */
    if (H5Tconvert(tid1, tid2, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    /* Convert data from the 2nd back to the 1st derived integer type. */
    if (H5Tconvert(tid2, tid1, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for (i = 0; i < nelmts; i++) {
        for (j = 0; j < src_size; j++)
            if (buf[i * src_size + j] != saved_buf[i * src_size + j])
                break;
        if (j == src_size)
            continue; /*no error*/

        /* Print errors */
        if (0 == fails_this_test++) {
            snprintf(str, sizeof(str), "\nTesting random sw derived integer -> derived integer conversions");
            printf("%-70s", str);
            fflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", saved_buf[i * src_size + ENDIAN(src_size, j, endian)]);
        printf("\n");

        printf("        dst = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", buf[i * src_size + ENDIAN(src_size, j, endian)]);
        printf("\n");

        if (fails_this_test >= max_fails) {
            puts("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if (H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    } /* end if */

    if (H5Fdelete(filename, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't delete file\n");
        goto error;
    }

    free(buf);
    free(saved_buf);

    PASSED();

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return 0;

error:
    if (buf)
        free(buf);
    if (saved_buf)
        free(saved_buf);
    fflush(stdout);
    H5E_BEGIN_TRY
    {
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Pclose(dxpl_id);
        H5Fclose(file);
    }
    H5E_END_TRY

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return MAX((int)fails_this_test, 1);
}

/*-------------------------------------------------------------------------
 * Function:    test_derived_complex
 *
 * Purpose:     Tests user-defined and query functions of complex number
 *              types.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_derived_complex(void)
{
    unsigned char *buf             = NULL;
    unsigned char *saved_buf       = NULL;
    unsigned int   fails_this_test = 0;
    const size_t   max_fails       = 40;
    size_t         spos, epos, esize, mpos, msize, size;
    size_t         src_size;
    size_t         nelmts  = NTESTELEM;
    hid_t          file    = H5I_INVALID_HID;
    hid_t          dxpl_id = H5I_INVALID_HID;
    hid_t          tid     = H5I_INVALID_HID;
    hid_t          flt_tid = H5I_INVALID_HID;
    char           filename[1024];
    char           str[256];
    int           *aligned = NULL;
    int            endian;

    TESTING("user-defined and query functions of complex number types");

    /* Create File */
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't create file\n");
        goto error;
    }

    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0) {
        H5_FAILED();
        printf("Can't create data transfer property list\n");
        goto error;
    }

    /*------------------------------------------------------------------------
     *                   derived floating-point type
     * size=7 byte, precision=42 bits, offset=3 bits, mantissa size=31 bits,
     * mantissa position=3, exponent size=10 bits, exponent position=34,
     * exponent bias=511.  It can be illustrated in little-endian order as
     *
     *          6       5       4       3       2       1       0
     *    ???????? ???SEEEE EEEEEEMM MMMMMMMM MMMMMMMM MMMMMMMM MMMMM???
     *
     * To create a new floating-point type, the following properties must be
     * set in the order of
     *   set fields -> set offset -> set precision -> set size.
     * All these properties must be set before the type can function. Other
     * properties can be set anytime.  Derived type size cannot be expanded
     * bigger than original size but can be decreased.  There should be no
     * holes among the significant bits.  Exponent bias usually is set
     * 2^(n-1)-1, where n is the exponent size.
     *-----------------------------------------------------------------------*/
    if ((flt_tid = H5Tcopy(H5T_IEEE_F64LE)) < 0) {
        H5_FAILED();
        printf("Can't copy data type\n");
        goto error;
    }
    if (H5Tset_fields(flt_tid, (size_t)44, (size_t)34, (size_t)10, (size_t)3, (size_t)31) < 0) {
        H5_FAILED();
        printf("Can't set fields\n");
        goto error;
    }
    if (H5Tset_offset(flt_tid, (size_t)3) < 0) {
        H5_FAILED();
        printf("Can't set offset\n");
        goto error;
    }
    if (H5Tset_precision(flt_tid, (size_t)42) < 0) {
        H5_FAILED();
        printf("Can't set precision 1\n");
        goto error;
    }
    if (H5Tset_size(flt_tid, (size_t)7) < 0) {
        H5_FAILED();
        printf("Can't set size\n");
        goto error;
    }
    if (H5Tset_ebias(flt_tid, (size_t)511) < 0) {
        H5_FAILED();
        printf("Can't set exponent bias\n");
        goto error;
    }
    if (H5Tset_pad(flt_tid, H5T_PAD_ZERO, H5T_PAD_ZERO) < 0) {
        H5_FAILED();
        printf("Can't set padding\n");
        goto error;
    }

    /* Create complex number type from derived floating-point type */
    if ((tid = H5Tcomplex_create(flt_tid)) < 0) {
        H5_FAILED();
        printf("Can't create complex number type\n");
        goto error;
    }

    if (H5Tcommit2(file, "new complex number type 1", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit datatype\n");
        goto error;
    }
    if (H5Tclose(tid) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((tid = H5Topen2(file, "new complex number type 1", H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't open datatype");
    if (H5Tget_fields(tid, &spos, &epos, &esize, &mpos, &msize) < 0) {
        H5_FAILED();
        printf("Can't get fields\n");
        goto error;
    }
    if (spos != 44 || epos != 34 || esize != 10 || mpos != 3 || msize != 31) {
        H5_FAILED();
        printf("Wrong field values\n");
        goto error;
    }
    if (H5Tget_precision(tid) != 42) {
        H5_FAILED();
        printf("Can't get precision or wrong precision\n");
        goto error;
    }
    if (H5Tget_offset(tid) != 3) {
        H5_FAILED();
        printf("Can't get offset or wrong offset\n");
        goto error;
    }
    if ((size = H5Tget_size(tid)) != 14) { /* Size of complex number type is 2 * floating-point type size */
        H5_FAILED();
        printf("Can't get size or wrong size\n");
        goto error;
    }
    if (H5Tget_ebias(tid) != 511) {
        H5_FAILED();
        printf("Can't get exponent bias or wrong bias\n");
        goto error;
    }

    /* Convert data from native integer to the derived complex number type.
     * Then convert data from the complex number type back to native integer.
     * Compare the final data with the original data.
     */
    src_size  = H5Tget_size(H5T_NATIVE_INT);
    endian    = H5Tget_order(H5T_NATIVE_INT);
    buf       = malloc(nelmts * (MAX(src_size, size)));
    saved_buf = malloc(nelmts * src_size);
    aligned   = calloc((size_t)1, src_size);
    memset(buf, 0, nelmts * MAX(src_size, size));
    memset(saved_buf, 0, nelmts * src_size);

    for (size_t i = 0; i < nelmts * src_size; i++)
        buf[i] = saved_buf[i] = (unsigned char)rand();

    if (H5Tconvert(H5T_NATIVE_INT, tid, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }
    if (H5Tconvert(tid, H5T_NATIVE_INT, nelmts, buf, NULL, dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't convert data\n");
        goto error;
    }

    /* Are the values still the same?*/
    for (size_t i = 0; i < nelmts; i++) {
        size_t j;

        for (j = 0; j < src_size; j++)
            if (buf[i * src_size + j] != saved_buf[i * src_size + j])
                break;
        if (j == src_size)
            continue; /*no error*/

        /* Print errors */
        if (0 == fails_this_test++) {
            snprintf(str, sizeof(str),
                     "\nTesting conversions between random integers and derived complex number type");
            printf("%-70s", str);
            fflush(stdout);
            H5_FAILED();
        }
        printf("    test %u elmt %u: \n", 1, (unsigned)i);

        printf("        src = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", saved_buf[i * src_size + ENDIAN(src_size, j, endian)]);

        memcpy(aligned, saved_buf + i * sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        printf("        dst = ");
        for (j = 0; j < src_size; j++)
            printf(" %02x", buf[i * src_size + ENDIAN(src_size, j, endian)]);

        memcpy(aligned, buf + i * sizeof(int), sizeof(int));
        printf(" %29d\n", *aligned);

        if (fails_this_test >= max_fails) {
            puts("    maximum failures reached, aborting test...");
            goto error;
        }
    }

    fails_this_test = 0;
    free(buf);
    free(saved_buf);
    free(aligned);
    buf       = NULL;
    saved_buf = NULL;
    aligned   = NULL;

    if (H5Tclose(flt_tid) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Tclose(tid) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Pclose(dxpl_id) < 0) {
        H5_FAILED();
        printf("Can't close property list\n");
        goto error;
    }

    if (H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    }

    if (H5Fdelete(filename, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't delete file\n");
        goto error;
    }

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 0;

error:
    free(buf);
    free(saved_buf);
    free(aligned);

    H5E_BEGIN_TRY
    {
        H5Tclose(flt_tid);
        H5Tclose(tid);
        H5Pclose(dxpl_id);
        H5Fclose(file);
    }
    H5E_END_TRY

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return MAX((int)fails_this_test, 1);
}

/*-------------------------------------------------------------------------
 * Function:    test_bfloat16
 *
 * Purpose:     Tests special values for bfloat16 datatypes
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_bfloat16(void)
{
    const unsigned char *buf_ptr;
    H5T_order_t          native_type_order;
    uint16_t             bf16_val;
    uint16_t             bf16_convval;
    size_t               float_spos;
    size_t               float_mpos;
    size_t               float_epos;
    size_t               float_msize;
    size_t               float_esize;
    hid_t                src_bf16_type;
    float                val_buf;

    TESTING("bfloat16 datatype special values");

    buf_ptr = (const unsigned char *)&val_buf;

    if ((native_type_order = H5Tget_order(H5T_NATIVE_FLOAT)) < 0) {
        H5_FAILED();
        printf("Can't check endian-ness of native float type\n");
        goto error;
    }

    /* Just test on little- or big-endian systems */
    if (native_type_order != H5T_ORDER_LE && native_type_order != H5T_ORDER_BE) {
        SKIPPED();
        return 0;
    }

    src_bf16_type = (native_type_order == H5T_ORDER_LE) ? H5T_FLOAT_BFLOAT16LE : H5T_FLOAT_BFLOAT16BE;

    if (H5Tget_fields(H5T_NATIVE_FLOAT, &float_spos, &float_epos, &float_esize, &float_mpos, &float_msize) <
        0) {
        H5_FAILED();
        printf("Can't get floating-point bit field information for native float type\n");
        goto error;
    }

    /* Until native support for bfloat16 type is added, use uint16_t
     * to represent initial value, then check properties after using
     * H5T to convert to float
     */

    bf16_val = 0x7F80; /* +Inf */
    memcpy(&val_buf, &bf16_val, 2);
    if (H5Tconvert(src_bf16_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert bfloat16 value to float\n");
        goto error;
    }

    if (0 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("bfloat16 positive infinity value wasn't infinity after conversion\n");
        goto error;
    }

    if (1 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("bfloat16 positive infinity value matched NaN\n");
        goto error;
    }

    /* Convert value back and check */
    if (H5Tconvert(H5T_NATIVE_FLOAT, src_bf16_type, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert float value to bfloat16\n");
        goto error;
    }

    memcpy(&bf16_convval, &val_buf, 2);
    if (0 != memcmp(&bf16_convval, &bf16_val, 2)) {
        H5_FAILED();
        printf("bfloat16 value wasn't preserved between conversions\n");
        goto error;
    }

    bf16_val = 0xFF80; /* -Inf */
    memcpy(&val_buf, &bf16_val, 2);
    if (H5Tconvert(src_bf16_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert bfloat16 value to float\n");
        goto error;
    }

    if (0 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("bfloat16 negative infinity value wasn't infinity after conversion\n");
        goto error;
    }

    if (1 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("bfloat16 negative infinity value matched NaN\n");
        goto error;
    }

    /* Convert value back and check */
    if (H5Tconvert(H5T_NATIVE_FLOAT, src_bf16_type, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert float value to bfloat16\n");
        goto error;
    }

    memcpy(&bf16_convval, &val_buf, 2);
    if (0 != memcmp(&bf16_convval, &bf16_val, 2)) {
        H5_FAILED();
        printf("bfloat16 value wasn't preserved between conversions\n");
        goto error;
    }

    /*
     * For NaN values, don't bother checking the value after converting
     * back. The library sets all bits in the significand to 1 when a
     * NaN is encountered, so the values won't match. Note that at least
     * on x86 and ARM CPUs this should convert the NaNs into quiet NaNs.
     * However, this may convert the NaNs to signaling NaNs on some CPUs
     * which could be problematic if the buffer is used in almost any
     * fashion. The my_isnan() function might attempt to print the value
     * into a buffer to compare against NaN strings, which could cause
     * a floating-point exception for some values. So far, this hasn't
     * been an issue in practice, but may need some exception handling
     * here if it becomes an issue.
     */

    bf16_val = 0xffc1; /* One of many qNaN values */
    memcpy(&val_buf, &bf16_val, 2);
    if (H5Tconvert(src_bf16_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert bfloat16 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("bfloat16 qNaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("bfloat16 qNaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    bf16_val = 0xff81; /* One of many sNaN values */
    memcpy(&val_buf, &bf16_val, 2);
    if (H5Tconvert(src_bf16_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert bfloat16 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("bfloat16 sNaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("bfloat16 sNaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    PASSED();

    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_fp8
 *
 * Purpose:     Tests special values for FP8 datatypes
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_fp8(void)
{
    const unsigned char *buf_ptr;
    H5T_order_t          native_type_order;
    uint8_t              fp8_val;
    uint8_t              fp8_convval;
    size_t               float_spos;
    size_t               float_mpos;
    size_t               float_epos;
    size_t               float_msize;
    size_t               float_esize;
    hid_t                src_fp8_type;
    float                val_buf;

    TESTING("FP8 datatype special values");

    /* Until native support for FP8 type is added, use uint8_t to
     * represent initial value, then check properties after using
     * H5T to convert to float.
     */
    buf_ptr = (const unsigned char *)&val_buf;

    if ((native_type_order = H5Tget_order(H5T_NATIVE_FLOAT)) < 0) {
        H5_FAILED();
        printf("Can't check endian-ness of native float type\n");
        goto error;
    }

    /* Just test on little- or big-endian systems */
    if (native_type_order != H5T_ORDER_LE && native_type_order != H5T_ORDER_BE) {
        SKIPPED();
        return 0;
    }

    if (H5Tget_fields(H5T_NATIVE_FLOAT, &float_spos, &float_epos, &float_esize, &float_mpos, &float_msize) <
        0) {
        H5_FAILED();
        printf("Can't get floating-point bit field information for native float type\n");
        goto error;
    }

    /*
     * Check E4M3 special values
     */
    src_fp8_type = H5T_FLOAT_F8E4M3;

    /* FP8 E4M3 doesn't have infinities, but the library converts some
     * of the larger values into infinities due to trying to interpret
     * the values according to the IEEE standard. So, just check NaN
     * values here. For NaN values, don't bother checking the value
     * after converting back. The library sets all bits in the significand
     * to 1 when a NaN is encountered, so the values won't match. Note
     * that at least on x86 and ARM CPUs this should convert the NaNs
     * into quiet NaNs. However, this may convert the NaNs to signaling
     * NaNs on some CPUs which could be problematic if the buffer is
     * used in almost any fashion. The my_isnan() function might attempt
     * to print the value into a buffer to compare against NaN strings,
     * which could cause a floating-point exception for some values. So
     * far, this hasn't been an issue in practice, but may need some
     * exception handling here if it becomes an issue.
     */

    fp8_val = 0x7f; /* One of the two NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E4M3 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E4M3 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E4M3 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0xff; /* The other NaN value */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E4M3 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E4M3 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E4M3 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    /*
     * Check E5M2 special values
     */
    src_fp8_type = H5T_FLOAT_F8E5M2;

    fp8_val = 0x7c; /* +Inf */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (0 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 positive infinity value wasn't infinity after conversion\n");
        goto error;
    }

    if (1 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 positive infinity value matched NaN\n");
        goto error;
    }

    /* Convert value back and check */
    if (H5Tconvert(H5T_NATIVE_FLOAT, src_fp8_type, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert float value to FP8 E5M2 value\n");
        goto error;
    }

    memcpy(&fp8_convval, &val_buf, 1);
    if (0 != memcmp(&fp8_convval, &fp8_val, 1)) {
        H5_FAILED();
        printf("FP8 E5M2 value wasn't preserved between conversions\n");
        goto error;
    }

    fp8_val = 0xfc; /* -Inf */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (0 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 negative infinity value wasn't infinity after conversion\n");
        goto error;
    }

    if (1 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 negative infinity value matched NaN\n");
        goto error;
    }

    /* Convert value back and check */
    if (H5Tconvert(H5T_NATIVE_FLOAT, src_fp8_type, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert float value to FP8 E5M2 value\n");
        goto error;
    }

    memcpy(&fp8_convval, &val_buf, 1);
    if (0 != memcmp(&fp8_convval, &fp8_val, 1)) {
        H5_FAILED();
        printf("FP8 E5M2 value wasn't preserved between conversions\n");
        goto error;
    }

    /*
     * For NaN values, don't bother checking the value after converting
     * back. The library sets all bits in the significand to 1 when a
     * NaN is encountered, so the values won't match. Note that at least
     * on x86 and ARM CPUs this should convert the NaNs into quiet NaNs.
     * However, this may convert the NaNs to signaling NaNs on some CPUs
     * which could be problematic if the buffer is used in almost any
     * fashion. The my_isnan() function might attempt to print the value
     * into a buffer to compare against NaN strings, which could cause
     * a floating-point exception for some values. So far, this hasn't
     * been an issue in practice, but may need some exception handling
     * here if it becomes an issue.
     */

    fp8_val = 0x7d; /* One of the three positive NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0x7e; /* Another of the three positive NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0x7f; /* Last of the three positive NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0xfd; /* One of the three negative NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0xfe; /* Another of the three negative NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    fp8_val = 0xff; /* Last of the three negative NaN values */
    memcpy(&val_buf, &fp8_val, 1);
    if (H5Tconvert(src_fp8_type, H5T_NATIVE_FLOAT, 1, &val_buf, NULL, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Couldn't convert FP8 E5M2 value to float\n");
        goto error;
    }

    if (1 == my_isinf((int)native_type_order, buf_ptr, sizeof(float), float_mpos, float_msize, float_epos,
                      float_esize)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value was an infinity value after conversion\n");
        goto error;
    }

    if (0 == my_isnan(FLT_FLOAT, &val_buf)) {
        H5_FAILED();
        printf("FP8 E5M2 NaN value wasn't a NaN value after conversion\n");
        goto error;
    }

    PASSED();

    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_1
 *
 * Purpose:    Test conversion of integer values from SRC to DST.
 *             These types should be any combination of:
 *
 *             H5T_NATIVE_SCHAR    H5T_NATIVE_UCHAR
 *             H5T_NATIVE_SHORT    H5T_NATIVE_USHORT
 *             H5T_NATIVE_INT      H5T_NATIVE_UINT
 *             H5T_NATIVE_LONG     H5T_NATIVE_ULONG
 *             H5T_NATIVE_LLONG    H5T_NATIVE_ULLONG
 *
 * Return:    Success:    0
 *            Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_1(const char *name, hid_t src, hid_t dst)
{
    size_t             nelmts          = 0;  /*num values per test    */
    const size_t       max_fails       = 8;  /*max number of failures*/
    size_t             fails_all_tests = 0;  /*number of failures    */
    size_t             fails_this_test;      /*fails for this test    */
    char               str[256];             /*hello string        */
    dtype_t            src_type, dst_type;   /*data types        */
    const char        *src_type_name = NULL; /*source type name    */
    const char        *dst_type_name = NULL; /*destination type name    */
    int                endian;               /*machine endianness    */
    size_t             src_size, dst_size;   /*type sizes        */
    unsigned char     *buf   = NULL;         /*buffer for conversion    */
    unsigned char     *saved = NULL;         /*original values    */
    size_t             j, k;                 /*counters        */
    unsigned char     *hw = NULL;            /*hardware conv result    */
    unsigned char      src_bits[32];         /*src value in LE order    */
    unsigned char      dst_bits[32];         /*dest value in LE order*/
    size_t             src_nbits;            /*source length in bits    */
    size_t             dst_nbits;            /*dst length in bits    */
    H5T_sign_t         src_sign;             /*source sign type      */
    H5T_sign_t         dst_sign;             /*dst sign type         */
    void              *aligned = NULL;       /*aligned temp buffer    */
    signed char        hw_char;
    unsigned char      hw_uchar;
    short              hw_short;
    unsigned short     hw_ushort;
    int                hw_int;
    unsigned           hw_uint;
    long               hw_long;
    unsigned long      hw_ulong;
    long long          hw_llong;
    unsigned long long hw_ullong;

    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_SCHAR)) {
        src_type_name = "signed char";
        src_type      = INT_SCHAR;
    }
    else if (H5Tequal(src, H5T_NATIVE_UCHAR)) {
        src_type_name = "unsigned char";
        src_type      = INT_UCHAR;
    }
    else if (H5Tequal(src, H5T_NATIVE_SHORT)) {
        src_type_name = "short";
        src_type      = INT_SHORT;
    }
    else if (H5Tequal(src, H5T_NATIVE_USHORT)) {
        src_type_name = "unsigned short";
        src_type      = INT_USHORT;
    }
    else if (H5Tequal(src, H5T_NATIVE_INT)) {
        src_type_name = "int";
        src_type      = INT_INT;
    }
    else if (H5Tequal(src, H5T_NATIVE_UINT)) {
        src_type_name = "unsigned int";
        src_type      = INT_UINT;
    }
    else if (H5Tequal(src, H5T_NATIVE_LONG)) {
        src_type_name = "long";
        src_type      = INT_LONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_ULONG)) {
        src_type_name = "unsigned long";
        src_type      = INT_ULONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_LLONG)) {
        src_type_name = "long long";
        src_type      = INT_LLONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_ULLONG)) {
        src_type_name = "unsigned long long";
        src_type      = INT_ULLONG;
    }
    else {
        src_type_name = "UNKNOWN";
        src_type      = OTHER;
    }

    if (H5Tequal(dst, H5T_NATIVE_SCHAR)) {
        dst_type_name = "signed char";
        dst_type      = INT_SCHAR;
    }
    else if (H5Tequal(dst, H5T_NATIVE_UCHAR)) {
        dst_type_name = "unsigned char";
        dst_type      = INT_UCHAR;
    }
    else if (H5Tequal(dst, H5T_NATIVE_SHORT)) {
        dst_type_name = "short";
        dst_type      = INT_SHORT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_USHORT)) {
        dst_type_name = "unsigned short";
        dst_type      = INT_USHORT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_INT)) {
        dst_type_name = "int";
        dst_type      = INT_INT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_UINT)) {
        dst_type_name = "unsigned int";
        dst_type      = INT_UINT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LONG)) {
        dst_type_name = "long";
        dst_type      = INT_LONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_ULONG)) {
        dst_type_name = "unsigned long";
        dst_type      = INT_ULONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LLONG)) {
        dst_type_name = "long long";
        dst_type      = INT_LLONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_ULLONG)) {
        dst_type_name = "unsigned long long";
        dst_type      = INT_ULLONG;
    }
    else {
        dst_type_name = "UNKNOWN";
        dst_type      = OTHER;
    }

    /* Sanity checks */
    if (OTHER == src_type || OTHER == dst_type) {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        H5_FAILED();
        puts("    Unknown data type.");
        goto error;
    }
    else {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        fflush(stdout);
        fails_this_test = 0;
    }

    /* Some information about datatypes */
    endian    = H5Tget_order(H5T_NATIVE_INT);
    src_size  = H5Tget_size(src);
    dst_size  = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */
    src_sign  = H5Tget_sign(src);
    dst_sign  = H5Tget_sign(dst);
    aligned   = calloc((size_t)1, sizeof(long long));

    /* Allocate and initialize the source buffer through macro INIT_INTEGER.  The BUF
     * will be used for the conversion while the SAVED buffer will be
     * used for the comparison later.
     */
    if (src_type == INT_SCHAR) {
        INIT_INTEGER(signed char, SCHAR_MAX, SCHAR_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_UCHAR) {
        INIT_INTEGER(unsigned char, UCHAR_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_SHORT) {
        INIT_INTEGER(short, SHRT_MAX, SHRT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_USHORT) {
        INIT_INTEGER(unsigned short, USHRT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_INT) {
        INIT_INTEGER(int, INT_MAX, INT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_UINT) {
        INIT_INTEGER(unsigned int, UINT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_LONG) {
        INIT_INTEGER(long, LONG_MAX, LONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_ULONG) {
        INIT_INTEGER(unsigned long, ULONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_LLONG) {
        INIT_INTEGER(long long, LLONG_MAX, LLONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_ULLONG) {
        INIT_INTEGER(unsigned long long, ULLONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else
        goto error;

    /* Perform the conversion */
    if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;

    /* Check the results from the library against hardware */
    for (j = 0; j < nelmts; j++) {
        if (INT_SCHAR == dst_type) {
            hw = (unsigned char *)&hw_char;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_char = (signed char)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_char = (signed char)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_char = (signed char)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_char = (signed char)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_char = (signed char)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_char = (signed char)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_char = (signed char)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_char = (signed char)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_char = (signed char)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_char = (signed char)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_UCHAR == dst_type) {
            hw = (unsigned char *)&hw_uchar;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_uchar = (unsigned char)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_uchar = (unsigned char)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_uchar = (unsigned char)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_uchar = (unsigned char)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_uchar = (unsigned char)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_uchar = (unsigned char)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_uchar = (unsigned char)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_uchar = (unsigned char)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_uchar = (unsigned char)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_uchar = (unsigned char)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_SHORT == dst_type) {
            hw = (unsigned char *)&hw_short;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(char), sizeof(char));
                    hw_short = (short)(*((char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_short = (short)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_short = (short)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_short = (short)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_short = (short)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_short = (short)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_short = (short)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_short = (short)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_short = (short)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_short = (short)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_USHORT == dst_type) {
            hw = (unsigned char *)&hw_ushort;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_ushort = (unsigned short)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_ushort = (unsigned short)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_ushort = (unsigned short)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_ushort = (unsigned short)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_ushort = (unsigned short)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_ushort = (unsigned short)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_ushort = (unsigned short)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_ushort = (unsigned short)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_ushort = (unsigned short)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_ushort = (unsigned short)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_INT == dst_type) {
            hw = (unsigned char *)&hw_int;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_int = (int)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_int = (int)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_int = (int)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_int = (int)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_int = (int)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_int = (int)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_int = (int)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_int = (int)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_int = (int)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_int = (int)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_UINT == dst_type) {
            hw = (unsigned char *)&hw_uint;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_uint = (unsigned int)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_uint = (unsigned int)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_uint = (unsigned int)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_uint = (unsigned int)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_uint = (unsigned int)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_uint = (unsigned int)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_uint = (unsigned int)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_uint = (unsigned int)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_uint = (unsigned int)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_uint = (unsigned int)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_LONG == dst_type) {
            hw = (unsigned char *)&hw_long;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_long = (long int)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_long = (long int)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_long = (long int)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_long = (long int)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_long = (long int)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_long = (long int)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_long = (long int)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_long = (long int)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_long = (long int)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_long = (long int)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_ULONG == dst_type) {
            hw = (unsigned char *)&hw_ulong;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_ulong = (unsigned long)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_ulong = (unsigned long)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_ulong = (unsigned long)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_ulong = (unsigned long)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_ulong = (unsigned long)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_ulong = (unsigned long)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_ulong = (unsigned long)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_ulong = (unsigned long)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_ulong = (unsigned long)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_ulong = (unsigned long)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_LLONG == dst_type) {
            hw = (unsigned char *)&hw_llong;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(char), sizeof(char));
                    hw_llong = (long long)(*((char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_llong = (long long)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_llong = (long long)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_llong = (long long)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_llong = (long long)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_llong = (long long)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_llong = (long long)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_llong = (long long)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_llong = (long long)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_llong = (long long)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }
        else if (INT_ULLONG == dst_type) {
            hw = (unsigned char *)&hw_ullong;
            switch (src_type) {
                case INT_SCHAR:
                    memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                    hw_ullong = (unsigned long long)(*((signed char *)aligned));
                    break;
                case INT_UCHAR:
                    memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                    hw_ullong = (unsigned long long)(*((unsigned char *)aligned));
                    break;
                case INT_SHORT:
                    memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                    hw_ullong = (unsigned long long)(*((short *)aligned));
                    break;
                case INT_USHORT:
                    memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                    hw_ullong = (unsigned long long)(*((unsigned short *)aligned));
                    break;
                case INT_INT:
                    memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                    hw_ullong = (unsigned long long)(*((int *)aligned));
                    break;
                case INT_UINT:
                    memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                    hw_ullong = (unsigned long long)(*((unsigned *)aligned));
                    break;
                case INT_LONG:
                    memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                    hw_ullong = (unsigned long long)(*((long *)aligned));
                    break;
                case INT_ULONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                    hw_ullong = (unsigned long long)(*((unsigned long *)aligned));
                    break;
                case INT_LLONG:
                    memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                    hw_ullong = (unsigned long long)(*((long long *)aligned));
                    break;
                case INT_ULLONG:
                    memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                    hw_ullong = (unsigned long long)(*((unsigned long long *)aligned));
                    break;
                case FLT_FLOAT16:
                case FLT_FLOAT:
                case FLT_DOUBLE:
                case FLT_LDOUBLE:
                case FLT_COMPLEX:
                case DBL_COMPLEX:
                case LDBL_COMPLEX:
                case OTHER:
                default:
                    assert(0 && "Unknown type");
                    break;
            }
        }

        /* Make certain that there isn't some weird number of destination bits */
        assert(dst_nbits % 8 == 0);

        /* Are the two results the same? */
        for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
            if (buf[j * dst_size + k] != hw[k])
                break;
        if (k == dst_size)
            continue; /*no error*/

        /*
         * Convert the source and destination values to little endian
         * order so we can use the HDF5 bit vector operations to test
         * certain things.  These routines have already been tested by
         * the `bittests' program.
         */
        for (k = 0; k < src_size; k++)
            src_bits[src_size - (k + 1)] = saved[j * src_size + ENDIAN(src_size, k, endian)];

        for (k = 0; k < dst_size; k++)
            dst_bits[dst_size - (k + 1)] = buf[j * dst_size + ENDIAN(dst_size, k, endian)];

        /*
         * Hardware usually doesn't handle overflows too gracefully. The
         * hardware conversion result during overflows is usually garbage
         * so we must handle those cases differetly when checking results.
         */
        if (H5T_SGN_2 == src_sign && H5T_SGN_2 == dst_sign) {
            if (src_nbits > dst_nbits) {
                if (0 == H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1) &&
                    H5T__bit_find(src_bits, dst_nbits - 1, (src_nbits - dst_nbits), H5T_BIT_MSB, 1) >= 0) {
                    /*
                     * Source is positive and the magnitude is too large for
                     * the destination.  The destination should be set to the
                     * maximum possible value: 0x7f...f
                     */
                    if (0 == H5T__bit_get_d(dst_bits, dst_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, (size_t)0, dst_nbits - 1, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                }
                else if (1 == H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1) &&
                         H5T__bit_find(src_bits, (size_t)0, src_nbits - 1, H5T_BIT_MSB, 0) + 1 >=
                             (ssize_t)dst_nbits) {
                    /*
                     * Source is negative but the magnitude is too large for
                     * the destination. The destination should be set to the
                     * smallest possible value: 0x80...0
                     */
                    if (1 == H5T__bit_get_d(dst_bits, dst_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, (size_t)0, dst_nbits - 1, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                }
            }
            else if (src_nbits < dst_nbits) {
                /* Source is smaller than the destination */
                if (0 == H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1)) {
                    /*
                     * Source is positive, so the excess bits in the
                     * destination should be set to 0's.
                     */
                    if (0 == H5T__bit_get_d(dst_bits, src_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, src_nbits, dst_nbits - src_nbits, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                }
                else {
                    /*
                     * Source is negative, so the excess bits in the
                     * destination should be set to 1's.
                     */
                    if (1 == H5T__bit_get_d(dst_bits, src_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, src_nbits, dst_nbits - src_nbits, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                }
            }
        }
        else if (H5T_SGN_2 == src_sign && H5T_SGN_NONE == dst_sign) {
            if (H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1)) {
                /*
                 * The source is negative so the result should be zero.
                 * The source is negative if the most significant bit is
                 * set.  The destination is zero if all bits are zero.
                 */
                if (H5T__bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 1) < 0)
                    continue; /*no error*/
            }
            else if (src_nbits > dst_nbits &&
                     H5T__bit_find(src_bits, dst_nbits - 1, src_nbits - dst_nbits, H5T_BIT_LSB, 1) >= 0) {
                /*
                 * The source is a value with a magnitude too large for
                 * the destination.  The destination should be the
                 * largest possible value: 0xff...f
                 */
                if (H5T__bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        }
        else if (H5T_SGN_NONE == src_sign && H5T_SGN_2 == dst_sign) {
            if (src_nbits >= dst_nbits &&
                H5T__bit_find(src_bits, dst_nbits - 1, (src_nbits - dst_nbits) + 1, H5T_BIT_LSB, 1) >= 0) {
                /*
                 * The source value has a magnitude that is larger than
                 * the destination can handle.  The destination should be
                 * set to the largest possible positive value: 0x7f...f
                 */
                if (0 == H5T__bit_get_d(dst_bits, dst_nbits - 1, (size_t)1) &&
                    H5T__bit_find(dst_bits, (size_t)0, dst_nbits - 1, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        }
        else {
            if (src_nbits > dst_nbits &&
                H5T__bit_find(src_bits, dst_nbits, src_nbits - dst_nbits, H5T_BIT_LSB, 1) >= 0) {
                /*
                 * The unsigned source has a value which is too large for
                 * the unsigned destination.  The destination should be
                 * set to the largest possible value: 0xff...f
                 */
                if (H5T__bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                    continue; /*no error*/
            }
        }

        /* Print errors */
        if (0 == fails_this_test++)
            H5_FAILED();
        printf("    elmt %u\n", (unsigned)j);

        printf("        src = ");
        for (k = 0; k < src_size; k++)
            printf(" %02x", saved[j * src_size + ENDIAN(src_size, k, endian)]);
        printf("%*s", (int)(3 * MAX(0, (ssize_t)dst_size - (ssize_t)src_size)), "");
        switch (src_type) {
            case INT_SCHAR:
                memcpy(aligned, saved + j * sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char *)aligned));
                break;
            case INT_UCHAR:
                memcpy(aligned, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char *)aligned));
                break;
            case INT_SHORT:
                memcpy(aligned, saved + j * sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short *)aligned));
                break;
            case INT_USHORT:
                memcpy(aligned, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short *)aligned));
                break;
            case INT_INT:
                memcpy(aligned, saved + j * sizeof(int), sizeof(int));
                printf(" %29d\n", *((int *)aligned));
                break;
            case INT_UINT:
                memcpy(aligned, saved + j * sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned *)aligned));
                break;
            case INT_LONG:
                memcpy(aligned, saved + j * sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long *)aligned));
                break;
            case INT_ULONG:
                memcpy(aligned, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long *)aligned));
                break;
            case INT_LLONG:
                memcpy(aligned, saved + j * sizeof(long long), sizeof(long long));
                fprintf(stdout, " %29lld\n", *((long long *)aligned));
                break;
            case INT_ULLONG:
                memcpy(aligned, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                fprintf(stdout, " %29llu\n", *((unsigned long long *)aligned));
                break;
            case FLT_FLOAT16:
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        printf("        dst = ");
        for (k = 0; k < dst_size; k++)
            printf(" %02x", buf[j * dst_size + ENDIAN(dst_size, k, endian)]);
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                memcpy(aligned, buf + j * sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)*((signed char *)aligned));
                break;
            case INT_UCHAR:
                memcpy(aligned, buf + j * sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)*((unsigned char *)aligned));
                break;
            case INT_SHORT:
                memcpy(aligned, buf + j * sizeof(short), sizeof(short));
                printf(" %29hd\n", *((short *)aligned));
                break;
            case INT_USHORT:
                memcpy(aligned, buf + j * sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", *((unsigned short *)aligned));
                break;
            case INT_INT:
                memcpy(aligned, buf + j * sizeof(int), sizeof(int));
                printf(" %29d\n", *((int *)aligned));
                break;
            case INT_UINT:
                memcpy(aligned, buf + j * sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", *((unsigned *)aligned));
                break;
            case INT_LONG:
                memcpy(aligned, buf + j * sizeof(long), sizeof(long));
                printf(" %29ld\n", *((long *)aligned));
                break;
            case INT_ULONG:
                memcpy(aligned, buf + j * sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", *((unsigned long *)aligned));
                break;
            case INT_LLONG:
                memcpy(aligned, buf + j * sizeof(long long), sizeof(long long));
                fprintf(stdout, " %29lld\n", *((long long *)aligned));
                break;
            case INT_ULLONG:
                memcpy(aligned, buf + j * sizeof(long long), sizeof(unsigned long long));
                fprintf(stdout, " %29llu\n", *((unsigned long long *)aligned));
                break;
            case FLT_FLOAT16:
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        printf("        ans = ");
        for (k = 0; k < dst_size; k++)
            printf(" %02x", hw[ENDIAN(dst_size, k, endian)]);
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                printf(" %29d\n", (int)*((signed char *)((void *)hw)));
                break;
            case INT_UCHAR:
                printf(" %29u\n", (unsigned)*((unsigned char *)((void *)hw)));
                break;
            case INT_SHORT:
                printf(" %29hd\n", *((short *)((void *)hw)));
                break;
            case INT_USHORT:
                printf(" %29hu\n", *((unsigned short *)((void *)hw)));
                break;
            case INT_INT:
                printf(" %29d\n", *((int *)((void *)hw)));
                break;
            case INT_UINT:
                printf(" %29u\n", *((unsigned *)((void *)hw)));
                break;
            case INT_LONG:
                printf(" %29ld\n", *((long *)((void *)hw)));
                break;
            case INT_ULONG:
                printf(" %29lu\n", *((unsigned long *)((void *)hw)));
                break;
            case INT_LLONG:
                fprintf(stdout, " %29lld\n", *((long long *)((void *)hw)));
                break;
            case INT_ULLONG:
                fprintf(stdout, " %29llu\n", *((unsigned long long *)((void *)hw)));
                break;
            case FLT_FLOAT16:
            case FLT_FLOAT:
            case FLT_DOUBLE:
            case FLT_LDOUBLE:
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        if (++fails_all_tests >= max_fails) {
            puts("    maximum failures reached, aborting test...");
            puts("    (dst is library's conversion output. ans is compiler's conversion output.)");
            goto done;
        }
    }
    PASSED();

done:
    if (buf)
        aligned_free(buf);
    if (saved)
        aligned_free(saved);
    if (aligned)
        free(aligned);
    fflush(stdout);

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return (int)fails_all_tests;

error:
    if (buf)
        aligned_free(buf);
    if (saved)
        aligned_free(saved);
    if (aligned)
        free(aligned);
    fflush(stdout);

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    return MAX((int)fails_all_tests, 1);
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_2
 *
 * Purpose:    Tests overlap calculates in H5T__conv_i_i(), which should be
 *        the same as for H5T__conv_f_f() and H5T__conv_s_s().
 *
 * Return:    Success:    0
 *
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_2(void)
{
    int   i, j;
    hid_t src_type, dst_type;
    char *buf;

    printf("%-70s", "Testing overlap calculations");
    fflush(stdout);

    buf = (char *)calloc(TMP_BUF_DIM1, TMP_BUF_DIM2);
    assert(buf);

    for (i = 1; i <= TMP_BUF_DIM1; i++) {
        for (j = 1; j <= TMP_BUF_DIM1; j++) {

            /* Source type */
            src_type = H5Tcopy(H5T_NATIVE_CHAR);
            H5Tset_size(src_type, (size_t)i);

            /* Destination type */
            dst_type = H5Tcopy(H5T_NATIVE_CHAR);
            H5Tset_size(dst_type, (size_t)j);

            /*
             * Conversion. If overlap calculations aren't right then an
             * assertion will fail in H5T__conv_i_i()
             */
            H5Tconvert(src_type, dst_type, (size_t)TMP_BUF_DIM2, buf, NULL, H5P_DEFAULT);
            H5Tclose(src_type);
            H5Tclose(dst_type);
        }
    }
    PASSED();
    free(buf);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    my_isnan
 *
 * Purpose:    Determines whether VAL points to NaN.
 *
 * Return:    true or false
 *
 *-------------------------------------------------------------------------
 */
static int
my_isnan(dtype_t type, void *val)
{
    int  retval = 0;
    char s[256];

    if (FLT_FLOAT == type) {
        float x = 0.0F;
        memcpy(&x, val, sizeof(float));
        retval = isnan(x);
    }
    else if (FLT_DOUBLE == type) {
        double x = 0.0;
        memcpy(&x, val, sizeof(double));
        retval = isnan(x);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (FLT_LDOUBLE == type) {
        long double x = 0.0L;
        memcpy(&x, val, sizeof(long double));
        retval = isnan(x);
#endif
    }
    else if (FLT_FLOAT16 == type) {
#ifdef H5_HAVE__FLOAT16
        H5__Float16 x;
        memcpy(&x, val, sizeof(H5__Float16));
        retval = isnan(x);
#else
        assert(0 && "Should not reach this point!");
#endif
    }
    else if (FLT_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
        H5_float_complex x;
        float            x_real, x_imag;
        memcpy(&x, val, sizeof(H5_float_complex));
        x_real = crealf(x);
        x_imag = cimagf(x);
        if (isinf(x_real) || isinf(x_imag))
            retval = 0;
        else
            retval = isnan(x_real) || isnan(x_imag);
#else
        assert(0 && "Should not reach this point!");
#endif
    }
    else if (DBL_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
        H5_double_complex x;
        double            x_real, x_imag;
        memcpy(&x, val, sizeof(H5_double_complex));
        x_real = creal(x);
        x_imag = cimag(x);
        if (isinf(x_real) || isinf(x_imag))
            retval = 0;
        else
            retval = isnan(x_real) || isnan(x_imag);
#else
        assert(0 && "Should not reach this point!");
#endif
    }
    else if (LDBL_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
        H5_ldouble_complex x;
        long double        x_real, x_imag;
        memcpy(&x, val, sizeof(H5_ldouble_complex));
        x_real = creall(x);
        x_imag = cimagl(x);
        if (isinf(x_real) || isinf(x_imag))
            retval = 0;
        else
            retval = isnan(x_real) || isnan(x_imag);
#else
        assert(0 && "Should not reach this point!");
#endif
    }
    else {
        return 0;
    }

    /*
     * Sometimes NaN==NaN (e.g., DEC Alpha) so we try to print it and see if
     * the result contains a NaN string.
     */
    if (!retval) {
        if (FLT_FLOAT == type) {
            float x = 0.0F;

            memcpy(&x, val, sizeof(float));
            snprintf(s, sizeof(s), "%g", (double)x);
        }
        else if (FLT_DOUBLE == type) {
            double x = 0.0;

            memcpy(&x, val, sizeof(double));
            snprintf(s, sizeof(s), "%g", x);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        }
        else if (FLT_LDOUBLE == type) {
            long double x = 0.0L;

            memcpy(&x, val, sizeof(long double));
            snprintf(s, sizeof(s), "%Lg", x);
#endif
        }
        else if (FLT_FLOAT16 == type) {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 x;

            memcpy(&x, val, sizeof(H5__Float16));
            snprintf(s, sizeof(s), "%g", (double)x);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
        else if (FLT_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
            H5_float_complex x;
            float            x_real;
            memcpy(&x, val, sizeof(H5_float_complex));
            x_real = crealf(x);
            snprintf(s, sizeof(s), "%g", (double)x_real);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
        else if (DBL_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
            H5_double_complex x;
            double            x_real;
            memcpy(&x, val, sizeof(H5_double_complex));
            x_real = creal(x);
            snprintf(s, sizeof(s), "%g", x_real);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
        else if (LDBL_COMPLEX == type) {
#ifdef H5_HAVE_COMPLEX_NUMBERS
            H5_ldouble_complex x;
            long double        x_real;
            memcpy(&x, val, sizeof(H5_ldouble_complex));
            x_real = creall(x);
            snprintf(s, sizeof(s), "%Lg", x_real);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
        else {
            return 0;
        }
        if (strstr(s, "NaN") || strstr(s, "NAN") || strstr(s, "nan"))
            retval = 1;
    }

    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    my_isinf
 *
 * Purpose:    Determines whether VAL points to +/-infinity.
 *
 * Return:    true or false
 *
 *-------------------------------------------------------------------------
 */
static int
my_isinf(int endian, const unsigned char *val, size_t size, size_t mpos, size_t msize, size_t epos,
         size_t esize)
{
    unsigned char *bits;
    int            retval = 0;
    size_t         i;

    bits = (unsigned char *)calloc((size_t)1, size);

    for (i = 0; i < size; i++)
        bits[size - (i + 1)] = *(val + ENDIAN(size, i, endian));

    if (H5T__bit_find(bits, mpos, msize, H5T_BIT_LSB, 1) < 0 &&
        H5T__bit_find(bits, epos, esize, H5T_BIT_LSB, 0) < 0)
        retval = 1;

    free(bits);

    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_flt16
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from _Float16 to another type by casting. Also checks for
 *              overflow and underflow when the destination type is a type
 *              with a smaller width than _Float16.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE__FLOAT16
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_flt16(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    H5__Float16     aligned;
    conv_func_ret_t ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));

    switch (dst_type) {
        case FLT_FLOAT16:
            *((H5__Float16 *)hw_dst) = aligned;
            break;
        case FLT_FLOAT:
            *((float *)hw_dst) = (float)aligned;
            break;
        case FLT_DOUBLE:
            *((double *)hw_dst) = (double)aligned;
            break;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        case FLT_LDOUBLE:
            *((long double *)hw_dst) = (long double)aligned;
            break;
#endif

#ifdef H5_HAVE_COMPLEX_NUMBERS
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
        case FLT_COMPLEX:
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
            break;
        case DBL_COMPLEX:
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
            break;
        case LDBL_COMPLEX:
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
            break;
#else  /* H5_HAVE_C99_COMPLEX_NUMBERS */
        case FLT_COMPLEX:
            *((H5_float_complex *)hw_dst) = H5_CMPLXF(aligned, 0.0F);
            break;
        case DBL_COMPLEX:
            *((H5_double_complex *)hw_dst) = H5_CMPLX(aligned, 0.0);
            break;
        case LDBL_COMPLEX:
            *((H5_ldouble_complex *)hw_dst) = H5_CMPLXL(aligned, 0.0L);
            break;
#endif /* H5_HAVE_C99_COMPLEX_NUMBERS */
#else  /* H5_HAVE_COMPLEX_NUMBERS */
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif /* H5_HAVE_COMPLEX_NUMBERS */

        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}
#endif

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_flt
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from float to another type by casting. Also checks for
 *              overflow and underflow when the destination type is a
 *              type with a smaller width than float.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_flt(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    float           aligned;
    conv_func_ret_t ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(float), sizeof(float));

    switch (dst_type) {
        case FLT_FLOAT:
            *((float *)hw_dst) = aligned;
            break;
        case FLT_DOUBLE:
            *((double *)hw_dst) = (double)aligned;
            break;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        case FLT_LDOUBLE:
            *((long double *)hw_dst) = (long double)aligned;
            break;
#endif
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;

            /* Check for overflow and underflow */
            if (fabsf(aligned) > (float)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsf(aligned) < (float)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif

#ifdef H5_HAVE_COMPLEX_NUMBERS
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
        case FLT_COMPLEX:
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
            break;
        case DBL_COMPLEX:
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
            break;
        case LDBL_COMPLEX:
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
            break;
#else  /* H5_HAVE_C99_COMPLEX_NUMBERS */
        case FLT_COMPLEX:
            *((H5_float_complex *)hw_dst) = H5_CMPLXF(aligned, 0.0F);
            break;
        case DBL_COMPLEX:
            *((H5_double_complex *)hw_dst) = H5_CMPLX(aligned, 0.0);
            break;
        case LDBL_COMPLEX:
            *((H5_ldouble_complex *)hw_dst) = H5_CMPLXL(aligned, 0.0L);
            break;
#endif /* H5_HAVE_C99_COMPLEX_NUMBERS */
#else  /* H5_HAVE_COMPLEX_NUMBERS */
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif /* H5_HAVE_COMPLEX_NUMBERS */

        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_double
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from double to another type by casting. Also checks for
 *              overflow and underflow when the destination type is a
 *              type with a smaller width than double.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_double(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    double          aligned;
    conv_func_ret_t ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(double), sizeof(double));

    switch (dst_type) {
        case FLT_FLOAT:
            *((float *)hw_dst) = (float)aligned;

            /* Check for overflow and underflow */
            if (fabs(aligned) > (double)FLT_MAX)
                ret = CONV_OVERFLOW;
            else if (fabs(aligned) < (double)FLT_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_DOUBLE:
            *((double *)hw_dst) = aligned;
            break;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        case FLT_LDOUBLE:
            *((long double *)hw_dst) = (long double)aligned;
            break;
#endif
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;

            /* Check for overflow and underflow */
            if (fabs(aligned) > (double)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabs(aligned) < (double)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
#else
            *((H5_float_complex *)hw_dst) = H5_CMPLXF(aligned, 0.0F);
#endif

            /* Check for overflow and underflow */
            if (fabs(aligned) > (double)FLT_MAX)
                ret = CONV_OVERFLOW_REAL;
            else if (fabs(aligned) < (double)FLT_MIN)
                ret = CONV_UNDERFLOW_REAL;

            break;
        case DBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
#else
            *((H5_double_complex *)hw_dst) = H5_CMPLX(aligned, 0.0);
#endif
            break;
        case LDBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
#else
            *((H5_ldouble_complex *)hw_dst) = H5_CMPLXL(aligned, 0.0L);
#endif
            break;
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_ldouble
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from long double to another type by casting. Also checks
 *              for overflow and underflow when the destination type is a
 *              type with a smaller width than long double.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_ldouble(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    long double     aligned;
    conv_func_ret_t ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(long double), sizeof(long double));

    switch (dst_type) {
        case FLT_FLOAT:
            *((float *)hw_dst) = (float)aligned;

            /* Check for overflow and underflow */
            if (fabsl(aligned) > (long double)FLT_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(aligned) < (long double)FLT_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_DOUBLE:
            *((double *)hw_dst) = (double)aligned;

            /* Check for overflow and underflow */
            if (fabsl(aligned) > (long double)DBL_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(aligned) < (long double)DBL_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_LDOUBLE:
            *((long double *)hw_dst) = aligned;
            break;
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;

            /* Check for overflow and underflow */
            if (fabsl(aligned) > (long double)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(aligned) < (long double)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
#else
            *((H5_float_complex *)hw_dst) = H5_CMPLXF(aligned, 0.0F);
#endif

            /* Check for overflow and underflow */
            if (fabsl(aligned) > (long double)FLT_MAX)
                ret = CONV_OVERFLOW_REAL;
            else if (fabsl(aligned) < (long double)FLT_MIN)
                ret = CONV_UNDERFLOW_REAL;

            break;
        case DBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
#else
            *((H5_double_complex *)hw_dst) = H5_CMPLX(aligned, 0.0);
#endif

            /* Check for overflow and underflow */
            if (fabsl(aligned) > (long double)DBL_MAX)
                ret = CONV_OVERFLOW_REAL;
            else if (fabsl(aligned) < (long double)DBL_MIN)
                ret = CONV_UNDERFLOW_REAL;

            break;
        case LDBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
#else
            *((H5_ldouble_complex *)hw_dst) = H5_CMPLXL(aligned, 0.0L);
#endif
            break;
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}
#endif

#ifdef H5_HAVE_COMPLEX_NUMBERS
/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_fcomplex
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from float _Complex / _Fcomplex to another type by casting.
 *              Also checks for overflow and underflow when the destination
 *              type is a type with a smaller width than float.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_fcomplex(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    H5_float_complex aligned;
    conv_func_ret_t  ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));

    switch (dst_type) {
        case FLT_FLOAT:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((float *)hw_dst) = (float)aligned;
#else
            *((float *)hw_dst) = crealf(aligned);
#endif
            break;
        case FLT_DOUBLE:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((double *)hw_dst) = (double)aligned;
#else
            *((double *)hw_dst) = (double)crealf(aligned);
#endif
            break;
        case FLT_LDOUBLE:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((long double *)hw_dst) = (long double)aligned;
#else
            *((long double *)hw_dst) = (long double)crealf(aligned);
#endif
            break;
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
        {
            float real_val = crealf(aligned);

            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;
#else
            *((H5__Float16 *)hw_dst) = (H5__Float16)real_val;
#endif

            /* Check for overflow and underflow */
            if (fabsf(real_val) > (float)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsf(real_val) < (float)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
        }
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
        case FLT_COMPLEX:
            *((H5_float_complex *)hw_dst) = aligned;
            break;
        case DBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
#else
            *((H5_double_complex *)hw_dst) = H5_CMPLX((double)crealf(aligned), (double)cimagf(aligned));
#endif
            break;
        case LDBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
#else
            *((H5_ldouble_complex *)hw_dst) =
                H5_CMPLXL((long double)crealf(aligned), (long double)crealf(aligned));
#endif
            break;
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_dcomplex
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from double _Complex / _Dcomplex to another type by
 *              casting. Also checks for overflow and underflow when the
 *              destination type is a type with a smaller width than
 *              double.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_dcomplex(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    H5_double_complex aligned;
    double            real_val, imag_val;
    conv_func_ret_t   ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));

    switch (dst_type) {
        case FLT_FLOAT:
            real_val = creal(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((float *)hw_dst) = (float)aligned;
#else
            *((float *)hw_dst) = (float)real_val;
#endif

            /* Check for overflow and underflow */
            if (fabs(real_val) > (double)FLT_MAX)
                ret = CONV_OVERFLOW;
            else if (fabs(real_val) < (double)FLT_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_DOUBLE:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((double *)hw_dst) = (double)aligned;
#else
            *((double *)hw_dst) = (double)creal(aligned);
#endif
            break;
        case FLT_LDOUBLE:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((long double *)hw_dst) = (long double)aligned;
#else
            *((long double *)hw_dst) = (long double)creal(aligned);
#endif
            break;
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;
#else
            *((H5__Float16 *)hw_dst) = (H5__Float16)creal(aligned);
#endif

            /* Check for overflow and underflow */
            real_val = creal(aligned);
            if (fabs(real_val) > (double)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabs(real_val) < (double)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
        case FLT_COMPLEX: {
            bool real_over = false, real_under = false;
            bool imag_over = false, imag_under = false;

            real_val = creal(aligned);
            imag_val = cimag(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
#else
            *((H5_float_complex *)hw_dst) = H5_CMPLXF((float)real_val, (float)imag_val);
#endif

            /* Check for overflow and underflow */
            if (fabs(real_val) > (double)FLT_MAX)
                real_over = true;
            else if (fabs(real_val) < (double)FLT_MIN)
                real_under = true;
            if (fabs(imag_val) > (double)FLT_MAX)
                imag_over = true;
            else if (fabs(imag_val) < (double)FLT_MIN)
                imag_under = true;

            if (real_over) {
                if (imag_over)
                    ret = CONV_OVERFLOW_BOTH;
                else if (imag_under)
                    ret = CONV_OVERUNDER;
                else
                    ret = CONV_OVERFLOW_REAL;
            }
            else if (real_under) {
                if (imag_over)
                    ret = CONV_UNDEROVER;
                else if (imag_under)
                    ret = CONV_UNDERFLOW_BOTH;
                else
                    ret = CONV_UNDERFLOW_REAL;
            }
            else if (imag_over)
                ret = CONV_OVERFLOW_IMAG;
            else if (imag_under)
                ret = CONV_UNDERFLOW_IMAG;

            break;
        }
        case DBL_COMPLEX:
            *((H5_double_complex *)hw_dst) = aligned;
            break;
        case LDBL_COMPLEX:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_ldouble_complex *)hw_dst) = (H5_ldouble_complex)aligned;
#else
            *((H5_ldouble_complex *)hw_dst) =
                H5_CMPLXL((long double)creal(aligned), (long double)cimag(aligned));
#endif
            break;
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1_hw_conv_from_lcomplex
 *
 * Purpose:     Helper function for test_conv_flt_1 to perform conversion
 *              from long double _Complex / _Lcomplex to another type by
 *              casting. Also checks for overflow and underflow when the
 *              destination type is a type with a smaller width than
 *              long double.
 *
 * Return:      enum conv_func_ret_t value
 *
 *-------------------------------------------------------------------------
 */
static conv_func_ret_t
test_conv_flt_1_hw_conv_from_lcomplex(void *hw_dst, unsigned char *src_buf, size_t idx, dtype_t dst_type)
{
    H5_ldouble_complex aligned;
    long double        real_val, imag_val;
    conv_func_ret_t    ret = CONV_SUCCESS;

    memcpy(&aligned, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));

    switch (dst_type) {
        case FLT_FLOAT:
            real_val = creall(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((float *)hw_dst) = (float)aligned;
#else
            *((float *)hw_dst) = (float)real_val;
#endif

            /* Check for overflow and underflow */
            if (fabsl(real_val) > (long double)FLT_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(real_val) < (long double)FLT_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_DOUBLE:
            real_val = creall(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((double *)hw_dst) = (double)aligned;
#else
            *((double *)hw_dst) = (double)real_val;
#endif

            /* Check for overflow and underflow */
            if (fabsl(real_val) > (long double)DBL_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(real_val) < (long double)DBL_MIN)
                ret = CONV_UNDERFLOW;

            break;
        case FLT_LDOUBLE:
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((long double *)hw_dst) = (long double)aligned;
#else
            *((long double *)hw_dst) = (long double)creall(aligned);
#endif
            break;
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5__Float16 *)hw_dst) = (H5__Float16)aligned;
#else
            *((H5__Float16 *)hw_dst) = (H5__Float16)creall(aligned);
#endif

            /* Check for overflow and underflow */
            real_val = creall(aligned);
            if (fabsl(real_val) > (long double)FLT16_MAX)
                ret = CONV_OVERFLOW;
            else if (fabsl(real_val) < (long double)FLT16_MIN)
                ret = CONV_UNDERFLOW;

            H5_WARN_NONSTD_SUFFIX_ON
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
#endif
        case FLT_COMPLEX: {
            bool real_over = false, real_under = false;
            bool imag_over = false, imag_under = false;

            real_val = creall(aligned);
            imag_val = cimagl(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_float_complex *)hw_dst) = (H5_float_complex)aligned;
#else
            *((H5_float_complex *)hw_dst) = H5_CMPLXF((float)real_val, (float)imag_val);
#endif

            /* Check for overflow and underflow */
            if (fabsl(real_val) > (long double)FLT_MAX)
                real_over = true;
            else if (fabsl(real_val) < (long double)FLT_MIN)
                real_under = true;
            if (fabsl(imag_val) > (long double)FLT_MAX)
                imag_over = true;
            else if (fabsl(imag_val) < (long double)FLT_MIN)
                imag_under = true;

            if (real_over) {
                if (imag_over)
                    ret = CONV_OVERFLOW_BOTH;
                else if (imag_under)
                    ret = CONV_OVERUNDER;
                else
                    ret = CONV_OVERFLOW_REAL;
            }
            else if (real_under) {
                if (imag_over)
                    ret = CONV_UNDEROVER;
                else if (imag_under)
                    ret = CONV_UNDERFLOW_BOTH;
                else
                    ret = CONV_UNDERFLOW_REAL;
            }
            else if (imag_over)
                ret = CONV_OVERFLOW_IMAG;
            else if (imag_under)
                ret = CONV_UNDERFLOW_IMAG;

            break;
        }
        case DBL_COMPLEX: {
            bool real_over = false, real_under = false;
            bool imag_over = false, imag_under = false;

            real_val = creall(aligned);
            imag_val = cimagl(aligned);

#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            *((H5_double_complex *)hw_dst) = (H5_double_complex)aligned;
#else
            *((H5_double_complex *)hw_dst) = H5_CMPLX((double)real_val, (double)imag_val);
#endif

            /* Check for overflow and underflow */
            if (fabsl(real_val) > (long double)DBL_MAX)
                real_over = true;
            else if (fabsl(real_val) < (long double)DBL_MIN)
                real_under = true;
            if (fabsl(imag_val) > (long double)DBL_MAX)
                imag_over = true;
            else if (fabsl(imag_val) < (long double)DBL_MIN)
                imag_under = true;

            if (real_over) {
                if (imag_over)
                    ret = CONV_OVERFLOW_BOTH;
                else if (imag_under)
                    ret = CONV_OVERUNDER;
                else
                    ret = CONV_OVERFLOW_REAL;
            }
            else if (real_under) {
                if (imag_over)
                    ret = CONV_UNDEROVER;
                else if (imag_under)
                    ret = CONV_UNDERFLOW_BOTH;
                else
                    ret = CONV_UNDERFLOW_REAL;
            }
            else if (imag_over)
                ret = CONV_OVERFLOW_IMAG;
            else if (imag_under)
                ret = CONV_UNDERFLOW_IMAG;

            break;
        }
        case LDBL_COMPLEX:
            *((H5_ldouble_complex *)hw_dst) = aligned;
            break;
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = CONV_ERROR;
            goto done;
    }

done:
    return ret;
}
#endif /* H5_HAVE_COMPLEX_NUMBERS */

/*-------------------------------------------------------------------------
 * Function:    test_conv_flt_1
 *
 * Purpose:    Test conversion of floating point values from SRC to
 *             DST. These types should be one of the following:
 *
 *             H5T_NATIVE_FLOAT16 (if available)
 *             H5T_NATIVE_FLOAT
 *             H5T_NATIVE_DOUBLE
 *             H5T_NATIVE_LDOUBLE
 *             H5T_NATIVE_FLOAT_COMPLEX (if available)
 *             H5T_NATIVE_DOUBLE_COMPLEX (if available)
 *             H5T_NATIVE_LDOUBLE_COMPLEX (if available)
 *
 * Return:    Success:    0
 *            Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_flt_1(const char *name, int run_test, hid_t src, hid_t dst)
{
    dtype_t        src_type, dst_type;   /*data types        */
    size_t         nelmts          = 0;  /*num values per test    */
    const size_t   max_fails       = 8;  /*max number of failures*/
    size_t         fails_all_tests = 0;  /*number of failures    */
    size_t         fails_this_test;      /*fails for this test    */
    const char    *src_type_name = NULL; /*source type name    */
    const char    *dst_type_name = NULL; /*destination type name    */
    size_t         src_size, dst_size;   /*type sizes        */
    unsigned char *buf   = NULL;         /*buffer for conversion    */
    unsigned char *saved = NULL;         /*original values    */
    char           str[256];             /*hello string        */
    void          *hw_p = NULL;
    float          hw_f; /*hardware-converted     */
    double         hw_d; /*hardware-converted    */
#ifdef H5_HAVE__FLOAT16
    H5__Float16 hw_half;
#endif
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    long double hw_ld; /*hardware-converted    */
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    H5_ldouble_complex hw_ldouble_complex;
    H5_double_complex  hw_double_complex;
    H5_float_complex   hw_float_complex;
#endif
    unsigned char *hw = NULL;    /*ptr to hardware-conv'd*/
    int            underflow;    /*underflow occurred    */
    int            overflow = 0; /*overflow occurred    */
    size_t         j, k;         /*counters        */
    int            sendian;      /* source type endianness */
    int            dendian;      /* Destination type endianness */
    size_t         dst_ebias;    /* Destination type's exponent bias */
    size_t         src_epos;     /* Source type's exponent position */
    size_t         src_esize;    /* Source type's exponent size */
    size_t         dst_epos;     /* Destination type's exponent position */
    size_t         dst_esize;    /* Destination type's exponent size */
    size_t         dst_mpos;     /* Destination type's mantissa position */
    size_t         dst_msize;    /* Destination type's mantissa size */
    size_t         src_nbits;    /* source length in bits */
    size_t         dst_nbits;    /* dst length in bits */

#ifdef HANDLE_SIGFPE
    pid_t child_pid; /*process ID of child    */
    int   status;    /*child exit status    */

    /*
     * Some systems generate SIGFPE during floating point overflow and we
     * cannot assume that we can continue from such a signal.  Therefore, we
     * fork here and let the child run the test and return the number of
     * failures with the exit status.
     */
    fflush(stdout);
    fflush(stderr);
    if ((child_pid = fork()) < 0) {
        perror("fork");
        return 1;
    }
    else if (child_pid > 0) {
        while (child_pid != waitpid(child_pid, &status, 0)) /*void*/
            ;
        if (WIFEXITED(status) && 255 == WEXITSTATUS(status)) {
            return 0; /*child exit after catching SIGFPE*/
        }
        else if (WIFEXITED(status)) {
            return WEXITSTATUS(status);
        }
        else if (WIFSIGNALED(status)) {
            snprintf(str, sizeof(str), "   Child caught signal %d.", WTERMSIG(status));
            puts(str);
            return 1; /*child exit after catching non-SIGFPE signal */
        }
        else {
            puts("   Child didn't exit normally.");
            return 1;
        }
    }
#endif

    /*
     * The remainder of this function is executed only by the child if
     * HANDLE_SIGFPE is defined.
     */
    signal(SIGFPE, fpe_handler);

    /* What are the names of the source and destination types */
    if (H5Tequal(src, H5T_NATIVE_FLOAT)) {
        src_type_name = "float";
        src_type      = FLT_FLOAT;
    }
    else if (H5Tequal(src, H5T_NATIVE_DOUBLE)) {
        src_type_name = "double";
        src_type      = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (H5Tequal(src, H5T_NATIVE_LDOUBLE)) {
        src_type_name = "long double";
        src_type      = FLT_LDOUBLE;
#endif
    }
#ifdef H5_HAVE__FLOAT16
    else if (H5Tequal(src, H5T_NATIVE_FLOAT16)) {
        src_type_name = "_Float16";
        src_type      = FLT_FLOAT16;
    }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (H5Tequal(src, H5T_NATIVE_FLOAT_COMPLEX)) {
        src_type_name = "float _Complex";
        src_type      = FLT_COMPLEX;
    }
    else if (H5Tequal(src, H5T_NATIVE_DOUBLE_COMPLEX)) {
        src_type_name = "double _Complex";
        src_type      = DBL_COMPLEX;
    }
    else if (H5Tequal(src, H5T_NATIVE_LDOUBLE_COMPLEX)) {
        src_type_name = "long double _Complex";
        src_type      = LDBL_COMPLEX;
    }
#endif
    else {
        src_type_name = "UNKNOWN";
        src_type      = OTHER;
    }

    if (H5Tequal(dst, H5T_NATIVE_FLOAT)) {
        dst_type_name = "float";
        dst_type      = FLT_FLOAT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_DOUBLE)) {
        dst_type_name = "double";
        dst_type      = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE)) {
        dst_type_name = "long double";
        dst_type      = FLT_LDOUBLE;
#endif
    }
#ifdef H5_HAVE__FLOAT16
    else if (H5Tequal(dst, H5T_NATIVE_FLOAT16)) {
        dst_type_name = "_Float16";
        dst_type      = FLT_FLOAT16;
    }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (H5Tequal(dst, H5T_NATIVE_FLOAT_COMPLEX)) {
        dst_type_name = "float _Complex";
        dst_type      = FLT_COMPLEX;
    }
    else if (H5Tequal(dst, H5T_NATIVE_DOUBLE_COMPLEX)) {
        dst_type_name = "double _Complex";
        dst_type      = DBL_COMPLEX;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE_COMPLEX)) {
        dst_type_name = "long double _Complex";
        dst_type      = LDBL_COMPLEX;
    }
#endif
    else {
        dst_type_name = "UNKNOWN";
        dst_type      = OTHER;
    }

    /* Sanity checks */
    if (sizeof(float) == sizeof(double))
        puts("Sizeof(float)==sizeof(double) - some tests may not be sensible.");
    if (OTHER == src_type || OTHER == dst_type) {
        if (!strcmp(name, "noop"))
            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        else if (run_test == TEST_SPECIAL)
            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else if (run_test == TEST_NORMAL)
            snprintf(str, sizeof(str), "Testing %s normalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else if (run_test == TEST_DENORM)
            snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);

        printf("%-70s", str);
        H5_FAILED();
        puts("    Unknown data type.");
        goto error;
    }
    else {
        if (!strcmp(name, "noop"))
            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        else if (run_test == TEST_SPECIAL)
            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else if (run_test == TEST_NORMAL)
            snprintf(str, sizeof(str), "Testing %s normalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else if (run_test == TEST_DENORM)
            snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);

        printf("%-70s", str);
        fflush(stdout);
        fails_this_test = 0;
    }

    /* Get "interesting" values */
    src_size  = H5Tget_size(src);
    dst_size  = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */
    dst_ebias = H5Tget_ebias(dst);
    H5Tget_fields(src, NULL, &src_epos, &src_esize, NULL, NULL);
    H5Tget_fields(dst, NULL, &dst_epos, &dst_esize, &dst_mpos, &dst_msize);
    sendian = H5Tget_order(src);
    dendian = H5Tget_order(dst);

    /* Allocate and initialize the source buffer through macro INIT_FP_NORM or INIT_FP_SPECIAL.
     * The BUF will be used for the conversion while the SAVED buffer will be used for
     * the comparison later.  INIT_FP_NORM will fill in the buffer with regular values like
     * normalized and denormalized values; INIT_FP_SPECIAL will fill with special values
     * like infinity, NaN.
     */
    switch (run_test) {
        case TEST_NOOP:
        case TEST_NORMAL:
            if (src_type == FLT_FLOAT) {
                INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP, src_size, dst_size, buf,
                             saved, nelmts);
            }
            else if (src_type == FLT_DOUBLE) {
                INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP, src_size, dst_size,
                             buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
            }
            else if (src_type == FLT_LDOUBLE) {
                INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP, src_size,
                             dst_size, buf, saved, nelmts);
#endif
            }
            else if (src_type == FLT_FLOAT16) {
#ifdef H5_HAVE__FLOAT16
                /* Suppress warning about non-standard floating-point literal suffix */
                H5_WARN_NONSTD_SUFFIX_OFF
                /* Suppress warning about float conversion in macro code path
                 * that sets H5__Float16 multiply = 100000000;, which shouldn't
                 * happen due to the small value of FLT16_MAX_10_EXP.
                 */
                H5_WARN_FLOAT_CONVERSION_OFF
                INIT_FP_NORM(H5__Float16, FLT16_MAX, FLT16_MIN, FLT16_MAX_10_EXP, FLT16_MIN_10_EXP, src_size,
                             dst_size, buf, saved, nelmts);
                H5_WARN_FLOAT_CONVERSION_ON
                H5_WARN_NONSTD_SUFFIX_ON
#else
                assert(0 && "Should not reach this point!");
#endif
            }
#ifdef H5_HAVE_COMPLEX_NUMBERS
            else if (src_type == FLT_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP, part_size, dst_size,
                             buf, saved, nelmts);
                /* Treat float buffer as float _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == DBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP, part_size, dst_size,
                             buf, saved, nelmts);
                /* Treat double buffer as double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == LDBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP, part_size,
                             dst_size, buf, saved, nelmts);
                /* Treat long double buffer as long double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
#endif
            else
                goto error;

            break;
        case TEST_DENORM:
            if (src_type == FLT_FLOAT) {
                INIT_FP_DENORM(float, FLT_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved,
                               nelmts);
            }
            else if (src_type == FLT_DOUBLE) {
                INIT_FP_DENORM(double, DBL_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved,
                               nelmts);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
            }
            else if (src_type == FLT_LDOUBLE) {
                INIT_FP_DENORM(long double, LDBL_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved,
                               nelmts);
#endif
            }
            else if (src_type == FLT_FLOAT16) {
#ifdef H5_HAVE__FLOAT16
                INIT_FP_DENORM(H5__Float16, FLT16_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf,
                               saved, nelmts);
#else
                assert(0 && "Should not reach this point!");
#endif
            }
#ifdef H5_HAVE_COMPLEX_NUMBERS
            else if (src_type == FLT_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_DENORM(float, FLT_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf, saved,
                               nelmts);
                /* Treat float buffer as float _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == DBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_DENORM(double, DBL_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf, saved,
                               nelmts);
                /* Treat double buffer as double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == LDBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_DENORM(long double, LDBL_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf,
                               saved, nelmts);
                /* Treat long double buffer as long double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
#endif
            else
                goto error;

            break;

        case TEST_SPECIAL:
            if (src_type == FLT_FLOAT) {
                INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT_MANT_DIG, dst_size, buf, saved, nelmts);
            }
            else if (src_type == FLT_DOUBLE) {
                INIT_FP_SPECIAL(src_size, src_nbits, sendian, DBL_MANT_DIG, dst_size, buf, saved, nelmts);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
            }
            else if (src_type == FLT_LDOUBLE) {
                INIT_FP_SPECIAL(src_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size, buf, saved, nelmts);
#endif
            }
            else if (src_type == FLT_FLOAT16) {
#ifdef H5_HAVE__FLOAT16
                INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT16_MANT_DIG, dst_size, buf, saved, nelmts);
#else
                assert(0 && "Should not reach this point!");
#endif
            }
#ifdef H5_HAVE_COMPLEX_NUMBERS
            else if (src_type == FLT_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_SPECIAL(part_size, src_nbits, sendian, FLT_MANT_DIG, dst_size, buf, saved, nelmts);
                /* Treat float buffer as float _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == DBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_SPECIAL(part_size, src_nbits, sendian, DBL_MANT_DIG, dst_size, buf, saved, nelmts);
                /* Treat double buffer as double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
            else if (src_type == LDBL_COMPLEX) {
                size_t part_size = src_size / 2;
                INIT_FP_SPECIAL(part_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size, buf, saved, nelmts);
                /* Treat long double buffer as long double _Complex buffer of nelmts / 2 elements */
                assert(nelmts % 2 == 0);
                nelmts /= 2;
            }
#endif
            else
                goto error;

            break;
        default:
            goto error;
    }

    /* Perform the conversion in software */
    if (H5Tconvert(src, dst, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;

    /* Set pointer to matching type for hardware conversion */
    if (FLT_FLOAT == dst_type)
        hw_p = &hw_f;
    else if (FLT_DOUBLE == dst_type)
        hw_p = &hw_d;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    else if (FLT_LDOUBLE == dst_type)
        hw_p = &hw_ld;
#endif
#ifdef H5_HAVE__FLOAT16
    else if (FLT_FLOAT16 == dst_type)
        hw_p = &hw_half;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (FLT_COMPLEX == dst_type)
        hw_p = &hw_float_complex;
    else if (DBL_COMPLEX == dst_type)
        hw_p = &hw_double_complex;
    else if (LDBL_COMPLEX == dst_type)
        hw_p = &hw_ldouble_complex;
#endif
    else
        goto error;

    /* Set convenience pointer for indexing into bytes of matching type */
    hw = (unsigned char *)hw_p;

    /* Check the software results against the hardware */
    for (j = 0; j < nelmts; j++) {
        conv_func_ret_t conv_ret = CONV_ERROR;

        hw_f = 911.0F;
        hw_d = 911.0;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        hw_ld = 911.0L;
#endif
#ifdef H5_HAVE__FLOAT16
        hw_half = 911.0;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        hw_float_complex   = H5_CMPLXF(911.0F, 911.0F);
        hw_double_complex  = H5_CMPLX(911.0, 911.0);
        hw_ldouble_complex = H5_CMPLXL(911.0L, 911.0L);
#endif

        /* The hardware conversion */
#ifdef H5_HAVE__FLOAT16
        if (FLT_FLOAT16 == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_flt16(hw_p, saved, j, dst_type);
        }
        else
#endif
            if (FLT_FLOAT == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_flt(hw_p, saved, j, dst_type);
        }
        else if (FLT_DOUBLE == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_double(hw_p, saved, j, dst_type);
        }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        else if (FLT_LDOUBLE == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_ldouble(hw_p, saved, j, dst_type);
        }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        else if (FLT_COMPLEX == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_fcomplex(hw_p, saved, j, dst_type);
        }
        else if (DBL_COMPLEX == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_dcomplex(hw_p, saved, j, dst_type);
        }
        else if (LDBL_COMPLEX == src_type) {
            conv_ret = test_conv_flt_1_hw_conv_from_lcomplex(hw_p, saved, j, dst_type);
        }
#endif

        if (conv_ret == CONV_ERROR)
            goto error;

        overflow =
            (conv_ret == CONV_OVERFLOW || conv_ret == CONV_OVERFLOW_REAL || conv_ret == CONV_OVERFLOW_IMAG ||
             conv_ret == CONV_OVERFLOW_BOTH || conv_ret == CONV_OVERUNDER || conv_ret == CONV_UNDEROVER);
        underflow = (conv_ret == CONV_UNDERFLOW || conv_ret == CONV_UNDERFLOW_REAL ||
                     conv_ret == CONV_UNDERFLOW_IMAG || conv_ret == CONV_UNDERFLOW_BOTH ||
                     conv_ret == CONV_OVERUNDER || conv_ret == CONV_UNDEROVER);

        /* For Intel machines, the size of "long double" is 12 bytes, precision
         * is 80 bits; for Intel IA64 and AMD processors, the size of "long double"
         * is 16 bytes, precision is 80 bits.  During hardware conversion, the
         * last few unused bytes may have garbage in them.  Clean them out with
         * 0s before compare the values.
         */
        if (sendian == H5T_ORDER_LE) {
            if (dst_type == FLT_LDOUBLE) {
                for (size_t q = dst_nbits / 8; q < dst_size; q++) {
                    buf[j * dst_size + q] = 0x00;
                    hw[q]                 = 0x00;
                }
            }
            else if (dst_type == LDBL_COMPLEX) {
                uint8_t *cur_buf   = buf + j * dst_size;
                uint8_t *cur_hw    = hw;
                size_t   part_size = dst_size / 2;
                for (size_t q = dst_nbits / 8; q < part_size; q++) {
                    cur_buf[q] = 0x00;
                    cur_hw[q]  = 0x00;
                }
                cur_buf += part_size;
                cur_hw += part_size;
                for (size_t q = dst_nbits / 8; q < part_size; q++) {
                    cur_buf[q] = 0x00;
                    cur_hw[q]  = 0x00;
                }
            }
        }

        /* Are the two results the same? */
        if (dst_type != FLT_COMPLEX && dst_type != DBL_COMPLEX && dst_type != LDBL_COMPLEX) {
            for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
                if (buf[j * dst_size + k] != hw[k])
                    break;
        }
        else {
            size_t part_size = dst_size / 2;

            /* Compare real part */
            for (k = (part_size - (dst_nbits / 8)); k < part_size; k++)
                if (buf[j * dst_size + k] != hw[k])
                    break;
            if (k == part_size) {
                /* Compare imaginary part */
                if (src_type == FLT_COMPLEX || src_type == DBL_COMPLEX || src_type == LDBL_COMPLEX) {
                    for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
                        if (buf[j * dst_size + k] != hw[k])
                            break;
                }
                else {
                    /* Imaginary part should have been zeroed out. Hardware value
                     * could be a positive or negative zero, so we'll just check
                     * the buffer value for simplicity.
                     */
                    for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
                        if (buf[j * dst_size + k] != 0x00)
                            break;
                }
            }
        }

        if (k == dst_size)
            continue; /*no error*/

        /*
         * Assume same if both results are NaN.  There are many NaN bit
         * patterns and the software doesn't attempt to emulate the
         * hardware in this regard.  Instead, software uses a single bit
         * pattern for NaN by setting the significand to all ones.
         */
        if (FLT_FLOAT == dst_type && my_isnan(dst_type, buf + j * sizeof(float)) && my_isnan(dst_type, hw)) {
            continue;
        }
        else if (FLT_DOUBLE == dst_type && my_isnan(dst_type, buf + j * sizeof(double)) &&
                 my_isnan(dst_type, hw)) {
            continue;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        }
        else if (FLT_LDOUBLE == dst_type && my_isnan(dst_type, buf + j * sizeof(long double)) &&
                 my_isnan(dst_type, hw)) {
            continue;
#endif
        }
#ifdef H5_HAVE__FLOAT16
        else if (FLT_FLOAT16 == dst_type && my_isnan(dst_type, buf + j * sizeof(H5__Float16)) &&
                 my_isnan(dst_type, hw)) {
            continue;
        }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        else if (FLT_COMPLEX == dst_type && my_isnan(dst_type, buf + j * sizeof(H5_float_complex)) &&
                 my_isnan(dst_type, hw)) {
            continue;
        }
        else if (DBL_COMPLEX == dst_type && my_isnan(dst_type, buf + j * sizeof(H5_double_complex)) &&
                 my_isnan(dst_type, hw)) {
            continue;
        }
        else if (LDBL_COMPLEX == dst_type && my_isnan(dst_type, buf + j * sizeof(H5_ldouble_complex)) &&
                 my_isnan(dst_type, hw)) {
            continue;
        }
#endif

        /*
         * Assume same if hardware result is NaN.  This is because the
         * hardware conversions on some machines return NaN instead of
         * overflowing to +Inf or -Inf or underflowing to +0 or -0.
         */
        if (my_isnan(dst_type, hw))
            continue;

        /*
         * Instead of matching down to the bit, just make sure the
         * exponents are the same and the mantissa is the same to a
         * certain precision.  This is needed on machines that don't
         * round as expected.
         * If the src number is smaller than the dst MIN float number,
         * consider it okay if the converted sw and hw dst are both
         * less than or equal to the dst MIN float number.
         * If overflow happens when the src value is greater than
         * the maximum dst value, the library assign INFINITY to dst.
         * This might be different from what the compiler does, i.e.
         * the SGI compiler assigns the dst's maximal value.
         */
        {
            double check_mant[4] = {0.0, 0.0, 0.0, 0.0};
            int    check_expo[4] = {0, 0, 0, 0};

            if (FLT_FLOAT == dst_type) {
                float x = 0.0F;
                memcpy(&x, &buf[j * dst_size], sizeof(float));
                if (underflow && fabsf(x) <= FLT_MIN && fabsf(hw_f) <= FLT_MIN)
                    continue; /* all underflowed, no error */
                if (overflow && my_isinf(dendian, buf + j * sizeof(float), dst_size, dst_mpos, dst_msize,
                                         dst_epos, dst_esize))
                    continue; /* all overflowed, no error */
                check_mant[0] = (double)frexpf(x, check_expo + 0);
                check_mant[1] = (double)frexpf(hw_f, check_expo + 1);
            }
            else if (FLT_DOUBLE == dst_type) {
                double x = 0.0;
                memcpy(&x, &buf[j * dst_size], sizeof(double));
                if (underflow && fabs(x) <= DBL_MIN && fabs(hw_d) <= DBL_MIN)
                    continue; /* all underflowed, no error */
                if (overflow && my_isinf(dendian, buf + j * sizeof(double), dst_size, dst_mpos, dst_msize,
                                         dst_epos, dst_esize))
                    continue; /* all overflowed, no error */
                check_mant[0] = frexp(x, check_expo + 0);
                check_mant[1] = frexp(hw_d, check_expo + 1);
#if (H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE)
            }
            else if (FLT_LDOUBLE == dst_type) {
                long double x = 0.0L;
                memcpy(&x, &buf[j * dst_size], sizeof(long double));
                /* dst is largest float, no need to check underflow. */
                check_mant[0] = (double)frexpl(x, check_expo + 0);
                check_mant[1] = (double)frexpl(hw_ld, check_expo + 1);
#endif
            }
            else if (FLT_FLOAT16 == dst_type) {
#ifdef H5_HAVE__FLOAT16
                H5__Float16 x;
                memcpy(&x, &buf[j * dst_size], sizeof(H5__Float16));

                /* Suppress warning about non-standard floating-point literal suffix */
                H5_WARN_NONSTD_SUFFIX_OFF
#ifdef H5_HAVE_FABSF16
                if (underflow && fabsf16(x) <= FLT16_MIN && fabsf16(hw_half) <= FLT16_MIN)
                    continue; /* all underflowed, no error */
#else
                if (underflow && fabsf((float)x) <= (float)FLT16_MIN &&
                    fabsf((float)hw_half) <= (float)FLT16_MIN)
                    continue; /* all underflowed, no error */
#endif
                H5_WARN_NONSTD_SUFFIX_ON

                if (overflow && my_isinf(dendian, buf + j * sizeof(H5__Float16), dst_size, dst_mpos,
                                         dst_msize, dst_epos, dst_esize))
                    continue; /* all overflowed, no error */
                check_mant[0] = (double)frexpf((float)x, check_expo + 0);
                check_mant[1] = (double)frexpf((float)hw_half, check_expo + 1);
#else
                assert(0 && "Should not reach this point!");
#endif
            }
#ifdef H5_HAVE_COMPLEX_NUMBERS
            else if (FLT_COMPLEX == dst_type) {
                H5_float_complex fc;
                float            real_val, hw_real_val;
                float            imag_val, hw_imag_val;
                bool             val_passes = true;

                memcpy(&fc, &buf[j * dst_size], sizeof(H5_float_complex));
                real_val    = crealf(fc);
                imag_val    = cimagf(fc);
                hw_real_val = crealf(hw_float_complex);
                hw_imag_val = cimagf(hw_float_complex);

                if (underflow) {
                    /* Check real part against hardware */
                    if (conv_ret == CONV_UNDERFLOW_REAL || conv_ret == CONV_UNDERFLOW_BOTH ||
                        conv_ret == CONV_UNDEROVER)
                        if (fabsf(real_val) > FLT_MIN || fabsf(hw_real_val) > FLT_MIN)
                            val_passes = false;

                    /* Check imaginary part against hardware */
                    if (conv_ret == CONV_UNDERFLOW_IMAG || conv_ret == CONV_UNDERFLOW_BOTH ||
                        conv_ret == CONV_OVERUNDER)
                        if (fabsf(imag_val) > FLT_MIN || fabsf(hw_imag_val) > FLT_MIN)
                            val_passes = false;
                }

                if (overflow) {
                    /* Check real part against hardware */
                    if (conv_ret == CONV_OVERFLOW_REAL || conv_ret == CONV_OVERFLOW_BOTH ||
                        conv_ret == CONV_OVERUNDER)
                        if (!my_isinf(dendian, buf + j * sizeof(H5_float_complex), dst_size / 2, dst_mpos,
                                      dst_msize, dst_epos, dst_esize))
                            val_passes = false;

                    /* Check imaginary part against hardware */
                    if (conv_ret == CONV_OVERFLOW_IMAG || conv_ret == CONV_OVERFLOW_BOTH ||
                        conv_ret == CONV_UNDEROVER)
                        if (!my_isinf(dendian, buf + (j * sizeof(H5_float_complex)) + (dst_size / 2),
                                      dst_size / 2, dst_mpos, dst_msize, dst_epos, dst_esize))
                            val_passes = false;
                }

                if (val_passes)
                    continue; /* matching pairs all underflowed or overflowed, no error */

                check_mant[0] = (double)frexpf(real_val, check_expo + 0);
                check_mant[1] = (double)frexpf(hw_real_val, check_expo + 1);
                check_mant[2] = (double)frexpf(imag_val, check_expo + 2);
                check_mant[3] = (double)frexpf(hw_imag_val, check_expo + 3);
            }
            else if (DBL_COMPLEX == dst_type) {
                H5_double_complex dc;
                double            real_val, hw_real_val;
                double            imag_val, hw_imag_val;
                bool              val_passes = true;

                memcpy(&dc, &buf[j * dst_size], sizeof(H5_double_complex));
                real_val    = creal(dc);
                imag_val    = cimag(dc);
                hw_real_val = creal(hw_double_complex);
                hw_imag_val = cimag(hw_double_complex);

                if (underflow) {
                    /* Check real part against hardware */
                    if (conv_ret == CONV_UNDERFLOW_REAL || conv_ret == CONV_UNDERFLOW_BOTH ||
                        conv_ret == CONV_UNDEROVER)
                        if (fabs(real_val) > DBL_MIN || fabs(hw_real_val) > DBL_MIN)
                            val_passes = false;

                    /* Check imaginary part against hardware */
                    if (conv_ret == CONV_UNDERFLOW_IMAG || conv_ret == CONV_UNDERFLOW_BOTH ||
                        conv_ret == CONV_OVERUNDER)
                        if (fabs(imag_val) > DBL_MIN || fabs(hw_imag_val) > DBL_MIN)
                            val_passes = false;
                }

                if (overflow) {
                    /* Check real part against hardware */
                    if (conv_ret == CONV_OVERFLOW_REAL || conv_ret == CONV_OVERFLOW_BOTH ||
                        conv_ret == CONV_OVERUNDER)
                        if (!my_isinf(dendian, buf + j * sizeof(H5_double_complex), dst_size / 2, dst_mpos,
                                      dst_msize, dst_epos, dst_esize))
                            val_passes = false;

                    /* Check imaginary part against hardware */
                    if (conv_ret == CONV_OVERFLOW_IMAG || conv_ret == CONV_OVERFLOW_BOTH ||
                        conv_ret == CONV_UNDEROVER)
                        if (!my_isinf(dendian, buf + (j * sizeof(H5_double_complex)) + (dst_size / 2),
                                      dst_size / 2, dst_mpos, dst_msize, dst_epos, dst_esize))
                            val_passes = false;
                }

                if (val_passes)
                    continue; /* matching pairs all underflowed or overflowed, no error */

                check_mant[0] = frexp(real_val, check_expo + 0);
                check_mant[1] = frexp(hw_real_val, check_expo + 1);
                check_mant[2] = frexp(imag_val, check_expo + 2);
                check_mant[3] = frexp(hw_imag_val, check_expo + 3);
            }
            else if (LDBL_COMPLEX == dst_type) {
                H5_ldouble_complex ldc;
                long double        real_val, hw_real_val;
                long double        imag_val, hw_imag_val;

                memcpy(&ldc, &buf[j * dst_size], sizeof(H5_ldouble_complex));
                real_val    = creall(ldc);
                imag_val    = cimagl(ldc);
                hw_real_val = creall(hw_ldouble_complex);
                hw_imag_val = cimagl(hw_ldouble_complex);

                /* dst is largest float, no need to check underflow. */
                check_mant[0] = (double)frexpl(real_val, check_expo + 0);
                check_mant[1] = (double)frexpl(hw_real_val, check_expo + 1);
                check_mant[2] = (double)frexpl(imag_val, check_expo + 2);
                check_mant[3] = (double)frexpl(hw_imag_val, check_expo + 3);
            }
#endif
            else
                goto error;

            /* Special check for denormalized values */
            if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
                if (check_expo[0] < (-(int)dst_ebias) || check_expo[1] < (-(int)dst_ebias) ||
                    check_expo[2] < (-(int)dst_ebias) || check_expo[3] < (-(int)dst_ebias)) {
                    double epsilon_real   = 1.0;
                    double epsilon_imag   = 1.0;
                    int    expo_diff_real = check_expo[0] - check_expo[1];
                    int    expo_diff_imag = check_expo[2] - check_expo[3];
                    int    valid_bits_real =
                        (int)((dst_ebias + dst_msize) + (size_t)MIN(check_expo[0], check_expo[1])) - 1;
                    int valid_bits_imag =
                        (int)((dst_ebias + dst_msize) + (size_t)MIN(check_expo[2], check_expo[3])) - 1;

                    /* Re-scale the mantissas based on any exponent difference */
                    if (expo_diff_real != 0)
                        check_mant[0] = ldexp(check_mant[0], expo_diff_real);
                    if (expo_diff_imag != 0)
                        check_mant[2] = ldexp(check_mant[2], expo_diff_imag);

                    /* Compute the proper epsilon */
                    epsilon_real = ldexp(epsilon_real, -valid_bits_real);
                    epsilon_imag = ldexp(epsilon_imag, -valid_bits_imag);

                    /* Check for "close enough" fit with scaled epsilon value */
                    if (fabs(check_mant[0] - check_mant[1]) <= epsilon_real &&
                        fabs(check_mant[2] - check_mant[3]) <= epsilon_imag)
                        continue;
                }
                else {
                    if ((check_expo[0] == check_expo[1] &&
                         fabs(check_mant[0] - check_mant[1]) < (double)FP_EPSILON) &&
                        (check_expo[2] == check_expo[3] &&
                         fabs(check_mant[2] - check_mant[3]) < (double)FP_EPSILON))
                        continue;
                }
            }
            else if (check_expo[0] < (-(int)dst_ebias) || check_expo[1] < (-(int)dst_ebias)) {
                int expo_diff = check_expo[0] - check_expo[1];
                int valid_bits =
                    (int)((dst_ebias + dst_msize) + (size_t)MIN(check_expo[0], check_expo[1])) - 1;
                double epsilon = 1.0;

                /* Re-scale the mantissas based on any exponent difference */
                if (expo_diff != 0)
                    check_mant[0] = ldexp(check_mant[0], expo_diff);

                /* Compute the proper epsilon */
                epsilon = ldexp(epsilon, -valid_bits);

                /* Check for "close enough" fit with scaled epsilon value */
                if (fabs(check_mant[0] - check_mant[1]) <= epsilon)
                    continue;
            } /* end if */
            else {
                if (check_expo[0] == check_expo[1] &&
                    fabs(check_mant[0] - check_mant[1]) < (double)FP_EPSILON)
                    continue;
            } /* end else */
        }

        if (0 == fails_this_test++) {
            if (run_test == TEST_NOOP || run_test == TEST_NORMAL) {
                H5_FAILED();
            }
            else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL) {
                H5_WARNING();
            }
        }
        printf("    elmt %u\n", (unsigned)j);

        printf("        src =");
        if (FLT_COMPLEX == src_type || DBL_COMPLEX == src_type || LDBL_COMPLEX == src_type) {
            unsigned char *saved_ptr = saved + (j * src_size);
            size_t         part_size = src_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", saved_ptr[ENDIAN(part_size, k, sendian)]);
            saved_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", saved_ptr[ENDIAN(part_size, k, sendian)]);
        }
        else {
            for (k = 0; k < src_size; k++)
                printf(" %02x", saved[j * src_size + ENDIAN(src_size, k, sendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)dst_size - (ssize_t)src_size)), "");
        if (FLT_FLOAT == src_type) {
            float x = 0.0F;
            memcpy(&x, &saved[j * src_size], sizeof(float));
            printf(" %29.20e\n", (double)x);
        }
        else if (FLT_DOUBLE == src_type) {
            double x = 0.0;
            memcpy(&x, &saved[j * src_size], sizeof(double));
            printf(" %29.20e\n", x);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        }
        else if (FLT_LDOUBLE == src_type) {
            long double x = 0.0L;
            memcpy(&x, &saved[j * src_size], sizeof(long double));
            fprintf(stdout, " %29.20Le\n", x);
#endif
        }
        else if (FLT_FLOAT16 == src_type) {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 x;
            memcpy(&x, &saved[j * src_size], sizeof(H5__Float16));
            printf(" %29.20e\n", (double)x);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        else if (FLT_COMPLEX == src_type) {
            H5_float_complex fc;
            memcpy(&fc, &saved[j * src_size], sizeof(H5_float_complex));
            printf(" %29.20e%+.20ei\n", (double)crealf(fc), (double)cimagf(fc));
        }
        else if (DBL_COMPLEX == src_type) {
            H5_double_complex dc;
            memcpy(&dc, &saved[j * src_size], sizeof(H5_double_complex));
            printf(" %29.20e%+.20ei\n", creal(dc), cimag(dc));
        }
        else if (LDBL_COMPLEX == src_type) {
            H5_ldouble_complex ldc;
            memcpy(&ldc, &saved[j * src_size], sizeof(H5_ldouble_complex));
            printf(" %29.20Le%+.20Lei\n", creall(ldc), cimagl(ldc));
        }
#endif
        else
            goto error;

        printf("        dst =");
        if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
            unsigned char *buf_ptr   = buf + (j * dst_size);
            size_t         part_size = dst_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", buf_ptr[ENDIAN(part_size, k, dendian)]);
            buf_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", buf_ptr[ENDIAN(part_size, k, dendian)]);
        }
        else {
            for (k = 0; k < dst_size; k++)
                printf(" %02x", buf[j * dst_size + ENDIAN(dst_size, k, dendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        if (FLT_FLOAT == dst_type) {
            float x = 0.0F;
            memcpy(&x, &buf[j * dst_size], sizeof(float));
            printf(" %29.20e\n", (double)x);
        }
        else if (FLT_DOUBLE == dst_type) {
            double x = 0.0;
            memcpy(&x, &buf[j * dst_size], sizeof(double));
            printf(" %29.20e\n", x);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        }
        else if (FLT_LDOUBLE == dst_type) {
            long double x = 0.0L;
            memcpy(&x, &buf[j * dst_size], sizeof(long double));
            fprintf(stdout, " %29.20Le\n", x);
#endif
        }
        else if (FLT_FLOAT16 == dst_type) {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 x;
            memcpy(&x, &buf[j * dst_size], sizeof(H5__Float16));
            printf(" %29.20e\n", (double)x);
#else
            assert(0 && "Should not reach this point!");
#endif
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        else if (FLT_COMPLEX == dst_type) {
            H5_float_complex fc;
            memcpy(&fc, &buf[j * dst_size], sizeof(H5_float_complex));
            printf(" %29.20e%+.20ei\n", (double)crealf(fc), (double)cimagf(fc));
        }
        else if (DBL_COMPLEX == dst_type) {
            H5_double_complex dc;
            memcpy(&dc, &buf[j * dst_size], sizeof(H5_double_complex));
            printf(" %29.20e%+.20ei\n", creal(dc), cimag(dc));
        }
        else if (LDBL_COMPLEX == dst_type) {
            H5_ldouble_complex ldc;
            memcpy(&ldc, &buf[j * dst_size], sizeof(H5_ldouble_complex));
            printf(" %29.20Le%+.20Lei\n", creall(ldc), cimagl(ldc));
        }
#endif
        else
            goto error;

        printf("        ans =");
        if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
            unsigned char *hw_tmp_ptr = hw;
            size_t         part_size  = dst_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", hw_tmp_ptr[ENDIAN(part_size, k, dendian)]);
            hw_tmp_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", hw_tmp_ptr[ENDIAN(part_size, k, dendian)]);
        }
        else {
            for (k = 0; k < dst_size; k++)
                printf(" %02x", hw[ENDIAN(dst_size, k, dendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        if (FLT_FLOAT == dst_type)
            printf(" %29.20e\n", (double)hw_f);
        else if (FLT_DOUBLE == dst_type)
            printf(" %29.20e\n", hw_d);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        else if (FLT_LDOUBLE == dst_type)
            fprintf(stdout, " %29.20Le\n", hw_ld);
#endif
#ifdef H5_HAVE__FLOAT16
        else if (FLT_FLOAT16 == dst_type)
            printf(" %29.20e\n", (double)hw_half);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        else if (FLT_COMPLEX == dst_type)
            printf(" %29.20e%+.20ei\n", (double)crealf(hw_float_complex), (double)cimagf(hw_float_complex));
        else if (DBL_COMPLEX == dst_type)
            printf(" %29.20e%+.20ei\n", creal(hw_double_complex), cimag(hw_double_complex));
        else if (LDBL_COMPLEX == dst_type)
            printf(" %29.20Le%+.20Lei\n", creall(hw_ldouble_complex), cimagl(hw_ldouble_complex));
#endif
        else
            goto error;

        /* If the source is normalized values, print out error message; if it is
         * denormalized or special values, print out warning message.*/
        if (++fails_all_tests >= max_fails) {
            if (run_test == TEST_NORMAL)
                puts("    maximum failures reached, aborting test...");
            else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
                puts("    maximum warnings reached, aborting test...");
            puts("    (dst is library's conversion output. ans is compiler's conversion output.)");

            goto done;
        }
    }

    if (!fails_all_tests)
        PASSED();

done:
    if (buf)
        aligned_free(buf);
    if (saved)
        aligned_free(saved);
    fflush(stdout);
#ifdef HANDLE_SIGFPE
    if (run_test == TEST_NOOP || run_test == TEST_NORMAL)
        exit(MIN((int)fails_all_tests, 254));
    else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
        exit(EXIT_SUCCESS);
    assert(0 && "Should not reach this point!");
    return 1;
#else
    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();

    /* If the source is normalized values, treat the failures as error;
     * if it is denormalized or special values, treat the failure as warning.*/
    if (run_test == TEST_NOOP || run_test == TEST_NORMAL)
        return (int)fails_all_tests;
    else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
        return 0;
#endif

error:
    if (buf)
        aligned_free(buf);
    if (saved)
        aligned_free(saved);
    fflush(stdout);
#ifdef HANDLE_SIGFPE
    if (run_test == TEST_NOOP || run_test == TEST_NORMAL)
        exit(MIN(MAX((int)fails_all_tests, 1), 254));
    else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
        exit(EXIT_FAILURE);
    assert(0 && "Should not reach this point!");
    return 1;
#else
    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();

    if (run_test == TEST_NOOP || run_test == TEST_NORMAL)
        return MAX((int)fails_all_tests, 1);
    else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
        return 1;
    return 1;
#endif
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_schar
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to signed char by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_schar(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    signed char aligned;
    int         ret = 0;

    memset(&aligned, 0, sizeof(signed char));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (signed char)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (signed char)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (signed char)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (signed char)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (signed char)fc;
#else
            aligned = (signed char)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (signed char)dc;
#else
            aligned = (signed char)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (signed char)ldc;
#else
            aligned = (signed char)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((signed char *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_uchar
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to unsigned char by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_uchar(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    unsigned char aligned;
    int           ret = 0;

    memset(&aligned, 0, sizeof(unsigned char));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (unsigned char)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (unsigned char)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (unsigned char)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (unsigned char)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned char)fc;
#else
            aligned = (unsigned char)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned char)dc;
#else
            aligned = (unsigned char)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned char)ldc;
#else
            aligned = (unsigned char)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((unsigned char *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_short
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to short by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_short(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    short aligned;
    int   ret = 0;

    memset(&aligned, 0, sizeof(short));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (short)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (short)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (short)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (short)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (short)fc;
#else
            aligned = (short)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (short)dc;
#else
            aligned = (short)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (short)ldc;
#else
            aligned = (short)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((short *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_ushort
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to unsigned short by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_ushort(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    unsigned short aligned;
    int            ret = 0;

    memset(&aligned, 0, sizeof(unsigned short));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (unsigned short)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (unsigned short)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (unsigned short)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (unsigned short)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned short)fc;
#else
            aligned = (unsigned short)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned short)dc;
#else
            aligned = (unsigned short)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned short)ldc;
#else
            aligned = (unsigned short)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((unsigned short *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_int
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to int by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_int(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    int aligned;
    int ret = 0;

    memset(&aligned, 0, sizeof(int));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (int)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (int)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (int)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (int)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (int)fc;
#else
            aligned = (int)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (int)dc;
#else
            aligned = (int)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (int)ldc;
#else
            aligned = (int)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((int *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_uint
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to unsigned int by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_uint(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    unsigned int aligned;
    int          ret = 0;

    memset(&aligned, 0, sizeof(unsigned int));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (unsigned int)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (unsigned int)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (unsigned int)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (unsigned int)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned int)fc;
#else
            aligned = (unsigned int)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned int)dc;
#else
            aligned = (unsigned int)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned int)ldc;
#else
            aligned = (unsigned int)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((unsigned int *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_long
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to long by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_long(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    long aligned;
    int  ret = 0;

    memset(&aligned, 0, sizeof(long));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (long)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (long)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (long)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (long)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long)fc;
#else
            aligned = (long)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long)dc;
#else
            aligned = (long)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long)ldc;
#else
            aligned = (long)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((long *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_ulong
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to unsigned long by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_ulong(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    unsigned long aligned;
    int           ret = 0;

    memset(&aligned, 0, sizeof(unsigned long));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (unsigned long)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (unsigned long)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (unsigned long)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (unsigned long)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long)fc;
#else
            aligned = (unsigned long)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long)dc;
#else
            aligned = (unsigned long)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long)ldc;
#else
            aligned = (unsigned long)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((unsigned long *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_llong
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to long long by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_llong(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    long long aligned;
    int       ret = 0;

    memset(&aligned, 0, sizeof(long long));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (long long)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (long long)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (long long)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (long long)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long long)fc;
#else
            aligned = (long long)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long long)dc;
#else
            aligned = (long long)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (long long)ldc;
#else
            aligned = (long long)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((long long *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_ullong
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to unsigned long long by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_ullong(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    unsigned long long aligned;
    int                ret = 0;

    memset(&aligned, 0, sizeof(unsigned long long));

    switch (src_type) {
        case FLT_FLOAT16: {
#ifdef H5_HAVE__FLOAT16
            H5__Float16 f16;
            memcpy(&f16, src_buf + idx * sizeof(H5__Float16), sizeof(H5__Float16));
            aligned = (unsigned long long)f16;
            break;
#else
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        }
        case FLT_FLOAT: {
            float f;
            memcpy(&f, src_buf + idx * sizeof(float), sizeof(float));
            aligned = (unsigned long long)f;
            break;
        }
        case FLT_DOUBLE: {
            double d;
            memcpy(&d, src_buf + idx * sizeof(double), sizeof(double));
            aligned = (unsigned long long)d;
            break;
        }
        case FLT_LDOUBLE: {
            long double ld;
            memcpy(&ld, src_buf + idx * sizeof(long double), sizeof(long double));
            aligned = (unsigned long long)ld;
            break;
        }
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX: {
            H5_float_complex fc;
            memcpy(&fc, src_buf + idx * sizeof(H5_float_complex), sizeof(H5_float_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long long)fc;
#else
            aligned = (unsigned long long)crealf(fc);
#endif
            break;
        }
        case DBL_COMPLEX: {
            H5_double_complex dc;
            memcpy(&dc, src_buf + idx * sizeof(H5_double_complex), sizeof(H5_double_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long long)dc;
#else
            aligned = (unsigned long long)creal(dc);
#endif
            break;
        }
        case LDBL_COMPLEX: {
            H5_ldouble_complex ldc;
            memcpy(&ldc, src_buf + idx * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (unsigned long long)ldc;
#else
            aligned = (unsigned long long)creall(ldc);
#endif
            break;
        }
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
#endif
        case INT_SCHAR:
        case INT_UCHAR:
        case INT_SHORT:
        case INT_USHORT:
        case INT_INT:
        case INT_UINT:
        case INT_LONG:
        case INT_ULONG:
        case INT_LLONG:
        case INT_ULLONG:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((unsigned long long *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_flt16
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to _Float16 by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE__FLOAT16
static int
test_conv_int_fp_conv_to_flt16(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    H5__Float16 aligned;
    int         ret = 0;

    memset(&aligned, 0, sizeof(H5__Float16));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
            aligned = (H5__Float16)c;
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
            aligned = (H5__Float16)uc;
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
            aligned = (H5__Float16)s;
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
            aligned = (H5__Float16)us;
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
            aligned = (H5__Float16)i;
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
            aligned = (H5__Float16)ui;
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
            aligned = (H5__Float16)l;
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
            aligned = (H5__Float16)ul;
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
            aligned = (H5__Float16)ll;
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
            aligned = (H5__Float16)ull;
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((H5__Float16 *)hw_p) = aligned;

done:
    return ret;
}
#endif

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_flt
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to float by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_flt(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    float aligned;
    int   ret = 0;

    memset(&aligned, 0, sizeof(float));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
            aligned = (float)c;
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
            aligned = (float)uc;
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
            aligned = (float)s;
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
            aligned = (float)us;
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
            aligned = (float)i;
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
            aligned = (float)ui;
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
            aligned = (float)l;
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
            aligned = (float)ul;
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
            aligned = (float)ll;
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
            aligned = (float)ull;
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((float *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_double
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to double by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_double(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    double aligned;
    int    ret = 0;

    memset(&aligned, 0, sizeof(double));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
            aligned = (double)c;
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
            aligned = (double)uc;
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
            aligned = (double)s;
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
            aligned = (double)us;
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
            aligned = (double)i;
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
            aligned = (double)ui;
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
            aligned = (double)l;
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
            aligned = (double)ul;
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
            aligned = (double)ll;
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
            aligned = (double)ull;
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((double *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_ldouble
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to long double by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_ldouble(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    long double aligned;
    int         ret = 0;

    memset(&aligned, 0, sizeof(long double));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
            aligned = (long double)c;
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
            aligned = (long double)uc;
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
            aligned = (long double)s;
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
            aligned = (long double)us;
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
            aligned = (long double)i;
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
            aligned = (long double)ui;
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
            aligned = (long double)l;
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
            aligned = (long double)ul;
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
            aligned = (long double)ll;
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
            aligned = (long double)ull;
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((long double *)hw_p) = aligned;

done:
    return ret;
}

#ifdef H5_HAVE_COMPLEX_NUMBERS
/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_fcomplex
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to float _Complex / _Fcomplex by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_fcomplex(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    H5_float_complex aligned;
    int              ret = 0;

    memset(&aligned, 0, sizeof(H5_float_complex));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)c;
#else
            aligned = H5_CMPLXF(c, 0.0F);
#endif
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)uc;
#else
            aligned = H5_CMPLXF(uc, 0.0F);
#endif
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)s;
#else
            aligned = H5_CMPLXF(s, 0.0F);
#endif
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)us;
#else
            aligned = H5_CMPLXF(us, 0.0F);
#endif
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)i;
#else
            aligned = H5_CMPLXF(i, 0.0F);
#endif
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)ui;
#else
            aligned = H5_CMPLXF(ui, 0.0F);
#endif
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)l;
#else
            aligned = H5_CMPLXF(l, 0.0F);
#endif
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)ul;
#else
            aligned = H5_CMPLXF(ul, 0.0F);
#endif
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)ll;
#else
            aligned = H5_CMPLXF(ll, 0.0F);
#endif
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_float_complex)ull;
#else
            aligned = H5_CMPLXF(ull, 0.0F);
#endif
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((H5_float_complex *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_dcomplex
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to double _Complex / _Dcomplex by casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_dcomplex(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    H5_double_complex aligned;
    int               ret = 0;

    memset(&aligned, 0, sizeof(H5_double_complex));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)c;
#else
            aligned = H5_CMPLX(c, 0.0);
#endif
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)uc;
#else
            aligned = H5_CMPLX(uc, 0.0);
#endif
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)s;
#else
            aligned = H5_CMPLX(s, 0.0);
#endif
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)us;
#else
            aligned = H5_CMPLX(us, 0.0);
#endif
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)i;
#else
            aligned = H5_CMPLX(i, 0.0);
#endif
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)ui;
#else
            aligned = H5_CMPLX(ui, 0.0);
#endif
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)l;
#else
            aligned = H5_CMPLX(l, 0.0);
#endif
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)ul;
#else
            aligned = H5_CMPLX(ul, 0.0);
#endif
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)ll;
#else
            aligned = H5_CMPLX(ll, 0.0);
#endif
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_double_complex)ull;
#else
            aligned = H5_CMPLX(ull, 0.0);
#endif
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((H5_double_complex *)hw_p) = aligned;

done:
    return ret;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp_conv_to_lcomplex
 *
 * Purpose:     Helper function for test_conv_int_fp to perform conversion
 *              from a datatype to long double _Complex / _Lcomplex by
 *              casting.
 *
 * Return:      -1 on failure
 *              0 on success
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp_conv_to_lcomplex(void *hw_p, unsigned char *src_buf, size_t idx, dtype_t src_type)
{
    H5_ldouble_complex aligned;
    int                ret = 0;

    memset(&aligned, 0, sizeof(H5_ldouble_complex));

    switch (src_type) {
        case INT_SCHAR: {
            signed char c;
            memcpy(&c, src_buf + idx * sizeof(signed char), sizeof(signed char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)c;
#else
            aligned = H5_CMPLXL(c, 0.0L);
#endif
            break;
        }
        case INT_UCHAR: {
            unsigned char uc;
            memcpy(&uc, src_buf + idx * sizeof(unsigned char), sizeof(unsigned char));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)uc;
#else
            aligned = H5_CMPLXL(uc, 0.0L);
#endif
            break;
        }
        case INT_SHORT: {
            short s;
            memcpy(&s, src_buf + idx * sizeof(short), sizeof(short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)s;
#else
            aligned = H5_CMPLXL(s, 0.0L);
#endif
            break;
        }
        case INT_USHORT: {
            unsigned short us;
            memcpy(&us, src_buf + idx * sizeof(unsigned short), sizeof(unsigned short));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)us;
#else
            aligned = H5_CMPLXL(us, 0.0L);
#endif
            break;
        }
        case INT_INT: {
            int i;
            memcpy(&i, src_buf + idx * sizeof(int), sizeof(int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)i;
#else
            aligned = H5_CMPLXL(i, 0.0L);
#endif
            break;
        }
        case INT_UINT: {
            unsigned int ui;
            memcpy(&ui, src_buf + idx * sizeof(unsigned int), sizeof(unsigned int));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)ui;
#else
            aligned = H5_CMPLXL(ui, 0.0L);
#endif
            break;
        }
        case INT_LONG: {
            long l;
            memcpy(&l, src_buf + idx * sizeof(long), sizeof(long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)l;
#else
            aligned = H5_CMPLXL(l, 0.0L);
#endif
            break;
        }
        case INT_ULONG: {
            unsigned long ul;
            memcpy(&ul, src_buf + idx * sizeof(unsigned long), sizeof(unsigned long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)ul;
#else
            aligned = H5_CMPLXL(ul, 0.0L);
#endif
            break;
        }
        case INT_LLONG: {
            long long ll;
            memcpy(&ll, src_buf + idx * sizeof(long long), sizeof(long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)ll;
#else
            aligned = H5_CMPLXL(ll, 0.0L);
#endif
            break;
        }
        case INT_ULLONG: {
            unsigned long long ull;
            memcpy(&ull, src_buf + idx * sizeof(unsigned long long), sizeof(unsigned long long));
#ifdef H5_HAVE_C99_COMPLEX_NUMBERS
            aligned = (H5_ldouble_complex)ull;
#else
            aligned = H5_CMPLXL(ull, 0.0L);
#endif
            break;
        }
        case FLT_FLOAT16:
        case FLT_FLOAT:
        case FLT_DOUBLE:
        case FLT_LDOUBLE:
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination conversion datatype");
            ret = -1;
            goto done;
    }

    *((H5_ldouble_complex *)hw_p) = aligned;

done:
    return ret;
}
#endif

/*-------------------------------------------------------------------------
 * Function:    test_conv_int_fp
 *
 * Purpose:    Test conversion between integer and float values
 *             from SRC to DST.  These types should be any combination of:
 *
 *             H5T_NATIVE_SCHAR    H5T_NATIVE_FLOAT
 *             H5T_NATIVE_SHORT    H5T_NATIVE_DOUBLE
 *             H5T_NATIVE_INT      H5T_NATIVE_LDOUBLE
 *             H5T_NATIVE_LONG
 *             H5T_NATIVE_LLONG
 *
 * Return:    Success:    0
 *            Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_int_fp(const char *name, int run_test, hid_t src, hid_t dst)
{
    hid_t                  dxpl_id;        /*dataset transfer property list*/
    int                    fill_value = 9; /*fill value for conversion exception*/
    H5T_conv_except_func_t op;             /*returned callback function for conversion exception*/
    void                  *user_data;      /*returned pointer to user data passed in to the callback*/
    bool                   except_set      = false; /*whether user's exception handling is set*/
    size_t                 nelmts          = 0;     /*num values per test    */
    const size_t           max_fails       = 40;    /*max number of failures*/
    size_t                 fails_all_tests = 0;     /*number of failures    */
    size_t                 fails_this_test;         /*fails for this test    */
    char                   str[256];                /*hello string        */
    dtype_t                src_type;                /*data types        */
    dtype_t                dst_type;                /*data types        */
    const char            *src_type_name = NULL;    /*source type name    */
    const char            *dst_type_name = NULL;    /*destination type name    */
    int                    sendian;                 /*source endianness    */
    int                    dendian;                 /*destination endianness    */
    size_t                 src_size, dst_size;      /*type sizes        */
    unsigned char         *buf   = NULL;            /*buffer for conversion    */
    unsigned char         *saved = NULL;            /*original values    */
    size_t                 j, k;                    /*counters        */
    unsigned char         *hw = NULL;               /*hardware conv result    */
    unsigned char          src_bits[32];            /*src value in LE order    */
    unsigned char          dst_bits[32];            /*dest value in LE order*/
    size_t                 src_nbits;               /*source length in bits    */
    size_t                 dst_nbits;               /*dst length in bits    */
    void                  *hw_p       = NULL;
    float                  hw_float   = 0;
    double                 hw_double  = 0;
    long double            hw_ldouble = 0;
    signed char            hw_schar   = 0;
    unsigned char          hw_uchar   = 0;
    short                  hw_short   = 0;
    unsigned short         hw_ushort  = 0;
    int                    hw_int     = 0;
    unsigned               hw_uint    = 0;
    long                   hw_long    = 0;
    unsigned long          hw_ulong   = 0;
    long long              hw_llong   = 0;
    unsigned long long     hw_ullong  = 0;
#ifdef H5_HAVE__FLOAT16
    H5__Float16 hw_half;
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    H5_ldouble_complex hw_ldouble_complex;
    H5_double_complex  hw_double_complex;
    H5_float_complex   hw_float_complex;
#endif

    /* What is the name of the source type */
    if (H5Tequal(src, H5T_NATIVE_SCHAR)) {
        src_type_name = "signed char";
        src_type      = INT_SCHAR;
    }
    else if (H5Tequal(src, H5T_NATIVE_UCHAR)) {
        src_type_name = "unsigned char";
        src_type      = INT_UCHAR;
    }
    else if (H5Tequal(src, H5T_NATIVE_SHORT)) {
        src_type_name = "short";
        src_type      = INT_SHORT;
    }
    else if (H5Tequal(src, H5T_NATIVE_USHORT)) {
        src_type_name = "unsigned short";
        src_type      = INT_USHORT;
    }
    else if (H5Tequal(src, H5T_NATIVE_INT)) {
        src_type_name = "int";
        src_type      = INT_INT;
    }
    else if (H5Tequal(src, H5T_NATIVE_UINT)) {
        src_type_name = "unsigned int";
        src_type      = INT_UINT;
    }
    else if (H5Tequal(src, H5T_NATIVE_LONG)) {
        src_type_name = "long";
        src_type      = INT_LONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_ULONG)) {
        src_type_name = "unsigned long";
        src_type      = INT_ULONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_LLONG)) {
        src_type_name = "long long";
        src_type      = INT_LLONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_ULLONG)) {
        src_type_name = "unsigned long long";
        src_type      = INT_ULLONG;
    }
    else if (H5Tequal(src, H5T_NATIVE_FLOAT)) {
        src_type_name = "float";
        src_type      = FLT_FLOAT;
    }
    else if (H5Tequal(src, H5T_NATIVE_DOUBLE)) {
        src_type_name = "double";
        src_type      = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (H5Tequal(src, H5T_NATIVE_LDOUBLE)) {
        src_type_name = "long double";
        src_type      = FLT_LDOUBLE;
#endif
    }
#ifdef H5_HAVE__FLOAT16
    else if (H5Tequal(src, H5T_NATIVE_FLOAT16)) {
        src_type_name = "_Float16";
        src_type      = FLT_FLOAT16;
    }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (H5Tequal(src, H5T_NATIVE_FLOAT_COMPLEX)) {
        src_type_name = "float _Complex";
        src_type      = FLT_COMPLEX;
    }
    else if (H5Tequal(src, H5T_NATIVE_DOUBLE_COMPLEX)) {
        src_type_name = "double _Complex";
        src_type      = DBL_COMPLEX;
    }
    else if (H5Tequal(src, H5T_NATIVE_LDOUBLE_COMPLEX)) {
        src_type_name = "long double _Complex";
        src_type      = LDBL_COMPLEX;
    }
#endif
    else {
        src_type_name = "UNKNOWN";
        src_type      = OTHER;
    }

    /* What is the name of the destination type */
    if (H5Tequal(dst, H5T_NATIVE_SCHAR)) {
        dst_type_name = "signed char";
        dst_type      = INT_SCHAR;
    }
    else if (H5Tequal(dst, H5T_NATIVE_UCHAR)) {
        dst_type_name = "unsigned char";
        dst_type      = INT_UCHAR;
    }
    else if (H5Tequal(dst, H5T_NATIVE_SHORT)) {
        dst_type_name = "short";
        dst_type      = INT_SHORT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_USHORT)) {
        dst_type_name = "unsigned short";
        dst_type      = INT_USHORT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_INT)) {
        dst_type_name = "int";
        dst_type      = INT_INT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_UINT)) {
        dst_type_name = "unsigned int";
        dst_type      = INT_UINT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LONG)) {
        dst_type_name = "long";
        dst_type      = INT_LONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_ULONG)) {
        dst_type_name = "unsigned long";
        dst_type      = INT_ULONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LLONG)) {
        dst_type_name = "long long";
        dst_type      = INT_LLONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_ULLONG)) {
        dst_type_name = "unsigned long long";
        dst_type      = INT_ULLONG;
    }
    else if (H5Tequal(dst, H5T_NATIVE_FLOAT)) {
        dst_type_name = "float";
        dst_type      = FLT_FLOAT;
    }
    else if (H5Tequal(dst, H5T_NATIVE_DOUBLE)) {
        dst_type_name = "double";
        dst_type      = FLT_DOUBLE;
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE)) {
        dst_type_name = "long double";
        dst_type      = FLT_LDOUBLE;
#endif
    }
#ifdef H5_HAVE__FLOAT16
    else if (H5Tequal(dst, H5T_NATIVE_FLOAT16)) {
        dst_type_name = "_Float16";
        dst_type      = FLT_FLOAT16;
    }
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (H5Tequal(dst, H5T_NATIVE_FLOAT_COMPLEX)) {
        dst_type_name = "float _Complex";
        dst_type      = FLT_COMPLEX;
    }
    else if (H5Tequal(dst, H5T_NATIVE_DOUBLE_COMPLEX)) {
        dst_type_name = "double _Complex";
        dst_type      = DBL_COMPLEX;
    }
    else if (H5Tequal(dst, H5T_NATIVE_LDOUBLE_COMPLEX)) {
        dst_type_name = "long double _Complex";
        dst_type      = LDBL_COMPLEX;
    }
#endif
    else {
        dst_type_name = "UNKNOWN";
        dst_type      = OTHER;
    }

    /* Sanity checks */
    if (OTHER == src_type || OTHER == dst_type) {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        H5_FAILED();
        puts("    Unknown data type.");
        goto error;
    }

    if ((INT_SCHAR == src_type || INT_UCHAR == src_type || INT_SHORT == src_type || INT_USHORT == src_type ||
         INT_INT == src_type || INT_UINT == src_type || INT_LONG == src_type || INT_ULONG == src_type ||
         INT_LLONG == src_type || INT_ULLONG == src_type) &&
        (FLT_FLOAT != dst_type && FLT_DOUBLE != dst_type && FLT_LDOUBLE != dst_type &&
         FLT_FLOAT16 != dst_type && FLT_COMPLEX != dst_type && DBL_COMPLEX != dst_type &&
         LDBL_COMPLEX != dst_type)) {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        H5_FAILED();
        puts("    1. Not an integer-float conversion.");
        goto error;
    }

    if ((FLT_FLOAT == src_type || FLT_DOUBLE == src_type || FLT_LDOUBLE == src_type ||
         FLT_FLOAT16 == src_type || FLT_COMPLEX == src_type || DBL_COMPLEX == src_type ||
         LDBL_COMPLEX == src_type) &&
        (INT_SCHAR != dst_type && INT_UCHAR != dst_type && INT_SHORT != dst_type && INT_USHORT != dst_type &&
         INT_INT != dst_type && INT_UINT != dst_type && INT_LONG != dst_type && INT_ULONG != dst_type &&
         INT_LLONG != dst_type && INT_ULLONG != dst_type)) {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        H5_FAILED();
        puts("    2. Not a float-integer conversion.");
        goto error;
    }

    if (INT_SCHAR == src_type || INT_UCHAR == src_type || INT_SHORT == src_type || INT_USHORT == src_type ||
        INT_INT == src_type || INT_UINT == src_type || INT_LONG == src_type || INT_ULONG == src_type ||
        INT_LLONG == src_type || INT_ULLONG == src_type) {
        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, src_type_name, dst_type_name);
        printf("%-70s", str);
        fflush(stdout);
        fails_this_test = 0;
    }
    else {
        if (run_test == TEST_NORMAL)
            snprintf(str, sizeof(str), "Testing %s normalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else if (run_test == TEST_DENORM)
            snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        else
            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, src_type_name,
                     dst_type_name);
        printf("%-70s", str);
        fflush(stdout);
        fails_this_test = 0;
    }

    /* Some information about datatypes */
    sendian   = H5Tget_order(src);
    dendian   = H5Tget_order(dst);
    src_size  = H5Tget_size(src);
    dst_size  = H5Tget_size(dst);
    src_nbits = H5Tget_precision(src); /* not 8*src_size, esp on J90 - QAK */
    dst_nbits = H5Tget_precision(dst); /* not 8*dst_size, esp on J90 - QAK */

#ifdef SHOW_OVERFLOWS
    noverflows_g = 0;
#endif

    /* This is for some Linux systems where long double has the size
     * 12 bytes but precision is 10 bytes.  The 2 unused bytes may
     * have garbage causing wrong value comparison.
     */
    memset(&hw_ldouble, 0, sizeof(long double));
#ifdef H5_HAVE_COMPLEX_NUMBERS
    memset(&hw_ldouble_complex, 0, sizeof(H5_ldouble_complex));
#endif

    /* Create a dataset transfer property list and datatype conversion
     * exception handler function and pass in fill value.  This is mainly
     * for NetCDF compatibility, which requests fill in fill value when
     * conversion exception happens.  We only test (unsigned) int - float
     * and float - (unsigned) int conversions, which should cover more cases.
     */
    if ((dxpl_id = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    if ((src_type == INT_INT && dst_type == FLT_FLOAT) || (src_type == INT_UINT && dst_type == FLT_FLOAT) ||
        (src_type == FLT_FLOAT && dst_type == INT_UINT) || (src_type == FLT_FLOAT && dst_type == INT_INT)) {
        if (H5Pset_type_conv_cb(dxpl_id, except_func, &fill_value) < 0)
            goto error;
        else
            except_set = true;

        if (H5Pget_type_conv_cb(dxpl_id, &op, &user_data) < 0)
            goto error;

        if (op != except_func || *(int *)user_data != fill_value)
            goto error;
    }

    /* Allocate and initialize the source buffer through macro INIT_INTEGER if the source is integer,
     * INIT_FP_NORM if floating-point or complex.  The BUF will be used for the conversion while the
     * SAVED buffer will be used for the comparison later.
     */
    if (src_type == INT_SCHAR) {
        INIT_INTEGER(signed char, SCHAR_MAX, SCHAR_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_UCHAR) {
        INIT_INTEGER(unsigned char, UCHAR_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_SHORT) {
        INIT_INTEGER(short, SHRT_MAX, SHRT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_USHORT) {
        INIT_INTEGER(unsigned short, USHRT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_INT) {
        INIT_INTEGER(int, INT_MAX, INT_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_UINT) {
        INIT_INTEGER(unsigned int, UINT_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_LONG) {
        INIT_INTEGER(long, LONG_MAX, LONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_ULONG) {
        INIT_INTEGER(unsigned long, ULONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_LLONG) {
        INIT_INTEGER(long long, LLONG_MAX, LLONG_MIN, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == INT_ULLONG) {
        INIT_INTEGER(unsigned long long, ULLONG_MAX, 0, src_size, dst_size, src_nbits, buf, saved, nelmts);
    }
    else if (src_type == FLT_FLOAT) {
        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP, src_size, dst_size, buf,
                         saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(float, FLT_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved, nelmts);
        }
        else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT_MANT_DIG, dst_size, buf, saved, nelmts);
        }
    }
    else if (src_type == FLT_DOUBLE) {
        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP, src_size, dst_size, buf,
                         saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(double, DBL_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved, nelmts);
        }
        else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, DBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    }
    else if (src_type == FLT_LDOUBLE) {
        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP, src_size,
                         dst_size, buf, saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(long double, LDBL_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved,
                           nelmts);
        }
        else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }
#endif
    }
    else if (src_type == FLT_FLOAT16) {
#ifdef H5_HAVE__FLOAT16
        if (run_test == TEST_NORMAL) {
            /* Suppress warning about non-standard floating-point literal suffix */
            H5_WARN_NONSTD_SUFFIX_OFF
            /* Suppress warning about float conversion in macro code path
             * that sets H5__Float16 multiply = 100000000;, which shouldn't
             * happen due to the small value of FLT16_MAX_10_EXP.
             */
            H5_WARN_FLOAT_CONVERSION_OFF
            INIT_FP_NORM(H5__Float16, FLT16_MAX, FLT16_MIN, FLT16_MAX_10_EXP, FLT16_MIN_10_EXP, src_size,
                         dst_size, buf, saved, nelmts);
            H5_WARN_FLOAT_CONVERSION_ON
            H5_WARN_NONSTD_SUFFIX_ON
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(H5__Float16, FLT16_MANT_DIG, src_size, src_nbits, sendian, dst_size, buf, saved,
                           nelmts);
        }
        else {
            INIT_FP_SPECIAL(src_size, src_nbits, sendian, FLT16_MANT_DIG, dst_size, buf, saved, nelmts);
        }
#else
        assert(0 && "Should not reach this point!");
#endif
    }
#ifdef H5_HAVE_COMPLEX_NUMBERS
    else if (src_type == FLT_COMPLEX) {
        size_t part_size = src_size / 2;

        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(float, FLT_MAX, FLT_MIN, FLT_MAX_10_EXP, FLT_MIN_10_EXP, part_size, dst_size, buf,
                         saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(float, FLT_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf, saved, nelmts);
        }
        else {
            INIT_FP_SPECIAL(part_size, src_nbits, sendian, FLT_MANT_DIG, dst_size, buf, saved, nelmts);
        }

        /* Treat float buffer as float _Complex buffer of nelmts / 2 elements */
        assert(nelmts % 2 == 0);
        nelmts /= 2;
    }
    else if (src_type == DBL_COMPLEX) {
        size_t part_size = src_size / 2;

        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(double, DBL_MAX, DBL_MIN, DBL_MAX_10_EXP, DBL_MIN_10_EXP, part_size, dst_size, buf,
                         saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(double, DBL_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf, saved, nelmts);
        }
        else {
            INIT_FP_SPECIAL(part_size, src_nbits, sendian, DBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }

        /* Treat double buffer as double _Complex buffer of nelmts / 2 elements */
        assert(nelmts % 2 == 0);
        nelmts /= 2;
    }
    else if (src_type == LDBL_COMPLEX) {
        size_t part_size = src_size / 2;

        if (run_test == TEST_NORMAL) {
            INIT_FP_NORM(long double, LDBL_MAX, LDBL_MIN, LDBL_MAX_10_EXP, LDBL_MIN_10_EXP, part_size,
                         dst_size, buf, saved, nelmts);
        }
        else if (run_test == TEST_DENORM) {
            INIT_FP_DENORM(long double, LDBL_MANT_DIG, part_size, src_nbits, sendian, dst_size, buf, saved,
                           nelmts);
        }
        else {
            INIT_FP_SPECIAL(part_size, src_nbits, sendian, LDBL_MANT_DIG, dst_size, buf, saved, nelmts);
        }

        /* Treat long double buffer as long double _Complex buffer of nelmts / 2 elements */
        assert(nelmts % 2 == 0);
        nelmts /= 2;
    }
#endif
    else
        goto error;

    /* Perform the conversion */
    if (H5Tconvert(src, dst, nelmts, buf, NULL, dxpl_id) < 0)
        goto error;

    /* Set pointer to matching type for hardware conversion */
    switch (dst_type) {
        case INT_SCHAR:
            hw_p = &hw_schar;
            break;
        case INT_UCHAR:
            hw_p = &hw_uchar;
            break;
        case INT_SHORT:
            hw_p = &hw_short;
            break;
        case INT_USHORT:
            hw_p = &hw_ushort;
            break;
        case INT_INT:
            hw_p = &hw_int;
            break;
        case INT_UINT:
            hw_p = &hw_uint;
            break;
        case INT_LONG:
            hw_p = &hw_long;
            break;
        case INT_ULONG:
            hw_p = &hw_ulong;
            break;
        case INT_LLONG:
            hw_p = &hw_llong;
            break;
        case INT_ULLONG:
            hw_p = &hw_ullong;
            break;
        case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            hw_p = &hw_half;
            break;
#else
            H5_FAILED();
            printf("invalid destination datatype\n");
            goto error;
#endif
        case FLT_FLOAT:
            hw_p = &hw_float;
            break;
        case FLT_DOUBLE:
            hw_p = &hw_double;
            break;
        case FLT_LDOUBLE:
            hw_p = &hw_ldouble;
            break;
#ifdef H5_HAVE_COMPLEX_NUMBERS
        case FLT_COMPLEX:
            hw_p = &hw_float_complex;
            break;
        case DBL_COMPLEX:
            hw_p = &hw_double_complex;
            break;
        case LDBL_COMPLEX:
            hw_p = &hw_ldouble_complex;
            break;
#else
        case FLT_COMPLEX:
        case DBL_COMPLEX:
        case LDBL_COMPLEX:
            H5_FAILED();
            printf("invalid destination datatype\n");
            goto error;
#endif
        case OTHER:
        default:
            H5_FAILED();
            printf("invalid destination datatype\n");
            goto error;
    }

    /* Set convenience pointer for indexing into bytes of matching type */
    hw = (unsigned char *)hw_p;

    /* Check the results from the library against hardware */
    for (j = 0; j < nelmts; j++) {
        int conv_ret = -1;

        if (FLT_FLOAT == src_type || FLT_DOUBLE == src_type || FLT_LDOUBLE == src_type ||
            FLT_FLOAT16 == src_type || FLT_COMPLEX == src_type || DBL_COMPLEX == src_type ||
            LDBL_COMPLEX == src_type)
            if (my_isnan(src_type, saved + j * src_size))
                continue;

        switch (dst_type) {
            case INT_SCHAR:
                conv_ret = test_conv_int_fp_conv_to_schar(hw_p, saved, j, src_type);
                break;
            case INT_UCHAR:
                conv_ret = test_conv_int_fp_conv_to_uchar(hw_p, saved, j, src_type);
                break;
            case INT_SHORT:
                conv_ret = test_conv_int_fp_conv_to_short(hw_p, saved, j, src_type);
                break;
            case INT_USHORT:
                conv_ret = test_conv_int_fp_conv_to_ushort(hw_p, saved, j, src_type);
                break;
            case INT_INT:
                conv_ret = test_conv_int_fp_conv_to_int(hw_p, saved, j, src_type);
                break;
            case INT_UINT:
                conv_ret = test_conv_int_fp_conv_to_uint(hw_p, saved, j, src_type);
                break;
            case INT_LONG:
                conv_ret = test_conv_int_fp_conv_to_long(hw_p, saved, j, src_type);
                break;
            case INT_ULONG:
                conv_ret = test_conv_int_fp_conv_to_ulong(hw_p, saved, j, src_type);
                break;
            case INT_LLONG:
                conv_ret = test_conv_int_fp_conv_to_llong(hw_p, saved, j, src_type);
                break;
            case INT_ULLONG:
                conv_ret = test_conv_int_fp_conv_to_ullong(hw_p, saved, j, src_type);
                break;
            case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
                conv_ret = test_conv_int_fp_conv_to_flt16(hw_p, saved, j, src_type);
                break;
#else
                H5_FAILED();
                printf("invalid destination datatype\n");
                goto error;
#endif
            case FLT_FLOAT:
                conv_ret = test_conv_int_fp_conv_to_flt(hw_p, saved, j, src_type);
                break;
            case FLT_DOUBLE:
                conv_ret = test_conv_int_fp_conv_to_double(hw_p, saved, j, src_type);
                break;
            case FLT_LDOUBLE:
                conv_ret = test_conv_int_fp_conv_to_ldouble(hw_p, saved, j, src_type);
                break;
#ifdef H5_HAVE_COMPLEX_NUMBERS
            case FLT_COMPLEX:
                conv_ret = test_conv_int_fp_conv_to_fcomplex(hw_p, saved, j, src_type);
                break;
            case DBL_COMPLEX:
                conv_ret = test_conv_int_fp_conv_to_dcomplex(hw_p, saved, j, src_type);
                break;
            case LDBL_COMPLEX:
                conv_ret = test_conv_int_fp_conv_to_lcomplex(hw_p, saved, j, src_type);
                break;
#else
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
                H5_FAILED();
                printf("invalid destination datatype\n");
                goto error;
#endif
            case OTHER:
            default:
                H5_FAILED();
                printf("invalid destination datatype\n");
                goto error;
        }

        if (conv_ret < 0)
            goto error;

        /* Make certain that there isn't some weird number of destination bits */
        assert(dst_nbits % 8 == 0);

        /* For Intel machines, the size of "long double" is 12 bytes, precision
         * is 80 bits; for AMD processors, the size of "long double" is 16 bytes,
         * precision is 80 bits.  During hardware conversion, the last few unused
         * bytes may have garbage in them.  Clean them out with 0s before compare
         * the values.
         */
        if (dendian == H5T_ORDER_LE) {
            if (dst_type == FLT_LDOUBLE) {
                for (size_t q = dst_nbits / 8; q < dst_size; q++)
                    buf[j * dst_size + q] = 0x00;
            }
            else if (dst_type == LDBL_COMPLEX) {
                uint8_t *cur       = buf + j * dst_size;
                size_t   part_size = dst_size / 2;
                for (size_t q = dst_nbits / 8; q < part_size; q++)
                    cur[q] = 0x00;
                cur += part_size;
                for (size_t q = dst_nbits / 8; q < part_size; q++)
                    cur[q] = 0x00;
            }
        }

        /* Are the two results the same? */
        if (dst_type != FLT_COMPLEX && dst_type != DBL_COMPLEX && dst_type != LDBL_COMPLEX) {
            for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
                if (buf[j * dst_size + k] != hw[k])
                    break;
        }
        else {
            size_t part_size = dst_size / 2;

            /* Compare real part */
            for (k = (part_size - (dst_nbits / 8)); k < part_size; k++)
                if (buf[j * dst_size + k] != hw[k])
                    break;
            if (k == part_size) {
                /* Imaginary part should have been zeroed out. Hardware value
                 * could be a positive or negative zero, so we'll just check
                 * the buffer value for simplicity.
                 */
                for (k = (dst_size - (dst_nbits / 8)); k < dst_size; k++)
                    if (buf[j * dst_size + k] != 0x00)
                        break;
            }
        }

        if (k == dst_size)
            continue; /*no error*/

        /*
         * Convert the source and destination values to little endian
         * order so we can use the HDF5 bit vector operations to test
         * certain things.  These routines have already been tested by
         * the `bittests' program.
         */

        if ((FLT_FLOAT == src_type || FLT_DOUBLE == src_type) && sendian == H5T_ORDER_VAX) {
            for (k = 0; k < src_size; k += 2) {
                src_bits[k]     = saved[j * src_size + (src_size - 2) - k];
                src_bits[k + 1] = saved[j * src_size + (src_size - 1) - k];
            }
        }
        else if (FLT_COMPLEX == src_type || DBL_COMPLEX == src_type || LDBL_COMPLEX == src_type) {
            unsigned char *src_bits_p = src_bits;
            unsigned char *saved_p    = saved + (j * src_size);
            size_t         part_size  = src_size / 2;

            for (k = 0; k < part_size; k++)
                src_bits_p[part_size - (k + 1)] = saved_p[ENDIAN(part_size, k, sendian)];
            src_bits_p += part_size;
            saved_p += part_size;
            for (k = 0; k < part_size; k++)
                src_bits_p[part_size - (k + 1)] = saved_p[ENDIAN(part_size, k, sendian)];
        }
        else {
            for (k = 0; k < src_size; k++)
                src_bits[src_size - (k + 1)] = saved[j * src_size + ENDIAN(src_size, k, sendian)];
        }

        if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
            unsigned char *dst_bits_p = dst_bits;
            unsigned char *buf_p      = buf + (j * dst_size);
            size_t         part_size  = dst_size / 2;

            for (k = 0; k < part_size; k++)
                dst_bits_p[part_size - (k + 1)] = buf_p[ENDIAN(part_size, k, dendian)];
            dst_bits_p += part_size;
            buf_p += part_size;
            for (k = 0; k < part_size; k++)
                dst_bits_p[part_size - (k + 1)] = buf_p[ENDIAN(part_size, k, dendian)];
        }
        else {
            for (k = 0; k < dst_size; k++)
                dst_bits[dst_size - (k + 1)] = buf[j * dst_size + ENDIAN(dst_size, k, dendian)];
        }

        /*          Test library's default overflow handling:
         * Hardware usually doesn't handle overflows too gracefully. The
         * hardware conversion result during overflows is usually garbage
         * so we must handle those cases differently when checking results.
         *
         *          Test user's exception handler when overflows:
         * Try to follow the except_func callback function to check if the
         * desired value was set.
         */
        if ((FLT_FLOAT == src_type || FLT_DOUBLE == src_type || FLT_LDOUBLE == src_type ||
             FLT_FLOAT16 == src_type || FLT_COMPLEX == src_type || DBL_COMPLEX == src_type ||
             LDBL_COMPLEX == src_type) &&
            (INT_SCHAR == dst_type || INT_SHORT == dst_type || INT_INT == dst_type || INT_LONG == dst_type ||
             INT_LLONG == dst_type)) {
            if (0 == H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1) &&
                overflows(src_bits, src, dst_nbits - 1)) {
                /*
                 * Source is positive and the magnitude is too large for
                 * the destination.  The destination should be set to the
                 * maximum possible value: 0x7f...f
                 */
                if (!except_set) {
                    if (0 == H5T__bit_get_d(dst_bits, dst_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, (size_t)0, dst_nbits - 1, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                }
                else {
                    /* fill_value is small so we know only the 1st byte is set */
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
            else if (1 == H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1) &&
                     overflows(src_bits, src, dst_nbits - 1)) {
                /*
                 * Source is negative but the magnitude is too large for
                 * the destination. The destination should be set to the
                 * smallest possible value: 0x80...0
                 */
                if (!except_set) {
                    if (1 == H5T__bit_get_d(dst_bits, dst_nbits - 1, (size_t)1) &&
                        H5T__bit_find(dst_bits, (size_t)0, dst_nbits - 1, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                }
                else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
        }

        if ((FLT_FLOAT == src_type || FLT_DOUBLE == src_type || FLT_LDOUBLE == src_type ||
             FLT_FLOAT16 == src_type || FLT_COMPLEX == src_type || DBL_COMPLEX == src_type ||
             LDBL_COMPLEX == src_type) &&
            (INT_UCHAR == dst_type || INT_USHORT == dst_type || INT_UINT == dst_type ||
             INT_ULONG == dst_type || INT_ULLONG == dst_type)) {
            if (H5T__bit_get_d(src_bits, src_nbits - 1, (size_t)1)) {
                /*
                 * The source is negative so the result should be zero.
                 * The source is negative if the most significant bit is
                 * set.  The destination is zero if all bits are zero.
                 */
                if (!except_set) {
                    if (H5T__bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 1) < 0)
                        continue; /*no error*/
                }
                else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
            else if (overflows(src_bits, src, dst_nbits)) {
                /*
                 * The source is a value with a magnitude too large for
                 * the destination.  The destination should be the
                 * largest possible value: 0xff...f
                 */
                if (!except_set) {
                    if (H5T__bit_find(dst_bits, (size_t)0, dst_nbits, H5T_BIT_LSB, 0) < 0)
                        continue; /*no error*/
                }
                else {
                    if (dst_bits[0] == fill_value)
                        continue; /*no error*/
                }
            }
        }

        /* Print errors */
        if (0 == fails_this_test++) {
            if (run_test == TEST_NORMAL) {
                H5_FAILED();
            }
            else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL) {
                H5_WARNING();
            }
        }
        printf("    elmt %u: \n", (unsigned)j);

        printf("        src = ");
        if (FLT_COMPLEX == src_type || DBL_COMPLEX == src_type || LDBL_COMPLEX == src_type) {
            unsigned char *saved_ptr = saved + (j * src_size);
            size_t         part_size = src_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", saved_ptr[ENDIAN(part_size, k, sendian)]);
            saved_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", saved_ptr[ENDIAN(part_size, k, sendian)]);
        }
        else {
            for (k = 0; k < src_size; k++)
                printf(" %02x", saved[j * src_size + ENDIAN(src_size, k, sendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)dst_size - (ssize_t)src_size)), "");
        switch (src_type) {
            case INT_SCHAR: {
                signed char sc;
                memcpy(&sc, saved + j * sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)sc);
                break;
            }
            case INT_UCHAR: {
                unsigned char uc;
                memcpy(&uc, saved + j * sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)uc);
                break;
            }
            case INT_SHORT: {
                short s;
                memcpy(&s, saved + j * sizeof(short), sizeof(short));
                printf(" %29hd\n", s);
                break;
            }
            case INT_USHORT: {
                unsigned short us;
                memcpy(&us, saved + j * sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", us);
                break;
            }
            case INT_INT: {
                int i;
                memcpy(&i, saved + j * sizeof(int), sizeof(int));
                printf(" %29d\n", i);
                break;
            }
            case INT_UINT: {
                unsigned int ui;
                memcpy(&ui, saved + j * sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", ui);
                break;
            }
            case INT_LONG: {
                long l;
                memcpy(&l, saved + j * sizeof(long), sizeof(long));
                printf(" %29ld\n", l);
                break;
            }
            case INT_ULONG: {
                unsigned long ul;
                memcpy(&ul, saved + j * sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", ul);
                break;
            }
            case INT_LLONG: {
                long long ll;
                memcpy(&ll, saved + j * sizeof(long long), sizeof(long long));
                fprintf(stdout, " %29lld\n", ll);
                break;
            }
            case INT_ULLONG: {
                unsigned long long ull;
                memcpy(&ull, saved + j * sizeof(unsigned long long), sizeof(unsigned long long));
                fprintf(stdout, " %29llu\n", ull);
                break;
            }
            case FLT_FLOAT: {
                float f;
                memcpy(&f, saved + j * sizeof(float), sizeof(float));
                printf(" %29f\n", (double)f);
                break;
            }
            case FLT_DOUBLE: {
                double d;
                memcpy(&d, saved + j * sizeof(double), sizeof(double));
                printf(" %29f\n", d);
                break;
            }
            case FLT_LDOUBLE: {
                long double ld;
                memcpy(&ld, saved + j * sizeof(long double), sizeof(long double));
                printf(" %29Lf\n", ld);
                break;
            }
            case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            {
                H5__Float16 f16;
                memcpy(&f16, saved + j * sizeof(H5__Float16), sizeof(H5__Float16));
                printf(" %29f\n", (double)f16);
                break;
            }
#else
                assert(0 && "Should not reach this point!");
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
            case FLT_COMPLEX: {
                H5_float_complex fc;
                memcpy(&fc, saved + j * sizeof(H5_float_complex), sizeof(H5_float_complex));
                printf(" %29f%+fi\n", (double)crealf(fc), (double)cimagf(fc));
                break;
            }
            case DBL_COMPLEX: {
                H5_double_complex dc;
                memcpy(&dc, saved + j * sizeof(H5_double_complex), sizeof(H5_double_complex));
                printf(" %29f%+fi\n", creal(dc), cimag(dc));
                break;
            }
            case LDBL_COMPLEX: {
                H5_ldouble_complex ldc;
                memcpy(&ldc, saved + j * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
                printf(" %29Lf%+Lfi\n", creall(ldc), cimagl(ldc));
                break;
            }
#else
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
                assert(0 && "Should not reach this point!");
#endif
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        printf("        dst = ");
        if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
            unsigned char *buf_ptr   = buf + (j * dst_size);
            size_t         part_size = dst_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", buf_ptr[ENDIAN(part_size, k, dendian)]);
            buf_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", buf_ptr[ENDIAN(part_size, k, dendian)]);
        }
        else {
            for (k = 0; k < dst_size; k++)
                printf(" %02x", buf[j * dst_size + ENDIAN(dst_size, k, dendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR: {
                signed char sc;
                memcpy(&sc, buf + j * sizeof(signed char), sizeof(signed char));
                printf(" %29d\n", (int)sc);
                break;
            }
            case INT_UCHAR: {
                unsigned char uc;
                memcpy(&uc, buf + j * sizeof(unsigned char), sizeof(unsigned char));
                printf(" %29u\n", (unsigned)uc);
                break;
            }
            case INT_SHORT: {
                short s;
                memcpy(&s, buf + j * sizeof(short), sizeof(short));
                printf(" %29hd\n", s);
                break;
            }
            case INT_USHORT: {
                unsigned short us;
                memcpy(&us, buf + j * sizeof(unsigned short), sizeof(unsigned short));
                printf(" %29hu\n", us);
                break;
            }
            case INT_INT: {
                int i;
                memcpy(&i, buf + j * sizeof(int), sizeof(int));
                printf(" %29d\n", i);
                break;
            }
            case INT_UINT: {
                unsigned int ui;
                memcpy(&ui, buf + j * sizeof(unsigned), sizeof(unsigned));
                printf(" %29u\n", ui);
                break;
            }
            case INT_LONG: {
                long l;
                memcpy(&l, buf + j * sizeof(long), sizeof(long));
                printf(" %29ld\n", l);
                break;
            }
            case INT_ULONG: {
                unsigned long ul;
                memcpy(&ul, buf + j * sizeof(unsigned long), sizeof(unsigned long));
                printf(" %29lu\n", ul);
                break;
            }
            case INT_LLONG: {
                long long ll;
                memcpy(&ll, buf + j * sizeof(long long), sizeof(long long));
                fprintf(stdout, " %29lld\n", ll);
                break;
            }
            case INT_ULLONG: {
                unsigned long long ull;
                memcpy(&ull, buf + j * sizeof(unsigned long long), sizeof(unsigned long long));
                fprintf(stdout, " %29llu\n", ull);
                break;
            }
            case FLT_FLOAT: {
                float f;
                memcpy(&f, buf + j * sizeof(float), sizeof(float));
                printf(" %29f\n", (double)f);
                break;
            }
            case FLT_DOUBLE: {
                double d;
                memcpy(&d, buf + j * sizeof(double), sizeof(double));
                printf(" %29f\n", d);
                break;
            }
            case FLT_LDOUBLE: {
                long double ld;
                memcpy(&ld, buf + j * sizeof(long double), sizeof(long double));
                printf(" %29Lf\n", ld);
                break;
            }
            case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
            {
                H5__Float16 f16;
                memcpy(&f16, buf + j * sizeof(H5__Float16), sizeof(H5__Float16));
                printf(" %29f\n", (double)f16);
                break;
            }
#else
                assert(0 && "Should not reach this point!");
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
            case FLT_COMPLEX: {
                H5_float_complex fc;
                memcpy(&fc, buf + j * sizeof(H5_float_complex), sizeof(H5_float_complex));
                printf(" %29f%+fi\n", (double)crealf(fc), (double)cimagf(fc));
                break;
            }
            case DBL_COMPLEX: {
                H5_double_complex dc;
                memcpy(&dc, buf + j * sizeof(H5_double_complex), sizeof(H5_double_complex));
                printf(" %29f%+fi\n", creal(dc), cimag(dc));
                break;
            }
            case LDBL_COMPLEX: {
                H5_ldouble_complex ldc;
                memcpy(&ldc, buf + j * sizeof(H5_ldouble_complex), sizeof(H5_ldouble_complex));
                printf(" %29Lf%+Lfi\n", creall(ldc), cimagl(ldc));
                break;
            }
#else
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
                assert(0 && "Should not reach this point!");
#endif
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        printf("        and = ");
        if (FLT_COMPLEX == dst_type || DBL_COMPLEX == dst_type || LDBL_COMPLEX == dst_type) {
            unsigned char *hw_tmp_ptr = hw;
            size_t         part_size  = dst_size / 2;
            for (k = 0; k < part_size; k++)
                printf(" %02x", hw_tmp_ptr[ENDIAN(part_size, k, dendian)]);
            hw_tmp_ptr += part_size;
            for (k = 0; k < part_size; k++)
                printf(" %02x", hw_tmp_ptr[ENDIAN(part_size, k, dendian)]);
        }
        else {
            for (k = 0; k < dst_size; k++)
                printf(" %02x", hw[ENDIAN(dst_size, k, dendian)]);
        }
        printf("%*s", (int)(3 * MAX(0, (ssize_t)src_size - (ssize_t)dst_size)), "");
        switch (dst_type) {
            case INT_SCHAR:
                printf(" %29d\n", (int)*((signed char *)((void *)hw)));
                break;
            case INT_UCHAR:
                printf(" %29u\n", (unsigned)*((unsigned char *)((void *)hw)));
                break;
            case INT_SHORT:
                printf(" %29hd\n", *((short *)((void *)hw)));
                break;
            case INT_USHORT:
                printf(" %29hu\n", *((unsigned short *)((void *)hw)));
                break;
            case INT_INT:
                printf(" %29d\n", *((int *)((void *)hw)));
                break;
            case INT_UINT:
                printf(" %29u\n", *((unsigned int *)((void *)hw)));
                break;
            case INT_LONG:
                printf(" %29ld\n", *((long *)((void *)hw)));
                break;
            case INT_ULONG:
                printf(" %29lu\n", *((unsigned long *)((void *)hw)));
                break;
            case INT_LLONG:
                fprintf(stdout, " %29lld\n", *((long long *)((void *)hw)));
                break;
            case INT_ULLONG:
                fprintf(stdout, " %29llu\n", *((unsigned long long *)((void *)hw)));
                break;
            case FLT_FLOAT:
                printf(" %29f\n", (double)*((float *)((void *)hw)));
                break;
            case FLT_DOUBLE:
                printf(" %29f\n", *((double *)((void *)hw)));
                break;
            case FLT_LDOUBLE:
                printf(" %29Lf\n", *((long double *)((void *)hw)));
                break;
            case FLT_FLOAT16:
#ifdef H5_HAVE__FLOAT16
                printf(" %29f\n", (double)*((H5__Float16 *)((void *)hw)));
                break;
#else
                assert(0 && "Should not reach this point!");
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
            case FLT_COMPLEX:
                printf(" %29f%+fi\n", (double)crealf(*((H5_float_complex *)((void *)hw))),
                       (double)cimagf(*((H5_float_complex *)((void *)hw))));
                break;
            case DBL_COMPLEX:
                printf(" %29f%+fi\n", creal(*((H5_double_complex *)((void *)hw))),
                       cimag(*((H5_double_complex *)((void *)hw))));
                break;
            case LDBL_COMPLEX:
                printf(" %29Lf%+Lfi\n", creall(*((H5_ldouble_complex *)((void *)hw))),
                       cimagl(*((H5_ldouble_complex *)((void *)hw))));
                break;
#else
            case FLT_COMPLEX:
            case DBL_COMPLEX:
            case LDBL_COMPLEX:
                assert(0 && "Should not reach this point!");
#endif
            case OTHER:
            default:
                assert(0 && "Unknown type");
                break;
        }

        /* If the source is normalized values, print out error message; if it is
         * denormalized or special values, print out warning message.*/
        if (++fails_all_tests >= max_fails) {
            if (run_test == TEST_NORMAL)
                puts("    maximum failures reached, aborting test...");
            else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
                puts("    maximum warnings reached, aborting test...");
            puts("    (dst is library's conversion output. ans is compiler's conversion output.)");

            goto done;
        }
    }

    if (!fails_all_tests)
        PASSED();

done:
    if (buf) {
        aligned_free(buf);
        buf = NULL;
    }
    if (saved) {
        aligned_free(saved);
        saved = NULL;
    }
    fflush(stdout);
    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    /* If the source is normalized floating values, treat the failures as error;
     * if it is denormalized or special floating values, treat the failure as warning.*/
    if (run_test == TEST_NORMAL)
        return (int)fails_all_tests;
    else if (run_test == TEST_DENORM || run_test == TEST_SPECIAL)
        return 0;

error:
    if (buf)
        aligned_free(buf);
    if (saved)
        aligned_free(saved);
    fflush(stdout);

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5(); /*print statistics*/

    if (run_test == TEST_NORMAL)
        return MAX((int)fails_all_tests, 1);
    else {
        assert(run_test == TEST_DENORM || run_test == TEST_SPECIAL);
        return 1;
    }
}

/*-------------------------------------------------------------------------
 * Function:    overflows
 *
 * Purpose:    When convert from float or double to any integer type,
 *              check if overflow occurs.
 *
 *
 * Return:    true:           overflow happens
 *
 *              false:          no overflow
 *
 *-------------------------------------------------------------------------
 */
static bool
overflows(unsigned char *origin_bits, hid_t src_id, size_t dst_num_bits)
{
    bool          ret_value = false;
    hsize_t       expt;
    size_t        mant_digits = 0, expt_digits = 0, bias = 0;
    size_t        epos, mpos;
    size_t        src_prec = 0; /*source type precision in bits*/
    H5T_norm_t    norm;
    ssize_t       indx;
    unsigned char bits[32], mant_bits[32];

    memset(bits, 0, (size_t)32);
    memset(mant_bits, 0, (size_t)32);

    /*
     * Sometimes, type size isn't equal to the precision like Linux's "long
     * double", where size is 96 bits and precision is 80 bits.
     */

    src_prec = H5Tget_precision(src_id);
    H5Tget_fields(src_id, NULL, &epos, &expt_digits, &mpos, &mant_digits);
    bias = H5Tget_ebias(src_id);
    norm = H5Tget_norm(src_id);

    memcpy(bits, origin_bits, src_prec / 8 + 1);

    /*Check for special cases: +Inf, -Inf*/
    if (H5T__bit_find(bits, mpos, mant_digits, H5T_BIT_LSB, true) < 0) {
        if (H5T__bit_find(bits, epos, expt_digits, H5T_BIT_LSB, false) < 0) {
            ret_value = true;
            goto done;
        }
    }
    else if (H5T_NORM_NONE == norm && H5T__bit_find(bits, mpos, mant_digits - 1, H5T_BIT_LSB, true) < 0 &&
             H5T__bit_find(bits, epos, expt_digits, H5T_BIT_LSB, false) < 0) {
        /*This is a special case for the source of no implied mantissa bit.
         *If the exponent bits are all 1s and only the 1st bit of mantissa
         *is set to 1.  It's infinity. The Intel-Linux "long double" is this case.*/
        ret_value = true;
        goto done;
    }

    /* get exponent */
    expt = H5T__bit_get_d(bits, mant_digits, expt_digits) - bias;

    if (expt >= (dst_num_bits - 1)) {
        ret_value = true;
        goto done;
    }

    /* get significand */
    H5T__bit_copy(mant_bits, (size_t)0, bits, (size_t)0, mant_digits);

    /* restore implicit bit if normalization is implied*/
    if (norm == H5T_NORM_IMPLIED) {
        H5T__bit_inc(mant_bits, mant_digits, (size_t)1);
        mant_digits++;
    }

    /* shift significand */
    H5T__bit_shift(mant_bits, (ssize_t)(expt - expt_digits), (size_t)0, (size_t)(32 * 8));

    indx = H5T__bit_find(mant_bits, (size_t)0, (size_t)(32 * 8), H5T_BIT_MSB, 1);

    if ((size_t)indx >= dst_num_bits)
        ret_value = true;

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    run_integer_tests
 *
 * Purpose:    Runs all integer tests.
 *
 * Return:    Number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
run_integer_tests(const char *name)
{
    int nerrors = 0;

    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UCHAR, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_SHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_USHORT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_INT, H5T_NATIVE_ULLONG);
#endif

    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_INT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULONG);
#endif
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_UINT, H5T_NATIVE_ULLONG);
#endif

#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULONG);
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_UINT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LONG);
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_LLONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULONG, H5T_NATIVE_ULLONG);
#endif
#endif

#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_LLONG, H5T_NATIVE_ULLONG);
#endif

#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UCHAR);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_SHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_USHORT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_INT);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_UINT);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LONG);
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_ULONG);
#endif
    nerrors += test_conv_int_1(name, H5T_NATIVE_ULLONG, H5T_NATIVE_LLONG);
#endif

    return nerrors;
}

/*-------------------------------------------------------------------------
 * Function:    run_fp_tests
 *
 * Purpose:    Runs all floating-point tests.
 *
 * Return:    Number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
run_fp_tests(const char *name)
{
    int nerrors = 0;

    if (!strcmp(name, "noop")) {
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT);
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE);
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LDOUBLE);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
        nerrors += test_conv_flt_1("noop", TEST_NOOP, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif
        goto done;
    }

    /*Test normalized values.  TEST_NORMAL indicates normalized values.*/
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT16);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s normalized %s -> %s conversions", name, "long double",
                 "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE */
#endif /* H5_HAVE__FLOAT16 */
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s normalized %s -> %s conversions", name, "long double _Complex",
                 "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_HAVE__FLOAT16 */
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_NORMAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
#endif /* H5_HAVE_COMPLEX_NUMBERS */

    /*Test denormalized values.  TEST_DENORM indicates denormalized values.*/
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
#else
    {
        char str[256]; /*string        */

        snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name, "long double",
                 "float");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */

    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#endif /* H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE */
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT16);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name, "long double",
                 "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE */
#endif /* H5_HAVE__FLOAT16 */
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name,
                 "long double _Complex", "float");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name,
                 "long double _Complex", "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_HAVE__FLOAT16 */
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s denormalized %s -> %s conversions", name,
                 "long double _Complex", "float _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
    nerrors += test_conv_flt_1(name, TEST_DENORM, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
#endif /* H5_HAVE_COMPLEX_NUMBERS */

    /*Test special values, +/-0, +/-infinity, +/-QNaN, +/-SNaN.*/
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE);
#else
    {
        char str[256]; /*string        */

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                 "float or double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
#endif /* H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE */
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT16);
#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                 "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE */
#endif /* H5_HAVE__FLOAT16 */
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "float");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */

#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_FLOAT_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                 "float _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif

#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_DOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                 "double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif

#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LDOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                 "long double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif

    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);

#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "long double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif

#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT16, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_FLOAT16);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#ifdef H5_LDOUBLE_TO_FLOAT16_CORRECT
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT16);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "_Float16");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LDOUBLE_TO_FLOAT16_CORRECT */
#endif /* H5_HAVE__FLOAT16 */
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LDOUBLE_COMPLEX);
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_FLOAT_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "float _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
    nerrors += test_conv_flt_1(name, TEST_SPECIAL, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_DOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double _Complex",
                 "double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
    }
#endif /* H5_DISABLE_SOME_LDOUBLE_CONV */
#endif /* H5_HAVE_COMPLEX_NUMBERS */

done:
    return nerrors;
}

/*-------------------------------------------------------------------------
 * Function:    run_int_fp_conv
 *
 * Purpose:    Runs all integer-float tests.
 *
 * Return:    Number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
run_int_fp_conv(const char *name)
{
    int nerrors = 0;

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_DOUBLE_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_LDOUBLE_COMPLEX);
#endif

#if H5_SIZEOF_LONG != H5_SIZEOF_INT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_DOUBLE_COMPLEX);
#if !defined(H5_LONG_TO_LDOUBLE_SPECIAL) && !defined(H5_DISABLE_SOME_LDOUBLE_CONV)
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_LDOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long", "long double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the special algorithm of hardware conversion.");
    }
#endif
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_DOUBLE_COMPLEX);
#if !defined(H5_LONG_TO_LDOUBLE_SPECIAL) && !defined(H5_DISABLE_SOME_LDOUBLE_CONV)
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_LDOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "unsigned long",
                 "long double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the special algorithm of hardware conversion.");
    }
#endif
#endif
#endif

#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_DOUBLE_COMPLEX);
#if H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_LDOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long long",
                 "long double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif
#endif

    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_DOUBLE);
#ifdef H5_HAVE__FLOAT16
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT16);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_FLOAT_COMPLEX);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_DOUBLE_COMPLEX);
#if H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_LDOUBLE_COMPLEX);
#else
    {
        char str[256];

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "unsigned long long",
                 "long double _Complex");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif
#endif
#endif

#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SCHAR, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UCHAR, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_SHORT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_USHORT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_INT, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_UINT, H5T_NATIVE_LDOUBLE);
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
#if !defined(H5_LONG_TO_LDOUBLE_SPECIAL) && !defined(H5_DISABLE_SOME_LDOUBLE_CONV)
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LONG, H5T_NATIVE_LDOUBLE);
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULONG, H5T_NATIVE_LDOUBLE);
#else
    {
        char str[256]; /*string        */

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "(unsigned) long", "long double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to the special algorithm of hardware conversion.");
    }
#endif
#endif /* H5_SIZEOF_LONG!=H5_SIZEOF_INT */
#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
#if H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_LLONG, H5T_NATIVE_LDOUBLE);
#else  /* H5_LLONG_TO_LDOUBLE_CORRECT */
    {
        char str[256]; /*hello string        */

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long long", "long double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler error in handling conversion.");
    }
#endif /* H5_LLONG_TO_LDOUBLE_CORRECT */
#if H5_LLONG_TO_LDOUBLE_CORRECT
    nerrors += test_conv_int_fp(name, TEST_NORMAL, H5T_NATIVE_ULLONG, H5T_NATIVE_LDOUBLE);
#else  /* H5_LLONG_TO_LDOUBLE_CORRECT */
    {
        char str[256]; /*hello string        */

        snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "unsigned long long",
                 "long double");
        printf("%-70s", str);
        SKIPPED();
        puts("    Test skipped due to compiler not handling conversion.");
    }
#endif /* H5_LLONG_TO_LDOUBLE_CORRECT */
#endif
#endif

    return nerrors;
}

/*-------------------------------------------------------------------------
 * Function:    run_fp_int_conv
 *
 * Purpose:    Runs all float-integer tests.
 *
 * Return:    Number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
run_fp_int_conv(const char *name)
{
    int nerrors = 0;
    int test_values;

    for (test_values = TEST_NORMAL; test_values <= TEST_SPECIAL; test_values++) {

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_SCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_SCHAR);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_SCHAR);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_SCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_SCHAR);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_SCHAR);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "signed char");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_UCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_UCHAR);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_UCHAR);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_UCHAR);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_UCHAR);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_UCHAR);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "unsigned char");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_SHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_SHORT);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_SHORT);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_SHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_SHORT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_SHORT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "short");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_USHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_USHORT);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_USHORT);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_USHORT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_USHORT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_USHORT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "unsigned short");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_INT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_INT);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_INT);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_INT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_INT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_INT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "int");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_UINT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_UINT);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_UINT);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_UINT);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_UINT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_UINT);
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        else {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name,
                     "long double _Complex", "unsigned int");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
        }
#endif
#endif

#if H5_SIZEOF_LONG != H5_SIZEOF_INT
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LONG);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_LONG);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LONG);
#ifndef H5_LDOUBLE_TO_LONG_SPECIAL
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL && test_values != TEST_NORMAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LONG);
#else
        {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double _Complex",
                     "long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the special algorithm of hardware conversion.");
        }
#endif
#endif

        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_ULONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_ULONG);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_ULONG);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_ULONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_ULONG);
#ifndef H5_LDOUBLE_TO_LONG_SPECIAL
#ifdef H5_DISABLE_SOME_LDOUBLE_CONV
        if (test_values != TEST_SPECIAL && test_values != TEST_NORMAL)
#endif
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_ULONG);
#else
        {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double _Complex",
                     "unsigned long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the special algorithm of hardware conversion.");
        }
#endif
#endif
#endif

#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
        if (!strcmp(name, "hw")) { /* Hardware conversion */
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LLONG);
#ifdef H5_HAVE__FLOAT16
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_LLONG);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LLONG);
#ifdef H5_LDOUBLE_TO_LLONG_ACCURATE
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LLONG);
#else
            {
                char str[256];

                snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double _Complex",
                         "long long");
                printf("%-70s", str);
                SKIPPED();
                puts("    Test skipped due to hardware conversion error.");
            }
#endif
#endif
        }
        else { /* Software conversion */
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_LLONG);
#ifdef H5_HAVE__FLOAT16
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_LLONG);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_LLONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_LLONG);
#ifdef H5_LDOUBLE_TO_LLONG_ACCURATE
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_LLONG);
#else
            {
                char str[256];

                snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double _Complex",
                         "long long");
                printf("%-70s", str);
                SKIPPED();
                puts("    Test skipped due to hardware conversion error.");
            }
#endif
#endif
        }
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT, H5T_NATIVE_ULLONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE, H5T_NATIVE_ULLONG);
#ifdef H5_HAVE__FLOAT16
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT16, H5T_NATIVE_ULLONG);
#endif
#ifdef H5_HAVE_COMPLEX_NUMBERS
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_FLOAT_COMPLEX, H5T_NATIVE_ULLONG);
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_DOUBLE_COMPLEX, H5T_NATIVE_ULLONG);
#ifdef H5_LDOUBLE_TO_LLONG_ACCURATE
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE_COMPLEX, H5T_NATIVE_ULLONG);
#else
        {
            char str[256];

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double _Complex",
                     "unsigned long long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to hardware conversion error.");
        }
#endif
#endif
#endif

#if H5_SIZEOF_LONG_DOUBLE != H5_SIZEOF_DOUBLE
        if (test_values != TEST_SPECIAL) {
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SCHAR);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UCHAR);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SHORT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_USHORT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_INT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UINT);
        }
        else {
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SCHAR);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UCHAR);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_SHORT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_USHORT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_INT);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_UINT);
#else
            char str[256]; /*string        */

            snprintf(str, sizeof(str), "Testing %s special %s -> %s conversions", name, "long double",
                     "signed and unsigned char, short, int, long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the conversion problem on IBM ppc64le cpu.");
#endif
        }
#if H5_SIZEOF_LONG != H5_SIZEOF_INT
#ifndef H5_LDOUBLE_TO_LONG_SPECIAL
        if (test_values != TEST_SPECIAL && test_values != TEST_NORMAL) {
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_ULONG);
        }
        else {
#ifndef H5_DISABLE_SOME_LDOUBLE_CONV
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LONG);
            nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_ULONG);
#endif
        }
#else
        {
            char str[256]; /*string        */

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double",
                     "(unsigned) long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to the special algorithm of hardware conversion.");
        }
#endif
#endif /*H5_SIZEOF_LONG!=H5_SIZEOF_INT */

#if H5_SIZEOF_LONG_LONG != H5_SIZEOF_LONG
#ifdef H5_LDOUBLE_TO_LLONG_ACCURATE
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_LLONG);
#else  /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
        {
            char str[256]; /*string        */

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double", "long long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to hardware conversion error.");
        }
#endif /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
#if defined(H5_LDOUBLE_TO_LLONG_ACCURATE)
        nerrors += test_conv_int_fp(name, test_values, H5T_NATIVE_LDOUBLE, H5T_NATIVE_ULLONG);
#else  /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
        {
            char str[256]; /*string        */

            snprintf(str, sizeof(str), "Testing %s %s -> %s conversions", name, "long double",
                     "unsigned long long");
            printf("%-70s", str);
            SKIPPED();
            puts("    Test skipped due to hardware conversion error.");
        }
#endif /*H5_LDOUBLE_TO_LLONG_ACCURATE*/
#endif
#endif
    } /* end for */

    return nerrors;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the data type(integer and floating-point number).
 *
 * Return:      Success:
 *
 *              Failure:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned long nerrors = 0;

    /* Set the random # seed */
    srand((unsigned)time(NULL));

    reset_hdf5();

    if (ALIGNMENT)
        printf("Testing non-aligned conversions (ALIGNMENT=%d)....\n", ALIGNMENT);

    /* Do the tests */

    /* Test H5Tcompiler_conv() for querying hard conversion. */
    nerrors += (unsigned long)test_hard_query();

    /* Test user-defined, query functions and software conversion
     * for user-defined floating-point types */
    nerrors += (unsigned long)test_derived_flt();

    /* Test user-defined, query functions and software conversion
     * for user-defined integer types */
    nerrors += (unsigned long)test_derived_integer();

    /* Test user-defined, query functions and software conversion
     * for user-defined complex number types */
    nerrors += (unsigned long)test_derived_complex();

    /* Test bfloat16 special values */
    nerrors += (unsigned long)test_bfloat16();

    /* Test fp8 special values */
    nerrors += (unsigned long)test_fp8();

    /* Test degenerate cases */
    nerrors += (unsigned long)run_fp_tests("noop");

    /* Test hardware floating-point conversion functions */
    nerrors += (unsigned long)run_fp_tests("hard");

    /* Test hardware integer conversion functions */
    nerrors += (unsigned long)run_integer_tests("hard");

    /* Test hardware integer-float conversion functions */
    nerrors += (unsigned long)run_int_fp_conv("hard");

    /* Test hardware float-integer conversion functions */
    nerrors += (unsigned long)run_fp_int_conv("hard");

    /* Test a few special values for hardware float-integer conversions */
    nerrors += (unsigned long)test_particular_fp_integer();

    /*----------------------------------------------------------------------
     * Software tests
     *----------------------------------------------------------------------
     */
    without_hardware_g = true;

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();

    /* Test software floating-point conversion functions */
    nerrors += (unsigned long)run_fp_tests("soft");

    /* Test software integer conversion functions */
    nerrors += (unsigned long)test_conv_int_2();
    nerrors += (unsigned long)run_integer_tests("soft");

    /* Test software float-integer conversion functions */
    nerrors += (unsigned long)run_fp_int_conv("soft");

    /* Test software integer-float conversion functions */
    nerrors += (unsigned long)run_int_fp_conv("soft");

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    reset_hdf5();

    /* Restore the default error handler (set in h5_test_init()) */
    h5_restore_err();

    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n", nerrors, 1 == nerrors ? "" : "S");
        exit(EXIT_FAILURE);
    }
    printf("All data type tests passed.\n");
    return 0;
}
