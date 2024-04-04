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
 * Purpose:     Tests the datatype interface (H5T)
 */

#include "testhdf5.h"
#include "H5srcdir.h"
#include "H5Iprivate.h" /* For checking that datatype id's don't leak */

/* Number of elements in each test */
#define NTESTELEM 100000

/* For test_compound_8 and test_compound_10 */
#define ARRAY_DIM 4

/*
 * Offset from aligned memory returned by malloc().  This can be used to test
 * that type conversions handle non-aligned buffers correctly.
 */
#define ALIGNMENT 1

/*
 * Define if you want to test alignment code on a machine that doesn't
 * normally require alignment. When set, all native datatypes must be aligned
 * on a byte boundary equal to the data size.
 */
#define TEST_ALIGNMENT

/* Alignment test stuff */
#ifdef TEST_ALIGNMENT
#define H5T_FRIEND /*suppress error about including H5Tpkg      */
#include "H5Tpkg.h"
#endif

/* Use in version bound test */
#define H5F_FRIEND /*suppress error about including H5Fpkg */
#define H5F_TESTING
#include "H5Fpkg.h" /* File access                          */

/* Use in version bound test */
#define H5O_FRIEND  /*suppress error about including H5Opkg */
#include "H5Opkg.h" /* Object headers                       */

#define SET_ALIGNMENT(TYPE, VAL) H5T_NATIVE_##TYPE##_ALIGN_g = MAX(H5T_NATIVE_##TYPE##_ALIGN_g, VAL)

/*
 * Macro for checking that the correct number of datatype ids are present.  Be
 * careful as the call to H5Tunregister removes *ALL* compound conversions from
 * the soft conversion list.  One must call reset_hdf5() after this.
 */
#define CHECK_NMEMBS(NMEMBS, SRC_ID, DST_ID)                                                                 \
    do {                                                                                                     \
        if (H5Tunregister(H5T_PERS_SOFT, NULL, SRC_ID, DST_ID, NULL) < 0)                                    \
            FAIL_STACK_ERROR;                                                                                \
        if (H5Tclose(SRC_ID) < 0 || ((SRC_ID) != (DST_ID) && H5Tclose(DST_ID) < 0))                          \
            FAIL_STACK_ERROR;                                                                                \
        if ((NMEMBS) != H5I_nmembers(H5I_DATATYPE)) {                                                        \
            H5_FAILED();                                                                                     \
            printf("    #dtype ids expected: %lld; found: %lld\n", (long long)(NMEMBS),                      \
                   (long long)H5I_nmembers(H5I_DATATYPE));                                                   \
            goto error;                                                                                      \
        }                                                                                                    \
    } while (0)

static const char *FILENAME[] = {"dtypes0",  "dtypes1",  "dtypes2",  "dtypes3", "dtypes4",
                                 "dtypes5",  "dtypes6",  "dtypes7",  "dtypes8", "dtypes9",
                                 "dtypes10", "dtypes11", "dtypes12", NULL};

#define TESTFILE "bad_compound.h5"

typedef struct complex_t {
    double re;
    double im;
} complex_t;

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
    FLT_FLOAT,
    FLT_DOUBLE,
    FLT_LDOUBLE,
    OTHER
} dtype_t; /* This doesn't seem to be used anywhere... -BMR */

typedef struct src_cmpd_t {
    uint32_t a;
    float    b;
} src_cmpd_t;

typedef struct dst_cmpd_t {
    float b;
} dst_cmpd_t;

typedef enum { E1_RED, E1_GREEN, E1_BLUE, E1_ORANGE, E1_YELLOW } color_t;

/* Constant for size of conversion buffer for int <-> float exception test */
#define CONVERT_SIZE 4

/* Constants for compound_13 test */
#define COMPOUND13_ARRAY_SIZE 256
#define COMPOUND13_ATTR_NAME  "attr"

/* Constants for delete_obj_named test */
#define DEL_OBJ_NAMED_DATASET     "/Dataset"
#define DEL_OBJ_NAMED_NAMED_DTYPE "/Dtype"
#define DEL_OBJ_NAMED_ATTRIBUTE   "Attr"

/* Constant for testing conversion of UTF-8 characters */
#define UTF8_DATASET   "utf8"
#define UTF8_DATASET2  "2nd_utf8"
#define ASCII_DATASET  "ascii"
#define ASCII_DATASET2 "2nd_ascii"

/* Count opaque conversions */
static int num_opaque_conversions_g = 0;

static int    opaque_check(int tag_it);
static herr_t convert_opaque(hid_t st, hid_t dt, H5T_cdata_t *cdata, size_t nelmts, size_t buf_stride,
                             size_t bkg_stride, void *_buf, void *bkg, hid_t dset_xfer_plid);
static int    opaque_long(void);
static int    opaque_funcs(void);

/*-------------------------------------------------------------------------
 * Function:    reset_hdf5
 *
 * Purpose:     Reset the hdf5 library.  This causes statistics to be printed
 *              and counters to be reset.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
reset_hdf5(void)
{
    h5_reset();
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
#endif
}

/* Conversion function to test user compound conversion functions */
static herr_t
user_compound_convert(hid_t H5_ATTR_UNUSED src_id, hid_t H5_ATTR_UNUSED dst_id, H5T_cdata_t *cdata,
                      size_t nelmts, size_t H5_ATTR_UNUSED buf_stride, size_t H5_ATTR_UNUSED bkg_stride,
                      void *buf, void H5_ATTR_UNUSED *bkg, hid_t H5_ATTR_UNUSED dxpl)
{
    herr_t retval = EXIT_SUCCESS;
    switch (cdata->command) {
        case H5T_CONV_INIT:
            break;
        case H5T_CONV_CONV:
            for (size_t i = 0; i < nelmts; ++i)
                ((dst_cmpd_t *)buf)[i].b = ((src_cmpd_t *)buf)[i].b;
            break;
        case H5T_CONV_FREE:
            break;
        default:
            break;
    }
    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    test_classes
 *
 * Purpose:     Test type classes
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_classes(void)
{
    struct complex { /* Struct with complex fields */
        hvl_t vl_c;
        hvl_t vl_s;
    };
    hid_t       cmpd_id; /* Compound datatype */
    hid_t       vlc_id;  /* VL type of char   */
    hid_t       vls_id;  /* VL string         */
    hid_t       memb_id; /* Compound member datatype */
    H5T_class_t memb_cls;
    H5T_class_t tcls;
    int         nmembs;
    unsigned    u;

    TESTING("H5Tget_class()");

    /*-------------------------------------------------------------
     *  Check class of some atomic types.
     *-----------------------------------------------------------*/
    if ((tcls = H5Tget_class(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if (H5T_INTEGER != tcls)
        TEST_ERROR;

    if ((tcls = H5Tget_class(H5T_NATIVE_DOUBLE)) < 0)
        TEST_ERROR;
    if (H5T_FLOAT != tcls)
        TEST_ERROR;

    /* Create a VL datatype of char.  It should be a VL, not a string class. */
    if ((vlc_id = H5Tvlen_create(H5T_NATIVE_CHAR)) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if ((tcls = H5Tget_class(vlc_id)) < 0)
        TEST_ERROR;
    if (H5T_VLEN != tcls)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5T_STRING == tcls)
        TEST_ERROR;

    /* Create a VL string.  It should be a string, not a VL class. */
    if ((vls_id = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(vls_id, H5T_VARIABLE) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if ((tcls = H5Tget_class(vls_id)) < 0)
        TEST_ERROR;
    if (H5T_STRING != tcls)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5T_VLEN == tcls)
        TEST_ERROR;

    /*-------------------------------------------------------------
     *  Check class for member types of compound type.
     *-----------------------------------------------------------*/
    /* Create a compound datatype and insert some complex types */
    if ((cmpd_id = H5Tcreate(H5T_COMPOUND, sizeof(struct complex))) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_id, "vl_c", HOFFSET(struct complex, vl_c), vlc_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_id, "vl_s", HOFFSET(struct complex, vl_s), vls_id) < 0)
        TEST_ERROR;

    if ((nmembs = H5Tget_nmembers(cmpd_id)) < 0)
        TEST_ERROR;

    for (u = 0; u < (unsigned)nmembs; u++) {
        /* Get member type ID */
        if ((memb_id = H5Tget_member_type(cmpd_id, u)) < 0)
            TEST_ERROR;

        /* Get member type class */
        if ((memb_cls = H5Tget_member_class(cmpd_id, u)) < 0)
            TEST_ERROR;

        /* Verify member class */
        if (H5Tdetect_class(memb_id, memb_cls) < 0)
            TEST_ERROR;

        /* Close member type ID */
        if (H5Tclose(memb_id) < 0)
            TEST_ERROR;
    } /* end for */

    /* Close datatypes */
    if (H5Tclose(cmpd_id) < 0)
        TEST_ERROR;
    if (H5Tclose(vlc_id) < 0)
        TEST_ERROR;
    if (H5Tclose(vls_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_copy
 *
 * Purpose:     Are we able to copy a datatype?
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_copy(void)
{
    hid_t  a_copy;
    herr_t status;

    TESTING("H5Tcopy()");

    if ((a_copy = H5Tcopy(H5T_NATIVE_SHORT)) < 0)
        goto error;
    if (H5Tclose(a_copy) < 0)
        goto error;

    /* We should not be able to close a built-in byte */
    H5E_BEGIN_TRY
    {
        status = H5Tclose(H5T_NATIVE_SCHAR);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Should not be able to close a predefined type!");
        goto error;
    }

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_detect
 *
 * Purpose:     Are we able to detect datatype classes correctly?  (Especially
 *              in nested types)
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_detect(void)
{
    struct atomic { /* Struct with atomic fields */
        int    i;
        float  f;
        char   c;
        double d;
        short  s;
    };
    struct complex { /* Struct with complex fields */
        hobj_ref_t arr_r[3][3];
        int        i;
        hvl_t      vl_f;
        hvl_t      vl_s;
        char       c;
        short      s;
    };
    hid_t    atom_cmpd_id;     /* Atomic Compound datatype */
    hid_t    atom_arr_id;      /* Atomic Array datatype */
    hid_t    atom_vlf_id;      /* Atomic VL datatype of float */
    hid_t    atom_vlc_id;      /* Atomic VL datatype of char */
    hid_t    atom_vls_id;      /* Atomic VL string datatype */
    hid_t    cplx_cmpd_id;     /* Complex Compound datatype */
    unsigned rank    = 2;      /* Rank for array datatype */
    hsize_t  dims[2] = {3, 3}; /* Dimensions for array datatype */

    TESTING("H5Tdetect_class()");

    /*--------------------------------------------------------------------------------
     *  Test class of some atomic types.
     *------------------------------------------------------------------------------*/
    /* Native integers should be in the integer class */
    if (H5Tdetect_class(H5T_NATIVE_INT, H5T_INTEGER) != true)
        TEST_ERROR;

    /* Native integers should _not_ be in other classes */
    if (H5Tdetect_class(H5T_NATIVE_INT, H5T_FLOAT) != false)
        TEST_ERROR;
    if (H5Tdetect_class(H5T_NATIVE_INT, H5T_ARRAY) != false)
        TEST_ERROR;
    if (H5Tdetect_class(H5T_NATIVE_INT, H5T_ENUM) != false)
        TEST_ERROR;

    /*--------------------------------------------------------------------------------
     *  Test class of a compound type with some atomic types as fields.
     *------------------------------------------------------------------------------*/
    /* Create a compound datatype and insert some atomic types */
    if ((atom_cmpd_id = H5Tcreate(H5T_COMPOUND, sizeof(struct atomic))) < 0)
        TEST_ERROR;
    if (H5Tinsert(atom_cmpd_id, "i", HOFFSET(struct atomic, i), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(atom_cmpd_id, "f", HOFFSET(struct atomic, f), H5T_NATIVE_FLOAT) < 0)
        TEST_ERROR;
    if (H5Tinsert(atom_cmpd_id, "c", HOFFSET(struct atomic, c), H5T_NATIVE_CHAR) < 0)
        TEST_ERROR;
    if (H5Tinsert(atom_cmpd_id, "d", HOFFSET(struct atomic, d), H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;
    if (H5Tinsert(atom_cmpd_id, "s", HOFFSET(struct atomic, s), H5T_NATIVE_SHORT) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(atom_cmpd_id, H5T_COMPOUND) != true)
        TEST_ERROR;
    if (H5Tdetect_class(atom_cmpd_id, H5T_INTEGER) != true)
        TEST_ERROR;
    if (H5Tdetect_class(atom_cmpd_id, H5T_FLOAT) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(atom_cmpd_id, H5T_VLEN) != false)
        TEST_ERROR;

    /*--------------------------------------------------------------------------------
     *  Test class of some complex types.
     *------------------------------------------------------------------------------*/
    /* Create an array datatype with an atomic base type */
    if ((atom_arr_id = H5Tarray_create2(H5T_STD_REF_OBJ, rank, dims)) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(atom_arr_id, H5T_ARRAY) != true)
        TEST_ERROR;
    if (H5Tdetect_class(atom_arr_id, H5T_REFERENCE) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(atom_arr_id, H5T_VLEN) != false)
        TEST_ERROR;
    if (H5Tdetect_class(atom_arr_id, H5T_FLOAT) != false)
        TEST_ERROR;
    if (H5Tdetect_class(atom_arr_id, H5T_INTEGER) != false)
        TEST_ERROR;

    /* Create a VL datatype with an atomic base type of float*/
    if ((atom_vlf_id = H5Tvlen_create(H5T_NATIVE_FLOAT)) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(atom_vlf_id, H5T_VLEN) != true)
        TEST_ERROR;
    if (H5Tdetect_class(atom_vlf_id, H5T_FLOAT) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(atom_vlf_id, H5T_COMPOUND) != false)
        TEST_ERROR;
    if (H5Tdetect_class(atom_vlf_id, H5T_INTEGER) != false)
        TEST_ERROR;

    /* Create a VL datatype with an atomic base type of char.  It should be a VL
     * but not a string class. */
    if ((atom_vlc_id = H5Tvlen_create(H5T_NATIVE_CHAR)) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(atom_vlc_id, H5T_VLEN) != true)
        TEST_ERROR;
    if (H5Tdetect_class(atom_vlc_id, H5T_INTEGER) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(atom_vlc_id, H5T_STRING) != false)
        TEST_ERROR;

    /* Create a VL string.  It should be a string, not a VL class. */
    if ((atom_vls_id = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(atom_vls_id, H5T_VARIABLE) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(atom_vls_id, H5T_STRING) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(atom_vls_id, H5T_VLEN) != false)
        TEST_ERROR;

    /*--------------------------------------------------------------------------------
     *  Test class of a compound type with some complex types as fields.
     *------------------------------------------------------------------------------*/
    /* Create a compound datatype and insert some complex types */
    if ((cplx_cmpd_id = H5Tcreate(H5T_COMPOUND, sizeof(struct complex))) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "arr_r", HOFFSET(struct complex, arr_r), atom_arr_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "i", HOFFSET(struct complex, i), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "vl_f", HOFFSET(struct complex, vl_f), atom_vlf_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "vl_s", HOFFSET(struct complex, vl_s), atom_vls_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "c", HOFFSET(struct complex, c), H5T_NATIVE_CHAR) < 0)
        TEST_ERROR;
    if (H5Tinsert(cplx_cmpd_id, "s", HOFFSET(struct complex, s), H5T_NATIVE_SHORT) < 0)
        TEST_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(cplx_cmpd_id, H5T_COMPOUND) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_ARRAY) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_REFERENCE) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_INTEGER) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_FLOAT) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_STRING) != true)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_VLEN) != true)
        TEST_ERROR;

    /* Make certain that an incorrect class is not detected */
    if (H5Tdetect_class(cplx_cmpd_id, H5T_TIME) != false)
        TEST_ERROR;
    if (H5Tdetect_class(cplx_cmpd_id, H5T_ENUM) != false)
        TEST_ERROR;

    /* Close complex compound datatype */
    if (H5Tclose(cplx_cmpd_id) < 0)
        TEST_ERROR;

    /* Close atomic VL datatype of float */
    if (H5Tclose(atom_vlf_id) < 0)
        TEST_ERROR;

    /* Close atomic VL datatype of char */
    if (H5Tclose(atom_vlc_id) < 0)
        TEST_ERROR;

    /* Close atomic VL string datatype  */
    if (H5Tclose(atom_vls_id) < 0)
        TEST_ERROR;

    /* Close atomic array datatype */
    if (H5Tclose(atom_arr_id) < 0)
        TEST_ERROR;

    /* Close atomic compound datatype */
    if (H5Tclose(atom_cmpd_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_1
 *
 * Purpose:     Tests various things about compound datatypes.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_1(void)
{
    hid_t       complex_id;
    hid_t       super;
    size_t      size;
    H5T_pad_t   lsb, msb;
    H5T_cset_t  cset;
    H5T_str_t   strpad;
    H5T_order_t order;
    H5T_sign_t  sign;
    char       *tag = NULL;
    int         offset;
    herr_t      ret;
    int         retval = 1;

    TESTING("compound datatypes");

    /* Create the empty type */
    if ((complex_id = H5Tcreate(H5T_COMPOUND, sizeof(complex_t))) < 0)
        goto error;

    /* Try to shrink and expand the size */
    if (H5Tset_size(complex_id, sizeof(double)) < 0)
        goto error;
    if (H5Tset_size(complex_id, sizeof(complex_t)) < 0)
        goto error;

    /* Attempt to add the new compound datatype as a field within itself */
    H5E_BEGIN_TRY
    {
        ret = H5Tinsert(complex_id, "compound", (size_t)0, complex_id);
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("Inserted compound datatype into itself?");
    } /* end if */

    /* Add a couple fields */
    if (H5Tinsert(complex_id, "real", HOFFSET(complex_t, re), H5T_NATIVE_DOUBLE) < 0)
        goto error;
    if (H5Tinsert(complex_id, "imaginary", HOFFSET(complex_t, im), H5T_NATIVE_DOUBLE) < 0)
        goto error;

    /* Test some functions that aren't supposed to work for compound type */
    /* Tries to shrink the size and trail the last member */
    H5E_BEGIN_TRY
    {
        ret = H5Tset_size(complex_id, sizeof(double));
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        size = H5Tget_precision(complex_id);
    }
    H5E_END_TRY
    if (size > 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    size = 128;
    H5E_BEGIN_TRY
    {
        ret = H5Tset_precision(complex_id, size);
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        ret = H5Tget_pad(complex_id, &lsb, &msb);
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        size = H5Tget_ebias(complex_id);
    }
    H5E_END_TRY
    if (size > 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        lsb = H5Tget_inpad(complex_id);
    }
    H5E_END_TRY
    if (lsb >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        cset = H5Tget_cset(complex_id);
    }
    H5E_END_TRY
    if (cset > -1) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        strpad = H5Tget_strpad(complex_id);
    }
    H5E_END_TRY
    if (strpad > -1) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        offset = H5Tget_offset(complex_id);
    }
    H5E_END_TRY
    if (offset >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    /* We started to support this function for compound type in 1.8.6 release. */
    if ((order = H5Tget_order(complex_id)) == H5T_ORDER_ERROR)
        FAIL_PUTS_ERROR("Can't get order for compound type.");
    if (order != H5T_ORDER_LE && order != H5T_ORDER_BE)
        FAIL_PUTS_ERROR("Wrong order for this type.");

    H5E_BEGIN_TRY
    {
        sign = H5Tget_sign(complex_id);
    }
    H5E_END_TRY
    if (sign > -1) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        tag = H5Tget_tag(complex_id);
    }
    H5E_END_TRY
    if (tag) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        super = H5Tget_super(complex_id);
    }
    H5E_END_TRY
    if (super >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    if (H5Tclose(complex_id) < 0)
        goto error;

    PASSED();
    retval = 0;

error:
    if (tag)
        H5free_memory(tag);
    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_2
 *
 * Purpose:     Tests a compound type conversion where the source and
 *              destination are the same except for the order of the
 *              elements.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_2(void)
{
    struct st {
        int a, b, c[4], d, e;
    } * s_ptr;
    struct dt {
        int e, d, c[4], b, a;
    } * d_ptr;

    const size_t   nelmts = NTESTELEM;
    const hsize_t  four   = 4;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    hid_t          st = H5I_INVALID_HID, dt = H5I_INVALID_HID;
    hid_t          array_dt;
    int64_t        nmembs;
    int            i;

    TESTING("compound element reordering");

    if ((nmembs = H5I_nmembers(H5I_DATATYPE)) < 0)
        FAIL_STACK_ERROR;

    /* Sizes should be the same, but be careful just in case */
    if (NULL == (buf = (unsigned char *)malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)))))
        goto error;
    if (NULL == (bkg = (unsigned char *)malloc(nelmts * sizeof(struct dt))))
        goto error;
    if (NULL == (orig = (unsigned char *)malloc(nelmts * sizeof(struct st))))
        goto error;
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr       = ((struct st *)((void *)orig)) + i;
        s_ptr->a    = i * 8 + 0;
        s_ptr->b    = i * 8 + 1;
        s_ptr->c[0] = i * 8 + 2;
        s_ptr->c[1] = i * 8 + 3;
        s_ptr->c[2] = i * 8 + 4;
        s_ptr->c[3] = i * 8 + 5;
        s_ptr->d    = i * 8 + 6;
        s_ptr->e    = i * 8 + 7;
    }
    memcpy(buf, orig, nelmts * sizeof(struct st));

    /* Build hdf5 datatypes */
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((st = H5Tcreate(H5T_COMPOUND, sizeof(struct st))) < 0 ||
        H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "c", HOFFSET(struct st, c), array_dt) < 0 ||
        H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((dt = H5Tcreate(H5T_COMPOUND, sizeof(struct dt))) < 0 ||
        H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(dt, "b", HOFFSET(struct dt, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(dt, "c", HOFFSET(struct dt, c), array_dt) < 0 ||
        H5Tinsert(dt, "d", HOFFSET(struct dt, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT) < 0)
        goto error;

    /* Compare results */
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr = ((struct st *)((void *)orig)) + i;
        d_ptr = ((struct dt *)((void *)buf)) + i;
        if (s_ptr->a != d_ptr->a || s_ptr->b != d_ptr->b || s_ptr->c[0] != d_ptr->c[0] ||
            s_ptr->c[1] != d_ptr->c[1] || s_ptr->c[2] != d_ptr->c[2] || s_ptr->c[3] != d_ptr->c[3] ||
            s_ptr->d != d_ptr->d || s_ptr->e != d_ptr->e) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n", s_ptr->a, s_ptr->b, s_ptr->c[0],
                   s_ptr->c[1], s_ptr->c[2], s_ptr->c[3], s_ptr->d, s_ptr->e);
            printf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n", d_ptr->a, d_ptr->b, d_ptr->c[0],
                   d_ptr->c[1], d_ptr->c[2], d_ptr->c[3], d_ptr->d, d_ptr->e);
            goto error;
        }
    }

    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    CHECK_NMEMBS(nmembs, st, dt);

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 0;

error:
    free(buf);
    free(bkg);
    free(orig);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_3
 *
 * Purpose:     Tests compound conversions where the source and destination
 *              are the same except the destination is missing a couple
 *              members which appear in the source.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_3(void)
{
    struct st {
        int a, b, c[4], d, e;
    } * s_ptr;
    struct dt {
        int a, c[4], e;
    } * d_ptr;

    const size_t   nelmts = NTESTELEM;
    const hsize_t  four   = 4;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    hid_t          st = H5I_INVALID_HID, dt = H5I_INVALID_HID;
    hid_t          array_dt;
    int64_t        nmembs;
    int            i;

    TESTING("compound subset conversions");

    if ((nmembs = H5I_nmembers(H5I_DATATYPE)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize */
    if (NULL == (buf = (unsigned char *)malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)))))
        goto error;
    if (NULL == (bkg = (unsigned char *)malloc(nelmts * sizeof(struct dt))))
        goto error;
    if (NULL == (orig = (unsigned char *)malloc(nelmts * sizeof(struct st))))
        goto error;
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr       = ((struct st *)((void *)orig)) + i;
        s_ptr->a    = i * 8 + 0;
        s_ptr->b    = i * 8 + 1;
        s_ptr->c[0] = i * 8 + 2;
        s_ptr->c[1] = i * 8 + 3;
        s_ptr->c[2] = i * 8 + 4;
        s_ptr->c[3] = i * 8 + 5;
        s_ptr->d    = i * 8 + 6;
        s_ptr->e    = i * 8 + 7;
    }
    memcpy(buf, orig, nelmts * sizeof(struct st));

    /* Build hdf5 datatypes */
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((st = H5Tcreate(H5T_COMPOUND, sizeof(struct st))) < 0 ||
        H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "c", HOFFSET(struct st, c), array_dt) < 0 ||
        H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((dt = H5Tcreate(H5T_COMPOUND, sizeof(struct dt))) < 0 ||
        H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(dt, "c", HOFFSET(struct dt, c), array_dt) < 0 ||
        H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT) < 0)
        goto error;

    /* Compare results */
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr = ((struct st *)((void *)orig)) + i;
        d_ptr = ((struct dt *)((void *)buf)) + i;
        if (s_ptr->a != d_ptr->a || s_ptr->c[0] != d_ptr->c[0] || s_ptr->c[1] != d_ptr->c[1] ||
            s_ptr->c[2] != d_ptr->c[2] || s_ptr->c[3] != d_ptr->c[3] || s_ptr->e != d_ptr->e) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n", s_ptr->a, s_ptr->b, s_ptr->c[0],
                   s_ptr->c[1], s_ptr->c[2], s_ptr->c[3], s_ptr->d, s_ptr->e);
            printf("    dst={a=%d, c=[%d,%d,%d,%d], e=%d\n", d_ptr->a, d_ptr->c[0], d_ptr->c[1], d_ptr->c[2],
                   d_ptr->c[3], d_ptr->e);
            goto error;
        }
    }

    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    CHECK_NMEMBS(nmembs, st, dt);

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    free(buf);
    free(bkg);
    free(orig);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_4
 *
 * Purpose:     Tests compound conversions when the destination has the same
 *              fields as the source but one or more of the fields are
 *              smaller.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_4(void)
{

    struct st {
        int a, b, c[4], d, e;
    } * s_ptr;
    struct dt {
        short b;
        int   a, c[4];
        short d;
        int   e;
    } * d_ptr;

    const size_t   nelmts = NTESTELEM;
    const hsize_t  four   = 4;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    hid_t          st = H5I_INVALID_HID, dt = H5I_INVALID_HID;
    hid_t          array_dt;
    int64_t        nmembs;
    int            i;

    TESTING("compound element shrinking & reordering");

    if ((nmembs = H5I_nmembers(H5I_DATATYPE)) < 0)
        FAIL_STACK_ERROR;

    /* Sizes should be the same, but be careful just in case */
    if (NULL == (buf = (unsigned char *)malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)))))
        goto error;
    if (NULL == (bkg = (unsigned char *)malloc(nelmts * sizeof(struct dt))))
        goto error;
    if (NULL == (orig = (unsigned char *)malloc(nelmts * sizeof(struct st))))
        goto error;
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr       = ((struct st *)((void *)orig)) + i;
        s_ptr->a    = i * 8 + 0;
        s_ptr->b    = (i * 8 + 1) & 0x7fff;
        s_ptr->c[0] = i * 8 + 2;
        s_ptr->c[1] = i * 8 + 3;
        s_ptr->c[2] = i * 8 + 4;
        s_ptr->c[3] = i * 8 + 5;
        s_ptr->d    = (i * 8 + 6) & 0x7fff;
        s_ptr->e    = i * 8 + 7;
    }
    memcpy(buf, orig, nelmts * sizeof(struct st));

    /* Build hdf5 datatypes */
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((st = H5Tcreate(H5T_COMPOUND, sizeof(struct st))) < 0 ||
        H5Tinsert(st, "a", HOFFSET(struct st, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "c", HOFFSET(struct st, c), array_dt) < 0 ||
        H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(st, "e", HOFFSET(struct st, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if ((dt = H5Tcreate(H5T_COMPOUND, sizeof(struct dt))) < 0 ||
        H5Tinsert(dt, "a", HOFFSET(struct dt, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(dt, "b", HOFFSET(struct dt, b), H5T_NATIVE_SHORT) < 0 ||
        H5Tinsert(dt, "c", HOFFSET(struct dt, c), array_dt) < 0 ||
        H5Tinsert(dt, "d", HOFFSET(struct dt, d), H5T_NATIVE_SHORT) < 0 ||
        H5Tinsert(dt, "e", HOFFSET(struct dt, e), H5T_NATIVE_INT) < 0)
        goto error;
    H5Tclose(array_dt);

    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT) < 0)
        goto error;

    /* Compare results */
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr = ((struct st *)((void *)orig)) + i;
        d_ptr = ((struct dt *)((void *)buf)) + i;
        if (s_ptr->a != d_ptr->a || s_ptr->b != d_ptr->b || s_ptr->c[0] != d_ptr->c[0] ||
            s_ptr->c[1] != d_ptr->c[1] || s_ptr->c[2] != d_ptr->c[2] || s_ptr->c[3] != d_ptr->c[3] ||
            s_ptr->d != d_ptr->d || s_ptr->e != d_ptr->e) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n", s_ptr->a, s_ptr->b, s_ptr->c[0],
                   s_ptr->c[1], s_ptr->c[2], s_ptr->c[3], s_ptr->d, s_ptr->e);
            printf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n", d_ptr->a, d_ptr->b, d_ptr->c[0],
                   d_ptr->c[1], d_ptr->c[2], d_ptr->c[3], d_ptr->d, d_ptr->e);
            goto error;
        }
    }

    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    CHECK_NMEMBS(nmembs, st, dt);

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    free(buf);
    free(bkg);
    free(orig);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_5
 *
 * Purpose:     Many versions of HDF5 have a bug in the optimized compound
 *              datatype conversion function, H5T_conv_struct_opt(), which
 *              is triggered when the top-level type contains a struct
 *              which must undergo a conversion.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_5(void)
{
    typedef struct {
        char  name[16];
        short tdim;
        short coll_ids[4];
    } src_type_t;

    typedef struct {
        char  name[16];
        short tdim;
        int   coll_ids[4];
    } dst_type_t;

    hsize_t    dims[1] = {4};
    hid_t      src_type, dst_type, short_array, int_array, string;
    hid_t      array_dt;
    src_type_t src[2] = {{"one", 102, {104, 105, 106, 107}}, {"two", 202, {204, 205, 206, 207}}};

    dst_type_t *dst;
    void       *buf    = calloc((size_t)2, sizeof(dst_type_t));
    void       *bkg    = calloc((size_t)2, sizeof(dst_type_t));
    int         retval = 1;

    TESTING("optimized struct converter");

    if (!buf || !bkg) {
        free(buf);
        free(bkg);
        return 1;
    }

    /* Build datatypes */
    short_array = H5Tcreate(H5T_COMPOUND, 4 * sizeof(short));
    array_dt    = H5Tarray_create2(H5T_NATIVE_SHORT, 1, dims);
    H5Tinsert(short_array, "_", (size_t)0, array_dt);
    H5Tclose(array_dt);

    int_array = H5Tcreate(H5T_COMPOUND, 4 * sizeof(int));
    array_dt  = H5Tarray_create2(H5T_NATIVE_INT, 1, dims);
    H5Tinsert(int_array, "_", (size_t)0, array_dt);
    H5Tclose(array_dt);

    string = H5Tcopy(H5T_C_S1);
    H5Tset_size(string, (size_t)16);

    src_type = H5Tcreate(H5T_COMPOUND, sizeof(src_type_t));
    H5Tinsert(src_type, "name", HOFFSET(src_type_t, name), string);
    H5Tinsert(src_type, "tdim", HOFFSET(src_type_t, tdim), H5T_NATIVE_SHORT);
    H5Tinsert(src_type, "coll_ids", HOFFSET(src_type_t, coll_ids), short_array);

    dst_type = H5Tcreate(H5T_COMPOUND, sizeof(dst_type_t));
    H5Tinsert(dst_type, "name", HOFFSET(dst_type_t, name), string);
    H5Tinsert(dst_type, "tdim", HOFFSET(dst_type_t, tdim), H5T_NATIVE_SHORT);
    H5Tinsert(dst_type, "coll_ids", HOFFSET(dst_type_t, coll_ids), int_array);

    /* Convert data */
    memcpy(buf, src, sizeof(src));
    H5Tconvert(src_type, dst_type, (size_t)2, buf, bkg, H5P_DEFAULT);
    dst = (dst_type_t *)buf;

    /* Cleanup */
    H5Tclose(src_type);
    H5Tclose(dst_type);
    H5Tclose(string);
    H5Tclose(short_array);
    H5Tclose(int_array);

    /* Check results */
    if (memcmp(src[1].name, dst[1].name, sizeof(src[1].name)) != 0 || src[1].tdim != dst[1].tdim ||
        src[1].coll_ids[0] != dst[1].coll_ids[0] || src[1].coll_ids[1] != dst[1].coll_ids[1] ||
        src[1].coll_ids[2] != dst[1].coll_ids[2] || src[1].coll_ids[3] != dst[1].coll_ids[3]) {
        H5_FAILED();
    }
    else {
        PASSED();
        retval = 0;
    }

    /* Free memory buffers */
    free(buf);
    free(bkg);
    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_6
 *
 * Purpose:     Tests compound conversions when the destination has the same
 *              fields as the source but one or more of the fields are
 *              larger.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_6(void)
{

    struct st {
        short b;
        short d;
    } * s_ptr;
    struct dt {
        long b;
        long d;
    } * d_ptr;

    const size_t   nelmts = NTESTELEM;
    unsigned char *buf = NULL, *orig = NULL, *bkg = NULL;
    hid_t          st = H5I_INVALID_HID, dt = H5I_INVALID_HID;
    int64_t        nmembs;
    int            i;

    TESTING("compound element growing");

    if ((nmembs = H5I_nmembers(H5I_DATATYPE)) < 0)
        FAIL_STACK_ERROR;

    /* Sizes should be the same, but be careful just in case */
    if (NULL == (buf = (unsigned char *)malloc(nelmts * MAX(sizeof(struct st), sizeof(struct dt)))))
        goto error;
    if (NULL == (bkg = (unsigned char *)malloc(nelmts * sizeof(struct dt))))
        goto error;
    if (NULL == (orig = (unsigned char *)malloc(nelmts * sizeof(struct st))))
        goto error;
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr    = ((struct st *)((void *)orig)) + i;
        s_ptr->b = (int16_t)((i * 8 + 1) & 0x7fff);
        s_ptr->d = (int16_t)((i * 8 + 6) & 0x7fff);
    }
    memcpy(buf, orig, nelmts * sizeof(struct st));

    /* Build hdf5 datatypes */
    if ((st = H5Tcreate(H5T_COMPOUND, sizeof(struct st))) < 0 ||
        H5Tinsert(st, "b", HOFFSET(struct st, b), H5T_NATIVE_SHORT) < 0 ||
        H5Tinsert(st, "d", HOFFSET(struct st, d), H5T_NATIVE_SHORT) < 0) {
        H5_FAILED();
        goto error;
    }

    if ((dt = H5Tcreate(H5T_COMPOUND, sizeof(struct dt))) < 0 ||
        H5Tinsert(dt, "b", HOFFSET(struct dt, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(dt, "d", HOFFSET(struct dt, d), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        goto error;
    }

    /* Perform the conversion */
    if (H5Tconvert(st, dt, nelmts, buf, bkg, H5P_DEFAULT) < 0) {
        H5_FAILED();
        goto error;
    }

    /* Compare results */
    for (i = 0; i < (int)nelmts; i++) {
        s_ptr = ((struct st *)((void *)orig)) + i;
        d_ptr = ((struct dt *)((void *)buf)) + i;
        if (s_ptr->b != d_ptr->b || s_ptr->d != d_ptr->d) {
            H5_FAILED();
            printf("    i=%d\n", i);
            printf("    src={b=%d, d=%d\n", (int)s_ptr->b, (int)s_ptr->d);
            printf("    dst={b=%ld, d=%ld\n", d_ptr->b, d_ptr->d);
            goto error;
        }
    }

    /* Release resources */
    free(buf);
    free(bkg);
    free(orig);
    CHECK_NMEMBS(nmembs, st, dt);

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_7
 *
 * Purpose:     Tests inserting fields into compound datatypes when the field
 *              overlaps the end of the compound datatype.  Also, tests
 *              increasing compound type size.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_7(void)
{
    struct s1 {
        int   a;
        float b;
        long  c;
    };

    struct s2 {
        int    a;
        float  b;
        long   c;
        double d;
    };

    hid_t  tid1, tid2;
    herr_t ret;

    TESTING("compound element insertion");

    if ((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct s1))) < 0) {
        H5_FAILED();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid1, "a", HOFFSET(struct s1, a), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'a'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid1, "b", HOFFSET(struct s1, b), H5T_NATIVE_FLOAT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'b'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid1, "c", HOFFSET(struct s1, c), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        printf("Can't insert field 'c'\n");
        goto error;
    } /* end if */

    if (H5Tget_size(tid1) != sizeof(struct s1)) {
        H5_FAILED();
        printf("Incorrect size for struct 1\n");
        goto error;
    } /* end if */

    if ((tid2 = H5Tcopy(tid1)) < 0) {
        H5_FAILED();
        printf("Can't copy datatype\n");
        goto error;
    } /* end if */

    if (H5Tget_size(tid2) == sizeof(struct s2)) {
        H5_FAILED();
        printf("Incorrect size for struct 2\n");
        goto error;
    } /* end if */

    /* Should not be able to insert field past end of compound datatype */
    H5E_BEGIN_TRY
    {
        ret = H5Tinsert(tid2, "d", HOFFSET(struct s2, d), H5T_NATIVE_DOUBLE);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5_FAILED();
        printf("Inserted field 'd'?\n");
        goto error;
    } /* end if */

    /* Should not be able to shrink size of compound datatype */
    H5E_BEGIN_TRY
    {
        ret = H5Tset_size(tid2, sizeof(struct s1) / 2);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5_FAILED();
        printf("Shrunk compound type?\n");
        goto error;
    } /* end if */

    /* Increase compound type size and try inserting field again */
    if (H5Tset_size(tid2, sizeof(struct s2)) < 0) {
        H5_FAILED();
        printf("Can't increase size for compound type\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid2, "d", HOFFSET(struct s2, d), H5T_NATIVE_DOUBLE) < 0) {
        H5_FAILED();
        printf("Can't expand compound datatype\n");
        goto error;
    } /* end if */

    if (H5Tget_size(tid2) != sizeof(struct s2)) {
        H5_FAILED();
        printf("Incorrect size for struct 2\n");
        goto error;
    } /* end if */

    /* Release resources */
    if (H5Tclose(tid1) < 0 || H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatypes\n");
        goto error;
    } /* end if */

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_8
 *
 * Purpose:     Tests H5Tpack for compound datatypes.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_8(void)
{
    typedef struct s1 {
        char a;
        int  b;
    } s1;

    struct s2 {
        char c;
        s1   d;
    };
    hid_t   tid1, tid1_copy, tid2, tid2_copy, tid3, arr_tid;
    size_t  tsize;
    hsize_t dims[1] = {ARRAY_DIM};
    herr_t  ret;

    TESTING("packing compound datatypes");

    /*------------------------------------------------------------
     *    Test H5Tpack for compound type
     */
    /* Create first compound datatype */
    if ((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct s1))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid1, "a", HOFFSET(struct s1, a), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'a'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid1, "b", HOFFSET(struct s1, b), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'b'\n");
        goto error;
    } /* end if */

    /* Make a copy of the type for later use */
    if ((tid1_copy = H5Tcopy(tid1)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't copy type #1\n");
        goto error;
    } /* end if */

    /* Test H5Tpack for the first compound type */
    if (H5Tpack(tid1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't pack the compound datatype\n");
        goto error;
    } /* end if */

    if (H5Tlock(tid1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't lock the compound datatype\n");
        goto error;
    } /* end if */

    /* If the type is already packed, packing a locked type is OK */
    if (H5Tpack(tid1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't pack the compound datatype for second time\n");
        goto error;
    } /* end if */

    /* Verify the size of packed compound type */
    if ((tsize = H5Tget_size(tid1)) == 0) {
        H5_FAILED();
        AT();
        printf("Can't get size of the compound datatype\n");
        goto error;
    } /* end if */

    if (tsize != (sizeof(char) + sizeof(int))) {
        H5_FAILED();
        AT();
        printf("The size of the packed compound datatype is incorrect\n");
        goto error;
    } /* end if */

    /*------------------------------------------------------------
     *    Test H5Tpack for nested compound type
     */
    /* Create second compound datatype */
    if ((tid2 = H5Tcreate(H5T_COMPOUND, sizeof(struct s2))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(tid2, "c", HOFFSET(struct s2, c), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c'\n");
        goto error;
    } /* end if */

    /* Insert the member of unpacked compound type */
    if (H5Tinsert(tid2, "d", HOFFSET(struct s2, d), tid1_copy) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'd'\n");
        goto error;
    } /* end if */

    /* Make a copy of the type for later */
    if ((tid3 = H5Tcopy(tid2)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't copy type #2\n");
        goto error;
    } /* end if */

    /* Make a copy of the type for later */
    if ((tid2_copy = H5Tcopy(tid2)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't copy type #2\n");
        goto error;
    } /* end if */

    /* Test H5Tpack for the second compound type */
    if (H5Tpack(tid2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't pack the compound datatype\n");
        goto error;
    } /* end if */

    if (H5Tlock(tid2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't lock the compound datatype\n");
        goto error;
    } /* end if */

    /* If the type is already packed, packing a locked type is OK */
    if (H5Tpack(tid2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't pack the compound datatype for second time\n");
        goto error;
    } /* end if */

    /* Lock unpacked type */
    if (H5Tlock(tid3) < 0) {
        H5_FAILED();
        AT();
        printf("Can't lock the compound datatype\n");
        goto error;
    } /* end if */

    /* If the type is not packed, packing a locked type shouldn't work */
    H5E_BEGIN_TRY
    {
        ret = H5Tpack(tid3);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5_FAILED();
        AT();
        printf("Packing locked datatype worked?\n");
        goto error;
    } /* end if */

    /* Verify the size of packed compound type */
    if ((tsize = H5Tget_size(tid2)) == 0) {
        H5_FAILED();
        AT();
        printf("Can't get size of the compound datatype\n");
        goto error;
    } /* end if */

    if (tsize != (sizeof(char) + sizeof(char) + sizeof(int))) {
        H5_FAILED();
        AT();
        printf("The size of the packed compound datatype is incorrect: tsize = %zu\n", tsize);
        goto error;
    } /* end if */

    /*------------------------------------------------------------
     *    Test H5Tpack for array type of nested compound type
     */
    /* Create an array type of compound type */
    if ((arr_tid = H5Tarray_create2(tid2_copy, 1, dims)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create an array datatype\n");
        goto error;
    } /* end if */

    /* Test H5Tpack for the array type */
    if (H5Tpack(arr_tid) < 0) {
        H5_FAILED();
        AT();
        printf("Can't pack the array datatype\n");
        goto error;
    } /* end if */

    /* Verify the size of packed compound type */
    if ((tsize = H5Tget_size(arr_tid)) == 0) {
        H5_FAILED();
        AT();
        printf("Can't get size of the array datatype\n");
        goto error;
    } /* end if */

    if (tsize != ARRAY_DIM * (sizeof(char) + sizeof(char) + sizeof(int))) {
        H5_FAILED();
        AT();
        printf("The size of the packed array datatype is incorrect\n");
        goto error;
    } /* end if */

    if (H5Tclose(tid1_copy) < 0) {
        H5_FAILED();
        AT();
        printf("Can't close the compound datatype\n");
        goto error;
    } /* end if */

    if (H5Tclose(tid2_copy) < 0) {
        H5_FAILED();
        AT();
        printf("Can't close the compound datatype\n");
        goto error;
    } /* end if */

    if (H5Tclose(arr_tid) < 0) {
        H5_FAILED();
        AT();
        printf("Can't close the array datatype\n");
        goto error;
    } /* end if */

    /* Can't release resources - they are locked */

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_9
 *
 * Purpose:     Tests compound datatype with VL string as field.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_9(void)
{
    typedef struct cmpd_struct_w {
        int         i1;
        const char *str;
        int         i2;
    } cmpd_struct_w;

    typedef struct cmpd_struct_r {
        int   i1;
        char *str;
        int   i2;
    } cmpd_struct_r;

    cmpd_struct_w wdata = {11, "variable-length string", 22};
    cmpd_struct_r rdata;
    hid_t         file;
    hid_t         cmpd_tid, str_id, dup_tid;
    hid_t         space_id;
    hid_t         dset_id;
    hsize_t       dim1[1];
    char          filename[1024];

    TESTING("compound datatype with VL string");

    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create file!\n");
        goto error;
    } /* end if */

    /* Create first compound datatype */
    if ((cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct_w))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_tid, "i1", HOFFSET(struct cmpd_struct_w, i1), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i1'\n");
        goto error;
    } /* end if */

    str_id = H5Tcopy(H5T_C_S1);
    if (H5Tset_size(str_id, H5T_VARIABLE) < 0) {
        H5_FAILED();
        AT();
        printf("Can't set size for VL string\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_tid, "vl_string", HOFFSET(cmpd_struct_w, str), str_id) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_tid, "i2", HOFFSET(struct cmpd_struct_w, i2), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i2'\n");
        goto error;
    } /* end if */

    if (H5Tcommit2(file, "compound", cmpd_tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't commit datatype\n");
        goto error;
    } /* end if */

    if (H5Tclose(cmpd_tid) < 0) {
        H5_FAILED();
        AT();
        printf("Can't close datatype\n");
        goto error;
    } /* end if */

    if ((cmpd_tid = H5Topen2(file, "compound", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if ((dup_tid = H5Tcopy(cmpd_tid)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't copy datatype\n");
        goto error;
    } /* end if */

    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create space\n");
        goto error;
    } /* end if */

    if ((dset_id = H5Dcreate2(file, "Dataset", dup_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    } /* end if */

    if (H5Dwrite(dset_id, dup_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    if (H5Dread(dset_id, dup_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (rdata.i1 != wdata.i1 || rdata.i2 != wdata.i2 || strcmp(rdata.str, wdata.str) != 0) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (H5Treclaim(dup_tid, space_id, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim read data\n");
        goto error;
    } /* end if */
    rdata.str = NULL;

    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Tclose(cmpd_tid) < 0)
        goto error;
    if (H5Tclose(dup_tid) < 0)
        goto error;
    if (H5Tclose(str_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open file\n");
        goto error;
    } /* end if */

    if ((dset_id = H5Dopen2(file, "Dataset", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open dataset\n");
        goto error;
    } /* end if */

    if ((space_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't get space\n");
        goto error;
    } /* end if */

    if ((cmpd_tid = H5Dget_type(dset_id)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open dataset\n");
        goto error;
    } /* end if */

    if ((dup_tid = H5Tcopy(cmpd_tid)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't copy datatype\n");
        goto error;
    } /* end if */

    rdata.i1 = rdata.i2 = 0;
    if (rdata.str)
        free(rdata.str);

    if (H5Dread(dset_id, dup_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (rdata.i1 != wdata.i1 || rdata.i2 != wdata.i2 || strcmp(rdata.str, wdata.str) != 0) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (H5Treclaim(dup_tid, space_id, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */
    rdata.str = NULL;

    if (rdata.str)
        free(rdata.str);

    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Tclose(cmpd_tid) < 0)
        goto error;
    if (H5Tclose(dup_tid) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_10
 *
 * Purpose:     Tests array datatype of compound type with VL string as field.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_10(void)
{
    typedef struct cmpd_struct {
        int   i1;
        char *str;
        hvl_t text;
        int   i2;
    } cmpd_struct;

    cmpd_struct wdata[ARRAY_DIM];
    cmpd_struct rdata[ARRAY_DIM];
    hid_t       file       = H5I_INVALID_HID;
    hid_t       arr_tid    = H5I_INVALID_HID;
    hid_t       cmpd_tid   = H5I_INVALID_HID;
    hid_t       cstr_id    = H5I_INVALID_HID;
    hid_t       vlstr_id   = H5I_INVALID_HID;
    hid_t       space_id   = H5I_INVALID_HID;
    hid_t       dset_id    = H5I_INVALID_HID;
    hsize_t     arr_dim[1] = {ARRAY_DIM}; /* Array dimensions */
    hsize_t     dim1[1];
    void       *t1 = NULL;
    void       *t2 = NULL;
    char        filename[1024];
    size_t      len;
    int         i;

    TESTING("array datatype of compound type with VL string");

    memset(wdata, 0, sizeof(wdata));
    memset(rdata, 0, sizeof(rdata));

    /* Initialize */
    for (i = 0; i < ARRAY_DIM; i++) {
        wdata[i].i1 = i * 10 + i;
        if (NULL == (wdata[i].str = strdup("C string A")))
            FAIL_PUTS_ERROR("Unable to duplicate string");
        wdata[i].str[9] = (char)(wdata[i].str[9] + i);
        wdata[i].i2     = i * 1000 + i * 10;

        if (NULL == (wdata[i].text.p = (void *)strdup("variable-length text A\0")))
            FAIL_PUTS_ERROR("Unable to duplicate string");
        len = wdata[i].text.len              = strlen((char *)wdata[i].text.p) + 1;
        ((char *)(wdata[i].text.p))[len - 2] = (char)(((char *)(wdata[i].text.p))[len - 2] + i);
        ((char *)(wdata[i].text.p))[len - 1] = '\0';
    }

    /* Create File */
    h5_fixname(FILENAME[4], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create first compound datatype */
    if ((cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0)
        TEST_ERROR;

    if (H5Tinsert(cmpd_tid, "i1", HOFFSET(struct cmpd_struct, i1), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((cstr_id = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(cstr_id, H5T_VARIABLE) < 0)
        TEST_ERROR;

    if (H5Tinsert(cmpd_tid, "c_string", HOFFSET(cmpd_struct, str), cstr_id) < 0)
        TEST_ERROR;

    /* Create vl-string datatype */
    if ((vlstr_id = H5Tvlen_create(H5T_NATIVE_CHAR)) < 0)
        TEST_ERROR;

    if (H5Tinsert(cmpd_tid, "vl_string", HOFFSET(cmpd_struct, text), vlstr_id) < 0)
        TEST_ERROR;

    if (H5Tinsert(cmpd_tid, "i2", HOFFSET(struct cmpd_struct, i2), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Create the array datatype for c_string data */
    if ((arr_tid = H5Tarray_create2(cmpd_tid, 1, arr_dim)) < 0)
        TEST_ERROR;

    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(file, "Dataset", arr_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dwrite(dset_id, arr_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
        TEST_ERROR;

    if (H5Dread(dset_id, arr_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
        TEST_ERROR;

    for (i = 0; i < ARRAY_DIM; i++) {
        if (rdata[i].i1 != wdata[i].i1 || rdata[i].i2 != wdata[i].i2 ||
            strcmp(rdata[i].str, wdata[i].str) != 0)
            FAIL_PUTS_ERROR("incorrect read data\n");

        if (rdata[i].text.len != wdata[i].text.len)
            FAIL_PUTS_ERROR("incorrect VL length\n");

        t1 = rdata[i].text.p;
        t2 = wdata[i].text.p;
        if (strcmp((char *)t1, (char *)t2) != 0)
            FAIL_PUTS_ERROR("incorrect VL read data\n");
    }

    if (H5Treclaim(arr_tid, space_id, H5P_DEFAULT, &rdata) < 0)
        TEST_ERROR;
    if (H5Treclaim(arr_tid, space_id, H5P_DEFAULT, &wdata) < 0)
        TEST_ERROR;

    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Tclose(arr_tid) < 0)
        TEST_ERROR;
    arr_tid = H5I_INVALID_HID;
    if (H5Tclose(cmpd_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(cstr_id) < 0)
        TEST_ERROR;
    if (H5Tclose(vlstr_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    space_id = H5I_INVALID_HID;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:

    H5E_BEGIN_TRY
    {
        if (arr_tid != H5I_INVALID_HID && space_id != H5I_INVALID_HID) {
            H5Treclaim(arr_tid, space_id, H5P_DEFAULT, &rdata);
            H5Treclaim(arr_tid, space_id, H5P_DEFAULT, &wdata);
        }
        else {
            /* Clean up memory if we failed out early */
            for (i = 0; i < ARRAY_DIM; i++) {
                free(wdata[i].str);
                free(wdata[i].text.p);
            }
        }

        H5Dclose(dset_id);
        H5Tclose(arr_tid);
        H5Tclose(cmpd_tid);
        H5Tclose(cstr_id);
        H5Tclose(vlstr_id);
        H5Sclose(space_id);
        H5Fclose(file);
    }
    H5E_END_TRY

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_11
 *
 * Purpose:     Tests whether registering/unregistering a type conversion
 *              function correctly causes compound datatypes to recalculate
 *              their cached field conversion information
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_11(void)
{
    typedef struct {
        double d1;
        int    i1;
        char  *s1;
        int    i2;
        double d2;
        double d3;
    } big_t;

    typedef struct {
        double d1;
        int    i1;
        char  *s1;
    } little_t;

    hid_t   var_string_tid;             /* Datatype IDs for type conversion */
    hid_t   big_tid, little_tid;        /* Datatype IDs for type conversion */
    hid_t   big_tid2, little_tid2;      /* Datatype IDs for type conversion */
    hid_t   opaq_src_tid, opaq_dst_tid; /* Datatype IDs for type conversion */
    hid_t   space_id;                   /* Dataspace for buffer elements */
    hsize_t dim[1];                     /* Dimensions for dataspace */
    void   *buf      = NULL;            /* Conversion buffer */
    void   *buf_orig = NULL;            /* Copy of original conversion buffer */
    void   *bkg      = NULL;            /* Background buffer */
    size_t  u;                          /* Local index variable */
    int     retval = 1;

    TESTING("registering type conversion routine with compound conversions");

    /* Create variable string type for use in both structs */
    if ((var_string_tid = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(var_string_tid, H5T_VARIABLE) < 0)
        TEST_ERROR;

    /* Create type for 'big' struct */
    if ((big_tid = H5Tcreate(H5T_COMPOUND, sizeof(big_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(big_tid, "d1", HOFFSET(big_t, d1), H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;
    if (H5Tinsert(big_tid, "i1", HOFFSET(big_t, i1), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(big_tid, "s1", HOFFSET(big_t, s1), var_string_tid) < 0)
        TEST_ERROR;
    if (H5Tinsert(big_tid, "i2", HOFFSET(big_t, i2), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(big_tid, "d2", HOFFSET(big_t, d2), H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;
    if (H5Tinsert(big_tid, "d3", HOFFSET(big_t, d3), H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;

    /* Create type for 'little' struct (with "out of order" inserts) */
    if ((little_tid = H5Tcreate(H5T_COMPOUND, sizeof(little_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(little_tid, "d1", HOFFSET(little_t, d1), H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;
    if (H5Tinsert(little_tid, "s1", HOFFSET(little_t, s1), var_string_tid) < 0)
        TEST_ERROR;
    if (H5Tinsert(little_tid, "i1", HOFFSET(little_t, i1), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Allocate buffers */
    if ((buf = malloc(sizeof(big_t) * NTESTELEM)) == NULL)
        TEST_ERROR;
    if ((buf_orig = malloc(sizeof(big_t) * NTESTELEM)) == NULL)
        TEST_ERROR;
    if ((bkg = malloc(sizeof(big_t) * NTESTELEM)) == NULL)
        TEST_ERROR;

    /* Initialize buffer */
    for (u = 0; u < NTESTELEM; u++) {
        ((big_t *)buf)[u].d1 = (double)u * 1.5;
        ((big_t *)buf)[u].d2 = (double)u * 2.5;
        ((big_t *)buf)[u].d3 = (double)u * 3.5;
        ((big_t *)buf)[u].i1 = (int)(u * 3);
        ((big_t *)buf)[u].i2 = (int)(u * 5);
        ((big_t *)buf)[u].s1 = (char *)malloc((size_t)32);
        if (!((big_t *)buf)[u].s1)
            TEST_ERROR;
        snprintf(((big_t *)buf)[u].s1, 32, "%u", (unsigned)u);
    } /* end for */

    /* Make copy of buffer before conversion */
    memcpy(buf_orig, buf, sizeof(big_t) * NTESTELEM);

    dim[0] = NTESTELEM;
    if ((space_id = H5Screate_simple(1, dim, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create space\n");
        goto error;
    } /* end if */

    /* Make copies of the 'big' and 'little' datatypes, so the type
     * conversion routine doesn't use the same ones this time and next time
     */
    if ((big_tid2 = H5Tcopy(big_tid)) < 0)
        TEST_ERROR;
    if ((little_tid2 = H5Tcopy(little_tid)) < 0)
        TEST_ERROR;

    /* Convert buffer from 'big' to 'little' struct */
    if (H5Tconvert(big_tid2, little_tid2, (size_t)NTESTELEM, buf, bkg, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Verify converted buffer is correct */
    for (u = 0; u < NTESTELEM; u++) {
        if (!H5_DBL_ABS_EQUAL(((big_t *)buf_orig)[u].d1, ((little_t *)buf)[u].d1)) {
            printf("Error, line #%d: buf_orig[%u].d1=%f, buf[%u].d1=%f\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].d1, (unsigned)u, ((little_t *)buf)[u].d1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].i1 != ((little_t *)buf)[u].i1) {
            printf("Error, line #%d: buf_orig[%u].i1=%d, buf[%u].i1=%d\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].i1, (unsigned)u, ((little_t *)buf)[u].i1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].s1 == NULL || ((little_t *)buf)[u].s1 == NULL) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
        else if (strcmp(((big_t *)buf_orig)[u].s1, ((little_t *)buf)[u].s1) != 0) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
    }     /* end for */
    if (H5Treclaim(little_tid2, space_id, H5P_DEFAULT, buf) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim data\n");
        goto error;
    } /* end if */

    /* Build source and destination types for conversion routine */
    if ((opaq_src_tid = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        TEST_ERROR;
    if (H5Tset_tag(opaq_src_tid, "opaque source type") < 0)
        TEST_ERROR;
    if ((opaq_dst_tid = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        TEST_ERROR;
    if (H5Tset_tag(opaq_dst_tid, "opaque destination type") < 0)
        TEST_ERROR;

    /* Register new type conversion routine */
    if (H5Tregister(H5T_PERS_HARD, "opaq_test", opaq_src_tid, opaq_dst_tid, convert_opaque) < 0)
        TEST_ERROR;

    /* Recover the original buffer information */
    memcpy(buf, buf_orig, sizeof(big_t) * NTESTELEM);

    /* Convert buffer from 'big' to 'little' struct */
    if (H5Tconvert(big_tid, little_tid, (size_t)NTESTELEM, buf, bkg, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Verify converted buffer is correct */
    for (u = 0; u < NTESTELEM; u++) {
        if (!H5_DBL_ABS_EQUAL(((big_t *)buf_orig)[u].d1, ((little_t *)buf)[u].d1)) {
            printf("Error, line #%d: buf_orig[%u].d1=%f, buf[%u].d1=%f\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].d1, (unsigned)u, ((little_t *)buf)[u].d1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].i1 != ((little_t *)buf)[u].i1) {
            printf("Error, line #%d: buf_orig[%u].i1=%d, buf[%u].i1=%d\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].i1, (unsigned)u, ((little_t *)buf)[u].i1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].s1 == NULL || ((little_t *)buf)[u].s1 == NULL) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
        else if (strcmp(((big_t *)buf_orig)[u].s1, ((little_t *)buf)[u].s1) != 0) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
    }     /* end for */
    if (H5Treclaim(little_tid, space_id, H5P_DEFAULT, buf) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim data\n");
        goto error;
    } /* end if */

    /* Unregister the conversion routine */
    if (H5Tunregister(H5T_PERS_HARD, "opaq_test", opaq_src_tid, opaq_dst_tid, convert_opaque) < 0)
        TEST_ERROR;

    /* Recover the original buffer information */
    memcpy(buf, buf_orig, sizeof(big_t) * NTESTELEM);

    /* Convert buffer from 'big' to 'little' struct */
    if (H5Tconvert(big_tid, little_tid, (size_t)NTESTELEM, buf, bkg, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Verify converted buffer is correct */
    for (u = 0; u < NTESTELEM; u++) {
        if (!H5_DBL_ABS_EQUAL(((big_t *)buf_orig)[u].d1, ((little_t *)buf)[u].d1)) {
            printf("Error, line #%d: buf_orig[%u].d1=%f, buf[%u].d1=%f\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].d1, (unsigned)u, ((little_t *)buf)[u].d1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].i1 != ((little_t *)buf)[u].i1) {
            printf("Error, line #%d: buf_orig[%u].i1=%d, buf[%u].i1=%d\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].i1, (unsigned)u, ((little_t *)buf)[u].i1);
            TEST_ERROR;
        } /* end if */
        if (((big_t *)buf_orig)[u].s1 == NULL || ((little_t *)buf)[u].s1 == NULL) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
        else if (strcmp(((big_t *)buf_orig)[u].s1, ((little_t *)buf)[u].s1) != 0) {
            printf("Error, line #%d: buf_orig[%u].s1=%s, buf[%u].s1=%s\n", __LINE__, (unsigned)u,
                   ((big_t *)buf_orig)[u].s1, (unsigned)u, ((little_t *)buf)[u].s1);
            TEST_ERROR;
        } /* end if */
    }     /* end for */
    if (H5Treclaim(little_tid, space_id, H5P_DEFAULT, buf) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim data\n");
        goto error;
    } /* end if */

    /* Free everything */
    for (u = 0; u < NTESTELEM; u++)
        free(((big_t *)buf_orig)[u].s1);
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(opaq_dst_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(opaq_src_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(little_tid2) < 0)
        TEST_ERROR;
    if (H5Tclose(big_tid2) < 0)
        TEST_ERROR;
    if (H5Tclose(little_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(big_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(var_string_tid) < 0)
        TEST_ERROR;

    PASSED();
    retval = 0;

error:
    if (buf)
        free(buf);
    if (buf_orig)
        free(buf_orig);
    if (bkg)
        free(bkg);
    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_12
 *
 * Purpose:     Tests size adjustment of compound datatypes.  Start with
 *              no member and 0 size, increase the size as inserting
 *              members.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_12(void)
{
    hid_t  complex_id;
    size_t size = 0;
    size_t offset, new_size, tmp_size;
    herr_t ret;

    TESTING("adjust size of compound datatypes");

    /* Create a compound type of minimal size */
    if ((complex_id = H5Tcreate(H5T_COMPOUND, (size_t)1)) < 0)
        goto error;

    /* Verify the size */
    if ((new_size = H5Tget_size(complex_id)) == 0)
        goto error;
    if (new_size != 1)
        goto error;

    /* Add a couple fields and adjust the size */
    offset = size;
    if ((tmp_size = H5Tget_size(H5T_NATIVE_DOUBLE)) == 0)
        goto error;
    size += tmp_size;
    if (H5Tset_size(complex_id, size) < 0)
        goto error;
    if (H5Tinsert(complex_id, "real", offset, H5T_NATIVE_DOUBLE) < 0)
        goto error;

    offset = size;
    if ((tmp_size = H5Tget_size(H5T_NATIVE_DOUBLE)) == 0)
        goto error;
    size += tmp_size;
    if (H5Tset_size(complex_id, size) < 0)
        goto error;
    if (H5Tinsert(complex_id, "imaginary", offset, H5T_NATIVE_DOUBLE) < 0)
        goto error;

    /* Increase and decrease the size. */
    if ((tmp_size = H5Tget_size(H5T_NATIVE_DOUBLE)) == 0)
        goto error;
    size += tmp_size;
    if (H5Tset_size(complex_id, size) < 0)
        goto error;

    if ((tmp_size = H5Tget_size(H5T_NATIVE_DOUBLE)) == 0)
        goto error;
    size -= tmp_size;
    if (H5Tset_size(complex_id, size) < 0)
        goto error;

    /* Verify the size */
    if ((new_size = H5Tget_size(complex_id)) == 0)
        goto error;
    if (new_size != size)
        goto error;

    /* Tries to cut last member.  Supposed to fail. */
    size--;
    H5E_BEGIN_TRY
    {
        ret = H5Tset_size(complex_id, size);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5_FAILED();
        puts("  Tries to cut off the last member. Should have failed.");
        goto error;
    }

    if (H5Tclose(complex_id) < 0)
        goto error;
    PASSED();
    return 0;

error:
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_compound_13
 *
 * Purpose:     Tests compound datatypes whose size is at the boundary for
 *              needing 2 bytes for the datatype size and "use the latest
 *              format" flag is enabled so that the size of the offsets uses
 *              the smallest # of bytes possible.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_13(void)
{
    struct s1 {
        unsigned char x[COMPOUND13_ARRAY_SIZE + 1];
        float         y;
    };
    struct s1 data_out, data_in;
    hid_t     fileid, grpid, dtypeid, array1_tid, spaceid, attid;
    hid_t     fapl_id;
    hsize_t   dims[1] = {COMPOUND13_ARRAY_SIZE + 1};
    char      filename[1024];
    unsigned  u;

    TESTING("compound datatypes of boundary size with latest format");

    /* Create some phony data. */
    memset(&data_out, 0, sizeof(data_out));
    for (u = 0; u < COMPOUND13_ARRAY_SIZE + 1; u++)
        data_out.x[u] = (unsigned char)u;
    data_out.y = 99.99F;

    /* Set latest_format in access property list to enable the latest
     * compound datatype format.
     */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_libver_bounds(fapl_id, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Open file and get root group. */
    h5_fixname(FILENAME[4], H5P_DEFAULT, filename, sizeof filename);
    if ((fileid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        FAIL_STACK_ERROR;
    if ((grpid = H5Gopen2(fileid, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create a compound type. */
    if ((dtypeid = H5Tcreate(H5T_COMPOUND, sizeof(struct s1))) < 0)
        FAIL_STACK_ERROR;
    if ((array1_tid = H5Tarray_create2(H5T_NATIVE_UCHAR, 1, dims)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(dtypeid, "x", HOFFSET(struct s1, x), array1_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(dtypeid, "y", HOFFSET(struct s1, y), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR;

    /* Create a space. */
    if ((spaceid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR;

    /* Create an attribute of this compound type. */
    if ((attid = H5Acreate2(grpid, COMPOUND13_ATTR_NAME, dtypeid, spaceid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write some data. */
    if (H5Awrite(attid, dtypeid, &data_out) < 0)
        FAIL_STACK_ERROR;

    /* Release all resources. */
    if (H5Aclose(attid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(array1_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(dtypeid) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(spaceid) < 0)
        FAIL_STACK_ERROR;
    if (H5Gclose(grpid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fileid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl_id) < 0)
        FAIL_STACK_ERROR;

    /* Now open the file and read it. */
    if ((fileid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((grpid = H5Gopen2(fileid, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((attid = H5Aopen(grpid, COMPOUND13_ATTR_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((dtypeid = H5Aget_type(attid)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tget_class(dtypeid) != H5T_COMPOUND)
        FAIL_STACK_ERROR;
    if (HOFFSET(struct s1, x) != H5Tget_member_offset(dtypeid, 0))
        TEST_ERROR;
    if (HOFFSET(struct s1, y) != H5Tget_member_offset(dtypeid, 1))
        TEST_ERROR;
    if (H5Aread(attid, dtypeid, &data_in) < 0)
        FAIL_STACK_ERROR;

    /* Check the data. */
    for (u = 0; u < COMPOUND13_ARRAY_SIZE + 1; u++)
        if (data_out.x[u] != data_in.x[u])
            TEST_ERROR;
    if (!H5_FLT_ABS_EQUAL(data_out.y, data_in.y))
        TEST_ERROR;

    /* Release all resources. */
    if (H5Aclose(attid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(dtypeid) < 0)
        FAIL_STACK_ERROR;
    if (H5Gclose(grpid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fileid) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_13() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_14
 *
 * Purpose:     Tests compound type conversions where a vlen string will
 *              be misaligned in the conversion buffer and the file.  The
 *              two compound types are meant to trigger two different
 *              conversion routines.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_14(void)
{
    typedef struct cmpd_struct_1_w {
        char        c1;
        char        c2;
        const char *str;
    } cmpd_struct_1_w;

    typedef struct cmpd_struct_1_r {
        char  c1;
        char  c2;
        char *str;
    } cmpd_struct_1_r;

    typedef struct cmpd_struct_2_w {
        char        c1;
        char        c2;
        const char *str;
        long        l1;
        long        l2;
        long        l3;
        long        l4;
    } cmpd_struct_2_w;

    typedef struct cmpd_struct_2_r {
        char  c1;
        char  c2;
        char *str;
        long  l1;
        long  l2;
        long  l3;
        long  l4;
    } cmpd_struct_2_r;

    cmpd_struct_1_w wdata1 = {'A', 'B', "variable-length string"};
    cmpd_struct_1_r rdata1;
    cmpd_struct_2_w wdata2 = {'C', 'D', "another vlen!", 1, 2, -1, 9001};
    cmpd_struct_2_r rdata2;
    hid_t           file;
    hid_t           cmpd_m1_tid, cmpd_f1_tid, cmpd_m2_tid, cmpd_f2_tid, str_id;
    hid_t           space_id;
    hid_t           dset1_id, dset2_id;
    hsize_t         dim1[1];
    char            filename[1024];

    TESTING("unaligned VL strings in compound");

    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create file!\n");
        goto error;
    } /* end if */

    /* Create memory compound datatype 1 */
    if ((cmpd_m1_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct_1_w))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m1_tid, "c1", HOFFSET(struct cmpd_struct_1_w, c1), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m1_tid, "c2", HOFFSET(struct cmpd_struct_1_w, c2), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c2'\n");
        goto error;
    } /* end if */

    str_id = H5Tcopy(H5T_C_S1);
    if (H5Tset_size(str_id, H5T_VARIABLE) < 0) {
        H5_FAILED();
        AT();
        printf("Can't set size for VL string\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m1_tid, "vl_string", HOFFSET(cmpd_struct_1_w, str), str_id) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'vl_string'\n");
        goto error;
    } /* end if */

    /* Create file compound datatype 1 */
    if ((cmpd_f1_tid = H5Tcreate(H5T_COMPOUND, 8 + 1 + sizeof(hvl_t))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f1_tid, "c1", (size_t)0, H5T_STD_I64BE) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f1_tid, "c2", (size_t)8, H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c2'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f1_tid, "vl_string", (size_t)(8 + 1), str_id) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'vl_string'\n");
        goto error;
    } /* end if */

    /* Create memory compound datatype 2 */
    if ((cmpd_m2_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct_2_w))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "c1", HOFFSET(struct cmpd_struct_2_w, c1), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "c2", HOFFSET(struct cmpd_struct_2_w, c2), H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c2'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "vl_string", HOFFSET(cmpd_struct_2_w, str), str_id) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'vl_string'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "l1", HOFFSET(struct cmpd_struct_2_w, l1), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "l2", HOFFSET(struct cmpd_struct_2_w, l2), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l2'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "l3", HOFFSET(struct cmpd_struct_2_w, l3), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l3'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m2_tid, "l4", HOFFSET(struct cmpd_struct_2_w, l4), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l4'\n");
        goto error;
    } /* end if */

    /* Create file compound datatype 2 */
    if ((cmpd_f2_tid = H5Tcreate(H5T_COMPOUND, 8 + 1 + sizeof(hvl_t) + 4 * sizeof(long))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "c1", (size_t)0, H5T_STD_I64BE) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "c2", (size_t)8, H5T_NATIVE_CHAR) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'c2'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "vl_string", (size_t)(8 + 1), str_id) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'vl_string'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "l1", 8 + 1 + sizeof(hvl_t), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "l2", 8 + 1 + sizeof(hvl_t) + sizeof(long), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l2'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "l3", 8 + 1 + sizeof(hvl_t) + 2 * sizeof(long), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l3'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f2_tid, "l4", 8 + 1 + sizeof(hvl_t) + 3 * sizeof(long), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'l4'\n");
        goto error;
    } /* end if */

    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create space\n");
        goto error;
    } /* end if */

    if ((dset1_id = H5Dcreate2(file, "Dataset1", cmpd_f1_tid, space_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    } /* end if */

    if ((dset2_id = H5Dcreate2(file, "Dataset2", cmpd_f2_tid, space_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    } /* end if */

    if (H5Dwrite(dset1_id, cmpd_m1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    if (H5Dwrite(dset2_id, cmpd_m2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    if (H5Dread(dset1_id, cmpd_m1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (H5Dread(dset2_id, cmpd_m2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (rdata1.c1 != wdata1.c1 || rdata1.c2 != wdata1.c2 || strcmp(rdata1.str, wdata1.str) != 0) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (rdata2.c1 != wdata2.c1 || rdata2.c2 != wdata2.c2 || strcmp(rdata2.str, wdata2.str) != 0 ||
        rdata2.l1 != wdata2.l1 || rdata2.l2 != wdata2.l2 || rdata2.l3 != wdata2.l3 ||
        rdata2.l4 != wdata2.l4) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (H5Treclaim(cmpd_m1_tid, space_id, H5P_DEFAULT, &rdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim read data\n");
        goto error;
    } /* end if */
    rdata1.str = NULL;
    if (H5Treclaim(cmpd_m2_tid, space_id, H5P_DEFAULT, &rdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim read data\n");
        goto error;
    } /* end if */
    rdata2.str = NULL;
    if (H5Dclose(dset1_id) < 0)
        goto error;
    if (H5Dclose(dset2_id) < 0)
        goto error;
    if (H5Tclose(cmpd_f1_tid) < 0)
        goto error;
    if (H5Tclose(cmpd_f2_tid) < 0)
        goto error;
    if (H5Tclose(str_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open file\n");
        goto error;
    } /* end if */

    if ((dset1_id = H5Dopen2(file, "Dataset1", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open dataset\n");
        goto error;
    } /* end if */

    if ((dset2_id = H5Dopen2(file, "Dataset2", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("cannot open dataset\n");
        goto error;
    } /* end if */

    if ((space_id = H5Dget_space(dset2_id)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't get space\n");
        goto error;
    } /* end if */

    rdata1.c1 = rdata1.c2 = 0;
    if (rdata1.str)
        free(rdata1.str);

    rdata2.c1 = rdata2.c2 = 0;
    rdata2.l1 = rdata2.l2 = rdata2.l3 = rdata2.l4 = 0;
    if (rdata2.str) {
        free(rdata2.str);
        rdata2.str = NULL;
    } /* end if */

    if (H5Dread(dset1_id, cmpd_m1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (H5Dread(dset2_id, cmpd_m2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    if (rdata1.c1 != wdata1.c1 || rdata1.c2 != wdata1.c2 || strcmp(rdata1.str, wdata1.str) != 0) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (rdata2.c1 != wdata2.c1 || rdata2.c2 != wdata2.c2 || strcmp(rdata2.str, wdata2.str) != 0 ||
        rdata2.l1 != wdata2.l1 || rdata2.l2 != wdata2.l2 || rdata2.l3 != wdata2.l3 ||
        rdata2.l4 != wdata2.l4) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    if (H5Treclaim(cmpd_m1_tid, space_id, H5P_DEFAULT, &rdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim read data\n");
        goto error;
    } /* end if */
    rdata1.str = NULL;
    if (H5Treclaim(cmpd_m2_tid, space_id, H5P_DEFAULT, &rdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim read data\n");
        goto error;
    } /* end if */
    rdata2.str = NULL;

    if (H5Dclose(dset1_id) < 0)
        goto error;
    if (H5Dclose(dset2_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Tclose(cmpd_m1_tid) < 0)
        goto error;
    if (H5Tclose(cmpd_m2_tid) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_14() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_15
 *
 * Purpose:     Tests that conversion occurs correctly when the source is
 *              subset of the destination, but there is extra space at the
 *              end of the source type.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_15(void)
{
    typedef struct cmpd_struct {
        int i1;
        int i2;
    } cmpd_struct;

    cmpd_struct wdata1 = {1254, 5471};
    cmpd_struct rdata;
    int         wdata2[2] = {1, 2};
    hid_t       file;
    hid_t       cmpd_m_tid, cmpd_f_tid;
    hid_t       space_id;
    hid_t       dset_id;
    hsize_t     dim1[1];
    char        filename[1024];

    TESTING("compound subset conversion with extra space in source");

    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create file!\n");
        goto error;
    } /* end if */

    /* Create file compound datatype */
    if ((cmpd_f_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f_tid, "i1", HOFFSET(struct cmpd_struct, i1), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i1'\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_f_tid, "i2", HOFFSET(struct cmpd_struct, i2), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i2'\n");
        goto error;
    } /* end if */

    /* Create memory compound datatype */
    if ((cmpd_m_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_m_tid, "i1", (size_t)0, H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'i1'\n");
        goto error;
    } /* end if */

    /* Create space, dataset, write wdata1 */
    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create space\n");
        goto error;
    } /* end if */

    if ((dset_id = H5Dcreate2(file, "Dataset", cmpd_f_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    } /* end if */

    if (H5Dwrite(dset_id, cmpd_f_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata1) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    /* Write wdata2.  The use of cmpd_m_tid here should cause only the first
     * element of wdata2 to be written. */
    if (H5Dwrite(dset_id, cmpd_m_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata2) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    /* Read data */
    if (H5Dread(dset_id, cmpd_f_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    /* Check for correctness of read data */
    if (rdata.i1 != wdata2[0] || rdata.i2 != wdata1.i2) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    /* Now try reading only the i1 field, verify it does not overwrite i2 in the
     * read buffer */
    rdata.i1 = wdata1.i1;
    rdata.i2 = wdata2[1];

    /* Read data */
    if (H5Dread(dset_id, cmpd_m_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    /* Check for correctness of read data */
    if (rdata.i1 != wdata2[0] || rdata.i2 != wdata2[1]) {
        H5_FAILED();
        AT();
        printf("incorrect read data\n");
        goto error;
    } /* end if */

    /* Close */
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Tclose(cmpd_f_tid) < 0)
        goto error;
    if (H5Tclose(cmpd_m_tid) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_15() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_15_attr
 *
 * Purpose:     Tests that conversion occurs correctly when the source is
 *              subset of the destination, but there is extra space at the
 *              end of the source type. This one tests on attribute while
 *              test_compound_15() tests on dataset.  That's the only
 *              difference.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_15_attr(void)
{
    typedef struct cmpd_struct {
        int i1;
        int i2;
    } cmpd_struct;

    cmpd_struct wdata1 = {1254, 5471};
    cmpd_struct rdata;
    int         wdata2[2]  = {1, 2};
    hid_t       file       = H5I_INVALID_HID;
    hid_t       cmpd_m_tid = H5I_INVALID_HID;
    hid_t       cmpd_f_tid = H5I_INVALID_HID;
    hid_t       space_id   = H5I_INVALID_HID;
    hid_t       attr_id    = H5I_INVALID_HID;
    hsize_t     dim1[1];
    char        filename[1024];

    TESTING("compound subset conversion with extra space in source for attribute");

    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof(filename));
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't create file!\n");

    /* Create file compound datatype */
    if ((cmpd_f_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0)
        FAIL_PUTS_ERROR("Can't create datatype!\n");

    if (H5Tinsert(cmpd_f_tid, "i1", HOFFSET(struct cmpd_struct, i1), H5T_NATIVE_INT) < 0)
        FAIL_PUTS_ERROR("Can't insert field 'i1'\n");

    if (H5Tinsert(cmpd_f_tid, "i2", HOFFSET(struct cmpd_struct, i2), H5T_NATIVE_INT) < 0)
        FAIL_PUTS_ERROR("Can't insert field 'i2'\n");

    /* Create memory compound datatype */
    if ((cmpd_m_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0)
        FAIL_PUTS_ERROR("Can't create datatype!\n");

    if (H5Tinsert(cmpd_m_tid, "i1", (size_t)0, H5T_NATIVE_INT) < 0)
        FAIL_PUTS_ERROR("Can't insert field 'i1'\n");

    /* Create space, dataset, write wdata1 */
    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0)
        FAIL_PUTS_ERROR("Can't create space\n");

    if ((attr_id = H5Acreate_by_name(file, ".", "attr_cmpd", cmpd_f_tid, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                     H5P_DEFAULT)) < 0)
        FAIL_PUTS_ERROR("Can't create attribute\n");

    if (H5Awrite(attr_id, cmpd_f_tid, &wdata1) < 0)
        FAIL_PUTS_ERROR("Can't write data\n");

    /* Write wdata2.  The use of cmpd_m_tid here should cause only the first
     * element of wdata2 to be written. */
    if (H5Awrite(attr_id, cmpd_m_tid, &wdata2) < 0)
        FAIL_PUTS_ERROR("Can't write data\n");

    /* Read data */
    if (H5Aread(attr_id, cmpd_f_tid, &rdata) < 0)
        FAIL_PUTS_ERROR("Can't read data\n");

    /* Check for correctness of read data */
    if (rdata.i1 != wdata2[0] || rdata.i2 != wdata1.i2)
        FAIL_PUTS_ERROR("incorrect read data\n");

    /* Now try reading only the i1 field, verify it does not overwrite i2 in the
     * read buffer */
    rdata.i1 = wdata1.i1;
    rdata.i2 = wdata2[1];

    /* Read data */
    if (H5Aread(attr_id, cmpd_m_tid, &rdata) < 0)
        FAIL_PUTS_ERROR("Can't read data\n");

    /* Check for correctness of read data */
    if (rdata.i1 != wdata2[0] || rdata.i2 != wdata2[1])
        FAIL_PUTS_ERROR("incorrect read data\n");

    /* Close */
    if (H5Aclose(attr_id) < 0)
        goto error;
    if (H5Tclose(cmpd_f_tid) < 0)
        goto error;
    if (H5Tclose(cmpd_m_tid) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(attr_id);
        H5Tclose(cmpd_f_tid);
        H5Tclose(cmpd_m_tid);
        H5Sclose(space_id);
        H5Fclose(file);
    }
    H5E_END_TRY

    return 1;
} /* end test_compound_15_attr() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_16
 *
 * Purpose:     Tests that committed types that can be registered during
 *              compound conversion are not visible to the application
 *              with H5Fget_obj_count or H5Fget_obj_ids.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_16(void)
{
    typedef struct cmpd_struct {
        int i1;
        int i2;
    } cmpd_struct;

    cmpd_struct wdata1 = {1254, 5471};
    ssize_t     obj_count;
    hid_t       file;
    hid_t       cmpd_m_tid, cmpd_f_tid, int_id;
    hid_t       space_id;
    hid_t       dset_id;
    hid_t       open_dtypes[2] = {0, 0};
    hsize_t     dim1[1]        = {1};
    char        filename[1024];

    TESTING("visibility of internally registered type ids");

    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Copy and commit integer datatype */
    if ((int_id = H5Tcopy(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if (H5Tcommit2(file, "int", int_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Create file compound datatype */
    if ((cmpd_f_tid = H5Tcreate(H5T_COMPOUND, 2 * sizeof(int) + 2)) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_f_tid, "i1", (size_t)0, int_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_f_tid, "i2", sizeof(int) + 1, int_id) < 0)
        TEST_ERROR;

    /* Create memory compound datatype */
    if ((cmpd_m_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_m_tid, "i1", HOFFSET(struct cmpd_struct, i1), int_id) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_m_tid, "i2", HOFFSET(struct cmpd_struct, i2), int_id) < 0)
        TEST_ERROR;

    /* Create space, dataset, write wdata1 */
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0)
        TEST_ERROR;
    if ((dset_id = H5Dcreate2(file, "Dataset", cmpd_f_tid, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) <
        0)
        TEST_ERROR;
    if (H5Dwrite(dset_id, cmpd_m_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata1) < 0)
        TEST_ERROR;

    /* Check behavior of H5Fget_obj_count */
    if ((obj_count = H5Fget_obj_count(file, H5F_OBJ_DATATYPE)) != 1) {
        H5_FAILED();
        AT();
        printf("    H5Fget_obj_count returned: %zd; expected: 1\n", obj_count);
        goto error;
    }

    /* Check behavior of H5Fget_obj_ids */
    if (H5Fget_obj_ids(file, H5F_OBJ_DATATYPE, (size_t)2, open_dtypes) < 0)
        TEST_ERROR;
    if (open_dtypes[1]) {
        H5_FAILED();
        AT();
        printf("    H5Fget_obj_ids returned as second id: %lld; expected: 0\n", (long long)open_dtypes[1]);
        goto error;
    }

    /* Close */
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(cmpd_f_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(cmpd_m_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(int_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_16() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_17
 *
 * Purpose:     Tests that compound types are packed correctly when they
 *              only have extra space at the end.  The compounds are
 *              "hidden" inside arrays to make sure that they are still
 *              detected correctly.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_17(void)
{
    hid_t   file;
    hid_t   cmpd_int, arr_int, cmpd_ext, arr_ext, tmp_dt;
    hsize_t dims[1] = {2};
    char    filename[1024];

    TESTING("that H5Tpack removes trailing bytes");

    /* Create inner compound datatype.  This type will be "packed" according
     * to the internal field, but will have trailing space at the end. */
    if ((cmpd_int = H5Tcreate(H5T_COMPOUND, (size_t)4)) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_int, "c", (size_t)0, H5T_NATIVE_CHAR) < 0)
        TEST_ERROR;

    /* Create inner array datatype */
    if ((arr_int = H5Tarray_create2(cmpd_int, 1, dims)) < 0)
        TEST_ERROR;

    /* Create outer compound datatype.  This type will be truly packed, with no
     * trailing space.  However, the internal compound contained within is not
     * packed. */
    if ((cmpd_ext = H5Tcreate(H5T_COMPOUND, (size_t)8)) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmpd_ext, "arr", (size_t)0, arr_int) < 0)
        TEST_ERROR;

    /* Create outer array datatype */
    if ((arr_ext = H5Tarray_create2(cmpd_ext, 1, dims)) < 0)
        TEST_ERROR;

    /* Try packing the internal array.  Size should be 2 after packing. */
    if ((tmp_dt = H5Tcopy(arr_int)) < 0)
        TEST_ERROR;
    if (H5Tpack(tmp_dt) < 0)
        TEST_ERROR;
    if (2 != H5Tget_size(tmp_dt)) {
        H5_FAILED();
        AT();
        printf("    Size after packing: %u; expected: 2\n", (unsigned)H5Tget_size(tmp_dt));
        goto error;
    }
    if (H5Tclose(tmp_dt) < 0)
        TEST_ERROR;

    /* Try packing the external array.  Size should be 4 after packing. */
    if ((tmp_dt = H5Tcopy(arr_ext)) < 0)
        TEST_ERROR;
    if (H5Tpack(tmp_dt) < 0)
        TEST_ERROR;
    if (4 != H5Tget_size(tmp_dt)) {
        H5_FAILED();
        AT();
        printf("    Size after packing: %u; expected: 4\n", (unsigned)H5Tget_size(tmp_dt));
        goto error;
    }
    if (H5Tclose(tmp_dt) < 0)
        TEST_ERROR;

    /* Now we will commit arr_int and arr_ext to a file, and verify that they
     * are still packed correctly after opening them from the file */
    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Commit the datatypes.  Note that they are still unpacked. */
    if (H5Tcommit2(file, "arr_int", arr_int, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (H5Tcommit2(file, "arr_ext", arr_ext, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Close IDs */
    if (H5Tclose(cmpd_int) < 0)
        TEST_ERROR;
    if (H5Tclose(arr_int) < 0)
        TEST_ERROR;
    if (H5Tclose(cmpd_ext) < 0)
        TEST_ERROR;
    if (H5Tclose(arr_ext) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Reopen file */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Open committed array datatypes */
    if ((arr_int = H5Topen2(file, "arr_int", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((arr_ext = H5Topen2(file, "arr_ext", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Try packing the internal array.  Size should be 2 after packing. */
    if ((tmp_dt = H5Tcopy(arr_int)) < 0)
        TEST_ERROR;
    if (H5Tpack(tmp_dt) < 0)
        TEST_ERROR;
    if (2 != H5Tget_size(tmp_dt)) {
        H5_FAILED();
        AT();
        printf("    Size after packing: %u; expected: 2\n", (unsigned)H5Tget_size(tmp_dt));
        goto error;
    }
    if (H5Tclose(tmp_dt) < 0)
        TEST_ERROR;

    /* Try packing the external array.  Size should be 4 after packing. */
    if ((tmp_dt = H5Tcopy(arr_ext)) < 0)
        TEST_ERROR;
    if (H5Tpack(tmp_dt) < 0)
        TEST_ERROR;
    if (4 != H5Tget_size(tmp_dt)) {
        H5_FAILED();
        AT();
        printf("    Size after packing: %u; expected: 4\n", (unsigned)H5Tget_size(tmp_dt));
        goto error;
    }
    if (H5Tclose(tmp_dt) < 0)
        TEST_ERROR;

    /* Close IDs */
    if (H5Tclose(arr_int) < 0)
        TEST_ERROR;
    if (H5Tclose(arr_ext) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_17() */

/*-------------------------------------------------------------------------
 * Function:    test_compound_18
 *
 * Purpose:     Tests that library fails correctly when opening a dataset
 *              a compound datatype with zero fields.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_compound_18(void)
{
    hid_t       file     = H5I_INVALID_HID;
    hid_t       gid      = H5I_INVALID_HID;
    hid_t       did      = H5I_INVALID_HID;
    hid_t       aid      = H5I_INVALID_HID;
    hid_t       tid      = H5I_INVALID_HID;
    hid_t       sid      = H5I_INVALID_HID;
    hsize_t     dim      = 1;
    const char *testfile = H5_get_srcdir_filename(TESTFILE); /* Corrected test file name */
    char        filename[1024];
    bool        driver_is_default_compatible;
    herr_t      ret;

    TESTING("accessing objects with compound datatypes that have no fields");

    /* Create compound datatype, but don't insert fields */
    tid = H5Tcreate(H5T_COMPOUND, (size_t)8);
    assert(tid > 0);

    /* Attempt to create file with compound datatype that has no fields */
    /* Create File */
    h5_fixname(FILENAME[3], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataspace to use */
    sid = H5Screate_simple(1, &dim, NULL);
    assert(sid > 0);

    /* Create a dataset with the bad compound datatype */
    H5E_BEGIN_TRY
    {
        did = H5Dcreate2(file, "dataset", tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (did > 0) {
        H5Dclose(did);
        FAIL_PUTS_ERROR("created dataset with bad compound datatype");
    } /* end if */

    /* Create a group */
    gid = H5Gcreate2(file, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(gid > 0);

    /* Create an attribute with the bad compound datatype */
    H5E_BEGIN_TRY
    {
        aid = H5Acreate2(gid, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (aid > 0) {
        H5Aclose(aid);
        FAIL_PUTS_ERROR("created attribute with bad compound datatype");
    } /* end if */

    /* Commit the datatype */
    H5E_BEGIN_TRY
    {
        ret = H5Tcommit2(file, "cmpnd", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("committed named datatype with bad compound datatype");
    } /* end if */

    /* Close IDs */
    if (H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Gclose(gid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    if (h5_driver_is_default_vfd_compatible(H5P_DEFAULT, &driver_is_default_compatible) < 0)
        FAIL_PUTS_ERROR("can't check if VFD is default VFD compatible");

    if (driver_is_default_compatible) {
        /* Open Generated File */
        /* (generated with gen_bad_compound.c) */
        if ((file = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Try to open the datatype */
        H5E_BEGIN_TRY
        {
            tid = H5Topen2(file, "cmpnd", H5P_DEFAULT);
        }
        H5E_END_TRY
        if (tid > 0) {
            H5Tclose(tid);
            FAIL_PUTS_ERROR("opened named datatype with bad compound datatype");
        } /* end if */

        /* Try to open the dataset */
        H5E_BEGIN_TRY
        {
            did = H5Dopen2(file, "dataset", H5P_DEFAULT);
        }
        H5E_END_TRY
        if (did > 0) {
            H5Dclose(did);
            FAIL_PUTS_ERROR("opened dataset with bad compound datatype");
        } /* end if */

        /* Open the group with the attribute */
        if ((gid = H5Gopen2(file, "group", H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* Try to open the dataset */
        H5E_BEGIN_TRY
        {
            aid = H5Aopen(gid, "attr", H5P_DEFAULT);
        }
        H5E_END_TRY
        if (aid > 0) {
            H5Aclose(aid);
            FAIL_PUTS_ERROR("opened attribute with bad compound datatype");
        } /* end if */

        /* Close IDs */
        if (H5Gclose(gid) < 0)
            FAIL_STACK_ERROR;
        if (H5Fclose(file) < 0)
            FAIL_STACK_ERROR;
    }

    PASSED();
    return 0;

error:
    return 1;
} /* end test_compound_18() */

/*-------------------------------------------------------------------------
 * Function:    test_user_compound_conversion
 *
 * Purpose:     Tests that library correctly handles a user-defined
 *              conversion function between two compound types.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_user_compound_conversion(void)
{
    hid_t             file_id = H5I_INVALID_HID;
    hid_t             dset_id = H5I_INVALID_HID;
    hid_t             src = H5I_INVALID_HID, dst = H5I_INVALID_HID;
    hid_t             space_id = H5I_INVALID_HID;
    struct src_cmpd_t buf[]    = {{1, 1.0}};

    hsize_t dim = 1;
    char    filename[1024];

    TESTING("compound conversion via user conversion callback");

    /* Create the source compound datatype */
    if ((src = H5Tcreate(H5T_COMPOUND, sizeof(struct src_cmpd_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(src, "a", HOFFSET(struct src_cmpd_t, a), H5T_NATIVE_UINT32) < 0)
        TEST_ERROR;

    if (H5Tinsert(src, "b", HOFFSET(struct src_cmpd_t, b), H5T_NATIVE_FLOAT) < 0)
        TEST_ERROR;

    /* Create the destination compound datatype */
    if ((dst = H5Tcreate(H5T_COMPOUND, sizeof(struct dst_cmpd_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(dst, "b", HOFFSET(struct dst_cmpd_t, b), H5T_IEEE_F32LE) < 0)
        TEST_ERROR;

    if (H5Tregister(H5T_PERS_SOFT, "src_cmpd_t->dst_cmpd_t", src, dst, &user_compound_convert) < 0)
        TEST_ERROR;

    /* Create File */
    h5_fixname(FILENAME[0], H5P_DEFAULT, filename, sizeof filename);
    if ((file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a dataspace to use */
    if ((space_id = H5Screate_simple(1, &dim, NULL)) < 0)
        TEST_ERROR;

    /* Create a dataset with the destination compound datatype */
    if ((dset_id = H5Dcreate2(file_id, "dataset", dst, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Dwrite(dset_id, src, space_id, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    /* Close IDs */
    if (H5Tunregister(H5T_PERS_SOFT, "src_cmpd_t->dst_cmpd_t", src, dst, &user_compound_convert) < 0)
        TEST_ERROR;
    if (H5Tclose(src) < 0)
        TEST_ERROR;
    if (H5Tclose(dst) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tunregister(H5T_PERS_SOFT, "src_cmpd_t->dst_cmpd_t", src, dst, &user_compound_convert);
        H5Tclose(src);
        H5Tclose(dst);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY;

    return 1;
} /* end test_user_compound_conversion() */

/*-------------------------------------------------------------------------
 * Function:    test_query
 *
 * Purpose:     Tests query functions of compound and enumeration types.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_query(void)
{
    struct s1 {
        int    a;
        float  b;
        long   c;
        double d;
    };
    hid_t file = H5I_INVALID_HID, tid1 = H5I_INVALID_HID, tid2 = H5I_INVALID_HID;
    char  filename[1024];
    char  compnd_type[] = "Compound_type", enum_type[] = "Enum_type";
    short enum_val;
    char  enum_name[16];

    TESTING("query functions of compound and enumeration types");

    /* Create File */
    h5_fixname(FILENAME[2], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Create a compound datatype */
    if ((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct s1))) < 0) {
        H5_FAILED();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */
    if (H5Tinsert(tid1, "a", HOFFSET(struct s1, a), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'a'\n");
        goto error;
    } /* end if */
    if (H5Tinsert(tid1, "b", HOFFSET(struct s1, b), H5T_NATIVE_FLOAT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'b'\n");
        goto error;
    } /* end if */
    if (H5Tinsert(tid1, "c", HOFFSET(struct s1, c), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        printf("Can't insert field 'c'\n");
        goto error;
    } /* end if */
    if (H5Tinsert(tid1, "d", HOFFSET(struct s1, d), H5T_NATIVE_DOUBLE) < 0) {
        H5_FAILED();
        printf("Can't insert field 'd'\n");
        goto error;
    } /* end if */

    /* Create a enumerate datatype */
    if ((tid2 = H5Tcreate(H5T_ENUM, sizeof(short))) < 0) {
        H5_FAILED();
        printf("Can't create enumerate type\n");
        goto error;
    } /* end if */
    enum_val = 10;
    if (H5Tenum_insert(tid2, "RED", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    } /* end if */
    enum_val = 11;
    if (H5Tenum_insert(tid2, "GREEN", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    } /* end if */
    enum_val = 12;
    if (H5Tenum_insert(tid2, "BLUE", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    } /* end if */
    enum_val = 13;
    if (H5Tenum_insert(tid2, "ORANGE", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    } /* end if */
    enum_val = 14;
    if (H5Tenum_insert(tid2, "YELLOW", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    } /* end if */

    /* Query member number and member index by name, for compound type. */
    if (H5Tget_nmembers(tid1) != 4) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    } /* end if */
    if (H5Tget_member_index(tid1, "c") != 2) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    } /* end if */

    /* Query member number and member index by member name, for enumeration type. */
    if (H5Tget_nmembers(tid2) != 5) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    } /* end if */
    if (H5Tget_member_index(tid2, "ORANGE") != 3) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    } /* end if */

    /* Commit compound datatype and close it */
    if (H5Tcommit2(file, compnd_type, tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit compound datatype\n");
        goto error;
    } /* end if */
    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    } /* end if */

    /* Commit enumeration datatype and close it */
    if (H5Tcommit2(file, enum_type, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit compound datatype\n");
        goto error;
    } /* end if */
    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    } /* end if */

    /* Open the dataytpe for query */
    if ((tid1 = H5Topen2(file, compnd_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((tid2 = H5Topen2(file, enum_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Query member number and member index by name, for compound type */
    if (H5Tget_nmembers(tid1) != 4) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    } /* end if */
    if (H5Tget_member_index(tid1, "c") != 2) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    } /* end if */

    /* Query member number and member index by member name, for enumeration type */
    if (H5Tget_nmembers(tid2) != 5) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    } /* end if */
    if (H5Tget_member_index(tid2, "ORANGE") != 3) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    } /* end if */

    /* Query member value by member name, for enumeration type */
    if (H5Tenum_valueof(tid2, "ORANGE", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't get value for enumerate member\n");
        goto error;
    } /* end if */
    if (enum_val != 13) {
        H5_FAILED();
        printf("Incorrect value for enum member\n");
        goto error;
    } /* end if */

    /* Query member value by member index, for enumeration type */
    if (H5Tget_member_value(tid2, 2, &enum_val) < 0) {
        H5_FAILED();
        printf("Can't get value for enum member\n");
        goto error;
    } /* end if */
    if (enum_val != 12) {
        H5_FAILED();
        printf("Incorrect value for enum member\n");
        goto error;
    } /* end if */

    /* Query member name by member value, for enumeration type */
    enum_val = 14;
    if (H5Tenum_nameof(tid2, &enum_val, enum_name, (size_t)16) < 0) {
        H5_FAILED();
        printf("Can't get name for enum member\n");
        goto error;
    } /* end if */
    if (strcmp("YELLOW", enum_name) != 0) {
        H5_FAILED();
        printf("Incorrect name for enum member\n");
        goto error;
    } /* end if */

    /* Close datatype and file */
    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    } /* end if */
    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    } /* end if */

    if (H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    } /* end if */

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_set_fields_offset
 *
 * Purpose:     Tests for a bug in H5Tset_fields in which the function
 *              didn't account for an offset set for a floating-point
 *              datatype when checking whether the values set for the
 *              floating-point fields make sense for the datatype.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_set_fields_offset(void)
{
    hid_t tid = H5I_INVALID_HID;

    if ((tid = H5Tcopy(H5T_NATIVE_FLOAT)) < 0) {
        H5_FAILED();
        printf("Can't copy datatype\n");
        goto error;
    }

    /* Create a custom 128-bit floating-point datatype */
    if (H5Tset_size(tid, 16) < 0) {
        H5_FAILED();
        printf("Can't set datatype size\n");
        goto error;
    }

    /* We will have 7 bytes of MSB padding + 5 bytes of offset padding */
    if (H5Tset_precision(tid, 116) < 0) {
        H5_FAILED();
        printf("Can't set datatype size\n");
        goto error;
    }

    if (H5Tset_offset(tid, 5) < 0) {
        H5_FAILED();
        printf("Can't set datatype offset\n");
        goto error;
    }

    if (H5Tset_ebias(tid, 16383) < 0) {
        H5_FAILED();
        printf("Can't set datatype exponent bias\n");
        goto error;
    }

    /*
     * Floating-point type with the following:
     *
     *   - 5 bits of LSB padding (bits 0 - 4)
     *   - 100-bit mantissa starting at bit 5
     *   - 15-bit exponent starting at bit 105
     *   - 1 sign bit at bit 120
     *   - 7 bits of MSB padding
     */
    if (H5Tset_fields(tid, 120, 105, 15, 5, 100) < 0) {
        H5_FAILED();
        printf("Can't set datatype's floating-point fields\n");
        goto error;
    }

    if (H5Tclose(tid) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid);
    }
    H5E_END_TRY;

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_transient
 *
 * Purpose:    Tests transient datatypes.
 *
 * Return:    Success:    0
 *
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_transient(hid_t fapl)
{
    static hsize_t ds_size[2] = {10, 20};
    hid_t file = H5I_INVALID_HID, type = H5I_INVALID_HID, space = H5I_INVALID_HID, dset = H5I_INVALID_HID,
          t2 = H5I_INVALID_HID;
    char   filename[1024];
    hid_t  ret_id; /* Generic hid_t return value    */
    herr_t status;

    TESTING("transient datatypes");

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        goto error;
    }
    if ((space = H5Screate_simple(2, ds_size, ds_size)) < 0)
        goto error;

    /* Predefined types cannot be modified or closed */
    H5E_BEGIN_TRY
    {
        status = H5Tset_precision(H5T_NATIVE_INT, (size_t)256);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Predefined types should not be modifiable!");
        goto error;
    }
    H5E_BEGIN_TRY
    {
        status = H5Tclose(H5T_NATIVE_INT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Predefined types should not be closable!");
        goto error;
    }

    /* Copying a predefined type results in a modifiable copy */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    if (H5Tset_precision(type, (size_t)256) < 0)
        goto error;

    /* It should not be possible to create an attribute for a transient type */
    H5E_BEGIN_TRY
    {
        ret_id = H5Acreate2(type, "attr1", H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (ret_id >= 0) {
        H5_FAILED();
        puts("    Attributes should not be allowed for transient types!");
        goto error;
    }

    /* Create a dataset from a transient datatype */
    if (H5Tclose(type) < 0)
        goto error;
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    if ((dset = H5Dcreate2(file, "dset1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* The type returned from a dataset should not be modifiable */
    if ((t2 = H5Dget_type(dset)) < 0)
        goto error;
    H5E_BEGIN_TRY
    {
        status = H5Tset_precision(t2, (size_t)256);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Dataset datatypes should not be modifiable!");
        goto error;
    }
    if (H5Tclose(t2) < 0)
        goto error;

    /*
     * Close the dataset and reopen it, testing that it's type is still
     * read-only.
     */
    if (H5Dclose(dset) < 0)
        goto error;
    if ((dset = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0)
        goto error;
    if ((t2 = H5Dget_type(dset)) < 0)
        goto error;
    H5E_BEGIN_TRY
    {
        status = H5Tset_precision(t2, (size_t)256);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Dataset datatypes should not be modifiable!");
        goto error;
    }
    if (H5Tclose(t2) < 0)
        goto error;

    /*
     * Get the dataset datatype by applying H5Tcopy() to the dataset. The
     * result should be modifiable.
     */
    if ((t2 = H5Tcopy(dset)) < 0)
        goto error;
    if (H5Tset_precision(t2, (size_t)256) < 0)
        goto error;
    if (H5Tclose(t2) < 0)
        goto error;

    if (H5Dclose(dset) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;
    if (H5Tclose(type) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(t2);
        H5Tclose(type);
        H5Sclose(space);
        H5Dclose(dset);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_named
 *
 * Purpose:    Tests named datatypes.
 *
 * Return:    Success:    0
 *
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_named(hid_t fapl)
{
    hid_t file = H5I_INVALID_HID, type = H5I_INVALID_HID, space = H5I_INVALID_HID, dset = H5I_INVALID_HID,
          t2 = H5I_INVALID_HID, t3 = H5I_INVALID_HID, attr1 = H5I_INVALID_HID;
    herr_t         status;
    static hsize_t ds_size[2] = {10, 20};
    size_t         i, j;
    unsigned       attr_data[10][20];
    char           filename[1024];

    TESTING("named datatypes");

    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        goto error;
    }
    if ((space = H5Screate_simple(2, ds_size, ds_size)) < 0)
        goto error;

    /* Predefined types cannot be committed */
    H5E_BEGIN_TRY
    {
        status = H5Tcommit2(file, "test_named_1 (should not exist)", H5T_NATIVE_INT, H5P_DEFAULT, H5P_DEFAULT,
                            H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Predefined types should not be committable!");
        goto error;
    }

    /* Copy a predefined datatype and commit the copy */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    if (H5Tcommit2(file, "native-int", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        goto error;
    if ((status = H5Tcommitted(type)) < 0)
        goto error;
    if (0 == status) {
        H5_FAILED();
        puts("    H5Tcommitted() returned false!");
        goto error;
    }

    /* We should not be able to modify a type after it has been committed. */
    H5E_BEGIN_TRY
    {
        status = H5Tset_precision(type, (size_t)256);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Committed type is not constant!");
        goto error;
    }

    /* We should not be able to re-commit a committed type */
    H5E_BEGIN_TRY
    {
        status =
            H5Tcommit2(file, "test_named_2 (should not exist)", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Committed types should not be recommitted!");
        goto error;
    }

    /* It should be possible to define an attribute for the named type */
    if ((attr1 = H5Acreate2(type, "attr1", H5T_NATIVE_UCHAR, space, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    for (i = 0; i < (size_t)ds_size[0]; i++)
        for (j = 0; j < (size_t)ds_size[1]; j++)
            attr_data[i][j] = (unsigned)(i * ds_size[1] + j);
    if (H5Awrite(attr1, H5T_NATIVE_UINT, attr_data) < 0)
        goto error;
    if (H5Aclose(attr1) < 0)
        goto error;

    /*
     * Copying a committed type should result in a transient type which is
     * not locked.
     */
    if ((t2 = H5Tcopy(type)) < 0)
        goto error;
    if ((status = H5Tcommitted(t2)) < 0)
        goto error;
    if (status) {
        H5_FAILED();
        puts("    Copying a named type should result in a transient type!");
        goto error;
    }
    if (H5Tset_precision(t2, (size_t)256) < 0)
        goto error;
    if (H5Tclose(t2) < 0)
        goto error;

    /*
     * Close the committed type and reopen it.  It should return a named type.
     */
    if (H5Tclose(type) < 0)
        goto error;
    if ((type = H5Topen2(file, "native-int", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((status = H5Tcommitted(type)) < 0)
        goto error;
    if (!status) {
        H5_FAILED();
        puts("    Opened named types should be named types!");
        goto error;
    }

    /* Create a dataset that uses the named type */
    if ((dset = H5Dcreate2(file, "dset1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Get the dataset's datatype and make sure it's a named type */
    if ((t2 = H5Dget_type(dset)) < 0)
        goto error;
    if ((status = H5Tcommitted(t2)) < 0)
        goto error;
    if (!status) {
        H5_FAILED();
        puts("    Dataset type should be a named type!");
        goto error;
    }

    /* Close the dataset, then close its type, then reopen the dataset */
    if (H5Dclose(dset) < 0)
        goto error;
    if (H5Tclose(t2) < 0)
        goto error;
    if ((dset = H5Dopen2(file, "dset1", H5P_DEFAULT)) < 0)
        goto error;

    /* Get the dataset's type and make sure it's named */
    if ((t2 = H5Dget_type(dset)) < 0)
        goto error;
    if ((status = H5Tcommitted(t2)) < 0)
        goto error;
    if (!status) {
        H5_FAILED();
        puts("    Dataset type should be a named type!");
        goto error;
    }

    /*
     * Close the dataset and create another with the type returned from the
     * first dataset.
     */
    if (H5Dclose(dset) < 0)
        goto error;
    if ((dset = H5Dcreate2(file, "dset2", t2, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Reopen the second dataset and make sure the type is shared */
    if (H5Tclose(t2) < 0)
        goto error;
    if (H5Dclose(dset) < 0)
        goto error;
    if ((dset = H5Dopen2(file, "dset2", H5P_DEFAULT)) < 0)
        goto error;
    if ((t2 = H5Dget_type(dset)) < 0)
        goto error;
    if ((status = H5Tcommitted(t2)) < 0)
        goto error;
    if (!status) {
        H5_FAILED();
        puts("    Dataset type should be a named type!");
        goto error;
    }
    if (H5Tclose(t2) < 0)
        goto error;

    /*
     * Get the dataset datatype by applying H5Tcopy() to the dataset. The
     * result should be modifiable.
     */
    if ((t2 = H5Tcopy(dset)) < 0)
        goto error;
    if (H5Tset_precision(t2, (size_t)256) < 0)
        goto error;
    if (H5Tclose(t2) < 0)
        goto error;
    if (H5Dclose(dset) < 0)
        goto error;

    /*
     * Copy of committed type used as dataset type should not be name type
     */
    if ((t2 = H5Tcopy(type)) < 0)
        goto error;
    if ((status = H5Tcommitted(t2)) < 0)
        goto error;
    if (status) {
        H5_FAILED();
        puts("    Copied type should not be a named type!");
        goto error;
    }
    if ((dset = H5Dcreate2(file, "dset3", t2, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;
    if ((t3 = H5Dget_type(dset)) < 0)
        goto error;
    if ((status = H5Tcommitted(t3)) < 0)
        goto error;
    if (status) {
        H5_FAILED();
        puts("    Datatype from dataset using copied type should not be a named type!");
        goto error;
    }
    if (H5Tclose(t3) < 0)
        goto error;
    if (H5Dclose(dset) < 0)
        goto error;

    /* Close */
    if (H5Tclose(type) < 0)
        goto error;
    if (H5Sclose(space) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    /* Reopen file with read only access */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        goto error;

    /* Verify that H5Tcommit2 returns an error */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    H5E_BEGIN_TRY
    {
        status =
            H5Tcommit2(file, "test_named_3 (should not exist)", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Types should not be committable to a read-only file!");
        goto error;
    }

    /* Verify that H5Tcommit_anon returns an error */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    H5E_BEGIN_TRY
    {
        status = H5Tcommit_anon(file, type, H5P_DEFAULT, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Types should not be committable to a read-only file!");
        goto error;
    }

    /* Close */
    if (H5Tclose(type) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(t3);
        H5Tclose(t2);
        H5Tclose(type);
        H5Sclose(space);
        H5Dclose(dset);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    mkstr
 *
 * Purpose:    Create a new string datatype
 *
 * Return:    Success:    New type
 *        Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static hid_t
mkstr(size_t len, H5T_str_t strpad)
{
    hid_t t;

    if ((t = H5Tcopy(H5T_C_S1)) < 0)
        return -1;
    if (H5Tset_size(t, len) < 0)
        return -1;
    if (H5Tset_strpad(t, strpad) < 0)
        return -1;

    return t;
}

/*-------------------------------------------------------------------------
 * Function:    test_str_create
 *
 * Purpose:    Test string type creation using H5Tcreate
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_str_create(void)
{
    hid_t  fixed_str1, fixed_str2;
    hid_t  vlen_str1, vlen_str2;
    htri_t is_vl_str = false;
    size_t query_size, str_size = 10;

    TESTING("string type creation using H5Tcreate");

    /* Create fixed-length string in two ways and make sure they are the same */
    if ((fixed_str1 = mkstr(str_size, H5T_STR_NULLTERM)) < 0)
        goto error;

    if ((fixed_str2 = H5Tcreate(H5T_STRING, str_size)) < 0)
        goto error;
    if (H5Tset_strpad(fixed_str2, H5T_STR_NULLTERM) < 0)
        goto error;

    if (!H5Tequal(fixed_str1, fixed_str2))
        goto error;

    if ((query_size = H5Tget_size(fixed_str1)) == 0)
        goto error;
    if (query_size != str_size)
        goto error;

    if ((query_size = H5Tget_size(fixed_str2)) == 0)
        goto error;
    if (query_size != str_size)
        goto error;

    if (H5Tclose(fixed_str1) < 0)
        goto error;
    if (H5Tclose(fixed_str2) < 0)
        goto error;

    /* Create variable-length string in two ways and make sure they are the same */
    if ((vlen_str1 = mkstr((size_t)H5T_VARIABLE, H5T_STR_NULLTERM)) < 0)
        goto error;

    if ((vlen_str2 = H5Tcreate(H5T_STRING, (size_t)H5T_VARIABLE)) < 0)
        goto error;
    if (H5Tset_strpad(vlen_str2, H5T_STR_NULLTERM) < 0)
        goto error;

    if (!H5Tequal(vlen_str1, vlen_str2))
        goto error;

    if ((is_vl_str = H5Tis_variable_str(vlen_str1)) < 0)
        goto error;
    if (!is_vl_str)
        goto error;

    if ((is_vl_str = H5Tis_variable_str(vlen_str2)) < 0)
        goto error;
    if (!is_vl_str)
        goto error;

    if (H5Tclose(vlen_str1) < 0)
        goto error;
    if (H5Tclose(vlen_str2) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_str_1
 *
 * Purpose:    Test string conversions
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_str_1(void)
{
    char *buf      = NULL;
    hid_t src_type = H5I_INVALID_HID;
    hid_t dst_type = H5I_INVALID_HID;

    TESTING("string conversions");

    /*
     * Convert a null-terminated string to a shorter and longer null
     * terminated string.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_NULLTERM)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)5, H5T_STR_NULLTERM)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghi\0abcdefghi\0", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd\0abcd\0abcdefghi\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Truncated C-string test failed");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd\0\0\0\0\0\0abcd\0\0\0\0\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Extended C-string test failed");
        goto error;
    }
    free(buf);
    buf = NULL;
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;

    /*
     * Convert a null padded string to a shorter and then longer string.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_NULLPAD)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)5, H5T_STR_NULLPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghijabcdefghij", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdeabcdeabcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Truncated C buffer test failed");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Extended C buffer test failed");
        goto error;
    }
    free(buf);
    buf = NULL;
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;

    /*
     * Convert a space-padded string to a shorter and then longer string.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_SPACEPAD)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)5, H5T_STR_SPACEPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghijabcdefghij", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdeabcdeabcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Truncated Fortran-string test failed");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcde     abcde     ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Extended Fortran-string test failed");
        goto error;
    }
    free(buf);
    buf = NULL;
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;

    /*
     * What happens if a null-terminated string is not null terminated?  If
     * the conversion is to an identical string then nothing happens but if
     * the destination is a different size or type of string then the right
     * thing should happen.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_NULLTERM)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)10, H5T_STR_NULLTERM)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghijabcdefghij", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Non-terminated string test 1");
        goto error;
    }
    H5Tclose(dst_type);
    if ((dst_type = mkstr((size_t)5, H5T_STR_NULLTERM)) < 0)
        goto error;
    memcpy(buf, "abcdefghijabcdefghij", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd\0abcd\0abcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Non-terminated string test 2");
        goto error;
    }
    memcpy(buf, "abcdeabcdexxxxxxxxxx", (size_t)20);
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Non-terminated string test 2");
        goto error;
    }
    free(buf);
    buf = NULL;
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;

    /*
     * Test C string to Fortran and vice versa.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_NULLTERM)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)10, H5T_STR_SPACEPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghi\0abcdefghi\0", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdefghi abcdefghi ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C string to Fortran test 1");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdefghi\0abcdefghi\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C string test 1");
        goto error;
    }
    if (H5Tclose(dst_type) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)5, H5T_STR_SPACEPAD)) < 0)
        goto error;
    memcpy(buf, "abcdefgh\0\0abcdefgh\0\0", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdeabcdeabcdefgh\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C string to Fortran test 2");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C string test 2");
        goto error;
    }
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;
    if ((src_type = mkstr((size_t)5, H5T_STR_NULLTERM)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)10, H5T_STR_SPACEPAD)) < 0)
        goto error;
    memcpy(buf, "abcd\0abcd\0xxxxxxxxxx", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd      abcd      ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C string to Fortran test 3");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd\0abcd\0abcd      ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C string test 3");
        goto error;
    }
    free(buf);
    buf = NULL;
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;

    /*
     * Test C buffer to Fortran and vice versa.
     */
    if ((src_type = mkstr((size_t)10, H5T_STR_NULLPAD)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)10, H5T_STR_SPACEPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc((size_t)2, (size_t)10)))
        goto error;
    memcpy(buf, "abcdefghijabcdefghij", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C buffer to Fortran test 1");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdefghijabcdefghij", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C buffer test 1");
        goto error;
    }
    if (H5Tclose(dst_type) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)5, H5T_STR_SPACEPAD)) < 0)
        goto error;
    memcpy(buf, "abcdefgh\0\0abcdefgh\0\0", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcdeabcdeabcdefgh\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C buffer to Fortran test 2");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcde\0\0\0\0\0abcde\0\0\0\0\0", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C buffer test 2");
        goto error;
    }
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;
    if ((src_type = mkstr((size_t)5, H5T_STR_NULLPAD)) < 0)
        goto error;
    if ((dst_type = mkstr((size_t)10, H5T_STR_SPACEPAD)) < 0)
        goto error;
    memcpy(buf, "abcd\0abcd\0xxxxxxxxxx", (size_t)20);
    if (H5Tconvert(src_type, dst_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd      abcd      ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    C buffer to Fortran test 3");
        goto error;
    }
    if (H5Tconvert(dst_type, src_type, (size_t)2, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (memcmp(buf, "abcd\0abcd\0abcd      ", (size_t)20) != 0) {
        H5_FAILED();
        puts("    Fortran to C buffer test 3");
        goto error;
    }
    if (H5Tclose(src_type) < 0)
        goto error;
    if (H5Tclose(dst_type) < 0)
        goto error;
    free(buf);

    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(src_type);
        H5Tclose(dst_type);
    }
    H5E_END_TRY

    if (buf)
        free(buf);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_str_2
 *
 * Purpose:    Tests C-to-Fortran and Fortran-to-C string conversion speed.
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_str_2(void)
{
    char        *buf    = NULL, s[80];
    hid_t        c_type = H5I_INVALID_HID;
    hid_t        f_type = H5I_INVALID_HID;
    const size_t nelmts = NTESTELEM;
    size_t       i, j, nchars;
    int          ret_value = 1;

    /*
     * Initialize types and buffer.
     */
    if ((c_type = mkstr((size_t)8, H5T_STR_NULLPAD)) < 0)
        goto error;
    if ((f_type = mkstr((size_t)8, H5T_STR_SPACEPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc(nelmts, (size_t)8)))
        goto error;
    for (i = 0; i < nelmts; i++) {
        nchars = (size_t)(HDrand() % 8);
        for (j = 0; j < nchars; j++)
            buf[i * 8 + j] = (char)('a' + HDrand() % 26);
        while (j < nchars)
            buf[i * 8 + j++] = '\0';
    } /* end for */

    /* Do the conversions */
    snprintf(s, sizeof(s), "Testing random string conversion speed");
    printf("%-70s", s);
    fflush(stdout);
    if (H5Tconvert(c_type, f_type, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (H5Tconvert(f_type, c_type, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    PASSED();

    ret_value = 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(c_type);
        H5Tclose(f_type);
    }
    H5E_END_TRY

    if (buf)
        free(buf);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_str_3
 *
 * Purpose:    Tests some functions that are or aren't supposed to work
 *              for string type.
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_str_3(void)
{
    char        *buf    = NULL;
    hid_t        type   = H5I_INVALID_HID;
    hid_t        super  = H5I_INVALID_HID;
    const size_t nelmts = NTESTELEM;
    size_t       i, j, nchars;
    int          ret_value = 1;
    size_t       size;
    H5T_pad_t    inpad;
    H5T_sign_t   sign;
    char        *tag = NULL;
    herr_t       ret;

    TESTING("some type functions for string");

    /*
     * Initialize types and buffer.
     */
    if ((type = mkstr((size_t)8, H5T_STR_NULLPAD)) < 0)
        goto error;
    if (NULL == (buf = (char *)calloc(nelmts, (size_t)8)))
        FAIL_PUTS_ERROR("Allocation failed.");
    for (i = 0; i < nelmts; i++) {
        nchars = (size_t)(HDrand() % 8);
        for (j = 0; j < nchars; j++)
            buf[i * 8 + j] = (char)('a' + HDrand() % 26);
        while (j < nchars)
            buf[i * 8 + j++] = '\0';
    } /* end for */

    if (H5Tget_precision(type) == 0)
        FAIL_STACK_ERROR;
    if (H5Tget_size(type) == 0)
        FAIL_STACK_ERROR;
    if (H5Tset_pad(type, H5T_PAD_ZERO, H5T_PAD_ONE) < 0)
        FAIL_STACK_ERROR;
    if (H5Tget_cset(type) < 0)
        FAIL_STACK_ERROR;
    if (H5Tget_strpad(type) < 0)
        FAIL_STACK_ERROR;
    if (H5Tset_offset(type, (size_t)0) < 0)
        FAIL_STACK_ERROR;
    if (H5Tget_order(type) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        ret = H5Tset_precision(type, nelmts);
    }
    H5E_END_TRY
    if (ret >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        size = H5Tget_ebias(type);
    }
    H5E_END_TRY
    if (size > 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        inpad = H5Tget_inpad(type);
    }
    H5E_END_TRY
    if (inpad > -1) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        sign = H5Tget_sign(type);
    }
    H5E_END_TRY
    if (sign > -1) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        tag = H5Tget_tag(type);
    }
    H5E_END_TRY
    if (tag) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    H5E_BEGIN_TRY
    {
        super = H5Tget_super(type);
    }
    H5E_END_TRY
    if (super >= 0) {
        FAIL_PUTS_ERROR("Operation not allowed for this type.");
    } /* end if */

    PASSED();
    ret_value = 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type);
        H5Tclose(super);
    }
    H5E_END_TRY

    if (buf)
        free(buf);
    if (tag)
        H5free_memory(tag); /* Technically allocated by API call */

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return ret_value; /* Number of errors */
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_enum_1
 *
 * Purpose:    Test conversion speed for enum datatypes
 *
 * Return:    Success:    0
 *
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_enum_1(void)
{
    const size_t nelmts = NTESTELEM;
    int          i, val, *buf = NULL;
    hid_t        t1 = H5I_INVALID_HID;
    hid_t        t2 = H5I_INVALID_HID;
    char         s[80];
    int          ret_value = 1;
    size_t       u;

    /* Build the datatypes */
    if ((t1 = H5Tcreate(H5T_ENUM, sizeof(int))) < 0)
        goto error;
    if ((t2 = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        goto error;
    s[1] = '\0';
    for (i = 0; i < 26; i++) {
        s[0] = (char)('A' + i);
        H5Tenum_insert(t1, s, &i);
        val = i * 1000 + i;
        H5Tenum_insert(t2, s, &val);
    } /* end for */

    /* Initialize the buffer */
    if (NULL == (buf = (int *)malloc(nelmts * MAX(H5Tget_size(t1), H5Tget_size(t2)))))
        goto error;
    for (u = 0; u < nelmts; u++)
        buf[u] = HDrand() % 26;

    /* Conversions */
    snprintf(s, sizeof(s), "Testing random enum conversion O(N)");
    printf("%-70s", s);
    fflush(stdout);
    if (H5Tconvert(t1, t2, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    PASSED();

    snprintf(s, sizeof(s), "Testing random enum conversion O(N log N)");
    printf("%-70s", s);
    fflush(stdout);
    if (H5Tconvert(t2, t1, nelmts, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    PASSED();

    ret_value = 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(t1);
        H5Tclose(t2);
    }
    H5E_END_TRY

    if (buf)
        free(buf);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_enum_2
 *
 * Purpose:     Tests enumeration conversions where source isn't a native type.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_enum_2(void)
{
    hid_t       srctype = H5I_INVALID_HID, dsttype = H5I_INVALID_HID, oddsize = H5I_INVALID_HID;
    int        *data = NULL, i, nerrors = 0;
    const char *mname[] = {"RED", "GREEN", "BLUE", "YELLOW", "PINK", "PURPLE", "ORANGE", "WHITE"};

    TESTING("non-native enumeration type conversion");

    /* Source enum type */
    oddsize = H5Tcopy(H5T_STD_I32BE);
    H5Tset_size(oddsize, (size_t)3); /*reduce to 24 bits, not corresponding to any native size*/
    srctype = H5Tenum_create(oddsize);
    for (i = 7; i >= 0; --i) {
        char pattern[3];
        pattern[2] = (char)i;
        pattern[0] = pattern[1] = 0;
        H5Tenum_insert(srctype, mname[i], pattern);
    }

    /* Destination enum type */
    dsttype = H5Tenum_create(H5T_NATIVE_INT);
    assert(H5Tget_size(dsttype) > H5Tget_size(srctype));
    for (i = 0; i < 8; i++)
        H5Tenum_insert(dsttype, mname[i], &i);

    /* Source data */
    data = (int *)malloc(NTESTELEM * sizeof(int));
    for (i = 0; i < NTESTELEM; i++) {
        ((char *)data)[i * 3 + 2] = (char)(i % 8);
        ((char *)data)[i * 3 + 0] = 0;
        ((char *)data)[i * 3 + 1] = 0;
    }

    /* Convert to destination type */
    H5Tconvert(srctype, dsttype, (size_t)NTESTELEM, data, NULL, H5P_DEFAULT);

    /* Check results */
    for (i = 0; i < NTESTELEM; i++) {
        if (data[i] != i % 8) {
            if (!nerrors++) {
                H5_FAILED();
                printf("element %d is %d but should have been  %d\n", i, data[i], i % 8);
            }
        }
    }

    /* Cleanup */
    free(data);
    H5Tclose(srctype);
    H5Tclose(dsttype);
    H5Tclose(oddsize);

    /* Failure */
    if (nerrors) {
        printf("total of %d conversion errors out of %d elements for enums\n", nerrors, NTESTELEM);
        return 1;
    }

    PASSED();
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    test_conv_bitfield
 *
 * Purpose:     Test bitfield conversions.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_conv_bitfield(void)
{
    unsigned char buf[4];
    hid_t         st = H5I_INVALID_HID, dt = H5I_INVALID_HID;

    TESTING("bitfield conversions");

    /*
     * First test a simple bitfield conversion:
     *                   1010101010101010
     *   ________________1010101010101010
     */
    st     = H5Tcopy(H5T_STD_B16LE);
    dt     = H5Tcopy(H5T_STD_B32LE);
    buf[0] = buf[1] = 0xAA;
    buf[2] = buf[3] = 0x55; /*irrelevant*/
    if (H5Tconvert(st, dt, (size_t)1, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (buf[0] != 0xAA || buf[1] != 0xAA || buf[2] != 0 || buf[3] != 0) {
        H5_FAILED();
        printf("    s=0xaaaa, d=0x%02x%02x%02x%02x (test 1)\n", buf[3], buf[2], buf[1], buf[0]);
        goto error;
    }

    /*
     * Test2: Offset a 12-byte value in the middle of a 16 and 32 byte
     * field.
     *              __10 1010 1010 10__
     *    ____ ____ __10 1010 1010 10__ ____ ____
     */
    H5Tset_precision(st, (size_t)12);
    H5Tset_offset(st, (size_t)2);
    H5Tset_precision(dt, (size_t)12);
    H5Tset_offset(dt, (size_t)10);
    buf[0] = 0xA8;
    buf[1] = 0x2A;
    buf[2] = buf[3] = 0;
    if (H5Tconvert(st, dt, (size_t)1, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (buf[0] != 0 || buf[1] != 0xA8 || buf[2] != 0x2A || buf[3] != 0) {
        H5_FAILED();
        printf("    s=0x2AA8 d=0x%02x%02x%02x%02x (test 2)\n", buf[3], buf[2], buf[1], buf[0]);
        goto error;
    }

    /*
     * Same as previous test except unused bits of the destination will
     * be filled with ones.
     */
    H5Tset_pad(dt, H5T_PAD_ONE, H5T_PAD_ONE);
    buf[0] = 0xA8;
    buf[1] = 0x2A;
    buf[2] = buf[3] = 0;
    if (H5Tconvert(st, dt, (size_t)1, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (buf[0] != 0xff || buf[1] != 0xAB || buf[2] != 0xEA || buf[3] != 0xff) {
        H5_FAILED();
        printf("    s=0x2AA8 d=0x%02x%02x%02x%02x (test 3)\n", buf[3], buf[2], buf[1], buf[0]);
        goto error;
    }

    H5Tclose(st);
    H5Tclose(dt);
    PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();
    return 0;

error:
    H5Tclose(st);
    H5Tclose(dt);

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_bitfield_funcs
 *
 * Purpose:     Test some datatype functions that are and aren't supposed
 *              work for bitfield type.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_bitfield_funcs(void)
{
    hid_t      type = H5I_INVALID_HID, ntype = H5I_INVALID_HID, super = H5I_INVALID_HID;
    size_t     size;
    char      *tag = 0;
    H5T_pad_t  inpad;
    H5T_cset_t cset;
    H5T_str_t  strpad;
    herr_t     ret;
    int        retval = -1;

    TESTING("some type functions for bitfield");

    /*
     * First create a bitfield type.
     */
    if ((type = H5Tcopy(H5T_STD_B32LE)) < 0)
        goto error;

    /*
     * Offset a 12-byte value in the middle of a 16 and 32 byte
     * field.  Pad unused bits with ones.
     *    ____ ____ __10 1010 1010 10__ ____ ____
     */
    if (H5Tset_precision(type, (size_t)12) < 0)
        goto error;
    if (H5Tset_offset(type, (size_t)10) < 0)
        goto error;
    if (H5Tset_pad(type, H5T_PAD_ONE, H5T_PAD_ONE))
        goto error;
    if ((size = H5Tget_size(type)) == 0)
        goto error;
    if (H5Tset_order(type, H5T_ORDER_BE) < 0)
        goto error;
    if ((ntype = H5Tget_native_type(type, H5T_DIR_ASCEND)) < 0)
        goto error;

    H5E_BEGIN_TRY
    {
        size = H5Tget_ebias(type);
    }
    H5E_END_TRY
    if (size > 0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        inpad = H5Tget_inpad(type);
    }
    H5E_END_TRY
    if (inpad > -1) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        cset = H5Tget_cset(type);
    }
    H5E_END_TRY
    if (cset > -1) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        strpad = H5Tget_strpad(type);
    }
    H5E_END_TRY
    if (strpad > -1) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        ret = H5Tset_sign(type, H5T_SGN_2);
    }
    H5E_END_TRY
    if (ret >= 0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        tag = H5Tget_tag(type);
    }
    H5E_END_TRY
    if (tag) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    H5E_BEGIN_TRY
    {
        super = H5Tget_super(type);
    }
    H5E_END_TRY
    if (super >= 0) {
        H5_FAILED();
        printf("Operation not allowed for this type.\n");
        goto error;
    } /* end if */

    retval = 0;

error:
    if (retval == -1)
        retval = 1;
    H5free_memory(tag);
    H5Tclose(ntype);
    H5Tclose(type);
    if (retval == 0)
        PASSED();

    /* Restore the default error handler (set in h5_reset()) */
    h5_restore_err();

    reset_hdf5();

    return retval;
}

/*-------------------------------------------------------------------------
 * Function:    convert_opaque
 *
 * Purpose:     A fake opaque conversion functions
 *
 * Return:      Success:    0
 *              Failure:    -1
 *
 *-------------------------------------------------------------------------
 */
static herr_t
convert_opaque(hid_t H5_ATTR_UNUSED st, hid_t H5_ATTR_UNUSED dt, H5T_cdata_t *cdata,
               size_t H5_ATTR_UNUSED nelmts, size_t H5_ATTR_UNUSED buf_stride,
               size_t H5_ATTR_UNUSED bkg_stride, void H5_ATTR_UNUSED *_buf, void H5_ATTR_UNUSED *bkg,
               hid_t H5_ATTR_UNUSED dset_xfer_plid)
{
    if (H5T_CONV_CONV == cdata->command)
        num_opaque_conversions_g++;
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    test_opaque
 *
 * Purpose:     Driver function to test opaque datatypes
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_opaque(void)
{
    int num_errors = 0;

    TESTING("opaque datatypes");

    /* Test opaque types with tags */
    num_errors += opaque_check(0);
    /* Test opaque types without tag */
    num_errors += opaque_check(1);
    /* Test named opaque types with very long tag */
    num_errors += opaque_long();
    /* Test some type functions with opaque type */
    num_errors += opaque_funcs();

    if (num_errors)
        goto error;

    PASSED();
    return 0;

error:
    return num_errors;
}

/*-------------------------------------------------------------------------
 * Function:    opaque_check
 *
 * Purpose:     Test opaque datatypes
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
opaque_check(int tag_it)
{
#define OPAQUE_NELMTS 1000
    hid_t  st = H5I_INVALID_HID, dt = H5I_INVALID_HID;
    herr_t status;
    char   buf[1]; /*not really used*/
    int    saved;

    saved = num_opaque_conversions_g = 0;

    /* Build source and destination types */
    if ((st = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        goto error;
    if (H5Tset_tag(st, "opaque source type") < 0)
        goto error;

    if ((dt = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        goto error;
    if (tag_it) {
        if (H5Tset_tag(dt, "opaque destination type") < 0)
            goto error;
    }

    /* Make sure that we can't convert between the types yet */
    H5E_BEGIN_TRY
    {
        status = H5Tconvert(st, dt, (size_t)OPAQUE_NELMTS, buf, NULL, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        printf("    opaque conversion should have failed but succeeded\n");
        goto error;
    }

    /* Register a conversion function */
    if (H5Tregister(H5T_PERS_HARD, "o_test", st, dt, convert_opaque) < 0)
        goto error;

    /* Try the conversion again, this time it should work */
    if (H5Tconvert(st, dt, (size_t)OPAQUE_NELMTS, buf, NULL, H5P_DEFAULT) < 0)
        goto error;
    if (saved + 1 != num_opaque_conversions_g) {
        H5_FAILED();
        printf("    unexpected number of opaque conversions\n");
        goto error;
    }

    /* Unregister conversion function */
    if (H5Tunregister(H5T_PERS_HARD, "o_test", st, dt, convert_opaque) < 0)
        goto error;

    H5Tclose(st);
    H5Tclose(dt);
    return 0;

error:
    if (st > 0)
        H5Tclose(st);
    if (dt > 0)
        H5Tclose(dt);
    H5_FAILED();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    opaque_long
 *
 * Purpose:     Test named (committed) opaque datatypes w/very long tags
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
opaque_long(void)
{
    char  *long_tag = NULL;
    hid_t  dt       = H5I_INVALID_HID;
    herr_t ret;

    /* Build opaque type */
    if ((dt = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        TEST_ERROR;

    /* Create long tag */
    if (NULL == (long_tag = (char *)malloc((size_t)(16384 + 1))))
        TEST_ERROR;
    memset(long_tag, 'a', (size_t)16384);
    long_tag[16384] = '\0';

    /* Set opaque type's tag */
    H5E_BEGIN_TRY
    {
        ret = H5Tset_tag(dt, long_tag);
    }
    H5E_END_TRY
    if (ret != FAIL)
        TEST_ERROR;

    /* Close datatype */
    if (H5Tclose(dt) < 0)
        TEST_ERROR;

    /* Release memory for tag */
    free(long_tag);

    return 0;

error:
    if (dt > 0)
        H5Tclose(dt);
    if (long_tag)
        free(long_tag);
    H5_FAILED();
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    opaque_funcs
 *
 * Purpose:     Test some type functions that are and aren't supposed to
 *              work with opaque type.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
opaque_funcs(void)
{
    hid_t      type = H5I_INVALID_HID, super = H5I_INVALID_HID;
    size_t     size;
    H5T_pad_t  inpad;
    H5T_cset_t cset;
    H5T_str_t  strpad;
    H5T_sign_t sign;
    herr_t     ret;

    /* Build opaque type */
    if ((type = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0)
        TEST_ERROR;
    if (H5Tset_tag(type, "opaque source type") < 0)
        TEST_ERROR;

    if ((size = H5Tget_size(type)) == 0)
        goto error;

    H5E_BEGIN_TRY
    {
        ret = H5Tset_precision(type, (size_t)32);
    }
    H5E_END_TRY
    if (ret >= 0) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        ret = H5Tset_pad(type, H5T_PAD_ZERO, H5T_PAD_ONE);
    }
    H5E_END_TRY
    if (ret >= 0) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        size = H5Tget_ebias(type);
    }
    H5E_END_TRY
    if (size > 0) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        inpad = H5Tget_inpad(type);
    }
    H5E_END_TRY
    if (inpad > -1) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        cset = H5Tget_cset(type);
    }
    H5E_END_TRY
    if (cset > -1) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        strpad = H5Tget_strpad(type);
    }
    H5E_END_TRY
    if (strpad > -1) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        ret = H5Tset_offset(type, (size_t)16);
    }
    H5E_END_TRY
    if (ret >= 0) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        sign = H5Tget_sign(type);
    }
    H5E_END_TRY
    if (sign > -1) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    H5E_BEGIN_TRY
    {
        super = H5Tget_super(type);
    }
    H5E_END_TRY
    if (super >= 0) {
        printf("Operation not allowed for this type.\n");
        TEST_ERROR;
    } /* end if */

    /* Close datatype */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    return 0;

error:
    if (type > 0)
        H5Tclose(type);
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test__Float16
 *
 * Purpose:     Tests the _Float16 datatype.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *-------------------------------------------------------------------------
 */
static int
test__Float16(void)
{
#ifdef H5_HAVE__FLOAT16
    H5T_path_t *path = NULL;
    const char *driver_name;
    hsize_t     dims[1];
    htri_t      is_little_endian;
    H5T_t      *native_dtype = NULL;
    H5T_t      *tmp_dtype    = NULL;
    hid_t       fid          = H5I_INVALID_HID;
    hid_t       space_id     = H5I_INVALID_HID;
    hid_t       dset_id      = H5I_INVALID_HID;
    hid_t       dcpl_id      = H5I_INVALID_HID;
    hid_t       native_type  = H5I_INVALID_HID;
    char        filename[256];

    TESTING("_Float16 datatype");

    driver_name = h5_get_test_driver_name();

    /* Check that native macro maps to a valid type */
    if (0 == H5Tget_size(H5T_NATIVE_FLOAT16)) {
        H5_FAILED();
        printf("Invalid size for H5T_NATIVE_FLOAT16 datatype\n");
        goto error;
    }

    /* Check that native type for standard 16-bit float type matches */
    if ((native_type = H5Tget_native_type(H5T_IEEE_F16LE, H5T_DIR_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't get native type for H5T_IEEE_F16LE\n");
        goto error;
    }

    if (0 == H5Tequal(native_type, H5T_NATIVE_FLOAT16)) {
        H5_FAILED();
        printf("Native _Float16 type for H5T_IEEE_F16LE wasn't equal to H5T_NATIVE_FLOAT16\n");
        goto error;
    }

    if (H5Tclose(native_type) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if ((native_type = H5Tget_native_type(H5T_IEEE_F16BE, H5T_DIR_DEFAULT)) < 0) {
        H5_FAILED();
        printf("Can't get native type for H5T_IEEE_F16BE\n");
        goto error;
    }

    if (0 == H5Tequal(native_type, H5T_NATIVE_FLOAT16)) {
        H5_FAILED();
        printf("Native _Float16 type for H5T_IEEE_F16BE wasn't equal to H5T_NATIVE_FLOAT16\n");
        goto error;
    }

    if (H5Tclose(native_type) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    /*
     * Ensure that conversion between native _Float16 datatype and
     * the matching standard datatype is covered by the no-op conversion
     * function. Ensure that conversion between native _Float16 datatype
     * and the other standard datatype is covered by the byte-order
     * conversion function.
     */
    if (NULL == (native_dtype = H5I_object_verify(H5T_NATIVE_FLOAT16, H5I_DATATYPE))) {
        H5_FAILED();
        printf("Can't get H5T_t structure for datatype\n");
        goto error;
    }

    if ((is_little_endian = H5Tequal(H5T_NATIVE_FLOAT16, H5T_IEEE_F16LE)) < 0) {
        H5_FAILED();
        printf("Can't check if native _Float16 type matches standard little-endian type\n");
        goto error;
    }

    if (NULL == (tmp_dtype = H5I_object_verify(H5T_IEEE_F16LE, H5I_DATATYPE))) {
        H5_FAILED();
        printf("Can't get H5T_t structure for H5T_IEEE_F16LE datatype\n");
        goto error;
    }

    if (NULL == (path = H5T_path_find(native_dtype, tmp_dtype))) {
        H5_FAILED();
        printf("Can't find datatype conversion path\n");
        goto error;
    }

    if (path->is_hard || path->conv.is_app) {
        H5_FAILED();
        printf("Invalid conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16LE\n");
        goto error;
    }

    if (is_little_endian) {
        if (path->conv.u.lib_func != H5T__conv_noop) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16LE was not H5T__conv_noop\n");
            goto error;
        }
    }
    else {
        if (path->conv.u.lib_func != H5T__conv_order_opt) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16LE was not H5T__conv_order\n");
            goto error;
        }
    }

    if (NULL == (tmp_dtype = H5I_object_verify(H5T_IEEE_F16BE, H5I_DATATYPE))) {
        H5_FAILED();
        printf("Can't get H5T_t structure for H5T_IEEE_F16BE datatype\n");
        goto error;
    }

    if (NULL == (path = H5T_path_find(native_dtype, tmp_dtype))) {
        H5_FAILED();
        printf("Can't find datatype conversion path\n");
        goto error;
    }

    if (path->is_hard || path->conv.is_app) {
        H5_FAILED();
        printf("Invalid conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16BE\n");
        goto error;
    }

    if (is_little_endian) {
        if (path->conv.u.lib_func != H5T__conv_order_opt) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16BE was not H5T__conv_order\n");
            goto error;
        }
    }
    else {
        if (path->conv.u.lib_func != H5T__conv_noop) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_IEEE_F16BE was not H5T__conv_noop\n");
            goto error;
        }
    }

    /*
     * Ensure that conversion between native _Float16 datatype and a
     * couple of other datatypes are the correct type of conversions.
     */
    if (is_little_endian) {
        /* Check for a native type that matches H5T_STD_I32LE before
         * checking for a hard conversion path
         */
        if (H5Tequal(H5T_NATIVE_SHORT, H5T_STD_I32LE) == true ||
            H5Tequal(H5T_NATIVE_INT, H5T_STD_I32LE) == true ||
            H5Tequal(H5T_NATIVE_LONG, H5T_STD_I32LE) == true) {
            if (H5Tcompiler_conv(H5T_NATIVE_FLOAT16, H5T_STD_I32LE) != true) {
                H5_FAILED();
                printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_STD_I32LE was not a hard conversion\n");
                goto error;
            }
        }

        if (H5Tcompiler_conv(H5T_NATIVE_FLOAT16, H5T_STD_I32BE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_STD_I32BE was not a soft conversion\n");
            goto error;
        }
    }
    else {
        if (H5Tcompiler_conv(H5T_NATIVE_FLOAT16, H5T_STD_I32LE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_STD_I32LE was not a soft conversion\n");
            goto error;
        }

        /* Check for a native type that matches H5T_STD_I32BE before
         * checking for a hard conversion path
         */
        if (H5Tequal(H5T_NATIVE_SHORT, H5T_STD_I32BE) == true ||
            H5Tequal(H5T_NATIVE_INT, H5T_STD_I32BE) == true ||
            H5Tequal(H5T_NATIVE_LONG, H5T_STD_I32BE) == true) {
            if (H5Tcompiler_conv(H5T_NATIVE_FLOAT16, H5T_STD_I32BE) != true) {
                H5_FAILED();
                printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_STD_I32BE was not a hard conversion\n");
                goto error;
            }
        }
    }

    if (H5Tcompiler_conv(H5T_NATIVE_FLOAT16, H5T_NATIVE_SCHAR) != true) {
        H5_FAILED();
        printf("Conversion path for H5T_NATIVE_FLOAT16 -> H5T_NATIVE_SCHAR was not a hard conversion\n");
        goto error;
    }

    /*
     * Ensure that conversion between standard _Float16 datatypes and a
     * couple of other datatypes are the correct type of conversions.
     */
    if (is_little_endian) {
        if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_NATIVE_FLOAT) != true) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16LE -> H5T_NATIVE_FLOAT was not a hard conversion\n");
            goto error;
        }

        /* Check for a native type that matches H5T_IEEE_F32LE before
         * checking for a hard conversion path
         */
        if (H5Tequal(H5T_NATIVE_FLOAT, H5T_IEEE_F32LE) == true ||
            H5Tequal(H5T_NATIVE_DOUBLE, H5T_IEEE_F32LE) == true ||
            H5Tequal(H5T_NATIVE_LDOUBLE, H5T_IEEE_F32LE) == true) {
            if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_IEEE_F32LE) != true) {
                H5_FAILED();
                printf("Conversion path for H5T_IEEE_F16LE -> H5T_IEEE_F32LE was not a hard conversion\n");
                goto error;
            }
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_IEEE_F32BE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16LE -> H5T_IEEE_F32BE was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_NATIVE_FLOAT) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16BE -> H5T_NATIVE_FLOAT was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_IEEE_F32BE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16BE -> H5T_IEEE_F32BE was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_IEEE_F32LE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16BE -> H5T_IEEE_F32LE was not a soft conversion\n");
            goto error;
        }
    }
    else {
        /* big-endian */
        if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_NATIVE_FLOAT) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16LE -> H5T_NATIVE_FLOAT was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_IEEE_F32LE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16LE -> H5T_IEEE_F32LE was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16LE, H5T_IEEE_F32BE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16LE -> H5T_IEEE_F32BE was not a soft conversion\n");
            goto error;
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_NATIVE_FLOAT) != true) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16BE -> H5T_NATIVE_FLOAT was not a hard conversion\n");
            goto error;
        }

        /* Check for a native type that matches H5T_IEEE_F32BE before
         * checking for a hard conversion path
         */
        if (H5Tequal(H5T_NATIVE_FLOAT, H5T_IEEE_F32LE) == true ||
            H5Tequal(H5T_NATIVE_DOUBLE, H5T_IEEE_F32LE) == true ||
            H5Tequal(H5T_NATIVE_LDOUBLE, H5T_IEEE_F32LE) == true) {
            if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_IEEE_F32BE) != true) {
                H5_FAILED();
                printf("Conversion path for H5T_IEEE_F16BE -> H5T_IEEE_F32BE was not a hard conversion\n");
                goto error;
            }
        }

        if (H5Tcompiler_conv(H5T_IEEE_F16BE, H5T_IEEE_F32LE) != false) {
            H5_FAILED();
            printf("Conversion path for H5T_IEEE_F16BE -> H5T_IEEE_F32LE was not a soft conversion\n");
            goto error;
        }
    }

    /*
     * Create a dataset with the datatype and check the dataset raw
     * data storage size, as well as the file size
     */
    h5_fixname(FILENAME[11], H5P_DEFAULT, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create file!\n");
        goto error;
    }

    dims[0] = 10000;
    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create dataspace\n");
        goto error;
    }

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create DCPL\n");
        goto error;
    }

    if (H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0) {
        H5_FAILED();
        AT();
        printf("Can't set alloc time\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(fid, "Dataset", H5T_NATIVE_FLOAT16, space_id, H5P_DEFAULT, dcpl_id,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    }

    if (H5Dget_storage_size(dset_id) != dims[0] * sizeof(H5__Float16)) {
        H5_FAILED();
        AT();
        printf("Incorrect dataset raw data storage size allocated in file\n");
        goto error;
    }

    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (!h5_driver_uses_multiple_files(driver_name, H5_EXCLUDE_NON_MULTIPART_DRIVERS)) {
        bool is_default_vfd_compat = false;

        if (h5_driver_is_default_vfd_compatible(H5P_DEFAULT, &is_default_vfd_compat) < 0)
            TEST_ERROR;
        if (is_default_vfd_compat) {
            h5_stat_size_t file_size = h5_get_file_size(filename, H5P_DEFAULT);

            if (file_size < 0)
                TEST_ERROR;
            if ((size_t)file_size < dims[0] * sizeof(H5__Float16)) {
                H5_FAILED();
                AT();
                printf("File size value was too small\n");
                goto error;
            }

            /* 4096 bytes is arbitrary, but should suffice for now */
            if ((size_t)file_size > (dims[0] * sizeof(H5__Float16)) + 4096) {
                H5_FAILED();
                AT();
                printf("File size value was too large\n");
                goto error;
            }
        }
    }

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(native_type);
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return 1;
#else
    TESTING("that _Float16 datatype is unavailable");

    /* Make sure H5T_NATIVE_FLOAT16 macro maps to invalid datatype */
    H5E_BEGIN_TRY
    {
        if (0 != H5Tget_size(H5T_NATIVE_FLOAT16)) {
            H5_FAILED();
            AT();
            printf("Valid size was returned for invalid datatype\n");
            return 1;
        }
    }
    H5E_END_TRY

    PASSED();

    return 0;
#endif
}

/*-------------------------------------------------------------------------
 * Function:    test_array_cmpd_vl
 *
 * Purpose:     Tests that conversion occurs correctly with an array of
 *              arrays of compounds containing a variable length sequence.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_array_cmpd_vl(void)
{
    typedef struct cmpd_struct {
        hvl_t vl;
    } cmpd_struct;

    int         int_wdata[2][3][2] = {{{0, 1}, {2, 3}, {4, 5}}, {{6, 7}, {8, 9}, {10, 11}}};
    cmpd_struct wdata[2][3];
    cmpd_struct rdata[2][3];
    hid_t       file;
    hid_t       vl_tid, cmpd_tid, inner_array_tid, outer_array_tid;
    hid_t       space_id;
    hid_t       dset_id;
    hsize_t     dim1[1];
    char        filename[1024];

    TESTING("array of arrays of compounds with a vlen");

    /* Create File */
    h5_fixname(FILENAME[12], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create file!\n");
        goto error;
    } /* end if */

    /* Create VL of ints datatype */
    if ((vl_tid = H5Tvlen_create(H5T_NATIVE_INT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    /* Create compound datatype */
    if ((cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd_struct))) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    if (H5Tinsert(cmpd_tid, "vl", HOFFSET(struct cmpd_struct, vl), vl_tid) < 0) {
        H5_FAILED();
        AT();
        printf("Can't insert field 'vl'\n");
        goto error;
    } /* end if */

    /* Create inner array type */
    dim1[0] = 3;
    if ((inner_array_tid = H5Tarray_create2(cmpd_tid, 1, dim1)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    /* Create outer array type */
    dim1[0] = 2;
    if ((outer_array_tid = H5Tarray_create2(inner_array_tid, 1, dim1)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create datatype!\n");
        goto error;
    } /* end if */

    /* Create space, dataset */
    dim1[0] = 1;
    if ((space_id = H5Screate_simple(1, dim1, NULL)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create space\n");
        goto error;
    } /* end if */

    if ((dset_id = H5Dcreate2(file, "Dataset", outer_array_tid, space_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        AT();
        printf("Can't create dataset\n");
        goto error;
    } /* end if */

    /* Initialize wdata */
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 3; j++) {
            wdata[i][j].vl.len = 2;
            wdata[i][j].vl.p   = int_wdata[i][j];
        }

    /* Write data */
    if (H5Dwrite(dset_id, outer_array_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    /* Initialize rdata */
    (void)memset(rdata, 0, sizeof(rdata));

    /* Read data */
    if (H5Dread(dset_id, outer_array_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    /* Check for correctness of read data */
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 3; j++)
            if (rdata[i][j].vl.len != 2 || ((int *)rdata[i][j].vl.p)[0] != int_wdata[i][j][0] ||
                ((int *)rdata[i][j].vl.p)[1] != int_wdata[i][j][1]) {
                H5_FAILED();
                AT();
                printf("incorrect read data at [%d][%d]\n", i, j);
                goto error;
            }

    /* Reclaim memory */
    if (H5Treclaim(outer_array_tid, space_id, H5P_DEFAULT, rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim memory\n");
        goto error;
    } /* end if */

    /* Adjust write buffer */
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 3; j++) {
            int_wdata[i][j][0] += 100;
            int_wdata[i][j][1] += 100;
        }

    /* Overwrite dataset with adjusted wdata */
    if (H5Dwrite(dset_id, outer_array_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't write data\n");
        goto error;
    } /* end if */

    /* Initialize rdata */
    (void)memset(rdata, 0, sizeof(rdata));

    /* Read data */
    if (H5Dread(dset_id, outer_array_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't read data\n");
        goto error;
    } /* end if */

    /* Check for correctness of read data */
    for (int i = 0; i < 2; i++)
        for (int j = 0; j < 3; j++)
            if (rdata[i][j].vl.len != 2 || ((int *)rdata[i][j].vl.p)[0] != int_wdata[i][j][0] ||
                ((int *)rdata[i][j].vl.p)[1] != int_wdata[i][j][1]) {
                H5_FAILED();
                AT();
                printf("incorrect read data at [%d][%d]\n", i, j);
                goto error;
            }

    /* Reclaim memory */
    if (H5Treclaim(outer_array_tid, space_id, H5P_DEFAULT, rdata) < 0) {
        H5_FAILED();
        AT();
        printf("Can't reclaim memory\n");
        goto error;
    } /* end if */

    /* Close */
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Tclose(outer_array_tid) < 0)
        goto error;
    if (H5Tclose(inner_array_tid) < 0)
        goto error;
    if (H5Tclose(cmpd_tid) < 0)
        goto error;
    if (H5Tclose(vl_tid) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    return 1;
} /* end test_array_cmpd_vl() */

/*-------------------------------------------------------------------------
 * Function:    test_encode
 *
 * Purpose:     Tests functions of encoding and decoding datatype.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *-------------------------------------------------------------------------
 */
static int
test_encode(void)
{
    struct cmpd {
        int    a;
        float  b;
        long   c;
        double d;
    };
    hid_t          file         = H5I_INVALID_HID;
    hid_t          tid1         = H5I_INVALID_HID;
    hid_t          tid2         = H5I_INVALID_HID;
    hid_t          tid3         = H5I_INVALID_HID;
    hid_t          decoded_tid1 = H5I_INVALID_HID;
    hid_t          decoded_tid2 = H5I_INVALID_HID;
    hid_t          decoded_tid3 = H5I_INVALID_HID;
    char           filename[1024];
    char           compnd_type[] = "Compound_type";
    char           enum_type[]   = "Enum_type";
    char           vlstr_type[]  = "VLstring_type";
    short          enum_val;
    size_t         cmpd_buf_size  = 0;
    size_t         enum_buf_size  = 0;
    size_t         vlstr_buf_size = 0;
    unsigned char *cmpd_buf       = NULL;
    unsigned char *enum_buf       = NULL;
    unsigned char *vlstr_buf      = NULL;
    hid_t          ret_id;
    herr_t         ret;

    TESTING("functions of encoding and decoding datatypes");

    /* Create File */
    h5_fixname(FILENAME[5], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /*-----------------------------------------------------------------------
     * Create compound, enumerate, and VL string datatypes
     *-----------------------------------------------------------------------
     */
    /* Create a compound datatype */
    if ((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct cmpd))) < 0) {
        H5_FAILED();
        printf("Can't create datatype!\n");
        goto error;
    }
    if (H5Tinsert(tid1, "a", HOFFSET(struct cmpd, a), H5T_NATIVE_INT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'a'\n");
        goto error;
    }
    if (H5Tinsert(tid1, "b", HOFFSET(struct cmpd, b), H5T_NATIVE_FLOAT) < 0) {
        H5_FAILED();
        printf("Can't insert field 'b'\n");
        goto error;
    }
    if (H5Tinsert(tid1, "c", HOFFSET(struct cmpd, c), H5T_NATIVE_LONG) < 0) {
        H5_FAILED();
        printf("Can't insert field 'c'\n");
        goto error;
    }
    if (H5Tinsert(tid1, "d", HOFFSET(struct cmpd, d), H5T_NATIVE_DOUBLE) < 0) {
        H5_FAILED();
        printf("Can't insert field 'd'\n");
        goto error;
    }

    /* Create a enumerate datatype */
    if ((tid2 = H5Tcreate(H5T_ENUM, sizeof(short))) < 0) {
        H5_FAILED();
        printf("Can't create enumerate type\n");
        goto error;
    }
    enum_val = 0;
    if (H5Tenum_insert(tid2, "RED", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    }
    enum_val = 1;
    if (H5Tenum_insert(tid2, "GREEN", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    }
    enum_val = 2;
    if (H5Tenum_insert(tid2, "BLUE", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    }
    enum_val = 3;
    if (H5Tenum_insert(tid2, "ORANGE", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    }
    enum_val = 4;
    if (H5Tenum_insert(tid2, "YELLOW", &enum_val) < 0) {
        H5_FAILED();
        printf("Can't insert field into enumeration type\n");
        goto error;
    }

    /* Create a variable-length string type */
    if ((tid3 = H5Tcopy(H5T_C_S1)) < 0) {
        H5_FAILED();
        printf("Can't copy a string type\n");
        goto error;
    }
    if (H5Tset_size(tid3, H5T_VARIABLE) < 0) {
        H5_FAILED();
        printf("Can't the string type to be variable-length\n");
        goto error;
    }

    /*-----------------------------------------------------------------------
     * Test encoding and decoding compound, enumerate, and VL string datatypes
     *-----------------------------------------------------------------------
     */
    /* Encode compound type in a buffer */
    if (H5Tencode(tid1, NULL, &cmpd_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode compound type\n");
        goto error;
    }

    if (cmpd_buf_size > 0)
        cmpd_buf = (unsigned char *)calloc((size_t)1, cmpd_buf_size);

    /* Try decoding an incorrect (empty) buffer (should fail) */
    H5E_BEGIN_TRY
    {
        ret_id = H5Tdecode(cmpd_buf);
    }
    H5E_END_TRY
    if (ret_id != FAIL) {
        H5_FAILED();
        printf("Decoded an empty buffer!\n");
        goto error;
    }

    if (H5Tencode(tid1, cmpd_buf, &cmpd_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode compound type\n");
        goto error;
    }

    /* Decode from the compound buffer and return an object handle */
    if ((decoded_tid1 = H5Tdecode(cmpd_buf)) < 0)
        FAIL_PUTS_ERROR("Can't decode compound type\n");

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid1, tid1) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /* Query member number and member index by name, for compound type. */
    if (H5Tget_nmembers(decoded_tid1) != 4) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    }
    if (H5Tget_member_index(decoded_tid1, "c") != 2) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    }

    /* Encode enumerate type in a buffer */
    if (H5Tencode(tid2, NULL, &enum_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode enumerate type\n");
        goto error;
    }

    if (enum_buf_size > 0)
        enum_buf = (unsigned char *)calloc((size_t)1, enum_buf_size);

    if (H5Tencode(tid2, enum_buf, &enum_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode enumerate type\n");
        goto error;
    }

    /* Decode from the enumerate buffer and return an object handle */
    if ((decoded_tid2 = H5Tdecode(enum_buf)) < 0) {
        H5_FAILED();
        printf("Can't decode enumerate type\n");
        goto error;
    }

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid2, tid2) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /* Query member number and member index by name, for enumeration type. */
    if (H5Tget_nmembers(decoded_tid2) != 5) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    }
    if (H5Tget_member_index(decoded_tid2, "ORANGE") != 3) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    }

    /* Encode VL string type in a buffer */
    if (H5Tencode(tid3, NULL, &vlstr_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode VL string type\n");
        goto error;
    }

    if (vlstr_buf_size > 0)
        vlstr_buf = (unsigned char *)calloc((size_t)1, vlstr_buf_size);

    if (H5Tencode(tid3, vlstr_buf, &vlstr_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode VL string type\n");
        goto error;
    }

    /* Decode from the VL string buffer and return an object handle */
    if ((decoded_tid3 = H5Tdecode(vlstr_buf)) < 0) {
        H5_FAILED();
        printf("Can't decode VL string type\n");
        goto error;
    }

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid3, tid3) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }
    if (!H5Tis_variable_str(decoded_tid3)) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /*-----------------------------------------------------------------------
     * Commit and reopen the compound, enumerate, VL string datatypes
     *-----------------------------------------------------------------------
     */
    /* Commit compound datatype and close it */
    if (H5Tcommit2(file, compnd_type, tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit compound datatype\n");
        goto error;
    }
    if (H5Tclose(tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    if (H5Tclose(decoded_tid1) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    free(cmpd_buf);
    cmpd_buf_size = 0;

    /* Commit enumeration datatype and close it */
    if (H5Tcommit2(file, enum_type, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit compound datatype\n");
        goto error;
    }
    if (H5Tclose(tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    if (H5Tclose(decoded_tid2) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    free(enum_buf);
    enum_buf_size = 0;

    /* Commit enumeration datatype and close it */
    if (H5Tcommit2(file, vlstr_type, tid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0) {
        H5_FAILED();
        printf("Can't commit vl string datatype\n");
        goto error;
    }
    if (H5Tclose(tid3) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    if (H5Tclose(decoded_tid3) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }
    free(vlstr_buf);
    vlstr_buf_size = 0;

    /* Open the dataytpe for query */
    if ((tid1 = H5Topen2(file, compnd_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((tid2 = H5Topen2(file, enum_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((tid3 = H5Topen2(file, vlstr_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /*-----------------------------------------------------------------------
     * Test encoding and decoding compound, enumerate, and vl string datatypes
     *-----------------------------------------------------------------------
     */
    /* Encode compound type in a buffer */
    if (H5Tencode(tid1, NULL, &cmpd_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode compound type\n");
        goto error;
    }

    if (cmpd_buf_size > 0)
        cmpd_buf = (unsigned char *)calloc((size_t)1, cmpd_buf_size);

    if (H5Tencode(tid1, cmpd_buf, &cmpd_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode compound type\n");
        goto error;
    }

    /* Decode from the compound buffer and return an object handle */
    if ((decoded_tid1 = H5Tdecode(cmpd_buf)) < 0)
        FAIL_PUTS_ERROR("Can't decode compound type\n");

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid1, tid1) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /* Query member number and member index by name, for compound type. */
    if (H5Tget_nmembers(decoded_tid1) != 4) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    }
    if (H5Tget_member_index(decoded_tid1, "c") != 2) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    }

    /* Encode enumerate type in a buffer */
    if (H5Tencode(tid2, NULL, &enum_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode enumerate type\n");
        goto error;
    }

    if (enum_buf_size > 0)
        enum_buf = (unsigned char *)calloc((size_t)1, enum_buf_size);

    if (H5Tencode(tid2, enum_buf, &enum_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode enumerate type\n");
        goto error;
    }

    /* Decode from the enumerate buffer and return an object handle */
    if ((decoded_tid2 = H5Tdecode(enum_buf)) < 0) {
        H5_FAILED();
        printf("Can't decode enumerate type\n");
        goto error;
    }

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid2, tid2) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /* Query member number and member index by name, for enumeration type. */
    if (H5Tget_nmembers(decoded_tid2) != 5) {
        H5_FAILED();
        printf("Can't get member number\n");
        goto error;
    }
    if (H5Tget_member_index(decoded_tid2, "ORANGE") != 3) {
        H5_FAILED();
        printf("Can't get correct index number\n");
        goto error;
    }

    /* Encode VL string type in a buffer */
    if (H5Tencode(tid3, NULL, &vlstr_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode VL string type\n");
        goto error;
    }

    if (vlstr_buf_size > 0)
        vlstr_buf = (unsigned char *)calloc((size_t)1, vlstr_buf_size);

    if (H5Tencode(tid3, vlstr_buf, &vlstr_buf_size) < 0) {
        H5_FAILED();
        printf("Can't encode VL string type\n");
        goto error;
    }

    /* Decode from the VL string buffer and return an object handle */
    if ((decoded_tid3 = H5Tdecode(vlstr_buf)) < 0) {
        H5_FAILED();
        printf("Can't decode VL string type\n");
        goto error;
    }
    free(vlstr_buf);

    /* Verify that the datatype was copied exactly */
    if (H5Tequal(decoded_tid3, tid3) <= 0) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }
    if (!H5Tis_variable_str(decoded_tid3)) {
        H5_FAILED();
        printf("Datatype wasn't encoded & decoded identically\n");
        goto error;
    }

    /*-----------------------------------------------------------------------
     * Test the reference count of the decoded datatypes
     *-----------------------------------------------------------------------
     */

    /* Make sure the reference counts for the decoded datatypes are one. */
    if (H5Iget_ref(decoded_tid1) != 1) {
        H5_FAILED();
        printf("Decoded datatype has incorrect reference count\n");
        goto error;
    }

    if (H5Iget_ref(decoded_tid2) != 1) {
        H5_FAILED();
        printf("Decoded datatype has incorrect reference count\n");
        goto error;
    }

    if (H5Iget_ref(decoded_tid3) != 1) {
        H5_FAILED();
        printf("Decoded datatype has incorrect reference count\n");
        goto error;
    }

    /* Make sure the reference counts for the decoded datatypes can be
     * decremented and the datatypes are closed. */
    if (H5Idec_ref(decoded_tid1) != 0) {
        H5_FAILED();
        printf("Decoded datatype can't close\n");
        goto error;
    }

    if (H5Idec_ref(decoded_tid2) != 0) {
        H5_FAILED();
        printf("Decoded datatype can't close\n");
        goto error;
    }

    if (H5Idec_ref(decoded_tid3) != 0) {
        H5_FAILED();
        printf("Decoded datatype can't close\n");
        goto error;
    }

    /* Make sure the decoded datatypes are already closed. */
    H5E_BEGIN_TRY
    {
        ret = H5Tclose(decoded_tid1);
    }
    H5E_END_TRY
    if (ret != FAIL) {
        H5_FAILED();
        printf("Decoded datatype should have been closed\n");
        goto error;
    }

    H5E_BEGIN_TRY
    {
        ret = H5Tclose(decoded_tid2);
    }
    H5E_END_TRY
    if (ret != FAIL) {
        H5_FAILED();
        printf("Decoded datatype should have been closed\n");
        goto error;
    }

    H5E_BEGIN_TRY
    {
        ret = H5Tclose(decoded_tid3);
    }
    H5E_END_TRY
    if (ret != FAIL) {
        H5_FAILED();
        printf("Decoded datatype should have been closed\n");
        goto error;
    }

    /*-----------------------------------------------------------------------
     * Close and release
     *-----------------------------------------------------------------------
     */
    /* Close datatype and file */
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
    if (H5Tclose(tid3) < 0) {
        H5_FAILED();
        printf("Can't close datatype\n");
        goto error;
    }

    if (H5Fclose(file) < 0) {
        H5_FAILED();
        printf("Can't close file\n");
        goto error;
    }

    free(cmpd_buf);
    free(enum_buf);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid1);
        H5Tclose(tid2);
        H5Tclose(tid3);
        H5Tclose(decoded_tid1);
        H5Tclose(decoded_tid2);
        H5Tclose(decoded_tid3);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_latest
 *
 * Purpose:     Test encoding datatypes with the "use the latest version of
 *              the file format" flag turned on.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_latest(void)
{
    struct s1 {
        int    a;
        float  b;
        long   c;
        double d;
    };
    hid_t             file = (H5I_INVALID_HID);                           /* File ID */
    hid_t             tid1 = (H5I_INVALID_HID), tid2 = (H5I_INVALID_HID); /* Datatype ID */
    hid_t             fapl = (H5I_INVALID_HID);                           /* File access property list */
    H5O_native_info_t oi;                              /* Stat buffer for committed datatype */
    hsize_t           old_dtype_oh_size;               /* Size of object header with "old" format */
    hsize_t           new_dtype_oh_size;               /* Size of object header with "new" format */
    char              filename[1024];                  /* Buffer for filename */
    const char        compnd_type[] = "Compound_type"; /* Name of committed datatype */

    TESTING("encoding datatypes with the 'use the latest format' flag");

    /* Create a compound datatype */
    if ((tid1 = H5Tcreate(H5T_COMPOUND, sizeof(struct s1))) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(tid1, "a", HOFFSET(struct s1, a), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(tid1, "b", HOFFSET(struct s1, b), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(tid1, "c", HOFFSET(struct s1, c), H5T_NATIVE_LONG) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(tid1, "d", HOFFSET(struct s1, d), H5T_NATIVE_DOUBLE) < 0)
        FAIL_STACK_ERROR;

    /* Create file using default FAPL */
    h5_fixname(FILENAME[5], H5P_DEFAULT, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Make a copy of the datatype, to commit */
    if ((tid2 = H5Tcopy(tid1)) < 0)
        FAIL_STACK_ERROR;

    /* Commit compound datatype */
    if (H5Tcommit2(file, compnd_type, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Get information about datatype on disk */
    if (H5Oget_native_info_by_name(file, compnd_type, &oi, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;
    old_dtype_oh_size = oi.hdr.space.total;

    /* Close datatype */
    if (H5Tclose(tid2) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    /* Check that datatype has been encoded/decoded correctly */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataytpe for query */
    if ((tid2 = H5Topen2(file, compnd_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Verify that the datatype was encoded/decoded correctly */
    if (H5Tequal(tid1, tid2) <= 0)
        FAIL_STACK_ERROR;

    /* Get information about datatype on disk */
    if (H5Oget_native_info_by_name(file, compnd_type, &oi, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Check that the object header info is still the same */
    if (old_dtype_oh_size != oi.hdr.space.total)
        TEST_ERROR;

    /* Close datatype */
    if (H5Tclose(tid2) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    /* Set the 'use the latest format' bounds in the FAPL */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create file using default FAPL */
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Make a copy of the datatype, to commit */
    if ((tid2 = H5Tcopy(tid1)) < 0)
        FAIL_STACK_ERROR;

    /* Commit compound datatype */
    if (H5Tcommit2(file, compnd_type, tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Get information about datatype on disk */
    if (H5Oget_native_info_by_name(file, compnd_type, &oi, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;
    new_dtype_oh_size = oi.hdr.space.total;

    /* Check that the new format is smaller than the old format */
    if (old_dtype_oh_size <= new_dtype_oh_size)
        TEST_ERROR;

    /* Close datatype */
    if (H5Tclose(tid2) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    /* Check that datatype has been encoded/decoded correctly */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataytpe for query */
    if ((tid2 = H5Topen2(file, compnd_type, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Verify that the datatype was encoded/decoded correctly */
    if (H5Tequal(tid1, tid2) <= 0)
        FAIL_STACK_ERROR;

    /* Get information about datatype on disk */
    if (H5Oget_native_info_by_name(file, compnd_type, &oi, H5O_NATIVE_INFO_HDR, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Check that the object header info is still the same */
    if (new_dtype_oh_size != oi.hdr.space.total)
        TEST_ERROR;

    /* Close datatype */
    if (H5Tclose(tid2) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    /* Close FAPL */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close datatype */
    if (H5Tclose(tid1) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(tid2);
        H5Tclose(tid1);
        H5Fclose(file);
        H5Pclose(fapl);
    }
    H5E_END_TRY

    return 1;
} /* end test_latest() */

typedef struct {
    unsigned num_range_hi;  /* Number of H5T_CONV_EXCEPT_RANGE_HI exceptions seen */
    unsigned num_range_low; /* Number of H5T_CONV_EXCEPT_RANGE_LOW exceptions seen */
    unsigned num_precision; /* Number of H5T_CONV_EXCEPT_PRECISION exceptions seen */
    unsigned num_truncate;  /* Number of H5T_CONV_EXCEPT_TRUNCATE exceptions seen */
    unsigned num_other;     /* Number of other exceptions seen */
} except_info_t;

static H5T_conv_ret_t
conv_except(H5T_conv_except_t except_type, hid_t H5_ATTR_UNUSED src_id, hid_t H5_ATTR_UNUSED dst_id,
            void H5_ATTR_UNUSED *src_buf, void H5_ATTR_UNUSED *dst_buf, void *_user_data)
{
    except_info_t *user_data = (except_info_t *)_user_data;

    if (except_type == H5T_CONV_EXCEPT_RANGE_HI)
        user_data->num_range_hi++;
    else if (except_type == H5T_CONV_EXCEPT_RANGE_LOW)
        user_data->num_range_low++;
    else if (except_type == H5T_CONV_EXCEPT_PRECISION)
        user_data->num_precision++;
    else if (except_type == H5T_CONV_EXCEPT_TRUNCATE)
        user_data->num_truncate++;
    else
        user_data->num_other++;

    return (H5T_CONV_UNHANDLED);
}

/*-------------------------------------------------------------------------
 * Function:    test_int_float_except
 *
 * Purpose:     Tests exception handling behavior of int <-> float
 *              conversions.
 *
 * Return:      Success:        0
 *
 *              Failure:        number of errors
 *
 * Notes: This routine is pretty specific to 4 byte integers and 4 byte
 *              floats and I can't think of a particularly good way to
 *              make it portable to other architectures, but further
 *              input and changes are welcome.  -QAK
 *
 *-------------------------------------------------------------------------
 */
static int
test_int_float_except(void)
{
#if H5_SIZEOF_INT == 4 && H5_SIZEOF_FLOAT == 4
    float  buf[CONVERT_SIZE]       = {(float)INT_MIN - 172.0F, (float)INT_MAX - 32.0F, (float)INT_MAX - 68.0F,
                               (float)4.5F};
    int    buf_int[CONVERT_SIZE]   = {INT_MIN, INT_MAX, INT_MAX - 127, 4};
    float  buf_float[CONVERT_SIZE] = {(float)INT_MIN, (float)INT_MAX + 1.0F, (float)INT_MAX - 127.0F, 4};
    int   *intp; /* Pointer to buffer, as integers */
    int    buf2[CONVERT_SIZE]       = {INT_MIN, INT_MAX, INT_MAX - 72, 0};
    float  buf2_float[CONVERT_SIZE] = {(float)INT_MIN, (float)INT_MAX, (float)INT_MAX - 127.0F, (float)0.0F};
    int    buf2_int[CONVERT_SIZE]   = {INT_MIN, INT_MAX, INT_MAX - 127, 0};
    float *floatp;   /* Pointer to buffer #2, as floats */
    hid_t  dxpl;     /* Dataset transfer property list */
    except_info_t e; /* Exception information */
    unsigned      u; /* Local index variables */
#endif               /* H5_SIZEOF_INT==4 && H5_SIZEOF_FLOAT==4 */

    TESTING("exceptions for int <-> float conversions");

#if H5_SIZEOF_INT == 4 && H5_SIZEOF_FLOAT == 4
    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Set the conversion exception handler in the DXPL */
    if (H5Pset_type_conv_cb(dxpl, conv_except, &e) < 0)
        TEST_ERROR;

    /* Convert buffer */
    memset(&e, 0, sizeof(except_info_t));
    if (H5Tconvert(H5T_NATIVE_FLOAT, H5T_NATIVE_INT, (size_t)CONVERT_SIZE, buf, NULL, dxpl) < 0)
        TEST_ERROR;

    /* Check the buffer after conversion, as integers */
    for (u = 0; u < CONVERT_SIZE; u++) {
        intp = (int *)&buf[u];
        if (*intp != buf_int[u])
            TEST_ERROR;
    } /* end for */

    /* Check for proper exceptions */
    if (e.num_range_hi != 1)
        TEST_ERROR;
    if (e.num_range_low != 1)
        TEST_ERROR;
    if (e.num_precision != 0)
        TEST_ERROR;
    if (e.num_truncate != 1)
        TEST_ERROR;
    if (e.num_other != 0)
        TEST_ERROR;

    /* Convert buffer */
    memset(&e, 0, sizeof(except_info_t));
    if (H5Tconvert(H5T_NATIVE_INT, H5T_NATIVE_FLOAT, (size_t)CONVERT_SIZE, buf, NULL, dxpl) < 0)
        TEST_ERROR;

    /* Check the buffer after conversion, as floats */
    for (u = 0; u < CONVERT_SIZE; u++) {
        floatp = (float *)&buf[u];
        if (!H5_FLT_ABS_EQUAL(*floatp, buf_float[u]))
            TEST_ERROR;
    } /* end for */

    /* Check for proper exceptions */
    if (e.num_range_hi != 0)
        TEST_ERROR;
    if (e.num_range_low != 0)
        TEST_ERROR;
    if (e.num_precision != 1)
        TEST_ERROR;
    if (e.num_truncate != 0)
        TEST_ERROR;
    if (e.num_other != 0)
        TEST_ERROR;

    /* Work on second buffer */

    /* Convert second buffer */
    memset(&e, 0, sizeof(except_info_t));
    if (H5Tconvert(H5T_NATIVE_INT, H5T_NATIVE_FLOAT, (size_t)CONVERT_SIZE, buf2, NULL, dxpl) < 0)
        TEST_ERROR;

    /* Check the buffer after conversion, as floats */
    for (u = 0; u < CONVERT_SIZE; u++) {
        floatp = (float *)&buf2[u];
        if (!H5_FLT_ABS_EQUAL(*floatp, buf2_float[u]))
            TEST_ERROR;
    } /* end for */

    /* Check for proper exceptions */
    if (e.num_range_hi != 0)
        TEST_ERROR;
    if (e.num_range_low != 0)
        TEST_ERROR;
    if (e.num_precision != 2)
        TEST_ERROR;
    if (e.num_truncate != 0)
        TEST_ERROR;
    if (e.num_other != 0)
        TEST_ERROR;

    /* Convert buffer */
    memset(&e, 0, sizeof(except_info_t));
    if (H5Tconvert(H5T_NATIVE_FLOAT, H5T_NATIVE_INT, (size_t)CONVERT_SIZE, buf2, NULL, dxpl) < 0)
        TEST_ERROR;

    /* Check the buffer after conversion, as integers */
    for (u = 0; u < CONVERT_SIZE; u++) {
        intp = (int *)&buf2[u];
        if (*intp != buf2_int[u])
            TEST_ERROR;
    } /* end for */

    /* Check for proper exceptions */
    if (e.num_range_hi != 1)
        TEST_ERROR;
    if (e.num_range_low != 0)
        TEST_ERROR;
    if (e.num_precision != 0)
        TEST_ERROR;
    if (e.num_truncate != 0)
        TEST_ERROR;
    if (e.num_other != 0)
        TEST_ERROR;

    /* Close DXPL */
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    PASSED();
#else  /* H5_SIZEOF_INT==4 && H5_SIZEOF_FLOAT==4 */
    SKIPPED();
    puts("    Test skipped due to int or float not 4 bytes.");
#endif /* H5_SIZEOF_INT==4 && H5_SIZEOF_FLOAT==4 */
    return 0;

#if H5_SIZEOF_INT == 4 && H5_SIZEOF_FLOAT == 4
error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dxpl);
    }
    H5E_END_TRY
    return 1;
#endif /* H5_SIZEOF_INT==4 && H5_SIZEOF_FLOAT==4 */
} /* end test_int_float_except() */

/*-------------------------------------------------------------------------
 * Function:    test_app_conv_ids_func
 *
 * Purpose:     Conversion function for test_app_conv_ids test that calls
 *              H5Tget_class on the ID for the source and destination
 *              datatypes to try to make sure they're valid.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_app_conv_ids_func(hid_t src_id, hid_t dst_id, H5T_cdata_t *cdata, size_t H5_ATTR_UNUSED nelmts,
                       size_t H5_ATTR_UNUSED buf_stride, size_t H5_ATTR_UNUSED bkg_stride,
                       void H5_ATTR_UNUSED *buf, void H5_ATTR_UNUSED *bkg, hid_t H5_ATTR_UNUSED dxpl)
{
    if (cdata->command == H5T_CONV_CONV) {
        if (H5Tget_class(src_id) < 0)
            return FAIL;
        if (H5Tget_class(dst_id) < 0)
            return FAIL;
    }

    return SUCCEED;
}

/*-------------------------------------------------------------------------
 * Function:    test_app_conv_ids
 *
 * Purpose:     Tests that the IDs passed to an application conversion
 *              function for different datatypes are valid.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_app_conv_ids(void)
{
    const size_t buf_size     = 1024; /* Must be big enough to hold element of largest datatype */
    hsize_t      array_dims[] = {1};
    hsize_t      dims[]       = {1};
    hid_t        src_type_id  = H5I_INVALID_HID;
    hid_t        dst_type_id  = H5I_INVALID_HID;
    hid_t        space_id     = H5I_INVALID_HID;
    hvl_t        vl_elem      = {0};
    int          vl_int       = 0;
    void        *conv_elem    = NULL;
    void        *bkg_buf      = NULL;

    TESTING("passing datatype IDs to application conversion function");

    if (NULL == (conv_elem = malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (bkg_buf = malloc(buf_size)))
        TEST_ERROR;

    vl_elem.len = 1;
    vl_elem.p   = &vl_int;

    for (int src_type = H5T_INTEGER; src_type < H5T_NCLASSES; src_type++) {
        switch ((H5T_class_t)src_type) {
            case H5T_BITFIELD:
                if ((src_type_id = H5Tcopy(H5T_STD_B32LE)) < 0)
                    TEST_ERROR;
                break;
            case H5T_REFERENCE:
                if ((src_type_id = H5Tcopy(H5T_STD_REF)) < 0)
                    TEST_ERROR;
                break;
            case H5T_ENUM:
                if ((src_type_id = H5Tenum_create(H5T_NATIVE_INT)) < 0)
                    TEST_ERROR;
                break;
            case H5T_VLEN:
                if ((src_type_id = H5Tvlen_create(H5T_NATIVE_INT)) < 0)
                    TEST_ERROR;
                break;
            case H5T_ARRAY:
                if ((src_type_id = H5Tarray_create2(H5T_NATIVE_INT, 1, array_dims)) < 0)
                    TEST_ERROR;
                break;
            case H5T_INTEGER:
            case H5T_FLOAT:
            case H5T_TIME:
            case H5T_STRING:
            case H5T_OPAQUE:
            case H5T_COMPOUND:
            default:
                if ((src_type_id = H5Tcreate((H5T_class_t)src_type, buf_size / 2)) < 0)
                    TEST_ERROR;
                break;

            case H5T_NO_CLASS:
            case H5T_NCLASSES:
                TEST_ERROR;
        }

        for (int dst_type = H5T_INTEGER; dst_type < H5T_NCLASSES; dst_type++) {
            /* Use different datatype sizes for dest type so we don't convert with no-op function */
            switch ((H5T_class_t)dst_type) {
                case H5T_BITFIELD:
                    if ((dst_type_id = H5Tcopy(H5T_STD_B64LE)) < 0)
                        TEST_ERROR;
                    break;
                case H5T_REFERENCE:
                    if ((dst_type_id = H5Tcopy(H5T_STD_REF)) < 0)
                        TEST_ERROR;
                    break;
                case H5T_ENUM:
                    if ((dst_type_id = H5Tenum_create(H5T_NATIVE_LONG)) < 0)
                        TEST_ERROR;
                    break;
                case H5T_VLEN:
                    if ((dst_type_id = H5Tvlen_create(H5T_NATIVE_LONG)) < 0)
                        TEST_ERROR;
                    break;
                case H5T_ARRAY:
                    if ((dst_type_id = H5Tarray_create2(H5T_NATIVE_LONG, 1, array_dims)) < 0)
                        TEST_ERROR;
                    break;
                case H5T_INTEGER:
                case H5T_FLOAT:
                case H5T_TIME:
                case H5T_STRING:
                case H5T_OPAQUE:
                case H5T_COMPOUND:
                default:
                    if ((dst_type_id = H5Tcreate((H5T_class_t)dst_type, buf_size / 4)) < 0)
                        TEST_ERROR;
                    break;

                case H5T_NO_CLASS:
                case H5T_NCLASSES:
                    TEST_ERROR;
            }

            if (H5Tregister(H5T_PERS_SOFT, "app_conv_ids_func", src_type_id, dst_type_id,
                            &test_app_conv_ids_func) < 0)
                TEST_ERROR;

            memset(conv_elem, 0, buf_size);

            if (src_type == H5T_VLEN)
                memcpy(conv_elem, &vl_elem, sizeof(hvl_t));

            if (H5Tconvert(src_type_id, dst_type_id, 1, conv_elem, bkg_buf, H5P_DEFAULT) < 0)
                TEST_ERROR;

            if (H5Tunregister(H5T_PERS_SOFT, "app_conv_ids_func", src_type_id, dst_type_id,
                              &test_app_conv_ids_func) < 0)
                TEST_ERROR;

            if (H5Tclose(dst_type_id) < 0)
                TEST_ERROR;
            dst_type_id = H5I_INVALID_HID;
        }

        if (H5Tclose(src_type_id) < 0)
            TEST_ERROR;
        src_type_id = H5I_INVALID_HID;
    }

    /* Reset library after type conversion path table was potentially modified */
    h5_restore_err();
    reset_hdf5();

    /* Test with container-like datatypes where the conversion on the top-level type
     * is performed with a library-internal conversion function, but conversions on
     * the member types are performed with an application conversion function
     */

    if (H5Tregister(H5T_PERS_HARD, "app_conv_ids_func", H5T_NATIVE_INT, H5T_NATIVE_LONG,
                    &test_app_conv_ids_func) < 0)
        TEST_ERROR;

    /*******************************
     * Top-level compound datatype *
     *******************************/
    if ((src_type_id = H5Tcreate(H5T_COMPOUND, sizeof(int))) < 0)
        TEST_ERROR;
    if (H5Tinsert(src_type_id, "comp_mem", 0, H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if ((dst_type_id = H5Tcreate(H5T_COMPOUND, sizeof(long))) < 0)
        TEST_ERROR;
    if (H5Tinsert(dst_type_id, "comp_mem", 0, H5T_NATIVE_LONG) < 0)
        TEST_ERROR;

    memset(conv_elem, 0, buf_size);
    if (H5Tconvert(src_type_id, dst_type_id, 1, conv_elem, bkg_buf, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Tclose(src_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dst_type_id) < 0)
        TEST_ERROR;

    /***************************
     * Top-level enum datatype *
     ***************************/
    if ((src_type_id = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if ((dst_type_id = H5Tenum_create(H5T_NATIVE_LONG)) < 0)
        TEST_ERROR;

    memset(conv_elem, 0, buf_size);
    if (H5Tconvert(src_type_id, dst_type_id, 1, conv_elem, bkg_buf, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Tclose(src_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dst_type_id) < 0)
        TEST_ERROR;

    /**************************************
     * Top-level variable-length datatype *
     **************************************/
    if ((src_type_id = H5Tvlen_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if ((dst_type_id = H5Tvlen_create(H5T_NATIVE_LONG)) < 0)
        TEST_ERROR;

    memset(conv_elem, 0, buf_size);
    memcpy(conv_elem, &vl_elem, sizeof(hvl_t));
    if (H5Tconvert(src_type_id, dst_type_id, 1, conv_elem, bkg_buf, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if ((space_id = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if (H5Treclaim(dst_type_id, space_id, H5P_DEFAULT, conv_elem) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;

    if (H5Tclose(src_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dst_type_id) < 0)
        TEST_ERROR;

    /****************************
     * Top-level array datatype *
     ****************************/
    if ((src_type_id = H5Tarray_create2(H5T_NATIVE_INT, 1, array_dims)) < 0)
        TEST_ERROR;
    if ((dst_type_id = H5Tarray_create2(H5T_NATIVE_LONG, 1, array_dims)) < 0)
        TEST_ERROR;

    memset(conv_elem, 0, buf_size);
    if (H5Tconvert(src_type_id, dst_type_id, 1, conv_elem, bkg_buf, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if (H5Tclose(src_type_id) < 0)
        TEST_ERROR;
    if (H5Tclose(dst_type_id) < 0)
        TEST_ERROR;

    if (H5Tunregister(H5T_PERS_HARD, "app_conv_ids_func", H5T_NATIVE_INT, H5T_NATIVE_LONG,
                      &test_app_conv_ids_func) < 0)
        TEST_ERROR;

    free(bkg_buf);
    bkg_buf = NULL;
    free(conv_elem);
    conv_elem = NULL;

    /* Reset library after type conversion path table was potentially modified */
    h5_restore_err();
    reset_hdf5();

    PASSED();

    return 0;

error:
    free(bkg_buf);
    free(conv_elem);

    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Tclose(src_type_id);
        H5Tclose(dst_type_id);
    }
    H5E_END_TRY

    /* Reset library after type conversion path table was potentially modified */
    h5_restore_err();
    reset_hdf5();

    return 1;
} /* end test_app_conv_ids() */

/*-------------------------------------------------------------------------
 * Function:    test_set_order
 *
 * Purpose:     Tests H5Tset_order/H5Tget_order.  Verifies that
 *              H5T_ORDER_NONE cannot be set.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_set_order(void)
{
    hid_t       dtype;            /* Datatype ID */
    H5T_order_t order;            /* Byte order */
    hsize_t     dims[2] = {3, 4}; /* Array dimensions */
    herr_t      ret;              /* Generic return value */

    TESTING("H5Tset/get_order");

    /* Integer */
    if ((dtype = H5Tcopy(H5T_STD_I32BE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_LE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Float */
    if ((dtype = H5Tcopy(H5T_IEEE_F64LE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_BE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Time */
    if ((dtype = H5Tcopy(H5T_UNIX_D64BE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_LE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Fixed length string */
    if ((dtype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(dtype, (size_t)5) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_NONE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;

    /* Variable length string */
    if (H5Tset_size(dtype, H5T_VARIABLE) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_BE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Bitfield */
    if ((dtype = H5Tcopy(H5T_STD_B16LE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_BE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Opaque - No effect on the order */
    if ((dtype = H5Tcreate(H5T_OPAQUE, (size_t)96)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_NONE) < 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_BE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Compound */
    if ((dtype = H5Tcreate(H5T_COMPOUND, (size_t)48)) < 0)
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_BE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if ((order = H5Tget_order(dtype)) == H5T_ORDER_ERROR)
        TEST_ERROR;
    if (order != H5T_ORDER_NONE)
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Object reference */
    if ((dtype = H5Tcopy(H5T_STD_REF_OBJ)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_NONE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Region reference */
    if ((dtype = H5Tcopy(H5T_STD_REF_DSETREG)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_NONE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_NONE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Enum */
    if ((dtype = H5Tenum_create(H5T_STD_I16BE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_LE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Vlen */
    if ((dtype = H5Tvlen_create(H5T_STD_U64LE)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_BE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    /* Array */
    if ((dtype = H5Tarray_create2(H5T_IEEE_F64BE, 2, dims)) < 0)
        TEST_ERROR;
    if (H5T_ORDER_BE != H5Tget_order(dtype))
        TEST_ERROR;
    H5E_BEGIN_TRY
    ret = H5Tset_order(dtype, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;
    if (H5Tset_order(dtype, H5T_ORDER_LE) < 0)
        TEST_ERROR;
    if (H5T_ORDER_LE != H5Tget_order(dtype))
        TEST_ERROR;
    if (H5Tclose(dtype) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    H5Tclose(dtype);
    H5E_END_TRY
    return 1;
} /* end test_set_order() */

/*-------------------------------------------------------------------------
 * Function:    test_set_order_compound
 *
 * Purpose:     Tests H5Tset_order/H5Tget_order for complicated compound
 *              type.
 *
 * Return:      Success:        0
 *              Failure:        number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_set_order_compound(hid_t fapl)
{
    typedef struct { /* Struct with atomic fields */
        int   i;
        char  c;
        short s;
        float f;
    } atomic_cmpd;

    typedef struct { /* Struct with complex fields */
        atomic_cmpd a;
        hvl_t       vl;
        double      b[3][4];
        atomic_cmpd d[3][4];
    } complex_cmpd;

    hid_t file = H5I_INVALID_HID;
    hid_t cmpd = H5I_INVALID_HID, memb_cmpd = H5I_INVALID_HID, memb_array1 = H5I_INVALID_HID,
          memb_array2 = H5I_INVALID_HID, cmpd_array = H5I_INVALID_HID;
    hid_t   vl_id   = H5I_INVALID_HID;
    hsize_t dims[2] = {3, 4}; /* Array dimensions */
    char    filename[1024];
    herr_t  ret; /* Generic return value */

    TESTING("H5Tset/get_order for compound type");

    if ((memb_cmpd = H5Tcreate(H5T_COMPOUND, sizeof(atomic_cmpd))) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(memb_cmpd, "i", HOFFSET(atomic_cmpd, i), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(memb_cmpd, "c", HOFFSET(atomic_cmpd, c), H5T_NATIVE_CHAR) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(memb_cmpd, "s", HOFFSET(atomic_cmpd, s), H5T_NATIVE_SHORT) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(memb_cmpd, "f", HOFFSET(atomic_cmpd, f), H5T_NATIVE_FLOAT) < 0)
        FAIL_STACK_ERROR;

    /* Set the order to little-endian. */
    if (H5Tset_order(memb_cmpd, H5T_ORDER_BE) < 0)
        FAIL_STACK_ERROR;

    /* Create the array datatypes */
    memb_array1 = H5Tarray_create2(H5T_NATIVE_DOUBLE, 2, dims);
    memb_array2 = H5Tarray_create2(memb_cmpd, 2, dims);

    /* Set the order to big-endian. */
    if (H5Tset_order(memb_array1, H5T_ORDER_LE) < 0)
        FAIL_STACK_ERROR;

    /* Create a variable-length datatype */
    if ((vl_id = H5Tvlen_create(H5T_NATIVE_UINT)) < 0)
        FAIL_STACK_ERROR;

    /* Create a compound type using the types above. */
    if ((cmpd = H5Tcreate(H5T_COMPOUND, sizeof(complex_cmpd))) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(cmpd, "a", HOFFSET(complex_cmpd, a), memb_cmpd) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(cmpd, "vl_type", HOFFSET(complex_cmpd, vl), vl_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(cmpd, "b", HOFFSET(complex_cmpd, b), memb_array1) < 0)
        FAIL_STACK_ERROR;
    if (H5Tinsert(cmpd, "d", HOFFSET(complex_cmpd, d), memb_array2) < 0)
        FAIL_STACK_ERROR;

    /* The order should be mixed now. */
    if (H5Tget_order(cmpd) != H5T_ORDER_MIXED)
        FAIL_STACK_ERROR;

    /* Create an array of the compound type above */
    cmpd_array = H5Tarray_create2(cmpd, 2, dims);

    /* The order of the array type should be the same as the compound type */
    if (H5Tget_order(cmpd_array) != H5T_ORDER_MIXED)
        FAIL_STACK_ERROR;

    /* Verify that the order can't be 'none'. */
    H5E_BEGIN_TRY
    ret = H5Tset_order(cmpd, H5T_ORDER_NONE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Verify that the order can't be 'mixed'. */
    H5E_BEGIN_TRY
    ret = H5Tset_order(cmpd, H5T_ORDER_MIXED);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Change the order of the compound type to big-endian*/
    if (H5Tset_order(cmpd, H5T_ORDER_BE) < 0)
        FAIL_STACK_ERROR;

    /* Verify that the order of the compound type is big-endian */
    if (H5Tget_order(cmpd) != H5T_ORDER_BE)
        FAIL_STACK_ERROR;

    /* Change the order of the array type to little-endian*/
    if (H5Tset_order(cmpd_array, H5T_ORDER_LE) < 0)
        FAIL_STACK_ERROR;

    /* Verify that the order of the array type is little-endian */
    if (H5Tget_order(cmpd_array) != H5T_ORDER_LE)
        FAIL_STACK_ERROR;

    /* Create file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Commit the data type */
    if (H5Tcommit2(file, "compound", cmpd, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Verify that committed type can't change order */
    H5E_BEGIN_TRY
    ret = H5Tset_order(cmpd, H5T_ORDER_LE);
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    if (H5Tclose(memb_cmpd) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(memb_array1) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(memb_array2) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(vl_id) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(cmpd) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(cmpd_array) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    H5Tclose(memb_cmpd);
    H5Tclose(memb_array1);
    H5Tclose(memb_array2);
    H5Tclose(vl_id);
    H5Tclose(cmpd);
    H5Tclose(cmpd_array);
    H5Fclose(file);
    H5E_END_TRY
    return 1;
} /* end test_set_order_compound() */

/*-------------------------------------------------------------------------
 * Function:    test_enum_member_order
 *
 * Purpose:     Tests that datatype conversions don't perturb the ordering
 *              of members within an enum datatype due to the way they sort
 *              the members internally during conversion.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
#define NUM_MEMBERS 3
static int
test_enum_member_order(void)
{
    typedef enum { SOLID, LIQUID, GAS, PLASMA } phase_t;

    const char *enum_names[4]              = {"SOLID", "LIQUID", "GAS", "PLASMA"};
    phase_t     enum_int_buf[NUM_MEMBERS]  = {LIQUID, GAS, PLASMA};
    long        enum_long_buf[NUM_MEMBERS] = {0, 0, 0};
    hid_t       enum_type1                 = H5I_INVALID_HID;
    hid_t       enum_type2                 = H5I_INVALID_HID;
    int         val_int                    = -1;
    long        val_long                   = -1;

    TESTING("stability of enum member ordering after datatype conversion");

    /* Test enum type */

    if ((enum_type1 = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    if ((enum_type2 = H5Tenum_create(H5T_NATIVE_LONG)) < 0)
        TEST_ERROR;

    for (size_t i = 0; i < sizeof(enum_names) / sizeof(enum_names[0]); i++) {
        val_int  = (int)i;
        val_long = (long)i;

        /* Insert members into enums in order: SOLID, LIQUID, GAS, PLASMA */
        if (H5Tenum_insert(enum_type1, enum_names[i], &val_int) < 0)
            TEST_ERROR;
        if (H5Tenum_insert(enum_type2, enum_names[i], &val_long) < 0)
            TEST_ERROR;
    }

    memcpy(enum_long_buf, enum_int_buf, NUM_MEMBERS * sizeof(int));

    /* Convert from int enum type to long enum type */
    if (H5Tconvert(enum_type1, enum_type2, NUM_MEMBERS, enum_long_buf, NULL, H5P_DEFAULT) < 0)
        TEST_ERROR;

    /* Sanity check */
    for (size_t i = 0; i < NUM_MEMBERS; i++) {
        if (enum_long_buf[i] != (long)enum_int_buf[i]) {
            H5_FAILED();
            printf("long enum buf member %zu mismatch after conversion; expected %ld, got %ld\n", i,
                   (long)enum_int_buf[i], enum_long_buf[i]);
            goto error;
        }
    }

    /* Check that each enum type's members are in the same order we inserted them in */
    for (size_t i = 0; i < sizeof(enum_names) / sizeof(enum_names[0]); i++) {
        if (H5Tget_member_value(enum_type1, (unsigned)i, &val_int) < 0)
            TEST_ERROR;
        if (val_int != (int)i) {
            H5_FAILED();
            printf("int enum member %zu was out of order; expected %d, got %d\n", i, (int)i, val_int);
            goto error;
        }

        if (H5Tget_member_value(enum_type2, (unsigned)i, &val_long) < 0)
            TEST_ERROR;
        if (val_long != (long)i) {
            H5_FAILED();
            printf("long enum member %zu was out of order; expected %ld, got %ld\n", i, (long)i, val_long);
            goto error;
        }
    }

    if (H5Tclose(enum_type1) < 0)
        TEST_ERROR;
    if (H5Tclose(enum_type2) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(enum_type1);
        H5Tclose(enum_type2);
    }
    H5E_END_TRY

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    test_named_indirect_reopen
 *
 * Purpose:     Tests that open named datatypes can be reopened indirectly
 *              through H5Dget_type without causing problems.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_named_indirect_reopen(hid_t fapl)
{
    hid_t file = H5I_INVALID_HID, type = H5I_INVALID_HID, reopened_type = H5I_INVALID_HID,
          strtype = H5I_INVALID_HID, dset = H5I_INVALID_HID, space = H5I_INVALID_HID;
    static hsize_t dims[1] = {3};
    size_t         dt_size;
    int            enum_value;
    const char    *tag     = "opaque_tag";
    char          *tag_ret = NULL;
    char           filename[1024];

    TESTING("indirectly reopening committed datatypes");

    /* Create file, dataspace */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((space = H5Screate_simple(1, dims, dims)) < 0)
        TEST_ERROR;

    /*
     * Compound
     */

    /* Create compound type */
    if ((strtype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(strtype, H5T_VARIABLE) < 0)
        TEST_ERROR;
    if ((type = H5Tcreate(H5T_COMPOUND, sizeof(char *))) < 0)
        TEST_ERROR;
    if (H5Tinsert(type, "vlstr", (size_t)0, strtype) < 0)
        TEST_ERROR;
    if (H5Tclose(strtype) < 0)
        TEST_ERROR;

    /* Get size of compound type */
    if ((dt_size = H5Tget_size(type)) == 0)
        TEST_ERROR;

    /* Commit compound type and verify the size doesn't change */
    if (H5Tcommit2(file, "cmpd_type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(type))
        TEST_ERROR;

    /* Create dataset with compound type */
    if ((dset = H5Dcreate2(file, "cmpd_dset", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen type and verify that the size doesn't change */
    if ((reopened_type = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(reopened_type))
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_type) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /*
     * Enum
     */

    /* Create enum type */
    if ((type = H5Tenum_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;
    enum_value = 0;
    if (H5Tenum_insert(type, "val1", &enum_value) < 0)
        TEST_ERROR;
    enum_value = 1;
    if (H5Tenum_insert(type, "val2", &enum_value) < 0)
        TEST_ERROR;

    /* Get size of enum type */
    if ((dt_size = H5Tget_size(type)) == 0)
        TEST_ERROR;

    /* Commit enum type and verify the size doesn't change */
    if (H5Tcommit2(file, "enum_type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(type))
        TEST_ERROR;

    /* Create dataset with enum type */
    if ((dset = H5Dcreate2(file, "enum_dset", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen type and verify that the size doesn't change */
    if ((reopened_type = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(reopened_type))
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_type) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /*
     * Vlen
     */

    /* Create vlen type */
    if ((type = H5Tvlen_create(H5T_NATIVE_INT)) < 0)
        TEST_ERROR;

    /* Get size of vlen type */
    if ((dt_size = H5Tget_size(type)) == 0)
        TEST_ERROR;

    /* Commit vlen type and verify the size doesn't change */
    if (H5Tcommit2(file, "vlen_type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(type))
        TEST_ERROR;

    /* Create dataset with vlen type */
    if ((dset = H5Dcreate2(file, "vlen_dset", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen type and verify that the size doesn't change */
    if ((reopened_type = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(reopened_type))
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_type) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /*
     * Opaque
     */

    /* Create opaque type */
    if ((type = H5Tcreate(H5T_OPAQUE, (size_t)13)) < 0)
        TEST_ERROR;
    if (H5Tset_tag(type, tag) < 0)
        TEST_ERROR;

    /* Get size of opaque type */
    if ((dt_size = H5Tget_size(type)) == 0)
        TEST_ERROR;

    /* Commit opaque type and verify the size and tag don't change */
    if (H5Tcommit2(file, "opaque_type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(type))
        TEST_ERROR;
    if (NULL == (tag_ret = H5Tget_tag(type)))
        TEST_ERROR;
    if (strcmp(tag, tag_ret) != 0)
        TEST_ERROR;
    H5free_memory(tag_ret);
    tag_ret = NULL;

    /* Create dataset with opaque type */
    if ((dset = H5Dcreate2(file, "opaque_dset", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen type and verify that the size and tag don't change */
    if ((reopened_type = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(reopened_type))
        TEST_ERROR;
    if (NULL == (tag_ret = H5Tget_tag(type)))
        TEST_ERROR;
    if (strcmp(tag, tag_ret) != 0)
        TEST_ERROR;
    H5free_memory(tag_ret);
    tag_ret = NULL;

    /* Close types and dataset */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_type) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /*
     * Array
     */

    /* Create array type */
    if ((type = H5Tarray_create2(H5T_NATIVE_INT, 1, dims)) < 0)
        TEST_ERROR;

    /* Get size of array type */
    if ((dt_size = H5Tget_size(type)) == 0)
        TEST_ERROR;

    /* Commit array type and verify the size doesn't change */
    if (H5Tcommit2(file, "array_type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(type))
        TEST_ERROR;

    /* Create dataset with array type */
    if ((dset = H5Dcreate2(file, "array_dset", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen type and verify that the size doesn't change */
    if ((reopened_type = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (dt_size != H5Tget_size(reopened_type))
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(type) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_type) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* Close file and dataspace */
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type);
        H5Tclose(strtype);
        H5Tclose(reopened_type);
        H5Sclose(space);
        H5Dclose(dset);
        H5Fclose(file);
    }
    H5E_END_TRY
    if (tag_ret)
        H5free_memory(tag_ret);
    return 1;
} /* end test_named_indirect_reopen() */

/*-------------------------------------------------------------------------
 * Function:    test_named_indirect_reopen_file
 *
 * Purpose: Tests that a named compound datatype that refers to a named
 *          string datatype can be reopened indirectly through H5Dget_type,
 *          and shows the correct H5Tcommitted() state, including after the
 *          file has been closed and reopened.
 *
 * Return:  Success:    0
 *          Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_named_indirect_reopen_file(hid_t fapl)
{
    hid_t   file             = H5I_INVALID_HID;
    hid_t   space            = H5I_INVALID_HID;
    hid_t   cmptype          = H5I_INVALID_HID;
    hid_t   reopened_cmptype = H5I_INVALID_HID;
    hid_t   strtype          = H5I_INVALID_HID;
    hid_t   reopened_strtype = H5I_INVALID_HID;
    hid_t   dset             = H5I_INVALID_HID;
    hsize_t dims[1]          = {3};
    size_t  strtype_size, cmptype_size;
    char    filename[1024];

    TESTING("indirectly reopening recursively committed datatypes including file reopening");

    /* PREPARATION */

    /* Create file, dataspace */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;
    if ((space = H5Screate_simple(1, dims, dims)) < 0)
        TEST_ERROR;

    /* Create string type */
    if ((strtype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;
    if (H5Tset_size(strtype, H5T_VARIABLE) < 0)
        TEST_ERROR;

    /* Get size of string type */
    if ((strtype_size = H5Tget_size(strtype)) == 0)
        TEST_ERROR;

    /* Commit string type and verify the size doesn't change */
    if (H5Tcommit2(file, "str_type", strtype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (strtype_size != H5Tget_size(strtype))
        TEST_ERROR;

    /* Create compound type */
    if ((cmptype = H5Tcreate(H5T_COMPOUND, sizeof(char *))) < 0)
        TEST_ERROR;
    if (H5Tinsert(cmptype, "vlstr", (size_t)0, strtype) < 0)
        TEST_ERROR;

    /* Get size of compound type */
    if ((cmptype_size = H5Tget_size(cmptype)) == 0)
        TEST_ERROR;

    /* Commit compound type and verify the size doesn't change */
    if (H5Tcommit2(file, "cmp_type", cmptype, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;
    if (cmptype_size != H5Tget_size(cmptype))
        TEST_ERROR;

    /* Create dataset with compound type */
    if ((dset = H5Dcreate2(file, "cmp_dset", cmptype, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close original types */
    if (H5Tclose(strtype) < 0)
        TEST_ERROR;
    if (H5Tclose(cmptype) < 0)
        TEST_ERROR;

    /* CHECK DATA TYPES WHILE STILL HOLDING THE FILE OPEN */

    /* Indirectly reopen compound type, verify that they report as committed, and the size doesn't change */
    if ((reopened_cmptype = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (cmptype_size != H5Tget_size(reopened_cmptype))
        TEST_ERROR;
    if (H5Tcommitted(reopened_cmptype) != 1)
        TEST_ERROR;

    /* Indirectly reopen string type, verify that they report as NOT committed, and the size doesn't change */
    if ((reopened_strtype = H5Tget_member_type(reopened_cmptype, 0)) < 0)
        TEST_ERROR;
    if (strtype_size != H5Tget_size(reopened_strtype))
        TEST_ERROR;
    if (H5Tcommitted(reopened_strtype) != 0)
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(reopened_strtype) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_cmptype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* CHECK DATA TYPES AFTER REOPENING THE SAME FILE */

    /* Close file */
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Reopen file */
    if ((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Reopen dataset */
    if ((dset = H5Dopen2(file, "cmp_dset", H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Indirectly reopen compound type, verify that they report as committed, and the size doesn't change */
    if ((reopened_cmptype = H5Dget_type(dset)) < 0)
        TEST_ERROR;
    if (cmptype_size != H5Tget_size(reopened_cmptype))
        TEST_ERROR;
    if (H5Tcommitted(reopened_cmptype) != 1)
        TEST_ERROR;

    /* Indirectly reopen string type, verify that they report as NOT committed, and the size doesn't change */
    if ((reopened_strtype = H5Tget_member_type(reopened_cmptype, 0)) < 0)
        TEST_ERROR;
    if (strtype_size != H5Tget_size(reopened_strtype))
        TEST_ERROR;
    if (H5Tcommitted(reopened_strtype) != 0)
        TEST_ERROR;

    /* Close types and dataset */
    if (H5Tclose(reopened_strtype) < 0)
        TEST_ERROR;
    if (H5Tclose(reopened_cmptype) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* DONE */

    /* Close file and dataspace */
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(cmptype);
        H5Tclose(strtype);
        H5Tclose(reopened_cmptype);
        H5Tclose(reopened_strtype);
        H5Sclose(space);
        H5Dclose(dset);
        H5Fclose(file);
    }
    H5E_END_TRY;
    return 1;
} /* end test_named_indirect_reopen() */
static void
create_del_obj_named_test_file(const char *filename, hid_t fapl, H5F_libver_t low, H5F_libver_t high)
{
    hid_t                        file;             /* File ID */
    hid_t                        type;             /* Datatype ID */
    hid_t                        space;            /* Dataspace ID */
    hid_t                        attr;             /* Attribute ID */
    hid_t                        dset;             /* Dataset ID */
    hid_t                        fcpl;             /* File creation property list ID */
    hid_t                        my_fapl;          /* Copy of file access property list ID */
    hid_t                        dcpl;             /* Dataset creation property list ID */
    unsigned                     use_at_least_v18; /* Whether to use old or new format */
    herr_t H5_ATTR_NDEBUG_UNUSED status;           /* Generic return value */

    /* Make copy of FAPL */
    my_fapl = H5Pcopy(fapl);
    assert(my_fapl > 0);

    /* Use low/high version of file format */
    status = H5Pset_libver_bounds(my_fapl, low, high);
    assert(status >= 0);

    /* Set new format flag.  Note: the case high < low should be caught in the caller */
    use_at_least_v18 = 0;
    if (low >= H5F_LIBVER_V18)
        use_at_least_v18 = 1;

    /* Create a file creation property list (used for the root group's creation property list) */
    fcpl = H5Pcreate(H5P_FILE_CREATE);
    assert(fcpl > 0);

    if (use_at_least_v18) {
        /* Use dense link storage for all links in root group */
        status = H5Pset_link_phase_change(fcpl, 0, 0);
        assert(status >= 0);
    } /* end if */

    /* Create file with attribute that uses committed datatype */
    file = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, my_fapl);
    assert(file > 0);

    /* Close FCPL */
    status = H5Pclose(fcpl);
    assert(status >= 0);

    /* Close FAPL */
    status = H5Pclose(my_fapl);
    assert(status >= 0);

    /* Create datatype to commit */
    type = H5Tvlen_create(H5T_NATIVE_INT);
    assert(type > 0);

    /* Commit datatype */
    status = H5Tcommit2(file, DEL_OBJ_NAMED_NAMED_DTYPE, type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    assert(status >= 0);

    /* Create scalar dataspace */
    space = H5Screate(H5S_SCALAR);
    assert(space > 0);

    /* Create a dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    assert(dcpl > 0);

    if (use_at_least_v18) {
        /* Use dense attribute storage for all attributes on dataset */
        status = H5Pset_attr_phase_change(dcpl, 0, 0);
        assert(status >= 0);
    } /* end if */

    /* Create dataset */
    dset = H5Dcreate2(file, DEL_OBJ_NAMED_DATASET, type, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    assert(dset > 0);

    /* Close DCPL */
    status = H5Pclose(dcpl);
    assert(status >= 0);

    /* Close dataset */
    status = H5Dclose(dset);
    assert(status >= 0);

    /* Create attribute */
    attr = H5Acreate_by_name(file, DEL_OBJ_NAMED_DATASET, DEL_OBJ_NAMED_ATTRIBUTE, type, space, H5P_DEFAULT,
                             H5P_DEFAULT, H5P_DEFAULT);
    assert(attr > 0);

    /* Close dataspace */
    status = H5Sclose(space);
    assert(status >= 0);

    /* Close datatype */
    status = H5Tclose(type);
    assert(status >= 0);

    /* Close attribute */
    status = H5Aclose(attr);
    assert(status >= 0);

    /* Close file */
    status = H5Fclose(file);
    assert(status >= 0);
} /* end create_del_obj_named_test_file() */

/*-------------------------------------------------------------------------
 * Function:    test_delete_obj_named
 *
 * Purpose:    Tests that delete objects that use named datatypes through
 *              different file IDs
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_delete_obj_named(hid_t fapl)
{
    hid_t        filea1 = H5I_INVALID_HID, filea2 = H5I_INVALID_HID, fileb = H5I_INVALID_HID; /* File IDs */
    hid_t        attr  = H5I_INVALID_HID; /* Attribute ID */
    hid_t        dset  = H5I_INVALID_HID; /* Dataset ID */
    hid_t        fapl2 = H5I_INVALID_HID; /* File access property list ID */
    H5F_libver_t low, high;               /* File format bounds */
    char         filename[1024], filename2[1024];

    TESTING("deleting objects that use named datatypes");

    /* Set up filenames & FAPLs */
    if ((fapl2 = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);
    h5_fixname(FILENAME[9], fapl2, filename2, sizeof filename2);

    /* Loop through all valid the combinations of low/high library format bounds,
       to test delete objects that use named datatypes through different file IDs */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Skip invalid low/high combination */
            if ((high == H5F_LIBVER_EARLIEST) || (low > high))
                continue;

            /* Create test file, with attribute that uses committed datatype */
            create_del_obj_named_test_file(filename, fapl, low, high);

            /* Test deleting dataset opened through different file ID */
            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;
            if ((filea2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((dset = H5Dopen2(filea1, DEL_OBJ_NAMED_DATASET, H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;
            if (H5Dclose(dset) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;

            if ((fileb = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0)
                FAIL_STACK_ERROR;

            if (H5Ldelete(filea2, DEL_OBJ_NAMED_DATASET, H5P_DEFAULT) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea2) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(fileb) < 0)
                FAIL_STACK_ERROR;

            /* Create test file, with attribute that uses committed datatype */
            create_del_obj_named_test_file(filename, fapl, low, high);

            /* Test deleting attribute opened through different file ID */
            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;
            if ((filea2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((attr = H5Aopen_by_name(filea1, DEL_OBJ_NAMED_DATASET, DEL_OBJ_NAMED_ATTRIBUTE, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;
            if (H5Aclose(attr) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;

            if ((fileb = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0)
                FAIL_STACK_ERROR;

            if (H5Adelete_by_name(filea2, DEL_OBJ_NAMED_DATASET, DEL_OBJ_NAMED_ATTRIBUTE, H5P_DEFAULT) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea2) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(fileb) < 0)
                FAIL_STACK_ERROR;
        } /* end high */
    }     /* end low */

    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(attr);
        H5Dclose(dset);
        H5Pclose(fapl2);
        H5Fclose(filea1);
        H5Fclose(filea2);
        H5Fclose(fileb);
    }
    H5E_END_TRY
    return 1;
} /* end test_delete_obj_named() */

/*-------------------------------------------------------------------------
 * Function:    test_delete_obj_named_fileid
 *
 * Purpose:    Tests that objects that use named datatypes through
 *              different file IDs get the correct file IDs
 *
 * Return:    Success:    0
 *        Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_delete_obj_named_fileid(hid_t fapl)
{
    hid_t        filea1 = H5I_INVALID_HID, filea2 = H5I_INVALID_HID, fileb = H5I_INVALID_HID; /* File IDs */
    hid_t        dset_fid = H5I_INVALID_HID; /* File ID from dataset */
    hid_t        type_fid = H5I_INVALID_HID; /* File ID from datatype */
    hid_t        attr_fid = H5I_INVALID_HID; /* File ID from attribute */
    hid_t        type     = H5I_INVALID_HID; /* Datatype ID */
    hid_t        attr     = H5I_INVALID_HID; /* Attribute ID */
    hid_t        dset     = H5I_INVALID_HID; /* Dataset ID */
    hid_t        fapl2    = H5I_INVALID_HID; /* File access property list ID */
    H5F_libver_t low, high;                  /* File format bounds */
    char         filename[1024], filename2[1024];

    TESTING("deleting objects that use named datatypes");

    /* Set up filenames & FAPLs */
    if ((fapl2 = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;
    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);
    h5_fixname(FILENAME[9], fapl2, filename2, sizeof filename2);

    /* Loop through all the combinations of low/high library format bounds */
    for (low = H5F_LIBVER_EARLIEST; low < H5F_LIBVER_NBOUNDS; low++) {
        for (high = H5F_LIBVER_EARLIEST; high < H5F_LIBVER_NBOUNDS; high++) {

            /* Skip invalid low/high combination */
            if ((high == H5F_LIBVER_EARLIEST) || (low > high))
                continue;

            /* Create test file, with attribute that uses committed datatype */
            create_del_obj_named_test_file(filename, fapl, low, high);

            /* Test getting file ID for dataset opened through different file ID */
            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((filea2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((dset = H5Dopen2(filea1, DEL_OBJ_NAMED_DATASET, H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;

            /* Verify file ID from dataset matches correct file */
            dset_fid = H5Iget_file_id(dset);
            if (!H5F__same_file_test(dset_fid, filea1))
                TEST_ERROR;
            H5Fclose(dset_fid);

            /* Verify file ID from datatype (from dataset) matches correct file */
            type     = H5Dget_type(dset);
            type_fid = H5Iget_file_id(type);
            if (!H5F__same_file_test(type_fid, filea1))
                TEST_ERROR;
            H5Fclose(type_fid);
            H5Tclose(type);

            if (H5Dclose(dset) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;

            if ((fileb = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0)
                FAIL_STACK_ERROR;

            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((dset = H5Dopen2(filea1, DEL_OBJ_NAMED_DATASET, H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;

            /* Verify file ID from dataset matches correct file */
            dset_fid = H5Iget_file_id(dset);
            if (!H5F__same_file_test(dset_fid, filea1))
                TEST_ERROR;
            H5Fclose(dset_fid);

            /* Verify file ID from datatype (from dataset) matches correct file */
            type     = H5Dget_type(dset);
            type_fid = H5Iget_file_id(type);
            if (!H5F__same_file_test(type_fid, filea1))
                TEST_ERROR;
            H5Fclose(type_fid);
            H5Tclose(type);

            if (H5Dclose(dset) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(filea2) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(fileb) < 0)
                FAIL_STACK_ERROR;

            /* Create test file, with attribute that uses committed datatype */
            create_del_obj_named_test_file(filename, fapl, low, high);

            /* Test getting file ID for attribute opened through different file ID */
            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;
            if ((filea2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((attr = H5Aopen_by_name(filea1, DEL_OBJ_NAMED_DATASET, DEL_OBJ_NAMED_ATTRIBUTE, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;

            /* Verify file ID from dataset matches correct file */
            attr_fid = H5Iget_file_id(attr);
            if (!H5F__same_file_test(attr_fid, filea1))
                TEST_ERROR;
            H5Fclose(attr_fid);

            /* Verify file ID from datatype (from dataset) matches correct file */
            type     = H5Aget_type(attr);
            type_fid = H5Iget_file_id(type);
            if (!H5F__same_file_test(type_fid, filea1))
                TEST_ERROR;
            H5Fclose(type_fid);
            H5Tclose(type);

            if (H5Aclose(attr) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;

            if ((fileb = H5Fcreate(filename2, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0)
                FAIL_STACK_ERROR;

            if ((filea1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR;

            if ((attr = H5Aopen_by_name(filea1, DEL_OBJ_NAMED_DATASET, DEL_OBJ_NAMED_ATTRIBUTE, H5P_DEFAULT,
                                        H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;

            /* Verify file ID from dataset matches correct file */
            attr_fid = H5Iget_file_id(attr);
            if (!H5F__same_file_test(attr_fid, filea1))
                TEST_ERROR;
            H5Fclose(attr_fid);

            /* Verify file ID from datatype (from dataset) matches correct file */
            type     = H5Aget_type(attr);
            type_fid = H5Iget_file_id(type);
            if (!H5F__same_file_test(type_fid, filea1))
                TEST_ERROR;
            H5Fclose(type_fid);
            H5Tclose(type);

            if (H5Aclose(attr) < 0)
                FAIL_STACK_ERROR;

            if (H5Fclose(filea1) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(filea2) < 0)
                FAIL_STACK_ERROR;
            if (H5Fclose(fileb) < 0)
                FAIL_STACK_ERROR;
        } /* end high */
    }     /* end low */

    if (H5Pclose(fapl2) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Aclose(attr);
        H5Tclose(type);
        H5Dclose(dset);
        H5Pclose(fapl2);
        H5Fclose(filea1);
        H5Fclose(filea2);
        H5Fclose(fileb);
        H5Fclose(attr_fid);
        H5Fclose(type_fid);
    }
    H5E_END_TRY
    return 1;
} /* end test_delete_obj_named_fileid() */

/*-------------------------------------------------------------------------
 * Function:    test_deprec
 *
 * Purpose:     Tests deprecated API routines for datatypes.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS
static int
test_deprec(hid_t fapl)
{
    hid_t    file     = H5I_INVALID_HID; /* File ID */
    hid_t    type     = H5I_INVALID_HID; /* Datatype ID */
    unsigned rank     = 2;               /* Rank for array datatype */
    hsize_t  dims[2]  = {3, 3};          /* Dimensions for array datatype */
    int      perm[2]  = {0, 1};          /* Dimensions permutations for array datatype */
    hsize_t  rdims[2] = {0, 0};          /* Dimensions for querying array datatype */
    int      rperm[2] = {-2, -2};        /* Dimensions permutations for array datatype */
    bool     dim_mismatch;               /* Whether any dimensions didn't match */
    char     filename[1024];
    unsigned u;      /* Local index variable */
    herr_t   status; /* Generic routine value */

    TESTING("deprecated API routines for datatypes");

    /* Create an array datatype with an atomic base type */
    /* (dimension permutations allowed, but not stored) */
    if ((type = H5Tarray_create1(H5T_NATIVE_INT, (int)rank, dims, perm)) < 0)
        FAIL_STACK_ERROR;

    /* Make certain that the correct classes can be detected */
    if (H5Tdetect_class(type, H5T_ARRAY) != true)
        FAIL_STACK_ERROR;
    if (H5Tdetect_class(type, H5T_INTEGER) != true)
        FAIL_STACK_ERROR;

    /* Get the array dimensions */
    /* (Query the dimension permutations, which is allowed, but ignored) */
    if (H5Tget_array_dims1(type, rdims, rperm) < 0)
        FAIL_STACK_ERROR;

    /* Check the array dimensions */
    dim_mismatch = false;
    for (u = 0; u < rank; u++)
        if (rdims[u] != dims[u]) {
            TestErrPrintf("Array dimension information doesn't match!, rdims1[%u]=%d, tdims1[%u]=%d\n", u,
                          (int)rdims[u], u, (int)dims[u]);
            dim_mismatch = true;
        } /* end if */
    if (dim_mismatch)
        FAIL_PUTS_ERROR("    Dimensions didn't match!");

    /* Check the array dimension permutations */
    dim_mismatch = false;
    for (u = 0; u < rank; u++)
        if (rperm[u] != -2) {
            TestErrPrintf(
                "Array dimension permutation information was modified!, rdims1[%u]=%d, tdims1[%u]=%d\n", u,
                rperm[u], u, perm[u]);
            dim_mismatch = true;
        } /* end if */
    if (dim_mismatch)
        FAIL_PUTS_ERROR("    Dimension permutations modified!");

    /* Close the datatype */
    if (H5Tclose(type) < 0)
        FAIL_STACK_ERROR;

    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Predefined types cannot be committed */
    H5E_BEGIN_TRY
    {
        status = H5Tcommit1(file, "test_named_1 (should not exist)", H5T_NATIVE_INT);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_PUTS_ERROR("    Predefined types should not be committable!");

    /* Copy a predefined datatype and commit the copy */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tcommit1(file, "native-int", type) < 0)
        FAIL_STACK_ERROR;
    if ((status = H5Tcommitted(type)) < 0)
        FAIL_STACK_ERROR;
    if (0 == status)
        FAIL_PUTS_ERROR("    H5Tcommitted() returned false!");

    /* We should not be able to modify a type after it has been committed. */
    H5E_BEGIN_TRY
    {
        status = H5Tset_precision(type, (size_t)256);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_PUTS_ERROR("    Committed type is not constant!");

    /* We should not be able to re-commit a committed type */
    H5E_BEGIN_TRY
    {
        status = H5Tcommit1(file, "test_named_2 (should not exist)", type);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_PUTS_ERROR("    Committed types should not be recommitted!");

    /*
     * Close the committed type and reopen it.  It should return a named type.
     */
    if (H5Tclose(type) < 0)
        FAIL_STACK_ERROR;
    if ((type = H5Topen1(file, "native-int")) < 0)
        FAIL_STACK_ERROR;
    if ((status = H5Tcommitted(type)) < 0)
        FAIL_STACK_ERROR;
    if (!status)
        FAIL_PUTS_ERROR("    Opened named types should be named types!");

    /* Close */
    if (H5Tclose(type) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(file) < 0)
        FAIL_STACK_ERROR;

    /* Reopen file with read only access */
    if ((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        goto error;

    /* Verify that H5Tcommit2 returns an error */
    if ((type = H5Tcopy(H5T_NATIVE_INT)) < 0)
        goto error;
    H5E_BEGIN_TRY
    {
        status = H5Tcommit1(file, "test_named_3 (should not exist)", type);
    }
    H5E_END_TRY
    if (status >= 0) {
        H5_FAILED();
        puts("    Types should not be committable to a read-only file!");
        goto error;
    }

    /* Close */
    if (H5Tclose(type) < 0)
        goto error;
    if (H5Fclose(file) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(type);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
} /* end test_deprec() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/*-------------------------------------------------------------------------
 * Function:    test_utf_ascii_conv
 *
 * Purpose:     Make sure the library doesn't conversion strings between
 *              ASCII and UTF8.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
static int
test_utf_ascii_conv(void)
{
    hid_t       fid        = H5I_INVALID_HID;
    hid_t       did        = H5I_INVALID_HID;
    hid_t       utf8_vtid  = H5I_INVALID_HID;
    hid_t       ascii_vtid = H5I_INVALID_HID;
    hid_t       utf8_tid   = H5I_INVALID_HID;
    hid_t       ascii_tid  = H5I_INVALID_HID;
    hid_t       sid        = H5I_INVALID_HID;
    const char *utf8_w     = "foo!";
    char       *ascii_r    = NULL;
    const char *ascii_w    = "bar!";
    char       *utf8_r     = NULL;
    char        filename[1024];
    char        ascii2[4], utf8_2[4];
    herr_t      status;

    TESTING("string conversion between ASCII and UTF");

    /************************************************
     * Test VL string conversion from UTF8 to ASCII
     ************************************************/
    /* Create a variable-length string */
    if ((utf8_vtid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;

    if ((status = H5Tset_size(utf8_vtid, H5T_VARIABLE)) < 0)
        FAIL_STACK_ERROR;

    /* Set the character set for the string to UTF-8 */
    if ((status = H5Tset_cset(utf8_vtid, H5T_CSET_UTF8)) < 0)
        FAIL_STACK_ERROR;

    /* Create a variable-length string */
    if ((ascii_vtid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;

    if ((status = H5Tset_size(ascii_vtid, H5T_VARIABLE)) < 0)
        FAIL_STACK_ERROR;

    /* Set the character set for the string to ASCII (should already be so) */
    if ((status = H5Tset_cset(ascii_vtid, H5T_CSET_ASCII) < 0))
        FAIL_STACK_ERROR;

    /* Test conversion in memory */
    H5E_BEGIN_TRY
    {
        status = H5Tconvert(utf8_vtid, ascii_vtid, 1, &utf8_w, NULL, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Create a file */
    h5_fixname(FILENAME[10], H5P_DEFAULT, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create a scalar dataspace for the dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataset of UTF8 string type */
    if ((did = H5Dcreate2(fid, UTF8_DATASET, utf8_vtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the UTF8 string, as UTF8 */
    if ((status = H5Dwrite(did, utf8_vtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &utf8_w)) < 0)
        FAIL_STACK_ERROR;

    /* Read the UTF8 string, as ASCII, supposed to fail */
    H5E_BEGIN_TRY
    {
        status = H5Dread(did, ascii_vtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &ascii_r);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if ((status = H5Dclose(did)) < 0)
        FAIL_STACK_ERROR;

    /************************************************
     * Test VL string conversion from ASCII to UTF8
     ************************************************/
    /* Test conversion in memory */
    H5E_BEGIN_TRY
    {
        status = H5Tconvert(ascii_vtid, utf8_vtid, 1, &ascii_w, NULL, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Create a dataset of ASCII string type */
    if ((did = H5Dcreate2(fid, ASCII_DATASET, ascii_vtid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the ASCII string, as ASCII */
    if ((status = H5Dwrite(did, ascii_vtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &ascii_w)) < 0)
        FAIL_STACK_ERROR;

    /* Read the ASCII string, as UTF8, supposed to fail */
    H5E_BEGIN_TRY
    {
        status = H5Dread(did, utf8_vtid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &utf8_r);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if ((status = H5Dclose(did)) < 0)
        FAIL_STACK_ERROR;

    /* Close the UTF8 VL-string datatype */
    if ((status = H5Tclose(utf8_vtid)) < 0)
        FAIL_STACK_ERROR;

    /* Close the ASCII VL-string datatype */
    if ((status = H5Tclose(ascii_vtid)) < 0)
        FAIL_STACK_ERROR;

    /**********************************************************
     * Test fixed-length string conversion from UTF8 to ASCII
     **********************************************************/
    /* Create a fixed-length UTF8 string */
    if ((utf8_tid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;

    if ((status = H5Tset_size(utf8_tid, 4)) < 0)
        FAIL_STACK_ERROR;

    /* Set the character set for the string to UTF-8 */
    if ((status = H5Tset_cset(utf8_tid, H5T_CSET_UTF8)) < 0)
        FAIL_STACK_ERROR;

    /* Create a fixed-length ASCII string */
    if ((ascii_tid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;

    if ((status = H5Tset_size(ascii_tid, 4)) < 0)
        FAIL_STACK_ERROR;

    /* Set the character set for the string to ASCII (should already be so) */
    if ((status = H5Tset_cset(ascii_tid, H5T_CSET_ASCII) < 0))
        FAIL_STACK_ERROR;

    /* Test conversion in memory */
    H5E_BEGIN_TRY
    {
        status = H5Tconvert(utf8_tid, ascii_tid, 1, utf8_2, NULL, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Create a dataset */
    if ((did = H5Dcreate2(fid, UTF8_DATASET2, utf8_tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the UTF8 string, as UTF8 */
    if ((status = H5Dwrite(did, utf8_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &utf8_w)) < 0)
        FAIL_STACK_ERROR;

    /* Read the UTF8 string as ASCII, supposed to fail */
    H5E_BEGIN_TRY
    {
        status = H5Dread(did, ascii_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &ascii2);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if ((status = H5Dclose(did)) < 0)
        FAIL_STACK_ERROR;

    /**********************************************************
     * Test fixed-length string conversion from ASCII to UTF8
     **********************************************************/
    /* Test conversion in memory */
    H5E_BEGIN_TRY
    {
        status = H5Tconvert(ascii_tid, utf8_tid, 1, ascii2, NULL, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Create a dataset */
    if ((did = H5Dcreate2(fid, ASCII_DATASET2, ascii_tid, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write the ASCII string, as ASCII */
    if ((status = H5Dwrite(did, ascii_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &ascii_w)) < 0)
        FAIL_STACK_ERROR;

    /* Read the UTF8 string as ASCII, supposed to fail */
    H5E_BEGIN_TRY
    {
        status = H5Dread(did, utf8_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, &utf8_2);
    }
    H5E_END_TRY
    if (status >= 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if ((status = H5Dclose(did)) < 0)
        FAIL_STACK_ERROR;

    /* Close the UTF8 string datatype */
    if ((status = H5Tclose(utf8_tid)) < 0)
        FAIL_STACK_ERROR;

    /* Close the ASCII string datatype */
    if ((status = H5Tclose(ascii_tid)) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataspace */
    if ((status = H5Sclose(sid)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if ((status = H5Fclose(fid)) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(utf8_vtid);
        H5Tclose(ascii_vtid);
        H5Tclose(utf8_tid);
        H5Tclose(ascii_tid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Fclose(fid);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    verify_version
 *
 * Purpose: Utility function to verify the datatype versions of nested
 *          datatype.
 *
 * Description:
 *      Verify the datatype message version
 *      --H5T_COMPOUND, H5T_ENUM, H5T_ARRAY:
 *          the library will set version according to low_bound
 *      --H5T_ARRAY:
 *          the earliest version the library will set is 2
 *      --H5T_INTEGER, H5T_FLOAT, H5T_TIME, H5T_STRING, H5T_BITFIELD,
 *        H5T_OPAQUE, H5T_REFERENCE:
 *          the library will only use basic version
 *
 *************************************************************************/
static herr_t
verify_version(hid_t dtype, H5F_libver_t low, unsigned *highest_version)
{
    hid_t       base_dtype = H5I_INVALID_HID;
    hid_t       mem_dtype  = H5I_INVALID_HID;
    H5T_t      *dtypep     = NULL;         /* Internal structure of a datatype */
    H5T_class_t type_cls   = H5T_NO_CLASS; /* Temporary var for datatype class */
    int         nmembers   = 0;
    unsigned    i;
    herr_t      ret = SUCCEED; /* Generic return value */

    dtypep = (H5T_t *)H5I_object(dtype);
    if (dtypep == NULL)
        TEST_ERROR;

    /* Carry out the verification according to the class of the datatype.
       For compound datatype, its members will be verified, recursively.
       For array datatype, its element datatype will be verified, recursively.*/
    type_cls = dtypep->shared->type;
    switch (type_cls) {
        case H5T_ARRAY: {
            H5T_t *base_dtypep = NULL; /* Internal structure of a datatype */

            if (low == H5F_LIBVER_EARLIEST)
                VERIFY(dtypep->shared->version, H5O_DTYPE_VERSION_2, "H5O_dtype_ver_bounds");
            else
                VERIFY(dtypep->shared->version, H5O_dtype_ver_bounds[low], "H5O_dtype_ver_bounds");

            /* Get the base datatype of this array type */
            base_dtype = H5Tget_super(dtype);
            CHECK(base_dtype, FAIL, "H5Tget_super");

            /* Get the base type's internal structure for version */
            base_dtypep = (H5T_t *)H5I_object(base_dtype);
            if (base_dtypep == NULL)
                TEST_ERROR;

            /* Reset highest version if this datatype has higher version than
               its outer type */
            if (*highest_version < base_dtypep->shared->version)
                *highest_version = base_dtypep->shared->version;

            /* Verify the base datatype recursively */
            ret = verify_version(base_dtype, low, highest_version);

            /* Close the member datatype before checking for failure */
            if ((H5Tclose(base_dtype)) < 0)
                TEST_ERROR;

            /* Check if verify_version fails */
            if (ret < 0)
                TEST_ERROR;

            break;
        }
        case H5T_COMPOUND: {
            H5T_t *mem_dtypep = NULL; /* Internal structure of a datatype */
            /* Get the number of members of this compound type */
            if ((nmembers = H5Tget_nmembers(dtype)) < 0)
                TEST_ERROR;

            /* Go through all its member datatypes */
            for (i = 0; i < (unsigned)nmembers; i++) {
                /* Get the member datatype to verify it recursively */
                mem_dtype = H5Tget_member_type(dtype, i);
                if (mem_dtype < 0)
                    TEST_ERROR;

                /* Get the member type's internal structure for version */
                mem_dtypep = (H5T_t *)H5I_object(mem_dtype);
                if (mem_dtypep == NULL)
                    TEST_ERROR;

                /* Reset highest version if this datatype has higher version than
                   its outer type */
                if (*highest_version < mem_dtypep->shared->version)
                    *highest_version = mem_dtypep->shared->version;

                /* Verify the datatype recursively */
                ret = verify_version(mem_dtype, low, highest_version);

                /* Close the member datatype before checking for failure */
                if ((H5Tclose(mem_dtype)) < 0)
                    TEST_ERROR;

                /* Check if verify_version fails */
                if (ret < 0)
                    TEST_ERROR;
            }
            /* If this compound datatype contains a datatype of higher version, it
               will be promoted to that version, thus, verify with highest version */
            if (*highest_version > H5O_dtype_ver_bounds[low])
                VERIFY(dtypep->shared->version, *highest_version, "verify_version");
            else
                VERIFY(dtypep->shared->version, H5O_dtype_ver_bounds[low], "verify_version");
            break;
        }
        case H5T_ENUM:
            VERIFY(dtypep->shared->version, H5O_dtype_ver_bounds[low], "verify_version");
            break;
        case H5T_VLEN:
        case H5T_FLOAT:
        case H5T_INTEGER:
            VERIFY(dtypep->shared->version, H5O_dtype_ver_bounds[H5F_LIBVER_EARLIEST], "verify_version");
            break;
        case H5T_NCLASSES:
        case H5T_NO_CLASS:
        case H5T_TIME:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_REFERENCE:
        default:
            TEST_ERROR;
    } /* end switch */

error:
    H5E_BEGIN_TRY
    {
        H5Tclose(base_dtype);
        H5Tclose(mem_dtype);
    }
    H5E_END_TRY
    return ret;
} /* end of verify_version */

/*-------------------------------------------------------------------------
 * Function:    test_versionbounds
 *
 * Purpose:     Tests version bounds.
 *
 * Description:
 *      This function creates a datatype for a dataset as followed:
 *          outer_arr_type
 *              outer_cmp_type
 *                  inner_cmp_type
 *                      inner_arr_type
 *                          simple_cmp_type
 *                              H5T_NATIVE_INT
 *                              H5T_ARRAY of H5T_NATIVE_CHAR
 *                      vlen_floattype
 *                  enum_type
 *      It then loops through all valid combination of the library version
 *      bounds to verify each datatype's version.
 *
 * Return:      Success:    0
 *              Failure:    number of errors
 *
 *-------------------------------------------------------------------------
 */
#define VERFNAME   "tverbounds_dtype.h5"
#define VERDSNAME  "dataset 1"
#define ARRAY_RANK 1
#define ARRAY_LEN  10
static int
test_versionbounds(void)
{
    typedef struct { /* Struct for the simple compound type */
        int  single_int;
        char char_arr[ARRAY_LEN];
    } simple_cmp_t;

    typedef struct { /* Struct for the inner compound type */
        simple_cmp_t inner_arr[ARRAY_LEN];
        hvl_t        vlen_float;
    } inner_cmp_t;

    typedef struct { /* Struct for the outer compound type */
        inner_cmp_t inner_cmp;
        color_t     enum_color;
    } outer_cmp_t;

    hid_t        file            = H5I_INVALID_HID; /* File ID */
    hid_t        space           = H5I_INVALID_HID; /* Dataspace ID */
    hid_t        dset            = H5I_INVALID_HID; /* Dataset ID */
    hid_t        fcpl            = H5I_INVALID_HID; /* File creation property list ID */
    hid_t        fapl            = H5I_INVALID_HID; /* Copy of file access property list ID */
    hid_t        dcpl            = H5I_INVALID_HID; /* Dataset creation property list ID */
    hid_t        dset_dtype      = H5I_INVALID_HID; /* Dataset's datatype */
    hid_t        arr_chartype    = H5I_INVALID_HID; /* Array of characters datatype */
    hid_t        vlen_floattype  = H5I_INVALID_HID; /* Vlen of float datatype */
    hid_t        enum_type       = H5I_INVALID_HID; /* Enumeration datatype */
    hid_t        outer_cmp_type  = H5I_INVALID_HID; /* Outer compound datatype */
    hid_t        inner_cmp_type  = H5I_INVALID_HID; /* Inner compound datatype */
    hid_t        simple_cmp_type = H5I_INVALID_HID; /* Simple cmpd dtype, contains no other cmpd */
    hid_t        outer_arr_type  = H5I_INVALID_HID; /* Outermost array datatype */
    hid_t        inner_arr_type  = H5I_INVALID_HID; /* Inner array datatype */
    H5F_t       *filep           = NULL;            /* Pointer to internal structure of a file */
    H5T_t       *dtypep          = NULL;            /* Pointer to internal structure of a datatype */
    hsize_t      arr_dim[]       = {ARRAY_LEN};     /* Length of the array */
    int          low, high;                         /* Indices for iterating over versions */
    H5F_libver_t versions[]     = {H5F_LIBVER_EARLIEST, H5F_LIBVER_V18,  H5F_LIBVER_V110,
                               H5F_LIBVER_V112,     H5F_LIBVER_V114, H5F_LIBVER_V114};
    int          versions_count = 6; /* Number of version bounds in the array */
    unsigned     highest_version;    /* Highest version in nested datatypes */
    color_t      enum_val;           /* Enum type index */
    herr_t       ret = 0;            /* Generic return value */

    TESTING("version bounds with nested datatypes");

    /* Create a file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    /* Create a scalar dataspace */
    if ((space = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    /* Create a dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Create an array datatype of chars */
    arr_chartype = H5Tarray_create2(H5T_NATIVE_CHAR, ARRAY_RANK, arr_dim);
    if (arr_chartype < 0)
        TEST_ERROR;

    /* Create the simple compound datatype that has an integer and an
       array of chars */
    simple_cmp_type = H5Tcreate(H5T_COMPOUND, sizeof(simple_cmp_t));
    if (simple_cmp_type < 0)
        TEST_ERROR;

    /* Insert integer field */
    ret = H5Tinsert(simple_cmp_type, "single_int", HOFFSET(simple_cmp_t, single_int), H5T_NATIVE_INT);
    if (ret < 0)
        TEST_ERROR;

    /* Insert array field */
    ret = H5Tinsert(simple_cmp_type, "char_arr", HOFFSET(simple_cmp_t, char_arr), arr_chartype);
    if (ret < 0)
        TEST_ERROR;

    /* Create an array datatype containing simple compound datatype */
    inner_arr_type = H5Tarray_create2(simple_cmp_type, ARRAY_RANK, arr_dim);
    if (inner_arr_type < 0)
        TEST_ERROR;

    /* Create a VL datatype of floats */
    vlen_floattype = H5Tvlen_create(H5T_NATIVE_FLOAT);
    if (vlen_floattype < 0)
        TEST_ERROR;

    /* Create the innermost compound datatype that houses inner_arr_type and vlen_floattype */
    inner_cmp_type = H5Tcreate(H5T_COMPOUND, sizeof(inner_cmp_t));
    if (inner_cmp_type < 0)
        TEST_ERROR;

    /* Insert integer field */
    ret = H5Tinsert(inner_cmp_type, "inner_arr", HOFFSET(inner_cmp_t, inner_arr), inner_arr_type);
    if (ret < 0)
        TEST_ERROR;

    /* Insert integer field */
    ret = H5Tinsert(inner_cmp_type, "vlen_float", HOFFSET(inner_cmp_t, vlen_float), vlen_floattype);
    if (ret < 0)
        TEST_ERROR;

    /* Create a enumerate datatype */
    enum_type = H5Tcreate(H5T_ENUM, sizeof(color_t));
    if (enum_type < 0)
        TEST_ERROR;

    enum_val = E1_RED;
    ret      = H5Tenum_insert(enum_type, "RED", &enum_val);
    if (ret < 0)
        TEST_ERROR;

    enum_val = E1_GREEN;
    ret      = H5Tenum_insert(enum_type, "GREEN", &enum_val);
    if (ret < 0)
        TEST_ERROR;

    enum_val = E1_BLUE;
    ret      = H5Tenum_insert(enum_type, "BLUE", &enum_val);
    if (ret < 0)
        TEST_ERROR;

    enum_val = E1_ORANGE;
    ret      = H5Tenum_insert(enum_type, "ORANGE", &enum_val);
    if (ret < 0)
        TEST_ERROR;

    enum_val = E1_YELLOW;
    ret      = H5Tenum_insert(enum_type, "YELLOW", &enum_val);
    if (ret < 0)
        TEST_ERROR;

    /* Create the outer compound datatype that contains the inner compound datatype and the enum datatype */
    outer_cmp_type = H5Tcreate(H5T_COMPOUND, sizeof(outer_cmp_t));
    if (ret < 0)
        TEST_ERROR;

    /* Insert integer field */
    ret = H5Tinsert(outer_cmp_type, "inner_cmp", HOFFSET(outer_cmp_t, inner_cmp), inner_cmp_type);
    if (ret < 0)
        TEST_ERROR;

    /* Insert enum field */
    ret = H5Tinsert(outer_cmp_type, "enum_color", HOFFSET(outer_cmp_t, enum_color), enum_type);
    if (ret < 0)
        TEST_ERROR;

    /* Create an array datatype containing the outer compound datatype */
    if ((outer_arr_type = H5Tarray_create2(outer_cmp_type, ARRAY_RANK, arr_dim)) < 0)
        TEST_ERROR;

    /* Loop through all the combinations of low/high library format bounds,
       skipping invalid combinations */
    /* Create the file, create and write to a dataset with compound datatype */
    /* Verify the dataset's datatype and its members */
    for (low = 0; low < versions_count; low++) {

        for (high = 0; high < versions_count; high++) {

            /* Set version bounds */
            H5E_BEGIN_TRY
            {
                ret = H5Pset_libver_bounds(fapl, versions[low], versions[high]);
            }
            H5E_END_TRY

            if (ret < 0) /* Invalid low/high combinations */
                continue;

            /* Create a file */
            file = H5Fcreate(VERFNAME, H5F_ACC_TRUNC, fcpl, fapl);
            if (file < 0)
                TEST_ERROR;

            /* Get the internal file pointer if the create succeeds */
            if ((filep = (H5F_t *)H5I_object(file)) == NULL)
                TEST_ERROR;

            /* Create dataset using the array type */
            dset = H5Dcreate2(file, VERDSNAME, outer_arr_type, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
            if (dset < 0)
                TEST_ERROR;

            /* Get the dataset's datatype */
            if ((dset_dtype = H5Dget_type(dset)) < 0)
                TEST_ERROR;

            /* Get the version of this datatype */
            dtypep = (H5T_t *)H5I_object(dset_dtype);
            if (dtypep == NULL)
                TEST_ERROR;
            highest_version = dtypep->shared->version;

            /* Verify version of the datatype recursevily */
            ret = verify_version(dset_dtype, versions[low], &highest_version);

            /* Close the dataset's datatype */
            if (H5Tclose(dset_dtype) < 0)
                TEST_ERROR;

            /* Close dataset and file */
            if (H5Dclose(dset) < 0)
                TEST_ERROR;
            if (H5Fclose(file) < 0)
                TEST_ERROR;

        } /* for high */
    }     /* for low */

    /* Close dataspace and property lists */
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Tclose(dset_dtype);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(file);
    }
    H5E_END_TRY
    return 1;
} /* end test_versionbounds() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test the datatype interface.
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
    bool  driver_is_parallel;
    long  nerrors = 0;
    hid_t fapl    = H5I_INVALID_HID;

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    reset_hdf5();
    fapl = h5_fileaccess();

    if (h5_using_parallel_driver(fapl, &driver_is_parallel) < 0) {
        printf("Can't check if driver is parallel-enabled\n");
        exit(EXIT_FAILURE);
    }

    if (ALIGNMENT)
        printf("Testing non-aligned conversions (ALIGNMENT=%d)....\n", ALIGNMENT);

    /* Do the tests */
    nerrors += test_classes();
    nerrors += test_copy();
    nerrors += test_detect();
    nerrors += test_compound_1();
    nerrors += test_query();
    nerrors += test_set_fields_offset();
    nerrors += test_transient(fapl);
    nerrors += test_named(fapl);
    nerrors += test_encode();
    nerrors += test_latest();
    nerrors += test_int_float_except();
    nerrors += test_named_indirect_reopen(fapl);
    nerrors += test_named_indirect_reopen_file(fapl);
    nerrors += test_delete_obj_named(fapl);
    nerrors += test_delete_obj_named_fileid(fapl);
    nerrors += test_set_order_compound(fapl);
    nerrors += test_enum_member_order();
    nerrors += test_str_create();
#ifndef H5_NO_DEPRECATED_SYMBOLS
    nerrors += test_deprec(fapl);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    h5_cleanup(FILENAME, fapl); /*must happen before first reset*/
    reset_hdf5();

    nerrors += test_conv_str_1();
    nerrors += test_conv_str_2();
    nerrors += test_conv_str_3();
    nerrors += test_compound_2();
    nerrors += test_compound_3();
    nerrors += test_compound_4();
    nerrors += test_compound_5();
    nerrors += test_compound_6();
    nerrors += test_compound_7();
    nerrors += test_compound_8();

    if (!driver_is_parallel) {
        nerrors += test_compound_9();
        nerrors += test_compound_10();
    }

    nerrors += test_compound_11();
    nerrors += test_compound_12();
    nerrors += test_compound_13();

    if (!driver_is_parallel) {
        nerrors += test_compound_14();
    }

    nerrors += test_compound_15();
    nerrors += test_compound_15_attr();
    nerrors += test_compound_16();
    nerrors += test_compound_17();
    nerrors += test_compound_18();
    nerrors += test_user_compound_conversion();
    nerrors += test_conv_enum_1();
    nerrors += test_conv_enum_2();
    nerrors += test_conv_bitfield();
    nerrors += test_bitfield_funcs();
    nerrors += test_opaque();
    nerrors += test_set_order();
    nerrors += test_array_cmpd_vl();

    nerrors += test__Float16();

    if (!driver_is_parallel) {
        nerrors += test_utf_ascii_conv();
    }

    nerrors += test_app_conv_ids();

    nerrors += test_versionbounds();

    if (nerrors) {
        printf("***** %lu FAILURE%s! *****\n", nerrors, 1 == nerrors ? "" : "S");
        exit(EXIT_FAILURE);
    }

    printf("All datatype tests passed.\n");

    return 0;
}
