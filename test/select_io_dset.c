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
 * Purpose:    Tests selection IO for the dataset interface (H5D)
 */

#include "testhdf5.h"
#include "H5srcdir.h"

const char *FILENAME[] = {"select_io", /* 0 */
                          NULL};

#define FILENAME_BUF_SIZE 1024

/*
 * Test configurations
 */
typedef enum {
    TEST_NO_TYPE_CONV,           /* no type conversion (null case) */
    TEST_NO_SIZE_CHANGE_NO_BKG,  /* no size change, no bkg buffer */
    TEST_LARGER_MEM_NO_BKG,      /* larger memory type, no bkg buffer */
    TEST_SMALLER_MEM_NO_BKG,     /* smaller memory type, no bkg buffer */
    TEST_CMPD_WITH_BKG,          /* compound types with bkg buffer */
    TEST_MULTI_CONV_NO_BKG,      /* multi dataset test 1 */
    TEST_MULTI_CONV_BKG,         /* multi dataset test 2 */
    TEST_MULTI_CONV_SIZE_CHANGE, /* multi dataset test 3 */
    TEST_MULTI_ALL,              /* multi dataset test 4 */
    TEST_SELECT_NTESTS
} test_select_config_t;

#define DSET_SELECT_DIM       100
#define DSET_SELECT_CHUNK_DIM 10

#define MULTI_NUM_DSETS 3
#define MULTI_MIN_DSETS 3
#define DSET_NAME_LEN   64

/* Compound type */
typedef struct s1_t {
    int a;
    int b;
    int c;
    int d;
} s1_t;

/*
 * Variation of s1 with:
 * --no conversion for 2 member types
 * --1 larger mem type,
 * --1 smaller mem type
 */
typedef struct s2_t {
    int   a;
    long  b;
    int   c;
    short d;
} s2_t;

/* Variation of s1: reverse of s1_t */
typedef struct s3_t {
    int d;
    int c;
    int b;
    int a;
} s3_t;

/* Variations of s1: only 2 members in s1_t */
typedef struct s4_t {
    unsigned int b;
    unsigned int d;
} s4_t;

/* Defines for test_multi_dsets_all() */
typedef enum {
    DSET_WITH_NO_CONV,         /* Dataset with no type conversion */
    DSET_WITH_CONV_AND_NO_BKG, /* Dataset with type conversion but no background buffer */
    DSET_WITH_CONV_AND_BKG,    /* Dataset with type conversion and background buffer */
    DSET_NTTYPES
} multi_dset_type_t;

/* Test setting A and B */
#define SETTING_A 1
#define SETTING_B 2

/* Definitions of the test modes for test_get_no_selection_io_cause() */
#define TEST_DISABLE_BY_API                    0x001
#define TEST_DATATYPE_CONVERSION               0x002
#define TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET 0x004
#define TEST_CONTIGUOUS_SIEVE_BUFFER           0x008
#define TEST_NO_VECTOR_OR_SELECTION_IO_CB      0x010
#define TEST_PAGE_BUFFER                       0x020
#define TEST_DATASET_FILTER                    0x040
#define TEST_CHUNK_CACHE                       0x080
#define TEST_TCONV_BUF_TOO_SMALL               0x100
#define TEST_IN_PLACE_TCONV                    0x200

static herr_t
check_actual_selection_io_mode(hid_t dxpl, uint32_t sel_io_mode_expected)
{
    uint32_t actual_sel_io_mode;

    if (H5Pget_actual_selection_io_mode(dxpl, &actual_sel_io_mode) < 0)
        TEST_ERROR;
    if (actual_sel_io_mode != sel_io_mode_expected)
        TEST_ERROR;

    return SUCCEED;
error:
    return FAIL;
}

/*
 *  Case 1: single dataset read/write, no type conversion (null case)
 *  --create dataset with H5T_NATIVE_INT
 *  --write/read dataset with H5T_NATIVE_INT
 */
static herr_t
test_no_type_conv(hid_t fid, unsigned set_cache, unsigned chunked, unsigned dtrans, unsigned mwbuf)
{
    int         i;
    hid_t       did         = H5I_INVALID_HID;
    hid_t       sid         = H5I_INVALID_HID;
    hid_t       dcpl        = H5I_INVALID_HID;
    hid_t       dxpl        = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    int         wbuf[DSET_SELECT_DIM];
    int         wbuf_bak[DSET_SELECT_DIM];
    int         trans_wbuf[DSET_SELECT_DIM];
    int         rbuf[DSET_SELECT_DIM];
    char        dset_name[DSET_NAME_LEN];
    const char *expr = "2*x";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Generate dataset name */
    snprintf(dset_name, sizeof(dset_name), "no_tconv_%s_%s_%s", chunked ? "chunked" : "contig",
             dtrans ? "xform" : "noxform", mwbuf ? "mwbuf" : "nomwbuf");

    /* Create dataset */
    if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = i;
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        TEST_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            TEST_ERROR;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(wbuf_bak, wbuf, sizeof(wbuf));

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(wbuf, wbuf_bak, sizeof(wbuf));

    /* Read data from the dataset without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read the data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (2 * trans_wbuf[i])) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Pclose(ntrans_dxpl);
    }
    H5E_END_TRY

    return FAIL;

} /* test_no_type_conv() */

/*
 *  Case 2: single dataset read/write, no size change, no background buffer
 *  --create dataset with H5T_STD_I32BE
 *  --write/read dataset with H5T_STD_I32LE
 *  --read again with H5T_STD_I32BE
 */
static herr_t
test_no_size_change_no_bkg(hid_t fid, unsigned set_cache, unsigned chunked, unsigned mwbuf)
{
    int     i;
    hid_t   did  = H5I_INVALID_HID;
    hid_t   sid  = H5I_INVALID_HID;
    hid_t   dcpl = H5I_INVALID_HID;
    hid_t   dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    char   *wbuf     = NULL;
    char   *wbuf_bak = NULL;
    char   *rbuf     = NULL;
    char    dset_name[DSET_NAME_LEN];
    int     fillvalue = (-1);

    if ((wbuf = (char *)malloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        TEST_ERROR;
    if (mwbuf && (wbuf_bak = (char *)malloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        TEST_ERROR;
    if ((rbuf = (char *)malloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Generate dataset name */
    snprintf(dset_name, sizeof(dset_name), "no_size_change_%s_%s", chunked ? "chunked" : "contig",
             mwbuf ? "mwbuf" : "nomwbuf");

    /* Create 1d dataset */
    if ((did = H5Dcreate2(fid, dset_name, H5T_STD_I32BE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i * 4 + 3] = 0x1;
        wbuf[i * 4 + 2] = 0x2;
        wbuf[i * 4 + 1] = 0x3;
        wbuf[i * 4 + 0] = 0x4;
    }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(wbuf_bak, wbuf, (size_t)(4 * DSET_SELECT_DIM));

    /* Write the data to the dataset with little endian */
    if (H5Dwrite(did, H5T_STD_I32LE, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(wbuf, wbuf_bak, (size_t)(4 * DSET_SELECT_DIM));

    /* Read the data from the dataset with little endian */
    if (H5Dread(did, H5T_STD_I32LE, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Verify data read little endian */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[4 * i + 0] != wbuf[4 * i + 0] || rbuf[4 * i + 1] != wbuf[4 * i + 1] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 2] || rbuf[4 * i + 3] != wbuf[4 * i + 3]) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }

    /* Read the data from the dataset with big endian */
    if (H5Dread(did, H5T_STD_I32BE, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify data read in big endian */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[4 * i + 0] != wbuf[4 * i + 3] || rbuf[4 * i + 1] != wbuf[4 * i + 2] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 1] || rbuf[4 * i + 3] != wbuf[4 * i + 0]) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    free(wbuf);
    free(wbuf_bak);
    free(rbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
    }
    H5E_END_TRY

    if (wbuf)
        free(wbuf);
    if (wbuf_bak)
        free(wbuf_bak);
    if (wbuf)
        free(rbuf);

    return FAIL;

} /* test_no_size_change_no_bkg() */

/*
 *  Case 3: single dataset read/write, larger mem type, no background buffer
 *  --create dataset with H5T_NATIVE_INT
 *  --write dataset with H5T_NATIVE_LONG
 *  --read dataset with H5T_NATIVE_LLONG
 *
 */
static herr_t
test_larger_mem_type_no_bkg(hid_t fid, unsigned set_cache, unsigned chunked, unsigned dtrans, unsigned mwbuf)
{
    int         i;
    hid_t       did         = H5I_INVALID_HID;
    hid_t       sid         = H5I_INVALID_HID;
    hid_t       dcpl        = H5I_INVALID_HID;
    hid_t       dxpl        = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    long        wbuf[DSET_SELECT_DIM];
    long        wbuf_bak[DSET_SELECT_DIM];
    long        trans_wbuf[DSET_SELECT_DIM];
    long long   rbuf[DSET_SELECT_DIM];
    char        dset_name[DSET_NAME_LEN];
    const char *expr = "5 * (10 - x)";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Generate dataset name */
    snprintf(dset_name, sizeof(dset_name), "larger_no_bkg_%s_%s_%s", chunked ? "chunked" : "contig",
             dtrans ? "xform" : "noxform", mwbuf ? "mwbuf" : "nomwbuf");

    /* Create dataset */
    if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = i;
        trans_wbuf[i] = 5 * (10 - wbuf[i]);
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        TEST_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            TEST_ERROR;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(wbuf_bak, wbuf, sizeof(wbuf));

    /* Write data to the dataset with/without data transform set in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(wbuf, wbuf_bak, sizeof(wbuf));

    /* Read the data from the dataset without data transform in dxpl */
    if (H5Dread(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (long long)(dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (long long)(5 * (10 - trans_wbuf[i]))) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Pclose(ntrans_dxpl);
    }
    H5E_END_TRY

    return FAIL;

} /* test_larger_mem_type_no_bkg() */

/*
 *  Case 4: single dataset reader/write, smaller mem type, no background buffer
 *  --create dataset with H5T_NATIVE_INT
 *  --write dataset with H5T_NATIVE_SHORT
 *  --read dataset with H5T_NATIVE_SHORT
 */
static herr_t
test_smaller_mem_type_no_bkg(hid_t fid, unsigned set_cache, unsigned chunked, unsigned dtrans, unsigned mwbuf)
{
    int         i;
    hid_t       did         = H5I_INVALID_HID;
    hid_t       sid         = H5I_INVALID_HID;
    hid_t       dcpl        = H5I_INVALID_HID;
    hid_t       dxpl        = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    short       wbuf[DSET_SELECT_DIM];
    int         wbuf_bak[DSET_SELECT_DIM];
    short       trans_wbuf[DSET_SELECT_DIM];
    short       rbuf[DSET_SELECT_DIM];
    char        dset_name[DSET_NAME_LEN];
    const char *expr = "2 * (10 + x)";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Generate dataset name */
    snprintf(dset_name, sizeof(dset_name), "smaller_no_bkg_%s_%s_%s", chunked ? "chunked" : "contig",
             dtrans ? "xform" : "noxform", mwbuf ? "mwbuf" : "nomwbuf");

    /* Create 1d chunked dataset with/without data transform */
    if ((did = H5Dcreate2(fid, dset_name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = (short)i;
        trans_wbuf[i] = (short)(2 * (10 + wbuf[i]));
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        TEST_ERROR;

    /* Set data transform */
    if (dtrans) {
        if (H5Pset_data_transform(dxpl, expr) < 0)
            TEST_ERROR;
    }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(wbuf_bak, wbuf, sizeof(wbuf));

    /* Write data to the dataset with/without data transform in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(wbuf, wbuf_bak, sizeof(wbuf));

    /* Read data from the dataset without data transform in dxpl */
    if (H5Dread(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (2 * (10 + trans_wbuf[i]))) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Pclose(ntrans_dxpl);
    }
    H5E_END_TRY

    return FAIL;

} /* test_smaller_mem_type_no_bkg() */

/*
 *  Case 5: single dataset reade/write, compound types with background buffer
 *
 *  (a) Initialize compound buffer in memory with unique values
 *      Write all compound fields to disk
 *      Verify values read
 *  (b) Update all fields of the compound type in memory write buffer with new unique values
 *      Write some but not all all compound fields to disk
 *      Read the entire compound type
 *      Verify the fields have the correct (old or new) values
 *  (c) Update all fields of the compound type in memory read buffer with new unique values
 *      Read some but not all the compound fields to memory
 *      Verify the fields have the correct (old, middle  or new) values
 *  (d) Set up a different compound type which has:
 *      --no conversion for member types
 *      --a field with larger mem type
 *      --a field with smaller mem type
 *      Write this compound type to disk
 *      Read the entire compound type
 *      Verify the values read
 *
 */
static herr_t
test_cmpd_with_bkg(hid_t fid, unsigned chunked, unsigned mwbuf)
{
    int     i;
    hid_t   did       = H5I_INVALID_HID;
    hid_t   sid       = H5I_INVALID_HID;
    hid_t   dcpl      = H5I_INVALID_HID;
    hid_t   dxpl      = H5I_INVALID_HID;
    hid_t   s1_tid    = H5I_INVALID_HID;
    hid_t   s2_tid    = H5I_INVALID_HID;
    hid_t   ss_ac_tid = H5I_INVALID_HID;
    hid_t   ss_bc_tid = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    s1_t   *s1_wbuf     = NULL;
    s1_t   *s1_wbuf_bak = NULL;
    s1_t   *s1_rbuf     = NULL;
    s2_t   *s2_wbuf     = NULL;
    s2_t   *s2_wbuf_bak = NULL;
    s2_t   *s2_rbuf     = NULL;
    char    dset_name[DSET_NAME_LEN];
    s1_t    fillvalue;

    /* Initialize the fill value */
    memset(&fillvalue, 0, sizeof(s1_t));

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    /* Allocate buffers for datasets */
    if (NULL == (s1_wbuf = (s1_t *)malloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        TEST_ERROR;
    if (mwbuf && NULL == (s1_wbuf_bak = (s1_t *)malloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        TEST_ERROR;
    if (NULL == (s1_rbuf = (s1_t *)malloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        TEST_ERROR;
    if (NULL == (s2_wbuf = (s2_t *)malloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        TEST_ERROR;
    if (mwbuf && NULL == (s2_wbuf_bak = (s2_t *)malloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        TEST_ERROR;
    if (NULL == (s2_rbuf = (s2_t *)malloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        TEST_ERROR;

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_fill_value(dcpl, s1_tid, &fillvalue) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Case 5(a) */

    /* Generate dataset name */
    snprintf(dset_name, sizeof(dset_name), "cmpd_with_bkg_%s_%s", chunked ? "chunked" : "contig",
             mwbuf ? "mwbuf" : "nomwbuf");

    /* Create 1d dataset */
    if ((did = H5Dcreate2(fid, dset_name, s1_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_wbuf[i].a = 4 * i;
        s1_wbuf[i].b = (4 * i) + 1;
        s1_wbuf[i].c = (4 * i) + 2;
        s1_wbuf[i].d = (4 * i) + 3;
    }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(s1_wbuf_bak, s1_wbuf, sizeof(s1_t) * DSET_SELECT_DIM);

    /* Write all the data to the dataset */
    if (H5Dwrite(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, s1_wbuf) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(s1_wbuf, s1_wbuf_bak, sizeof(s1_t) * DSET_SELECT_DIM);

    /* Read all the data from the dataset */
    if (H5Dread(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_wbuf[i].a != s1_rbuf[i].a || s1_wbuf[i].b != s1_rbuf[i].b || s1_wbuf[i].c != s1_rbuf[i].c ||
            s1_wbuf[i].d != s1_rbuf[i].d) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    /* Case 5(b) */

    /* Update s1_wbuf with unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_wbuf[i].a = (4 * i) + DSET_SELECT_DIM;
        s1_wbuf[i].b = (4 * i) + DSET_SELECT_DIM + 1;
        s1_wbuf[i].c = (4 * i) + DSET_SELECT_DIM + 2;
        s1_wbuf[i].d = (4 * i) + DSET_SELECT_DIM + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_ac_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(s1_wbuf_bak, s1_wbuf, sizeof(s1_t) * DSET_SELECT_DIM);

    /* Write s1_wbuf to the dataset with only subset members in ss_tid */
    if (H5Dwrite(did, ss_ac_tid, H5S_ALL, H5S_ALL, dxpl, s1_wbuf) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(s1_wbuf, s1_wbuf_bak, sizeof(s1_t) * DSET_SELECT_DIM);

    /* Read the whole compound back */
    if (H5Dread(did, ss_ac_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        TEST_ERROR;

    /* Verify the compound fields have the correct (old or new) values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != s1_wbuf[i].a || s1_rbuf[i].b != ((4 * i) + 1) || s1_rbuf[i].c != s1_wbuf[i].c ||
            s1_rbuf[i].d != ((4 * i) + 3)) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    /* Case 5(c) */

    /* Update s1_rbuf with new unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_rbuf[i].a = (4 * i) + (2 * DSET_SELECT_DIM);
        s1_rbuf[i].b = (4 * i) + (2 * DSET_SELECT_DIM) + 1;
        s1_rbuf[i].c = (4 * i) + (2 * DSET_SELECT_DIM) + 2;
        s1_rbuf[i].d = (4 * i) + (2 * DSET_SELECT_DIM) + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_bc_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Read the dataset: will read only what is set in */
    if (H5Dread(did, ss_bc_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != ((4 * i) + (2 * DSET_SELECT_DIM)) || s1_rbuf[i].b != ((4 * i) + 1) ||
            s1_rbuf[i].c != ((4 * i) + DSET_SELECT_DIM + 2) ||
            s1_rbuf[i].d != ((4 * i) + (2 * DSET_SELECT_DIM) + 3)) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    /* Case 5(d) */

    /* Create s2_t compound type with:
     * --no conversion for 2 member types,
     * --1 larger mem type
     * --1 smaller mem type
     */
    if ((s2_tid = H5Tcreate(H5T_COMPOUND, sizeof(s2_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_SHORT) < 0)
        TEST_ERROR;

    /* Update s2_wbuf with unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s2_wbuf[i].a = 8 * i;
        s2_wbuf[i].b = (long)((8 * i) + 1);
        s2_wbuf[i].c = (8 * i) + 2;
        s2_wbuf[i].d = (short)((8 * i) + 3);
    }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(s2_wbuf_bak, s2_wbuf, sizeof(s2_t) * DSET_SELECT_DIM);

    if (H5Dwrite(did, s2_tid, H5S_ALL, H5S_ALL, dxpl, s2_wbuf) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(s2_wbuf, s2_wbuf_bak, sizeof(s2_t) * DSET_SELECT_DIM);

    /* Read it back */
    if (H5Dread(did, s2_tid, H5S_ALL, H5S_ALL, dxpl, s2_rbuf) < 0) {
        goto error;
    }

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s2_wbuf[i].a != s2_rbuf[i].a || s2_wbuf[i].b != s2_rbuf[i].b || s2_wbuf[i].c != s2_rbuf[i].c ||
            s2_wbuf[i].d != s2_rbuf[i].d) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Tclose(s1_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(s2_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(ss_ac_tid) < 0)
        TEST_ERROR;
    if (H5Tclose(ss_bc_tid) < 0)
        TEST_ERROR;
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    /* Release buffers */
    free(s1_wbuf);
    free(s1_wbuf_bak);
    free(s1_rbuf);
    free(s2_wbuf);
    free(s2_wbuf_bak);
    free(s2_rbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Tclose(s1_tid);
        H5Tclose(s2_tid);
        H5Tclose(ss_ac_tid);
        H5Tclose(ss_bc_tid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
    }
    H5E_END_TRY

    if (s1_wbuf)
        free(s1_wbuf);
    if (s1_wbuf_bak)
        free(s1_wbuf_bak);
    if (s1_rbuf)
        free(s1_rbuf);
    if (s2_wbuf)
        free(s2_wbuf);
    if (s2_wbuf_bak)
        free(s2_wbuf_bak);
    if (s2_rbuf)
        free(s2_rbuf);
    return FAIL;

} /* test_cmpd_with_bkg() */

/*
 *  Test 1 for multi-dataset:
 *  --Datasets with/without type conversion+smaller/larger mem type+no background buffer
 *
 *  Create datasets: randomized H5T_NATIVE_INT or H5T_NATIVE_LONG
 *
 *  Case a--setting for multi write/read to ndsets:
 *    Datatype for all datasets: H5T_NATIVE_INT
 *
 *  Case b--setting for multi write/read to ndsets:
 *    Datatype for all datasets: H5T_NATIVE_LONG
 */
static herr_t
test_multi_dsets_no_bkg(hid_t fid, unsigned set_cache, unsigned chunked, unsigned dtrans, unsigned mwbuf)
{
    size_t  ndsets;
    int     i, j;
    hid_t   dcpl        = H5I_INVALID_HID;
    hid_t   dxpl        = H5I_INVALID_HID;
    hid_t   ntrans_dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];

    hid_t file_sids[MULTI_NUM_DSETS];
    hid_t mem_sids[MULTI_NUM_DSETS];
    hid_t mem_tids[MULTI_NUM_DSETS];

    char  dset_names[MULTI_NUM_DSETS][DSET_NAME_LEN];
    hid_t dset_dids[MULTI_NUM_DSETS];

    size_t buf_size;
    int   *total_wbuf        = NULL;
    int   *total_wbuf_bak    = NULL;
    int   *total_trans_wbuf  = NULL;
    int   *total_rbuf        = NULL;
    long  *total_lwbuf       = NULL;
    long  *total_lwbuf_bak   = NULL;
    long  *total_trans_lwbuf = NULL;
    long  *total_lrbuf       = NULL;

    int *wbufi[MULTI_NUM_DSETS];
    int *trans_wbufi[MULTI_NUM_DSETS];
    int *rbufi[MULTI_NUM_DSETS];

    long *lwbufi[MULTI_NUM_DSETS];
    long *trans_lwbufi[MULTI_NUM_DSETS];
    long *lrbufi[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];
    const char *expr = "2*x";

    ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

    dims[0] = DSET_SELECT_DIM;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        TEST_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            TEST_ERROR;

    /* Set up file space ids, mem space ids, and dataset ids */
    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;

        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;

        /* Generate dataset name */
        snprintf(dset_names[i], sizeof(dset_names[i]), "multi_dset%d_%s_%s_%s", i,
                 chunked ? "chunked" : "contig", dtrans ? "xform" : "noxform", mwbuf ? "mwbuf" : "nomwbuf");

        /* Create ith dataset */
        if ((dset_dids[i] =
                 H5Dcreate2(fid, dset_names[i], ((HDrandom() % 2) ? H5T_NATIVE_LONG : H5T_NATIVE_INT),
                            file_sids[i], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            TEST_ERROR;
    }

    buf_size = ndsets * DSET_SELECT_DIM * sizeof(int);

    /* Allocate buffers for all datasets */
    if (NULL == (total_wbuf = (int *)malloc(buf_size)))
        TEST_ERROR;
    if (mwbuf && NULL == (total_wbuf_bak = (int *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_trans_wbuf = (int *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_rbuf = (int *)malloc(buf_size)))
        TEST_ERROR;

    buf_size = ndsets * DSET_SELECT_DIM * sizeof(long);

    if (NULL == (total_lwbuf = (long *)malloc(buf_size)))
        TEST_ERROR;
    if (mwbuf && NULL == (total_lwbuf_bak = (long *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_trans_lwbuf = (long *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_lrbuf = (long *)malloc(buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        wbufi[i]       = total_wbuf + (i * DSET_SELECT_DIM);
        trans_wbufi[i] = total_trans_wbuf + (i * DSET_SELECT_DIM);
        rbufi[i]       = total_rbuf + (i * DSET_SELECT_DIM);

        wbufs[i] = wbufi[i];
        rbufs[i] = rbufi[i];
    }

    /* Initialize the buffer data */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            wbufi[i][j]       = (int)j;
            trans_wbufi[i][j] = 2 * wbufi[i][j];
        }

    /* Case a */

    /* Datatype setting for multi write/read */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = H5T_NATIVE_INT;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_wbuf_bak, total_wbuf, ndsets * DSET_SELECT_DIM * sizeof(int));

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_wbuf, total_wbuf_bak, ndsets * DSET_SELECT_DIM * sizeof(int));

    /* Read data from the dataset (if dtrans, without data transform set in dxpl) */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, ntrans_dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify selection I/O mode */
    if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (rbufi[i][j] != (dtrans ? trans_wbufi[i][j] : wbufi[i][j])) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (dtrans) {

        /* Read the data from the dataset with data transform set in dxpl */
        if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
            TEST_ERROR;

        /* Verify selection I/O mode */
        if (check_actual_selection_io_mode(dxpl, chunked && !set_cache ? 0 : H5D_SCALAR_IO) < 0)
            TEST_ERROR;

        /* Verify */
        for (i = 0; i < (int)ndsets; i++)
            for (j = 0; j < DSET_SELECT_DIM; j++)
                if (rbufi[i][j] != (2 * trans_wbufi[i][j])) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
    }

    /* Case b */

    /* Datatype setting for multi write/read */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = H5T_NATIVE_LONG;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        lwbufi[i]       = total_lwbuf + (i * DSET_SELECT_DIM);
        trans_lwbufi[i] = total_trans_lwbuf + (i * DSET_SELECT_DIM);
        lrbufi[i]       = total_lrbuf + (i * DSET_SELECT_DIM);
        wbufs[i]        = lwbufi[i];
        rbufs[i]        = lrbufi[i];
    }

    /* Initialize the buffer data */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            lwbufi[i][j]       = (long)j + DSET_SELECT_DIM;
            trans_lwbufi[i][j] = 2 * lwbufi[i][j];
        }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_lwbuf_bak, total_lwbuf, ndsets * DSET_SELECT_DIM * sizeof(long));

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_lwbuf, total_lwbuf_bak, ndsets * DSET_SELECT_DIM * sizeof(long));

    /* Read data from the dataset (if dtrans, with data transform again in dxpl) */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            if (lrbufi[i][j] != (dtrans ? (2 * trans_lwbufi[i][j]) : lwbufi[i][j])) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }
        }
    }

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            TEST_ERROR;
    }

    free(total_wbuf);
    free(total_wbuf_bak);
    free(total_rbuf);
    free(total_trans_wbuf);
    free(total_lwbuf);
    free(total_lwbuf_bak);
    free(total_lrbuf);
    free(total_trans_lwbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    H5Pclose(dcpl);
    H5Pclose(dxpl);
    H5Pclose(ntrans_dxpl);
    for (i = 0; i < (int)ndsets; i++) {
        H5Sclose(file_sids[i]);
        H5Sclose(mem_sids[i]);
        H5Dclose(dset_dids[i]);
    }
    H5E_END_TRY

    if (total_wbuf)
        free(total_wbuf);
    if (total_wbuf_bak)
        free(total_wbuf_bak);
    if (total_trans_wbuf)
        free(total_trans_wbuf);
    if (total_rbuf)
        free(total_rbuf);
    if (total_lwbuf)
        free(total_lwbuf);
    if (total_lwbuf_bak)
        free(total_lwbuf_bak);
    if (total_lrbuf)
        free(total_lrbuf);
    if (total_trans_lwbuf)
        free(total_trans_lwbuf);

    return FAIL;

} /* test_multi_dsets_no_bkg() */

/*
 * Test 2 for multi-dataset:
 *
 *   Datasets with compound types+background buffer
 *
 *   Create datasets with the same compound type
 *   (a) Initialize compound buffer in memory with unique values
 *       All datasets:
 *       --Write all compound fields to disk
 *       --Read the entire compound type for all datasets
 *       --Verify values read
 *   (b) Update all fields of the compound type in memory write buffer with new unique values
 *       dset0:
 *       --Write some but not all all compound fields to disk
 *       --Read and verify the fields have the correct (old(a) or new) values
 *       Remaining datasets:
 *       --Untouched
 *       --Read and verify the fields have the correct old(a) values
 *   (c) Update all fields of the compound type in memory read buffer with new unique values
 *       Randomized <mm> dataset:
 *       --Read some but not all the compound fields to memory
 *       --Verify the fields have the correct (old(a) or new) values
 *       dset0:
 *       --Untouched
 *       --Read and verify the fields have the correct (old(a) or middle(b)) values
 *       Remaining datasets:
 *       --Untouched
 *       --Read and verify the fields have the correct old(a) values
 *  (d)  Set up a different compound type which has:
 *      --no type conversion for 2 member types
 *      --a field with larger mem type
 *      --a field with smaller mem type
 *      All datasets:
 *      --Write the compound fields to disk
 *      --Read the entire compound type
 *      --Verify values read
 */
static herr_t
test_multi_dsets_cmpd_with_bkg(hid_t fid, unsigned chunked, unsigned mwbuf)
{
    size_t  ndsets;
    int     i, j, mm;
    hid_t   dcpl = H5I_INVALID_HID;
    hid_t   dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];

    hid_t file_sids[MULTI_NUM_DSETS];
    hid_t mem_sids[MULTI_NUM_DSETS];
    hid_t mem_tids[MULTI_NUM_DSETS];

    hid_t s1_tid    = H5I_INVALID_HID;
    hid_t ss_ac_tid = H5I_INVALID_HID;
    hid_t ss_bc_tid = H5I_INVALID_HID;
    hid_t s2_tid    = H5I_INVALID_HID;

    char  dset_names[MULTI_NUM_DSETS][DSET_NAME_LEN];
    hid_t dset_dids[MULTI_NUM_DSETS];

    size_t buf_size;
    size_t s2_buf_size;

    s1_t *total_wbuf     = NULL;
    s1_t *total_wbuf_bak = NULL;
    s1_t *total_rbuf     = NULL;

    s2_t *s2_total_wbuf     = NULL;
    s2_t *s2_total_wbuf_bak = NULL;
    s2_t *s2_total_rbuf     = NULL;

    s1_t *wbufi[MULTI_NUM_DSETS];
    s1_t *rbufi[MULTI_NUM_DSETS];

    s2_t *s2_wbufi[MULTI_NUM_DSETS];
    s2_t *s2_rbufi[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];

    ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    dims[0] = DSET_SELECT_DIM;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;
        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;

        /* Generate dataset name */
        snprintf(dset_names[i], sizeof(dset_names[i]), "multi_cmpd_dset%d_%s_%s", i,
                 chunked ? "chunked" : "contig", mwbuf ? "mwbuf" : "nomwbuf");

        /* Create ith dataset */
        if ((dset_dids[i] =
                 H5Dcreate2(fid, dset_names[i], s1_tid, file_sids[i], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            TEST_ERROR;
    }

    buf_size    = ndsets * DSET_SELECT_DIM * sizeof(s1_t);
    s2_buf_size = ndsets * DSET_SELECT_DIM * sizeof(s2_t);

    /* Allocate buffers */
    if (NULL == (total_wbuf = (s1_t *)malloc(buf_size)))
        TEST_ERROR;
    if (mwbuf && NULL == (total_wbuf_bak = (s1_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_rbuf = (s1_t *)malloc(buf_size)))
        TEST_ERROR;

    if (NULL == (s2_total_wbuf = (s2_t *)malloc(s2_buf_size)))
        TEST_ERROR;
    if (mwbuf && NULL == (s2_total_wbuf_bak = (s2_t *)malloc(s2_buf_size)))
        TEST_ERROR;
    if (NULL == (s2_total_rbuf = (s2_t *)malloc(s2_buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        wbufi[i] = total_wbuf + (i * DSET_SELECT_DIM);
        rbufi[i] = total_rbuf + (i * DSET_SELECT_DIM);

        wbufs[i] = wbufi[i];
        rbufs[i] = rbufi[i];
    }

    /* Case a */

    /* Initialize the buffer data for all the datasets */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            wbufi[i][j].a = (4 * j);
            wbufi[i][j].b = (4 * j) + 1;
            wbufi[i][j].c = (4 * j) + 2;
            wbufi[i][j].d = (4 * j) + 3;
        }

    /* Datatype setting for multi write */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = s1_tid;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_wbuf_bak, total_wbuf, buf_size);

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_wbuf, total_wbuf_bak, buf_size);

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            if (wbufi[i][j].a != rbufi[i][j].a || wbufi[i][j].b != rbufi[i][j].b ||
                wbufi[i][j].c != rbufi[i][j].c || wbufi[i][j].d != rbufi[i][j].d) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }
        }

    /* Case b */

    /* Update data in wbufi for dset0 with unique values */
    for (j = 0; j < DSET_SELECT_DIM; j++) {
        wbufi[0][j].a = (4 * j) + DSET_SELECT_DIM;
        wbufi[0][j].b = (4 * j) + DSET_SELECT_DIM + 1;
        wbufi[0][j].c = (4 * j) + DSET_SELECT_DIM + 2;
        wbufi[0][j].d = (4 * j) + DSET_SELECT_DIM + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_ac_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Untouched memory and file spaces for other datasets */
    for (i = 0; i < (int)ndsets; i++) {
        if (i == 0)
            continue;

        if (H5Sselect_none(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sselect_none(file_sids[i]) < 0)
            TEST_ERROR;
    }

    /* Datatype setting for write to dset0 */
    mem_tids[0] = ss_ac_tid;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_wbuf_bak, total_wbuf, buf_size);

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_wbuf, total_wbuf_bak, buf_size);

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (i == 0) { /* dset0 */
                if (wbufi[i][j].a != rbufi[i][j].a || ((4 * (int)j) + 1) != rbufi[i][j].b ||
                    wbufi[i][j].c != rbufi[i][j].c || ((4 * (int)j) + 3) != rbufi[i][j].d) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
            }
            else { /* other datasets */
                for (j = 0; j < DSET_SELECT_DIM; j++)
                    if ((4 * (int)j) != rbufi[i][j].a || ((4 * (int)j) + 1) != rbufi[i][j].b ||
                        ((4 * (int)j) + 2) != rbufi[i][j].c || ((4 * (int)j) + 3) != rbufi[i][j].d) {
                        H5_FAILED();
                        printf("    Read different values than written.\n");
                        printf("    For dset %d at index %d\n", i, j);
                        TEST_ERROR;
                    }
            }

    /* Case c */
    mm = HDrandom() % (int)ndsets;
    if (!mm)
        mm++;

    /* Update data in rbufi for dset1 with new unique values */
    for (j = 0; j < DSET_SELECT_DIM; j++) {
        rbufi[mm][j].a = (4 * j) + (2 * DSET_SELECT_DIM);
        rbufi[mm][j].b = (4 * j) + (2 * DSET_SELECT_DIM) + 1;
        rbufi[mm][j].c = (4 * j) + (2 * DSET_SELECT_DIM) + 2;
        rbufi[mm][j].d = (4 * j) + (2 * DSET_SELECT_DIM) + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_bc_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    /* Reset memory and file space for <mm> dataset */
    if (H5Sselect_all(mem_sids[mm]) < 0)
        TEST_ERROR;
    if (H5Sselect_all(file_sids[mm]) < 0)
        TEST_ERROR;

    /* Untouched memory and file space for other datasets */
    for (i = 0; i < (int)ndsets; i++) {
        if (i == 0 || i == mm)
            continue;
        if (H5Sselect_none(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sselect_none(file_sids[i]) < 0)
            TEST_ERROR;
    }

    /* Datatype setting for read from <mm> dataset */
    mem_tids[mm] = ss_bc_tid;

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    /* dset0 */
    for (j = 0; j < DSET_SELECT_DIM; j++)
        if (wbufi[0][j].a != rbufi[0][j].a || ((4 * (int)j) + 1) != rbufi[0][j].b ||
            wbufi[0][j].c != rbufi[0][j].c || ((4 * (int)j) + 3) != rbufi[0][j].d) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    For dset0 at index %d\n", j);
            TEST_ERROR;
        }

    /* <mm> dset */
    for (j = 0; j < DSET_SELECT_DIM; j++)
        if (rbufi[mm][j].a != ((4 * (int)j) + (2 * DSET_SELECT_DIM)) ||
            rbufi[mm][j].b != ((4 * (int)j) + 1) || rbufi[mm][j].c != ((4 * (int)j) + 2) ||
            rbufi[mm][j].d != ((4 * (int)j) + (2 * DSET_SELECT_DIM) + 3)) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    For dset1 at index %d\n", j);
            TEST_ERROR;
        }

    /* other datasets */
    for (i = 0; i < (int)ndsets; i++) {
        if (i == 0 || i == mm)
            continue;

        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (rbufi[i][j].a != (4 * (int)j) || rbufi[i][j].b != ((4 * (int)j) + 1) ||
                rbufi[i][j].c != ((4 * (int)j) + 2) || rbufi[i][j].d != ((4 * (int)j) + 3)) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }
    }

    /* Case d */

    /* Create s2_t compound type with:
     * --no conversion for 2 member types,
     * --1 larger mem type
     * --1 smaller mem type
     */
    if ((s2_tid = H5Tcreate(H5T_COMPOUND, sizeof(s2_t))) < 0)
        TEST_ERROR;

    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_SHORT) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        s2_wbufi[i] = s2_total_wbuf + (i * DSET_SELECT_DIM);
        s2_rbufi[i] = s2_total_rbuf + (i * DSET_SELECT_DIM);

        wbufs[i] = s2_wbufi[i];
        rbufs[i] = s2_rbufi[i];

        mem_tids[i] = s2_tid;

        if (H5Sselect_all(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sselect_all(file_sids[i]) < 0)
            TEST_ERROR;
    }

    /* Initialize the buffer data for all the datasets */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            s2_wbufi[i][j].a = 8 * j;
            s2_wbufi[i][j].b = (long)((8 * j) + 1);
            s2_wbufi[i][j].c = (8 * j) + 2;
            s2_wbufi[i][j].d = (short)((8 * j) + 3);
        }

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(s2_total_wbuf_bak, s2_total_wbuf, s2_buf_size);

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(s2_total_wbuf, s2_total_wbuf_bak, s2_buf_size);

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (s2_wbufi[i][j].a != s2_rbufi[i][j].a || s2_wbufi[i][j].b != s2_rbufi[i][j].b ||
                s2_wbufi[i][j].c != s2_rbufi[i][j].c || s2_wbufi[i][j].d != s2_rbufi[i][j].d) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            TEST_ERROR;
    }

    free(total_wbuf);
    free(total_wbuf_bak);
    free(total_rbuf);
    free(s2_total_wbuf);
    free(s2_total_wbuf_bak);
    free(s2_total_rbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    H5Pclose(dcpl);
    H5Pclose(dxpl);
    for (i = 0; i < (int)ndsets; i++) {
        H5Sclose(file_sids[i]);
        H5Sclose(mem_sids[i]);
        H5Dclose(dset_dids[i]);
    }
    H5E_END_TRY

    if (total_wbuf)
        free(total_wbuf);
    if (total_wbuf_bak)
        free(total_wbuf_bak);
    if (total_rbuf)
        free(total_rbuf);
    if (s2_total_wbuf)
        free(s2_total_wbuf);
    if (s2_total_wbuf_bak)
        free(s2_total_wbuf_bak);
    if (s2_total_rbuf)
        free(s2_total_rbuf);

    return FAIL;

} /* test_multi_dsets_cmpd_with_bkg() */

/*
 *  Test 3 for multi-dataset:
 *  --Datasets with/without type conv+size change+no background buffer
 *
 *  Create dset0: H5T_STD_I32BE
 *  Create other dateasets: randomized H5T_STD_I64LE or H5T_STD_I16LE
 *
 *  Case a--setting for write/read to ndsets:
 *    Datatype for all datasets: H5T_STD_I32BE
 *
 *  Case b--setting for write/read to ndsets
 *    Datatype for all datasets: H5T_STD_I64BE
 *
 *  Case c--setting for write/read to ndsets
 *    Datatype for all datasets: H5T_STD_I16BE
 */
static herr_t
test_multi_dsets_size_change_no_bkg(hid_t fid, unsigned chunked, unsigned mwbuf)
{
    size_t  ndsets;
    int     i, j;
    hid_t   dcpl = H5I_INVALID_HID;
    hid_t   dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];

    hid_t file_sids[MULTI_NUM_DSETS];
    hid_t mem_sids[MULTI_NUM_DSETS];
    hid_t mem_tids[MULTI_NUM_DSETS];

    char  dset_names[MULTI_NUM_DSETS][DSET_NAME_LEN];
    hid_t dset_dids[MULTI_NUM_DSETS];

    size_t   buf_size, ss;
    uint8_t *total_wbuf      = NULL;
    uint8_t *total_wbuf_bak  = NULL;
    uint8_t *total_rbuf      = NULL;
    uint8_t *total_lwbuf     = NULL;
    uint8_t *total_lwbuf_bak = NULL;
    uint8_t *total_lrbuf     = NULL;
    uint8_t *total_swbuf     = NULL;
    uint8_t *total_swbuf_bak = NULL;
    uint8_t *total_srbuf     = NULL;

    uint8_t *wbufi[MULTI_NUM_DSETS];
    uint8_t *rbufi[MULTI_NUM_DSETS];
    uint8_t *lwbufi[MULTI_NUM_DSETS];
    uint8_t *lrbufi[MULTI_NUM_DSETS];
    uint8_t *swbufi[MULTI_NUM_DSETS];
    uint8_t *srbufi[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];

    ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    /* Set modify write buffer if requested */
    if (mwbuf)
        if (H5Pset_modify_write_buf(dxpl, true) < 0)
            TEST_ERROR;

    dims[0] = DSET_SELECT_DIM;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    /* Set up file space ids, mem space ids, and dataset ids */
    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;

        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            TEST_ERROR;

        /* Generate dataset name */
        snprintf(dset_names[i], sizeof(dset_names[i]), "multi_size_dset%d_%s_%s", i,
                 chunked ? "chunked" : "contig", mwbuf ? "mwbuf" : "nomwbuf");

        /* Create ith dataset */
        if ((dset_dids[i] = H5Dcreate2(fid, dset_names[i], H5T_STD_I32BE, file_sids[i], H5P_DEFAULT, dcpl,
                                       H5P_DEFAULT)) < 0)
            TEST_ERROR;
    }

    /* Case a */

    ss       = H5Tget_size(H5T_STD_I32BE);
    buf_size = ndsets * ss * DSET_SELECT_DIM;

    /* Allocate buffers for all datasets */
    if (NULL == (total_wbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_wbuf_bak = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_rbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        wbufi[i] = total_wbuf + (i * (int)ss * DSET_SELECT_DIM);
        rbufi[i] = total_rbuf + (i * (int)ss * DSET_SELECT_DIM);

        wbufs[i] = wbufi[i];
        rbufs[i] = rbufi[i];
    }

    /* Initialize the buffer data: big endian */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            wbufi[i][j * (int)ss + 0] = 0x1;
            wbufi[i][j * (int)ss + 1] = 0x2;
            wbufi[i][j * (int)ss + 2] = 0x3;
            wbufi[i][j * (int)ss + 3] = (uint8_t)(0x4 + j);
        }

    /* Datatype setting for multi write/read */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = H5T_STD_I32BE;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_wbuf_bak, total_wbuf, buf_size);

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_wbuf, total_wbuf_bak, buf_size);

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        /* Only compare when it's at least the size of H5T_STD_I32BE */
        if (H5Tget_size(H5Dget_type(dset_dids[i])) >= ss) {
            for (j = 0; j < DSET_SELECT_DIM; j++)
                if (rbufi[i][(int)ss * j + 0] != wbufi[i][(int)ss * j + 0] ||
                    rbufi[i][(int)ss * j + 1] != wbufi[i][(int)ss * j + 1] ||
                    rbufi[i][(int)ss * j + 2] != wbufi[i][(int)ss * j + 2] ||
                    rbufi[i][(int)ss * j + 3] != wbufi[i][(int)ss * j + 3]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
        }

    /* Case b */

    ss       = H5Tget_size(H5T_STD_I64BE);
    buf_size = ndsets * (ss * DSET_SELECT_DIM);

    /* Allocate buffers for all datasets */
    if (NULL == (total_lwbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_lwbuf_bak = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_lrbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        lwbufi[i] = total_lwbuf + (i * (int)ss * DSET_SELECT_DIM);
        lrbufi[i] = total_lrbuf + (i * (int)ss * DSET_SELECT_DIM);

        wbufs[i] = lwbufi[i];
        rbufs[i] = lrbufi[i];
    }

    /* Initialize the buffer data: big endian */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            lwbufi[i][j * (int)ss + 0] = 0x1;
            lwbufi[i][j * (int)ss + 1] = 0x2;
            lwbufi[i][j * (int)ss + 2] = 0x3;
            lwbufi[i][j * (int)ss + 3] = 0x4;
            lwbufi[i][j * (int)ss + 4] = 0x5;
            lwbufi[i][j * (int)ss + 5] = 0x6;
            lwbufi[i][j * (int)ss + 6] = 0x7;
            lwbufi[i][j * (int)ss + 7] = (uint8_t)(0x8 + j);
        }

    /* Datatype setting for multi write/read */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = H5T_STD_I64BE;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_lwbuf_bak, total_lwbuf, buf_size);

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_lwbuf, total_lwbuf_bak, buf_size);

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        /* Only compare when it's the size of H5T_STD_I64BE */
        if (H5Tget_size(H5Dget_type(dset_dids[i])) >= ss) {
            for (j = 0; j < DSET_SELECT_DIM; j++)
                if (lrbufi[i][(int)ss * j + 0] != lwbufi[i][(int)ss * j + 0] ||
                    lrbufi[i][(int)ss * j + 1] != lwbufi[i][(int)ss * j + 1] ||
                    lrbufi[i][(int)ss * j + 2] != lwbufi[i][(int)ss * j + 2] ||
                    lrbufi[i][(int)ss * j + 3] != lwbufi[i][(int)ss * j + 3] ||
                    lrbufi[i][(int)ss * j + 4] != lwbufi[i][(int)ss * j + 4] ||
                    lrbufi[i][(int)ss * j + 5] != lwbufi[i][(int)ss * j + 5] ||
                    lrbufi[i][(int)ss * j + 6] != lwbufi[i][(int)ss * j + 6] ||
                    lrbufi[i][(int)ss * j + 7] != lwbufi[i][(int)ss * j + 7]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
        }

    /* Case c */

    ss       = H5Tget_size(H5T_STD_I16BE);
    buf_size = ndsets * (ss * DSET_SELECT_DIM);

    /* Allocate buffers for all datasets */
    if (NULL == (total_swbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_swbuf_bak = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_srbuf = (uint8_t *)malloc(buf_size)))
        TEST_ERROR;

    /* Initialize buffer indices */
    for (i = 0; i < (int)ndsets; i++) {
        swbufi[i] = total_swbuf + (i * (int)ss * DSET_SELECT_DIM);
        srbufi[i] = total_srbuf + (i * (int)ss * DSET_SELECT_DIM);

        wbufs[i] = swbufi[i];
        rbufs[i] = srbufi[i];
    }

    /* Initialize the buffer data: big endian */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            swbufi[i][j * (int)ss + 0] = 0x1;
            swbufi[i][j * (int)ss + 1] = (uint8_t)(0x2 + j);
        }

    /* Datatype setting for multi write/read */
    for (i = 0; i < (int)ndsets; i++)
        mem_tids[i] = H5T_STD_I16BE;

    /* Copy wbuf if the library will be modifying it */
    if (mwbuf)
        memcpy(total_swbuf_bak, total_swbuf, buf_size);

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Restore wbuf from backup if the library modified it */
    if (mwbuf)
        memcpy(total_swbuf, total_swbuf_bak, buf_size);

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        /* Can compare for all cases */
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (srbufi[i][(int)ss * j + 0] != swbufi[i][(int)ss * j + 0] ||
                srbufi[i][(int)ss * j + 1] != swbufi[i][(int)ss * j + 1]) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            TEST_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            TEST_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            TEST_ERROR;
    }

    free(total_wbuf);
    free(total_wbuf_bak);
    free(total_rbuf);
    free(total_lwbuf);
    free(total_lwbuf_bak);
    free(total_lrbuf);
    free(total_swbuf);
    free(total_swbuf_bak);
    free(total_srbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    H5Pclose(dcpl);
    H5Pclose(dxpl);
    for (i = 0; i < (int)ndsets; i++) {
        H5Sclose(file_sids[i]);
        H5Sclose(mem_sids[i]);
        H5Dclose(dset_dids[i]);
    }
    H5E_END_TRY

    if (total_wbuf)
        free(total_wbuf);
    if (total_wbuf_bak)
        free(total_wbuf_bak);
    if (total_rbuf)
        free(total_rbuf);
    if (total_lwbuf)
        free(total_lwbuf);
    if (total_lwbuf_bak)
        free(total_lwbuf_bak);
    if (total_lrbuf)
        free(total_lrbuf);
    if (total_swbuf)
        free(total_swbuf);
    if (total_swbuf_bak)
        free(total_swbuf_bak);
    if (total_srbuf)
        free(total_srbuf);

    return FAIL;

} /* test_multi_dsets_size_change_no_bkg() */

/*
 * Test 4 for multi-dataset:
 *
 * Repeat the following test for niter times to ensure the
 * random combinations of all dataset types are hit.
 *
 * Create randomized contiguous or chunked datasets with:
 * --DSET_WITH_NO_CONV:
 *   --with no type conversion
 *   --dataset with H5T_NATIVE_INT
 * --DSET_WITH_CONV_AND_NO_BKG:
 *   --type conversion without background buffer
 *   --dataset with H5T_NATIVE_LONG
 * --DSET_WITH_CONV_AND_BKG:
 *   --type conversion with background buffer
 *   --dataset with compound type s1_t
 *
 * Do H5Dwrite_multi() and H5Dread_multi() for the above datasets:
 * Setting A:
 * --DSET_WITH_NO_CONV:
 *   --write: mem_tids[] = H5T_NATIVE_INT
 *   --read: r_mem_tids[] = H5T_NATIVE_INT
 * --DSET_WITH_CONV_AND_NO_BKG:
 *   --write: mem_tids[] = H5T_NATIVE_ULONG
 *   --read: r_mem_tids[] = H5T_NATIVE_LONG
 * --DSET_WITH_CONV_AND_BKG:
 *   --write: mem_tids[] = s1_tid;
 *   --read: r_mem_tids[i] = s3_tid;
 *
 * Setting B:
 * --DSET_WITH_NO_CONV:
 *   --write: mem_tids[] = H5T_NATIVE_INT
 *   --read: r_mem_tids[] = H5T_NATIVE_INT
 * --DSET_WITH_CONV_AND_NO_BKG:
 *   --write: mem_tids[] = H5T_NATIVE_LONG;
 *   --read: r_mem_tids[] = H5T_NATIVE_SHORT;
 * --DSET_WITH_CONV_AND_BKG:
 *   --write: mem_tids[] = s4_tid;
 *   --read: r_mem_tids[i] = s1_tid;
 *
 * Verify the result read as below:
 * Setting A:
 * --DSET_WITH_NO_CONV:
 *   --verify data read in rbufi1[i][j] is same as wbufi1[i][j]
 * --DSET_WITH_CONV_AND_NO_BKG:
 *   --verify data read in l_rbufi2[i][j] is all LONG_MAX
 * --DSET_WITH_CONV_AND_BKG:
 *   --verify all fields read in s3_rbufi3[i][j] is the
 *     reverse of s1_wbufi3[i][j]
 * Setting B:
 * --DSET_WITH_NO_CONV:
 *   --verify data read in rbufi1[i][j] is same as wbufi1[i][j]
 * --DSET_WITH_CONV_AND_NO_BKG:
 *   --verify data read in s_rbufi2[i][j] is all SHRT_MAX
 * --DSET_WITH_CONV_AND_BKG:
 *   --verify fields read in s1_rbufi3[i][j] is as follows:
 *     --fields 'a' and 'c' are as  s1_wbufi3[i][j].a and s1_wbufi3[i][j].c
 *     --fields 'b' and 'd' are (DSET_SELECT_DIM + j + start[0])
 *
 */
static herr_t
test_multi_dsets_all(int niter, hid_t fid, unsigned chunked, unsigned mwbuf)
{
    size_t  ndsets;
    int     i, j, mm;
    int     s, n;
    hid_t   dcpl = H5I_INVALID_HID;
    hid_t   dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];

    hid_t file_sids[MULTI_NUM_DSETS];
    hid_t mem_sids[MULTI_NUM_DSETS];
    hid_t mem_tids[MULTI_NUM_DSETS];
    hid_t r_mem_tids[MULTI_NUM_DSETS];

    multi_dset_type_t dset_types[MULTI_NUM_DSETS];

    hid_t s1_tid = H5I_INVALID_HID;
    hid_t s3_tid = H5I_INVALID_HID;
    hid_t s4_tid = H5I_INVALID_HID;

    char  dset_names[MULTI_NUM_DSETS][DSET_NAME_LEN];
    hid_t dset_dids[MULTI_NUM_DSETS];

    size_t buf_size;

    int *total_wbuf1     = NULL;
    int *total_wbuf1_bak = NULL;
    int *total_rbuf1     = NULL;

    int *wbufi1[MULTI_NUM_DSETS];
    int *rbufi1[MULTI_NUM_DSETS];

    unsigned long *ul_total_wbuf2     = NULL;
    unsigned long *ul_total_wbuf2_bak = NULL;
    long          *l_total_rbuf2      = NULL;
    unsigned long *ul_wbufi2[MULTI_NUM_DSETS];
    long          *l_rbufi2[MULTI_NUM_DSETS];

    long  *l_total_wbuf2     = NULL;
    long  *l_total_wbuf2_bak = NULL;
    short *s_total_rbuf2     = NULL;
    long  *l_wbufi2[MULTI_NUM_DSETS];
    short *s_rbufi2[MULTI_NUM_DSETS];

    s1_t *s1_total_wbuf3     = NULL;
    s1_t *s1_total_wbuf3_bak = NULL;
    s3_t *s3_total_rbuf3     = NULL;
    s1_t *s1_wbufi3[MULTI_NUM_DSETS];
    s3_t *s3_rbufi3[MULTI_NUM_DSETS];

    s4_t *s4_total_wbuf3     = NULL;
    s4_t *s4_total_wbuf3_bak = NULL;
    s1_t *s1_total_rbuf3     = NULL;
    s4_t *s4_wbufi3[MULTI_NUM_DSETS];
    s1_t *s1_rbufi3[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];

    /* for n niter to ensure that all randomized dset_types with multi_dset_type_t will be covered */
    for (n = 0; n < niter; n++) {

        /* Set up the number of datasets for testing */
        ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

        /* Create dataset transfer property list */
        if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
            TEST_ERROR;

        /* Enable selection I/O */
        if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
            TEST_ERROR;

        /* Set modify write buffer if requested */
        if (mwbuf)
            if (H5Pset_modify_write_buf(dxpl, true) < 0)
                TEST_ERROR;

        /* Set dataset layout: contiguous or chunked */
        dims[0] = DSET_SELECT_DIM;
        if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR;

        if (chunked) {
            cdims[0] = DSET_SELECT_CHUNK_DIM;
            if (H5Pset_chunk(dcpl, 1, cdims) < 0)
                TEST_ERROR;
        }

        /* Create compound data type: s1_t  */
        if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
            TEST_ERROR;

        if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
            TEST_ERROR;

        /* Create compound data type: s3_t  */
        if ((s3_tid = H5Tcreate(H5T_COMPOUND, sizeof(s3_t))) < 0)
            TEST_ERROR;

        if (H5Tinsert(s3_tid, "a", HOFFSET(s3_t, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s3_tid, "b", HOFFSET(s3_t, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s3_tid, "c", HOFFSET(s3_t, c), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(s3_tid, "d", HOFFSET(s3_t, d), H5T_NATIVE_INT) < 0)
            TEST_ERROR;

        /* Create compound data type: s4_t  */
        if ((s4_tid = H5Tcreate(H5T_COMPOUND, sizeof(s4_t))) < 0)
            TEST_ERROR;

        if (H5Tinsert(s4_tid, "b", HOFFSET(s4_t, b), H5T_NATIVE_UINT) < 0 ||
            H5Tinsert(s4_tid, "d", HOFFSET(s4_t, d), H5T_NATIVE_UINT) < 0)
            TEST_ERROR;

        /* Create dataset for i ndsets */
        for (i = 0; i < (int)ndsets; i++) {

            /* File space ids */
            if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
                TEST_ERROR;

            /* Memory space ids */
            if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
                TEST_ERROR;

            mm = HDrandom() % (int)ndsets;
            if (mm == 0) {
                dset_types[i] = DSET_WITH_NO_CONV;
                snprintf(dset_names[i], sizeof(dset_names[i]), "multi_all_nconv_dset%d_%s_%s", i,
                         chunked ? "chunked" : "contig", mwbuf ? "mwbuf" : "nomwbuf");
                if ((dset_dids[i] = H5Dcreate2(fid, dset_names[i], H5T_NATIVE_INT, file_sids[i], H5P_DEFAULT,
                                               dcpl, H5P_DEFAULT)) < 0)
                    TEST_ERROR;
            }
            else if (mm == 1) {
                dset_types[i] = DSET_WITH_CONV_AND_NO_BKG;
                snprintf(dset_names[i], sizeof(dset_names[i]), "multi_all_conv_nbkg_dset%d_%s_%s", i,
                         chunked ? "chunked" : "contig", mwbuf ? "mwbuf" : "nomwbuf");
                if ((dset_dids[i] = H5Dcreate2(fid, dset_names[i], H5T_NATIVE_LONG, file_sids[i], H5P_DEFAULT,
                                               dcpl, H5P_DEFAULT)) < 0)
                    TEST_ERROR;
            }
            else {
                dset_types[i] = DSET_WITH_CONV_AND_BKG;
                snprintf(dset_names[i], sizeof(dset_names[i]), "multi_all_conv_bkg_dset%d_%s_%s", i,
                         chunked ? "chunked" : "contig", mwbuf ? "mwbuf" : "nomwbuf");
                if ((dset_dids[i] = H5Dcreate2(fid, dset_names[i], s1_tid, file_sids[i], H5P_DEFAULT, dcpl,
                                               H5P_DEFAULT)) < 0)
                    TEST_ERROR;
            }

        } /* end for i ndsets */

        /* Allocate buffers for all datasets */

        /* DSET_WITH_NO_CONV */
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(int);
        if (NULL == (total_wbuf1 = (int *)malloc(buf_size)))
            TEST_ERROR;
        if (mwbuf && NULL == (total_wbuf1_bak = (int *)malloc(buf_size)))
            TEST_ERROR;
        if (NULL == (total_rbuf1 = (int *)malloc(buf_size)))
            TEST_ERROR;

        /* DSET_WITH_CONV_AND_NO_BKG */
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(unsigned long);
        if (NULL == (ul_total_wbuf2 = (unsigned long *)malloc(buf_size)))
            TEST_ERROR;
        if (mwbuf && NULL == (ul_total_wbuf2_bak = (unsigned long *)malloc(buf_size)))
            TEST_ERROR;
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(long);
        if (NULL == (l_total_rbuf2 = (long *)malloc(buf_size)))
            TEST_ERROR;

        buf_size = ndsets * DSET_SELECT_DIM * sizeof(long);
        if (NULL == (l_total_wbuf2 = (long *)malloc(buf_size)))
            TEST_ERROR;
        if (mwbuf && NULL == (l_total_wbuf2_bak = (long *)malloc(buf_size)))
            TEST_ERROR;
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(short);
        if (NULL == (s_total_rbuf2 = (short *)malloc(buf_size)))
            TEST_ERROR;

        /* DSET_WITH_CONV_AND_BKG */
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(s1_t);
        if (NULL == (s1_total_wbuf3 = (s1_t *)malloc(buf_size)))
            TEST_ERROR;
        if (mwbuf && NULL == (s1_total_wbuf3_bak = (s1_t *)malloc(buf_size)))
            TEST_ERROR;
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(s3_t);
        if (NULL == (s3_total_rbuf3 = (s3_t *)malloc(buf_size)))
            TEST_ERROR;

        buf_size = ndsets * DSET_SELECT_DIM * sizeof(s4_t);
        if (NULL == (s4_total_wbuf3 = (s4_t *)malloc(buf_size)))
            TEST_ERROR;
        if (mwbuf && NULL == (s4_total_wbuf3_bak = (s4_t *)malloc(buf_size)))
            TEST_ERROR;
        buf_size = ndsets * DSET_SELECT_DIM * sizeof(s1_t);
        if (NULL == (s1_total_rbuf3 = (s1_t *)malloc(buf_size)))
            TEST_ERROR;

        /* Test with s settings for ndsets */
        for (s = SETTING_A; s <= SETTING_B; s++) {

            /* for i ndsets */
            for (i = 0; i < (int)ndsets; i++) {

                switch (dset_types[i]) {

                    case DSET_WITH_NO_CONV:
                        /* Initialize buffer indices */
                        wbufi1[i] = total_wbuf1 + (i * DSET_SELECT_DIM);
                        rbufi1[i] = total_rbuf1 + (i * DSET_SELECT_DIM);

                        wbufs[i] = wbufi1[i];
                        rbufs[i] = rbufi1[i];

                        /* Initialize the buffer data */
                        for (j = 0; j < DSET_SELECT_DIM; j++)
                            wbufi1[i][j] = (int)j;

                        /* Same for all cases */
                        mem_tids[i]   = H5T_NATIVE_INT;
                        r_mem_tids[i] = H5T_NATIVE_INT;

                        break;

                    case DSET_WITH_CONV_AND_NO_BKG:

                        if (s == SETTING_A) {
                            /* Initialize buffer indices */
                            ul_wbufi2[i] = ul_total_wbuf2 + (i * DSET_SELECT_DIM);
                            l_rbufi2[i]  = l_total_rbuf2 + (i * DSET_SELECT_DIM);

                            wbufs[i] = ul_wbufi2[i];
                            rbufs[i] = l_rbufi2[i];

                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                ul_wbufi2[i][j] = ULONG_MAX - (unsigned long)j;

                            mem_tids[i]   = H5T_NATIVE_ULONG;
                            r_mem_tids[i] = H5T_NATIVE_LONG;
                        }
                        else if (s == SETTING_B) {
                            /* Initialize buffer indices */
                            l_wbufi2[i] = l_total_wbuf2 + (i * DSET_SELECT_DIM);
                            s_rbufi2[i] = s_total_rbuf2 + (i * DSET_SELECT_DIM);

                            wbufs[i] = l_wbufi2[i];
                            rbufs[i] = s_rbufi2[i];

                            /* Initialize the buffer data */
                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                l_wbufi2[i][j] = LONG_MAX - (long)j;

                            mem_tids[i]   = H5T_NATIVE_LONG;
                            r_mem_tids[i] = H5T_NATIVE_SHORT;
                        }

                        break;

                    case DSET_WITH_CONV_AND_BKG:

                        if (s == SETTING_A) {
                            /* Initialize buffer indices */
                            s1_wbufi3[i] = s1_total_wbuf3 + (i * DSET_SELECT_DIM);
                            s3_rbufi3[i] = s3_total_rbuf3 + (i * DSET_SELECT_DIM);

                            wbufs[i] = s1_wbufi3[i];
                            rbufs[i] = s3_rbufi3[i];

                            /* Initialize buffer data for s1_t */
                            for (j = 0; j < DSET_SELECT_DIM; j++) {
                                s1_wbufi3[i][j].a = (4 * j);
                                s1_wbufi3[i][j].b = (4 * j) + 1;
                                s1_wbufi3[i][j].c = (4 * j) + 2;
                                s1_wbufi3[i][j].d = (4 * j) + 3;
                            }
                            mem_tids[i]   = s1_tid;
                            r_mem_tids[i] = s3_tid;
                        }
                        else if (s == SETTING_B) {
                            /* Initialize buffer indices */
                            s4_wbufi3[i] = s4_total_wbuf3 + (i * DSET_SELECT_DIM);
                            s1_rbufi3[i] = s1_total_rbuf3 + (i * DSET_SELECT_DIM);

                            wbufs[i] = s4_wbufi3[i];
                            rbufs[i] = s1_rbufi3[i];

                            /* Initialize buffer data for s4_t */
                            for (j = 0; j < DSET_SELECT_DIM; j++) {
                                s4_wbufi3[i][j].b = DSET_SELECT_DIM + (unsigned int)j;
                                s4_wbufi3[i][j].d = DSET_SELECT_DIM + (unsigned int)j;
                            }
                            mem_tids[i]   = s4_tid;
                            r_mem_tids[i] = s1_tid;
                        }

                        break;

                    case DSET_NTTYPES:
                    default:
                        TEST_ERROR;

                } /* end switch dset_types */

            } /* end for i ndsets */

            /* Copy wbufs if the library will be modifying them */
            if (mwbuf) {
                memcpy(total_wbuf1_bak, total_wbuf1, ndsets * DSET_SELECT_DIM * sizeof(int));
                memcpy(ul_total_wbuf2_bak, ul_total_wbuf2, ndsets * DSET_SELECT_DIM * sizeof(unsigned long));
                memcpy(l_total_wbuf2_bak, l_total_wbuf2, ndsets * DSET_SELECT_DIM * sizeof(long));
                memcpy(s1_total_wbuf3_bak, s1_total_wbuf3, ndsets * DSET_SELECT_DIM * sizeof(s1_t));
                memcpy(s4_total_wbuf3_bak, s4_total_wbuf3, ndsets * DSET_SELECT_DIM * sizeof(s4_t));
            }

            if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
                TEST_ERROR;

            /* Restore wbufs from backup if the library modified them */
            if (mwbuf) {
                memcpy(total_wbuf1, total_wbuf1_bak, ndsets * DSET_SELECT_DIM * sizeof(int));
                memcpy(ul_total_wbuf2, ul_total_wbuf2_bak, ndsets * DSET_SELECT_DIM * sizeof(unsigned long));
                memcpy(l_total_wbuf2, l_total_wbuf2_bak, ndsets * DSET_SELECT_DIM * sizeof(long));
                memcpy(s1_total_wbuf3, s1_total_wbuf3_bak, ndsets * DSET_SELECT_DIM * sizeof(s1_t));
                memcpy(s4_total_wbuf3, s4_total_wbuf3_bak, ndsets * DSET_SELECT_DIM * sizeof(s4_t));
            }

            if (H5Dread_multi(ndsets, dset_dids, r_mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
                TEST_ERROR;

            /* Verify result read */
            /* for i ndsets */
            for (i = 0; i < (int)ndsets; i++) {
                switch (dset_types[i]) {

                    case DSET_WITH_NO_CONV:
                        for (j = 0; j < DSET_SELECT_DIM; j++)
                            if (rbufi1[i][j] != wbufi1[i][j]) {
                                H5_FAILED();
                                printf("    Read different values than written.\n");
                                printf("    For dset %d at index %d\n", i, j);
                                TEST_ERROR;
                            }

                        break;

                    case DSET_WITH_CONV_AND_NO_BKG:
                        if (s == SETTING_A) {
                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                if (l_rbufi2[i][j] != LONG_MAX) {
                                    H5_FAILED();
                                    printf("    Read different values than written.\n");
                                    printf("    For dset %d at index %d\n", i, j);
                                    TEST_ERROR;
                                }
                        }
                        else if (s == SETTING_B) {
                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                if (s_rbufi2[i][j] != SHRT_MAX) {
                                    H5_FAILED();
                                    printf("    Read different values than written.\n");
                                    printf("    For dset %d at index %d\n", i, j);
                                }
                        }

                        break;

                    case DSET_WITH_CONV_AND_BKG:
                        if (s == SETTING_A) {
                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                if (s3_rbufi3[i][j].a != s1_wbufi3[i][j].a ||
                                    s3_rbufi3[i][j].b != s1_wbufi3[i][j].b ||
                                    s3_rbufi3[i][j].c != s1_wbufi3[i][j].c ||
                                    s3_rbufi3[i][j].d != s1_wbufi3[i][j].d) {
                                    H5_FAILED();
                                    printf("    Read different values than written.\n");
                                    printf("    For dset %d at index %d\n", i, j);
                                }
                        }
                        else if (s == SETTING_B) {
                            for (j = 0; j < DSET_SELECT_DIM; j++)
                                if (s1_rbufi3[i][j].a != s1_wbufi3[i][j].a ||
                                    s1_rbufi3[i][j].b != (DSET_SELECT_DIM + j) ||
                                    s1_rbufi3[i][j].c != s1_wbufi3[i][j].c ||
                                    s1_rbufi3[i][j].d != (DSET_SELECT_DIM + j)) {
                                    H5_FAILED();
                                    printf("    Read different values than written.\n");
                                    printf("    For dset %d at index %d\n", i, j);
                                }
                        }

                        break;

                    case DSET_NTTYPES:
                    default:
                        TEST_ERROR;

                } /* end switch dset_types */

            } /* end for i ndsets */

        } /* end for s settings */

        /* Closing */
        if (H5Pclose(dcpl) < 0)
            TEST_ERROR;
        if (H5Pclose(dxpl) < 0)
            TEST_ERROR;

        if (H5Tclose(s1_tid) < 0)
            TEST_ERROR;
        if (H5Tclose(s3_tid) < 0)
            TEST_ERROR;
        if (H5Tclose(s4_tid) < 0)
            TEST_ERROR;

        for (i = 0; i < (int)ndsets; i++) {
            if (H5Sclose(file_sids[i]) < 0)
                TEST_ERROR;
            if (H5Dclose(dset_dids[i]) < 0)
                TEST_ERROR;
            /* Don't delete the last set of datasets */
            if ((n + 1) != niter)
                if (H5Ldelete(fid, dset_names[i], H5P_DEFAULT) < 0)
                    TEST_ERROR;
        }

        /* Freeing */
        free(total_wbuf1);
        total_wbuf1 = NULL;
        free(total_wbuf1_bak);
        total_wbuf1_bak = NULL;
        free(total_rbuf1);
        total_rbuf1 = NULL;

        free(ul_total_wbuf2);
        ul_total_wbuf2 = NULL;
        free(ul_total_wbuf2_bak);
        ul_total_wbuf2_bak = NULL;
        free(l_total_rbuf2);
        l_total_rbuf2 = NULL;
        free(l_total_wbuf2);
        l_total_wbuf2 = NULL;
        free(l_total_wbuf2_bak);
        l_total_wbuf2_bak = NULL;
        free(s_total_rbuf2);
        s_total_rbuf2 = NULL;

        free(s1_total_wbuf3);
        s1_total_wbuf3 = NULL;
        free(s1_total_wbuf3_bak);
        s1_total_wbuf3_bak = NULL;
        free(s3_total_rbuf3);
        s3_total_rbuf3 = NULL;
        free(s4_total_wbuf3);
        s4_total_wbuf3 = NULL;
        free(s4_total_wbuf3_bak);
        s4_total_wbuf3_bak = NULL;
        free(s1_total_rbuf3);
        s1_total_rbuf3 = NULL;

    } /* end for n niter */

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    H5Pclose(dcpl);
    H5Pclose(dxpl);
    H5Tclose(s1_tid);
    H5Tclose(s3_tid);
    H5Tclose(s4_tid);
    for (i = 0; i < (int)ndsets; i++) {
        H5Sclose(file_sids[i]);
        H5Sclose(mem_sids[i]);
        H5Dclose(dset_dids[i]);
    }
    H5E_END_TRY

    if (total_wbuf1)
        free(total_wbuf1);
    if (total_wbuf1_bak)
        free(total_wbuf1_bak);
    if (total_rbuf1)
        free(total_rbuf1);

    if (ul_total_wbuf2)
        free(ul_total_wbuf2);
    if (ul_total_wbuf2_bak)
        free(ul_total_wbuf2_bak);
    if (l_total_rbuf2)
        free(l_total_rbuf2);
    if (l_total_wbuf2)
        free(l_total_wbuf2);
    if (l_total_wbuf2_bak)
        free(l_total_wbuf2_bak);
    if (s_total_rbuf2)
        free(s_total_rbuf2);

    if (s1_total_wbuf3)
        free(s1_total_wbuf3);
    if (s1_total_wbuf3_bak)
        free(s1_total_wbuf3_bak);
    if (s3_total_rbuf3)
        free(s3_total_rbuf3);
    if (s4_total_wbuf3)
        free(s4_total_wbuf3);
    if (s4_total_wbuf3_bak)
        free(s4_total_wbuf3_bak);
    if (s1_total_rbuf3)
        free(s1_total_rbuf3);

    return FAIL;

} /* test_multi_dsets_all() */

/*
 * Verify H5Pset/get_selection_io API works as expected
 */
static herr_t
test_set_get_select_io_mode(const char *filename, hid_t fapl)
{
    hid_t                   fid  = H5I_INVALID_HID;
    hid_t                   did  = H5I_INVALID_HID;
    hid_t                   sid  = H5I_INVALID_HID;
    hid_t                   dcpl = H5I_INVALID_HID;
    hid_t                   dxpl = H5I_INVALID_HID;
    hsize_t                 dims[1];
    hsize_t                 cdims[1];
    int                     i;
    long                    wbuf[DSET_SELECT_DIM];
    H5D_selection_io_mode_t selection_io_mode;

    printf("\n");
    TESTING("H5Pget/set_selection_io_mode()");

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* default case */
    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    /* Disable case */
    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_OFF) < 0)
        TEST_ERROR;

    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    if (selection_io_mode != H5D_SELECTION_IO_MODE_OFF)
        TEST_ERROR;

    /* Enable case */
    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        TEST_ERROR;

    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    if (selection_io_mode != H5D_SELECTION_IO_MODE_ON)
        TEST_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    cdims[0] = DSET_SELECT_CHUNK_DIM;
    if (H5Pset_chunk(dcpl, 1, cdims) < 0)
        TEST_ERROR;

    if ((did = H5Dcreate2(fid, "test_chk_dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        wbuf[i] = i;

    /* May change the selection io actually performed */
    if (H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    /* Should still be enabled */
    if (selection_io_mode != H5D_SELECTION_IO_MODE_ON)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* test_set_get_select_io_mode() */

/*
 * To test with various test_mode that no selelction I/O is performed
 *
 * Note: It's the responsibility of the tester to feed proper combination
 *       of test_mode as needed.
 */
static herr_t
test_no_selection_io_cause_mode(const char *filename, hid_t fapl, uint32_t test_mode)
{
    hid_t    dcpl = H5I_INVALID_HID;
    hid_t    dxpl = H5I_INVALID_HID;
    hid_t    fid  = H5I_INVALID_HID;
    hid_t    fcpl = H5I_INVALID_HID;
    hid_t    did  = H5I_INVALID_HID;
    hid_t    sid  = H5I_INVALID_HID;
    hsize_t  dims[1];
    hsize_t  cdims[1];
    bool     is_chunked                           = false;
    hid_t    tid                                  = H5T_NATIVE_INT;
    uint32_t no_selection_io_cause_write          = 0;
    uint32_t no_selection_io_cause_read           = 0;
    uint32_t no_selection_io_cause_write_expected = 0;
    uint32_t no_selection_io_cause_read_expected  = 0;
    int      wbuf[DSET_SELECT_DIM];
    int      rbuf[DSET_SELECT_DIM];
    int      i;

    /* Check for (currently) incompatible combinations */
    if (test_mode & TEST_PAGE_BUFFER) {
        const char *driver_name = h5_get_test_driver_name();

        /* The split and multi driver are not compatible with page buffering.  No message since the other
         * cases aren't skipped. */
        if (driver_name && (!strcmp(driver_name, "split") || !strcmp(driver_name, "multi")))
            return 0;
    }

    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Enable page buffering to trigger H5D_PAGE_BUFFER */
    if (test_mode & TEST_PAGE_BUFFER) {
        if (H5Pset_page_buffer_size(fapl, 4096, 0, 0) < 0)
            TEST_ERROR;
        if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_PAGE, 0, (hsize_t)1) < 0)
            TEST_ERROR;
    }
    else {
        /* Not page buffer test, reset to default */
        if (H5Pset_page_buffer_size(fapl, 0, 0, 0) < 0)
            TEST_ERROR;
        if (H5Pset_file_space_strategy(fcpl, H5F_FSPACE_STRATEGY_FSM_AGGR, 0, (hsize_t)1) < 0)
            TEST_ERROR;
    }

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    if (test_mode & TEST_CONTIGUOUS_SIEVE_BUFFER) {
        no_selection_io_cause_write_expected |= H5D_SEL_IO_CONTIGUOUS_SIEVE_BUFFER;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_CONTIGUOUS_SIEVE_BUFFER;
    }

    if (test_mode & TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET) {
        if (H5Pset_layout(dcpl, H5D_COMPACT) < 0)
            TEST_ERROR;
        no_selection_io_cause_write_expected |= H5D_SEL_IO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;
    }

    if (test_mode == TEST_DATASET_FILTER) {
        if (H5Pset_deflate(dcpl, 9) < 0)
            TEST_ERROR;
        is_chunked = true;
        no_selection_io_cause_write_expected |= H5D_SEL_IO_DATASET_FILTER;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_DATASET_FILTER;
    }

    if (test_mode == TEST_CHUNK_CACHE) {
        is_chunked = true;
        no_selection_io_cause_write_expected |= H5D_SEL_IO_CHUNK_CACHE;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_CHUNK_CACHE;
    }

    if (test_mode == TEST_DISABLE_BY_API) {
        if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_OFF) < 0)
            TEST_ERROR;
        no_selection_io_cause_write_expected |= H5D_SEL_IO_DISABLE_BY_API;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_DISABLE_BY_API;
    }

    if (test_mode & TEST_NO_VECTOR_OR_SELECTION_IO_CB) {
        no_selection_io_cause_write_expected |= H5D_SEL_IO_DEFAULT_OFF;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_DEFAULT_OFF;
    }

    /* Datatype conversion */
    if (test_mode & TEST_DATATYPE_CONVERSION) {
        if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
            TEST_ERROR;
        tid = H5T_NATIVE_UINT;

        /* If we're testing a too small tconv buffer, set the buffer to be too small */
        if (test_mode & TEST_TCONV_BUF_TOO_SMALL) {
            if (H5Pset_buffer(dxpl, sizeof(int), NULL, NULL) < 0)
                TEST_ERROR;

            /* If we're using in-place type conversion sel io will succeed and only switch to scalar at the
             * VFL */
            if (test_mode & TEST_IN_PLACE_TCONV) {
                if (H5Pset_modify_write_buf(dxpl, true) < 0)
                    TEST_ERROR;
                no_selection_io_cause_write_expected |= H5D_SEL_IO_NO_VECTOR_OR_SELECTION_IO_CB;
            }
            else
                no_selection_io_cause_write_expected |= H5D_SEL_IO_TCONV_BUF_TOO_SMALL;

            /* In-place type conversion for read doesn't require modify_write_buf */
            no_selection_io_cause_read_expected |= H5D_SEL_IO_NO_VECTOR_OR_SELECTION_IO_CB;
        }
        else {
            /* sel io will succeed and only switch to scalar at the VFL */
            no_selection_io_cause_write_expected |= H5D_SEL_IO_NO_VECTOR_OR_SELECTION_IO_CB;
            no_selection_io_cause_read_expected |= H5D_SEL_IO_NO_VECTOR_OR_SELECTION_IO_CB;
        }
    }

    if (test_mode & TEST_PAGE_BUFFER) {
        no_selection_io_cause_write_expected |= H5D_SEL_IO_PAGE_BUFFER;
        no_selection_io_cause_read_expected |= H5D_SEL_IO_PAGE_BUFFER;
    }

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    if (is_chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            TEST_ERROR;
    }

    if ((did = H5Dcreate2(fid, "no_selection_io_cause", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl,
                          H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        wbuf[i] = i;

    if (H5Dwrite(did, tid, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        TEST_ERROR;

    if (test_mode & TEST_CONTIGUOUS_SIEVE_BUFFER) {
        if (H5Dwrite(did, tid, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
            TEST_ERROR;
    }

    if (H5Pget_no_selection_io_cause(dxpl, &no_selection_io_cause_write) < 0)
        TEST_ERROR;

    /* Verify causes of no selection I/O for write are as expected */
    if (no_selection_io_cause_write != no_selection_io_cause_write_expected)
        TEST_ERROR;

    /* Flush to clear the sieve buf */
    if (test_mode & TEST_NO_VECTOR_OR_SELECTION_IO_CB || test_mode & TEST_DATATYPE_CONVERSION ||
        test_mode & TEST_PAGE_BUFFER) {

        if (H5Dflush(did) < 0)
            TEST_ERROR;
    }

    if (H5Dread(did, tid, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        TEST_ERROR;

    /* Verify causes of no selection I/O for write is as expected */
    if (H5Pget_no_selection_io_cause(dxpl, &no_selection_io_cause_read) < 0)
        TEST_ERROR;

    /* Verify causes of no selection I/O for read are as expected */
    if (no_selection_io_cause_read != no_selection_io_cause_read_expected)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fcpl);
        H5Pclose(dcpl);
        H5Pclose(dxpl);
        H5Dclose(did);
        H5Dclose(sid);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;
} /* test_no_selection_io_cause_mode() */

/*
 * Test for causes of not performing selection I/O
 */
static herr_t
test_get_no_selection_io_cause(const char *filename, hid_t fapl)
{

    hid_t                   dxpl = H5I_INVALID_HID;
    H5D_selection_io_mode_t selection_io_mode;
    int                     errs = 0;

    printf("\n");
    TESTING("H5Pget_no_selection_io_cause()");

    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    if (H5Pclose(dxpl) < 0)
        TEST_ERROR;

    /* The following tests are based on H5D_SELECTION_IO_MODE_DEFAULT as the
       default setting in the library; skip the tests if that is not true */
    if (selection_io_mode != H5D_SELECTION_IO_MODE_DEFAULT) {
        SKIPPED();
        return SUCCEED;
    }

    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_DISABLE_BY_API);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_NOT_CONTIGUOUS_OR_CHUNKED_DATASET);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_CONTIGUOUS_SIEVE_BUFFER);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_DATASET_FILTER);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_CHUNK_CACHE);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_NO_VECTOR_OR_SELECTION_IO_CB);
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_DATATYPE_CONVERSION);
    errs +=
        test_no_selection_io_cause_mode(filename, fapl, TEST_DATATYPE_CONVERSION | TEST_TCONV_BUF_TOO_SMALL);
    errs += test_no_selection_io_cause_mode(
        filename, fapl, TEST_DATATYPE_CONVERSION | TEST_TCONV_BUF_TOO_SMALL | TEST_IN_PLACE_TCONV);
#ifndef H5_HAVE_PARALLEL
    errs += test_no_selection_io_cause_mode(filename, fapl, TEST_PAGE_BUFFER);
#endif

error:
    if (errs) {
        printf(" FAILED\n");
        return FAIL;
    }
    else {
        PASSED();
        return SUCCEED;
    }
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test cases for selection I/O
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int      nerrors = 0;
    char     filename[FILENAME_BUF_SIZE];
    hid_t    fapl  = H5I_INVALID_HID;
    hid_t    fapl2 = H5I_INVALID_HID;
    hid_t    fid   = H5I_INVALID_HID;
    int      test_select_config;
    unsigned set_cache; /* Set chunk cache to 0 or not */
    unsigned chunked;   /* Set to chunked dataset or not */
    unsigned dtrans;    /* Set to using data transform or not */
    unsigned mwbuf;     /* With/without modifying write buffer */

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if ((fapl2 = H5Pcopy(fapl)) < 0)
        TEST_ERROR;

    for (set_cache = FALSE; set_cache <= TRUE; set_cache++) {

        /* Disable chunk caching on fapl2 */
        if (set_cache) {
            if (H5Pset_cache(fapl2, 0, (size_t)0, (size_t)0, 0.0) < 0)
                TEST_ERROR;
        }

        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl2)) < 0)
            TEST_ERROR;

        /* Test with contiguous or chunked dataset */
        for (chunked = false; chunked <= true; chunked++) {

            /* Data transforms only apply to integer or floating-point datasets */
            /* therefore, not all tests are run with data transform */
            for (dtrans = false; dtrans <= true; dtrans++) {

                /* Test with and without modify_write_buf turned on */
                for (mwbuf = false; mwbuf <= true; mwbuf++) {

                    /* Print configuration message */
                    printf("Testing for selection I/O ");

                    if (set_cache)
                        printf("with 0 chunk cache, ");
                    else
                        printf("with default chunk cache, ");

                    if (chunked)
                        printf("with chunked dataset, ");
                    else
                        printf("with contiguous dataset, ");

                    if (dtrans)
                        printf("data transform, ");
                    else
                        printf("without data transform, ");

                    if (mwbuf)
                        printf("and with modifying write buffers\n");
                    else
                        printf("and without modifying write buffers\n");

                    for (test_select_config = (int)TEST_NO_TYPE_CONV;
                         test_select_config < (int)TEST_SELECT_NTESTS; test_select_config++) {

                        switch (test_select_config) {
                            case TEST_NO_TYPE_CONV: /* case 1 */
                                TESTING_2("No type conversion (null case)");

                                nerrors +=
                                    (test_no_type_conv(fid, set_cache, chunked, dtrans, mwbuf) < 0 ? 1 : 0);

                                break;

                            case TEST_NO_SIZE_CHANGE_NO_BKG: /* case 2 */
                                TESTING_2("No size change, no background buffer");

                                /* Data transforms does not apply to the dataset datatype for this test */
                                if (dtrans)
                                    SKIPPED();
                                else
                                    nerrors +=
                                        (test_no_size_change_no_bkg(fid, set_cache, chunked, mwbuf) < 0 ? 1
                                                                                                        : 0);

                                break;

                            case TEST_LARGER_MEM_NO_BKG: /* case 3 */
                                TESTING_2("Larger memory type, no background buffer");

                                nerrors +=
                                    (test_larger_mem_type_no_bkg(fid, set_cache, chunked, dtrans, mwbuf) < 0
                                         ? 1
                                         : 0);

                                break;

                            case TEST_SMALLER_MEM_NO_BKG: /* case 4 */
                                TESTING_2("Smaller memory type, no background buffer");

                                nerrors +=
                                    (test_smaller_mem_type_no_bkg(fid, set_cache, chunked, dtrans, mwbuf) < 0
                                         ? 1
                                         : 0);

                                break;

                            case TEST_CMPD_WITH_BKG: /* case 5 */
                                TESTING_2("Compound types with background buffer");

                                /* Data transforms does not apply to the dataset datatype for this test */
                                if (dtrans)
                                    SKIPPED();
                                else
                                    nerrors += (test_cmpd_with_bkg(fid, chunked, mwbuf) < 0 ? 1 : 0);

                                break;

                            case TEST_MULTI_CONV_NO_BKG: /* case 6 */
                                TESTING_2("multi-datasets: type conv + no bkg buffer");

                                nerrors += test_multi_dsets_no_bkg(fid, set_cache, chunked, dtrans, mwbuf);

                                break;

                            case TEST_MULTI_CONV_BKG: /* case 7 */
                                TESTING_2("multi-datasets: type conv + bkg buffer");

                                /* Data transforms does not apply to the dataset datatype for this test */
                                if (dtrans)
                                    SKIPPED();
                                else
                                    nerrors += test_multi_dsets_cmpd_with_bkg(fid, chunked, mwbuf);

                                break;

                            case TEST_MULTI_CONV_SIZE_CHANGE: /* case 8 */
                                TESTING_2("multi-datasets: type conv + size change + no bkg buffer");

                                /* Data transforms does not apply to the dataset datatype for this test */
                                if (dtrans)
                                    SKIPPED();
                                else
                                    nerrors += test_multi_dsets_size_change_no_bkg(fid, chunked, mwbuf);

                                break;

                            case TEST_MULTI_ALL: /* case 9 */
                                TESTING_2("multi-datasets: no conv + conv without bkg + conv with bkg");

                                /* Data transforms does not apply to the dataset datatype for this test */
                                if (dtrans)
                                    SKIPPED();
                                else
                                    nerrors += test_multi_dsets_all(10, fid, chunked, mwbuf);

                                break;

                            case TEST_SELECT_NTESTS:
                            default:
                                TEST_ERROR;

                        } /* end switch */
                    }     /* end for test_select_config */

                } /* end mwbuf */

            } /* end dtrans */

        } /* end chunked */

        if (H5Fclose(fid) < 0)
            TEST_ERROR;

    } /* end set_cache */

    /* Use own file */
    nerrors += test_set_get_select_io_mode(filename, fapl);

    /* Use own file */
    nerrors += test_get_no_selection_io_cause(filename, fapl);

    if (nerrors)
        goto error;

    printf("\n===================================\n");
    printf("All selection I/O dataset tests passed.\n");
    printf("===================================\n");

    h5_cleanup(FILENAME, fapl);

    H5Pclose(fapl2);

    exit(EXIT_SUCCESS);

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(fid);
    }
    H5E_END_TRY

    nerrors = MAX(1, nerrors);
    printf("***** %d SELECTION I/O DATASET TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    exit(EXIT_FAILURE);

} /* end main() */
