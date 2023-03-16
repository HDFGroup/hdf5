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
 * Programmer:
 *
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
    TEST_SELECT_NTESTS
} test_select_config_t;

#define DSET_SELECT_DIM       100
#define DSET_SELECT_CHUNK_DIM 10

#define DSET_CONTIG_NO_CONV_NTRANS "contig_no_conv_no_trans"
#define DSET_CONTIG_NO_CONV_TRANS  "contig_no_conv_trans"
#define DSET_CHK_NO_CONV_NTRANS    "chunked_no_conv_no_trans"
#define DSET_CHK_NO_CONV_TRANS     "chunked_no_conv_trans"

#define DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG  "contig_no_size_change_no_bkg"
#define DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG "chunked_no_size_change_no_bkg"

#define DSET_CONTIG_LARGER_NO_BKG_NTRANS "contig_larger_mem_no_bkg_no_trans"
#define DSET_CONTIG_LARGER_NO_BKG_TRANS  "contig_larger_mem_no_bkg_trans"
#define DSET_CHK_LARGER_NO_BKG_NTRANS    "chunked_larger_mem_no_bkg_no_trans"
#define DSET_CHK_LARGER_NO_BKG_TRANS     "chunked_larger_mem_no_bkg_trans"

#define DSET_CONTIG_SMALLER_NO_BKG_NTRANS "contig_smaller_mem_no_bkg_no_trans"
#define DSET_CONTIG_SMALLER_NO_BKG_TRANS  "contig_smaller_mem_no_bkg_trans"
#define DSET_CHK_SMALLER_NO_BKG_NTRANS    "chk_smaller_mem_no_bkg_no_trans"
#define DSET_CHK_SMALLER_NO_BKG_TRANS     "chk_smaller_mem_no_bkg_trans"

#define DSET_CONTIG_CMPD_WITH_BKG  "contig_cmpd_with_bkg"
#define DSET_CHUNKED_CMPD_WITH_BKG "chunked_cmpd_with_bkg"

#define MULTI_NUM_DSETS 3
#define MULTI_MIN_DSETS 3
#define DSET_NAME_LEN   30

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

/*
 *  Case 1: single dataset read/write, no type conversion (null case)
 *  --create dataset with H5T_NATIVE_INT
 *  --write/read dataset with H5T_NATIVE_INT
 */
static herr_t
test_no_type_conv(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int         i;
    hid_t       did  = H5I_INVALID_HID;
    hid_t       sid  = H5I_INVALID_HID;
    hid_t       dcpl = H5I_INVALID_HID;
    hid_t       dxpl = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    int         wbuf[DSET_SELECT_DIM];
    int         trans_wbuf[DSET_SELECT_DIM];
    int         rbuf[DSET_SELECT_DIM];
    const char *expr = "2*x";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;

        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_NO_CONV_TRANS : DSET_CHK_NO_CONV_NTRANS, H5T_NATIVE_INT,
                              sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_NO_CONV_TRANS : DSET_CONTIG_NO_CONV_NTRANS,
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = i;
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read data from the dataset without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read the data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            FAIL_STACK_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (2 * trans_wbuf[i])) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        FAIL_STACK_ERROR;

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
    H5E_END_TRY;

    return FAIL;

} /* test_no_type_conv() */

/*
 *  Case 2: single dataset read/write, no size change, no background buffer
 *  --create dataset with H5T_STD_I32BE
 *  --write/read dataset with H5T_STD_I32LE
 *  --read again with H5T_STD_I32BE
 */
static herr_t
test_no_size_change_no_bkg(hid_t fid, unsigned chunked)
{
    int     i;
    hid_t   did  = H5I_INVALID_HID;
    hid_t   sid  = H5I_INVALID_HID;
    hid_t   dcpl = H5I_INVALID_HID;
    hid_t   dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    char   *wbuf = NULL;
    char   *rbuf = NULL;

    if ((wbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        FAIL_STACK_ERROR;
    if ((rbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        FAIL_STACK_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
    }

    /* Create 1d dataset */
    if ((did =
             H5Dcreate2(fid, chunked ? DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG : DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG,
                        H5T_STD_I32BE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i * 4 + 3] = 0x1;
        wbuf[i * 4 + 2] = 0x2;
        wbuf[i * 4 + 1] = 0x3;
        wbuf[i * 4 + 0] = 0x4;
    }

    /* Write the data to the dataset with little endian */
    if (H5Dwrite(did, H5T_STD_I32LE, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read the data from the dataset with little endian */
    if (H5Dread(did, H5T_STD_I32LE, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data read little endian */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[4 * i + 0] != wbuf[4 * i + 0] || rbuf[4 * i + 1] != wbuf[4 * i + 1] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 2] || rbuf[4 * i + 3] != wbuf[4 * i + 3]) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    /* Read the data from the dataset with big endian */
    if (H5Dread(did, H5T_STD_I32BE, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data read in big endian */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[4 * i + 0] != wbuf[4 * i + 3] || rbuf[4 * i + 1] != wbuf[4 * i + 2] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 1] || rbuf[4 * i + 3] != wbuf[4 * i + 0]) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    HDfree(wbuf);
    HDfree(rbuf);

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
    H5E_END_TRY;

    if (wbuf)
        HDfree(wbuf);
    if (wbuf)
        HDfree(rbuf);

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
test_larger_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int         i;
    hid_t       did  = H5I_INVALID_HID;
    hid_t       sid  = H5I_INVALID_HID;
    hid_t       dcpl = H5I_INVALID_HID;
    hid_t       dxpl = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    long        wbuf[DSET_SELECT_DIM];
    long        trans_wbuf[DSET_SELECT_DIM];
    long long   rbuf[DSET_SELECT_DIM];
    const char *expr = "5 * (10 - x)";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_LARGER_NO_BKG_TRANS : DSET_CHK_LARGER_NO_BKG_NTRANS,
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        /* Create 1d contiguous dataset with/without data transform */
        if ((did =
                 H5Dcreate2(fid, dtrans ? DSET_CONTIG_LARGER_NO_BKG_TRANS : DSET_CONTIG_LARGER_NO_BKG_NTRANS,
                            H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = i;
        trans_wbuf[i] = 5 * (10 - wbuf[i]);
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;

    /* Write data to the dataset with/without data transform set in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read the data from the dataset without data transform in dxpl */
    if (H5Dread(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (long long)(dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_LLONG, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            FAIL_STACK_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (long long)(5 * (10 - trans_wbuf[i]))) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        FAIL_STACK_ERROR;

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
    H5E_END_TRY;

    return FAIL;

} /* test_larger_mem_type_no_bkg() */

/*
 *  Case 4: single dataset reader/write, smaller mem type, no background buffer
 *  --create dataset with H5T_NATIVE_INT
 *  --write dataset with H5T_NATIVE_SHORT
 *  --read dataset with H5T_NATIVE_SHORT
 */
static herr_t
test_smaller_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int         i;
    hid_t       did  = H5I_INVALID_HID;
    hid_t       sid  = H5I_INVALID_HID;
    hid_t       dcpl = H5I_INVALID_HID;
    hid_t       dxpl = H5I_INVALID_HID;
    hid_t       ntrans_dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    short       wbuf[DSET_SELECT_DIM];
    short       trans_wbuf[DSET_SELECT_DIM];
    short       rbuf[DSET_SELECT_DIM];
    const char *expr = "2 * (10 + x)";

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;

        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_SMALLER_NO_BKG_TRANS : DSET_CHK_SMALLER_NO_BKG_NTRANS,
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }
    else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid,
                              dtrans ? DSET_CONTIG_SMALLER_NO_BKG_TRANS : DSET_CONTIG_SMALLER_NO_BKG_NTRANS,
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i]       = (short)i;
        trans_wbuf[i] = (short)(2 * (10 + wbuf[i]));
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans) {
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;
    }

    /* Write data to the dataset with/without data transform in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read data from the dataset without data transform in dxpl */
    if (H5Dread(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, ntrans_dxpl, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i] : wbuf[i])) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            FAIL_STACK_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] != (2 * (10 + trans_wbuf[i]))) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        FAIL_STACK_ERROR;

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
    H5E_END_TRY;

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
test_cmpd_with_bkg(hid_t fid, unsigned chunked)
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
    s1_t   *s1_wbuf = NULL;
    s1_t   *s1_rbuf = NULL;
    s2_t   *s2_wbuf = NULL;
    s2_t   *s2_rbuf = NULL;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    /* Allocate buffers for datasets */
    if (NULL == (s1_wbuf = (s1_t *)HDmalloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        FAIL_STACK_ERROR;
    if (NULL == (s1_rbuf = (s1_t *)HDmalloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        FAIL_STACK_ERROR;
    if (NULL == (s2_wbuf = (s2_t *)HDmalloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        FAIL_STACK_ERROR;
    if (NULL == (s2_rbuf = (s2_t *)HDmalloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        FAIL_STACK_ERROR;

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        FAIL_STACK_ERROR;

    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
    }

    /* Case 5(a) */

    /* Create 1d dataset */
    if ((did = H5Dcreate2(fid, chunked ? DSET_CHUNKED_CMPD_WITH_BKG : DSET_CONTIG_CMPD_WITH_BKG, s1_tid, sid,
                          H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_wbuf[i].a = 4 * i;
        s1_wbuf[i].b = (4 * i) + 1;
        s1_wbuf[i].c = (4 * i) + 2;
        s1_wbuf[i].d = (4 * i) + 3;
    }

    /* Write all the data to the dataset */
    if (H5Dwrite(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, s1_wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read all the data from the dataset */
    if (H5Dread(did, s1_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_wbuf[i].a != s1_rbuf[i].a || s1_wbuf[i].b != s1_rbuf[i].b || s1_wbuf[i].c != s1_rbuf[i].c ||
            s1_wbuf[i].d != s1_rbuf[i].d) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
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
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Write s1_wbuf to the dataset with only subset members in ss_tid */
    if (H5Dwrite(did, ss_ac_tid, H5S_ALL, H5S_ALL, dxpl, s1_wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read the whole compound back */
    if (H5Dread(did, ss_ac_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify the compound fields have the correct (old or new) values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != s1_wbuf[i].a || s1_rbuf[i].b != ((4 * i) + 1) || s1_rbuf[i].c != s1_wbuf[i].c ||
            s1_rbuf[i].d != ((4 * i) + 3)) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
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
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Read the dataset: will read only what is set in */
    if (H5Dread(did, ss_bc_tid, H5S_ALL, H5S_ALL, dxpl, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != ((4 * i) + (2 * DSET_SELECT_DIM)) || s1_rbuf[i].b != ((4 * i) + 1) ||
            s1_rbuf[i].c != ((4 * i) + DSET_SELECT_DIM + 2) ||
            s1_rbuf[i].d != ((4 * i) + (2 * DSET_SELECT_DIM) + 3)) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
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
        FAIL_STACK_ERROR;

    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_SHORT) < 0)
        FAIL_STACK_ERROR;

    /* Update s2_wbuf with unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s2_wbuf[i].a = 8 * i;
        s2_wbuf[i].b = (long)((8 * i) + 1);
        s2_wbuf[i].c = (8 * i) + 2;
        s2_wbuf[i].d = (short)((8 * i) + 3);
    }
    if (H5Dwrite(did, s2_tid, H5S_ALL, H5S_ALL, dxpl, s2_wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read it back */
    if (H5Dread(did, s2_tid, H5S_ALL, H5S_ALL, dxpl, s2_rbuf) < 0) {
        goto error;
    }

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s2_wbuf[i].a != s2_rbuf[i].a || s2_wbuf[i].b != s2_rbuf[i].b || s2_wbuf[i].c != s2_rbuf[i].c ||
            s2_wbuf[i].d != s2_rbuf[i].d) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(s1_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(s2_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(ss_ac_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(ss_bc_tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    /* Release buffers */
    HDfree(s1_wbuf);
    HDfree(s1_rbuf);
    HDfree(s2_wbuf);
    HDfree(s2_rbuf);

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
    H5E_END_TRY;

    if (s1_wbuf)
        HDfree(s1_wbuf);
    if (s1_rbuf)
        HDfree(s1_rbuf);
    if (s2_wbuf)
        HDfree(s2_wbuf);
    if (s2_rbuf)
        HDfree(s2_rbuf);
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
test_multi_dsets_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
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
    int   *total_trans_wbuf  = NULL;
    int   *total_rbuf        = NULL;
    long  *total_lwbuf       = NULL;
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
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;

    /* Set up file space ids, mem space ids, and dataset ids */
    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;

        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;

        /* Create ith dataset */
        if (chunked) {
            if (snprintf(dset_names[i], DSET_NAME_LEN,
                         dtrans ? "multi_chk_trans_dset%u" : "multi_chk_ntrans_dset%u", i) < 0)
                FAIL_STACK_ERROR;
        }
        else {
            if (snprintf(dset_names[i], DSET_NAME_LEN,
                         dtrans ? "multi_contig_trans_dset%u" : "multi_contig_ntrans_dset%u", i) < 0)
                FAIL_STACK_ERROR;
        }

        if ((dset_dids[i] =
                 H5Dcreate2(fid, dset_names[i], ((HDrandom() % 2) ? H5T_NATIVE_LONG : H5T_NATIVE_INT),
                            file_sids[i], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    buf_size = ndsets * DSET_SELECT_DIM * sizeof(int);

    /* Allocate buffers for all datasets */
    if (NULL == (total_wbuf = (int *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_trans_wbuf = (int *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_rbuf = (int *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;

    buf_size = ndsets * DSET_SELECT_DIM * sizeof(long);

    if (NULL == (total_lwbuf = (long *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_trans_lwbuf = (long *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_lrbuf = (long *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;

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

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Read data from the dataset (if dtrans, without data transform set in dxpl) */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, ntrans_dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (rbufi[i][j] != (dtrans ? trans_wbufi[i][j] : wbufi[i][j])) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (dtrans) {

        /* Read the data from the dataset with data transform set in dxpl */
        if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
            TEST_ERROR;

        /* Verify */
        for (i = 0; i < (int)ndsets; i++)
            for (j = 0; j < DSET_SELECT_DIM; j++)
                if (rbufi[i][j] != (2 * trans_wbufi[i][j])) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    For dset %d at index %d\n", i, j);
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
            lwbufi[i][j]       = (int)j + DSET_SELECT_DIM;
            trans_lwbufi[i][j] = 2 * lwbufi[i][j];
        }

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    /* Read data from the dataset (if dtrans, with data transform again in dxpl) */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            if (lrbufi[i][j] != (dtrans ? (2 * trans_lwbufi[i][j]) : lwbufi[i][j])) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }
        }
    }

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(ntrans_dxpl) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            FAIL_STACK_ERROR;
    }

    HDfree(total_wbuf);
    HDfree(total_rbuf);
    HDfree(total_trans_wbuf);
    HDfree(total_lwbuf);
    HDfree(total_lrbuf);
    HDfree(total_trans_lwbuf);

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
    H5E_END_TRY;

    if (total_wbuf)
        HDfree(total_wbuf);
    if (total_trans_wbuf)
        HDfree(total_trans_wbuf);
    if (total_rbuf)
        HDfree(total_rbuf);
    if (total_lwbuf)
        HDfree(total_lrbuf);
    if (total_lrbuf)
        HDfree(total_lrbuf);
    if (total_trans_lwbuf)
        HDfree(total_lrbuf);

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
test_multi_dsets_cmpd_with_bkg(hid_t fid, unsigned chunked)
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

    s1_t *total_wbuf = NULL;
    s1_t *total_rbuf = NULL;

    s2_t *s2_total_wbuf = NULL;
    s2_t *s2_total_rbuf = NULL;

    s1_t *wbufi[MULTI_NUM_DSETS];
    s1_t *rbufi[MULTI_NUM_DSETS];

    s2_t *s2_wbufi[MULTI_NUM_DSETS];
    s2_t *s2_rbufi[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];

    ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_selection_io(dxpl, H5D_SELECTION_IO_MODE_ON) < 0)
        FAIL_STACK_ERROR;

    dims[0] = DSET_SELECT_DIM;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
    }

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        FAIL_STACK_ERROR;

    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;
        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;

        if (snprintf(dset_names[i], DSET_NAME_LEN,
                     chunked ? "multi_chk_cmpd_dset%u" : "multi_contig_cmpd_dset%u", i) < 0)
            FAIL_STACK_ERROR;
        if ((dset_dids[i] =
                 H5Dcreate2(fid, dset_names[i], s1_tid, file_sids[i], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    buf_size    = ndsets * DSET_SELECT_DIM * sizeof(s1_t);
    s2_buf_size = ndsets * DSET_SELECT_DIM * sizeof(s2_t);

    /* Allocate buffers */
    if (NULL == (total_wbuf = (s1_t *)HDmalloc(buf_size)))
        TEST_ERROR;
    if (NULL == (total_rbuf = (s1_t *)HDmalloc(buf_size)))
        TEST_ERROR;

    if (NULL == (s2_total_wbuf = (s2_t *)HDmalloc(s2_buf_size)))
        TEST_ERROR;
    if (NULL == (s2_total_rbuf = (s2_t *)HDmalloc(s2_buf_size)))
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

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++) {
            if (wbufi[i][j].a != rbufi[i][j].a || wbufi[i][j].b != rbufi[i][j].b ||
                wbufi[i][j].c != rbufi[i][j].c || wbufi[i][j].d != rbufi[i][j].d) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
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
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

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

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (i == 0) { /* dset0 */
                if (wbufi[i][j].a != rbufi[i][j].a || ((4 * (int)j) + 1) != rbufi[i][j].b ||
                    wbufi[i][j].c != rbufi[i][j].c || ((4 * (int)j) + 3) != rbufi[i][j].d) {
                    H5_FAILED();
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
            }
            else { /* other datasets */
                for (j = 0; j < DSET_SELECT_DIM; j++)
                    if ((4 * (int)j) != rbufi[i][j].a || ((4 * (int)j) + 1) != rbufi[i][j].b ||
                        ((4 * (int)j) + 2) != rbufi[i][j].c || ((4 * (int)j) + 3) != rbufi[i][j].d) {
                        H5_FAILED();
                        HDprintf("    Read different values than written.\n");
                        HDprintf("    For dset %d at index %d\n", i, j);
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
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Reset memory and file space for <mm> dataset */
    if (H5Sselect_all(mem_sids[mm]) < 0)
        FAIL_STACK_ERROR;
    if (H5Sselect_all(file_sids[mm]) < 0)
        FAIL_STACK_ERROR;

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
            HDprintf("    Read different values than written.\n");
            HDprintf("    For dset0 at index %d\n", j);
            TEST_ERROR;
        }

    /* <mm> dset */
    for (j = 0; j < DSET_SELECT_DIM; j++)
        if (rbufi[mm][j].a != ((4 * (int)j) + (2 * DSET_SELECT_DIM)) ||
            rbufi[mm][j].b != ((4 * (int)j) + 1) || rbufi[mm][j].c != ((4 * (int)j) + 2) ||
            rbufi[mm][j].d != ((4 * (int)j) + (2 * DSET_SELECT_DIM) + 3)) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    For dset1 at index %d\n", j);
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
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
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
        FAIL_STACK_ERROR;

    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_SHORT) < 0)
        FAIL_STACK_ERROR;

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

    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, wbufs) < 0)
        TEST_ERROR;

    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, dxpl, rbufs) < 0)
        TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)ndsets; i++)
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (s2_wbufi[i][j].a != s2_rbufi[i][j].a || s2_wbufi[i][j].b != s2_rbufi[i][j].b ||
                s2_wbufi[i][j].c != s2_rbufi[i][j].c || s2_wbufi[i][j].d != s2_rbufi[i][j].d) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            FAIL_STACK_ERROR;
    }

    HDfree(total_wbuf);
    HDfree(total_rbuf);
    HDfree(s2_total_wbuf);
    HDfree(s2_total_rbuf);

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
    H5E_END_TRY;

    if (total_wbuf)
        HDfree(total_wbuf);
    if (total_rbuf)
        HDfree(total_rbuf);
    if (s2_total_wbuf)
        HDfree(s2_total_wbuf);
    if (s2_total_rbuf)
        HDfree(s2_total_rbuf);

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
test_multi_dsets_size_change_no_bkg(hid_t fid, unsigned chunked)
{
    size_t  ndsets;
    int     i, j;
    hid_t   dcpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];

    hid_t file_sids[MULTI_NUM_DSETS];
    hid_t mem_sids[MULTI_NUM_DSETS];
    hid_t mem_tids[MULTI_NUM_DSETS];

    char  dset_names[MULTI_NUM_DSETS][DSET_NAME_LEN];
    hid_t dset_dids[MULTI_NUM_DSETS];

    size_t   buf_size, ss;
    uint8_t *total_wbuf  = NULL;
    uint8_t *total_rbuf  = NULL;
    uint8_t *total_lwbuf = NULL;
    uint8_t *total_lrbuf = NULL;
    uint8_t *total_swbuf = NULL;
    uint8_t *total_srbuf = NULL;

    uint8_t *wbufi[MULTI_NUM_DSETS];
    uint8_t *rbufi[MULTI_NUM_DSETS];
    uint8_t *lwbufi[MULTI_NUM_DSETS];
    uint8_t *lrbufi[MULTI_NUM_DSETS];
    uint8_t *swbufi[MULTI_NUM_DSETS];
    uint8_t *srbufi[MULTI_NUM_DSETS];

    const void *wbufs[MULTI_NUM_DSETS];
    void       *rbufs[MULTI_NUM_DSETS];

    ndsets = MAX(MULTI_MIN_DSETS, MULTI_NUM_DSETS);

    dims[0] = DSET_SELECT_DIM;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            FAIL_STACK_ERROR;
    }

    /* Set up file space ids, mem space ids, and dataset ids */
    for (i = 0; i < (int)ndsets; i++) {
        if ((file_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;

        if ((mem_sids[i] = H5Screate_simple(1, dims, NULL)) < 0)
            FAIL_STACK_ERROR;

        /* Create ith dataset */
        if (snprintf(dset_names[i], DSET_NAME_LEN,
                     chunked ? "multi_chk_size_dset%u" : "multi_contig_size_dset%u", i) < 0)
            FAIL_STACK_ERROR;

        if (i == 0) {
            if ((dset_dids[i] = H5Dcreate2(fid, dset_names[i], H5T_STD_I32BE, file_sids[i], H5P_DEFAULT, dcpl,
                                           H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;
        }
        else {
            if ((dset_dids[i] =
                     H5Dcreate2(fid, dset_names[i], ((HDrandom() % 2) ? H5T_STD_I64LE : H5T_STD_I16LE),
                                file_sids[i], H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR;
        }
    }

    /* Case a */

    ss       = H5Tget_size(H5T_STD_I32BE);
    buf_size = ndsets * ss * DSET_SELECT_DIM;

    /* Allocate buffers for all datasets */
    if (NULL == (total_wbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_rbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;

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

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, wbufs) < 0)
        TEST_ERROR;

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, rbufs) < 0)
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
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
        }

    /* Case b */

    ss       = H5Tget_size(H5T_STD_I64BE);
    buf_size = ndsets * (ss * DSET_SELECT_DIM);

    /* Allocate buffers for all datasets */
    if (NULL == (total_lwbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_lrbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;

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

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, wbufs) < 0)
        TEST_ERROR;

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, rbufs) < 0)
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
                    HDprintf("    Read different values than written.\n");
                    HDprintf("    For dset %d at index %d\n", i, j);
                    TEST_ERROR;
                }
        }

    /* Case c */

    ss       = H5Tget_size(H5T_STD_I16BE);
    buf_size = ndsets * (ss * DSET_SELECT_DIM);

    /* Allocate buffers for all datasets */
    if (NULL == (total_swbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;
    if (NULL == (total_srbuf = (uint8_t *)HDmalloc(buf_size)))
        FAIL_STACK_ERROR;

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

    /* Write data to the dataset */
    if (H5Dwrite_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, wbufs) < 0)
        TEST_ERROR;

    /* Read data from the dataset */
    if (H5Dread_multi(ndsets, dset_dids, mem_tids, mem_sids, file_sids, H5P_DEFAULT, rbufs) < 0)
        TEST_ERROR;

    /* Verify */
    for (i = 0; i < (int)ndsets; i++)
        /* Can compare for all cases */
        for (j = 0; j < DSET_SELECT_DIM; j++)
            if (srbufi[i][(int)ss * j + 0] != swbufi[i][(int)ss * j + 0] ||
                srbufi[i][(int)ss * j + 1] != swbufi[i][(int)ss * j + 1]) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    For dset %d at index %d\n", i, j);
                TEST_ERROR;
            }

    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    for (i = 0; i < (int)ndsets; i++) {
        if (H5Sclose(file_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Sclose(mem_sids[i]) < 0)
            FAIL_STACK_ERROR;
        if (H5Dclose(dset_dids[i]) < 0)
            FAIL_STACK_ERROR;
    }

    HDfree(total_wbuf);
    HDfree(total_rbuf);
    HDfree(total_lwbuf);
    HDfree(total_lrbuf);
    HDfree(total_swbuf);
    HDfree(total_srbuf);

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    H5Pclose(dcpl);
    for (i = 0; i < (int)ndsets; i++) {
        H5Sclose(file_sids[i]);
        H5Sclose(mem_sids[i]);
        H5Dclose(dset_dids[i]);
    }
    H5E_END_TRY;

    if (total_wbuf)
        HDfree(total_wbuf);
    if (total_rbuf)
        HDfree(total_rbuf);
    if (total_lwbuf)
        HDfree(total_lwbuf);
    if (total_lrbuf)
        HDfree(total_lrbuf);
    if (total_swbuf)
        HDfree(total_swbuf);
    if (total_srbuf)
        HDfree(total_srbuf);

    return FAIL;

} /* test_multi_dsets_size_change_no_bkg() */

/*
 * Verify H5Pset/get_selection_io API works as expected
 */
static herr_t
test_set_get_select_io_mode(hid_t fid)
{
    hid_t       did  = H5I_INVALID_HID;
    hid_t       sid  = H5I_INVALID_HID;
    hid_t       dcpl = H5I_INVALID_HID;
    hid_t       dxpl = H5I_INVALID_HID;
    hsize_t     dims[1];
    hsize_t     cdims[1];
    int         i;
    long        wbuf[DSET_SELECT_DIM];
    H5D_selection_io_mode_t selection_io_mode;

    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* default case */
    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    if (selection_io_mode != H5D_SELECTION_IO_MODE_DEFAULT)
        TEST_ERROR;

    /* Disable case */
    if (H5Pset_selection_io(dxpl,H5D_SELECTION_IO_MODE_OFF) < 0)
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
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;

    cdims[0] = DSET_SELECT_CHUNK_DIM;
    if (H5Pset_chunk(dcpl, 1, cdims) < 0)
        FAIL_STACK_ERROR;

    if ((did = H5Dcreate2(fid, "test_chk_dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        wbuf[i]       = i;

    /* May change the selection io actually performed */
    if (H5Dwrite(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;

    if (H5Pget_selection_io(dxpl, &selection_io_mode) < 0)
        TEST_ERROR;

    /* Should still be enabled */
    if (selection_io_mode != H5D_SELECTION_IO_MODE_ON)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

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
    H5E_END_TRY;

    return FAIL;
} /* test_set_get_select_io_mode() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test cases for selection I/O
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int      nerrors = 0;
    char     filename[FILENAME_BUF_SIZE];
    hid_t    fapl = H5I_INVALID_HID;
    hid_t    fid  = H5I_INVALID_HID;
    int      test_select_config;
    unsigned chunked;
    unsigned dtrans;

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Test with contiguous or chunked dataset */
    for (chunked = FALSE; chunked <= TRUE; chunked++) {

        /* Data transforms only apply to integer or floating-point datasets */
        /* therefore, not all tests are run with data transform */
        for (dtrans = FALSE; dtrans <= TRUE; dtrans++) {

            if (chunked) {
                if (dtrans)
                    HDputs("\nTesting for selection I/O with chunked dataset and data transform\n");
                else
                    HDputs("\nTesting for selection I/O with chunked dataset and without data transform\n");
            }
            else {
                if (dtrans)
                    HDputs("\nTesting for selection I/O with contiguous dataset and data transform\n");
                else
                    HDputs(
                        "\nTesting for selection I/O with contiguous dataset and without data transform\n");
            }

            for (test_select_config = (int)TEST_NO_TYPE_CONV; test_select_config < (int)TEST_SELECT_NTESTS;
                 test_select_config++) {

                switch (test_select_config) {
                    case TEST_NO_TYPE_CONV: /* case 1 */
                        TESTING_2("No type conversion (null case)");

                        nerrors += (test_no_type_conv(fid, chunked, dtrans) < 0 ? 1 : 0);

                        break;

                    case TEST_NO_SIZE_CHANGE_NO_BKG: /* case 2 */
                        TESTING_2("No size change, no background buffer");

                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans)
                            SKIPPED();
                        else
                            nerrors += (test_no_size_change_no_bkg(fid, chunked) < 0 ? 1 : 0);

                        break;

                    case TEST_LARGER_MEM_NO_BKG: /* case 3 */
                        TESTING_2("Larger memory type, no background buffer");

                        nerrors += (test_larger_mem_type_no_bkg(fid, chunked, dtrans) < 0 ? 1 : 0);

                        break;

                    case TEST_SMALLER_MEM_NO_BKG: /* case 4 */
                        TESTING_2("Smaller memory type, no background buffer");

                        nerrors += (test_smaller_mem_type_no_bkg(fid, chunked, dtrans) < 0 ? 1 : 0);

                        break;

                    case TEST_CMPD_WITH_BKG: /* case 5 */
                        TESTING_2("Compound types with background buffer");

                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans)
                            SKIPPED();
                        else
                            nerrors += (test_cmpd_with_bkg(fid, chunked) < 0 ? 1 : 0);

                        break;

                    case TEST_MULTI_CONV_NO_BKG: /* case 6 */
                        TESTING_2("multi-datasets: type conversion+no bkg buffer");
                        nerrors += test_multi_dsets_no_bkg(fid, chunked, dtrans);
                        break;

                    case TEST_MULTI_CONV_BKG: /* case 7 */
                        TESTING_2("multi-datasets: type conversion+bkg buffer");

                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans)
                            SKIPPED();
                        else
                            nerrors += test_multi_dsets_cmpd_with_bkg(fid, chunked);

                        break;

                    case TEST_MULTI_CONV_SIZE_CHANGE: /* case 8 */
                        TESTING_2("multi-datasets: type conversion+size change+no bkg buffer");

                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans)
                            SKIPPED();
                        else
                            nerrors += test_multi_dsets_size_change_no_bkg(fid, chunked);

                        break;

                    case TEST_SELECT_NTESTS:
                    default:
                        TEST_ERROR;

                } /* end switch */
            }     /* end for test_select_config */

        } /* end dtrans */

    } /* end chunked */

    nerrors += test_set_get_select_io_mode(fid);

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (nerrors)
        goto error;

    HDprintf("All selection I/O dataset tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    HDexit(EXIT_SUCCESS);

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d SELECTION I/O DATASET TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    HDexit(EXIT_FAILURE);

} /* end main() */
