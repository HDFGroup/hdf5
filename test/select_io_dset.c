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

const char *FILENAME[] = {"select_io",             /* 0 */
                          NULL};

#define FILENAME_BUF_SIZE 1024


/*
 * Test configurations
 */
typedef enum {
    TEST_NO_TYPE_CONV,              /* no type conversion (null case) */
    TEST_NO_SIZE_CHANGE_NO_BKG,     /* no size change, no bkg buffer */
    TEST_LARGER_MEM_NO_BKG,         /* larger memory type, no bkg buffer */
    TEST_SMALLER_MEM_NO_BKG,        /* smaller memory type, no bkg buffer */
    TEST_CMPD_WITH_BKG,             /* compound types with bkg buffer */
    TEST_SELECT_NTESTS
} test_select_config_t;

#define DSET_SELECT_DIM         100
#define DSET_SELECT_CHUNK_DIM   10

#define DSET_CONTIG_NO_CONV_NTRANS "contig_no_conv_no_trans"
#define DSET_CONTIG_NO_CONV_TRANS  "contig_no_conv_trans"
#define DSET_CHK_NO_CONV_NTRANS    "chunked_no_conv_no_trans"
#define DSET_CHK_NO_CONV_TRANS     "chunked_no_conv_trans"

#define DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG    "contig_no_size_change_no_bkg"
#define DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG   "chunked_no_size_change_no_bkg"

#define DSET_CONTIG_LARGER_NO_BKG_NTRANS    "contig_larger_mem_no_bkg_no_trans"
#define DSET_CONTIG_LARGER_NO_BKG_TRANS     "contig_larger_mem_no_bkg_trans"
#define DSET_CHK_LARGER_NO_BKG_NTRANS       "chunked_larger_mem_no_bkg_no_trans"
#define DSET_CHK_LARGER_NO_BKG_TRANS        "chunked_larger_mem_no_bkg_trans"

#define DSET_CONTIG_SMALLER_NO_BKG_NTRANS   "contig_smaller_mem_no_bkg_no_trans"
#define DSET_CONTIG_SMALLER_NO_BKG_TRANS    "contig_smaller_mem_no_bkg_trans"
#define DSET_CHK_SMALLER_NO_BKG_NTRANS      "chk_smaller_mem_no_bkg_no_trans"
#define DSET_CHK_SMALLER_NO_BKG_TRANS       "chk_smaller_mem_no_bkg_trans"

#define DSET_CONTIG_CMPD_WITH_BKG    "contig_cmpd_with_bkg"
#define DSET_CHUNKED_CMPD_WITH_BKG   "chunked_cmpd_with_bkg"

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
    int a;
    long b;
    int c;
    short d;
} s2_t;



/*
 *  Case 1: single dataset read/write, no type conversion (null case)
 */
static herr_t
test_no_type_conv(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    int rbuf[DSET_SELECT_DIM];
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

        /* Create 1d chunked dataset with/without data tranform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_NO_CONV_TRANS : DSET_CHK_NO_CONV_NTRANS, H5T_NATIVE_INT, 
                              sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    } else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_NO_CONV_TRANS : DSET_CONTIG_NO_CONV_NTRANS, H5T_NATIVE_INT, 
                              sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i] = i;
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;
                
    /* Read data from the dataset without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i]:wbuf[i])) {
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

    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dxpl);
    }
    H5E_END_TRY;

    return FAIL;

} /* test_no_type_conv() */

/*
 *  Case 2: single dataset read/write, no size change, no background buffer
 */
static herr_t
test_no_size_change_no_bkg(hid_t fid, unsigned chunked)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    char *wbuf = NULL;
    char *rbuf = NULL;

    if ((wbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        FAIL_STACK_ERROR;
    if ((rbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
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
    if ((did = H5Dcreate2(fid, chunked?DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG:DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG, 
                H5T_STD_I32BE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i * 4 + 0] = 0x1;
        wbuf[i * 4 + 1] = 0x2;
        wbuf[i * 4 + 2] = 0x3;
        wbuf[i * 4 + 3] = 0x4;
    }

    /* Write the data to the dataset */
    if (H5Dwrite(did, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR;
                
    /* Read the data from the dataset */
    if (H5Dread(did, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (rbuf[4 * i + 0] != wbuf[4 * i + 3] || rbuf[4 * i + 1] != wbuf[4 * i + 2] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 1] || rbuf[4 * i + 3] != wbuf[4 * i + 0]) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
    }
    H5E_END_TRY;

    return FAIL;

} /* test_no_size_change_no_bkg() */


/*
 *  Case 3: single dataset read/write, larger mem type, no background buffer
 */
static herr_t
test_larger_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    long rbuf[DSET_SELECT_DIM];
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
        if ((did = H5Dcreate2(fid, dtrans ?DSET_CHK_LARGER_NO_BKG_TRANS : DSET_CHK_LARGER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    } else {
        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_LARGER_NO_BKG_TRANS : DSET_CONTIG_LARGER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i] = i;
        trans_wbuf[i] = 5 * (10 - wbuf[i]);
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;

    /* Write data to the dataset with/without data transform set in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;
                
    /* Read the data from the dataset with default dxpl */
    if (H5Dread(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (long)(dtrans ? trans_wbuf[i]:wbuf[i])) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_LONG, H5S_ALL, H5S_ALL, dxpl, rbuf) < 0)
            FAIL_STACK_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < DSET_SELECT_DIM; i++)
            if (rbuf[i] !=  (5 * (10 - trans_wbuf[i]))) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dxpl);
    }
    H5E_END_TRY;

    return FAIL;

} /* test_larger_mem_type_no_bkg() */

/*
 *  Case 4: single dataset reader/write, smaller mem type, no background buffer
 */
static herr_t
test_smaller_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    short rbuf[DSET_SELECT_DIM];
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
    } else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_SMALLER_NO_BKG_TRANS : DSET_CONTIG_SMALLER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        wbuf[i] = i;
        trans_wbuf[i] = 2 * (10 +  wbuf[i]);
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        FAIL_STACK_ERROR;

    /* Set data transform */
    if (dtrans) {
        if (H5Pset_data_transform(dxpl, expr) < 0)
            FAIL_STACK_ERROR;
    }


    /* Write data to the dataset with/without data transform in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, wbuf) < 0)
        FAIL_STACK_ERROR;
                
    /* Read data from the dataset with default dxpl */
    if (H5Dread(did, H5T_NATIVE_SHORT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verify data or transformed data read */
    for (i = 0; i < DSET_SELECT_DIM; i++)
        if (rbuf[i] != (short)(dtrans ? trans_wbuf[i]:wbuf[i])) {
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
            if (rbuf[i] != (2 * (10 +  trans_wbuf[i]))) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
            }
    }

    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if(H5Pclose(dxpl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Dclose(did);
        H5Pclose(dxpl);
    }
    H5E_END_TRY;

    return FAIL;

} /* test_smaller_mem_type_no_bkg() */



/*
 *  Case 5: single dataset reade/write, compound types with background buffer
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
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t s1_tid = H5I_INVALID_HID;
    hid_t s2_tid = H5I_INVALID_HID;
    hid_t ss_ac_tid = H5I_INVALID_HID;
    hid_t ss_bc_tid = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    s1_t *s1_wbuf = NULL;
    s1_t *s1_rbuf = NULL;
    s2_t *s2_wbuf = NULL;
    s2_t *s2_rbuf = NULL;

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
    if ((did = H5Dcreate2(fid, chunked?DSET_CHUNKED_CMPD_WITH_BKG:DSET_CONTIG_CMPD_WITH_BKG, 
                          s1_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize data */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_wbuf[i].a    = 4 * i;
        s1_wbuf[i].b    = (4 * i) + 1;
        s1_wbuf[i].c    = (4 * i) + 2;
        s1_wbuf[i].d    = (4 * i) + 3;
    }

    /* Write all the data to the dataset */
    if (H5Dwrite(did, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1_wbuf) < 0)
        FAIL_STACK_ERROR;
                
    /* Read all the data from the dataset */
    if (H5Dread(did, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verfy data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_wbuf[i].a != s1_rbuf[i].a || s1_wbuf[i].b != s1_rbuf[i].b ||
            s1_wbuf[i].c != s1_rbuf[i].c || s1_wbuf[i].d != s1_rbuf[i].d) {
            H5_FAILED();
            HDprintf("    Read different values than written.\n");
            HDprintf("    At index %d\n", i);
            TEST_ERROR;
        }
    }

    /* Case 5(b) */

    /* Update s1_wbuf with unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_wbuf[i].a    = (4 * i) + DSET_SELECT_DIM;
        s1_wbuf[i].b    = (4 * i) + DSET_SELECT_DIM + 1;
        s1_wbuf[i].c    = (4 * i) + DSET_SELECT_DIM + 2;
        s1_wbuf[i].d    = (4 * i) + DSET_SELECT_DIM + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_ac_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Write s1_wbuf to the dataset with only subset members in ss_tid */
    if (H5Dwrite(did, ss_ac_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1_wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read the whole compound back */
    if (H5Dread(did, ss_ac_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verfy the compound fields have the correct (old or new) values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != s1_wbuf[i].a || s1_rbuf[i].b != ((4 * i) + 1) ||
            s1_rbuf[i].c != s1_wbuf[i].c || s1_rbuf[i].d != ((4 * i) + 3) ) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
        }
    }

    /* Case 5(c) */

    /* Update s1_rbuf with new unique values */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        s1_rbuf[i].a    = (4 * i) + (2 * DSET_SELECT_DIM);
        s1_rbuf[i].b    = (4 * i) + (2 * DSET_SELECT_DIM) + 1;
        s1_rbuf[i].c    = (4 * i) + (2 * DSET_SELECT_DIM) + 2;
        s1_rbuf[i].d    = (4 * i) + (2 * DSET_SELECT_DIM) + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_bc_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        FAIL_STACK_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        FAIL_STACK_ERROR;

    /* Read the dataset: will read only what is set in */
    if (H5Dread(did, ss_bc_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1_rbuf) < 0)
        FAIL_STACK_ERROR;

    /* Verfy data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s1_rbuf[i].a != ((4 * i) + (2 * DSET_SELECT_DIM))|| 
            s1_rbuf[i].b != ((4 * i) + 1) ||
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
        s2_wbuf[i].a    = 8 * i;
        s2_wbuf[i].b    = (long)((8 * i) + 1);
        s2_wbuf[i].c    = (8 * i) + 2;
        s2_wbuf[i].d    = (short)((8 * i) + 3);
    }
    if (H5Dwrite(did, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2_wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Read it back */
    if (H5Dread(did, s2_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s2_rbuf) < 0) {
        goto error;
    }

    /* Verfy data read */
    for (i = 0; i < DSET_SELECT_DIM; i++) {
        if (s2_wbuf[i].a != s2_rbuf[i].a || s2_wbuf[i].b != s2_rbuf[i].b ||
            s2_wbuf[i].c != s2_rbuf[i].c || s2_wbuf[i].d != s2_rbuf[i].d) {
                H5_FAILED();
                HDprintf("    Read different values than written.\n");
                HDprintf("    At index %d\n", i);
                TEST_ERROR;
        }
    }

    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(s1_tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(s2_tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(ss_ac_tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Tclose(ss_bc_tid) < 0)
        FAIL_STACK_ERROR;
    if(H5Dclose(did) < 0)
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
    int         nerrors = 0;
    char filename[FILENAME_BUF_SIZE];
    hid_t fapl = H5I_INVALID_HID;
    hid_t fid = H5I_INVALID_HID;
    int test_select_config;
    unsigned chunked;
    unsigned dtrans;

    /* Activate selection I/O via environment variable */
    if (HDsetenv("HDF5_USE_SELECTION_IO", "true", TRUE) < 0)
        FAIL_STACK_ERROR;

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
            } else {
                if (dtrans)
                    HDputs("\nTesting for selection I/O with contiguous dataset and data transform\n");
                else
                    HDputs("\nTesting for selection I/O with contiguous dataset and without data transform\n");
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
                        if (dtrans) {
                            SKIPPED();
                            continue;
                        }

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
                        if (dtrans) {
                            SKIPPED();
                            continue;
                        }

                        nerrors += (test_cmpd_with_bkg(fid, chunked) < 0 ? 1 : 0);

                        break;

                    case TEST_SELECT_NTESTS:
                    default:
                        TEST_ERROR;

                } /* end switch */
            } /* end for test_select_config */
        } /* end dtrans */
    } /* end chunked */


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
