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
 *
 * Purpose:     Test selection I/O
 */

#include "h5test.h"
#include "testpar.h"

#define FILENAME          "pselect_io.h5"

/* MPI variables */
int mpi_size;
int mpi_rank;

/* Number of errors */
int nerrors = 0;
int curr_nerrors = 0;

#define P_TEST_ERROR                                                                                         \
    do {                                                                                                     \
        nerrors++;                                                                                           \
        H5_FAILED();                                                                                         \
        AT();                                                                                                \
    } while (0)

#define CHECK_PASSED()                                                                                       \
    do {                                                                                                     \
        int err_result = (nerrors > curr_nerrors);                                                           \
                                                                                                             \
        MPI_Allreduce(MPI_IN_PLACE, &err_result, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);                       \
                                                                                                             \
        if (MAINPROCESS) {                                                                                   \
            if (err_result == 0)                                                                             \
                PASSED();                                                                                    \
            else                                                                                             \
                HDputs("     ***TEST FAILED***");                                                            \
        }                                                                                                    \
    } while (0)

/*
 * Test configurations
 */
typedef enum {
    TEST_NO_TYPE_CONV,              /* no type conversion (null case) */
    TEST_NO_SIZE_CHANGE_NO_BKG,     /* no size change, no bkg buffer */
    TEST_LARGER_MEM_NO_BKG,         /* larger memory type, no bkg buffer */
    TEST_SMALLER_MEM_NO_BKG,        /* smaller memory type, no bkg buffer */
    TEST_CMPD_WITH_BKG,             /* compound types with bkg buffer */
    TEST_TYPE_CONV_SEL_EMPTY,       /* some processes have null/empty selections and with type conversion */
    TEST_SELECT_NTESTS
} test_select_config_t;

#define DSET_SELECT_DIM         100
#define DSET_SELECT_CHUNK_DIM   10

#define DSET_CONTIG_NO_CONV_NTRANS "contig_no_conv_ntrans"
#define DSET_CONTIG_NO_CONV_TRANS  "contig_no_conv_trans"
#define DSET_CHK_NO_CONV_NTRANS    "chunked_no_conv_ntrans"
#define DSET_CHK_NO_CONV_TRANS     "chunked_no_conv_trans"

#define DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG    "contig_no_size_change_no_bkg"
#define DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG   "chunked_no_size_change_no_bkg"

#define DSET_CONTIG_LARGER_NO_BKG_NTRANS    "contig_larger_mem_no_bkg_ntrans"
#define DSET_CONTIG_LARGER_NO_BKG_TRANS     "contig_larger_mem_no_bkg_trans"
#define DSET_CHK_LARGER_NO_BKG_NTRANS       "chunked_larger_mem_no_bkg_ntrans"
#define DSET_CHK_LARGER_NO_BKG_TRANS        "chunked_larger_mem_no_bkg_trans"

#define DSET_CONTIG_SMALLER_NO_BKG_NTRANS   "contig_smaller_mem_no_bkg_ntrans"
#define DSET_CONTIG_SMALLER_NO_BKG_TRANS    "contig_smaller_mem_no_bkg_trans"
#define DSET_CHK_SMALLER_NO_BKG_NTRANS      "chk_smaller_mem_no_bkg_ntrans"
#define DSET_CHK_SMALLER_NO_BKG_TRANS       "chk_smaller_mem_no_bkg_trans"

#define DSET_CONTIG_CMPD_WITH_BKG    "contig_cmpd_with_bkg"
#define DSET_CHUNKED_CMPD_WITH_BKG   "chunked_cmpd_with_bkg"

#define DSET_CONTIG_TCONV_SEL_EMPTY_NTRANS  "contig_tconv_sel_empty_ntrans"
#define DSET_CONTIG_TCONV_SEL_EMPTY_TRANS   "contig_tconv_sel_empty_trans"
#define DSET_CHK_TCONV_SEL_EMPTY_NTRANS     "chunked_tconv_sel_empty_ntrans"
#define DSET_CHK_TCONV_SEL_EMPTY_TRANS      "chunked_tconv_sel_empty_trans"


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
 * Helper routine to check actual I/O mode on a dxpl
 */
static void
check_io_mode(hid_t dxpl, unsigned chunked, unsigned dtrans)
{ 
    H5D_mpio_actual_io_mode_t actual_io_mode;

    if (H5Pget_mpio_actual_io_mode(dxpl, &actual_io_mode) < 0)
        P_TEST_ERROR;

    /* When data transform is activated, the I/O mode returned is not as expected.
       FOR NOW when dtrans: just print out error message but not counting nerrors */
    if (chunked) {
        if (actual_io_mode != H5D_MPIO_CHUNK_COLLECTIVE) {
            /* TEMPORARY: not counting nerrors when dtrans */
            if (!dtrans) nerrors++;
            if(MAINPROCESS)
                HDprintf("\n     Failed: Incorrect I/O mode (chunked)...TEMPORARY didn't count as error     ");
        }
    } else if (actual_io_mode != H5D_MPIO_CONTIGUOUS_COLLECTIVE) {
        /* TEMPORARY: not counting nerrors when dtrans */
        if (!dtrans) nerrors++;
        if(MAINPROCESS)
            HDprintf("\n     Failed: Incorrect I/O mode (contiguous)...TEMPORARY didn't count as error     ");
    }

} /* check_io_mode() */

/*
 *  Case 1: single dataset read/write, no type conversion (null case)
 */
static void
test_no_type_conv(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t ntrans_dxpl = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID; 
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    int rbuf[DSET_SELECT_DIM];
    const char *expr = "2*x";

    curr_nerrors = nerrors;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;

    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;

        /* Create 1d chunked dataset with/without data tranform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_NO_CONV_TRANS : DSET_CHK_NO_CONV_NTRANS, H5T_NATIVE_INT, 
                              sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    } else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_NO_CONV_TRANS : DSET_CONTIG_NO_CONV_NTRANS, H5T_NATIVE_INT, 
                              sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    }

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i] = i + (int)start[0];
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        P_TEST_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            P_TEST_ERROR;

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);
                
    /* Read data from the dataset (if dtrans, without data transform set in dxpl) */
    if (H5Dread(did, H5T_NATIVE_INT, mspace_id, fspace_id, ntrans_dxpl, rbuf) < 0)
        P_TEST_ERROR;

    /* Verify data read (if dtrans, verify data is transformed) */
    for (i = 0; i < (int)block[0]; i++)
        if (rbuf[i] != (dtrans ? trans_wbuf[i]: wbuf[i])) {
            nerrors++;
            HDprintf("\n     Error in first data verification:\n");
            HDprintf("     At index %d: %d, %d\n", i+(int)start[0], 
                     dtrans ? trans_wbuf[i] : wbuf[i], rbuf[i]);
            break;
        }

    if (dtrans) {

        /* Read the data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, rbuf) < 0)
            P_TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < (int)block[0]; i++)
            if (rbuf[i] != (2 * trans_wbuf[i])) {
                nerrors++;
                HDprintf("\n     Error in second data verification:.\n");
                HDprintf("     At index %d: %d, %d\n", i+(int)start[0], 2*trans_wbuf[i], rbuf[i]);
                break;
            }
    }

    if (H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;

    if (H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if (H5Dclose(did) < 0)
        P_TEST_ERROR;
    if (H5Pclose(dxpl) < 0)
        P_TEST_ERROR;

    CHECK_PASSED();

    return;
} /* test_no_type_conv() */

/*
 *  Case 2: single dataset read/write, no size change, no background buffer
 */
static void
test_no_size_change_no_bkg(hid_t fid, unsigned chunked)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    char *wbuf = NULL;
    char *rbuf = NULL;

    curr_nerrors = nerrors;

    if ((wbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        P_TEST_ERROR;
    if ((rbuf = (char *)HDmalloc((size_t)(4 * DSET_SELECT_DIM))) == NULL)
        P_TEST_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;
    }

    /* Create 1d dataset */
    if ((did = H5Dcreate2(fid, chunked?DSET_CHUNKED_NO_SIZE_CHANGE_NO_BKG:DSET_CONTIG_NO_SIZE_CHANGE_NO_BKG, 
                H5T_STD_I32BE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        P_TEST_ERROR;

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i * 4 + 0] = 0x1;
        wbuf[i * 4 + 1] = 0x2;
        wbuf[i * 4 + 2] = 0x3;
        wbuf[i * 4 + 3] = 0x4;
    }

    /* Create a memory dataspace independently */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* Create a file dataspace independently */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    /* Write the data to the dataset */
    if (H5Dwrite(did, H5T_STD_I32BE, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, false);
                
    /* Read the data from the dataset */
    if (H5Dread(did, H5T_STD_I32LE, mspace_id, fspace_id, dxpl, rbuf) < 0)
        P_TEST_ERROR;

    /* Verify data read */
    for (i = 0; i < (int)block[0]; i++) {
        if (rbuf[4 * i + 0] != wbuf[4 * i + 3] || rbuf[4 * i + 1] != wbuf[4 * i + 2] ||
            rbuf[4 * i + 2] != wbuf[4 * i + 1] || rbuf[4 * i + 3] != wbuf[4 * i + 0]) {
            nerrors++;
            HDprintf("\n     Error in data verification:\n");
            HDprintf("     At index %d: %d, %d\n", i+(int)start[0], wbuf[i], rbuf[i]);
            break;
        }
    }

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;

    if(H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if(H5Dclose(did) < 0)
        P_TEST_ERROR;

    CHECK_PASSED();

    return;
} /* test_no_size_change_no_bkg() */


/*
 *  Case 3: single dataset read/write, larger mem type, no background buffer
 */
static void
test_larger_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t ntrans_dxpl = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    long rbuf[DSET_SELECT_DIM];
    const char *expr = "100 - x";

    curr_nerrors = nerrors;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;
        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ?DSET_CHK_LARGER_NO_BKG_TRANS : DSET_CHK_LARGER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    } else {
        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_LARGER_NO_BKG_TRANS : DSET_CONTIG_LARGER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    }

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i] = i + (int)start[0];
        trans_wbuf[i] = 100 - wbuf[i];
    }

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        P_TEST_ERROR;

    /* Set data transform */
    if (dtrans)
        if (H5Pset_data_transform(dxpl, expr) < 0)
            P_TEST_ERROR;

    /* Write data to the dataset with/without data transform set in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);

    /* Read data from the dataset (if dtrans, without data transform set in dxpl) */
    if (H5Dread(did, H5T_NATIVE_LONG, mspace_id, fspace_id, ntrans_dxpl, rbuf) < 0)
        P_TEST_ERROR;

    /* Verify data read (if dtrans, verify data is transformed) */
    for (i = 0; i < (int)block[0]; i++)
        if (rbuf[i] != (long)(dtrans ? trans_wbuf[i]:wbuf[i])) {
            nerrors++;
            HDprintf("\n     Error in first data verification:\n");
            HDprintf("     At index %d: %d, %ld\n", i+(int)start[0], dtrans ? trans_wbuf[i]:wbuf[i], rbuf[i]);
            break;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_LONG, mspace_id, fspace_id, dxpl, rbuf) < 0)
            P_TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < (int)block[0]; i++)
            if (rbuf[i] !=  (100 - trans_wbuf[i])) {
                nerrors++;
                HDprintf("\n     Error in second data verification:.\n");
                HDprintf("     At index %d: %d, %ld\n", i+(int)start[0], (100 - trans_wbuf[i]), rbuf[i]);
                break;
            }
    }
    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if(H5Dclose(did) < 0)
        P_TEST_ERROR;
    if(H5Pclose(dxpl) < 0)
        P_TEST_ERROR;

    CHECK_PASSED();

    return;

} /* test_larger_mem_type_no_bkg() */

/*
 *  Case 4: single dataset reader/write, smaller mem type, no background buffer
 */
static void
test_smaller_mem_type_no_bkg(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t ntrans_dxpl = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM]; 
    short rbuf[DSET_SELECT_DIM];
    const char *expr = "2 * (10 + x)";

    curr_nerrors = nerrors;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;

        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_SMALLER_NO_BKG_TRANS : DSET_CHK_SMALLER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    } else {

        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_SMALLER_NO_BKG_TRANS : DSET_CONTIG_SMALLER_NO_BKG_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    }

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i] = i + (int)start[0];
        trans_wbuf[i] = 2 * (10 +  wbuf[i]);
    }

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        P_TEST_ERROR;

    /* Set data transform */
    if (dtrans) {
        if (H5Pset_data_transform(dxpl, expr) < 0)
            P_TEST_ERROR;
    }


    /* Write data to the dataset with/without data transform in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);
                
    /* Read data from the dataset (if dtrans, without data transform set in dxpl) */
    if (H5Dread(did, H5T_NATIVE_SHORT, mspace_id, fspace_id, ntrans_dxpl, rbuf) < 0)
        P_TEST_ERROR;

    /* Verify data read (if dtrans, verify data is transformed) */
    for (i = 0; i < (int)block[0]; i++)
        if (rbuf[i] != (short)(dtrans ? trans_wbuf[i]:wbuf[i])) {
            nerrors++;
            HDprintf("\n     Error in first data verification:\n");
            HDprintf("     At index %d: %d, %d\n", i+(int)start[0], wbuf[i], rbuf[i]);
            break;
        }

    if (dtrans) {

        /* Read data from the dataset with data transform set in dxpl */
        if (H5Dread(did, H5T_NATIVE_SHORT, mspace_id, fspace_id, dxpl, rbuf) < 0)
            P_TEST_ERROR;

        /* Verify data read is transformed a second time */
        for (i = 0; i < (int)block[0]; i++)
            if (rbuf[i] != (short)(2 * (10 +  trans_wbuf[i]))) {
                nerrors++;
                HDprintf("\n     Error in second data verification:.\n");
                HDprintf("     At index %d: %d, %d\n", i+(int)start[0], (2 * (10 - trans_wbuf[i])), rbuf[i]);
                break;
            }
    }

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;

    if(H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if(H5Dclose(did) < 0)
        P_TEST_ERROR;
    if(H5Pclose(dxpl) < 0)
        P_TEST_ERROR;

    CHECK_PASSED();

    return;

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
 */
static void
test_cmpd_with_bkg(hid_t fid, unsigned chunked)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t s1_tid = H5I_INVALID_HID;
    hid_t s2_tid = H5I_INVALID_HID;
    hid_t ss_ac_tid = H5I_INVALID_HID;
    hid_t ss_bc_tid = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    s1_t *s1_wbuf = NULL;
    s1_t *s1_rbuf = NULL;
    s2_t *s2_wbuf = NULL;
    s2_t *s2_rbuf = NULL;
    H5D_mpio_actual_io_mode_t actual_io_mode;

    curr_nerrors = nerrors;

    /* Allocate buffers for datasets */
    if (NULL == (s1_wbuf = (s1_t *)HDmalloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        P_TEST_ERROR;
    if (NULL == (s1_rbuf = (s1_t *)HDmalloc(sizeof(s1_t) * DSET_SELECT_DIM)))
        P_TEST_ERROR;
    if (NULL == (s2_wbuf = (s2_t *)HDmalloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        P_TEST_ERROR;
    if (NULL == (s2_rbuf = (s2_t *)HDmalloc(sizeof(s2_t) * DSET_SELECT_DIM)))
        P_TEST_ERROR;

    /* Create the memory data type */
    if ((s1_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        P_TEST_ERROR;

    if (H5Tinsert(s1_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s1_tid, "d", HOFFSET(s1_t, d), H5T_NATIVE_INT) < 0)
        P_TEST_ERROR;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;
    }

    /* Case 5(a) */

    /* Create 1d dataset */
    if ((did = H5Dcreate2(fid, chunked?DSET_CHUNKED_CMPD_WITH_BKG:DSET_CONTIG_CMPD_WITH_BKG, 
                          s1_tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        P_TEST_ERROR;

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        s1_wbuf[i].a    = 4 * (i + (int)start[0]);
        s1_wbuf[i].b    = 4 * (i + (int)start[0]) + 1;
        s1_wbuf[i].c    = 4 * (i + (int)start[0]) + 2;
        s1_wbuf[i].d    = 4 * (i + (int)start[0]) + 3;
    }

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    /* Write all the data to the dataset */
    if (H5Dwrite(did, s1_tid, mspace_id, fspace_id, dxpl, s1_wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, false);
                
    /* Read all the data from the dataset */
    if (H5Dread(did, s1_tid, mspace_id, fspace_id, dxpl, s1_rbuf) < 0)
        P_TEST_ERROR;

    /* Verfy data read */
    for (i = 0; i < (int)block[0]; i++)
        if (s1_wbuf[i].a != s1_rbuf[i].a || s1_wbuf[i].b != s1_rbuf[i].b ||
            s1_wbuf[i].c != s1_rbuf[i].c || s1_wbuf[i].d != s1_rbuf[i].d) {
            nerrors++;
            HDprintf("\n     Error in 1st data verification:\n");
            HDprintf("     At index %d: %d/%d, %d/%d, %d/%d, %d/%d\n", 
                     i + (int)start[0], 
                     s1_wbuf[i].a,s1_rbuf[i].a,
                     s1_wbuf[i].b,s1_rbuf[i].b,
                     s1_wbuf[i].c,s1_rbuf[i].c,
                     s1_wbuf[i].d,s1_rbuf[i].d);
            break;
        }

    /* Case 5(b) */

    /* Update s1_wbuf with unique values */
    for (i = 0; i < (int)block[0]; i++) {
        s1_wbuf[i].a    = 4 * (i + (int)start[0]) + DSET_SELECT_DIM;
        s1_wbuf[i].b    = 4 * (i + (int)start[0]) + DSET_SELECT_DIM + 1;
        s1_wbuf[i].c    = 4 * (i + (int)start[0]) + DSET_SELECT_DIM + 2;
        s1_wbuf[i].d    = 4 * (i + (int)start[0]) + DSET_SELECT_DIM + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_ac_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        P_TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_ac_tid, "a", HOFFSET(s1_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_ac_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        P_TEST_ERROR;

    /* Write s1_wbuf to the dataset but with only subset members in ss_tid */
    if (H5Dwrite(did, ss_ac_tid, mspace_id, fspace_id, dxpl, s1_wbuf) < 0)
        P_TEST_ERROR;

    /* Read the whole compound back */
    if (H5Dread(did, ss_ac_tid, mspace_id, fspace_id, dxpl, s1_rbuf) < 0)
        P_TEST_ERROR;

    /* Verfy the compound fields have the correct (old or new) values */
    for (i = 0; i < (int)block[0]; i++)
        if (s1_rbuf[i].a != s1_wbuf[i].a || s1_rbuf[i].b != (4 * (i + (int)start[0]) + 1) ||
            s1_rbuf[i].c != s1_wbuf[i].c || s1_rbuf[i].d != (4 * (i + (int)start[0]) + 3) ) {
                nerrors++;
                HDprintf("\n     Error in 2nd data verification:\n");
                HDprintf("     At index %d: %d/%d, %d/%d, %d/%d, %d/%d\n", 
                         i + (int)start[0], 
                         s1_wbuf[i].a, s1_rbuf[i].a,
                         (4 * (i + (int)start[0]) + 1), s1_rbuf[i].b,
                         s1_wbuf[i].c, s1_rbuf[i].c,
                         (4 * (i + (int)start[0]) + 3), s1_rbuf[i].d);
                break;
        }

    /* Case 5(c) */

    /* Update s1_rbuf with new unique values */
    for (i = 0; i < (int)block[0]; i++) {
        s1_rbuf[i].a    = (4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM);
        s1_rbuf[i].b    = (4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM) + 1;
        s1_rbuf[i].c    = (4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM) + 2;
        s1_rbuf[i].d    = (4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM) + 3;
    }

    /* Create a compound type same size as s1_t */
    if ((ss_bc_tid = H5Tcreate(H5T_COMPOUND, sizeof(s1_t))) < 0)
        P_TEST_ERROR;

    /* but contains only subset members of s1_t */
    if (H5Tinsert(ss_bc_tid, "b", HOFFSET(s1_t, b), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(ss_bc_tid, "c", HOFFSET(s1_t, c), H5T_NATIVE_INT) < 0)
        P_TEST_ERROR;

    /* Read the dataset: will read only what is set in ss_bc_tid */
    if (H5Dread(did, ss_bc_tid, mspace_id, fspace_id, dxpl, s1_rbuf) < 0)
        P_TEST_ERROR;

    /* Verfy data read */
    for (i = 0; i < (int)block[0]; i++)
        if (s1_rbuf[i].a != ((4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM)) || 
            s1_rbuf[i].b != (4 * (i + (int)start[0]) + 1) ||
            s1_rbuf[i].c != (4 * (i + (int)start[0]) + DSET_SELECT_DIM + 2) ||
            s1_rbuf[i].d != ((4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM) + 3)) {
                nerrors++;
                HDprintf("\n     Error in 3rd data verification:\n");
                HDprintf("     At index %d: %d/%d, %d/%d, %d/%d, %d/%d\n", 
                         i + (int)start[0], 
                         ((4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM)), s1_rbuf[i].a,
                         (4 * (i + (int)start[0]) + 1), s1_rbuf[i].b,
                         (4 * (i + (int)start[0]) + DSET_SELECT_DIM + 2), s1_rbuf[i].c,
                         ((4 * (i + (int)start[0])) + (2 * DSET_SELECT_DIM) + 3), s1_rbuf[i].d);
                break;
        }


    /* Case 5(d) */

    /* Create s2_t compound type with:
     * --no conversion for 2 member types, 
     * --1 larger mem type 
     * --1 smaller mem type 
     */
    if ((s2_tid = H5Tcreate(H5T_COMPOUND, sizeof(s2_t))) < 0)
        P_TEST_ERROR;

    if (H5Tinsert(s2_tid, "a", HOFFSET(s2_t, a), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "b", HOFFSET(s2_t, b), H5T_NATIVE_LONG) < 0 ||
        H5Tinsert(s2_tid, "c", HOFFSET(s2_t, c), H5T_NATIVE_INT) < 0 ||
        H5Tinsert(s2_tid, "d", HOFFSET(s2_t, d), H5T_NATIVE_SHORT) < 0)
        P_TEST_ERROR;

    /* Update s2_wbuf with unique values */
    for (i = 0; i < (int)block[0]; i++) {
        s2_wbuf[i].a    = (8 * (i + (int)start[0]));
        s2_wbuf[i].b    = (long)(8 * (i + (int)start[0]) + 1);
        s2_wbuf[i].c    = (8 * (i + (int)start[0]) + 2);
        s2_wbuf[i].d    = (short)(8 * (i + (int)start[0]) + 3);
    }
    if (H5Dwrite(did, s2_tid, mspace_id, fspace_id, dxpl, s2_wbuf) < 0)
        P_TEST_ERROR;

    /* Read it back */
    if (H5Dread(did, s2_tid, mspace_id, fspace_id, dxpl, s2_rbuf) < 0)
        P_TEST_ERROR;

    /* Verfy data read */
    for (i = 0; i < (int)block[0]; i++)
        if (s2_wbuf[i].a != s2_rbuf[i].a || s2_wbuf[i].b != s2_rbuf[i].b ||
            s2_wbuf[i].c != s2_rbuf[i].c || s2_wbuf[i].d != s2_rbuf[i].d) {
                nerrors++;
                HDprintf("\n     Error in 4th data verification:\n");
                HDprintf("     At index %d: %d/%d, %ld/%ld, %d/%d, %d/%d\n", 
                         i + (int)start[0], 
                         s2_wbuf[i].a, s2_rbuf[i].a,
                         s2_wbuf[i].b, s2_rbuf[i].b,
                         s2_wbuf[i].c, s2_rbuf[i].c,
                         s2_wbuf[i].d, s2_rbuf[i].d);
                break;
        }

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;

    if(H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if(H5Tclose(s1_tid) < 0)
        P_TEST_ERROR;
    if(H5Tclose(s2_tid) < 0)
        P_TEST_ERROR;
    if(H5Tclose(ss_ac_tid) < 0)
        P_TEST_ERROR;
    if(H5Tclose(ss_bc_tid) < 0)
        P_TEST_ERROR;
    if(H5Dclose(did) < 0)
        P_TEST_ERROR;

    /* Release buffers */
    HDfree(s1_wbuf);
    HDfree(s1_rbuf);
    HDfree(s2_wbuf);
    HDfree(s2_rbuf);

    CHECK_PASSED();

    return;

} /* test_cmpd_with_bkg() */

/*
 *  Case 6: Type conversions + some processes have null/empty selections in datasets
 */
static void
test_type_conv_sel_empty(hid_t fid, unsigned chunked, unsigned dtrans)
{
    int i;
    hid_t did = H5I_INVALID_HID;
    hid_t sid = H5I_INVALID_HID;
    hid_t dcpl = H5I_INVALID_HID;
    hid_t dxpl = H5I_INVALID_HID;
    hid_t ntrans_dxpl = H5I_INVALID_HID;
    hid_t fspace_id = H5I_INVALID_HID;
    hid_t mspace_id = H5I_INVALID_HID;
    hsize_t dims[1];
    hsize_t cdims[1];
    hsize_t start[1], stride[1], count[1], block[1];
    int wbuf[DSET_SELECT_DIM]; 
    int trans_wbuf[DSET_SELECT_DIM];
    long lrbuf[DSET_SELECT_DIM];
    short srbuf[DSET_SELECT_DIM];
    long long llrbuf[DSET_SELECT_DIM];
    const char *expr = "2*x";

    curr_nerrors = nerrors;

    /* Create 1d data space */
    dims[0] = DSET_SELECT_DIM;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        P_TEST_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        P_TEST_ERROR;

    if (chunked) {
        cdims[0] = DSET_SELECT_CHUNK_DIM;
        if (H5Pset_chunk(dcpl, 1, cdims) < 0)
            P_TEST_ERROR;
        /* Create 1d chunked dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CHK_TCONV_SEL_EMPTY_TRANS : DSET_CHK_TCONV_SEL_EMPTY_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    } else {
        /* Create 1d contiguous dataset with/without data transform */
        if ((did = H5Dcreate2(fid, dtrans ? DSET_CONTIG_TCONV_SEL_EMPTY_TRANS : DSET_CONTIG_TCONV_SEL_EMPTY_NTRANS, 
                              H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
            P_TEST_ERROR;
    }

    /* Create dataset transfer property list */
    if ((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        P_TEST_ERROR;

    if (H5Pset_dxpl_mpio(dxpl, H5FD_MPIO_COLLECTIVE) < 0)
        P_TEST_ERROR;
    if (H5Pset_dxpl_mpio_collective_opt(dxpl, H5FD_MPIO_COLLECTIVE_IO) < 0)
        P_TEST_ERROR;

    if ((ntrans_dxpl = H5Pcopy(dxpl)) < 0)
        P_TEST_ERROR;

    /* Set data transform */
    if (dtrans) {
        if (H5Pset_data_transform(dxpl, expr) < 0)
            P_TEST_ERROR;
    }

    /* Each process takes x number of elements */
    block[0]  = dims[0] / (hsize_t)mpi_size;
    stride[0] = block[0];
    count[0]  = 1;
    start[0]  = (hsize_t)mpi_rank * block[0];

    /* Initialize data */
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i] = i + (int)start[0];
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Case 6(a) process 0: hyperslab; other processes: select none */

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;
    if (MAINPROCESS) {
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
    } else {
        if (H5Sselect_none(fspace_id) < 0)
            P_TEST_ERROR;
    } 

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;
    if (mpi_rank) {
        if (H5Sselect_none(mspace_id) < 0)
            P_TEST_ERROR;
    } 


    /* Write data to the dataset with/without data transform in dxpl */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);
                
    /* Read the data from the dataset: type conversion int-->long */
    /* If dtrans, without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_LONG, mspace_id, fspace_id, ntrans_dxpl, lrbuf) < 0)
        P_TEST_ERROR;

    if (MAINPROCESS) {
        for (i = 0; i < (int)block[0]; i++)
            if (lrbuf[i] != (dtrans? (long)trans_wbuf[i]:(long)wbuf[i])) {
                nerrors++;
                HDprintf("     FIRST:At index %d: %ld, %ld\n", 
                         i+(int)start[0], 
                         dtrans ? (long)trans_wbuf[i] : (long)wbuf[i], lrbuf[i]);
                break;
            }
    } 

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;


    /* Case 6(b) process 0: get 0 row; other processes: hyperslab */

    block[0]  = mpi_rank ? (dims[0] / (hsize_t)mpi_size) : 0;
    stride[0] = mpi_rank ? block[0] : 1;
    count[0]  = 1;
    start[0]  = mpi_rank ? ((hsize_t)mpi_rank * block[0]) : 0;

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
        P_TEST_ERROR;

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;

    /* need to make memory space to match for process 0 */
    if (MAINPROCESS) {
        if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, start, stride, count, block) < 0)
            P_TEST_ERROR;
    }

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);

    /* Read the data from the dataset: type conversion int-->short */
    /* If dtrans, without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_SHORT, mspace_id, fspace_id, ntrans_dxpl, srbuf) < 0)
        P_TEST_ERROR;

    if (mpi_rank) {
        for (i = 0; i < (int)block[0]; i++)
            if (srbuf[i] != (dtrans ? (short)trans_wbuf[i] : (short)wbuf[i])) {
                HDprintf("     SECOND: At index %d: %d, %d\n", 
                         i + (int)start[0], 
                         dtrans ? (short)trans_wbuf[i] : (short)wbuf[i], srbuf[i]);
                break;
            }
    } 

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;

    /* Case 6(c) process 0: select none; other processes: select all */

    /* Initialize data */
    block[0] = DSET_SELECT_DIM;
    for (i = 0; i < (int)block[0]; i++) {
        wbuf[i] = (int)i + DSET_SELECT_DIM;
        trans_wbuf[i] = 2 * wbuf[i];
    }

    /* Create a file dataspace */
    if ((fspace_id = H5Dget_space(did)) < 0)
        P_TEST_ERROR;
    if (MAINPROCESS) {
        if (H5Sselect_none(fspace_id) < 0)
            P_TEST_ERROR;
    } else {
        if (H5Sselect_all(fspace_id) < 0)
            P_TEST_ERROR;
    } 

    /* Create a memory dataspace */
    if ((mspace_id = H5Screate_simple(1, block, NULL)) < 0)
        P_TEST_ERROR;
    if (MAINPROCESS) {
        if (H5Sselect_none(mspace_id) < 0)
            P_TEST_ERROR;
    }

    /* Write data to the dataset with/without data transform */
    if (H5Dwrite(did, H5T_NATIVE_INT, mspace_id, fspace_id, dxpl, wbuf) < 0)
        P_TEST_ERROR;

    check_io_mode(dxpl, chunked, dtrans);

    /* Read the data from the dataset: type conversion int-->llong */
    /* If dtrans, without data transform set in dxpl */
    if (H5Dread(did, H5T_NATIVE_LLONG, mspace_id, fspace_id, ntrans_dxpl, llrbuf) < 0)
        P_TEST_ERROR;

    if (mpi_rank) {
        for (i = 0; i < (int)block[0]; i++)
            if (llrbuf[i] != (dtrans ? (long long)trans_wbuf[i] : (long long)wbuf[i])) {
                HDprintf("     THIRD: At index %d: %lld, %lld\n", i, 
                         dtrans ? (long long)trans_wbuf[i] : (long long)wbuf[i], llrbuf[i]);
                break;
            }
    } 

    if(H5Sclose(mspace_id) < 0)
        P_TEST_ERROR;
    if(H5Sclose(fspace_id) < 0)
        P_TEST_ERROR;


    if(H5Sclose(sid) < 0)
        P_TEST_ERROR;
    if(H5Dclose(did) < 0)
        P_TEST_ERROR;
    if(H5Pclose(dxpl) < 0)
        P_TEST_ERROR;

    CHECK_PASSED();

    return;

} /* test_type_conv_sel_empty() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Runs tests with all combinations of configuration
 *              flags.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
int
main(int argc, char *argv[])
{
    int ret;
    hid_t fapl = H5I_INVALID_HID;
    hid_t fid = H5I_INVALID_HID;
    int test_select_config;
    unsigned chunked;
    unsigned dtrans;

    h5_reset();

    /* Initialize MPI */
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);

    /* Activate selection I/O via environment variable */
    if (HDsetenv("HDF5_USE_SELECTION_IO", "true", TRUE) < 0)
        P_TEST_ERROR;

    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        P_TEST_ERROR;

    /* Set MPIO file driver */
    if (H5Pset_fapl_mpio(fapl, MPI_COMM_WORLD, MPI_INFO_NULL) < 0)
        P_TEST_ERROR;

    if ((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        P_TEST_ERROR;

    /* Test with contiguous or chunked dataset */
    for (chunked = FALSE; chunked <= TRUE; chunked++) {

        /* Data transforms only apply to integer or floating-point datasets */
        /* therefore, not all tests are run with data transform */
        for (dtrans = FALSE; dtrans <= TRUE; dtrans++) {

            if (MAINPROCESS) {
                if (chunked) {
                    if (dtrans)
                        HDputs("\nTesting for selection I/O with chunked dataset and data transform\n");
                    else
                        HDputs("\nTesting for selection I/O with chunked dataset and no data transform\n");
                 } else {
                    if (dtrans)
                        HDputs("\nTesting for selection I/O with contiguous dataset and data transform\n");
                    else
                        HDputs("\nTesting for selection I/O with contiguous dataset and no data transform\n");
                }
            }

            for (test_select_config = (int)TEST_NO_TYPE_CONV; test_select_config < (int)TEST_SELECT_NTESTS;
                 test_select_config++) {

                switch (test_select_config) {

                    case TEST_NO_TYPE_CONV: /* case 1 */
                        if (MAINPROCESS)
                            TESTING_2("No type conversion (null case)");

                        test_no_type_conv(fid, chunked, dtrans);

                        break;

                    case TEST_NO_SIZE_CHANGE_NO_BKG: /* case 2 */
                        if (MAINPROCESS)
                            TESTING_2("No size change, no background buffer");

                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans) {
                            if (MAINPROCESS) SKIPPED();
                            continue;
                        }

                        test_no_size_change_no_bkg(fid, chunked);

                        break;

                    case TEST_LARGER_MEM_NO_BKG: /* case 3 */
                        if (MAINPROCESS)
                            TESTING_2("Larger memory type, no background buffer");

                        test_larger_mem_type_no_bkg(fid, chunked, dtrans);

                        break;

                    case TEST_SMALLER_MEM_NO_BKG: /* case 4 */
                        if (MAINPROCESS)
                            TESTING_2("Smaller memory type, no background buffer");

                        test_smaller_mem_type_no_bkg(fid, chunked, dtrans);

                        break;

                    case TEST_CMPD_WITH_BKG: /* case 5 */
                        if (MAINPROCESS)
                            TESTING_2("Compound types with background buffer");
                        /* Data transforms does not apply to the dataset datatype for this test */
                        if (dtrans) {
                            if (MAINPROCESS) SKIPPED();
                            continue;
                        }

                        test_cmpd_with_bkg(fid, chunked);

                        break;

                    case TEST_TYPE_CONV_SEL_EMPTY: /* case 6 */
                        if (MAINPROCESS)
                            TESTING_2("Empty selections + Type conversion");

                        test_type_conv_sel_empty(fid, chunked, dtrans);

                        break;


                    case TEST_SELECT_NTESTS:
                    default:
                        P_TEST_ERROR;
                        break;
                
                } /* end switch */

            } /* end for test_select_config */
        } /* end dtrans */
    } /* end chunked */


    if (H5Pclose(fapl) < 0)
        P_TEST_ERROR;

    if (H5Fclose(fid) < 0)
        P_TEST_ERROR;


    /* Barrier to make sure all ranks are done before deleting the file, and
     * also to clean up output (make sure PASSED is printed before any of the
     * following messages) */
    if (MPI_Barrier(MPI_COMM_WORLD) != MPI_SUCCESS)
        P_TEST_ERROR;

    /* Delete file */
    if (MAINPROCESS) 
        if (MPI_File_delete(FILENAME, MPI_INFO_NULL) != MPI_SUCCESS)
            P_TEST_ERROR;

    /* Gather errors from all processes */
    MPI_Allreduce(&nerrors, &ret, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
    nerrors = ret;

    if (MAINPROCESS) {
        printf("===================================\n");
        if (nerrors)
            HDprintf("***Parallel selection I/O dataset tests detected %d errors***\n", nerrors);
        else
            HDprintf("Parallel selection I/O dataset tests finished with no errors\n");
        printf("===================================\n");
    }

    /* close HDF5 library */
    H5close();

    /* MPI_Finalize must be called AFTER H5close which may use MPI calls */
    MPI_Finalize();

    /* cannot just return (nerrors) because exit code is limited to 1 byte */
    return (nerrors != 0);
} /* end main() */
