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
 * Purpose:     This program is to generate HDF5 data files used to test
 *              version bounds.
 *
 * Description
 * ===========
 *              gen_bounds.c will generate the following files:
 *              - bounds_earliest_latest.h5
 *              - bounds_earliest_v18.h5
 *              - bounds_latest_latest.h5
 *              - bounds_v18_latest.h5
 *              - bounds_v18_v18.h5
 *              These files are copied to 1.6 and 1.8 libraries for verifying
 *              that they can or cannot read particular file format.
 */

/*
 * Add two routines gen_ref_files() and gen_sel_files() to generate the
 * following test files:
 *
 *  (1) gen_ref_files():
 *      bounds_ref_earliest_latest.h5
 *      bounds_ref_latest_latest.h5
 *      bounds_ref_v110_v110.h5
 *      bounds_ref_v18_v18.h5
 *  (1) gen_sel_files():
 *      bounds_sel_earliest_latest.h5
 *      bounds_sel_latest_latest.h5
 *      bounds_sel_v112_v112.h5
 *      bounds_sel_v110_v110.h5
 *
 * These test files will be copied to 1.12, 1.10 and 1.8 libraries for
 * compatibility testing.
 */
#include "h5test.h"

/*
 * Defines for gen_ref_files()
 */
/* File names used for references */
#define FILENAME_REF_E_L       "bounds_ref_earliest_latest.h5"
#define FILENAME_REF_L_L       "bounds_ref_latest_latest.h5"
#define FILENAME_REF_V112_V112 "bounds_ref_v112_v112.h5"
#define FILENAME_REF_V110_V110 "bounds_ref_v110_v110.h5"
#define FILENAME_REF_V18_V18   "bounds_ref_v18_v18.h5"

/* Dataset names for references */
#define REVISED_REFS_DSET "Revised_refs_dset"
#define OLD_REF_OBJ_DSET  "Old_ref_object_dset"
#define OLD_REF_REG_DSET  "Old_ref_region_dset"

#define GROUP   "Group"
#define ATTR    "Attr"
#define DATASET "Dataset"
#define POWER32 4294967296 /* 2^32 */

/*
 * Defines for gen_sel_files()
 */
/* File names for hyperslab/point selections */
#define FILENAME_SEL_E_L       "bounds_sel_earliest_latest.h5"
#define FILENAME_SEL_L_L       "bounds_sel_latest_latest.h5"
#define FILENAME_SEL_V112_V112 "bounds_sel_v112_v112.h5"
#define FILENAME_SEL_V110_V110 "bounds_sel_v110_v110.h5"

/* Dataset names for hyperslab/point selections */
#define SEL_EX_REG_DSET "Sel_ex32_reg_dset"
#define SEL_EX_IRR_DSET "Sel_ex32_irr_dset"
#define SEL_EX_PT_DSET  "Sel_ex32_pt_dset"

/***********************************************************************
 * gen_earliest_latest() creates file "bounds_earliest_latest.h5"
 *
 * File contents:
 * - Version 0 superblock (default)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4". (H5Pset_chunk_opts)
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/

/* File names for different file format */
#define FILENAME_E_L   "bounds_earliest_latest.h5"
#define FILENAME_E_18  "bounds_earliest_v18.h5"
#define FILENAME_L_L   "bounds_latest_latest.h5"
#define FILENAME_18_L  "bounds_v18_latest.h5"
#define FILENAME_18_18 "bounds_v18_v18.h5"

/* 2-D dataset with fixed dimensions */
#define RANK     2
#define DIM1     100
#define DIM2     200
#define CHK_DIM1 50
#define CHK_DIM2 50

static herr_t
gen_earliest_latest(void)
{
    hid_t   fid              = H5I_INVALID_HID;      /* File ID */
    hid_t   fapl             = H5I_INVALID_HID;      /* File access property list ID */
    hid_t   fcpl             = H5I_INVALID_HID;      /* File creation property list ID */
    hid_t   dcpl             = H5I_INVALID_HID;      /* Dataset creation property list ID */
    hid_t   space            = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   dset             = H5I_INVALID_HID;      /* Dataset ID */
    float  *buf              = NULL;                 /* Buffer for writing data */
    float  *bufp             = NULL;                 /* Pointer to data buffer */
    hsize_t dims[RANK]       = {DIM1, DIM2};         /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the earliest/latest version of the format" bounds
       for creating objects in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create file */
    if ((fid = H5Fcreate(FILENAME_E_L, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Close file property lists */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */
    buf = (float *)malloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL)
        TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if ((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;

    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Close property list and dataset, will reuse dataspace */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Disable partial chunk filters, triggers layout version 4 */
    if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Release allocated buffer */
    free(buf);
    bufp = buf = NULL;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        free(buf);
    }
    H5E_END_TRY
    return FAIL;
} /* gen_earliest_latest */

/***********************************************************************
 * gen_earliest_v18() creates file "bounds_earliest_v18.h5"
 *
 * File contents:
 * - Version 0 superblock (default)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_earliest_v18(void)
{
    hid_t   fid              = H5I_INVALID_HID;      /* File ID */
    hid_t   fapl             = H5I_INVALID_HID;      /* File access property list ID */
    hid_t   fcpl             = H5I_INVALID_HID;      /* File creation property list ID */
    hid_t   dcpl             = H5I_INVALID_HID;      /* Dataset creation property list ID */
    hid_t   space            = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   dset             = H5I_INVALID_HID;      /* Dataset ID */
    float  *buf              = NULL;                 /* Buffer for writing data */
    float  *bufp             = NULL;                 /* Pointer to data buffer */
    hsize_t dims[RANK]       = {DIM1, DIM2};         /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the earliest/v18 version of the format" bounds
       for creating objects in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18) < 0)
        TEST_ERROR;

    /* Create file */
    if ((fid = H5Fcreate(FILENAME_E_18, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Close file property lists */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)malloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL)
        TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if ((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;

    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Release allocated buffer */
    free(buf);
    bufp = buf = NULL;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        free(buf);
    }
    H5E_END_TRY
    return FAIL;
} /* gen_earliest_v18 */

/***********************************************************************
 * gen_latest_latest() creates file "bounds_latest_latest.h5"
 *
 * NOTE: As of March 2018, latest is 1.10.
 *
 * File contents:
 * - Version 3 superblock (NOTE: this can also be triggered by passing in
 *   H5F_ACC_SWMR_WRITE, in place of H5F_ACC_TRUNC, to H5Fcreate)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4".
 *   (triggered by H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS)
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_latest_latest(void)
{
    hid_t   fid              = H5I_INVALID_HID;      /* File ID */
    hid_t   fapl             = H5I_INVALID_HID;      /* File access property list ID */
    hid_t   dcpl             = H5I_INVALID_HID;      /* Dataset creation property list ID */
    hid_t   space            = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   dset             = H5I_INVALID_HID;      /* Dataset ID */
    float  *buf              = NULL;                 /* Buffer for writing data */
    float  *bufp             = NULL;                 /* Pointer to data buffer */
    hsize_t dims[RANK]       = {DIM1, DIM2};         /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the latest/latest version of the format" bounds
       for creating objects in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create the file with version 3 superblock */
    fid = H5Fcreate(FILENAME_L_L, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    if (fid < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    buf = (float *)malloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL)
        TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if ((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Disable partial chunk filters, triggers layout version 4 */
    if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Release allocated buffer */
    free(buf);
    bufp = buf = NULL;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Fclose(fid);
        free(buf);
    }
    H5E_END_TRY
    return FAIL;
} /* gen_latest_latest */

/***********************************************************************
 * gen_v18_latest() creates file "bounds_v18_latest.h5"
 *
 * NOTE: As of March 2018, latest is 1.10.
 *
 * File contents:
 * - Version 2 superblock
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_v18_latest(void)
{
    hid_t   fid              = H5I_INVALID_HID;      /* File ID */
    hid_t   fapl             = H5I_INVALID_HID;      /* File access property list ID */
    hid_t   fcpl             = H5I_INVALID_HID;      /* File creation property list ID */
    hid_t   dcpl             = H5I_INVALID_HID;      /* Dataset creation property list ID */
    hid_t   space            = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   dset             = H5I_INVALID_HID;      /* Dataset ID */
    float  *buf              = NULL;                 /* Buffer for writing data */
    float  *bufp             = NULL;                 /* Pointer to data buffer */
    hsize_t dims[RANK]       = {DIM1, DIM2};         /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the v18/latest version of the format" bounds
       for creating objects in the file, also trigger version 2 superblock */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create file */
    if ((fid = H5Fcreate(FILENAME_18_L, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Close file property lists */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)malloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL)
        TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if ((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Release allocated buffer */
    free(buf);
    bufp = buf = NULL;

    /* Close property list and dataset, will reuse dataspace */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        free(buf);
    }
    H5E_END_TRY
    return FAIL;
} /* gen_v18_latest */

/***********************************************************************
 * gen_v18_v18() creates file "bounds_v18_v18.h5"
 *
 * File contents:
 * - Version 2 superblock (H5Pset_libver_bounds(v18, v18)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4". (H5Pset_chunk_opts)
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_v18_v18(void)
{
    hid_t   fid              = H5I_INVALID_HID;      /* File ID */
    hid_t   fapl             = H5I_INVALID_HID;      /* File access property list ID */
    hid_t   fcpl             = H5I_INVALID_HID;      /* File creation property list ID */
    hid_t   dcpl             = H5I_INVALID_HID;      /* Dataset creation property list ID */
    hid_t   space            = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t   dset             = H5I_INVALID_HID;      /* Dataset ID */
    float  *buf              = NULL;                 /* Buffer for writing data */
    float  *bufp             = NULL;                 /* Pointer to data buffer */
    hsize_t dims[RANK]       = {DIM1, DIM2};         /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the v18 version of the format" bounds
       for creating objects in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18) < 0)
        TEST_ERROR;

    /* Create file */
    if ((fid = H5Fcreate(FILENAME_18_18, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
        TEST_ERROR;

    /* Close file property lists */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Pclose(fcpl) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)malloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL)
        TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if ((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Close property list and dataset, will reuse dataspace */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;

    /* Close the file, then reopen it with the latest version */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set the "use the v18/latest version of the format" bounds
       for creating a layout version 4 object in the file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen(FILENAME_18_18, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set up for chunked data */
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Disable partial chunk filters */
    if (H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0)
        TEST_ERROR;

    /* Release allocated buffer */
    free(buf);
    bufp = buf = NULL;

    /* Close everything */
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(space) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        free(buf);
    }
    H5E_END_TRY
    return FAIL;
} /* gen_v18_v18 */

/***********************************************************************
 * gen_sel_files() is used to create the following test files:
 *      bounds_sel_earliest_latest.h5
 *      bounds_sel_latest_latest.h5
 *      bounds_sel_v112_v112.h5
 *      bounds_sel_v110_v110.h5
 *
 * File contents for:
 * --bounds_sel_earliest_latest.h5
 * --bounds_sel_latest_latest.h5
 * --bounds_sel_v112_v112.h5
 *      --each file contains 3 datasets with old region reference type
 *          (1) Sel_ex32_reg_dset:
 *          --regular hyperslab selection exceeding 32 bits integer limit
 *          (2) Sel_ex32_irr_dset:
 *          --irregular hyperslab selection exceeding 32 bits integer limit
 *          (3) Sel_ex32_pt_dset:
 *          --point selection exceeding 32 bits integer limit
 *
 * File contents for:
 * --bounds_ref_v110_v110.h5
 *      (1) Sel_ex32_reg_dset: a dataset with old region reference type
 *          --regular hyperslab selection exceeding 32 bits integer limit
 *      (2) Sel_ex32_irr_dset: does not exist, cannot be created
 *      (3) Sel_ex32_pt_dset: does not exist, cannot be created
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_sel_files(const char *filename, H5F_libver_t low_bound, H5F_libver_t high_bound)
{
    hid_t           fid             = H5I_INVALID_HID; /* File ID */
    hid_t           fapl            = H5I_INVALID_HID; /* File access property list */
    hid_t           sid             = H5I_INVALID_HID; /* Dataspace ID */
    hid_t           did             = H5I_INVALID_HID; /* Dataset ID */
    hsize_t         numparticles    = 8388608;
    hsize_t         total_particles = numparticles * 513;
    hsize_t         vdsdims[1]      = {total_particles}; /* Dataset dimension size */
    hsize_t         coord[4];                            /* Point selection */
    hsize_t         ref_start;                           /* Starting location of hyperslab */
    hsize_t         ref_stride;                          /* Stride of hyperslab */
    hsize_t         ref_count;                           /* Element count of hyperslab */
    hsize_t         ref_block;                           /* Block size of hyperslab */
    hid_t           ref_sid     = H5I_INVALID_HID;       /* Dataspace ID for the reference dataset */
    hid_t           ref_did     = H5I_INVALID_HID;       /* Dataset ID for the reference dataset */
    hsize_t         ref_dims[1] = {1};                   /* Dimension for reference dataset */
    hdset_reg_ref_t ref_wbuf[1];                         /* Buffer for dataset region reference */

    /*
     * Create test file, attribute, group and dataset
     */

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a dataset */
    if ((sid = H5Screate_simple(1, vdsdims, NULL)) < 0)
        TEST_ERROR;

    if ((did = H5Dcreate2(fid, DATASET, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set to use the low/high bounds in fapl */
    if (H5Pset_libver_bounds(fapl, low_bound, high_bound) < 0)
        TEST_ERROR;

    /* Open the file with fapl */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Open the dataset */
    if ((did = H5Dopen2(fid, DATASET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get the dataset's dataspace  */
    if ((sid = H5Dget_space(did)) < 0)
        TEST_ERROR;

    /* Create dataspace for the reference dataset */
    ref_dims[0] = 1;
    if ((ref_sid = H5Screate_simple(1, ref_dims, NULL)) < 0)
        TEST_ERROR;

    /* Generate regular hyperslab exceeding 32 */
    ref_start  = 0;
    ref_count  = 2;
    ref_block  = 4;
    ref_stride = POWER32;

    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, &ref_start, &ref_stride, &ref_count, &ref_block) < 0)
        TEST_ERROR;

    /* Should succeed for v110 and above */
    if (high_bound >= H5F_LIBVER_V110) {

        /* Create the first reference */
        if (H5Rcreate(&ref_wbuf[0], fid, DATASET, H5R_DATASET_REGION, sid) < 0)
            TEST_ERROR;

        /* Create the reference dataset */
        if ((ref_did = H5Dcreate2(fid, SEL_EX_REG_DSET, H5T_STD_REF_DSETREG, ref_sid, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* Write to the reference dataset */
        if (H5Dwrite(ref_did, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_wbuf) < 0)
            TEST_ERROR;

        if (H5Dclose(ref_did) < 0)
            TEST_ERROR;
    }

    /* Generate irregular hyperslab exceeding 32 */
    ref_start  = 8;
    ref_count  = 5;
    ref_block  = 2;
    ref_stride = POWER32;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_OR, &ref_start, &ref_stride, &ref_count, &ref_block) < 0)
        TEST_ERROR;

    /* Should succeed for v112 and above */
    if (high_bound >= H5F_LIBVER_V112) {

        /* Create the second reference */
        if (H5Rcreate(&ref_wbuf[0], fid, DATASET, H5R_DATASET_REGION, sid) < 0)
            TEST_ERROR;

        /* Create the reference dataset */
        if ((ref_did = H5Dcreate2(fid, SEL_EX_IRR_DSET, H5T_STD_REF_DSETREG, ref_sid, H5P_DEFAULT,
                                  H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* Write to the reference dataset */
        if (H5Dwrite(ref_did, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_wbuf) < 0)
            TEST_ERROR;

        if (H5Dclose(ref_did) < 0)
            TEST_ERROR;
    }

    /* Generate point selection exceeding 32 */
    coord[0] = 5;
    coord[1] = 15;
    coord[2] = POWER32 + 1;
    coord[3] = 19;

    if (H5Sselect_elements(sid, H5S_SELECT_SET, (size_t)4, coord) < 0)
        TEST_ERROR;

    /* Should succeed for v112 and above */
    if (high_bound >= H5F_LIBVER_V112) {

        /* Create the third reference */
        if (H5Rcreate(&ref_wbuf[0], fid, DATASET, H5R_DATASET_REGION, sid) < 0)
            TEST_ERROR;

        /* Create the reference dataset */
        if ((ref_did = H5Dcreate2(fid, SEL_EX_PT_DSET, H5T_STD_REF_DSETREG, ref_sid, H5P_DEFAULT, H5P_DEFAULT,
                                  H5P_DEFAULT)) < 0)
            TEST_ERROR;

        /* Write to the reference dataset */
        if (H5Dwrite(ref_did, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, ref_wbuf) < 0)
            TEST_ERROR;

        if (H5Dclose(ref_did) < 0)
            TEST_ERROR;
    }

    /* Closing */
    if (H5Sclose(ref_sid) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;

    if (H5Sclose(sid) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(ref_did);
        H5Sclose(ref_sid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return FAIL;

} /* gen_sel_files() */

/***********************************************************************
 * gen_ref_files() is used to create the following test files:
 *      bounds_ref_earliest_latest.h5
 *      bounds_ref_latest_latest.h5
 *      bounds_ref_v110_v110.h5
 *      bounds_ref_v18_v18.h5
 *
 * File contents for:
 * --bounds_ref_earliest_latest.h5
 * --bounds_ref_latest_latest.h5
 * --bounds_ref_v112_v112.h5
 *      (1) Revised_refs_dset: a dataset created with the revised reference type
 *          --attribute reference
 *          --object reference
 *          --dataset region reference
 *      (2) Old_ref_object_dset:
 *          --a dataset created with the old object reference type
 *      (3) Old_ref_region_dset:
 *          --a dataset created with the old dataset region reference type
 *
 * File contents for:
 * --bounds_ref_v110_v110.h5
 * --bounds_ref_v18_v18.h5
 *      (1) Old_ref_object_dset:
 *          --a dataset created with the old object reference type
 *      (2) Old_ref_region_dset:
 *          --a dataset created with the old dataset region reference type
 *
 * Return: SUCCEED/FAIL
 *
 ***********************************************************************/
static herr_t
gen_ref_files(const char *filename, H5F_libver_t low_bound, H5F_libver_t high_bound)
{
    hid_t           fid             = H5I_INVALID_HID; /* File ID */
    hid_t           gid             = H5I_INVALID_HID; /* Group ID */
    hid_t           fapl            = H5I_INVALID_HID; /* File access property list */
    hid_t           aid             = H5I_INVALID_HID; /* Attribute ID */
    hid_t           asid            = H5I_INVALID_HID; /* Dataspace ID for attribute */
    hid_t           sid             = H5I_INVALID_HID; /* Dataspace ID */
    hid_t           did             = H5I_INVALID_HID; /* Dataset ID */
    hsize_t         dims[1]         = {100};           /* Dimension size */
    unsigned       *dwbuf           = NULL;            /* Buffer for writing data */
    hid_t           ref_sid         = H5I_INVALID_HID; /* Dataspace ID for the reference dataset */
    hid_t           ref_did         = H5I_INVALID_HID; /* Dataset ID for the reference dataset */
    hsize_t         rev_ref_dims[1] = {3};             /* Dimension size for the reference dataset */
    H5R_ref_t       rev_ref_wbuf[3];                   /* Buffer for storing the revised references */
    hobj_ref_t      old_ref_obj_wbuf[1];               /* Buffer for storing the old reference object */
    hdset_reg_ref_t old_ref_reg_wbuf[1];  /* Buffer for storing the old dataset region reference */
    hsize_t         old_ref_dims[] = {1}; /* Dimension size for the reference dataset */
    hsize_t         start[1];             /* Starting location of hyperslab */
    hsize_t         stride[1];            /* Stride of hyperslab */
    hsize_t         count[1];             /* Element count of hyperslab */
    hsize_t         block[1];             /* Block size of hyperslab */
    unsigned        i;                    /* Local index variable */

    /*
     * Create test file, attribute, group and dataset
     */

    if ((dwbuf = calloc(sizeof(unsigned), 100)) == NULL)
        TEST_ERROR;

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace for the attribute */
    if ((asid = H5Screate(H5S_SCALAR)) < 0)
        TEST_ERROR;

    /* Create an attribute to the root group */
    if ((aid = H5Acreate2(fid, ATTR, H5T_NATIVE_UINT, asid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a group */
    if ((gid = H5Gcreate2(fid, GROUP, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace for the dataset */
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create a dataset in the group */
    if ((did = H5Dcreate2(gid, DATASET, H5T_NATIVE_UINT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize data to write */
    for (i = 0; i < 100; i++)
        dwbuf[i] = i * 3;

    /* Write data to disk */
    if (H5Dwrite(did, H5T_NATIVE_UINT, H5S_ALL, H5S_ALL, H5P_DEFAULT, dwbuf) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Sclose(asid) < 0)
        TEST_ERROR;
    if (H5Aclose(aid) < 0)
        TEST_ERROR;
    if (H5Gclose(gid) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if (dwbuf) {
        free(dwbuf);
        dwbuf = NULL;
    }

    /* Create file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Set to use the low/high bounds in fapl */
    if (H5Pset_libver_bounds(fapl, low_bound, high_bound) < 0)
        TEST_ERROR;

    /* Open the file with fapl */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /*
     * Create the revised and old references in the file
     */

    /* Retrieve dataspace for the existing dataset */
    if ((did = H5Dopen2(fid, "/Group/Dataset", H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((sid = H5Dget_space(did)) < 0)
        TEST_ERROR;

    /* Select 15 2x1 hyperslabs for the dataset region reference */
    start[0]  = 2;
    stride[0] = 5;
    count[0]  = 15;
    block[0]  = 2;
    if (H5Sselect_hyperslab(sid, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    if (high_bound >= H5F_LIBVER_V112) {

        /* Create dataspace for the reference dataset */
        if ((ref_sid = H5Screate_simple(1, rev_ref_dims, NULL)) < 0)
            TEST_ERROR;

        /* Create a dataset with the revised reference type */
        ref_did =
            H5Dcreate2(fid, REVISED_REFS_DSET, H5T_STD_REF, ref_sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        /* Store the reference to "Attr" */
        if (H5Rcreate_attr(fid, "/", "Attr", H5P_DEFAULT, &rev_ref_wbuf[0]) < 0)
            TEST_ERROR;

        /* Store the reference to /Group */
        if (H5Rcreate_object(fid, "/Group", H5P_DEFAULT, &rev_ref_wbuf[1]) < 0)
            TEST_ERROR;

        /* Store the dataset region referenced to /Group/Dataset */
        if (H5Rcreate_region(fid, "/Group/Dataset", sid, H5P_DEFAULT, &rev_ref_wbuf[2]) < 0)
            TEST_ERROR;

        /* Write to the reference dataset */
        if (H5Dwrite(ref_did, H5T_STD_REF, H5S_ALL, H5S_ALL, H5P_DEFAULT, rev_ref_wbuf) < 0)
            TEST_ERROR;

        /* Destroy references */
        for (i = 0; i < 3; i++)
            if (H5Rdestroy(&rev_ref_wbuf[i]) < 0)
                TEST_ERROR;

        /* Closing */
        if (H5Dclose(ref_did) < 0)
            TEST_ERROR;

        if (H5Sclose(ref_sid) < 0)
            TEST_ERROR;
    }

    /* Create dataspace for the reference dataset */
    if ((ref_sid = H5Screate_simple(1, old_ref_dims, NULL)) < 0)
        TEST_ERROR;

    /* Create a dataset with the old object reference type */
    if ((ref_did = H5Dcreate2(fid, OLD_REF_OBJ_DSET, H5T_STD_REF_OBJ, ref_sid, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create reference to /Group */
    if (H5Rcreate(&old_ref_obj_wbuf[0], fid, "/Group", H5R_OBJECT, -1) < 0)
        TEST_ERROR;

    /* Write to the reference dataset */
    if (H5Dwrite(ref_did, H5T_STD_REF_OBJ, H5S_ALL, H5S_ALL, H5P_DEFAULT, old_ref_obj_wbuf) < 0)
        TEST_ERROR;

    /* Close the dataset */
    if (H5Dclose(ref_did) < 0)
        TEST_ERROR;

    /* Create a dataset with the old dataset region reference type */
    if ((ref_did = H5Dcreate2(fid, OLD_REF_REG_DSET, H5T_STD_REF_DSETREG, ref_sid, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataset region reference */
    if (H5Rcreate(&old_ref_reg_wbuf[0], fid, "/Group/Dataset", H5R_DATASET_REGION, sid) < 0)
        TEST_ERROR;

    /* Write selection to the reference dataset */
    if (H5Dwrite(ref_did, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT, old_ref_reg_wbuf) < 0)
        TEST_ERROR;

    /* Closing */
    if (H5Dclose(ref_did) < 0)
        TEST_ERROR;
    if (H5Sclose(ref_sid) < 0)
        TEST_ERROR;

    if (H5Dclose(did) < 0)
        TEST_ERROR;
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(ref_did);
        H5Sclose(ref_sid);
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(fapl);
        H5Fclose(fid);
        free(dwbuf);
    }
    H5E_END_TRY

    return FAIL;

} /* gen_ref_files() */

int
main(void)
{
    /* Generate file bounds_earliest_latest.h5 */
    if (gen_earliest_latest() < 0)
        TEST_ERROR;

    /* Generate file bounds_earliest_v18.h5 */
    if (gen_earliest_v18() < 0)
        TEST_ERROR;

    /* Generate file bounds_latest_latest.h5 */
    if (gen_latest_latest() < 0)
        TEST_ERROR;

    /* Generate file bounds_v18_latest.h5 */
    if (gen_v18_latest() < 0)
        TEST_ERROR;

    /* Generate file bounds_v18_v18.h5 */
    if (gen_v18_v18() < 0)
        TEST_ERROR;

    /*
     * Files generated via gen_ref_files()
     */

    /* Generate bounds_ref_earliest_latest.h5 */
    if (gen_ref_files(FILENAME_REF_E_L, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Generate bounds_ref_latest_latest.h5 */
    if (gen_ref_files(FILENAME_REF_L_L, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Generate bounds_ref_v112_v112.h5 */
    if (gen_ref_files(FILENAME_REF_V112_V112, H5F_LIBVER_V112, H5F_LIBVER_V112) < 0)
        TEST_ERROR;

    /* Generate bounds_ref_v110_v110.h5 */
    if (gen_ref_files(FILENAME_REF_V110_V110, H5F_LIBVER_V110, H5F_LIBVER_V110) < 0)
        TEST_ERROR;

    /* Generate bounds_ref_v18_v18.h5 */
    if (gen_ref_files(FILENAME_REF_V18_V18, H5F_LIBVER_V18, H5F_LIBVER_V18) < 0)
        TEST_ERROR;

    /*
     * Files generated via gen_sel_files()
     */

    /* Generate bounds_sel_earliest_latest.h5 */
    if (gen_sel_files(FILENAME_SEL_E_L, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Generate bounds_sel_latest_latest.h5 */
    if (gen_sel_files(FILENAME_SEL_L_L, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Generate bounds_sel_v112_v112.h5 */
    if (gen_sel_files(FILENAME_SEL_V112_V112, H5F_LIBVER_V112, H5F_LIBVER_V112) < 0)
        TEST_ERROR;

    /* Generate bounds_sel_v110_v110.h5 */
    if (gen_sel_files(FILENAME_SEL_V110_V110, H5F_LIBVER_V110, H5F_LIBVER_V110) < 0)
        TEST_ERROR;

    return EXIT_SUCCESS;

error:
    return EXIT_FAILURE;
}
