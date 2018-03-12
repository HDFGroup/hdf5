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

#include "h5test.h"

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

static herr_t gen_earliest_latest(void)
{
    hid_t   fid = -1;      /* File ID */
    hid_t   fapl = -1;     /* File access property list ID */
    hid_t   fcpl = -1;     /* File creation property list ID */
    hid_t   dcpl = -1;     /* Dataset creation property list ID */
    hid_t   space = -1;    /* Dataspace ID */
    hid_t   dset = -1;     /* Dataset ID */
    float  *buf = NULL;    /* Buffer for writing data */
    float  *bufp = NULL;   /* Pointer to data buffer */
    hsize_t dims[RANK] = {DIM1, DIM2}; /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the earliest/latest version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_E_L, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        TEST_ERROR;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */
    buf = (float *)HDmalloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL) TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0) TEST_ERROR;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;

    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Disable partial chunk filters, triggers layout version 4 */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Release allocated buffer */
    HDfree(buf);
    bufp = buf = NULL;

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;
    if(H5Sclose(space) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        HDfree(buf);
    } H5E_END_TRY;
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
static herr_t gen_earliest_v18(void)
{
    hid_t   fid = -1;      /* File ID */
    hid_t   fapl = -1;     /* File access property list ID */
    hid_t   fcpl = -1;     /* File creation property list ID */
    hid_t   dcpl = -1;     /* Dataset creation property list ID */
    hid_t   space = -1;    /* Dataspace ID */
    hid_t   dset = -1;     /* Dataset ID */
    float  *buf = NULL;    /* Buffer for writing data */
    float  *bufp = NULL;   /* Pointer to data buffer */
    hsize_t dims[RANK] = {DIM1, DIM2}; /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the earliest/v18 version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18) < 0)
        TEST_ERROR;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_E_18, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        TEST_ERROR;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)HDmalloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL) TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0) TEST_ERROR;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;

    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Release allocated buffer */
    HDfree(buf);
    bufp = buf = NULL;

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;
    if(H5Sclose(space) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        HDfree(buf);
    } H5E_END_TRY;
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
static herr_t gen_latest_latest(void)
{
    hid_t   fid = -1;      /* File ID */
    hid_t   fapl = -1;     /* File access property list ID */
    hid_t   dcpl = -1;     /* Dataset creation property list ID */
    hid_t   space = -1;    /* Dataspace ID */
    hid_t   dset = -1;     /* Dataset ID */
    float  *buf = NULL;    /* Buffer for writing data */
    float  *bufp = NULL;   /* Pointer to data buffer */
    hsize_t dims[RANK] = {DIM1, DIM2}; /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the latest/latest version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create the file with version 3 superblock */
    fid = H5Fcreate(FILENAME_L_L, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    if (fid < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    buf = (float *)HDmalloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL) TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0) TEST_ERROR;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Disable partial chunk filters, triggers layout version 4 */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Release allocated buffer */
    HDfree(buf);
    bufp = buf = NULL;

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;
    if(H5Sclose(space) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Fclose(fid);
        HDfree(buf);
    } H5E_END_TRY;
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
static herr_t gen_v18_latest(void)
{
    hid_t   fid = -1;      /* File ID */
    hid_t   fapl = -1;     /* File access property list ID */
    hid_t   fcpl = -1;     /* File creation property list ID */
    hid_t   dcpl = -1;     /* Dataset creation property list ID */
    hid_t   space = -1;    /* Dataspace ID */
    hid_t   dset = -1;     /* Dataset ID */
    float  *buf = NULL;    /* Buffer for writing data */
    float  *bufp = NULL;   /* Pointer to data buffer */
    hsize_t dims[RANK] = {DIM1, DIM2}; /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the v18/latest version of the format" bounds
       for creating objects in the file, also trigger version 2 superblock */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_18_L, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        TEST_ERROR;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)HDmalloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL) TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0) TEST_ERROR;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Release allocated buffer */
    HDfree(buf);
    bufp = buf = NULL;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;
    if(H5Sclose(space) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        HDfree(buf);
    } H5E_END_TRY;
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
static herr_t gen_v18_v18(void)
{
    hid_t   fid = -1;      /* File ID */
    hid_t   fapl = -1;     /* File access property list ID */
    hid_t   fcpl = -1;     /* File creation property list ID */
    hid_t   dcpl = -1;     /* Dataset creation property list ID */
    hid_t   space = -1;    /* Dataspace ID */
    hid_t   dset = -1;     /* Dataset ID */
    float  *buf = NULL;    /* Buffer for writing data */
    float  *bufp = NULL;   /* Pointer to data buffer */
    hsize_t dims[RANK] = {DIM1, DIM2}; /* Dimensions */
    hsize_t chunk_dims[RANK] = {CHK_DIM1, CHK_DIM2}; /* Dimensions of chunk */
    int     i, j;
    herr_t  ret = SUCCEED; /* Generic return value */

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the v18 version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18) < 0)
        TEST_ERROR;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_18_18, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        TEST_ERROR;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(fcpl) < 0) TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    buf = (float *)HDmalloc((size_t)DIM1 * (size_t)DIM2 * sizeof(float));
    if (buf == NULL) TEST_ERROR;

    /* Fill sample data */
    bufp = buf;
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            *bufp = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0) TEST_ERROR;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;

    /* Close the file, then reopen it with the latest version */
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;

    /* Set the "use the v18/latest version of the format" bounds
       for creating a layout version 4 object in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    if((fid = H5Fopen(FILENAME_18_18, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR;

    /* Disable partial chunk filters */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        TEST_ERROR;

    /* Create and write the dataset */
    dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if (dset < 0) TEST_ERROR;
    ret = H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf);
    if (ret < 0) TEST_ERROR;

    /* Release allocated buffer */
    HDfree(buf);
    bufp = buf = NULL;

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Dclose(dset) < 0) TEST_ERROR;
    if(H5Sclose(space) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        HDfree(buf);
    } H5E_END_TRY;
    return FAIL;
} /* gen_v18_v18 */

int main(void)
{
    /* Generate file bounds_earliest_latest.h5 */
    if (gen_earliest_latest() < 0) TEST_ERROR;

    /* Generate file bounds_earliest_v18.h5 */
    if (gen_earliest_v18() < 0) TEST_ERROR;

    /* Generate file bounds_latest_latest.h5 */
    if (gen_latest_latest() < 0) TEST_ERROR;

    /* Generate file bounds_v18_latest.h5 */
    if (gen_v18_latest() < 0) TEST_ERROR;

    /* Generate file bounds_v18_v18.h5 */
    if (gen_v18_v18() < 0) TEST_ERROR;

    return EXIT_SUCCESS;

error:
    return EXIT_FAILURE;
}

