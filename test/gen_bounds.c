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
 *              Oct 24, 2017
 *
 * Purpose:     This program is run to generate HDF5 data files used to test
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
 *
 */

#include "hdf5.h"

/***********************************************************************
 * gen_earliest_latest() creates file "bounds_earliest_latest.h5"
 *
 * File contents:
 * - Version 0 superblock (default)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4". (H5Pset_chunk_opts)
 *
 ***********************************************************************/
#define FILENAME_E_L "bounds_earliest_latest.h5"

/* 2-D dataset with fixed dimensions */
#define RANK  2
#define DIM1  100
#define DIM2  200
#define CHUNK_DIM1  50
#define CHUNK_DIM2  50

int gen_earliest_latest()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t fcpl = -1;    /* File creation property list ID */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hsize_t dims[RANK] = {DIM1, DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    float buf[DIM1][DIM2];          /* Buffer for writing data */
    int i, j;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the earliest/latest version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_E_L, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        goto error;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    /* Fill sample data */
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            buf[i][j] = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Disable partial chunk filters, triggers layout version 4 */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close everything */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;
    if(H5Sclose(space) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

/***********************************************************************
 * gen_earliest_v18() creates file "bounds_earliest_v18.h5"
 *
 * File contents:
 * - Version 0 superblock (default)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 *
 ***********************************************************************/
#define FILENAME_E_18 "bounds_earliest_v18.h5"

int gen_earliest_v18()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t fcpl = -1;    /* File creation property list ID */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hsize_t dims[RANK] = {DIM1, DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    float buf[DIM1][DIM2];          /* Buffer for writing data */
    int i, j;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the earliest/v18 version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18) < 0)
        goto error;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_E_18, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        goto error;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    /* Fill sample data */
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            buf[i][j] = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close everything */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;
    if(H5Sclose(space) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

/***********************************************************************
 * gen_latest_latest() creates file "bounds_latest_latest.h5"
 *
 * File contents:
 * - Version 3 superblock (H5Fcreate with H5F_ACC_SWMR_WRITE)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4". (H5Pset_chunk_opts)
 *
 ***********************************************************************/
#define FILENAME_L_L "bounds_latest_latest.h5"

int gen_latest_latest()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t fcpl = -1;    /* File creation property list ID */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hsize_t dims[RANK] = {DIM1, DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    float buf[DIM1][DIM2];          /* Buffer for writing data */
    int i, j;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Create file with H5F_ACC_SWMR_WRITE, triggers version 3 superblock */
    if((fid = H5Fcreate(FILENAME_L_L, H5F_ACC_SWMR_WRITE, fcpl, fapl)) <0)
        goto error;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Fill sample data */
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            buf[i][j] = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Disable partial chunk filters, triggers layout version 4 */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close everything */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;
    if(H5Sclose(space) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

/***********************************************************************
 * gen_v18_latest() creates file "bounds_v18_latest.h5"
 *
 * File contents:
 * - Version 2 superblock
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 *
 ***********************************************************************/
#define FILENAME_18_L "bounds_v18_latest.h5"

int gen_v18_latest()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t fcpl = -1;    /* File creation property list ID */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hsize_t dims[RANK] = {DIM1, DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    float buf[DIM1][DIM2];          /* Buffer for writing data */
    int i, j;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the v18/latest version of the format" bounds
       for creating objects in the file, also trigger version 2 superblock */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        goto error;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_18_L, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        goto error;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    /* Fill sample data */
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            buf[i][j] = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;
    if(H5Sclose(space) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

/***********************************************************************
 * gen_v18_v18() creates file "bounds_v18_v18.h5"
 *
 * File contents:
 * - Version 2 superblock (H5Pset_libver_bounds(v18, v18)
 * - A chunked dataset with layout version 3, "DS_chunked_layout_3". (default)
 * - A chunked dataset with layout version 4, "DS_chunked_layout_4". (H5Pset_chunk_opts)
 *
 ***********************************************************************/
#define FILENAME_18_18 "bounds_v18_v18.h5"

int gen_v18_v18()
{
    hid_t fid = -1;     /* File ID */
    hid_t fapl = -1;    /* File access property list ID */
    hid_t fcpl = -1;    /* File creation property list ID */
    hid_t dcpl = -1;    /* Dataset creation property list ID */
    hid_t space = -1;   /* Dataspace ID */
    hid_t dset = -1;    /* Dataset ID */
    hsize_t dims[RANK] = {DIM1, DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    float buf[DIM1][DIM2];          /* Buffer for writing data */
    int i, j;

    /* Create file creation property list */
    if((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the v18 version of the format" bounds
       for creating objects in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_V18) < 0)
        goto error;

    /* Create file */
    if((fid = H5Fcreate(FILENAME_18_18, H5F_ACC_TRUNC, fcpl, fapl)) <0)
        goto error;

    /* Close file property lists */
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Pclose(fcpl) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 3 (default)
     */

    /* Fill sample data */
    for (i = 0; i < DIM1; i++)
        for (j = 0; j < DIM2; j++)
            buf[i][j] = 100.0F;

    /* Create the dataspace */
    if((space = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_3", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close property list and dataset, will reuse dataspace */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;

    /* Close the file, then reopen it with the latest version */
    if(H5Fclose(fid) < 0)
        goto error;

    /* Create file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;

    /* Set the "use the latest version of the format" bounds
       for creating a layout version 4 object in the file */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_V18, H5F_LIBVER_LATEST) < 0)
        goto error;

    if((fid = H5Fopen(FILENAME_18_18, H5F_ACC_RDWR, fapl)) < 0)
        goto error;

    /*
     * Add a chunked dataset with layout version 4 (H5Pset_chunk_opts)
     */

    /* Create the dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /* Set up for chunked data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        goto error;

    /* Disable partial chunk filters */
    if(H5Pset_chunk_opts(dcpl, H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS) < 0)
        goto error;

    /* Create and write the dataset */
    if((dset = H5Dcreate2(fid, "DS_chunked_layout_4", H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;
    if(H5Dwrite(dset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        goto error;

    /* Close everything */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Pclose(fapl) < 0)
        goto error;
    if(H5Dclose(dset) < 0)
        goto error;
    if(H5Sclose(space) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(space);
        H5Pclose(dcpl);
        H5Pclose(fcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;
    return 1;
}

int main(void)
{
    /* Generate file bounds_earliest_latest.h5 */
    if (gen_earliest_latest() < 0)
        goto error;

    /* Generate file bounds_earliest_v18.h5 */
    if (gen_earliest_v18() < 0)
        goto error;

    /* Generate file bounds_latest_latest.h5 */
    if (gen_latest_latest() < 0)
        goto error;

    /* Generate file bounds_v18_latest.h5 */
    if (gen_v18_latest() < 0)
        goto error;

    /* Generate file bounds_v18_v18.h5 */
    if (gen_v18_v18() < 0)
        goto error;

    return 0;

error:
    return 1;
}

