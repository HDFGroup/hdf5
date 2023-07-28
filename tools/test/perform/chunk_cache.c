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
 *  Purpose: check the performance of chunk cache in these two cases (HDFFV-10601):
 *          1. partial chunks exist along any dimension.
 *          2. number of slots in chunk cache is smaller than the number of chunks
 *             in the fastest-growing dimension.
 */
#include "hdf5.h"
#include "H5private.h"
#include "h5test.h"

#define FILENAME "chunk_cache_perf.h5"

#define RANK 2

#define DSET1_NAME  "partial_chunks"
#define DSET1_DIM1  (9 * 1000)
#define DSET1_DIM2  9
#define CHUNK1_DIM1 (2 * 1000)
#define CHUNK1_DIM2 2

#define DSET2_NAME  "hash_value"
#define DSET2_DIM1  300
#define DSET2_DIM2  600
#define CHUNK2_DIM1 100
#define CHUNK2_DIM2 100

#define RDCC_NSLOTS 5
#define RDCC_NBYTES (1024 * 1024 * 10)
#define RDCC_W0     0.75

#define FILTER_COUNTER 306
static size_t nbytes_global;

typedef struct test_time_t {
    long tv_sec;
    long tv_usec;
} test_time_t;

/* Local function prototypes for the dummy filter */
static size_t counter(unsigned flags, size_t cd_nelmts, const unsigned *cd_values, size_t nbytes,
                      size_t *buf_size, void **buf);

/* This message derives from H5Z */
static const H5Z_class2_t H5Z_COUNTER[1] = {{
    H5Z_CLASS_T_VERS, /* H5Z_class_t version          */
    FILTER_COUNTER,   /* Filter id number             */
    1, 1,             /* Encoding and decoding enabled */
    "counter",        /* Filter name for debugging    */
    NULL,             /* The "can apply" callback     */
    NULL,             /* The "set local" callback     */
    counter,          /* The actual filter function   */
}};

/*-------------------------------------------------------------------------
 *      Count number of bytes but don't do anything else.  Keep
 *      track of the data of chunks being read from file into memory.
 */
static size_t
counter(unsigned H5_ATTR_UNUSED flags, size_t H5_ATTR_UNUSED cd_nelmts,
        const unsigned H5_ATTR_UNUSED *cd_values, size_t nbytes, size_t H5_ATTR_UNUSED *buf_size,
        void H5_ATTR_UNUSED **buf)
{
    nbytes_global += nbytes;
    return nbytes;
}

/*---------------------------------------------------------------------------*/
static void
cleanup(void)
{
    if (!getenv(HDF5_NOCLEANUP)) {
        remove(FILENAME);
    }
}

/*-------------------------------------------------------------------------------
 *      Create a chunked dataset with partial chunks along either dimensions:
 *          dataset dimension:  9000 x 9
 *          chunk dimension:    2000 x 2
 */
static int
create_dset1(hid_t file)
{
    hid_t   dataspace = H5I_INVALID_HID, dataset = H5I_INVALID_HID;
    hid_t   dcpl             = H5I_INVALID_HID;
    hsize_t dims[RANK]       = {DSET1_DIM1, DSET1_DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK1_DIM1, CHUNK1_DIM2};
    struct {
        int arr[DSET1_DIM1][DSET1_DIM2];
    } *data = malloc(sizeof(*data));

    /* Create the data space. */
    if ((dataspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking  */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        goto error;

    /* Set the dummy filter simply for counting the number of bytes being read into the memory */
    if (H5Zregister(H5Z_COUNTER) < 0)
        goto error;

    if (H5Pset_filter(dcpl, FILTER_COUNTER, 0, 0, NULL) < 0)
        goto error;

    /* Create a new dataset within the file using chunk creation properties.  */
    if ((dataset = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) <
        0)
        goto error;

    /* Fill array */
    H5TEST_FILL_2D_HEAP_ARRAY(data, int);

    /* Write data to dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto error;

    /* Close resources */
    H5Dclose(dataset);
    H5Pclose(dcpl);
    H5Sclose(dataspace);
    free(data);
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dataset);
        H5Pclose(dcpl);
        H5Sclose(dataspace);
    }
    H5E_END_TRY
    free(data);

    return 1;
}

/*---------------------------------------------------------------------------
 *      Create a chunked dataset for testing hash values:
 *          dataset dimensions: 300 x 600
 *          chunk dimensions:   100 x 100
 */
static int
create_dset2(hid_t file)
{
    hid_t   dataspace = H5I_INVALID_HID, dataset = H5I_INVALID_HID;
    hid_t   dcpl             = H5I_INVALID_HID;
    hsize_t dims[RANK]       = {DSET2_DIM1, DSET2_DIM2};
    hsize_t chunk_dims[RANK] = {CHUNK2_DIM1, CHUNK2_DIM2};
    struct {
        int arr[DSET2_DIM1][DSET2_DIM2];
    } *data = malloc(sizeof(*data));

    /* Create the data space. */
    if ((dataspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking  */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl, RANK, chunk_dims) < 0)
        goto error;

    /* Set the dummy filter simply for counting the number of bytes being read into the memory */
    if (H5Zregister(H5Z_COUNTER) < 0)
        goto error;
    if (H5Pset_filter(dcpl, FILTER_COUNTER, 0, 0, NULL) < 0)
        goto error;

    /* Create a new dataset within the file using chunk creation properties.  */
    if ((dataset = H5Dcreate2(file, DSET2_NAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dcpl, H5P_DEFAULT)) <
        0)
        goto error;

    /* Fill array */
    H5TEST_FILL_2D_HEAP_ARRAY(data, int);

    /* Write data to dataset */
    if (H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto error;

    /* Close resources */
    H5Dclose(dataset);
    H5Pclose(dcpl);
    H5Sclose(dataspace);
    free(data);

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dataset);
        H5Pclose(dcpl);
        H5Sclose(dataspace);
    }
    H5E_END_TRY
    free(data);

    return 1;
}

/*---------------------------------------------------------------------------
 *      Check the performance of the chunk cache when partial chunks exist
 *      along the dataset dimensions.
 */
static int
check_partial_chunks_perf(hid_t file)
{
    hid_t dataset   = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID;
    hid_t memspace  = H5I_INVALID_HID;
    hid_t dapl      = H5I_INVALID_HID;

    int rdata[DSET1_DIM2]; /* data for reading */
    int i;

    hsize_t row_rank    = 1;
    hsize_t row_dim[1]  = {DSET1_DIM2};
    hsize_t start[RANK] = {0, 0};
    hsize_t count[RANK] = {1, DSET1_DIM2};
    double  start_t, end_t;

    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        goto error;
    if (H5Pset_chunk_cache(dapl, RDCC_NSLOTS, RDCC_NBYTES, RDCC_W0) < 0)
        goto error;

    dataset = H5Dopen2(file, DSET1_NAME, dapl);

    H5_CHECK_OVERFLOW(row_rank, hsize_t, int);
    memspace  = H5Screate_simple((int)row_rank, row_dim, NULL);
    filespace = H5Dget_space(dataset);

    nbytes_global = 0;

    start_t = H5_get_time();

    /* Read the data row by row */
    for (i = 0; i < DSET1_DIM1; i++) {
        start[0] = (hsize_t)i;
        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            goto error;

        if (H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, rdata) < 0)
            goto error;
    }

    end_t = H5_get_time();

    if ((end_t - start_t) > 0.0)
        printf("1. Partial chunks: total read time is %lf; number of bytes being read from file is %zu\n",
               (end_t - start_t), nbytes_global);
    else
        printf("1. Partial chunks: no total read time because timer is not available; number of bytes being "
               "read from file is %zu\n",
               nbytes_global);

    H5Dclose(dataset);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dapl);

    return 0;
error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dataset);
        H5Sclose(filespace);
        H5Sclose(memspace);
        H5Pclose(dapl);
    }
    H5E_END_TRY
    return 1;
}

/*---------------------------------------------------------------------------
 *      Check the performance of chunk cache when the number of cache slots
 *      is smaller than the number of chunks along the fastest-growing
 *      dimension of the dataset.
 */
static int
check_hash_value_perf(hid_t file)
{
    hid_t dataset   = H5I_INVALID_HID;
    hid_t filespace = H5I_INVALID_HID;
    hid_t memspace  = H5I_INVALID_HID;
    hid_t dapl      = H5I_INVALID_HID;

    int rdata[DSET2_DIM1]; /* data for reading */
    int i;

    hsize_t column_rank   = 1;
    hsize_t column_dim[1] = {DSET2_DIM1};
    hsize_t start[RANK]   = {0, 0};
    hsize_t count[RANK]   = {DSET2_DIM1, 1};
    double  start_t, end_t;

    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        goto error;
    if (H5Pset_chunk_cache(dapl, RDCC_NSLOTS, RDCC_NBYTES, RDCC_W0) < 0)
        goto error;

    if ((dataset = H5Dopen2(file, DSET2_NAME, dapl)) < 0)
        goto error;

    H5_CHECK_OVERFLOW(column_rank, hsize_t, int);
    if ((memspace = H5Screate_simple((int)column_rank, column_dim, NULL)) < 0)
        goto error;
    if ((filespace = H5Dget_space(dataset)) < 0)
        goto error;

    nbytes_global = 0;

    start_t = H5_get_time();

    /* Read the data column by column */
    for (i = 0; i < DSET2_DIM2; i++) {
        start[1] = (hsize_t)i;
        if (H5Sselect_hyperslab(filespace, H5S_SELECT_SET, start, NULL, count, NULL) < 0)
            goto error;

        if (H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace, H5P_DEFAULT, rdata) < 0)
            goto error;
    }

    end_t = H5_get_time();

    if ((end_t - start_t) > 0.0)
        printf("2. Hash value: total read time is %lf; number of bytes being read from file is %zu\n",
               (end_t - start_t), nbytes_global);
    else
        printf("2. Hash value: no total read time because timer is not available; number of bytes being read "
               "from file is %zu\n",
               nbytes_global);

    H5Dclose(dataset);
    H5Sclose(filespace);
    H5Sclose(memspace);
    H5Pclose(dapl);
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dataset);
        H5Sclose(filespace);
        H5Sclose(memspace);
        H5Pclose(dapl);
    }
    H5E_END_TRY
    return 1;
}

/*-------------------------------------------------------------------------------------
 *  Purpose: check the performance of chunk cache in these two cases (HDFFV-10601):
 *          1. partial chunks exist along any dimension.
 *          2. number of slots in chunk cache is smaller than the number of chunks
 *             in the fastest-growing dimension.
 *-------------------------------------------------------------------------------------*/
int
main(void)
{
    hid_t file    = H5I_INVALID_HID; /* file ID */
    int   nerrors = 0;

    /* Create a new file. If file exists its contents will be overwritten. */
    if ((file = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    nerrors += create_dset1(file);
    nerrors += create_dset2(file);

    if (H5Fclose(file) < 0)
        goto error;

    /* Re-open the file for testing performance. */
    if ((file = H5Fopen(FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        goto error;

    nerrors += check_partial_chunks_perf(file);
    nerrors += check_hash_value_perf(file);

    if (H5Fclose(file) < 0)
        goto error;

    if (nerrors > 0)
        goto error;
    cleanup();
    return 0;

error:
    fprintf(stderr, "*** ERRORS DETECTED ***\n");
    return 1;
}
