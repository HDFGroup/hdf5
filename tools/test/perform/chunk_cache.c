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
 *  Purpose: check the performance of chunk cache in these two cases (HDFFV-10601):
 *          1. partial chunks exist along any dimension.
 *          2. number of slots in chunk cache is smaller than the number of chunks
 *             in the fastest-growing dimension.
 */
#include <time.h>
#include "hdf5.h"

#define FILENAME    "chunk_cache_perf.h5"

#define RANK        2

#define DSET1_NAME  "partial_chunks"
#define DSET1_DIM1   9 * 1000
#define DSET1_DIM2   9
#define CHUNK1_DIM1  2 * 1000
#define CHUNK1_DIM2  2

#define DSET2_NAME  "hash_value"
#define DSET2_DIM1   300
#define DSET2_DIM2   600
#define CHUNK2_DIM1  100
#define CHUNK2_DIM2  100

#define RDCC_NSLOTS  5
#define RDCC_NBYTES  1024 * 1024 * 10
#define RDCC_W0      0.75F

#define FILTER_COUNTER    306
static size_t    nbytes_global;

typedef struct test_time_t {
    long tv_sec;
    long tv_usec;
} test_time_t;

/* Local function prototypes for the dummy filter */
static size_t
counter (unsigned flags, size_t cd_nelmts,
         const unsigned *cd_values, size_t nbytes,
         size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_COUNTER[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version          */
    FILTER_COUNTER,             /* Filter id number             */
    1, 1,                       /* Encoding and decoding enabled */
    "counter",                  /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    counter,                    /* The actual filter function   */
}};

/*-------------------------------------------------------------------------
 *      Count number of bytes but don't do anything else.  Keep
 *      track of the data of chunks being read from file into memory.
 */
static size_t
counter (unsigned flags, size_t cd_nelmts,
         const unsigned *cd_values, size_t nbytes,
         size_t *buf_size, void **buf)
{
    nbytes_global += nbytes;
    return nbytes;
}

/*---------------------------------------------------------------------------*/
static int
test_time_get_current(test_time_t *tv)
{
    struct timespec tp;
    
    if (!tv)
        return -1;
    if (clock_gettime(CLOCK_MONOTONIC, &tp))
        return -1;
    
    tv->tv_sec = tp.tv_sec;
    tv->tv_usec = tp.tv_nsec / 1000;
    
    return 0;
}

/*---------------------------------------------------------------------------*/
static double
test_time_to_double(test_time_t tv)
{
    return (double) tv.tv_sec + (double) (tv.tv_usec) * 0.000001;
}

/*---------------------------------------------------------------------------*/
static test_time_t
test_time_add(test_time_t in1, test_time_t in2)
{
    test_time_t out;
    
    out.tv_sec = in1.tv_sec + in2.tv_sec;
    out.tv_usec = in1.tv_usec + in2.tv_usec;
    if(out.tv_usec > 1000000) {
        out.tv_usec -= 1000000;
        out.tv_sec += 1;
    }
    
    return out;
}

/*---------------------------------------------------------------------------*/
static test_time_t
test_time_subtract(test_time_t in1, test_time_t in2)
{
    test_time_t out;
    
    out.tv_sec = in1.tv_sec - in2.tv_sec;
    out.tv_usec = in1.tv_usec - in2.tv_usec;
    if(out.tv_usec < 0) {
        out.tv_usec += 1000000;
        out.tv_sec -= 1;
    }
    
    return out;
}

/*-------------------------------------------------------------------------------
 *      Create a chunked dataset with partial chunks along either dimensions:
 *          dataset dimension:  9000 x 9
 *          chunk dimension:    2000 x 2
 */
static void create_dset1(hid_t file)
{
    hid_t        dataspace, dataset;
    hid_t        dcpl;
    hsize_t      dims[RANK]  = {DSET1_DIM1, DSET1_DIM2};
    herr_t       status;
    hsize_t      chunk_dims[RANK] = {CHUNK1_DIM1, CHUNK1_DIM2};
    int          data[DSET1_DIM1][DSET1_DIM2];    /* data for writing */
    int          i, j;
    
    /* Create the data space. */
    dataspace = H5Screate_simple (RANK, dims, NULL);
    
    /* Modify dataset creation properties, i.e. enable chunking  */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_chunk (dcpl, RANK, chunk_dims);
    
    /* Set the dummy filter simply for counting the number of bytes being read into the memory */
    H5Zregister(H5Z_COUNTER);
    H5Pset_filter(dcpl, FILTER_COUNTER, 0, 0, NULL);
    
    /* Create a new dataset within the file using chunk creation properties.  */
    dataset = H5Dcreate2 (file, DSET1_NAME, H5T_NATIVE_INT, dataspace,
                          H5P_DEFAULT, dcpl, H5P_DEFAULT);
    
    for (i = 0; i < DSET1_DIM1; i++)
        for (j = 0; j < DSET1_DIM2; j++)
            data[i][j] = i+j;
    
    /* Write data to dataset */
    status = H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                       H5P_DEFAULT, data);
    
    /* Close resources */
    status = H5Dclose (dataset);
    status = H5Pclose (dcpl);
    status = H5Sclose (dataspace);
}

/*---------------------------------------------------------------------------
 *      Create a chunked dataset for testing hash values:
 *          dataset dimensions: 300 x 600
 *          chunk dimensions:   100 x 100
 */
static void create_dset2(hid_t file)
{
    hid_t        dataspace, dataset;
    hid_t        dcpl;
    hsize_t      dims[RANK]  = {DSET2_DIM1, DSET2_DIM2};
    herr_t       status;
    hsize_t      chunk_dims[RANK] = {CHUNK2_DIM1, CHUNK2_DIM2};
    int          data[DSET2_DIM1][DSET2_DIM2];    /* data for writing */
    int          i, j;
    
    /* Create the data space. */
    dataspace = H5Screate_simple (RANK, dims, NULL);
    
    /* Modify dataset creation properties, i.e. enable chunking  */
    dcpl = H5Pcreate (H5P_DATASET_CREATE);
    status = H5Pset_chunk (dcpl, RANK, chunk_dims);
    
    /* Set the dummy filter simply for counting the number of bytes being read into the memory */
    H5Zregister(H5Z_COUNTER);
    H5Pset_filter(dcpl, FILTER_COUNTER, 0, 0, NULL);
    
    /* Create a new dataset within the file using chunk creation properties.  */
    dataset = H5Dcreate2 (file, DSET2_NAME, H5T_NATIVE_INT, dataspace,
                          H5P_DEFAULT, dcpl, H5P_DEFAULT);
    
    for (i = 0; i < DSET2_DIM1; i++)
        for (j = 0; j < DSET2_DIM2; j++)
            data[i][j] = i+j;
    
    /* Write data to dataset */
    status = H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
                       H5P_DEFAULT, data);
    
    /* Close resources */
    status = H5Dclose (dataset);
    status = H5Pclose (dcpl);
    status = H5Sclose (dataspace);
}
/*---------------------------------------------------------------------------
 *      Check the performance of the chunk cache when partial chunks exist
 *      along the dataset dimensions.
 */
static void check_partial_chunks_perf(hid_t file)
{
    hid_t        dataset;
    hid_t        filespace;
    hid_t        memspace;
    hid_t        dapl;
    
    herr_t       status;
    int          rdata[DSET1_DIM2];   /* data for reading */
    int          i;
    
    hsize_t      row_rank = 1;
    hsize_t      row_dim[1] = {DSET1_DIM2};
    hsize_t      start[RANK] = {0, 0};
    hsize_t      count[RANK] = {1, DSET1_DIM2};
    test_time_t t = {0, 0}, t1 = {0, 0}, t2 = {0, 0};
    
    dapl = H5Pcreate(H5P_DATASET_ACCESS);
    status = H5Pset_chunk_cache (dapl, RDCC_NSLOTS, RDCC_NBYTES, RDCC_W0);

    dataset = H5Dopen2 (file, DSET1_NAME, dapl);
    
    memspace = H5Screate_simple(row_rank, row_dim, NULL);
    filespace = H5Dget_space(dataset);
    
    nbytes_global = 0;
    
    test_time_get_current(&t1);
    
    /* Read the data row by row */
    for(i = 0; i < DSET1_DIM1; i++) {
        start[0] = i;
        status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET,
                                     start, NULL, count, NULL);
        
        status = H5Dread (dataset, H5T_NATIVE_INT, memspace, filespace,
                          H5P_DEFAULT, rdata);
    }
    
    test_time_get_current(&t2);
    t = test_time_add(t, test_time_subtract(t2, t1));
    
    printf("1. Partial chunks: total read time is %lf; number of bytes being read from file is %lu\n", test_time_to_double(t), nbytes_global);
    
    status = H5Dclose (dataset);
    status = H5Sclose (filespace);
    status = H5Sclose (memspace);
    status = H5Pclose (dapl);
}

/*---------------------------------------------------------------------------
 *      Check the performance of chunk cache when the number of cache slots
 *      is smaller than the number of chunks along the fastest-growing
 *      dimension of the dataset.
 */
static void check_hash_value_perf(hid_t file)
{
    hid_t        dataset;
    hid_t        filespace;
    hid_t        memspace;
    hid_t        dapl;
    
    herr_t       status;
    int          rdata[DSET2_DIM1];   /* data for reading */
    int          i;
    
    hsize_t      column_rank = 1;
    hsize_t      column_dim[1] = {DSET2_DIM1};
    hsize_t      start[RANK] = {0, 0};
    hsize_t      count[RANK] = {DSET2_DIM1, 1};
    test_time_t t = {0, 0}, t1 = {0, 0}, t2 = {0, 0};
    
    dapl = H5Pcreate(H5P_DATASET_ACCESS);
    status = H5Pset_chunk_cache (dapl, RDCC_NSLOTS, RDCC_NBYTES, RDCC_W0);
    
    dataset = H5Dopen2 (file, DSET2_NAME, dapl);
    
    memspace = H5Screate_simple(column_rank, column_dim, NULL);
    filespace = H5Dget_space(dataset);
    
    nbytes_global = 0;
    
    test_time_get_current(&t1);
    
    /* Read the data column by column */
    for(i = 0; i < DSET2_DIM2; i++) {
        start[1] = i;
        status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET,
                                     start, NULL, count, NULL);
        
        status = H5Dread (dataset, H5T_NATIVE_INT, memspace, filespace,
                          H5P_DEFAULT, rdata);
    }
    
    test_time_get_current(&t2);
    t = test_time_add(t, test_time_subtract(t2, t1));
    
    printf("2. Hash value: total read time is %lf; number of bytes being read from file is %lu\n", test_time_to_double(t), nbytes_global);
    
    status = H5Dclose (dataset);
    status = H5Sclose (filespace);
    status = H5Sclose (memspace);
    status = H5Pclose (dapl);
}

/*-------------------------------------------------------------------------------------
 *  Purpose: check the performance of chunk cache in these two cases (HDFFV-10601):
 *          1. partial chunks exist along any dimension.
 *          2. number of slots in chunk cache is smaller than the number of chunks
 *             in the fastest-growing dimension.
 *-------------------------------------------------------------------------------------*/
int
main (void)
{
    hid_t        file;                          /* handles */

    /* Create a new file. If file exists its contents will be overwritten. */
    file = H5Fcreate (FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    create_dset1(file);
    create_dset2(file);
    
    H5Fclose (file);

    /* Re-open the file for testing performance. */
    file = H5Fopen (FILENAME, H5F_ACC_RDONLY, H5P_DEFAULT);

    check_partial_chunks_perf(file);
    check_hash_value_perf(file);
    
    H5Fclose (file);
    
    return 0;
}
