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

#include "h5test.h"

#if defined(H5_HAVE_ZLIB_H) && !defined(H5_ZLIB_HEADER)
# define H5_ZLIB_HEADER "zlib.h"
#endif
#if defined(H5_ZLIB_HEADER)
# include H5_ZLIB_HEADER /* "zlib.h" */
#endif

#define FILE_NAME "direct_chunk.h5"

/* Datasets for Direct Write tests */
#define DATASETNAME1        "direct_write"
#define DATASETNAME2        "skip_one_filter"
#define DATASETNAME3        "skip_two_filters"
#define DATASETNAME4        "data_conv"
#define DATASETNAME5        "contiguous_dset"
#define DATASETNAME6        "invalid_argue"
#define DATASETNAME7        "overwrite_chunk"
/* Datasets for Direct Read tests */
#define DATASETNAME8        "disabled_chunk_cache"
#define DATASETNAME9        "flush_chunk_cache"
#define DATASETNAME10       "read_w_valid_cache"
#define DATASETNAME11       "unallocated_chunk"
#define DATASETNAME12       "unfiltered_data"

#define RANK         2
#define NX     16
#define NY     16
#define CHUNK_NX     4
#define CHUNK_NY     4

#define DEFLATE_SIZE_ADJUST(s) (HDceil(((double)(s))*H5_DOUBLE(1.001))+H5_DOUBLE(12.0))

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS1    305
#define H5Z_FILTER_BOGUS2    306
#define ADD_ON            7
#define FACTOR            3

/* Constants for the overwrite test */
#define OVERWRITE_NDIMS         3
#define OVERWRITE_CHUNK_NX      3
#define OVERWRITE_CHUNK_2NX     6
#define OVERWRITE_CHUNK_NY      2
#define OVERWRITE_VALUE         42

/* Test configurations */
#define CONFIG_LATEST       0x01
#define CONFIG_REOPEN_FILE  0x02
#define CONFIG_REOPEN_DSET  0x04
#define CONFIG_DIRECT_WRITE 0x08
#define CONFIG_DIRECT_READ  0x10
#define CONFIG_END          0x20

/* Defines used in test_single_chunk_latest() */
#define FILE            "single_latest.h5"
#define DATASET         "dataset"
#define DIM0            4
#define DIM1            32
#define CHUNK0          DIM0
#define CHUNK1          DIM1

/* Local prototypes for filter functions */
static size_t filter_bogus1(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_bogus2(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS1[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS1,        /* Filter id number        */
    1, 1,               /* Encoding and decoding enabled */
    "bogus1",            /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus1,        /* The actual filter function    */
}};

const H5Z_class2_t H5Z_BOGUS2[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS2,        /* Filter id number        */
    1, 1,               /* Encoding and decoding enabled */
    "bogus2",            /* Filter name for debugging    */
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus2,        /* The actual filter function    */
}};

/*-------------------------------------------------------------------------
 * Function:    test_direct_chunk_write
 *
 * Purpose:        Test the basic functionality of H5Dwrite_chunk
 *
 * Return:        Success:    0
 *                Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_DEFLATE
static int
test_direct_chunk_write (hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         ret;
    int         data[NX][NY];
    int         i, j, n;

    unsigned    filter_mask = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);

    const Bytef *z_src = (const Bytef*)(direct_buf);
    Bytef        *z_dst = NULL;            /*destination buffer        */
    uLongf         z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    uLong         z_src_nbytes = (uLong)buf_size;
    int          aggression = 9;        /* Compression aggression setting */
    void        *outbuf = NULL;         /* Pointer to new buffer */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("basic functionality of H5Dwrite_chunk");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /*
     * Modify dataset creation properties, i.e. enable chunking and compression
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    if((status = H5Pset_deflate( cparms, (unsigned) aggression)) < 0)
        goto error;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME1, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    /* Initialize the dataset */
    for(i = n = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
        data[i][j] = n++;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /*
     * Write the data for the dataset.  It should stay in the chunk cache.
     * It will be evicted from the cache by the H5Dwrite_chunk calls.
     */
    if((status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
            dxpl, data)) < 0)
        goto error;

    /* Initialize data for one chunk */
    for(i = n = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++)
        direct_buf[i][j] = n++;

    /* Allocate output (compressed) buffer */
    outbuf = HDmalloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        HDfprintf(stderr, "overflow");
        goto error;
    } else if(Z_MEM_ERROR == ret) {
        HDfprintf(stderr, "deflate memory error");
        goto error;
    } else if(Z_OK != ret) {
        HDfprintf(stderr, "other deflate error");
        goto error;
    }

    /* Write the compressed chunk data repeatedly to cover all the chunks in the
     * dataset, using the direct writing function.     */
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
            offset[1] += CHUNK_NY;
        }
        offset[0] += CHUNK_NX;
        offset[1] = 0;
    }

    if(outbuf)
        HDfree(outbuf);

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME1, H5P_DEFAULT)) < 0)
        goto error;

    /*
     * Select hyperslab for one chunk in the file
     */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        goto error;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != check_chunk[i][j]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[i][j], check_chunk[i][j]);
                goto error;
            }
        }
    }

    /* Reinitialize different data for one chunk */
    for(i = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++)
        direct_buf[i][j] = i + j;

    /* Allocate output (compressed) buffer */
    outbuf = HDmalloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        HDfprintf(stderr, "overflow");
        goto error;
    } else if(Z_MEM_ERROR == ret) {
        HDfprintf(stderr, "deflate memory error");
        goto error;
    } else if(Z_OK != ret) {
        HDfprintf(stderr, "other deflate error");
        goto error;
    }

    /* Rewrite the compressed chunk data repeatedly to cover all the chunks in the
     * dataset, using the direct writing function.     */
    offset[0] = offset[1] = 0;
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
            offset[1] += CHUNK_NY;
        }
        offset[0] += CHUNK_NX;
        offset[1] = 0;
    }

    if(outbuf)
        HDfree(outbuf);

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME1, H5P_DEFAULT)) < 0)
        goto error;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != check_chunk[i][j]) {
                HDprintf("    2. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[i][j], check_chunk[i][j]);
                goto error;
            }
        }
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    if(outbuf)
        HDfree(outbuf);

    H5_FAILED();
    return 1;
} /* test_direct_chunk_write() */
#endif /* H5_HAVE_FILTER_DEFLATE */

/*-------------------------------------------------------------------------
 * Function:    test_direct_chunk_overwrite_data
 *
 * Purpose:     Test overwriting a chunk with new data.
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Dana Robinson
 *              Spring 2017
 *
 *-------------------------------------------------------------------------
 */
static int
test_direct_chunk_overwrite_data(hid_t fid)
{
    size_t      buf_size = OVERWRITE_CHUNK_NX * OVERWRITE_CHUNK_NY * sizeof(int16_t);
    int16_t     data_buf[OVERWRITE_CHUNK_NY][OVERWRITE_CHUNK_NX];
    int16_t     overwrite_buf[OVERWRITE_CHUNK_NY][OVERWRITE_CHUNK_NX];
    uint32_t    filter_mask = 0;
    hid_t       tid = H5T_NATIVE_UINT16;
    hid_t       dcpl_id = -1;
    hid_t       sid = -1;
    hid_t       did = -1;
    uint16_t    fill_value = 0;
    hsize_t     dset_dims[] = {1, OVERWRITE_CHUNK_NY, OVERWRITE_CHUNK_2NX};
    hsize_t     dset_max_dims[] = {H5S_UNLIMITED, OVERWRITE_CHUNK_NY, OVERWRITE_CHUNK_2NX};
    hsize_t     chunk_dims[] = {1, OVERWRITE_CHUNK_NY, OVERWRITE_CHUNK_NX};
    hsize_t     offset[] = {0, 0, 0};
    hsize_t     i, j;
    int16_t     n;
    int16_t     read_buf[OVERWRITE_CHUNK_NY][OVERWRITE_CHUNK_2NX];

    TESTING("overwriting existing data with H5Dwrite_chunk");

    /* Create the dataset's data space */
    if ((sid = H5Screate_simple(OVERWRITE_NDIMS, dset_dims, dset_max_dims)) < 0)
        FAIL_STACK_ERROR

    /* Set chunk size and filll value */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if (H5Pset_fill_value(dcpl_id, tid, &fill_value) < 0)
        FAIL_STACK_ERROR
    if (H5Pset_chunk(dcpl_id, OVERWRITE_NDIMS, chunk_dims) < 0)
        FAIL_STACK_ERROR

    /* Create dataset */
    if ((did = H5Dcreate2(fid, DATASETNAME7, tid, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Initialize data buffers */
    n = 0;
    for (i = 0; i < OVERWRITE_CHUNK_NY; i++) {
        for (j = 0; j < OVERWRITE_CHUNK_NX; j++) {
            data_buf[i][j] = n++;
            overwrite_buf[i][j] = OVERWRITE_VALUE;
        }
    }

    /* Write chunk data using the direct write function. */
    if (H5Dwrite_chunk(did, H5P_DEFAULT, filter_mask, offset, buf_size, data_buf) < 0)
        FAIL_STACK_ERROR

    /* Write second chunk. */
    offset[2] = OVERWRITE_CHUNK_NX;
    if (H5Dwrite_chunk(did, H5P_DEFAULT, filter_mask, offset, buf_size, data_buf) < 0)
        FAIL_STACK_ERROR

    /* Overwrite first chunk. */
    offset[2] = 0;
    if (H5Dwrite_chunk(did, H5P_DEFAULT, filter_mask, offset, buf_size, overwrite_buf) < 0)
        FAIL_STACK_ERROR

    /* Read the data back out */
    if (H5Dread(did, tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_buf) < 0)
        FAIL_STACK_ERROR

    /* Ensure that the data are correct in chunk 1 */
    for (i = 0; i < OVERWRITE_CHUNK_NY; i++)
        for (j = 0; j < OVERWRITE_CHUNK_NX; j++) {
            if (read_buf[i][j] != OVERWRITE_VALUE)
                TEST_ERROR
        }

    if (H5Pclose(dcpl_id) < 0)
        FAIL_STACK_ERROR
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Sclose(sid);
        H5Dclose(did);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* end test_direct_chunk_overwrite_data() */

/*-------------------------------------------------------------------------
 * Function:    test_skip_compress_write1
 *
 * Purpose:    Test skipping compression filter when it is the only filter
 *              for the dataset
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static int
test_skip_compress_write1(hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         i, j, n;

    unsigned    filter_mask = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int         aggression = 9;     /* Compression aggression setting */

    unsigned    read_filter_mask = 0; /* filter mask after direct read */
    int         read_direct_buf[CHUNK_NX][CHUNK_NY];
    hsize_t     read_buf_size = 0; /* buf size */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("skipping compression filter for H5Dwrite_chunk/H5Dread_chunk");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /*
     * Modify dataset creation properties, i.e. enable chunking and compression
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    if((status = H5Pset_deflate( cparms, (unsigned ) aggression)) < 0)
        goto error;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME2, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Initialize data for one chunk */
    for(i = n = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++) {
            direct_buf[i][j] = n++;
    }

    /* write the uncompressed chunk data repeatedly to dataset, using the direct writing function.
     * Indicate skipping the compression filter.     */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    filter_mask = 0x00000001;

    if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
        goto error;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME2, H5P_DEFAULT)) < 0)
        goto error;

    /*
     * Select hyperslab for the chunk just written in the file
     */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        goto error;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != check_chunk[i][j]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[i][j], check_chunk[i][j]);
                goto error;
            }
        }
    }

    /* Query chunk storage size */
    if((status = H5Dget_chunk_storage_size(dataset, offset, &read_buf_size)) < 0)
        goto error;
    if(read_buf_size != buf_size)
        goto error;

    /* Read the raw chunk back */
    HDmemset(&read_direct_buf, 0, sizeof(read_direct_buf));
    if((status = H5Dread_chunk(dataset, H5P_DEFAULT, offset, &read_filter_mask, read_direct_buf)) < 0)
        goto error;
    if(read_filter_mask != filter_mask)
        goto error;

    /* Check that the direct chunk read is the same as the chunk written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != read_direct_buf[i][j]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    direct_buf=%d, read_direct_buf=%d\n", direct_buf[i][j], read_direct_buf[i][j]);
                goto error;
            }
        }
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_skip_compress_write1() */

/*-------------------------------------------------------------------------
 * Function:    filter_bogus1
 *
 * Purpose:        A bogus filter that adds ADD_ON to the original value
 *
 * Return:        Success:    Data chunk size
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus1(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr=(int *)*buf;          /* Pointer to the data values */
    ssize_t buf_left=(ssize_t)*buf_size;  /* Amount of data buffer left to process */

    if(flags & H5Z_FLAG_REVERSE) { /* read */
        /* Substract the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ -= (int)ADD_ON;
            buf_left -= (ssize_t)sizeof(int);
        } /* end while */
    } /* end if */
    else { /* write */
        /* Add the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ += (int)ADD_ON;
            buf_left -= (ssize_t)sizeof(int);
        } /* end while */
    } /* end else */

    return nbytes;
} /* filter_bogus1() */

/*-------------------------------------------------------------------------
 * Function:    filter_bogus2
 *
 * Purpose:    A bogus filter that multiplies the original value by FACTOR.
 *
 * Return:    Success:    Data chunk size
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus2(unsigned int flags, size_t H5_ATTR_UNUSED cd_nelmts,
      const unsigned int H5_ATTR_UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr=(int *)*buf;          /* Pointer to the data values */
    ssize_t buf_left=(ssize_t)*buf_size;  /* Amount of data buffer left to process */

    if(flags & H5Z_FLAG_REVERSE) { /* read */
        /* Substract the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ /= (int)FACTOR;
            buf_left -= (ssize_t)sizeof(int);
        } /* end while */
    } /* end if */
    else { /* write */
        /* Add the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ *= (int)FACTOR;
            buf_left -= (ssize_t)sizeof(int);
        } /* end while */
    } /* end else */

    return nbytes;
} /* filter_bogus2() */

/*-------------------------------------------------------------------------
 * Function:    test_skip_compress_write2
 *
 * Purpose:    Test skipping compression filter when there are three filters
 *              for the dataset
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static int
test_skip_compress_write2(hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         i, j, n;

    unsigned    filter_mask = 0;    /* orig filter mask */
    int         origin_direct_buf[CHUNK_NX][CHUNK_NY];
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int         aggression = 9;     /* Compression aggression setting */

    unsigned    read_filter_mask = 0;   /* filter mask after direct read */
    int         read_direct_buf[CHUNK_NX][CHUNK_NY];
    hsize_t     read_buf_size = 0;  /* buf size */

    hsize_t start[2];   /* Start of hyperslab */
    hsize_t stride[2];  /* Stride of hyperslab */
    hsize_t count[2];   /* Block count */
    hsize_t block[2];   /* Block sizes */

    TESTING("skipping compression filters but keep two other filters");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /*
     * Modify dataset creation properties, i.e. enable chunking and compression.
     * The order of filters is bogus 1 + deflate + bogus 2.
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    /* Register and enable first bogus filter */
    if(H5Zregister (H5Z_BOGUS1) < 0)
        goto error;

    if(H5Pset_filter(cparms, H5Z_FILTER_BOGUS1, 0, (size_t)0, NULL) < 0)
        goto error;

    /* Enable compression filter */
    if((status = H5Pset_deflate( cparms, (unsigned) aggression)) < 0)
        goto error;

    /* Register and enable second bogus filter */
    if(H5Zregister (H5Z_BOGUS2) < 0)
        goto error;

    if(H5Pset_filter(cparms, H5Z_FILTER_BOGUS2, 0, (size_t)0, NULL) < 0)
        goto error;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME3, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Initialize data for one chunk. Apply operations of two bogus filters to the chunk */
    for(i = n = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++) {
            origin_direct_buf[i][j] = n++;
            direct_buf[i][j] = (origin_direct_buf[i][j] + ADD_ON) * FACTOR;
        }

    /* write the uncompressed chunk data repeatedly to dataset, using the direct writing function.
     * Indicate skipping the compression filter but keep the other two bogus filters */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    /* compression filter is the middle one to be skipped */
    filter_mask = 0x00000002;

    if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
        goto error;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME3, H5P_DEFAULT)) < 0)
        goto error;

    /*
     * Select hyperslab for one chunk in the file
     */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        goto error;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(origin_direct_buf[i][j] != check_chunk[i][j]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    origin_direct_buf=%d, check_chunk=%d\n", origin_direct_buf[i][j], check_chunk[i][j]);
                goto error;
            }
        }
    }

    /* Query chunk storage size */
    if((status = H5Dget_chunk_storage_size(dataset, offset, &read_buf_size)) < 0)
        goto error;
    if(read_buf_size != buf_size)
        goto error;

    /* Read the raw chunk back */
    HDmemset(&read_direct_buf, 0, sizeof(read_direct_buf));
    if((status = H5Dread_chunk(dataset, H5P_DEFAULT, offset, &read_filter_mask, read_direct_buf)) < 0)
        goto error;
    if(read_filter_mask != filter_mask)
        goto error;

    /* Check that the direct chunk read is the same as the chunk written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != read_direct_buf[i][j]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", i, j);
                HDprintf("    direct_buf=%d, read_direct_buf=%d\n", direct_buf[i][j], read_direct_buf[i][j]);
                goto error;
            }
        }
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_skip_compress_write2() */

/*-------------------------------------------------------------------------
 * Function:    test_data_conv
 *
 * Purpose:    Test data conversion
 *
 * Return:    Success:    0
 *            Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static int
test_data_conv(hid_t file)
{
    typedef struct {
        int a, b, c[4], d, e;
    } src_type_t;
    typedef struct {
        int a,    c[4],    e;
    } dst_type_t;

    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         i, j, n;
    const hsize_t    four = 4;
    hid_t    st=-1, dt=-1;
    hid_t       array_dt;

    unsigned    filter_mask = 0;
    src_type_t  direct_buf[CHUNK_NX][CHUNK_NY];
    dst_type_t  check_chunk[CHUNK_NX][CHUNK_NY];
    src_type_t  read_chunk[CHUNK_NX][CHUNK_NY];       /* For H5Dread_chunk */

    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(src_type_t);

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("data conversion for H5Dwrite_chunk/H5Dread_chunk");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /*
     * Modify dataset creation properties, i.e. enable chunking
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    /* Build hdf5 datatypes */
    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if((st = H5Tcreate(H5T_COMPOUND, sizeof(src_type_t))) < 0 ||
            H5Tinsert(st, "a", HOFFSET(src_type_t, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(st, "b", HOFFSET(src_type_t, b), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(st, "c", HOFFSET(src_type_t, c), array_dt) < 0 ||
            H5Tinsert(st, "d", HOFFSET(src_type_t, d), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(st, "e", HOFFSET(src_type_t, e), H5T_NATIVE_INT) < 0)
        goto error;

    if(H5Tclose(array_dt) < 0)
        goto error;

    array_dt = H5Tarray_create2(H5T_NATIVE_INT, 1, &four);
    if((dt = H5Tcreate(H5T_COMPOUND, sizeof(dst_type_t))) < 0 ||
            H5Tinsert(dt, "a", HOFFSET(dst_type_t, a), H5T_NATIVE_INT) < 0 ||
            H5Tinsert(dt, "c", HOFFSET(dst_type_t, c), array_dt) < 0 ||
            H5Tinsert(dt, "e", HOFFSET(dst_type_t, e), H5T_NATIVE_INT) < 0)
        goto error;

    if(H5Tclose(array_dt) < 0)
        goto error;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME4, st, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Initialize data for one chunk */
    for(i = n = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            (direct_buf[i][j]).a    = i*j+0;
            (direct_buf[i][j]).b    = i*j+1;
            (direct_buf[i][j]).c[0] = i*j+2;
            (direct_buf[i][j]).c[1] = i*j+3;
            (direct_buf[i][j]).c[2] = i*j+4;
            (direct_buf[i][j]).c[3] = i*j+5;
            (direct_buf[i][j]).d    = i*j+6;
            (direct_buf[i][j]).e    = i*j+7;
        }
    }

    /* write the chunk data to dataset, using the direct writing function.
     * There should be no data conversion involved. */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
        goto error;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME4, H5P_DEFAULT)) < 0)
        goto error;

    /* Use H5Dread_chunk() to read the uncompressed data */
    if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, read_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if ((direct_buf[i][j]).a    != (read_chunk[i][j]).a    ||
                (direct_buf[i][j]).b    != (read_chunk[i][j]).b    ||
                (direct_buf[i][j]).c[0] != (read_chunk[i][j]).c[0] ||
                (direct_buf[i][j]).c[1] != (read_chunk[i][j]).c[1] ||
                (direct_buf[i][j]).c[2] != (read_chunk[i][j]).c[2] ||
                (direct_buf[i][j]).c[3] != (read_chunk[i][j]).c[3] ||
                (direct_buf[i][j]).d    != (read_chunk[i][j]).d    ||
                (direct_buf[i][j]).e    != (read_chunk[i][j]).e) {
                    HDprintf("    1. Read different values than written.");
                    HDprintf("    At index %d,%d\n", i, j);
                    HDprintf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
                        (direct_buf[i][j]).a, (direct_buf[i][j]).b, (direct_buf[i][j]).c[0], (direct_buf[i][j]).c[1],
                        (direct_buf[i][j]).c[2], (direct_buf[i][j]).c[3], (direct_buf[i][j]).d, (direct_buf[i][j]).e);
                    HDprintf("    dst={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
                        (read_chunk[i][j]).a, (read_chunk[i][j]).b, (read_chunk[i][j]).c[0], (read_chunk[i][j]).c[1],
                        (read_chunk[i][j]).c[2], (read_chunk[i][j]).c[3], (read_chunk[i][j]).d, (read_chunk[i][j]).e);

                    goto error;
            }
        }
    }

    /*
     * Select hyperslab for the chunk just written in the file
     */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        goto error;

    /* Read the chunk back. Data should be converted */
    if((status = H5Dread(dataset, dt, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if ((direct_buf[i][j]).a    != (check_chunk[i][j]).a    ||
                (direct_buf[i][j]).c[0] != (check_chunk[i][j]).c[0] ||
                (direct_buf[i][j]).c[1] != (check_chunk[i][j]).c[1] ||
                (direct_buf[i][j]).c[2] != (check_chunk[i][j]).c[2] ||
                (direct_buf[i][j]).c[3] != (check_chunk[i][j]).c[3] ||
                (direct_buf[i][j]).e    != (check_chunk[i][j]).e) {
                    HDprintf("    1. Read different values than written.");
                    HDprintf("    At index %d,%d\n", i, j);
                    HDprintf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
                        (direct_buf[i][j]).a, (direct_buf[i][j]).b, (direct_buf[i][j]).c[0], (direct_buf[i][j]).c[1],
                        (direct_buf[i][j]).c[2], (direct_buf[i][j]).c[3], (direct_buf[i][j]).d, (direct_buf[i][j]).e);
                    HDprintf("    dst={a=%d, c=[%d,%d,%d,%d], e=%d\n",
                        (check_chunk[i][j]).a, (check_chunk[i][j]).c[0], (check_chunk[i][j]).c[1], (check_chunk[i][j]).c[2],
                        (check_chunk[i][j]).c[3], (check_chunk[i][j]).e);

                    goto error;
            }
        }
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);
    H5Tclose(st);
    H5Tclose(dt);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Tclose(st);
        H5Tclose(dt);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_data_conv() */

/*-------------------------------------------------------------------------
 * Function:    test_invalid_parameters
 *
 * Purpose:     Test invalid parameters for H5Dwrite_chunk and H5Dread_chunk
 *
 * Return:      Success:    0
 *              Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static int
test_invalid_parameters(hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         i, j, n;

    unsigned    filter_mask = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int         aggression = 9;     /* Compression aggression setting */

    hsize_t     chunk_nbytes;       /* Chunk size */

    TESTING("invalid parameters for H5Dwrite_chunk/H5Dread_chunk");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        goto error;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /*
     * Modify dataset creation properties
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;

    /*
     * Create a new contiguous dataset to verify H5Dwrite_chunk/H5Dread_chunk doesn't work
     */
    if((dataset = H5Dcreate2(file, DATASETNAME5, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Initialize data for one chunk */
    for(i = n = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++) {
            direct_buf[i][j] = n++;
        }

    /* Try to write the chunk data to contiguous dataset.  It should fail */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;

    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Try to get chunk size for a contiguous dataset.  It should fail */
    H5E_BEGIN_TRY {
        if((status = H5Dget_chunk_storage_size(dataset, offset, &chunk_nbytes)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Try to H5Dread_chunk from the contiguous dataset.  It should fail */
    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    if(H5Dclose(dataset) < 0)
        goto error;


    /* Create a chunked dataset with compression filter */
    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    if((status = H5Pset_deflate( cparms, (unsigned ) aggression)) < 0)
        goto error;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME6, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    /* Check invalid dataset ID for H5Dwrite_chunk and H5Dread_chunk */
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk((hid_t)-1, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk((hid_t)-1, dxpl, offset, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid DXPL ID for H5Dwrite_chunk and H5Dread_chunk */
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, (hid_t)-1, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, (hid_t)-1, offset, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid OFFSET for H5Dwrite_chunk and H5Dread_chunk */
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, NULL, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, dxpl, NULL, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check when OFFSET is out of dataset range for H5Dwrite_chunk and H5Dread_chunk */
    offset[0] = NX + 1;
    offset[1] = NY;
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check when OFFSET is not on chunk boundary for H5Dwrite_chunk and H5Dread_chunk */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY + 1;
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid buffer size for H5Dwrite_chunk only */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    buf_size = 0;
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid data buffer for H5Dwrite_chunk and H5Dread_chunk */
    buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    H5E_BEGIN_TRY {
        if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, NULL)) != FAIL)
            goto error;
    } H5E_END_TRY;

    H5E_BEGIN_TRY {
        if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, NULL)) != FAIL)
            goto error;
    } H5E_END_TRY;

    if(H5Dclose(dataset) < 0)
        goto error;

    /*
     * Close/release resources.
     */
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_invalid_parameters() */

/*-------------------------------------------------------------------------
 * Function:    test_direct_chunk_read_no_cache
 *
 * Purpose:     Test the basic functionality of H5Dread_chunk with the
 *              chunk cache diabled.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Matthew Strong (GE Healthcare)
 *              14 February 2016
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_DEFLATE
static int
test_direct_chunk_read_no_cache (hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1, dapl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] = {CHUNK_NX, CHUNK_NY};
    herr_t      status;             /* status from H5 function calls */
    int         ret;                /* deflate return status */
    int         data[NX][NY];
    int         i, j, k, l, n;      /* local index variables */

    unsigned    filter_mask = 0;    /* filter mask returned from H5Dread_chunk */
    int         direct_buf[CHUNK_NX][CHUNK_NY];  /* chunk read with H5Dread and manually decompressed */
    int         check_chunk[CHUNK_NX][CHUNK_NY]; /* chunk read with H5Dread */
    hsize_t     offset[2]; /* chunk offset used for H5Dread_chunk */
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);

    Bytef       *z_src = NULL;      /* source buffer        */
    uLongf       z_src_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    Bytef       *z_dst = (Bytef*)(direct_buf);
    uLong        z_dst_nbytes = (uLong)buf_size;
    int          aggression = 9;     /* Compression aggression setting */
    void        *outbuf = NULL;      /* Pointer to new buffer */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("basic functionality of H5Dread_chunk (chunk cache disabled)");

    /* Create the data space with unlimited dimensions. */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking and compression */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;
    if((status = H5Pset_deflate( cparms, (unsigned) aggression)) < 0)
        goto error;
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        goto error;

    /* Disable chunk cache by setting number of slots to 0 */
    if((status = H5Pset_chunk_cache(dapl, 0, H5D_CHUNK_CACHE_NBYTES_DEFAULT, H5D_CHUNK_CACHE_W0_DEFAULT)) < 0)
        goto error;

    /* Create a new dataset within the file using cparms creation properties. */
    if((dataset = H5Dcreate2(file, DATASETNAME8, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, dapl)) < 0)
        goto error;

    /* Initialize the dataset */
    for(i = n = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
            data[i][j] = n++;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Write the data for the dataset.
     * Data will skip chunk cache and go directly to disk. */
    if((status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
              dxpl, data)) < 0)
        goto error;

    /* Allocate output (compressed) buffer */
    outbuf = HDmalloc(z_src_nbytes);
    z_src = (Bytef *)outbuf;

    /* For each chunk in the dataset, compare the result of H5Dread and H5Dread_chunk. */
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            /* Select hyperslab for one chunk in the file */
            start[0]  = (hsize_t)i * CHUNK_NX; start[1]  = (hsize_t)j * CHUNK_NY;
            stride[0] = 1; stride[1] = 1;
            count[0]  = 1; count[1]  = 1;
            block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;

            /* Hyperslab selection equals single chunk */
            if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
                goto error;

            /* Read the chunk back */
            if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
                goto error;

            offset[0] = (hsize_t)i * CHUNK_NX; offset[1] = (hsize_t)j * CHUNK_NY;
            /* Read the compressed chunk back using the direct read function. */
            if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, outbuf)) < 0)
                goto error;

            /* Check filter mask return value */
            if(filter_mask != 0)
                goto error;

            /* Perform decompression from the source to the destination buffer */
            ret = uncompress(z_dst, &z_dst_nbytes, z_src, z_src_nbytes);

            /* Check for various zlib errors */
            if(Z_BUF_ERROR == ret) {
                HDfprintf(stderr, "overflow\n");
                goto error;
            } else if(Z_MEM_ERROR == ret) {
                HDfprintf(stderr, "deflate memory error\n");
                goto error;
            } else if(Z_DATA_ERROR == ret) {
                HDfprintf(stderr, "corrupted data\n");
                goto error;
            } else if(Z_OK != ret) {
                HDfprintf(stderr, "other deflate error\n");
                goto error;
            }

            /* Check that the decompressed values match those read from H5Dread */
            for(k = 0; k < CHUNK_NX; k++) {
                for(l = 0; l < CHUNK_NY; l++) {
                    if(direct_buf[k][l] != check_chunk[k][l]) {
                        HDprintf("\n    1. Read different values than written.");
                        HDprintf("    At index %d,%d\n", k, l);
                        HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[k][l], check_chunk[k][l]);
                        goto error;
                    }
                }
            }
        }
    }

    /* Close/release resources. */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);
    H5Pclose(dapl);

    if(outbuf)
        HDfree(outbuf);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Pclose(dapl);
    } H5E_END_TRY;

    if(outbuf)
        HDfree(outbuf);

    H5_FAILED();
    return 1;
} /* test_direct_chunk_read_no_cache() */
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_DEFLATE
static int
test_direct_chunk_read_cache (hid_t file, hbool_t flush)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] = {CHUNK_NX, CHUNK_NY};
    herr_t      status;             /* status from H5 function calls */
    int         ret;                /* deflate return status */
    int         data[NX][NY];
    int         i, j, k, l, n;      /* local index variables */

    unsigned    filter_mask = 0;    /* filter mask returned from H5Dread_chunk */
    int         direct_buf[CHUNK_NX][CHUNK_NY];  /* chunk read with H5Dread and manually decompressed */
    int         check_chunk[CHUNK_NX][CHUNK_NY]; /* chunk read with H5Dread */
    hsize_t     offset[2]; /* chunk offset used for H5Dread_chunk */
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);

    Bytef       *z_src = NULL;      /* source buffer        */
    uLongf       z_src_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    Bytef       *z_dst = (Bytef*)(direct_buf);
    uLong        z_dst_nbytes = (uLong)buf_size;
    int          aggression = 9;     /* Compression aggression setting */
    void        *outbuf = NULL;      /* Pointer to new buffer */
    hsize_t     read_buf_size = 0;

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    if(flush) {
        TESTING("basic functionality of H5Dread_chunk (flush chunk cache)");
    } else {
        TESTING("basic functionality of H5Dread_chunk (does not flush chunk cache)");
    }

    /* Create the data space with unlimited dimensions. */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking and compression */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;
    if((status = H5Pset_deflate( cparms, (unsigned) aggression)) < 0)
        goto error;

    /* Create a new dataset within the file using cparms creation properties. */
    if((dataset = H5Dcreate2(file, flush?DATASETNAME9:DATASETNAME10, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    /* Initialize the dataset */
    for(i = n = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
            data[i][j] = n++;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Write the data for the dataset.
     * It should stay in the chunk cache. */
    if((status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
              dxpl, data)) < 0)
        goto error;

    if(flush) {
        /* Flush the chunk cache to disk. Cache entry is not evicted. */
        if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
            goto error;
    }

    /* Allocate output (compressed) buffer */
    outbuf = HDmalloc(z_src_nbytes);
    z_src = (Bytef *)outbuf;

    /* For each chunk in the dataset, compare the result of H5Dread and H5Dread_chunk. */
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            /* Select hyperslab for one chunk in the file */
            start[0]  = (hsize_t)i * CHUNK_NX; start[1]  = (hsize_t)j * CHUNK_NY;
            stride[0] = 1; stride[1] = 1;
            count[0]  = 1; count[1]  = 1;
            block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;

            /* Hyperslab selection equals single chunk */
            if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
                goto error;

            /* Read the chunk back */
            if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
                goto error;

            offset[0] = (hsize_t)i * CHUNK_NX; offset[1] = (hsize_t)j * CHUNK_NY;

            /* Query chunk storage size */
            if((status = H5Dget_chunk_storage_size(dataset, offset, &read_buf_size)) < 0)
                goto error;
            if(read_buf_size == 0)
                goto error;

            /* Read the compressed chunk back using the direct read function. */
            if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, outbuf)) < 0)
                goto error;

            /* Check filter mask return value */
            if(filter_mask != 0)
                goto error;

            /* Perform decompression from the source to the destination buffer */
            ret = uncompress(z_dst, &z_dst_nbytes, z_src, z_src_nbytes);

            /* Check for various zlib errors */
            if(Z_BUF_ERROR == ret) {
                HDfprintf(stderr, "overflow\n");
                goto error;
            } else if(Z_MEM_ERROR == ret) {
                HDfprintf(stderr, "deflate memory error\n");
                goto error;
            } else if(Z_DATA_ERROR == ret) {
                HDfprintf(stderr, "corrupted data\n");
                goto error;
            } else if(Z_OK != ret) {
                HDfprintf(stderr, "other deflate error\n");
                goto error;
            }

            /* Check that the decompressed values match those read from H5Dread */
            for(k = 0; k < CHUNK_NX; k++) {
                for(l = 0; l < CHUNK_NY; l++) {
                    if(direct_buf[k][l] != check_chunk[k][l]) {
                        HDprintf("\n    1. Read different values than written.");
                        HDprintf("    At index %d,%d\n", k, l);
                        HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[k][l], check_chunk[k][l]);
                        goto error;
                    }
                }
            }
        }
    }

    /* Close/release resources. */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    if(outbuf)
        HDfree(outbuf);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    if(outbuf)
        HDfree(outbuf);

    H5_FAILED();
    return 1;
} /* test_direct_chunk_read_cache() */
#endif /* H5_HAVE_FILTER_DEFLATE */

/*-------------------------------------------------------------------------
 * Function:    test_read_unfiltered_dset
 *
 * Purpose:     Test the basic functionality of H5Dread_chunk on a dataset
 *              without no filters applied.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Matthew Strong (GE Healthcare)
 *              30 November 2016
 *
 *-------------------------------------------------------------------------
 */
static int
test_read_unfiltered_dset(hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         data[NX][NY];
    int         i, j, k, l, n;

    unsigned    filter_mask = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY]; /* chunk read with H5Dread */
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    hsize_t     read_buf_size = 0;

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("basic functionality of H5Dread_chunk on unfiltered datasets");

    /* Create the data space with unlimited dimensions. */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking, no compression */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    /* Create a new dataset within the file using cparms creation properties. */
    if((dataset = H5Dcreate2(file, DATASETNAME12, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    /* Initialize the dataset */
    for(i = n = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
            data[i][j] = n++;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Write the data for the dataset.
     * It should stay in the chunk cache and will be evicted/flushed by
     * the H5Dread_chunk function call. */
    if((status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
              dxpl, data)) < 0)
        goto error;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    /* For each chunk in the dataset, compare the result of H5Dread and H5Dread_chunk. */
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            /* Select hyperslab for one chunk in the file */
            start[0]  = (hsize_t)i * CHUNK_NX; start[1]  = (hsize_t)j * CHUNK_NY;
            stride[0] = 1; stride[1] = 1;
            count[0]  = 1; count[1]  = 1;
            block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;

            /* Hyperslab selection equals single chunk */
            if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
                goto error;

            /* Read the chunk back */
            if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
                goto error;

            /* Query chunk storage size */
            if((status = H5Dget_chunk_storage_size(dataset, offset, &read_buf_size)) < 0)
                goto error;

            if(read_buf_size != buf_size )
                goto error;

            offset[0] = (hsize_t)i * CHUNK_NX; offset[1] = (hsize_t)j * CHUNK_NY;
            /* Read the raw chunk back */
            HDmemset(&direct_buf, 0, sizeof(direct_buf));
            filter_mask = UINT_MAX;
            if((status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, direct_buf)) < 0)
                goto error;

            /* Check filter mask return value */
            if(filter_mask != 0)
                goto error;

            /* Check that the decompressed values match those read from H5Dread */
            for(k = 0; k < CHUNK_NX; k++) {
                for(l = 0; l < CHUNK_NY; l++) {
                    if(direct_buf[k][l] != check_chunk[k][l]) {
                        HDprintf("\n    1. Read different values than written.");
                        HDprintf("    At index %d,%d\n", k, l);
                        HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[k][l], check_chunk[k][l]);
                        goto error;
                    }
                }
            }
        }
    }

    /* Close/release resources. */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_read_unfiltered_dset() */

/*-------------------------------------------------------------------------
 * Function:    test_read_unallocated_chunk
 *
 * Purpose:     Tests the H5Dread_chunk and H5Dget_chunk_storage_size with valid
 *              offets to chunks that have not been written to the dataset and are
 *              not allocated in the chunk storage on disk.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Matthew Strong (GE Healthcare)
 *              30 November 2016
 *
 *-------------------------------------------------------------------------
 */
static int
test_read_unallocated_chunk (hid_t file)
{
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] = {CHUNK_NX, CHUNK_NY};
    hsize_t     chunk_nbytes = CHUNK_NX*CHUNK_NY*sizeof(int);
    hsize_t     direct_chunk_nbytes = 0;        /* size (bytes) of the on-disk chunk */
    herr_t      status;     /* status from H5 function calls */
    hsize_t     i, j;       /* local index variables */

    unsigned    filter_mask = 0;    /* filter mask returned from H5Dread_chunk */
    int         direct_buf[CHUNK_NX][CHUNK_NY]; /* chunk read with H5Dread and manually decompressed */
    hsize_t     offset[2];  /* chunk offset used for H5Dread_chunk */

    TESTING("H5Dread_chunk with unallocated chunks");

    /* Create the data space with unlimited dimensions. */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        goto error;
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        goto error;

    /* Modify dataset creation properties, i.e. enable chunking, no compression */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        goto error;

    /* Create a new dataset within the file using cparms creation properties. */
    if((dataset = H5Dcreate2(file, DATASETNAME11, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
            cparms, H5P_DEFAULT)) < 0)
        goto error;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        goto error;

    /* Write a single chunk to intialize the chunk storage */
    HDmemset(direct_buf, 0, CHUNK_NX * CHUNK_NY * sizeof(int));
    offset[0] = 0; offset[1] = 0;

    if(H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, chunk_nbytes, direct_buf) < 0)
        goto error;

    /* Attempt to read each chunk in the dataset. Chunks are not allocated,
     * therefore we expect the result of H5Dread_chunk to fail. Chunk idx starts
     * at 1, since one chunk was written to init the chunk storage. */
    for(i=1; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {

            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;

            /* Read a non-existant chunk using the direct read function. */
            H5E_BEGIN_TRY {
                status = H5Dread_chunk(dataset, dxpl, offset, &filter_mask, &direct_buf);
            } H5E_END_TRY;

            /* Check that the chunk read call does not succeed. */
            if(status != -1)
                goto error;

            /* Query the size of the non-existant chunk */
            direct_chunk_nbytes = ULONG_MAX;
            H5E_BEGIN_TRY {
                status = H5Dget_chunk_storage_size(dataset, offset, &direct_chunk_nbytes);
            } H5E_END_TRY;

            /* Check that the chunk storage size call does not succeed. */
            if(status != -1 )
                goto error;
            if(direct_chunk_nbytes != 0 )
                goto error;

        }
    }

    /* Close/release resources. */
    H5Dclose(dataset);
    H5Sclose(mem_space);
    H5Sclose(dataspace);
    H5Pclose(cparms);
    H5Pclose(dxpl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_read_unallocated_chunk() */

/*-------------------------------------------------------------------------
 * Function:    test_single_chunk
 *
 * Purpose:     This is to verify the fix for jira issue HDFFV-10425.
 *              The problem was due to a bug in the internal ilbrary routine
 *              H5D__chunk_direct_write() which passed a null dataset
 *              pointer to the insert callback for the chunk index type.
 *              Currently, the single chunk index is the only one that
 *              used the dataset pointer in the insert callback.
 *
 *              This routine is based on the test program attached to
 *              this jira issue:
 *                  Create a file with the latest format and a chunked dataset
 *                  with one single chunk.  The library will use single chunk
 *                  index for the dataset.
 *                  Verify that the data read is the same as the written data.
 *
 *              Since expanded to test multiple combinations of cases
 *              involving a single chunk
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 *-------------------------------------------------------------------------
 */
static int
test_single_chunk(unsigned config)
{
    hid_t fid = H5I_INVALID_HID;            /* File ID */
    hid_t fapl = H5I_INVALID_HID;           /* File access property list ID */
    hid_t sid = H5I_INVALID_HID;            /* Dataspace ID */
    hid_t did = H5I_INVALID_HID;            /* Dataset ID */
    hid_t dcpl = H5I_INVALID_HID;           /* Dataset creation property list */
    hsize_t dims[2] = {DIM0, DIM1};         /* Dimension sizes */
    hsize_t chunk[2] = {CHUNK0, CHUNK1};    /* Chunk dimension sizes */
    hsize_t offset[2] = {0,0};              /* Offset for writing */
    uint32_t filters;       /* Filter mask out */
    int wdata[DIM0][DIM1];  /* Write buffer */
    int rdata[DIM0][DIM1];  /* Read buffer */
    int i, j;               /* Local index variable */

    TESTING("Single chunk I/O");

    /* Initialize data */
    for (i=0; i<DIM0; i++) {
      for (j=0; j< DIM1; j++)
        wdata[i][j] = j/CHUNK0;
    }

    /* Create a new file with the latest format  */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        goto error;
    if(config & CONFIG_LATEST)
        if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            goto error;
    if((fid = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        goto error;

    /* Create dataspace */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0)
        goto error;

    /* Create the dataset creation property list and set the chunk size */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if(H5Pset_chunk(dcpl, 2, chunk) < 0)
        goto error;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        goto error;

    if(config & CONFIG_DIRECT_WRITE) {
        /* Write the data directly to the dataset */
        if(H5Dwrite_chunk(did, H5P_DEFAULT, 0, offset, CHUNK0*CHUNK1*4, (void *)wdata) < 0)
            goto error;
    } /* end if */
    else
        /* Write the data to the dataset */
        if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, (void *)wdata) < 0)
            goto error;

    /*
     * Close and release resources.
     */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(config & CONFIG_REOPEN_DSET)
        if(H5Dclose(did) < 0)
            goto error;
    if(H5Sclose(sid) < 0)
        goto error;
    if(H5Pclose(fapl) < 0)
        goto error;
    if(config & CONFIG_REOPEN_FILE)
        if(H5Fclose(fid) < 0)
            goto error;

    /* Open the file and dataset with default properties  */
    if(config & CONFIG_REOPEN_FILE)
        if((fid = H5Fopen(FILE, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            goto error;
    if(config & CONFIG_REOPEN_DSET)
        if((did = H5Dopen2(fid, DATASET, H5P_DEFAULT)) < 0)
            goto error;

    /* Retrieve dataset creation property list */
    if((dcpl = H5Dget_create_plist(did)) < 0)
        goto error;

    if(config & CONFIG_DIRECT_READ) {
        /* Read the data directly */
        if(H5Dread_chunk(did, H5P_DEFAULT, offset, &filters, rdata) < 0)
            goto error;

        /* Verify returned filter mask */
        if(filters != 0)
            goto error;
    } /* end if */
    else
        /* Read the data */
        if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0)
            goto error;

    /* Verify that the data read was correct.  */
    for (i = 0; i < DIM0; i++) {
        for (j = 0; j < DIM1; j++) {
            if(rdata[i][j] != wdata[i][j])
                goto error;
        }
    }

    /*
     * Close and release resources
     */
    if(H5Pclose(dcpl) < 0)
        goto error;
    if(H5Dclose(did) < 0)
        goto error;
    if(H5Fclose(fid) < 0)
        goto error;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_single_chunk_latest() */

/*-------------------------------------------------------------------------
 * Function:    Main function
 *
 * Purpose:        Test direct chunk write function H5Dwrite_chunk and
 *              chunk direct read function H5Dread_chunk
 *
 * Return:        Success:    0
 *                Failure:    1
 *
 * Programmer:  Raymond Lu
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
int main( void )
{
    hid_t file_id;
    unsigned config;
    int   nerrors=0;

    /*
     * Create a new file. If file exists its contents will be overwritten.
     */
    if((file_id = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Test direct chunk write and direct chunk read */
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += test_direct_chunk_write(file_id);
#endif /* H5_HAVE_FILTER_DEFLATE */
    nerrors += test_direct_chunk_overwrite_data(file_id);
    nerrors += test_skip_compress_write1(file_id);
    nerrors += test_skip_compress_write2(file_id);
    nerrors += test_data_conv(file_id);
    nerrors += test_invalid_parameters(file_id);

    /* Test direct chunk read */
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += test_direct_chunk_read_no_cache(file_id);
    nerrors += test_direct_chunk_read_cache(file_id, TRUE);
    nerrors += test_direct_chunk_read_cache(file_id, FALSE);
#endif /* H5_HAVE_FILTER_DEFLATE */
    nerrors += test_read_unfiltered_dset(file_id);
    nerrors += test_read_unallocated_chunk(file_id);

    /* Loop over test configurations */
    for(config = 0; config < CONFIG_END; config++) {
        hbool_t need_comma = FALSE;

        /* Check for invalid combinations */
        if((config & CONFIG_REOPEN_FILE) && !(config & CONFIG_REOPEN_DSET))
            continue;

        /* Print configuration */
        HDprintf("Configuration: ");
        if(config == 0)
            HDprintf("<empty>");
        if(config & CONFIG_LATEST) {
            if(need_comma)
                HDprintf(", ");
            HDprintf("latest format");
            need_comma = TRUE;
        } /* end if */
        if(config & CONFIG_REOPEN_FILE) {
            if(need_comma)
                HDprintf(", ");
            HDprintf("reopen file");
            need_comma = TRUE;
        } /* end if */
        else if(config & CONFIG_REOPEN_DSET) {
            if(need_comma)
                HDprintf(", ");
            HDprintf("reopen dataset");
            need_comma = TRUE;
        } /* end if */
        if(config & CONFIG_DIRECT_WRITE) {
            if(need_comma)
                HDprintf(", ");
            HDprintf("direct write");
            need_comma = TRUE;
        } /* end if */
        if(config & CONFIG_DIRECT_READ) {
            if(need_comma)
                HDprintf(", ");
            HDprintf("direct read");
            need_comma = TRUE;
        } /* end if */
        HDprintf(":\n");
        fflush(stdout);

        nerrors += test_single_chunk(config);
    } /* end for */

    if(H5Fclose(file_id) < 0)
        goto error;

    /* check for errors */
    if (nerrors)
        goto error;

    HDputs("All direct chunk read/write tests passed.");
    return EXIT_SUCCESS;

error:
    HDputs("*** TESTS FAILED ***");
    return EXIT_FAILURE;
}

