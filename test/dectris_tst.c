/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This test is for the DECTRIS project to the H5PSIdirect_write function
 *
 */

#include "h5test.h"
#include <zlib.h>
#include <math.h>
#include <stdlib.h>

const char *FILENAME[] = {
    "dectris",
    NULL
};

#define DATASETNAME        "Array"
#define RANK         2
#define NX     16
#define NY     16
#define CHUNK_NX     4
#define CHUNK_NY     4

#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s))*1.001)+12)

int
main (void)
{
    char        filename[1024];
    hid_t       file;                          /* handles */
    hid_t       fapl;
    hid_t       dataspace, dataset;
    hid_t       mem_space;
    hid_t       cparms, dxpl;
    hsize_t     dims[2]  = {NX, NY};        
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    herr_t      status;
    int         ret;
    int         data[NX][NY];
    int         check[NX][NY];
    int         i, j, n;

    unsigned    filter_mask = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);

    const Bytef *z_src = (const Bytef*)(direct_buf);
    Bytef	    *z_dst;		/*destination buffer		*/
    uLongf	     z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    uLong	     z_src_nbytes = (uLong)buf_size;
    int          aggression = 9;     /* Compression aggression setting */
    void	*outbuf = NULL;         /* Pointer to new buffer */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("H5PSIdirect_write for DECTRIS project");

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR;

    /*
     * Create a new file. If file exists its contents will be overwritten.
     */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Modify dataset creation properties, i.e. enable chunking and compression
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        TEST_ERROR;

    if((status = H5Pset_deflate( cparms, aggression)) < 0)
        TEST_ERROR;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Initialize the dataset */
    for(i = n = 0; i < NX; i++)
        for(j = 0; j < NY; j++)
	    data[i][j] = n++;

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /*
     * Write the data for the dataset.  It should stay in the chunk cache.
     * It will be evicted from the cache by the H5PSIdirect_write calls. 
     */
    if((status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, data)) < 0)
        TEST_ERROR;

    /* Initialize data for one chunk */
    for(i = n = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++)
	    direct_buf[i][j] = n++;

    /* Allocate output (compressed) buffer */
    outbuf = malloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        fprintf(stderr, "overflow");
        TEST_ERROR;
    } else if(Z_MEM_ERROR == ret) {
	fprintf(stderr, "deflate memory error");
        TEST_ERROR;
    } else if(Z_OK != ret) {
	fprintf(stderr, "other deflate error");
        TEST_ERROR;
    }

    /* Write the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5PSIdirect_write(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
            offset[1] += CHUNK_NY;
        }
        offset[0] += CHUNK_NX;
        offset[1] = 0;
    }

    if(outbuf)
        free(outbuf);

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0) 
        TEST_ERROR;

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /*
     * Select hyperslab for one chunk in the file
     */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        TEST_ERROR;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != check_chunk[i][j]) {
                printf("    1. Read different values than written.");
                printf("    At index %d,%d\n", i, j);
                printf("    direct_buf=%d, check_chunk=%d\n", direct_buf[i][j], check_chunk[i][j]); 
                goto error;
            }
        }
    }

    /* Reinitialize different data for one chunk */
    for(i = 0; i < CHUNK_NX; i++)
        for(j = 0; j < CHUNK_NY; j++)
	    direct_buf[i][j] = i + j;

    /* Allocate output (compressed) buffer */
    outbuf = malloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        fprintf(stderr, "overflow");
        TEST_ERROR;
    } else if(Z_MEM_ERROR == ret) {
	fprintf(stderr, "deflate memory error");
        TEST_ERROR;
    } else if(Z_OK != ret) {
	fprintf(stderr, "other deflate error");
        TEST_ERROR;
    }

    /* Rewrite the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    offset[0] = offset[1] = 0;
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5PSIdirect_write(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
            offset[1] += CHUNK_NY;
        }
        offset[0] += CHUNK_NX;
        offset[1] = 0;
    }

    if(outbuf)
        free(outbuf);

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0) 
        TEST_ERROR;

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < CHUNK_NX; i++) {
        for(j = 0; j < CHUNK_NY; j++) {
            if(direct_buf[i][j] != check_chunk[i][j]) {
                printf("    2. Read different values than written.");
                printf("    At index %d,%d\n", i, j);
                printf("    direct_buf=%d, check_chunk=%d\n", direct_buf[i][j], check_chunk[i][j]); 
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
    H5Fclose(file);
    
    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Fclose(file);
    } H5E_END_TRY;

    if(outbuf)
        free(outbuf);

    return 1;
}
