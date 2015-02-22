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
 * This test is for the DECTRIS project to the H5DOwrite_chunk function
 *
 */

#include "hdf5_hl.h"
#include <zlib.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
 
const char *FILENAME[] = {
    "dectris_perf",
    "unix.raw",
    NULL
};

/*
 * Print the current location on the standard output stream.
 */
#define FUNC __func__
#define AT()     printf ("   at %s:%d in %s()...\n",        \
        __FILE__, __LINE__, FUNC);
#define H5_FAILED()  {puts("*FAILED*");fflush(stdout);}
#define TEST_ERROR      {H5_FAILED(); AT(); goto error;}
#define TESTING(WHAT)  {printf("Testing %-62s",WHAT); fflush(stdout);}
#define PASSED()  {puts(" PASSED");fflush(stdout);}

#define DIRECT_UNCOMPRESSED_DSET        "direct_uncompressed_dset"
#define DIRECT_COMPRESSED_DSET        "direct_compressed_dset"
#define REG_COMPRESSED_DSET       "reg_compressed_dset"
#define REG_NO_COMPRESS_DSET   "reg_no_compress_dset"
#define RANK         3
#define NX     100
#define NY     1000
#define NZ     250
#define CHUNK_NX     1 
#define CHUNK_NY     1000
#define CHUNK_NZ     250

#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s))*1.001)+12)
char        filename[1024];
unsigned int *outbuf[NX];
size_t         data_size[NX];
double       total_size = 0.0;
unsigned int *direct_buf[NX];
double       MB = 1048576.0;

/*--------------------------------------------------
 * Function to report IO rate
 *--------------------------------------------------
 */
void reportTime(struct timeval start, double mbytes)
{
    struct timeval timeval_stop,timeval_diff;

    /*end timing*/
    gettimeofday(&timeval_stop,NULL);

    /* Calculate the elapsed gettimeofday time */
    timeval_diff.tv_usec=timeval_stop.tv_usec-start.tv_usec;
    timeval_diff.tv_sec=timeval_stop.tv_sec-start.tv_sec;

    if(timeval_diff.tv_usec<0) {
        timeval_diff.tv_usec+=1000000;
        timeval_diff.tv_sec--;
    } /* end if */

/*printf("mbytes=%lf, sec=%lf, usec=%lf\n", mbytes, (double)timeval_diff.tv_sec, (double)timeval_diff.tv_usec);*/
    printf("MBytes/second: %lf\n", (double)mbytes/((double)timeval_diff.tv_sec+((double)timeval_diff.tv_usec/(double)1000000.0)));        
}

/*--------------------------------------------------
 *  Create file, datasets, and initialize data
 *--------------------------------------------------
 */
int create_file(hid_t fapl_id)
{
    hid_t       file;                          /* handles */
    hid_t       fapl;
    hid_t       cparms;
    hid_t       dataspace, dataset;
    hsize_t     dims[RANK]  = {NX, NY, NZ};        
    hsize_t     chunk_dims[RANK] ={CHUNK_NX, CHUNK_NY, CHUNK_NZ};
    unsigned int         aggression = 9;     /* Compression aggression setting */
    int         ret;
    int         i, j, n;

    int flag;
    int unix_file;

    unsigned int *p;
    size_t      buf_size = CHUNK_NY*CHUNK_NZ*sizeof(unsigned int);

    const Bytef *z_src;
    Bytef	    *z_dst;		/*destination buffer		*/
    uLongf	     z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    uLong	     z_src_nbytes = (uLong)buf_size;

    TESTING("Create a file and dataset");

    /*
     * Create the data space with unlimited dimensions.
     */
    if((dataspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /*
     * Create a new file. If file exists its contents will be overwritten.
     */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /*
     * Modify dataset creation properties, i.e. enable chunking and compression
     */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if(H5Pset_chunk( cparms, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    if((dataset = H5Dcreate2(file, DIRECT_UNCOMPRESSED_DSET, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;

    if((dataset = H5Dcreate2(file, REG_NO_COMPRESS_DSET, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;

    /* Set compression */
    if(H5Pset_deflate( cparms, aggression) < 0)
        TEST_ERROR;

    if((dataset = H5Dcreate2(file, DIRECT_COMPRESSED_DSET, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;


    if((dataset = H5Dcreate2(file, REG_COMPRESSED_DSET, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dataset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0) 
        TEST_ERROR;

    if(H5Sclose(dataspace) < 0)
        TEST_ERROR;

    if(H5Pclose(cparms) < 0)
        TEST_ERROR;

    /* create a unix file*/
    flag = O_CREAT|O_TRUNC|O_WRONLY;

    if ((unix_file=open(FILENAME[1],flag,S_IRWXU))== -1)
        TEST_ERROR;

    if (close(unix_file) < 0)
    {
        printf(" unable to close the file\n");
        TEST_ERROR;
    }


    /* Initialize data for chunks */
    for(i = 0; i < NX; i++) {
	p = direct_buf[i] = (unsigned int*)malloc(CHUNK_NY*CHUNK_NZ*sizeof(unsigned int));
        
        for(j=0; j < CHUNK_NY*CHUNK_NZ; j++, p++)
            *p = rand() % 65000;

        z_src = (const Bytef*)direct_buf[i];

        z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
        /* Allocate output (compressed) buffer */
        outbuf[i] = (unsigned int*)malloc((size_t)z_dst_nbytes);
        z_dst = (Bytef *)outbuf[i];

        /* Perform compression from the source to the destination buffer */
        ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

        data_size[i] = (size_t)z_dst_nbytes;
        total_size += data_size[i];

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
    }


    PASSED();

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*--------------------------------------------------
 *  Benchmark the performance of the new function 
 *  with precompressed data.
 *--------------------------------------------------
 */
int
test_direct_write_uncompressed_data(hid_t fapl_id)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;
    hid_t       dxpl;
    herr_t      status;
    int         i;

    unsigned    filter_mask = 0;
    hsize_t     offset[RANK] = {0, 0, 0};

    struct timeval timeval_start;    
    
    TESTING("H5DOwrite_chunk for uncompressed data");

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Start the timer */
    gettimeofday(&timeval_start,NULL);

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, DIRECT_UNCOMPRESSED_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;


    /* Write the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    for(i=0; i<NX; i++) {
        status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, CHUNK_NY*CHUNK_NZ*sizeof(unsigned int), direct_buf[i]);
        (offset[0])++;
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Pclose(dxpl);
    H5Fclose(file);

    /* Report the performance */ 
    reportTime(timeval_start, (double)(NX*NY*NZ*sizeof(unsigned int)/MB));    

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Pclose(dxpl);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}


/*--------------------------------------------------
 *  Benchmark the performance of the new function 
 *  with precompressed data.
 *--------------------------------------------------
 */
int
test_direct_write_compressed_data(hid_t fapl_id)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;
    hid_t       dxpl;
    herr_t      status;
    int         i;

    unsigned    filter_mask = 0;
    hsize_t     offset[RANK] = {0, 0, 0};

    struct timeval timeval_start;    
    
    TESTING("H5DOwrite_chunk for pre-compressed data");

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Start the timer */
    gettimeofday(&timeval_start,NULL);

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, DIRECT_COMPRESSED_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;


    /* Write the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    for(i=0; i<NX; i++) {
        status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, data_size[i], outbuf[i]);
        (offset[0])++;
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Pclose(dxpl);
    H5Fclose(file);
  
    /* Report the performance */ 
    reportTime(timeval_start, (double)(total_size/MB));    

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Pclose(dxpl);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*--------------------------------------------------
 *  Benchmark the performance of the regular H5Dwrite
 *  with compression filter enabled.
 *--------------------------------------------------
 */
int
test_compressed_write(hid_t fapl_id)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;
    hid_t       mem_space;
    hsize_t     chunk_dims[RANK] ={CHUNK_NX, CHUNK_NY, CHUNK_NZ};
    hid_t       dxpl;
    herr_t      status;
    int         i;

    hsize_t start[RANK];  /* Start of hyperslab */
    hsize_t stride[RANK]; /* Stride of hyperslab */
    hsize_t count[RANK];  /* Block count */
    hsize_t block[RANK];  /* Block sizes */

    struct timeval timeval_start;    

    TESTING("H5Dwrite with compression enabled");

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR;

    /* Start the timer */
    gettimeofday(&timeval_start,NULL);

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, REG_COMPRESSED_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if((dataspace = H5Dget_space(dataset)) < 0)
        TEST_ERROR;

    start[0]  = start[1]  = start[2] = 0;
    stride[0] = stride[1] = stride[2] = 1;
    count[0]  = count[1]  = count[2] = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY; block[2] = CHUNK_NZ;
    
    for(i=0; i<NX; i++) {
        /*
         * Select hyperslab for one chunk in the file
         */
        if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
            TEST_ERROR;
        (start[0])++;        

        if((status = H5Dwrite(dataset, H5T_NATIVE_INT, mem_space, dataspace,
    		      H5P_DEFAULT, direct_buf[i])) < 0)
            TEST_ERROR;
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(dataspace);
    H5Sclose(mem_space);
    H5Pclose(dxpl);
    H5Fclose(file);
 
    /* Report the performance */ 
    reportTime(timeval_start, (double)(NX*NY*NZ*sizeof(unsigned int)/MB));    
   
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(dataspace);
        H5Sclose(mem_space);
        H5Pclose(dxpl);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*--------------------------------------------------
 *  Benchmark the performance of the regular H5Dwrite
 *  with compression
 *--------------------------------------------------
 */
int
test_no_compress_write(hid_t fapl_id)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;
    hid_t       mem_space;
    hsize_t     chunk_dims[RANK] ={CHUNK_NX, CHUNK_NY, CHUNK_NZ};
    hid_t       dxpl;
    herr_t      status;
    int         i;

    hsize_t start[RANK];  /* Start of hyperslab */
    hsize_t stride[RANK]; /* Stride of hyperslab */
    hsize_t count[RANK];  /* Block count */
    hsize_t block[RANK];  /* Block sizes */

    struct timeval timeval_start;    

    TESTING("H5Dwrite without compression");

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR;

    /* Start the timer */
    gettimeofday(&timeval_start,NULL);

    /* Reopen the file and dataset */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    if((dataset = H5Dopen2(file, REG_NO_COMPRESS_DSET, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if((dataspace = H5Dget_space(dataset)) < 0)
        TEST_ERROR;

    start[0]  = start[1]  = start[2] = 0;
    stride[0] = stride[1] = stride[2] = 1;
    count[0]  = count[1]  = count[2] = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY; block[2] = CHUNK_NZ;
    
    for(i=0; i<NX; i++) {
        /*
         * Select hyperslab for one chunk in the file
         */
        if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
            TEST_ERROR;
        (start[0])++;        

        if((status = H5Dwrite(dataset, H5T_NATIVE_INT, mem_space, dataspace,
    		      H5P_DEFAULT, direct_buf[i])) < 0)
            TEST_ERROR;
    }

    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(dataspace);
    H5Sclose(mem_space);
    H5Pclose(dxpl);
    H5Fclose(file);
 
    /* Report the performance */ 
    reportTime(timeval_start, (double)(NX*NY*NZ*sizeof(unsigned int)/MB));    
   
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(dataspace);
        H5Sclose(mem_space);
        H5Pclose(dxpl);
        H5Fclose(file);
    } H5E_END_TRY;
    return 1;
}

/*--------------------------------------------------
 *  Benchmark the performance for writing compressed
 *  data to a Unix file
 *--------------------------------------------------
 */
int 
test_unix_write(void)
{
    int file, flag;
    ssize_t op_size;    
    int i;
    struct timeval timeval_start;    

    TESTING("Write compressed data to a Unix file");

    /* create file*/
    flag = O_WRONLY;

    /* Start the timer */
    gettimeofday(&timeval_start,NULL);

    if ((file=open(FILENAME[1],flag))== -1)
        TEST_ERROR;

    /* Write the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    for(i=0; i<NX; i++) {
        op_size = write(file, outbuf[i],data_size[i]);
        if (op_size < 0)
        {
            printf(" Error in writing data to file because %s \n", strerror(errno));
            TEST_ERROR;
        }
        else if (op_size == 0)
        {
            printf(" unable to write sufficent data to file because %s \n", strerror(errno));
            TEST_ERROR;
        }
    }

    if (close(file) < 0)
    {
        printf(" unable to close the file\n");
        TEST_ERROR;
    }

    /* Report the performance */ 
    reportTime(timeval_start, (double)(total_size/MB));    

    PASSED();
    return 0;

error:
    return 1; 
}

/*--------------------------------------------------
 *  Main function
 *--------------------------------------------------
 */
int
main (void)
{
    hid_t       fapl = H5P_DEFAULT;
    int         i;

    /* Testing setup */
/*    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);*/

    sprintf(filename, "%s.h5", FILENAME[0]);

    create_file(fapl);
    test_direct_write_uncompressed_data(fapl);
    test_direct_write_compressed_data(fapl);
    test_no_compress_write(fapl);
    test_compressed_write(fapl);
    test_unix_write();

    for(i=0; i<NX; i++) {
        free(outbuf[i]);
        free(direct_buf[i]);
    }
 
/*    h5_cleanup(FILENAME, fapl);*/
    return 0;
}
