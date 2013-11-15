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

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5srcdir.h"
#include "H5DOpublic.h"
#include <math.h>

#if defined(H5_HAVE_ZLIB_H) && !defined(H5_ZLIB_HEADER) 
# define H5_ZLIB_HEADER "zlib.h"
#endif
#if defined(H5_ZLIB_HEADER)
# include H5_ZLIB_HEADER /* "zlib.h" */
#endif

#define FILE_NAME "test_dectris.h5"

#define DATASETNAME1        "direct_write"
#define DATASETNAME2        "skip_one_filter"
#define DATASETNAME3        "skip_two_filters"
#define DATASETNAME4        "data_conv"
#define DATASETNAME5        "contiguous_dset"
#define DATASETNAME6        "invalid_argue"
#define RANK         2
#define NX     16
#define NY     16
#define CHUNK_NX     4
#define CHUNK_NY     4

#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s))*1.001)+12)

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS1	305
#define H5Z_FILTER_BOGUS2	306
#define ADD_ON			7
#define FACTOR			3

/* Local prototypes for filter functions */
static size_t filter_bogus1(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_bogus2(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS1[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS1,		/* Filter id number		*/
    1, 1,               /* Encoding and decoding enabled */
    "bogus1",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus1,		/* The actual filter function	*/
}};

const H5Z_class2_t H5Z_BOGUS2[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS2,		/* Filter id number		*/
    1, 1,               /* Encoding and decoding enabled */
    "bogus2",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus2,		/* The actual filter function	*/
}};

/*-------------------------------------------------------------------------
 * Function:	test_direct_chunk_write
 *
 * Purpose:	Test the basic functionality of H5DOwrite_chunk
 *
 * Return:	Success:	0
 *
 *		Failure:	1
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
    Bytef	    *z_dst;		/*destination buffer		*/
    uLongf	     z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    uLong	     z_src_nbytes = (uLong)buf_size;
    int          aggression = 9;     /* Compression aggression setting */
    void	*outbuf = NULL;         /* Pointer to new buffer */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("basic functionality of H5DOwrite_chunk");

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
     * It will be evicted from the cache by the H5DOwrite_chunk calls. 
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
        fprintf(stderr, "overflow");
        goto error;
    } else if(Z_MEM_ERROR == ret) {
	fprintf(stderr, "deflate memory error");
        goto error;
    } else if(Z_OK != ret) {
	fprintf(stderr, "other deflate error");
        goto error;
    }

    /* Write the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
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
    outbuf = HDmalloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        fprintf(stderr, "overflow");
        goto error;
    } else if(Z_MEM_ERROR == ret) {
	fprintf(stderr, "deflate memory error");
        goto error;
    } else if(Z_OK != ret) {
	fprintf(stderr, "other deflate error");
        goto error;
    }

    /* Rewrite the compressed chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.     */ 
    offset[0] = offset[1] = 0;
    for(i=0; i<NX/CHUNK_NX; i++) {
        for(j=0; j<NY/CHUNK_NY; j++) {
            status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, z_dst_nbytes, outbuf);
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

    return 1;
}
#endif /* H5_HAVE_FILTER_DEFLATE */

/*-------------------------------------------------------------------------
 * Function:	test_skip_compress_write1
 *
 * Purpose:	Test skipping compression filter when it is the only filter
 *              for the dataset
 *
 * Return:	Success:	0
 *
 *		Failure:	1
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

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("skipping compression filter for H5DOwrite_chunk");

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

    if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
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
                printf("    1. Read different values than written.");
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

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	filter_bogus1
 *
 * Purpose:	A bogus filte that adds ADD_ON to the original value
 *
 * Return:	Success:	Data chunk size
 *
 * Programmer:  Raymond Lu	
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus1(unsigned int flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr=(int *)*buf;          /* Pointer to the data values */
    ssize_t buf_left=*buf_size;  /* Amount of data buffer left to process */

    if(flags & H5Z_FLAG_REVERSE) { /* read */
        /* Substract the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ -= (int)ADD_ON;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end if */
    else { /* write */
        /* Add the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ += (int)ADD_ON;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end else */

    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:	filter_bogus2
 *
 * Purpose:	A bogus filter that multiplies the original value by FACTOR.
 *
 * Return:	Success:	Data chunk size
 *
 * Programmer:  Raymond Lu	
 *              30 November 2012
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus2(unsigned int flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int *int_ptr=(int *)*buf;          /* Pointer to the data values */
    ssize_t buf_left=*buf_size;  /* Amount of data buffer left to process */

    if(flags & H5Z_FLAG_REVERSE) { /* read */
        /* Substract the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ /= (int)FACTOR;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end if */
    else { /* write */
        /* Add the "add on" value to all the data values */
        while(buf_left>0) {
            *int_ptr++ *= (int)FACTOR;
            buf_left -= sizeof(int);
        } /* end while */
    } /* end else */

    return nbytes;
}

/*-------------------------------------------------------------------------
 * Function:	test_skip_compress_write2
 *
 * Purpose:	Test skipping compression filter when there are three filters
 *              for the dataset
 *
 * Return:	Success:	0
 *
 *		Failure:	1
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

    unsigned    filter_mask = 0;
    int         origin_direct_buf[CHUNK_NX][CHUNK_NY];
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int          aggression = 9;     /* Compression aggression setting */

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

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

    if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
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
                printf("    1. Read different values than written.");
                printf("    At index %d,%d\n", i, j);
                printf("    origin_direct_buf=%d, check_chunk=%d\n", origin_direct_buf[i][j], check_chunk[i][j]); 
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

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	test_data_conv
 *
 * Purpose:	Test data conversion
 *
 * Return:	Success:	0
 *
 *		Failure:	1
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
    const hsize_t	four = 4;
    hid_t	st=-1, dt=-1;
    hid_t       array_dt;

    unsigned    filter_mask = 0;
    src_type_t  direct_buf[CHUNK_NX][CHUNK_NY];
    dst_type_t  check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(src_type_t);

    hsize_t start[2];  /* Start of hyperslab */
    hsize_t stride[2]; /* Stride of hyperslab */
    hsize_t count[2];  /* Block count */
    hsize_t block[2];  /* Block sizes */

    TESTING("data conversion for H5DOwrite_chunk");

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

    if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
        goto error;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0) 
        goto error;

    if(H5Dclose(dataset) < 0)
        goto error;

    if((dataset = H5Dopen2(file, DATASETNAME4, H5P_DEFAULT)) < 0)
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
                printf("    1. Read different values than written.");
                printf("    At index %d,%d\n", i, j);
	        printf("    src={a=%d, b=%d, c=[%d,%d,%d,%d], d=%d, e=%d\n",
                   (direct_buf[i][j]).a, (direct_buf[i][j]).b, (direct_buf[i][j]).c[0], (direct_buf[i][j]).c[1], 
                   (direct_buf[i][j]).c[2], (direct_buf[i][j]).c[3], (direct_buf[i][j]).d, (direct_buf[i][j]).e);
	        printf("    dst={a=%d, c=[%d,%d,%d,%d], e=%d\n",
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

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	test_invalid_parameters
 *
 * Purpose:	Test invalid parameters for H5DOwrite_chunk
 *
 * Return:	Success:	0
 *
 *		Failure:	1
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

    TESTING("invalid parameters for H5DOwrite_chunk");

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
     * Create a new contiguous dataset to verify H5DOwrite_chunk doesn't work
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
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
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

    /* Check invalid dataset ID */ 
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(-1, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid DXPL ID */
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, -1, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid OFFSET */
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, NULL, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check when OFFSET is out of dataset range */
    offset[0] = NX + 1;
    offset[1] = NY;
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check when OFFSET is not on chunk boundary */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY + 1;
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid buffer size */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    buf_size = 0; 
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) != FAIL)
            goto error;
    } H5E_END_TRY;

    /* Check invalid data buffer */
    buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    H5E_BEGIN_TRY {
        if((status = H5DOwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, NULL)) != FAIL)
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

    return 1;
}

/*-------------------------------------------------------------------------
 * Function:	Main function
 *
 * Purpose:	Test direct chunk write function H5DOwrite_chunk
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:  Raymond Lu	
 *              30 November 2012
 *
 *-------------------------------------------------------------------------
 */
int main( void )
{
    hid_t file_id;
    int   nerrors=0;

    /*
     * Create a new file. If file exists its contents will be overwritten.
     */
    if((file_id = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Test direct chunk write */
#ifdef H5_HAVE_FILTER_DEFLATE
    nerrors += test_direct_chunk_write(file_id);
#endif /* H5_HAVE_FILTER_DEFLATE */
    nerrors += test_skip_compress_write1(file_id);
    nerrors += test_skip_compress_write2(file_id);
    nerrors += test_data_conv(file_id);
    nerrors += test_invalid_parameters(file_id);

    if(H5Fclose(file_id) < 0)
        goto error;

    /* check for errors */
    if (nerrors)
        goto error;

    return 0;

error:
    return 1;
}
