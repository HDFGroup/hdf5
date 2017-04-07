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

#include <stdlib.h>
#include <string.h>
#include "h5hltest.h"
#include "H5DOpublic.h"
#include <math.h>

#if defined(H5_HAVE_ZLIB_H) && !defined(H5_ZLIB_HEADER) 
# define H5_ZLIB_HEADER "zlib.h"
#endif
#if defined(H5_ZLIB_HEADER)
# include H5_ZLIB_HEADER /* "zlib.h" */
#endif

#define FILE		"test_append.h5"
#define DNAME_NOTSET	"dataset_notset"
#define DNAME_UNLIM	"dataset_unlim"
#define DNAME_LESS	"dataset_less"
#define DNAME_VARY	"dataset_vary"
#define DNAME_ROW	"dataset_row"
#define DNAME_COLUMN	"dataset_column"
#define DBUGNAME1	"dataset_bug1"
#define DBUGNAME2	"dataset_bug2"

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_notset
 *
 * Purpose:	Verify that H5DOappend works properly with default dapl.
 *		That is, H5Pset_append_flush() is not used to set boundary
 *		and callback in dapl.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Aug 2016
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_notset(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};			/* Current dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, 20};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};		/* Chunk dimension sizes */
    int lbuf[10];			/* The data buffers */
    int i, j;				/* Local index variables */
    h5_stat_t  sb1, sb2;		/* File info */

    TESTING("Append flush with H5DOappend()--append rows with default dapl");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_NOTSET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
	FAIL_STACK_ERROR;

    /* Append 6 rows to the dataset */
    for(i = 0; i < 6; i++) {
	for(j = 0; j < 10; j++)
	    lbuf[j] = (i * 10) + (j + 1);
	/* Append without boundary, callback and flush */
	if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)1, H5T_NATIVE_INT, lbuf) < 0)
	    FAIL_STACK_ERROR;
    } /* end for */

    /* File size when not flushed */
    if(HDstat(FILE, &sb1) < 0)
	TEST_ERROR;

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* File size after flushing */
    if(HDstat(FILE, &sb2) < 0)
	TEST_ERROR;

    /* File size before flushing should be less */
    if(sb1.st_size > sb2.st_size) 
	TEST_ERROR;

    /* Closing */
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_notset() */

/* The callback function for the object flush property */
static herr_t
flush_func(hid_t H5_ATTR_UNUSED obj_id, void *_udata)
{
    unsigned *flush_ct = (unsigned*)_udata;
    ++(*flush_ct);
    return 0;
}

/* The callback function for the append flush property */
static herr_t
append_func(hid_t H5_ATTR_UNUSED dset_id, hsize_t H5_ATTR_UNUSED *cur_dims, void *_udata)
{
    unsigned *append_ct = (unsigned *)_udata;
    ++(*append_ct);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_rows_columns
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *		are working properly when appending rows and columns to a dataset
 *		with 2 extendible dimensions.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_rows_columns(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};					/* Current dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};				/* Chunk dimension sizes */
    int lbuf[10], cbuf[6];		/* The data buffers */
    int buf[6][13], rbuf[6][13];	/* The data buffers */

    hsize_t boundary[2] = {1, 1};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    int i, j;				/* Local index variables */

    TESTING("Append flush with H5DOappend()--append rows & columns");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_UNLIM, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 6 rows to the dataset */
    for(i = 0; i < 6; i++) {
	for(j = 0; j < 10; j++)
	    lbuf[j] = buf[i][j] = (i * 10) + (j + 1);
	if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)1, H5T_NATIVE_INT, lbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 6)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 6)
	TEST_ERROR;

    /* Append 3 columns to the dataset */
    for(i = 0; i < 3; i++) {
	for(j = 0; j < 6; j++)
	    cbuf[j] = buf[j][i + 10] = ((i * 6) + (j + 1)) * -1;
	if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)1, H5T_NATIVE_INT, cbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 9)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 9)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Clear the buffer */
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen2(fid, DNAME_UNLIM, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_rows_columns() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_rows
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *		are working properly when appending rows to a dataset with
 *		one extendible dimension (row).
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_rows(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};			/* Current dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, 10};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};		/* Chunk dimension sizes */
    int lbuf[10];			/* The data buffer */
    int buf[6][10], rbuf[6][10];	/* The data buffers */
    int i, j;				/* Local index variables */

    hsize_t boundary[2] = {1, 0};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append rows");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 1 extendible dimension */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_ROW, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 6 rows to the dataset */
    for(i = 0; i < 6; i++) {
	for(j = 0; j < 10; j++)
	    lbuf[j] = buf[i][j] = (i * 10) + (j + 1);
	if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)1, H5T_NATIVE_INT, lbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 6)
	TEST_ERROR;

    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 6)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 10; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Clear the buffer */
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen2(fid, DNAME_ROW, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 10; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_rows() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_columns
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *		are working properly when appending columns to a dataset
 *		with one extendible dimension (column).
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_columns(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {6, 0};			/* Current dimension sizes */
    hsize_t maxdims[2] = {6, H5S_UNLIMITED};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};		/* Chunk dimension sizes */
    int cbuf[6];				/* The data buffer */
    int buf[6][3], rbuf[6][3];			/* The data buffers */
    int i, j;					/* Local index variable */

    hsize_t boundary[2] = {0, 1};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append columns");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 1 extendible dimension */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_COLUMN, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 3 columns to the dataset */
    for(i = 0; i < 3; i++) {
	for(j = 0; j < 6; j++)
	    cbuf[j] = buf[j][i] = ((i * 6) + (j + 1)) * -1;
	if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)1, H5T_NATIVE_INT, cbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 3)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 3)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 3; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Clear the buffer */
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen2(fid, DNAME_COLUMN, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 3; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_columns() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_BUG1
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *              are working properly when appending rows and columns to an
 *              extendible dataset.
 *		A BUG occurs:
 *		 when the extendible dataset is set up as follows:
 *			hsize_t dims[2] = {0, 10};
 *		 	hsize_t maxdims[2] = {H5S_UNLIMITED, 50};
 *		 when append 6 rows and 3 columns to the dataset;
 *		 The data is correct when the dataset is read at this point;
 *		 The data is incorrect when the dataset is closed, opened again, and read at this point;
 *		 NOTE: the problem does not occur when H5Dflush() is not performed for each row/column.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_BUG1(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* Dataset creation property */
    hid_t dapl = -1;			/* Dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};			/* Current dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, 50};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};		/* Chunk dimension sizes */
    int lbuf[10], cbuf[6];		/* The data buffers */
    int buf[6][13], rbuf[6][13];	/* The data buffers */
    int i, j;				/* Local index variables */

    hsize_t boundary[2] = {1, 1};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append rows & columns--BUG1");
    
    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DBUGNAME1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 6 rows to the dataset */
    for(i = 0; i < 6; i++) {
	for(j = 0; j < 10; j++)
	    lbuf[j] = buf[i][j] = (i * 10) + (j + 1);
	if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)1, H5T_NATIVE_INT, lbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 6)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 6)
	TEST_ERROR;

    /* Append 3 columns to the dataset */
    for(i = 0; i < 3; i++) {
	for(j = 0; j < 6; j++)
	    cbuf[j] = buf[j][i+10] = ((i * 6) + (j + 1)) * -1;
	if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)1, H5T_NATIVE_INT, cbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 9)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 9)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

#ifdef BUG1
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen(fid, DBUGNAME1, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;
#endif

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Pclose(dapl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_BUG1() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_BUG2
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *              are working properly when appending rows and columns to an
 *              extendible dataset.
 *		A BUG occurs:
 *		 when the extendible dataset is set up as follows:
 *			hsize_t dims[2] = {0, 10};
 *		 	hsize_t maxdims[2] = {50, H5S_UNLIMITED};
 *		 when append 6 rows and 3 columns to the dataset;
 *		 The data is correct when the dataset is read at this point;
 *		 The data is incorrect when the dataset is closed, opened again, and read at this point;
 *		 NOTE: the problem does not occur when H5Dflush() is not performed for each row/column.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_BUG2(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* Dataset creation property */
    hid_t dapl = -1;			/* Dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};			/* Current dimension sizes */
    hsize_t maxdims[2] = {50, H5S_UNLIMITED};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};		/* Chunk dimension sizes */
    int lbuf[10], cbuf[6];			/* Data buffers */
    int buf[6][13], rbuf[6][13];		/* Data buffers */
    int i, j;					/* Local index variables */

    hsize_t boundary[2] = {1, 1};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append rows & columns--BUG2");
    
    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DBUGNAME2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 6 rows to the dataset */
    for(i = 0; i < 6; i++) {
	for(j = 0; j < 10; j++)
	    lbuf[j] = buf[i][j] = (i * 10) + (j + 1);
	if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)1, H5T_NATIVE_INT, lbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 6)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 6)
	TEST_ERROR;


    /* Append 3 columns to the dataset */
    for(i = 0; i < 3; i++) {
	for(j = 0; j < 6; j++)
	    cbuf[j] = buf[j][i+10] = ((i * 6) + (j + 1)) * -1;
	if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)1, H5T_NATIVE_INT, cbuf) < 0)
	    TEST_ERROR;
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 9)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 9)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

#ifdef BUG2
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen(fid, DBUGNAME2, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;
#endif

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Pclose(dapl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_BUG2() */


/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_less
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *		are working properly when appending rows and columns to an
 *		extendible dataset where the append size is less than the boundary
 *		size.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_less(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};		/* Current dimension sizes */
    hsize_t maxdims[2] = {100, 100};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};	/* Chunk dimension sizes */
    int lbuf[20], cbuf[6][3];		/* Data buffers */
    int buf[6][13], rbuf[6][13];	/* Data buffers */
    int i, j, k;			/* Local index variables */

    hsize_t boundary[2] = {3, 3};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append size < boundary size");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_LESS, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append to the dataset 2 rows at a time for 3 times */
    for(i = 0, k = 0; i < 6; i++) {
	for(j = 0; j < 10; j++, k++)
	    buf[i][j] = lbuf[k] = (i * 10) + (j + 1);

	if((i + 1) % 2 == 0) {
	    if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)2, H5T_NATIVE_INT, lbuf) < 0)
		TEST_ERROR;
	    k = 0;
	} /* end if */
    } /* end for */

    /* Verify the # of appends */
    if(append_ct != 2)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 2)
	TEST_ERROR;

    /* Append 3 columns to the dataset, once */
    for(i = 0; i < 3; i++)
	for(j = 0; j < 6; j++, k++)
	    cbuf[j][i] = buf[j][i + 10] = ((i * 6) + (j + 1)) * -1;
    if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)3, H5T_NATIVE_INT, cbuf) < 0)
	TEST_ERROR;

    /* Verify the # of appends */
    if(append_ct != 3)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 3)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Clear the buffer */
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen2(fid, DNAME_LESS, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_less() */

/*-------------------------------------------------------------------------
 * Function:	test_dataset_append_vary
 *
 * Purpose:	Verify that the object flush property and the append flush property
 *		are working properly when appending rows and columns to an
 *		extendible dataset where 
 *			row: the append size is 3 times of the boundary size
 *			      the append callback/flush is performed on the 1st boundary hit
 *			column: the boundary is greater than the append size
 *				the boundary is not hit at all
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
static int
test_dataset_append_vary(hid_t fid)
{
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ffapl = -1;			/* The file's file access property list */

    hsize_t dims[2] = {0, 10};					/* Current dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};	/* Maximum dimension sizes */
    hsize_t chunk_dims[2] = {2,5};				/* Chunk dimension sizes */
    int lbuf[60], cbuf[6][3];		/* Data buffers */
    int buf[6][13], rbuf[6][13];	/* Data buffers */
    int i, j, k;			/* Local index variables */

    hsize_t boundary[2] = {3, 7};	/* Boundary sizes */
    unsigned append_ct = 0;		/* The # of appends */
    unsigned *flush_ptr;		/* Points to the flush counter */

    TESTING("Append flush with H5DOappend()--append & boundary size vary");

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Set to create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;

    /* Set append flush property */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Pset_append_flush(dapl, 2, boundary, append_func, &append_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the dataset */
    if((did = H5Dcreate2(fid, DNAME_VARY, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl)) < 0) 
	TEST_ERROR;

    /* Append 6 rows to the dataset, once */
    for(i = 0, k = 0; i < 6; i++)
	for(j = 0; j < 10; j++, k++)
	    buf[i][j] = lbuf[k] = (i * 10) + (j + 1);
    if(H5DOappend(did, H5P_DEFAULT, 0, (size_t)6, H5T_NATIVE_INT, lbuf) < 0)
	TEST_ERROR;

    /* Verify the # of appends */
    if(append_ct != 1)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 1)
	TEST_ERROR;

    /* Append 3 columns to the dataset, once */
    for(i = 0; i < 3; i++)
	for(j = 0; j < 6; j++, k++)
	    cbuf[j][i] = buf[j][i + 10] = ((i * 6) + (j + 1)) * -1;
    if(H5DOappend(did, H5P_DEFAULT, 1, (size_t)3, H5T_NATIVE_INT, cbuf) < 0)
	TEST_ERROR;

    /* Verify the # of appends */
    if(append_ct != 1)
	TEST_ERROR;

    /* Retrieve and verify object flush counts */
    if(H5Pget_object_flush_cb(ffapl, NULL, (void **)&flush_ptr) < 0)
	FAIL_STACK_ERROR;
    if(*flush_ptr != 1)
	TEST_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;
    
    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Clear the dataset */
    HDmemset(rbuf, 0, sizeof(rbuf));

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset again */
    if((did = H5Dopen2(fid, DNAME_VARY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Read the dataset */
    if(H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
	FAIL_STACK_ERROR;

    /* Verify the data */
    for(i = 0; i < 6; i++)
	for(j = 0; j < 13; j++)
            if(buf[i][j] != rbuf[i][j])
                TEST_ERROR;

    /* Closing */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(dcpl);
	H5Pclose(sid);
	H5Dclose(did);
	H5Pclose(ffapl);
    } H5E_END_TRY;

    return 1;
} /* test_dataset_append_vary() */

/*-------------------------------------------------------------------------
 * Function:	Main function
 *
 * Purpose:	Test H5Pset/get_object_flush_cb() and H5Pset/get_append_flush()
 *		along with H5DOappend().
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:  Vailin Choi; Jan 2014
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    hid_t fid = -1;		/* File ID */
    hid_t fapl = -1;		/* File access property list */
    unsigned flush_ct = 0;	/* The # of flushes */
    int   nerrors = 0;		/* The # of errors encountered */

    /* Get a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) 
	FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR;

    /* Set object flush property */
    if(H5Pset_object_flush_cb(fapl, flush_func, &flush_ct) < 0)
	FAIL_STACK_ERROR;

    /* Create the test file */
    if((fid = H5Fcreate(FILE, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR;

    nerrors += test_dataset_append_notset(fid);

    nerrors += test_dataset_append_rows(fid);

    flush_ct = 0;	/* Reset flush counter */
    nerrors += test_dataset_append_columns(fid);

    flush_ct = 0;	/* Reset flush counter */
    nerrors += test_dataset_append_rows_columns(fid);

#ifdef BUG1_BUG2
/*
 * The following tests illustrate the scenarios when H5DOappend does not work with extensible array indexing:
 *	- when the the dataset has 1 unlimited dimension and the other dimension is fixed but extendible
 *	- the dataset expands along 1 dimension and then expands along the other dimension
 */
     flush_ct = 0;	/* Reset flush counter */
     nerrors += test_dataset_append_BUG1(fid);

     flush_ct = 0;	/* Reset flush counter */
     nerrors += test_dataset_append_BUG2(fid);
#endif

    flush_ct = 0;	/* Reset flush counter */
    nerrors += test_dataset_append_less(fid);

    flush_ct = 0;	/* Reset flush counter */
    nerrors += test_dataset_append_vary(fid);

    /* Closing */
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Check for errors */
    if(nerrors)
        goto error;

    return 0;

error:
    return 1;
}

