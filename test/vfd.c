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
 * Programmer:  Raymond Lu<slu@ncsa.uiuc.edu>
 *              Tuesday, Sept 24, 2002
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 */

#include "h5test.h"

#define KB              1024U
#define FAMILY_NUMBER   4
#define FAMILY_SIZE     (1*KB)
#define FAMILY_SIZE2    (5*KB)
#define FAMILY_SIZE_AIO (32 * KB * KB)
#define MULTI_SIZE      128
#define CORE_INCREMENT  (4*KB)

/*Macros for Direct VFD*/
#define MBOUNDARY	512
#define FBSIZE		(4*KB)
#define CBSIZE		(8*KB)
#define THRESHOLD 	1
#define DSET1_NAME	"dset1"
#define DSET1_DIM1      1024
#define DSET1_DIM2      32
#define DSET2_NAME	"dset2"
#define DSET2_DIM       4

const char *FILENAME[] = {
    "sec2_file",
    "core_file",
    "family_file",
    "new_family_v16_",
    "multi_file",
    "direct_file",
    "sec2_aio_test",
    "core_aio_test",
    "stdio_aio_test",
    "family_aio_test",
    "multi_aio_test",
    "multi_aio_test2",
    "sec2_aio_error_test",
    "core_aio_error_test",
    "stdio_aio_error_test",
    "family_aio_error_test",
    "multi_aio_error_test",
    NULL
};

#define COMPAT_BASENAME "family_v16_"

#define READ_OP		0
#define WRITE_OP	1

static void aio_multi_read_write_fsync_cancel_check(H5FD_t * file,
				                    int op_count,
                                                    int ops[],
			                            H5FD_mem_t types[],
				                    haddr_t offsets[],
				                    size_t lengths[],
				                    const char * tags[],
                                                    hbool_t * pass_ptr, 
                                                    const char ** failure_mssg_ptr);

static void aio_multi_write_sync_read_check(H5FD_t * file,
                                            int write_count,
                                            H5FD_mem_t types[],
                                            haddr_t offsets[],
                                            size_t lengths[],
                                            const char * tags[],
                                            hbool_t * pass_ptr, 
                                            const char ** failure_mssg_ptr);

static void aio_single_write_read_check(H5FD_t * file,
                                        H5FD_mem_t type,
                                        const char * tag_string,
                                        haddr_t offset,
                                        const size_t write_size,
                                        hbool_t do_wait,
                                        hbool_t * pass_ptr,
                                        const char ** failure_mssg_ptr);

static int generic_aio_test(const char * test_banner, 
                            const int file_name_num,
		            hid_t fapl_id,
                            haddr_t maxaddr);

static int generic_aio_input_error_tests(const char * test_banner,
                                         const char * tag_string,
                                         const int file_name_num,
                                         hid_t fapl_id,
                                         hbool_t verbose);

static int multi_file_driver_aio_test(const char * test_banner, 
                                      const int file_name_num);


/*-------------------------------------------------------------------------
 * Function:    test_sec2
 *
 * Purpose:     Tests the file handle interface for SEC2 driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_sec2(void)
{
    hid_t       file=(-1), fapl, access_fapl = -1;
    char        filename[1024];
    int         *fhandle=NULL;
    hsize_t     file_size;

    TESTING("SEC2 file driver");

    /* Set property list and file name for SEC2 driver. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_sec2(fapl) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  It's 2KB right now.
     */
    if(file_size<1*KB || file_size>4*KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;
    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_direct
 *
 * Purpose:     Tests the file handle interface for DIRECT I/O driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Wednesday, 20 September 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_direct(void)
{
#ifdef H5_HAVE_DIRECT
    hid_t       file=(-1), fapl, access_fapl = -1;
    hid_t	dset1=-1, dset2=-1, space1=-1, space2=-1;
    char        filename[1024];
    int         *fhandle=NULL;
    hsize_t     file_size;
    hsize_t	dims1[2], dims2[1];
    size_t	mbound;
    size_t	fbsize;
    size_t	cbsize;
    int		*points, *check, *p1, *p2;
    int		wdata2[DSET2_DIM] = {11,12,13,14};
    int		rdata2[DSET2_DIM];
    int		i, j, n;
#endif /*H5_HAVE_DIRECT*/

    TESTING("Direct I/O file driver");

#ifndef H5_HAVE_DIRECT
    SKIPPED();
    return 0;
#else /*H5_HAVE_DIRECT*/

    /* Set property list and file name for Direct driver.  Set memory alignment boundary
     * and file block size to 512 which is the minimum for Linux 2.6. */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_direct(fapl, MBOUNDARY, FBSIZE, CBSIZE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Verify the file access properties */
    if(H5Pget_fapl_direct(fapl, &mbound, &fbsize, &cbsize) < 0)
        TEST_ERROR;
    if(mbound != MBOUNDARY || fbsize != FBSIZE || cbsize != CBSIZE)
	TEST_ERROR;

    if(H5Pset_alignment(fapl, (hsize_t)THRESHOLD, (hsize_t)FBSIZE) < 0)
	TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    } H5E_END_TRY;
    if(file<0) {
        H5Pclose (fapl);
        SKIPPED();
        printf("	Probably the file system doesn't support Direct I/O\n");
        return 0;
    }

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file handle API */
    if(H5Fget_vfd_handle(file, H5P_DEFAULT, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no guarantee of the number of metadata allocations, but it's
     * 4 currently and the size of the file should be between 3 & 4 file buffer
     * sizes..
     */
    if(file_size < (FBSIZE * 3) || file_size >= (FBSIZE * 4))
        TEST_ERROR;

    /* Allocate aligned memory for data set 1. For data set 1, everything is aligned including
     * memory address, size of data, and file address. */
    if(posix_memalign(&points, (size_t)FBSIZE, (size_t)(DSET1_DIM1*DSET1_DIM2*sizeof(int)))!=0)
        TEST_ERROR;

    if(posix_memalign(&check, (size_t)FBSIZE, (size_t)(DSET1_DIM1*DSET1_DIM2*sizeof(int)))!=0)
        TEST_ERROR;

    /* Initialize the dset1 */
    p1 = points;
    for(i = n = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    *p1++ = n++;

    /* Create the data space1 */
    dims1[0] = DSET1_DIM1;
    dims1[1] = DSET1_DIM2;
    if((space1 = H5Screate_simple(2, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    /* Create the data space2. For data set 2, memory address and data size are not aligned. */
    dims2[0] = DSET2_DIM;
    if((space2 = H5Screate_simple(1, dims2, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset2 */
    if((dset2 = H5Dcreate2(file, DSET2_NAME, H5T_NATIVE_INT, space2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata2) < 0)
        TEST_ERROR;

    if(H5Dclose(dset2) < 0)
        TEST_ERROR;

    if((dset2 = H5Dopen2(file, DSET2_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata2) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < DSET2_DIM; i++)
	if(wdata2[i] != rdata2[i]) {
	    H5_FAILED();
	    printf("    Read different values than written in data set 2.\n");
	    printf("    At index %d\n", i);
            TEST_ERROR;
	} /* end if */

    if(H5Sclose(space1) < 0)
        TEST_ERROR;
    if(H5Dclose(dset1) < 0)
        TEST_ERROR;
    if(H5Sclose(space2) < 0)
        TEST_ERROR;
    if(H5Dclose(dset2) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;
    if(points)
	free(points);
    if(check)
	free(check);

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Sclose(space1);
        H5Dclose(dset1);
        H5Sclose(space2);
        H5Dclose(dset2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
#endif /*H5_HAVE_DIRECT*/
}


/*-------------------------------------------------------------------------
 * Function:    test_core
 *
 * Purpose:     Tests the file handle interface for CORE driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *              Raymond Lu, 2006-11-30
 *              Enabled the driver to read an existing file depending on
 *              the setting of the backing_store and file open flags.
 *-------------------------------------------------------------------------
 */
static herr_t
test_core(void)
{
    hid_t       file=(-1), fapl, access_fapl = -1;
    char        filename[1024];
    void        *fhandle=NULL;
    hsize_t     file_size;
    int		*points, *check, *p1, *p2;
    hid_t	dset1=-1, space1=-1;
    hsize_t	dims1[2];
    int		i, j, n;

    TESTING("CORE file driver");

    /* Set property list and file name for CORE driver */
    fapl = h5_fileaccess();
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    if(H5Fget_vfd_handle(file, H5P_DEFAULT, &fhandle) < 0)
        TEST_ERROR;
    if(fhandle==NULL)
    {
        printf("fhandle==NULL\n");
               TEST_ERROR;
    }

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable.  Why is this 4KB?
     */
    if(file_size<2*KB || file_size>6*KB)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;


    /* Open the file with backing store off for read and write.
     * Changes won't be saved in file. */
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, FALSE) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Allocate memory for data set. */
    points=(int*)malloc(DSET1_DIM1*DSET1_DIM2*sizeof(int));
    check=(int*)malloc(DSET1_DIM1*DSET1_DIM2*sizeof(int));

    /* Initialize the dset1 */
    p1 = points;
    for(i = n = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    *p1++ = n++;

    /* Create the data space1 */
    dims1[0] = DSET1_DIM1;
    dims1[1] = DSET1_DIM2;
    if((space1 = H5Screate_simple(2, dims1, NULL)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Open the file with backing store on for read and write.
     * Changes will be saved in file. */
    if(H5Pset_fapl_core(fapl, (size_t)CORE_INCREMENT, TRUE) < 0)
        TEST_ERROR;

    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create the dset1 */
    if((dset1 = H5Dcreate2(file, DSET1_NAME, H5T_NATIVE_INT, space1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data to the dset1 */
    if(H5Dwrite(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
        TEST_ERROR;

    if(H5Dclose(dset1) < 0)
        TEST_ERROR;

    if((dset1 = H5Dopen2(file, DSET1_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Reallocate memory for reading buffer. */
    if(check)
	free(check);

    check = (int*)malloc(DSET1_DIM1 * DSET1_DIM2 * sizeof(int));

    /* Read the data back from dset1 */
    if(H5Dread(dset1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    p1 = points;
    p2 = check;
    for(i = 0; i < DSET1_DIM1; i++)
	for(j = 0; j < DSET1_DIM2; j++)
	    if(*p1++ != *p2++) {
		H5_FAILED();
		printf("    Read different values than written in data set 1.\n");
		printf("    At index %d,%d\n", i, j);
        	TEST_ERROR;
	    } /* end if */

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* There is no garantee the size of metadata in file is constant.
     * Just try to check if it's reasonable. */
    if(file_size<64*KB || file_size>256*KB)
        TEST_ERROR;

    if(H5Sclose(space1) < 0)
        TEST_ERROR;
    if(H5Dclose(dset1) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;
    if(points)
	free(points);
    if(check)
	free(check);

    h5_cleanup(FILENAME, fapl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose (fapl);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family_opens
 *
 * Purpose:     Private function for test_family() to tests wrong ways of
 *              reopening family file.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_opens(char *fname, hid_t fa_pl)
{
    hid_t file;
    char first_name[1024];
    char wrong_name[1024];
    int i;

    /* Case 1: reopen file with 1st member file name and default property list */
    sprintf(first_name, fname, 0);

    H5E_BEGIN_TRY {
        file=H5Fopen(first_name, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 2: reopen file with correct name template but default property list */
    H5E_BEGIN_TRY {
        file=H5Fopen(fname, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 3: reopen file with wrong member size */
    if(H5Pset_fapl_family(fa_pl, (hsize_t)128, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fopen(fname, H5F_ACC_RDWR, fa_pl);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    /* Case 4: reopen file with wrong name template */
    HDstrcpy(wrong_name, fname);
    for(i = 0; i < 1024; i++)
        if(wrong_name[i] == '5') {
            wrong_name[i] = '4';
            break;
        }

    if(H5Pset_fapl_family(fa_pl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        file=H5Fopen(wrong_name, H5F_ACC_RDWR, fa_pl);
    } H5E_END_TRY;
    if(file >= 0) TEST_ERROR

    return 0;

error:
    return -1;
} /* end test_family_opens() */


/*-------------------------------------------------------------------------
 * Function:    test_family
 *
 * Purpose:     Tests the file handle interface for FAMILY driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *              Raymond Lu
 *              June 2, 2005
 *              Added a function test_family_opens() to test different
 *              wrong way to reopen family files.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), space=(-1), dset=(-1);
    hid_t       access_fapl = -1;
    char        filename[1024];
    char        dname[]="dataset";
    unsigned int i, j;
    int         *fhandle=NULL, *fhandle2=NULL;
    int         buf[FAMILY_NUMBER][FAMILY_SIZE];
    hsize_t     dims[2]={FAMILY_NUMBER, FAMILY_SIZE};
    hsize_t     file_size;

    TESTING("FAMILY file driver");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files where there's only
     * one member file existing. */
    if(test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with default member file size */
    if(H5Pset_fapl_family(fapl, (hsize_t)H5F_FAMILY_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* The file size is supposed to be about 800 bytes right now. */
    if(file_size < (KB / 2) || file_size > KB)
        TEST_ERROR;

    /* Create and write dataset */
    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for(i=0; i<FAMILY_NUMBER; i++)
        for(j=0; j<FAMILY_SIZE; j++)
            buf[i][j] = i*10000+j;

    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    /* check file handle API */
    if((fapl2=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if(H5Pset_family_offset(fapl2, (hsize_t)0) < 0)
        TEST_ERROR;

    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    if(H5Pset_family_offset(fapl2, (hsize_t)(FAMILY_SIZE*2)) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if(*fhandle2<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* Some data has been written.  The file size should be bigger(18KB+976
     * bytes if int size is 4 bytes) now. */
    if(sizeof(int)<=4) {
        if(file_size<18*KB || file_size>20*KB)
            TEST_ERROR;
    } else if(sizeof(int)>=8) {
        if(file_size<32*KB || file_size>40*KB)
            TEST_ERROR;
    }

    if(H5Sclose(space) < 0)
        TEST_ERROR;
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
    if(H5Pclose(fapl2) < 0)
        TEST_ERROR;
    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Test different wrong ways to reopen family files when there're multiple
     * member files existing. */
    if(test_family_opens(filename, fapl) < 0)
        TEST_ERROR;

    /* Reopen the file with correct member file size. */
    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose (fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_family_compat
 *
 * Purpose:     Tests the backward compatibility for FAMILY driver.
 *              See if we can open files created with v1.6 library.
 *              The source file was created by the test/file_handle.c
 *              of the v1.6 library.  Then tools/misc/h5repart.c was
 *              used to concantenated.  The command was "h5repart -m 5k
 *              family_file%05d.h5 family_v16_%05d.h5".
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              June 3, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_family_compat(void)
{
    hid_t       file = (-1), fapl;
    hid_t       dset;
    char        dname[]="dataset";
    char        filename[1024];
    char        pathname[1024], pathname_individual[1024];
    char        newname[1024], newname_individual[1024];
    int         counter = 0;

    TESTING("FAMILY file driver backward compatibility");

    /* Set property list and file name for FAMILY driver */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE2, H5P_DEFAULT) < 0)
        TEST_ERROR;

    h5_fixname(COMPAT_BASENAME, fapl, filename, sizeof filename);
    h5_fixname(FILENAME[3], fapl, newname, sizeof newname);

    pathname[0] = '\0';
    HDstrcat(pathname, filename);

    /* The following code makes the copies of the family files in the source directory.
     * Since we're going to open the files with write mode, this protects the original
     * files.
     */
    sprintf(newname_individual, newname, counter);
    sprintf(pathname_individual, pathname, counter);

    while (h5_make_local_copy(pathname_individual, newname_individual) >= 0) {
        counter++;
        sprintf(newname_individual, newname, counter);
        sprintf(pathname_individual, pathname, counter);
    }

    /* Make sure we can open the file.  Use the read and write mode to flush the
     * superblock. */
    if((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    /* Open the file again to make sure it isn't corrupted. */
    if((file = H5Fopen(newname, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    if((dset = H5Dopen2(file, dname, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
        H5Pclose(fapl);
    } H5E_END_TRY;

    return -1;
} /* end test_family_compat() */


/*-------------------------------------------------------------------------
 * Function:    test_multi_opens
 *
 * Purpose:     Private function for test_multi() to tests wrong ways of
 *              reopening multi file.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Raymond Lu
 *              Thursday, May 19, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi_opens(char *fname)
{
    hid_t file;
    char  super_name[1024];     /*name string "%%s-s.h5"*/
    char  sf_name[1024];        /*name string "multi_file-s.h5"*/

    /* Case: reopen with the name of super file and default property list */
    sprintf(super_name, "%%s-%c.h5", 's');
    sprintf(sf_name, super_name, fname);

    H5E_BEGIN_TRY {
        file=H5Fopen(sf_name, H5F_ACC_RDWR, H5P_DEFAULT);
    } H5E_END_TRY;

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    test_multi
 *
 * Purpose:     Tests the file handle interface for MUTLI driver
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 * Modifications:
 *
 *              Raymond Lu
 *              Wednesday, June 23, 2004
 *              Added test for H5Fget_filesize.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multi(void)
{
    hid_t       file=(-1), fapl, fapl2=(-1), dset=(-1), space=(-1);
    hid_t       root, attr, aspace, atype;
    hid_t       access_fapl = -1;
    char        filename[1024];
    int         *fhandle2=NULL, *fhandle=NULL;
    hsize_t     file_size;
    H5FD_mem_t  mt, memb_map[H5FD_MEM_NTYPES];
    hid_t       memb_fapl[H5FD_MEM_NTYPES];
    haddr_t     memb_addr[H5FD_MEM_NTYPES];
    const char  *memb_name[H5FD_MEM_NTYPES];
    char        sv[H5FD_MEM_NTYPES][32];
    hsize_t     dims[2]={MULTI_SIZE, MULTI_SIZE};
    hsize_t     adims[1]={1};
    char        dname[]="dataset";
    char        meta[] = "this is some metadata on this file";
    int         i, j;
    int         buf[MULTI_SIZE][MULTI_SIZE];

    TESTING("MULTI file driver");
    /* Set file access property list for MULTI driver */
    fapl = h5_fileaccess();

    HDmemset(memb_map, 0,  sizeof memb_map);
    HDmemset(memb_fapl, 0, sizeof memb_fapl);
    HDmemset(memb_name, 0, sizeof memb_name);
    HDmemset(memb_addr, 0, sizeof memb_addr);
    HDmemset(sv, 0, sizeof sv);

    for(mt=H5FD_MEM_DEFAULT; mt<H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t,mt)) {
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt] = H5FD_MEM_SUPER;
    }
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;
    memb_map[H5FD_MEM_BTREE] = H5FD_MEM_BTREE;
    memb_map[H5FD_MEM_GHEAP] = H5FD_MEM_GHEAP;

    sprintf(sv[H5FD_MEM_SUPER], "%%s-%c.h5", 's');
    memb_name[H5FD_MEM_SUPER] = sv[H5FD_MEM_SUPER];
    memb_addr[H5FD_MEM_SUPER] = 0;

    sprintf(sv[H5FD_MEM_BTREE],  "%%s-%c.h5", 'b');
    memb_name[H5FD_MEM_BTREE] = sv[H5FD_MEM_BTREE];
    memb_addr[H5FD_MEM_BTREE] = HADDR_MAX/4;

    sprintf(sv[H5FD_MEM_DRAW], "%%s-%c.h5", 'r');
    memb_name[H5FD_MEM_DRAW] = sv[H5FD_MEM_DRAW];
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    sprintf(sv[H5FD_MEM_GHEAP], "%%s-%c.h5", 'g');
    memb_name[H5FD_MEM_GHEAP] = sv[H5FD_MEM_GHEAP];
    memb_addr[H5FD_MEM_GHEAP] = HADDR_MAX*3/4;


    if(H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, memb_addr, TRUE) < 0)
        TEST_ERROR;
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;


    /* Test wrong ways to reopen multi files */
    if(test_multi_opens(filename) < 0)
        TEST_ERROR;

    /* Reopen the file */
    if((file=H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create and write data set */
    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Retrieve the access property list... */
    if ((access_fapl = H5Fget_access_plist(file)) < 0)
        TEST_ERROR;

    /* ...and close the property list */
    if (H5Pclose(access_fapl) < 0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* Before any data is written, the raw data file is empty.  So
     * the file size is only the size of b-tree + HADDR_MAX/4.
     */
    if(file_size < HADDR_MAX/4 || file_size > HADDR_MAX/2)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, dname, H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    for(i=0; i<MULTI_SIZE; i++)
        for(j=0; j<MULTI_SIZE; j++)
            buf[i][j] = i*10000+j;
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        TEST_ERROR;

    if((fapl2=H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if(H5Pset_multi_type(fapl2, H5FD_MEM_SUPER) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle) < 0)
        TEST_ERROR;
    if(*fhandle<0)
        TEST_ERROR;

    if(H5Pset_multi_type(fapl2, H5FD_MEM_DRAW) < 0)
        TEST_ERROR;
    if(H5Fget_vfd_handle(file, fapl2, (void **)&fhandle2) < 0)
        TEST_ERROR;
    if(*fhandle2<0)
        TEST_ERROR;

    /* Check file size API */
    if(H5Fget_filesize(file, &file_size) < 0)
        TEST_ERROR;

    /* After the data is written, the file size is huge because the
     * beginning of raw data file is set at HADDR_MAX/2.  It's supposed
     * to be (HADDR_MAX/2 + 128*128*4)
     */
    if(file_size < HADDR_MAX/2 || file_size > HADDR_MAX)
        TEST_ERROR;

    if(H5Sclose(space) < 0)
        TEST_ERROR;
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
    if(H5Pclose(fapl2) < 0)
        TEST_ERROR;

    /* Create and write attribute for the root group. */
    if((root = H5Gopen2(file, "/", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Attribute string. */
    if((atype = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if(H5Tset_size(atype, strlen(meta) + 1) < 0)
        TEST_ERROR;

    if(H5Tset_strpad(atype, H5T_STR_NULLTERM) < 0)
        TEST_ERROR;

    /* Create and write attribute */
    if((aspace = H5Screate_simple(1, adims, NULL)) < 0)
        TEST_ERROR;

    if((attr = H5Acreate2(root, "Metadata", atype, aspace, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Awrite(attr, atype, meta) < 0)
        TEST_ERROR;

    /* Close IDs */
    if(H5Tclose(atype) < 0)
        TEST_ERROR;
    if(H5Sclose(aspace) < 0)
        TEST_ERROR;
    if(H5Aclose(attr) < 0)
        TEST_ERROR;

    if(H5Fclose(file) < 0)
        TEST_ERROR;

    h5_cleanup(FILENAME, fapl);
    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Pclose(fapl);
        H5Pclose(fapl2);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	aio_multi_read_write_fsync_cancel_check
 *
 * Purpose:	Kick off the specified set of AIO reads and writes, call
 *		H5FDaio_fsync, and then use H5FDaio_cancel to cancel all
 *		operations.  
 *
 *		As H5FDaio_cancel() discards the control blocks, there is 
 *		no real test to perform, other than to verify that 
 *		H5FDaio_cancel() returns successfully.
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 2/2/11
 * 
 *-------------------------------------------------------------------------
 */

static void
aio_multi_read_write_fsync_cancel_check(H5FD_t * file,
				        int op_count,
                                        int ops[],
			                H5FD_mem_t types[],
				        haddr_t offsets[],
				        size_t lengths[],
				        const char * tags[],
                                        hbool_t * pass_ptr, 
                                        const char ** failure_mssg_ptr)
{
    const char * fcn_name = "aio_multi_read_write_fsync_cancel_check()";
    const char * H5FD_mem_t_strings[H5FD_MEM_NTYPES] =
        {
          "H5FD_MEM_DEFAULT ",
          "H5FD_MEM_SUPER ",
          "H5FD_MEM_BTREE ",
          "H5FD_MEM_DRAW ",
          "H5FD_MEM_GHEAP ",
          "H5FD_MEM_LHEAP ",
          "H5FD_MEM_OHDR "
        };
    const char * type_string = NULL;
    const char * tag_string = NULL;
    char write_num[16];
    char ** bufs = NULL;
    void * fsync_aioctlblk = NULL;
    void ** ctlblks = NULL;
    hbool_t show_progress = FALSE;
    int i;
    int j;
    int tag_len;
    int type_string_len;
    int op_num_len;
    herr_t result;

    if ( *pass_ptr ) {

        if ( ( file == NULL ) ||
             ( op_count <= 0  ) ||
             ( ops == NULL ) ||
             ( types == NULL ) ||
             ( offsets == NULL ) ||
             ( lengths == NULL ) ||
             ( tags == NULL ) ||
             ( failure_mssg_ptr == NULL ) ) {

	    *pass_ptr = FALSE;
            *failure_mssg_ptr = 
	        "bad param(s) passed to aio_multi_read_write_fsync_cancel_check()";
        }
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s:%d: allocating buffers.\n", fcn_name, *pass_ptr);

    if ( *pass_ptr ) { /* allocate arrays of pointers to */

        bufs = (char **)HDmalloc((size_t)op_count * sizeof(char *));
        ctlblks = (void **)HDmalloc((size_t)op_count * sizeof(void *));

        if ( ( bufs == NULL ) ||
             ( ctlblks == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "buffer allocation(s) failed(1).";

        } else {

            for ( i = 0; i < op_count; i++ ) {

                bufs[i] = NULL;
                ctlblks[i] = NULL;
            }
        }
    }

    if ( *pass_ptr ) { /* allocate and intialize buffers */

        i = 0;

        while ( ( *pass_ptr ) &&
                ( i < op_count ) ) {

            if ( lengths[i] <= 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "length[i] <= 16.";

            } else {

                bufs[i] = (char *)HDmalloc(lengths[i] * sizeof(char));

                if ( bufs[i] == NULL ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "buffer allocation(s) failed(2).";
                }
                else 
                {
                    sprintf(write_num, "%d: ", i);
                    op_num_len = (int)HDstrlen(write_num);

                    HDassert( op_num_len < 16 );

		    type_string = H5FD_mem_t_strings[types[i]];
		    type_string_len = (int)HDstrlen(type_string);

		    HDassert( type_string_len < 20 );

		    tag_string = tags[i];
		    tag_len = (int)HDstrlen(tag_string);

                    for ( j = 0; j < (int)lengths[i]; j++ ) {

			if ( j < op_num_len ) {

                            *(bufs[i] + j) = write_num[j];

                        } else if ( j < op_num_len + type_string_len ) {

                            *(bufs[i] + j) = type_string[j - op_num_len];

			} else if ( j < op_num_len + type_string_len + tag_len ) {

                            *(bufs[i] + j) = tag_string[j - op_num_len - type_string_len];

			} else {

                            *(bufs[i] + j) = '\0';
                        }
                    }
                }
            }

            i++;
        }
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: kicking off reads and writes.\n", fcn_name, *pass_ptr);

    /* kick off the reads and writes */
    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < op_count ) ) {

        switch( ops[i] ) {

            case READ_OP:
                result = H5FDaio_read(file, types[i], H5P_DEFAULT, 
                                       offsets[i], lengths[i], (void *)(bufs[i]), 
                                       &(ctlblks[i]));

                if ( ( result < 0 ) || ( ctlblks[i] == NULL ) ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_read(0) failed.";
                }
		break;

	    case WRITE_OP:
                result = H5FDaio_write(file, types[i], H5P_DEFAULT, 
                                       offsets[i], lengths[i], (void *)(bufs[i]), 
                                       &(ctlblks[i]));

                if ( ( result < 0 ) || ( ctlblks[i] == NULL ) ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_write(0) failed.";
                }
                break;

	    default:
                *pass_ptr = FALSE;
                *failure_mssg_ptr = "unknown op.";
		break;
	}

        i++;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: calling aio fsync.\n", fcn_name, *pass_ptr);

    /* do an aio_fsync */
    if ( *pass_ptr ) {

        result = H5FDaio_fsync(file, &fsync_aioctlblk);

        if ( ( result < 0 ) || ( fsync_aioctlblk ==  NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_fsync(0) failed.";
        }
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: canceling all aio read/write ops.\n", fcn_name, *pass_ptr);

    /* canceling the reads and writes */
    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < op_count ) ) {

        result = H5FDaio_cancel(file, ctlblks[i]);

	if ( result < 0 ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_cancel(0) failed.";
        }

        i++;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: canceling aio fsync op.\n", fcn_name, *pass_ptr);

    /* canceling the aio_fsync */
    if ( *pass_ptr ) {

        result = H5FDaio_cancel(file, fsync_aioctlblk);

	if ( result < 0 ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_cancel(1) failed.";
        }
    }

    /* discard the buffers */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: discarding buffers.\n", fcn_name, *pass_ptr);

    if ( bufs != NULL ) {

        for ( i = 0; i < op_count; i++ ) {

            if ( bufs[i] != NULL ) {

                HDfree(bufs[i]);
                bufs[i] = NULL;
            }
        }
        HDfree(bufs);
        bufs = NULL;
    }

    if ( ctlblks != NULL ) {

        HDfree(ctlblks);
        ctlblks = NULL;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: done.\n", fcn_name, *pass_ptr);

    return;

} /* aio_multi_read_write_fsync_cancel_check() */


/*-------------------------------------------------------------------------
 * Function:	aio_multi_write_sync_read_check
 *
 * Purpose:	Kick off the specified number of writes, each of the 
 *		the specified length, memory type, and address.  
 *
 *		Wait and/or test until the aio writes are complete, and
 *              call aio_fsync.
 *
 *		Then read the data back into memory, and verify that 
 *		we read the expected data.
 *
 *		Do nothing if *pass_ptr is FALSE on entry.
 *
 *		Set *pass_ptr to FALSE and set *failure_mssg_ptr if any
 *		error is detected.
 *
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 11/2/10
 * 
 *-------------------------------------------------------------------------
 */

static void
aio_multi_write_sync_read_check(H5FD_t * file,
				int write_count,
			        H5FD_mem_t types[],
				haddr_t offsets[],
				size_t lengths[],
				const char * tags[],
                                hbool_t * pass_ptr, 
                                const char ** failure_mssg_ptr)
{
    const char * fcn_name = "aio_multi_write_sync_read_check()";
    const char * H5FD_mem_t_strings[H5FD_MEM_NTYPES] =
        {
          "H5FD_MEM_DEFAULT ",
          "H5FD_MEM_SUPER ",
          "H5FD_MEM_BTREE ",
          "H5FD_MEM_DRAW ",
          "H5FD_MEM_GHEAP ",
          "H5FD_MEM_LHEAP ",
          "H5FD_MEM_OHDR "
        };
    const char * type_string = NULL;
    const char * tag_string = NULL;
    char write_num[16];
    char ** write_bufs = NULL;
    char ** read_bufs = NULL;
    void * fsync_aioctlblk = NULL;
    void ** ctlblks = NULL;
    hbool_t done;
    hbool_t do_wait;
    hbool_t show_progress = FALSE;
    int error_num;
    int i;
    int j;
    int tag_len;
    int type_string_len;
    int write_num_len;
    herr_t result;

    if ( *pass_ptr ) {

        if ( ( file == NULL ) ||
             ( write_count <= 0  ) ||
             ( types == NULL ) ||
             ( offsets == NULL ) ||
             ( lengths == NULL ) ||
             ( tags == NULL ) ||
             ( failure_mssg_ptr == NULL ) ) {

	    *pass_ptr = FALSE;
            *failure_mssg_ptr = 
	        "bad param(s) passed to aio_multi_write_sync_read_check()";
        }
    }

    if ( show_progress ) 
        HDfprintf(stdout, "%s:%d: allocating buffers.\n", fcn_name, *pass_ptr);

    if ( *pass_ptr ) { /* allocate arrays of pointers to */

        write_bufs = (char **)HDmalloc((size_t)write_count * sizeof(char *));
        read_bufs  = (char **)HDmalloc((size_t)write_count * sizeof(char *));
        ctlblks    = (void **)HDmalloc((size_t)write_count * sizeof(void *));

        if ( ( write_bufs == NULL ) ||
             ( read_bufs == NULL ) ||
             ( ctlblks == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "buffer allocation(s) failed(1).";

        } else {

            for ( i = 0; i < write_count; i++ ) {

                write_bufs[i] = NULL;
                read_bufs[i] = NULL;
                ctlblks[i] = NULL;
            }
        }
    }

    if ( *pass_ptr ) { /* allocate and intialize read and write buffers */

        i = 0;

        while ( ( *pass_ptr ) &&
                ( i < write_count ) ) {

            if ( lengths[i] <= 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "length[i] <= 16.";

            } else {

                write_bufs[i] = (char *)HDmalloc(lengths[i] * sizeof(char));
                read_bufs[i]  = (char *)HDmalloc(lengths[i] * sizeof(char));

                if ( ( write_bufs[i] == NULL ) ||
                     ( read_bufs[i] == NULL ) ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "buffer allocation(s) failed(2).";
                }
                else 
                {
                    sprintf(write_num, "%d: ", i);
                    write_num_len = (int)HDstrlen(write_num);

                    HDassert( write_num_len < 16 );

		    type_string = H5FD_mem_t_strings[types[i]];
		    type_string_len = (int)HDstrlen(type_string);

		    HDassert( type_string_len < 20 );

		    tag_string = tags[i];
		    tag_len = (int)HDstrlen(tag_string);

                    for ( j = 0; j < (int)lengths[i]; j++ ) {

			if ( j < write_num_len ) {

                            *(write_bufs[i] + j) = write_num[j];

                        } else if ( j < write_num_len + type_string_len ) {

                            *(write_bufs[i] + j) = 
				type_string[j - write_num_len];

			} else if ( j < write_num_len + 
                                        type_string_len + 
                                        tag_len ) {

                            *(write_bufs[i] + j) = 
				tag_string[j - write_num_len - type_string_len];

			} else {

                            *(write_bufs[i] + j) = '\0';
                        }
                    
                        *(read_bufs[i] + j) = '\0';
                    }
                }
            }

            i++;
        }
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: kicking off writes.\n", fcn_name, *pass_ptr);

    /* kick off the writes */
    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        result = H5FDaio_write(file, types[i], H5P_DEFAULT, 
                               offsets[i], lengths[i], (void *)(write_bufs[i]), 
                               &(ctlblks[i]));

        if ( ( result < 0 ) || ( ctlblks[i] == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_write(0) failed.";
        }

        i++;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: waiting for writes.\n", fcn_name, *pass_ptr);

    /* wait for the writes to complete -- test half the time, and wait the 
     * other half 
     */
    i = 0;
    do_wait = FALSE;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        if ( do_wait ) {

            if ( *pass_ptr ) {

                result = H5FDaio_wait(file, ctlblks[i]);

                if ( result < 0 ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_wait(0) failed.";
                }
            }
        } else {

            done = FALSE;
            while ( ( *pass_ptr ) && ( ! done ) ) {

                result = H5FDaio_test(file, &done, ctlblks[i]);

                if ( result < 0 ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_test(0) failed.";
                }
            }
        }

        do_wait = (hbool_t)(! do_wait);

        i++;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: finishing writes.\n", fcn_name, *pass_ptr);

    /* finish the writes */
    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        result = H5FDaio_finish(file, &error_num, ctlblks[i]);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_finish(0) failed.";
        }

        ctlblks[i] = NULL;

        i++;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: doing aio fsync.\n", fcn_name, *pass_ptr);

    /* do an aio_fsync */
    if ( *pass_ptr ) {

        result = H5FDaio_fsync(file, &fsync_aioctlblk);

        if ( ( result < 0 ) || ( fsync_aioctlblk ==  NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_fsync(0) failed.";
        }
    }

    if ( * pass_ptr ) {

        result = H5FDaio_wait(file, fsync_aioctlblk);

        if ( result < 0 ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_fsync(0) failed.";
        }
    }

    if ( * pass_ptr ) {

        result = H5FDaio_finish(file, &error_num, fsync_aioctlblk);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_finish(0) failed.";
        }
    }


    /* kick off the reads */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: starting reads.\n", fcn_name, *pass_ptr);

    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        result = H5FDaio_read(file, types[0], H5P_DEFAULT, 
                              offsets[i], lengths[i], (void *)(read_bufs[i]), 
                              &(ctlblks[i]));

        if ( ( result < 0 ) || ( ctlblks[i] == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_write(0) failed.";
        }

        i++;
    }

    /* wait for the reads to complete -- test half the time, and wait the 
     * other half 
     */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: waiting for reads.\n", fcn_name, *pass_ptr);

    i = 0;
    do_wait = TRUE;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        if ( do_wait ) {

            if ( *pass_ptr ) {

                result = H5FDaio_wait(file, ctlblks[i]);

                if ( result < 0 ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_wait(0) failed.";
                }
            }
        } else {

            done = FALSE;
            while ( ( *pass_ptr ) && ( ! done ) ) {

                result = H5FDaio_test(file, &done, ctlblks[i]);

                if ( result < 0 ) {

                    *pass_ptr = FALSE;
                    *failure_mssg_ptr = "H5FDaio_test(0) failed.";
                }
            }
        }

        do_wait = (hbool_t)(! do_wait);

        i++;
    }

    /* finish the reads */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: finishing reads.\n", fcn_name, *pass_ptr);

    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        result = H5FDaio_finish(file, &error_num, ctlblks[i]);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_finish(0) failed.";
        }

        ctlblks[i] = NULL;

        i++;
    }


    /* verify the reads */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: verifying reads.\n", fcn_name, *pass_ptr);

    i = 0;
    while ( ( *pass_ptr ) &&
            ( i < write_count ) ) {

        j = 0;
        while ( ( *pass_ptr ) &&
                ( j < (int)lengths[i] ) ) {

            if ( (write_bufs[i])[j] != (read_bufs[i])[j] ) {

                HDfprintf(stdout, "lengths[%d] = %d\n", i, lengths[i]);
                HDfprintf(stdout, "offsets[%d] = %lld\n", 
                          i, (long long)(offsets[i]));
                HDfprintf(stdout, "write_bufs[%d][%d] = %c\n", 
                          i, j, (write_bufs[i])[j]);
                HDfprintf(stdout, "read_bufs[%d][%d] = %c\n", 
                          i, j, (read_bufs[i])[j]);
                *pass_ptr = FALSE;
                *failure_mssg_ptr = "data mismatch(1).";
            }
            j++;
        }

        i++;
    }

    /* discard the buffers */

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: discarding buffers.\n", fcn_name, *pass_ptr);

    if ( write_bufs != NULL ) {

        for ( i = 0; i < write_count; i++ ) {

            if ( write_bufs[i] != NULL ) {

                HDfree(write_bufs[i]);
                write_bufs[i] = NULL;
            }
        }
        HDfree(write_bufs);
        write_bufs = NULL;
    }

    if ( read_bufs != NULL ) {

        for ( i = 0; i < write_count; i++ ) {

            if ( read_bufs[i] != NULL ) {

                HDfree(read_bufs[i]);
                read_bufs[i] = NULL;
            }
        }
        HDfree(write_bufs);
        write_bufs = NULL;
    }

    if ( ctlblks != NULL ) {

        HDfree(ctlblks);
        ctlblks = NULL;
    }

    if ( show_progress ) 
	HDfprintf(stdout, "%s:%d: done.\n", fcn_name, *pass_ptr);

    return;

} /* aio_multi_write_sync_read_check() */


/*-------------------------------------------------------------------------
 * Function:    aio_single_write_read_check()
 *
 * Purpose:     Kick of a single asynchronous write, wait until it is done
 *		(via either H5FDtest or H5FDwait, as directed), and then
 *		conplete the write.
 *
 *		Kick off a single asynchronous read of the data just 
 *		written, wait until it is done, (via either H5FDtest 
 *		or H5FDwait, as directed), and then complete the read.
 *
 *		Verify that the read buffer contains the expected data.
 *
 *		In the event of failure, set *pass_ptr to FALSE, and set
 *		*failure_mssg_ptr to an appropriate error message.
 *
 *		If *pass_ptr is FALSE on entry, do nothing and return.
 *
 * Return:      void.
 *
 * Programmer:  JRM -- 7/15/10
 *
 *-------------------------------------------------------------------------
 */

static void
aio_single_write_read_check(H5FD_t * file,
                            H5FD_mem_t type,
                            const char * tag_string,
                            haddr_t offset,
                            const size_t write_size,
                            hbool_t do_wait,
                            hbool_t * pass_ptr,
                            const char ** failure_mssg_ptr)
{
    const char * fcn_name = "aio_single_write_read_check()";
    const char * H5FD_mem_t_strings[H5FD_MEM_NTYPES] =
	{
	  "H5FD_MEM_DEFAULT",
	  "H5FD_MEM_SUPER",
	  "H5FD_MEM_BTREE",
	  "H5FD_MEM_DRAW",
	  "H5FD_MEM_GHEAP",
	  "H5FD_MEM_LHEAP",
 	  "H5FD_MEM_OHDR"
	};
    const char * type_string = NULL;
    char * write_buf = NULL;
    char * read_buf = NULL;
    hbool_t done;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int error_num;
    int cp = 0;
    int i;
    int type_string_len;
    int tag_len;
    herr_t result;
    void * aioctlblk_ptr = NULL;

    HDassert( ( type >= 0 ) && ( type < H5FD_MEM_NTYPES ) );

    if ( verbose ) {

        HDfprintf(stdout, "entering %s.\n", fcn_name);
        HDfprintf(stdout, "	file->driver_id = 0x%llx.\n",
		  (unsigned long long)(file->driver_id));
        if ( file->driver_id == H5FD_CORE ) {
            HDfprintf(stdout, "	file driver == CORE.\n");
        }
        if ( ( type >= 0 ) && ( type < H5FD_MEM_NTYPES ) ) {

            HDfprintf(stdout, "	type		= %d (\"%s\").\n", 
                      (int)type, H5FD_mem_t_strings[(int)type]);

        } else {

            HDfprintf(stdout, "	type		= %d (\?\?\?).\n", (int)type);
        }
        HDfprintf(stdout, "	tag_string      = \"%s\".\n", tag_string);
        HDfprintf(stdout, "	offset          = 0x%llx.\n", 
                  (unsigned long long)offset);
        HDfprintf(stdout, "	write_size      = 0x%llx.\n", 
                  (unsigned long long)write_size);
        HDfprintf(stdout, "	do_wait         = %d.\n", (int)do_wait);
        HDfprintf(stdout, "	*pass_ptr       = %d.\n", (int)(*pass_ptr));
    }

    if ( *pass_ptr ) {

        if ( ( file == NULL ) ||
             ( type < 0 ) ||
	     ( type >= H5FD_MEM_NTYPES ) ||
             ( tag_string == NULL ) ||
             ( write_size <= 0 ) ||
             ( HDstrlen(tag_string) > write_size ) ) {

	    *pass_ptr = FALSE;
            *failure_mssg_ptr = 
		"bad param(s) passed to aio_single_write_read_check()";
        }
    }

    if ( show_progress ) { /* cp == 0 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- alloc & init buffers.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( *pass_ptr ) { /* allocate and initialize buffers */

        write_buf = (char *)HDmalloc(write_size + 1);
        read_buf  = (char *)HDmalloc(write_size + 1);

        if ( ( write_buf == NULL ) ||
             ( read_buf == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "buffer allocation(s) failed.";

        } else {

            type_string = H5FD_mem_t_strings[type];

            HDassert( type_string != NULL );

            type_string_len = (int)HDstrlen(type_string);

	    HDassert( type_string_len < 20 );

            tag_len = (int)HDstrlen(tag_string);

            for ( i = 0; i < (int)write_size; i++ ) {

		if ( i < type_string_len ) {

                    write_buf[i] = type_string[i];

                } else if ( i == type_string_len ) {

                    write_buf[i] = ' ';

		} else if ( i < type_string_len + tag_len + 1 ) {

                    write_buf[i] = tag_string[i - type_string_len - 1];

                } else if ( i < (int)(write_size - 1) ) {

                    write_buf[i] = ' ';

                } else {

                    write_buf[i] = '\0';
                }
                
                read_buf[i] = '\0';
            }
        }
    }

    if ( show_progress ) { /* cp == 1 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- kicking off write.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( *pass_ptr ) {

        result = H5FDaio_write(file, type, H5P_DEFAULT, 
                               offset, write_size, (void *)(write_buf), 
                               &aioctlblk_ptr);

        if ( ( result < 0 ) || ( aioctlblk_ptr == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_write(0) failed.";
        }
    }

    if ( show_progress ) { /* cp == 2 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d, do_wait = %d -- waiting til done.\n", 
                  fcn_name, cp++, (int)(*pass_ptr), (int)do_wait);
    }

    if ( do_wait ) {

        if ( *pass_ptr ) {

            result = H5FDaio_wait(file, aioctlblk_ptr);

            if ( result < 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "H5FDaio_wait(1) failed.";
            }
        }
    } else {

        done = FALSE;
        while ( ( *pass_ptr ) && ( ! done ) ) {

            result = H5FDaio_test(file, &done, aioctlblk_ptr);

            if ( result < 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "H5FDaio_test(0) failed.";
            }
        }
    }

    if ( show_progress ) { /* cp == 3 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- finishing write.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( *pass_ptr ) {

        result = H5FDaio_finish(file, &error_num, aioctlblk_ptr);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_finish(0) failed.";
        }
    }

    if ( show_progress ) { /* cp == 4 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- kicking off read.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( *pass_ptr ) {

        aioctlblk_ptr = NULL;

        result = H5FDaio_read(file, type, H5P_DEFAULT,
                              offset, write_size, (void *)read_buf, 
                              &aioctlblk_ptr);

        if ( ( result < 0 ) || ( aioctlblk_ptr == NULL ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_read(0) failed.";
        }
    }

    if ( show_progress ) { /* cp == 5 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d, do_wait = %d -- waiting til done.\n", 
                  fcn_name, cp++, (int)(*pass_ptr), (int)do_wait);
    }

    if ( do_wait ) {

        if ( *pass_ptr ) {

            result = H5FDaio_wait(file, aioctlblk_ptr);

            if ( result < 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "H5FDaio_wait(1) failed.";
            }
        }
    } else {

        done = FALSE;
        while ( ( *pass_ptr ) && ( ! done ) ) {

            result = H5FDaio_test(file, &done, aioctlblk_ptr);

            if ( result < 0 ) {

                *pass_ptr = FALSE;
                *failure_mssg_ptr = "H5FDaio_test(1) failed.";
            }
        }
    }

    if ( show_progress ) { /* cp == 6 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- finishing read.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( *pass_ptr ) {

        result = H5FDaio_finish(file, &error_num, aioctlblk_ptr);

        if ( ( result < 0 ) || ( error_num != 0 ) ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "H5FDaio_finish(1) failed.";
        }
    }

    if ( show_progress ) { /* cp == 7 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- comparing buffers.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    i = 0;
    while ( ( *pass_ptr ) && ( i < (int)write_size ) ) {

        if ( read_buf[i] != write_buf[i] ) {

            *pass_ptr = FALSE;
            *failure_mssg_ptr = "data mismatch(1).";
        }
        i++;
    }

    if ( show_progress ) { /* cp == 8 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- discarding write buf.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( write_buf != NULL ) { /* must discard write buffer */

        HDfree(write_buf);
        write_buf = NULL;
    }

    if ( show_progress ) { /* cp == 9 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- discarding read buf.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( read_buf != NULL ) { /* must discard read buffer */

        HDfree(read_buf);
        read_buf = NULL;
    }

    if ( show_progress ) { /* cp == 10 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- done.\n", 
                  fcn_name, cp++, (int)(*pass_ptr));
    }

    if ( verbose ) {

        HDfprintf(stdout, "exiting %s.\n", fcn_name);
    }


    return;

} /* aio_single_write_read_check() */


/*-------------------------------------------------------------------------
 * Function:    generic_aio_test()
 *
 * Purpose:     Run a basic functionality test with the specified file
 *		and driver.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  JRM -- 7/15/10
 *
 *-------------------------------------------------------------------------
 */

static int
generic_aio_test(const char * test_banner, 
                 const int file_name_num,
		 hid_t fapl_id,
                 haddr_t maxaddr)
{
    const char * fcn_name = "generic_aio_test()";
    const char * failure_mssg = NULL;
    const char * tags[] = { "1 KB write",
                            "512 B write",
                            "16 B wrt",
		            "4 KB write",
                            "31 B write",
		            "1 MB write",
		            "4 MB write",
		            "1 MB + 1 B write",
		            "64 MB write",
                            "24 B write" };
    char file_name[1024];
    hbool_t pass = TRUE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    int ret_val = 0;
    int write_count = 10;
    int op_count = 10;
    int ops[] = { READ_OP,
		  WRITE_OP,
		  READ_OP,
		  WRITE_OP,
		  READ_OP,
                  WRITE_OP,
                  READ_OP,
                  WRITE_OP,
                  READ_OP,
                  WRITE_OP };
    H5FD_mem_t types[] = { H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_DRAW };
    size_t write_size;
    size_t lengths[] =  {  1024, 
                            512, 
                             16,
                           4096, 
                             31, 
                  (1024 * 1024), 
              (4 * 1024 * 1024), 
              (1024 * 1024 + 1), 
              (64 * 1024 *1024), 
                            24};
    herr_t result;
    haddr_t offset;
    haddr_t offsets[] = {(haddr_t)0, 
                         (haddr_t)1024, 
                         (haddr_t)1536, 
                         (haddr_t)1552,
                         (haddr_t)5648, 
                         (haddr_t)5679, 
                         (haddr_t)1054255,
                         (haddr_t)5248559,
			 (haddr_t)6297136,
                         (haddr_t)73406000};
    H5FD_t * file;

    if ( verbose ) {

        HDfprintf(stdout, 
                  "entering generic_aio_test(\"%s\", %d(\"%s\"), %d, 0x%llx)\n",
                  test_banner, file_name_num, FILENAME[file_name_num],
                  (int)fapl_id, (unsigned long long)maxaddr);
    }

    TESTING(test_banner);

    if ( show_progress ) { /* cp == 0 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- fixing file name.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* setup the file name */
    if ( pass ) {

        if ( NULL == h5_fixname(FILENAME[file_name_num], fapl_id, 
                                file_name, sizeof(file_name)) ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.";
        }
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s: file_name = \"%s\".\n", fcn_name, file_name);
    }

    if ( show_progress ) { /* cp == 1 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- opening file.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* create the file */
    if ( pass ) {

        file = H5FDopen(file_name, (H5F_ACC_RDWR | H5F_ACC_CREAT), 
                        fapl_id, maxaddr);

        if ( file == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";
        }
    }

    if ( show_progress ) { /* cp == 2 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- setting eoa.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* set the EOA */
    if ( pass ) { 

        result = H5FDset_eoa(file, H5FD_MEM_DEFAULT, maxaddr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }
    }

    if ( show_progress ) { /* cp == 3 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 bytes, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* first do some simple write, read back, and compare results tests with 
     * buffers of various sizes
     */

    offset = (haddr_t)0;
    write_size = (size_t)64;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "64 bytes -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 4 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 bytes, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "64 bytes -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 5 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 bytes, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "256 bytes -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 6 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 bytes, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "256 bytes -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 7 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 KB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "1 KB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 8 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 KB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "1 KB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 9 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 4 KB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "4 KB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 10 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 4 KB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "4 KB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 11 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 KB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "64 KB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 12 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 KB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "64 KB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 13 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 MB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "1 MB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 14 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 MB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "1 MB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 15 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 16 MB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "16 MB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 16 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 16 MB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "16 MB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 17 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 MB, poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "256 MB -- test for completion",
                                offset,
                                write_size,
                                /* do_wait = */ FALSE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 18 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 MB, wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    aio_single_write_read_check(file,
                                H5FD_MEM_DRAW,
                                "256 MB -- wait for completion",
                                offset,
                                write_size,
                                /* do_wait = */ TRUE,
                                &pass,
                                &failure_mssg);

    if ( show_progress ) { /* cp == 19 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- mwsrchk.\n", 
                  fcn_name, cp++, (int)pass);
    }

    aio_multi_write_sync_read_check(file,
				    write_count,
			            types,
				    offsets,
				    lengths,
				    tags,
                                    &pass,
                                    &failure_mssg);

    if ( show_progress ) { /* cp == 20 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- wrfcchk.\n", 
                  fcn_name, cp++, (int)pass);
    }
#if 1 /* JRM */
    aio_multi_read_write_fsync_cancel_check(file,
				            op_count,
                                            ops,
			                    types,
				            offsets,
				            lengths,
				            tags,
                                            &pass, 
                                            &failure_mssg);
#endif /* JRM */
    if ( show_progress ) { /* cp == 21 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- close file.\n", 
                  fcn_name, cp++, (int)pass);
    }

    if ( file != NULL ) {

        result = H5FDclose(file);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDclose() failed.";

        } else if ( h5_cleanup(FILENAME, fapl_id) == 0 ) {

            pass = FALSE;
            failure_mssg = "h5_cleanup() failed.\n";
        }
    }

    if ( show_progress ) { /* cp == 22 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- report results.\n", 
                  fcn_name, cp++, (int)pass);
    }

    if ( pass ) { 

	PASSED(); 

    } else { 

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
	H5_FAILED(); 
        ret_val = -1;
    }

    if ( verbose ) {

        HDfprintf(stdout, "exiting generic_aio_test() -- ret_val == %d.\n",
                  ret_val);
    }

    return(ret_val);

} /* generic_aio_test() */


/*-------------------------------------------------------------------------
 * Function:    generic_aio_input_error_tests()
 *
 * Purpose:     Run a basic set of input error rejection tests on the 
 *		various AIO VFD calls on the driver specified by the 
 *		supplied FAPL.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  JRM -- 2/21/11
 *
 *-------------------------------------------------------------------------
 */

static int
generic_aio_input_error_tests(const char * test_banner,
                              const char * tag_string,
                              const int file_name_num,
		              hid_t fapl_id,
                              hbool_t verbose)
{
    const char * fcn_name = "generic_aio_input_error_tests()";
    const char * failure_mssg = NULL;
    const char * type_string = NULL;
    char * write_buf = NULL;
    char * read_buf = NULL;
    char file_name[1024];
    hbool_t done;
    hbool_t pass = TRUE;
    hbool_t show_progress = FALSE;
    int cp = 0;
    int error_num;
    int i;
    int ret_val = 0;
    int type_string_len;
    int tag_len;
    const size_t write_size = 0x100;
    herr_t result;
    haddr_t maxaddr = 0x1000;
    H5F_mem_t mem_type;
    H5FD_t * file;
    void * fsync_ctlblk_ptr = NULL;
    void * write_ctlblk_ptr = NULL;
    void * read_ctlblk_ptr = NULL;

    if ( verbose ) {

        HDfprintf(stdout, 
                  "entering generic_aio_input_error_tests(\"%s\", %d(\"%s\"), %d, %d)\n",
                  test_banner, file_name_num, FILENAME[file_name_num],
                  (int)fapl_id, (int)verbose);
    }

    TESTING(test_banner);

    if ( show_progress ) { /* cp == 0 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }

    /* setup the file name */
    if ( pass ) {

        if ( NULL == h5_fixname(FILENAME[file_name_num], fapl_id, 
                                file_name, sizeof(file_name)) ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.";
        }
    }

    if ( verbose ) {

        HDfprintf(stdout, "%s: file_name = \"%s\".\n", fcn_name, file_name);
    }

    if ( show_progress ) { /* cp == 1 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }

    /* create the file */
    if ( pass ) {

        file = H5FDopen(file_name, (H5F_ACC_RDWR | H5F_ACC_CREAT), 
                        fapl_id, maxaddr);

        if ( file == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";
        }
    }

    if ( show_progress ) { /* cp == 2 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }

    /* set the EOA */
    if ( pass ) { 

        result = H5FDset_eoa(file, H5FD_MEM_DEFAULT, maxaddr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }
    }

    if ( show_progress ) { /* cp == 3 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }

    /* in what follows, we will verify that various AIO calls reject invalid 
     * input in the expected manner.  However, before we do so, we must construct
     * some valid inputs.  Do this now.
     */

    if ( pass ) { /* allocate and initialize buffers, initialize mem_type */

        if ( verbose ) {

            HDfprintf(stdout, "%s: allocating write_buf = (char *)HDmalloc(%d).\n",
                      fcn_name, (int)(write_size + 1));
        }

        write_buf = (char *)HDmalloc(write_size + 1);

        if ( verbose ) {

            HDfprintf(stdout, "%s: allocating read_buf = (char *)HDmalloc(%d).\n",
                      fcn_name, (int)(write_size + 1));
        }

        read_buf  = (char *)HDmalloc(write_size + 1);

        if ( ( write_buf == NULL ) ||
             ( read_buf == NULL ) ) {

            pass = FALSE;
            failure_mssg = "buffer allocation(s) failed.";

        } else {

            mem_type = H5FD_MEM_DEFAULT;

            type_string = "H5FD_MEM_DEFAULT";

            HDassert( type_string != NULL );

            type_string_len = (int)HDstrlen(type_string);

            HDassert( type_string_len < 20 );

            tag_len = (int)HDstrlen(tag_string);

            for ( i = 0; i < (int)write_size; i++ ) {

                if ( i < type_string_len ) {

                    write_buf[i] = type_string[i];

                } else if ( i == type_string_len ) {

                    write_buf[i] = ' ';

                } else if ( i < type_string_len + tag_len + 1 ) {

                    write_buf[i] = tag_string[i - type_string_len - 1];

                } else {

                    write_buf[i] = '\0';
                }

                read_buf[i] = '\0';
            }
        }
    }

    if ( show_progress ) { /* cp == 4 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)(pass));
    }

    /* now make several attempts to queue an asynchronous write with invalid input 
     * of some sort or another.  All such calls should be rejected.
     */

    /* try to queue a write with a NULL file */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(NULL, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x000, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with NULL file.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(0)";
        }
    }

    if ( show_progress ) { /* cp == 5 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with an invalid memory type */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_NTYPES, H5P_DEFAULT, (haddr_t)0x000, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with invalid memory type.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(1)";
        }
    }

    if ( show_progress ) { /* cp == 6 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with an invalid dxpl -- do this by passing in a fapl ID 
     * instead.
     */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, fapl_id, (haddr_t)0x000, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with invalid dxpl.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(2)";
        }
    }

    if ( show_progress ) { /* cp == 7 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with an invalid address */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, HADDR_UNDEF, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with invalid address.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(3)";
        }
    }

    if ( show_progress ) { /* cp == 8 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with an address beyond the eoa */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, maxaddr + 1, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with address beyond eoa.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(4)";
        }
    }

    if ( show_progress ) { /* cp == 9 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with zero size */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                                   0, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with zero size.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(5)";
        }
    }

    if ( show_progress ) { /* cp == 10 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with address + size greater than eoa */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, 
                                   maxaddr - (write_size / 2), write_size, 
                                   (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with addr + size > eoa.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(6)";
        }
    }

    if ( show_progress ) { /* cp == 11 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with a NULL buffer pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                                   write_size, (void *)(NULL), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with NULL buffer pointer.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(7)";
        }
    }

    if ( show_progress ) { /* cp == 12 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with a NULL control block pointer pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                                   write_size, (void *)(write_buf), NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with NULL ctlblk_ptr_ptr.";

	} else if ( write_ctlblk_ptr != NULL ) {

	    pass = FALSE;
            failure_mssg = "write_ctlblk_ptr != NULL after failed call to H5FDaio_write(8)";
        }
    }

    if ( show_progress ) { /* cp == 13 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a write with *ctlblk_ptr_ptr != NULL */
    if ( pass ) {

        write_ctlblk_ptr = (void *)read_buf;

        H5E_BEGIN_TRY {
            result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                                   write_size, (void *)(write_buf), &write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_write() succeeded with *ctlblk_ptr_ptr != NULL.";

        }

        write_ctlblk_ptr = NULL;
    }

    if ( show_progress ) { /* cp == 14 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* now go ahead and queue the write successfully, as we will need an operation
     * in progress to test H5FDaio_test() and H5FDaio_wait().
     */
    if ( pass ) {

        result = H5FDaio_write(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                               write_size, (void *)(write_buf), &write_ctlblk_ptr);

        if ( ( result < 0 ) || ( write_ctlblk_ptr == NULL ) ) {

            pass = FALSE;
            failure_mssg = "valid call to H5FDaio_write() failed.";
        }
    }

    if ( show_progress ) { /* cp == 15 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }



    /* Make several calls to H5FDaio_test() with invalid input of some sort.
     * All such calls should fail.
     */

    /* try to test the status of the write using a NULL file pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_test(NULL, &done, write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_test() succeeded with NULL file.";
        }
    }

    if ( show_progress ) { /* cp == 16 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to test the status of the write using a NULL done_ptr */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_test(file, NULL, write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_test() succeeded with NULL done pointer.";
        }
    }

    if ( show_progress ) { /* cp == 17 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to test the status of the write using a NULL control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_test(file, &done, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_test() succeeded with NULL ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 18 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to test the status of the write using an invalid control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_test(file, &done, (void *)read_buf);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_test() succeeded with invalid ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 19 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* Now make several calls to H5FDaio_wait() with invalid input of some sort.
     * All such calls should fail.
     */

    /* try to do a wait with a NULL file pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_wait(NULL, write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_wait() succeeded with NULL file.";
        }
    }

    if ( show_progress ) { /* cp == 20 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to do a wait with a NULL control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_wait(file, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_wait() succeeded with NULL ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 21 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to do a wait with an invalid control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_wait(file, (void *)read_buf);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_wait() succeeded with invalid ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 22 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* finally, do a valid call to H5FDaio_wait() to ensure that the write has
     * completed, as we will need a completed write for our tests of H5FDaio_finish().
     */
    if ( pass ) {

        result = H5FDaio_wait(file, write_ctlblk_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "valid call H5FDaio_wait() failed.";
        }
    }

    if ( show_progress ) { /* cp == 23 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }




    /* Testing H5FDaio_finish() is a bit difficult, as in general, the function
     * should fail if the target operation is not complete when H5FDaio_finish()
     * is called.  Unfortunately, given the vaguries of AIO implementations (or
     * lack of same) it will be difficult to reliably provide an operation that 
     * is in progress to test this error.  Thus for now, we will neglect this 
     * issue.
     *
     * Hence, in the following tests we will make several calls to H5FDaio_finish()
     * with other invalid input.  Needless to say, all of these calls should fail.
     */

    /* try to finish the write with a NULL file pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_finish(NULL, &error_num, write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_finish() succeeded with NULL file.";
        }
    }

    if ( show_progress ) { /* cp == 24 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to finish the write with a NULL errno_ptr */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_finish(file, NULL, write_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_finish() succeeded with NULL errno_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 25 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to finish the write with a NULL control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_finish(file, &error_num, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_finish() succeeded with NULL ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 26 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to finish the write with an invalid control block pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_finish(file, &error_num, (void *)read_buf);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_finish() succeeded with an invalid ctlblk_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 27 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* finally, finish the write properly */
    if ( pass ) {

        result = H5FDaio_finish(file, &error_num, write_ctlblk_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "Valid call to H5FDaio_finish() failed for write.";

        } else if ( error_num != 0 ) {

            pass = FALSE;
            failure_mssg = "AIO write failed.";

        }
    }

    if ( show_progress ) { /* cp == 28 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }



    /* Next, make several attempts to queue an asynchonous fsync with invalid input
     * of some sort.  All attempts should fail. 
     */

    /* try to queue an AIO fsync with a NULL file pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_fsync(NULL, &fsync_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_fsync() succeeded with NULL file.";
        }
    }

    if ( show_progress ) { /* cp == 29 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue an AIO fsync with a NULL control block pointer pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_fsync(file, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_fsync() succeeded with NULL ctlblk_ptr_ptr.";
        }
    }

    if ( show_progress ) { /* cp == 30 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue an AIO fsync with *ctlblk_ptr_ptr != NULL */
    if ( pass ) {

        fsync_ctlblk_ptr = (void *)read_buf;

        H5E_BEGIN_TRY {
            result = H5FDaio_fsync(NULL, &fsync_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_fsync() succeeded with *ctlblk_ptr_ptr != NULL.";
        }
    }

    if ( show_progress ) { /* cp == 31 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }



    /* Next, make several attempts to queue an AIO read with invalid input of some 
     * sort -- all attempts should fail.
     */

    /* try to queue a read with a NULL file */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(NULL, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x000, 
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with NULL file.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(0)";
        }
    }

    if ( show_progress ) { /* cp == 32 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with an invalid memory type */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_NTYPES, H5P_DEFAULT, (haddr_t)0x000,
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with invalid memory type.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(1)";
        }
    }

    if ( show_progress ) { /* cp == 33 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with an invalid dxpl -- do this by passing in a fapl ID
     * instead.
     */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, fapl_id, (haddr_t)0x000, 
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with invalid memory type.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(2)";
        }
    }

    if ( show_progress ) { /* cp == 34 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with an invalid address */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, HADDR_UNDEF, 
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with invalid address.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(3)";
        }
    }

    if ( show_progress ) { /* cp == 35 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with an address beyond the eoa */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, maxaddr + 1, 
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with address beyond eoa.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(4)";
        }
    }

    if ( show_progress ) { /* cp == 36 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with zero size */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000, 
                                  0, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with zero size.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(5)";
        }
    }

    if ( show_progress ) { /* cp == 37 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with address + size greater than eoa */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, 
                                  maxaddr - (write_size / 2),  write_size,
                                  (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with addr + size > eoa.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(6)";
        }
    }

    if ( show_progress ) { /* cp == 38 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with a NULL buffer pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000,
                                  write_size, (void *)(NULL), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with NULL buffer pointer.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(7)";
        }
    }

    if ( show_progress ) { /* cp == 39 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with a NULL control block pointer pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000,
                                  write_size, (void *)(read_buf), NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with NULL ctlblk_ptr_ptr.";

        } else if ( read_ctlblk_ptr != NULL ) {

            pass = FALSE;
            failure_mssg = "read_ctlblk_ptr != NULL after failed call to H5FDaio_read(8)";
        }
    }

    if ( show_progress ) { /* cp == 40 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to queue a read with *ctlblk_ptr_ptr != NULL */
    if ( pass ) {

        read_ctlblk_ptr = (void *)read_buf;

        H5E_BEGIN_TRY {
            result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000,
                                  write_size, (void *)(read_buf), &read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_read() succeeded with *ctlblk_ptr_ptr != NULL.";
        }

        read_ctlblk_ptr = NULL;
    }

    if ( show_progress ) { /* cp == 41 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* go ahead and queue the read successfully, as we will need an operation
     * (possibly) in progress to test H5FDaio_cancel()
     */
    if ( pass ) {

        result = H5FDaio_read(file, H5FD_MEM_DEFAULT, H5P_DEFAULT, (haddr_t)0x0000,
                              write_size, (void *)(read_buf), &read_ctlblk_ptr);

        if ( ( result < 0 ) || ( read_ctlblk_ptr == NULL ) ) {

            pass = FALSE;
            failure_mssg = "valid call to H5FDaio_read() failed.";
        }
    }

    if ( show_progress ) { /* cp == 42 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* Now, make several calls to H5FDaio_cancel() with invalid input of some sort. */

    /* try to cancel the read with a NULL file pointer */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_cancel(NULL, read_ctlblk_ptr);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_cancel() succeeded with NULL file.";

        }
    }

    if ( show_progress ) { /* cp == 43 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to cancel the read with a NULL control block ptr */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_cancel(file, NULL);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_cancel() succeeded with NULL ctlblk_ptr.";

        }
    }

    if ( show_progress ) { /* cp == 44 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* try to cancel the read with an invalid control block ptr */
    if ( pass ) {

        H5E_BEGIN_TRY {
            result = H5FDaio_cancel(file, (void *)write_buf);
        } H5E_END_TRY;

        if ( result >= 0 ) {

            pass = FALSE;
            failure_mssg = "Call to H5FDaio_cancel() succeeded with invalid ctlblk_ptr.";

        }
    }

    if ( show_progress ) { /* cp == 45 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    /* finally, try to cancel the read with valid input -- should succeed */
    if ( pass ) {

        result = H5FDaio_cancel(file, read_ctlblk_ptr);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "Valid call to H5FDaio_cancel() failed.";

        }
    }

    if ( show_progress ) { /* cp == 46 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }


    if ( file != NULL ) {

        result = H5FDclose(file);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDclose() failed.";

        } else if ( h5_cleanup(FILENAME, fapl_id) == 0 ) {

            pass = FALSE;
            failure_mssg = "h5_cleanup() failed.\n";
        }
    }

    if ( show_progress ) { /* cp == 47 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d.\n", fcn_name, cp++, (int)pass);
    }

    if ( pass ) { 

	PASSED(); 

    } else { 

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
	H5_FAILED(); 
        ret_val = -1;
    }

    if ( verbose ) {

        HDfprintf(stdout, "exiting generic_aio_input_error_tests() -- ret_val == %d.\n",
                  ret_val);
    }

    return(ret_val);

} /* generic_aio_input_error_tests() */


/*-------------------------------------------------------------------------
 * Function:    multi_file_driver_aio_test()
 *
 * Purpose:     On most file drivers, the type of the data written is 
 *		largely irrelevant.  However, for the multi file driver,
 *		the type of the data indicates the file to which the 
 *		data is written, and also specifies a corrected offset
 *		in the target file.
 *
 *		Thus the generic aio tests are not sufficient here --
 *		we must write all types of data, and verify that they
 *		are directed properly.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  JRM -- 10/21/10
 *
 *-------------------------------------------------------------------------
 */

#define TYPE_SLICE ((haddr_t)0x24000000LL)

static int
multi_file_driver_aio_test(const char * test_banner, 
                           const int file_name_num)
{
    const char * fcn_name = "multi_file_driver_aio_test()";
    const char * memb_name[H5FD_MEM_NTYPES];
    const char * (type_names[H5FD_MEM_NTYPES]) =
        {
            "H5FD_MEM_DEFAULT",
            "H5FD_MEM_SUPER",
            "H5FD_MEM_BTREE",
            "H5FD_MEM_DRAW",
            "H5FD_MEM_GHEAP",
            "H5FD_MEM_LHEAP",
            "H5FD_MEM_OHDR"
        };
    const char * failure_mssg = NULL;
    const char * tags[] = {
		            "1 KB write",
		            "1 KB write",
		            "1 KB write",
		            "1 KB write",
		            "1 KB write",
		            "1 KB write",

                            "512 B write",
                            "512 B write",
                            "512 B write",
                            "512 B write",
                            "512 B write",
                            "512 B write",
     
                            "16 B wrt",
                            "16 B wrt",
                            "16 B wrt",
                            "16 B wrt",
                            "16 B wrt",
                            "16 B wrt",
     
                            "4 KB write",
                            "4 KB write",
                            "4 KB write",
                            "4 KB write",
                            "4 KB write",
                            "4 KB write",
     
                            "31 B write",
                            "31 B write",
                            "31 B write",
                            "31 B write",
                            "31 B write",
                            "31 B write",
     
                            "1 MB write",
                            "1 MB write",
                            "1 MB write",
                            "1 MB write",
                            "1 MB write",
                            "1 MB write",
     
                            "4 MB write",
                            "4 MB write",
                            "4 MB write",
                            "4 MB write",
                            "4 MB write",
                            "4 MB write",
     
                            "1 MB + 1 B write",
                            "1 MB + 1 B write",
                            "1 MB + 1 B write",
                            "1 MB + 1 B write",
                            "1 MB + 1 B write",
                            "1 MB + 1 B write",
     
                            "64 MB write",
                            "64 MB write",
                            "64 MB write",
                            "64 MB write",
                            "64 MB write",
                            "64 MB write",
     
                            "24 B write",
                            "24 B write",
                            "24 B write",
                            "24 B write",
                            "24 B write",
                            "24 B write",
                          };
    char file_name[1024];
    hbool_t pass = TRUE;
    hbool_t show_progress = FALSE;
    hbool_t verbose = FALSE;
    int cp = 0;
    int i;
    int ret_val = 0;
    int write_count = 60;
    int op_count = 60;
    int ops[] = { READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,
                  WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP,
		  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,
                  WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP,
		  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,
                  WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP,
		  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,
                  WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP,
		  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP,  READ_OP, 
                  WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP, WRITE_OP };
    hid_t       fapl_id;
    hid_t       memb_fapl[H5FD_MEM_NTYPES];
    H5FD_mem_t types[] = { 
			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,

			   H5FD_MEM_SUPER,
			   H5FD_MEM_BTREE,
                           H5FD_MEM_DRAW,
                           H5FD_MEM_GHEAP,
                           H5FD_MEM_LHEAP,
                           H5FD_MEM_OHDR,
                         };
    size_t write_size;
    size_t lengths[] =  
        {  
                           1024, 
                           1024, 
                           1024, 
                           1024, 
                           1024, 
                           1024, 

                            512, 
                            512, 
                            512, 
                            512, 
                            512, 
                            512, 

                             16,
                             16,
                             16,
                             16,
                             16,
                             16,

                           4096,
                           4096,
                           4096,
                           4096,
                           4096,
                           4096,

                             31,
                             31,
                             31,
                             31,
                             31,
                             31,

                  (1024 * 1024),
                  (1024 * 1024),
                  (1024 * 1024),
                  (1024 * 1024),
                  (1024 * 1024),
                  (1024 * 1024),

              (4 * 1024 * 1024),
              (4 * 1024 * 1024),
              (4 * 1024 * 1024),
              (4 * 1024 * 1024),
              (4 * 1024 * 1024),
              (4 * 1024 * 1024),

              (1024 * 1024 + 1),
              (1024 * 1024 + 1),
              (1024 * 1024 + 1),
              (1024 * 1024 + 1),
              (1024 * 1024 + 1),
              (1024 * 1024 + 1),

              (64 * 1024 *1024), 
              (64 * 1024 *1024), 
              (64 * 1024 *1024), 
              (64 * 1024 *1024), 
              (64 * 1024 *1024), 
              (64 * 1024 *1024), 

                            24,
                            24,
                            24,
                            24,
                            24,
                            24,
        };
    herr_t result;
    haddr_t eoa;
    haddr_t max_addr;
    haddr_t memb_addr[H5FD_MEM_NTYPES];
    haddr_t offset;
    haddr_t offsets[] = {
                          (haddr_t)(0), 
                          (haddr_t)(0 + (1 * TYPE_SLICE)), 
                          (haddr_t)(0 + (2 * TYPE_SLICE)), 
                          (haddr_t)(0 + (3 * TYPE_SLICE)), 
                          (haddr_t)(0 + (4 * TYPE_SLICE)), 
                          (haddr_t)(0 + (5 * TYPE_SLICE)),

                          (haddr_t)(1024), 
                          (haddr_t)(1024 + (1 * TYPE_SLICE)), 
                          (haddr_t)(1024 + (2 * TYPE_SLICE)), 
                          (haddr_t)(1024 + (3 * TYPE_SLICE)), 
                          (haddr_t)(1024 + (4 * TYPE_SLICE)), 
                          (haddr_t)(1024 + (5 * TYPE_SLICE)),

                          (haddr_t)(1536), 
                          (haddr_t)(1536 + (1 * TYPE_SLICE)), 
                          (haddr_t)(1536 + (2 * TYPE_SLICE)), 
                          (haddr_t)(1536 + (3 * TYPE_SLICE)), 
                          (haddr_t)(1536 + (4 * TYPE_SLICE)), 
                          (haddr_t)(1536 + (5 * TYPE_SLICE)),

                          (haddr_t)(1552), 
                          (haddr_t)(1552 + (1 * TYPE_SLICE)), 
                          (haddr_t)(1552 + (2 * TYPE_SLICE)), 
                          (haddr_t)(1552 + (3 * TYPE_SLICE)), 
                          (haddr_t)(1552 + (4 * TYPE_SLICE)), 
                          (haddr_t)(1552 + (5 * TYPE_SLICE)),

                          (haddr_t)(5648), 
                          (haddr_t)(5648 + (1 * TYPE_SLICE)), 
                          (haddr_t)(5648 + (2 * TYPE_SLICE)), 
                          (haddr_t)(5648 + (3 * TYPE_SLICE)), 
                          (haddr_t)(5648 + (4 * TYPE_SLICE)), 
                          (haddr_t)(5648 + (5 * TYPE_SLICE)),

                          (haddr_t)(5679), 
                          (haddr_t)(5679 + (1 * TYPE_SLICE)), 
                          (haddr_t)(5679 + (2 * TYPE_SLICE)), 
                          (haddr_t)(5679 + (3 * TYPE_SLICE)), 
                          (haddr_t)(5679 + (4 * TYPE_SLICE)), 
                          (haddr_t)(5679 + (5 * TYPE_SLICE)),

                          (haddr_t)(1054255), 
                          (haddr_t)(1054255 + (1 * TYPE_SLICE)), 
                          (haddr_t)(1054255 + (2 * TYPE_SLICE)), 
                          (haddr_t)(1054255 + (3 * TYPE_SLICE)), 
                          (haddr_t)(1054255 + (4 * TYPE_SLICE)), 
                          (haddr_t)(1054255 + (5 * TYPE_SLICE)),

                          (haddr_t)(5248559), 
                          (haddr_t)(5248559 + (1 * TYPE_SLICE)), 
                          (haddr_t)(5248559 + (2 * TYPE_SLICE)), 
                          (haddr_t)(5248559 + (3 * TYPE_SLICE)), 
                          (haddr_t)(5248559 + (4 * TYPE_SLICE)), 
                          (haddr_t)(5248559 + (5 * TYPE_SLICE)),

                          (haddr_t)(6297136), 
                          (haddr_t)(6297136 + (1 * TYPE_SLICE)), 
                          (haddr_t)(6297136 + (2 * TYPE_SLICE)), 
                          (haddr_t)(6297136 + (3 * TYPE_SLICE)), 
                          (haddr_t)(6297136 + (4 * TYPE_SLICE)), 
                          (haddr_t)(6297136 + (5 * TYPE_SLICE)),

                          (haddr_t)(73406000), 
                          (haddr_t)(73406000 + (1 * TYPE_SLICE)), 
                          (haddr_t)(73406000 + (2 * TYPE_SLICE)), 
                          (haddr_t)(73406000 + (3 * TYPE_SLICE)), 
                          (haddr_t)(73406000 + (4 * TYPE_SLICE)), 
                          (haddr_t)(73406000 + (5 * TYPE_SLICE)),
                        };
    haddr_t type_offset;
    H5FD_mem_t  mt;
    H5FD_mem_t  memb_map[H5FD_MEM_NTYPES];
    H5FD_t * file;

    if ( verbose ) {

        HDfprintf(stdout, 
                  "entering multi_file_driver_aio_test(\"%s\", %d(\"%s\"))\n",
                  test_banner, file_name_num, FILENAME[file_name_num]);
    }

    TESTING(test_banner);

    if ( show_progress ) { /* cp == 0 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- setting up fapl.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* setup the fapl -- this is somewhat involved fot the multi file driver */
    if ( pass ) {

	for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

            memb_addr[mt] = HADDR_UNDEF;
            memb_fapl[mt] = H5P_DEFAULT;
            memb_map[mt]  = H5FD_MEM_DRAW;
            memb_name[mt] = NULL;
        }

        memb_map[H5FD_MEM_SUPER]  = H5FD_MEM_SUPER;
        memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
        memb_name[H5FD_MEM_SUPER] = "%s-s.h5";
        memb_addr[H5FD_MEM_SUPER] = 0;

        memb_map[H5FD_MEM_BTREE]  = H5FD_MEM_BTREE;
        memb_fapl[H5FD_MEM_BTREE] = H5P_DEFAULT;
        memb_name[H5FD_MEM_BTREE] = "%s-b.h5";
        memb_addr[H5FD_MEM_BTREE] = memb_addr[H5FD_MEM_SUPER] + TYPE_SLICE;

        memb_map[H5FD_MEM_DRAW]   = H5FD_MEM_DRAW;
        memb_fapl[H5FD_MEM_DRAW]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_DRAW]  = "%s-r.h5";
        memb_addr[H5FD_MEM_DRAW]  =  memb_addr[H5FD_MEM_BTREE] + TYPE_SLICE;

        memb_map[H5FD_MEM_GHEAP]  = H5FD_MEM_GHEAP;
        memb_fapl[H5FD_MEM_GHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_GHEAP] = "%s-g.h5";
        memb_addr[H5FD_MEM_GHEAP] = memb_addr[H5FD_MEM_DRAW] + TYPE_SLICE;

        memb_map[H5FD_MEM_LHEAP]  = H5FD_MEM_LHEAP;
        memb_fapl[H5FD_MEM_LHEAP] = H5P_DEFAULT;
        memb_name[H5FD_MEM_LHEAP] = "%s-l.h5";
        memb_addr[H5FD_MEM_LHEAP] = memb_addr[H5FD_MEM_GHEAP] + TYPE_SLICE;

        memb_map[H5FD_MEM_OHDR]   = H5FD_MEM_OHDR;
        memb_fapl[H5FD_MEM_OHDR]  = H5P_DEFAULT;
        memb_name[H5FD_MEM_OHDR]  = "%s-o.h5";
        memb_addr[H5FD_MEM_OHDR]  = memb_addr[H5FD_MEM_LHEAP] + TYPE_SLICE;

	max_addr = memb_addr[H5FD_MEM_OHDR] + TYPE_SLICE;

        fapl_id = h5_fileaccess();

        if ( H5Pset_fapl_multi(fapl_id, memb_map, memb_fapl, memb_name,
                               memb_addr, FALSE) < 0 ) {

            pass = FALSE;
            failure_mssg = "H5Pset_fapl_multi() failed.";

        }
    }
 

    if ( show_progress ) { /* cp == 1 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- fixing name.\n", 
                  fcn_name, cp++, (int)pass);
    }


    /* setup the file name */
    if ( pass ) {

        if ( NULL == h5_fixname(FILENAME[file_name_num], fapl_id, 
                                file_name, sizeof(file_name)) ) {

            pass = FALSE;
            failure_mssg = "h5_fixname() failed.";
        }
    }

    if ( show_progress ) { /* cp == 2 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- opening file.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* create the file */
    if ( pass ) {

        file = H5FDopen(file_name, (H5F_ACC_RDWR | H5F_ACC_CREAT), 
                        fapl_id, max_addr);

        if ( file == NULL ) {

            pass = FALSE;
            failure_mssg = "H5FDopen() failed.";
        }
    }

    if ( show_progress ) { /* cp == 3 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- setting eoa.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* set the EOA */
    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(file, mt);

            if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(file, %s) failed.\n",
                          fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(file, %s) returned 0x%llx.\n",
                          fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    mt = H5FD_MEM_SUPER;
    while ( ( pass ) && ( mt <= H5FD_MEM_OHDR ) ) {

        if ( verbose ) {

            HDfprintf(stdout,
                      "calling H5FDset_eoa(file, %s, (%d * TYPE_SLICE) - 1).\n",
                      type_names[(int)mt], (int)(mt));
        }

        result = H5FDset_eoa(file, mt, (((haddr_t)(mt)) * TYPE_SLICE) - 1);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDset_eoa() failed.";
        }

        mt++;
    }

    if ( verbose ) {

        for ( mt = H5FD_MEM_SUPER; mt <= H5FD_MEM_OHDR; mt++ ) {

            eoa = H5FDget_eoa(file, mt);

            if ( eoa == HADDR_UNDEF ) {

                HDfprintf(stdout, "%s: H5FDget_eoa(file, %s) failed.\n",
                          fcn_name, type_names[(int)mt]);

            } else {

                HDfprintf(stdout, "%s: H5FDget_eoa(file, %s) returned 0x%llx.\n",
                          fcn_name, type_names[(int)mt], (unsigned long long)eoa);

            }
        }
    }

    if ( show_progress ) { /* cp == 4 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 bytes - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    /* first do some simple write, read back, and compare results tests with 
     * buffers of various sizes
     */

    offset = (haddr_t)0;
    write_size = (size_t)64;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)i * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "64 bytes -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 5 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 bytes - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "64 bytes -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }


    if ( show_progress ) { /* cp == 6 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 bytes - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "256 bytes -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 7 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 bytes - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "256 bytes -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }


    if ( show_progress ) { /* cp == 8 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 KB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "1 KB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 9 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 KB - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

	    aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "1 KB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 10 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 4 KB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)4;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "4 KB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 11 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 4 KB - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "4 KB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }


    if ( show_progress ) { /* cp == 12 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 KB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "64 KB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 13 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 64 KB - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "64 KB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 14 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 MB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "1 MB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 15 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 1 MB wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "1 MB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 16 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 16 MB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "16 MB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 17 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 16 MB - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "16 MB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 18 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 MB - poll.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    write_size *= (size_t)16;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

            HDassert( type_offset < ((haddr_t)(i) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "256 MB -- test for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ FALSE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 19 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- swrchk 256 MB - wait.\n", 
                  fcn_name, cp++, (int)pass);
    }

    offset += (haddr_t)write_size;

    if ( verbose ) {

        HDfprintf(stdout, "%s: offset = 0x%llx, write_size = 0x%llx, eow = 0x%llx.\n",
                  fcn_name,
                  (unsigned long long)offset, (unsigned long long)write_size,
                  (unsigned long long)(offset + write_size));
    }

    for ( i = 1; i < H5FD_MEM_NTYPES; i++ ) {

        if ( pass ) {

            type_offset = ((haddr_t)(i - 1) * TYPE_SLICE) + offset;

	    if ( type_offset >= ((haddr_t)(i) * TYPE_SLICE) ) {

		HDfprintf(stdout, "type_offset = 0x%llx.\n", type_offset);
		HDfprintf(stdout, 
                          "i = %d, TYPE_SLICE = 0x%llx, offset = 0x%llx.\n",
			  i, (long long)TYPE_SLICE, (long long)offset);
		HDfprintf(stdout, "((i + 1) * TYPE_SLICE) = 0x%llx.\n",
                          (long long)((haddr_t)(i + 1) * TYPE_SLICE) );
	    }

            HDassert( type_offset < ((haddr_t)(i + 1) * TYPE_SLICE) );

            aio_single_write_read_check(file,
                                        (H5FD_mem_t)i,
                                        "256 MB -- wait for completion",
                                        type_offset,
                                        write_size,
                                        /* do_wait = */ TRUE,
                                        &pass,
                                        &failure_mssg);
        }
    }

    if ( show_progress ) { /* cp == 20 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- mwsrchk.\n", 
                  fcn_name, cp++, (int)pass);
    }

    aio_multi_write_sync_read_check(file,
				    write_count,
			            types,
				    offsets,
				    lengths,
				    tags,
                                    &pass,
                                    &failure_mssg);

    if ( show_progress ) { /* cp == 21 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- wrfcchk.\n", 
                  fcn_name, cp++, (int)pass);
    }
#if 1 /* JRM */
    aio_multi_read_write_fsync_cancel_check(file,
				            op_count,
                                            ops,
			                    types,
				            offsets,
				            lengths,
				            tags,
                                            &pass, 
                                            &failure_mssg);
#endif /* JRM */
    if ( show_progress ) { /* cp == 22 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- close file.\n", 
                  fcn_name, cp++, (int)pass);
    }

    if ( file != NULL ) {

        result = H5FDclose(file);

        if ( result < 0 ) {

            pass = FALSE;
            failure_mssg = "H5FDclose() failed.";

        } else if ( h5_cleanup(FILENAME, fapl_id) == 0 ) {

            pass = FALSE;
            failure_mssg = "h5_cleanup() failed.\n";

        }
    }

    if ( show_progress ) { /* cp == 22 */
        HDfprintf(stdout, "%s: cp = %d, pass = %d -- done.\n", 
                  fcn_name, cp++, (int)pass);
    }

    if ( pass ) { 

	PASSED(); 

    } else { 

        HDfprintf(stdout, "%s: failure_mssg = \"%s\".\n",
                  fcn_name, failure_mssg);
	H5_FAILED(); 
        ret_val = -1;
    }

    if ( verbose ) {

        HDfprintf(stdout, "exiting generic_aio_test() -- ret_val == %d.\n",
                  ret_val);
    }

    return(ret_val);

} /* multi_file_driver_aio_test() */

#undef TYPE_SLICE


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the basic features of Virtual File Drivers
 *
 * Return:      Success:        exit(0)
 *              Failure:        exit(1)
 *
 * Programmer:  Raymond Lu
 *              Tuesday, Sept 24, 2002
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    const char *memb_name[H5FD_MEM_NTYPES];
    int 	nerrors = 0;
    int		result;
    hid_t 	fapl;
    hid_t 	memb_fapl[H5FD_MEM_NTYPES];
    haddr_t     memb_addr[H5FD_MEM_NTYPES];
    H5FD_mem_t  mt;
    H5FD_mem_t  memb_map[H5FD_MEM_NTYPES];

    h5_reset();
#if 1
    nerrors += test_sec2() < 0      ? 1 : 0;
    nerrors += test_core() < 0      ? 1 : 0;
    nerrors += test_family() < 0    ? 1 : 0;
    nerrors += test_family_compat() < 0    ? 1 : 0;
    nerrors += test_multi() < 0     ? 1 : 0;
    nerrors += test_direct() < 0      ? 1 : 0;
#endif
/* Note that we don't need to close the fapl in the 
 * following tests as generic_aio_test() will do this
 * for us.
 */
#if 1 /* SEC2 test */

    fapl = h5_fileaccess();

    if ( H5Pset_fapl_sec2(fapl) < 0 ) {

        nerrors++;

    } else {

        result = generic_aio_test("AIO on SEC2 file driver", 6, fapl, 
                                  (haddr_t)0x40000000);
        nerrors += ( result < 0 ) ? 1 : 0;
    }

    fapl = h5_fileaccess();

    if ( H5Pset_fapl_sec2(fapl) < 0 ) {

        nerrors++;

    } else {


        result = generic_aio_input_error_tests("AIO on SEC2 file driver error rejection", 
                                               "AIO SEC2 error rejection",
                                               12, fapl, FALSE);
        nerrors += ( result < 0 ) ? 1 : 0;
    }
#endif /* SEC2 test */

#if 1 /* CORE test */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_core(fapl, (size_t)0x40000000, TRUE) < 0) {

        nerrors++;

    } else {

        result = generic_aio_test("AIO on CORE file driver", 7, fapl, 
                                   (haddr_t)0x40000000);
        nerrors += ( result < 0 ) ? 1 : 0;
    }

    fapl = h5_fileaccess();

    if(H5Pset_fapl_core(fapl, (size_t)0x40000000, TRUE) < 0) {

        nerrors++;

    } else {

        result = generic_aio_input_error_tests("AIO on CORE file driver error rejection", 
                                               "AIO CORE error rejection",
                                               13, fapl, FALSE);
        nerrors += ( result < 0 ) ? 1 : 0;
    }
#endif

#if 1 /* STDIO test */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_stdio(fapl) < 0) {

        nerrors++;

    } else {

        result = generic_aio_test("AIO on STDIO file driver", 8, fapl, 
                                  (haddr_t)0x40000000);
        nerrors += ( result < 0 ) ? 1 : 0;
    }

    fapl = h5_fileaccess();

    if(H5Pset_fapl_stdio(fapl) < 0) {

        nerrors++;

    } else {

        result = generic_aio_input_error_tests("AIO on STDIO file driver error rejection", 
                                               "AIO STDIO error rejection",
                                               14, fapl, FALSE);
        nerrors += ( result < 0 ) ? 1 : 0;
    }
#endif

#if 1 /* FAMILY File test */
    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE_AIO, H5P_DEFAULT) < 0) {

        nerrors++;

    } else {

        result = generic_aio_test("AIO on FAMILY file driver", 9, fapl, 
                                  (haddr_t)0x40000000);
        nerrors += ( result < 0 ) ? 1 : 0;
    }

    fapl = h5_fileaccess();

    if(H5Pset_fapl_family(fapl, (hsize_t)FAMILY_SIZE_AIO, H5P_DEFAULT) < 0) {

        nerrors++;

    } else {

        result = generic_aio_input_error_tests("AIO on FAMILY file driver error rejection", 
                                               "AIO FAMILY error rejection",
                                               15, fapl, FALSE);
        nerrors += ( result < 0 ) ? 1 : 0;
    }
#endif

#if 1 /* MULTI File test */
    /* The generic aio test is ill suited to the multi file driver, as 
     * it takes no real cognisance of the type of data being written.
     * However, it is a good initial smoke check so we will begin with
     * it.
     */

    /* set up member map so all metadata goes in one file, everything
     * else in a second file.
     */

    for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

        memb_addr[mt] = HADDR_UNDEF;
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt]  = H5FD_MEM_SUPER;
        memb_name[mt] = NULL;
    }
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
    memb_name[H5FD_MEM_SUPER] = "%s-m.h5";
    memb_addr[H5FD_MEM_SUPER] = 0;
 
    memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
    memb_name[H5FD_MEM_DRAW] = "%s-r.h5";
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    fapl = h5_fileaccess();

    if ( H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, 
                           memb_addr, FALSE) < 0 ) {

        nerrors++;

    } else {

        result = generic_aio_test("AIO on MULTI file driver", 10, fapl, 
                                  (haddr_t)0x40000000);
        nerrors += ( result < 0 ) ? 1 : 0;
    
        result = multi_file_driver_aio_test(
			    "AIO with varied mem types on MULTI file driver", 11);
        nerrors += ( result < 0 ) ? 1 : 0;
    }

    for ( mt = 0; mt < H5FD_MEM_NTYPES; mt++ ) {

        memb_addr[mt] = HADDR_UNDEF;
        memb_fapl[mt] = H5P_DEFAULT;
        memb_map[mt]  = H5FD_MEM_SUPER;
        memb_name[mt] = NULL;
    }
    memb_map[H5FD_MEM_DRAW] = H5FD_MEM_DRAW;

    memb_fapl[H5FD_MEM_SUPER] = H5P_DEFAULT;
    memb_name[H5FD_MEM_SUPER] = "%s-m.h5";
    memb_addr[H5FD_MEM_SUPER] = 0;
 
    memb_fapl[H5FD_MEM_DRAW] = H5P_DEFAULT;
    memb_name[H5FD_MEM_DRAW] = "%s-r.h5";
    memb_addr[H5FD_MEM_DRAW] = HADDR_MAX/2;

    fapl = h5_fileaccess();

    if ( H5Pset_fapl_multi(fapl, memb_map, memb_fapl, memb_name, 
                           memb_addr, FALSE) < 0 ) {

        nerrors++;

    } else {

        result = generic_aio_input_error_tests("AIO on MULTI file driver error rejection", 
                                               "AIO MULTI error rejection",
                                               15, fapl, FALSE);
        nerrors += ( result < 0 ) ? 1 : 0;
    }
#endif

    if(nerrors) {
	printf("***** %d Virtual File Driver TEST%s FAILED! *****\n",
		nerrors, nerrors > 1 ? "S" : "");
	return 1;
    }

    printf("All Virtual File Driver tests passed.\n");
    return 0;
}
