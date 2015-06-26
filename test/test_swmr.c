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

/***********************************************************
*
* Test program:	 test_swmr
*
* 	To test SWMR related public routines:
*		H5Fstart_swmr_write()
*		H5Pget/set_object_flush_cb()
*		H5Pget/set_append_flush()
*
*************************************************************/

#include "hdf5.h"
#include "h5test.h"
#include "H5srcdir.h"

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file testing code.
 */
#define H5F_PACKAGE
#define H5F_TESTING
#include "H5Fpkg.h"		/* File access	 			*/

const char *FILENAME[] = {
    "test_swmr",		/* 0 */
    NULL
};

#define NAME_BUF_SIZE  	1024		/* Length of file name */

/* Name of message file that is used by test_start_swmr_write_concur() */
#define DONE_MESSAGE 	"DONE_MESSAGE"  /* The message file to create */

/* Tests for H5Fstart_swmr_write() */
static int test_start_swmr_write(hid_t in_fapl);
static int test_err_start_swmr_write(hid_t in_fapl);
static int test_start_swmr_write_concur(hid_t in_fapl);

/* Tests for H5Pget/set_object_flush_cb() */
static herr_t flush_cb(hid_t obj_id, void *_udata);
static int test_object_flush_cb(hid_t in_fapl);

/* Tests for H5Pget/set_append_flush() */
static herr_t append_cb(hid_t dset_id, hsize_t *cur_dims, void *_udata);
static herr_t append_cb2(hid_t dset_id, hsize_t *cur_dims, void *_udata);
static int test_append_flush_generic(void);
static int test_append_flush_dataset_chunked(hid_t in_fapl);
static int test_append_flush_dataset_fixed(hid_t in_fapl);
static int test_append_flush_dataset_multiple(hid_t in_fapl);

/*
 * Tests for H5Fstart_swmr_write()
 */

/*
 * Verify SWMR writing is enabled via H5Fstart_swmr_write().
 */
static int
test_start_swmr_write(hid_t in_fapl)
{
    hid_t fid = -1;			/* File ID */
    hid_t fapl = -1;			/* A copy of file access property */
    hid_t file_fapl = -1;		/* File access property for the file */
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hsize_t dims[2] = {5, 10};		/* Dimension sizes */
    int buf[50];			/* Data buffer */
    int i;				/* Local index variable */
    unsigned attempts;			/* The retrieved # of read attempts */
    char filename[NAME_BUF_SIZE];	/* File name */

    TESTING("H5Fstart_swmr_write()");

    /* Get a copy of the parameter fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Get the file's access_property list */
    if((file_fapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the # of read attempts from the file's fapl */
    if(H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
	FAIL_STACK_ERROR

    /* Close the property list */
    if(H5Pclose(file_fapl) < 0)
	FAIL_STACK_ERROR;

    /* Should be 1 */
    if(attempts != H5F_METADATA_READ_ATTEMPTS)
	TEST_ERROR;

    /* Create a dataset */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) 
	FAIL_STACK_ERROR;
    if((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Enable SWMR writing */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR;

    /* Open the dataset */
    if((did = H5Dopen2(fid, "dataset", H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Initialize data buffer */
    for(i = 0; i < 50; i++)
	buf[i] = i;

    /* Write to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
	FAIL_STACK_ERROR;

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Close the dataspace */
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;

    /* Get the file's access_property list */
    if((file_fapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the # of read attempts from file's fapl */
    if(H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
	FAIL_STACK_ERROR

    /* Should be 100 */
    if(attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
	TEST_ERROR;

    /* Closing */
    if(H5Pclose(file_fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(fid);
	H5Pclose(fapl);
	H5Pclose(file_fapl);
	H5Dclose(did);
	H5Sclose(sid);
    } H5E_END_TRY;

    return -1;
} /* test_start_swmr_write() */

/*
 * Verify failures in enabling SWMR writing mode via H5Fstart_swmr_write():
 *	When creating a file:
 *	(1) the file is created with SWMR write access
 *	(2) the file is not created with the latest format
 *	(3) there are opened objects in the file
 *	When opening a file with the latest format:
 *	(1) the file is already opened with SWMR write access
 *	(2) the file is opened with read access only
 *	(3) the file is opened with SWMR read access only
 *	(4) there are opened objects in the file
 *	When opening a file without the latest format:
 *	(1) the file is not opened with the latest format
 *	(2) the file is opened with read access only
 *	(3) the file is opened with SWMR read access only
 *	When there are multiple opens for the file:
 *	(1) create a file, enable SWMR writing mode twice (fail second time)
 *	(2) create a file and enable SWMR writing mode
 *	    reopen the same file and enable SWMR writing mode (fail)
 *	(3) create a file, open the same file
 *	    enable SWMR writing for the file created
 *	    enable SWMR writing for the file opened (fail)
 */
static int
test_err_start_swmr_write(hid_t in_fapl)
{
    hid_t fid = -1;			/* File ID */
    hid_t fid2 = -1;			/* File ID */
    hid_t fapl = -1;			/* A copy of file access property */
    hid_t gid = -1;			/* Group ID */
    hid_t did = -1;			/* Dataset ID */
    hid_t sid = -1;			/* Dataspace ID */
    hid_t tid = -1;			/* Datatype ID */
    hsize_t dims[2] = {5, 10};		/* Dimension size */
    herr_t ret;				/* Return value */
    char filename[NAME_BUF_SIZE];	/* File name */
    
    /* Create a copy of the input parameter in_fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    TESTING("H5Fstart_swmr_write() failure conditions");

    /* 
     * When creating a file:
     */

    /* Case 1 */

    /* Cannot enable SWMR writing when the file is already in SWMR writing mode */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl);
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Case 2 */

    /* Cannot enable SWMR writing mode without latest format */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, in_fapl);
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to create the file with SWMR write access when not using latest format */
    H5E_BEGIN_TRY {
	ret = H5Fcreate(filename, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, in_fapl);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR


    /* Case 3 */

    /* Create a file with the latest format */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Create a group */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;

    /* Create a dataset in the group */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) 
	FAIL_STACK_ERROR;
    if((did = H5Dcreate2(gid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	TEST_ERROR

    /* Should fail to enable SWMR writing mode: there are opened objects */
    H5E_BEGIN_TRY {
	ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR;

    /* Close the dataset */
    if(H5Dclose(did) < 0)
	FAIL_STACK_ERROR;

    /* Fail to enable SWMR writing mode */
    H5E_BEGIN_TRY {
	ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR;

    /* Close the group */
    if(H5Gclose(gid) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR;

    /* Closing */
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* 
     * When opening a file with latest format:
     */

    /* Create and close a file with latest format */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;
    
    /* Case 1 */

    /* Open the file with SWMR write access */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Cannot enable SWMR writing when already in SWMR writing mode */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Case 2 */

    /* Open the file with read only access */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is opened with read only access */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Case 3 */

    /* Open the file file with SWMR read access */
    if((fid = H5Fopen(filename, H5F_ACC_SWMR_READ, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is opened with SWMR read access only */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Case 4 */

    /* Open the file with latest format */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Create and commit a named datatype */
    if((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
	FAIL_STACK_ERROR;
    if(H5Tcommit2(fid, "TID", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing mode */
    H5E_BEGIN_TRY {
	ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR;

    /* Close the datatype */
    if(H5Tclose(tid) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* 
     * Opening a file without latest format:
     */

    /* Case 1 */

    /* Open the file without latest format */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, in_fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing mode */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to open the file with SWMR write access when not using latest format */
    H5E_BEGIN_TRY {
	ret = H5Fopen(filename, H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE, in_fapl);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Case 2 */

    /* Open the file with read only */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, in_fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is opened with read only */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* Case 3 */

    /* Open the file with SWMR read only */
    if((fid = H5Fopen(filename, H5F_ACC_SWMR_READ, in_fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing mode when the file is opened with SWMR read only */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /* 
     * Failure cases for multiple opens 
     */

    /* Case 1 */

    /* Create a file with latest format */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Should fail for a second call to start SWMR writing mode */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;


    /* Case 2 */

    /* Create a file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in starting SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Re-open the same file */
    if((fid2 = H5Freopen(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing mode for fid2 */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid2);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the files */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid2) < 0)
	FAIL_STACK_ERROR;

    /* Case 3 */

    /* Create a file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Open the same file */
    if((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing mode for fid */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Should fail to enable SWMR writing mode for fid2 */
    H5E_BEGIN_TRY {
        ret = H5Fstart_swmr_write(fid2);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Close the files */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid2) < 0)
	FAIL_STACK_ERROR;

    /* Close the file access property list */
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Fclose(fid);
	H5Pclose(fapl);
    } H5E_END_TRY;

    return -1;
} /* test_err_start_swmr_write() */


/*
 * test_start_swmr_write_concur():
 * 	Verify concurrent access for H5Fstart_swmr_write()--
 *	(1) Open a file with write access
 *	    Concurrent open of the file with read & SWMR read (fail)
 *	(2) Open a file with write access; enable SWMR writing mode
 *	    Concurrent open of the file with read & SWMR read (succeed)
 *	(3) Open a file with write access; enable SWMR writing mode
 *	    Concurrent open of the file with read only (fail)
 *	(4) Open a file with write access; enable SWMR writing mode
 *	    Concurrent open of the file with write access (fail)
 *	(5) Open a file with write access; enable SWMR writing mode
 *	    Concurrent open of the file with write and SWMR write access (fail)
 */
static int
test_start_swmr_write_concur(hid_t in_fapl)
{
    hid_t fid;    			/* File ID */
    hid_t fapl;    			/* File access property list */
    herr_t ret;         		/* Generic return value */
    pid_t childpid=0;			/* Child process ID */
    pid_t tmppid;			/* Child process ID returned by waitpid */
    int child_status;			/* Status passed to waitpid */
    int child_wait_option=0;		/* Options passed to waitpid */
    int child_ret_value;		/* Exit status of the child */
    char filename[NAME_BUF_SIZE];	/* File name */

    /* Output message about test being performed */
    TESTING("Testing H5Fstart_swmr_write()--concurrent access");

#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID))

    SKIPPED();
    HDputs("    Test skipped due to fork or waitpid not defined.");

#else /* defined(H5_HAVE_FORK && defined(H5_HAVE_WAITPID) */

    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* Remove the message file to be sure */
    HDremove(DONE_MESSAGE);

    /* 
     *  Case (1):
     * 	Verify concurrent file open with H5F_ACC_RDONLY|H5F_ACC_SWMR_READ 
     *  will fail without H5Fstart_swmr_write() 
     */

    /* Fork child process */
    if((childpid = HDfork()) < 0)
	FAIL_STACK_ERROR

    if(childpid == 0) { /* Child process */

	/* Wait till parent process completes the open */
	if(h5_wait_message(DONE_MESSAGE) < 0)
	    exit(1);

	/* Should fail */
	H5E_BEGIN_TRY {
	    /* Open the test file */
	    ret = H5Fopen(filename, H5F_ACC_RDONLY|H5F_ACC_SWMR_READ, fapl);
	} H5E_END_TRY;
	if(ret >= 0)
	    exit(1);
	exit(0);
    }

    /* Open the test file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Send the message that the open completes */
    h5_send_message(DONE_MESSAGE);

    /* Wait for child process to complete */
    if((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
	FAIL_STACK_ERROR

    /* Check if child terminated normally */
    if((ret = WIFEXITED(child_status)) < 0)
	FAIL_STACK_ERROR

    /* Check exit status of child process */
    if((child_ret_value = WEXITSTATUS(child_status)) != 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    
    /* 
     *  Case (2):
     * 	Verify concurrent file open with H5F_ACC_RDONLY|H5F_ACC_SWMR_READ 
     *  will succeed with H5Fstart_swmr_write() 
     */

    /* Remove the message file to be sure */
    HDremove(DONE_MESSAGE);

    /* Fork child process */
    if((childpid = HDfork()) < 0)
	FAIL_STACK_ERROR

    if(childpid == 0) { /* Child process */
	hid_t child_fid;	/* File ID */

	/* Wait till parent process completes the open */
	if(h5_wait_message(DONE_MESSAGE) < 0)
	    exit(1);

	/* Should succeed in opening the test file */
	if((child_fid = H5Fopen(filename, H5F_ACC_RDONLY|H5F_ACC_SWMR_READ, fapl)) < 0) {
	    TEST_ERROR
	    exit(1);
	}
	if(H5Fclose(child_fid) < 0) {
	    FAIL_STACK_ERROR
	    exit(1);
	}
	exit(0);
    }

    /* Open the test file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Send the message that H5Fstart_swmr_write() completes */
    h5_send_message(DONE_MESSAGE);

    /* Wait for child process to complete */
    if((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
	FAIL_STACK_ERROR

    /* Check if child terminated normally */
    if((ret = WIFEXITED(child_status)) < 0)
	FAIL_STACK_ERROR

    /* Check exit status of child process */
    if((child_ret_value = WEXITSTATUS(child_status)) != 0)
	TEST_ERROR

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* 
     *  Case (3):
     * 	Verify concurrent file open with H5F_ACC_RDONLY
     *  will fail with H5Fstart_swmr_write() 
     */

    /* Remove the message file to be sure */
    HDremove(DONE_MESSAGE);

    /* Fork child process */
    if((childpid = HDfork()) < 0)
	FAIL_STACK_ERROR


    if(childpid == 0) { /* Child process */

	/* Wait till parent process completes the open */
	if(h5_wait_message(DONE_MESSAGE) < 0)
	    exit(1);

	/* Should fail in opening the test file */
	H5E_BEGIN_TRY {
	    ret = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
	} H5E_END_TRY;
	if(ret >= 0)
	    exit(1);
	exit(0);
    }

    /* Open the test file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Send the message that H5Fstart_swmr_write() completes */
    h5_send_message(DONE_MESSAGE);

    /* Wait for child process to complete */
    if((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
	FAIL_STACK_ERROR

    /* Check if child terminated normally */
    if((ret = WIFEXITED(child_status)) < 0)
	FAIL_STACK_ERROR

    /* Check exit status of child process */
    if((child_ret_value = WEXITSTATUS(child_status)) != 0)
	TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* 
     *  Case (4):
     * 	Verify concurrent file open with H5F_ACC_RDWR
     *  will fail with H5Fstart_swmr_write() 
     */

    /* Remove the message file to be sure */
    HDremove(DONE_MESSAGE);

    /* Fork child process */
    if((childpid = HDfork()) < 0)
	FAIL_STACK_ERROR


    if(childpid == 0) { /* Child process */

	/* Wait till parent process completes the open */
	if(h5_wait_message(DONE_MESSAGE) < 0)
	    exit(1);

	/* Should fail in opening the test file */
	H5E_BEGIN_TRY {
	    ret = H5Fopen(filename, H5F_ACC_RDWR, fapl);
	} H5E_END_TRY;
	if(ret >= 0)
	    exit(1);
	exit(0);
    }

    /* Open the test file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Send the message that H5Fstart_swmr_write() completes */
    h5_send_message(DONE_MESSAGE);

    /* Wait for child process to complete */
    if((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
	FAIL_STACK_ERROR

    /* Check if child terminated normally */
    if((ret = WIFEXITED(child_status)) < 0)
	FAIL_STACK_ERROR

    /* Check exit status of child process */
    if((child_ret_value = WEXITSTATUS(child_status)) != 0)
	TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* 
     *  Case (5):
     * 	Verify concurrent file open with H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE
     *  will fail with H5Fstart_swmr_write() 
     */

    /* Remove the message file to be sure */
    HDremove(DONE_MESSAGE);

    /* Fork child process */
    if((childpid = HDfork()) < 0)
	FAIL_STACK_ERROR


    if(childpid == 0) { /* Child process */

	/* Wait till parent process completes the open */
	if(h5_wait_message(DONE_MESSAGE) < 0)
	    exit(1);

	/* Should fail in opening the test file */
	H5E_BEGIN_TRY {
	    ret = H5Fopen(filename, H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE, fapl);
	} H5E_END_TRY;
	if(ret >= 0)
	    exit(1);
	exit(0);
    }

    /* Open the test file */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Enable SWMR writing mode */
    if(H5Fstart_swmr_write(fid) < 0)
	TEST_ERROR

    /* Send the message that H5Fstart_swmr_write() completes */
    h5_send_message(DONE_MESSAGE);

    /* Wait for child process to complete */
    if((tmppid = HDwaitpid(childpid, &child_status, child_wait_option)) < 0)
	FAIL_STACK_ERROR

    /* Check if child terminated normally */
    if((ret = WIFEXITED(child_status)) < 0)
	FAIL_STACK_ERROR

    /* Check exit status of child process */
    if((child_ret_value = WEXITSTATUS(child_status)) != 0)
	TEST_ERROR;

    /* Close the file */
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    /* Close the property list */
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
	H5Fclose(fid);
    } H5E_END_TRY;

    return -1;
#endif

} /* end test_start_swmr_write_concur() */


/*
 * Tests for H5Pset/get_object_flush_cb() 
 */

/* The callback function for object flush property */
static herr_t 
flush_cb(hid_t obj_id, void *_udata)
{
    unsigned *flush_ct = (unsigned*)_udata;
    ++(*flush_ct);
    return 0;
}

/*
 * test_object_flush_cb()
 *	To verify the public routines H5Pget/set_object_flush_cb()
 *	work as specified:
 *	1) To verify the failure condition in setting object flush property
 *	2) To verify the object flush property values retrieved from a default 
 *	   file access property list.
 *	3) To verify the object flush property values retrieved from a non-default
 *	   file access property list.
 *	4) To verify the object flush property values retrieved from a default
 *	   file access property list of a file
 *	5) To verify the object flush property values retrieved from a non-default
 *	   file access property list of a file
 *	   To verify the object flush callback is invoked when doing H5Oflush(),
 *	   H5Dflush(), H5Gflush() and H5Tflush().
 */
static int
test_object_flush_cb(hid_t in_fapl)
{
    hid_t fapl = -1;			/* A copy of file access property list */
    hid_t ffapl = -1;			/* A file's file access property list */
    hid_t fid = -1;			/* File ID */
    hid_t gid = -1;			/* Group ID */
    hid_t did1 = -1, did2 = -1;		/* Dataset IDs */
    hid_t sid = -1;			/* Dataspace ID */
    hsize_t dims[2] = {5, 10};		/* Dataset dimension sizes */
    int buf[50];			/* Data buffer */
    H5F_flush_cb_t ret_cb;		/* The callback function set in object flush property */
    void *ret_ct;			/* The user data set in object flush property */
    unsigned flush_ct = 0;		/* The user data for object flush property */
    char filename[NAME_BUF_SIZE];	/* File name */
    int i;				/* Local index variable */
    herr_t ret;         		/* Generic return value */

    TESTING("H5Pget/set_obj_flush_cb()");
    
    /*
     * Case (1) 
     *	To verify the failure condition in setting object flush property
     */
    /* Should fail if the callback function is not defined but user data is defined */
    H5E_BEGIN_TRY {
	ret = H5Pset_object_flush_cb(fapl, NULL, &flush_ct);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /*
     * Case (2)
     * 	To verify the object flush property values retrieved from a
     *  default file access property list.
     */

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve object flush property values for the default file access property list */
    if(H5Pget_object_flush_cb(fapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR
    /* Should be null */
    if(ret_cb != NULL || ret_ct != NULL)
	TEST_ERROR

    /*
     * Case (3)
     * 	To verify the object flush property values retrieved from a
     * 	non-default file access property list.
     */
    /* Set the object flush property */
    if(H5Pset_object_flush_cb(fapl, flush_cb, &flush_ct) < 0)
	TEST_ERROR

    /* Increment the counter */
    ++flush_ct;

    /* Retrieve object flush property values for the non-default file access property list */
    if(H5Pget_object_flush_cb(fapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != flush_cb || *(unsigned *)ret_ct != 1)
	TEST_ERROR

    /* Close the property list */
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;


    /*
     * Case (4)
     *	To verify the object flush property values retrieved from a
     *	default file access property list of a file
     */

    /* Reset values */
    flush_ct = 0;
    ret_cb = NULL;
    ret_ct = NULL;

    /* Make a copy of the input parameter in_fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file: without setting object flush property in fapl */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if(H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != NULL || ret_ct != NULL)
	TEST_ERROR

    /* Closing */
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    /*
     * Cases (5)
     *	To verify the object flush property values retrieved from a non-default
     *  file access property list of a file.
     *  To verify the object flush callback is invoked when doing H5Oflush(),
     *	H5Dflush(), H5Gflush() and H5Tflush().
     */
    /* Reset values */
    flush_ct = 0;
    ret_cb = NULL;
    ret_ct = NULL;

    /* Set the object flush property */
    if(H5Pset_object_flush_cb(fapl, flush_cb, &flush_ct) < 0)
	FAIL_STACK_ERROR

    /* Open the test file: with object flush property setting in fapl */
    if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
	FAIL_STACK_ERROR;

    /* Create a dataset */
    if((sid = H5Screate_simple(2, dims, dims)) < 0) 
	FAIL_STACK_ERROR;

    /* Create a dataset */
    if((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	FAIL_STACK_ERROR

    /* Initialize data buffer */
    for(i = 0; i < 50; i++)
	buf[i] = i + 1;

    /* Write to the dataset */
    if(H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
	FAIL_STACK_ERROR;

    /* Flush the dataset object */
    if(H5Oflush(did1) < 0)
	FAIL_STACK_ERROR;

    /* Get the file's file access property list */
    if((ffapl = H5Fget_access_plist(fid)) < 0)
	FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if(H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != flush_cb || *(unsigned *)ret_ct != 1)
	TEST_ERROR

    /* Create a group */
    if((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	FAIL_STACK_ERROR

    /* Flush the group */
    if(H5Gflush(gid) < 0)
	TEST_ERROR

    /* Retrieve the object flush property values */
    if(H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != flush_cb || *(unsigned *)ret_ct != 2)
	TEST_ERROR

    /* Create a dataset */
    if((did2 = H5Dcreate2(gid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	FAIL_STACK_ERROR

    /* Flush the dataset */
    if(H5Dflush(did2) < 0)
	FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if(H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != flush_cb || *(unsigned *)ret_ct != 3)
	TEST_ERROR

    /* Closing */
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did1) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;
    if(H5Gclose(gid) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(ffapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(fapl);
	H5Pclose(ffapl);
	H5Sclose(sid);
	H5Dclose(did1);
	H5Dclose(did2);
	H5Gclose(gid);
	H5Fclose(fid);
    } H5E_END_TRY;

    return -1;
} /* test_object_flush_cb() */

/*
 * Tests for H5Pset/get_append_flush() 
 */


/* The callback function for append flush property */
static herr_t
append_cb(hid_t dset_id, hsize_t *cur_dims, void *_udata)
{
    unsigned *count = (unsigned *)_udata;
    ++(*count++);
    return 0;
} /* append_cb() */


/* The callback function for append flush property */
static herr_t
append_cb2(hid_t dset_id, hsize_t *cur_dims, void *_udata)
{
    unsigned *count = (unsigned *)_udata;
    ++(*count++);
    return 0;
} /* append_cb2() */



/*
 * test_append_flush_generic()
 *	To verify H5Pget/set_append_flush() work as specified for 
 *	a generic dataset access property list:
 *	1) To verify the append flush property values retrieved from a default
 *	   access property list.
 *		-- zero boundary, null callback function, null user data
 *	2) To verify the failure conditions in setting append flush property:
 *		-- an invalid dataset rank: <= 0, > H5S_MAX_RANK
 *		-- undefined callback but defined user data
 *		-- no boundary specified
 *		-- invalid boundary size: H5S_UNLIMITED, negative value
 *	3) To verify the append flush property values retrieved from a non-default
 *	   access property list.
 *		-- the set callback function, the set user data
 *		-- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
 */
static int
test_append_flush_generic(void)
{
    hid_t dapl = -1;			/* A copy of dataset access property */
    hsize_t boundary[3];		/* The boundary for append flush property */
    unsigned count = 0;			/* The user data for append flush property */
    hsize_t ret_boundary[3];		/* The boundary set in append flush property */
    H5D_append_cb_t ret_cb;		/* The callback function set in append flush property */
    unsigned *ret_count;		/* The user data set in append flush property */
    herr_t ret;				/* The return value */

    TESTING("H5Fget/set_append_flush() for a generic dataset access property list");


    /* 
     * Case (1)
     *	To verify the retrieved append flush property values:
     *		-- zero boundary, null callback function, null user data
     */

    /* Create a copy of dataset access property list */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(dapl, 2, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	FAIL_STACK_ERROR

    /* Verify expected values */
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0)
	TEST_ERROR;
    if(ret_cb != NULL || ret_count != NULL)
	TEST_ERROR

    /* Close the property list */
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;

    /* 
     * Case (2)
     *	To verify the failure conditions in setting append flush property:
     *	-- an invalid dataset rank: <= 0, > H5S_MAX_RANK
     *	-- no boundary specified
     *	-- undefined callback but defined user data
     *	-- invalid boundary size: H5S_UNLIMITED, negative value
     */

    /* Create a copy of dataset access property list */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
	FAIL_STACK_ERROR

    /* Invalid dataset rank: zero value */
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, 0, NULL, NULL, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Invalid dataset rank: negative value */
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, -1, NULL, NULL, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Invalid dataset rank: > H5S_MAX_RANK */
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, H5S_MAX_RANK+1, NULL, NULL, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* No boundary specified */
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, 2, NULL, NULL, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Set up a valid boundary */
    boundary[0] = 1;
    boundary[1] = 1;

    /* Undefined callback function but defined user data */
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, 2, boundary, NULL, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Invalid boundary size: negative value */
    boundary[0] = -1;
    boundary[1] = 1;
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, 2, boundary, append_cb, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR

    /* Invalid boundary size: H5S_UNLIMITED */
    boundary[0] = 1;
    boundary[1] = H5S_UNLIMITED;
    H5E_BEGIN_TRY {
	ret = H5Pset_append_flush(dapl, 2, boundary, append_cb, &count);
    } H5E_END_TRY;
    if(ret >= 0)
	TEST_ERROR
    
    /*
     * Case (3)
     *	To verify the append flush property values retrieved from a non-default
     *  access property list:
     *		-- the set callback function, the set user data
     *		-- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
     */
    boundary[0] = boundary[1] = 1;
    boundary[2] = 0;
    count = 1;
    if(H5Pset_append_flush(dapl, 2, boundary, append_cb, &count) < 0)
	FAIL_STACK_ERROR;
    ++count;

    /* Verify expected values: with boundary rank > set boundary rank */
    if(H5Pget_append_flush(dapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    if(ret_boundary[0] != 1 || ret_boundary[1] != 1 || boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb == NULL || ret_count == NULL || *ret_count != 2)
	TEST_ERROR

    /* Verify expected values: with boundary rank < set boundary rank */
    HDmemset(ret_boundary, 0, sizeof(ret_boundary));
    if(H5Pget_append_flush(dapl, 1, ret_boundary, NULL, NULL) < 0)
	TEST_ERROR
    if(ret_boundary[0] != 1 || ret_boundary[1] != 0 || boundary[2] != 0)
	TEST_ERROR;

    /* Closing */
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
    } H5E_END_TRY;

    return -1;
} /* test_append_flush_generic() */

/*
 * test_append_flush_dataset_chunked()
 *	To verify H5Pget/set_append_flush() work as specified for 
 *	a chunked dataset's access property list:
 *	  1) To verify the append flush property values retrieved from a default access
 *	     property list:
 *		-- zero boundary, null callback function, null user data
 *	  2) To verify failure in creating dataset when:
 *		-- the rank set in append flush property is not the same as the dataset's rank
 *		-- boundary (non-zero) is set for a non-extendible dimension
 *	  3) To verify the append flush property values retrieved from a non-default
 *	     access property list:
 *		-- the set callback function, the set user data
 *		-- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
 */
static int
test_append_flush_dataset_chunked(hid_t in_fapl)
{
    hid_t fid = -1;			/* file ID */
    hid_t fapl = -1;			/* A copy of file access property */
    hid_t did1 = -1, did2 = -1; 	/* The datset ID */
    hid_t sid = -1;			/* The dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ddapl = -1;			/* The dataset access property of the opened dataset */

    hsize_t boundary[3]; 		/* Boundary size */
    unsigned count = 0;			/* User data */

    hsize_t ret_boundary[3];		/* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;		/* The callback function set in the append flush property */
    unsigned *ret_count;		/* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE];	/* file name */

    hsize_t dims[2] = {100, 0};			/* The dataset dimension sizes */
    hsize_t maxdims[2] = {100, H5S_UNLIMITED};	/* The dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {5,2};		/* The chunk dimesion sizes */

    TESTING("H5Fget/set_append_flush() for a chunked dataset's access property list");

    /* 
     *  Case (1)--
     *	For a chunked dataset's access property list:
     *	--to verify the append flush property values retrieved from a default access
     *	  a default access property list is:
     *		zero rank, zero boundary, null callback function, null user data
     */

    /* Get a copy of the input parameter in_fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR


    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Create a chunked dataset with 1 extendible dimension */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;
    if((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
	TEST_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did1)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values */
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != NULL || ret_count != NULL) 
	TEST_ERROR

    /* Close the dataset's access property list */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;

    /* 
     *  Case (2)--
     *	For a chunked dataset's access property list:
     *	--to verify failure in creating the dataset when:
     *		--the rank set in append flush property is not the same as the dataset's rank
     *		-- boundary (non-zero) is set for a non-extendible dimension
     *  --to verify failure in opening the dataset
     *		-- boundary (non-zero) is set for a non-extendible dimension
     */
    /* Create a copy of dataset access property list */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR

    /* Set boundary dimension rank > the rank of dataset to be created */
    HDmemset(boundary, 0, sizeof(boundary));
    if(H5Pset_append_flush(dapl, 3, boundary, NULL, NULL) < 0)
	FAIL_STACK_ERROR

    /* Should fail to Create the dataset */
    H5E_BEGIN_TRY {
	did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl);
    } H5E_END_TRY;
    if(did2 >= 0)
	TEST_ERROR

    /* Set boundary for a non-extendible dimension */
    boundary[0] = boundary[1] = 1;
    if(H5Pset_append_flush(dapl, 2, boundary, NULL, NULL) < 0)
	FAIL_STACK_ERROR

    /* Should fail to create the dataset */
    H5E_BEGIN_TRY {
	did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl);
    } H5E_END_TRY;
    if(did2 >= 0)
	TEST_ERROR

    /* Create and close the dataset */
    if((did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;

    /* Should fail to open the dataset */
    H5E_BEGIN_TRY {
	did2 = H5Dopen(fid, "dataset2", dapl);
    } H5E_END_TRY;
    if(did2 >= 0)
	TEST_ERROR

    /* 
     *  Case (3)--
     *	For a chunked dataset's access property list:
     *	--To verify the append flush property values retrieved from a non-default
     *    access property list:
     *		-- the set callback function, the set user data
     *		-- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
     */
	 
    boundary[0] = 0;
    boundary[1] = 1;
    if(H5Pset_append_flush(dapl, 2, boundary, append_cb, &count) < 0)
	FAIL_STACK_ERROR
    if((did2 = H5Dopen(fid, "dataset2", dapl)) < 0)
	FAIL_STACK_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    HDmemset(ret_boundary, 0, sizeof(ret_boundary));
    ret_cb = NULL;
    ret_count = NULL;
    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != append_cb || ret_count != &count)
	TEST_ERROR
    if(ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
	TEST_ERROR

    HDmemset(ret_boundary, 0, sizeof(ret_boundary));
    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 1, ret_boundary, NULL, NULL) < 0)
	TEST_ERROR
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR

    /* Closing */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;

    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did1) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Pclose(dapl);
	H5Pclose(ddapl);
	H5Dclose(did1);
	H5Dclose(did2);
	H5Pclose(fapl);
	H5Sclose(sid);
	H5Fclose(fid);
    } H5E_END_TRY;

    return -1;
} /* test_append_flush_dataset_chunked() */

/*
 * test_append_flush_dataset_fixed():
 *	To verify H5Pget/set_append_flush() work as specified for 
 *	a non-chunked (fixed size) dataset's access property list:
 *	(1) To verify success in creating the dataset--whatever is set for the append flush property setting
 *	(2) To verify that default append flush property values are retrieved for both 
 *	    default or non-default access property list:
 *		-- zero boundary, null callback function, null user data
 */
static int
test_append_flush_dataset_fixed(hid_t in_fapl)
{
    hid_t fid = -1;			/* file ID */
    hid_t fapl = -1;			/* A copy of file access property */
    hid_t did1 = -1, did2 = -1; 	/* The datset ID */
    hid_t sid = -1;			/* The dataspace ID */
    hid_t dapl = -1;			/* A copy of dataset access property */
    hid_t ddapl = -1;			/* The dataset access property of the opened dataset */

    hsize_t boundary[3]; 		/* Boundary size */
    unsigned count = 0;			/* User data */

    hsize_t ret_boundary[3];		/* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;		/* The callback function set in the append flush property */
    unsigned *ret_count;		/* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE];	/* file name */

    hsize_t dims[1] = {100};

    TESTING("H5Fget/set_append_flush() for a non-chunked dataset's access property list");

    /* 
     *  Case (1)--
     *	For a non-chunked dataset's access property list:
     *	--to verify the append flush property values retrieved from
     *	  a default access property list is:
     *		zero boundary, null callback function, null user data
     */

    /* Get a copy of the input parameter in_fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR


    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Create a dataset */
    if((sid = H5Screate_simple(1, dims, dims)) < 0) 
	FAIL_STACK_ERROR;
    if((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) 
	TEST_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did1)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values */
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != NULL || ret_count != NULL) 
	TEST_ERROR

    /* Close the dataset's access property list */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;

    /* 
     *  Case (2)--
     *	For a non-chunked dataset's access property list:
     *	--to verify success in creating and opening the dataset even when append flush property 
     * 	  is setup with error conditions:
     *		--the rank set in append flush property is not the same as the dataset's rank
     *		--boundary is set
     *  --to verify the append flush property values are:
     *		zero boundary, null callback function, null user data
     */
    /* Create a copy of dataset access property list */
    if((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR

    boundary[0] = 1;
    boundary[1] = boundary[2] = 0;
    if(H5Pset_append_flush(dapl, 3, boundary, append_cb, &count) < 0)
	FAIL_STACK_ERROR

    /* Should succeed to create the dataset: append flush property has no effect */
    if((did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, dapl)) < 0)
	TEST_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != NULL || ret_count != NULL)
	TEST_ERROR
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR

    /* Closing */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;

    /* Should succeed in opening the dataset: append flush property has no effect */
    if((did2 = H5Dopen(fid, "dataset2", dapl)) < 0)
	TEST_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != NULL || ret_count != NULL)
	TEST_ERROR
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR

    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR
    /* 
     * 	Case (3)--
     *	For a non-chunked dataset's access property list:
     *	--To verify the append flush property values retrieved from a non-default
     *    access property list:
     *		zero boundary, null callback function, null user data
     */
	 
    HDmemset(boundary, 0, sizeof(boundary));
    if(H5Pset_append_flush(dapl, 1, boundary, append_cb, &count) < 0)
	FAIL_STACK_ERROR
    if((did2 = H5Dopen(fid, "dataset2", dapl)) < 0)
	FAIL_STACK_ERROR

    /* Get the dataset's access property list */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 1, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR

    /* Verify expected values */
    if(ret_cb != NULL || ret_count != NULL)
	TEST_ERROR
    if(ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR

    /* Closing */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did1) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dapl);
	H5Pclose(ddapl);
	H5Dclose(did1);
	H5Dclose(did2);
	H5Pclose(fapl);
	H5Sclose(sid);
	H5Fclose(fid);
    } H5E_END_TRY;

    return -1;
} /* test_append_flush_dataset_fixed() */

/*
 * test_append_flush_multiple()
 *	To verify H5Pget/set_append_flush() work as specified for
 *	multiple opens of a dataset:
 *	(1) did1 = H5Dcreate(...dapl1...)
 *	    did2 = H5Dopen2(...dapl2)
 *	    H5Pget_append_flush(did1...)
 *	    H5Pget_append_flush(did2...) 
 *		-- should return append flush property values set in dapl1
 *	(2) H5Dcreate(...H5P_DEFAULT...)
 *	    H5Dclose()
 *	    did1 = H5Dopen(...dapl1)
 *	    did2 = H5Dopen(..dapl2) 
 *	    H5Pget_append_flush(did1, ...)
 *	    H5Pget_append_flush(did2, ...)
 *		-- should return append flush property values set in dapl1
 * 	NOTE: 
 *	  FOR NOW: return the append flush property values of the create or the very first open
 *	  LATER ON: should REJECT subsequent dataset open if append flush property values differ
 */
static int
test_append_flush_dataset_multiple(hid_t in_fapl)
{
    hid_t fid = -1;			/* file ID */
    hid_t fapl = -1;			/* A copy of file access property */
    hid_t did1 = -1, did2 = -1; 	/* The datset ID */
    hid_t sid = -1;			/* The dataspace ID */
    hid_t dcpl = -1;			/* A copy of dataset creation property */
    hid_t dapl1 = -1;			/* A copy of dataset access property */
    hid_t dapl2 = -1;			/* A copy of dataset access property */
    hid_t ddapl = -1;			/* The dataset access property of the opened dataset */

    hsize_t boundary1[3]; 		/* Boundary size */
    hsize_t boundary2[3]; 		/* Boundary size */
    unsigned count1 = 0;		/* User data */
    unsigned count2 = 0;		/* User data */

    hsize_t ret_boundary[3];		/* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;		/* The callback function set in the append flush property */
    unsigned *ret_count;		/* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE];	/* file name */

    hsize_t dims[2] = {0, 0};					/* The dataset dimension sizes */
    hsize_t maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};	/* The dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {5,2};				/* The chunk dimesion sizes */

    TESTING("H5Fget/set_append_flush() for multiple opens of a chunked dataset");

    /* 
     *  Case (1) 
     *	For a chunked dataset's access property list:
     *		did1 = H5Dcreate(...dapl1...)
     *	    	did2 = H5Dopen2(...dapl2)
     *	    	H5Pget_append_flush(did1...)
     *	    	H5Pget_append_flush(did2...) 
     *		-- should return append flush property values set in dapl1
     */

    /* Create a copy of dataset access property list */
    if((dapl1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR
    if((dapl2 = H5Pcreate(H5P_DATASET_ACCESS)) < 0) 
	FAIL_STACK_ERROR

    boundary1[0] = 0;
    boundary1[1] = 1;
    count1 = 0;
    if(H5Pset_append_flush(dapl1, 2, boundary1, append_cb, &count1) < 0)
	FAIL_STACK_ERROR
    boundary2[0] = 1;
    boundary2[1] = 0;
    count2 = 0;
    if(H5Pset_append_flush(dapl2, 2, boundary2, append_cb2, &count2) < 0)
	FAIL_STACK_ERROR

    /* Get a copy of the input parameter in_fapl */
    if((fapl = H5Pcopy(in_fapl)) < 0) 
	FAIL_STACK_ERROR

    /* Set to use the latest library format */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
	FAIL_STACK_ERROR

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
	FAIL_STACK_ERROR

    /* Create a chunked dataset with 2 extendible dimensions */
    if((sid = H5Screate_simple(2, dims, maxdims)) < 0) 
	FAIL_STACK_ERROR;
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) 
	FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
	FAIL_STACK_ERROR;
    if((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0) 
	FAIL_STACK_ERROR;

    /* Open the dataset */
    if((did2 = H5Dopen2(fid, "dataset1", dapl2)) < 0)
	FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did1 */
    if((ddapl = H5Dget_access_plist(did1)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values: should be the setting in dapl1 */
    if(ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != append_cb || ret_count != &count1) 
	TEST_ERROR

    /* Close the dataset's access property list */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did2 */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values: should be the setting in dapl1 */
    if(ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != append_cb || ret_count != &count1) 
	TEST_ERROR

    /* Close the dataset's access property list */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;
    H5Dclose(did1);
    H5Dclose(did2);

    /* 
     * Case (2) 
     *	For a chunked dataset's access property list:
     *		H5Dcreate(...H5P_DEFAULT...)
     *	    	H5Dclose()
     *	    	did1 = H5Dopen(...dapl1)
     *	    	did2 = H5Dopen(..dapl2) 
     *	    	H5Pget_append_flush(did1, ...)
     *	    	H5Pget_append_flush(did2, ...)
     *		-- should return append flush property values set in dapl1
     */
    if((did1 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) 
	FAIL_STACK_ERROR;
    if(H5Dclose(did1) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset with append flush setting in dapl2 */
    if((did1 = H5Dopen2(fid, "dataset2", dapl2)) < 0)
	FAIL_STACK_ERROR;

    /* Open the dataset with append flush setting in dapl1 */
    if((did2 = H5Dopen2(fid, "dataset2", dapl1)) < 0)
	FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did1 */
    if((ddapl = H5Dget_access_plist(did1)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values: should be the setting in dapl2 */
    if(ret_boundary[0] != 1 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != append_cb2 || ret_count != &count2) 
	TEST_ERROR

    /* Close the access property list */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;


    /* Get the dataset's access property list for did2 */
    if((ddapl = H5Dget_access_plist(did2)) < 0)
	FAIL_STACK_ERROR

    /* Retrieve the append flush property values */
    if(H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
	TEST_ERROR
    
    /* Verify expected values: should be the setting in dapl2 */
    if(ret_boundary[0] != 1 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
	TEST_ERROR;
    if(ret_cb != append_cb2 || ret_count != &count2) 
	TEST_ERROR

    /* Closing */
    if(H5Pclose(ddapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl2) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dapl1) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(dcpl) < 0)
	FAIL_STACK_ERROR;
    if(H5Pclose(fapl) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did1) < 0)
	FAIL_STACK_ERROR;
    if(H5Dclose(did2) < 0)
	FAIL_STACK_ERROR;
    if(H5Sclose(sid) < 0)
	FAIL_STACK_ERROR;
    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
	H5Pclose(dcpl);
	H5Pclose(dapl1);
	H5Pclose(dapl2);
	H5Pclose(ddapl);
	H5Dclose(did1);
	H5Dclose(did2);
	H5Pclose(fapl);
	H5Sclose(sid);
	H5Fclose(fid);
    } H5E_END_TRY;

    return -1;
} /* test_append_flush_dataset_multiple() */

/****************************************************************
**
**  test_file(): Main low-level file I/O test routine.
**
****************************************************************/
int
main(void)
{
    int nerrors = 0;	/* The # of errors */
    hid_t fapl = -1;	/* File access property list ID */

    /* Set up */
    h5_reset();

    /* Get file access property list */
    fapl = h5_fileaccess();

    /* Tests on H5Fstart_swmr_write() */
    nerrors += test_start_swmr_write(fapl);
    nerrors += test_err_start_swmr_write(fapl);
    nerrors += test_start_swmr_write_concur(fapl);

    /* Tests for H5Pget/set_object_flush_cb() */
    nerrors += test_object_flush_cb(fapl);

    /* Tests on H5Pget/set_append_flush() */
    nerrors += test_append_flush_generic();
    nerrors += test_append_flush_dataset_chunked(fapl);
    nerrors += test_append_flush_dataset_fixed(fapl);
    nerrors += test_append_flush_dataset_multiple(fapl);
    
    if(nerrors)
	goto error;

    printf("All tests passed for H5Fstart_swmr_write(), H5Pget/set_object_flush_cb(), H5Pget/set_append_flush().\n");

    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d SWMR TEST%s FAILED! *****\n",
		nerrors, 1 == nerrors ? "" : "S");
    return 1;

} /* main() */
