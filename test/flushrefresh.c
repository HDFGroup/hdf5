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
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer: Mike McGreevy
 *             June 30, 2010
 *
 * Purpose: This test file contains routines used to test flushing and 
 *          refreshing individual objects' metadata from the cache. 
 *          
 *          Note: This file should NOT be run manually. Instead, invoke it 
 *          via its associated test script, testflushrefresh.sh
 *
 */

#define H5FD_FRIEND		/*suppress error about including H5FDpkg	  */
#define H5FD_TESTING

/* ======== */
/* Includes */
/* ======== */

#include "testhdf5.h"
#include "H5FDpkg.h"		/* File Drivers				*/

/* ======= */
/* Defines */
/* ======= */

/* Name of Test File */
#define FILENAME "flushrefresh.h5"

/* Names of Signal Files */
#define SIGNAL_TO_SCRIPT "flushrefresh_VERIFICATION_START"
#define SIGNAL_BETWEEN_PROCESSES_1 "flushrefresh_VERIFICATION_CHECKPOINT1"
#define SIGNAL_BETWEEN_PROCESSES_2 "flushrefresh_VERIFICATION_CHECKPOINT2"
#define SIGNAL_FROM_SCRIPT "flushrefresh_VERIFICATION_DONE"

/* Paths to Various Objects in the Testfile */
#define RG "/"
#define D1 "/Dataset1"
#define D2 "/Group1/Dataset2"
#define D3 "/Group3/Dataset3"
#define G1 "/Group1"
#define G2 "/Group1/Group2"
#define G3 "/Group3"
#define T1 "/CommittedDatatype1"
#define T2 "/Group1/Group2/CommittedDatatype2"
#define T3 "/Group3/CommittedDatatype3"

/* Flushed States */
#define FLUSHED "FLUSHED"
#define NOT_FLUSHED "NOT_FLUSHED"

/* Error Handling */
/* For errors occuring in the main process, use the standard TEST_ERROR macro.
   For errors occurring in the spawned process (from the test script), use
   the PROCESS_ERROR macro, which will send a signal to the main process so the
   main process can propogate errors correctly. */
FILE * errorfile;
#define ERRFILE "flushrefresh_ERROR"
#define PROCESS_ERROR                                            \
    {                                                            \
      errorfile = HDfopen(ERRFILE, "w+");                        \
      HDfprintf(errorfile, "Error occurred in flushrefresh.\n"); \
      HDfflush(errorfile);                                       \
      HDfclose(errorfile);                                       \
      TEST_ERROR;                                                \
    }

#define CLEANUP_FILES                       \
    {                                       \
      HDremove(ERRFILE);                    \
      HDremove(FILENAME);                   \
      HDremove(SIGNAL_TO_SCRIPT);           \
      HDremove(SIGNAL_BETWEEN_PROCESSES_1); \
      HDremove(SIGNAL_BETWEEN_PROCESSES_2); \
      HDremove(SIGNAL_FROM_SCRIPT);         \
    }                                       \

/* ===================== */
/* Function Declarations */
/* ===================== */

/* Main */
int main(int argc, const char *argv[]);

/* Flush Test Framework */
herr_t test_flush(void);
herr_t flush_verification(const char * obj_pathname, const char * expected);
herr_t run_flush_verification_process(const char * obj_pathname, const char * expected);

/* Refresh Test Framework */
herr_t test_refresh(void);
herr_t refresh_verification(const char * obj_pathname);
herr_t start_refresh_verification_process(const char * obj_pathname);
herr_t end_refresh_verification_process(void);

/* Other Helper Functions */
herr_t check_for_errors(void);
herr_t end_verification(void);

/* ========= */
/* Functions */
/* ========= */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     This function coordinates the test of flush/refresh
 *              functionality verification. It accepts either one, two or 
 *              no command line parameters. The main test routine runs
 *              with no command line parameters specified, while verification
 *              routines run with one or two command line parameters.
 * 
 *              Note: This program should not be run manually, as the 
 *              test is controlled by the testflushrefresh.sh script. Running
 *              the flushrefresh program manually will result in failure, as
 *              it will time out waiting for a signal from the test script
 *              which will never come.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 1, 2010
 *
 *-------------------------------------------------------------------------
 */
int main(int argc, const char *argv[]) 
{
    /* Variables */
    const char *envval = NULL;

    /* Initialize library */
    if(H5open() < 0)
        TEST_ERROR;

    /* Parse command line options */
    if(argc == 1) {
        /* No arguments supplied. Run main test routines if 
         * using sec2 or stdio driver, otherwise don't run 
         * anything. */

        /* Determine driver being used */
        envval = HDgetenv("HDF5_DRIVER");

        if(envval == NULL || H5FD_supports_swmr_test(envval)) {
            if(test_flush() != SUCCEED) TEST_ERROR;
            if(test_refresh() != SUCCEED) TEST_ERROR;
        } /* end if */
        else {
            HDfprintf(stdout, "Skipping all flush/refresh tests (only run with SWMR-enabled file drivers).\n");
            
            /* Test script is expecting some signals, so send them out to end it. */
            if(end_verification() < 0) TEST_ERROR;
            if(end_verification() < 0) TEST_ERROR;
        } /* end else */
    } else if(argc == 3) {
        /* Two arguments supplied. Pass them to flush verification routine. */
        if(flush_verification(argv[1], argv[2]) != 0) PROCESS_ERROR;
    } else if(argc == 2) {
        /* One argument supplied. Pass it to refresh verification routine. */
        if(refresh_verification(argv[1]) != 0) PROCESS_ERROR;
    } else {
        /* Illegal number of arguments supplied. Error. */
        HDfprintf(stderr, "Error. %d is an Invalid number of arguments to main().\n", argc);
        PROCESS_ERROR
    } /* end if */

    return SUCCEED;

error:
    /* Return */
    return FAIL;
} /* main */


/*-------------------------------------------------------------------------
 * Function:    test_flush
 *
 * Purpose:     This function tests flushing individual objects' metadata
 *              from the metadata cache.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 1, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t test_flush(void) 
{
    /**************************************************************************
     *
     * Test Description:
     *
     * This test will build an HDF5 file with several objects in a varying 
     * hierarchical layout. It will then attempt to flush the objects
     * in the file one by one, individually, using the four H5*flush
     * routines (D,G,T, and O). After each call to either create or flush an 
     * object, a series of verifications will occur on each object in the file.
     *
     * Each verification consists of spawning off a new process and determining
     * if the object can be opened and its information retreived in said 
     * alternate process. It reports the results, which are compared to an 
     * expected value (either that the object can be found on disk, or that it
     * cannot).
     *
     * Note that to spawn a verification, this program sends a signal (by creating
     * a file on disk) to the test script controlling it, indicating how to 
     * run the verification.
     * 
     * Implementation is funky, but basically, an example: 
     *
     * Step 1. Dataset is created.
     * Step 2. Verify that dataset can't be opened by separate process, as
     *         it should not have been flushed to disk yet.
     * Step 3. Group is created.
     * Step 4. Verify that group can't be opened by separate process.
     * Step 5. H5Gflush is called on the group.
     * Step 6. Verify that group CAN be opened, but dataset still has
     *         yet to hit disk, and CANNOT be opened. Success! Only the group 
     *         was flushed.
     *
     **************************************************************************/

    /**************************************************************************
      * Generated Test File will look like this:
      * 
      * GROUP "/"
      *   DATASET "Dataset1"
      *   GROUP "Group1" {
      *     DATASET "Dataset2"
      *     GROUP "Group2" {
      *       DATATYPE "CommittedDatatype3"
      *     }
      *   }
      *   GROUP "Group3" {
      *     DATASET "Dataset3"
      *     DATATYPE "CommittedDatatype2"
      *   }
      *   DATATYPE "CommittedDatatype1"
     **************************************************************************/

    /* Variables */
    hid_t fid,gid,gid2,gid3,sid,tid1,tid2,tid3,did,did2,did3,rid,fapl,status = 0;
    hsize_t dims[2] = {3,5};

    /* Testing Message */
    HDfprintf(stdout, "Testing individual object flush behavior:\n");

    /* Cleanup any old error or signal files */
    CLEANUP_FILES;
    
    /* ================ */
    /* CREATE TEST FILE */
    /* ================ */

    /* Create file, open root group - have to use latest file format for SWMR */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR; 
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
    if((rid = H5Gopen2(fid, "/", H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create data space and types */
    if((sid = H5Screate_simple(2, dims, dims)) < 0) TEST_ERROR;
    if((tid1 = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if((tid2 = H5Tcopy(H5T_NATIVE_CHAR)) < 0) TEST_ERROR;
    if((tid3 = H5Tcopy(H5T_NATIVE_LONG)) < 0) TEST_ERROR;

    /* Create Group1 */
    if((gid = H5Gcreate2(fid, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Group2 */
    if((gid2 = H5Gcreate2(gid, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Group3 */
    if((gid3 = H5Gcreate2(fid, "Group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset1 */
    if((did = H5Dcreate2(fid, "Dataset1", tid1, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset2 */
    if((did2 = H5Dcreate2(gid, "Dataset2", tid3, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset3 */
    if((did3 = H5Dcreate2(gid3, "Dataset3", tid2, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype1 */
    if((status = H5Tcommit2(fid, "CommittedDatatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype2 */
    if((status = H5Tcommit2(gid2, "CommittedDatatype2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype3 */
    if((status = H5Tcommit2(gid3, "CommittedDatatype3", tid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* ============ */
    /* FLUSH GROUPS */
    /* ============ */

    /* Test */
    TESTING("to ensure H5Gflush correctly flushes single groups");

    /* First, let's verify that nothing is currently flushed. */
    if(run_flush_verification_process(RG, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Then, flush the root group and verify it's the only thing on disk */
    if((status = H5Gflush(rid)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush Group1 and Verify it is recently flushed, and nothing 
     * else has changed. */
    if((status = H5Gflush(gid)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush Group2 and Verify it is recently flushed, and nothing 
     * else has changed. */
    if((status = H5Gflush(gid2)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    PASSED();

    /* ============== */
    /* FLUSH DATASETS */
    /* ============== */

    /* Test */
    TESTING("to ensure H5Dflush correctly flushes single datasets");

    /* Flush Dataset1 and verify it's the only thing that hits disk. */
    if((status = H5Dflush(did)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush Dataset2 and verify it's the only thing that hits disk. */
    if((status = H5Dflush(did2)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    PASSED();

    /* =============== */
    /* FLUSH DATATYPES */
    /* =============== */

    /* Test */
    TESTING("to ensure H5Tflush correctly flushes single datatypes");

    /* Flush Datatype 1 and verify it's the only thing that hits disk. */
    if((status = H5Tflush(tid1)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush Datatype 2 and verify it's the only thing that hits disk. */
    if((status = H5Tflush(tid2)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    PASSED();
    
    /* ============= */
    /* FLUSH OBJECTS */
    /* ============= */

    /* Test */
    TESTING("to ensure H5Oflush correctly flushes single objects");

    /* Flush Group3 and verify it's the only thing that hits disk. */
    if((status = H5Oflush(gid3)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, NOT_FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush Dataset3 and verify it's the only thing that hits disk. */
    if((status = H5Oflush(did3)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, NOT_FLUSHED) != 0) TEST_ERROR;

    /* Flush CommittedDatatype3 and verify it's the only thing that hits disk. */
    if((status = H5Oflush(tid3)) < 0) TEST_ERROR;
    if(run_flush_verification_process(RG, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(G3, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(D3, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T1, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T2, FLUSHED) != 0) TEST_ERROR;
    if(run_flush_verification_process(T3, FLUSHED) != 0) TEST_ERROR;

    PASSED();

    /* ================== */
    /* Cleanup and Return */  
    /* ================== */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Dclose(did) < 0) TEST_ERROR;
    if(H5Dclose(did2) < 0) TEST_ERROR;
    if(H5Gclose(rid) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Delete test file */
    HDremove(FILENAME);

    if(end_verification() < 0) TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
} /* end test_flush */


/*-------------------------------------------------------------------------
 * Function:    test_refresh
 *
 * Purpose:     This function tests refresh (evict/reload) of individual 
 *              objects' metadata from the metadata cache.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              August 17, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t test_refresh(void) 
{
    /**************************************************************************
     *
     * Test Description:
     *
     * This test will build an HDF5 file with several objects in a varying 
     * hierarchical layout. It will then flush the entire file to disk. Then,
     * an attribute will be added to each object in the file.
     * 
     * One by one, this process will flush each object to disk, individually.
     * It will also be coordinating with another process, which will open
     * the object before it is flushed by this process, and then refresh the
     * object after it's been flushed, comparing the before and after object
     * information to ensure that they are as expected. (i.e., most notably,
     * that an attribute has been added, and is only visible after a 
     * successful call to a H5*refresh function).
     * 
     * As with the flush case, the implemention is a bit tricky as it's 
     * dealing with signals going back and forth between the two processes
     * to ensure the timing is correct, but basically, an example: 
     *
     * Step 1. Dataset is created.
     * Step 2. Dataset is flushed.
     * Step 3. Attribute on Dataset is created.
     * Step 4. Another process opens the dataset and verifies that it does
     *         not see an attribute (as the attribute hasn't been flushed yet).
     * Step 5. This process flushes the dataset again (with Attribute attached).
     * Step 6. The other process calls H5Drefresh, which should evict/reload
     *         the object's metadata, and thus pick up the attribute that's
     *         attached to it. Most other before/after object information is 
     *         compared for sanity as well.
     * Step 7. Rinse and Repeat for each object in the file.
     *
     **************************************************************************/

    /**************************************************************************
      * Generated Test File will look like this:
      * 
      * GROUP "/"
      *   DATASET "Dataset1"
      *   GROUP "Group1" {
      *     DATASET "Dataset2"
      *     GROUP "Group2" {
      *       DATATYPE "CommittedDatatype3"
      *     }
      *   }
      *   GROUP "Group3" {
      *     DATASET "Dataset3"
      *     DATATYPE "CommittedDatatype2"
      *   }
      *   DATATYPE "CommittedDatatype1"
     **************************************************************************/

    /* Variables */
    hid_t aid,fid,sid,tid1,did,dcpl,fapl = 0;
    hid_t gid,gid2,gid3,tid2,tid3,did2,did3;
    herr_t status = 0;
    hsize_t dims[2] = {50,50};
    hsize_t cdims[2] = {1,1};
    int fillval = 2;

    /* Testing Message */
    HDfprintf(stdout, "Testing individual object refresh behavior:\n");

    /* Cleanup any old error or signal files */
    CLEANUP_FILES;

    /* ================ */
    /* CREATE TEST FILE */
    /* ================ */

    /* Create File */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR;
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR;
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC|H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create data space and types */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if(H5Pset_chunk(dcpl, 2, cdims) < 0) TEST_ERROR;
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillval) < 0) TEST_ERROR;
    if((sid = H5Screate_simple(2, dims, dims)) < 0) TEST_ERROR;
    if((tid1 = H5Tcopy(H5T_NATIVE_INT)) < 0) TEST_ERROR;
    if((tid2 = H5Tcopy(H5T_NATIVE_CHAR)) < 0) TEST_ERROR;
    if((tid3 = H5Tcopy(H5T_NATIVE_LONG)) < 0) TEST_ERROR;

    /* Create Group1 */
    if((gid = H5Gcreate2(fid, "Group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Group2 */
    if((gid2 = H5Gcreate2(gid, "Group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Group3 */
    if((gid3 = H5Gcreate2(fid, "Group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset1 */
    if((did = H5Dcreate2(fid, "Dataset1", tid1, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset2 */
    if((did2 = H5Dcreate2(gid, "Dataset2", tid3, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create Dataset3 */
    if((did3 = H5Dcreate2(gid3, "Dataset3", tid2, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype1 */
    if((status = H5Tcommit2(fid, "CommittedDatatype1", tid1, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype2 */
    if((status = H5Tcommit2(gid2, "CommittedDatatype2", tid2, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Create CommittedDatatype3 */
    if((status = H5Tcommit2(gid3, "CommittedDatatype3", tid3, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Flush File to Disk */
    if(H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0) TEST_ERROR;

    /* ================ */
    /* Refresh Datasets */
    /* ================ */

    TESTING("to ensure that H5Drefresh correctly refreshes single datasets");

    /* Create an attribute on each object before flush. */

    /* Verify First Dataset can be refreshed with H5Drefresh */
    if(start_refresh_verification_process(D1) != 0) TEST_ERROR;

    if((aid = H5Acreate2(did, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(did) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    /* Verify Second Dataset can be refreshed with H5Drefresh */
    if(start_refresh_verification_process(D2) != 0) TEST_ERROR;

    if((aid = H5Acreate2(did2, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(did2) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    PASSED();

    /* ============== */
    /* Refresh Groups */
    /* ============== */

    TESTING("to ensure that H5Grefresh correctly refreshes single groups");

    /* Verify First Group can be refreshed with H5Grefresh */
    if(start_refresh_verification_process(G1) != 0) TEST_ERROR;

    if((aid = H5Acreate2(gid, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(gid) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    /* Verify Second Group can be refreshed with H5Grefresh */
    if(start_refresh_verification_process(G2) != 0) TEST_ERROR;

    if((aid = H5Acreate2(gid2, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(gid2) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    PASSED();

    /* ================= */
    /* Refresh Datatypes */
    /* ================= */

    TESTING("to ensure that H5Trefresh correctly refreshes single datatypes");

    /* Verify First Committed Datatype can be refreshed with H5Trefresh */
    if(start_refresh_verification_process(T1) != 0) TEST_ERROR;

    if((aid = H5Acreate2(tid1, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(tid1) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    /* Verify Second Committed Datatype can be refreshed with H5Trefresh */
    if(start_refresh_verification_process(T2) != 0) TEST_ERROR;

    if((aid = H5Acreate2(tid2, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(tid2) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    PASSED();

    /* =============== */
    /* Refresh Objects */
    /* =============== */

    TESTING("to ensure that H5Orefresh correctly refreshes single objects");

    /* Verify Third Dataset can be refreshed with H5Orefresh */
    if(start_refresh_verification_process(D3) != 0) TEST_ERROR;

    if((aid = H5Acreate2(did3, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(did3) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    /* Verify Third Group can be refreshed with H5Orefresh */
    if(start_refresh_verification_process(G3) != 0) TEST_ERROR;

    if((aid = H5Acreate2(gid3, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(gid3) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    /* Verify Third Committed Datatype can be refreshed with H5Orefresh */
    if(start_refresh_verification_process(T3) != 0) TEST_ERROR;

    if((aid = H5Acreate2(tid3, "Attribute", tid1, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;
    if(H5Aclose(aid) < 0) TEST_ERROR;
    if(H5Oflush(tid3) < 0) TEST_ERROR;

    if(end_refresh_verification_process() != 0) TEST_ERROR;

    PASSED();

    /* ================== */
    /* Cleanup and Return */  
    /* ================== */

    /* Close Stuff */
    if(H5Pclose(fapl) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Tclose(tid1) < 0) TEST_ERROR;
    if(H5Tclose(tid2) < 0) TEST_ERROR;
    if(H5Tclose(tid3) < 0) TEST_ERROR;
    if(H5Dclose(did) < 0) TEST_ERROR;
    if(H5Dclose(did2) < 0) TEST_ERROR;
    if(H5Dclose(did3) < 0) TEST_ERROR;
    if(H5Gclose(gid) < 0) TEST_ERROR;
    if(H5Gclose(gid2) < 0) TEST_ERROR;
    if(H5Gclose(gid3) < 0) TEST_ERROR;
    if(H5Sclose(sid) < 0) TEST_ERROR;
    if(H5Fclose(fid) < 0) TEST_ERROR;

    /* Delete Test File */
    HDremove(FILENAME);

    if(end_verification() < 0) TEST_ERROR;

    return SUCCEED;

error:
    /* Return */
    return FAIL;
} /* test_refresh() */


/*-------------------------------------------------------------------------
 * Function:    run_flush_verification_process
 *
 * Purpose:     This function is used to communicate with the test script
 *              in order to spawn off a process to verify that a flush
 *              of an individual object was successful. 
 * 
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t run_flush_verification_process(const char * obj_pathname, const char * expected) 
{
    HDremove(SIGNAL_FROM_SCRIPT);

    /* Send Signal to SCRIPT indicating that it should kick off a verification process. */
    h5_send_message(SIGNAL_TO_SCRIPT, obj_pathname, expected);

    /* Wait for Signal from SCRIPT indicating that verification process has completed. */
    if(h5_wait_message(SIGNAL_FROM_SCRIPT) < 0) TEST_ERROR;

    /* Check to see if any errors occurred */
    if(check_for_errors() < 0) TEST_ERROR;

    /* Return */
    return SUCCEED;

error:
    return FAIL;
} /* run_flush_verification_process */


/*-------------------------------------------------------------------------
 * Function:    flush_verification
 *
 * Purpose:     This function tries to open target object in the test file.
 *              It compares the success of the open function to the expected 
 *              value, and succeeds if they are equal and fails if they differ.
 *
 *              Note that full path to the object must be provided as the 
 *              obj_pathname argument.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t flush_verification(const char * obj_pathname, const char * expected) 
{
    /* Variables */
    hid_t oid = -1, fid = -1;
    herr_t status = 0;
    H5O_info_t oinfo;

    /* Try to open the testfile and then obj_pathname within the file */
    H5E_BEGIN_TRY {
        fid = H5Fopen(FILENAME, H5F_ACC_SWMR_READ, H5P_DEFAULT);
        oid = H5Oopen(fid, obj_pathname, H5P_DEFAULT);
        status = H5Oget_info(oid, &oinfo);
    } H5E_END_TRY;

    /* Compare to expected result */
    if(HDstrcmp(expected, FLUSHED) == 0) {
        if((oid < 0) || (status < 0)) {
            HDfprintf(stderr, "Error! %s should be on disk, but was NOT!\n", obj_pathname);
            PROCESS_ERROR;
        } /* end if */
    } else if(HDstrcmp(expected, NOT_FLUSHED) == 0) {
        if((oid > 0) || (status > 0)) {
            HDfprintf(stderr, "Error! %s not expected to be flushed, but it was found on disk!\n", obj_pathname);
            PROCESS_ERROR;
        } /* end if */
    } else {
        HDfprintf(stderr, "Error! Bad verification parameters. %s is an invalid expected outcome.\n", expected);
        PROCESS_ERROR;
    } /* end if */

    /* Cleanup */
    H5E_BEGIN_TRY {
        H5Oclose(oid);
        H5Fclose(fid);
    } H5E_END_TRY;

    return SUCCEED;

error:
    return FAIL;
} /* flush_verification */


/*-------------------------------------------------------------------------
 * Function:    start_refresh_verification_process
 *
 * Purpose:     This function is used to communicate with the test script
 *              in order to spawn off a process which will test the
 *              H5*refresh routine.
 * 
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t start_refresh_verification_process(const char * obj_pathname) 
{
    HDremove(SIGNAL_BETWEEN_PROCESSES_1);

    /* Send Signal to SCRIPT indicating that it should kick off a refresh 
       verification process */
    h5_send_message(SIGNAL_TO_SCRIPT, obj_pathname, NULL);
    
    /* Wait for Signal from VERIFICATION PROCESS indicating that it's opened the
       target object and ready for MAIN PROCESS to modify it */
    if(h5_wait_message(SIGNAL_BETWEEN_PROCESSES_1) < 0) TEST_ERROR;

    /* Check to see if any errors occurred */
    if(check_for_errors() < 0) TEST_ERROR;

    /* Return */
    return SUCCEED;

error:
    return FAIL;
} /* start_refresh_verification_process */


/*-------------------------------------------------------------------------
 * Function:    end_refresh_verification_process
 *
 * Purpose:     This function is used to communicate with the verification
 *              process spawned by the start_refresh_verification_process 
 *              function. It gives it the go-ahead to call H5*refresh
 *              on an object and conlcude the refresh verification.
 * 
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t end_refresh_verification_process(void) 
{ 
    HDremove(SIGNAL_FROM_SCRIPT);

    /* Send Signal to REFRESH VERIFICATION PROCESS indicating that the object
        has been modified and it should now attempt to refresh its metadata,
        and verify the results. */
    h5_send_message(SIGNAL_BETWEEN_PROCESSES_2, NULL, NULL);

    /* Wait for Signal from SCRIPT indicating that the refresh verification
        process has completed. */
    if(h5_wait_message(SIGNAL_FROM_SCRIPT) < 0) TEST_ERROR;

    /* Check to see if any errors occurred */
    if(check_for_errors() < 0) TEST_ERROR;

    /* Return */
    return SUCCEED;

error:
    return FAIL;
} /* end_refresh_verification_process */


/*-------------------------------------------------------------------------
 * Function:    refresh_verification
 *
 * Purpose:     This function opens the specified object, and checks to see
 *              that is does not have any attributes on it. It then sends
 *              a signal to the main process, which will flush the object
 *              (putting an attribute on the object on disk). This function
 *              will then refresh the object, and verify that it has picked
 *              up the new metadata reflective of the added attribute.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t refresh_verification(const char * obj_pathname) 
{
    /* Variables */
    hid_t oid,fid,status = 0;
    H5O_info_t flushed_oinfo;
    H5O_info_t refreshed_oinfo;
    int tries = 800, sleep_tries = 400;
    hbool_t ok = FALSE;
    
    HDremove(SIGNAL_BETWEEN_PROCESSES_2);

    /* Open Object */
    if((fid = H5Fopen(FILENAME, H5F_ACC_SWMR_READ, H5P_DEFAULT)) < 0) PROCESS_ERROR;
    if((oid = H5Oopen(fid, obj_pathname, H5P_DEFAULT)) < 0) PROCESS_ERROR;

    /* Get Object info */
    if((status = H5Oget_info(oid, &flushed_oinfo)) < 0) PROCESS_ERROR;
    
    /* Make sure there are no attributes on the object. This is just a sanity
        check to ensure we didn't erroneously flush the attribute before
        starting the verification. */
    if(flushed_oinfo.num_attrs != 0)
	PROCESS_ERROR;

    /* Send Signal to MAIN PROCESS indicating that it can go ahead and modify the 
        object. */
    h5_send_message(SIGNAL_BETWEEN_PROCESSES_1, obj_pathname, NULL);

    /* Wait for Signal from MAIN PROCESS indicating that it's modified the 
        object and we can run verification now. */
    if(h5_wait_message(SIGNAL_BETWEEN_PROCESSES_2) < 0) PROCESS_ERROR;

    /* Get object info again. This will NOT reflect what's on disk, only what's 
       in the cache. Thus, all values will be unchanged from above, despite 
       newer information being on disk. */
    if((status = H5Oget_info(oid, &refreshed_oinfo)) < 0) PROCESS_ERROR;

    /* Verify that before doing a refresh, getting the object info returns stale
       information. (i.e., unchanged from above, despite new info on disk). */
    if(flushed_oinfo.addr != refreshed_oinfo.addr) PROCESS_ERROR;
    if(flushed_oinfo.type != refreshed_oinfo.type) PROCESS_ERROR;
    if(flushed_oinfo.hdr.version != refreshed_oinfo.hdr.version) PROCESS_ERROR;
    if(flushed_oinfo.hdr.flags != refreshed_oinfo.hdr.flags) PROCESS_ERROR;
    if(flushed_oinfo.num_attrs != refreshed_oinfo.num_attrs) PROCESS_ERROR;
    if(flushed_oinfo.hdr.nmesgs != refreshed_oinfo.hdr.nmesgs) PROCESS_ERROR;
    if(flushed_oinfo.hdr.nchunks != refreshed_oinfo.hdr.nchunks) PROCESS_ERROR;
    if(flushed_oinfo.hdr.space.total != refreshed_oinfo.hdr.space.total) PROCESS_ERROR;

    /* Refresh object */
    /* The H5*refresh function called depends on which object we are trying
     * to refresh. (MIKE: add desired refresh call as parameter so adding new
     * test cases is easy). */
    do {

	if((HDstrcmp(obj_pathname, D1) == 0) || (HDstrcmp(obj_pathname, D2) == 0)) {
	    if(H5Drefresh(oid) < 0) PROCESS_ERROR;
	} /* end if */
	else if((HDstrcmp(obj_pathname, G1) == 0) || (HDstrcmp(obj_pathname, G2) == 0)) {
	    if(H5Grefresh(oid) < 0) PROCESS_ERROR;
	} /* end if */
	else if((HDstrcmp(obj_pathname, T1) == 0) || (HDstrcmp(obj_pathname, T2) == 0)) {
	    if(H5Trefresh(oid) < 0) PROCESS_ERROR;
	} /* end if */
	else if((HDstrcmp(obj_pathname, D3) == 0) || (HDstrcmp(obj_pathname, G3) == 0) ||
                (HDstrcmp(obj_pathname, T3) == 0)) {
	    if(H5Orefresh(oid) < 0) PROCESS_ERROR;
	} /* end if */
	else {
	    HDfprintf(stdout, "Error. %s is an unrecognized object.\n", obj_pathname);
	    PROCESS_ERROR;
	} /* end else */

	/* Get object info. This should now accurately reflect the refreshed object on disk. */
	if((status = H5Oget_info(oid, &refreshed_oinfo)) < 0) PROCESS_ERROR;
    
	/* Confirm following (first 4) attributes are the same: */
	/* Confirm following (last 4) attributes are different */
	if( (flushed_oinfo.addr == refreshed_oinfo.addr) &&
	    (flushed_oinfo.type == refreshed_oinfo.type) &&
	    (flushed_oinfo.hdr.version == refreshed_oinfo.hdr.version) &&
	    (flushed_oinfo.hdr.flags == refreshed_oinfo.hdr.flags) &&
	    (flushed_oinfo.num_attrs != refreshed_oinfo.num_attrs) &&
	    (flushed_oinfo.hdr.nmesgs != refreshed_oinfo.hdr.nmesgs) &&
	    (flushed_oinfo.hdr.nchunks != refreshed_oinfo.hdr.nchunks) &&
	    (flushed_oinfo.hdr.space.total != refreshed_oinfo.hdr.space.total) ) {
		ok = TRUE;
		break;
	}
	if(tries == sleep_tries)
	    HDsleep(1);

    } while(--tries);
    
    if(!ok) {
	printf("FLUSHED: num_attrs=%d, nmesgs=%d, nchunks=%d, total=%d\n",
	    (int)flushed_oinfo.num_attrs, (int)flushed_oinfo.hdr.nmesgs,
	    (int)flushed_oinfo.hdr.nchunks, (int)flushed_oinfo.hdr.space.total);
	printf("REFRESHED: num_attrs=%d, nmesgs=%d, nchunks=%d, total=%d\n",
	    (int)refreshed_oinfo.num_attrs, (int)refreshed_oinfo.hdr.nmesgs,
	    (int)refreshed_oinfo.hdr.nchunks, (int)refreshed_oinfo.hdr.space.total);
	PROCESS_ERROR;
    }
    
    /* Close objects */
    if(H5Oclose(oid) < 0) PROCESS_ERROR;
    if(H5Fclose(fid) < 0) PROCESS_ERROR;

    /* Return */
    return SUCCEED;

error:
    return FAIL;
} /* refresh_verification */


/*-------------------------------------------------------------------------
 * Function:    check_for_errors()
 *
 * Purpose:     This function checks the status of external verification
 *              processes to see if they've succeeded. It checks for the
 *              existance of flushrefresh_ERROR file. If present, that indicates
 *              an external verification process has failed, and this function
 *              thus fails as well. If not present, then nothing else has 
 *              failed, and this function succeeds.
 *
 * Return:      0 on Success, 1 on Failure
 *
 * Programmer:  Mike McGreevy
 *              July 1, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t check_for_errors(void) 
{
    FILE * file;

    if((file = HDfopen(ERRFILE, "r"))) {
        HDfclose(file);
        HDremove(ERRFILE);
        return FAIL;
    } /* end if */

    return SUCCEED;
} /* check_for_errors */


/*-------------------------------------------------------------------------
 * Function:    end_verification
 * 
 * Purpose:     Tells test script that verification routines are completed and 
 *              that the test can wrap up. 
 *
 * Return:      void
 *
 * Programmer:  Mike McGreevy
 *              July 16, 2010
 * 
 *-------------------------------------------------------------------------
 */
herr_t end_verification(void) 
{
    HDremove(SIGNAL_FROM_SCRIPT);

    /* Send Signal to SCRIPT to indicate that we're done with verification. */
    h5_send_message(SIGNAL_TO_SCRIPT, "VERIFICATION_DONE", "VERIFICATION_DONE");
    
    /* Wait for Signal from SCRIPT indicating that we can continue. */
    if(h5_wait_message(SIGNAL_FROM_SCRIPT) < 0) TEST_ERROR;

    return SUCCEED;

error:
    return FAIL;
} /* end_verification */

