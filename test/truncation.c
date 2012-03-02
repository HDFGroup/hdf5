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

/*********************************************************************
*
* Test Name:    truncation
*
* Purpose:      Tests related to file truncation.
* 
* Developer:    Mike McGreevy
*               February 23, 2012
*
*********************************************************************/

#define H5F_PACKAGE
#define FILENAME "truncation.h5"

#include "hdf5.h"
#include "testhdf5.h"
#include "H5Iprivate.h"
#include "H5FDprivate.h"
#include "H5Fpkg.h"

/* Test Function Definitions */
int test_avoid_truncate_property(hid_t fapl);
int test_truncation_occurrence(hid_t fapl);
int test_truncation_detection(hid_t fapl);
int test_fcreate_with_at(hid_t fcpl, hid_t fapl, H5F_avoid_truncate_t at, hbool_t eoa);
hid_t create_test_file_eof_gt_eoa(char * filename, hid_t fcpl, hid_t fapl, haddr_t * eoa, haddr_t * eof);
hid_t create_test_file_eof_lt_eoa(char * filename, hid_t fcpl, hid_t fapl, haddr_t * eoa, haddr_t * eof);
int test_truncation_detection_helper(hid_t fcpl, hid_t fapl);


/*-------------------------------------------------------------------------
 *
 * Function:    main
 *
 * Purpose:     Tests related to file truncation.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              February 23, 2012
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{

    /* Variable Declarations */
    unsigned nerrors = 0; /* Number of errors */
    hid_t fapl = -1; /* file access property list */
    const char * env_h5_drvr; /* driver name */

    /* Get current driver */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");

    /* Skip tests with multi, split, log, and family drivers (for now ...) */
    if ((env_h5_drvr != NULL) &&
        ((!HDstrcmp(env_h5_drvr,"multi")) ||
         (!HDstrcmp(env_h5_drvr,"split")) ||
         (!HDstrcmp(env_h5_drvr,"log")) ||
         (!HDstrcmp(env_h5_drvr,"family")))) {

        HDfprintf(stdout, "Truncation Detection Test skipped with %s file driver\n", env_h5_drvr);

    } /* end if */
    else {

        /* Open the HDF5 Library */
        H5open();
        
        /* Create a fapl */
        fapl = h5_fileaccess();

        /* Run tests on truncation detection methods */
        if (!nerrors) nerrors += test_avoid_truncate_property(fapl);
        if (!nerrors) nerrors += test_truncation_occurrence(fapl);
        if (!nerrors) nerrors += test_truncation_detection(fapl);

    } /* end else */

return (nerrors > 0);

} /* end main */


/*-------------------------------------------------------------------------
 *
 * Function:    test_avoid_truncate_property
 *
 * Purpose:     Tests the library's setting of the avoid_truncate property
 *              and resulting addition of an 'EOA' message to the
 *              superblock extension.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              February 23, 2012
 *
 *-------------------------------------------------------------------------
 */
int test_avoid_truncate_property(hid_t fapl)
{
    /* ===================== */
    /* Variable Declarations */
    /* ===================== */
    hid_t fcpl, fapl_latest;
    H5F_avoid_truncate_t avoid_truncate;

    /* Testing Message */
    HDfprintf(stdout, "Testing avoid truncate property\n");

    /* ========== */
    /* Test Setup */
    /* ========== */

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) FAIL_STACK_ERROR;

    /* Copy the provided fapl */
    if ((fapl_latest = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR;

    /* Set latest format flag in copied fapl */
    if (H5Pset_libver_bounds(fapl_latest, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) FAIL_STACK_ERROR;

    /* ============= */
    /* Begin Testing */
    /* ============= */

    TESTING("default fcpl's 'avoid truncate' property = off");
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != H5F_AVOID_TRUNCATE_OFF) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = default, fapl = default");
    if (test_fcreate_with_at(fcpl, fapl, H5F_AVOID_TRUNCATE_OFF,FALSE) < 0) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = default, fapl = latest_format");
    if (test_fcreate_with_at(fcpl, fapl_latest, H5F_AVOID_TRUNCATE_EXTEND,TRUE) < 0) TEST_ERROR;
    PASSED();

    TESTING("set/get 'avoid truncate' property = all");
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_ALL) < 0) TEST_ERROR;
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != H5F_AVOID_TRUNCATE_ALL) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = all, fapl = default");
    if (test_fcreate_with_at(fcpl, fapl, H5F_AVOID_TRUNCATE_ALL,TRUE) < 0) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = all, fapl = latest_format");
    if (test_fcreate_with_at(fcpl, fapl_latest, H5F_AVOID_TRUNCATE_ALL,TRUE) < 0) TEST_ERROR;
    PASSED();
    
    TESTING("set/get 'avoid truncate' property = extend");
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_EXTEND) < 0) TEST_ERROR;
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != H5F_AVOID_TRUNCATE_EXTEND) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = extend, fapl = default");
    if (test_fcreate_with_at(fcpl, fapl, H5F_AVOID_TRUNCATE_EXTEND,TRUE) < 0) TEST_ERROR;
    PASSED();

    TESTING("file creation when fcpl = extend, fapl = latest_format");
    if (test_fcreate_with_at(fcpl, fapl_latest, H5F_AVOID_TRUNCATE_EXTEND,TRUE) < 0) TEST_ERROR;
    PASSED();

    TESTING("get/set 'avoid truncate' property = off");
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_OFF) < 0) TEST_ERROR;
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != H5F_AVOID_TRUNCATE_OFF) TEST_ERROR;
    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* test_avoid_truncate_property */


/*-------------------------------------------------------------------------
 *
 * Function:    test_fcreate_with_at
 *
 * Purpose:     Tests resulting avoid_truncate setting on files created
 *              with specific fcpl and fapl.
 *
 * Parameters:  hid_t fcpl:
 *                  file creation property list to use in H5Fcreate
 *              hid_t fapl:
 *                  file access property list to use in H5Fcreate
 *              H5F_avoid_truncate_t at
 *                  expected value of avoid truncate property
 *              hbool_t eoa
 *                  if the 'EOA' message should exist or not
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              February 23, 2012
 *
 *-------------------------------------------------------------------------
 */
int test_fcreate_with_at(hid_t fcpl, hid_t fapl, H5F_avoid_truncate_t at, hbool_t eoa) {

    /* Variables */
    hid_t fid = -1;         /* Object Descriptors */
    H5F_t * f = NULL;               /* Internal File Pointer */
    char filename[1024]; /* File Name */
    H5O_loc_t ext_loc;
    
    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) TEST_ERROR;

    /* Get internal file pointer */
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Verify that shared file pointer's avoid truncate value is as expected */
    if(f->shared->avoid_truncate != at) TEST_ERROR;

    /* Verify expected existence of 'EOA' message */
    if (f->shared->sblock->ext_addr != HADDR_UNDEF) {
        /* Open the superblock extension, if it exists. */
        if(H5F_super_ext_open(f, f->shared->sblock->ext_addr, &ext_loc) < 0) FAIL_STACK_ERROR;

        if(H5O_msg_exists(&ext_loc, H5O_EOA_ID, H5AC_dxpl_id)==TRUE) {
            if(eoa!=TRUE) TEST_ERROR;
        } else if(H5O_msg_exists(&ext_loc, H5O_EOA_ID, H5AC_dxpl_id)==FALSE) {
            if(eoa!=FALSE) TEST_ERROR;
        } else {
            FAIL_STACK_ERROR;
        } /* end else */

        /* Close sblock extension */
        if(H5F_super_ext_close(f, &ext_loc, H5AC_dxpl_id, FALSE) <0) FAIL_STACK_ERROR;
    } else {
        /* If there is no sblock extension but there's supposed to be an 'EOA' message: fail */
        if(eoa)TEST_ERROR;
    } /* end else */

    /* close file */
    if (H5Fclose(fid) < 0) FAIL_STACK_ERROR;

    /* remove file */
    HDremove(filename);

    return SUCCEED;
error:
    return FAIL;
} /* test_fcreate_with_at */


/*-------------------------------------------------------------------------
 *
 * Function:    test_truncation_occurrence
 *
 * Purpose:     Tests the cases when the library should/shouldn't truncate
 *              the file on file close.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              February 23, 2012
 *
 *-------------------------------------------------------------------------
 */
int test_truncation_occurrence(hid_t fapl)
{
    /* ===================== */
    /* Variable Declarations */
    /* ===================== */
    hid_t fcpl, fid;
    haddr_t eoa,eof;
    int filesize;
    char filename[1024]; /* File Name */

    /* Testing Message */
    HDfprintf(stdout, "Testing truncation occurrence\n");

    /* ========== */
    /* Test Setup */
    /* ========== */

    /* Fix the file's name */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) FAIL_STACK_ERROR;

    TESTING("at=off; file truncates when EOF > EOA prior to close")
    /* Create a file with EOF > EOA immediately prior to file close */
    if ((fid = create_test_file_eof_gt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has been truncated to match the EOA value */
    if (eoa != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is less than its EOF value before close */
    if ((unsigned)filesize >= eof) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    TESTING("at=off; file truncates when EOF < EOA prior to close")
    /* Create a file with EOF < EOA immediately prior to file close */
    if ((fid = create_test_file_eof_lt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has been truncated to match the EOA value */
    if (eoa != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is greater than the EOF value before close */
    if ((unsigned)filesize <= eof) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    /* Now set the avoid truncate property */
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_ALL) < 0) TEST_ERROR;

    TESTING("at=on; file avoids truncating when EOF > EOA prior to close")
    /* Create a file with EOF > EOA immediately prior to file close */
    if ((fid = create_test_file_eof_gt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has NOT been truncated, and still equals EOF before close */
    if (eof != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is greater than the EOA value */
    if ((unsigned)filesize <= eoa) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    TESTING("at=on; file avoids truncating when EOF < EOA prior to close")
    /* Create a file with EOF < EOA immediately prior to file close */
    if ((fid = create_test_file_eof_lt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has NOT been truncated, and still equals EOF before close */
    if (eof != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is less than the EOA value */
    if ((unsigned)filesize >= eoa) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    /* And finally test when only truncating on extend */
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_EXTEND) < 0) TEST_ERROR;

    TESTING("at=extend; file truncates when EOF > EOA prior to close")
    /* Create a file with EOF > EOA immediately prior to file close */
    if ((fid = create_test_file_eof_gt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has been truncated, and size equals EOA */
    if (eoa != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is less than the reported EOF value before close */
    if ((unsigned)filesize >= eof) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    TESTING("at=extend; file avoids truncate when EOF < EOA prior to close")
    /* Create a file with EOF < EOA immediately prior to file close */
    if ((fid = create_test_file_eof_lt_eoa(filename, fcpl, fapl, &eoa, &eof)) <0) FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get the file's size */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) FAIL_STACK_ERROR;

    /* Verify that the file has NOT been truncated, and still equals EOF */
    if (eof != (unsigned)filesize) TEST_ERROR;

    /* ... and that that value is less than the EOA value */
    if ((unsigned)filesize >= eoa) TEST_ERROR;

    /* remove file */
    HDremove(filename);
    PASSED();

    return SUCCEED;
error:
    return FAIL;
} /* test_truncation_occurrence */

/* Create a test file w/ EOF > EOA prior to file close */
hid_t create_test_file_eof_gt_eoa(char * filename, hid_t fcpl, hid_t fapl, haddr_t * eoa, haddr_t * eof)
{
    /* Variable declarations */
    hid_t fid,sid,did;
    H5F_t * f = NULL;               /* Internal File Pointer */

    /* Create a file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl))<0) FAIL_STACK_ERROR;

    /* Create dataspace for dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR;

    /* Create dataset */
    if ((did = H5Dcreate2(fid, "Dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Close up */
    H5Dclose(did);
    H5Sclose(sid);
    H5Fclose(fid);
    
    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Get internal file pointer */
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Unlink the dataset, reducing the 'EOA' value (but not EOF) */
    if (H5Ldelete(fid, "Dataset", H5P_DEFAULT) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((*eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((*eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* We want to be creating the case when EOA < EOF at file close */
    if (*eoa >= *eof) TEST_ERROR;

    return fid;
error:
    return FAIL;
} /* create_test_file_eof_gt_eoa */

/* Create a test file w/ EOF < EOA prior to file close */
hid_t create_test_file_eof_lt_eoa(char * filename, hid_t fcpl, hid_t fapl, haddr_t * eoa, haddr_t * eof)
{
    /* Variable declarations */
    hid_t fid,sid,did;
    H5F_t * f = NULL;               /* Internal File Pointer */
    hsize_t dims[4] = {150, 15, 5, 5};
    hsize_t start[4] = {0,0,0,0};     /* Starting location of hyperslab */
    hsize_t count[4] = {1,1,1,1};     /* Element count of hyperslab */
    uint16_t *wbuf;       /* buffer to write to disk */

    /* Allocate write buffer */
    if ((wbuf = (uint16_t *)HDmalloc((size_t)(sizeof(uint16_t) * dims[0] * dims[1]
                                     * dims[2] * dims[3]))) == NULL) TEST_ERROR;

    /* Create a file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl))<0) FAIL_STACK_ERROR;

    /* Get internal file pointer */
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Create dataspace for dataset */
    if ((sid = H5Screate_simple(4, dims, NULL)) < 0) TEST_ERROR;

    /* Select contiguous hyperslab for disk dataset */
    if (H5Sselect_hyperslab(sid,H5S_SELECT_SET,start,NULL,count,NULL) < 0) TEST_ERROR;

    /* Create a dataset */
    if ((did = H5Dcreate2(fid, "Dataset", H5T_STD_U16LE, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Write selection to disk */
    if (H5Dwrite(did, H5T_NATIVE_USHORT, sid, sid, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR;

    /* Free memory buffer */
    HDfree(wbuf);

    /* Close Dataspace and Dataset */
    if (H5Sclose(sid) < 0) TEST_ERROR;
    if (H5Dclose(did) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((*eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((*eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* We want to be creating the case when EOA > EOF at file close */
    if (*eoa <= *eof) TEST_ERROR;

    return fid;
error:
    return FAIL;
} /* create_test_file_eof_lt_eoa */


/*-------------------------------------------------------------------------
 *
 * Function:    test_truncation_detection
 *
 * Purpose:     Tests the library's ability to detect truncated files
 *              during file open.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              February 23, 2012
 *
 *-------------------------------------------------------------------------
 */
int test_truncation_detection(hid_t fapl)
{
    /* ===================== */
    /* Variable Declarations */
    /* ===================== */
    hid_t fcpl;

    /* Testing Message */
    HDfprintf(stdout, "Testing truncation detection\n");

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) FAIL_STACK_ERROR;

    TESTING("truncation detection on file without 'EOA' message");
    test_truncation_detection_helper(fcpl,fapl);
    PASSED();

    /* Set avoid truncate property in fcpl */
    if (H5Pset_avoid_truncate(fcpl, H5F_AVOID_TRUNCATE_ALL) < 0) TEST_ERROR;

    TESTING("truncation detection on file with 'EOA' message");
    test_truncation_detection_helper(fcpl,fapl);
    PASSED();
 
return SUCCEED;
error:
return FAIL;  
} /* test_truncation_detection */

int test_truncation_detection_helper(hid_t fcpl, hid_t fapl) {

    hid_t fid;
    int filesize;
    char filename[1024]; /* File Name */

    /* Fix the file's name */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

    /* Create & Close File */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) FAIL_STACK_ERROR;
    if (H5Fclose(fid)<0) FAIL_STACK_ERROR;

    /* Get file size. */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR;

    /* Extending the file should be fine. Add a byte to the end of the
     * file, and make sure HDF5 doesn't care. (it will, however, write
     * over the byte). */
    if (truncate(filename, (off_t)filesize+1) != 0) TEST_ERROR;

    /* Make sure we can open the file without problem */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* Manually truncate the file by a byte, rendering it unreadable by HDF5 */
    if (truncate(filename, (off_t)filesize-1) != 0) TEST_ERROR;

    /* Try to re-open file: this should fail, as the file has been truncated */
    H5E_BEGIN_TRY {
        fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    } H5E_END_TRY;
    if (fid >= 0) TEST_ERROR;

    /* Delete test file */
    HDremove(filename);

return SUCCEED;
error:
return FAIL;

} /* test_truncation_detection_helper */
