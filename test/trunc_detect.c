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
* Test Name:    trunc_detect
*
* Purpose:      Tests the library's methods of truncation detection.
* 
* Developer:    Mike McGreevy
*               January 31, 2011
*
*********************************************************************/

#define H5F_PACKAGE
#define FILENAME "trunc_detect.h5"
#define MULTIPARTNAME  "trunc_detect.h5-s.h5"
#define SPLITPARTNAME  "trunc_detect.h5-m.h5"
#define FAMILYPARTNAME "trunc_detect.h500000.h5"

#include "hdf5.h"
#include "testhdf5.h"
#include "H5srcdir.h"
#include "H5Iprivate.h"
#include "H5FDprivate.h"
#include "H5Fpkg.h"

/* Test Function Definitions */
int test_standard_file_truncation(hid_t fapl);
int test_avoid_truncate_1(hid_t fapl);
int test_avoid_truncate_2(hid_t fapl);


/*-------------------------------------------------------------------------
 *
 * Function:    main
 *
 * Purpose:     Tests the library's methods of truncation detection.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 31, 2011
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    /* Variable Declarations */
    unsigned nerrors = 0; /* Number of errors */
    hid_t fapl = -1; /* file access property list */
    const char * env_h5_drvr;

    /* Get current driver */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");

    /* Skip tests with multi, split, and family drivers (for now ... ) */
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
        HDfprintf(stdout, "Running tests on library's truncation detection methods:\n");
        if (!nerrors) nerrors += test_standard_file_truncation(fapl);
        if (!nerrors) nerrors += test_avoid_truncate_1(fapl);
        if (!nerrors) nerrors += test_avoid_truncate_2(fapl);

    } /* end else */

    return(nerrors > 0);
} /* main() */


/*-------------------------------------------------------------------------
 *
 * Function:    test_standard_file_truncation
 *
 * Purpose:     Tests the library's standard file truncation detection 
 *              mechanism (i.e., on file close, the library truncates
 *              the file to match the EOA value, and on file open then checks
 *              to see that the file size and stored EOF values are the same).
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 31, 2011
 *
 *-------------------------------------------------------------------------
 */
int test_standard_file_truncation(hid_t fapl)
{
    /* Variables */
    hid_t fid,did,sid = -1;         /* Object Descriptors */
    H5F_t * f = NULL;               /* Internal File Pointer */
    haddr_t eoa,eof = HADDR_UNDEF; /* End of File/Allocation values */
    char filename[1024]; /* File Name */
    int filesize;
    hbool_t v = FALSE;

    /* Testing Message */
    TESTING("truncating file to match EOA during file close");
    
    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Get internal file pointer */
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Create dataspace for dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR;

    /* Create dataset */
    if ((did = H5Dcreate2(fid, "Dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Close dataset, dataspace, and file */
    if (H5Dclose(did) < 0) TEST_ERROR;
    if (H5Sclose(sid) < 0) TEST_ERROR;
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Unlink the dataset */
    if (H5Ldelete(fid, "Dataset", H5P_DEFAULT) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;
    
    /* make sure the values are as different */
    if (eoa == eof) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* now get file size. */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR;

    /* File size should now match previous EOA value */
    if (eoa != (unsigned)filesize) TEST_ERROR;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* All values should now be in alignment. */
    if (eoa != eof) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* Extending the file should be fine. Add a byte to the end of the
        file, and make sure HDF5 doesn't care. (it will, however, write
        over the byte). */
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

    /* Return */
    PASSED();
    return SUCCEED;
error:
    return FAIL;
} /* test_standard_file_truncation */


/*-------------------------------------------------------------------------
 *
 * Function:    test_avoid_truncate_1
 *
 * Purpose:     Tests the library's method of stored the EOA in the superblock
 *              in order to detect file truncation rather than truncating the 
 *              file to match the EOA on file close.
 *
 *              This test verifies that the 'avoid truncate' feature can
 *              be enabled via setting the latest format flag. It also verifies
 *              that file truncation is detected correctly when a file is
 *              closed with its EOA value less than its EOF. (i.e., the file
 *              is not truncated down in size at file close).
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 31, 2011
 *
 *-------------------------------------------------------------------------
 */
int test_avoid_truncate_1(hid_t fapl)
{
    /* Variables */
    hid_t fcpl,fid,did,sid = -1;         /* Object Descriptors */
    H5F_t * f = NULL;               /* Internal File Pointer */
    haddr_t eoa,eof,eoa_new,eof_new = HADDR_UNDEF; /* End of File/Allocation values */
    char filename[1024]; /* File Name */
    int filesize;
    hbool_t avoid_truncate;
    hbool_t v = FALSE;

    /* Testing Message */
    TESTING("avoiding truncation when EOA < EOF at file close");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

#if 0
    /* Enable latest format (which should in turn enable 'avoid truncate') */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST,
                             H5F_LIBVER_LATEST) < 0) TEST_ERROR;

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;
#else
    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Check to see that 'avoid truncate' feature is disabled by default */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != FALSE) TEST_ERROR;

    /* Enable 'avoid truncate' feature */
    if (H5Pset_avoid_truncate(fcpl, TRUE) < 0) TEST_ERROR;

    /* Verify that retrieval gets new value for 'avoid truncate' feature */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != TRUE) TEST_ERROR;

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) TEST_ERROR;
#endif

    /* Get internal file pointer */
    if ( NULL == (f = (H5F_t *)H5I_object(fid)) ) TEST_ERROR;

    /* Create dataspace for dataset */
    if ((sid = H5Screate(H5S_SCALAR)) < 0) TEST_ERROR;

    /* Create dataset */
    if ((did = H5Dcreate2(fid, "Dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT,
                          H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;

    if ((eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* Close property list, dataset, dataspace, and file */
    if (H5Dclose(did) < 0) TEST_ERROR;
    if (H5Sclose(sid) < 0) TEST_ERROR;
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Get this file's creation properties */
    if ((fcpl = H5Fget_create_plist(fid)) < 0) TEST_ERROR;

    /* Verify that 'avoid truncate' feature is detected as enabled */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != TRUE) TEST_ERROR;

    /* Close the fcpl */
    if (H5Pclose(fcpl) < 0) TEST_ERROR;

    /* Unlink the dataset, reducing the 'EOA' value (but not EOF) */
    if (H5Ldelete(fid, "Dataset", H5P_DEFAULT) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;
    
    /* make sure the values are as expected */
    /* We want to be testing the case when EOA < EOF at file close */
    if (eoa >= eof) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;  

    /* now get file size. */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR;

    /* File size should be greater than previous 'EOA' value, as there should
        not have been a truncate call during file close */
    if (eoa >= (unsigned)filesize) TEST_ERROR;
    
    /* also verify that filesize is equal to previously retrieved 'EOF' value */
    if (filesize != eof) TEST_ERROR;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa_new = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof_new = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* Values should not have changed since before we closed the file */
    if (eoa != eoa_new) TEST_ERROR;
    if (eof != eof_new) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* Extending the file should be fine. Add a byte to the end of the
        file, and make sure HDF5 doesn't care. (it will, however, write
        over the byte). */
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

    /* Return */
    PASSED();
    return SUCCEED;
error:
    return FAIL;
} /* test_avoid_truncate_1 */


/*-------------------------------------------------------------------------
 *
 * Function:    test_avoid_truncate_2
 *
 * Purpose:     Tests the library's method of stored the EOA in the superblock
 *              in order to detect fiel truncation rather than truncating the 
 *              file to match the EOA on file close.
 *
 *              This test verifies that the 'avoid truncate' feature can
 *              be enabled and retrieved via its associated H5P API. It
 *              also verifies that file truncation is detected correctly when
 *              a file is closed with its EOA value greater than its EOF.
 *              (i.e., the file is not extended in size to match the EOA
 *              at file close).
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 31, 2011
 *
 *-------------------------------------------------------------------------
 */
int test_avoid_truncate_2(hid_t fapl)
{
    /* Variables */
    hid_t fcpl,fid,did,sid = -1;         /* Object Descriptors */
    H5F_t * f = NULL;               /* Internal File Pointer */
    haddr_t eoa,eof,eoa_new,eof_new = HADDR_UNDEF; /* End of File/Allocation values */
    int filesize;
    hbool_t avoid_truncate;
    hbool_t v = TRUE;
    hsize_t		dims[4] = {150, 15, 5, 5};
    hsize_t	    start[4] = {0,0,0,0};     /* Starting location of hyperslab */
    hsize_t		count[4] = {1,1,1,1};     /* Element count of hyperslab */
    uint16_t   *wbuf;       /* buffer to write to disk */
    char filename[1024]; /* File Name */
    herr_t		ret;		/* Generic return value		*/

    /* Testing Message */
    TESTING("avoiding truncation when EOA > EOF at file close");

    /* Allocate write buffer */
    if ((wbuf = (uint16_t *)HDmalloc(sizeof(uint16_t) * dims[0] * dims[1]
                                     * dims[2] * dims[3])) == NULL) TEST_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME, fapl, filename, (size_t)1024);

    /* Create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0) TEST_ERROR;

    /* Check to see that 'avoid truncate' feature is disabled by default */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != FALSE) TEST_ERROR;

    /* Enable 'avoid truncate' feature */
    if (H5Pset_avoid_truncate(fcpl, TRUE) < 0) TEST_ERROR;

    /* Verify that retrieval gets new value for 'avoid truncate' feature */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != TRUE) TEST_ERROR;

    /* Create a file that avoids truncation */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0) TEST_ERROR;

    /* Close the fcpl */
    if (H5Pclose(fcpl) < 0) TEST_ERROR;

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

    /* Retrieve EOA and EOF files. Make sure we set up a situation where EOA >
     * EOF prior to file close. */
    if ((eoa = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;
    if (eoa <= eof) TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0) TEST_ERROR;

    /* now get file size. */
    if ((filesize = h5_get_file_size(filename, fapl)) < 0) TEST_ERROR;

    /* Verify that file size is equal to previously retrieved EOF value (i.e.,
     * file was not truncated to the EOA value */
    if (filesize != eof) TEST_ERROR;

    /* Re-open file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Determine EOA and EOF values */
    if ((eoa_new = H5FD_get_eoa(f->shared->lf, H5FD_MEM_SUPER)) == HADDR_UNDEF)
        TEST_ERROR;
    if ((eof_new = H5FD_get_eof(f->shared->lf)) == HADDR_UNDEF) TEST_ERROR;

    /* Verify that values were properly set up to be as they were prior to
     * file close */
    if (eoa != eoa_new) TEST_ERROR;
    if (eof != eof_new) TEST_ERROR;
        
    /* Verify that EOA is still larger than EOF */
    if(eoa_new <= eof_new) TEST_ERROR;

    /* Get this file's creation properties */
    if ((fcpl = H5Fget_create_plist(fid)) < 0) TEST_ERROR;

    /* Verify that 'avoid truncate' feature is detected as enabled */
    if (H5Pget_avoid_truncate(fcpl, &avoid_truncate) < 0) TEST_ERROR;
    if (avoid_truncate != TRUE) TEST_ERROR;

    /* Close the fcpl */
    if (H5Pclose(fcpl) < 0) TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0) TEST_ERROR;  

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

    /* Return */
    PASSED();
    return SUCCEED;
error:
    return FAIL;
} /* test_avoid_truncate_2 */

