/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/***********************************************************
 *
 * Test program:  tverbounds18
 *
 * Test 1.8 compatibility with version bounds
 *
 * Description
 * ===========
 *       This file tests the compatibility in the files generated
 *       by gen_bounds.c in HDF5 1.10.2:
 *       - bounds_earliest_latest.h5
 *       - bounds_earliest_v18.h5
 *       - bounds_latest_latest.h5
 *       - bounds_v18_latest.h5
 *       - bounds_v18_v18.h5
 *
 * Oct 30, 2017
 *
 *************************************************************/

#include "h5test.h"
#include "H5srcdir.h"
#include "testhdf5.h"

/***********************************************************************
 * test_earliest_latest() reads file "bounds_earliest_latest.h5"
 *
 * Description:
 *   This test shows that the 1.8 library is able to open a chunked dataset
 *   with layout version 3 in a file with superblock version 0.
 *   However, it cannot open the chunked dataset with layout version 4
 *   in the same file.
 *
 ***********************************************************************/
#define FILENAME_E_L "bounds_earliest_latest.h5"
static void
test_earliest_latest(void)
{
    hid_t  fid  = FAIL; /* File ID */
    hid_t  dset = FAIL; /* Dataset ID */
    herr_t ret;         /* Return value */

    /* Test file name must have correct path when srcdir is used */
    const char *testfile = H5_get_srcdir_filename(FILENAME_E_L);

    /* Open file */
    fid = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /*
     * Open the chunked dataset with layout version 3
     */

    /* Open the dataset */
    dset = H5Dopen2(fid, "DS_chunked_layout_3", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /*
     * Open the chunked dataset with layout version 4.  This should fail
     * with HDF5 1.8.
     */

    /* Open the dataset */
    H5E_BEGIN_TRY
    {
        dset = H5Dopen2(fid, "DS_chunked_layout_4", H5P_DEFAULT);
    }
    H5E_END_TRY;
    VERIFY(dset, FAIL, "H5Dopen2");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/***********************************************************************
 * test_earliest_v18() reads file "bounds_earliest_v18.h5"
 *
 * Description:
 *   This test shows that the 1.8 library is able to open a chunked dataset
 *   with layout version 3 in a file with superblock version 0.
 *
 ***********************************************************************/
#define FILENAME_E_18 "bounds_earliest_v18.h5"

static void
test_earliest_v18(void)
{
    hid_t  fid  = FAIL; /* File ID */
    hid_t  dset = FAIL; /* Dataset ID */
    herr_t ret;         /* Return value */

    /* Test file name must have correct path when srcdir is used */
    const char *testfile = H5_get_srcdir_filename(FILENAME_E_18);

    /* Open file */
    fid = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /*
     * Open the chunked dataset with layout version 3
     */

    /* Open the dataset */
    dset = H5Dopen2(fid, "DS_chunked_layout_3", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/***********************************************************************
 * test_latest_latest() reads file "bounds_latest_latest.h5"
 *
 * Description:
 *   This test shows that the 1.8 library is unable to open a file with
 *   superblock version 3.
 *
 ***********************************************************************/
#define FILENAME_L_L "bounds_latest_latest.h5"

static void
test_latest_latest(void)
{
    hid_t fid = FAIL; /* File ID */

    /* Test file name must have correct path when srcdir is used */
    const char *testfile = H5_get_srcdir_filename(FILENAME_L_L);

    /* Opening the file of latest version bounds should fail with HDF5 1.8. */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY;
    VERIFY(fid, FAIL, "H5Fopen file with latest version bounds");
}

/***********************************************************************
 * test_v18_latest() reads file "bounds_v18_latest.h5"
 *
 * Description:
 *   This test shows that the 1.8 library is able to open a chunked dataset
 *   with layout version 3 in a file with superblock version 2.
 *
 ***********************************************************************/
#define FILENAME_18_L "bounds_v18_latest.h5"

static void
test_v18_latest(void)
{
    hid_t  fid  = FAIL; /* File ID */
    hid_t  dset = FAIL; /* Dataset ID */
    herr_t ret;         /* Return value */

    /* Test file name must have correct path when srcdir is used */
    const char *testfile = H5_get_srcdir_filename(FILENAME_18_L);

    /* Open file */
    fid = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /*
     * Open the chunked dataset with layout version 3
     */

    /* Open the dataset */
    dset = H5Dopen2(fid, "DS_chunked_layout_3", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/***********************************************************************
 * test_v18_v18() reads file "bounds_v18_v18.h5"
 *
 * Description:
 *   This test shows that the 1.8 library is able to open a chunked dataset
 *   with layout version 3 in a file with superblock version 2 and
 *   version 1.8 bounds.  However, it cannot open the chunked
 *   dataset with layout version 4 in the same file.
 *
 ***********************************************************************/
#define FILENAME_18_18 "bounds_v18_v18.h5"

static void
test_v18_v18(void)
{
    hid_t  fid  = FAIL; /* File ID */
    hid_t  dset = FAIL; /* Dataset ID */
    herr_t ret;         /* Return value */

    /* Test file name must have correct path when srcdir is used */
    const char *testfile = H5_get_srcdir_filename(FILENAME_18_18);

    /* Open file */
    fid = H5Fopen(testfile, H5F_ACC_RDWR, H5P_DEFAULT);
    CHECK(fid, FAIL, "H5Fopen");

    /*
     * Open the chunked dataset with layout version 3
     */

    /* Open the dataset */
    dset = H5Dopen2(fid, "DS_chunked_layout_3", H5P_DEFAULT);
    CHECK(dset, FAIL, "H5Dopen2");

    ret = H5Dclose(dset);
    CHECK(ret, FAIL, "H5Dclose");

    /*
     * Open the chunked dataset with layout version 4.  This should fail
     * with HDF5 1.8.
     */

    /* Open the dataset */
    H5E_BEGIN_TRY
    {
        dset = H5Dopen2(fid, "DS_chunked_layout_4", H5P_DEFAULT);
    }
    H5E_END_TRY;
    VERIFY(dset, FAIL, "H5Dopen2");

    /* Close the file */
    ret = H5Fclose(fid);
    CHECK(ret, FAIL, "H5Fclose");
}

/*************************************************************************
**  test_verbounds_18()
**      Main routine to test library version bounds with HDF5 1.8 library.
**
*************************************************************************/
void
test_verbounds_18(void)
{
    /* Output message about test being performed */
    MESSAGE(5, ("Testing Compatibility of Version Bounds with 1.8\n"));

    /* Test with file bounds_earliest_latest.h5 */
    test_earliest_latest();

    /* Test with file bounds_earliest_v18.h5 */
    test_earliest_v18();

    /* Test with file bounds_latest_latest.h5 */
    test_latest_latest();

    /* Test with file bounds_v18_latest.h5 */
    test_v18_latest();

    /* Test with file bounds_v18_v18.h5 */
    test_v18_v18();
}

/*-------------------------------------------------------------------------
 * Function:    cleanup_verbounds_18
 *
 * Purpose:     Cleanup temporary test files
 *
 * Return:      none
 *
 *-------------------------------------------------------------------------
 */
void
cleanup_verbounds_18(void)
{
}
