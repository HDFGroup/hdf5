/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Test program:  swmr
 *
 *   To test new public routines from SWMR project:
 *       H5Pget/set_metadata_read_attempts()
 *       H5Fget_metadata_read_retry_info()
 *       H5Fstart_swmr_write()
 *       H5Pget/set_object_flush_cb()
 *       H5Pget/set_append_flush()
 *
 *************************************************************/

#include "h5test.h"
#include "H5Iprivate.h"
#include "H5VLprivate.h" /* Virtual Object Layer                     */

/*
 * This file needs to access private information from the H5F package.
 * This file also needs to access the file, file driver, dataset,
 * and object header testing code.
 */
#define H5F_FRIEND /*suppress error about including H5Fpkg   */
#define H5F_TESTING
#include "H5Fpkg.h" /* File access              */

#define H5D_FRIEND /*suppress error about including H5Dpkg          */
#define H5D_TESTING
#include "H5Dpkg.h" /* Datasets                 */

#define H5FD_FRIEND /*suppress error about including H5FDpkg      */
#define H5FD_TESTING
#include "H5FDpkg.h" /* File drivers             */

#define H5O_FRIEND /*suppress error about including H5Opkg      */
#define H5O_TESTING
#include "H5Opkg.h" /* Object headers            */

static const char *FILENAME[] = {"swmr0", /* 0 */
                                 "swmr1", /* 1 */
                                 "swmr2", /* 2 */
                                 NULL};

#define NAME_BUF_SIZE 1024 /* Length of file name */

/* Epsilon for floating-point comparisons */
#define FP_EPSILON 0.000001F

/* Tests for H5Pget/set_metadata_read_attempts(), H5Fget_metadata_read_retry_info */
static int test_metadata_read_attempts(hid_t in_fapl);
static int test_metadata_read_retry_info(hid_t in_fapl);

/* Tests for H5Fstart_swmr_write() */
static int test_start_swmr_write(hid_t in_fapl, bool new_format);
static int test_err_start_swmr_write(hid_t in_fapl, bool new_format);
static int test_start_swmr_write_concur(hid_t in_fapl, bool new_format);
static int test_start_swmr_write_stress_ohdr(hid_t in_fapl);
static int test_start_swmr_write_persist_dapl(hid_t in_fapl);

/* Tests for H5Pget/set_object_flush_cb() */
static herr_t flush_cb(hid_t obj_id, void *_udata);
static int    test_object_flush_cb(hid_t in_fapl);

/* Tests for H5Pget/set_append_flush() */
static herr_t append_cb(hid_t dset_id, hsize_t *cur_dims, void *_udata);
static herr_t append_cb2(hid_t dset_id, hsize_t *cur_dims, void *_udata);
static int    test_append_flush_generic(void);
static int    test_append_flush_dataset_chunked(hid_t in_fapl);
static int    test_append_flush_dataset_fixed(hid_t in_fapl);
static int    test_append_flush_dataset_multiple(hid_t in_fapl);

/* Tests for file open flags/SWMR flags: single process access */
static int test_file_lock_same(hid_t fapl);
static int test_file_lock_swmr_same(hid_t fapl);

/* Tests for file open flags/SWMR flags: concurrent process access */
static int test_file_lock_concur(hid_t fapl);
static int test_file_lock_swmr_concur(hid_t fapl);

/* Test file lock environment variable */
static int test_file_locking(hid_t in_fapl, bool turn_locking_on, bool env_var_override);

/* Tests for SWMR VFD flag */
static int test_swmr_vfd_flag(void);

/* Tests for H5Drefresh: concurrent access */
static int test_refresh_concur(hid_t in_fapl, bool new_format);

/* Tests for multiple opens of files and datasets with H5Drefresh() & H5Fstart_swmr_write(): same process */
static int test_multiple_same(hid_t in_fapl, bool new_format);

/*
 * Tests for H5Pget/set_metadata_read_attemps(), H5Fget_metadata_read_retry_info()
 */

/*
 *  test_metadata_read_attempts():
 *
 *  Checks the following two public routines work as specified:
 *  H5Pset_metadata_read_attempts()
 *  H5Pget_metadata_read_attempts()
 */
static int
test_metadata_read_attempts(hid_t in_fapl)
{
    hid_t         fapl      = H5I_INVALID_HID; /* File access property list            */
    hid_t         file_fapl = H5I_INVALID_HID; /* The file's access property list      */
    hid_t         fid = H5I_INVALID_HID, fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID; /* File IDs */
    hid_t         driver_id    = H5I_INVALID_HID; /* ID for this VFD                      */
    unsigned long driver_flags = 0;               /* VFD feature flags                    */
    bool          compat_w_default_vfd;           /* current VFD compat w/ H5P_DEFAULT?   */
    unsigned      attempts;                       /* The # of read attempts               */
    char          filename[NAME_BUF_SIZE];        /* File name                            */
    herr_t        ret;                            /* Generic return value                 */

    /* Output message about test being performed */
    TESTING("H5Pget/set_metadata_read_attempts()");

    /* Check if the driver is compatible with the default VFD.
     * Most of the tests will attempt to create and open files with both the
     * VFD specified in the passed-in fapl and the default VFD. Since this
     * will clearly not work with VFDs that are not compatible with the default
     * fapl (e.g.: split/multi), we just skip this entire test.
     */
    if ((driver_id = H5Pget_driver(in_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5FDdriver_query(driver_id, &driver_flags) < 0)
        FAIL_STACK_ERROR;
    compat_w_default_vfd = (driver_flags & H5FD_FEAT_DEFAULT_VFD_COMPATIBLE) ? true : false;

    if (!compat_w_default_vfd) {
        SKIPPED();
        puts("    The current VFD is not compatible with the default VFD.");
        return 0;
    }

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /*
     * Set A:
     *  Tests on verifying the # of read attempts when:
     *    --setting/getting read attempts from a
     *      file access property list.
     */
    /* Get # of read attempts -- should be the default: 1 */
    if (H5Pget_metadata_read_attempts(fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 1)
        TEST_ERROR;

    /* Set the # of read attempts to 0--should fail */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_metadata_read_attempts(fapl, 0);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Set the # of read attempts to a # > 0--should succeed */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        TEST_ERROR;

    /* Retrieve the # of read attempts -- should be 9 */
    if (H5Pget_metadata_read_attempts(fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 9)
        TEST_ERROR;

    /* Set the # of read attempts to the default for non-SWMR access: H5F_METADATA_READ_ATTEMPTS --should
     * succeed */
    if (H5Pset_metadata_read_attempts(fapl, H5F_METADATA_READ_ATTEMPTS) < 0)
        TEST_ERROR;

    /* Retrieve the # of read attempts -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Set the # of read attempts to the default for SWMR access: H5F_SWMR_METADATA_READ_ATEMPTS --should
     * succeed */
    if (H5Pset_metadata_read_attempts(fapl, H5F_SWMR_METADATA_READ_ATTEMPTS) < 0)
        TEST_ERROR;

    /* Retrieve the # of read attempts -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set B:
     *  Tests on verifying read attempts when:
     *    --create a file with non-SWMR access
     *    --opening files with SWMR access
     *    --using default or non-default file access property list
     */
    /* Test 1 */
    /* Create a file with non-SWMR access and default fapl */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 2 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access and default read attempts */
    if ((fid = H5Fopen(filename, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 3 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access and fapl (non-default & set to 9) */
    if ((fid = H5Fopen(filename, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be 9 */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 9)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 4 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 1) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access and fapl (non-default & set to 1) */
    if ((fid = H5Fopen(filename, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be 1 */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 1)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 5 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR_READ and fapl (non-default read attempts but unset) */
    if ((fid = H5Fopen(filename, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set C:
     *  Tests on verifying read attempts when:
     *    --create a file with SWMR access
     *    --opening files with non-SWMR access
     *    --using default or non-default file access property list
     */
    /* Test 1 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create a file with non-SWMR access and default read attempts */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 2 */
    /* Open the file with non-SWMR access and default fapl */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 3 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with non-SWMR access and fapl (non-default & set to 9) */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be 9 */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 9)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 4 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 1) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with non-SWMR access and fapl (non-default & set to 1) */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be 1 */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != 1)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Test 5 */
    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with non-SWMR_READ and fapl (non-default but unset) */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file's fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        FAIL_STACK_ERROR;

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with non-SWMR access and default fapl */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with SWMR access and default read attempts */
    if ((fid = H5Fopen(filename, H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set D:
     *  Tests on verifying read attempts when:
     *    --create with non-SWMR access
     *    --opening files with SWMR access
     *    --H5reopen the files
     */

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with SWMR access and default read attempts */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with SWMR access and fapl (non-default & set to 9) */
    if ((fid2 = H5Fopen(filename, (H5F_ACC_RDONLY | H5F_ACC_SWMR_READ), fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Re-open fid1 */
    if ((fid = H5Freopen(fid1)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open fid2 */
    if ((fid = H5Freopen(fid2)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_SWMR_METADATA_READ_ATTEMPTS, not 9 */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close all the files */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Set E:
     *  Tests on verifying read attempts when:
     *    --create with SWMR access
     *    --opening files with non-SWMR access
     *    --H5reopen the files
     */

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with non-SWMR access and default fapl */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the parameter fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the # of read attempts */
    if (H5Pset_metadata_read_attempts(fapl, 9) < 0)
        FAIL_STACK_ERROR;

    /* Open file again with non-SWMR access and fapl (non-default & set to 9) */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close fapl */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Re-open fid1 */
    if ((fid = H5Freopen(fid1)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Re-open fid2 */
    if ((fid = H5Freopen(fid2)) < 0)
        FAIL_STACK_ERROR;

    /* Get file's fapl */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from file fapl -- should be H5F_METADATA_READ_ATTEMPTS */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file's fapl */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close all the files */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(file_fapl);
        H5Fclose(fid);
        H5Fclose(fid1);
        H5Fclose(fid2);
    }
    H5E_END_TRY

    return -1;

} /* test_metadata_read_attempts() */

/*
 *  test_metadata_read_retry_info():
 *
 *  Checks whether the public routine H5Fget_metadata_read_retry_info
 *  works as specified.
 *
 */
static int
test_metadata_read_retry_info(hid_t in_fapl)
{
    hid_t            fapl     = H5I_INVALID_HID;                 /* File access property list */
    hid_t            new_fapl = H5I_INVALID_HID;                 /* File access property list */
    hid_t            fid      = H5I_INVALID_HID;                 /* File ID */
    hid_t            fid1     = H5I_INVALID_HID;                 /* File ID */
    H5F_retry_info_t info, info1;                                /* The collection of metadata retries */
    H5F_t           *f = NULL, *f1 = NULL;                       /* Internal file object pointers */
    unsigned         i, j, n;                                    /* Local index variables */
    hid_t            did1    = H5I_INVALID_HID;                  /* Dataset ID */
    hid_t            did2    = H5I_INVALID_HID;                  /* Dataset ID */
    hid_t            sid     = H5I_INVALID_HID;                  /* Dataspace ID */
    hid_t            dcpl    = H5I_INVALID_HID;                  /* Dataset creation property list */
    hsize_t          dims[2] = {6, 10};                          /* Dataset dimensions */
    char             filename[NAME_BUF_SIZE];                    /* File name */
    int              buf[6][10], chkbuf1[6][10], chkbuf2[6][10]; /* Buffers for data */
    hsize_t          max_dims_1un[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* Dataset maximum dimensions */
    hsize_t          max_dims_2un[2] = {500, H5S_UNLIMITED};           /* Dataset maximum dimensions */
    hsize_t          chunk_dims[2]   = {2, 2};                         /* Chunk dimensions */

    /* Output message about test being performed */
    TESTING("H5Fset_metadata_read_retry_info()");

    /* Get a copy of the parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create a file without SWMR access */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with 1 unlimited dimension: extensible array indexing will be used */
    if ((sid = H5Screate_simple(2, dims, max_dims_1un)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did1 = H5Dcreate2(fid, "DSET_1UNLIM", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with 2 unlimited dimension: v2 Btree indexing will be used */
    if ((sid = H5Screate_simple(2, dims, max_dims_2un)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dcreate2(fid, "DSET_2UNLIM", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize buffer data */
    for (i = n = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            buf[i][j] = (int)n++;

    /* Write to the 2 datasets */
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        FAIL_STACK_ERROR;
    if (H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case 1: tests on nbins
     */
    /*
     * Open a file without SWMR access, default # of read attempts--
     *   info.nbins should be 0
     *   info.retries should all be NULL
     */
    /* Open the file without SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did1 = H5Dopen2(fid, "DSET_1UNLIM", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chkbuf1) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did2 = H5Dopen2(fid, "DSET_2UNLIM", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chkbuf2) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retries information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 0 */
    if (info.nbins != 0)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, default # of read attempts--
     *   info.nbins should be 2
     *   info.retries should all be NULL
     */
    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retries information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 2 */
    if (info.nbins != 2)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, # of read_attempts is 10:
     *   info.nbins should be 1
     *   info.retries should all be NULL
     */
    if ((new_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_metadata_read_attempts(new_fapl, 10) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, new_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retry information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 1 */
    if (info.nbins != 1)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, # of read attempts is 101:
     *   info.nbins should be 3
     *   info.retries should all be NULL
     */
    if ((new_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_metadata_read_attempts(new_fapl, 101) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, new_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retry information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 3 */
    if (info.nbins != 3)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, # of read_attempts is 10000:
     *   info.nbins should be 4
     *   info.retries should all be NULL
     */
    if ((new_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_metadata_read_attempts(new_fapl, 10000) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, new_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retry information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 4 */
    if (info.nbins != 4)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, # of read_attempts is 1:
     *   info.nbins should be 0
     *   info.retries should all be NULL
     */
    if ((new_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;

    if (H5Pset_metadata_read_attempts(new_fapl, 1) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, new_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retry information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 0 */
    if (info.nbins != 0)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Closing */
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 2: tests on retries info
     */

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did1 = H5Dopen2(fid, "DSET_1UNLIM", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Read data from the dataset */
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chkbuf1) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did2 = H5Dopen2(fid, "DSET_2UNLIM", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Read data from the dataset */
    if (H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, chkbuf2) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retry information */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 2 */
    if (info.nbins != 2)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            TEST_ERROR;

    /* Get a pointer to the internal file object */
    if ((f = (H5F_t *)H5VL_object(fid)) == NULL)
        FAIL_STACK_ERROR;

    /*
     * Increment 1st set of retries for metadata items:
     *   a) v2 B-tree leaf node--retries[4][1]
     *   b) Extensive array data block--retries[15][1]
     *   c) File's superblock--retries[20][0]
     */

    /* v2 B-tree leaf node: log retry 99 for 500 times */
    for (i = 0; i < 500; i++) {
        if (H5F_track_metadata_read_retries(f, H5AC_BT2_LEAF_ID, 99) < 0)
            FAIL_STACK_ERROR;
    }

    /* Extensive array data block: log retry 10 for 1000 times */
    for (i = 0; i < 1000; i++)
        if (H5F_track_metadata_read_retries(f, H5AC_EARRAY_DBLOCK_ID, 10) < 0)
            FAIL_STACK_ERROR;

    /* File's superblock: log retry 1 for 1 time */
    if (H5F_track_metadata_read_retries(f, H5AC_SUPERBLOCK_ID, 1) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the collection of metadata read retries */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Verify retries for v2 B-tree leaf node */
    if (info.retries[4][0] != 0)
        TEST_ERROR;
    if (info.retries[4][1] != 500)
        TEST_ERROR;

    /* Verify retries for extensive array data block */
    if (info.retries[15][0] != 0)
        TEST_ERROR;
    if (info.retries[15][1] != 1000)
        TEST_ERROR;

    /* Verify retries for file's superblock */
    if (info.retries[20][0] != 1)
        TEST_ERROR;
    if (info.retries[20][1] != 0)
        TEST_ERROR;

    /* Free memory for info.retries */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        if (info.retries[i] != NULL)
            H5free_memory(info.retries[i]);
    }

    /*
     * Increment 2nd set of retries for metadata items:
     *   a) Object header--retries[0][0]
     *   b) Extensive array datablock--retries[15][0]
     *   c) Fixed array header--retries[17][1]
     *   d) File's superblock--retries[20][0]
     */

    /* Object header: log retry 5 for 5 times */
    for (i = 0; i < 5; i++) {
        if (H5F_track_metadata_read_retries(f, H5AC_OHDR_ID, 5) < 0)
            TEST_ERROR;
    }

    /* Extensive array data block: log retry 4 for 1 time */
    if (H5F_track_metadata_read_retries(f, H5AC_EARRAY_DBLOCK_ID, 4) < 0)
        TEST_ERROR;

    /* Fixed array header : log retry 50 for 10000 times */
    for (i = 0; i < 10000; i++) {
        if (H5F_track_metadata_read_retries(f, H5AC_FARRAY_HDR_ID, 50) < 0)
            TEST_ERROR;
    }

    /* File's superblock: log retry 1 for 1 more time */
    if (H5F_track_metadata_read_retries(f, H5AC_SUPERBLOCK_ID, 1) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the collection of metadata read retries */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /*
     * Verify info has both previous + current retries information:
     */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        switch (i) {
            case 0: /* Object header */
                if (info.retries[i][0] != 5)
                    TEST_ERROR;
                if (info.retries[i][1] != 0)
                    TEST_ERROR;
                break;

            case 4: /* v2 B-tree leaf node */
                if (info.retries[i][0] != 0)
                    TEST_ERROR;
                if (info.retries[i][1] != 500)
                    TEST_ERROR;
                break;

            case 15: /* Extensive array data block */
                if (info.retries[i][0] != 1)
                    TEST_ERROR;
                if (info.retries[i][1] != 1000)
                    TEST_ERROR;
                break;

            case 17: /* Fixed array header */
                if (info.retries[i][0] != 0)
                    TEST_ERROR;
                if (info.retries[i][1] != 10000)
                    TEST_ERROR;
                break;

            case 20: /* File's superblock */
                if (info.retries[i][0] != 2)
                    TEST_ERROR;
                if (info.retries[i][1] != 0)
                    TEST_ERROR;
                break;

            default:
                if (info.retries[i] != NULL)
                    TEST_ERROR;
                break;
        }
    }

    /* Free memory for info.retries */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            H5free_memory(info.retries[i]);

    /* Closing */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the file access property list */
    if ((new_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the number of metadata read attempts to 101 */
    if (H5Pset_metadata_read_attempts(new_fapl, 101) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, new_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if ((f = (H5F_t *)H5VL_object(fid)) == NULL)
        FAIL_STACK_ERROR;

    /* File's superblock: log retry 1 for 1 time */
    if (H5F_track_metadata_read_retries(f, H5AC_SUPERBLOCK_ID, 1) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the collection of metadata read retries */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Should be 3 */
    if (info.nbins != 3)
        TEST_ERROR;

    /* Verify retries info */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        switch (i) {
            case 20: /* File's superblock */
                if (info.retries[i][0] != 1)
                    TEST_ERROR;
                if (info.retries[i][1] != 0)
                    TEST_ERROR;
                if (info.retries[i][2] != 0)
                    TEST_ERROR;
                break;

            default:
                if (info.retries[i] != NULL)
                    TEST_ERROR;
                break;
        }
    }

    /* Free memory */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++)
        if (info.retries[i] != NULL)
            H5free_memory(info.retries[i]);

    /* Closing */
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 3: Tests on retrieving the collection of retries
     *         when H5Fopen and H5Freopen the same file.
     */

    /*
     * Open a file without SWMR access, default # of read attempts--
     * H5Freopen the same file--
     * Both files should:
     *   nbins should be 0
     *   retries should all be NULL
     */
    /* Open the file without SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Re-open fid */
    if ((fid1 = H5Freopen(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retries information for fid */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve retries information for fid1*/
    if (H5Fget_metadata_read_retry_info(fid1, &info1) < 0)
        FAIL_STACK_ERROR;

    /* Should be 0 */
    if (info.nbins != 0)
        TEST_ERROR;
    if (info1.nbins != 0)
        TEST_ERROR;

    /* Should be all NULL */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        if (info.retries[i] != NULL)
            TEST_ERROR;
        if (info1.retries[i] != NULL)
            TEST_ERROR;
    }

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /*
     * Open a file with SWMR access, default # of read attempts:
     *   --increment retries for metadata item: fixed array data block page (retries[19][0])
     * H5Freopen the same file:
     *   --increment retries for metadata item: free-space sections (retries[9][1])--
     */
    /* Open the file with SWMR access */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object for fid */
    if ((f = (H5F_t *)H5VL_object(fid)) == NULL)
        FAIL_STACK_ERROR;

    /* Re-open fid */
    if ((fid1 = H5Freopen(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object for fid1 */
    if ((f1 = (H5F_t *)H5VL_object(fid1)) == NULL)
        FAIL_STACK_ERROR;

    /* For fid: fixed array data block page--log retry 9 for 500 times */
    for (i = 0; i < 500; i++) {
        if (H5F_track_metadata_read_retries(f, H5AC_FARRAY_DBLK_PAGE_ID, 9) < 0)
            FAIL_STACK_ERROR;
    }

    /* For fid1: free-space sections--log retry 99 for 1000 times */
    for (i = 0; i < 1000; i++) {
        if (H5F_track_metadata_read_retries(f1, H5AC_FSPACE_SINFO_ID, 99) < 0)
            FAIL_STACK_ERROR;
    }

    /* Retrieve the collection of metadata read retries for fid */
    if (H5Fget_metadata_read_retry_info(fid, &info) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the collection of metadata read retries for fid1 */
    if (H5Fget_metadata_read_retry_info(fid1, &info1) < 0)
        FAIL_STACK_ERROR;

    /* Verify nbins for fid & fid1: should be 2 */
    if (info.nbins != 2)
        TEST_ERROR;
    if (info1.nbins != 2)
        TEST_ERROR;

    /* Verify retries for fid: fixed array data block page */
    if (info.retries[19][0] != 500)
        TEST_ERROR;
    if (info.retries[19][1] != 0)
        TEST_ERROR;

    /* Verify retries for fid: free-space sections */
    /* (Since file was re-opened) */
    if (info.retries[9][0] != 0)
        TEST_ERROR;
    if (info.retries[9][1] != 1000)
        TEST_ERROR;

    /* Verify retries for fid1: free-space sections */
    if (info1.retries[9][0] != 0)
        TEST_ERROR;
    if (info1.retries[9][1] != 1000)
        TEST_ERROR;

    /* Verify retries for fid1: fixed array data block page */
    /* (Since file was re-opened) */
    if (info1.retries[19][0] != 500)
        TEST_ERROR;
    if (info1.retries[19][1] != 0)
        TEST_ERROR;

    /* Free memory for info.retries and info1.retries */
    for (i = 0; i < H5F_NUM_METADATA_READ_RETRY_TYPES; i++) {
        if (info.retries[i] != NULL)
            H5free_memory(info.retries[i]);
        if (info1.retries[i] != NULL)
            H5free_memory(info1.retries[i]);
    } /* end for */

    /* Closing */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(new_fapl);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Fclose(fid);
        H5Fclose(fid1);
    }
    H5E_END_TRY

    return -1;

} /* test_metadata_read_retry_info() */

/*
 * Tests for H5Fstart_swmr_write()
 */

/*
 *  test_start_swmr_write():
 *
 *  Verify SWMR writing is enabled via H5Fstart_swmr_write():
 *  Mainly test for file created with SWMR_WRITE + with/without latest format:
 *  --file will have v3 superblock and all latest version support enabled
 *
 *  (A) Creating a file
 *      Create a file with SWMR_WRITE + non-latest-format
 *      Create a chunked dataset "dataset1" in the file -- should be using latest chunk indexing
 *      Should fail to enable SWMR as the file is already in SWMR writing mode
 *      Close the file
 *
 *  (B) Opening a file
 *      Open the file with write + non-latest-format
 *      --file has v3 superblock and all latest version support enabled
 *      Open dataset "dataset1" 3 times--keep it open
 *    Write to "dataset1"
 *    Create a group in the file
 *      Create a chunked dataset "dataset2" in the group--should be using latest chunk indexing--keep it open
 *      Should succeed in enabling SWMR
 *    Should succeed in reading from multiple opens of "dataset1"
 *      Close multiple opens of "dataset1"
 *    Close "dataset2"
 *      Create "dataset3"--should be using latest chunk indexing
 *      Close "dataset3"
 *      Close the group and file
 */
static int
test_start_swmr_write(hid_t in_fapl, bool new_format)
{
    hid_t   fid       = H5I_INVALID_HID; /* File ID */
    hid_t   fapl      = H5I_INVALID_HID; /* File access property */
    hid_t   gid       = H5I_INVALID_HID; /* Group ID */
    hid_t   dcpl      = H5I_INVALID_HID; /* Dataset creation property */
    hid_t   file_fapl = H5I_INVALID_HID; /* File access property for the file */
    hid_t   did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID, did3 = H5I_INVALID_HID; /* Dataset IDs */
    hid_t   did1_a = H5I_INVALID_HID, did1_b = H5I_INVALID_HID;
    hid_t   sid1 = H5I_INVALID_HID, sid2 = H5I_INVALID_HID, sid3 = H5I_INVALID_HID; /* Dataspace IDs */
    hsize_t dim[1]        = {1};                                                    /* Dimension sizes */
    hsize_t max_dim[1]    = {H5S_UNLIMITED};                /* Maximum dimension sizes */
    hsize_t chunk_dim[1]  = {2};                            /* Chunk dimension sizes */
    hsize_t dim2[2]       = {5, 10};                        /* Dimension sizes */
    hsize_t max_dim2[2]   = {H5S_UNLIMITED, H5S_UNLIMITED}; /* Maximum dimension sizes */
    hsize_t chunk_dim2[2] = {2, 7};                         /* Chunk dimension sizes */
    H5D_chunk_index_t idx_type;                             /* Dataset chunk index type */
    int               wdata = 99;                           /* Data to write */
    int               rdata;                                /* Data read */
    unsigned          attempts;                             /* The retrieved # of read attempts */
    char              filename[NAME_BUF_SIZE];              /* File name */
    herr_t            ret;                                  /* Return value */

    /* Get a copy of the parameter fapl (non-latest-format) */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    if (new_format) {
        TESTING("H5Fstart_swmr_write() when creating/opening a file with latest format");

        /* Set to use the latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        TESTING("H5Fstart_swmr_write() when creating/opening a file without latest format");
    } /* end if */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /*
     * Case A: when creating a file
     */

    /* Create the file with SWMR write + non-latest-format */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT,
                         fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's access_property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts from the file's fapl */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;

    /* Should be 100 */
    if (attempts != (unsigned int)(new_format ? H5F_METADATA_READ_ATTEMPTS : H5F_SWMR_METADATA_READ_ATTEMPTS))
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Create "dataset1" */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dim) < 0)
        FAIL_STACK_ERROR;
    if ((sid1 = H5Screate_simple(1, dim, max_dim)) < 0)
        FAIL_STACK_ERROR;
    if ((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid1, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get the chunk index type */
    if (H5D__layout_idx_type_test(did1, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if (idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using extensible array as index");

    /* Write to the dataset */
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR for non-latest-format */
    /* Should succeed in enabling SWMR for latest format */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (new_format) {
        if (ret < 0)
            TEST_ERROR;
    }
    else if (ret >= 0)
        TEST_ERROR;

    /* Read from the dataset */
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
        FAIL_STACK_ERROR;

    /* Verify the data is correct */
    if (wdata != rdata)
        TEST_ERROR;

    /* Close "dataset1", dataspace, dataset creation property list */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's access_property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;

    /* Should be 100 */
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the file access property list */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case B: when opening a file
     */

    /* Open the file again with write + non-latest-format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's access_property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;

    /* Should be 1 */
    if (attempts != H5F_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    /* open "dataset1", keep it open */
    if ((did1 = H5Dopen2(fid, "dataset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* open "dataset1" second time */
    if ((did1_a = H5Dopen2(fid, "dataset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* open "dataset1" third time */
    if ((did1_b = H5Dopen2(fid, "dataset1", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to "dataset1" */
    wdata = 88;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
        FAIL_STACK_ERROR;

    /* Create a group */
    if ((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Create "dataset2" in the group, keep it open */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dim2) < 0)
        FAIL_STACK_ERROR;
    if ((sid2 = H5Screate_simple(2, dim2, max_dim2)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dcreate2(gid, "dataset2", H5T_NATIVE_INT, sid2, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get the chunk index type for "dataset2" */
    if (H5D__layout_idx_type_test(did2, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using v2 B-tree chunk indexing");

    /* Should succeed in enabling SWMR writing */
    if (H5Fstart_swmr_write(fid) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's access_property list */
    if ((file_fapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the # of read attempts */
    if (H5Pget_metadata_read_attempts(file_fapl, &attempts) < 0)
        FAIL_STACK_ERROR;

    /* Should be 100 */
    if (attempts != H5F_SWMR_METADATA_READ_ATTEMPTS)
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(file_fapl) < 0)
        FAIL_STACK_ERROR;

    rdata = 0;
    /* Read from "dataset1" via did1_b (multiple opens) */
    if (H5Dread(did1_b, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
        FAIL_STACK_ERROR;
    if (wdata != rdata)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1_b) < 0)
        FAIL_STACK_ERROR;

    /* Read from "dataset1" */
    rdata = 0;
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
        FAIL_STACK_ERROR;
    if (wdata != rdata)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;

    rdata = 0;
    /* Read from "dataset1" via did1_a (multiple opens) */
    if (H5Dread(did1_a, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
        FAIL_STACK_ERROR;
    if (wdata != rdata)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1_a) < 0)
        FAIL_STACK_ERROR;

    /* Close "dataset2", dataspace, dataset creation property list */
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Create "dataset3" */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dim2) < 0)
        FAIL_STACK_ERROR;
    if ((sid3 = H5Screate_simple(2, dim2, max_dim2)) < 0)
        FAIL_STACK_ERROR;
    if ((did3 = H5Dcreate2(fid, "dataset3", H5T_NATIVE_INT, sid3, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Get the chunk index type for "dataset3" */
    if (H5D__layout_idx_type_test(did3, &idx_type) < 0)
        FAIL_STACK_ERROR;
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using v2 B-tree as index");

    /* Close "dataset3", dataspace, dataset creation property list */
    if (H5Dclose(did3) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid3) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Close the group */
    if (H5Gclose(gid) < 0)
        FAIL_STACK_ERROR;

    /* Close the file access property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl);
        H5Pclose(file_fapl);
        H5Gclose(gid);
        H5Pclose(dcpl);
        H5Dclose(did1);
        H5Dclose(did1_a);
        H5Dclose(did1_b);
        H5Dclose(did2);
        H5Dclose(did3);
        H5Sclose(sid1);
        H5Sclose(sid2);
        H5Sclose(sid3);
    }
    H5E_END_TRY

    return -1;
} /* test_start_swmr_write() */

/*
 *  test_err_start_swmr_write():
 *
 *  Verify failure conditions in enabling SWMR writing mode via H5Fstart_swmr_write():
 *  (A) When creating a file:
 *      (1) Create a file with SWMR write + with/without latest format
 *      --fail to enable SWMR because the file is already in SWMR writing mode
 *      If (latest-format):
 *      (2a) Create a file with write + latest format and with opened named datatype
 *      --fail to enable SWMR because there are opened datatype
 *      If !(latest-format):
 *      (2b) Create a file with write + non-latest-format
 *              --fail to enable SWMR because superblock version is not at least 3
 *
 *  (B) When opening a file which is created with write + latest format:
 *      (1) Open the file with SWMR write + with/without latest format
 *      --fail to enable SWMR because the file is already in SWMR writing mode
 *      (2) Open the file with read only + with/without latest format
 *      --fail to enable SWMR because the file is not opened with write
 *      (3) Open the file with SWMR read only + with/without latest format
 *      --fail to enable SWMR because the file is not opened with write
 *      (4) Open the file with write + with/without latest format and with opened named datatype/attribute
 *      --fail to enable SWMR because there are opened datatype/attribute
 *
 *  (C) When doing multiple opens for a file:
 *      Create a file with (a) write + latest format or (b) SMWR write + non-latest-format
 *      Close the file
 *      (1) --Open the file with write + with/without latest format
 *      --Enable SWMR writing mode twice
 *      --First time succeed, second time fail
 *      --Close the file
        (2) --Open the file with write + with/without latest format
 *      --Succeed to enable SWMR writing mode
 *      --Reopen the same file
 *          --fail to enable SWMR writing mode for the reopened file
 *      --Close the file
        (3) --Open the file with write + with/without latest format
 *      --Open the same file again
 *          --succeed to enable SWMR for the first opened file
 *          --fail to enable SWMR for the second opened file
 *      --Close the file
 *
 *  (D) (!new_format): When opening a file which is created with write + non-latest-format:
 *          (1) Open the file with SWMR write+latest format
 *              --fail to open due to superblock version not 3
 *          (2) Open the file with SWMR write+non-latest-format
 *              --fail to open due to superblock version not 3

 *          (3) Open the file with write+latest format
 *              --fail to enable SWMR due to superblock version not 3
 *          (4) Open the file with write+non-latest-format
 *              --fail to enable SWMR due to superblock version not 3
 */
static int
test_err_start_swmr_write(hid_t in_fapl, bool new_format)
{
    hid_t  fid      = H5I_INVALID_HID; /* File ID */
    hid_t  fid2     = H5I_INVALID_HID; /* File ID */
    hid_t  fapl     = H5I_INVALID_HID; /* A copy of file access property */
    hid_t  new_fapl = H5I_INVALID_HID; /* A copy of file access property */
    hid_t  gid      = H5I_INVALID_HID; /* Group ID */
    hid_t  did      = H5I_INVALID_HID; /* Dataset ID */
    hid_t  sid      = H5I_INVALID_HID; /* Dataspace ID */
    hid_t  aid      = H5I_INVALID_HID; /* Attribute ID */
    hid_t  tid      = H5I_INVALID_HID; /* Datatype ID */
    hid_t  bad_fid  = H5I_INVALID_HID; /* Test fid (should never represent a real ID) */
    herr_t ret;                        /* Return value */
    char   filename[NAME_BUF_SIZE];    /* File name */

    /* Create a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    if ((new_fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;
    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(new_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    if (new_format) {
        TESTING("H5Fstart_swmr_write() on failure conditions for latest format");

        if ((fapl = H5Pcopy(new_fapl)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        TESTING("H5Fstart_swmr_write() on failure conditions for without latest format");
    }

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /*
     * (A) When creating a file:
     */

    /* Case 1 */

    /* Create the file with SWMR_WRITE + with/without latest format */
    fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl);

    /* Should fail to enable SWMR writing when the file is already in SWMR writing mode */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 2 */

    if (new_format) {

        /* Create the file with write + latest format */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;

        /* Create and commit a named datatype */
        if ((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
            FAIL_STACK_ERROR;
        if (H5Tcommit2(fid, "TID", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
            FAIL_STACK_ERROR;

        /* Should fail to enable SWMR writing when there is an opened named datatype */
        H5E_BEGIN_TRY
        {
            ret = H5Fstart_swmr_write(fid);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;

        /* Close the datatype */
        if (H5Tclose(tid) < 0)
            FAIL_STACK_ERROR;

        /* Should succeed in enabling SWMR writing */
        if (H5Fstart_swmr_write(fid) < 0)
            TEST_ERROR;

        /* Close the file */
        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;
    }
    else {

        /* Create a file with write + non-latest-format */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR;

        /* Should fail to enable SWMR writing because the file's superblock version is not at least 3 */
        H5E_BEGIN_TRY
        {
            ret = H5Fstart_swmr_write(fid);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;

        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;
    } /* end if */

    /*
     * (B) When opening a file which is created with the latest format
     */

    /* Create a file with write + latest format */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, new_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 1 */

    /* Open the file with SWMR write + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is already in SWMR writing mode */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 2 */

    /* Open the file with read only access + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is opened with read only access */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 3 */

    /* Open the file file with SWMR read access + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when the file is opened with SWMR read access only */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 4 */

    /* Open the file with write + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create and commit a named datatype */
    if ((tid = H5Tcopy(H5T_NATIVE_INT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tcommit2(fid, "TID", tid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        FAIL_STACK_ERROR;

    /* Create dataspace */
    if ((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR;

    /* Attach an attribute to the named datatype */
    if ((aid = H5Acreate2(tid, "attr", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing when there are opened named datatype and attribute */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the datatype */
    if (H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;

    /* Still fail to enable SWMR writing when the attribute is still opened */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the attribute */
    if (H5Aclose(aid) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Close the dataspace */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * (C) Failure cases for multiple opens
     */

    /* Case 1 */

    /* Create a file with write + with/without latest format */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | (new_format ? 0 : H5F_ACC_SWMR_WRITE), H5P_DEFAULT,
                         fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Open the file with write + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing mode */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Should fail for a second call to enable SWMR writing mode */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 2 */

    /* Open the file with write + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing mode */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Re-open the same file */
    if ((fid2 = H5Freopen(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to enable SWMR writing mode for fid2 */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid2);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the files */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Case 3 */

    /* Open the file with write + with/without latest format */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the same file */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed in enabling SWMR writing for fid */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Should fail to enable SWMR writing for fid2 */
    H5E_BEGIN_TRY
    {
        ret = H5Fstart_swmr_write(fid2);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Close the files */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    if (!new_format) {

        /*
         * (D) When opening a file which is created without the latest format:
         */

        /* Create a file with write + without latest format */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;

        /* Case 1 */

        /* Should fail to open the file with SWMR write + latest format due to superblock version not at least
         * 3 */
        H5E_BEGIN_TRY
        {
            bad_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, new_fapl);
        }
        H5E_END_TRY
        if (bad_fid >= 0)
            TEST_ERROR;

        /* Case 2 */

        /* Should fail to open the file with SWMR write + non-latest-format due to superblock version not at
         * least 3 */
        H5E_BEGIN_TRY
        {
            bad_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY
        if (bad_fid >= 0)
            TEST_ERROR;

        /* Case 3 */

        /* Open the file with write + latest format */
        if ((fid = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR;

        /* Should fail to enable SWMR writing due to superblock version not at least 3 */
        H5E_BEGIN_TRY
        {
            ret = H5Fstart_swmr_write(fid);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;

        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;

        /* Case 4 */

        /* Open the file with write + non-latest-format */
        if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR;

        /* Should fail to enable SWMR writing because the file's superblock version is not at least 3 */
        H5E_BEGIN_TRY
        {
            ret = H5Fstart_swmr_write(fid);
        }
        H5E_END_TRY
        if (ret >= 0)
            TEST_ERROR;

        if (H5Fclose(fid) < 0)
            FAIL_STACK_ERROR;

    } /* not new */

    /* Close the file access property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Gclose(gid);
        H5Dclose(did);
        H5Fclose(fid);
        H5Fclose(fid2);
        H5Pclose(fapl);
        H5Pclose(new_fapl);
        /* bad_fid should only represent a read ID in the error case.
         * It never needs to be closed in the normal case.
         */
        H5Fclose(bad_fid);
    }
    H5E_END_TRY

    return -1;
} /* test_err_start_swmr_write() */

/*
 * test_start_swmr_write_concur():
 *
 * The "new_format" parameter indicates whether to create the file with latest format or not.
 *  To have SWMR support, can use either one of the following in creating a file:
 *  (a) Create the file with write + latest format:
 *      --result in v3 superblock with latest chunk indexing types
 *  (b) Create the file with SWMR write + non-latest-format:
 *      --result in v3 superblock with latest chunk indexing types
 *  Create a chunked dataset with 1 extendible dimension in the file
 *
 * Verify concurrent access for H5Fstart_swmr_write()--
 *  (1) Parent: open a file with write access
 *      Child: concurrent open of the file with read & SWMR read (fail)
 *  (2) Parent: open a file with write access; enable SWMR writing mode
 *      Child: open the file 2 times with read & SWMR read (succeed)
 *           open the dataset 2 times with the 2 file opens
 *           verify data read from multiple opens of the dataset is correct
 *  (3) Parent: open a file with write access; enable SWMR writing mode
 *      Child: Concurrent open of the file with read only (fail)
 *  (4) Parent: open a file with write access; enable SWMR writing mode
 *      Child: concurrent open of the file with write access (fail)
 *  (5) Parent: open a file with write access; enable SWMR writing mode
 *      Child: concurrent open of the file with write and SWMR write access (fail)
 */
#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID))

static int
test_start_swmr_write_concur(hid_t H5_ATTR_UNUSED in_fapl, bool new_format)
{
    if (new_format) {
        TESTING("H5Fstart_swmr_write()--concurrent access for latest format");
    }
    else {
        TESTING("H5Fstart_swmr_write()--concurrent access for non-latest-format");
    }

    SKIPPED();
    puts("    Test skipped due to fork or waitpid not defined.");
    return 0;
} /* test_start_swmr_write_concur() */

#else  /* !defined(H5_HAVE_FORK && defined(H5_HAVE_WAITPID) */

static int
test_start_swmr_write_concur(hid_t in_fapl, bool new_format)
{
    hid_t fid = H5I_INVALID_HID, fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID; /* File IDs */
    hid_t fapl;                    /* File access property list */
    pid_t childpid = 0;            /* Child process ID */
    pid_t tmppid;                  /* Child process ID returned by waitpid */
    int   child_status;            /* Status passed to waitpid */
    int   child_wait_option = 0;   /* Options passed to waitpid */
    int   child_exit_val;          /* Exit status of the child */
    char  filename[NAME_BUF_SIZE]; /* File name */

    hid_t   did = H5I_INVALID_HID, did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID, did3 = H5I_INVALID_HID;
    hid_t   sid           = H5I_INVALID_HID;
    hid_t   dcpl          = H5I_INVALID_HID;
    hsize_t chunk_dims[1] = {1};
    hsize_t maxdims[1]    = {H5S_UNLIMITED};
    hsize_t dims[1]       = {1};
    int     wdata         = 0;

    int out_pdf[2];
    int in_pdf[2];
    int notify = 0;

    /* Output message about test being performed */
    if (new_format) {
        TESTING("H5Fstart_swmr_write()--concurrent access for latest format");
    }
    else {
        TESTING("H5Fstart_swmr_write()--concurrent access for non-latest-format");
    }

    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if (new_format) {
        /* Set to use the latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR;

        /* Create the test file */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        /* Create the test file without latest format but with SWMR write */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    } /* end if */

    /* Create a chunked dataset with 1 extendible dimension */
    if ((sid = H5Screate_simple(1, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (1):
     *  Verify concurrent file open with H5F_ACC_RDONLY|H5F_ACC_SWMR_READ
     *  will fail without H5Fstart_swmr_write()
     */

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        int child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Should fail */
        H5E_BEGIN_TRY
        {
            /* Open the test file */
            fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
        }
        H5E_END_TRY
        if (fid >= 0)
            exit(EXIT_FAILURE);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* child process terminated abnormally */
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (2):
     *  Verify concurrent file open with H5F_ACC_RDONLY|H5F_ACC_SWMR_READ
     *  will succeed with H5Fstart_swmr_write()
     */

    /* Create 2 pipes */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;
    if (pipe(in_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) {                                                  /* Child process */
        hid_t child_fid1 = H5I_INVALID_HID, child_fid2;                   /* File IDs */
        hid_t child_did1 = H5I_INVALID_HID, child_did2 = H5I_INVALID_HID; /* Dataset IDs */
        int   child_notify = 0;
        int   rdata        = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);
        /* close unused read end for in_pdf */
        if (HDclose(in_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Should succeed in opening the test file 2 times */
        if ((child_fid1 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
            exit(EXIT_FAILURE);
        if ((child_fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
            exit(EXIT_FAILURE);

        /* open "dataset" 2 times */
        if ((child_did1 = H5Dopen2(child_fid1, "dataset", H5P_DEFAULT)) < 0)
            exit(EXIT_FAILURE);
        if ((child_did2 = H5Dopen2(child_fid2, "dataset", H5P_DEFAULT)) < 0)
            exit(EXIT_FAILURE);

        /* Read from "dataset" via child_did1 */
        rdata = 0;
        if (H5Dread(child_did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
            exit(EXIT_FAILURE);
        if (rdata != 88)
            exit(EXIT_FAILURE);

        /* Notify parent process */
        child_notify = 2;
        if (HDwrite(in_pdf[1], &child_notify, sizeof(int)) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 3) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Refresh "dataset" via child_did2 */
        if (H5Drefresh(child_did2) < 0)
            exit(EXIT_FAILURE);

        /* Read from "dataset" child_did2 */
        rdata = 0;
        if (H5Dread(child_did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
            exit(EXIT_FAILURE);
        if (rdata != 99)
            exit(EXIT_FAILURE);

        /* Read from "dataset" child_did1 */
        rdata = 0;
        if (H5Dread(child_did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rdata) < 0)
            exit(EXIT_FAILURE);
        if (rdata != 99)
            exit(EXIT_FAILURE);

        /* Close the dataset */
        if (H5Dclose(child_did1))
            exit(EXIT_FAILURE);
        if (H5Dclose(child_did2))
            exit(EXIT_FAILURE);

        /* Close the file */
        if (H5Fclose(child_fid1) < 0)
            exit(EXIT_FAILURE);
        if (H5Fclose(child_fid2) < 0)
            exit(EXIT_FAILURE);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);
        if (HDclose(in_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;
    /* Close unused write end for in_pdf */
    if (HDclose(in_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* open "dataset", keep it open */
    if ((did1 = H5Dopen2(fid1, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid2, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did3 = H5Dopen2(fid1, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to "dataset" */
    wdata = 88;
    if (H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
        FAIL_STACK_ERROR;

    /* Flush to disk */
    if (H5Fflush(fid1, H5F_SCOPE_LOCAL) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR writing mode */
    if (H5Fstart_swmr_write(fid1) < 0)
        TEST_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Wait for notification from child process */
    while (notify != 2) {
        if (HDread(in_pdf[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Write to "dataset" */
    wdata = 99;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wdata) < 0)
        FAIL_STACK_ERROR;

    /* Flush to disk */
    if (H5Fflush(fid1, H5F_SCOPE_LOCAL) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 3;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did3) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipes */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(in_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* Child process terminated abnormally */
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (3):
     *  Verify concurrent file open with H5F_ACC_RDONLY
     *  will fail with H5Fstart_swmr_write()
     */
    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        int child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Should fail in opening the test file */
        H5E_BEGIN_TRY
        {
            fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        }
        H5E_END_TRY
        if (fid >= 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    } /* end if */

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR writing mode */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* Child process terminated abnormally */
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (4):
     *  Verify concurrent file open with H5F_ACC_RDWR
     *  will fail with H5Fstart_swmr_write()
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        int child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Should fail in opening the test file */
        H5E_BEGIN_TRY
        {
            fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
        }
        H5E_END_TRY
        if (fid >= 0)
            exit(EXIT_FAILURE);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    } /* end if */

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR writing mode */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* Child process terminated abnormally */
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (5):
     *  Verify concurrent file open with H5F_ACC_RDWR|H5F_ACC_SWMR_WRITE
     *  will fail with H5Fstart_swmr_write()
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        int child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Should fail in opening the test file */
        H5E_BEGIN_TRY
        {
            fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY
        if (fid >= 0)
            exit(EXIT_FAILURE);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR writing mode */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* Child process terminated abnormally */
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;

} /* test_start_swmr_write_concur() */
#endif /* !defined(H5_HAVE_FORK && defined(H5_HAVE_WAITPID) */

/*
 * test_start_swmr_write_stress_ohdr():
 *
 * Verify that H5Fswmr_start_write() works correctly when the dataspace header
 * message is not located in chunk #0 of the object header.
 *
 */
static int
test_start_swmr_write_stress_ohdr(hid_t in_fapl)
{
    hid_t    fid = H5I_INVALID_HID;                         /* File IDs */
    hid_t    fapl;                                          /* File access property list */
    char     filename[NAME_BUF_SIZE];                       /* File name */
    hid_t    did = H5I_INVALID_HID, did2 = H5I_INVALID_HID; /* Dataset IDs */
    hid_t    sid           = H5I_INVALID_HID;               /* Dataspace ID */
    hid_t    tid           = H5I_INVALID_HID;               /* Datatype ID */
    hid_t    dcpl          = H5I_INVALID_HID;               /* Dataset creation property list ID */
    hid_t    aid           = H5I_INVALID_HID;               /* Attribute ID */
    hsize_t  chunk_dims[2] = {10, 10};
    hsize_t  maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    char     fill[256];     /* Fill value for dataset */
    char     attr_data[32]; /* Data value for attribute */
    hsize_t  dims[2] = {1, 1};
    unsigned chunk_num; /* Object header chunk # for dataspace message */

    /* Output message about test being performed */
    TESTING("H5Fstart_swmr_write()--stress object header messages");

    /* Initialize buffers */
    memset(fill, 0, sizeof(fill));
    memset(attr_data, 0, sizeof(attr_data));

    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with 2 extendible dimensions */
    if ((sid = H5Screate_simple(1, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((tid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tset_size(tid, 256) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_fill_value(dcpl, tid, &fill) < 0)
        FAIL_STACK_ERROR;
    if ((did = H5Dcreate2(fid, "dataset", tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the chunk # for the dataspace message */
    chunk_num = UINT_MAX;
    if (H5O__msg_get_chunkno_test(did, H5O_SDSPACE_ID, &chunk_num) < 0)
        FAIL_STACK_ERROR;
    /* Should be in chunk #0 for now */
    if (0 != chunk_num)
        TEST_ERROR;

    /* Create a second chunked dataset with 2 extendible dimensions */
    /* (So that the original dataset's object header can't be extended) */
    if ((sid = H5Screate_simple(1, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((tid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tset_size(tid, 256) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_fill_value(dcpl, tid, &fill) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dcreate2(fid, "dataset2", tid, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close the second dataset */
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;

    /* Close the objects for the dataset creation */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Create attribute on original dataset, to push dataspace header message out of header chunk #0 */
    if ((sid = H5Screate(H5S_SCALAR)) < 0)
        FAIL_STACK_ERROR;
    if ((tid = H5Tcopy(H5T_C_S1)) < 0)
        FAIL_STACK_ERROR;
    if (H5Tset_size(tid, 32) < 0)
        FAIL_STACK_ERROR;
    if (H5Tset_strpad(tid, H5T_STR_NULLTERM) < 0)
        FAIL_STACK_ERROR;
    if ((aid = H5Acreate2(did, "attr", tid, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Awrite(aid, tid, attr_data) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Tclose(tid) < 0)
        FAIL_STACK_ERROR;
    if (H5Aclose(aid) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the chunk # for the dataspace message */
    chunk_num = UINT_MAX;
    if (H5O__msg_get_chunkno_test(did, H5O_SDSPACE_ID, &chunk_num) < 0)
        FAIL_STACK_ERROR;
    /* Should be in chunk #0 for now */
    if (1 != chunk_num)
        TEST_ERROR;

    /* Enable SWMR write */
    if (H5Fstart_swmr_write(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the FAPL */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(aid);
        H5Sclose(tid);
        H5Sclose(sid);
        H5Sclose(did);
        H5Sclose(did2);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_start_swmr_write_stress_ohdr() */

/*
 *  test_start_swmr_write_persist_dapl():
 *
 *  Verify H5Fstart_swmr_write() doesn't wipe out dataset access properties for
 *  open datasets.
 *
 */
static herr_t
dummy_append_flush_cb(hid_t H5_ATTR_UNUSED dataset_id, hsize_t H5_ATTR_UNUSED *cur_dims,
                      void H5_ATTR_UNUSED *user_data)
{
    return SUCCEED;
}

static herr_t
tssw_persist_dapl_verify(hid_t did, hid_t vdsid1, hid_t vdsid2, hsize_t boundary, H5D_append_cb_t append_func,
                         void *append_func_ud, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0,
                         const char *efile_prefix, const char *virtual_prefix, hsize_t gap_size,
                         H5D_vds_view_t virtual_view)
{
    hid_t           dapl               = H5I_INVALID_HID;
    hid_t           vds_dapl1          = H5I_INVALID_HID;
    hid_t           vds_dapl2          = H5I_INVALID_HID;
    hsize_t         boundary_out       = 0;
    H5D_append_cb_t append_func_out    = NULL;
    void           *append_func_ud_out = NULL;
    size_t          rdcc_nslots_out    = 0;
    size_t          rdcc_nbytes_out    = 0;
    double          rdcc_w0_out        = 0.;
    char            efile_prefix_out[64];
    char            virtual_prefix_out[64];
    hsize_t         gap_size_out     = 0;
    H5D_vds_view_t  virtual_view_out = H5D_VDS_LAST_AVAILABLE;

    /* Get dataset access property lists */
    if ((dapl = H5Dget_access_plist(did)) < 0)
        TEST_ERROR;
    if ((vds_dapl1 = H5Dget_access_plist(vdsid1)) < 0)
        TEST_ERROR;
    if ((vds_dapl2 = H5Dget_access_plist(vdsid2)) < 0)
        TEST_ERROR;

    /* Get append flush property and verify */
    if (H5Pget_append_flush(dapl, 1, &boundary_out, &append_func_out, &append_func_ud_out) < 0)
        TEST_ERROR;
    if (boundary != boundary_out)
        TEST_ERROR;
    if (append_func != append_func_out)
        TEST_ERROR;
    if (append_func_ud != append_func_ud_out)
        TEST_ERROR;

    /* Get chunk cache property and verify */
    if (H5Pget_chunk_cache(dapl, &rdcc_nslots_out, &rdcc_nbytes_out, &rdcc_w0_out) < 0)
        TEST_ERROR;
    if (rdcc_nslots != rdcc_nslots_out)
        TEST_ERROR;
    if (rdcc_nbytes != rdcc_nbytes_out)
        TEST_ERROR;
    if (fabs(rdcc_w0 - rdcc_w0_out) > (double)FP_EPSILON)
        TEST_ERROR;

    /* Get efile prefix property and verify */
    if (H5Pget_efile_prefix(dapl, efile_prefix_out, sizeof(efile_prefix_out)) < 0)
        TEST_ERROR;
    if (strncmp(efile_prefix, efile_prefix_out, sizeof(efile_prefix_out)))
        TEST_ERROR;

    /* Get virtual prefix property and verify */
    if (H5Pget_virtual_prefix(vds_dapl1, virtual_prefix_out, sizeof(virtual_prefix_out)) < 0)
        TEST_ERROR;
    if (strncmp(virtual_prefix, virtual_prefix_out, sizeof(virtual_prefix_out)))
        TEST_ERROR;

    /* Get virtual printf gap property and verify */
    if (H5Pget_virtual_printf_gap(vds_dapl1, &gap_size_out) < 0)
        TEST_ERROR;
    if (gap_size != gap_size_out)
        TEST_ERROR;

    /* Get virtual view property and verify.  Use vds_dapl2 since a VDS can't
     * have both LAST_AVAILABLE and a printf gap set.  vds1 has a printf gap and
     * vds2 has LAST_AVAILABLE. */
    if (H5Pget_virtual_view(vds_dapl2, &virtual_view_out) < 0)
        TEST_ERROR;
    if (virtual_view != virtual_view_out)
        TEST_ERROR;

    /* Close DAPLs */
    if (H5Pclose(dapl) < 0)
        TEST_ERROR;
    if (H5Pclose(vds_dapl1) < 0)
        TEST_ERROR;
    if (H5Pclose(vds_dapl2) < 0)
        TEST_ERROR;

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dapl);
        H5Pclose(vds_dapl1);
        H5Pclose(vds_dapl2);
    }
    H5E_END_TRY

    return FAIL;
} /* tssw_persist_dapl_verify() */

static int
test_start_swmr_write_persist_dapl(hid_t in_fapl)
{
    hid_t           fid            = H5I_INVALID_HID; /* File ID */
    hid_t           fapl           = H5I_INVALID_HID; /* File access property */
    hid_t           dcpl           = H5I_INVALID_HID; /* Dataset creation property */
    hid_t           vds_dcpl       = H5I_INVALID_HID; /* Virtual dataset creation property */
    hid_t           dapl1          = H5I_INVALID_HID; /* Dataset access property */
    hid_t           dapl2          = H5I_INVALID_HID; /* Dataset access property */
    hid_t           did            = H5I_INVALID_HID; /* Dataset ID */
    hid_t           vdsid1         = H5I_INVALID_HID; /* Virtual Dataset ID */
    hid_t           vdsid2         = H5I_INVALID_HID; /* Virtual Dataset ID */
    hid_t           sid            = H5I_INVALID_HID; /* Dataspace IDs*/
    hsize_t         dim[1]         = {1};             /* Dimension sizes */
    hsize_t         max_dim[1]     = {H5S_UNLIMITED}; /* Maximum dimension sizes */
    hsize_t         chunk_dim[1]   = {2};             /* Chunk dimension sizes */
    hsize_t         boundary       = 23;
    H5D_append_cb_t append_func    = dummy_append_flush_cb;
    void           *append_func_ud = &boundary;
    size_t          rdcc_nslots    = 125;
    size_t          rdcc_nbytes    = 23434;
    double          rdcc_w0        = 0.68419;
    const char     *efile_prefix   = "dummy_efile_prefix";
    const char     *virtual_prefix = "dummy_virtual_prefix";
    hsize_t         gap_size       = 421;
    H5D_vds_view_t  virtual_view   = H5D_VDS_FIRST_MISSING;
    char            filename[NAME_BUF_SIZE]; /* File name */

    /* Get a copy of the parameter fapl (non-latest-format) */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    TESTING("H5Fstart_swmr_write() persists DAPL settings");

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file with SWMR write + non-latest-format */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Create dapl with custom properties */
    if ((dapl1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        TEST_ERROR;

    /* Set append flush property */
    boundary = 23;
    if (H5Pset_append_flush(dapl1, 1, &boundary, dummy_append_flush_cb, append_func_ud) < 0)
        TEST_ERROR;

    /* Set chunk cache property */
    if (H5Pset_chunk_cache(dapl1, rdcc_nslots, rdcc_nbytes, rdcc_w0) < 0)
        TEST_ERROR;

    /* Set efile prefix property */
    if (H5Pset_efile_prefix(dapl1, efile_prefix) < 0)
        TEST_ERROR;

    /* Set virtual prefix property */
    if (H5Pset_virtual_prefix(dapl1, virtual_prefix) < 0)
        TEST_ERROR;

    /* Set virtual printf gap property */
    if (H5Pset_virtual_printf_gap(dapl1, gap_size) < 0)
        TEST_ERROR;

    /* Must create separate dapl2 for a different view, since the non-default
     * view of FIRST_MISSING wipes out the printf gap setting */
    if ((dapl2 = H5Pcopy(dapl1)) < 0)
        TEST_ERROR;

    /* Set virtual view property */
    if (H5Pset_virtual_view(dapl2, virtual_view) < 0)
        TEST_ERROR;

    /*
     * Case A: create file, create dataset
     */

    /* Create "dataset1" */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dim) < 0)
        TEST_ERROR;
    if ((sid = H5Screate_simple(1, dim, max_dim)) < 0)
        TEST_ERROR;
    if ((did = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0)
        TEST_ERROR;

    /* Create "vds1" */
    if ((vds_dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_layout(vds_dcpl, H5D_VIRTUAL) < 0)
        TEST_ERROR;
    if ((vdsid1 = H5Dcreate2(fid, "vds1", H5T_NATIVE_INT, sid, H5P_DEFAULT, vds_dcpl, dapl1)) < 0)
        TEST_ERROR;

    /* Create "vds2" */
    if ((vdsid2 = H5Dcreate2(fid, "vds2", H5T_NATIVE_INT, sid, H5P_DEFAULT, vds_dcpl, dapl2)) < 0)
        TEST_ERROR;

    /* Enable swmr write */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Verify dataset still has correct access properties */
    if (tssw_persist_dapl_verify(did, vdsid1, vdsid2, boundary, append_func, append_func_ud, rdcc_nslots,
                                 rdcc_nbytes, rdcc_w0, efile_prefix, virtual_prefix, gap_size,
                                 virtual_view) < 0)
        TEST_ERROR;

    /* Close "dataset1" */
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Close "vds1" */
    if (H5Dclose(vdsid1) < 0)
        TEST_ERROR;

    /* Close "vds2" */
    if (H5Dclose(vdsid2) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case B: opened file, open dataset
     */

    /* Open the file again with write */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Open "dataset1", keep it open */
    if ((did = H5Dopen2(fid, "dataset1", dapl1)) < 0)
        FAIL_STACK_ERROR;

    /* Open "vds1", keep it open */
    if ((vdsid1 = H5Dopen2(fid, "vds1", dapl1)) < 0)
        FAIL_STACK_ERROR;

    /* Open "vds1", keep it open */
    if ((vdsid2 = H5Dopen2(fid, "vds2", dapl2)) < 0)
        FAIL_STACK_ERROR;

    /* Enable swmr write */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Verify dataset still has correct access properties */
    if (tssw_persist_dapl_verify(did, vdsid1, vdsid2, boundary, append_func, append_func_ud, rdcc_nslots,
                                 rdcc_nbytes, rdcc_w0, efile_prefix, virtual_prefix, gap_size,
                                 virtual_view) < 0)
        TEST_ERROR;

    /* Close "dataset1" */
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Close "vds1" */
    if (H5Dclose(vdsid1) < 0)
        TEST_ERROR;

    /* Close "vds2" */
    if (H5Dclose(vdsid2) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case C: opened file, create dataset
     */

    /* Open the file again with write */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR;

    /* Create "dataset2" */
    if ((did = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0)
        TEST_ERROR;

    /* Create "vds3" */
    if ((vdsid1 = H5Dcreate2(fid, "vds3", H5T_NATIVE_INT, sid, H5P_DEFAULT, vds_dcpl, dapl1)) < 0)
        TEST_ERROR;

    /* Create "vds4" */
    if ((vdsid2 = H5Dcreate2(fid, "vds4", H5T_NATIVE_INT, sid, H5P_DEFAULT, vds_dcpl, dapl2)) < 0)
        TEST_ERROR;

    /* Enable swmr write */
    if (H5Fstart_swmr_write(fid) < 0)
        TEST_ERROR;

    /* Verify dataset still has correct access properties */
    if (tssw_persist_dapl_verify(did, vdsid1, vdsid2, boundary, append_func, append_func_ud, rdcc_nslots,
                                 rdcc_nbytes, rdcc_w0, efile_prefix, virtual_prefix, gap_size,
                                 virtual_view) < 0)
        TEST_ERROR;

    /* Close "dataset1" */
    if (H5Dclose(did) < 0)
        TEST_ERROR;

    /* Close "vds1" */
    if (H5Dclose(vdsid1) < 0)
        TEST_ERROR;

    /* Close "vds2" */
    if (H5Dclose(vdsid2) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close dataspace, dapl, dcpl*/
    if (H5Sclose(sid) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(vds_dcpl) < 0)
        TEST_ERROR;
    if (H5Pclose(dapl1) < 0)
        TEST_ERROR;
    if (H5Pclose(dapl2) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(fid);
        H5Pclose(fapl);
        H5Pclose(dcpl);
        H5Pclose(vds_dcpl);
        H5Pclose(dapl1);
        H5Pclose(dapl2);
        H5Dclose(did);
        H5Dclose(vdsid1);
        H5Dclose(vdsid2);
        H5Sclose(sid);
    }
    H5E_END_TRY

    return -1;
} /* test_start_swmr_write_persist_dapl() */

/*
 * Tests for H5Pset/get_object_flush_cb()
 */

/* The callback function for object flush property */
static herr_t
flush_cb(hid_t H5_ATTR_UNUSED obj_id, void *_udata)
{
    unsigned *flush_ct = (unsigned *)_udata;
    ++(*flush_ct);
    return 0;
}

/*
 * test_object_flush_cb()
 *
 * Verify the public routines H5Pget/set_object_flush_cb() work as specified:
 *  1) To verify the failure condition in setting object flush property
 *  2) To verify the object flush property values retrieved from a default
 *     file access property list.
 *  3) To verify the object flush property values retrieved from a non-default
 *     file access property list.
 *  4) To verify the object flush property values retrieved from a default
 *     file access property list of a file
 *  5) To verify the object flush property values retrieved from a non-default
 *     file access property list of a file
 *     To verify the object flush callback is invoked when doing H5Oflush(),
 *     H5Dflush(), H5Gflush() and H5Tflush().
 */
static int
test_object_flush_cb(hid_t in_fapl)
{
    hid_t          fapl  = H5I_INVALID_HID;                        /* A copy of file access property list */
    hid_t          ffapl = H5I_INVALID_HID;                        /* A file's file access property list */
    hid_t          fid   = H5I_INVALID_HID;                        /* File ID */
    hid_t          gid   = H5I_INVALID_HID;                        /* Group ID */
    hid_t          did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID; /* Dataset IDs */
    hid_t          sid     = H5I_INVALID_HID;                      /* Dataspace ID */
    hsize_t        dims[2] = {5, 10};                              /* Dataset dimension sizes */
    int            buf[50];                                        /* Data buffer */
    H5F_flush_cb_t ret_cb;                  /* The callback function set in object flush property */
    void          *ret_ct;                  /* The user data set in object flush property */
    unsigned       flush_ct = 0;            /* The user data for object flush property */
    char           filename[NAME_BUF_SIZE]; /* File name */
    int            i;                       /* Local index variable */
    herr_t         ret;                     /* Generic return value */

    TESTING("H5Pget/set_obj_flush_cb()");

    /*
     * Case (1)
     *  To verify the failure condition in setting object flush property
     */
    /* Should fail if the callback function is not defined but user data is defined */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_object_flush_cb(fapl, NULL, &flush_ct);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /*
     * Case (2)
     *  To verify the object flush property values retrieved from a
     *  default file access property list.
     */

    /* Create a copy of file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve object flush property values for the default file access property list */
    if (H5Pget_object_flush_cb(fapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;
    /* Should be null */
    if (ret_cb != NULL || ret_ct != NULL)
        TEST_ERROR;

    /*
     * Case (3)
     *  To verify the object flush property values retrieved from a
     *  non-default file access property list.
     */
    /* Set the object flush property */
    if (H5Pset_object_flush_cb(fapl, flush_cb, &flush_ct) < 0)
        TEST_ERROR;

    /* Increment the counter */
    ++flush_ct;

    /* Retrieve object flush property values for the non-default file access property list */
    if (H5Pget_object_flush_cb(fapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != flush_cb || *(unsigned *)ret_ct != 1)
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case (4)
     *  To verify the object flush property values retrieved from a
     *  default file access property list of a file
     */

    /* Reset values */
    flush_ct = 0;
    ret_cb   = NULL;
    ret_ct   = NULL;

    /* Make a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file: without setting object flush property in fapl */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's file access property list */
    if ((ffapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if (H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != NULL || ret_ct != NULL)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(ffapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Cases (5)
     *  To verify the object flush property values retrieved from a non-default
     *  file access property list of a file.
     *  To verify the object flush callback is invoked when doing H5Oflush(),
     *  H5Dflush(), H5Gflush() and H5Tflush().
     */
    /* Reset values */
    flush_ct = 0;
    ret_cb   = NULL;
    ret_ct   = NULL;

    /* Set the object flush property */
    if (H5Pset_object_flush_cb(fapl, flush_cb, &flush_ct) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file: with object flush property setting in fapl */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataset */
    if ((sid = H5Screate_simple(2, dims, dims)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataset */
    if ((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Initialize data buffer */
    for (i = 0; i < 50; i++)
        buf[i] = i + 1;

    /* Write to the dataset */
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
        FAIL_STACK_ERROR;

    /* Flush the dataset object */
    if (H5Oflush(did1) < 0)
        FAIL_STACK_ERROR;

    /* Get the file's file access property list */
    if ((ffapl = H5Fget_access_plist(fid)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if (H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != flush_cb || *(unsigned *)ret_ct != 1)
        TEST_ERROR;

    /* Create a group */
    if ((gid = H5Gcreate2(fid, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Flush the group */
    if (H5Gflush(gid) < 0)
        TEST_ERROR;

    /* Retrieve the object flush property values */
    if (H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != flush_cb || *(unsigned *)ret_ct != 2)
        TEST_ERROR;

    /* Create a dataset */
    if ((did2 = H5Dcreate2(gid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Flush the dataset */
    if (H5Dflush(did2) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the object flush property values */
    if (H5Pget_object_flush_cb(ffapl, &ret_cb, &ret_ct) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != flush_cb || *(unsigned *)ret_ct != 3)
        TEST_ERROR;

    /* Closing */
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Gclose(gid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(ffapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Pclose(ffapl);
        H5Sclose(sid);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Gclose(gid);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_object_flush_cb() */

/*
 * Tests for H5Pset/get_append_flush()
 */

/* The callback function for append flush property */
static herr_t
append_cb(hid_t H5_ATTR_UNUSED dset_id, hsize_t H5_ATTR_UNUSED *cur_dims, void *_udata)
{
    unsigned *count = (unsigned *)_udata;
    ++(*count++);
    return 0;
} /* append_cb() */

/* The callback function for append flush property */
static herr_t
append_cb2(hid_t H5_ATTR_UNUSED dset_id, hsize_t H5_ATTR_UNUSED *cur_dims, void *_udata)
{
    unsigned *count = (unsigned *)_udata;
    ++(*count++);
    return 0;
} /* append_cb2() */

/*
 * test_append_flush_generic()
 *
 * Verify H5Pget/set_append_flush() work as specified for a generic dataset
 * access property list:
 *  1) To verify the append flush property values retrieved from a default
 *     access property list.
 *      -- zero boundary, null callback function, null user data
 *  2) To verify the failure conditions in setting append flush property:
 *      -- an invalid dataset rank: <= 0, > H5S_MAX_RANK
 *      -- undefined callback but defined user data
 *      -- no boundary specified
 *      -- invalid boundary size: H5S_UNLIMITED, negative value
 *  3) To verify the append flush property values retrieved from a non-default
 *     access property list.
 *      -- the set callback function, the set user data
 *      -- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
 */
static int
test_append_flush_generic(void)
{
    hid_t           dapl = H5I_INVALID_HID; /* A copy of dataset access property */
    hsize_t         boundary[3];            /* The boundary for append flush property */
    unsigned        count = 0;              /* The user data for append flush property */
    hsize_t         ret_boundary[3];        /* The boundary set in append flush property */
    H5D_append_cb_t ret_cb;                 /* The callback function set in append flush property */
    unsigned       *ret_count;              /* The user data set in append flush property */
    herr_t          ret;                    /* The return value */

    TESTING("H5Fget/set_append_flush() for a generic dataset access property list");

    /*
     * Case (1)
     *  To verify the retrieved append flush property values:
     *      -- zero boundary, null callback function, null user data
     */

    /* Create a copy of dataset access property list */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(dapl, 2, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        FAIL_STACK_ERROR;

    /* Verify expected values */
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0)
        TEST_ERROR;
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;

    /* Close the property list */
    if (H5Pclose(dapl) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case (2)
     *  To verify the failure conditions in setting append flush property:
     *  -- an invalid dataset rank: <= 0, > H5S_MAX_RANK
     *  -- no boundary specified
     *  -- undefined callback but defined user data
     *  -- invalid boundary size: H5S_UNLIMITED, negative value
     */

    /* Create a copy of dataset access property list */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Invalid dataset rank: zero value */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, 0, NULL, NULL, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Invalid dataset rank: > H5S_MAX_RANK */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, H5S_MAX_RANK + 1, NULL, NULL, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* No boundary specified */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, 2, NULL, NULL, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Set up a valid boundary */
    boundary[0] = 1;
    boundary[1] = 1;

    /* Undefined callback function but defined user data */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, 2, boundary, NULL, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Invalid boundary size: negative value */
    boundary[0] = (hsize_t)-1;
    boundary[1] = 1;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, 2, boundary, append_cb, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /* Invalid boundary size: H5S_UNLIMITED */
    boundary[0] = 1;
    boundary[1] = H5S_UNLIMITED;
    H5E_BEGIN_TRY
    {
        ret = H5Pset_append_flush(dapl, 2, boundary, append_cb, &count);
    }
    H5E_END_TRY
    if (ret >= 0)
        TEST_ERROR;

    /*
     * Case (3)
     *  To verify the append flush property values retrieved from a non-default
     *  access property list:
     *      -- the set callback function, the set user data
     *      -- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
     */
    boundary[0] = boundary[1] = 1;
    boundary[2]               = 0;
    count                     = 1;
    if (H5Pset_append_flush(dapl, 2, boundary, append_cb, &count) < 0)
        FAIL_STACK_ERROR;
    ++count;

    /* Verify expected values: with boundary rank > set boundary rank */
    if (H5Pget_append_flush(dapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;
    if (ret_boundary[0] != 1 || ret_boundary[1] != 1 || boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb == NULL || ret_count == NULL || *ret_count != 2)
        TEST_ERROR;

    /* Verify expected values: with boundary rank < set boundary rank */
    memset(ret_boundary, 0, sizeof(ret_boundary));
    if (H5Pget_append_flush(dapl, 1, ret_boundary, NULL, NULL) < 0)
        TEST_ERROR;
    if (ret_boundary[0] != 1 || ret_boundary[1] != 0 || boundary[2] != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(dapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dapl);
    }
    H5E_END_TRY

    return -1;
} /* test_append_flush_generic() */

/*
 * test_append_flush_dataset_chunked()
 *
 * Verify H5Pget/set_append_flush() work as specified for a chunked dataset's
 * access property list:
 *  1) To verify the append flush property values retrieved from a default
 *     access property list:
 *     -- zero boundary, null callback function, null user data
 *      2) To verify failure in creating dataset when:
 *     -- the rank set in append flush property is not the same as the dataset's rank
 *     -- boundary (non-zero) is set for a non-extendible dimension
 *      3) To verify the append flush property values retrieved from a non-default
 *     access property list:
 *     -- the set callback function, the set user data
 *     -- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
 */
static int
test_append_flush_dataset_chunked(hid_t in_fapl)
{
    hid_t fid  = H5I_INVALID_HID;                         /* file ID */
    hid_t fapl = H5I_INVALID_HID;                         /* A copy of file access property */
    hid_t did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID; /* The dataset ID */
    hid_t sid   = H5I_INVALID_HID;                        /* The dataspace ID */
    hid_t dcpl  = H5I_INVALID_HID;                        /* A copy of dataset creation property */
    hid_t dapl  = H5I_INVALID_HID;                        /* A copy of dataset access property */
    hid_t ddapl = H5I_INVALID_HID; /* The dataset access property of the opened dataset */

    hsize_t  boundary[3]; /* Boundary size */
    unsigned count = 0;   /* User data */

    hsize_t         ret_boundary[3]; /* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;          /* The callback function set in the append flush property */
    unsigned       *ret_count;       /* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE]; /* file name */

    hsize_t dims[2]       = {100, 0};             /* The dataset dimension sizes */
    hsize_t maxdims[2]    = {100, H5S_UNLIMITED}; /* The dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {5, 2};               /* The chunk dimension sizes */

    TESTING("H5Fget/set_append_flush() for a chunked dataset's access property list");

    /*
     *  Case (1)--
     *  For a chunked dataset's access property list:
     *  --to verify the append flush property values retrieved from a default access
     *    a default access property list is:
     *      zero rank, zero boundary, null callback function, null user data
     */

    /* Get a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with 1 extendible dimension */
    if ((sid = H5Screate_simple(2, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did1)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;

    /* Close the dataset's access property list */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (2)--
     *  For a chunked dataset's access property list:
     *  --to verify failure in creating the dataset when:
     *      --the rank set in append flush property is not the same as the dataset's rank
     *      -- boundary (non-zero) is set for a non-extendible dimension
     *  --to verify failure in opening the dataset
     *      -- boundary (non-zero) is set for a non-extendible dimension
     */
    /* Create a copy of dataset access property list */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    /* Set boundary dimension rank > the rank of dataset to be created */
    memset(boundary, 0, sizeof(boundary));
    if (H5Pset_append_flush(dapl, 3, boundary, NULL, NULL) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to Create the dataset */
    H5E_BEGIN_TRY
    {
        did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl);
    }
    H5E_END_TRY
    if (did2 >= 0)
        TEST_ERROR;

    /* Set boundary for a non-extendible dimension */
    boundary[0] = boundary[1] = 1;
    if (H5Pset_append_flush(dapl, 2, boundary, NULL, NULL) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to create the dataset */
    H5E_BEGIN_TRY
    {
        did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl);
    }
    H5E_END_TRY
    if (did2 >= 0)
        TEST_ERROR;

    /* Create and close the dataset */
    if ((did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;

    /* Should fail to open the dataset */
    H5E_BEGIN_TRY
    {
        did2 = H5Dopen2(fid, "dataset2", dapl);
    }
    H5E_END_TRY
    if (did2 >= 0)
        TEST_ERROR;

    /*
     *  Case (3)--
     *  For a chunked dataset's access property list:
     *  --To verify the append flush property values retrieved from a non-default
     *    access property list:
     *      -- the set callback function, the set user data
     *      -- the # of boundary sizes retrieved does not exceed MIN(input ndims, the ndims set)
     */

    boundary[0] = 0;
    boundary[1] = 1;
    if (H5Pset_append_flush(dapl, 2, boundary, append_cb, &count) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid, "dataset2", dapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    memset(ret_boundary, 0, sizeof(ret_boundary));
    ret_cb    = NULL;
    ret_count = NULL;
    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != append_cb || ret_count != &count)
        TEST_ERROR;
    if (ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
        TEST_ERROR;

    memset(ret_boundary, 0, sizeof(ret_boundary));
    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 1, ret_boundary, NULL, NULL) < 0)
        TEST_ERROR;
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dapl);
        H5Pclose(ddapl);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Pclose(fapl);
        H5Sclose(sid);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_append_flush_dataset_chunked() */

/*
 * test_append_flush_dataset_fixed():
 *
 * Verify H5Pget/set_append_flush() work as specified for a
 * non-chunked (fixed size) dataset's access property list:
 *  (1) To verify success in creating the dataset--whatever is set for the append flush property setting
 *  (2) To verify that default append flush property values are retrieved for both
 *      default or non-default access property list:
 *      -- zero boundary, null callback function, null user data
 */
static int
test_append_flush_dataset_fixed(hid_t in_fapl)
{
    hid_t fid  = H5I_INVALID_HID;                         /* file ID */
    hid_t fapl = H5I_INVALID_HID;                         /* A copy of file access property */
    hid_t did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID; /* The dataset ID */
    hid_t sid   = H5I_INVALID_HID;                        /* The dataspace ID */
    hid_t dapl  = H5I_INVALID_HID;                        /* A copy of dataset access property */
    hid_t ddapl = H5I_INVALID_HID; /* The dataset access property of the opened dataset */

    hsize_t  boundary[3]; /* Boundary size */
    unsigned count = 0;   /* User data */

    hsize_t         ret_boundary[3]; /* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;          /* The callback function set in the append flush property */
    unsigned       *ret_count;       /* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE]; /* file name */

    hsize_t dims[1] = {100};

    TESTING("H5Fget/set_append_flush() for a non-chunked dataset's access property list");

    /*
     *  Case (1)--
     *  For a non-chunked dataset's access property list:
     *  --to verify the append flush property values retrieved from
     *    a default access property list is:
     *      zero boundary, null callback function, null user data
     */

    /* Get a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a dataset */
    if ((sid = H5Screate_simple(1, dims, dims)) < 0)
        FAIL_STACK_ERROR;
    if ((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did1)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;

    /* Close the dataset's access property list */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;

    /*
     *  Case (2)--
     *  For a non-chunked dataset's access property list:
     *  --to verify success in creating and opening the dataset even when append flush property
     *    is setup with error conditions:
     *      --the rank set in append flush property is not the same as the dataset's rank
     *      --boundary is set
     *  --to verify the append flush property values are:
     *      zero boundary, null callback function, null user data
     */
    /* Create a copy of dataset access property list */
    if ((dapl = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    boundary[0] = 1;
    boundary[1] = boundary[2] = 0;
    if (H5Pset_append_flush(dapl, 3, boundary, append_cb, &count) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed to create the dataset: append flush property has no effect */
    if ((did2 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, dapl)) < 0)
        TEST_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;

    /* Should succeed in opening the dataset: append flush property has no effect */
    if ((did2 = H5Dopen2(fid, "dataset2", dapl)) < 0)
        TEST_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;

    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    /*
     *  Case (3)--
     *  For a non-chunked dataset's access property list:
     *  --To verify the append flush property values retrieved from a non-default
     *    access property list:
     *      zero boundary, null callback function, null user data
     */

    memset(boundary, 0, sizeof(boundary));
    if (H5Pset_append_flush(dapl, 1, boundary, append_cb, &count) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid, "dataset2", dapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 1, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values */
    if (ret_cb != NULL || ret_count != NULL)
        TEST_ERROR;
    if (ret_boundary[0] != 0 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dapl);
        H5Pclose(ddapl);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Pclose(fapl);
        H5Sclose(sid);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_append_flush_dataset_fixed() */

/*
 * test_append_flush_multiple()
 *
 * Verify H5Pget/set_append_flush() work as specified for multiple opens
 * of a dataset:
 *  (1) did1 = H5Dcreate(...dapl1...)
 *      did2 = H5Dopen2(...dapl2)
 *      H5Pget_append_flush(did1...)
 *      H5Pget_append_flush(did2...)
 *      -- should return append flush property values set in dapl1
 *  (2) H5Dcreate(...H5P_DEFAULT...)
 *      H5Dclose()
 *      did1 = H5Dopen2(...dapl1)
 *      did2 = H5Dopen2(..dapl2)
 *      H5Pget_append_flush(did1, ...)
 *      H5Pget_append_flush(did2, ...)
 *      -- should return append flush property values set in dapl1
 *  NOTE:
 *    FOR NOW: return the append flush property values of the create or the very first open
 *    LATER ON: should REJECT subsequent dataset open if append flush property values differ
 */
static int
test_append_flush_dataset_multiple(hid_t in_fapl)
{
    hid_t fid  = H5I_INVALID_HID;                         /* file ID */
    hid_t fapl = H5I_INVALID_HID;                         /* A copy of file access property */
    hid_t did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID; /* The dataset ID */
    hid_t sid   = H5I_INVALID_HID;                        /* The dataspace ID */
    hid_t dcpl  = H5I_INVALID_HID;                        /* A copy of dataset creation property */
    hid_t dapl1 = H5I_INVALID_HID;                        /* A copy of dataset access property */
    hid_t dapl2 = H5I_INVALID_HID;                        /* A copy of dataset access property */
    hid_t ddapl = H5I_INVALID_HID; /* The dataset access property of the opened dataset */

    hsize_t  boundary1[3]; /* Boundary size */
    hsize_t  boundary2[3]; /* Boundary size */
    unsigned count1 = 0;   /* User data */
    unsigned count2 = 0;   /* User data */

    hsize_t         ret_boundary[3]; /* Boundary size set in the append flush property */
    H5D_append_cb_t ret_cb;          /* The callback function set in the append flush property */
    unsigned       *ret_count;       /* The user data set in the append flush property */

    char filename[NAME_BUF_SIZE]; /* file name */

    hsize_t dims[2]       = {0, 0};                         /* The dataset dimension sizes */
    hsize_t maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED}; /* The dataset maximum dimension sizes */
    hsize_t chunk_dims[2] = {5, 2};                         /* The chunk dimension sizes */

    TESTING("H5Fget/set_append_flush() for multiple opens of a chunked dataset");

    /*
     *  Case (1)
     *  For a chunked dataset's access property list:
     *      did1 = H5Dcreate(...dapl1...)
     *          did2 = H5Dopen2(...dapl2)
     *          H5Pget_append_flush(did1...)
     *          H5Pget_append_flush(did2...)
     *      -- should return append flush property values set in dapl1
     */

    /* Create a copy of dataset access property list */
    if ((dapl1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if ((dapl2 = H5Pcreate(H5P_DATASET_ACCESS)) < 0)
        FAIL_STACK_ERROR;

    boundary1[0] = 0;
    boundary1[1] = 1;
    count1       = 0;
    if (H5Pset_append_flush(dapl1, 2, boundary1, append_cb, &count1) < 0)
        FAIL_STACK_ERROR;
    boundary2[0] = 1;
    boundary2[1] = 0;
    count2       = 0;
    if (H5Pset_append_flush(dapl2, 2, boundary2, append_cb2, &count2) < 0)
        FAIL_STACK_ERROR;

    /* Get a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the test file to work on */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Create a chunked dataset with 2 extendible dimensions */
    if ((sid = H5Screate_simple(2, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did1 = H5Dcreate2(fid, "dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did2 = H5Dopen2(fid, "dataset1", dapl2)) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did1 */
    if ((ddapl = H5Dget_access_plist(did1)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values: should be the setting in dapl1 */
    if (ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != append_cb || ret_count != &count1)
        TEST_ERROR;

    /* Close the dataset's access property list */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did2 */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values: should be the setting in dapl1 */
    if (ret_boundary[0] != 0 || ret_boundary[1] != 1 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != append_cb || ret_count != &count1)
        TEST_ERROR;

    /* Close the dataset's access property list */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;
    H5Dclose(did1);
    H5Dclose(did2);

    /*
     * Case (2)
     *  For a chunked dataset's access property list:
     *      H5Dcreate(...H5P_DEFAULT...)
     *          H5Dclose()
     *          did1 = H5Dopen2(...dapl1)
     *          did2 = H5Dopen2(..dapl2)
     *          H5Pget_append_flush(did1, ...)
     *          H5Pget_append_flush(did2, ...)
     *      -- should return append flush property values set in dapl1
     */
    if ((did1 = H5Dcreate2(fid, "dataset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset with append flush setting in dapl2 */
    if ((did1 = H5Dopen2(fid, "dataset2", dapl2)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset with append flush setting in dapl1 */
    if ((did2 = H5Dopen2(fid, "dataset2", dapl1)) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did1 */
    if ((ddapl = H5Dget_access_plist(did1)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values: should be the setting in dapl2 */
    if (ret_boundary[0] != 1 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != append_cb2 || ret_count != &count2)
        TEST_ERROR;

    /* Close the access property list */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;

    /* Get the dataset's access property list for did2 */
    if ((ddapl = H5Dget_access_plist(did2)) < 0)
        FAIL_STACK_ERROR;

    /* Retrieve the append flush property values */
    if (H5Pget_append_flush(ddapl, 3, ret_boundary, &ret_cb, (void **)&ret_count) < 0)
        TEST_ERROR;

    /* Verify expected values: should be the setting in dapl2 */
    if (ret_boundary[0] != 1 || ret_boundary[1] != 0 || ret_boundary[2] != 0)
        TEST_ERROR;
    if (ret_cb != append_cb2 || ret_count != &count2)
        TEST_ERROR;

    /* Closing */
    if (H5Pclose(ddapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl2) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dapl1) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl);
        H5Pclose(dapl1);
        H5Pclose(dapl2);
        H5Pclose(ddapl);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Pclose(fapl);
        H5Sclose(sid);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_append_flush_dataset_multiple() */

/****************************************************************
**
**  test_file_lock_same():
**    With the implementation of file locking, this test checks file
**    open with different combinations of flags.
**    This is for single process access.
**
*****************************************************************/
static int
test_file_lock_same(hid_t in_fapl)
{
    hid_t    fid = H5I_INVALID_HID, fid2 = H5I_INVALID_HID; /* File IDs */
    hid_t    fapl = H5I_INVALID_HID;                        /* File access property list */
    unsigned intent;                                        /* File access flags */
    char     filename[NAME_BUF_SIZE];                       /* file name */

    /* Output message about test being performed */
    TESTING("File open with different combinations of flags--single process access");

    /* Set locking in the fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_file_locking(fapl, true, true) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    /*
     * Case 1: 1) RDWR 2) RDWR : should succeed
     */
    /* Create file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check file intent */
    if (H5Fget_intent(fid, &intent) < 0)
        FAIL_STACK_ERROR;

    if (intent != H5F_ACC_RDWR)
        TEST_ERROR;

    /* Open the same file with RDWR */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent */
    if (H5Fget_intent(fid2, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDWR)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 2: 1) RDWR 2) RDONLY : should succeed
     */
    /* Open file with RDWR */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent */
    if (H5Fget_intent(fid, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDWR)
        TEST_ERROR;

    /* Open file with RDONLY */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent: should get intent from 1st open */
    if (H5Fget_intent(fid2, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDWR)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 3: 1) RDONLY 2) RDWR : should fail
     */
    /* Open file with RDONLY */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent */
    if (H5Fget_intent(fid, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDONLY)
        TEST_ERROR;

    /* Open file with RDWR should fail */
    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close first file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 4: 1) RDONLY 2) RDONLY : should succeed
     */
    /* Open file with RDONLY */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent */
    if (H5Fget_intent(fid, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDONLY)
        TEST_ERROR;

    /* Open file with RDONLY */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get and check the intent */
    if (H5Fget_intent(fid2, &intent) < 0)
        FAIL_STACK_ERROR;
    if (intent != H5F_ACC_RDONLY)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Fclose(fid2);
    }
    H5E_END_TRY

    return -1;
} /* end test_file_lock_same() */

/****************************************************************
**
**  test_file_lock_swmr_same():
**    With the implementation of file locking, this test checks file
**    open with different combinations of flags + SWMR flags.
**    This is for single process access.
**
*****************************************************************/
static int
test_file_lock_swmr_same(hid_t in_fapl)
{
    hid_t fid  = H5I_INVALID_HID; /* File IDs */
    hid_t fid2 = H5I_INVALID_HID;
    hid_t fapl = H5I_INVALID_HID;  /* File access property list */
    char  filename[NAME_BUF_SIZE]; /* file name */

    /* Output message about test being performed */
    TESTING("File open with different combinations of flags + SWMR flags--single process access");

    /* Set locking in the fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create a file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Cases a, b, c, d: H5Fopen failure cases
     */

    /*
     * Case a: RDWR|SWRM_READ : should fail
     */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_READ, fapl);
    }
    H5E_END_TRY
    if (fid >= 0)
        TEST_ERROR;

    /*
     * Case b: RDWR|SWMM_WRTE|SWMR_READ : should fail
     */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE | H5F_ACC_SWMR_READ, fapl);
    }
    H5E_END_TRY
    if (fid >= 0)
        TEST_ERROR;

    /*
     * Case c: RDONLY|SWMM_WRITE : should fail
     */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_WRITE, fapl);
    }
    H5E_END_TRY
    if (fid >= 0)
        TEST_ERROR;

    /*
     * Case d: RDONLY|SWMM_WRITE|SWMR_READ : should fail
     */
    H5E_BEGIN_TRY
    {
        fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_WRITE | H5F_ACC_SWMR_READ, fapl);
    }
    H5E_END_TRY
    if (fid >= 0)
        TEST_ERROR;

    /*
     * Cases 1 - 12: combinations of different flags for 1st and 2nd opens
     */

    /*
     * Case 1: 1) RDWR 2) RDWR|SWMR_WRITE : should fail
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 2: 1) RDWR 2) RDONLY|SWMR_READ : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 3: 1) RDWR|SWMR_WRITE 2)RDWR : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 4: 1) RDWR|SWMR_WRITE 2) RDWR|SWMR_WRITE : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 5: 1) RDWR|SWMR_WRITE 2) RDONLY|SWMR_READ : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 6: 1) RDWR|SWMR_WRITE 2) RDONLY : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 7: 1) RDONLY|SWMR_READ 2)RDWR : should fail
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 8: 1) RDONLY|SWMR_READ 2) RDWR|SWMR_WRITE : should fail
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 9: 1) RDONLY|SWMR_READ 2) RDONLY|SWMR_READ : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 10: 1) RDONLY|SWMR_READ 2) RDONLY : should succeed
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        TEST_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close file */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 11: 1) RDONLY 2) RDWR|SWMR_WRITE: should fail
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 12: 1) RDONLY 2) RDONLY|SWMR_READ : should fail
     */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    H5E_BEGIN_TRY
    {
        fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
    }
    H5E_END_TRY
    if (fid2 >= 0)
        TEST_ERROR;

    /* Close file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Fclose(fid2);
    }
    H5E_END_TRY

    return -1;
} /* end test_file_lock_swmr_same() */

/****************************************************************
**
**  test_file_lock_concur():
**    With the implementation of file locking, this test checks file
**    open with different combinations of flags.
**    This is for concurrent access.
**
*****************************************************************/
#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID) && defined(H5_HAVE_FLOCK))

static int
test_file_lock_concur(hid_t H5_ATTR_UNUSED in_fapl)
{
    /* Output message about test being performed */
    TESTING("File open with different combinations of flags--concurrent access");
    SKIPPED();
    puts("    Test skipped due to fork or waitpid not defined.");
    return 0;

} /* end test_file_lock_concur() */

#else /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID) && defined(H5_HAVE_FLOCK)) */

static int
test_file_lock_concur(hid_t in_fapl)
{
    hid_t fid  = H5I_INVALID_HID;  /* File ID */
    hid_t fapl = H5I_INVALID_HID;  /* File access property list */
    char  filename[NAME_BUF_SIZE]; /* file name */
    pid_t childpid = 0;            /* Child process ID */
    int   child_status;            /* Status passed to waitpid */
    int   child_wait_option = 0;   /* Options passed to waitpid */
    int   out_pdf[2];
    int   notify = 0;

    /* Output message about test being performed */
    TESTING("File open with different combinations of flags--concurrent access");

    /* Set locking in the fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_file_locking(fapl, true, true) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 1: 1) RDWR 2) RDWR : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 2: 1) RDWR 2) RDONLY : should fail
     */
    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Opens the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Opens the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 3: 1) RDONLY 2) RDWR : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Opens the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    } /* end if */

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Opens the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 4: 1) RDONLY 2) RDONLY : should succeed
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Opens the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl);
        }
        H5E_END_TRY

        /* Should succeed */
        if (child_fid >= 0) {
            /* Close the file */
            if (H5Fclose(child_fid) < 0)
                exit(EXIT_FAILURE);

            /* Close the pipe */
            if (HDclose(out_pdf[0]) < 0)
                exit(EXIT_FAILURE);

            exit(EXIT_SUCCESS);
        } /* end if */

        exit(EXIT_FAILURE);
    } /* end if */

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Create file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;

} /* end test_file_lock_concur() */

#endif /* #if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID) && defined(H5_HAVE_FLOCK)) */

/****************************************************************
**
**  test_file_lock_swmr_concur(): low-level file test routine.
**    With the implementation of file locking, this test checks file
**    open with different combinations of flags + SWMR flags.
**    This is for concurrent access.
**
*****************************************************************/
#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID))

static int
test_file_lock_swmr_concur(hid_t H5_ATTR_UNUSED in_fapl)
{
    /* Output message about test being performed */
    TESTING("File open with different combintations of flags + SWMR flags--concurrent access");
    SKIPPED();
    puts("    Test skipped due to fork or waitpid not defined.");
    return 0;

} /* end test_file_lock_swmr_concur() */

#else /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */

static int
test_file_lock_swmr_concur(hid_t in_fapl)
{
    hid_t fid = H5I_INVALID_HID;   /* File ID */
    hid_t fapl;                    /* File access property list */
    char  filename[NAME_BUF_SIZE]; /* file name */
    pid_t childpid = 0;            /* Child process ID */
    int   child_status;            /* Status passed to waitpid */
    int   child_wait_option = 0;   /* Options passed to waitpid */
    int   out_pdf[2];
    int   notify = 0;

    /* Output message about test being performed */
    TESTING("File open with different combintations of flags + SWMR flags--concurrent access");

    /* Set locking in the fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_file_locking(fapl, true, true) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[2], fapl, filename, sizeof(filename));

    /* Set to use latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 1: 1) RDWR 2) RDWR|SWMR_WRITE : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 2: 1) RDWR 2) RDONLY|SWMR_READ: should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 3: 1) RDWR|SWMR_WRITE 2) RDWR : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR, H5P_DEFAULT);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 4: 1) RDWR|SWMR_WRITE 2) RDWR|SWMR_WRITE : should fail
     */

    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 5: 1) RDWR|SWMR_WRITE 2) RDONLY|SWMR_READ : should succeed
     */

    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
        }
        H5E_END_TRY

        /* Should succeed */
        if (child_fid >= 0) {
            if (H5Fclose(child_fid) < 0)
                FAIL_STACK_ERROR;
            exit(EXIT_SUCCESS);
        }

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 6: 1) RDWR|SWMR_WRITE 2) RDONLY : should fail
     */

    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 7: 1) RDONLY|SWMR_READ 2) RDWR : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 8: 1) RDONLY|SWMR_READ 2) RDWR|SWMR_WRITE : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 9: 1) RDONLY|SWMR_READ 2) RDONLY|SWMR_READ : should succeed
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
        }
        H5E_END_TRY

        /* Should succeed */
        if (child_fid >= 0) {
            if (H5Fclose(child_fid) < 0)
                FAIL_STACK_ERROR;
            exit(EXIT_SUCCESS);
        }

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 10: 1) RDONLY|SWMR_READ 2) RDONLY : should succeed
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        if ((child_fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
            FAIL_STACK_ERROR;

        /* Should succeed */
        if (child_fid >= 0) {
            if (H5Fclose(child_fid) < 0)
                FAIL_STACK_ERROR;
            exit(EXIT_SUCCESS);
        }

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 11: 1) RDONLY 2) RDWR|SWMR_WRITE : should fail
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl);
        }
        H5E_END_TRY

        /* Should fail */
        if (child_fid == FAIL)
            exit(EXIT_SUCCESS);

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* Close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /*
     * Case 12: 1) RDONLY 2) RDONLY|SWMR_READ : should succeed
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) { /* Child process */
        hid_t child_fid; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl);
        }
        H5E_END_TRY

        /* Should succeed */
        if (child_fid >= 0) {
            if (H5Fclose(child_fid) < 0)
                FAIL_STACK_ERROR;
            exit(EXIT_SUCCESS);
        }

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_FAILURE);
    }

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        FAIL_STACK_ERROR;

    /* Check if child terminated normally */
    if (WIFEXITED(child_status)) {
        /* Check exit status of the child */
        if (WEXITSTATUS(child_status) != 0)
            TEST_ERROR;
    }
    else
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;

} /* end test_file_lock_swmr_concur() */

#endif /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */

/****************************************************************
**
**  test_file_locking():
**    Tests various combinations of file locking flags and
**    and environment variables.
**
*****************************************************************/
static int
test_file_locking(hid_t in_fapl, bool turn_locking_on, bool env_var_override)
{
#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID))
    if (turn_locking_on && env_var_override)
        TESTING("File locking: ON w/ env var override");
    else if (turn_locking_on && !env_var_override)
        TESTING("File locking: ON");
    else if (!turn_locking_on && env_var_override)
        TESTING("File locking: OFF w/ env var override");
    else
        TESTING("File locking: OFF");
    SKIPPED();
    puts("    Test skipped due to fork or waitpid not defined.");
    return 0;
#else /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */
    hid_t  fid  = H5I_INVALID_HID;  /* File ID */
    hid_t  fapl = H5I_INVALID_HID;  /* File access property list */
    char   filename[NAME_BUF_SIZE]; /* file name */
    pid_t  childpid = 0;            /* Child process ID */
    int    child_status;            /* Status passed to waitpid */
    int    child_wait_option = 0;   /* Options passed to waitpid */
    int    out_pdf[2];
    int    notify      = 0;
    int    exit_status = 0;
    herr_t ret;

    if (turn_locking_on && env_var_override)
        TESTING("File locking: ON w/ env var override");
    else if (turn_locking_on && !env_var_override)
        TESTING("File locking: ON");
    else if (!turn_locking_on && env_var_override)
        TESTING("File locking: OFF w/ env var override");
    else
        TESTING("File locking: OFF");

    /* Copy the incoming fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        TEST_ERROR;

    /* Set locking in the fapl */
    if (H5Pset_file_locking(fapl, turn_locking_on ? true : false, true) < 0)
        TEST_ERROR;

    /* If requested, set the environment variable */
    if (env_var_override) {
        if (HDsetenv(HDF5_USE_FILE_LOCKING, turn_locking_on ? "FALSE" : "TRUE", true) < 0)
            TEST_ERROR;
        if (H5F__reparse_file_lock_variable_test() < 0)
            TEST_ERROR;
    }
    else {
        if (HDsetenv(HDF5_USE_FILE_LOCKING, "", true) < 0)
            TEST_ERROR;
        if (H5F__reparse_file_lock_variable_test() < 0)
            TEST_ERROR;
    }

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl, filename, sizeof(filename));

    /* Create the test file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Open a file for read-only and then read-write. This will fail
     * when the locking scheme is turned on.
     */

    /* Create 1 pipe */
    if (pipe(out_pdf) < 0)
        TEST_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        TEST_ERROR;

    if (childpid == 0) {

        /* Child process */

        hid_t child_fid    = H5I_INVALID_HID; /* File ID */
        int   child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1) {
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);
        }

        /* Open and close the test file */
        H5E_BEGIN_TRY
        {
            child_fid = H5Fopen(filename, H5F_ACC_RDWR, fapl);
            ret       = H5Fclose(child_fid);
        }
        H5E_END_TRY

        /* Close the pipe */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        if (H5I_INVALID_HID == child_fid || FAIL == ret)
            exit(EXIT_FAILURE);
        else
            exit(EXIT_SUCCESS);
    } /* end child process work */

    /* close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        TEST_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        TEST_ERROR;

    /* Close the pipe */
    if (HDclose(out_pdf[1]) < 0)
        TEST_ERROR;

    /* Wait for child process to complete */
    if (waitpid(childpid, &child_status, child_wait_option) < 0)
        TEST_ERROR;

    /* Check exit status of the child */
    if (WIFEXITED(child_status))
        exit_status = WEXITSTATUS(child_status);
    else
        TEST_ERROR;

    /* The child process should have passed or failed as follows:
     *
     * locks on:                            FAIL
     * locks off:                           PASS
     * locks on, env var override:          PASS
     * locks off, env var override:         FAIL
     */
    if (turn_locking_on && !env_var_override && (0 == exit_status))
        TEST_ERROR;
    else if (!turn_locking_on && !env_var_override && (0 != exit_status))
        TEST_ERROR;
    else if (turn_locking_on && env_var_override && (0 != exit_status))
        TEST_ERROR;
    else if (!turn_locking_on && env_var_override && (0 == exit_status))
        TEST_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    /* Close the copied property list */
    if (H5Pclose(fapl) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;

#endif /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */

} /* end test_file_locking() */

/****************************************************************
**
**  test_different_lock_flags():
**    Tests opening a file multiple times with different lock
**    flags.
**
*****************************************************************/
static int
test_different_lock_flags(hid_t in_fapl)
{
    hid_t fid1    = H5I_INVALID_HID; /* File ID */
    hid_t fid2    = H5I_INVALID_HID; /* File ID */
    hid_t fid3    = H5I_INVALID_HID; /* File ID */
    hid_t fapl_id = H5I_INVALID_HID; /* File access property list */
    char  filename[NAME_BUF_SIZE];   /* File name */

    TESTING("Using different lock flags");

    /* Copy the incoming fapl */
    if ((fapl_id = H5Pcopy(in_fapl)) < 0)
        TEST_ERROR;

    /* Set locking in the fapl */
    if (H5Pset_file_locking(fapl_id, true, true) < 0)
        TEST_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[1], fapl_id, filename, sizeof(filename));

    /* Create the test file */
    if ((fid1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
        TEST_ERROR;

    /* Open the test file with the same flags (should pass) */
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl_id)) < 0)
        TEST_ERROR;

    /* Unset locking in the fapl */
    if (H5Pset_file_locking(fapl_id, false, false) < 0)
        TEST_ERROR;

    /* Open the test file with different flags (should FAIL) */
    H5E_BEGIN_TRY
    {
        fid3 = H5Fopen(filename, H5F_ACC_RDWR, fapl_id);
    }
    H5E_END_TRY
    if (H5I_INVALID_HID != fid3)
        FAIL_PUTS_ERROR("Should not have been able to open a file with different locking flags");

    /* Close the files */
    if (H5Fclose(fid1) < 0)
        TEST_ERROR;
    if (H5Fclose(fid2) < 0)
        TEST_ERROR;

    /* Close the copied property list */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Fclose(fid3);
    }
    H5E_END_TRY

    return -1;
} /* end test_different_lock_flags() */

static int
test_swmr_vfd_flag(void)
{
    hid_t fid       = H5I_INVALID_HID; /* file ID */
    hid_t sec2_fapl = H5I_INVALID_HID; /* fapl ID of a VFD that supports SWMR writes (sec2) */
    hid_t bad_fapl  = H5I_INVALID_HID; /* fapl ID of a VFD that does not support SWMR writes (stdio) */
    char  filename[NAME_BUF_SIZE];     /* file name */

    TESTING("SWMR-enabled VFD flag functionality");

    /* Attempt to open a file using a SWMR-compatible VFD. */

    if ((sec2_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_fapl_sec2(sec2_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_libver_bounds(sec2_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    h5_fixname(FILENAME[0], sec2_fapl, filename, sizeof(filename));
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, sec2_fapl)) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Attempt to open a file using a non-SWMR-compatible VFD. */

    if ((bad_fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_fapl_stdio(bad_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_libver_bounds(bad_fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    fid = -1;
    h5_fixname(FILENAME[0], bad_fapl, filename, sizeof(filename));
    H5E_BEGIN_TRY
    {
        fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, bad_fapl);
    }
    H5E_END_TRY
    if (fid >= 0)
        TEST_ERROR;

    if (H5Pclose(sec2_fapl) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(bad_fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(sec2_fapl);
        H5Pclose(bad_fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_swmr_vfd_flag() */

#ifdef OUT
/*
 * This exposes a bug for H5Orefresh while handling opened objects for H5Fstart_swmr_write().
 * The boolean to skip file truncation test when reading in superblock will fix the problem.
 * Will work to move that to test/flushrefresh.c later.
 */
static int
test_bug_refresh(hid_t in_fapl)
{
    hid_t  fid  = H5I_INVALID_HID;
    hid_t  fapl = H5I_INVALID_HID;
    H5F_t *f;
    hid_t  gid1 = H5I_INVALID_HID;
    hid_t  gid2 = H5I_INVALID_HID;
    hid_t  gid3 = H5I_INVALID_HID;
    hid_t  gid4 = H5I_INVALID_HID;
    hid_t  gid5 = H5I_INVALID_HID;
    hid_t  gid6 = H5I_INVALID_HID;
    hid_t  gid7 = H5I_INVALID_HID;
    hid_t  gid8 = H5I_INVALID_HID;
    hid_t  gid9 = H5I_INVALID_HID;
    char   filename[NAME_BUF_SIZE]; /* File name */

    /* Create a copy of the input parameter in_fapl */
    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set to use the latest library format */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    TESTING("H5Orefresh failure conditions");

    /* Create a file with the latest format */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Get a pointer to the internal file object */
    if (NULL == (f = (H5F_t *)H5VL_object(fid)))
        FAIL_STACK_ERROR;

    /* Create groups: compact to dense storage */
    if ((gid1 = H5Gcreate2(fid, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid2 = H5Gcreate2(fid, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid3 = H5Gcreate2(fid, "group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid4 = H5Gcreate2(fid, "group4", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid5 = H5Gcreate2(fid, "group5", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid6 = H5Gcreate2(fid, "group6", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid7 = H5Gcreate2(fid, "group7", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid8 = H5Gcreate2(fid, "group8", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((gid9 = H5Gcreate2(fid, "group9", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    if (H5Fflush(fid, H5F_SCOPE_GLOBAL) < 0)
        TEST_ERROR;

    if (H5Grefresh(gid1) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid2) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid3) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid4) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid5) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid6) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid7) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid8) < 0)
        TEST_ERROR;
    if (H5Grefresh(gid9) < 0)
        TEST_ERROR;

    H5Gclose(gid1);
    H5Gclose(gid2);
    H5Gclose(gid3);
    H5Gclose(gid4);
    H5Gclose(gid5);
    H5Gclose(gid6);
    H5Gclose(gid7);
    H5Gclose(gid8);
    H5Gclose(gid9);
    H5Pclose(fapl);
    H5Fclose(fid);
    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(gid1);
        H5Gclose(gid2);
        H5Gclose(gid3);
        H5Gclose(gid4);
        H5Gclose(gid5);
        H5Gclose(gid6);
        H5Gclose(gid7);
        H5Gclose(gid8);
        H5Gclose(gid9);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;
} /* test_bug_refresh() */
#endif /* OUT */

/*
 * test_refresh_concur():
 *
 * The "new_format" parameter indicates whether to create the file with latest format or not.
 *  To have SWMR support, can use either one of the following in creating a file:
 *  (a) Create the file with write + latest format:
 *      --result in v3 superblock with latest chunk indexing types
 *  (b) Create the file with SWMR write + non-latest-format:
 *      --result in v3 superblock with latest chunk indexing types
 *
 * Verify H5Drefresh() works correctly with concurrent access:
 *      Parent process:
 *              (1) Open the test file, write to the dataset
 *              (2) Notify child process #A
 *              (3) Wait for notification from child process #B
 *              (4) Extend the dataset, write to the dataset, flush the file
 *              (5) Notify child process #C
 *      Child process:
 *              (1) Wait for notification from parent process #A
 *              (2) Open the file 2 times
 *              (3) Open the dataset 2 times with the 2 files
 *              (4) Verify the dataset's dimension and data read are correct
 *              (5) Notify parent process #B
 *              (6) Wait for notification from parent process #C
 *              (7) Refresh the dataset
 *              (8) Verify the dataset's dimension and data are correct
 */
#if !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID))

static int
test_refresh_concur(hid_t H5_ATTR_UNUSED in_fapl, bool new_format)
{
    if (new_format) {
        TESTING("H5Drefresh()--concurrent access for latest format");
    }
    else {
        TESTING("H5Drefresh()--concurrent access for non-latest-format");
    }

    SKIPPED();
    puts("    Test skipped due to fork or waitpid not defined.");
    return 0;
} /* test_refresh_concur() */

#else  /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */

static int
test_refresh_concur(hid_t in_fapl, bool new_format)
{
    hid_t fid = H5I_INVALID_HID;   /* File ID */
    hid_t fapl;                    /* File access property list */
    pid_t childpid = 0;            /* Child process ID */
    pid_t tmppid;                  /* Child process ID returned by waitpid */
    int   child_status;            /* Status passed to waitpid */
    int   child_wait_option = 0;   /* Options passed to waitpid */
    int   child_exit_val;          /* Exit status of the child */
    char  filename[NAME_BUF_SIZE]; /* File name */

    hid_t   did           = H5I_INVALID_HID;
    hid_t   sid           = H5I_INVALID_HID;
    hid_t   dcpl          = H5I_INVALID_HID;
    hsize_t chunk_dims[1] = {1};
    hsize_t maxdims[1]    = {H5S_UNLIMITED};
    hsize_t dims[1]       = {1};
    hsize_t new_dims[1]   = {2};

    int out_pdf[2];
    int in_pdf[2];
    int notify = 0;
    int wbuf[2];

    /* Output message about test being performed */
    if (new_format) {
        TESTING("H5Drefresh()--concurrent access for latest format");
    }
    else {
        TESTING("H5Drefresh()--concurrent access for non-latest-format");
    } /* end if */

    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if (new_format) {
        /* Set to use the latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR;

        /* Create the test file */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        /* Create the test file without latest format but with SWMR write */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    } /* end if */

    /* Create a chunked dataset with 1 extendible dimension */
    if ((sid = H5Screate_simple(1, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 1, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Create 2 pipes */
    if (pipe(out_pdf) < 0)
        FAIL_STACK_ERROR;
    if (pipe(in_pdf) < 0)
        FAIL_STACK_ERROR;

    /* Fork child process */
    if ((childpid = fork()) < 0)
        FAIL_STACK_ERROR;

    if (childpid == 0) {                      /* Child process */
        hid_t   child_fid1 = H5I_INVALID_HID; /* File ID */
        hid_t   child_fid2 = H5I_INVALID_HID; /* File ID */
        hid_t   child_did1 = H5I_INVALID_HID, child_did2 = H5I_INVALID_HID;
        hid_t   child_sid = H5I_INVALID_HID;
        hsize_t tdims[1];
        int     rbuf[2]      = {0, 0};
        int     child_notify = 0;

        /* Close unused write end for out_pdf */
        if (HDclose(out_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        /* close unused read end for in_pdf */
        if (HDclose(in_pdf[0]) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 1)
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);

        /* Open the file 2 times */
        if ((child_fid1 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
            exit(EXIT_FAILURE);

        if ((child_fid2 = H5Fopen(filename, H5F_ACC_RDONLY | H5F_ACC_SWMR_READ, fapl)) < 0)
            exit(EXIT_FAILURE);

        /* Open the dataset 2 times */
        if ((child_did1 = H5Dopen2(child_fid1, "dataset", H5P_DEFAULT)) < 0)
            exit(EXIT_FAILURE);
        if ((child_did2 = H5Dopen2(child_fid2, "dataset", H5P_DEFAULT)) < 0)
            exit(EXIT_FAILURE);

        /* Get the dataset's dataspace via did1 */
        if ((child_sid = H5Dget_space(child_did1)) < 0)
            exit(EXIT_FAILURE);
        if (H5Sget_simple_extent_dims(child_sid, tdims, NULL) < 0)
            exit(EXIT_FAILURE);
        if (tdims[0] != 1)
            exit(EXIT_FAILURE);

        /* Read from the dataset via did2 */
        if (H5Dread(child_did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
            exit(EXIT_FAILURE);

        /* Verify the data is correct */
        if (rbuf[0] != 99)
            exit(EXIT_FAILURE);

        /* Notify parent process */
        child_notify = 2;
        if (HDwrite(in_pdf[1], &child_notify, sizeof(int)) < 0)
            exit(EXIT_FAILURE);

        /* Wait for notification from parent process */
        while (child_notify != 3)
            if (HDread(out_pdf[0], &child_notify, sizeof(int)) < 0)
                exit(EXIT_FAILURE);

        /* Refresh dataset via did1 */
        if (H5Drefresh(child_did1) < 0)
            exit(EXIT_FAILURE);

        /* Get the dataset's dataspace and verify */
        if ((child_sid = H5Dget_space(child_did1)) < 0)
            exit(EXIT_FAILURE);
        if (H5Sget_simple_extent_dims(child_sid, tdims, NULL) < 0)
            exit(EXIT_FAILURE);

        if (tdims[0] != 2)
            exit(EXIT_FAILURE);

        /* Read from the dataset */
        if (H5Dread(child_did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
            exit(EXIT_FAILURE);

        /* Verify the data is correct */
        if (rbuf[0] != 100 || rbuf[1] != 100)
            exit(EXIT_FAILURE);

        /* Close the 2 datasets */
        if (H5Dclose(child_did1) < 0)
            exit(EXIT_FAILURE);
        if (H5Dclose(child_did2) < 0)
            exit(EXIT_FAILURE);

        /* Close the 2 files */
        if (H5Fclose(child_fid1) < 0)
            exit(EXIT_FAILURE);
        if (H5Fclose(child_fid2) < 0)
            exit(EXIT_FAILURE);

        /* Close the pipes */
        if (HDclose(out_pdf[0]) < 0)
            exit(EXIT_FAILURE);
        if (HDclose(in_pdf[1]) < 0)
            exit(EXIT_FAILURE);

        exit(EXIT_SUCCESS);
    }

    /* Close unused read end for out_pdf */
    if (HDclose(out_pdf[0]) < 0)
        FAIL_STACK_ERROR;
    /* Close unused write end for in_pdf */
    if (HDclose(in_pdf[1]) < 0)
        FAIL_STACK_ERROR;

    /* Open the test file */
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset */
    if ((did = H5Dopen2(fid, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to the dataset */
    wbuf[0] = wbuf[1] = 99;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Flush to disk */
    if (H5Fflush(fid, H5F_SCOPE_LOCAL) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 1;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Wait for notification from child process */
    while (notify != 2) {
        if (HDread(in_pdf[0], &notify, sizeof(int)) < 0)
            FAIL_STACK_ERROR;
    }

    /* Cork the metadata cache, to prevent the object header from being
     * flushed before the data has been written */
    if (H5Odisable_mdc_flushes(did) < 0)
        FAIL_STACK_ERROR;

    /* Extend the dataset */
    if (H5Dset_extent(did, new_dims) < 0)
        FAIL_STACK_ERROR;

    /* Write to the dataset */
    wbuf[0] = wbuf[1] = 100;
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Uncork the metadata cache */
    if (H5Oenable_mdc_flushes(did) < 0)
        FAIL_STACK_ERROR;

    /* Flush to disk */
    if (H5Fflush(fid, H5F_SCOPE_LOCAL) < 0)
        FAIL_STACK_ERROR;

    /* Notify child process */
    notify = 3;
    if (HDwrite(out_pdf[1], &notify, sizeof(int)) < 0)
        FAIL_STACK_ERROR;

    /* Close the pipes */
    if (HDclose(out_pdf[1]) < 0)
        FAIL_STACK_ERROR;
    if (HDclose(in_pdf[0]) < 0)
        FAIL_STACK_ERROR;

    /* Wait for child process to complete */
    if ((tmppid = waitpid(childpid, &child_status, child_wait_option)) < 0)
        FAIL_STACK_ERROR;

    /* Check exit status of child process */
    if (WIFEXITED(child_status)) {
        if ((child_exit_val = WEXITSTATUS(child_status)) != 0)
            TEST_ERROR;
    }
    else /* Child process terminated abnormally */
        TEST_ERROR;

    /* Close the dataset */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
    }
    H5E_END_TRY

    return -1;

} /* test_refresh_concur() */
#endif /* !(defined(H5_HAVE_FORK) && defined(H5_HAVE_WAITPID)) */

/*
 * test_multiple_same():
 *
 * The "new_format" parameter indicates whether to create the file with latest format or not.
 *  To have SWMR support, can use either one of the following in creating a file:
 *  (a) Create the file with write + latest format:
 *      --result in v3 superblock with latest chunk indexing types
 *  (b) Create the file with SWMR write + non-latest-format:
 *      --result in v3 superblock with latest chunk indexing types
 *
 * Verify that H5Drefresh() and H5Fstart_swmr_write() work properly with multiple
 * opens of files and datasets.
 */
static int
test_multiple_same(hid_t in_fapl, bool new_format)
{
    hid_t fid = H5I_INVALID_HID, fid1 = H5I_INVALID_HID, fid2 = H5I_INVALID_HID,
          fid3 = H5I_INVALID_HID;    /* File IDs */
    hid_t   fapl;                    /* File access property list */
    char    filename[NAME_BUF_SIZE]; /* File name */
    hid_t   did = H5I_INVALID_HID, did1 = H5I_INVALID_HID, did2 = H5I_INVALID_HID, did3 = H5I_INVALID_HID;
    hid_t   sid           = H5I_INVALID_HID;
    hid_t   dcpl          = H5I_INVALID_HID;
    hsize_t chunk_dims[2] = {1, 2};
    hsize_t maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t dims[2]       = {1, 1};
    int     rbuf          = 0;
    int     wbuf          = 0;

    /* Output message about test being performed */
    if (new_format) {
        TESTING("multiple--single process access for latest format");
    }
    else {
        TESTING("multiple--single process access for non-latest-format");
    } /* end if */

    if ((fapl = H5Pcopy(in_fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if (new_format) {
        /* Set to use the latest library format */
        if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR;

        /* Create the test file */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    }
    else {
        /* Create the test file without latest format but with SWMR write */
        if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC | H5F_ACC_SWMR_WRITE, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR;
    } /* end if */

    /* Create a chunked dataset with 1 extendible dimension */
    if ((sid = H5Screate_simple(2, dims, maxdims)) < 0)
        FAIL_STACK_ERROR;
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR;
    if (H5Pset_chunk(dcpl, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR;
    if ((did = H5Dcreate2(fid, "dataset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Closing */
    if (H5Dclose(did) < 0)
        FAIL_STACK_ERROR;
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR;
    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR;

    /* Close the file */
    if (H5Fclose(fid) < 0)
        FAIL_STACK_ERROR;

    /* Case 1 */

    /* Open the file 3 times: SWMR-write, read-write, read-only */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDWR | H5F_ACC_SWMR_WRITE, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid3 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset 3 times with fid1, fid2, fid3 */
    if ((did1 = H5Dopen2(fid1, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid2, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did3 = H5Dopen2(fid3, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to the dataset via did1 */
    wbuf = 88;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Refresh via did2 */
    if (H5Drefresh(did2) < 0)
        FAIL_STACK_ERROR;

    /* Read from the dataset via did2 */
    rbuf = 0;
    if (H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    /* Verify the data is correct */
    if (rbuf != 88)
        FAIL_STACK_ERROR;

    /* Write to the dataset via did3 */
    wbuf = 99;
    if (H5Dwrite(did3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Refresh via did1 */
    if (H5Drefresh(did1) < 0)
        FAIL_STACK_ERROR;

    /* Read from the dataset via did1 */
    rbuf = 0;
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    /* Verify the data is correct */
    if (rbuf != 99)
        FAIL_STACK_ERROR;

    /* Close datasets */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did3) < 0)
        FAIL_STACK_ERROR;

    /* Close files */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;

    /* Case 2 */

    /* Open the file 3 times: read-write, read-only, read-write */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid3 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset 3 times with fid1, fid2, fid3 */
    if ((did1 = H5Dopen2(fid1, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid2, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did3 = H5Dopen2(fid3, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to the dataset via did1 */
    wbuf = 88;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Refresh via did2 */
    if (H5Drefresh(did2) < 0)
        FAIL_STACK_ERROR;

    /* Read from dataset via did2 */
    rbuf = 0;
    if (H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    if (rbuf != wbuf)
        FAIL_STACK_ERROR;

    /* Write to dataset via did3 */
    wbuf = 99;
    if (H5Dwrite(did3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR write */
    if (H5Fstart_swmr_write(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Read from dataset via did1 and verify data is correct */
    rbuf = 0;
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    if (rbuf != wbuf)
        FAIL_STACK_ERROR;

    /* Write to dataset via did2 */
    wbuf = 100;
    if (H5Dwrite(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Refresh dataset via did3 */
    if (H5Drefresh(did3) < 0)
        FAIL_STACK_ERROR;

    /* Read from dataset via did3 and verify data is correct */
    rbuf = 0;
    if (H5Dread(did3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    if (rbuf != wbuf)
        FAIL_STACK_ERROR;

    /* Close datasets */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;
    if (H5Dclose(did3) < 0)
        FAIL_STACK_ERROR;

    /* Close files */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;
    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;

    /* Case 3 */

    /* Open the file 3 times: read-write, read-only, read-only */
    if ((fid1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid2 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;
    if ((fid3 = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR;

    /* Open the dataset 3 times with fid1, fid2, fid3 */
    if ((did1 = H5Dopen2(fid1, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did2 = H5Dopen2(fid2, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;
    if ((did3 = H5Dopen2(fid3, "dataset", H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR;

    /* Write to the dataset via did1 */
    wbuf = 88;
    if (H5Dwrite(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Refresh dataset via did2 */
    if (H5Drefresh(did2) < 0)
        FAIL_STACK_ERROR;

    /* Read from dataset via did2 and verify data is correct */
    rbuf = 0;
    if (H5Dread(did2, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    if (rbuf != wbuf)
        FAIL_STACK_ERROR;

    /* Close dataset via did2 */
    if (H5Dclose(did2) < 0)
        FAIL_STACK_ERROR;

    /* Close file via fid2 */
    if (H5Fclose(fid2) < 0)
        FAIL_STACK_ERROR;

    /* Write to dataset via did3 */
    wbuf = 99;
    if (H5Dwrite(did3, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &wbuf) < 0)
        FAIL_STACK_ERROR;

    /* Close dataset via did3 */
    if (H5Dclose(did3) < 0)
        FAIL_STACK_ERROR;

    /* Close file via fid3 */
    if (H5Fclose(fid3) < 0)
        FAIL_STACK_ERROR;

    /* Enable SWMR writing */
    if (H5Fstart_swmr_write(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Read from dataset via did1 and verify data is correct */
    rbuf = 0;
    if (H5Dread(did1, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &rbuf) < 0)
        FAIL_STACK_ERROR;
    if (rbuf != wbuf)
        FAIL_STACK_ERROR;

    /* Close dataset via did1 */
    if (H5Dclose(did1) < 0)
        FAIL_STACK_ERROR;

    /* Close file via fid1 */
    if (H5Fclose(fid1) < 0)
        FAIL_STACK_ERROR;

    /* Close the property list */
    if (H5Pclose(fapl) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(did);
        H5Dclose(did1);
        H5Dclose(did2);
        H5Dclose(did3);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(fapl);
        H5Fclose(fid);
        H5Fclose(fid1);
        H5Fclose(fid2);
        H5Fclose(fid3);
    }
    H5E_END_TRY

    return 1;

} /* test_multiple_same() */

/****************************************************************
**
**  Tests for new public routines introduced from the SWMR project.
**
****************************************************************/
int
main(void)
{
    const char *driver_name  = NULL;            /* VFD string (from env variable) */
    int         nerrors      = 0;               /* The # of errors */
    hid_t       fapl         = H5I_INVALID_HID; /* File access property list ID */
    char       *lock_env_var = NULL;            /* file locking env var pointer */
    bool        use_file_locking;               /* read from env var */
    bool        file_locking_enabled = false;   /* Checks if the file system supports locks */

    /* Testing setup */
    h5_reset();

    /* Skip this test if SWMR I/O is not supported for the VFD specified
     * by the environment variable.
     */
    driver_name = h5_get_test_driver_name();
    if (!H5FD__supports_swmr_test(driver_name)) {
        printf("This VFD does not support SWMR I/O\n");
        return EXIT_SUCCESS;
    }

    /* Check the environment variable that determines if we care
     * about file locking. File locking should be used unless explicitly
     * disabled.
     */
    lock_env_var = getenv(HDF5_USE_FILE_LOCKING);
    if (lock_env_var && !strcmp(lock_env_var, "FALSE"))
        use_file_locking = false;
    else
        use_file_locking = true;

    /* Check if file locking is enabled on this file system */
    if (use_file_locking)
        if (h5_check_if_file_locking_enabled(&file_locking_enabled) < 0) {
            printf("Error when determining if file locks are enabled\n");
            return EXIT_FAILURE;
        }

    /* Get file access property list */
    fapl = h5_fileaccess();

#ifdef OUT
    nerrors += test_bug_refresh(fapl);
#endif
    nerrors += test_refresh_concur(fapl, true);
    nerrors += test_refresh_concur(fapl, false);
    nerrors += test_multiple_same(fapl, true);
    nerrors += test_multiple_same(fapl, false);

    /* Tests on H5Pget/set_metadata_read_attempts() and H5Fget_metadata_read_retry_info() */
    nerrors += test_metadata_read_attempts(fapl);
    nerrors += test_metadata_read_retry_info(fapl);

    /* Tests on H5Fstart_swmr_write() */
    /*
     * Modify the following routines to test for files:
     *   H5Fcreate(write, latest format) or  H5Fcreate(SWMR write, non-latest-format)
     *   --both result in v3 superblock and latest version support
     */
    nerrors += test_start_swmr_write(fapl, true);
    nerrors += test_start_swmr_write(fapl, false);
    nerrors += test_err_start_swmr_write(fapl, true);
    nerrors += test_err_start_swmr_write(fapl, false);
    nerrors += test_start_swmr_write_concur(fapl, true);
    nerrors += test_start_swmr_write_concur(fapl, false);
    nerrors += test_start_swmr_write_stress_ohdr(fapl);
    nerrors += test_start_swmr_write_persist_dapl(fapl);

    /* Tests for H5Pget/set_object_flush_cb() */
    nerrors += test_object_flush_cb(fapl);

    /* Tests on H5Pget/set_append_flush() */
    nerrors += test_append_flush_generic();
    nerrors += test_append_flush_dataset_chunked(fapl);
    nerrors += test_append_flush_dataset_fixed(fapl);
    nerrors += test_append_flush_dataset_multiple(fapl);

    if (use_file_locking && file_locking_enabled) {
        /*
         * Tests for:
         *   file open flags--single process access
         *   file open flags--concurrent access
         */
        nerrors += test_file_lock_same(fapl);
        nerrors += test_file_lock_concur(fapl);
        /*
         * Tests for:
         *   file open flags+SWMR flags--single process access
         *   file open flags+SWMR flags--concurrent access
         *
         * Modify the following 2 routines to test for files:
         *   H5Fcreate(write, latest format) or  H5Fcreate(SWMR write, non-latest-format)
         *   --both result in v3 superblock and latest version support
         */
        nerrors += test_file_lock_swmr_same(fapl);
        nerrors += test_file_lock_swmr_concur(fapl);
    }

    /* Tests SWMR VFD compatibility flag.
     * Only needs to run when the VFD is the default (sec2).
     */
    if (NULL == driver_name || !strcmp(driver_name, "") || !strcmp(driver_name, H5_DEFAULT_VFD_NAME))
        nerrors += test_swmr_vfd_flag();

    /* Test multiple opens via different locking flags */
    if (use_file_locking && file_locking_enabled)
        nerrors += test_different_lock_flags(fapl);

    /* These tests change the HDF5_USE_FILE_LOCKING environment variable
     * so they should be run last.
     */
    if (use_file_locking && file_locking_enabled) {
        nerrors += test_file_locking(fapl, true, true);
        nerrors += test_file_locking(fapl, true, false);
        nerrors += test_file_locking(fapl, false, true);
        nerrors += test_file_locking(fapl, false, false);
    }

    if (nerrors)
        goto error;

    printf("All tests passed.\n");

    h5_cleanup(FILENAME, fapl);

    return EXIT_SUCCESS;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d SWMR TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return EXIT_FAILURE;

} /* end main() */
