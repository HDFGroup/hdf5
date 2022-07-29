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

#define H5_SYSTEM_TEST_PATH_MAX 4096

/***********************************************************
 *
 * Test program:  th5_system
 *
 * Testing for the routines available in H5system.c
 *
 *************************************************************/

#include "testhdf5.h"

static void
test_h5_dirname(void)
{
    herr_t ret;
    char * path    = NULL;
    char * dirname = NULL;

    MESSAGE(5, ("Testing H5_dirname\n"));

    path = HDmalloc(H5_SYSTEM_TEST_PATH_MAX);
    CHECK_PTR(path, "HDmalloc");
    if (!path)
        return;

    /* Check that H5_dirname fails for a NULL path */
    dirname = NULL;
    H5E_BEGIN_TRY
    {
        ret = H5_dirname(NULL, &dirname);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_dirname with NULL path");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_dirname fails for a NULL dirname pointer */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir%sfinaldir",
               H5_DIR_SEPS, H5_DIR_SEPS);
    H5E_BEGIN_TRY
    {
        ret = H5_dirname(path, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_dirname with NULL dirname pointer");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_dirname returns "." for an empty path string */
    *path = '\0';
    dirname = NULL;
    ret = H5_dirname(path, &dirname);
    CHECK(ret, FAIL, "H5_dirname with empty string");
    VERIFY_STR(dirname, ".", "comparing H5_dirname with empty string to \".\"");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns "." for a path that
     * doesn't contain the system file separator character
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdirname");
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, ".", "comparing H5_dirname with non-separated path to \".\"");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for the simple path containing just the system file separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with file separator path to file separator");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains a leading separator and a path
     * component
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with leading separator path to file separator");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns the "." for a path which
     * contains a path component and a trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, ".", "comparing H5_dirname with trailing separator path to \".\"");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains a leading separator, a path
     * component and a trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with leading and trailing separator path to file separator");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * "normal" pathname that has no leading separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, "topdir", "comparing H5_dirname with normal path to proper dirname");
    HDfree(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * "normal" pathname that has a leading separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS "topdir", "comparing H5_dirname with normal path to proper dirname");
    HDfree(dirname);

    HDfree(path);
}

static void
test_h5_basename(void)
{
    herr_t ret;
    char * path     = NULL;
    char * basename = NULL;

    MESSAGE(5, ("Testing H5_basename\n"));

    path = HDmalloc(H5_SYSTEM_TEST_PATH_MAX);
    CHECK_PTR(path, "HDmalloc");
    if (!path)
        return;

    /* Check that H5_basename fails for a NULL path */
    basename = NULL;
    H5E_BEGIN_TRY
    {
        ret = H5_basename(NULL, &basename);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_basename with NULL path");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_basename fails for a NULL basename pointer */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir%sfinaldir",
               H5_DIR_SEPS, H5_DIR_SEPS);
    H5E_BEGIN_TRY
    {
        ret = H5_basename(path, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_basename with NULL basename pointer");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_basename returns "." for an empty path string */
    *path = '\0';
    basename = NULL;
    ret = H5_basename(path, &basename);
    CHECK(ret, FAIL, "H5_basename with empty string");
    VERIFY_STR(basename, ".", "comparing H5_basename with empty string to \".\"");
    HDfree(basename);

    /*
     * Check that H5_basename returns the specified path for a
     * path that doesn't contain the system file separator character
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdirname");
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdirname", "comparing H5_basename with non-separated path to same path");
    HDfree(basename);

    /*
     * Check that H5_basename returns the system file separator
     * for the simple path containing just the system file separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, H5_DIR_SEPS, "comparing H5_basename with file separator path to file separator");
    HDfree(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a leading separator and a path
     * component
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir", "comparing H5_basename with leading separator path to path component");
    HDfree(basename);

    /*
     * Check that H5_basename returns an empty string for a path
     * which contains a path component and a trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "", "comparing H5_basename with trailing separator path to \"\"");
    HDfree(basename);

    /*
     * Check that H5_basename returns an empty string for
     * a path which contains a leading separator, a path
     * component and a trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "", "comparing H5_basename with leading and trailing separator path to empty string");
    HDfree(basename);

    /*
     * Check that H5_basename returns a proper basename with
     * a "normal" pathname that has no leading separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "underdir", "comparing H5_basename with normal path to proper basename");
    HDfree(basename);

    /*
     * Check that H5_basename returns a proper basename with
     * a "normal" pathname that has a leading separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "underdir", "comparing H5_basename with normal path to proper basename");
    HDfree(basename);

    HDfree(path);
}

void
test_h5_system(void)
{
    MESSAGE(5, ("Testing H5system routines\n"));

    test_h5_dirname();
    test_h5_basename();
}

void
cleanup_h5_system(void)
{
    /* Nothing to cleanup yet */
}
