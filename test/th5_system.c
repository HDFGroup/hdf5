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

#include "H5MMprivate.h"

static void
test_h5_dirname(void)
{
    herr_t ret;
    char  *path    = NULL;
    char  *dirname = NULL;

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
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir%sfinaldir", H5_DIR_SEPS, H5_DIR_SEPS);
    H5E_BEGIN_TRY
    {
        ret = H5_dirname(path, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_dirname with NULL dirname pointer");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_dirname returns "." for an empty path string */
    *path   = '\0';
    dirname = NULL;
    ret     = H5_dirname(path, &dirname);
    CHECK(ret, FAIL, "H5_dirname with empty string");
    VERIFY_STR(dirname, ".", "comparing H5_dirname with empty string to \".\"");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns "." for a path that
     * doesn't contain the system file separator character
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdirname");
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, ".", "comparing H5_dirname with non-separated path to \".\"");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for the simple path containing just the system file separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with file separator path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains a leading separator and a path
     * component
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with leading separator path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains several leading separators and
     * a path component
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS, "comparing H5_dirname with leading separators path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the "." for a path which
     * contains a path component and a trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, ".", "comparing H5_dirname with trailing separator path to \".\"");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the "." for a path which
     * contains a path component and several trailing separators
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, ".", "comparing H5_dirname with trailing separators path to \".\"");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains a leading separator, a path
     * component and a trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS,
               "comparing H5_dirname with leading and trailing separator path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains several leading separators, a
     * path component and a trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(
        dirname, H5_DIR_SEPS,
        "comparing H5_dirname with leading separators and a trailing separator path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains a leading separator, a path
     * component and several trailing separators
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS,
               "comparing H5_dirname with leading separator and trailing separators path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns the system file separator
     * for a path which contains several leading separators, a
     * path component and several trailing separators
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS,
               "comparing H5_dirname with leading and trailing separators path to file separator");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * "normal" pathname that has no leading separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir", H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, "topdir", "comparing H5_dirname with normal path to proper dirname");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * "normal" pathname that has a leading separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS "topdir", "comparing H5_dirname with normal path to proper dirname");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * "normal" pathname that has a leading and trailing separator
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS "topdir", "comparing H5_dirname with normal path to proper dirname");
    H5MM_free(dirname);

    /*
     * Check that H5_dirname returns a proper dirname with a
     * contrived pathname
     */
    dirname = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir%s%s%sfinaldir%s", H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_dirname(path, &dirname);
    VERIFY_STR(dirname, H5_DIR_SEPS "topdir" H5_DIR_SEPS "underdir",
               "comparing H5_dirname with contrived path to proper dirname");
    H5MM_free(dirname);

    HDfree(path);
}

static void
test_h5_basename(void)
{
    herr_t ret;
    char  *path     = NULL;
    char  *basename = NULL;

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
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir%sfinaldir", H5_DIR_SEPS, H5_DIR_SEPS);
    H5E_BEGIN_TRY
    {
        ret = H5_basename(path, NULL);
    }
    H5E_END_TRY;
    VERIFY(ret, FAIL, "H5_basename with NULL basename pointer");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_basename returns "." for an empty path string */
    *path    = '\0';
    basename = NULL;
    ret      = H5_basename(path, &basename);
    CHECK(ret, FAIL, "H5_basename with empty string");
    VERIFY_STR(basename, ".", "comparing H5_basename with empty string to \".\"");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the specified path for a
     * path that doesn't contain the system file separator character
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdirname");
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdirname", "comparing H5_basename with non-separated path to same path");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the system file separator
     * for the simple path containing just the system file separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, H5_DIR_SEPS, "comparing H5_basename with file separator path to file separator");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a leading separator and a path
     * component
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with leading separator path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains several leading separators and a path
     * component
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with leading separators path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a path component and a trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with trailing separator path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a path component and several trailing
     * separators
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "testdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with trailing separators path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a leading separator, a path component
     * and a trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with leading and trailing separator path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains several leading separators, a path
     * component and a trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(
        basename, "testdir",
        "comparing H5_basename with leading separators and a trailing separator path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains a leading separator, a path component
     * and several trailing separators
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stestdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(
        basename, "testdir",
        "comparing H5_basename with leading separator and trailing separators path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns the proper basename for a
     * path which contains several leading separators, a path
     * component and several trailing separators
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%s%s%s%stestdir%s%s%s%s", H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "testdir",
               "comparing H5_basename with leading and trailing separators path to filename component");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns a proper basename with
     * a "normal" pathname that has no leading separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "topdir%sunderdir", H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "underdir", "comparing H5_basename with normal path to proper basename");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns a proper basename with
     * a "normal" pathname that has a leading separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir", H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "underdir", "comparing H5_basename with normal path to proper basename");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns a proper basename with
     * a "normal" pathname that has a leading and trailing separator
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir%s", H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "underdir", "comparing H5_basename with normal path to proper basename");
    H5MM_free(basename);

    /*
     * Check that H5_basename returns a proper basename with a
     * contrived pathname
     */
    basename = NULL;
    HDsnprintf(path, H5_SYSTEM_TEST_PATH_MAX, "%stopdir%sunderdir%s%s%sfinaldir%s", H5_DIR_SEPS, H5_DIR_SEPS,
               H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS, H5_DIR_SEPS);
    ret = H5_basename(path, &basename);
    VERIFY_STR(basename, "finaldir", "comparing H5_basename with contrived path to proper basename");
    H5MM_free(basename);

    HDfree(path);
}

static void
test_h5_strcasestr(void)
{
    const char *const haystack = "My test string";
    char             *str      = NULL;

    MESSAGE(5, ("Testing H5_strcasestr\n"));

    /* check that H5_strcasestr returns target in empty search */
    str = H5_strcasestr(haystack, "");
    CHECK_PTR_EQ(str, haystack, "H5_strcasestr search for empty");

    /* Check that H5_strcasestr find a string of same case */
    str = H5_strcasestr(haystack, "string");
    CHECK_PTR_EQ(str, &(haystack[8]), "H5_strcasestr search same case");

    /* Check that H5_strcasestr find a string of different case */
    str = H5_strcasestr(haystack, "sTrInG");
    CHECK_PTR_EQ(str, &(haystack[8]), "H5_strcasestr search different case");

    /* Check that H5_strcasestr returns NULL if no match is found */
    str = H5_strcasestr(haystack, "nomatch");
    CHECK_PTR_NULL(str, "H5_strcasestr search with no match");
}

static void
test_h5_strndup(void)
{
#ifdef H5_HAVE_WIN32_API
    const char *const teststr = "myteststring";
    char             *str     = NULL;

    MESSAGE(5, ("Testing H5_strndup\n"));

    /* Check that H5_strndup fails for a NULL string pointer */
    H5E_BEGIN_TRY
    {
        str = H5_strndup(NULL, 20);
    }
    H5E_END_TRY;
    CHECK_PTR_NULL(str, "H5_strndup with NULL string pointer");
    H5Eclear2(H5E_DEFAULT);

    /* Check that H5_strndup correctly performs a 0-byte copy */
    str = H5_strndup(teststr, 0);
    CHECK_PTR(str, "H5_strndup for 0-byte copy");
    if (str)
        VERIFY_STR(str, "", "comparing H5_strndup for 0-byte copy to empty string");
    str = H5MM_xfree(str);

    /* Check that H5_strndup correctly performs partial copies */
    str = H5_strndup(teststr, 6);
    CHECK_PTR(str, "H5_strndup for partial copy");
    if (str)
        VERIFY_STR(str, "mytest", "comparing H5_strndup for partial copy to partial string");
    str = H5MM_xfree(str);

    /* Check that H5_strndup correctly performs identical copies */
    str = H5_strndup(teststr, HDstrlen(teststr));
    CHECK_PTR(str, "H5_strndup for identical copy");
    if (str)
        VERIFY_STR(str, teststr, "comparing H5_strndup for identical copy to original string");
    str = H5MM_xfree(str);

    /*
     * Check that H5_strndup correctly performs copies when
     * `n` is greater than the original string
     */
    str = H5_strndup(teststr, HDstrlen(teststr) + 2);
    CHECK_PTR(str, "H5_strndup for larger 'n'");
    if (str)
        VERIFY_STR(str, teststr, "comparing H5_strndup with larger 'n' value to original string");
    str = H5MM_xfree(str);
#endif /* H5_HAVE_WIN32_API */
}

void
test_h5_system(void)
{
    MESSAGE(5, ("Testing H5system routines\n"));

    test_h5_dirname();
    test_h5_basename();
    test_h5_strcasestr();
    test_h5_strndup();
}

void
cleanup_h5_system(void)
{
    /* Nothing to cleanup yet */
}
