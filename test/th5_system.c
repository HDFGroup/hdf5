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
    H5E_END_TRY
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

    test_h5_strndup();
}

void
cleanup_h5_system(void)
{
    /* Nothing to cleanup yet */
}
