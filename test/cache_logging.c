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

/* Purpose: Tests the metadata cache logging framework */

#include "h5test.h"

#define LOG_LOCATION "cache_logging.out"

const char *FILENAME[] = {"cache_logging", NULL};

#define N_GROUPS 100

/*-------------------------------------------------------------------------
 * Function:    test_logging_api
 *
 * Purpose:     Tests the API calls that affect mdc logging
 *
 * Return:      Success:        0
 *              Failure:        -1
 *-------------------------------------------------------------------------
 */
static herr_t
test_logging_api(void)
{
    hid_t   fapl = -1;
    hbool_t is_enabled;
    hbool_t is_enabled_out;
    hbool_t start_on_access;
    hbool_t start_on_access_out;
    char *  location = NULL;
    size_t  size;

    hid_t   fid = -1;
    hid_t   gid = -1;
    hbool_t is_currently_logging;
    char    group_name[12];
    char    filename[1024];
    int     i;

    TESTING("metadata cache log api calls");

    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Set up metadata cache logging */
    is_enabled      = TRUE;
    start_on_access = FALSE;
    if (H5Pset_mdc_log_options(fapl, is_enabled, LOG_LOCATION, start_on_access) < 0)
        TEST_ERROR;

    /* Check to make sure that the property list getter returns the correct
     * location string buffer size;
     */
    is_enabled_out      = FALSE;
    start_on_access_out = TRUE;
    location            = NULL;
    size                = 999;
    if (H5Pget_mdc_log_options(fapl, &is_enabled_out, location, &size, &start_on_access_out) < 0)
        TEST_ERROR;
    if (size != HDstrlen(LOG_LOCATION) + 1)
        TEST_ERROR;

    /* Check to make sure that the property list getter works */
    if (NULL == (location = (char *)HDcalloc(size, sizeof(char))))
        TEST_ERROR;
    if (H5Pget_mdc_log_options(fapl, &is_enabled_out, location, &size, &start_on_access_out) < 0)
        TEST_ERROR;
    if ((is_enabled != is_enabled_out) || (start_on_access != start_on_access_out) ||
        HDstrcmp(LOG_LOCATION, location) != 0)
        TEST_ERROR;

    /* Create a file */
    if (H5Pset_libver_bounds(fapl, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0)
        TEST_ERROR;
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Check to see if the logging flags were set correctly */
    is_enabled           = FALSE;
    is_currently_logging = TRUE;
    if ((H5Fget_mdc_logging_status(fid, &is_enabled, &is_currently_logging) < 0) || (is_enabled != TRUE) ||
        (is_currently_logging != FALSE))
        TEST_ERROR;

    /* Turn on logging and check flags */
    if (H5Fstart_mdc_logging(fid) < 0)
        TEST_ERROR;
    is_enabled           = FALSE;
    is_currently_logging = FALSE;
    if ((H5Fget_mdc_logging_status(fid, &is_enabled, &is_currently_logging) < 0) || (is_enabled != TRUE) ||
        (is_currently_logging != TRUE))
        TEST_ERROR;

    /* Perform some manipulations */
    for (i = 0; i < N_GROUPS; i++) {
        HDmemset(group_name, 0, sizeof(group_name));
        HDsnprintf(group_name, sizeof(group_name), "%d", i);
        if ((gid = H5Gcreate2(fid, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            TEST_ERROR;
        if (H5Gclose(gid) < 0)
            TEST_ERROR;
    }

    /* Turn off logging and check flags */
    if (H5Fstop_mdc_logging(fid) < 0)
        TEST_ERROR;
    is_enabled           = FALSE;
    is_currently_logging = TRUE;
    if ((H5Fget_mdc_logging_status(fid, &is_enabled, &is_currently_logging) < 0) || (is_enabled != TRUE) ||
        (is_currently_logging != FALSE))
        TEST_ERROR;

    /* Clean up */
    HDfree(location);
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    HDremove(LOG_LOCATION);
    h5_clean_files(FILENAME, fapl);

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl);
    }
    H5E_END_TRY

    return 1;
} /* test_logging_api() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Test basic cache logging operations
 *
 * Return:      Success:        zero
 *              Failure:        non-zero
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    /* Reset library */
    h5_reset();

    HDprintf("Testing basic metadata cache logging functionality.\n");

    nerrors += test_logging_api();

    if (nerrors) {
        HDprintf("***** %d Metadata cache logging TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        HDexit(EXIT_FAILURE);
    }

    HDprintf("All Metadata Cache Logging tests passed.\n");

    HDexit(EXIT_SUCCESS);
}
