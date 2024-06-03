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

/* Purpose:     This is a small program that checks if the HDF5_DRIVER or
 *              HDF5_TEST_DRIVER environment variable is set to a value that
 *              supports SWMR.
 *
 *              It is intended for use in shell scripts.
 */

#include "h5test.h"

/* This file needs to access the file driver testing code */
#define H5FD_FRIEND /*suppress error about including H5FDpkg            */
#define H5FD_TESTING
#include "H5FDpkg.h" /* File drivers                                     */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Inspects the HDF5_DRIVER and HDF5_TEST_DRIVER environment
 *              variables, which determines the VFD that the test harness
 *              will use with the majority of the tests.
 *
 * Return:      VFD supports SWMR:          EXIT_SUCCESS
 *
 *              VFD does not support SWMR
 *              or failure:                 EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char *driver = NULL;

    driver = getenv(HDF5_DRIVER);
    if (!driver)
        driver = getenv("HDF5_TEST_DRIVER");

    if (H5FD__supports_swmr_test(driver))
        return EXIT_SUCCESS;
    else
        return EXIT_FAILURE;

} /* end main() */
