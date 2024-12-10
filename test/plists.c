/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:    Test property list behavior
 */

#include "h5test.h"

/*-------------------------------------------------------------------------
 * Function:    test_set_default_plist_fail
 *
 * Purpose:     Tests that H5P setters fail when attempting to change
 *              a default property list
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set_default_plist_fail(void)
{
    hid_t  vol_id = H5I_INVALID_HID;
    herr_t ret    = FAIL;

    TESTING("Default property list unmodifiability");

    /* Attempt to modify the default generic property list */
    H5E_BEGIN_TRY
    {
        ret = H5Pset_vol(H5P_DEFAULT, H5VL_NATIVE, NULL);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_vol() succeeded on default property list");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_file_space(H5P_FILE_CREATE_DEFAULT, (H5F_file_space_type_t)0, 0);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_file_space() succeeded on default FCPL");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_layout(H5P_DATASET_CREATE_DEFAULT, H5D_CONTIGUOUS);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_layout() succeeded on default DCPL");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_efile_prefix(H5P_DATASET_ACCESS_DEFAULT, "prefix");
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_efile_prefix() succeeded on default DAPL");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_vol(H5P_FILE_ACCESS_DEFAULT, vol_id, NULL);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_vol() succeeded on default FAPL");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_preserve(H5P_DATASET_XFER_DEFAULT, true);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_preserve() succeeded on default DXPL");

    H5E_BEGIN_TRY
    {
        ret = H5Pset_local_heap_size_hint(H5P_GROUP_CREATE_DEFAULT, 0);
    }
    H5E_END_TRY

    if (ret >= 0)
        FAIL_PUTS_ERROR("H5Pset_local_heap_size_hint() succeeded on default GCPL");

    PASSED();

    return SUCCEED;
error:

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the H5P interface (H5P)
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    puts("Testing property list (H5P) functionality.");

    nerrors += test_set_default_plist_fail() < 0 ? 1 : 0;

    if (nerrors) {
        printf("***** %d Property List TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
        exit(EXIT_FAILURE);
    }

    puts("All Property List (H5P) tests passed.");

    exit(EXIT_SUCCESS);
}