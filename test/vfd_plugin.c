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

/*
 * Purpose:     Tests basic VFD plugin operations. Uses a basic testing VFD
 *              which is loaded as a dynamic plugin.
 */

#include "h5test.h"

#include "null_vfd_plugin.h"

/*-------------------------------------------------------------------------
 * Function:    test_set_by_name()
 *
 * Purpose:     Tests if we can load and register a VFD plugin by name.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set_by_name(void)
{
    htri_t is_registered = FAIL;
    hid_t  driver_id     = H5I_INVALID_HID;
    hid_t  fapl_id       = H5I_INVALID_HID;

    TESTING("VFD plugin registration by name");

    /* The null VFD should not be registered at the start of the test */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    /* Register the null VFD by name */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_driver_by_name(fapl_id, NULL_VFD_NAME, NULL) < 0)
        TEST_ERROR;

    /* The null VFD should be registered now */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (false == is_registered)
        FAIL_PUTS_ERROR("NULL VFD was not registered");

    /* Unregister the null VFD */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;
    if (H5FDunregister(driver_id) < 0)
        TEST_ERROR;

    /* Close FAPL, which holds last reference to null VFD */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* The null VFD should not be registered now */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_set_by_value()
 *
 * Purpose:     Tests if we can load and register a VFD plugin by value
 *              (ID).
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set_by_value(void)
{
    htri_t is_registered = FAIL;
    hid_t  driver_id     = H5I_INVALID_HID;
    hid_t  fapl_id       = H5I_INVALID_HID;

    TESTING("VFD plugin registration by value (ID)");

    /* The null VFD should not be registered at the start of the test */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    /* Register the null VFD by value */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    if (H5Pset_driver_by_value(fapl_id, NULL_VFD_VALUE, NULL) < 0)
        TEST_ERROR;

    /* The null VFD should be registered now */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (false == is_registered)
        FAIL_PUTS_ERROR("NULL VFD was not registered");

    /* Unregister the null VFD */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;
    if (H5FDunregister(driver_id) < 0)
        TEST_ERROR;

    /* Close FAPL, which holds last reference to null VFD */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* The null VFD should not be registered now */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_set_multi()
 *
 * Purpose:     Tests if we can register a VFD plugin multiple times.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
#define N_REGISTRATIONS 10
static herr_t
test_set_multi(void)
{
    htri_t is_registered = FAIL;
    hid_t  driver_id     = H5I_INVALID_HID;
    hid_t  fapl_id       = H5I_INVALID_HID;
    int    i;

    TESTING("registering a VFD plugin multiple times");

    /* The null VFD should not be registered at the start of the test */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    /* Register the VFD multiple times */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    for (i = 0; i < N_REGISTRATIONS; i++) {
        if (H5Pset_driver_by_name(fapl_id, NULL_VFD_NAME, NULL) < 0)
            TEST_ERROR;
    }

    /* The null VFD should be registered now */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (false == is_registered)
        FAIL_PUTS_ERROR("NULL VFD was not registered");

    /* Unregister the null VFD */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;
    for (i = 0; i < N_REGISTRATIONS; i++) {
        if (H5FDunregister(driver_id) < 0)
            TEST_ERROR;
    }

    /* Close FAPL, which holds last reference to null VFD */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* The null VFD should not be registered now */
    if ((is_registered = H5FDis_driver_registered_by_name(NULL_VFD_NAME)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    /* Repeat testing with the _by_value routines */

    /* The null VFD should not be registered at the start of the test */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    /* Register the VFD multiple times */
    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;
    for (i = 0; i < N_REGISTRATIONS; i++) {
        if (H5Pset_driver_by_value(fapl_id, NULL_VFD_VALUE, NULL) < 0)
            TEST_ERROR;
    }

    /* The null VFD should be registered now */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (false == is_registered)
        FAIL_PUTS_ERROR("NULL VFD was not registered");

    /* Unregister the null VFD */
    if ((driver_id = H5Pget_driver(fapl_id)) < 0)
        TEST_ERROR;
    for (i = 0; i < N_REGISTRATIONS; i++) {
        if (H5FDunregister(driver_id) < 0)
            TEST_ERROR;
    }

    /* Close FAPL, which holds last reference to null VFD */
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    /* The null VFD should not be registered now */
    if ((is_registered = H5FDis_driver_registered_by_value(NULL_VFD_VALUE)) < 0)
        TEST_ERROR;
    if (true == is_registered)
        FAIL_PUTS_ERROR("NULL VFD is inappropriately registered");

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
}
#undef N_REGISTRATIONS

/*-------------------------------------------------------------------------
 * Function:    test_get_config_str()
 *
 * Purpose:     Tests if we can retrieve a configuration string set for a
 *              VFL driver.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_config_str(void)
{
    const char *const config_str     = "{name: sec2}";
    ssize_t           config_str_len = 0;
    hid_t             fapl_id        = H5I_INVALID_HID;
    char              config_str_buf[128];

    TESTING("Retrieval of VFD configuration string");

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR;

    /* Try to retrieve length of default configuration string - should be 0 */
    memset(config_str_buf, 0, 128);

    if ((config_str_len = H5Pget_driver_config_str(fapl_id, config_str_buf, 128)) < 0)
        TEST_ERROR;
    if (0 != config_str_len)
        TEST_ERROR;
    if (strlen(config_str_buf) > 0)
        TEST_ERROR;

    /* Set a new configuration string on the FAPL and retrieve it */
    if (H5Pset_driver_by_name(fapl_id, H5_DEFAULT_VFD_NAME, config_str) < 0)
        TEST_ERROR;
    if ((config_str_len = H5Pget_driver_config_str(fapl_id, config_str_buf, 128)) < 0)
        TEST_ERROR;
    if (strlen(config_str) != config_str_len)
        TEST_ERROR;
    if (strncmp(config_str_buf, config_str, 128))
        TEST_ERROR;

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
    }
    H5E_END_TRY

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_env_var
 *
 * Purpose:     Tests loading of NULL VFD plugin with HDF5_DRIVER
 *              environment variable and setting of VFD configuration
 *              string with HDF5_DRIVER_CONFIG environment variable
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
static int
test_env_var(void)
{
    const char *const config_str     = "{name: null}";
    ssize_t           config_str_len = 0;
    htri_t            driver_is_registered;
    char              config_str_buf[128];

    TESTING("Loading of VFD plugin with HDF5_DRIVER environment variable");

    /* Try to retrieve length of default configuration string - should be 0 */
    memset(config_str_buf, 0, 128);

    if ((config_str_len = H5Pget_driver_config_str(H5P_FILE_ACCESS_DEFAULT, config_str_buf, 128)) < 0)
        TEST_ERROR;
    if (0 != config_str_len)
        TEST_ERROR;
    if (strlen(config_str_buf) > 0)
        TEST_ERROR;

    /* Set default driver and driver configuration using environment variables */
    if (HDsetenv(HDF5_DRIVER, "null_vfd_plugin", 1) < 0)
        TEST_ERROR;
    if (HDsetenv(HDF5_DRIVER_CONFIG, config_str, 1) < 0)
        TEST_ERROR;

    /* Close and re-open HDF5 to have it parse the environment variables */
    if (H5close() < 0)
        TEST_ERROR;
    if (H5open() < 0)
        TEST_ERROR;

    /* Check driver */
    if ((driver_is_registered = H5FDis_driver_registered_by_name("null_vfd_plugin")) < 0)
        TEST_ERROR;
    if (!driver_is_registered)
        TEST_ERROR;
    if (H5Pget_driver(H5P_FILE_ACCESS_DEFAULT) == H5_DEFAULT_VFD)
        TEST_ERROR;

    /* Check driver configuration string */
    memset(config_str_buf, 0, 128);
    if ((config_str_len = H5Pget_driver_config_str(H5P_FILE_ACCESS_DEFAULT, config_str_buf, 128)) < 0)
        TEST_ERROR;
    if (strlen(config_str) != config_str_len)
        TEST_ERROR;
    if (strncmp(config_str_buf, config_str, 128))
        TEST_ERROR;

    /* Unset environment variables */
    if (HDsetenv(HDF5_DRIVER, "", 1) < 0)
        TEST_ERROR;
    if (HDsetenv(HDF5_DRIVER_CONFIG, "", 1) < 0)
        TEST_ERROR;

    PASSED();

    return SUCCEED;

error:
    HDsetenv(HDF5_DRIVER, "", 1);
    HDsetenv(HDF5_DRIVER_CONFIG, "", 1);

    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests VFD plugin operations
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    h5_reset();

    puts("Testing VFD plugin functionality.");

    nerrors += (test_set_by_name() < 0) ? 1 : 0;
    nerrors += (test_set_by_value() < 0) ? 1 : 0;
    nerrors += (test_set_multi() < 0) ? 1 : 0;
    nerrors += (test_get_config_str() < 0) ? 1 : 0;
    nerrors += (test_env_var() < 0) ? 1 : 0;

    if (nerrors) {
        printf("***** %d VFD plugin TEST%s FAILED! *****\n", nerrors, nerrors > 1 ? "S" : "");
        exit(EXIT_FAILURE);
    }

    puts("All VFD plugin tests passed.");

    exit(EXIT_SUCCESS);
}
