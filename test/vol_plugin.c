/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Purpose:     Tests basic VOL plugin operations (registration, etc.).
 *              Uses the null VOL connector (built with the testing code)
 *              which is loaded as a dynamic plugin.
 *
 * TO DO:       Adapt the null VOL connector to do something interesting with
 *              the property list.
 */

#include "h5test.h"

#include "null_vol_connector.h"


/*-------------------------------------------------------------------------
 * Function:    test_registration_by_value()
 *
 * Purpose:     Tests if we can load, register, and close a VOL
 *              connector by value.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_registration_by_value(void)
{
    htri_t  is_registered   = FAIL;
    hid_t   vol_id          = H5I_INVALID_HID;

    TESTING("VOL registration by value");

    /* The null VOL connector should not be registered at the start of the test */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    /* Register the connector by value */
    if((vol_id = H5VLregister_connector_by_value(NULL_VOL_CONNECTOR_VALUE, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* The connector should be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(FALSE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector was not registered");

    /* Unregister the connector */
    if(H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    /* The connector should not be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5VLunregister_connector(vol_id);
    } H5E_END_TRY;
    return FAIL;

} /* end test_registration_by_value() */


/*-------------------------------------------------------------------------
 * Function:    test_registration_by_name()
 *
 * Purpose:     Tests if we can load, register, and close a VOL
 *              connector by name.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_registration_by_name(void)
{
    htri_t  is_registered   = FAIL;
    hid_t   vol_id          = H5I_INVALID_HID;

    TESTING("VOL registration by name");

    /* The null VOL connector should not be registered at the start of the test */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    /* Register the connector by name */
    if((vol_id = H5VLregister_connector_by_name(NULL_VOL_CONNECTOR_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* The connector should be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(FALSE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector was not registered");

    /* Unregister the connector */
    if(H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    /* The connector should not be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5VLunregister_connector(vol_id);
    } H5E_END_TRY;
    return FAIL;

} /* end test_registration_by_name() */


/*-------------------------------------------------------------------------
 * Function:    test_multiple_registration()
 *
 * Purpose:     Tests if we can register a VOL connector multiple times.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
#define N_REGISTRATIONS 10
static herr_t
test_multiple_registration(void)
{
    htri_t  is_registered   = FAIL;
    hid_t   vol_ids[N_REGISTRATIONS];
    int     i;

    TESTING("registering a VOL connector multiple times");

    /* The null VOL connector should not be registered at the start of the test */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    /* Register the connector multiple times */
    for(i = 0; i < N_REGISTRATIONS; i++) {
        if((vol_ids[i] = H5VLregister_connector_by_name(NULL_VOL_CONNECTOR_NAME, H5P_DEFAULT)) < 0)
            TEST_ERROR;
    }

    /* The connector should be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(FALSE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector was not registered");

    /* Unregister the connector */
    for(i = 0; i < N_REGISTRATIONS; i++) {
        if(H5VLunregister_connector(vol_ids[i]) < 0)
            TEST_ERROR;
        /* Also test close on some of the IDs. This call currently works
         * identically to unregister.
         */
        i++;
        if(H5VLclose(vol_ids[i]) < 0)
            TEST_ERROR;
    }

    /* The connector should not be registered now */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        for(i = 0; i < N_REGISTRATIONS; i++)
            H5VLunregister_connector(vol_ids[i]);
    } H5E_END_TRY;
    return FAIL;

} /* end test_multiple_registration() */


/*-------------------------------------------------------------------------
 * Function:    test_getters()
 *
 * Purpose:     Tests H5VL getters
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_getters(void)
{
    htri_t  is_registered   = FAIL;
    hid_t   vol_id          = H5I_INVALID_HID;
    hid_t   vol_id_out      = H5I_INVALID_HID;

    TESTING("VOL getters");

    /* The null VOL connector should not be registered at the start of the test */
    if((is_registered = H5VLis_connector_registered(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(TRUE == is_registered)
        FAIL_PUTS_ERROR("NULL VOL connector is inappropriately registered");

    /* Register the connector by name */
    if((vol_id = H5VLregister_connector_by_name(NULL_VOL_CONNECTOR_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Get the connector's ID */
    if((vol_id_out = H5VLget_connector_id_by_name(NULL_VOL_CONNECTOR_NAME)) < 0)
        TEST_ERROR;
    if(vol_id != vol_id_out)
        FAIL_PUTS_ERROR("VOL connector IDs don't match");

    /* Unregister the connector */
    if(H5VLunregister_connector(vol_id) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5VLunregister_connector(vol_id);
    } H5E_END_TRY;
    return FAIL;

} /* end test_getters() */


/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests VOL connector plugin operations
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

    HDputs("Testing VOL connector plugin functionality.");

    nerrors += test_registration_by_name() < 0          ? 1 : 0;
    nerrors += test_registration_by_value() < 0         ? 1 : 0;
    nerrors += test_multiple_registration() < 0         ? 1 : 0;
    nerrors += test_getters() < 0                       ? 1 : 0;

    if(nerrors) {
        HDprintf("***** %d VOL connector plugin TEST%s FAILED! *****\n",
            nerrors, nerrors > 1 ? "S" : "");
        HDexit(EXIT_FAILURE);
    }

    HDputs("All VOL connector plugin tests passed.");

    HDexit(EXIT_SUCCESS);

} /* end main() */

