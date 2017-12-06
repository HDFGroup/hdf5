/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Purpose:	Tests the virtual object layer (H5VL)
 *
 *          This is a minimal test to ensure VOL usage (setting a VOL, etc.)
 *          works as expected. Actual VOL functionality is tested using
 *          other mechanisms.
 */

#include "h5test.h"
#include "H5VLnative.h"


#define FAKE_VOL_NAME   "fake"

/* A VOL class struct that describes a VOL class with no
 * functionality.
 */
static const H5VL_class_t fake_vol_g = {
    0,                                              /* version      */
    (H5VL_class_value_t)999,                        /* value        */
    FAKE_VOL_NAME,                                  /* name         */
    NULL,                                           /* initialize   */
    NULL,                                           /* terminate    */
    (size_t)0,                                      /* fapl size    */
    NULL,                                           /* fapl copy    */
    NULL,                                           /* fapl free    */
    {   /* attribute_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* read         */
        NULL,                                       /* write        */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* dataset_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* read         */
        NULL,                                       /* write        */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* datatype_cls */
        NULL,                                       /* commit       */
        NULL,                                       /* open         */
        NULL,                                       /* get_size     */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* file_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* group_cls */
        NULL,                                       /* create       */
        NULL,                                       /* open         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL,                                       /* optional     */
        NULL                                        /* close        */
    },
    {   /* link_cls */
        NULL,                                       /* create       */
        NULL,                                       /* copy         */
        NULL,                                       /* move         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL                                        /* optional     */
    },
    {   /* object_cls */
        NULL,                                       /* open         */
        NULL,                                       /* copy         */
        NULL,                                       /* get          */
        NULL,                                       /* specific     */
        NULL                                        /* optional     */
    },
    {   /* async_cls */
        NULL,                                       /* cancel       */
        NULL,                                       /* test         */
        NULL                                        /* wait         */
    },
    NULL                                            /* optional     */
};


/*-------------------------------------------------------------------------
 * Function:    test_vol_registration()
 *
 * Purpose:     Tests if we can load, register, and close a simple
 *              VOL driver.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_vol_registration(void)
{
    htri_t is_registered;
    hid_t vol_id = -1;

    TESTING("VOL registration");

    /* The test/fake VOL driver should not be registered at the start of the test */
    if ((is_registered = H5VLis_registered(FAKE_VOL_NAME)) < 0)
        FAIL_STACK_ERROR;
    if (is_registered > 0)
        FAIL_PUTS_ERROR("native VOL driver is inappropriately registered");

    /* Load a VOL interface */
    if ((vol_id = H5VLregister(&fake_vol_g)) < 0)
        FAIL_STACK_ERROR;

    /* The test/fake VOL driver should be registered now */
    if ((is_registered = H5VLis_registered(FAKE_VOL_NAME)) < 0)
        FAIL_STACK_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("native VOL driver is un-registered");

    /* Close the VOL interface */
    if (H5VLclose(vol_id) < 0)
        FAIL_STACK_ERROR;

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5VLclose(vol_id);
    } H5E_END_TRY;
    return FAIL;

} /* end test_vol_registration() */


/*-------------------------------------------------------------------------
 * Function:    test_native_vol_init()
 *
 * Purpose:     Tests if the native VOL driver gets initialized.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_native_vol_init(void)
{
    htri_t is_registered;

    TESTING("Native VOL driver initialization");

    /* The native VOL driver should always be registered */
    if ((is_registered = H5VLis_registered(H5VL_NATIVE_NAME)) < 0)
        FAIL_STACK_ERROR;
    if (0 == is_registered)
        FAIL_PUTS_ERROR("native VOL driver is un-registered");

    PASSED();
    return SUCCEED;

error:
    return FAIL;

} /* end test_native_vol_init() */


/*-------------------------------------------------------------------------
 * Function:    test_basic_vol_operation()
 *
 * Purpose:     Uses the native VOL driver to test basic VOL operations
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_vol_operation(void)
{
    hid_t fid = -1;

    ssize_t obj_count;

    TESTING("Basic VOL operations");

    if ((fid = H5Fcreate("native_vol_test", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if ((obj_count = H5Fget_obj_count(fid, H5F_OBJ_FILE)) < 0)
        TEST_ERROR;
    if ((obj_count = H5Fget_obj_count(fid, H5F_OBJ_ALL)) < 0)
        TEST_ERROR;
    if ((obj_count = H5Fget_obj_count((hid_t)H5F_OBJ_ALL, H5F_OBJ_DATASET)) < 0)
        TEST_ERROR;

    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    if ((fid = H5Fopen("native_vol_test", H5F_ACC_RDWR, H5P_DEFAULT)) < 0)
        TEST_ERROR;
    if (H5Fclose(fid) < 0)
        TEST_ERROR;

    PASSED();
    return SUCCEED;

error:
    return FAIL;

} /* end test_basic_vol_operation() */


/*-------------------------------------------------------------------------
 * Function:    test_echo_vol_operation()
 *
 * Purpose:     Uses the echo VOL driver to test basic VOL operations
 *              via the H5VL public API.
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_echo_vol_operation(void)
{
    char name[25];

    TESTING("Echo VOL operations");

    PASSED();
    return SUCCEED;

error:
    return FAIL;

} /* end test_basic_vol_operation() */




/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the virtual object layer interface (H5VL)
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

    HDputs("Testing basic Virtual Object Layer (VOL) functionality.");

    nerrors += test_vol_registration() < 0          ? 1 : 0;
    nerrors += test_native_vol_init() < 0           ? 1 : 0;
    nerrors += test_basic_vol_operation() < 0       ? 1 : 0;

    if (nerrors) {
        HDprintf("***** %d Virtual Object Layer TEST%s FAILED! *****\n",
            nerrors, nerrors > 1 ? "S" : "");
        HDexit(EXIT_FAILURE);
    }

    HDputs("All Virtual Object Layer (VOL) tests passed.");

    HDexit(EXIT_SUCCESS);

} /* end main() */

