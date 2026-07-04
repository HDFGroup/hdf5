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
#include "ttsafe.h"

#ifdef H5_HAVE_THREADSAFE_API

#define ERR_CLS_NAME        "Custom error class"
#define ERR_CLS_LIB_NAME    "example_lib"
#define ERR_CLS_LIB_VERSION "0.1"

#define ERR_MAJOR_MSG "Okay, Houston, we've had a problem here"
#define ERR_MINOR_MSG "Oops!"

H5TS_THREAD_RETURN_TYPE generate_hdf5_error(void *arg);
H5TS_THREAD_RETURN_TYPE generate_user_error(void *arg);

hid_t err_cls_id = H5I_INVALID_HID;

/* Helper routine to generate an HDF5 library error */
H5TS_THREAD_RETURN_TYPE
generate_hdf5_error(void H5_ATTR_UNUSED *arg)
{
    H5TS_thread_ret_t ret_value = 0;
    ssize_t           nobjs     = 0;

    H5E_BEGIN_TRY
    {
        nobjs = H5Fget_obj_count(H5I_INVALID_HID, H5F_OBJ_ALL);
    }
    H5E_END_TRY

    /* Expect call to fail */
    VERIFY(nobjs, FAIL, "H5Fget_obj_count");

    return ret_value;
}

/* Helper routine to generate a user-defined error */
H5TS_THREAD_RETURN_TYPE
generate_user_error(void H5_ATTR_UNUSED *arg)
{
    H5TS_thread_ret_t ret_value = 0;
    hid_t             major     = H5I_INVALID_HID;
    hid_t             minor     = H5I_INVALID_HID;
    herr_t            status    = FAIL;

    err_cls_id = H5Eregister_class(ERR_CLS_NAME, ERR_CLS_LIB_NAME, ERR_CLS_LIB_VERSION);
    CHECK(err_cls_id, H5I_INVALID_HID, "H5Eregister_class");

    major = H5Ecreate_msg(err_cls_id, H5E_MAJOR, ERR_MAJOR_MSG);
    CHECK(major, H5I_INVALID_HID, "H5Ecreate_msg");

    minor = H5Ecreate_msg(err_cls_id, H5E_MINOR, ERR_MINOR_MSG);
    CHECK(minor, H5I_INVALID_HID, "H5Ecreate_msg");

    status = H5Epush2(H5E_DEFAULT, __FILE__, __func__, __LINE__, err_cls_id, major, minor, "Hello, error\n");
    CHECK(status, FAIL, "H5Epush2");

    return ret_value;
}

/*
**********************************************************************
* tts_error_stacks
*
* Test that error stacks with user-defined error classes and messages
* in secondary threads are properly cleaned up at library shutdown time.
**********************************************************************
*/
void
tts_error_stacks(void H5_ATTR_UNUSED *params)
{
    H5TS_thread_t threads[2];
    herr_t        status = FAIL;

    /* Open library */
    H5open();

    status = H5TS_thread_create(&threads[0], generate_hdf5_error, NULL);
    CHECK(status, FAIL, "H5TS_thread_create");

    status = H5TS_thread_join(threads[0], NULL);
    CHECK(status, FAIL, "H5TS_thread_join");

    status = H5TS_thread_create(&threads[1], generate_user_error, NULL);
    CHECK(status, FAIL, "H5TS_thread_create");

    status = H5TS_thread_join(threads[1], NULL);
    CHECK(status, FAIL, "H5TS_thread_join");

    if (err_cls_id <= 0) {
        TestErrPrintf("Failed to set up user error\n");
        return;
    }

    status = H5Eunregister_class(err_cls_id);
    CHECK(status, FAIL, "H5Eunregister_class");

    /* Close library */
    H5close();
}

#endif
