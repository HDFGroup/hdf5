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
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    http://hdfgroup.org/HDF5/doc/
 *
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <jni.h>
#include <stdlib.h>
#include "hdf5.h"
#include "H5VLprivate.h"
#include "h5jni.h"
#include "h5jni_test.h"

/*
 * Class:     test_H5TestUtils
 * Method:    H5VLfapl_is_native
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_test_H5TestUtils_H5VLfapl_1is_1native(JNIEnv *env, jclass clss, jlong fapl_id)
{
    hbool_t bval = JNI_FALSE;

    UNUSED(clss);

    if (H5VL_fapl_is_native((hid_t)fapl_id, &bval) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
}

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
