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

#include <jni.h>
/* Header for HDF5 JNI test routines */

#ifndef Included_h5jni_test
#define Included_h5jni_test

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     test_H5TestUtils
 * Method:    H5VLfapl_is_native
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL Java_test_H5TestUtils_H5VLfapl_1is_1native(JNIEnv *, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_h5jni_test */
