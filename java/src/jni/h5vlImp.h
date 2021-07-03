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
/* Header for class hdf_hdf5lib_H5_H5VL */

#ifndef Included_hdf_hdf5lib_H5_H5VL
#define Included_hdf_hdf5lib_H5_H5VL

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_name
 * Signature: (Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1name(JNIEnv *, jclass, jobject,
                                                                              jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_value
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1value(JNIEnv *, jclass, jint, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLis_connector_registered_by_name
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1name(JNIEnv *, jclass,
                                                                                       jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLis_connector_registered_by_value
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1value(JNIEnv *, jclass,
                                                                                        jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5VLget_1connector_1id(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id_by_name
 * Signature: (Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1name(JNIEnv *, jclass, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id_by_value
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1value(JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_name
 * Signature: (J)Ljava/lang/String;
 */

JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5VLget_1connector_1name(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLclose
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5VLclose(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLunregister_connector
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5VLunregister_1connector(JNIEnv *, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_hdf_hdf5lib_H5_H5VL */
