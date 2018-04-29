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

#include <jni.h>
/* Header for class hdf_hdf5lib_H5_H5I */

#ifndef _Included_hdf_hdf5lib_H5_H5I
#define _Included_hdf_hdf5lib_H5_H5I

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_type
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1type
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_name_long
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1name_1long
  (JNIEnv*, jclass, jlong, jobjectArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1name
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1ref
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iinc_ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iinc_1ref
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idec_1ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Idec_1ref
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_file_id
 * Signature: (J)J
 */

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1file_1id
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1type_1ref
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idec_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Idec_1type_1ref
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iinc_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iinc_1type_1ref
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Inmembers
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Inmembers
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iis_valid
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Iis_1valid
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Itype_exists
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Itype_1exists
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iclear_type
 * Signature: (IZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Iclear_1type
  (JNIEnv*, jclass, jint, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idestroy_type
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Idestroy_1type
  (JNIEnv*, jclass, jint);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5I */
