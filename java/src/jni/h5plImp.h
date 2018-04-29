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
/* Header for class hdf_hdf5lib_H5_H5PL */

#ifndef _Included_hdf_hdf5lib_H5_H5PL
#define _Included_hdf_hdf5lib_H5_H5PL

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLset_loading_state
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLset_1loading_1state
  (JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLget_loading_state
 * Signature: (V)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5PLget_1loading_1state
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLappend
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLappend
  (JNIEnv *, jclass, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLprepend
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLprepend
  (JNIEnv *, jclass, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLreplace
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLreplace
  (JNIEnv *, jclass, jobjectArray, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLinsert
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLinsert
  (JNIEnv *, jclass, jobjectArray, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLremove
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLremove
  (JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLget
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5PLget
  (JNIEnv *, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLsize
 * Signature: (V)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5PLsize
  (JNIEnv *, jclass);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PL */
