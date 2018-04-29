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
/* Header for class hdf_hdf5lib_H5_H5 */

#ifndef _Included_hdf_hdf5lib_H5_H5
#define _Included_hdf_hdf5lib_H5_H5

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5open
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5open
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5close
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5close
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5dont_atexit
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5dont_1atexit
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5get_libversion
 * Signature: ([I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5get_1libversion
  (JNIEnv *, jclass, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5check_version
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5check_1version
  (JNIEnv *, jclass, jint, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5garbage_collect
 * Signature: ()I
 *
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5garbage_1collect
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5set_free_list_limits
 * Signature: (IIIIII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5set_1free_1list_1limits
  (JNIEnv *, jclass, jint, jint, jint, jint, jint, jint );

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5is_library_threadsafe
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5is_1library_1threadsafe
  (JNIEnv *, jclass);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5 */
