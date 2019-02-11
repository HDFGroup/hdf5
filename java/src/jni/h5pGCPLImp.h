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

#ifndef _Included_hdf_hdf5lib_H5_H5PGCPL
#define _Included_hdf_hdf5lib_H5_H5PGCPL

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_local_heap_size_hint
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1local_1heap_1size_1hint
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_local_heap_size_hint
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1local_1heap_1size_1hint
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1creation_1order
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_est_link_info
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1est_1link_1info
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_est_link_info
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1est_1link_1info
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1phase_1change
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1phase_1change
(JNIEnv *, jclass, jlong, jintArray);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PGCPL */
