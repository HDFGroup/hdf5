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

#ifndef _Included_hdf_hdf5lib_H5_H5PStrCPL
#define _Included_hdf_hdf5lib_H5_H5PStrCPL

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_char_encoding
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_char_encoding
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding
(JNIEnv *, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PStrCPL */
