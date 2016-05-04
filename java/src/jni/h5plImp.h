/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
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

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PL */
