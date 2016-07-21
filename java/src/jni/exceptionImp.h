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
/* Header for class hdf_hdf5lib_H5_exception */

#ifndef _Included_hdf_hdf5lib_H5_exception
#define _Included_hdf_hdf5lib_H5_exception

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_exceptions_HDF5Library
 * Method:    H5error_off
 * Signature: ()I
 *
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5error_1off
  (JNIEnv *env, jclass clss );


/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    printStackTrace0
 * Signature: (Ljava/lang/Object;)V
 *
 *  Call the HDF-5 library to print the HDF-5 error stack to 'file_name'.
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_printStackTrace0
  (JNIEnv *env, jobject obj, jstring file_name);

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMajorErrorNumber
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMajorErrorNumber
  (JNIEnv *env, jobject obj);

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMinorErrorNumber
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMinorErrorNumber
  (JNIEnv *env, jobject obj);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_exception */
