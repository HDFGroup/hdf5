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
/* Header for class hdf_hdf5lib_HDFNativeData */

#ifndef _Included_hdf_hdf5lib_HDFNativeData
#define _Included_hdf_hdf5lib_HDFNativeData

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/* returns int [] */
JNIEXPORT jintArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToInt___3B
(JNIEnv *, jclass, jbyteArray);

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat___3B
(JNIEnv *, jclass, jbyteArray);

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort___3B
(JNIEnv *, jclass, jbyteArray);

/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong___3B
(JNIEnv *, jclass, jbyteArray);

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble___3B
(JNIEnv *, jclass, jbyteArray);

/* returns int [] */
JNIEXPORT jintArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToInt__II_3B
(JNIEnv *, jclass, jint, jint, jbyteArray);

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort__II_3B
(JNIEnv *, jclass, jint, jint, jbyteArray);

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat__II_3B
(JNIEnv *, jclass, jint, jint, jbyteArray);

/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong__II_3B
(JNIEnv *, jclass, jint, jint, jbyteArray);

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble__II_3B
(JNIEnv *, jclass, jint, jint, jbyteArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__II_3I
(JNIEnv *, jclass, jint, jint, jintArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__II_3S
(JNIEnv *, jclass, jint, jint, jshortArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__II_3F
(JNIEnv *, jclass, jint, jint, jfloatArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__II_3D
(JNIEnv *, jclass, jint, jint, jdoubleArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__II_3J
(JNIEnv *, jclass, jint, jint, jlongArray);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__I
(JNIEnv *, jclass, jint);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__F
(JNIEnv *, jclass, jfloat);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__S
(JNIEnv *, jclass, jshort);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__D
(JNIEnv *env, jclass, jdouble);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__J
(JNIEnv *, jclass, jlong);

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToByte__B
(JNIEnv *, jclass, jbyte);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_HDFNativeData */
