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
/* Header for class hdf_hdf5lib_H5_H5_H5L */

#ifndef _Included_hdf_hdf5lib_H5_H5L
#define _Included_hdf_hdf5lib_H5_H5L

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcopy
  (JNIEnv*, jclass, jlong, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_external
 * Signature: (Ljava/lang/String;Ljava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1external
(JNIEnv*, jclass, jstring, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_hard
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1hard
  (JNIEnv*, jclass, jlong, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_soft
 * Signature: (Ljava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1soft
  (JNIEnv*, jclass, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ldelete
 * Signature: (JLjava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ldelete
  (JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ldelete_by_idx
 * Signature: (JLjava/lang/String;IIJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ldelete_1by_1idx
(JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lexists
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Lexists
  (JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_info
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5L_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1info
(JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5L_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1info_1by_1idx
(JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_name_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1name_1by_1idx
(JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_value
 * Signature: (JLjava/lang/String;[Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1value
  (JNIEnv*, jclass, jlong, jstring, jobjectArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_value_by_idx
 * Signature: (JLjava/lang/String;IIJ[Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1value_1by_1idx
(JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jobjectArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lmove
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lmove
  (JNIEnv*, jclass, jlong, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lvisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lvisit
  (JNIEnv*, jclass, jlong, jint, jint, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lvisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lvisit_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jint, jint, jobject, jobject, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Literate
 * Signature: (JIIJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Literate
  (JNIEnv*, jclass, jlong, jint, jint, jlong, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Literate_by_name
 * Signature: (JLjava/lang/String;IIJLjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Literate_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jobject, jobject, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lis_registered
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lis_1registered
  (JNIEnv*, jclass, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lunregister
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lunregister
  (JNIEnv*, jclass, jint);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5L */
