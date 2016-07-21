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
/* Header for class hdf_hdf5lib_H5_H5_H5O */

#ifndef _Included_hdf_hdf5lib_H5_H5O
#define _Included_hdf_hdf5lib_H5_H5O

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen
  (JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Oclose
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ocopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ocopy
  (JNIEnv*, jclass, jlong, jstring, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info
(JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_name
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name
(JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx
(JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Olink
 * Signature: (JJLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Olink
  (JNIEnv*, jclass, jlong, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit
  (JNIEnv*, jclass, jlong, jint, jint, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jint, jint, jobject, jobject, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment
  (JNIEnv*, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment_by_name
 * Signature: (JLjava/lang/String;J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oexists_by_name
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Oexists_1by_1name
  (JNIEnv*, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odecr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Odecr_1refcount
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oincr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oincr_1refcount
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_addr
 * Signature: (JJ)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1addr
  (JNIEnv*, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_idx
 * Signature: (JLjava/lang/String;IIJJ)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx
  (JNIEnv*, jclass, jlong, jstring, jint, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Oflush
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Orefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Orefresh
  (JNIEnv*, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5O */
