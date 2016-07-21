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
/* Header for class hdf_hdf5lib_H5_H5E */

#ifndef _Included_hdf_hdf5lib_H5_H5E
#define _Included_hdf_hdf5lib_H5_H5E

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eauto_is_v2
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Eauto_1is_1v2
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eregister_class
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eregister_1class
  (JNIEnv *, jclass, jstring, jstring, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eunregister_class
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eunregister_1class
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_msg
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1msg
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_msg
 * Signature: (JILjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1msg
  (JNIEnv *, jclass, jlong, jint, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1stack
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_current_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1current_1stack
  (JNIEnv *, jclass);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1stack
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eprint2
 * Signature: (JLjava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eprint2
  (JNIEnv *, jclass, jlong, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1class_1name
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eset_current_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eset_1current_1stack
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epop
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epop
  (JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epush2
 * Signature: (JLjava/lang/String;Ljava/lang/String;IJJJLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epush2
  (JNIEnv *, jclass, jlong, jstring, jstring, jint, jlong, jlong, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclear2
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclear2
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_msg
 * Signature: (J[I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1msg
  (JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_num
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1num
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ewalk2
 * Signature: (JJLjava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ewalk2
  (JNIEnv *, jclass, jlong, jlong, jobject, jobject);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5E */
