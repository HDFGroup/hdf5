/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <jni.h>
/* Header for class hdf_hdf5lib_H5_H5R */

#ifndef Included_hdf_hdf5lib_H5_H5R
#define Included_hdf_hdf5lib_H5_H5R

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* H5R: HDF5 1.12 Reference API Functions */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_object
 * Signature: (JLjava/lang/String;)[B
 */
JNIEXPORT jbyteArray JNICALL Java_hdf_hdf5lib_H5_H5Rcreate_1object(JNIEnv *, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_region
 * Signature: (JLjava/lang/String;J)[B
 */
JNIEXPORT jbyteArray JNICALL Java_hdf_hdf5lib_H5_H5Rcreate_1region(JNIEnv *, jclass, jlong, jstring, jlong,
                                                                   jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_attr
 * Signature: (JLjava/lang/String;Ljava/lang/String;)[B
 */
JNIEXPORT jbyteArray JNICALL Java_hdf_hdf5lib_H5_H5Rcreate_1attr(JNIEnv *, jclass, jlong, jstring, jstring,
                                                                 jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rdestroy
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Rdestroy(JNIEnv *, jclass, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_type
 * Signature: ([B)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Rget_1type(JNIEnv *, jclass, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Requal
 * Signature: ([B[B)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5Requal(JNIEnv *, jclass, jbyteArray, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcopy
 * Signature: ([B)[B
 */
JNIEXPORT jbyteArray JNICALL Java_hdf_hdf5lib_H5_H5Rcopy(JNIEnv *, jclass, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_object
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Ropen_1object(JNIEnv *, jclass, jbyteArray, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_region
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Ropen_1region(JNIEnv *, jclass, jbyteArray, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_attr
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Ropen_1attr(JNIEnv *, jclass, jbyteArray, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_obj_type3
 * Signature: ([BJ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Rget_1obj_1type3(JNIEnv *, jclass, jbyteArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_file_name
 * Signature: ([B)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Rget_1file_1name(JNIEnv *, jclass, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_obj_name
 * Signature: ([BJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Rget_1obj_1name(JNIEnv *, jclass, jbyteArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_attr_name
 * Signature: ([B)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Rget_1attr_1name(JNIEnv *, jclass, jbyteArray);

/* H5R: HDF5 1.8 Reference API Functions */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate
 * Signature: ([BJLjava/lang/String;IJ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Rcreate(JNIEnv *, jclass, jbyteArray, jlong, jstring, jint,
                                                     jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Rdereference
 * Signature: (JJI[B)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Rdereference(JNIEnv *, jclass, jlong, jlong, jint,
                                                             jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_region
 * Signature: (JI[B)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Rget_1region(JNIEnv *, jclass, jlong, jint, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5G_obj_t H5Rget_obj_type
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Rget_1obj_1type(JNIEnv *, jclass, jlong, jint, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    int H5Rget_obj_type2
 * Signature: (JI[B[I)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Rget_1obj_1type2(JNIEnv *, jclass, jlong, jint, jbyteArray,
                                                              jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_name
 * Signature: (JI[B[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5Rget_1name(JNIEnv *, jclass, jlong, jint, jbyteArray,
                                                         jobjectArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_name_string
 * Signature: (JI[B)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Rget_1name_1string(JNIEnv *, jclass, jlong, jint, jbyteArray);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_hdf_hdf5lib_H5_H5R */
