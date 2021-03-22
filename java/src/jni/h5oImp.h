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
/* Header for class hdf_hdf5lib_H5_H5_H5O */

#ifndef Included_hdf_hdf5lib_H5_H5O
#define Included_hdf_hdf5lib_H5_H5O

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Oopen(JNIEnv *, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5__1H5Oclose(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ocopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Ocopy(JNIEnv *, jclass, jlong, jstring, jlong, jstring, jlong,
                                                   jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info
 * Signature: (JI)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1info(JNIEnv *, jclass, jlong, jint fields);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_name
 * Signature: (JLjava/lang/String;IJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name(JNIEnv *, jclass, jlong, jstring,
                                                                     jint fields, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_idx
 * Signature: (JLjava/lang/String;IIJIJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint,
                                                                    jint, jlong, jint fields, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info
 * Signature: (JI)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1native_1info(JNIEnv *, jclass, jlong, jint fields);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info_by_name
 * Signature: (JLjava/lang/String;IJ)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1name(JNIEnv *, jclass, jlong, jstring,
                                                                             jint fields, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info_by_idx
 * Signature: (JLjava/lang/String;IIJIJ)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1idx(JNIEnv *, jclass, jlong, jstring,
                                                                            jint, jint, jlong, jint fields,
                                                                            jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Olink
 * Signature: (JJLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Olink(JNIEnv *, jclass, jlong, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;I)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Ovisit(JNIEnv *, jclass, jlong, jint, jint, jobject, jobject,
                                                    jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;IJ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name(JNIEnv *, jclass, jlong, jstring, jint, jint,
                                                              jobject, jobject, jint, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Oset_1comment(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name(JNIEnv *, jclass, jlong, jstring,
                                                                     jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Oget_1comment(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment_by_name
 * Signature: (JLjava/lang/String;J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name(JNIEnv *, jclass, jlong, jstring,
                                                                        jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oexists_by_name
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5Oexists_1by_1name(JNIEnv *, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odecr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Odecr_1refcount(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oincr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Oincr_1refcount(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_token
 * Signature: (JLhdf/hdf5lib/structs/H5O_token_t;)J;
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Oopen_1by_1token(JNIEnv *, jclass, jlong, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_idx
 * Signature: (JLjava/lang/String;IIJJ)J;
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint, jint,
                                                               jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Oflush(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Orefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Orefresh(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odisable_mdc_flushes
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Odisable_1mdc_1flushes(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oenable_mdc_flushes
 * Signature: (J)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Oenable_1mdc_1flushes(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oare_mdc_flushes_disabled
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5Oare_1mdc_1flushes_1disabled(JNIEnv *, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_hdf_hdf5lib_H5_H5O */
