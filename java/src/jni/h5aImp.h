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
/* Header for class hdf_hdf5lib_H5_H5A */

#ifndef Included_hdf_hdf5lib_H5_H5A
#define Included_hdf_hdf5lib_H5_H5A

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acreate
 * Signature: (JLjava/lang/String;JJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Acreate(JNIEnv *, jclass, jlong, jstring, jlong, jlong,
                                                        jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_name
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aopen_1name(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_idx
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aopen_1idx(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread(JNIEnv *, jclass, jlong, jlong, jbyteArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite(JNIEnv *, jclass, jlong, jlong, jbyteArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_short
 * Signature: (JJ[SZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1short(JNIEnv *, jclass, jlong, jlong, jshortArray,
                                                          jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_short
 * Signature: (JJ[SZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1short(JNIEnv *, jclass, jlong, jlong, jshortArray,
                                                           jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_int
 * Signature: (JJ[IZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1int(JNIEnv *, jclass, jlong, jlong, jintArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_int
 * Signature: (JJ[IZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1int(JNIEnv *, jclass, jlong, jlong, jintArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_long
 * Signature: (JJ[JZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1long(JNIEnv *, jclass, jlong, jlong, jlongArray,
                                                         jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_long
 * Signature: (JJ[JZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1long(JNIEnv *, jclass, jlong, jlong, jlongArray,
                                                          jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_float
 * Signature: (JJ[FZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1float(JNIEnv *, jclass, jlong, jlong, jfloatArray,
                                                          jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_float
 * Signature: (JJ[FZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1float(JNIEnv *, jclass, jlong, jlong, jfloatArray,
                                                           jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_double
 * Signature: (JJ[DZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1double(JNIEnv *, jclass, jlong, jlong, jdoubleArray,
                                                           jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_double
 * Signature: (JJ[DZ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1double(JNIEnv *, jclass, jlong, jlong, jdoubleArray,
                                                            jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadVL
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5AreadVL(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AwriteVL
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5AwriteVL(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_string
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1string(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_string
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1string(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_VLStrings
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1VLStrings(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_VLStrings
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Awrite_1VLStrings(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_reg_ref
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aread_1reg_1ref(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_space
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aget_1space(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_type
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aget_1type(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Aget_1name(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_num_attrs
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aget_1num_1attrs(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Adelete(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5__1H5Aclose(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Acreate2
 * Signature: (JLjava/lang/String;JJJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Acreate2(JNIEnv *, jclass, jlong, jstring, jlong, jlong,
                                                         jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aopen(JNIEnv *, jclass, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_idx
 * Signature: (JLjava/lang/String;IIJJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aopen_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint, jint,
                                                               jlong, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Acreate_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;JJJJJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Acreate_1by_1name(JNIEnv *, jclass, jlong, jstring, jstring,
                                                                  jlong, jlong, jlong, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5Aexists_1by_1name(JNIEnv *, jclass, jlong, jstring, jstring,
                                                                   jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename
 * Signature: (JLjava/lang/String;Ljava/lang/String)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Arename(JNIEnv *, jclass, jlong, jstring, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Arename_1by_1name(JNIEnv *, jclass, jlong, jstring, jstring,
                                                               jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_hdf_hdf5lib_H5_H5Aget_1name_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint,
                                                                    jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_storage_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5_H5Aget_1storage_1size(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Aget_1info(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint,
                                                                    jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1name(JNIEnv *, jclass, jlong, jstring,
                                                                     jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Adelete_1by_1name(JNIEnv *, jclass, jlong, jstring, jstring,
                                                               jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL Java_hdf_hdf5lib_H5_H5Aexists(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_idx
 * Signature: (JLjava/lang/String;IIJJ)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5Adelete_1by_1idx(JNIEnv *, jclass, jlong, jstring, jint, jint,
                                                              jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;JJ)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aopen_1by_1name(JNIEnv *, jclass, jlong, jstring, jstring,
                                                                jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL Java_hdf_hdf5lib_H5__1H5Aget_1create_1plist(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate
 * Signature: (JIIJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aiterate(JNIEnv *, jclass, jlong, jint, jint, jlong, jobject,
                                                      jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate_by_name
 * Signature: (JLjava/lang/String;IIJLjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Aiterate_1by_1name(JNIEnv *, jclass, jlong, jstring, jint, jint,
                                                                jlong, jobject, jobject, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* Included_hdf_hdf5lib_H5_H5A */
