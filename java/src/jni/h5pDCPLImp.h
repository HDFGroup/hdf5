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

#ifndef _Included_hdf_hdf5lib_H5_H5PDCPL
#define _Included_hdf_hdf5lib_H5_H5PDCPL

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_layout
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1layout
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_layout
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1layout
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk
(JNIEnv *, jclass, jlong, jint, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk
 * Signature: (JI[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk
(JNIEnv *, jclass, jlong, jint, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk_opts
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk_1opts
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk_opts
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk_1opts
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_dset_no_attrs_hint
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1dset_1no_1attrs_1hint
  (JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_dset_no_attrs_hint
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1dset_1no_1attrs_1hint
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_deflate
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1deflate
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1value
(JNIEnv *, jclass, jlong, jlong, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1value
(JNIEnv *, jclass, jlong, jlong, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pfill_value_defined
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1time
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1time
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_alloc_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alloc_1time
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alloc_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alloc_1time
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1filter
(JNIEnv *, jclass, jlong, jint, jint, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pall_filters_avail
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pall_1filters_1avail
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nfilters
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nfilters
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter
(JNIEnv *, jclass, jlong, jint, jintArray, jlongArray, jintArray, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter2
(JNIEnv *, jclass, jlong, jint, jintArray, jlongArray, jintArray, jlong, jobjectArray, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id
(JNIEnv *, jclass, jlong, jint, jintArray, jlongArray, jintArray, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2
(JNIEnv *, jclass, jlong, jint, jintArray, jlongArray, jintArray, jlong, jobjectArray, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pmodify_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pmodify_1filter
(JNIEnv *, jclass, jlong, jint, jint, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove_filter
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5P1remove_1filter
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fletcher32
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fletcher32
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nbit
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nbit
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_scaleoffset
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1scaleoffset
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shuffle
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shuffle
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_szip
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1szip
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_external
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1external
(JNIEnv *, jclass, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external
 * Signature: (JIJ[Ljava/lang/String;[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external
(JNIEnv *, jclass, jlong, jint, jlong, jobjectArray, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external_count
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external_1count
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual
 * Signature: (JJLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual
(JNIEnv *, jclass, jlong, jlong, jstring, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_count
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1count
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_dsetname
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1dsetname
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_filename
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_srcspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1srcspace
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_vspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_prefix
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1prefix
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_prefix
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1prefix
(JNIEnv *, jclass, jlong);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PDCPL */
