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
/* Header for class hdf_hdf5lib_H5_H5_H5P */

#ifndef _Included_hdf_hdf5lib_H5_H5P
#define _Included_hdf_hdf5lib_H5_H5P

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcreate
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcopy
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_version
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1version
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_userblock
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1userblock
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_userblock
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1userblock
(JNIEnv *, jclass, jlong, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sizes
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sizes
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sizes
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sizes
(JNIEnv *, jclass, jlong, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sym_k
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sym_1k
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sym_k
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sym_1k
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_istore_k
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1istore_1k
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_istore_k
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1istore_1k
(JNIEnv *, jclass, jlong, jintArray);

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
 * Method:    H5Pset_alignment
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alignment
(JNIEnv *, jclass, jlong, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alignment
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alignment
(JNIEnv *, jclass, jlong, jlongArray);

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
 * Method:    H5Pget_external_count
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external_1count
(JNIEnv *, jclass, jlong);

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
 * Method:    H5Pset_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1filter
(JNIEnv *, jclass, jlong, jint, jint, jlong, jintArray);

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
 * Method:    H5Pget_driver
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1driver
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_cache
 * Signature: (JIJJD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1cache
(JNIEnv *, jclass, jlong, jint, jlong, jlong, jdouble);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_cache
 * Signature: (J[I[J[J[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1cache
(JNIEnv *, jclass, jlong, jintArray, jlongArray, jlongArray, jdoubleArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer
 * Signature: (JJ[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer
(JNIEnv *, jclass, jlong, jlong, jbyteArray, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer
 * Signature: (J[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer
(JNIEnv *, jclass, jlong, jbyteArray, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer_1size
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer_1size
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_preserve
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1preserve
(JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_preserve
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1preserve
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
 * Method:    H5Pset_gc_references
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1gc_1references
(JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_gc_references
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1gc_1references
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_btree_ratios
 * Signature: (JDDD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1btree_1ratios
(JNIEnv *, jclass, jlong, jdouble, jdouble, jdouble);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_btree_ratios
 * Signature: (J[D[D[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1btree_1ratios
(JNIEnv *, jclass, jlong, jdoubleArray, jdoubleArray, jdoubleArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_small_data_block_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1small_1data_1block_1size
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_small_data_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1small_1data_1block_1size
(JNIEnv *, jclass, jlong);

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
 * Method:    H5Pfill_value_defined
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined
(JNIEnv *, jclass, jlong, jintArray);

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
 * Method:    H5Pset_edc_check
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1edc_1check
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_edc_check
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1edc_1check
(JNIEnv *, jclass, jlong);

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
 * Method:    H5Pset_hyper_vector_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1hyper_1vector_1size
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_hyper_vector_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1hyper_1vector_1size
(JNIEnv *, jclass, jlong, jlongArray);

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
 * Method:    H5Pmodify_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pmodify_1filter
(JNIEnv *, jclass, jlong, jint, jint, jlong, jintArray);

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
 * Method:    H5Pset_fclose_degree
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fclose_1degree
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fclose_degree
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fclose_1degree
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_family
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1family
(JNIEnv *, jclass, jlong, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_family
 * Signature: (J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1family
(JNIEnv *, jclass, jlong, jlongArray, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_core
 * Signature: (JJZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1core
(JNIEnv *, jclass, jlong, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_core
 * Signature: (J[J[Z)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1core
(JNIEnv *, jclass, jlong, jlongArray, jbooleanArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_family_offset
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1family_1offset
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_family_offset
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1family_1offset
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_log
 * Signature: (JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log
(JNIEnv *, jclass, jlong, jstring, jlong, jlong);

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
 * Method:    H5Pset
 * Signature: (JLjava/lang/String;I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pset
(JNIEnv *, jclass, jlong, jstring, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pexist
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pexist
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_size
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1size
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nprops
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nprops
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1name
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_parent
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1parent
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pisa_class
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pisa_1class
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pequal
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pequal
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy_prop
 * Signature: (JJLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pcopy_1prop
(JNIEnv *, jclass, jlong, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Premove
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Punregister
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Punregister
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pclose_class
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose_1class
(JNIEnv *, jclass, jlong);

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
 * Method:    H5Pget_filter_by_id2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2
(JNIEnv *, jclass, jlong, jint, jintArray, jlongArray, jintArray, jlong, jobjectArray, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nlinks
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nlinks
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nlinks
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nlinks
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_libver_bounds
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_libver_bounds
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1creation_1order
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1creation_1order
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1creation_1order
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_copy_object
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1copy_1object
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_copy_object
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1copy_1object
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_create_intermediate_group
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1create_1intermediate_1group
(JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_create_intermediate_group
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1create_1intermediate_1group
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_data_transform
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1data_1transform
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_data_transform
 * Signature: (J[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1data_1transform
(JNIEnv *, jclass, jlong, jobjectArray, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_acc_flags
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1acc_1flags
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_acc_flags
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1acc_1flags
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1phase_1change
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1phase_1change
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1phase_1change
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_phase_change
 * Signature: (JII)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1phase_1change
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1phase_1change
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1phase_1change
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_nindexes
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1nindexes
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_nindexes
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1nindexes
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_index
 * Signature: (JIII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1index
(JNIEnv *, jclass, jlong, jint, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_index
 * Signature: (JI[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1index
(JNIEnv *, jclass, jlong, jint, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_local_heap_size_hint
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1local_1heap_1size_1hint
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_local_heap_size_hint
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1local_1heap_1size_1hint
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
 * Method:    H5Pset_est_link_info
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1est_1link_1info
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_est_link_info
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1est_1link_1info
(JNIEnv *, jclass, jlong, jintArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_fapl
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1fapl
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pget_elink_fapl
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pget_1elink_1fapl
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_prefix
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1prefix
(JNIEnv *, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_prefix
 * Signature: (J[Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1prefix
(JNIEnv *, jclass, jlong, jobjectArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_direct
 * Signature: (JJJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1direct
(JNIEnv *, jclass, jlong, jlong, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_direct
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1direct
(JNIEnv *, jclass, jlong, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_sec2
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1sec2
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_stdio
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1stdio
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_windows
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1windows
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_muti
 * Signature: (J[I[J[Ljava/lang/String;[J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi
(JNIEnv *, jclass, jlong, jintArray, jlongArray, jobjectArray, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_muti
 * Signature: (J[I[J[Ljava/lang/String;[JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi
(JNIEnv *, jclass, jlong, jintArray, jlongArray, jobjectArray, jlongArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_split
 * Signature: (JLjava/lang/String;JLjava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1split
(JNIEnv *, jclass, jlong, jstring, jlong, jstring, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_meta_block_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1meta_1block_1size
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_meta_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1meta_1block_1size
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sieve_buf_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sieve_1buf_1size
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sieve_buf_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sieve_1buf_1size
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_file_cache_size
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1file_1cache_1size
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_file_cache_size
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1file_1cache_1size
(JNIEnv *, jclass, jlong);


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_config
 * Signature: (J)Lhdf/hdf5lib/structs/H5AC_cache_config_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_config
 * Signature: (JLhdf/hdf5lib/structs/H5AC_cache_config_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config
(JNIEnv *, jclass, jlong, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk_cache
 * Signature: (JJJD)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk_1cache
(JNIEnv *, jclass, jlong, jlong, jlong, jdouble);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk_cache
 * Signature: (J[J[J[D)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk_1cache
(JNIEnv *, jclass, jlong, jlongArray, jlongArray, jdoubleArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_obj_track_times
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1obj_1track_1times
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_obj_track_times
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1obj_1track_1times
(JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_char_encoding
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_char_encoding
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding
(JNIEnv *, jclass, jlong, jint);

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
 * Method:    H5Pget_virtual_vspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace
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
 * Method:    H5Pget_virtual_filename
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename
(JNIEnv *, jclass, jlong, jlong);

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
 * Method:    H5Pget_virtual_view
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1view
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_view
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1view
(JNIEnv *, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_printf_gap
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1printf_1gap
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_printf_gap
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1printf_1gap
(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space
 * Signature: (J[I[J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space
(JNIEnv *, jclass, jlong, jintArray, jlongArray);


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_file_space
 * Signature: (JIJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1file_1space
(JNIEnv *, jclass, jlong, jint, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_log_options
 * Signature: (JZLjava/lang/String;Z)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1log_1options
(JNIEnv *, jclass, jlong, jboolean, jstring, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_log_options
 * Signature: (J[Z)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1log_1options
(JNIEnv *, jclass, jlong, jbooleanArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_append_flush
 * Signature: (JI[JLjava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1append_1flush
  (JNIEnv*, jclass, jlong, jint, jlongArray, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class_nocb
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class_1nocb
  (JNIEnv*, jclass, jlong, jstring);
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class
 * Signature: (JLjava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class
  (JNIEnv*, jclass, jlong, jstring, jobject, jobject, jobject, jobject, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2_1nocb
  (JNIEnv*, jclass, jlong, jstring, jlong, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2
  (JNIEnv*, jclass, jlong, jstring, jlong, jbyteArray, jobject, jobject, jobject, jobject, jobject, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2_1nocb
  (JNIEnv*, jclass, jlong, jstring, jlong, jbyteArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2
  (JNIEnv*, jclass, jlong, jstring, jlong, jbyteArray, jobject, jobject, jobject, jobject, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Piterate
 * Signature: (J[ILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Piterate
  (JNIEnv*, jclass, jlong, jintArray, jobject, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_metadata_read_attempts
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1metadata_1read_1attempts
(JNIEnv *, jclass, jlong);


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_metadata_read_attempts
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts
(JNIEnv *, jclass, jlong, jlong);


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5P */
