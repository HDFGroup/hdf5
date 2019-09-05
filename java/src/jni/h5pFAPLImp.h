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

#ifndef _Included_hdf_hdf5lib_H5_H5PFAPL
#define _Included_hdf_hdf5lib_H5_H5PFAPL

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * TODO: H5Pset_driver
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_driver
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1driver
(JNIEnv *, jclass, jlong);

/*
 * TODO: H5Pget_driver_info
 */

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
 * TODO: H5Pset_core_write_tracking
 */

/*
 * TODO: H5Pget_core_write_tracking
 */

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
 * Method:    H5Pget_fapl_hdfs
 * Signature: (J)Lhdf/hdf5lib/structs/H5FD_hdfs_fapl_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1hdfs
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_hdfs
 * Signature: (JLhdf/hdf5lib/structs/H5FD_hdfs_fapl_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1hdfs
(JNIEnv *, jclass, jlong, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_log
 * Signature: (JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log
(JNIEnv *, jclass, jlong, jstring, jlong, jlong);

/*
 * TODO: H5Pset_fapl_mpio
 */

/*
 * TODO: H5Pget_fapl_mpio
 */

/*
 * TODO: H5Pset_fapl_mpiposix
 */

/*
 * TODO: H5Pget_fapl_mpiposix
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_multi
 * Signature: (J[I[J[Ljava/lang/String;[JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi
(JNIEnv *, jclass, jlong, jintArray, jlongArray, jobjectArray, jlongArray, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_multi
 * Signature: (J[I[J[Ljava/lang/String;[J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi
(JNIEnv *, jclass, jlong, jintArray, jlongArray, jobjectArray, jlongArray);

/*
 * TODO: H5Pset_multi_type
 */

/*
 * TODO: H5Pget_multi_type
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_ros3
 * Signature: (J)Lhdf/hdf5lib/structs/H5FD_ros3_fapl_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1ros3
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_ros3
 * Signature: (JLhdf/hdf5lib/structs/H5FD_ros3_fapl_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1ros3
(JNIEnv *, jclass, jlong, jobject);

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
 * TODO: H5Pset_file_image
 */

/*
 * TODO: H5Pget_file_image
 */

/*
 * TODO: H5Pset_file_image_callbacks
 */

/*
 * TODO: H5Pget_file_image_callbacks
 */

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
 * TODO: H5Pset_page_buffer_size
 */

/*
 * TODO: H5Pget_page_buffer_size
 */

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
 * Method:    H5Pset_evict_on_close
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1evict_1on_1close
(JNIEnv *, jclass, jlong, jboolean);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_evict_on_close
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1evict_1on_1close
(JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_metadata_read_attempts
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts
(JNIEnv *, jclass, jlong, jlong);

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
 * Method:    H5Pset_mdc_config
 * Signature: (JLhdf/hdf5lib/structs/H5AC_cache_config_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config
(JNIEnv *, jclass, jlong, jobject);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_config
 * Signature: (J)Lhdf/hdf5lib/structs/H5AC_cache_config_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config
(JNIEnv *, jclass, jlong);

/*
 * TODO: H5Pset_mdc_image_config
 */

/*
 * TODO: H5Pget_mdc_image_config
 */

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
 * TODO: H5Pset_all_coll_metadata_ops
 */

/*
 * TODO: H5Pget_all_coll_metadata_ops
 */

/*
 * TODO: H5Pset_coll_metadata_write
 */

/*
 * TODO: H5Pget_coll_metadata_write
 */

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
 * Method:    H5Pset_libver_bounds
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds
(JNIEnv *, jclass, jlong, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_libver_bounds
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds
(JNIEnv *, jclass, jlong, jintArray);

/*
 * TODO: H5Pset_object_flush_cb
 */

/*
 * TODO: H5Pget_object_flush_cb
 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PFAPL */
