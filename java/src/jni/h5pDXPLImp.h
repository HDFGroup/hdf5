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

#ifndef _Included_hdf_hdf5lib_H5_H5PDXPL
#define _Included_hdf_hdf5lib_H5_H5PDXPL

#include <jni.h>

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

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
 * TODO: H5Pset_filter_callback
 */

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
 * TODO: H5Pset_type_conv_cb
 */

/*
 * TODO: H5Pget_type_conv_cb
 */

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
 * TODO: H5Pset_vlen_mem_manager
 */

/*
 * TODO: H5Pget_vlen_mem_manager
 */

/*
 * TODO: H5Pset_dxpl_mpio
 */

/*
 * TODO: H5Pget_dxpl_mpio
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt_num
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt_ratio
 */

/*
 * TODO: H5Pset_dxpl_mpio_collective_opt
 */

/*
 * TODO: H5Pget_mpio_actual_chunk_opt_mode
 */

/*
 * TODO: H5Pget_mpio_actual_io_mode
 */

/*
 * TODO: H5Pget_mpio_no_collective_cause
 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5PDXPL */
