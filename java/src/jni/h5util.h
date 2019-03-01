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

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    http://hdfgroup.org/HDF5/doc/
 *
 */

#ifndef H5UTIL_H__
#define H5UTIL_H__

#include "h5jni.h"

#ifndef SUCCEED
#define SUCCEED     0
#endif

#ifndef FAIL
#define FAIL        (-1)
#endif

typedef struct h5str_t {
    char    *s;
    size_t   max;  /* the allocated size of the string */
} h5str_t;

extern void    h5str_new(h5str_t *str, size_t len);
extern void    h5str_free(h5str_t *str);
extern void    h5str_resize(h5str_t *str, size_t new_len);
extern char*   h5str_append(h5str_t *str, const char* cstr);
extern size_t  h5str_convert(JNIEnv *env, char **in_str, hid_t container, hid_t tid, void *out_buf, size_t out_buf_offset);
extern size_t  h5str_sprintf(JNIEnv *env, h5str_t *out_str, hid_t container, hid_t tid, void *in_buf, size_t in_buf_len, int expand_data);
extern void    h5str_array_free(char **strs, size_t len);
extern int     h5str_dump_simple_dset(JNIEnv *env, FILE *stream, hid_t dset, int binary_order);
extern int     h5str_dump_region_blocks_data(JNIEnv *env, h5str_t *str, hid_t region, hid_t region_obj);
extern int     h5str_dump_region_points_data(JNIEnv *env, h5str_t *str, hid_t region, hid_t region_obj);

extern htri_t  H5Tdetect_variable_str(hid_t tid);

/*
 * Symbols used to format the output of h5str_sprintf and
 * to interpret the input to h5str_convert.
 */
#define H5_COMPOUND_BEGIN_INDICATOR "{"
#define H5_COMPOUND_END_INDICATOR "}"
#define H5_ARRAY_BEGIN_INDICATOR "["
#define H5_ARRAY_END_INDICATOR "]"
#define H5_VLEN_BEGIN_INDICATOR "("
#define H5_VLEN_END_INDICATOR ")"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadComplex
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AreadComplex
  (JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Copies the content of one dataset to another dataset
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Acopy
  (JNIEnv *, jclass, jlong, jlong);

/*
 * Copies the content of one dataset to another dataset
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dcopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dcopy
  (JNIEnv*, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_full
 * Signature: (JLjava/lang/String;[Ljava/lang/String;[I[I[J[JIII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1full
  (JNIEnv*, jclass, jlong, jstring, jobjectArray, jintArray, jintArray, jlongArray, jlongArray, jint, jint, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_max
 * Signature: (J[Ljava/lang/String;[I[I[JJI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1max
  (JNIEnv*, jclass, jlong, jobjectArray, jintArray, jintArray, jlongArray, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5export_dataset
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5export_1dataset
  (JNIEnv*, jclass, jstring, jstring, jstring, jint);

#endif  /* H5UTIL_H__ */
