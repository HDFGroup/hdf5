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

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    https://portal.hdfgroup.org/documentation/index.html
 *
 */

#ifndef H5UTIL_H__
#define H5UTIL_H__

#include "h5jni.h"

#ifndef SUCCEED
#define SUCCEED 0
#endif

#ifndef FAIL
#define FAIL (-1)
#endif

typedef struct h5str_t {
    char  *s;
    size_t max; /* the allocated size of the string */
} h5str_t;

extern void   h5str_new(h5str_t *str, size_t len);
extern void   h5str_free(h5str_t *str);
extern void   h5str_resize(h5str_t *str, size_t new_len);
extern char  *h5str_append(h5str_t *str, const char *cstr);
extern htri_t h5str_detect_vlen(hid_t tid);
extern size_t h5str_convert(JNIEnv *env, char **in_str, hid_t container, hid_t tid, void *out_buf,
                            size_t out_buf_offset);
extern int    h5str_sprint_old_reference(JNIEnv *env, h5str_t *out_str, hid_t region_obj, void *ref_buf);
extern int    h5str_sprint_reference(JNIEnv *env, h5str_t *out_str, void *ref_p);
extern size_t h5str_sprintf(JNIEnv *env, h5str_t *out_str, hid_t container, hid_t tid, void *in_buf,
                            int expand_data);
extern void   h5str_array_free(char **strs, size_t len);
extern int    h5str_dump_simple_dset(JNIEnv *env, FILE *stream, hid_t dset, int binary_order);
extern int    h5str_dump_simple_mem(JNIEnv *env, FILE *stream, hid_t attr, int binary_order);

extern htri_t H5Tdetect_variable_str(hid_t tid);

extern void translate_rbuf(JNIEnv *env, jobjectArray ret_buf, jlong mem_type_id, H5T_class_t type_class,
                           jsize count, void *raw_buf);
extern void translate_wbuf(JNIEnv *env, jobjectArray ret_buf, jlong mem_type_id, H5T_class_t type_class,
                           jsize count, void *raw_buf);

/*
 * Symbols used to format the output of h5str_sprintf and
 * to interpret the input to h5str_convert.
 */
#define H5_COMPOUND_BEGIN_INDICATOR "{"
#define H5_COMPOUND_END_INDICATOR   "}"
#define H5_ARRAY_BEGIN_INDICATOR    "["
#define H5_ARRAY_END_INDICATOR      "]"
#define H5_VLEN_BEGIN_INDICATOR     "("
#define H5_VLEN_END_INDICATOR       ")"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadComplex
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5AreadComplex(JNIEnv *, jclass, jlong, jlong, jobjectArray);

/*
 * Copies the content of one dataset to another dataset
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Acopy(JNIEnv *, jclass, jlong, jlong);

/*
 * Copies the content of one dataset to another dataset
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dcopy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Dcopy(JNIEnv *, jclass, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_full
 * Signature: (JLjava/lang/String;[Ljava/lang/String;[I[I[J[JIII)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1full(JNIEnv *, jclass, jlong, jstring,
                                                                   jobjectArray, jintArray, jintArray,
                                                                   jlongArray, jobjectArray, jint, jint,
                                                                   jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_obj_info_max
 * Signature: (J[Ljava/lang/String;[I[I[JJI)I
 */
JNIEXPORT jint JNICALL Java_hdf_hdf5lib_H5_H5Gget_1obj_1info_1max(JNIEnv *, jclass, jlong, jobjectArray,
                                                                  jintArray, jintArray, jlongArray, jlong,
                                                                  jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5export_dataset
 * Signature: (Ljava/lang/String;JLjava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5export_1dataset(JNIEnv *, jclass, jstring, jlong, jstring, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5export_attribute
 * Signature: (Ljava/lang/String;JLjava/lang/String;I)V
 */
JNIEXPORT void JNICALL Java_hdf_hdf5lib_H5_H5export_1attribute(JNIEnv *, jclass, jstring, jlong, jstring,
                                                               jint);

#endif /* H5UTIL_H__ */
