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

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5pDAPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
extern JavaVM *jvm;

typedef struct _cb_wrapper {
    jobject visit_callback;
    jobject op_data;
} cb_wrapper;

static herr_t H5D_append_cb(hid_t dataset_id, hsize_t *cur_dims, void *cb_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk_cache
 * Signature: (JJJD)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk_1cache
    (JNIEnv *env, jclass clss, jlong dapl, jlong rdcc_nslots,
        jlong rdcc_nbytes, jdouble rdcc_w0)
{
    UNUSED(clss);

    if (H5Pset_chunk_cache((hid_t)dapl, (size_t)rdcc_nslots, (size_t)rdcc_nbytes, (double) rdcc_w0) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1chunk_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk_cache
 * Signature: (J[J[J[D)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk_1cache
    (JNIEnv *env, jclass clss, jlong dapl, jlongArray rdcc_nslots,
        jlongArray rdcc_nbytes, jdoubleArray rdcc_w0)
{
    jboolean  isCopy;
    jdouble  *w0Array = NULL;
    jlong    *rdcc_nslotsArray = NULL;
    jlong    *nbytesArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL != rdcc_w0)
        PIN_DOUBLE_ARRAY(ENVONLY, rdcc_w0, w0Array, &isCopy, "H5Pget_chunk_cache: rdcc_w0 array not pinned");
    if (NULL != rdcc_nslots)
        PIN_LONG_ARRAY(ENVONLY, rdcc_nslots, rdcc_nslotsArray, &isCopy, "H5Pget_chunk_cache: rdcc_nslots array not pinned");
    if (NULL != rdcc_nbytes)
        PIN_LONG_ARRAY(ENVONLY, rdcc_nbytes, nbytesArray, &isCopy, "H5Pget_chunk_cache: nbytesArray array not pinned");

    {
        /* direct cast (size_t *)variable fails on 32-bit environment */
        long long rdcc_nslots_temp = *rdcc_nslotsArray;
        long long nbytes_temp = *nbytesArray;
        size_t    rdcc_nslots_t = (size_t) rdcc_nslots_temp;
        size_t    nbytes_t = (size_t) nbytes_temp;

        if ((status = H5Pget_chunk_cache((hid_t)dapl, &rdcc_nslots_t, &nbytes_t, (double *)w0Array)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        *rdcc_nslotsArray = (jlong)rdcc_nslots_t;
        *nbytesArray = (jlong)nbytes_t;
    } /* end direct cast special handling */

done:
    if (nbytesArray)
        UNPIN_LONG_ARRAY(ENVONLY, rdcc_nbytes, nbytesArray, (status < 0) ? JNI_ABORT : 0);
    if (rdcc_nslotsArray)
        UNPIN_LONG_ARRAY(ENVONLY, rdcc_nslots, rdcc_nslotsArray, (status < 0) ? JNI_ABORT : 0);
    if (w0Array)
        UNPIN_DOUBLE_ARRAY(ENVONLY, rdcc_w0, w0Array, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Pget_1chunk_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_efile_prefix
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1efile_1prefix
    (JNIEnv *env, jclass clss, jlong dapl_id, jstring prefix)
{
    const char *extFilePrefix = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == prefix)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_efile_prefix: external file prefix is NULL");

    PIN_JAVA_STRING(ENVONLY, prefix, extFilePrefix, NULL, "H5Pset_efile_prefix: external file prefix not pinned");

    if ((retVal = H5Pset_efile_prefix((hid_t)dapl_id, extFilePrefix)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (extFilePrefix)
        UNPIN_JAVA_STRING(ENVONLY, prefix, extFilePrefix);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1efile_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_efile_prefix
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1efile_1prefix
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    ssize_t  prefix_size = -1;
    char    *pre = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    if ((prefix_size = H5Pget_efile_prefix((hid_t)dapl_id, (char *)NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (pre = (char *) HDmalloc(sizeof(char) * (size_t)prefix_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_efile_prefix: memory allocation failed");

    if (H5Pget_efile_prefix((hid_t)dapl_id, (char *)pre, (size_t)prefix_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    pre[(size_t)prefix_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, pre))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_efile_prefix: out of memory - unable to construct string from UTF characters");
    }

done:
    if (pre)
        HDfree(pre);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1efile_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_append_flush
 * Signature: (JI[JLjava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1append_1flush
    (JNIEnv *env, jclass clss, jlong plist_id, jint ndims, jlongArray boundary, jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = { callback_op, op_data };
    herr_t     status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_append_flush: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_append_flush: callback_op is NULL");

    if ((status = H5Pset_append_flush((hid_t)plist_id, (unsigned)ndims, (const hsize_t *)boundary, (H5D_append_cb_t)H5D_append_cb, (void *)&wrapper)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1append_1flush */

/*
 * TODO: H5Pget_append_flush
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_view
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1view
    (JNIEnv *env, jclass clss, jlong dapl_id, jint view)
{
    UNUSED(clss);

    if (H5Pset_virtual_view((hid_t)dapl_id, (H5D_vds_view_t)view) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual_1view */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_view
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1view
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    H5D_vds_view_t virtual_view = H5D_VDS_ERROR;

    UNUSED(clss);

    if (H5Pget_virtual_view((hid_t)dapl_id, &virtual_view) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)virtual_view;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1view */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_printf_gap
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1printf_1gap
    (JNIEnv *env, jclass clss, jlong dapl_id, jlong gap_size)
{
    UNUSED(clss);

    if (H5Pset_virtual_printf_gap((hid_t)dapl_id, (hsize_t)gap_size) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual_1printf_1gap */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_printf_gap
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1printf_1gap
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    hsize_t gap_size = 0;

    UNUSED(clss);

    if (H5Pget_virtual_printf_gap((hid_t)dapl_id, &gap_size) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)gap_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1printf_1gap */

static herr_t
H5D_append_cb
    (hid_t dataset_id, hsize_t *cur_dims, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jlongArray  cur_dimsArray;
    jmethodID   mid;
    jobject     visit_callback = wrapper->visit_callback;
    jclass      cls;
    JNIEnv     *cbenv = NULL;
    void       *op_data = (void *)wrapper->op_data;
    jint        status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5D_append_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(J[JLhdf/hdf5lib/callbacks/H5D_append_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL != cur_dims) {
        if (NULL == (cur_dimsArray = CBENVPTR->NewLongArray(CBENVONLY, 2)))
            CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

        CBENVPTR->SetLongArrayRegion(CBENVONLY, cur_dimsArray, 0, 2, (const jlong *)cur_dims);
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

        status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, dataset_id, cur_dims, op_data);
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);
    }

done:
    if (CBENVONLY)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5D_append_cb */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
