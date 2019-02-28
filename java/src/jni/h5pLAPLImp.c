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
#include "h5pLAPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nlinks
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nlinks
    (JNIEnv *env, jclass clss, jlong lapl_id, jlong nlinks)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if (nlinks <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_nlinks: nlinks <= 0");

    if ((retVal = H5Pset_nlinks((hid_t)lapl_id, (size_t)nlinks)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1nlinks */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nlinks
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nlinks
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    size_t nlinks = 0;

    UNUSED(clss);

    if (H5Pget_nlinks((hid_t)lapl_id, &nlinks) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) nlinks;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nlinks */

/*
 * TODO: H5Pset_elink_cb
 */

/*
 * TODO: H5Pget_elink_cb
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_prefix
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1prefix
    (JNIEnv *env, jclass clss, jlong lapl_id, jstring prefix)
{
    const char *linkPrefix = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == prefix)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_elink_prefix: prefix is NULL");

    PIN_JAVA_STRING(ENVONLY, prefix, linkPrefix, NULL, "H5Pset_elink_prefix: link prefix not pinned");

    if ((retVal = H5Pset_elink_prefix((hid_t)lapl_id, linkPrefix)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (linkPrefix)
        UNPIN_JAVA_STRING(ENVONLY, prefix, linkPrefix);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_prefix
 * Signature: (J[Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1prefix
    (JNIEnv *env, jclass clss, jlong lapl_id, jobjectArray prefix)
{
    ssize_t  prefix_size = -1;
    size_t   size = 0;
    char    *pre = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    if (NULL == prefix)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_elink_prefix: prefix is NULL");

    if ((prefix_size = H5Pget_elink_prefix((hid_t)lapl_id, (char *)NULL, size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (pre = (char *) HDmalloc(sizeof(char) * (size_t) prefix_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_elink_prefix: memory allocation failed");

    if (H5Pget_elink_prefix((hid_t)lapl_id, (char *)pre, (size_t) prefix_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    pre[prefix_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, pre))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_elink_prefix: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, prefix, 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (pre)
        HDfree(pre);

    return (jlong)prefix_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_fapl
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1fapl
    (JNIEnv *env, jclass clss, jlong lapl_id, jlong fapl_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_elink_fapl((hid_t)lapl_id, (hid_t)fapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1fapl */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pget_elink_fapl
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pget_1elink_1fapl
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pget_elink_fapl((hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pget_1elink_1fapl */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_acc_flags
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1acc_1flags
    (JNIEnv *env, jclass clss, jlong lapl_id, jint flags)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_elink_acc_flags((hid_t)lapl_id, (unsigned)flags)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1acc_1flags */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_acc_flags
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1acc_1flags
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    unsigned flags;

    UNUSED(clss);

    if (H5Pget_elink_acc_flags((hid_t)lapl_id, &flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1acc_1flags */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
