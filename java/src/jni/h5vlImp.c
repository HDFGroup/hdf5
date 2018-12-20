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

#include "hdf5.h"
#include <jni.h>
#include <stdlib.h>
#include "h5jni.h"
#include "h5vlImp.h"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_name
 * Signature: (Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1name
  (JNIEnv *env, jclass clss, jobject connector_name, jlong vipl_id)
{
    hid_t       status = -1;
    const char *vlName;

    PIN_JAVA_STRING(connector_name, vlName);
    if (vlName != NULL) {
        status = H5VLregister_connector_by_name(vlName, (hid_t)vipl_id);

        UNPIN_JAVA_STRING(connector_name, vlName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_value
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1value
  (JNIEnv *env, jclass clss, jint connector_value, jlong vipl_id)
{
    hid_t status = H5VLregister_connector_by_value((H5VL_class_value_t)connector_value, (hid_t)vipl_id);

    if (status < 0)
        h5libraryError(env);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLis_connector_registered
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered
  (JNIEnv *env, jclass clss, jobject connector_name)
{
    htri_t      bval = JNI_FALSE;
    const char *vlName;

    PIN_JAVA_STRING(connector_name, vlName);
    if (vlName != NULL) {
        bval = H5VLis_connector_registered(vlName);

        UNPIN_JAVA_STRING(connector_name, vlName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id
 * Signature: (Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1id
  (JNIEnv *env, jclass clss, jobject connector_name)
{
    hid_t       status = -1;
    const char *vlName;

    PIN_JAVA_STRING(connector_name, vlName);
    if (vlName != NULL) {
        status = H5VLget_connector_id(vlName);

        UNPIN_JAVA_STRING(connector_name, vlName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1id */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_name
 * Signature: (J)Ljava/lang/String;
 */

JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1name
  (JNIEnv *env, jclass clss, jlong object_id)
{
    char    *vlName;
    ssize_t  buf_size;
    ssize_t  status;
    jstring  str = NULL;

    /* get the length of the comment */
    buf_size = H5VLget_connector_name((hid_t)object_id, NULL, 0);
    if (buf_size < 0) {
        H5Eprint2(H5E_DEFAULT, NULL);

        h5badArgument(env, "H5VLget_connector_name:  buf_size < 0");
    } /* end if */
    else if (buf_size > 0) {
        buf_size++; /* add extra space for the null terminator */
        vlName = (char *)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (vlName == NULL) {
            /* exception -- out of memory */
            h5outOfMemory(env, "H5VLget_connector_name:  malloc failed");
        } /* end if */
        else {
            status = H5VLget_connector_name((hid_t)object_id, vlName, (size_t)buf_size);

            if (status < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                /*  may throw OutOfMemoryError */
                str = ENVPTR->NewStringUTF(ENVPAR vlName);
                if (str == NULL) {
                    h5JNIFatalError(env, "H5VLget_connector_name:  return string not allocated");
                } /* end if */
            } /* end else */
            HDfree(vlName);
        }
    } /* end else if */

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLclose
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5VLclose
  (JNIEnv *env, jclass clss, jlong connector_id)
{
    herr_t retValue = H5VLclose((hid_t)connector_id);

    if (retValue < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5VLclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLunregister_connector
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5VLunregister_1connector
    (JNIEnv *env, jclass clss, jlong connector_id)
{
    herr_t retValue = H5VLunregister_connector((hid_t)connector_id);

    if (retValue < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5VLunregister_1connector */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
