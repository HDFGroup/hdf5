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

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <jni.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5vlImp.h"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_name
 * Signature: (Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1name(JNIEnv *env, jclass clss, jobject connector_name,
                                                      jlong vipl_id)
{
    const char *volName = NULL;
    hid_t       status  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == connector_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5VLregister_connector_by_name: VOL connector name is NULL");

    PIN_JAVA_STRING(ENVONLY, connector_name, volName, NULL,
                    "H5VLregister_connector_by_name: VOL connector name not pinned");

    if ((status = H5VLregister_connector_by_name(volName, (hid_t)vipl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (volName)
        UNPIN_JAVA_STRING(ENVONLY, connector_name, volName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLregister_connector_by_value
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1value(JNIEnv *env, jclass clss, jint connector_value,
                                                       jlong vipl_id)
{
    hid_t status = H5I_INVALID_HID;

    UNUSED(clss);

    if ((status = H5VLregister_connector_by_value((H5VL_class_value_t)connector_value, (hid_t)vipl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLregister_1connector_1by_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLis_connector_registered_by_name
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1name(JNIEnv *env, jclass clss, jobject connector_name)
{
    const char *volName = NULL;
    htri_t      bval    = JNI_FALSE;

    UNUSED(clss);

    if (NULL == connector_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5VLis_connector_registered_by_name: VOL connector name is NULL");

    PIN_JAVA_STRING(ENVONLY, connector_name, volName, NULL,
                    "H5VLis_connector_registered_by_name: VOL connector name not pinned");

    if ((bval = H5VLis_connector_registered_by_name(volName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (volName)
        UNPIN_JAVA_STRING(ENVONLY, connector_name, volName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLis_connector_registered_by_value
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1value(JNIEnv *env, jclass clss, jint connector_value)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if (connector_value < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5VLis_connector_registered_by_value: VOL connector value < 0");

    if ((bval = H5VLis_connector_registered_by_value((H5VL_class_value_t)connector_value)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5VLis_1connector_1registered_1by_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1id(JNIEnv *env, jclass clss, jlong obj_id)
{
    hid_t status = H5I_INVALID_HID;

    UNUSED(clss);

    if ((status = H5VLget_connector_id((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1id */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id_by_name
 * Signature: (Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1name(JNIEnv *env, jclass clss, jobject connector_name)
{
    const char *volName = NULL;
    hid_t       status  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == connector_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5VLget_connector_id_by_name: VOL connector name is NULL");

    PIN_JAVA_STRING(ENVONLY, connector_name, volName, NULL,
                    "H5VLget_connector_id_by_name: VOL connector name not pinned");

    if ((status = H5VLget_connector_id_by_name(volName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (volName)
        UNPIN_JAVA_STRING(ENVONLY, connector_name, volName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_id_by_value
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1value(JNIEnv *env, jclass clss, jint connector_value)
{
    hid_t status = H5I_INVALID_HID;

    UNUSED(clss);

    if (connector_value < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5VLget_connector_id_by_value: VOL connector value < 0");

    if ((status = H5VLget_connector_id_by_value((H5VL_class_value_t)connector_value)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1id_1by_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLget_connector_name
 * Signature: (J)Ljava/lang/String;
 */

JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5VLget_1connector_1name(JNIEnv *env, jclass clss, jlong object_id)
{
    ssize_t buf_size, status;
    char   *volName = NULL;
    jstring str     = NULL;

    UNUSED(clss);

    /* Get the length of the comment */
    if ((buf_size = H5VLget_connector_name((hid_t)object_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (buf_size > 0) {
        if (NULL == (volName = (char *)malloc(sizeof(char) * (size_t)buf_size + 1)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                   "H5VLget_connector_name: failed to allocated VOL connector name buffer");

        if ((status = H5VLget_connector_name((hid_t)object_id, volName, (size_t)buf_size + 1)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
        volName[buf_size] = '\0';

        if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, volName)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (volName)
        free(volName);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5VLget_1connector_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLclose
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5VLclose(JNIEnv *env, jclass clss, jlong connector_id)
{
    herr_t retValue = FAIL;

    UNUSED(clss);

    if ((retValue = H5VLclose((hid_t)connector_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5VLclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5VLunregister_connector
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5VLunregister_1connector(JNIEnv *env, jclass clss, jlong connector_id)
{
    herr_t retValue = FAIL;

    UNUSED(clss);

    if ((retValue = H5VLunregister_connector((hid_t)connector_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5VLunregister_1connector */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
