/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "hdf5.h"
#include <jni.h>
#include <stdlib.h>
#include "h5jni.h"
#include "h5zImp.h"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zunregister
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zunregister(JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zunregister((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zunregister */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zfilter_avail
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zfilter_1avail(JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zfilter_avail((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zfilter_1avail */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zget_filter_info
 * Signature: (I)I
 */

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zget_1filter_1info(JNIEnv *env, jclass clss, jint filter)
{
    unsigned int flags = 0;

    UNUSED(clss);

    if (H5Zget_filter_info((H5Z_filter_t)filter, (unsigned *)&flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Zget_1filter_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zget_filter_class_info
 * Signature: (I)Lhdf/hdf5lib/structs/H5Z_class_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Zget_1filter_1class_1info(JNIEnv *env, jclass clss, jint filter)
{
    H5Z_class_info_t info;
    jobject          ret_obj = NULL;
    jvalue           args[7];
    jstring          name_str = NULL;
    jstring          desc_str = NULL;

    UNUSED(clss);

    memset(&info, 0, sizeof(info));

    if (H5Zget_filter_class_info((H5Z_filter_t)filter, &info) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    name_str = info.name ? ENVPTR->NewStringUTF(ENVONLY, info.name) : NULL;
    if (info.name && NULL == name_str) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zget_filter_class_info: could not create name string");
    }

    desc_str = info.description ? ENVPTR->NewStringUTF(ENVONLY, info.description) : NULL;
    if (info.description && NULL == desc_str) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zget_filter_class_info: could not create description string");
    }

    args[0].i = (jint)info.id;
    args[1].i = (jint)info.config_flags;
    args[2].l = name_str;
    args[3].l = desc_str;
    args[4].z = (jboolean)(info.has_set_config ? JNI_TRUE : JNI_FALSE);
    args[5].z = (jboolean)(info.has_get_config ? JNI_TRUE : JNI_FALSE);
    args[6].z = JNI_FALSE; /* has_blob_callbacks — reserved, always false */

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5Z_class_info_t",
                     "(IILjava/lang/String;Ljava/lang/String;ZZZ)V", args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Zget_1filter_1class_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_has_key
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1has_1key(JNIEnv *env, jclass clss, jstring params, jstring key)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_has_key: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_has_key: key string is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_has_key: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_has_key: key not pinned");

    if ((status = H5Zconfig_has_key(c_params, c_key)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Zconfig_1has_1key */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_param
 * Signature: (Ljava/lang/String;Ljava/lang/String;[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3J(JNIEnv *env, jclass clss,
                                                                                     jstring    params,
                                                                                     jstring    key,
                                                                                     jlongArray out)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    jlong      *out_arr = NULL;
    jboolean    isCopyOut;
    int64_t     c_val  = 0;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: key string is NULL");
    if (NULL == out)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: out array is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_get_param: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_get_param: key not pinned");

    if (NULL == (out_arr = ENVPTR->GetLongArrayElements(ENVONLY, out, &isCopyOut)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Zconfig_get_param: could not pin output array");

    if ((status = H5Zconfig_get_int(c_params, c_key, &c_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (status > 0)
        out_arr[0] = (jlong)c_val;

done:
    if (out_arr)
        ENVPTR->ReleaseLongArrayElements(ENVONLY, out, out_arr, status > 0 ? 0 : JNI_ABORT);
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3J */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_param
 * Signature: (Ljava/lang/String;Ljava/lang/String;[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3D(JNIEnv *env, jclass clss,
                                                                                     jstring      params,
                                                                                     jstring      key,
                                                                                     jdoubleArray out)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    jdouble    *out_arr = NULL;
    jboolean    isCopyOut;
    double      c_val  = 0.0;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: key string is NULL");
    if (NULL == out)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: out array is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_get_param: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_get_param: key not pinned");

    if (NULL == (out_arr = ENVPTR->GetDoubleArrayElements(ENVONLY, out, &isCopyOut)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Zconfig_get_param: could not pin output array");

    if ((status = H5Zconfig_get_double(c_params, c_key, &c_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (status > 0)
        out_arr[0] = (jdouble)c_val;

done:
    if (out_arr)
        ENVPTR->ReleaseDoubleArrayElements(ENVONLY, out, out_arr, status > 0 ? 0 : JNI_ABORT);
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3D */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_param
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Z)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3Z(JNIEnv *env, jclass clss,
                                                                                     jstring       params,
                                                                                     jstring       key,
                                                                                     jbooleanArray out)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    jboolean   *out_arr = NULL;
    jboolean    isCopyOut;
    hbool_t     c_val  = false;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: key string is NULL");
    if (NULL == out)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: out array is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_get_param: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_get_param: key not pinned");

    if (NULL == (out_arr = ENVPTR->GetBooleanArrayElements(ENVONLY, out, &isCopyOut)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Zconfig_get_param: could not pin output array");

    if ((status = H5Zconfig_get_bool(c_params, c_key, &c_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (status > 0)
        out_arr[0] = (jboolean)(c_val ? JNI_TRUE : JNI_FALSE);

done:
    if (out_arr)
        ENVPTR->ReleaseBooleanArrayElements(ENVONLY, out, out_arr, status > 0 ? 0 : JNI_ABORT);
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3Z */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zconfig_get_param
 * Signature: (Ljava/lang/String;Ljava/lang/String;[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2(
    JNIEnv *env, jclass clss, jstring params, jstring key, jobjectArray value)
{
    const char *c_params = NULL;
    const char *c_key    = NULL;
    jboolean    isCopy1, isCopy2;
    char       *c_valbuf = NULL;
    size_t      buf_size = H5Z_CONFIG_STRING_MAX;
    jstring     str;
    htri_t      status = -1;

    UNUSED(clss);

    if (NULL == params)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: params string is NULL");
    if (NULL == key)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Zconfig_get_param: key string is NULL");

    PIN_JAVA_STRING(ENVONLY, params, c_params, &isCopy1, "H5Zconfig_get_param: params not pinned");
    PIN_JAVA_STRING(ENVONLY, key, c_key, &isCopy2, "H5Zconfig_get_param: key not pinned");

    if (NULL == (c_valbuf = (char *)malloc(buf_size + 1)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zconfig_get_param: malloc failed");

    if ((status = H5Zconfig_get_str(c_params, c_key, c_valbuf, &buf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    c_valbuf[buf_size < H5Z_CONFIG_STRING_MAX ? buf_size : H5Z_CONFIG_STRING_MAX] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, status > 0 ? c_valbuf : ""))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Zconfig_get_param: could not create string");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, value, 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (c_valbuf)
        free(c_valbuf);
    if (c_key)
        UNPIN_JAVA_STRING(ENVONLY, key, c_key);
    if (c_params)
        UNPIN_JAVA_STRING(ENVONLY, params, c_params);

    return (jint)status;
} /* end
     Java_hdf_hdf5lib_H5_H5Zconfig_1get_1param__Ljava_lang_String_2Ljava_lang_String_2_3Ljava_lang_String_2 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
