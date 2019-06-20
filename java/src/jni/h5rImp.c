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
#include "h5rImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate
 * Signature: ([BJLjava/lang/String;IJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rcreate
    (JNIEnv *env, jclass clss, jbyteArray ref, jlong loc_id, jstring name, jint ref_type, jlong space_id)
{
    const char *refName = NULL;
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: reference is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: name is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: ref array length < 0");
    }

    if ((H5R_OBJECT == ref_type) && (refBufLen != H5R_OBJ_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: reference input array length != H5R_OBJ_REF_BUF_SIZE")
    else if ((H5R_DATASET_REGION == ref_type) && (refBufLen != H5R_DSET_REG_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: region reference input array length != H5R_DSET_REG_REF_BUF_SIZE")
    else if ((H5R_OBJECT != ref_type) && (H5R_DATASET_REGION != ref_type))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rcreate: unknown reference type");

    PIN_JAVA_STRING(ENVONLY, name, refName, NULL, "H5Rcreate: reference name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rcreate: reference buffer not pinned");

    if ((status = H5Rcreate(refBuf, (hid_t)loc_id, refName, (H5R_type_t)ref_type, (hid_t)space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (status < 0) ? JNI_ABORT : 0);
    if (refName)
        UNPIN_JAVA_STRING(ENVONLY, name, refName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Rcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Rdereference
 * Signature: (JJI[B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Rdereference
    (JNIEnv *env, jclass clss, jlong dataset, jlong access_list, jint ref_type, jbyteArray ref)
{
    jboolean  isCopy;
    jbyte    *refBuf = NULL;
    jsize     refBufLen;
    hid_t     retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rdereference: reference buffer is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rdereference: ref array length < 0");
    }

    if ((H5R_OBJECT == ref_type) && (refBufLen != H5R_OBJ_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rdereference: reference input array length != H5R_OBJ_REF_BUF_SIZE")
    else if ((H5R_DATASET_REGION == ref_type) && (refBufLen != H5R_DSET_REG_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rdereference: region reference input array length != H5R_DSET_REG_REF_BUF_SIZE")
    else if ((H5R_OBJECT != ref_type) && (H5R_DATASET_REGION != ref_type))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rdereference: unknown reference type");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rderefernce: reference buffer not pinned");

    if ((retVal = H5Rdereference2((hid_t)dataset, (hid_t)access_list, (H5R_type_t)ref_type, refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Rdereference */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_region
 * Signature: (JI[B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Rget_1region
    (JNIEnv *env, jclass clss, jlong dataset, jint ref_type, jbyteArray ref)
{
    jboolean  isCopy;
    jbyte    *refBuf = NULL;
    jsize     refBufLen;
    hid_t     retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (H5R_DATASET_REGION != ref_type)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_region: bad reference type");
    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_region: reference buffer is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_region: ref array length < 0");
    }

    if (refBufLen != H5R_DSET_REG_REF_BUF_SIZE)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_region: region reference input array length != H5R_DSET_REG_REF_BUF_SIZE");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_region: reference buffer not pinned");

    if ((retVal = H5Rget_region((hid_t)dataset, (H5R_type_t)ref_type, refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Rget_1region */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5G_obj_t H5Rget_obj_type
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1type
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref)
{
    H5O_type_t  object_info;
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    int         retVal = -1;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_obj_type: reference buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_obj_type: reference buffer not pinned");

    if ((retVal = H5Rget_obj_type2((hid_t)loc_id, (H5R_type_t)ref_type, refBuf, &object_info)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (retVal >= 0)
        retVal = object_info;

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    int H5Rget_obj_type2
 * Signature: (JI[B[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1type2
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref, jintArray ref_obj)
{
    jboolean  isCopy, isCopy2;
    jbyte    *refBuf = NULL;
    jint     *ref_objP = NULL;
    jint      status = -1;
    int       retVal = -1;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_obj_type: reference buffer is NULL");
    if (NULL == ref_obj)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_obj_type: reference object is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_obj_type: reference buffer not pinned");

    PIN_INT_ARRAY(ENVONLY, ref_obj, ref_objP, &isCopy2, "H5Rget_obj_type: reference object array not pinned");

    if ((status = H5Rget_obj_type2((hid_t)loc_id, (H5R_type_t)ref_type, refBuf, (H5O_type_t *)ref_objP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    retVal = ref_objP[0];

done:
    if (ref_objP)
        UNPIN_INT_ARRAY(ENVONLY, ref_obj, ref_objP, (status < 0) ? JNI_ABORT : 0);
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, JNI_ABORT);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1type2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_name
 * Signature: (JI[B[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref, jobjectArray name, jlong size)
{
    jboolean  isCopy;
    jstring   str;
    jsize     refBufLen;
    jbyte    *refBuf = NULL;
    char     *aName = NULL;
    jlong     ret_val = -1;

    UNUSED(clss);

    if (size <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: size <= 0");
    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: reference buffer is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: ref array length < 0");
    }

    if ((H5R_OBJECT == ref_type) && (refBufLen != H5R_OBJ_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: reference input array length != H5R_OBJ_REF_BUF_SIZE")
    else if ((H5R_DATASET_REGION == ref_type) && (refBufLen != H5R_DSET_REG_REF_BUF_SIZE))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: region reference input array length != H5R_DSET_REG_REF_BUF_SIZE")
    else if ((H5R_OBJECT != ref_type) && (H5R_DATASET_REGION != ref_type))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_name: unknown reference type");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_name: reference buffer not pinned");

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rget_name: failed to allocate referenced object name buffer");

    if ((ret_val = (jlong)H5Rget_name((hid_t)loc_id, (H5R_type_t)ref_type, refBuf, aName, (size_t)size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    aName[(size_t)size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetObjectArrayElement(ENVONLY, name, (jsize) 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (aName)
        HDfree(aName);
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, JNI_ABORT);

    return ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1name */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
