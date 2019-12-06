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


/* H5R: HDF5 1.12 Reference API Functions */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_object
 * Signature: (JLjava/lang/String;)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Rcreate_1object
  (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong aid)
{
    const char *refName = NULL;
    jbyteArray  ref = NULL;
    jbyte      *refBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate_object: name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, refName, NULL, "H5Rcreate_object: reference name not pinned");

    if (NULL == (refBuf = (unsigned char *) HDcalloc((size_t) 1, H5R_REF_BUF_SIZE)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rcreate_object: failed to allocate reference buffer");

    if ((status = H5Rcreate_object((hid_t)loc_id, refName, (hid_t)aid, (const H5R_ref_t *)refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (ref = ENVPTR->NewByteArray(ENVONLY, (jsize)H5R_REF_BUF_SIZE)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetByteArrayRegion(ENVONLY, ref, 0, (jsize)H5R_REF_BUF_SIZE, (jbyte *)refBuf);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (refName)
        UNPIN_JAVA_STRING(ENVONLY, name, refName);
    if (refBuf)
        HDfree(refBuf);

    return ref;
} /* end Java_hdf_hdf5lib_H5_H5Rcreate_1object */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_region
 * Signature: (JLjava/lang/String;J)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Rcreate_1region
  (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong space_id, jlong aid)
{
    const char *refName = NULL;
    jbyteArray  ref = NULL;
    jbyte      *refBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate_region: name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, refName, NULL, "H5Rcreate_region: reference name not pinned");

    if (NULL == (refBuf = (unsigned char *) HDcalloc((size_t) 1, H5R_REF_BUF_SIZE)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rcreate_region: failed to allocate reference buffer");

    if ((status = H5Rcreate_region((hid_t)loc_id, refName, space_id, (hid_t)aid, (const H5R_ref_t *)refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (ref = ENVPTR->NewByteArray(ENVONLY, (jsize)H5R_REF_BUF_SIZE)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetByteArrayRegion(ENVONLY, ref, 0, (jsize)H5R_REF_BUF_SIZE, (jbyte *)refBuf);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (refName)
        UNPIN_JAVA_STRING(ENVONLY, name, refName);
    if (refBuf)
        HDfree(refBuf);

    return ref;
} /* end Java_hdf_hdf5lib_H5_H5Rcreate_1region */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate_attr
 * Signature: (JLjava/lang/String;Ljava/lang/String;)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Rcreate_1attr
  (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jstring attr_name, jlong aid)
{
    const char *refName = NULL;
    const char *attrName = NULL;
    jbyteArray  ref = NULL;
    jbyte      *refBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate_attr: name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcreate_attr: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Rcreate_attr: attribute name not pinned");

    PIN_JAVA_STRING(ENVONLY, name, refName, NULL, "H5Rcreate_attr: reference name not pinned");

    if (NULL == (refBuf = (unsigned char *) HDcalloc((size_t) 1, H5R_REF_BUF_SIZE)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rcreate_attr: failed to allocate reference buffer");

    if ((status = H5Rcreate_attr((hid_t)loc_id, refName, attrName, (hid_t)aid, (const H5R_ref_t *)refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (ref = ENVPTR->NewByteArray(ENVONLY, (jsize)H5R_REF_BUF_SIZE)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetByteArrayRegion(ENVONLY, ref, 0, (jsize)H5R_REF_BUF_SIZE, (jbyte *)refBuf);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (refName)
        UNPIN_JAVA_STRING(ENVONLY, name, refName);
    if (refBuf)
        HDfree(refBuf);

    return ref;
} /* end Java_hdf_hdf5lib_H5_H5Rcreate_1attr */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rdestroy
 * Signature: ([B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Rdestroy
  (JNIEnv *env, jclass clss, jbyteArray ref)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rdestroy: reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rdestroy: reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rdestroy: reference buffer not pinned");

    if ((status = H5Rdestroy((const H5R_ref_t *)refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Rdestroy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_type
 * Signature: ([B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1type
  (JNIEnv *env, jclass clss, jbyteArray ref)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    H5R_type_t  ref_type = -1;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_type: reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rget_type: reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_type: reference buffer not pinned");

    if ((ref_type = H5Rget_type((const H5R_ref_t *)refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (ref_type < 0) ? JNI_ABORT : 0);

    return (jint)ref_type;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Requal
 * Signature: ([B[B)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Requal
  (JNIEnv *env, jclass clss, jbyteArray ref1, jbyteArray ref2)
{
    jboolean    isCopy;
    jbyte      *refBuf1 = NULL;
    jbyte      *refBuf2 = NULL;
    jsize       refBufLen;
    htri_t      bval = JNI_FALSE;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == ref1)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Requal: reference1 is NULL");

    if (NULL == ref2)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Requal: reference2 is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref1)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Requal: reference1 array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref1, refBuf1, &isCopy, "H5Requal: reference1 buffer not pinned");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref2)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Requal: reference2 array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref2, refBuf2, &isCopy, "H5Requal: reference2 buffer not pinned");

    if ((bval = H5Requal((const H5R_ref_t *)refBuf1, (const H5R_ref_t *)refBuf2)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    status = bval;

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (refBuf2)
        UNPIN_BYTE_ARRAY(ENVONLY, ref2, refBuf2, (status < 0) ? JNI_ABORT : 0);
    if (refBuf1)
        UNPIN_BYTE_ARRAY(ENVONLY, ref1, refBuf1, (status < 0) ? JNI_ABORT : 0);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Requal */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcopy
 * Signature: ([B)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Rcopy
  (JNIEnv *env, jclass clss, jbyteArray src_ref)
{
    jboolean    isCopy;
    jbyte      *src_refBuf = NULL;
    jsize       refBufLen;
    jbyteArray  dst_ref = NULL;
    jbyte      *dst_refBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == src_ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rcopy: src reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, src_ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Rcopy: src reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, src_ref, src_refBuf, &isCopy, "H5Rcopy: src reference buffer not pinned");

    if (NULL == (dst_refBuf = (unsigned char *) HDcalloc((size_t) 1, H5R_REF_BUF_SIZE)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rcreate_attr: failed to allocate dst reference buffer");

    if ((status = H5Rcopy((const H5R_ref_t *)src_refBuf, (const H5R_ref_t *)dst_refBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (dst_ref = ENVPTR->NewByteArray(ENVONLY, (jsize)H5R_REF_BUF_SIZE)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetByteArrayRegion(ENVONLY, dst_ref, 0, (jsize)H5R_REF_BUF_SIZE, (jbyte *)dst_refBuf);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (src_refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, src_ref, src_refBuf, (status < 0) ? JNI_ABORT : 0);
    if (dst_refBuf)
        HDfree(dst_refBuf);

    return dst_ref;
} /* end Java_hdf_hdf5lib_H5_H5Rcopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_object
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ropen_1object
  (JNIEnv *env, jclass clss, jbyteArray ref, jlong rapl_id, jlong oapl_id)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    hid_t     retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ropen_object: reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Ropen_object: reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Ropen_object: reference buffer not pinned");

    if ((retVal = H5Ropen_object((const H5R_ref_t *)refBuf, (hid_t)rapl_id, (hid_t)oapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Ropen_1object */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_region
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ropen_1region
  (JNIEnv *env, jclass clss, jbyteArray ref, jlong rapl_id, jlong oapl_id)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ropen_region: reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Ropen_region: reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Ropen_region: reference buffer not pinned");

    if ((retVal = H5Ropen_region((const H5R_ref_t *)refBuf, (hid_t)rapl_id, (hid_t)oapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Ropen_1region */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ropen_attr
 * Signature: ([BJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ropen_1attr
  (JNIEnv *env, jclass clss, jbyteArray ref, jlong rapl_id, jlong aapl_id)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jsize       refBufLen;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ropen_attr: reference is NULL");

    if ((refBufLen = ENVPTR->GetArrayLength(ENVONLY, ref)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Ropen_attr: reference array length < 0");
    }

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Ropen_attr: reference buffer not pinned");

    if ((retVal = H5Ropen_attr((const H5R_ref_t *)refBuf, (hid_t)rapl_id, (hid_t)aapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Ropen_1attr */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_obj_type3
 * Signature: ([BJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1type3
  (JNIEnv *env, jclass clss, jbyteArray ref, jlong rapl_id)
{
    H5O_type_t  object_info;
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    int         retVal = -1;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_obj_type3: reference buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_obj_type3: reference buffer not pinned");

    if ((retVal = H5Rget_obj_type3((const H5R_ref_t *)refBuf, (hid_t)rapl_id, &object_info)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (retVal >= 0)
        retVal = object_info;

done:
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1type3 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_file_name
 * Signature: ([B)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1file_1name
  (JNIEnv *env, jclass clss, jbyteArray ref)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jstring     str = NULL;
    ssize_t     buf_size;
    ssize_t     check_size = -1;
    char       *namePtr = NULL;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_file_name: reference buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_file_name: reference buffer not pinned");

    /* Get the length of the name */
    if ((buf_size = H5Rget_file_name((const H5R_ref_t *)refBuf, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rget_file_name: malloc failed");

    if ((check_size = H5Rget_file_name((const H5R_ref_t *)refBuf, namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (namePtr)
        HDfree(namePtr);
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (check_size < 0) ? JNI_ABORT : 0);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1file_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_obj_name
 * Signature: ([BJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1name
  (JNIEnv *env, jclass clss, jbyteArray ref, jlong rapl_id)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jstring     str = NULL;
    ssize_t     buf_size;
    ssize_t     check_size = -1;
    char       *namePtr = NULL;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_obj_name: reference buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_obj_name: reference buffer not pinned");

    /* Get the length of the name */
    if ((buf_size = H5Rget_obj_name((const H5R_ref_t *)refBuf, (hid_t)rapl_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rget_obj_name: malloc failed");

    if ((check_size = H5Rget_obj_name((const H5R_ref_t *)refBuf, (hid_t)rapl_id, namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (namePtr)
        HDfree(namePtr);
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (check_size < 0) ? JNI_ABORT : 0);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_attr_name
 * Signature: ([B)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1attr_1name
  (JNIEnv *env, jclass clss, jbyteArray ref)
{
    jboolean    isCopy;
    jbyte      *refBuf = NULL;
    jstring     str = NULL;
    ssize_t     buf_size;
    ssize_t     check_size = -1;
    char       *namePtr = NULL;

    UNUSED(clss);

    if (NULL == ref)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Rget_attr_name: reference buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, ref, refBuf, &isCopy, "H5Rget_attr_name: reference buffer not pinned");

    /* Get the length of the name */
    if ((buf_size = H5Rget_attr_name((const H5R_ref_t *)refBuf, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Rget_attr_name: malloc failed");

    if ((check_size = H5Rget_attr_name((const H5R_ref_t *)refBuf, namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (namePtr)
        HDfree(namePtr);
    if (refBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, ref, refBuf, (check_size < 0) ? JNI_ABORT : 0);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1attr_1name */

/* H5R: HDF5 1.8 Reference API Functions */

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
