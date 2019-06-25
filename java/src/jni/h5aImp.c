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

#include <jni.h>
#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "h5util.h"
#include "h5jni.h"
#include "h5aImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
extern JavaVM *jvm;

typedef struct _cb_wrapper {
    jobject visit_callback;
    jobject op_data;
} cb_wrapper;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5AwriteVL_asstr(JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AwriteVL_str(JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AreadVL_asstr(JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);
static herr_t H5AreadVL_str(JNIEnv *env, hid_t attr_id, hid_t mem_id, jobjectArray buf);

static herr_t H5A_iterate_cb(hid_t g_id, const char *name, const H5A_info_t *info, void *cb_data);

/********************/
/* Local Macros     */
/********************/


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Acreate
 * Signature: (JLjava/lang/String;JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
          jlong space_id, jlong create_plist)
{
    const char *attrName = NULL;
    hid_t       attr_id = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Acreate: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, attrName, NULL, "H5Acreate: attribute name not pinned");

    if ((attr_id = H5Acreate2((hid_t)loc_id, attrName, (hid_t)type_id, (hid_t)space_id, (hid_t)create_plist, (hid_t)H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, name, attrName);

    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_name
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    const char *attrName = NULL;
    hid_t       attr_id = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aopen_name: attribute name is null");

    PIN_JAVA_STRING(ENVONLY, name, attrName, NULL, "H5Aopen_name: attribute name not pinned");

    if((attr_id = H5Aopen_name((hid_t)loc_id, attrName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, name, attrName);

    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aopen_idx
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jint idx)
{
    hid_t attr_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((attr_id = H5Aopen_idx((hid_t) loc_id, (unsigned int) idx)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)attr_id;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread
 * Signature: (JJ[BZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jbyteArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    jbyte    *readBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread: variable length type not supported");

    if (isCriticalPinning) {
        PIN_BYTE_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread: read buffer not critically pinned");
    }
    else {
        PIN_BYTE_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_BYTE_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite
 * Signature: (JJ[BZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jbyteArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jbyte    *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite: variable length type not supported");

    if (isCriticalPinning) {
        PIN_BYTE_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite: write buffer not critically pinned");
    }
    else {
        PIN_BYTE_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_BYTE_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_short
 * Signature: (JJ[SZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1short
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jshortArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    jshort   *readBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_short: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_short: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_short: variable length type not supported");

    if (isCriticalPinning) {
        PIN_SHORT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_short: read buffer not critically pinned");
    }
    else {
        PIN_SHORT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_short: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_SHORT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1short */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_short
 * Signature: (JJ[SZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1short
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jshortArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jshort   *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_short: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_short: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_short: variable length type not supported");

    if (isCriticalPinning) {
        PIN_SHORT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_short: write buffer not critically pinned");
    }
    else {
        PIN_SHORT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_short: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_SHORT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1short */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_int
 * Signature: (JJ[IZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1int
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jintArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    htri_t    data_class;
    jint     *readBuf = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (buf == NULL)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_int: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_int: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_int: variable length type not supported");

    if (isCriticalPinning) {
        PIN_INT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_int: read buffer not critically pinned");
    }
    else {
        PIN_INT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_int: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_INT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_int
 * Signature: (JJ[IZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1int
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jintArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jint     *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (buf == NULL)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_int: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_int: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_int: variable length type not supported");

    if (isCriticalPinning) {
        PIN_INT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_int: write buffer not critically pinned");
    }
    else {
        PIN_INT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_int: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_INT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_long
 * Signature: (JJ[JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1long
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jlongArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    jlong    *readBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_long: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_long: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_long: variable length type not supported");

    if (isCriticalPinning) {
        PIN_LONG_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_long: read buffer not critically pinned");
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_long: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_LONG_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_long
 * Signature: (JJ[JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1long
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jlongArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jlong    *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_long: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_long: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_long: variable length type not supported");

    if (isCriticalPinning) {
        PIN_LONG_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_long: write buffer not critically pinned");
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_long: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_LONG_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_float
 * Signature: (JJ[FZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1float
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jfloatArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    jfloat   *readBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_float: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_float: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_float: variable length type not supported");

    if (isCriticalPinning) {
        PIN_FLOAT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_float: read buffer not critically pinned");
    }
    else {
        PIN_FLOAT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_float: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_FLOAT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1float */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_float
 * Signature: (JJ[FZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1float
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jfloatArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jfloat   *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_float: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_float: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_float: variable length type not supported");

    if (isCriticalPinning) {
        PIN_FLOAT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_float: write buffer not critically pinned");
    }
    else {
        PIN_FLOAT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_float: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_FLOAT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1float */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_double
 * Signature: (JJ[DZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1double
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jdoubleArray buf, jboolean isCriticalPinning)
{
    jboolean  readBufIsCopy;
    jdouble  *readBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_double: read buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_double: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_double: variable length type not supported");

    if (isCriticalPinning) {
        PIN_DOUBLE_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_double: read buffer not critically pinned");
    }
    else {
        PIN_DOUBLE_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Aread_double: read buffer not pinned");
    }

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_DOUBLE_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1double */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_double
 * Signature: (JJ[DZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1double
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jdoubleArray buf, jboolean isCriticalPinning)
{
    jboolean  writeBufIsCopy;
    jdouble  *writeBuf = NULL;
    htri_t    data_class;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_double: write buffer is NULL");

    if ((data_class = H5Tdetect_class(mem_type_id, H5T_VLEN)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_double: variable length type not supported");

    /* Recursively detect any vlen string in type (compound, array ...) */
    if ((data_class = H5Tdetect_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (data_class)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_double: variable length type not supported");

    if (isCriticalPinning) {
        PIN_DOUBLE_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_double: write buffer not critically pinned");
    }
    else {
        PIN_DOUBLE_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Awrite_double: write buffer not pinned");
    }

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_DOUBLE_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1double */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_string
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1string
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray j_buf)
{
    jstring  jstr;
    size_t   str_len;
    size_t   pos;
    jsize    i, n;
    char    *c_buf = NULL;
    char    *cstr = NULL;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == j_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aread_string: read buffer is NULL");

    if ((n = ENVPTR->GetArrayLength(ENVONLY, j_buf)) <= 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_string: read buffer length <= 0");
    }

    if (!(str_len = H5Tget_size((hid_t)mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (cstr = (char *) HDmalloc(str_len + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aread_string: memory allocation failed");

    if (NULL == (c_buf = (char *) HDmalloc((size_t)n * str_len)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aread_string: memory allocation failed");

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, c_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0, pos = 0; i < n; i++) {
        HDmemcpy(cstr, c_buf+pos, str_len);
        cstr[str_len] = '\0';

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, cstr))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Aread_string: out of memory - unable to construct string from UTF characters");
        }

        ENVPTR->SetObjectArrayElement(ENVONLY, j_buf, i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        pos += str_len;

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (c_buf)
        HDfree(c_buf);
    if (cstr)
        HDfree(cstr);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1string */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Awrite_string
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Awrite_1string
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray j_buf)
{
    const char *utf8 = NULL;
    jstring     obj;
    size_t      i, str_len;
    jsize       n;
    char       *c_buf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == j_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Awrite_string: write buffer is NULL");

    if ((n = ENVPTR->GetArrayLength(ENVONLY, j_buf)) <= 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Awrite_string: write buffer length <= 0");
    }

    if (!(str_len = H5Tget_size((hid_t)mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (c_buf = (char *) HDmalloc((size_t)n * str_len)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Awrite_string: memory allocation failed");

    for (i = 0; i < (size_t) n; i++) {
        if (NULL == (obj = (jstring) ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)j_buf, (jsize) i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            HDmemset(&c_buf[i * str_len], 0, str_len);
            continue;
        }

        /*
         * length = ENVPTR->GetStringUTFLength(ENVONLY, obj);
         * CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
         */

        PIN_JAVA_STRING(ENVONLY, obj, utf8, NULL, "H5Awrite_string: string not pinned");

        HDstrncpy(&c_buf[i * str_len], utf8, str_len);

        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, obj);
    } /* end for */

    if ((status = H5Awrite((hid_t)attr_id, (hid_t)mem_type_id, c_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
    if (c_buf)
        HDfree(c_buf);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1string */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AreadVL
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AreadVL
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray buf)
{
    H5T_class_t type_class;
    htri_t      isStr = 0;
    htri_t      isVlenStr = 0;
    htri_t      isComplex = 0;
    htri_t      isComplex2 = 0;
    hid_t       nested_tid = H5I_INVALID_HID;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5AreadVL: read buffer is NULL");

    if ((isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (type_class == H5T_COMPOUND) {
        unsigned i;
        int      num_members;

        if ((num_members = H5Tget_nmembers(mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        for (i = 0; i < (unsigned) num_members; i++) {
            if ((nested_tid = H5Tget_member_type((hid_t)mem_type_id, i)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if ((isComplex = H5Tdetect_class((hid_t)nested_tid, H5T_COMPOUND)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if ((isComplex2 = H5Tdetect_class((hid_t)nested_tid, H5T_VLEN)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            isComplex = isComplex || isComplex2;

            if (H5Tclose(nested_tid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            nested_tid = H5I_INVALID_HID;
        }
    }
    else if (type_class == H5T_VLEN) {
        isVlenStr = 1; /* Strings created by H5Tvlen_create(H5T_C_S1) */
    }

    if (!isStr || isComplex || isVlenStr) {
        if ((status = H5AreadVL_asstr(env, (hid_t)attr_id, (hid_t)mem_type_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else if (isStr) {
        if ((status = H5AreadVL_str(env, (hid_t)attr_id, (hid_t)mem_type_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (nested_tid >= 0)
        H5Tclose(nested_tid);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1VL */

/*
 * Helper method to read in a buffer of variable-length strings from an HDF5
 * attribute. Each C-string is converted to a Java string and set in the output
 * buffer in turn.
 */
static herr_t
H5AreadVL_str
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    jstring   jstr;
    jsize     i, n;
    char    **strs = NULL;
    herr_t    status = FAIL;

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5AreadVL_str: buf length < 0");
    }

    if (NULL == (strs = (char **) HDcalloc((size_t)n, sizeof(char *))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5AreadVL_str: failed to allocate variable length string read buffer");

    if ((status = H5Aread(aid, tid, strs)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /*
     * When repeatedly reading a dataset with a large number of strs (e.g., 1,000,000 strings),
     * H5Dvlen_reclaim() may crash on Windows because the Java GC will not be able to collect
     * free space in time. Instead, we use "H5free_memory(strs[i])" to free individual strings
     * once done.
     */
    for (i = 0; i < n; i++) {
        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, strs[i])))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, buf, i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        H5free_memory(strs[i]);
        strs[i] = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (strs) {
        for (i = 0; i < n; i++) {
            if (strs[i])
                H5free_memory(strs[i]);
        }

        HDfree(strs);
    }

    return status;
} /* end H5AreadVL_str */

/*
 * Helper method to read in a buffer of variable-length (hvl_t)
 * structures from an HDF5 attribute and convert each variable-length
 * element's buffer into a Java string. Each string is then set
 * in the output buffer in turn.
 */
static herr_t
H5AreadVL_asstr
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    hsize_t      dims[H5S_MAX_RANK];
    jstring      jstr;
    h5str_t      h5str;
    size_t       typeSize;
    size_t       i;
    hid_t        sid = H5I_INVALID_HID;
    jsize        n;
    void        *readBuf = NULL;
    herr_t       status = FAIL;

    HDmemset(&h5str, 0, sizeof(h5str_t));

    /* Get size of string array */
    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5AreadVL_asstr: buf length < 0");
    }

    dims[0] = (hsize_t)n;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (readBuf = HDcalloc((size_t)n, typeSize)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5AreadVL_asstr: failed to allocate read buffer");

    if ((status = H5Aread(aid, tid, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Allocate a decent-sized initial string */
    h5str_new(&h5str, 4 * typeSize);

    if (!h5str.s)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5AreadVL_asstr: failed to allocate buffer");

    /* Convert each element to a char string */
    for (i = 0; i < (size_t) n; i++) {
        h5str.s[0] = '\0';

        if (!h5str_sprintf(ENVONLY, &h5str, aid, tid, &(((char *) readBuf)[i * typeSize]), typeSize, 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, h5str.s)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, buf, (jsize) i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (h5str.s)
        h5str_free(&h5str);
    if (readBuf) {
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, readBuf);
        HDfree(readBuf);
    }
    if (sid >= 0)
        H5Sclose(sid);

    return status;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5AwriteVL
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5AwriteVL
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray buf)
{
    H5T_class_t type_class;
    htri_t      isStr = 0;
    htri_t      isVlenStr = 0;
    htri_t      isComplex = 0;
    htri_t      isComplex2 = 0;
    hid_t       nested_tid = H5I_INVALID_HID;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5AwriteVL: write buffer is NULL");

    if ((isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (type_class == H5T_COMPOUND) {
        unsigned i;
        int      num_members;

        if ((num_members = H5Tget_nmembers(mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        for(i = 0; i < (unsigned) num_members; i++) {
            if ((nested_tid = H5Tget_member_type((hid_t)mem_type_id, i)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if ((isComplex = H5Tdetect_class((hid_t)nested_tid, H5T_COMPOUND)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if ((isComplex2 = H5Tdetect_class((hid_t)nested_tid, H5T_VLEN)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            isComplex = isComplex || isComplex2;

            if (H5Tclose(nested_tid) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            nested_tid = H5I_INVALID_HID;
        }
    }
    else if (type_class == H5T_VLEN) {
        isVlenStr = 1; /* Strings created by H5Tvlen_create(H5T_C_S1) */
    }

    if (!isStr || isComplex || isVlenStr) {
        if ((status = H5AwriteVL_asstr(env, (hid_t)attr_id, (hid_t)mem_type_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else if (isStr) {
        if ((status = H5AwriteVL_str(env, (hid_t)attr_id, (hid_t)mem_type_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (nested_tid >= 0)
        H5Tclose(nested_tid);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Awrite_1VL */

/*
 * Helper method to convert an array of Java strings into a buffer of C-strings.
 * The buffer of C-strings is then written to the HDF5 attribute specified.
 */
static herr_t
H5AwriteVL_str
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    const char  *utf8 = NULL;
    jstring      obj;
    jsize        size;
    jint         i;
    char       **writeBuf = NULL;
    herr_t       status = FAIL;

    if ((size = ENVPTR->GetArrayLength(ENVONLY, (jarray) buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5AwriteVL_str: buf length < 0");
    }

    if (NULL == (writeBuf = (char **) HDcalloc((size_t)size + 1, sizeof(char *))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5AwriteVL_str: failed to allocate variable length string write buffer")

    for (i = 0; i < size; ++i) {
        jsize length;

        if (NULL == (obj = (jstring) ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray) buf, i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            writeBuf[i] = NULL;
            continue;
        }

        length = ENVPTR->GetStringUTFLength(ENVONLY, obj);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        PIN_JAVA_STRING(ENVONLY, obj, utf8, NULL, "H5AwriteVL_str: string not pinned");

        if (NULL == (writeBuf[i] = (char *) HDmalloc((size_t)length + 1)))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5AwriteVL_str: failed to allocate string buffer");

        HDstrncpy(writeBuf[i], utf8, (size_t)length);
        writeBuf[i][length] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, obj);
    } /* end for (i = 0; i < size; ++i) */

    if ((status = H5Awrite((hid_t)aid, (hid_t)tid, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
    if (writeBuf) {
        for (i = 0; i < size; i++) {
            if (writeBuf[i])
                HDfree(writeBuf[i]);
        }

        HDfree(writeBuf);
    }

    return (jint)status;
}

/*
 * Helper method to convert an array of Java strings into a buffer of
 * variable-length (hvl_t) elements. The buffer of variable-length
 * elements is then written to the HDF5 attribute.
 */
static herr_t
H5AwriteVL_asstr
    (JNIEnv *env, hid_t aid, hid_t tid, jobjectArray buf)
{
    const char *utf8 = NULL;
    hsize_t     dims[H5S_MAX_RANK];
    jstring     jstr;
    size_t      typeSize;
    size_t      i;
    hid_t       sid = H5I_INVALID_HID;
    jsize       n;
    void       *writeBuf = NULL;
    herr_t      status = FAIL;

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5AwriteVL_asstr: buf length < 0");
    }

    dims[0] = (hsize_t)n;
    if ((sid = H5Screate_simple(1, dims, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (writeBuf = HDcalloc((size_t)n, typeSize)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5AwriteVL_asstr: failed to allocate write buffer");

    /*
     * When repeatedly writing a dataset with a large number of strs (e.g., 1,000,000 strings),
     * H5Dvlen_reclaim() may crash on Windows because the Java GC will not be able to collect
     * free space in time. Instead, we use "H5free_memory(strs[i])" to free individual strings
     * once done.
     */
    for (i = 0; i < (size_t) n; i++) {
        if (NULL == (jstr = (jstring) ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray) buf, (jsize) i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            HDmemset(&(((char *) writeBuf)[i * typeSize]), 0, typeSize);
            continue;
        }

        /*
         * length = ENVPTR->GetStringUTFLength(ENVONLY, jstr);
         * CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
         */

        PIN_JAVA_STRING(ENVONLY, jstr, utf8, NULL, "H5AwriteVL_asstr: failed to pin string buffer");

        /*
         * TODO: If the string isn't a copy, we should probably make
         * one before destroying it with h5str_convert.
         */

        if (!h5str_convert(ENVONLY, (char **) &utf8, aid, tid, &(((char *) writeBuf)[i * typeSize]), 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        UNPIN_JAVA_STRING(ENVONLY, jstr, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for (i = 0; i < n; i++) */

    if ((status = H5Awrite(aid, tid, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, jstr, utf8);
    if (writeBuf) {
        H5Dvlen_reclaim(tid, sid, H5P_DEFAULT, writeBuf);
        HDfree(writeBuf);
    }
    if (sid >= 0)
        H5Sclose(sid);

    return status;
} /* end H5AwriteVL_str */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aread_reg_ref
 * Signature: (JJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aread_1reg_1ref
    (JNIEnv *env, jclass clss, jlong attr_id, jlong mem_type_id, jobjectArray buf)
{
    hdset_reg_ref_t *ref_data = NULL;
    h5str_t          h5str;
    jstring          jstr;
    jsize            i, n;
    herr_t           status = FAIL;

    UNUSED(clss);

    HDmemset(&h5str, 0, sizeof(h5str_t));

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread_reg_ref: buf length < 0");
    }

    if (NULL == (ref_data = (hdset_reg_ref_t *) HDcalloc(1, (size_t)n * sizeof(hdset_reg_ref_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aread_reg_ref: failed to allocate read buffer");

    if ((status = H5Aread((hid_t)attr_id, (hid_t)mem_type_id, ref_data)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    h5str_new(&h5str, 1024);

    if (!h5str.s)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aread_reg_ref: failed to allocate buffer");

    for (i = 0; i < n; i++) {
        h5str.s[0] = '\0';

        if (!h5str_sprintf(ENVONLY, &h5str, (hid_t)attr_id, (hid_t)mem_type_id, ref_data[i], 0, 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, h5str.s)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, buf, i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (h5str.s)
        h5str_free(&h5str);
    if (ref_data)
        HDfree(ref_data);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aread_1reg_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_space
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1space
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Aget_space((hid_t)attr_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1space */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_type
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1type
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Aget_type((hid_t)attr_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1name
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    jstring  str = NULL;
    ssize_t  buf_size;
    char    *attrName = NULL;

    UNUSED(clss);

    if ((buf_size = H5Aget_name((hid_t)attr_id, 0, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (attrName = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aget_name: failed to allocate attribute name buffer");

    if (H5Aget_name((hid_t)attr_id, (size_t)buf_size + 1, attrName) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    attrName[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, attrName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (attrName)
        HDfree(attrName);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_num_attrs
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1num_1attrs
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Aget_num_attrs((hid_t)loc_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1num_1attrs */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Adelete
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    const char *attrName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Adelete: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, attrName, NULL, "H5Adelete: attribute name not pinned");

    if ((status = H5Adelete((hid_t)loc_id, attrName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, name, attrName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Adelete */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Aclose
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Aclose((hid_t)attr_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Acreate2
 * Signature: (JLjava/lang/String;JJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
        jlong space_id, jlong create_plist, jlong access_plist)
{
    const char *attrName = NULL;
    hid_t       status = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Acreate2: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, attrName, NULL, "H5Acreate2: attribute name not pinned");

    if ((status = H5Acreate2((hid_t)loc_id, attrName, (hid_t)type_id,
            (hid_t)space_id, (hid_t)create_plist, (hid_t)access_plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, name, attrName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen
    (JNIEnv *env, jclass clss, jlong obj_id, jstring name, jlong access_plist)

{
    const char *attrName = NULL;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aopen: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, attrName, NULL, "H5Aopen: attribute name not pinned");

    if ((retVal = H5Aopen((hid_t)obj_id, attrName, (hid_t)access_plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, name, attrName);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_idx
 * Signature: (JLjava/lang/String;IIJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint idx_type, jint order, jlong n, jlong aapl_id, jlong lapl_id)
{
    const char *objName = NULL;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aopen_by_idx: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Aopen_by_idx: object name not pinned");

    if ((retVal = H5Aopen_by_idx((hid_t)loc_id, objName, (H5_index_t)idx_type,
            (H5_iter_order_t)order, (hsize_t)n, (hid_t)aapl_id, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1by_1idx */

/*
* Class:     hdf_hdf5lib_H5
* Method:    _H5Acreate_by_name
* Signature: (JLjava/lang/String;Ljava/lang/String;JJJJJ)J
*/
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Acreate_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong type_id, jlong space_id, jlong acpl_id, jlong aapl_id, jlong lapl_id)
{
    const char *objName = NULL;
    const char *attrName = NULL;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Acreate_by_name: object name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Acreate_by_name: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Acreate_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Acreate_by_name: attribute name not pinned");

    if ((retVal = H5Acreate_by_name((hid_t)loc_id, objName, attrName, (hid_t)type_id,
            (hid_t)space_id, (hid_t)acpl_id, (hid_t)aapl_id, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Acreate_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Aexists_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    const char *objName = NULL;
    const char *attrName = NULL;
    htri_t      bval = JNI_FALSE;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aexists_by_name: object name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aexists_by_name: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Aexists_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Aexists_by_name: attribute name not pinned");

    if ((bval = H5Aexists_by_name((hid_t)loc_id, objName, attrName, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Aexists_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename
 * Signature: (JLjava/lang/String;Ljava/lang/String)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Arename
    (JNIEnv *env, jclass clss, jlong loc_id, jstring old_attr_name, jstring new_attr_name)
{
    const char *oldAttrName = NULL;
    const char *newAttrName = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == old_attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Arename: old attribute name is NULL");
    if (NULL == new_attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Arename: new attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, old_attr_name, oldAttrName, NULL, "H5Arename: old attribute name not pinned");
    PIN_JAVA_STRING(ENVONLY, new_attr_name, newAttrName, NULL, "H5Arename: new attribute name not pinned");

    if ((retVal = H5Arename((hid_t)loc_id, oldAttrName, newAttrName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newAttrName)
        UNPIN_JAVA_STRING(ENVONLY, new_attr_name, newAttrName);
    if (oldAttrName)
        UNPIN_JAVA_STRING(ENVONLY, old_attr_name, oldAttrName);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Arename */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Arename_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Arename_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring old_attr_name, jstring new_attr_name, jlong lapl_id)
{
    const char *objName = NULL;
    const char *oldAttrName = NULL;
    const char *newAttrName = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Arename_by_name: object name is NULL");
    if (NULL == old_attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Arename_by_name: old attribute name is NULL");
    if (NULL == new_attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Arename_by_name: new attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Arename_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, old_attr_name, oldAttrName, NULL, "H5Arename_by_name: old attribute name not pinned");
    PIN_JAVA_STRING(ENVONLY, new_attr_name, newAttrName, NULL, "H5Arename_by_name: new attribute name not pinned");

    if ((retVal = H5Arename_by_name((hid_t)loc_id, objName, oldAttrName, newAttrName, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newAttrName)
        UNPIN_JAVA_STRING(ENVONLY, new_attr_name, newAttrName);
    if (oldAttrName)
        UNPIN_JAVA_STRING(ENVONLY, old_attr_name, oldAttrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Arename_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_name_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1name_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    const char *objName = NULL;
    jstring     str = NULL;
    ssize_t     status_size = -1;
    char       *attrName = NULL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aget_name_by_idx: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Aget_name_by_idx: object name not pinned");

    /* Get the length of the attribute name */
    if ((status_size = H5Aget_name_by_idx((hid_t)loc_id, objName, (H5_index_t)idx_type,
            (H5_iter_order_t) order, (hsize_t) n, (char *)NULL, (size_t)0, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (attrName = (char *) HDmalloc(sizeof(char) * (size_t) status_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Aget_name_by_idx: failed to allocate buffer for attribute name");

    if ((H5Aget_name_by_idx((hid_t)loc_id, objName, (H5_index_t)idx_type,
            (H5_iter_order_t) order, (hsize_t) n, (char *)attrName, (size_t)status_size + 1, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    attrName[status_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, attrName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (attrName)
        HDfree(attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1name_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_storage_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1storage_1size
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hsize_t retVal = 0;

    UNUSED(clss);

    if (!(retVal = H5Aget_storage_size((hid_t)attr_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1storage_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    H5A_info_t ainfo;
    jobject    ret_obj = NULL;
    jvalue     args[4];

    UNUSED(clss);

    if (H5Aget_info((hid_t)attr_id, &ainfo) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].z = ainfo.corder_valid;
    args[1].j = ainfo.corder;
    args[2].i = ainfo.cset;
    args[3].j = (jlong)ainfo.data_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    const char *objName = NULL;
    H5A_info_t  ainfo;
    herr_t      status;
    jvalue      args[4];
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aget_info_by_idx: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Aget_info_by_idx: object name not pinned");

    if ((status = H5Aget_info_by_idx((hid_t)loc_id, objName, (H5_index_t)idx_type,
            (H5_iter_order_t)order, (hsize_t)n, &ainfo, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].z = ainfo.corder_valid;
    args[1].j = ainfo.corder;
    args[2].i = ainfo.cset;
    args[3].j = (jlong)ainfo.data_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args, ret_obj);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_info_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)Lhdf/hdf5lib/structs/H5A_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    const char *objName = NULL;
    const char *attrName = NULL;
    H5A_info_t  ainfo;
    herr_t      status;
    jvalue      args[4];
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aget_info_by_name: object name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aget_info_by_name: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Aget_info_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Aget_info_by_name: attribute name not pinned");

    if ((status = H5Aget_info_by_name((hid_t)loc_id, objName, attrName, &ainfo, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].z = ainfo.corder_valid;
    args[1].j = ainfo.corder;
    args[2].i = ainfo.cset;
    args[3].j = (jlong)ainfo.data_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5A_info_t", "(ZJIJ)V", args, ret_obj);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Aget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Adelete_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong lapl_id)
{
    const char *objName = NULL;
    const char *attrName = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Adelete_by_name: object name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Adelete_by_name: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Adelete_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Adelete_by_name: attribute name not pinned");

    if ((retVal = H5Adelete_by_name((hid_t)loc_id, objName, attrName, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Adelete_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aexists
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Aexists
    (JNIEnv *env, jclass clss, jlong obj_id, jstring attr_name)
{
    const char *attrName = NULL;
    htri_t      bval = JNI_FALSE;

    UNUSED(clss);

    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aexists: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Aexists: attribute name not pinned");

    if ((bval = H5Aexists((hid_t)obj_id, attrName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Aexists */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Adelete_by_idx
 * Signature: (JLjava/lang/String;IIJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Adelete_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jint idx_type, jint order, jlong n, jlong lapl_id)
{
    const char *objName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Adelete_by_idx: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Adelete_by_idx: object name not pinned");

    if ((status = H5Adelete_by_idx((hid_t)loc_id, objName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t)n, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);
} /* end Java_hdf_hdf5lib_H5_H5Adelete_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Aopen_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aopen_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring obj_name, jstring attr_name, jlong aapl_id, jlong lapl_id)

{
    const char *attrName = NULL;
    const char *objName = NULL;
    hid_t       status = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == obj_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aopen_by_name: object name is NULL");
    if (NULL == attr_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aopen_by_name: attribute name is NULL");

    PIN_JAVA_STRING(ENVONLY, obj_name, objName, NULL, "H5Aopen_by_name: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, attr_name, attrName, NULL, "H5Aopen_by_name: attribute name not pinned");

    if ((status = H5Aopen_by_name((hid_t)loc_id, objName, attrName, (hid_t)aapl_id, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (attrName)
        UNPIN_JAVA_STRING(ENVONLY, attr_name, attrName);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, obj_name, objName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Aopen_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Aget_1create_1plist
    (JNIEnv *env, jclass clss, jlong attr_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Aget_create_plist((hid_t)attr_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Aget_1create_1plist */

static herr_t
H5A_iterate_cb
    (hid_t g_id, const char *name, const H5A_info_t *info, void *cb_data) {
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jmethodID   constructor, mid;
    jobject     cb_info_t = NULL;
    jobject     visit_callback = wrapper->visit_callback;
    jstring     str;
    JNIEnv     *cbenv = NULL;
    jclass      cls;
    jvalue      args[4];
    void       *op_data = (void *)wrapper->op_data;
    jint        status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5A_iterate_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5A_info_t;Lhdf/hdf5lib/callbacks/H5A_iterate_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[0].z = info->corder_valid;
    args[1].j = info->corder;
    args[2].i = info->cset;
    args[3].j = (jlong)info->data_size;

    /* Get a reference to your class if you don't have it already */
    if (NULL == (cls = CBENVPTR->FindClass(CBENVONLY, "hdf/hdf5lib/structs/H5A_info_t")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    /* Get a reference to the constructor; the name is <init> */
    if (NULL == (constructor = CBENVPTR->GetMethodID(CBENVONLY, cls, "<init>", "(ZJIJ)V")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (cb_info_t = CBENVPTR->NewObjectA(CBENVONLY, cls, constructor, args))) {
        HDprintf("FATAL ERROR:  hdf/hdf5lib/structs/H5A_info_t: Creation failed\n");
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);
    }

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, g_id, str, cb_info_t, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5A_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate
 * Signature: (JIIJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aiterate
    (JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
          jlong idx, jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = { callback_op, op_data };
    hsize_t    start_idx = (hsize_t)idx;
    herr_t     status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aiterate: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aiterate: callback_op is NULL");

    if ((status = H5Aiterate2((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5A_operator2_t)H5A_iterate_cb, (void*)&wrapper)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aiterate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Aiterate_by_name
 * Signature: (JLjava/lang/String;IIJLjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Aiterate_1by_1name
    (JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type, jint order,
          jlong idx, jobject callback_op, jobject op_data, jlong access_id)
{
    const char *objName = NULL;
    cb_wrapper  wrapper = { callback_op, op_data };
    hsize_t     start_idx = (hsize_t)idx;
    herr_t      status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aiterate_by_name: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aiterate_by_name: callback_op is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Aiterate_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Aiterate_by_name: object name not pinned");

    if ((status = H5Aiterate_by_name((hid_t)grp_id, objName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5A_operator2_t)H5A_iterate_cb, (void*)&wrapper, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Aiterate_1by_1name */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
