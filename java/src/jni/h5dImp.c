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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "h5util.h"
#include "h5jni.h"
#include "h5dImp.h"

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

static herr_t H5DreadVL_asstr(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid,
                              hid_t xfer_plist_id, jobjectArray buf);
static herr_t H5DreadVL_str(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid,
                            hid_t xfer_plist_id, jobjectArray buf);
static herr_t H5DwriteVL_asstr(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid,
                               hid_t xfer_plist_id, jobjectArray buf);
static herr_t H5DwriteVL_str(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid,
                             hid_t xfer_plist_id, jobjectArray buf);

/********************/
/* Local Macros     */
/********************/

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dcreate
 * Signature: (JLjava/lang/String;JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dcreate(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
                                jlong space_id, jlong create_plist_id)
{
    const char *dsetName = NULL;
    hid_t       dset_id  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dcreate: dataset name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, dsetName, NULL, "H5Dcreate: dataset name not pinned");

    if ((dset_id = H5Dcreate2((hid_t)loc_id, dsetName, (hid_t)type_id, (hid_t)space_id, H5P_DEFAULT,
                              (hid_t)create_plist_id, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dsetName)
        UNPIN_JAVA_STRING(ENVONLY, name, dsetName);

    return (jlong)dset_id;
} /* end Java_hdf_hdf5lib_H5__1H5Dcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dopen
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dopen(JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    const char *dsetName = NULL;
    hid_t       dset_id  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dopen: dataset name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, dsetName, NULL, "H5Dopen: dataset name not pinned");

    if ((dset_id = H5Dopen2((hid_t)loc_id, dsetName, H5P_DEFAULT)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dsetName)
        UNPIN_JAVA_STRING(ENVONLY, name, dsetName);

    return (jlong)dset_id;
} /* end Java_hdf_hdf5lib_H5__1H5Dopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dget_space
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dget_1space(JNIEnv *env, jclass clss, jlong dataset_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Dget_space((hid_t)dataset_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Dget_1space */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dget_type
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dget_1type(JNIEnv *env, jclass clss, jlong dataset_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Dget_type((hid_t)dataset_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Dget_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dget_1create_1plist(JNIEnv *env, jclass clss, jlong dataset_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Dget_create_plist((hid_t)dataset_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Dget_1create_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread
 * Signature: (JJJJJ[BZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id, jlong mem_space_id,
                            jlong file_space_id, jlong xfer_plist_id, jbyteArray buf,
                            jboolean isCriticalPinning)
{
    jboolean    readBufIsCopy;
    jbyte      *readBuf = NULL;
    size_t      typeSize;
    H5T_class_t type_class;
    jsize       vl_array_len = 0; // Only used by vl_data_class types
    htri_t      vl_data_class;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread: read buffer is NULL");

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (vl_data_class) {
        /* Get size of data array */
        if ((vl_array_len = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
        }

        if (!(typeSize = H5Tget_size(mem_type_id)))
            H5_LIBRARY_ERROR(ENVONLY);

        if (NULL == (readBuf = calloc((size_t)vl_array_len, typeSize)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dread: failed to allocate raw VL read buffer");
    }
    else {
        if (isCriticalPinning) {
            PIN_BYTE_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                                    "H5Dread: read buffer not critically pinned");
        }
        else {
            PIN_BYTE_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread: read buffer not pinned");
        }
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, (void *)readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (vl_data_class) {
        if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        translate_rbuf(env, buf, mem_type_id, type_class, vl_array_len, readBuf);
    }

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (vl_data_class) {
            free(readBuf);
        }
        else {
            if (isCriticalPinning) {
                UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
            }
            else {
                UNPIN_BYTE_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
            }
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite
 * Signature: (JJJJJ[BZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                             jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id, jbyteArray buf,
                             jboolean isCriticalPinning)
{
    jboolean    writeBufIsCopy;
    jbyte      *writeBuf = NULL;
    size_t      typeSize;
    H5T_class_t type_class;
    jsize       vl_array_len = 0; // Only used by vl_data_class types
    htri_t      vl_data_class;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite: write buffer is NULL");

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (vl_data_class) {
        /* Get size of data array */
        if ((vl_array_len = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dwrite: write buffer length < 0");
        }

        if (!(typeSize = H5Tget_size(mem_type_id)))
            H5_LIBRARY_ERROR(ENVONLY);

        if (NULL == (writeBuf = calloc((size_t)vl_array_len, typeSize)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dwrite: failed to allocate raw VL write buffer");
    }
    else {
        if (isCriticalPinning) {
            PIN_BYTE_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                                    "H5Dwrite: write buffer not critically pinned");
        }
        else {
            PIN_BYTE_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite: write buffer not pinned");
        }
    }

    if (vl_data_class) {
        if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        translate_wbuf(ENVONLY, buf, mem_type_id, type_class, vl_array_len, writeBuf);
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (vl_data_class) {
            free(writeBuf);
        }
        else {
            if (isCriticalPinning) {
                UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
            }
            else {
                UNPIN_BYTE_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
            }
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Dclose(JNIEnv *env, jclass clss, jlong dataset_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Dclose((hid_t)dataset_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Dclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dget_storage_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Dget_1storage_1size(JNIEnv *env, jclass clss, jlong dataset_id)
{
    hsize_t retVal = 0;

    UNUSED(clss);

    if (dataset_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dget_storage_size: not a dataset");

    if (!(retVal = H5Dget_storage_size((hid_t)dataset_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Dget_1storage_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dvlen_reclaim
 * Signature: (JJJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dvlen_1reclaim(JNIEnv *env, jclass clss, jlong type_id, jlong space_id,
                                     jlong xfer_plist_id, jbyteArray buf)
{
#ifndef H5_NO_DEPRECATED_SYMBOLS
    jboolean vlenBufIsCopy;
    jbyte   *vlenBuf = NULL;
#endif
    herr_t status = FAIL;

    UNUSED(clss);

#ifdef H5_NO_DEPRECATED_SYMBOLS
    UNUSED(type_id);
    UNUSED(space_id);
    UNUSED(xfer_plist_id);
    UNUSED(buf);
    H5_UNIMPLEMENTED(ENVONLY, "H5Dvlen_reclaim: not implemented");
#else
    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dvlen_reclaim: buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, buf, vlenBuf, &vlenBufIsCopy, "H5Dvlen_reclaim: buffer not pinned");

    if ((status = H5Dvlen_reclaim((hid_t)type_id, (hid_t)space_id, (hid_t)xfer_plist_id, vlenBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
#endif

done:
#ifndef H5_NO_DEPRECATED_SYMBOLS
    if (vlenBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, buf, vlenBuf, (status < 0) ? JNI_ABORT : 0);
#endif

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dvlen_1reclaim */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_short
 * Signature: (JJJJJ[SZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1short(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                   jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                   jshortArray buf, jboolean isCriticalPinning)
{
    jboolean readBufIsCopy;
    jshort  *readBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_short: read buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_SHORT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                                 "H5Dread_short: read buffer not critically pinned");
    }
    else {
        PIN_SHORT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread_short: read buffer not pinned");
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_SHORT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1short */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_short
 * Signature: (JJJJJ[SZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1short(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                    jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                    jshortArray buf, jboolean isCriticalPinning)
{
    jboolean writeBufIsCopy;
    jshort  *writeBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_short: write buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_SHORT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                                 "H5Dwrite_short: write buffer not critically pinned");
    }
    else {
        PIN_SHORT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite_short: write buffer not pinned");
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_SHORT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1short */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_int
 * Signature: (JJJJJ[IZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1int(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                 jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id, jintArray buf,
                                 jboolean isCriticalPinning)
{
    jboolean readBufIsCopy;
    jint    *readBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_int: read buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_INT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                               "H5Dread_int: read buffer not critically pinned");
    }
    else {
        PIN_INT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread_int: read buffer not pinned");
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_INT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_int
 * Signature: (JJJJJ[IZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1int(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                  jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id, jintArray buf,
                                  jboolean isCriticalPinning)
{
    jboolean writeBufIsCopy;
    jint    *writeBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_int: write buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_INT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                               "H5Dwrite_int: write buffer not critically pinned");
    }
    else {
        PIN_INT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite_int: write buffer not pinned");
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_INT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_long
 * Signature: (JJJJJ[JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1long(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                  jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                  jlongArray buf, jboolean isCriticalPinning)
{
    jboolean readBufIsCopy;
    jlong   *readBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_long: read buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_LONG_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                                "H5Dread_long: read buffer not critically pinned");
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread_long: read buffer not pinned");
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_LONG_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_long
 * Signature: (JJJJJ[JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1long(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                   jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                   jlongArray buf, jboolean isCriticalPinning)
{
    jboolean writeBufIsCopy;
    jlong   *writeBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_long: write buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_LONG_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                                "H5Dwrite_long: write buffer not critically pinned");
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite_long: write buffer not pinned");
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_LONG_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_float
 * Signature: (JJJJJ[FZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1float(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                   jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                   jfloatArray buf, jboolean isCriticalPinning)
{
    jboolean readBufIsCopy;
    jfloat  *readBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_float: read buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_FLOAT_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                                 "H5Dread_float: read buffer not critically pinned");
    }
    else {
        PIN_FLOAT_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread_float: read buffer not pinned");
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_FLOAT_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1float */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_float
 * Signature: (JJJJJ[FZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1float(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                    jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                    jfloatArray buf, jboolean isCriticalPinning)
{
    jboolean writeBufIsCopy;
    jfloat  *writeBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_float: write buffer is NULL");

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread: readBuf length < 0");
    }

    if (isCriticalPinning) {
        PIN_FLOAT_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                                 "H5Dwrite_float: write buffer not critically pinned");
    }
    else {
        PIN_FLOAT_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite_float: write buffer not pinned");
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_FLOAT_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1float */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_double
 * Signature: (JJJJJ[DZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1double(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                    jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                    jdoubleArray buf, jboolean isCriticalPinning)
{
    jboolean readBufIsCopy;
    jdouble *readBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_double: read buffer is NULL");

    /* Get size of data array */
    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Aread: readBuf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_DOUBLE_ARRAY_CRITICAL(ENVONLY, buf, readBuf, &readBufIsCopy,
                                  "H5Dread_double: read buffer not critically pinned");
    }
    else {
        PIN_DOUBLE_ARRAY(ENVONLY, buf, readBuf, &readBufIsCopy, "H5Dread_double: read buffer not pinned");
    }

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_DOUBLE_ARRAY(ENVONLY, buf, readBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1double */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_double
 * Signature: (JJJJJ[DZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1double(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                     jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                     jdoubleArray buf, jboolean isCriticalPinning)
{
    jboolean writeBufIsCopy;
    jdouble *writeBuf = NULL;
    htri_t   vl_data_class;
    herr_t   status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_double: write buffer is NULL");

    if (ENVPTR->GetArrayLength(ENVONLY, buf) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_double: buf length < 0");
    }

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (isCriticalPinning) {
        PIN_DOUBLE_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, &writeBufIsCopy,
                                  "H5Dwrite_double: write buffer not critically pinned");
    }
    else {
        PIN_DOUBLE_ARRAY(ENVONLY, buf, writeBuf, &writeBufIsCopy, "H5Dwrite_double: write buffer not pinned");
    }

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);

        if (isCriticalPinning) {
            UNPIN_ARRAY_CRITICAL(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
        else {
            UNPIN_DOUBLE_ARRAY(ENVONLY, buf, writeBuf, (status < 0) ? JNI_ABORT : 0);
        }
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1double */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_string
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1string(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                    jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                    jobjectArray j_buf)
{
    jstring jstr;
    size_t  str_len;
    size_t  pos;
    jsize   i, n;
    char   *c_buf  = NULL;
    char   *cstr   = NULL;
    herr_t  status = FAIL;

    UNUSED(clss);

    if (NULL == j_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dread_string: read buffer is NULL");

    if ((n = ENVPTR->GetArrayLength(ENVONLY, j_buf)) <= 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread_string: read buffer length <= 0");
    }

    if (!(str_len = H5Tget_size((hid_t)mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (cstr = (char *)malloc(str_len + 1)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dread_string: memory allocation failed");

    if (NULL == (c_buf = (char *)malloc((size_t)n * str_len)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dread_string: memory allocation failed");

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, c_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0, pos = 0; i < n; i++) {
        memcpy(cstr, c_buf + pos, str_len);
        cstr[str_len] = '\0';

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, cstr))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_OUT_OF_MEMORY_ERROR(
                ENVONLY, "H5Dread_string: out of memory - unable to construct string from UTF characters");
        }

        ENVPTR->SetObjectArrayElement(ENVONLY, j_buf, i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        pos += str_len;

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (c_buf)
        free(c_buf);
    if (cstr)
        free(cstr);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1string */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_string
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1string(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                     jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                     jobjectArray j_buf)
{
    const char *utf8 = NULL;
    jstring     obj;
    size_t      i, str_len;
    jsize       n;
    char       *c_buf  = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == j_buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_string: write buffer is NULL");

    if ((n = ENVPTR->GetArrayLength(ENVONLY, j_buf)) <= 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dwrite_string: write buffer length <= 0");
    }

    if (!(str_len = H5Tget_size((hid_t)mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (c_buf = (char *)malloc((size_t)n * str_len)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dwrite_string: memory allocation failed");

    for (i = 0; i < (size_t)n; i++) {
        if (NULL == (obj = (jstring)ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)j_buf, (jsize)i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            memset(&c_buf[i * str_len], 0, str_len);
            continue;
        }

        /*
         * length = ENVPTR->GetStringUTFLength(ENVONLY, obj);
         * CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
         */

        PIN_JAVA_STRING(ENVONLY, obj, utf8, NULL, "H5Dwrite_string: string not pinned");

        strncpy(&c_buf[i * str_len], utf8, str_len);

        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, obj);
    } /* end for */

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, c_buf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
    if (c_buf)
        free(c_buf);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1string */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5DreadVL
 * Signature: (JJJJJ[java/util/ArrayList;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5DreadVL(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                              jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id, jobjectArray buf)
{
    void       *readBuf = NULL;
    size_t      typeSize;
    H5T_class_t type_class;
    jsize       vl_array_len;
    htri_t      vl_data_class;
    herr_t      status      = FAIL;
    htri_t      is_variable = 0;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5DreadVL: read buffer is NULL");

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    /* Get size of data array */
    if ((vl_array_len = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DreadVL: readBuf length < 0");
    }
    if ((is_variable = H5Tis_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (readBuf = calloc((size_t)vl_array_len, typeSize)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5DreadVL: failed to allocate raw VL read buffer");

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          (hid_t)xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    translate_rbuf(env, buf, mem_type_id, type_class, vl_array_len, readBuf);

done:
    if (readBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, readBuf);
        if (is_variable) {
            for (size_t i = 0; i < (size_t)vl_array_len; i++)
                free(((char **)readBuf)[i]);
        }
        free(readBuf);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5DreadVL */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5DwriteVL
 * Signature: (JJJJJ[java/util/ArrayList;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5DwriteVL(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                               jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id, jobjectArray buf)
{
    void       *writeBuf = NULL;
    size_t      typeSize;
    H5T_class_t type_class;
    jsize       vl_array_len; // Only used by vl_data_class types
    htri_t      vl_data_class;
    herr_t      status      = FAIL;
    htri_t      is_variable = 0;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5DwriteVL: write buffer is NULL");

    if ((vl_data_class = h5str_detect_vlen(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Get size of data array */
    if ((vl_array_len = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DwriteVL: write buffer length < 0");
    }
    if ((is_variable = H5Tis_variable_str(mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!(typeSize = H5Tget_size(mem_type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (writeBuf = calloc((size_t)vl_array_len, typeSize)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5DwriteVL: failed to allocate raw VL write buffer");

    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    translate_wbuf(ENVONLY, buf, mem_type_id, type_class, vl_array_len, writeBuf);

    if ((status = H5Dwrite((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                           (hid_t)xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (writeBuf) {
        if ((status >= 0) && vl_data_class)
            H5Treclaim(dataset_id, mem_space_id, H5P_DEFAULT, writeBuf);
        if (is_variable) {
            for (size_t i = 0; i < (size_t)vl_array_len; i++)
                free(((char **)writeBuf)[i]);
        }

        free(writeBuf);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5DwriteVL */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_VLStrings
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1VLStrings(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                       jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                       jobjectArray buf)
{
    H5T_class_t type_class;
    htri_t      isStr      = 0;
    htri_t      isVlenStr  = 0;
    htri_t      isComplex  = 0;
    htri_t      isComplex2 = 0;
    hid_t       nested_tid = H5I_INVALID_HID;
    herr_t      status     = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5DreadVLStrings: read buffer is NULL");

    if ((isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (type_class == H5T_COMPOUND) {
        unsigned i;
        int      num_members;

        if ((num_members = H5Tget_nmembers(mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        for (i = 0; i < (unsigned)num_members; i++) {
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
        if ((status = H5DreadVL_asstr(env, (hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id,
                                      (hid_t)file_space_id, (hid_t)xfer_plist_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else if (isStr) {
        if ((status = H5DreadVL_str(env, (hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id,
                                    (hid_t)file_space_id, (hid_t)xfer_plist_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (nested_tid >= 0)
        H5Tclose(nested_tid);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1VLStrings */

/*
 * Helper method to read in a buffer of variable-length strings from an HDF5
 * dataset. Each C-string is converted to a Java string and set in the output
 * buffer in turn.
 */
static herr_t
H5DreadVL_str(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hid_t xfer_plist_id,
              jobjectArray buf)
{
    jstring jstr;
    jsize   i, n;
    char  **strs   = NULL;
    herr_t  status = FAIL;

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DreadVL_str: buf length < 0");
    }

    if (NULL == (strs = (char **)calloc((size_t)n, sizeof(char *))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "H5DreadVL_str: failed to allocate variable length string read buffer");

    if ((status = H5Dread(did, tid, mem_sid, file_sid, xfer_plist_id, strs)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /*
     * When repeatedly reading a dataset with a large number of strs (e.g., 1,000,000 strings),
     * H5Treclaim() may crash on Windows because the Java GC will not be able to collect
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

        free(strs);
    }

    return status;
} /* end H5DreadVL_str */

/*
 * Helper method to read in a buffer of variable-length (hvl_t)
 * structures from an HDF5 dataset and convert each variable-length
 * element's buffer into a Java string. Each string is then set
 * in the output buffer in turn.
 */
static herr_t
H5DreadVL_asstr(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hid_t xfer_plist_id,
                jobjectArray buf)
{
    H5T_class_t tclass;
    jstring     jstr;
    h5str_t     h5str;
    bool        close_mem_space = false;
    size_t      typeSize;
    size_t      i;
    hid_t       mem_space = mem_sid;
    jsize       n;
    void       *readBuf = NULL;
    herr_t      status  = FAIL;

    memset(&h5str, 0, sizeof(h5str_t));

    if (mem_space == H5S_ALL) {
        mem_space = file_sid;

        if (mem_space == H5S_ALL) {
            /*
             * Retrieve a valid dataspace for H5Treclaim().
             */
            if ((mem_space = H5Dget_space(did)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            close_mem_space = true;

            if (H5Sselect_all(mem_space) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
        }
    }

    /* Get size of string array */
    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DreadVL_asstr: buf length < 0");
    }

    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (readBuf = calloc((size_t)n, typeSize)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5DreadVL_asstr: failed to allocate read buffer");

    if ((status = H5Dread(did, tid, mem_sid, file_sid, xfer_plist_id, readBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Allocate a decent-sized initial string */
    h5str_new(&h5str, 4 * typeSize);

    if (!h5str.s)
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5DreadVL_asstr: failed to allocate buffer");

    if ((tclass = H5Tget_class(tid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Convert each element to a char string */
    for (i = 0; i < (size_t)n; i++) {
        h5str.s[0] = '\0';

        if (!h5str_sprintf(ENVONLY, &h5str, did, tid, &(((char *)readBuf)[i * typeSize]), 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (NULL == (jstr = ENVPTR->NewStringUTF(ENVONLY, h5str.s)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->SetObjectArrayElement(ENVONLY, buf, (jsize)i, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for */

done:
    if (h5str.s)
        h5str_free(&h5str);
    if (readBuf) {
        H5Treclaim(tid, mem_space, xfer_plist_id, readBuf);
        free(readBuf);
    }
    if (close_mem_space)
        H5Sclose(mem_space);

    return status;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dwrite_VLStrings
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dwrite_1VLStrings(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                        jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                        jobjectArray buf)
{
    H5T_class_t type_class;
    htri_t      isStr      = 0;
    htri_t      isVlenStr  = 0;
    htri_t      isComplex  = 0;
    htri_t      isComplex2 = 0;
    hid_t       nested_tid = H5I_INVALID_HID;
    herr_t      status     = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5DwriteVLStrings: write buffer is NULL");

    if ((isStr = H5Tdetect_class((hid_t)mem_type_id, H5T_STRING)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if ((type_class = H5Tget_class((hid_t)mem_type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (type_class == H5T_COMPOUND) {
        unsigned i;
        int      num_members;

        if ((num_members = H5Tget_nmembers(mem_type_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        for (i = 0; i < (unsigned)num_members; i++) {
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
        if ((status = H5DwriteVL_asstr(env, (hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id,
                                       (hid_t)file_space_id, (hid_t)xfer_plist_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else if (isStr) {
        if ((status = H5DwriteVL_str(env, (hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id,
                                     (hid_t)file_space_id, (hid_t)xfer_plist_id, buf)) < 0)
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (nested_tid >= 0)
        H5Tclose(nested_tid);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dwrite_1VLStrings */

/*
 * Helper method to convert an array of Java strings into a buffer of C-strings.
 * The buffer of C-strings is then written to the HDF5 dataset specified.
 */
static herr_t
H5DwriteVL_str(JNIEnv *env, hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
               hid_t xfer_plist_id, jobjectArray buf)
{
    const char *utf8 = NULL;
    jstring     obj;
    jsize       size;
    jint        i;
    char      **writeBuf = NULL;
    herr_t      status   = FAIL;

    if ((size = ENVPTR->GetArrayLength(ENVONLY, (jarray)buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DwriteVL_str: buf length < 0");
    }

    if (NULL == (writeBuf = (char **)calloc((size_t)size + 1, sizeof(char *))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                               "H5DwriteVL_str: failed to allocate variable length string write buffer");

    for (i = 0; i < size; ++i) {
        jsize length;

        if (NULL == (obj = (jstring)ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)buf, i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            writeBuf[i] = NULL;
            continue;
        }

        length = ENVPTR->GetStringUTFLength(ENVONLY, obj);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        PIN_JAVA_STRING(ENVONLY, obj, utf8, NULL, "H5DwriteVL_str: string not pinned");

        if (NULL == (writeBuf[i] = (char *)malloc((size_t)length + 1)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5DwriteVL_str: failed to allocate string buffer");

        strncpy(writeBuf[i], utf8, (size_t)length + 1);
        writeBuf[i][length] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, obj);
    } /* end for (i = 0; i < size; ++i) */

    if ((status = H5Dwrite(dataset_id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, writeBuf)) <
        0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
    if (writeBuf) {
        for (i = 0; i < size; i++) {
            if (writeBuf[i])
                free(writeBuf[i]);
        }

        free(writeBuf);
    }

    return status;
} /* end H5DwriteVL_str */

/*
 * Helper method to convert an array of Java strings into a buffer of
 * variable-length (hvl_t) elements. The buffer of variable-length
 * elements is then written to the HDF5 dataset.
 */
static herr_t
H5DwriteVL_asstr(JNIEnv *env, hid_t did, hid_t tid, hid_t mem_sid, hid_t file_sid, hid_t xfer_plist_id,
                 jobjectArray buf)
{
    const char *utf8            = NULL;
    jstring     jstr            = NULL;
    bool        close_mem_space = false;
    size_t      typeSize;
    size_t      i;
    hid_t       mem_space = mem_sid;
    jsize       n;
    void       *writeBuf = NULL;
    herr_t      status   = FAIL;

    if (mem_space == H5S_ALL) {
        mem_space = file_sid;

        if (mem_space == H5S_ALL) {
            /*
             * Retrieve a valid dataspace for H5Treclaim().
             */
            if ((mem_space = H5Dget_space(did)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
            close_mem_space = true;

            if (H5Sselect_all(mem_space) < 0)
                H5_LIBRARY_ERROR(ENVONLY);
        }
    }

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5DwriteVL_asstr: buf length < 0");
    }

    if (!(typeSize = H5Tget_size(tid)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (writeBuf = calloc((size_t)n, typeSize)))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5AwriteVL_asstr: failed to allocate write buffer");

    for (i = 0; i < (size_t)n; ++i) {
        if (NULL == (jstr = (jstring)ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray)buf, (jsize)i))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            /*
             * If the string object was NULL, skip it.
             */
            memset(&(((char *)writeBuf)[i * typeSize]), 0, typeSize);
            continue;
        }

        PIN_JAVA_STRING(ENVONLY, jstr, utf8, NULL, "H5DwriteVL_asstr: failed to pin string buffer");

        /*
         * Make a copy of the string since h5str_convert uses strtok.
         */
        char *utf8_copy = NULL;

        jsize length = ENVPTR->GetStringUTFLength(ENVONLY, jstr);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        if (NULL == (utf8_copy = HDstrndup(utf8, (size_t)length)))
            H5_LIBRARY_ERROR(ENVONLY);

        if (!h5str_convert(ENVONLY, &utf8_copy, did, tid, &(((char *)writeBuf)[i * typeSize]), 0))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        free(utf8_copy);

        UNPIN_JAVA_STRING(ENVONLY, jstr, utf8);
        utf8 = NULL;

        ENVPTR->DeleteLocalRef(ENVONLY, jstr);
    } /* end for (i = 0; i < size; ++i) */

    if ((status = H5Dwrite(did, tid, mem_sid, file_sid, xfer_plist_id, writeBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, jstr, utf8);
    if (writeBuf) {
        H5Treclaim(tid, mem_space, xfer_plist_id, writeBuf);
        free(writeBuf);
    }
    if (close_mem_space)
        H5Sclose(mem_space);

    return status;
} /* end H5DwriteVL_asstr */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dread_reg_ref
 * Signature: (JJJJJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dread_1reg_1ref(JNIEnv *env, jclass clss, jlong dataset_id, jlong mem_type_id,
                                      jlong mem_space_id, jlong file_space_id, jlong xfer_plist_id,
                                      jobjectArray buf)
{
    H5R_ref_t *ref_data = NULL;
    h5str_t    h5str;
    jstring    jstr;
    jsize      i, n;
    herr_t     status = FAIL;

    UNUSED(clss);

    memset(&h5str, 0, sizeof(h5str_t));

    if ((n = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dread_reg_ref: buf length < 0");
    }

    if (NULL == (ref_data = (H5R_ref_t *)calloc(1, (size_t)n * sizeof(H5R_ref_t))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dread_reg_ref: failed to allocate read buffer");

    if ((status = H5Dread((hid_t)dataset_id, (hid_t)mem_type_id, (hid_t)mem_space_id, (hid_t)file_space_id,
                          xfer_plist_id, ref_data)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    h5str_new(&h5str, 1024);

    if (!h5str.s)
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dread_reg_ref: failed to allocate buffer");

    for (i = 0; i < n; i++) {
        h5str.s[0] = '\0';

        if (!h5str_sprintf(ENVONLY, &h5str, (hid_t)dataset_id, (hid_t)mem_type_id, (void *)&ref_data[i], 0))
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
        free(ref_data);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Dread_1reg_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dcreate2
 * Signature: (JLjava/lang/String;JJJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dcreate2(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type_id,
                                 jlong space_id, jlong link_plist_id, jlong create_plist_id,
                                 jlong access_plist_id)
{
    const char *dsetName = NULL;
    hid_t       dset_id  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dcreate2: dataset name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, dsetName, NULL, "H5Dcreate2: dataset name not pinned");

    if ((dset_id = H5Dcreate2((hid_t)loc_id, dsetName, (hid_t)type_id, (hid_t)space_id, (hid_t)link_plist_id,
                              (hid_t)create_plist_id, (hid_t)access_plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dsetName)
        UNPIN_JAVA_STRING(ENVONLY, name, dsetName);

    return (jlong)dset_id;
} /* end Java_hdf_hdf5lib_H5__1H5Dcreate2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dopen2
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dopen2(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist)
{
    const char *dsetName = NULL;
    hid_t       dset_id  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dopen2: dataset name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, dsetName, NULL, "H5Dopen2: dataset name not pinned");

    if ((dset_id = H5Dopen2((hid_t)loc_id, dsetName, (hid_t)access_plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dsetName)
        UNPIN_JAVA_STRING(ENVONLY, name, dsetName);

    return (jlong)dset_id;
} /* end Java_hdf_hdf5lib_H5__1H5Dopen2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Dcreate_anon
 * Signature: (JJJJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Dcreate_1anon(JNIEnv *env, jclass clss, jlong loc_id, jlong type_id, jlong space_id,
                                      jlong dcpl_id, jlong dapl_id)
{
    hid_t dset_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((dset_id = H5Dcreate_anon((hid_t)loc_id, (hid_t)type_id, (hid_t)space_id, (hid_t)dcpl_id,
                                  (hid_t)dapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)dset_id;
} /* end Java_hdf_hdf5lib_H5__1H5Dcreate_1anon */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dget_space_status
 * Signature: (J)I;
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Dget_1space_1status(JNIEnv *env, jclass clss, jlong loc_id)
{
    H5D_space_status_t space_status = H5D_SPACE_STATUS_ERROR;

    UNUSED(clss);

    if (H5Dget_space_status((hid_t)loc_id, &space_status) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)space_status;
} /* end Java_hdf_hdf5lib_H5_H5Dget_1space_1status */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dget_access_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Dget_1access_1plist(JNIEnv *env, jclass clss, jlong loc_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Dget_access_plist((hid_t)loc_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Dget_1access_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dget_offset
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Dget_1offset(JNIEnv *env, jclass clss, jlong loc_id)
{
    haddr_t offset = HADDR_UNDEF;

    UNUSED(clss);

    if (HADDR_UNDEF == (offset = H5Dget_offset((hid_t)loc_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)offset;
} /* end Java_hdf_hdf5lib_H5_H5Dget_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dvlen_get_buf_size
 * Signature: (JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Dvlen_1get_1buf_1size(JNIEnv *env, jclass clss, jlong dataset_id, jlong type_id,
                                            jlong space_id)
{
    hsize_t sz = 0;

    UNUSED(clss);

    if (H5Dvlen_get_buf_size((hid_t)dataset_id, (hid_t)type_id, (hid_t)space_id, &sz) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)sz;
} /* end Java_hdf_hdf5lib_H5_H5Dvlen_1get_1buf_1size_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dfill
 * Signature: ([BJ[BJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Dfill(JNIEnv *env, jclass clss, jbyteArray fill, jlong fill_type_id, jbyteArray buf,
                            jlong buf_type_id, jlong space_id)
{
    jboolean isCopy1;
    jboolean isCopy2;
    herr_t   status = FAIL;
    jbyte   *fillP  = NULL;
    jbyte   *buffP  = NULL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dfill: buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, buf, buffP, &isCopy2, "H5Dfill: buffer not pinned");

    if (fill) {
        PIN_BYTE_ARRAY(ENVONLY, fill, fillP, &isCopy1, "H5Dfill: fill buffer not pinned");
    }

    if ((status = H5Dfill((const void *)fillP, (hid_t)fill_type_id, (void *)buffP, (hid_t)buf_type_id,
                          (hid_t)space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (fillP) {
        UNPIN_BYTE_ARRAY(ENVONLY, fill, fillP, JNI_ABORT);
        fillP = NULL;
    }

done:
    if (fillP)
        UNPIN_BYTE_ARRAY(ENVONLY, fill, fillP, JNI_ABORT);
    if (buffP)
        UNPIN_BYTE_ARRAY(ENVONLY, buf, buffP, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Dfill */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dset_extent
 * Signature: (J[J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Dset_1extent(JNIEnv *env, jclass clss, jlong loc_id, jlongArray buf)
{
    jboolean isCopy;
    hsize_t *dims = NULL;
    herr_t   status;
    jsize    rank;
    jlong   *dimsBuf = NULL;
    int      i       = 0;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Dset_extent: buffer is NULL");

    if ((rank = ENVPTR->GetArrayLength(ENVONLY, buf)) <= 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Dset_extent: buf length <= 0");
    }

    PIN_LONG_ARRAY(ENVONLY, buf, dimsBuf, &isCopy, "H5Dset_extent: buffer not pinned");

    if (NULL == (dims = (hsize_t *)malloc((size_t)rank * sizeof(hsize_t))))
        H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Dset_extent: failed to allocate dataset dimension buffer");

    for (i = 0; i < rank; i++)
        dims[i] = (hsize_t)dimsBuf[i];

    if ((status = H5Dset_extent((hid_t)loc_id, (hsize_t *)dims)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dims)
        free(dims);
    if (dimsBuf)
        UNPIN_LONG_ARRAY(ENVONLY, buf, dimsBuf, JNI_ABORT);
} /* end Java_hdf_hdf5lib_H5_H5Dset_1extent */

static herr_t
H5D_iterate_cb(void *elem, hid_t elem_id, unsigned ndim, const hsize_t *point, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jbyteArray  elemArray;
    jlongArray  pointArray;
    jmethodID   mid;
    jobject     visit_callback = wrapper->visit_callback;
    jclass      cls;
    JNIEnv     *cbenv  = NULL;
    jint        status = FAIL;
    jsize       size;
    void       *op_data = (void *)wrapper->op_data;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5D_iterate_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback",
                                             "([BJI[JLhdf/hdf5lib/callbacks/H5D_iterate_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == elem)
        H5_NULL_ARGUMENT_ERROR(CBENVONLY, "H5D_iterate_cb: element buffer is NULL");
    if (NULL == point)
        H5_NULL_ARGUMENT_ERROR(CBENVONLY, "H5D_iterate_cb: point is NULL");

    if (!(size = (jsize)H5Tget_size(elem_id)))
        H5_LIBRARY_ERROR(CBENVONLY);

    if (NULL == (elemArray = CBENVPTR->NewByteArray(CBENVONLY, size)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    CBENVPTR->SetByteArrayRegion(CBENVONLY, elemArray, 0, size, (jbyte *)elem);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (pointArray = CBENVPTR->NewLongArray(CBENVONLY, 2)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    CBENVPTR->SetLongArrayRegion(CBENVONLY, pointArray, 0, 2, (const jlong *)point);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, (void *)elemArray, elem_id, ndim,
                                     pointArray, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    CBENVPTR->GetByteArrayRegion(CBENVONLY, elemArray, 0, size, (jbyte *)elem);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5D_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Diterate
 * Signature: ([BJJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Diterate(JNIEnv *env, jclass clss, jbyteArray buf, jlong buf_type, jlong space,
                               jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = {callback_op, op_data};
    jboolean   isCopy;
    jbyte     *iterBuf = NULL;
    herr_t     status  = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Diterate: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Diterate: callback_op is NULL");
    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Diterate: buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, buf, iterBuf, &isCopy, "H5Diterate: buffer not pinned");

    if ((status = H5Diterate((void *)iterBuf, (hid_t)buf_type, (hid_t)space, (H5D_operator_t)H5D_iterate_cb,
                             (void *)&wrapper)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (iterBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, buf, iterBuf,
                         (status < 0) ? (JNI_ABORT) : ((isCopy == JNI_TRUE) ? 0 : JNI_ABORT));

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Diterate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Dflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Dflush(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Dflush((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Drefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Drefresh(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Drefresh((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
