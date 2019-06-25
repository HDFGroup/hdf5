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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5tImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Topen2
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Topen2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist)
{
    const char *datatypeName = NULL;
    hid_t       status = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Topen2: datatype name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, datatypeName, NULL, "H5Topen2: datatype name not pinned");

    if ((status = H5Topen2((hid_t)loc_id, datatypeName, (hid_t)access_plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (datatypeName)
        UNPIN_JAVA_STRING(ENVONLY, name, datatypeName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Topen2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tcommitted
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Tcommitted
    (JNIEnv *env, jclass clss, jlong type_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Tcommitted(type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Tcommitted */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tcreate
 * Signature: (IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tcreate
    (JNIEnv *env, jclass clss, jint dclass, jlong size)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tcreate((H5T_class_t )dclass, (size_t)size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tcopy
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tcopy
    (JNIEnv *env, jclass clss, jlong type_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tcopy((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tcopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tequal
 * Signature: (JJ)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Tequal
    (JNIEnv *env, jclass clss, jlong type_id1, jlong type_id2)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Tequal((hid_t)type_id1, (hid_t)type_id2)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Tequal */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tlock
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tlock
    (JNIEnv *env, jclass clss, jlong type_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tlock((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tlock */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_class
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1class
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_class_t retVal = H5T_NO_CLASS;

    UNUSED(clss);

    if (H5T_NO_CLASS == (retVal = H5Tget_class((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1size
    (JNIEnv *env, jclass clss, jlong type_id)
{
    size_t retVal = 0;

    UNUSED(clss);

    if (!(retVal = H5Tget_size((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1size*/

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1size
    (JNIEnv *env, jclass clss, jlong type_id, jlong size)
{
    size_t tsize = (size_t)size;
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_size((hid_t)type_id, tsize)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end ifJava_hdf_hdf5lib_H5_H5Tset_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1order
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_order_t retVal = H5T_ORDER_ERROR;

    UNUSED(clss);

    if (H5T_ORDER_ERROR == (retVal = H5Tget_order((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1order
    (JNIEnv *env, jclass clss, jlong type_id, jint order)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_order((hid_t)type_id, (H5T_order_t)order)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_precision
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1precision
    (JNIEnv *env, jclass clss, jlong type_id)
{
    size_t retVal = 0;

    UNUSED(clss);

    if (!(retVal = H5Tget_precision((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1precision */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_precision_long
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1precision_1long
    (JNIEnv *env, jclass clss, jlong type_id)
{
    size_t retVal = 0;

    UNUSED(clss);

    if (!(retVal = H5Tget_precision((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1precision_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_precision
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1precision
    (JNIEnv *env, jclass clss, jlong type_id, jlong precision)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_precision((hid_t)type_id, (size_t)precision)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1precision */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_offset
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1offset
    (JNIEnv *env, jclass clss, jlong type_id)
{
    int retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Tget_offset((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_offset
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1offset
    (JNIEnv *env, jclass clss, jlong type_id, jlong offset)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_offset((hid_t)type_id, (size_t)offset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_pad
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1pad
    (JNIEnv *env, jclass clss, jlong type_id, jintArray pad)
{
    jboolean  isCopy;
    jint     *P = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == pad)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_pad: pad is NULL");

    PIN_INT_ARRAY(ENVONLY, pad, P, &isCopy, "H5Tget_pad: pad not pinned");

    if ((status = H5Tget_pad((hid_t)type_id, (H5T_pad_t *)&(P[0]), (H5T_pad_t *)&(P[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (P)
        UNPIN_INT_ARRAY(ENVONLY, pad, P, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1pad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_pad
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1pad
    (JNIEnv *env, jclass clss, jlong type_id, jint lsb, jint msb)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_pad((hid_t)type_id, (H5T_pad_t)lsb, (H5T_pad_t)msb)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1pad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_sign
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1sign
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_sign_t retVal = H5T_SGN_ERROR;

    UNUSED(clss);

    if (H5T_SGN_ERROR == (retVal = H5Tget_sign((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1sign */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_sign
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1sign
    (JNIEnv *env, jclass clss, jlong type_id, jint sign)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_sign((hid_t)type_id, (H5T_sign_t)sign)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1sign */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_fields_int
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1fields_1int
    (JNIEnv *env, jclass clss, jlong type_id, jintArray fields)
{
    jboolean  isCopy;
    jsize     arrLen;
    jint     *P = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == fields)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields_int: fields is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, fields)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields_int: fields array length < 0");
    }
    if (arrLen < 5)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields_int: fields input array < order 5");

    PIN_INT_ARRAY(ENVONLY, fields, P, &isCopy, "H5Tget_fields_int: fields not pinned");

    if ((status = H5Tget_fields((hid_t)type_id, (size_t *)&(P[0]), (size_t *)&(P[1]), (size_t *)&(P[2]), (size_t *)&(P[3]), (size_t *)&(P[4]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (P)
        UNPIN_INT_ARRAY(ENVONLY, fields, P, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1fields_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_fields
 * Signature: (J[J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1fields
    (JNIEnv *env, jclass clss, jlong type_id, jlongArray fields)
{
    jboolean  isCopy;
    jlong    *fieldsArray = NULL;
    jsize     arrLen;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == fields)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields: fields is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, fields)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields: fields array length < 0");
    }
    if (arrLen < 5)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tget_fields: fields input array < order 5");

    PIN_LONG_ARRAY(ENVONLY, fields, fieldsArray, &isCopy, "H5Tget_fields: fields not pinned");

    { /* Direct cast (size_t *)variable fails on 32-bit environment */
        size_t spos_t = 0;
        size_t epos_t = 0;
        size_t esize_t = 0;
        size_t mpos_t = 0;
        size_t msize_t = 0;
        long long fields_temp = *(&fieldsArray[0]);
        spos_t = (size_t)fields_temp;
        fields_temp = *(&fieldsArray[1]);
        epos_t = (size_t)fields_temp;
        fields_temp = *(&fieldsArray[2]);
        esize_t = (size_t)fields_temp;
        fields_temp = *(&fieldsArray[3]);
        mpos_t = (size_t)fields_temp;
        fields_temp = *(&fieldsArray[4]);
        msize_t = (size_t)fields_temp;

        if ((status = H5Tget_fields((hid_t)type_id, &spos_t, &epos_t, &esize_t, &mpos_t, &msize_t)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        *(&fieldsArray[0]) = (jlong)spos_t;
        *(&fieldsArray[1]) = (jlong)epos_t;
        *(&fieldsArray[2]) = (jlong)esize_t;
        *(&fieldsArray[3]) = (jlong)mpos_t;
        *(&fieldsArray[4]) = (jlong)msize_t;
    } /* end direct cast special handling */

done:
    if (fieldsArray)
        UNPIN_LONG_ARRAY(ENVONLY, fields, fieldsArray, (status < 0) ? JNI_ABORT : 0);

    return;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1fields */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_fields
 * Signature: (JJJJJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1fields
    (JNIEnv *env, jclass clss, jlong type_id, jlong spos, jlong epos,
        jlong esize, jlong mpos, jlong msize)
{
    UNUSED(clss);

    if (H5Tset_fields((hid_t)type_id, (size_t)spos, (size_t)epos, (size_t)esize, (size_t)mpos, (size_t)msize) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1fields */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_ebias
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1ebias
    (JNIEnv *env, jclass clss, jlong type_id)
{
    size_t retVal = 0;

    UNUSED(clss);

    if (!(retVal =  H5Tget_ebias((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1ebias */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_ebias_long
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1ebias_1long
    (JNIEnv *env, jclass clss, jlong type_id)
{
    size_t retVal = 0;

    UNUSED(clss);

    if (!(retVal = H5Tget_ebias((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1ebias_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_ebias
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1ebias
    (JNIEnv *env, jclass clss, jlong type_id, jlong ebias)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_ebias((hid_t)type_id, (size_t)ebias)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1ebias */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_norm
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1norm
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_norm_t retVal = H5T_NORM_ERROR;

    UNUSED(clss);

    if (H5T_NORM_ERROR == (retVal = H5Tget_norm((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1norm */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_norm
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1norm
    (JNIEnv *env, jclass clss, jlong type_id, jint norm)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_norm((hid_t)type_id, (H5T_norm_t )norm)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1norm */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_inpad
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1inpad
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_pad_t retVal = H5T_PAD_ERROR;

    UNUSED(clss);

    if (H5T_PAD_ERROR == (retVal = H5Tget_inpad((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1inpad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_inpad
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1inpad
    (JNIEnv *env, jclass clss, jlong type_id, jint inpad)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_inpad((hid_t)type_id, (H5T_pad_t) inpad)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1inpad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_cset
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1cset
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_cset_t retVal = H5T_CSET_ERROR;

    UNUSED(clss);

    if (H5T_CSET_ERROR == (retVal = H5Tget_cset((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1cset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_cset
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1cset
    (JNIEnv *env, jclass clss, jlong type_id, jint cset)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_cset((hid_t)type_id, (H5T_cset_t)cset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1cset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_strpad
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1strpad
    (JNIEnv *env, jclass clss, jlong type_id)
{
    H5T_str_t retVal = H5T_STR_ERROR;

    UNUSED(clss);

    if (H5T_STR_ERROR == (retVal = H5Tget_strpad((hid_t)type_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1strpad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_strpad
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1strpad
    (JNIEnv *env, jclass clss, jlong type_id, jint strpad)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tset_strpad((hid_t)type_id, (H5T_str_t)strpad)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1strpad */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_nmembers
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1nmembers
    (JNIEnv *env, jclass clss, jlong type_id)
{
    int retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Tget_nmembers((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1nmembers */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_name
 * Signature: (JI)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1name
    (JNIEnv *env, jclass clss, jlong type_id, jint field_idx)
{
    char    *member_name = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    if (NULL == (member_name = H5Tget_member_name((hid_t)type_id, (unsigned)field_idx)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, member_name)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (member_name)
        H5free_memory(member_name);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_index
 * Signature: (JLjava/lang/String)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1index
    (JNIEnv *env, jclass clss, jlong type_id, jstring field_name)
{
    const char *datatypeName = NULL;
    int         index = -1;

    UNUSED(clss);

    if (NULL == field_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_member_index: field name is NULL");

    PIN_JAVA_STRING(ENVONLY, field_name, datatypeName, NULL, "H5Tget_member_index: field name not pinned");

    if ((index = H5Tget_member_index((hid_t)type_id, datatypeName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (datatypeName)
        UNPIN_JAVA_STRING(ENVONLY, field_name, datatypeName);

    return (jint)index;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1index */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_type
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tget_1member_1type
    (JNIEnv *env, jclass clss, jlong type_id, jint field_idx)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tget_member_type((hid_t)type_id, (unsigned)field_idx)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tget_1member_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_offset
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1offset
    (JNIEnv *env, jclass clss, jlong type_id, jint memno)
{
    UNUSED(env);
    UNUSED(clss);

    return (jlong)H5Tget_member_offset((hid_t)type_id, (unsigned)memno);
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_class
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1class
    (JNIEnv *env, jclass clss, jlong type_id, jint memno)
{
    H5T_class_t retVal = H5T_NO_CLASS;

    UNUSED(clss);

    if (H5T_NO_CLASS == (retVal = H5Tget_member_class((hid_t)type_id, (unsigned)memno)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tinsert
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tinsert
    (JNIEnv *env, jclass clss, jlong type_id, jstring name, jlong offset, jlong field_id)
{
    const char *datatypeName = NULL;
    long        off = (long)offset;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tinsert: datatype name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, datatypeName, NULL, "H5Tinsert: datatype name not pinned");

    if ((status = H5Tinsert((hid_t)type_id, datatypeName, (size_t)off, field_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (datatypeName)
        UNPIN_JAVA_STRING(ENVONLY, name, datatypeName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tinsert */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tpack
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tpack
    (JNIEnv *env, jclass clss, jlong type_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tpack((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Tpack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Tclose
    (JNIEnv *env, jclass clss, jlong type_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Tclose((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tvlen_create
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tvlen_1create
    (JNIEnv *env, jclass clss, jlong base_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tvlen_create((hid_t)base_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tvlen_1create */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tset_tag
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tset_1tag
    (JNIEnv *env, jclass clss, jlong type, jstring tag)
{
    const char *tagBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == tag)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tset_tag: tag is NULL");

    PIN_JAVA_STRING(ENVONLY, tag, tagBuf, NULL, "H5Tset_tag: tag buffer not pinned");

    if ((status = H5Tset_tag((hid_t)type, tagBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (tagBuf)
        UNPIN_JAVA_STRING(ENVONLY, tag, tagBuf);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tset_1tag */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_tag
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1tag
    (JNIEnv *env, jclass clss, jlong type)
{
    jstring  str = NULL;
    char    *tag = NULL;

    UNUSED(clss);

    if (NULL == (tag = H5Tget_tag((hid_t)type)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, tag)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (tag)
        H5free_memory(tag);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1tag */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_super
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tget_1super
    (JNIEnv *env, jclass clss, jlong type_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tget_super((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tget_1super */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tenum_create
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tenum_1create
    (JNIEnv *env, jclass clss, jlong base_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Tenum_create((hid_t)base_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tenum_1create */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_insert_int
 * Signature: (JLjava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1insert_1int
    (JNIEnv *env, jclass clss, jlong type_id, jstring name, jintArray value)
{
    const char *memberName = NULL;
    jboolean    isCopy;
    jint       *intBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_insert_int: value is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_insert_int: member name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, memberName, NULL, "H5Tenum_insert_int: member name not pinned");

    PIN_INT_ARRAY(ENVONLY, value, intBuf, &isCopy, "H5Tenum_insert_int: int buffer not pinned");

    if ((status = H5Tenum_insert((hid_t)type_id, memberName, intBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (intBuf)
        UNPIN_INT_ARRAY(ENVONLY, value, intBuf, (status < 0) ? JNI_ABORT : 0);
    if (memberName)
        UNPIN_JAVA_STRING(ENVONLY, name, memberName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1insert_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_insert
 * Signature: (JLjava/lang/String;[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1insert
    (JNIEnv *env, jclass clss, jlong type_id, jstring name, jbyteArray value)
{
    const char *memberName = NULL;
    jboolean    isCopy;
    jbyte      *memberBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_insert: value is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_insert: member name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, memberName, NULL, "H5Tenum_insert: member name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, value, memberBuf, &isCopy, "H5Tenum_insert: member buffer not pinned");

    if ((status = H5Tenum_insert((hid_t)type_id, memberName, memberBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (memberBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, value, memberBuf, (status < 0) ? JNI_ABORT : 0);
    if (memberName)
        UNPIN_JAVA_STRING(ENVONLY, name, memberName);
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1insert */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_nameof_int
 * Signature: (J[I[Ljava/lang/String;I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1nameof_1int
    (JNIEnv *env, jclass clss, jlong type_id, jintArray value, jobjectArray name,
          jint size)
{
    jboolean  isCopy;
    jstring   str;
    jint     *intP = NULL;
    char     *nameP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_nameof_int: value is NULL");
    if (size <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tenum_nameof_int: name size < 0");

    if (NULL == (nameP = (char *) HDmalloc(sizeof(char) * (size_t)size)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tenum_nameof_int: failed to allocate name buffer");

    PIN_INT_ARRAY(ENVONLY, value, intP, &isCopy, "H5Tenum_nameof_int: value not pinned");

    if ((status = H5Tenum_nameof((hid_t)type_id, intP, nameP, (size_t)size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    nameP[(size_t)size - 1] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, nameP)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (intP)
        UNPIN_INT_ARRAY(ENVONLY, value, intP, JNI_ABORT);
    if (nameP)
        HDfree(nameP);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1nameof_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_nameof
 * Signature: (J[BJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1nameof
    (JNIEnv *env, jclass clss, jlong type_id, jbyteArray value, jlong size)
{
    jboolean  isCopy;
    jstring   str = NULL;
    jbyte    *byteP = NULL;
    char     *nameP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (size <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tenum_nameof: name size < 0");
    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_nameof: value is NULL");

    if (NULL == (nameP = (char *) HDmalloc(sizeof(char) * (size_t)size)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tenum_nameof: failed to allocate name buffer");

    PIN_BYTE_ARRAY(ENVONLY, value, byteP, &isCopy, "H5Tenum_nameof: value not pinned");

    if ((status = H5Tenum_nameof((hid_t)type_id, byteP, nameP, (size_t)size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    nameP[(size_t)size - 1] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, nameP)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (byteP)
        UNPIN_BYTE_ARRAY(ENVONLY, value, byteP, JNI_ABORT);
    if (nameP)
        HDfree(nameP);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1nameof */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_valueof_int
 * Signature: (JLjava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1valueof_1int
    (JNIEnv *env, jclass clss, jlong type_id, jstring name, jintArray value)
{
    const char *enumValue = NULL;
    jboolean    isCopy;
    jint       *enumValueBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_valueof_int: value is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_valueof_int: enum value name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, enumValue, NULL, "H5Tenum_valueof_int: enum value not pinned");

    PIN_INT_ARRAY(ENVONLY, value, enumValueBuf, &isCopy, "H5Tenum_valueof_int: enum value buffer not pinned");

    if ((status = H5Tenum_valueof((hid_t)type_id, enumValue, enumValueBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (enumValueBuf)
        UNPIN_INT_ARRAY(ENVONLY, value, enumValueBuf, (status < 0) ? JNI_ABORT : 0);
    if (enumValue)
        UNPIN_JAVA_STRING(ENVONLY, name, enumValue);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1valueof_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tenum_valueof
 * Signature: (JLjava/lang/String;[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tenum_1valueof
    (JNIEnv *env, jclass clss, jlong type_id, jstring name, jbyteArray value)
{
    const char *enumValue = NULL;
    jboolean    isCopy;
    jbyte      *enumValueBuf = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_valueof: value is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tenum_valueof: enum value name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, enumValue, NULL, "H5Tenum_valueof: enum value not pinned");

    PIN_BYTE_ARRAY(ENVONLY, value, enumValueBuf, &isCopy, "H5Tenum_valueof: enum value buffer not pinned");

    if ((status = H5Tenum_valueof((hid_t)type_id, enumValue, enumValueBuf)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (enumValueBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, value, enumValueBuf, (status < 0) ? JNI_ABORT : 0);
    if (enumValue)
        UNPIN_JAVA_STRING(ENVONLY, name, enumValue);
} /* end Java_hdf_hdf5lib_H5_H5Tenum_1valueof */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_value_int
 * Signature: (JI[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1value_1int
    (JNIEnv *env, jclass clss, jlong type_id, jint membno, jintArray value)
{
    jboolean  isCopy;
    jint     *intP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_member_value: value is NULL");

    PIN_INT_ARRAY(ENVONLY, value, intP, &isCopy, "H5Tget_member_value: value not pinned");

    if ((status = H5Tget_member_value((hid_t)type_id, (unsigned)membno, intP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (intP)
        UNPIN_INT_ARRAY(ENVONLY, value, intP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1value_1int */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_member_value
 * Signature: (JI[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1member_1value
    (JNIEnv *env, jclass clss, jlong type_id, jint membno, jbyteArray value)
{
    jboolean  isCopy;
    jbyte    *byteP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == value)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_member_value: value is NULL");

    PIN_BYTE_ARRAY(ENVONLY, value, byteP, &isCopy, "H5Tget_member_value: value not pinned");

    if ((status = H5Tget_member_value((hid_t)type_id, (unsigned)membno, byteP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (byteP)
        UNPIN_BYTE_ARRAY(ENVONLY, value, byteP, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Tget_1member_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_array_dims
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1array_1ndims
    (JNIEnv *env, jclass clss, jlong type_id)
{
    int ndims = -1;

    UNUSED(clss);

    if ((ndims = H5Tget_array_ndims((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)ndims;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1array_1ndims */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tarray_get_dims
 * Signature: (J[I[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1array_1dims
    (JNIEnv *env, jclass clss, jlong type_id, jintArray dims, jintArray perms)
{
    jboolean  isCopy;
    hsize_t  *cdims = NULL;
    size_t    i;
    jsize     dlen;
    jint     *dimsP = NULL;
    int       ndims = -1;

    UNUSED(clss);
    UNUSED(perms);

    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tget_array_dims: dims array is NULL");

    PIN_INT_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Tget_array_dims: dimsP not pinned");

    if ((dlen = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tget_array_dims: dims array length < 0");
    }

    if (NULL == (cdims = (hsize_t *) HDmalloc((size_t)dlen * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tget_array_dims: failed to allocate dimension buffer");

    if ((ndims = H5Tget_array_dims2((hid_t)type_id, cdims)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (size_t) dlen; i++) {
        dimsP[i] = (jint) cdims[i];
    } /* end for */

done:
    if (cdims)
        HDfree(cdims);
    if (dimsP)
        UNPIN_INT_ARRAY(ENVONLY, dims, dimsP, (ndims < 0) ? JNI_ABORT : 0);

    return (jint)ndims;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1array_1dims */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tis_variable_str
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Tis_1variable_1str
    (JNIEnv *env, jclass clss, jlong type_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Tis_variable_str((hid_t)type_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Tis_1variable_1str */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_native_type
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tget_1native_1type
    (JNIEnv *env, jclass clss, jlong type_id, jint direction)
{
    hid_t native_tid = H5I_INVALID_HID;

    UNUSED(clss);

    if ((native_tid = H5Tget_native_type((hid_t)type_id, (H5T_direction_t)direction)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)native_tid;
} /* end Java_hdf_hdf5lib_H5__1H5Tget_1native_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tdetect_class
 * Signature: (JI)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Tdetect_1class
    (JNIEnv *env, jclass clss, jlong type_id, jint dtype_class)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Tdetect_class((hid_t)type_id, (H5T_class_t)dtype_class)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Tdetect_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tcommit
 * Signature: (JLjava/lang/String;JJJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tcommit
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong type,
          jlong link_plist_id, jlong create_plist_id, jlong access_plist_id)
{
    const char *datatypeName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tcommit: datatype name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, datatypeName, NULL, "H5Tcommit: datatype name not pinned");

    if ((status = H5Tcommit2((hid_t)loc_id, datatypeName, (hid_t)type, (hid_t)link_plist_id, (hid_t)create_plist_id, (hid_t)access_plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (datatypeName)
        UNPIN_JAVA_STRING(ENVONLY, name, datatypeName);
} /* end Java_hdf_hdf5lib_H5_H5Tcommit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Tarray_create2
 * Signature: (JI[J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Tarray_1create2
    (JNIEnv *env, jclass clss, jlong base_id, jint rank, jlongArray dims)
{
    jboolean  isCopy;
    hsize_t  *cdims = NULL;
    size_t    i;
    jlong    *dimsP = NULL;
    jsize     dlen;
    hid_t     retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (rank <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tarray_create: rank is < 1");
    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tarray_create: dims is NULL");

    PIN_LONG_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Tarray_create: dimsP not pinned");

    if ((dlen = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tarray_create: dims array length < 0");
    }

    if (dlen != rank)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tarray_create: dimension array length != array rank");

    if (NULL == (cdims = (hsize_t *) HDmalloc((size_t)dlen * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tarray_create: failed to allocate dimension buffer");

    for (i = 0; i < (size_t) dlen; i++) {
        cdims[i] = (hsize_t)dimsP[i];
    } /* end for */

    if ((retVal = H5Tarray_create2((hid_t)base_id, (unsigned)rank, (const hsize_t *)cdims)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cdims)
        HDfree(cdims);
    if (dimsP)
        UNPIN_LONG_ARRAY(ENVONLY, dims, dimsP, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tarray_1create2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tget_array_dims2
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1array_1dims2
    (JNIEnv *env, jclass clss, jlong type_id, jlongArray dims)
{
    jboolean  isCopy;
    hsize_t  *cdims = NULL;
    size_t    i;
    jlong    *dimsP = NULL;
    jsize     dlen;
    int       ndims = -1;

    UNUSED(clss);

    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Tarray_get_dims2: value is NULL");

    PIN_LONG_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Tarray_get_dims2: dimsP not pinned");

    if ((dlen = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tarray_get_dims2: dims array length < 0");
    }

    if (NULL == (cdims = (hsize_t *) HDmalloc((size_t)dlen * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Tarray_get_dims2: failed to allocate dimension buffer");

    if ((ndims = H5Tget_array_dims2((hid_t)type_id, (hsize_t*)cdims)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (size_t) dlen; i++) {
        dimsP[i] = (jlong) cdims[i];
    } /* end for */

done:
    if (cdims)
        HDfree(cdims);
    if (dimsP)
        UNPIN_LONG_ARRAY(ENVONLY, dims, dimsP, (ndims < 0) ? JNI_ABORT : 0);

    return (jint)ndims;
} /* end Java_hdf_hdf5lib_H5_H5Tget_1array_1dims2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tconvert
 * Signature: (JJJ[B[BJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tconvert
    (JNIEnv *env, jclass clss, jlong src_id, jlong dst_id, jlong nelmts,
      jbyteArray buf, jbyteArray background, jlong plist_id)
{
    jboolean  isCopy;
    jbyte    *bufP = NULL;
    jbyte    *bgP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (nelmts <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Tconvert: nelmts < 0");

    PIN_BYTE_ARRAY(ENVONLY, buf, bufP, &isCopy, "H5Tconvert: buffer not pinned");

    if (background)
        PIN_BYTE_ARRAY(ENVONLY, background, bgP, &isCopy, "H5Tconvert: background buffer not pinned");

    if ((status = H5Tconvert((hid_t)src_id, (hid_t)dst_id, (size_t)nelmts, (void *)bufP, (void *)bgP, (hid_t)plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (bgP)
        UNPIN_BYTE_ARRAY(ENVONLY, background, bgP, (status < 0) ? JNI_ABORT : 0);
    if (bufP)
        UNPIN_BYTE_ARRAY(ENVONLY, buf, bufP, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Tconvert */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tflush(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Tflush((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Trefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Trefresh(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Trefresh((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
