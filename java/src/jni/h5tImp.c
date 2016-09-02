/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    http://hdfdfgroup.org/HDF5/doc/
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

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Topen2
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Topen2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist)
{
    hid_t       status = -1;
    const char *tName;

    PIN_JAVA_STRING(name, tName);
    if (tName != NULL) {
        status = H5Topen2((hid_t)loc_id, tName, (hid_t)access_plist);

        UNPIN_JAVA_STRING(name, tName);

        if (status < 0)
            h5libraryError(env);
    }

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

    bval = H5Tcommitted(type_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal = H5Tcreate((H5T_class_t )dclass, (size_t)size);
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal = H5Tcopy((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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

    bval = H5Tequal((hid_t)type_id1, (hid_t)type_id2);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tlock((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_class((hid_t)type_id);
    if (retVal == H5T_NO_CLASS)
        h5libraryError(env);

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

    retVal = H5Tget_size((hid_t)type_id);
    if (retVal == 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_size((hid_t)type_id, tsize);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_order((hid_t)type_id);
    if (retVal == H5T_ORDER_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_order((hid_t)type_id, (H5T_order_t)order);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_precision((hid_t)type_id);
    if (retVal == 0)
        h5libraryError(env);

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

    retVal = H5Tget_precision((hid_t)type_id);
    if (retVal == 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_precision((hid_t)type_id, (size_t)precision);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_offset((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_offset((hid_t)type_id, (size_t)offset);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t   status = -1;
    jboolean isCopy;
    jint    *P;

    if (pad == NULL) {
        h5nullArgument(env, "H5Tget_pad:  pad is NULL");
    } /* end if */
    else {
        P = ENVPTR->GetIntArrayElements(ENVPAR pad, &isCopy);
        if (P == NULL) {
            h5JNIFatalError(env, "H5Tget_pad:  pad not pinned");
        } /* end if */
        else {
            status = H5Tget_pad((hid_t)type_id, (H5T_pad_t *)&(P[0]), (H5T_pad_t *)&(P[1]));

            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR pad, P, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR pad, P, 0);
        } /* end else */
    } /* end else */

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
    herr_t retVal = -1;

    retVal = H5Tset_pad((hid_t)type_id, (H5T_pad_t)lsb, (H5T_pad_t)msb);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_sign((hid_t)type_id);
    if (retVal == H5T_SGN_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_sign((hid_t)type_id, (H5T_sign_t)sign);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t   status;
    jboolean isCopy;
    jint    *P;

    if (fields == NULL) {
        h5nullArgument(env, "H5Tget_fields:  fields is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR fields) < 5) {
        h5badArgument(env, "H5Tget_fields:  fields input array < order 5");
    } /* end else if */
    else {
        P = ENVPTR->GetIntArrayElements(ENVPAR fields, &isCopy);
        if (P == NULL) {
            h5JNIFatalError(env,  "H5Tget_fields:  fields not pinned");
        } /* end if */
        else {
            status = H5Tget_fields((hid_t)type_id, (size_t *)&(P[0]), (size_t *)&(P[1]), (size_t *)&(P[2]), (size_t *)&(P[3]), (size_t *)&(P[4]));

            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR fields, P, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR fields, P, 0);
        } /* end else */
    } /* end else */

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
    herr_t   status;
    jboolean isCopy;
    jlong   *fieldsArray;

    if (fields == NULL) {
        h5nullArgument(env, "H5Tget_fields:  fields is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR fields) < 5) {
        h5badArgument(env, "H5Tget_fields:  fields input array < order 5");
    } /* end else if */
    else {
        fieldsArray = ENVPTR->GetLongArrayElements(ENVPAR fields, &isCopy);
        if (fieldsArray == NULL) {
            h5JNIFatalError(env,  "H5Tget_fields:  fields not pinned");
        } /* end if */
        else {
            { /* direct cast (size_t *)variable fails on 32-bit environment */
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

                status = H5Tget_fields((hid_t)type_id, &spos_t, &epos_t, &esize_t, &mpos_t, &msize_t);

                *(&fieldsArray[0]) = (jlong)spos_t;
                *(&fieldsArray[1]) = (jlong)epos_t;
                *(&fieldsArray[2]) = (jlong)esize_t;
                *(&fieldsArray[3]) = (jlong)mpos_t;
                *(&fieldsArray[4]) = (jlong)msize_t;
            } /* end direct cast special handling */

            if (status < 0) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR fields, fieldsArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseLongArrayElements(ENVPAR fields, fieldsArray, 0);
        } /* end else */
    } /* end else */
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
    if (H5Tset_fields((hid_t)type_id, (size_t)spos, (size_t)epos, (size_t)esize, (size_t)mpos, (size_t)msize) < 0)
        h5libraryError(env);
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

    retVal =  H5Tget_ebias((hid_t)type_id);
    if (retVal == 0)
        h5libraryError(env);

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

    retVal =  H5Tget_ebias((hid_t)type_id);
    if (retVal == 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal =  H5Tset_ebias((hid_t)type_id, (size_t)ebias);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal =  H5Tget_norm((hid_t)type_id);
    if (retVal == H5T_NORM_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal =  H5Tset_norm((hid_t)type_id, (H5T_norm_t )norm);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal =  H5Tget_inpad((hid_t)type_id );
    if (retVal == H5T_PAD_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_inpad((hid_t)type_id, (H5T_pad_t) inpad);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal =  H5Tget_cset((hid_t)type_id);
    if (retVal == H5T_CSET_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tset_cset((hid_t)type_id, (H5T_cset_t)cset);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Tget_strpad((hid_t)type_id);
    if (retVal == H5T_STR_ERROR)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal =  H5Tset_strpad((hid_t)type_id, (H5T_str_t)strpad);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal =  H5Tget_nmembers((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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
    char   *name;
    jstring str = NULL;

    name = H5Tget_member_name((hid_t)type_id, (unsigned)field_idx);
    if (name != NULL) {
        str = ENVPTR->NewStringUTF(ENVPAR name);
        H5free_memory(name);

        if (str == NULL)
            h5JNIFatalError(env,  "H5Tget_member_name:  returned string not created");
    } /* end if */

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
    const char *tName;
    int         index = -1;

    PIN_JAVA_STRING(field_name, tName);
    if (tName != NULL) {
        index = H5Tget_member_index((hid_t)type_id, tName);

        UNPIN_JAVA_STRING(field_name, tName);

        if (index < 0)
            h5libraryError(env);
    }

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
    hid_t retVal = -1;

    retVal =  H5Tget_member_type((hid_t)type_id, (unsigned)field_idx);
    if (retVal < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Tget_member_class((hid_t)type_id, (unsigned)memno);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t      status = -1;
    const char *tName;
    long        off = (long)offset;

    PIN_JAVA_STRING(name, tName);
    if (tName != NULL) {
        status = H5Tinsert((hid_t)type_id, tName, (size_t)off, field_id);

        UNPIN_JAVA_STRING(name,tName);
        if (status < 0)
            h5libraryError(env);
    }

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
    herr_t retVal = -1;

    retVal = H5Tpack((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Tclose((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal = H5Tvlen_create((hid_t)base_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t      status = -1;
    const char *tagP;

    PIN_JAVA_STRING(tag, tagP);
    if (tagP != NULL) {
        status = H5Tset_tag((hid_t)type, tagP);

        UNPIN_JAVA_STRING(tag,tagP);

        if (status < 0)
            h5libraryError(env);
    }

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
    jstring str = NULL;
    char   *tag;

    tag = H5Tget_tag((hid_t)type);
    if (tag != NULL) {
        str = ENVPTR->NewStringUTF(ENVPAR tag);
        H5free_memory(tag);

        if (str == NULL)
            h5JNIFatalError(env,  "H5Tget_tag:  returned string not created");
    } /* end if */

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
    hid_t retVal = -1;

    retVal = H5Tget_super((hid_t)type_id);
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal = H5Tenum_create((hid_t)base_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t      status = -1;
    jint       *intP;
    const char *nameP;
    jboolean    isCopy2;

    if (value == NULL ) {
        h5nullArgument(env, "H5Tenum_insert:  value is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, nameP);
        if (nameP != NULL) {
            intP = ENVPTR->GetIntArrayElements(ENVPAR value, &isCopy2);
            if (intP == NULL) {
                h5JNIFatalError(env, "H5Tenum_insert:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_insert((hid_t)type_id, nameP, intP);

                ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, JNI_ABORT);

                if (status < 0)
                    h5libraryError(env);
            } /* end else */
            UNPIN_JAVA_STRING(name, nameP);
        }
    } /* end else */

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
    herr_t      status = -1;
    jbyte      *byteP;
    const char *nameP;
    jboolean    isCopy2;

    if (value == NULL) {
        h5nullArgument(env, "H5Tenum_insert:  value is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, nameP);
        if (nameP != NULL) {
            byteP = ENVPTR->GetByteArrayElements(ENVPAR value, &isCopy2);
            if (byteP == NULL) {
                h5JNIFatalError(env, "H5Tenum_insert:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_insert((hid_t)type_id, nameP, byteP);

                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);

                if (status < 0)
                    h5libraryError(env);
            } /* end else */
            UNPIN_JAVA_STRING(name, nameP);
        }
    } /* end else */
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
    herr_t   status = -1;
    jint    *intP;
    char    *nameP;
    jboolean isCopy;
    jstring  str;

    if (size <= 0) {
        h5badArgument(env, "H5Tenum_nameof:  name size < 0");
    } /* end if */
    else  if (value == NULL) {
        h5nullArgument(env, "H5Tenum_nameof:  value is NULL");
    } /* end if */
    else {
        nameP = (char*)HDmalloc(sizeof(char) * (size_t)size);
        if (nameP == NULL) {
            /* exception -- out of memory */
            h5outOfMemory(env, "H5Tenum_nameof:  malloc name size");
        } /* end if */
        else {
            intP = ENVPTR->GetIntArrayElements(ENVPAR value, &isCopy);
            if (intP == NULL) {
                HDfree(nameP);
                h5JNIFatalError(env, "H5Tenum_nameof:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_nameof((hid_t)type_id, intP, nameP, (size_t)size);

                ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, JNI_ABORT);

                if (status < 0) {
                    HDfree(nameP);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR nameP);
                    HDfree(nameP);
                    if (str == NULL) {
                        h5JNIFatalError(env, "H5Tenum_nameof:  return array not created");
                    } /* end if */
                    else
                        ENVPTR->SetObjectArrayElement(ENVPAR name, 0, (jobject)str);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

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
    herr_t status = -1;
    jbyte *byteP;
    char *nameP;
    jboolean isCopy;
    jstring str = NULL;

    if (size <= 0) {
        h5badArgument(env, "H5Tenum_nameof:  name size < 0");
    } /* end if */
    else if (value == NULL ) {
        h5nullArgument(env, "H5Tenum_nameof:  value is NULL");
    } /* end if */
    else {
        nameP = (char*)HDmalloc(sizeof(char) * (size_t)size);
        if (nameP == NULL) {
            h5outOfMemory(env, "H5Tenum_nameof:  malloc name size");
        } /* end if */
        else {
            byteP = ENVPTR->GetByteArrayElements(ENVPAR value, &isCopy);
            if (byteP == NULL) {
                HDfree(nameP);
                h5JNIFatalError(env, "H5Tenum_nameof:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_nameof((hid_t)type_id, byteP, nameP, (size_t)size);

                /* free the buffer without copying back */
                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);

                if (status < 0) {
                    HDfree(nameP);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR nameP);
                    HDfree(nameP);
                    if (str == NULL) {
                        h5JNIFatalError(env, "H5Tenum_nameof:  return array not created");
                    } /* end if */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

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
    herr_t      status = -1;
    jint       *intP;
    const char *nameP;
    jboolean    isCopy2;

    if (value == NULL) {
        h5nullArgument(env, "H5Tenum_valueof:  value is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, nameP);
        if (nameP != NULL) {
            intP = ENVPTR->GetIntArrayElements(ENVPAR value, &isCopy2);
            if (intP == NULL)  {
                h5JNIFatalError(env, "H5Tenum_valueof:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_valueof((hid_t)type_id, nameP, intP);

                if (status < 0) {
                    ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else
                    ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, 0);
            } /* end else */
            UNPIN_JAVA_STRING(name, nameP);
        }
    } /* end else */

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
    herr_t      status = -1;
    jbyte      *byteP;
    const char *nameP;
    jboolean    isCopy2;

    if (value == NULL) {
        h5nullArgument(env, "H5Tenum_valueof:  value is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, nameP);
        if (nameP != NULL) {
            byteP = ENVPTR->GetByteArrayElements(ENVPAR value, &isCopy2);
            if (byteP == NULL)  {
                h5JNIFatalError(env, "H5Tenum_valueof:  value not pinned");
            } /* end if */
            else {
                status = H5Tenum_valueof((hid_t)type_id, nameP, byteP);

                if (status < 0) {
                    ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else
                    ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, 0);
            } /* end else */
            UNPIN_JAVA_STRING(name, nameP);
        }
    } /* end else */
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
    herr_t   status = -1;
    jint    *intP;
    jboolean isCopy;

    if (value == NULL)  {
        h5nullArgument(env, "H5Tget_member_value:  value is NULL");
    } /* end if */
    else {
        intP = ENVPTR->GetIntArrayElements(ENVPAR value, &isCopy);
        if (intP == NULL) {
            h5JNIFatalError(env, "H5Tget_member_value:  value not pinned");
        } /* end if */
        else {
            status = H5Tget_member_value((hid_t)type_id, (unsigned)membno, intP);


            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR value, intP, 0);
        } /* end else */
    } /* end else */

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
    herr_t   status = -1;
    jbyte   *byteP;
    jboolean isCopy;

    if (value == NULL) {
        h5nullArgument( env, "H5Tget_member_value:  value is NULL");
    } /* end if */
    else {
        byteP = ENVPTR->GetByteArrayElements(ENVPAR value,&isCopy);
        if (byteP == NULL) {
            h5JNIFatalError(env, "H5Tget_member_value:  value not pinned");
        } /* end if */
        else {
            status = H5Tget_member_value((hid_t)type_id, (unsigned)membno, byteP);

            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, 0);
        } /* end else */
    } /* end else */
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

    ndims = H5Tget_array_ndims((hid_t)type_id);
    if (ndims < 0)
        h5libraryError(env);

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
    int      ndims = -1;
    int      dlen;
    int      i;
    jint    *dimsP;
    hsize_t *cdims = NULL;
    jboolean isCopy;

    if (dims == NULL) {
        h5nullArgument( env, "H5Tget_array_dims:  value is NULL");
    } /* end if */
    else {
        dimsP = ENVPTR->GetIntArrayElements(ENVPAR dims, &isCopy);
        if (dimsP == NULL) {
            h5JNIFatalError(env, "H5Tget_array_dims:  dimsP not pinned");
        } /* end if */
        else {
            dlen = ENVPTR->GetArrayLength(ENVPAR dims);
            cdims = (hsize_t*)HDmalloc((size_t)dlen * sizeof(hsize_t));

            ndims = H5Tget_array_dims2((hid_t)type_id, cdims);

            if (ndims < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                for (i = 0; i < dlen; i++) {
                    dimsP[i] = (jint) cdims[i];
                } /* end for */
                ENVPTR->ReleaseIntArrayElements(ENVPAR dims, dimsP, 0);

                if (cdims) HDfree(cdims);
            } /* end else */
        } /* end else */
    } /* end else */

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

    bval = H5Tis_variable_str((hid_t)type_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    hid_t native_tid = -1;

    native_tid = H5Tget_native_type((hid_t)type_id, (H5T_direction_t)direction);

    if (native_tid < 0)
        h5libraryError(env);

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

    bval = H5Tdetect_class((hid_t)type_id, (H5T_class_t)dtype_class);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    herr_t      status = -1;
    const char *tName;

    PIN_JAVA_STRING(name, tName);
    if (tName != NULL) {
        status = H5Tcommit2((hid_t)loc_id, tName, (hid_t)type, (hid_t)link_plist_id, (hid_t)create_plist_id, (hid_t)access_plist_id);

        UNPIN_JAVA_STRING(name, tName);

        if (status < 0)
            h5libraryError(env);
    }
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
    hid_t    retVal = -1;
    jlong   *dimsP;
    int      dlen;
    hsize_t *cdims = NULL;
    jboolean isCopy;
    int      i;

    if (rank <= 0) {
        h5badArgument(env, "H5Tarray_create:  rank is < 1");
    } /* end if */
    else if (dims == NULL) {
        h5nullArgument(env, "H5Tarray_create:  dims is NULL");
    } /* end else if */
    else {
        dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims, &isCopy);
        if (dimsP == NULL) {
            h5JNIFatalError( env, "H5Tarray_create:  dimsP not pinned");
        } /* end if */
        else {
            dlen = ENVPTR->GetArrayLength(ENVPAR dims);
            if (dlen != rank) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            } /* end if */
            else {
                cdims = (hsize_t*)HDmalloc((size_t)dlen * sizeof(hsize_t));
                for (i = 0; i < dlen; i++) {
                    cdims[i] = (hsize_t)dimsP[i];
                } /* end for */

                retVal = H5Tarray_create2((hid_t)base_id, (unsigned)rank, (const hsize_t*)cdims);

                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, 0);

                HDfree(cdims);
                if (retVal < 0)
                    h5libraryError(env);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Tarray_1create2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tarray_get_dims2
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Tget_1array_1dims2
    (JNIEnv *env, jclass clss, jlong type_id, jlongArray dims)
{
    int      ndims = -1;
    int      dlen;
    int      i;
    jlong   *dimsP;
    hsize_t *cdims=NULL;
    jboolean isCopy;

    if (dims == NULL) {
        h5nullArgument(env, "H5Tget_array_dims:  value is NULL");
    } /* end if */
    else {
        dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims,&isCopy);
        if (dimsP == NULL) {
            h5JNIFatalError(env, "H5Tget_array_dims:  dimsP not pinned");
        } /* end if */
        else {
            dlen = ENVPTR->GetArrayLength(ENVPAR dims);
            cdims = (hsize_t*)HDmalloc((size_t)dlen * sizeof(hsize_t));

            ndims = H5Tget_array_dims2((hid_t)type_id, (hsize_t*)cdims);

            if (ndims < 0) {
                if (cdims)
                    free(cdims);
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                for (i = 0; i < dlen; i++) {
                    dimsP[i] = (jlong) cdims[i];
                } /* end for */
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, 0);

                if (cdims)
                    HDfree(cdims);
            } /* end else */
        } /* end else */
    } /* end else */

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
    herr_t   status;
    jbyte   *bufP;
    jbyte   *bgP = NULL;
    jboolean isCopy;

    if (nelmts <= 0) {
        h5badArgument(env, "H5Tconvert:  name nelmts < 0");
    } /* end if */
    else {
        bufP = ENVPTR->GetByteArrayElements(ENVPAR buf, &isCopy);
        if (bufP == NULL) {
            h5JNIFatalError(env, "H5Tconvert:  value not pinned");
        } /* end if */
        else {
            if (background)
            bgP = ENVPTR->GetByteArrayElements(ENVPAR background, &isCopy);

            status = H5Tconvert((hid_t)src_id, (hid_t)dst_id, (size_t)nelmts, (void *)bufP, (void *)bgP, (hid_t)plist_id) ;

            ENVPTR->ReleaseByteArrayElements(ENVPAR buf, bufP, 0);

            if (bgP)
                ENVPTR->ReleaseByteArrayElements(ENVPAR background, bgP, 0);

            if (status < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Tconvert */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Tflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Tflush(JNIEnv *env, jclass clss, jlong loc_id)
{
    if (H5Tflush((hid_t)loc_id) < 0)
        h5libraryError(env);
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Trefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Trefresh(JNIEnv *env, jclass clss, jlong loc_id)
{
    if (H5Trefresh((hid_t)loc_id) < 0)
        h5libraryError(env);
}



#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
