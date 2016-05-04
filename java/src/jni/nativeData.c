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
/*
 *  This module contains the implementation of all the native methods
 *  used for number conversion.  This is represented by the Java
 *  class HDFNativeData.
 *
 *  These routines convert one dimensional arrays of bytes into
 *  one-D arrays of other types (int, float, etc) and vice versa.
 *
 *  These routines are called from the Java parts of the Java-C
 *  interface.
 *
 *  ***Important notes:
 *
 *     1.  These routines are designed to be portable--they use the
 *         C compiler to do the required native data manipulation.
 *     2.  These routines copy the data at least once -- a serious
 *         but unavoidable performance hit.
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <jni.h>
#include "hdf5.h"
#include "h5jni.h"
#include "nativeData.h"


/* returns int [] */
JNIEXPORT jintArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToInt___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jintArray rarray = NULL;
    int blen;
    jint *iarray;
    jboolean bb;
    char *bp;
    jint *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToInt: bdata is NULL?");
    } /* end if */
    else {
        barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
        if (barr == NULL) {
            h5JNIFatalError(env,  "byteToInt: pin failed");
        } /* end if */
        else {
            blen = ENVPTR->GetArrayLength(ENVPAR bdata);

            len = blen/(int)sizeof(jint);
            rarray = ENVPTR->NewIntArray(ENVPAR len);
            if (rarray == NULL) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
                h5outOfMemory( env,  "byteToInt" );
                return NULL;
            } /* end if */

            iarray = ENVPTR->GetIntArrayElements(ENVPAR rarray,&bb);
            if (iarray == NULL) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
                h5JNIFatalError(env,  "byteToInt: pin iarray failed");
                return NULL;
            } /* end if */

            bp = (char *)barr;
            iap = iarray;
            for (ii = 0; ii < len; ii++) {
                *iap = *(jint *)bp;
                iap++;
                bp += sizeof(jint);
            } /* end for */

            ENVPTR->ReleaseIntArrayElements(ENVPAR rarray,iarray, 0);
        } /* end else */
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
    }  /* end else */
    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToInt___3B */

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jfloatArray rarray;
    int blen;
    jfloat *farray;
    jboolean bb;
    char *bp;
    jfloat *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToFloat: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError(env,  "byteToFloat: pin failed");
        return NULL;
    } /* end if */
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/(int)sizeof(jfloat);
    rarray = ENVPTR->NewFloatArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToFloat" );
        return NULL;
    } /* end if */
    farray = ENVPTR->GetFloatArrayElements(ENVPAR rarray,&bb);
    if (farray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToFloat: pin farray failed");
        return NULL;
    } /* end if */

    bp = (char *)barr;
    iap = farray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    } /* end for */

    ENVPTR->ReleaseFloatArrayElements(ENVPAR rarray,farray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToFloat___3B */

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jshortArray rarray;
    int blen;
    jshort *sarray;
    jboolean bb;
    char *bp;
    jshort *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToShort: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError(env,  "byteToShort: pin failed");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/(int)sizeof(jshort);
    rarray = ENVPTR->NewShortArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToShort" );
        return NULL;
    } /* end if */

    sarray = ENVPTR->GetShortArrayElements(ENVPAR rarray,&bb);
    if (sarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToShort: pin sarray failed");
        return NULL;
    } /* end if */

    bp = (char *)barr;
    iap = sarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    } /* end for */

    ENVPTR->ReleaseShortArrayElements(ENVPAR rarray,sarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToShort___3B */


/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jlongArray rarray;
    int blen;
    jlong *larray;
    jboolean bb;
    char *bp;
    jlong *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToLong: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError(env,  "byteToLong: pin failed");
        return NULL;
    } /* end if */
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/(int)sizeof(jlong);
    rarray = ENVPTR->NewLongArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToLong" );
        return NULL;
    } /* end if */

    larray = ENVPTR->GetLongArrayElements(ENVPAR rarray,&bb);
    if (larray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToLong: pin larray failed");
        return NULL;
    } /* end if */

    bp = (char *)barr;
    iap = larray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    } /* end for */
    ENVPTR->ReleaseLongArrayElements(ENVPAR rarray,larray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToLong___3B */


/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jbyte *barr;
    jdoubleArray rarray;
    int blen;
    jdouble *darray;
    jboolean bb;
    char *bp;
    jdouble *iap;
    int ii;
    int len;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToDouble: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError(env,  "byteToDouble: pin failed");
        return NULL;
    } /* end if */
    blen = ENVPTR->GetArrayLength(ENVPAR bdata);

    len = blen/(int)sizeof(jdouble);
    rarray = ENVPTR->NewDoubleArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToDouble" );
        return NULL;
    } /* end if */

    darray = ENVPTR->GetDoubleArrayElements(ENVPAR rarray,&bb);
    if (darray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToDouble: pin darray failed");
        return NULL;
    } /* end if */

    bp = (char *)barr;
    iap = darray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    } /* end for */

    ENVPTR->ReleaseDoubleArrayElements(ENVPAR rarray,darray,0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToDouble___3B */


/* returns int [] */
JNIEXPORT jintArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToInt__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jintArray rarray;
    int blen;
    jint *iarray;
    jint *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToInt: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError(env,  "byteToInt: pin failed");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(int)sizeof(jint))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToInt: getLen failed");
        return NULL;
    } /* end if */

    bp = (char *)barr + start;

    rarray = ENVPTR->NewIntArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToInt" );
        return NULL;
    } /* end if */

    iarray = ENVPTR->GetIntArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError(env,  "byteToInt: pin iarray failed");
        return NULL;
    } /* end if */

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jint *)bp;
        iap++;
        bp += sizeof(jint);
    } /* end for */

    ENVPTR->ReleaseIntArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToInt__II_3B */

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jshortArray rarray;
    int blen;
    jshort *iarray;
    jshort *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToShort: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError( env,  "byteToShort: getByte failed?");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(int)sizeof(jshort))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5badArgument( env,  "byteToShort: start or len is out of bounds");
        return NULL;
    } /* end if */

    bp = (char *)barr + start;

    rarray = ENVPTR->NewShortArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToShort" );
        return NULL;
    } /* end if */

    iarray = ENVPTR->GetShortArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError( env,  "byteToShort: getShort failed?");
        return NULL;
    } /* end if */

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    } /* end for */

    ENVPTR->ReleaseShortArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToShort__II_3B */

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jfloatArray rarray;
    int blen;
    jfloat *iarray;
    jfloat *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToFloat: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError( env,  "byteToFloat: getByte failed?");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(int)sizeof(jfloat))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5badArgument( env,  "byteToFloat: start or len is out of bounds");
        return NULL;
    } /* end if */

    bp = (char *)barr + start;

    rarray = ENVPTR->NewFloatArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToFloat" );
        return NULL;
    } /* end if */

    iarray = ENVPTR->GetFloatArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError( env,  "byteToFloat: getFloat failed?");
        return NULL;
    } /* end if */

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    } /* end for */

    ENVPTR->ReleaseFloatArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToFloat__II_3B */

/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jlongArray rarray;
    int blen;
    jlong *iarray;
    jlong *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToLong: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError( env,  "byteToLong: getByte failed?");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(int)sizeof(jlong))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5badArgument( env,  "byteToLong: start or len is out of bounds");
        return NULL;
    } /* end if */

    bp = (char *)barr + start;

    rarray = ENVPTR->NewLongArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToLong" );
        return NULL;
    } /* end if */

    iarray = ENVPTR->GetLongArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError( env,  "byteToLong: getLong failed?");
        return NULL;
    } /* end if */

    iap = iarray;
    for (ii = 0; ii < len; ii++) {

        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    } /* end for */

    ENVPTR->ReleaseLongArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToLong__II_3B */

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    char *bp;
    jbyte *barr;
    jdoubleArray rarray;
    int blen;
    jdouble *iarray;
    jdouble *iap;
    int ii;
    jboolean bb;

    if (bdata == NULL) {
        h5nullArgument( env,  "byteToDouble: bdata is NULL?");
        return NULL;
    } /* end if */
    barr = ENVPTR->GetByteArrayElements(ENVPAR bdata,&bb);
    if (barr == NULL) {
        h5JNIFatalError( env,  "byteToDouble: getByte failed?");
        return NULL;
    } /* end if */

    blen = ENVPTR->GetArrayLength(ENVPAR bdata);
    if ((start < 0) || ((int)(start + (len*(int)sizeof(jdouble))) > blen)) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5badArgument( env,  "byteToDouble: start or len is out of bounds");
        return NULL;
    } /* end if */

    bp = (char *)barr + start;

    rarray = ENVPTR->NewDoubleArray(ENVPAR len);
    if (rarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5outOfMemory( env,  "byteToDouble" );
        return NULL;
    } /* end if */

    iarray = ENVPTR->GetDoubleArrayElements(ENVPAR rarray,&bb);
    if (iarray == NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);
        h5JNIFatalError( env,  "byteToDouble: getDouble failed?");
        return NULL;
    } /* end if */

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    } /* end for */

    ENVPTR->ReleaseDoubleArrayElements(ENVPAR rarray,iarray, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bdata,barr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToDouble__II_3B */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__II_3I
    (JNIEnv *env, jclass clss, jint start, jint len, jintArray idata)  /* IN: array of int */
{
    jint *ip;
    jint *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        int ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h5nullArgument( env,  "intToByte: idata is NULL?");
        return NULL;
    } /* end if */

    iarr = ENVPTR->GetIntArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h5JNIFatalError( env,  "intToByte: getInt failed?");
        return NULL;
    } /* end if */

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5badArgument( env,  "intToByte: start or len is out of bounds");
        return NULL;
    } /* end if */

    ip = iarr + start;

    blen = ilen * (int)sizeof(jint);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5outOfMemory( env,  "intToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5JNIFatalError( env,  "intToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jint); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseIntArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_intToByte__II_3I */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__II_3S
    (JNIEnv *env, jclass clss, jint start, jint len, jshortArray idata)  /* IN: array of short */
{
    jshort *ip;
    jshort *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        short ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h5nullArgument( env,  "shortToByte: idata is NULL?");
        return NULL;
    } /* end if */
    iarr = ENVPTR->GetShortArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h5JNIFatalError( env,  "shortToByte: getShort failed?");
        return NULL;
    } /* end if */

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5badArgument( env,  "shortToByte: start or len is out of bounds");
        return NULL;
    } /* end if */

    ip = iarr + start;

    blen = ilen * (int)sizeof(jshort);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5outOfMemory( env,  "shortToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5JNIFatalError( env,  "shortToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jshort); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseShortArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_shortToByte__II_3S */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__II_3F
    (JNIEnv *env, jclass clss, jint start, jint len, jfloatArray idata)  /* IN: array of float */
{
    jfloat *ip;
    jfloat *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        float ival;
        char bytes[4];
    } u;

    if (idata == NULL) {
        h5nullArgument( env,  "floatToByte: idata is NULL?");
        return NULL;
    } /* end if */
    iarr = ENVPTR->GetFloatArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h5JNIFatalError( env,  "floatToByte: getFloat failed?");
        return NULL;
    } /* end if */

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5badArgument( env,  "floatToByte: start or len is out of bounds");
        return NULL;
    } /* end if */

    ip = iarr + start;

    blen = ilen * (int)sizeof(jfloat);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5outOfMemory( env,  "floatToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5JNIFatalError( env,  "floatToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jfloat); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseFloatArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_floatToByte__II_3F */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__II_3D
    (JNIEnv *env, jclass clss, jint start, jint len, jdoubleArray idata)  /* IN: array of double */
{
    jdouble *ip;
    jdouble *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        double ival;
        char bytes[8];
    } u;

    if (idata == NULL) {
        h5nullArgument( env,  "doubleToByte: idata is NULL?");
        return NULL;
    } /* end if */
    iarr = ENVPTR->GetDoubleArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h5JNIFatalError( env,  "doubleToByte: getDouble failed?");
        return NULL;
    } /* end if */

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5badArgument( env,  "doubleToByte: start or len is out of bounds");
        return NULL;
    } /* end if */

    ip = iarr + start;

    blen = ilen * (int)sizeof(jdouble);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5outOfMemory( env,  "doubleToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5JNIFatalError( env,  "doubleToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jdouble); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseDoubleArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_doubleToByte__II_3D */


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__II_3J
    (JNIEnv *env, jclass clss, jint start, jint len, jlongArray idata)  /* IN: array of long */
{
    jlong *ip;
    jlong *iarr;
    int ilen;
    jbyteArray rarray;
    int blen;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ii;
    int ij;
    union things {
        jlong ival;
        char bytes[8];
    } u;

    if (idata == NULL) {
        h5nullArgument( env,  "longToByte: idata is NULL?");
        return NULL;
    } /* end if */
    iarr = ENVPTR->GetLongArrayElements(ENVPAR idata,&bb);
    if (iarr == NULL) {
        h5JNIFatalError( env,  "longToByte: getLong failed?");
        return NULL;
    } /* end if */

    ilen = ENVPTR->GetArrayLength(ENVPAR idata);
    if ((start < 0) || (((start + len)) > ilen)) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5badArgument( env,  "longToByte: start or len is out of bounds?\n");
        return NULL;
    } /* end if */

    ip = iarr + start;

    blen = ilen * (int)sizeof(jlong);
    rarray = ENVPTR->NewByteArray(ENVPAR blen);
    if (rarray == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5outOfMemory( env,  "longToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);
        h5JNIFatalError( env,  "longToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; ij < sizeof(jlong); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);
    ENVPTR->ReleaseLongArrayElements(ENVPAR idata,iarr,JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_longToByte__II_3J */


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__I
    (JNIEnv *env, jclass clss, jint idata)  /* IN: int */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    int ij;
    jboolean bb;
    union things {
        int ival;
        char bytes[sizeof(int)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jint));
    if (rarray == NULL) {
        h5outOfMemory( env,  "intToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "intToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jint); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_intToByte__I */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__F
    (JNIEnv *env, jclass clss, jfloat idata)  /* IN: int */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        float ival;
        char bytes[sizeof(float)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jfloat));
    if (rarray == NULL) {
        h5outOfMemory( env,  "floatToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "floatToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jfloat); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_floatToByte__F */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__S
    (JNIEnv *env, jclass clss, jshort idata)  /* IN: short */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        short ival;
        char bytes[sizeof(short)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jshort));
    if (rarray == NULL) {
        h5outOfMemory( env,  "shortToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "shortToByte: getShort failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jshort); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_shortToByte__S */


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__D
    (JNIEnv *env, jclass clss, jdouble idata)  /* IN: double */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        double ival;
        char bytes[sizeof(double)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jdouble));
    if (rarray == NULL) {
        h5outOfMemory( env,  "doubleToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "doubleToByte: getDouble failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jdouble); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_doubleToByte__D */


/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__J
    (JNIEnv *env, jclass clss, jlong idata)  /* IN: array of long */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        jlong ival;
        char bytes[sizeof(jlong)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jlong));
    if (rarray == NULL) {
        h5outOfMemory( env,  "longToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "longToByte: getLong failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jlong); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_longToByte__J */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToByte__B
    (JNIEnv *env, jclass clss, jbyte idata)  /* IN: array of long */
{
    jbyteArray rarray;
    jbyte *barray;
    jbyte *bap;
    jboolean bb;
    int ij;
    union things {
        jbyte ival;
        char bytes[sizeof(jbyte)];
    } u;

    rarray = ENVPTR->NewByteArray(ENVPAR sizeof(jbyte));
    if (rarray == NULL) {
        h5outOfMemory( env,  "byteToByte" );
        return NULL;
    } /* end if */

    barray = ENVPTR->GetByteArrayElements(ENVPAR rarray,&bb);
    if (barray == NULL) {
        h5JNIFatalError( env,  "byteToByte: getByte failed?");
        return NULL;
    } /* end if */

    bap = barray;
    u.ival = idata;
    for (ij = 0; ij < sizeof(jbyte); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

    ENVPTR->ReleaseByteArrayElements(ENVPAR rarray,(jbyte *)barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToByte__B */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
