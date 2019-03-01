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
    jintArray  rarray = NULL;
    jboolean   bb;
    jbyte     *barr = NULL;
    jint      *iarray = NULL;
    jint      *iap = NULL;
    char      *bp = NULL;
    int        blen;
    int        ii;
    int        len;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToInt: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToInt: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToInt: bdata length < 0");
    }

    len = blen / (int)sizeof(jint);

    if (NULL == (rarray = ENVPTR->NewIntArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_INT_ARRAY(ENVONLY, rarray, iarray, &bb, "byteToInt: int array not pinned");

    bp = (char *)barr;
    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jint *)bp;
        iap++;
        bp += sizeof(jint);
    } /* end for */

done:
    if (iarray)
        UNPIN_INT_ARRAY(ENVONLY, rarray, iarray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToInt___3B */

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jfloatArray  rarray;
    jboolean     bb;
    jfloat      *farray = NULL;
    jfloat      *iap = NULL;
    jbyte       *barr = NULL;
    char        *bp = NULL;
    int          blen;
    int          ii;
    int          len;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToFloat: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToFloat: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToFloat: bdata length < 0");
    }

    len = blen / (int)sizeof(jfloat);

    if (NULL == (rarray = ENVPTR->NewFloatArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_FLOAT_ARRAY(ENVONLY, rarray, farray, &bb, "byteToFloat: float array not pinned");

    bp = (char *)barr;
    iap = farray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    } /* end for */

done:
    if (farray)
        UNPIN_FLOAT_ARRAY(ENVONLY, rarray, farray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToFloat___3B */

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jshortArray  rarray;
    jboolean     bb;
    jshort      *sarray = NULL;
    jshort      *iap = NULL;
    jbyte       *barr = NULL;
    char        *bp = NULL;
    int          blen;
    int          ii;
    int          len;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToShort: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToShort: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToShort: bdata length < 0");
    }

    len = blen / (int)sizeof(jshort);

    if (NULL == (rarray = ENVPTR->NewShortArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_SHORT_ARRAY(ENVONLY, rarray, sarray, &bb, "byteToShort: short array not pinned");

    bp = (char *)barr;
    iap = sarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    } /* end for */

done:
    if (sarray)
        UNPIN_SHORT_ARRAY(ENVONLY, rarray, sarray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToShort___3B */

/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jlongArray  rarray;
    jboolean    bb;
    jlong      *larray = NULL;
    jlong      *iap = NULL;
    jbyte      *barr = NULL;
    char       *bp = NULL;
    int         blen;
    int         ii;
    int         len;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToLong: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToLong: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToLong: bdata length < 0");
    }

    len = blen / (int)sizeof(jlong);

    if (NULL == (rarray = ENVPTR->NewLongArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_LONG_ARRAY(ENVONLY, rarray, larray, &bb, "byteToLong: long array not pinned");

    bp = (char *)barr;
    iap = larray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    } /* end for */

done:
    if (larray)
        UNPIN_LONG_ARRAY(ENVONLY, rarray, larray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToLong___3B */

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble___3B
    (JNIEnv *env, jclass clss, jbyteArray bdata)  /* IN: array of bytes */
{
    jdoubleArray  rarray;
    jboolean      bb;
    jdouble      *darray = NULL;
    jdouble      *iap = NULL;
    jbyte        *barr = NULL;
    char         *bp = NULL;
    int           blen;
    int           ii;
    int           len;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToDouble: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToDouble: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToDouble: bdata length < 0");
    }

    len = blen / (int)sizeof(jdouble);

    if (NULL == (rarray = ENVPTR->NewDoubleArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_DOUBLE_ARRAY(ENVONLY, rarray, darray, &bb, "byteToDouble: double array not pinned");

    bp = (char *)barr;
    iap = darray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    } /* end for */

done:
    if (darray)
        UNPIN_DOUBLE_ARRAY(ENVONLY, rarray, darray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToDouble___3B */

/* returns int [] */
JNIEXPORT jintArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToInt__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    jintArray  rarray;
    jboolean   bb;
    jint      *iarray = NULL;
    jint      *iap = NULL;
    jbyte     *barr = NULL;
    char      *bp = NULL;
    int        blen;
    int        ii;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToInt: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToInt: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToInt: bdata length < 0");
    }

    if ((start < 0) || ((int)(start + (len * (int)sizeof(jint))) > blen))
        H5_JNI_FATAL_ERROR(ENVONLY, "byteToInt: start < 0 or len exceeded buffer length");

    bp = (char *)barr + start;

    if (NULL == (rarray = ENVPTR->NewIntArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_INT_ARRAY(ENVONLY, rarray, iarray, &bb, "byteToInt: int array not pinned");

    iap = iarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jint *)bp;
        iap++;
        bp += sizeof(jint);
    } /* end for */

done:
    if (iarray)
        UNPIN_INT_ARRAY(ENVONLY, rarray, iarray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToInt__II_3B */

/* returns short [] */
JNIEXPORT jshortArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToShort__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    jshortArray  rarray;
    jboolean     bb;
    jshort      *sarray = NULL;
    jshort      *iap = NULL;
    jbyte       *barr = NULL;
    char        *bp = NULL;
    int          blen;
    int          ii;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToShort: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToShort: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToShort: bdata length < 0");
    }

    if ((start < 0) || ((int)(start + (len * (int)sizeof(jshort))) > blen))
        H5_JNI_FATAL_ERROR(ENVONLY, "byteToShort: start < 0 or len exceeded buffer length");

    bp = (char *)barr + start;

    if (NULL == (rarray = ENVPTR->NewShortArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_SHORT_ARRAY(ENVONLY, rarray, sarray, &bb, "byteToShort: short array not pinned");

    iap = sarray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jshort *)bp;
        iap++;
        bp += sizeof(jshort);
    } /* end for */

done:
    if (sarray)
        UNPIN_SHORT_ARRAY(ENVONLY, rarray, sarray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToShort__II_3B */

/* returns float [] */
JNIEXPORT jfloatArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToFloat__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    jfloatArray  rarray;
    jboolean     bb;
    jfloat      *farray = NULL;
    jfloat      *iap = NULL;
    jbyte       *barr = NULL;
    char        *bp = NULL;
    int          blen;
    int          ii;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToFloat: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToFloat: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToFloat: bdata length < 0");
    }

    if ((start < 0) || ((int)(start + (len * (int)sizeof(jfloat))) > blen))
        H5_JNI_FATAL_ERROR(ENVONLY, "byteToFloat: start < 0 or len exceeded buffer length");

    bp = (char *)barr + start;

    if (NULL == (rarray = ENVPTR->NewFloatArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_FLOAT_ARRAY(ENVONLY, rarray, farray, &bb, "byteToFloat: float array not pinned");

    iap = farray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jfloat *)bp;
        iap++;
        bp += sizeof(jfloat);
    } /* end for */

done:
    if (farray)
        UNPIN_FLOAT_ARRAY(ENVONLY, rarray, farray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToFloat__II_3B */

/* returns long [] */
JNIEXPORT jlongArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToLong__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    jlongArray  rarray;
    jboolean    bb;
    jlong      *larray = NULL;
    jlong      *iap = NULL;
    jbyte      *barr = NULL;
    char       *bp = NULL;
    int         blen;
    int         ii;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToLong: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToLong: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToLong: bdata length < 0");
    }

    if ((start < 0) || ((int)(start + (len * (int)sizeof(jlong))) > blen))
        H5_JNI_FATAL_ERROR(ENVONLY, "byteToLong: start < 0 or len exceeded buffer length");

    bp = (char *)barr + start;

    if (NULL == (rarray = ENVPTR->NewLongArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_LONG_ARRAY(ENVONLY, rarray, larray, &bb, "byteToLong: long array not pinned");

    iap = larray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jlong *)bp;
        iap++;
        bp += sizeof(jlong);
    } /* end for */

done:
    if (larray)
        UNPIN_LONG_ARRAY(ENVONLY, rarray, larray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToLong__II_3B */

/* returns double [] */
JNIEXPORT jdoubleArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToDouble__II_3B
    (JNIEnv *env, jclass clss, jint start, jint len, jbyteArray bdata)  /* IN: array of bytes */
{
    jdoubleArray  rarray;
    jboolean      bb;
    jdouble      *darray = NULL;
    jdouble      *iap = NULL;
    jbyte        *barr = NULL;
    char         *bp = NULL;
    int           blen;
    int           ii;

    UNUSED(clss);

    if (NULL == bdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "byteToDouble: byte array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, bdata, barr, &bb, "byteToDouble: byte array not pinned");

    if ((blen = ENVPTR->GetArrayLength(ENVONLY, bdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "byteToDouble: bdata length < 0");
    }

    if ((start < 0) || ((int)(start + (len * (int)sizeof(jdouble))) > blen))
        H5_JNI_FATAL_ERROR(ENVONLY, "byteToDouble: start < 0 or len exceeded buffer length");

    bp = (char *)barr + start;

    if (NULL == (rarray = ENVPTR->NewDoubleArray(ENVONLY, len)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_DOUBLE_ARRAY(ENVONLY, rarray, darray, &bb, "byteToDouble: double array not pinned");

    iap = darray;
    for (ii = 0; ii < len; ii++) {
        *iap = *(jdouble *)bp;
        iap++;
        bp += sizeof(jdouble);
    } /* end for */

done:
    if (darray)
        UNPIN_DOUBLE_ARRAY(ENVONLY, rarray, darray, rarray ? 0 : JNI_ABORT);
    if (barr)
        UNPIN_BYTE_ARRAY(ENVONLY, bdata, barr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToDouble__II_3B */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__II_3I
    (JNIEnv *env, jclass clss, jint start, jint len, jintArray idata)  /* IN: array of int */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    jint       *ip = NULL;
    jint       *iarr = NULL;
    int         ilen;
    int         blen;
    int         ii;
    int         ij;
    union things {
        int ival;
        char bytes[4];
    } u;

    UNUSED(clss);

    if (NULL == idata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "intToByte: int array is NULL");

    PIN_INT_ARRAY(ENVONLY, idata, iarr, &bb, "intToByte: int array not pinned");

    if ((ilen = ENVPTR->GetArrayLength(ENVONLY, idata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "intToByte: idata length < 0");
    }

    if ((start < 0) || (((start + len)) > ilen))
        H5_JNI_FATAL_ERROR(ENVONLY, "intToByte: start < 0 or len exceeded buffer length");

    ip = iarr + start;

    blen = ilen * (int)sizeof(jint);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, blen)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "intToByte: byte array not pinned");

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; (size_t) ij < sizeof(jint); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, rarray ? 0 : JNI_ABORT);
    if (iarr)
        UNPIN_INT_ARRAY(ENVONLY, idata, iarr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_intToByte__II_3I */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__II_3S
    (JNIEnv *env, jclass clss, jint start, jint len, jshortArray sdata)  /* IN: array of short */
{
    jbyteArray  rarray;
    jboolean    bb;
    jshort     *ip = NULL;
    jshort     *sarr = NULL;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ilen;
    int         blen;
    int         ii;
    int         ij;
    union things {
        short ival;
        char bytes[4];
    } u;

    UNUSED(clss);

    if (NULL == sdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "shortToByte: short array is NULL");

    PIN_SHORT_ARRAY(ENVONLY, sdata, sarr, &bb, "shortToByte: short array not pinned");

    if ((ilen = ENVPTR->GetArrayLength(ENVONLY, sdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "shortToByte: sdata length < 0");
    }

    if ((start < 0) || (((start + len)) > ilen))
        H5_JNI_FATAL_ERROR(ENVONLY, "shortToByte: start < 0 or len exceeded buffer length");

    ip = sarr + start;

    blen = ilen * (int)sizeof(jshort);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, blen)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "shortToByte: byte array not pinned");

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; (size_t) ij < sizeof(jshort); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, rarray ? 0 : JNI_ABORT);
    if (sarr)
        UNPIN_SHORT_ARRAY(ENVONLY, sdata, sarr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_shortToByte__II_3S */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__II_3F
    (JNIEnv *env, jclass clss, jint start, jint len, jfloatArray fdata)  /* IN: array of float */
{
    jbyteArray  rarray;
    jboolean    bb;
    jfloat     *ip = NULL;
    jfloat     *farr = NULL;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ilen;
    int         blen;
    int         ii;
    int         ij;
    union things {
        float ival;
        char bytes[4];
    } u;

    UNUSED(clss);

    if (NULL == fdata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "floatToByte: float array is NULL");

    PIN_FLOAT_ARRAY(ENVONLY, fdata, farr, &bb, "floatToByte: float array not pinned");

    if ((ilen = ENVPTR->GetArrayLength(ENVONLY, fdata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "floatToByte: fdata length < 0");
    }

    if ((start < 0) || (((start + len)) > ilen))
        H5_JNI_FATAL_ERROR(ENVONLY, "floatToByte: start < 0 or len exceeded buffer length");

    ip = farr + start;

    blen = ilen * (int)sizeof(jfloat);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, blen)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "floatToByte: byte array not pinned");

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; (size_t) ij < sizeof(jfloat); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, rarray ? 0 : JNI_ABORT);
    if (farr)
        UNPIN_FLOAT_ARRAY(ENVONLY, fdata, farr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_floatToByte__II_3F */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__II_3D
    (JNIEnv *env, jclass clss, jint start, jint len, jdoubleArray ddata)  /* IN: array of double */
{
    jbyteArray  rarray;
    jboolean    bb;
    jdouble    *ip = NULL;
    jdouble    *darr = NULL;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ilen;
    int         blen;
    int         ii;
    int         ij;
    union things {
        double ival;
        char bytes[8];
    } u;

    UNUSED(clss);

    if (NULL == ddata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "doubleToByte: double array is NULL");

    PIN_DOUBLE_ARRAY(ENVONLY, ddata, darr, &bb, "doubleToByte: double array not pinned");

    if ((ilen = ENVPTR->GetArrayLength(ENVONLY, ddata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "doubleToByte: ddata length < 0");
    }

    if ((start < 0) || (((start + len)) > ilen))
        H5_JNI_FATAL_ERROR(ENVONLY, "doubleToByte: start < 0 or len exceeded buffer length");

    ip = darr + start;

    blen = ilen * (int)sizeof(jdouble);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, blen)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "doubleToByte: byte array not pinned");

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; (size_t) ij < sizeof(jdouble); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, rarray ? 0 : JNI_ABORT);
    if (darr)
        UNPIN_DOUBLE_ARRAY(ENVONLY, ddata, darr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_doubleToByte__II_3D */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__II_3J
    (JNIEnv *env, jclass clss, jint start, jint len, jlongArray ldata)  /* IN: array of long */
{
    jbyteArray  rarray;
    jboolean    bb;
    jlong      *ip = NULL;
    jlong      *larr = NULL;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ilen;
    int         blen;
    int         ii;
    int         ij;
    union things {
        jlong ival;
        char bytes[8];
    } u;

    UNUSED(clss);

    if (NULL == ldata)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "longToByte: long array is NULL");

    PIN_LONG_ARRAY(ENVONLY, ldata, larr, &bb, "longToByte: long array not pinned");

    if ((ilen = ENVPTR->GetArrayLength(ENVONLY, ldata)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "longToByte: ldata length < 0");
    }

    if ((start < 0) || (((start + len)) > ilen))
        H5_JNI_FATAL_ERROR(ENVONLY, "longToByte: start < 0 or len exceeded buffer length");

    ip = larr + start;

    blen = ilen * (int)sizeof(jlong);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, blen)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "longToByte: byte array not pinned");

    bap = barray;
    for (ii = 0; ii < len; ii++) {
        u.ival = *ip++;
        for (ij = 0; (size_t) ij < sizeof(jlong); ij++) {
            *bap = u.bytes[ij];
            bap++;
        } /* end for */
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, rarray ? 0 : JNI_ABORT);
    if (larr)
        UNPIN_LONG_ARRAY(ENVONLY, ldata, larr, JNI_ABORT);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_longToByte__II_3J */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_intToByte__I
    (JNIEnv *env, jclass clss, jint idata)  /* IN: int */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        int ival;
        char bytes[sizeof(int)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jint))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "intToByte: byte array not pinned");

    bap = barray;
    u.ival = idata;
    for (ij = 0; (size_t) ij < sizeof(jint); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_intToByte__I */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_floatToByte__F
    (JNIEnv *env, jclass clss, jfloat fdata)  /* IN: float */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        float ival;
        char bytes[sizeof(float)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jfloat))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "floatToByte: byte array not pinned");

    bap = barray;
    u.ival = fdata;
    for (ij = 0; (size_t) ij < sizeof(jfloat); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_floatToByte__F */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_shortToByte__S
    (JNIEnv *env, jclass clss, jshort sdata)  /* IN: short */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        short ival;
        char bytes[sizeof(short)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jshort))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "shortToByte: byte array not pinned");

    bap = barray;
    u.ival = sdata;
    for (ij = 0; (size_t) ij < sizeof(jshort); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_shortToByte__S */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_doubleToByte__D
    (JNIEnv *env, jclass clss, jdouble ddata)  /* IN: double */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        double ival;
        char bytes[sizeof(double)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jdouble))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "doubleToByte: byte array not pinned");

    bap = barray;
    u.ival = ddata;
    for (ij = 0; (size_t) ij < sizeof(jdouble); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_doubleToByte__D */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_longToByte__J
    (JNIEnv *env, jclass clss, jlong ldata)  /* IN: long */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        jlong ival;
        char bytes[sizeof(jlong)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jlong))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "longToByte: byte array not pinned");

    bap = barray;
    u.ival = ldata;
    for (ij = 0; (size_t) ij < sizeof(jlong); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_longToByte__J */

/* returns byte [] */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_HDFNativeData_byteToByte__B
    (JNIEnv *env, jclass clss, jbyte bdata)  /* IN: byte */
{
    jbyteArray  rarray;
    jboolean    bb;
    jbyte      *barray = NULL;
    jbyte      *bap = NULL;
    int         ij;
    union things {
        jbyte ival;
        char bytes[sizeof(jbyte)];
    } u;

    UNUSED(clss);

    if (NULL == (rarray = ENVPTR->NewByteArray(ENVONLY, sizeof(jbyte))))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, rarray, barray, &bb, "byteToByte: byte array not pinned");

    bap = barray;
    u.ival = bdata;
    for (ij = 0; (size_t) ij < sizeof(jbyte); ij++) {
        *bap = u.bytes[ij];
        bap++;
    } /* end for */

done:
    if (barray)
        UNPIN_BYTE_ARRAY(ENVONLY, rarray, barray, 0);

    return rarray;
} /* end Java_hdf_hdf5lib_HDFNativeData_byteToByte__B */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
