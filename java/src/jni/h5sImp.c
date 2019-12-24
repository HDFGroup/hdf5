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

#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5sImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Screate
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Screate
    (JNIEnv *env, jclass clss, jint type)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Screate((H5S_class_t) type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Screate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Screate_simple
 * Signature: (I[J[J)G
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Screate_1simple
    (JNIEnv *env, jclass clss, jint rank, jlongArray dims, jlongArray maxdims)
{
    jboolean  isCopy;
    hsize_t  *sa = NULL;
    hsize_t  *msa = NULL;
    hsize_t  *lp = NULL;
    jlong    *dimsP = NULL, *maxdimsP = NULL;
    jlong    *jlp = NULL;
    jsize     drank = 0, mrank = 0;
    int       i;
    hid_t     retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (rank < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: rank is invalid");
    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: dims is NULL");

    if ((drank = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: dims array length < 0");
    }

    if (drank != rank)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: dims rank is invalid");

    if (NULL != maxdims) {
        if ((mrank = ENVPTR->GetArrayLength(ENVONLY, maxdims)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: maxdims array length < 0");
        }

        if (mrank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Screate_simple: maxdims rank is invalid");
    }

    PIN_LONG_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Screate_simple: dims not pinned");

    if (NULL == (sa = lp = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Screate_simple: failed to allocate dims buffer")

    jlp = (jlong *) dimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end for */

    if (NULL == maxdims) {
        maxdimsP = NULL;
        msa = (hsize_t *)maxdimsP;
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, &isCopy, "H5Screate_simple: maxdims not pinned");

        if (NULL == (msa = lp = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Screate_simple: failed to allocate maxdims buffer")

        jlp = (jlong *) maxdimsP;
        for (i = 0; i < mrank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    }

    if ((retVal = H5Screate_simple(rank, (const hsize_t *)sa, (const hsize_t *)msa)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (msa)
        HDfree(msa);
    if (maxdimsP)
        UNPIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, JNI_ABORT);
    if (sa)
        HDfree(sa);
    if (dimsP)
        UNPIN_LONG_ARRAY(ENVONLY, dims, dimsP, JNI_ABORT);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Screate_1simple */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Scopy
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Scopy
    (JNIEnv *env, jclass clss, jlong space_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Scopy(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Scopy */

#ifdef notdef
// 10/28/99 -- added code to copy the array -- this is not used,
// but serves as a reminder in case we try to implement this in
// the future....
/*
 *  Note:  the argument coord is actually long coord[][], which has been
 *         flattened by the caller.
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_elements
 * Signature: (JII[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1elements
    (JNIEnv *env, jclass clss, jlong space_id, jint op, jint num_elemn, jlongArray coords)
{
    jboolean  isCopy;
    hssize_t *sa = NULL;
    jlong    *P = NULL;
    jint      i;
    int       rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == coords)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sselect_elements: coords is NULL");

    PIN_LONG_ARRAY(ENVONLY, coords, P, &isCopy, "H5Sselect_elements: coords not pinned");

    if (NULL == (sa = (hssize_t *) HDmalloc( (size_t)num_elemn * 2 * sizeof(hssize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_elements: failed to allocate coordinate buffer");

    for (i = 0; i < (num_elemn * 2); i++) {
        sa[i] = P[i];
    } /* end for */

    if ((status = H5Sselect_elements(space_id, (H5S_seloper_t)op, num_elemn, (const hssize_t **)&sa)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (sa)
        HDfree(sa);
    if (P)
        UNPIN_LONG_ARRAY(ENVONLY, coords, P, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1elements */
#endif

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_elements
 * Signature: (JII[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1elements
    (JNIEnv *env, jclass clss, jlong space_id, jint op, jint num_elemn, jbyteArray coords)
{
    jboolean  isCopy;
    hsize_t  *lp = NULL;
    hsize_t  *llp = NULL;
    jlong    *jlp = NULL;
    jbyte    *P = NULL;
    jsize     size;
    int       ii;
    int       nlongs;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == coords)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sselect_elements: coords is NULL");

    PIN_BYTE_ARRAY(ENVONLY, coords, P, &isCopy, "H5Sselect_elements: coords not pinned");

    if ((size = ENVPTR->GetArrayLength(ENVONLY, coords)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_elements: coords array length < 0");
    }

    nlongs = (int)((size_t)size / sizeof(jlong));

    if (NULL == (lp = (hsize_t *) HDmalloc((size_t)nlongs * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_elements: failed to allocate coordinate buffer");

    jlp = (jlong *) P;
    llp = lp;
    for (ii = 0; ii < nlongs; ii++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end for */

    if ((status = H5Sselect_elements(space_id, (H5S_seloper_t)op, (size_t)num_elemn, (const hsize_t *)llp)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (llp)
        HDfree(llp);
    if (P)
        UNPIN_BYTE_ARRAY(ENVONLY, coords, P, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1elements */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_all
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1all
    (JNIEnv *env, jclass clss, jlong space_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Sselect_all(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1all */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_none
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1none
    (JNIEnv *env, jclass clss, jlong space_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Sselect_none(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1none */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_valid
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1valid
    (JNIEnv *env, jclass clss, jlong space_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Sselect_valid(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1valid */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_npoints
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1npoints
    (JNIEnv *env, jclass clss, jlong space_id)
{
    hssize_t retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Sget_simple_extent_npoints(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1npoints */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_npoints
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1npoints
    (JNIEnv *env, jclass clss, jlong space_id)
{
    hssize_t retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Sget_select_npoints(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1npoints */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_type
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1type
    (JNIEnv *env, jclass clss, jlong space_id)
{
    int retVal = -1;

    UNUSED(clss);

    if (H5S_SEL_ERROR == (retVal = H5Sget_select_type(space_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_ndims
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1ndims
    (JNIEnv *env, jclass clss, jlong space_id)
{
    int retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Sget_simple_extent_ndims(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1ndims */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_dims
 * Signature: (J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1dims
    (JNIEnv *env, jclass clss, jlong space_id, jlongArray dims, jlongArray maxdims)
{
    jboolean  isCopy;
    hsize_t  *sa = NULL;
    hsize_t  *msa = NULL;
    jlong    *dimsP = NULL, *maxdimsP = NULL;
    int       i;
    int       rank = -1;
    int       mrank;
    int       status = -1;

    UNUSED(clss);

    if (NULL == dims) {
        dimsP = NULL;
        sa = (hsize_t *) dimsP;
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Sget_simple_extent_dims: dims not pinned");

        if ((rank = (int)ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_simple_extent_dims: dims array length < 0");
        }

        if (NULL == (sa = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_simple_extent_dims: failed to allocate dimension buffer");
    }

    if (NULL == maxdims) {
        maxdimsP = NULL;
        msa = (hsize_t *) maxdimsP;
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, &isCopy, "H5Sget_simple_extent_dims: maxdims not pinned");

        if ((mrank = (int) ENVPTR->GetArrayLength(ENVONLY, maxdims)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_simple_extent_dims: maxdims array length < 0");
        }

        if (rank < 0)
            rank = mrank;
        else if (mrank != rank)
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_simple_extent_dims: maxdims rank not same as dims");

        if (NULL == (msa = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_simple_extent_dims: failed to allocate maximum dimension buffer");
    }

    if ((status = H5Sget_simple_extent_dims(space_id, (hsize_t *)sa, (hsize_t *)msa)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL != dimsP) {
        for (i = 0; i < rank; i++) {
            dimsP[i] = (jlong)sa[i];
        } /* end for */
    }

    if (NULL != maxdimsP) {
        for (i = 0; i < rank; i++) {
            maxdimsP[i] = (jlong)msa[i];
        } /* end for */
    }

done:
    if (msa)
        HDfree(msa);
    if (maxdimsP)
        UNPIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, (status < 0) ? JNI_ABORT : 0);
    if (sa)
        HDfree(sa);
    if (dimsP)
        UNPIN_LONG_ARRAY(ENVONLY, dims, dimsP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1dims */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_simple_extent_type
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1type
    (JNIEnv *env, jclass clss, jlong space_id)
{
    H5S_class_t retVal = H5S_NO_CLASS;

    UNUSED(clss);

    if (space_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_simple_extent_type: space_id < 0");

    if (H5S_NO_CLASS == (retVal = H5Sget_simple_extent_type(space_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1simple_1extent_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sset_extent_simple
 * Signature: (JI[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sset_1extent_1simple
    (JNIEnv *env, jclass clss, jlong space_id, jint rank, jlongArray dims, jlongArray maxdims)
{
    jboolean  isCopy;
    hsize_t  *sa = NULL;
    hsize_t  *msa = NULL;
    hsize_t  *lp = NULL;
    jlong    *dimsP = NULL, *maxdimsP = NULL;
    jlong    *jlp = NULL;
    jsize     drank, mrank;
    int       i;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sset_extent_simple: dims is NULL");

    if ((drank = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sset_extent_simple: dims array length < 0");
    }

    if (drank != rank)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sset_extent_simple: dims rank is invalid");

    if (NULL != maxdims) {
        if ((mrank = ENVPTR->GetArrayLength(ENVONLY, maxdims)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sset_extent_simple: maxdims array length < 0");
        }

        if (mrank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sset_extent_simple: maxdims rank is invalid");
    }

    PIN_LONG_ARRAY(ENVONLY, dims, dimsP, &isCopy, "H5Sset_extent_simple: dims not pinned");

    if (NULL == (sa = lp = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sset_extent_simple: failed to allocate dimension buffer");

    jlp = (jlong *) dimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end for */

    if (NULL == maxdims) {
        maxdimsP = NULL;
        msa = (hsize_t *) maxdimsP;
    }
    else {
        PIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, &isCopy, "H5Sset_extent_simple: maxdims not pinned");

        if (NULL == (msa = lp = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sset_extent_simple: failed to allocate maximum dimension buffer");

        jlp = (jlong *) maxdimsP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    }

    if ((status = H5Sset_extent_simple(space_id, rank, (hsize_t *) sa, (hsize_t *) msa)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (msa)
        HDfree(msa);
    if (maxdimsP)
        UNPIN_LONG_ARRAY(ENVONLY, maxdims, maxdimsP, JNI_ABORT);
    if (sa)
        HDfree(sa);
    if (dimsP)
        UNPIN_LONG_ARRAY(ENVONLY, dims, dimsP, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sset_1extent_1simple */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sis_simple
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Sis_1simple
    (JNIEnv *env, jclass clss, jlong space_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Sis_simple(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Sis_1simple */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Soffset_simple
 * Signature: (J[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Soffset_1simple
    (JNIEnv *env, jclass clss, jlong space_id, jbyteArray offset)
{
    jboolean  isCopy;
    hssize_t *sa = NULL;
    hssize_t *lp = NULL;
    size_t    rank;
    jsize     i;
    jbyte    *P = NULL;
    jlong    *jlp = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL != offset) {
        PIN_BYTE_ARRAY(ENVONLY, offset, P, &isCopy, "H5Soffset_simple: offset not pinned");

        if ((i = ENVPTR->GetArrayLength(ENVONLY, offset)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Soffset_simple: offset array length < 0");
        }

        rank = (size_t) i / sizeof(jlong);

        if (NULL == (sa = lp = (hssize_t *) HDmalloc((size_t)rank * sizeof(hssize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Soffset_simple: failed to allocate offset buffer");

        jlp = (jlong *) P;
        for (i = 0; (size_t) i < rank; i++) {
            *lp = (hssize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    }
    else {
        P = NULL;
        sa = (hssize_t *) P;
    }

    if ((status = H5Soffset_simple(space_id, sa)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (sa)
        HDfree(sa);
    if (P)
        UNPIN_BYTE_ARRAY(ENVONLY, offset, P, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Soffset_1simple */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sextent_copy
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sextent_1copy
    (JNIEnv *env, jclass clss, jlong space_id, jlong src_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Sextent_copy(space_id, src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sextent_1copy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sextent_equal
 * Signature: (JJ)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Sextent_1equal
    (JNIEnv *env, jclass clss, jlong space_id, jlong src_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Sextent_equal(space_id, src_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Sextent_1equal */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sset_extent_none
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sset_1extent_1none
    (JNIEnv *env, jclass clss, jlong space_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Sset_extent_none(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sset_1extent_1none */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sselect_hyperslab
 * Signature: (JI[J[J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sselect_1hyperslab
    (JNIEnv *env, jclass clss, jlong space_id, jint op, jlongArray start, jlongArray stride, jlongArray count, jlongArray block)
{
    jboolean  isCopy;
    hsize_t  *strt = NULL, *strd = NULL, *cnt = NULL, *blk = NULL;
    hsize_t  *lp = NULL;
    jlong    *startP = NULL, *strideP = NULL, *countP = NULL, *blockP = NULL;
    jlong    *jlp = NULL;
    jsize     start_rank, stride_rank, count_rank, block_rank;
    int       i;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == start)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: start is NULL");
    if (NULL == count)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: count is NULL");

    if ((start_rank = ENVPTR->GetArrayLength(ENVONLY, start)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: start array length < 0");
    }
    if ((count_rank = ENVPTR->GetArrayLength(ENVONLY, count)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: count array length < 0");
    }

    if (start_rank != count_rank)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: count and start have different rank!");

    PIN_LONG_ARRAY(ENVONLY, start, startP, &isCopy, "H5Sselect_hyperslab: start not pinned");

    if (NULL == (strt = lp = (hsize_t *) HDmalloc((size_t)start_rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_hyperslab: failed to allocate start buffer");

    jlp = (jlong *) startP;
    for (i = 0; i < start_rank; i++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end if */

    PIN_LONG_ARRAY(ENVONLY, count, countP, &isCopy, "H5Sselect_hyperslab: count not pinned");

    if (NULL == (cnt = lp = (hsize_t *) HDmalloc((size_t)count_rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_hyperslab: failed to allocate count buffer");

    jlp = (jlong *) countP;
    for (i = 0; i < count_rank; i++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end if */

    if (NULL == stride) {
        strideP = NULL;
        strd = (hsize_t *) strideP;
    }
    else {
        if ((stride_rank = ENVPTR->GetArrayLength(ENVONLY, stride)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: stride array length < 0");
        }

        if (stride_rank != start_rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: stride and start have different rank!");

        PIN_LONG_ARRAY(ENVONLY, stride, strideP, &isCopy, "H5Sselect_hyperslab: stride not pinned");

        if (NULL == (strd = lp = (hsize_t *) HDmalloc((size_t)stride_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_hyperslab: failed to allocate stride buffer");

        jlp = (jlong *) strideP;
        for (i = 0; i < stride_rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end if */
    }

    if (NULL == block) {
        blockP = NULL;
        blk = (hsize_t *) blockP;
    }
    else {
        if ((block_rank = ENVPTR->GetArrayLength(ENVONLY, block)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: block array length < 0");
        }

        if (block_rank != start_rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sselect_hyperslab: block and start have different rank!");

        PIN_LONG_ARRAY(ENVONLY, block, blockP, &isCopy, "H5Sselect_hyperslab: block not pinned");

        if (NULL == (blk = lp = (hsize_t *) HDmalloc((size_t)block_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sselect_hyperslab: failed to allocate block buffer");

        jlp = (jlong *) blockP;
        for (i = 0; i < block_rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    }

    if ((status = H5Sselect_hyperslab(space_id, (H5S_seloper_t) op, (const hsize_t *) strt, (const hsize_t *) strd,
                (const hsize_t *) cnt, (const hsize_t *) blk)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (blk)
        HDfree(blk);
    if (blockP)
        UNPIN_LONG_ARRAY(ENVONLY, block, blockP, JNI_ABORT);
    if (strd)
        HDfree(strd);
    if (strideP)
        UNPIN_LONG_ARRAY(ENVONLY, stride, strideP, JNI_ABORT);
    if (cnt)
        HDfree(cnt);
    if (countP)
        UNPIN_LONG_ARRAY(ENVONLY, count, countP, JNI_ABORT);
    if (strt)
        HDfree(strt);
    if (startP)
        UNPIN_LONG_ARRAY(ENVONLY, start, startP, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sselect_1hyperslab */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Sclose
    (JNIEnv *env, jclass clss, jlong space_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Sclose(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Sclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_hyper_nblocks
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1nblocks
    (JNIEnv *env, jclass clss, jlong spaceid)
{
    hssize_t retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Sget_select_hyper_nblocks((hid_t) spaceid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1nblocks */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_elem_npoints
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1elem_1npoints
    (JNIEnv *env, jclass clss, jlong spaceid)
{
    hssize_t retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Sget_select_elem_npoints((hid_t) spaceid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1elem_1npoints */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_hyper_blocklist
 * Signature: (JJJ[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1blocklist
    (JNIEnv *env, jclass clss, jlong spaceid, jlong startblock, jlong numblocks, jlongArray buf)
{
    jboolean  isCopy;
    hsize_t  *ba = NULL;
    size_t    i, buf_size;
    jlong    *bufP = NULL;
    jsize     buflen;
    int       rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_hyper_blocklist: buf is NULL");
    if (numblocks < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_hyper_blocklist: numblocks < 0");

    if ((rank = H5Sget_simple_extent_ndims(spaceid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (rank == 0)
        rank = 1;

    if ((buflen = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_hyper_blocklist: buf array length < 0");
    }

    if (buflen < (numblocks * rank))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_hyper_blocklist: buffer input array too small");

    PIN_LONG_ARRAY(ENVONLY, buf, bufP, &isCopy, "H5Sget_select_hyper_blocklist: buffer not pinned");

    buf_size = (size_t) numblocks * (size_t) 2 * (size_t) rank * sizeof(hsize_t);
    if (NULL == (ba = (hsize_t *) HDmalloc(buf_size)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_select_hyper_blocklist: failed to allocate block list buffer");

    if ((status = H5Sget_select_hyper_blocklist((hid_t)spaceid, (hsize_t) startblock, (hsize_t) numblocks, (hsize_t *)ba)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (buf_size / sizeof(hsize_t)); i++) {
        bufP[i] = (jlong) ba[i];
    } /* end for */

done:
    if (ba)
        HDfree(ba);
    if (bufP)
        UNPIN_LONG_ARRAY(ENVONLY, buf, bufP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1hyper_1blocklist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_elem_pointlist
 * Signature: (JJJ[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1elem_1pointlist
    (JNIEnv *env, jclass clss, jlong spaceid, jlong startpoint, jlong numpoints, jlongArray buf)
{
    jboolean  isCopy;
    hsize_t  *ba = NULL;
    jlong    *bufP = NULL;
    jsize     buflen;
    int       i, rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_elem_pointlist: buffer is NULL");

    if ((rank = H5Sget_simple_extent_ndims(spaceid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (rank == 0)
        rank = 1;

    if ((buflen = ENVPTR->GetArrayLength(ENVONLY, buf)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_elem_pointlist: buf array length < 0");
    }

    if (buflen < (numpoints * rank))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_elem_pointlist: buffer input array too small");

    PIN_LONG_ARRAY(ENVONLY, buf, bufP, &isCopy, "H5Sget_select_elem_pointlist: buffer not pinned");

    if (NULL == (ba = (hsize_t *) HDmalloc(((size_t)numpoints * (size_t)rank) * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_select_elem_pointlist: failed to allocate point list buffer");

    if ((status = H5Sget_select_elem_pointlist((hid_t)spaceid, (hsize_t)startpoint, (hsize_t)numpoints, (hsize_t *)ba)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (numpoints * rank); i++) {
        bufP[i] = (jlong) ba[i];
    } /* end for */

done:
    if (ba)
        HDfree(ba);
    if (bufP)
        UNPIN_LONG_ARRAY(ENVONLY, buf, bufP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end if */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_select_bounds
 * Signature: (J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1select_1bounds
    (JNIEnv *env, jclass clss, jlong spaceid, jlongArray start, jlongArray end)
{
    jboolean  isCopy;
    hsize_t  *strt = NULL;
    hsize_t  *en = NULL;
    size_t    i;
    jlong    *startP = NULL, *endP = NULL;
    jsize     rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == start)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_bounds: start is NULL");
    if (NULL == end)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_bounds: end is NULL");

    PIN_LONG_ARRAY(ENVONLY, start, startP, &isCopy, "H5Sget_select_bounds: start not pinned");

    if ((rank = ENVPTR->GetArrayLength(ENVONLY, start)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_select_bounds: start array length < 0");
    }

    if (NULL == (strt = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_select_bounds: failed to allocate start buffer");

    PIN_LONG_ARRAY(ENVONLY, end, endP, &isCopy, "H5Sget_select_bounds: end not pinned");

    if (NULL == (en = (hsize_t *) HDmalloc((size_t)rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_select_bounds: failed to allocate end buffer");

    if ((status = H5Sget_select_bounds((hid_t) spaceid, (hsize_t *) strt, (hsize_t *) en)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (size_t) rank; i++) {
        startP[i] = (jlong) strt[i];
        endP[i] = (jlong) en[i];
    } /* end for */

done:
    if (en)
        HDfree(en);
    if (endP)
        UNPIN_LONG_ARRAY(ENVONLY, end, endP, (status < 0) ? JNI_ABORT : 0);
    if (strt)
        HDfree(strt);
    if (startP)
        UNPIN_LONG_ARRAY(ENVONLY, start, startP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1bounds */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sencode
 * Signature: (J)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Sencode
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    unsigned char *bufPtr = NULL;
    size_t         buf_size = 0;
    herr_t         status = FAIL;
    jbyteArray     returnedArray = NULL;

    UNUSED(clss);

    if (obj_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sencode: invalid object ID");

    if ((status = H5Sencode(obj_id, NULL, &buf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (buf_size == 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sencode: buf_size = 0");

    if (NULL == (bufPtr = (unsigned char *) HDcalloc((size_t) 1, buf_size)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Sencode: failed to allocate encoding buffer");

    if ((status = H5Sencode((hid_t) obj_id, bufPtr, &buf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (returnedArray = ENVPTR->NewByteArray(ENVONLY, (jsize) buf_size)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetByteArrayRegion(ENVONLY, returnedArray, 0, (jsize) buf_size, (jbyte *) bufPtr);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (bufPtr)
        HDfree(bufPtr);

    return returnedArray;
} /* end Java_hdf_hdf5lib_H5_H5Sencode */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sdecode
 * Signature: ([B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sdecode
    (JNIEnv *env, jclass clss, jbyteArray buf)
{
    jboolean  isCopy;
    jbyte    *bufP = NULL;
    hid_t     sid = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == buf)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Sdecode: buffer is NULL");

    PIN_BYTE_ARRAY(ENVONLY, buf, bufP, &isCopy, "H5Sdecode: buffer not pinned");

    if ((sid = H5Sdecode(bufP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (bufP)
        UNPIN_BYTE_ARRAY(ENVONLY, buf, bufP, JNI_ABORT);

    return (jlong)sid;
} /* end Java_hdf_hdf5lib_H5_H5Sdecode */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sis_regular_hyperslab
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Sis_1regular_1hyperslab
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Sis_regular_hyperslab((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Sis_1regular_1hyperslab */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sget_regular_hyperslab
 * Signature: (J[J[J[J[J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Sget_1regular_1hyperslab
    (JNIEnv *env, jclass clss, jlong space_id, jlongArray start, jlongArray stride, jlongArray count, jlongArray block)
{
    jboolean  isCopy;
    hsize_t  *strt = NULL, *strd = NULL, *cnt = NULL, *blk = NULL;
    jlong    *startP = NULL, *strideP = NULL, *countP = NULL, *blockP = NULL;
    jsize     start_rank = -1, stride_rank = -1, count_rank = -1, block_rank = -1;
    int       i, rank = -1;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (space_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: invalid dataspace ID");

    if ((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == start) {
        startP = NULL;
        strt = (hsize_t *) startP;
    }
    else {
        if ((start_rank = ENVPTR->GetArrayLength(ENVONLY, start)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: start array length < 0");
        }

        if (start_rank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: start rank doesn't match dataspace rank!");

        PIN_LONG_ARRAY(ENVONLY, start, startP, &isCopy, "H5Sget_regular_hyperslab: start not pinned");

        if (NULL == (strt = (hsize_t *) HDmalloc((size_t)start_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_regular_hyperslab: failed to allocate start buffer");
    }

    if (NULL == stride) {
        strideP = NULL;
        strd = (hsize_t *) strideP;
    }
    else {
        if ((stride_rank = ENVPTR->GetArrayLength(ENVONLY, stride)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: stride array length < 0");
        }

        if (stride_rank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: stride rank doesn't match dataspace rank!");

        PIN_LONG_ARRAY(ENVONLY, stride, strideP, &isCopy, "H5Sget_regular_hyperslab: stride not pinned");

        if (NULL == (strd = (hsize_t *) HDmalloc((size_t)stride_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_regular_hyperslab: failed to allocate stride buffer");
    }

    if (NULL == count) {
        countP = NULL;
        cnt = (hsize_t *) countP;
    }
    else {
        if ((count_rank = ENVPTR->GetArrayLength(ENVONLY, count)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: count array length < 0");
        }

        if (count_rank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: count rank doesn't match dataspace rank!");

        PIN_LONG_ARRAY(ENVONLY, count, countP, &isCopy, "H5Sget_regular_hyperslab: count not pinned");

        if (NULL == (cnt = (hsize_t *) HDmalloc((size_t)count_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_regular_hyperslab: failed to allocate count buffer");
    }

    if (NULL == block) {
        blockP = NULL;
        blk = (hsize_t *) blockP;
    }
    else {
        if ((block_rank = ENVPTR->GetArrayLength(ENVONLY, block)) < 0) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: block array length < 0");
        }

        if (block_rank != rank)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Sget_regular_hyperslab: block rank doesn't match dataspace rank!");

        PIN_LONG_ARRAY(ENVONLY, block, blockP, &isCopy, "H5Sget_regular_hyperslab: block not pinned");

        if (NULL == (blk = (hsize_t *) HDmalloc((size_t)block_rank * sizeof(hsize_t))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Sget_regular_hyperslab: failed to allocate block buffer");
    }

    if ((status = H5Sget_regular_hyperslab(space_id, (hsize_t *) strt, (hsize_t *) strd, (hsize_t *) cnt, (hsize_t *) blk)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < start_rank; i++) {
        startP[i] = (jlong) strt[i];
        countP[i] = (jlong) cnt[i];
        strideP[i] = (jlong) strd[i];
        blockP[i] = (jlong) blk[i];
    } /* end for */

done:
    if (blk)
        HDfree(blk);
    if (blockP)
        UNPIN_LONG_ARRAY(ENVONLY, block, blockP, (status < 0) ? JNI_ABORT : 0);
    if (cnt)
        HDfree(cnt);
    if (countP)
        UNPIN_LONG_ARRAY(ENVONLY, count, countP, (status < 0) ? JNI_ABORT : 0);
    if (strd)
        HDfree(strd);
    if (strideP)
        UNPIN_LONG_ARRAY(ENVONLY, stride, strideP, (status < 0) ? JNI_ABORT : 0);
    if (strt)
        HDfree(strt);
    if (startP)
        UNPIN_LONG_ARRAY(ENVONLY, start, startP, (status < 0) ? JNI_ABORT : 0);

    return;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1regular_1hyperslab */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
