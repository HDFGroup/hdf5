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

#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5sImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Screate
 * Signature: (I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Screate
    (JNIEnv *env, jclass clss, jint type)
{
    hid_t retVal = -1;

    retVal = H5Screate((H5S_class_t) type);
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;
    jlong *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa = NULL;
    hsize_t *msa = NULL;
    int i;
    int drank, mrank;
    hsize_t *lp;
    jlong *jlp;

    if (rank < 0) {
        h5badArgument(env, "H5Screate_simple:  rank is invalid");
    } /* end if */
    else if (dims == NULL) {
        h5nullArgument(env, "H5Screate_simple:  dims is NULL");
    } /* end else if */
    else {
        drank = (int)ENVPTR->GetArrayLength(ENVPAR dims);
        if (drank != rank) {
            h5badArgument(env, "H5Screate_simple:  dims rank is invalid");
            return -1;
        } /* end if */
        if (maxdims != NULL) {
            mrank = (int)ENVPTR->GetArrayLength(ENVPAR maxdims);
            if (mrank != rank) {
                h5badArgument(env, "H5Screate_simple:  maxdims rank is invalid");
                return -1;
            } /* end if */
        } /* end if */
        dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims, &isCopy);
        if (dimsP == NULL) {
            h5JNIFatalError(env, "H5Screate_simple:  dims not pinned");
            return -1;
        } /* end if */

        sa = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (sa == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            h5JNIFatalError(env, "H5Screate_simple:  dims not converted to hsize_t");
            return -1;
        } /* end if */

        jlp = (jlong *) dimsP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */

        if (maxdims == NULL) {
            maxdimsP = NULL;
            msa = (hsize_t *)maxdimsP;
        } /* end if */
        else {
            maxdimsP = ENVPTR->GetLongArrayElements(ENVPAR maxdims, &isCopy);
            if (maxdimsP == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                HDfree(sa);
                h5JNIFatalError(env, "H5Screate_simple:  maxdims not pinned");
                return -1;
            } /* end if */
            msa = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
            if (msa == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
                HDfree(sa);
                h5JNIFatalError(env, "H5Screate_simple:  dims not converted to hsize_t");
                return -1;
            } /* end if */
            jlp = (jlong *)maxdimsP;
            for (i = 0; i < mrank; i++) {
                *lp = (hsize_t)*jlp;
                lp++;
                jlp++;
            } /* end for */
        } /* end else */

        retVal = H5Screate_simple(rank, (const hsize_t *)sa, (const hsize_t *)msa);

        if (maxdimsP != NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
            if (msa)
                HDfree(msa);
        } /* end if */

        ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
        if (sa)
            HDfree(sa);

        if (retVal < 0)
            h5libraryError(env);
    } /* end else */

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
    hid_t retVal = -1;

    retVal = H5Scopy(space_id);
    if (retVal < 0)
        h5libraryError(env);

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
    (JNIEnv *env, jclass clss, jlong space_id, jint op, jint num_elemn, jlongArray coord)
{
    herr_t status = -1;
    jint i;
    jlong *P;
    jboolean isCopy;
    hssize_t *sa;
    int rank;

    if (coord == NULL) {
        h5nullArgument( env, "H5Sselect_elements:  coord is NULL");
        return -1;
    } /* end if */

    P = ENVPTR->GetLongArrayElements(ENVPAR env,coord,&isCopy);
    if (P == NULL) {
        h5JNIFatalError(env, "H5Sselect_elements:  coord not pinned");
        return -1;
    } /* end if */
    sa = (hssize_t *)HDmalloc( (size_t)num_elems * 2 * sizeof(hssize_t));
    if (sa == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR env,coord,P,JNI_ABORT);
        h5JNIFatalError(env, "H5Sselect_elements:  coord array not converted to hssize_t");
        return -1;
    } /* end if */
    for (i= 0; i < (num_elsms * 2); i++) {
        sa[i] = P[i];
    } /* end for */

    status = H5Sselect_elements (space_id, (H5S_seloper_t)op, num_elemn, (const hssize_t **)&sa);
    ENVPTR->ReleaseLongArrayElements(ENVPAR env, coord, P, JNI_ABORT);
    HDfree(sa);

    if (status < 0)
        h5libraryError(env);

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
    (JNIEnv *env, jclass clss, jlong space_id, jint op, jint num_elemn, jbyteArray coord)
{
    int      ii;
    hsize_t *lp = NULL;
    hsize_t *llp;
    jlong   *jlp;
    herr_t   status = -1;
    jbyte   *P;
    jboolean isCopy;
    jsize    size;
    int      nlongs;

    if (coord == NULL) {
        h5nullArgument(env, "H5Sselect_elements:  coord is NULL");
    } /* end if */
    else {
        P = ENVPTR->GetByteArrayElements(ENVPAR coord, &isCopy);
        if (P == NULL) {
            h5JNIFatalError(env, "H5Sselect_elements:  coord not pinned");
        } /* end if */
        else {
            size = (int)ENVPTR->GetArrayLength(ENVPAR coord);
            nlongs = (int)((size_t)size / sizeof(jlong));
            lp = (hsize_t *)HDmalloc((size_t)nlongs * sizeof(hsize_t));
            jlp = (jlong *) P;
            llp = lp;
            for (ii = 0; ii < nlongs; ii++) {
                *lp = (hsize_t) *jlp;
                lp++;
                jlp++;
            } /* end for */

            status = H5Sselect_elements(space_id, (H5S_seloper_t)op, (size_t)num_elemn, (const hsize_t *)llp);

            ENVPTR->ReleaseByteArrayElements(ENVPAR coord, P, JNI_ABORT);

            if (llp)
                HDfree(llp);

            if (status < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

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
    herr_t retVal = -1;

    retVal = H5Sselect_all(space_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Sselect_none(space_id);
    if (retVal < 0)
        h5libraryError(env);

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

    bval = H5Sselect_valid(space_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    hssize_t retVal = H5Sget_simple_extent_npoints(space_id);

    if (retVal < 0)
        h5libraryError(env);

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
    hssize_t retVal = H5Sget_select_npoints(space_id);

    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Sget_select_type(space_id);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Sget_simple_extent_ndims(space_id);
    if (retVal < 0)
        h5libraryError(env);

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
    int status = -1;
    jlong *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa;
    hsize_t *msa;
    int i;
    int rank = -1;
    int mrank;

    if (dims == NULL) {
        dimsP = NULL;
        sa = (hsize_t *) dimsP;
    } /* end if */
    else {
        dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims, &isCopy);
        if (dimsP == NULL) {
            h5JNIFatalError(env, "H5Pget_simple_extent_dims:  dims not pinned");
            return -1;
        } /* end if */
        rank = (int)ENVPTR->GetArrayLength(ENVPAR dims);
        sa = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (sa == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sget_simple_extent_dims:  dims not converted to hsize_t");
            return -1;
        } /* end if */
    } /* end else */
    if (maxdims == NULL) {
        maxdimsP = NULL;
        msa = (hsize_t *) maxdimsP;
    } /* end if */
    else {
        maxdimsP = ENVPTR->GetLongArrayElements(ENVPAR maxdims, &isCopy);
        if (maxdimsP == NULL) {
            if (dimsP != NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                HDfree(sa);
            } /* end if */
            h5JNIFatalError(env, "H5Pget_simple_extent_dims:  maxdims not pinned");
            return -1;
        } /* end if */
        mrank = (int) ENVPTR->GetArrayLength(ENVPAR maxdims);
        if (rank < 0)
            rank = mrank;
        else if (mrank != rank) {
            if (dimsP != NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                HDfree(sa);
            } /* end if */
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sget_simple_extent_dims:  maxdims rank not same as dims");
            return -1;
        } /* end else if */
        msa = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (msa == NULL) {
            if (dimsP != NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
                HDfree(sa);
            } /* end if */
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sget_simple_extent_dims:  maxdims not converted to hsize_t");
            return -1;
        } /* end if */
    } /* end else */

    status = H5Sget_simple_extent_dims(space_id, (hsize_t *)sa, (hsize_t *)msa);

    if (status < 0) {
        if (dimsP != NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            HDfree(sa);
        } /* end if */
        if (maxdimsP != NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
            HDfree(msa);
        } /* end if */
        h5libraryError(env);
        return -1;
    } /* end if */

    if (dimsP != NULL) {
        for (i = 0; i < rank; i++) {
            dimsP[i] = (jlong)sa[i];
        } /* end for */
        HDfree(sa);
        ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, 0);
    } /* end if */
    if (maxdimsP != NULL) {
        for (i = 0; i < rank; i++) {
            maxdimsP[i] = (jlong)msa[i];
        } /* end for */
        HDfree(msa);
        ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, 0);
    } /* end if */

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

    if (space_id < 0)
        h5libraryError(env);

    retVal = H5Sget_simple_extent_type(space_id);

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
    herr_t status = -1;
    jlong *dimsP, *maxdimsP;
    jboolean isCopy;
    hsize_t *sa;
    hsize_t *msa;
    int i;
    int drank, mrank;
    hsize_t *lp;
    jlong *jlp;

    if (dims == NULL) {
        h5nullArgument(env, "H5Sset_simple_extent:  dims is NULL");
        return -1;
    } /* end if */
    drank = (int) ENVPTR->GetArrayLength(ENVPAR dims);
    if (drank != rank) {
        h5badArgument(env, "H5Screate_simple:  dims rank is invalid");
        return -1;
    } /* end if */
    if (maxdims != NULL) {
        mrank = (int) ENVPTR->GetArrayLength(ENVPAR maxdims);
        if (mrank != rank) {
            h5badArgument(env, "H5Screate_simple:  maxdims rank is invalid");
            return -1;
        } /* end if */
    } /* end if */
    dimsP = ENVPTR->GetLongArrayElements(ENVPAR dims, &isCopy);
    if (dimsP == NULL) {
        h5JNIFatalError(env, "H5Pset_simple_extent:  dims not pinned");
        return -1;
    } /* end if */
    sa = lp = (hsize_t *) malloc((size_t)rank * sizeof(hsize_t));
    if (sa == NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
        h5JNIFatalError(env, "H5Sset_simple_extent:  dims not converted to hsize_t");
        return -1;
    } /* end if */
    jlp = (jlong *) dimsP;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t) *jlp;
        lp++;
        jlp++;
    } /* end for */
    if (maxdims == NULL) {
        maxdimsP = NULL;
        msa = (hsize_t *) maxdimsP;
    } /* end if */
    else {
        maxdimsP = ENVPTR->GetLongArrayElements(ENVPAR maxdims, &isCopy);
        if (maxdimsP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            h5JNIFatalError(env, "H5Pset_simple_extent:  maxdims not pinned");
            return -1;
        } /* end if */
        msa = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (msa == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
            HDfree(sa);
            h5JNIFatalError(env, "H5Sset_simple_extent:  maxdims not converted to hsize_t");
            return -1;
        } /* end if */
        jlp = (jlong *) maxdimsP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    } /* end else */

    status = H5Sset_extent_simple(space_id, rank, (hsize_t *) sa, (hsize_t *) msa);

    ENVPTR->ReleaseLongArrayElements(ENVPAR dims, dimsP, JNI_ABORT);
    HDfree(sa);
    if (maxdimsP != NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR maxdims, maxdimsP, JNI_ABORT);
        HDfree(msa);
    } /* end if */

    if (status < 0)
        h5libraryError(env);

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

    bval = H5Sis_simple(space_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    herr_t    status;
    jbyte    *P = NULL;
    jboolean  isCopy;
    hssize_t *sa;
    size_t    rank;
    size_t    i;
    hssize_t *lp;
    jlong    *jlp;

    if (offset != NULL) {
        P = ENVPTR->GetByteArrayElements(ENVPAR offset, &isCopy);
        if (P == NULL) {
            h5JNIFatalError(env, "H5Soffset_simple:  offset not pinned");
            return -1;
        } /* end if */
        i = (size_t)ENVPTR->GetArrayLength(ENVPAR offset);
        rank = i / sizeof(jlong);
        sa = lp = (hssize_t *)HDmalloc((size_t)rank * sizeof(hssize_t));
        if (sa == NULL) {
            ENVPTR->ReleaseByteArrayElements(ENVPAR offset, P, JNI_ABORT);
            h5JNIFatalError(env, "H5Soffset_simple:  offset not converted to hssize_t");
            return -1;
        } /* end if */
        jlp = (jlong *) P;
        for (i = 0; i < rank; i++) {
            *lp = (hssize_t) *jlp;
            lp++;
            jlp++;
        } /* end for */
    } /* end if */
    else {
        P = NULL;
        sa = (hssize_t *) P;
    } /* end else */

    status = H5Soffset_simple(space_id, sa);
    if (P != NULL) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR offset, P, JNI_ABORT);
        HDfree(sa);
    } /* end if */

    if (status < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Sextent_copy(space_id, src_id);
    if (retVal < 0)
        h5libraryError(env);

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

    bval = H5Sextent_equal(space_id, src_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    herr_t retVal = -1;

    retVal = H5Sset_extent_none(space_id);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t status = -1;
    jlong *startP, *strideP, *countP, *blockP;
    jboolean isCopy;
    hsize_t *strt;
    hsize_t *strd;
    hsize_t *cnt;
    hsize_t *blk;
    int rank;
    int i;
    hsize_t *lp;
    jlong *jlp;

    if (start == NULL) {
        h5nullArgument(env, "H5Sselect_hyperslab:  start is NULL");
    } /* end if */
    else if (count == NULL) {
        h5nullArgument(env, "H5Sselect_hyperslab:  count is NULL");
    } /* end if */
    else {
        rank = (int) ENVPTR->GetArrayLength(ENVPAR start);
        if (rank != ENVPTR->GetArrayLength(ENVPAR count)) {
            h5badArgument(env, "H5Sselect_hyperslab:  count and start have different rank!");
            return -1;
        } /* end if */

        startP = ENVPTR->GetLongArrayElements(ENVPAR start, &isCopy);
        if (startP == NULL) {
            h5JNIFatalError(env, "H5Sselect_hyperslab:  start not pinned");
            return -1;
        } /* end if */
        strt = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (strt == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sselect_hyperslab:  start not converted to hsize_t");
            return -1;
        } /* end if */

        jlp = (jlong *) startP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end if */

        countP = ENVPTR->GetLongArrayElements(ENVPAR count, &isCopy);
        if (countP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sselect_hyperslab:  count not pinned");
            return -1;
        } /* end if */
        cnt = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (cnt == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sselect_hyperslab:  count not converted to hsize_t");
            return -1;
        } /* end if */

        jlp = (jlong *) countP;
        for (i = 0; i < rank; i++) {
            *lp = (hsize_t) *jlp;
            lp++;
            jlp++;
        } /* end if */
        if (stride == NULL) {
            strideP = NULL;
            strd = (hsize_t *) strideP;
        } /* end if */
        else {
            strideP = ENVPTR->GetLongArrayElements(ENVPAR stride, &isCopy);
            if (strideP == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
                HDfree(cnt);
                HDfree(strt);
                h5badArgument(env, "H5Sselect_hyperslab:  stride not pinned");
                return -1;
            } /* end if */
            strd = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
            if (strd == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
                HDfree(cnt);
                HDfree(strt);
                h5JNIFatalError(env, "H5Sselect_hyperslab:  stride not converted to hsize_t");
                return -1;
            } /* end if */
            jlp = (jlong *) strideP;
            for (i = 0; i < rank; i++) {
                *lp = (hsize_t) *jlp;
                lp++;
                jlp++;
            } /* end if */
        } /* end if */
        if (block == NULL) {
            blockP = NULL;
            blk = (hsize_t *) blockP;
        } /* end if */
        else {
            blockP = ENVPTR->GetLongArrayElements(ENVPAR block, &isCopy);
            if (blockP == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
                HDfree(cnt);
                HDfree(strt);
                if (strd != NULL)
                    free(strd);

                h5JNIFatalError(env, "H5Sselect_hyperslab:  block not pinned");
                return -1;
            } /* end if */
            blk = lp = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
            if (blk == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR block, blockP, JNI_ABORT);
                HDfree(cnt);
                HDfree(strt);
                if (strd != NULL)
                    free(strd);

                h5JNIFatalError(env, "H5Sget_simple_extent:  block not converted to hsize_t");
                return -1;
            } /* end if */
            jlp = (jlong *) blockP;
            for (i = 0; i < rank; i++) {
                *lp = (hsize_t) *jlp;
                lp++;
                jlp++;
            } /* end for */
        } /* end else */

        status = H5Sselect_hyperslab(space_id, (H5S_seloper_t) op, (const hsize_t *) strt, (const hsize_t *) strd,
                (const hsize_t *) cnt, (const hsize_t *) blk);

        ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
        ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
        HDfree(strt);
        HDfree(cnt);
        if (strideP != NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
            HDfree(strd);
        } /* end if */
        if (blockP != NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR block, blockP, JNI_ABORT);
            HDfree(blk);
        } /* end if */

        if (status < 0)
            h5libraryError(env);
    } /* end else */

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
    herr_t retVal = -1;

    retVal = H5Sclose(space_id);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Sget_select_hyper_nblocks((hid_t) spaceid);
    if (retVal < 0)
        h5libraryError(env);

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

    retVal = H5Sget_select_elem_npoints((hid_t) spaceid);
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t status = -1;
    jlong *bufP;
    jboolean isCopy;
    hsize_t *ba;
    int i;
    int rank;
    hsize_t st;
    hsize_t nb;

    st = (hsize_t) startblock;
    nb = (hsize_t) numblocks;

    if (buf == NULL) {
        h5nullArgument(env, "H5Sget_select_hyper_blocklist:  buf is NULL");
    } /* end if */
    else {
        rank = H5Sget_simple_extent_ndims(spaceid);
        if (rank <= 0)
            rank = 1;
        if (ENVPTR->GetArrayLength(ENVPAR buf) < (numblocks * rank)) {
            h5badArgument(env, "H5Sget_select_hyper_blocklist:  buf input array too small");
        } /* end if */
        else {
            bufP = ENVPTR->GetLongArrayElements(ENVPAR buf, &isCopy);
            if (bufP == NULL) {
                h5JNIFatalError(env, "H5Sget_select_hyper_blocklist:  buf not pinned");
            } /* end if */
            else {
                ba = (hsize_t *) malloc((size_t)nb * 2 * (size_t)rank * sizeof(hsize_t));
                if (ba == NULL) {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, JNI_ABORT);
                    h5JNIFatalError(env, "H5Screate-simple:  buffer not converted to hsize_t");
                } /* end if */
                else {
                    status = H5Sget_select_hyper_blocklist((hid_t)spaceid, st, nb, (hsize_t *)ba);

                    if (status < 0) {
                        ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, JNI_ABORT);
                        free(ba);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        for (i = 0; i < (numblocks * 2 * rank); i++) {
                            bufP[i] = (jlong)ba[i];
                        } /* end for */
                        free(ba);
                        ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, 0);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

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
    herr_t status = -1;
    jlong *bufP;
    jboolean isCopy;
    hsize_t *ba;
    int i;
    int rank;

    if (buf == NULL) {
        h5nullArgument(env, "H5Sget_select_elem_pointlist:  buf is NULL");
    } /* end if */
    else {
        rank = H5Sget_simple_extent_ndims(spaceid);
        if (rank <= 0)
            rank = 1;
        if (ENVPTR->GetArrayLength(ENVPAR buf) < (numpoints * rank)) {
            h5badArgument(env, "H5Sget_select_elem_pointlist:  buf input array too small");
        } /* end if */
        else {
            bufP = ENVPTR->GetLongArrayElements(ENVPAR buf, &isCopy);
            if (bufP == NULL) {
                h5JNIFatalError(env, "H5Sget_select_elem_pointlist:  buf not pinned");
            } /* end if */
            else {
                ba = (hsize_t *)HDmalloc(((size_t)numpoints * (size_t)rank) * sizeof(hsize_t));
                if (ba == NULL) {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, JNI_ABORT);
                    h5JNIFatalError(env, "H5Sget_select_elem_pointlist:  buf not converted to hsize_t");
                } /* end if */
                else {
                    status = H5Sget_select_elem_pointlist((hid_t) spaceid, (hsize_t)startpoint, (hsize_t)numpoints, (hsize_t *)ba);

                    if (status < 0) {
                        HDfree(ba);
                        ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, JNI_ABORT);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        for (i = 0; i < (numpoints * rank); i++) {
                            bufP[i] = (jlong)ba[i];
                        } /* end for */
                        HDfree(ba);
                        ENVPTR->ReleaseLongArrayElements(ENVPAR buf, bufP, 0);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

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
    herr_t status = -1;
    jlong *startP, *endP;
    jboolean isCopy;
    hsize_t *strt;
    hsize_t *en;
    int rank;
    int i;

    if (start == NULL) {
        h5nullArgument(env, "H5Sget_select_bounds:  start is NULL");
    } /* end if */
    else if (end == NULL) {
        h5nullArgument(env, "H5Sget_select_bounds:  end is NULL");
    } /* end else if */
    else {
        startP = ENVPTR->GetLongArrayElements(ENVPAR start, &isCopy);
        if (startP == NULL) {
            h5JNIFatalError(env, "H5Sget_select_bounds:  start not pinned");
            return -1;
        } /* end if */
        rank = (int) ENVPTR->GetArrayLength(ENVPAR start);
        strt = (hsize_t *)malloc((size_t)rank * sizeof(hsize_t));
        if (strt == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sget_select_bounds:  start not converted to hsize_t");
            return -1;
        } /* end if */

        endP = ENVPTR->GetLongArrayElements(ENVPAR end, &isCopy);
        if (endP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            free(strt);
            h5JNIFatalError(env, "H5Sget_select_bounds:  end not pinned");
            return -1;
        } /* end if */
        en = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (en == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR end, endP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sget_simple_extent:  dims not converted to hsize_t");
            return -1;
        } /* end if */

        status = H5Sget_select_bounds((hid_t) spaceid, (hsize_t *) strt, (hsize_t *) en);

        if (status < 0) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR end, endP, JNI_ABORT);
            HDfree(strt);
            HDfree(en);
            h5libraryError(env);
        } /* end if */
        else {
            for (i = 0; i < rank; i++) {
                startP[i] = (jlong)strt[i];
                endP[i] = (jlong)en[i];
            } /* end for */
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR end, endP, 0);
            HDfree(strt);
            HDfree(en);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Sget_1select_1bounds */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sencode
 * Signature: (J)[B
 */
JNIEXPORT jbyteArray JNICALL
Java_hdf_hdf5lib_H5_H5Sencode
    (JNIEnv *env, jclass cls, jlong obj_id)
{
    herr_t status = -1;
    unsigned char *bufPtr;
    size_t buf_size = 0;
    jbyteArray returnedArray = NULL;

    if (obj_id < 0) {
        h5badArgument(env, "H5Sencode: invalid argument");
   } /* end if */
    else {
        status = H5Sencode(obj_id, NULL, &buf_size);

        if (status < 0) {
            h5libraryError(env);
        } /* end else if */
        else if (buf_size < 0) {
            h5badArgument(env, "H5Sencode:  buf_size < 0");
        } /* end if */
        else {
            bufPtr = (unsigned char*)HDcalloc((size_t) 1, buf_size);
            if (bufPtr == NULL) {
                h5outOfMemory(env, "H5Sencode:  calloc failed");
            } /* end if */
            else {
                status = H5Sencode((hid_t) obj_id, bufPtr, &buf_size);

                if (status < 0) {
                    HDfree(bufPtr);
                    h5libraryError(env);
                } /* end if */
                else {
                    returnedArray = ENVPTR->NewByteArray(ENVPAR (jsize)buf_size);
                    ENVPTR->SetByteArrayRegion(ENVPAR returnedArray, 0, (jsize)buf_size, (jbyte*) bufPtr);
                    HDfree(bufPtr);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return returnedArray;
} /* end Java_hdf_hdf5lib_H5_H5Sencode */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sdecode
 * Signature: ([B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Sdecode
    (JNIEnv *env, jclass cls, jbyteArray buf)
{
    hid_t sid = -1;
    jbyte *bufP;
    jboolean isCopy;

    if (buf == NULL) {
        h5nullArgument(env, "H5Sdecode:  buf is NULL");
    } /* end if */
    else {
        bufP = ENVPTR->GetByteArrayElements(ENVPAR buf, &isCopy);
        if (bufP == NULL) {
            h5JNIFatalError( env, "H5Sdecode:  buf not pinned");
        } /* end if */
        else {
            sid = H5Sdecode(bufP);

            ENVPTR->ReleaseByteArrayElements(ENVPAR buf, bufP, JNI_ABORT);

            if (sid < 0)
                h5libraryError(env);
        } /* end else if */
    } /* end else if */

    return (jlong)sid;
} /* end Java_hdf_hdf5lib_H5_H5Sdecode */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Sis_regular_hyperslab
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Sis_1regular_1hyperslab
    (JNIEnv *env, jclass cls, jlong obj_id)
{
    htri_t bval = JNI_FALSE;

    bval = H5Sis_regular_hyperslab((hid_t)obj_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
    herr_t status;
    jlong *startP, *strideP, *countP, *blockP;
    jboolean isCopy;
    hsize_t *strt;
    hsize_t *strd;
    hsize_t *cnt;
    hsize_t *blk;
    int rank;
    int i;

    if (start == NULL) {
        h5nullArgument(env, "H5Sget_regular_hyperslab:  start is NULL");
    } /* end if */
    else if (count == NULL) {
        h5nullArgument(env, "H5Sget_regular_hyperslab:  count is NULL");
    } /* end else if */
    else {
        rank = (int) ENVPTR->GetArrayLength(ENVPAR start);
        if (rank != ENVPTR->GetArrayLength(ENVPAR count)) {
            h5badArgument(env, "H5Sget_regular_hyperslab:  count and start have different rank!");
            return;
        } /* end if */

        startP = ENVPTR->GetLongArrayElements(ENVPAR start, &isCopy);
        if (startP == NULL) {
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  start not pinned");
            return;
        } /* end if */
        strt = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (strt == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  start not converted to hsize_t");
            return;
        } /* end if */

        countP = ENVPTR->GetLongArrayElements(ENVPAR count, &isCopy);
        if (countP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  count not pinned");
            return;
        } /* end if */
        cnt = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (cnt == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  count not converted to hsize_t");
            return;
        } /* end if */

        strideP = ENVPTR->GetLongArrayElements(ENVPAR stride, &isCopy);
        if (strideP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            HDfree(cnt);
            HDfree(strt);
            h5badArgument(env, "H5Sget_regular_hyperslab:  stride not pinned");
            return;
        } /* end if */
        strd = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (strd == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
            HDfree(cnt);
            HDfree(strt);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  stride not converted to hsize_t");
            return;
        } /* end if */

        blockP = ENVPTR->GetLongArrayElements(ENVPAR block, &isCopy);
        if (blockP == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            HDfree(cnt);
            HDfree(strt);
            HDfree(strd);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  block not pinned");
            return;
        } /* end if */
        blk = (hsize_t *)HDmalloc((size_t)rank * sizeof(hsize_t));
        if (blk == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR block, blockP, JNI_ABORT);
            HDfree(cnt);
            HDfree(strt);
            HDfree(strd);
            h5JNIFatalError(env, "H5Sget_regular_hyperslab:  block not converted to hsize_t");
            return;
        } /* end if */

        status = H5Sget_regular_hyperslab(space_id, (hsize_t *) strt, (hsize_t *) strd, (hsize_t *) cnt, (hsize_t *) blk);

        if (status < 0) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR block, blockP, JNI_ABORT);
            HDfree(strt);
            HDfree(cnt);
            HDfree(strd);
            HDfree(blk);
            h5libraryError(env);
        } /* end if */
        else {
            for (i = 0; i < (rank); i++) {
                startP[i] = (jlong)strt[i];
                countP[i] = (jlong)cnt[i];
                strideP[i] = (jlong)strd[i];
                blockP[i] = (jlong)blk[i];
            } /* end for */
            HDfree(strt);
            HDfree(cnt);
            HDfree(strd);
            HDfree(blk);
            ENVPTR->ReleaseLongArrayElements(ENVPAR start, startP, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR count, countP, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR stride, strideP, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR block, blockP, 0);
        } /* end else */
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Sget_1regular_1hyperslab */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
