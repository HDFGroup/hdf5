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
#include "h5util.h"
#include "h5pImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;
extern jobject copy_callback;
extern jobject close_callback;
extern jobject create_callback;
extern jobject compare_callback;
extern jobject get_callback;
extern jobject set_callback;
extern jobject delete_callback;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5P_cls_create_func_cb(hid_t prop_id, void *create_data);
static herr_t H5P_cls_copy_func_cb(hid_t new_prop_id, hid_t old_prop_id, void *copy_data);
static herr_t H5P_cls_close_func_cb(hid_t prop_id, void *close_data);

static herr_t H5P_prp_create_func_cb(const char *name, size_t size, void *value);
static herr_t H5P_prp_copy_func_cb(const char *name, size_t size, void *value);
static herr_t H5P_prp_close_func_cb(const char *name, size_t size, void *value);
static int H5P_prp_compare_func_cb(void *value1, void *value2, size_t size);
static herr_t H5P_prp_get_func_cb(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P_prp_set_func_cb(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P_prp_delete_func_cb(hid_t prop_id, const char *name, size_t size, void *value);

static herr_t H5P_iterate_cb(hid_t prop_id, const char *name, void *op_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcreate
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate
    (JNIEnv *env, jclass clss, jlong type)
{
    hid_t retVal = -1;

    retVal = H5Pcreate((hid_t)type);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = 0;

    if (plist > 0)
        retVal = H5Pclose((hid_t)plist);

    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = -1;

    retVal = H5Pget_class((hid_t) plist);
    if (retVal == H5P_ROOT)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcopy
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = -1;

    retVal = H5Pcopy((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pcopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_version
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1version
    (JNIEnv *env, jclass clss, jlong plist, jintArray version_info)
{
    herr_t   status = -1;
    jint    *theArray;
    jboolean isCopy;

    if (version_info == NULL) {
        h5nullArgument(env, "H5Pget_version:  version_info input array is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR version_info) < 4) {
        h5badArgument(env, "H5Pget_version:  version_info input array < 4");
    } /* end else if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR version_info, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_version:  version_info not pinned");
        } /* end if */
        else {
            status = H5Pget_version((hid_t)plist, (unsigned *)&(theArray[0]),
                    (unsigned *)&(theArray[1]), (unsigned *)&(theArray[2]), (unsigned *)&(theArray[3]));
            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR version_info, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR version_info, theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1version */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_userblock
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1userblock
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    herr_t retVal = -1;
    long sz = (long)size;

    retVal = H5Pset_userblock((hid_t)plist, (hsize_t)sz);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1userblock */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_userblock
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1userblock
    (JNIEnv *env, jclass clss, jlong plist, jlongArray size)
{
    herr_t   status = -1;
    jlong   *theArray;
    jboolean isCopy;
    hsize_t  s;

    if (size == NULL) {
        h5nullArgument(env, "H5Pget_userblock:  size is NULL");
    } /* end if */
    else {
        theArray = (jlong*)ENVPTR->GetLongArrayElements(ENVPAR size, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_userblock:  size not pinned");
        } /* end if */
        else {
            status = H5Pget_userblock((hid_t)plist, &s);

            if (status < 0) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR size, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = (jlong)s;
                ENVPTR->ReleaseLongArrayElements(ENVPAR size, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1userblock */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sizes
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sizes
    (JNIEnv *env, jclass clss, jlong plist, jint sizeof_addr, jint sizeof_size)
{
    herr_t retVal = -1;

    retVal = H5Pset_sizes((hid_t)plist, (size_t)sizeof_addr, (size_t)sizeof_size);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1sizes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sizes
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sizes
    (JNIEnv *env, jclass clss, jlong plist, jlongArray size)
{
    herr_t   status = -1;
    jlong   *theArray;
    jboolean isCopy;
    size_t   ss;
    size_t   sa;

    if (size == NULL) {
        h5nullArgument(env, "H5Pget_sizes:  size is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR size) < 2) {
        h5badArgument(env, "H5Pget_sizes:  size input array < 2 elements");
    }
    else {
        theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR size, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_sizes:  size not pinned");
        } /* end if */
        else {
            status = H5Pget_sizes((hid_t)plist, &sa, &ss);
            if (status < 0) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR size, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = (jlong)sa;
                theArray[1] = (jlong)ss;
                ENVPTR->ReleaseLongArrayElements(ENVPAR size, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1sizes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sym_k
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sym_1k
    (JNIEnv *env, jclass clss, jlong plist, jint ik, jint lk)
{
    herr_t retVal = -1;

    retVal = H5Pset_sym_k((hid_t)plist, (unsigned)ik, (unsigned)lk);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1sym_1k */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sym_k
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sym_1k
    (JNIEnv *env, jclass clss, jlong plist, jintArray size)
{
    herr_t   status = -1;
    jint    *theArray;
    jboolean isCopy;

    if (size == NULL) {
        h5nullArgument(env, "H5Pget_sym_k:  size is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR size) < 2) {
        h5badArgument(env, "H5Pget_sym_k:  size < 2 elements");
    } /* end else if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR size, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_sym_k:  size not pinned");
        } /* end if */
        else {
            status = H5Pget_sym_k((hid_t)plist, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]));
            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR size, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR size, theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1sym_1k */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_istore_k
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1istore_1k
    (JNIEnv *env, jclass clss, jlong plist, jint ik)
{
    herr_t retVal = -1;

    retVal = H5Pset_istore_k((hid_t)plist, (unsigned)ik);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1istore_1k */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_istore_k
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1istore_1k
    (JNIEnv *env, jclass clss, jlong plist, jintArray ik)
{
    herr_t   status = -1;
    jint    *theArray;
    jboolean isCopy;

    if (ik == NULL) {
        h5nullArgument(env, "H5Pget_store_k:  ik is NULL");
    } /* end if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR ik, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_store_k:  size not pinned");
        } /* end if */
        else {
            status = H5Pget_istore_k((hid_t)plist, (unsigned *)&(theArray[0]));
            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR ik, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR ik, theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1istore_1k */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_layout
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1layout
    (JNIEnv *env, jclass clss, jlong plist, jint layout)
{
    herr_t retVal = -1;

    retVal = H5Pset_layout((hid_t)plist, (H5D_layout_t)layout);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1layout */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_layout
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1layout
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5D_layout_t retVal = H5D_LAYOUT_ERROR;

    retVal = H5Pget_layout((hid_t)plist);
    if (retVal == H5D_LAYOUT_ERROR)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1layout */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk
    (JNIEnv *env, jclass clss, jlong plist, jint ndims, jbyteArray dim)
{
    herr_t   status = -1;
    jbyte   *theArray;
    jboolean isCopy;
    hsize_t *da;
    hsize_t *lp;
    jlong   *jlp;
    size_t   i;
    size_t   rank;

    if (dim == NULL) {
        h5nullArgument(env, "H5Pset_chunk:  dim array is NULL");
    } /* end if */
    else {
        i = (size_t)ENVPTR->GetArrayLength(ENVPAR dim);
        rank = i / sizeof(jlong);
        if (rank < ndims) {
            h5badArgument(env, "H5Pset_chunk:  dims array < ndims");
        } /* end if */
        else {
            theArray = (jbyte *)ENVPTR->GetByteArrayElements(ENVPAR dim, &isCopy);
            if (theArray == NULL) {
                h5JNIFatalError(env, "H5Pset_chunk:  dim array not pinned");
            } /* end if */
            else {
                da = lp = (hsize_t *)HDmalloc(rank * sizeof(hsize_t));
                if (da == NULL) {
                    ENVPTR->ReleaseByteArrayElements(ENVPAR dim, theArray, JNI_ABORT);
                    h5JNIFatalError(env,  "H5Pset_chunk:  dims not converted to hsize_t");
                } /* end if */
                else {
                    jlp = (jlong *)theArray;
                    for (i = 0; i < rank; i++) {
                        *lp = (hsize_t)*jlp;
                        lp++;
                        jlp++;
                    } /* end if */

                    status = H5Pset_chunk((hid_t)plist, (int)ndims, da);

                    ENVPTR->ReleaseByteArrayElements(ENVPAR dim, theArray, JNI_ABORT);
                    HDfree(da);

                    if (status < 0)
                        h5libraryError(env);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1chunk */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk
 * Signature: (JI[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk
    (JNIEnv *env, jclass clss, jlong plist, jint max_ndims, jlongArray dims)
{
    herr_t   status = -1;
    jlong   *theArray;
    jboolean isCopy;
    hsize_t *da;
    int      i;

    if (dims == NULL) {
        h5nullArgument(env, "H5Pget_chunk:  dims is NULL");
    } /* end if */
    else if (ENVPTR->GetArrayLength(ENVPAR dims) < max_ndims) {
        h5badArgument(env, "H5Pget_chunk:  dims array < max_ndims");
    } /* end else if */
    else {
        theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR dims, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_chunk:  input dims not pinned");
        } /* end if */
        else {
            da = (hsize_t *)HDmalloc((size_t)max_ndims * sizeof(hsize_t));
            if (da == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR dims, theArray, JNI_ABORT);
                h5JNIFatalError(env, "H5Pget_chunk:  dims not converted to hsize_t");
            } /* end if */
            else {
                status = H5Pget_chunk((hid_t)plist, (int)max_ndims, da);

                if (status < 0)  {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR dims, theArray, JNI_ABORT);
                    HDfree (da);
                    h5libraryError(env);
                } /* end if */
                else {
                    for (i = 0; i < max_ndims; i++) {
                        theArray[i] = (jlong)da[i];
                    }
                    HDfree (da);
                    ENVPTR->ReleaseLongArrayElements(ENVPAR dims, theArray, 0);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1chunk */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_alignment
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alignment
    (JNIEnv *env, jclass clss, jlong plist, jlong threshold, jlong alignment)
{
    herr_t retVal = -1;
    long thr = (long)threshold;
    long align = (long)alignment;

    retVal = H5Pset_alignment((hid_t)plist, (hsize_t)thr, (hsize_t)align);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1alignment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alignment
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alignment
    (JNIEnv *env, jclass clss, jlong plist, jlongArray alignment)
{
    herr_t   status = -1;
    jlong   *theArray;
    jboolean isCopy;
    hsize_t  t;
    hsize_t  a;

    if (alignment == NULL) {
        h5nullArgument(env, "H5Pget_alignment:  input alignment is NULL");
    } /* end if */
    else {
        if (ENVPTR->GetArrayLength(ENVPAR alignment) < 2) {
            h5badArgument(env, "H5Pget_alignment:  alignment input array < 2");
        } /* end if */
        else {
            theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR alignment, &isCopy);
            if (theArray == NULL) {
                h5JNIFatalError(env, "H5Pget_alignment:  input array not pinned");
            } /* end if */
            else {
                status = H5Pget_alignment((hid_t)plist, &t, &a);
                if (status < 0)  {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR alignment, theArray, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else {
                    theArray[0] = (jlong)t;
                    theArray[1] = (jlong)a;
                    ENVPTR->ReleaseLongArrayElements(ENVPAR alignment, theArray, 0);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1alignment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_external
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1external
    (JNIEnv *env, jclass clss, jlong plist, jstring name, jlong offset, jlong size)
{
    herr_t      status = -1;
    const char *fileName;
    off_t       off;
    hsize_t     sz;
    hid_t       plid;

    plid = (hid_t)plist;
    off = (off_t)offset;
    sz = (hsize_t)size;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        status = H5Pset_external(plid, fileName, off, sz);

        UNPIN_JAVA_STRING(name, fileName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1external */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external_count
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external_1count
    (JNIEnv *env, jclass clss, jlong plist)
{
    int retVal = -1;

    retVal = H5Pget_external_count((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1external_1count */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external
 * Signature: (JIJ[Ljava/lang/String;[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external
    (JNIEnv *env, jclass clss, jlong plist, jint idx, jlong name_size,
        jobjectArray name, jlongArray size)
{
    herr_t   status = -1;
    jlong   *theArray;
    jboolean isCopy;
    char    *file = NULL;
    jstring  str;
    off_t    o;
    hsize_t  s;

    if (name_size < 0) {
        h5badArgument(env, "H5Pget_external:  name_size < 0");
    } /* end if */
    else if ((size != NULL) && (ENVPTR->GetArrayLength(ENVPAR size) < 2)) {
        h5badArgument(env, "H5Pget_external:  size input array < 2");
    } /* end else if */
    else {
        if (name_size > 0) {
            file = (char *)HDmalloc(sizeof(char)*(size_t)name_size);
        } /* end else */

        status = H5Pget_external((hid_t) plist, (unsigned)idx, (size_t)name_size,
                file, (off_t *)&o, (hsize_t *)&s);
        if (status < 0) {
            HDfree(file);
            h5libraryError(env);
        } /* end if */
        else {
            if (size != NULL) {
                theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR size, &isCopy);
                if (theArray == NULL) {
                    h5JNIFatalError( env, "H5Pget_external:  size array not pinned");
                } /* end if */
                else {
                    theArray[0] = o;
                    theArray[1] = (jlong)s;
                    ENVPTR->ReleaseLongArrayElements(ENVPAR size, theArray, 0);
                }
            } /* end if */

            if (file != NULL) {
                /*  NewStringUTF may throw OutOfMemoryError */
                str = ENVPTR->NewStringUTF(ENVPAR file);
                if (str == NULL) {
                    HDfree(file);
                    h5JNIFatalError(env, "H5Pget_external:  return array not created");
                } /* end if */
                else {
                    /*  SetObjectArrayElement may raise exceptions */
                    ENVPTR->SetObjectArrayElement(ENVPAR name, 0, (jobject)str);
                    HDfree(file);
                } /* end else */
            } /* end if */
        } /* end else */
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1external */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1value
    (JNIEnv *env, jclass clss, jlong plist_id, jlong type_id, jbyteArray value)
{
    jint     status = -1;
    jbyte   *byteP = NULL;
    jboolean isCopy;

    if (value != NULL) {
        byteP = ENVPTR->GetByteArrayElements(ENVPAR value, &isCopy);
        if (byteP == NULL) {
            h5JNIFatalError(env, "H5Pget_fill_value:  value array not pinned");
        } /* end if */
        else {
            status = H5Pset_fill_value((hid_t)plist_id, (hid_t)type_id, byteP);

            ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);
        }
    } /* end if */
    else {
        status = H5Pset_fill_value((hid_t)plist_id, (hid_t)type_id, byteP);
    }

    if (status < 0)
        h5libraryError(env);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fill_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1value
    (JNIEnv *env, jclass clss, jlong plist_id, jlong type_id, jbyteArray value)
{
    jint     status = -1;
    jbyte   *byteP;
    jboolean isCopy;

    if (value == NULL) {
        h5badArgument(env, "H5Pget_fill_value:  value is NULL");
    } /* end if */
    else {
        byteP = ENVPTR->GetByteArrayElements(ENVPAR value, &isCopy);
        if (byteP == NULL) {
            h5JNIFatalError(env, "H5Pget_fill_value:  value array not pinned");
        } /* end if */
        else {
            status = H5Pget_fill_value((hid_t)plist_id, (hid_t)type_id, byteP);
            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseByteArrayElements(ENVPAR value, byteP, 0);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fill_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter, jint flags,
  jlong cd_nelmts, jintArray cd_values)
{
    herr_t   status = -1;
    jint    *theArray;
    jboolean isCopy;

    if (cd_values == NULL) {
        status = H5Pset_filter((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int)flags, (size_t)cd_nelmts, NULL);
        if (status < 0)
            h5libraryError(env);
    } /* end if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR cd_values, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pset_filter:  input array  not pinned");
        }/* end if */
        else {
            status = H5Pset_filter((hid_t)plist, (H5Z_filter_t)filter,
                    (unsigned int)flags, (size_t)cd_nelmts, (const unsigned int *)theArray);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, theArray, JNI_ABORT);
            if (status < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nfilters
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nfilters
    (JNIEnv *env, jclass clss, jlong plist)
{
    int retVal = -1;

    retVal = H5Pget_nfilters((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nfilters */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter_number, jintArray flags,
        jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name)
{
    herr_t   status = -1;
    jint    *flagsArray;
    jlong   *cd_nelmtsArray;
    jint    *cd_valuesArray;
    jint     mode = JNI_ABORT;
    jboolean isCopy;
    jstring  str;
    char    *filter;

    if (namelen <= 0) {
        h5badArgument(env, "H5Pget_filter:  namelen <= 0");
    } /* end if */
    else if (flags == NULL) {
        h5badArgument(env, "H5Pget_filter:  flags is NULL");
    } /* end else if */
    else if (cd_nelmts == NULL) {
        h5badArgument(env, "H5Pget_filter:  cd_nelmts is NULL");
    } /* end else if */
    else if (cd_values == NULL) {
        h5badArgument(env, "H5Pget_filter:  cd_values is NULL");
    } /* end else if */
    else {
        filter = (char *)HDmalloc(sizeof(char)*(size_t)namelen);
        if (filter == NULL) {
            h5outOfMemory(env, "H5Pget_filter:  namelen malloc failed");
        } /* end if */
        else {
            flagsArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR flags, &isCopy);
            if (flagsArray == NULL) {
                h5JNIFatalError(env, "H5Pget_filter:  flags array not pinned");
            } /* end if */
            else {
                cd_nelmtsArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR cd_nelmts, &isCopy);
                if (cd_nelmtsArray == NULL) {
                    h5JNIFatalError(env, "H5Pget_filter:  nelmts array not pinned");
                } /* end if */
                else {
                    cd_valuesArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR cd_values, &isCopy);
                    if (cd_valuesArray == NULL)  {
                        h5JNIFatalError(env, "H5Pget_filter:  elmts array not pinned");
                    } /* end if */
                    else {
                        /* direct cast (size_t *)variable fails on 32-bit environment */
                        long long cd_nelmts_temp = *(cd_nelmtsArray);
                        size_t cd_nelmts_t = (size_t)cd_nelmts_temp;
                        unsigned int filter_config;
                        status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                                (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                                (size_t)namelen, filter, &filter_config);

                        *cd_nelmtsArray = (jlong)cd_nelmts_t;
                        /* end direct cast special */

                        if (status < 0) {
                            h5libraryError(env);
                        } /* end if */
                        else {
                            mode = 0;

                            /*  NewStringUTF may throw OutOfMemoryError */
                            str = ENVPTR->NewStringUTF(ENVPAR filter);
                            if (str == NULL) {
                                h5JNIFatalError(env, "H5Pget_filter:  return string not pinned");
                            } /* end if */
                            else {
                                /*  SetObjectArrayElement may throw exceptiosn */
                                ENVPTR->SetObjectArrayElement(ENVPAR name, 0, (jobject)str);
                            } /* end else */
                        } /* end else */
                        ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, mode);
                    }
                    ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, mode);
                }
                ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, mode);
            }
            HDfree(filter);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_driver
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1driver
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = -1;

    retVal = H5Pget_driver((hid_t) plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1driver */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_cache
 * Signature: (JIJJD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1cache
    (JNIEnv *env, jclass clss, jlong plist, jint mdc_nelmts, jlong rdcc_nelmts,
  jlong rdcc_nbytes, jdouble rdcc_w0)
{
    herr_t retVal = -1;

    retVal = H5Pset_cache((hid_t)plist, (int)mdc_nelmts, (size_t)rdcc_nelmts,
                (size_t)rdcc_nbytes, (double) rdcc_w0);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_cache
 * Signature: (J[I[J[J[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1cache
    (JNIEnv *env, jclass clss, jlong plist, jintArray mdc_nelmts,
        jlongArray rdcc_nelmts, jlongArray rdcc_nbytes, jdoubleArray rdcc_w0)
{
    herr_t   status = -1;
    jint     mode;
    jdouble *w0Array = (jdouble *)NULL;
    jlong   *rdcc_nelmtsArray = (jlong *)NULL;
    jlong   *nbytesArray = (jlong *)NULL;
    jboolean isCopy;

    if (rdcc_w0 != NULL) {
        w0Array = (jdouble *)ENVPTR->GetDoubleArrayElements(ENVPAR rdcc_w0, &isCopy);
        if (w0Array == NULL) {
            h5JNIFatalError(env, "H5Pget_cache:  w0_array array not pinned");
            return -1;
        } /* end if */
    } /* end else */

    if (rdcc_nelmts != NULL) {
        rdcc_nelmtsArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR rdcc_nelmts, &isCopy);
        if (rdcc_nelmtsArray == NULL) {
            /* exception -- out of memory */
            if (w0Array != NULL) {
                ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, JNI_ABORT);
            }
            h5JNIFatalError(env, "H5Pget_cache:  rdcc_nelmts array not pinned");
            return -1;
        } /* end if */
    } /* end else */

    if (rdcc_nbytes != NULL) {
        nbytesArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR rdcc_nbytes, &isCopy);
        if (nbytesArray == NULL) {
            if (w0Array != NULL) {
                ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, JNI_ABORT);
            } /* end if */
            if (rdcc_nelmtsArray != NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nelmts, rdcc_nelmtsArray, JNI_ABORT);
            } /* end if */
            h5JNIFatalError(env, "H5Pget_cache:  nbytesArray array not pinned");
            return -1;
        } /* end if */
    } /* end else */

    { /* direct cast (size_t *)variable fails on 32-bit environment */
        long long rdcc_nelmts_temp = *(rdcc_nelmtsArray);
        size_t rdcc_nelmts_t = (size_t)rdcc_nelmts_temp;
        long long nbytes_temp = *(nbytesArray);
        size_t nbytes_t = (size_t)nbytes_temp;

        status = H5Pget_cache((hid_t)plist, (int *)NULL, &rdcc_nelmts_t,
                &nbytes_t, (double *)w0Array);

        *rdcc_nelmtsArray = (jlong)rdcc_nelmts_t;
        *nbytesArray = (jlong)nbytes_t;
    } /* end direct cast special */


    if (status < 0) {
        mode = JNI_ABORT;
    } /* end if */
    else {
        mode = 0; /* commit and free */
    } /* end else */

    if (rdcc_nelmtsArray != NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nelmts, rdcc_nelmtsArray, mode);
    } /* end if */

    if (nbytesArray != NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nbytes, nbytesArray, mode);
    } /* end if */

    if (w0Array != NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, mode);
    } /* end if */

    if (status < 0) {
        h5libraryError(env);
    } /* end if */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer
 * Signature: (JJ[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer
    (JNIEnv *env, jclass clss, jlong plist, jlong size, jbyteArray tconv, jbyteArray bkg)
{
    h5unimplemented(env, "H5Pset_buffer:  not implemented");
    return -1;
#ifdef notdef

/* DON'T IMPLEMENT THIS!!! */
    jint     status = -1;
    jbyte   *tconvP;
    jbyte   *bkgP;
    jboolean isCopy;

    if (tconv == NULL)
        tconvP = (jbyte *)NULL;
    else {
        tconvP = ENVPTR->GetByteArrayElements(ENVPAR tconv, &isCopy);
        if (tconvP == NULL) {
            h5JNIFatalError(env, "H5Pset_buffer:  tconv not pinned");
            return -1;
        }
    }
    if (bkg == NULL)
        bkgP = (jbyte *)NULL;
    else {
        bkgP = ENVPTR->GetByteArrayElements(ENVPAR bkg, &isCopy);
        if (bkgP == NULL) {
            h5JNIFatalError(env, "H5Pset_buffer:  bkg not pinned");
            return -1;
        }
    }

    status = H5Pset_buffer((hid_t)plist, (size_t)size, tconvP, bkgP);
    if (status < 0) {
        if (tconv != NULL)
            ENVPTR->ReleaseByteArrayElements(ENVPAR tconv, tconvP, JNI_ABORT);
        if (bkg != NULL)
            ENVPTR->ReleaseByteArrayElements(ENVPAR bkg, bkgP, JNI_ABORT);
        h5libraryError(env);
        return -1;
    }

    if (tconv != NULL)
        ENVPTR->ReleaseByteArrayElements(ENVPAR tconv, tconvP, 0);
    if (bkg != NULL)
        ENVPTR->ReleaseByteArrayElements(ENVPAR bkg, bkgP, 0);

    return (jint)status;
#endif
} /* end Java_hdf_hdf5lib_H5_H5Pset_1buffer */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer
 * Signature: (J[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer
    (JNIEnv *env, jclass clss, jlong plist, jbyteArray tconv, jbyteArray bkg)
{
    h5unimplemented(env, "H5Pget_buffer:  not implemented");
    return -1;
#ifdef notdef

/* DON'T IMPLEMENT THIS!!! */
    jlong     status = -1;
    jbyte   *tconvP;
    jbyte   *bkgP;
    jboolean isCopy;

    if (tconv == NULL) {
        h5nullArgument(env, "H5Pget_buffer:  tconv input array is NULL");
        return -1;
    }
    tconvP = ENVPTR->GetByteArrayElements(ENVPAR tconv, &isCopy);
    if (tconvP == NULL) {
        h5JNIFatalError(env, "H5Pget_buffer:  tconv not pinned");
        return -1;
    }
    if (bkg == NULL) {
        h5nullArgument(env, "H5Pget_buffer:  bkg array is NULL");
        return -1;
    }
    bkgP = ENVPTR->GetByteArrayElements(ENVPAR bkg, &isCopy);
    if (bkgP == NULL) {
        h5JNIFatalError(env, "H5Pget_buffer:  bkg not pinned");
        return -1;
    }

    status = H5Pget_buffer((hid_t)plist, tconvP, bkgP);
    if (status < 0) {
        ENVPTR->ReleaseByteArrayElements(ENVPAR tconv, tconvP, JNI_ABORT);
        ENVPTR->ReleaseByteArrayElements(ENVPAR bkg, bkgP, JNI_ABORT);
        h5libraryError(env);
        return -1;
    }
    ENVPTR->ReleaseByteArrayElements(ENVPAR tconv, tconvP, 0);
    ENVPTR->ReleaseByteArrayElements(ENVPAR bkg, bkgP, 0);

    return (jint)status;
#endif
} /* end Java_hdf_hdf5lib_H5_H5Pget_1buffer */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    if (H5Pset_buffer((hid_t)plist, (size_t)size, NULL, NULL) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1buffer_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    size_t size = 0;

    size = H5Pget_buffer((hid_t)plist, NULL, NULL);
    if (size == 0)
        h5libraryError(env);

    return (jlong)size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1buffer_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_preserve
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1preserve
    (JNIEnv *env, jclass clss, jlong plist, jboolean status)
{
    hbool_t st;
    herr_t  retVal = -1;

    if (status == JNI_TRUE) {
        st = TRUE;
    } /* end if */
    else if (status == JNI_FALSE) {
        st = false;
    } /* end else if */
    else {
        h5badArgument(env, "H5Pset_preserve:  status not TRUE or FALSE");
        return -1;
    } /* end else */

    retVal = H5Pset_preserve((hid_t)plist, st);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1preserve */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_preserve
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1preserve
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = -1;

    retVal = H5Pget_preserve((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1preserve */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_deflate
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1deflate
    (JNIEnv *env, jclass clss, jlong plist, jint level)
{
    herr_t retVal = -1;

    retVal = H5Pset_deflate((hid_t)plist, (unsigned)level);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1deflate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_gc_references
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1gc_1references
    (JNIEnv *env, jclass clss, jlong fapl_id, jboolean gc_ref)
{
    herr_t   retVal = -1;
    unsigned gc_ref_val;

    if (gc_ref == JNI_TRUE)
        gc_ref_val = 1;
    else
        gc_ref_val = 0;

    retVal = H5Pset_gc_references((hid_t)fapl_id, gc_ref_val);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1gc_1references */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_gc_references
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1gc_1references
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    unsigned  gc_ref_val = 0;
    jboolean  bval = JNI_FALSE;

    if (H5Pget_gc_references((hid_t)fapl_id, (unsigned *)&gc_ref_val) < 0) {
        h5libraryError(env);
    } /* end if */
    else {
        if (gc_ref_val == 1)
            bval =  JNI_TRUE;
    } /* end else */

    return bval;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1gc_1references */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_btree_ratios
 * Signature: (JDDD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1btree_1ratios
    (JNIEnv *env, jclass clss, jlong plist_id, jdouble left, jdouble middle, jdouble right)
{
    herr_t status = -1;

    status = H5Pset_btree_ratios((hid_t)plist_id, (double)left,(double)middle, (double)right);
    if (status < 0)
        h5libraryError(env);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1btree_1ratios */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_btree_ratios
 * Signature: (J[D[D[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1btree_1ratios
    (JNIEnv *env, jclass clss, jlong plist_id, jdoubleArray left,
        jdoubleArray middle, jdoubleArray right)
{
    herr_t   status = -1;
    jdouble *leftP;
    jdouble *middleP;
    jdouble *rightP;
    jboolean isCopy;

    if (left == NULL) {
        h5nullArgument(env, "H5Pget_btree_ratios:  left input array is NULL");
    } /* end if */
    else if (middle == NULL) {
        h5nullArgument(env, "H5Pget_btree_ratios:  middle input array is NULL");
    } /* end else if */
    else if (right == NULL) {
        h5nullArgument(env, "H5Pget_btree_ratios:  right input array is NULL");
    } /* end else if */
    else {
        leftP = (jdouble *)ENVPTR->GetDoubleArrayElements(ENVPAR left, &isCopy);
        if (leftP == NULL) {
            h5JNIFatalError(env, "H5Pget_btree_ratios:  left not pinned");
        } /* end if */
        else {
            middleP = (jdouble *)ENVPTR->GetDoubleArrayElements(ENVPAR middle, &isCopy);
            if (middleP == NULL) {
                ENVPTR->ReleaseDoubleArrayElements(ENVPAR left, leftP, JNI_ABORT);
                h5JNIFatalError(env, "H5Pget_btree_ratios:  middle not pinned");
            } /* end if */
            else {
                rightP = (jdouble *)ENVPTR->GetDoubleArrayElements(ENVPAR right, &isCopy);
                if (rightP == NULL) {
                    ENVPTR->ReleaseDoubleArrayElements(ENVPAR left, leftP, JNI_ABORT);
                    ENVPTR->ReleaseDoubleArrayElements(ENVPAR middle, middleP, JNI_ABORT);
                    h5JNIFatalError(env, "H5Pget_btree_ratios:  middle not pinned");
                } /* end if */
                else {
                    status = H5Pget_btree_ratios((hid_t)plist_id, (double *)leftP,
                            (double *)middleP, (double *)rightP);
                    if (status < 0) {
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR left, leftP, JNI_ABORT);
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR middle, middleP, JNI_ABORT);
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR right, rightP, JNI_ABORT);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR left, leftP, 0);
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR middle, middleP, 0);
                        ENVPTR->ReleaseDoubleArrayElements(ENVPAR right, rightP, 0);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1btree_1ratios */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_small_data_block_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1small_1data_1block_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    long sz = (long)size;
    herr_t retVal = -1;

    retVal = H5Pset_small_data_block_size((hid_t)plist, (hsize_t)sz);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1small_1data_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_small_data_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1small_1data_1block_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    hsize_t  s;

    if (H5Pget_small_data_block_size((hid_t)plist, &s) < 0)
        h5libraryError(env);

    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1small_1data_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_alloc_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alloc_1time
    (JNIEnv *env, jclass clss, jlong plist, jint alloc_time)
{
    herr_t retVal = -1;

    retVal = H5Pset_alloc_time((hid_t)plist, (H5D_alloc_time_t)alloc_time);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1alloc_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alloc_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alloc_1time
    (JNIEnv *env, jclass clss, jlong plist, jintArray alloc_time)
{
    herr_t           retVal = -1;
    jint            *theArray;
    jboolean         isCopy;
    H5D_alloc_time_t time;

    if (alloc_time == NULL) {
        /* exception ? */
        h5nullArgument(env, "H5Pget_alloc_time:  alloc_time is NULL");
    } /* end if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR alloc_time, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_alloc_time:  alloc_time not pinned");
        } /* end if */
        else {
            retVal =  H5Pget_alloc_time((hid_t)plist, &time);
            if (retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR alloc_time, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = time;
                ENVPTR->ReleaseIntArrayElements(ENVPAR alloc_time, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1alloc_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1time
    (JNIEnv *env, jclass clss, jlong plist, jint fill_time)
{
    herr_t retVal = -1;

    retVal = H5Pset_fill_time((hid_t)plist, (H5D_fill_time_t)fill_time);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fill_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1time
    (JNIEnv *env, jclass clss, jlong plist, jintArray fill_time)
{
    herr_t          retVal = -1;
    jint           *theArray;
    jboolean        isCopy;
    H5D_fill_time_t time;

    if (fill_time == NULL) {
        /* exception ? */
        h5nullArgument(env, "H5Pget_fill_time:  fill_time is NULL");
    } /* end if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR fill_time, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_fill_time:  fill_time not pinned");
        } /* end if */
        else {
            retVal = H5Pget_fill_time((hid_t)plist, &time);
            if (retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR fill_time, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = (jint)time;
                ENVPTR->ReleaseIntArrayElements(ENVPAR fill_time, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fill_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pfill_value_defined
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined
    (JNIEnv *env, jclass clss, jlong plist, jintArray status)
{
    herr_t retVal = -1;
    jint *theArray;
    jboolean isCopy;
    H5D_fill_value_t value;

    if (status == NULL) {
        /* exception ? */
        h5nullArgument(env, "H5Pfill_value_defined:  status is NULL");
    } /* end if */
    else {
        theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR status, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pfill_value_defined:  status not pinned");
        } /* end if */
        else {
            retVal = H5Pfill_value_defined((hid_t)plist, &value);
            if (retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR status, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = value;
                ENVPTR->ReleaseIntArrayElements(ENVPAR status, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fletcher32
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fletcher32
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = -1;

    retVal = H5Pset_fletcher32((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fletcher32 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_edc_check
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1edc_1check
    (JNIEnv *env, jclass clss, jlong plist, jint check)
{
    herr_t retVal = -1;

    retVal = H5Pset_edc_check((hid_t)plist, (H5Z_EDC_t)check);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1edc_1check */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_edc_check
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1edc_1check
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5Z_EDC_t retVal = -1;

    retVal = H5Pget_edc_check((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1edc_1check */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shuffle
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shuffle
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = -1;

    retVal = H5Pset_shuffle((hid_t)plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shuffle */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_szip
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1szip
    (JNIEnv *env, jclass clss, jlong plist, jint options_mask, jint pixels_per_block)
{
    herr_t retVal = -1;

    retVal = H5Pset_szip((hid_t)plist, (unsigned int)options_mask, (unsigned int)pixels_per_block);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1szip */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_hyper_vector_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1hyper_1vector_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong vector_size)
{
    herr_t retVal = -1;

    retVal = H5Pset_hyper_vector_size((hid_t)plist, (size_t)vector_size);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1hyper_1vector_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_hyper_vector_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1hyper_1vector_1size
    (JNIEnv *env, jclass clss, jlong plist, jlongArray vector_size)
{
    herr_t   retVal = -1;
    jlong   *theArray;
    size_t   size;
    jboolean isCopy;

    if (vector_size == NULL) {
        /* exception ? */
        h5nullArgument(env, "H5Pget_hyper_vector_size:  vector_size is NULL");
    } /* end if */
    else {
        theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR vector_size, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_hyper_vector_size:  vector_size not pinned");
        } /* end if */
        else {
            retVal =  H5Pget_hyper_vector_size((hid_t)plist, &size);
            if (retVal < 0) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR vector_size, theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                theArray[0] = (jlong)size;
                ENVPTR->ReleaseLongArrayElements(ENVPAR vector_size, theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1hyper_1vector_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pall_filters_avail
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pall_1filters_1avail
    (JNIEnv *env, jclass clss, jlong dcpl_id)
{
    htri_t bval = JNI_FALSE;

    bval = H5Pall_filters_avail((hid_t)dcpl_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Pall_1filters_1avail */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pmodify_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pmodify_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jint flags, jlong cd_nelmts, jintArray cd_values)
{
    herr_t   status = -1;
    jint    *cd_valuesP;
    jboolean isCopy;

    if (cd_values == NULL) {
        h5nullArgument(env, "H5Pmodify_filter:  cd_values is NULL");
    } /* end if */
    else {
        cd_valuesP = ENVPTR->GetIntArrayElements(ENVPAR cd_values,&isCopy);
        if (cd_valuesP == NULL) {
            h5JNIFatalError(env, "H5Pmodify_filter:  cd_values not pinned");
        } /* end if */
        else {
            status = H5Pmodify_filter((hid_t)plist, (H5Z_filter_t)filter,(const unsigned int)flags,
                    (size_t)cd_nelmts, (unsigned int *)cd_valuesP);

            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesP, JNI_ABORT);

            if (status < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pmodify_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name)
{
    jboolean     isCopy;
    herr_t       status = -1;
    jint        *cd_valuesArray;
    jint        *flagsArray;
    jlong       *cd_nelmtsArray;
    jstring      str;
    char        *aName;
    int          i = 0;
    int          rank;
    long         bs;

    bs = (long)namelen;
    if (bs <= 0) {
        h5badArgument(env, "H5Pget_filter_by_id:  namelen <= 0");
    } /* end if */
    else if (flags == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  flags is NULL");
    } /* end else if */
    else if (cd_nelmts == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  cd_nelms is NULL");
    } /* end else if */
    else if (cd_values == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  cd_values is NULL");
    } /* end else if */
    else if (name == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  name is NULL");
    } /* end else if */
    else {
        aName = (char*)HDmalloc(sizeof(char) * (size_t)bs);
        if (aName == NULL) {
            h5outOfMemory(env, "H5Pget_filter_by_id:  malloc failed");
            return -1;
        } /* end if */

        flagsArray = ENVPTR->GetIntArrayElements(ENVPAR flags, &isCopy);
        if (flagsArray == NULL) {
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  flags not pinned");
            return -1;
        } /* end if */

        cd_nelmtsArray = ENVPTR->GetLongArrayElements(ENVPAR cd_nelmts, &isCopy);
        if (cd_nelmtsArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  cd_nelms not pinned");
            return -1;
        } /* end if */

        cd_valuesArray = ENVPTR->GetIntArrayElements(ENVPAR cd_values, &isCopy);
        rank = ENVPTR->GetArrayLength(ENVPAR cd_values);
        if (cd_valuesArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, JNI_ABORT);
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  cd_values array not converted to unsigned int.");
            return -1;
        } /* end if */

        { /* direct cast (size_t *)variable fails on 32-bit environment */
            long long cd_nelmts_temp = *(cd_nelmtsArray);
            size_t cd_nelmts_t = (size_t)cd_nelmts_temp;
            unsigned int filter_config;

            status = H5Pget_filter_by_id2( (hid_t)plist, (H5Z_filter_t)filter,
                    (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                    (size_t)namelen, (char *)aName, &filter_config);

            *cd_nelmtsArray = (jlong)cd_nelmts_t;
        } /* end direct cast special */

        if (status < 0) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, JNI_ABORT);
            HDfree(aName);
            h5libraryError(env);
        } /* end if */
        else {
            str = ENVPTR->NewStringUTF(ENVPAR aName);
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, 0);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, 0);

            HDfree(aName);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fclose_degree
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fclose_1degree
    (JNIEnv *env, jclass clss, jlong plist, jint fc_degree)
{
    herr_t  retVal = -1;

    retVal = H5Pset_fclose_degree((hid_t)plist, (H5F_close_degree_t)fc_degree);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fclose_1degree */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fclose_degree
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fclose_1degree
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5F_close_degree_t degree;

    if (H5Pget_fclose_degree((hid_t)plist, &degree) < 0)
        h5libraryError(env);

    return (jint)degree;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fclose_1degree */


/**********************************************************************
 *                                                                    *
 *                    File access properties                          *
 *                                                                    *
 **********************************************************************/

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_family
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1family
    (JNIEnv *env, jclass clss, jlong plist, jlong memb_size, jlong memb_plist)
{
    long ms = (long)memb_size;
    herr_t retVal = -1;

    retVal = H5Pset_fapl_family((hid_t)plist, (hsize_t)ms, (hid_t)memb_plist);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1family */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_family
 * Signature: (J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1family
    (JNIEnv *env, jclass clss, jlong tid, jlongArray memb_size, jlongArray memb_plist)
{
    herr_t   status = -1;
    jlong   *sizeArray;
    jlong   *plistArray;
    jint     mode = JNI_ABORT;
    jboolean isCopy;
    hsize_t *sa;
    size_t   i;
    size_t   rank;

    if (memb_size == NULL) {
        h5nullArgument(env, "H5Pget_family:  memb_size is NULL");
    } /* end if */
    else if (memb_plist == NULL) {
        h5nullArgument(env, "H5Pget_family:  memb_plist is NULL");
    } /* end else if */
    else {
        sizeArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR memb_size, &isCopy);
        if (sizeArray == NULL) {
            h5JNIFatalError(env,  "H5Pget_family:  sizeArray not pinned");
        } /* end if */
        else {
            rank = (size_t)ENVPTR->GetArrayLength(ENVPAR  memb_size);
            sa = (hsize_t *)HDmalloc(rank * sizeof(hsize_t));
            if (sa == NULL) {
                h5JNIFatalError(env,  "H5Screate-simple:  dims not converted to hsize_t");
            } /* end if */
            else {
                plistArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR memb_plist, &isCopy);
                if (plistArray == NULL) {
                    h5JNIFatalError(env,  "H5Pget_family:  plistArray not pinned");
                } /* end if */
                else {
                    status = H5Pget_fapl_family ((hid_t)tid, sa, (hid_t *)plistArray);

                    if (status < 0) {
                        h5libraryError(env);
                    } /* end if */
                    else {
                        for (i = 0; i < rank; i++) {
                            sizeArray[i] = (jlong)sa[i];
                        } /* end for */
                        mode = 0;
                    } /* end else */
                    ENVPTR->ReleaseLongArrayElements(ENVPAR memb_plist, plistArray, mode);
                }
                HDfree(sa);
            }
            ENVPTR->ReleaseLongArrayElements(ENVPAR memb_size, sizeArray, mode);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1family */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_core
 * Signature: (JJZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1core
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong increment, jboolean backing_store)
{
    herr_t retVal = -1;

    retVal =  H5Pset_fapl_core((hid_t)fapl_id, (size_t)increment, (hbool_t)backing_store);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1core */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_core
 * Signature: (J[J[Z)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1core
    (JNIEnv *env, jclass clss, jlong fapl_id, jlongArray increment, jbooleanArray backing_store)
{
    herr_t    status = -1;
    jint      mode = JNI_ABORT;
    jlong    *incArray;
    jboolean *backArray;
    jboolean  isCopy;

    if (increment == NULL) {
        h5nullArgument(env, "H5Pget_fapl_core:  increment is NULL");
    } /* end if */
    else if (backing_store == NULL) {
        h5nullArgument(env, "H5Pget_fapl_core:  backing_store is NULL");
    } /* end else if */
    else {
        incArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR increment, &isCopy);
        if (incArray == NULL) {
            h5JNIFatalError(env,  "H5Pget_fapl_core:  incArray not pinned");
        } /* end if */
        else {
            backArray = (jboolean *)ENVPTR->GetBooleanArrayElements(ENVPAR backing_store, &isCopy);
            if (backArray == NULL) {
                h5JNIFatalError(env, "H5Pget_fapl_core:  backArray not pinned");
            } /* end if */
            else {
                /* direct cast (size_t *)variable fails on 32-bit environment */
                long long inc_temp = *(incArray);
                size_t inc_t = (size_t)inc_temp;

                status = H5Pget_fapl_core((hid_t)fapl_id, &inc_t, (hbool_t *)backArray);

                *incArray = (jlong)inc_t;
                /* end direct cast special */

                if (status < 0) {
                    h5libraryError(env);
                } /* end if */
                else {
                    mode = 0;
                } /* end else */
                ENVPTR->ReleaseBooleanArrayElements(ENVPAR backing_store, backArray, mode);
            }
            ENVPTR->ReleaseLongArrayElements(ENVPAR increment, incArray, mode);
        }
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1core */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_family_offset
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1family_1offset
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong offset)
{
    herr_t retVal = -1;

    retVal =  H5Pset_family_offset ((hid_t)fapl_id, (hsize_t)offset);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1family_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_family_offset
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1family_1offset
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    hsize_t offset = 0;
    herr_t  retVal = -1;

    retVal = H5Pget_family_offset ((hid_t)fapl_id, &offset);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)offset;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1family_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_log
 * Signature: (JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log
    (JNIEnv *env, jclass clss, jlong fapl_id, jstring logfile, jlong flags, jlong buf_size)
{
    herr_t      retVal = -1;
    const char *pLogfile;

    PIN_JAVA_STRING(logfile, pLogfile);
    if (pLogfile != NULL) {
        retVal = H5Pset_fapl_log( (hid_t)fapl_id, pLogfile, (unsigned long long)flags, (size_t)buf_size );

        UNPIN_JAVA_STRING(logfile, pLogfile);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove_filter
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5P1remove_1filter
    (JNIEnv *env, jclass clss, jlong obj_id, jint filter)
{
    herr_t status = -1;

    status = H5Premove_filter ((hid_t)obj_id, (H5Z_filter_t)filter);
    if (status < 0)
        h5libraryError(env);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5P1remove_1filter */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset
 * Signature: (JLjava/lang/String;I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pset
    (JNIEnv *env, jclass clss, jlong plid, jstring name, jint val)
{
    hid_t       retVal = -1;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal =  H5Pset((hid_t)plid, cstr, &val);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pexist
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pexist
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    htri_t bval = JNI_FALSE;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        bval = H5Pexist((hid_t)plid, cstr);

        UNPIN_JAVA_STRING(name, cstr);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Pexist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_size
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1size
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    hid_t       retVal = -1;
    const char *cstr;
    size_t      size = 0;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal = H5Pget_size((hid_t)plid, cstr, &size);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong) size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nprops
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nprops
    (JNIEnv *env, jclass clss, jlong plid)
{
    size_t nprops;

    if (H5Pget_nprops((hid_t)plid, &nprops) < 0)
        h5libraryError(env);

    return (jlong)nprops;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nprops */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1name
    (JNIEnv *env, jclass clss, jlong plid)
{
    char   *c_str;
    jstring j_str = NULL;

    c_str = H5Pget_class_name((hid_t)plid);
    if (c_str == NULL) {
        h5libraryError(env);
    } /* end if */
    else {
        j_str = ENVPTR->NewStringUTF(ENVPAR c_str);
        H5free_memory(c_str);

        if (j_str == NULL)
            h5JNIFatalError(env,"H5Pget_class_name: return string failed");
    } /* end else */
    return j_str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_parent
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1parent
    (JNIEnv *env, jclass clss, jlong plid)
{
    hid_t retVal = -1;

    retVal = H5Pget_class_parent((hid_t)plid);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class_1parent */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pisa_class
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pisa_1class
    (JNIEnv *env, jclass clss, jlong plid, jlong pcls)
{
    htri_t retVal = -1;

    retVal = H5Pisa_class((hid_t)plid, (hid_t)pcls);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pisa_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    herr_t      retVal = -1;
    const char *cstr;
    jint        val;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal = H5Pget((hid_t)plid, cstr, &val);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)val;
} /* end Java_hdf_hdf5lib_H5_H5Pget */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pequal
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pequal
    (JNIEnv *env, jclass clss, jlong plid1, jlong plid2)
{
    htri_t retVal = -1;

    retVal = H5Pequal((hid_t)plid1, (hid_t)plid2);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pequal */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy_prop
 * Signature: (JJLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pcopy_1prop
    (JNIEnv *env, jclass clss, jlong dst_plid, jlong src_plid, jstring name)
{
    herr_t      retVal = -1;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal = H5Pcopy_prop((hid_t)dst_plid, (hid_t)src_plid, cstr);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pcopy_1prop */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Premove
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    herr_t      retVal = -1;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal = H5Premove((hid_t)plid, cstr);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Premove */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Punregister
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Punregister
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    herr_t      retVal = -1;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        retVal = H5Punregister((hid_t)plid, cstr);

        UNPIN_JAVA_STRING(name, cstr);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Punregister */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pclose_class
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose_1class
    (JNIEnv *env, jclass clss, jlong plid)
{
    herr_t  retVal = -1;

    retVal = H5Pclose_class((hid_t)plid);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pclose_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter2
    (JNIEnv *env, jclass clss, jlong plist, jint filter_number,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen,
        jobjectArray name, jintArray filter_config)
{
    herr_t   status = -1;
    jint    *flagsArray;
    jlong   *cd_nelmtsArray;
    jint    *cd_valuesArray;
    jint    *filter_configArray;
    jboolean isCopy;
    char    *filter;
    jstring  str;

    if (namelen <= 0) {
        h5badArgument(env, "H5Pget_filter:  namelen <= 0");
    } /* end if */
    else if (flags == NULL) {
        h5badArgument(env, "H5Pget_filter:  flags is NULL");
    } /* end else if */
    else if (cd_nelmts == NULL) {
        h5badArgument(env, "H5Pget_filter:  cd_nelmts is NULL");
    } /* end else if */
    else if (filter_config == NULL) {
        h5badArgument(env, "H5Pget_filter:  filter_config is NULL");
    } /* end else if */
    else {
        filter = (char*)HDmalloc(sizeof(char)*(size_t)namelen);
        if (filter == NULL) {
            h5outOfMemory(env, "H5Pget_filter:  namelent malloc failed");
            return -1;
        } /* end if */
        flagsArray = (jint*)ENVPTR->GetIntArrayElements(ENVPAR flags, &isCopy);
        if (flagsArray == NULL) {
            HDfree(filter);
            h5JNIFatalError(env,  "H5Pget_filter:  flags array not pinned");
            return -1;
        } /* end if */

        cd_nelmtsArray = (jlong*)ENVPTR->GetLongArrayElements(ENVPAR cd_nelmts, &isCopy);
        if (cd_nelmtsArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            HDfree(filter);
            h5JNIFatalError(env,  "H5Pget_filter:  nelmts array not pinned");
            return -1;
        } /* end if */
        filter_configArray = (jint*)ENVPTR->GetIntArrayElements(ENVPAR filter_config, &isCopy);
        if (filter_configArray == NULL) {
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            HDfree(filter);
            h5JNIFatalError(env,  "H5Pget_filter:  filter_config array not pinned");
            return -1;
        } /* end if */

        if (*cd_nelmtsArray == 0 && cd_values == NULL) {
            /* direct cast (size_t *)variable fails on 32-bit environment */
            long long cd_nelmts_temp = 0;
            size_t cd_nelmts_t = (size_t)cd_nelmts_temp;

            status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                    (unsigned int *)flagsArray, &cd_nelmts_t, NULL,
                    (size_t)namelen, filter, (unsigned int *)filter_configArray);

            *cd_nelmtsArray = (jlong)cd_nelmts_t;
        } /* end if */
        else {
            if (cd_values == NULL) {
                h5badArgument(env, "H5Pget_filter:  cd_values is NULL");
                return -1;
            } /* end if */
            cd_valuesArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR cd_values, &isCopy);
            if (cd_valuesArray == NULL)  {
                ENVPTR->ReleaseIntArrayElements(ENVPAR filter_config, filter_configArray, JNI_ABORT);
                ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
                ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
                HDfree(filter);
                h5JNIFatalError(env,  "H5Pget_filter:  elmts array not pinned");
                return -1;
            } /* end if */

            { /* direct cast (size_t *)variable fails on 32-bit environment */
                long long cd_nelmts_temp = *(cd_nelmtsArray);
                size_t cd_nelmts_t = (size_t)cd_nelmts_temp;

                status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                        (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                        (size_t)namelen, filter, (unsigned int *)filter_configArray);

                *cd_nelmtsArray = (jlong)cd_nelmts_t;
            } /* end direct cast special */
        } /* end else */

        if (status < 0) {
            if (cd_values)
                ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR filter_config, filter_configArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            HDfree(filter);
            h5libraryError(env);
        } /* end if */
        else {
            if (cd_values)
                ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, 0);
            ENVPTR->ReleaseIntArrayElements(ENVPAR filter_config, filter_configArray, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, 0);
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, 0);
            /*  NewStringUTF may throw OutOfMemoryError */
            str = ENVPTR->NewStringUTF(ENVPAR filter);
            HDfree(filter);
            if (str == NULL)
                h5JNIFatalError(env,  "H5Pget_filter:  return string not pinned");
            else
                ENVPTR->SetObjectArrayElement(ENVPAR name, 0, (jobject)str);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name, jintArray filter_config)
{
    herr_t   status = -1;
    int      i = 0;
    jint    *cd_valuesArray;
    jint    *flagsArray;
    jint    *filter_configArray;
    jlong   *cd_nelmtsArray;
    jboolean isCopy;
    long     bs;
    char    *aName;
    jstring  str;

    bs = (long)namelen;
    if (bs <= 0) {
        h5badArgument(env, "H5Pget_filter_by_id:  namelen <= 0");
    } /* end if */
    else if (flags == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  flags is NULL");
    } /* end else if */
    else if (cd_nelmts == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  cd_nelms is NULL");
    } /* end else if */
    else if (cd_values == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  cd_values is NULL");
    } /* end else if */
    else if (name == NULL) {
        h5nullArgument(env, "H5Pget_filter_by_id:  name is NULL");
    } /* end else if */
    else if (filter_config == NULL) {
        h5badArgument(env, "H5Pget_filter_by_id:  filter_config is NULL");
    } /* end else if */
    else {
        aName = (char*)HDmalloc(sizeof(char) * (size_t)bs);
        if (aName == NULL) {
            h5outOfMemory(env, "H5Pget_filter_by_id:  malloc failed");
            return -1;
        } /* end if */
        flagsArray = ENVPTR->GetIntArrayElements(ENVPAR flags,&isCopy);
        if (flagsArray == NULL) {
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  flags not pinned");
            return -1;
        } /* end if */
        cd_nelmtsArray = ENVPTR->GetLongArrayElements(ENVPAR cd_nelmts, &isCopy);
        if (cd_nelmtsArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  cd_nelms not pinned");
            return -1;
        } /* end if */
        cd_valuesArray = ENVPTR->GetIntArrayElements(ENVPAR cd_values, &isCopy);
        if (cd_valuesArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            HDfree(aName);
            h5JNIFatalError(env, "H5Pget_filter_by_id:  cd_values array not converted to unsigned int.");
            return -1;
        } /* end if */
        filter_configArray = ENVPTR->GetIntArrayElements(ENVPAR filter_config, &isCopy);
        if (filter_configArray == NULL) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, JNI_ABORT);
            HDfree(aName);
            h5JNIFatalError(env,  "H5Pget_filter_by_id:  flags not pinned");
            return -1;
        } /* end if */

        { /* direct cast (size_t *)variable fails on 32-bit environment */
            long long cd_nelmts_temp = *(cd_nelmtsArray);
            size_t cd_nelmts_t = (size_t)cd_nelmts_temp;

            status = H5Pget_filter_by_id2((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                (size_t)namelen, (char *)aName, (unsigned int *)filter_configArray);

            *cd_nelmtsArray = (jlong)cd_nelmts_t;
        } /* end direct cast special handling */

        if (status < 0) {
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, JNI_ABORT);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, JNI_ABORT);
            ENVPTR->ReleaseIntArrayElements(ENVPAR filter_config, filter_configArray, JNI_ABORT);
            HDfree(aName);
            h5libraryError(env);
        } /* end if */
        else {
            str = ENVPTR->NewStringUTF(ENVPAR aName);
            HDfree(aName);
            ENVPTR->ReleaseIntArrayElements(ENVPAR flags, flagsArray, 0);
            ENVPTR->ReleaseLongArrayElements(ENVPAR cd_nelmts, cd_nelmtsArray, 0);
            ENVPTR->ReleaseIntArrayElements(ENVPAR cd_values, cd_valuesArray, 0);
            ENVPTR->ReleaseIntArrayElements(ENVPAR filter_config, filter_configArray, 0);
        } /* end else */
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nlinks
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nlinks
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    size_t nlinks;
    if (H5Pget_nlinks((hid_t)lapl_id, &nlinks) < 0)
        h5libraryError(env);

    return (jlong) nlinks;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nlinks */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nlinks
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nlinks
    (JNIEnv *env, jclass clss, jlong lapl_id, jlong nlinks)
{
     herr_t retVal = -1;

     if (nlinks <= 0) {
         h5badArgument(env, "H5Pset_1nlinks:  nlinks_l <= 0");
     } /* end if */
     else {
        retVal = H5Pset_nlinks((hid_t)lapl_id, (size_t)nlinks);
        if(retVal < 0)
            h5libraryError(env);
     } /* end else */
     return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1nlinks */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_libver_bounds
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds
    (JNIEnv *env, jclass clss, jlong fapl_id, jintArray libver)
{
    herr_t        retVal = -1;
    H5F_libver_t *theArray = NULL;
    jboolean      isCopy;

    if (libver == NULL) {
        h5nullArgument(env, "H5Pget_libver_bounds:  libversion bounds is NULL");
    } /* end if */
    else {
        theArray = (H5F_libver_t*)ENVPTR->GetIntArrayElements(ENVPAR libver, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_libver_bounds:  input not pinned");
        } /* end if */
        else {
            retVal = H5Pget_libver_bounds((hid_t)fapl_id, &(theArray[0]), &(theArray[1]));
            if(retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR libver, (jint*)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR libver, (jint*)theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_libver_bounds
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds
    (JNIEnv *env, jclass clss, jlong fapl_id, jint low, jint high)
{
    herr_t retVal = -1;

    if ((H5F_libver_t)high != H5F_LIBVER_LATEST) {
        h5badArgument(env, "H5Pset_libver_bounds:  invalid high library version bound");
    } /* end if */
    else if(((H5F_libver_t)low !=H5F_LIBVER_EARLIEST) && ((H5F_libver_t)low != H5F_LIBVER_LATEST)) {
        h5badArgument(env, "H5Pset_libver_bounds:  invalid low library version bound");
    } /* end else if */
    else {
        retVal = H5Pset_libver_bounds((hid_t)fapl_id, (H5F_libver_t)low, (H5F_libver_t)high);
        if(retVal < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1creation_1order
    (JNIEnv *env, jclass clss, jlong gcpl_id)
{
    unsigned crt_order_flags;

    if(H5Pget_link_creation_order((hid_t)gcpl_id, &crt_order_flags) < 0)
        h5libraryError(env);

    return (jint)crt_order_flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1link_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint crt_order_flags)
{
    herr_t retVal = -1;

    retVal = H5Pset_link_creation_order((hid_t)gcpl_id, (unsigned)crt_order_flags);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1creation_1order
    (JNIEnv *env, jclass clss, jlong ocpl_id)
{
    unsigned crt_order_flags;

    if(H5Pget_attr_creation_order((hid_t)ocpl_id, &crt_order_flags) < 0)
        h5libraryError(env);

    return (jint)crt_order_flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1attr_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1creation_1order
    (JNIEnv *env, jclass clss, jlong ocpl_id, jint crt_order_flags)
{
    herr_t retVal = -1;

    retVal = H5Pset_attr_creation_order((hid_t)ocpl_id, (unsigned)crt_order_flags);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1attr_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_copy_object
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1copy_1object
    (JNIEnv *env, jclass clss, jlong ocp_plist_id, jint copy_options)
{
    herr_t retVal = -1;

    retVal = H5Pset_copy_object((hid_t)ocp_plist_id, (unsigned)copy_options);
    if(retVal < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1copy_1object */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_copy_object
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1copy_1object
    (JNIEnv *env, jclass clss, jlong ocp_plist_id)
{
    unsigned copy_options;

    if(H5Pget_copy_object((hid_t)ocp_plist_id, &copy_options) < 0)
        h5libraryError(env);

    return (jint)copy_options;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1copy_1object */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_create_intermediate_group
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1create_1intermediate_1group
    (JNIEnv *env, jclass clss, jlong lcpl_id, jboolean crt_intermed_group)
{
    herr_t retVal = -1;

    retVal = H5Pset_create_intermediate_group((hid_t)lcpl_id, (unsigned)crt_intermed_group);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1create_1intermediate_1group */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_create_intermediate_group
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1create_1intermediate_1group
    (JNIEnv *env, jclass clss, jlong lcpl_id)
{
    unsigned crt_intermed_group;

    if(H5Pget_create_intermediate_group((hid_t)lcpl_id, &crt_intermed_group) < 0)
        h5libraryError(env);

    return (jboolean)crt_intermed_group;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1create_1intermediate_1group */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_data_transform
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1data_1transform
    (JNIEnv *env, jclass clss, jlong plist_id, jstring expression)
{
    herr_t      retVal = -1;
    const char *express;

    PIN_JAVA_STRING(expression, express);
    if (express != NULL) {
        retVal = H5Pset_data_transform((hid_t)plist_id, express);

        UNPIN_JAVA_STRING(expression, express);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1data_1transform */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_data_transform
 * Signature: (J[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1data_1transform
    (JNIEnv *env, jclass clss, jlong plist_id, jobjectArray expression, jlong size)
{
    size_t   buf_size;
    char    *express;
    jlong    express_size = -1;
    jstring  str = NULL;

    if (size <= 0) {
        h5badArgument(env, "H5Pget_data_transform:  size <= 0");
    } /* end if */
    else {
        express_size = (jlong)H5Pget_data_transform((hid_t)plist_id, (char*)NULL, (size_t)size);
        if(express_size < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            buf_size = (size_t)express_size + 1;/* add extra space for the null terminator */
            express = (char*)HDmalloc(sizeof(char) * buf_size);
            if (express == NULL) {
                h5outOfMemory(env, "H5Pget_data_transform:  malloc failed ");
            } /* end if */
            else {
                express_size = (jlong)H5Pget_data_transform((hid_t)plist_id, express, (size_t)size);
                if (express_size < 0) {
                    HDfree(express);
                    h5libraryError(env);
                }
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR express);
                    HDfree(express);
                    if (str == NULL)
                        h5JNIFatalError(env, "H5Pget_data_transform:  return string not created");
                    else
                        ENVPTR->SetObjectArrayElement(ENVPAR expression, 0, str);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */
    return express_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1data_1transform */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_acc_flags
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1acc_1flags
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    unsigned flags;

    if(H5Pget_elink_acc_flags((hid_t)lapl_id, &flags) < 0)
        h5libraryError(env);

    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1acc_1flags */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_acc_flags
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1acc_1flags
    (JNIEnv *env, jclass clss, jlong lapl_id, jint flags)
{
    herr_t retVal = -1;

    retVal = H5Pset_elink_acc_flags((hid_t)lapl_id, (unsigned)flags);
    if (retVal < 0)
        h5libraryError(env);

    return (jint) retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1acc_1flags */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1phase_1change
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint max_compact, jint min_dense)
{
    herr_t retVal = -1;

    if(max_compact < min_dense) {
        h5badArgument(env, "H5Pset_link_phase_change: max compact value must be >= min dense value");
    } /* end if */
    else if(max_compact > 65535) {
        h5badArgument(env, "H5Pset_link_phase_change: max compact value must be < 65536");
    } /* end else if */
    else if(min_dense > 65535) {
        h5badArgument(env, "H5Pset_link_phase_change: min dense value must be < 65536");
    } /* end else if */
    else {
        retVal = H5Pset_link_phase_change((hid_t)gcpl_id, (unsigned)max_compact, (unsigned)min_dense);
        if(retVal < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1link_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_link_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1link_1phase_1change
    (JNIEnv *env, jclass clss, jlong gcpl_id, jintArray links)
{
    herr_t    retVal = -1;
    unsigned *theArray = NULL;
    jboolean  isCopy;

    if (links == NULL) {
        h5nullArgument( env, "H5Pget_link_phase_change:  links is NULL");
    } /* end if */
    else {
        theArray = (unsigned *)ENVPTR->GetIntArrayElements(ENVPAR links, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError( env, "H5Pget_link_phase_change:  input not pinned");
        } /* end if */
        else {
            retVal = H5Pget_link_phase_change((hid_t)gcpl_id, &(theArray[0]), &(theArray[1]));
            if(retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR links, (jint *)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR links, (jint *)theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1link_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1phase_1change
    (JNIEnv *env, jclass clss, jlong ocpl_id, jintArray attributes)
{
    herr_t    retVal = -1;
    unsigned *theArray = NULL;
    jboolean  isCopy;

    if (attributes == NULL) {
        h5nullArgument(env, "H5Pget_attr_phase_change:  attributes is NULL");
    } /* end if */
    else {
        theArray = (unsigned *)ENVPTR->GetIntArrayElements(ENVPAR attributes, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_attr_phase_change:  input not pinned");
        } /* end if */
        else {
            retVal = H5Pget_attr_phase_change((hid_t)ocpl_id, &(theArray[0]), &(theArray[1]));
            if(retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR attributes, (jint *)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR attributes, (jint *)theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1attr_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_phase_change
 * Signature: (JII)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1phase_1change
    (JNIEnv *env, jclass clss, jlong ocpl_id, jint max_compact, jint min_dense)
{
    herr_t    retVal = -1;

    retVal = H5Pset_attr_phase_change((hid_t)ocpl_id, (unsigned)max_compact, (unsigned)min_dense);
    if(retVal < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1attr_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1phase_1change
    (JNIEnv *env, jclass clss, jlong fcpl_id, jintArray size)
{
    herr_t    retVal = -1;
    unsigned *theArray = NULL;
    jboolean  isCopy;

    if (size == NULL) {
        h5nullArgument(env, "H5Pget_shared_mesg_phase_change:  size is NULL");
    } /* end if */
    else {
        theArray = (unsigned *)ENVPTR->GetIntArrayElements(ENVPAR size, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_shared_mesg_phase_change:  input not pinned");
        } /* end if */
        else {
            retVal = H5Pget_shared_mesg_phase_change((hid_t)fcpl_id, &(theArray[0]), &(theArray[1]));
            if(retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR size, (jint *)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR size, (jint *)theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1phase_1change
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint max_list, jint min_btree)
{
    herr_t retVal = -1;

    /* Check that values are sensible.  The min_btree value must be no greater
     * than the max list plus one.
     *
     * Range check to make certain they will fit into encoded form.
     */

    if(max_list + 1 < min_btree) {
        h5badArgument(env, "H5Pset_shared_mesg_phase_change: minimum B-tree value is greater than maximum list value");
    } /* end if */
    else if(max_list > H5O_SHMESG_MAX_LIST_SIZE) {
        h5badArgument(env, "H5Pset_shared_mesg_phase_change: max list value is larger than H5O_SHMESG_MAX_LIST_SIZE");
    } /* end else if */
    else if(min_btree > H5O_SHMESG_MAX_LIST_SIZE) {
        h5badArgument(env, "H5Pset_shared_mesg_phase_change: min btree value is larger than H5O_SHMESG_MAX_LIST_SIZE");
    } /* end else if */
    else {
        retVal = H5Pset_shared_mesg_phase_change((hid_t)fcpl_id, (unsigned)max_list, (unsigned)min_btree);
        if(retVal < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_nindexes
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1nindexes
    (JNIEnv *env, jclass clss, jlong fcpl_id)
{
    unsigned nindexes;

    if(H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes) < 0)
        h5libraryError(env);

    return (jint)nindexes;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1nindexes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_nindexes
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1nindexes
    (JNIEnv *env, jclass clss, jlong plist_id, jint nindexes)
{
    herr_t retVal = -1;

    if (nindexes > H5O_SHMESG_MAX_NINDEXES) {
        h5badArgument(env, "H5Pset_shared_mesg_nindexes: number of indexes is greater than H5O_SHMESG_MAX_NINDEXES");
    } /* end if */
    else {
        retVal = H5Pset_shared_mesg_nindexes((hid_t)plist_id, (unsigned)nindexes);
        if(retVal < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1nindexes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_index
 * Signature: (JIII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1index
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint index_num,
        jint mesg_type_flags, jint min_mesg_size)
{
    herr_t      retVal = -1;
    unsigned    nindexes;/* Number of SOHM indexes */

    /* Check arguments */
    if(mesg_type_flags > H5O_SHMESG_ALL_FLAG) {
        h5badArgument(env, "H5Pset_shared_mesg_index: unrecognized flags in mesg_type_flags");
    } /* end if */
    else if(H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes) < 0) { /* Read the current number of indexes */
        h5libraryError(env);
    } /* end else if */
    else {
        /* Range check */
        if((unsigned)index_num >= nindexes) {
            h5badArgument(env, "H5Pset_shared_mesg_index: index_num is too large; no such index");
        } /* end if */
        else {
            retVal = H5Pset_shared_mesg_index((hid_t)fcpl_id, (unsigned)index_num, (unsigned) mesg_type_flags, (unsigned) min_mesg_size);
            if(retVal < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1index */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_index
 * Signature: (JI[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1index
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint index_num, jintArray mesg_info)
{
    herr_t    retVal = -1;
    unsigned  nindexes;/* Number of SOHM indexes */
    unsigned *theArray = NULL;
    jboolean  isCopy;

    /* Read the current number of indexes */
    if(H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes)<0) {
        h5libraryError(env);
    } /* end if */
    else {
        /* Range check */
        if((unsigned)index_num >= nindexes) {
            h5badArgument(env, "H5Pget_shared_mesg_index: index_num is too large; no such index");
        } /* end if */
        else if (mesg_info == NULL) {
            h5nullArgument(env, "H5Pget_shared_mesg_index:  mesg_info is NULL");
        } /* end else if */
        else {
            theArray = (unsigned *)ENVPTR->GetIntArrayElements(ENVPAR mesg_info, &isCopy);
            if (theArray == NULL) {
                h5JNIFatalError(env, "H5Pget_shared_mesg_index:  input not pinned");
            } /* end if */
            else {
                retVal = H5Pget_shared_mesg_index((hid_t)fcpl_id, (unsigned)index_num, &(theArray[0]), &(theArray[1]));
                if(retVal < 0) {
                    ENVPTR->ReleaseIntArrayElements(ENVPAR mesg_info, (jint*)theArray, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else
                    ENVPTR->ReleaseIntArrayElements(ENVPAR mesg_info, (jint*)theArray, 0);
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1index */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_local_heap_size_hint
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1local_1heap_1size_1hint
    (JNIEnv *env, jclass clss, jlong gcpl_id, jlong size_hint)
{
    herr_t retVal = -1;

    retVal = H5Pset_local_heap_size_hint((hid_t)gcpl_id, (size_t)size_hint);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1local_1heap_1size_1hint */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_local_heap_size_hint
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1local_1heap_1size_1hint
    (JNIEnv *env, jclass clss, jlong gcpl_id)
{
    size_t size_hint;

    if(H5Pget_local_heap_size_hint((hid_t)gcpl_id, &size_hint) < 0)
        h5libraryError(env);

    return (jlong)size_hint;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1local_1heap_1size_1hint */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nbit
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nbit
    (JNIEnv *env, jclass clss, jlong plist_id)
{
    herr_t retVal = -1;

    retVal = H5Pset_nbit((hid_t)plist_id);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1nbit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_scaleoffset
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1scaleoffset
    (JNIEnv *env, jclass clss, jlong plist_id, jint scale_type, jint scale_factor)
{
    herr_t retVal = -1;

    /* Check arguments */
    if(scale_factor < 0) {
        h5badArgument(env, "H5Pset_scaleoffset: scale factor must be > 0");
    } /* end if */
    else {
        if(scale_type != H5Z_SO_FLOAT_DSCALE && scale_type != H5Z_SO_FLOAT_ESCALE && scale_type != H5Z_SO_INT){
            h5badArgument(env, "H5Pset_scaleoffset: invalid scale type");
        } /* end if */
        else {
            retVal = H5Pset_scaleoffset((hid_t)plist_id, (H5Z_SO_scale_type_t)scale_type, scale_factor);
            if(retVal < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1scaleoffset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_est_link_info
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1est_1link_1info
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint est_num_entries, jint est_name_len)
{
    herr_t retVal = -1;

    /* Range check values */
    if((est_num_entries > 65535) || (est_name_len > 65535)) {
        h5badArgument(env, "H5Pset_est_link_info: est. name length or number of entries must be < 65536");
    } /* end if */
    else {
        retVal = H5Pset_est_link_info((hid_t)gcpl_id, (unsigned)est_num_entries, (unsigned)est_name_len);
        if(retVal < 0)
            h5libraryError(env);
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1est_1link_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_est_link_info
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1est_1link_1info
    (JNIEnv *env, jclass clss, jlong gcpl_id, jintArray link_info)
{
    herr_t    retVal = -1;
    unsigned *theArray = NULL;
    jboolean  isCopy;

    if (link_info == NULL) {
        h5nullArgument(env, "H5Pget_est_link_info:  link_info is NULL");
    } /* end if */
    else {
        theArray = (unsigned *)ENVPTR->GetIntArrayElements(ENVPAR link_info,&isCopy);
        if (theArray == NULL) {
            h5JNIFatalError(env, "H5Pget_est_link_info:  input not pinned");
        } /* end if */
        else {
            retVal= H5Pget_est_link_info((hid_t)gcpl_id, &(theArray[0]), &(theArray[1]));
            if(retVal < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR link_info, (jint *)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else
                ENVPTR->ReleaseIntArrayElements(ENVPAR link_info, (jint *)theArray, 0);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1est_1link_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_fapl
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1fapl
    (JNIEnv *env, jclass clss, jlong lapl_id, jlong fapl_id)
{
    herr_t retVal = -1;

    retVal = H5Pset_elink_fapl((hid_t)lapl_id, (hid_t)fapl_id);
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1fapl */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pget_elink_fapl
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pget_1elink_1fapl
    (JNIEnv *env, jclass clss, jlong lapl_id)
{
    hid_t retVal = -1;

    retVal = H5Pget_elink_fapl((hid_t)lapl_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pget_1elink_1fapl */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_prefix
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1prefix
    (JNIEnv *env, jclass clss, jlong lapl_id, jstring prefix)
{
    herr_t      retVal = -1;
    const char *aName;

    PIN_JAVA_STRING(prefix, aName);
    if (aName != NULL) {
        retVal = H5Pset_elink_prefix((hid_t)lapl_id, aName);

        UNPIN_JAVA_STRING(prefix, aName);

        if(retVal < 0)
            h5libraryError(env);
    }

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_prefix
 * Signature: (J[Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1prefix
    (JNIEnv *env, jclass clss, jlong lapl_id, jobjectArray prefix)
{
    size_t  size = 0;
    char   *pre;
    jlong   prefix_size = -1;
    jstring str = NULL;

    if (prefix == NULL) {
        h5nullArgument(env, "H5Pget_elink_prefix: prefix is NULL");
    } /* end if */
    else {
        prefix_size = (jlong)H5Pget_elink_prefix((hid_t)lapl_id, (char*)NULL, size);
        if(prefix_size < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            size = (size_t)prefix_size + 1;/* add extra space for the null terminator */
            pre = (char*)HDmalloc(sizeof(char)*size);
            if (pre == NULL) {
                h5outOfMemory(env, "H5Pget_elink_prefix:  malloc failed ");
            } /* end if */
            else {
                prefix_size = (jlong)H5Pget_elink_prefix((hid_t)lapl_id, (char*)pre, size);

                if (prefix_size < 0) {
                    HDfree(pre);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR pre);
                    HDfree(pre);
                    if (str == NULL) {
                        h5JNIFatalError(env, "H5Pget_elink_prefix:  return string not created");
                    } /* end if */
                    else
                        ENVPTR->SetObjectArrayElement(ENVPAR prefix, 0, str);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return prefix_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_direct
 * Signature: (JJJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1direct
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong alignment,
        jlong block_size, jlong cbuf_size)
{
    herr_t retVal = -1;

#ifdef H5_HAVE_DIRECT
    retVal = H5Pset_fapl_direct((hid_t)fapl_id, (size_t)alignment, (size_t)block_size, (size_t)cbuf_size);
#endif
    if(retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1direct */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_direct
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1direct
    (JNIEnv *env, jclass clss, jlong fapl_id, jlongArray info)
{
    herr_t   retVal = -1;

#ifdef H5_HAVE_DIRECT
    size_t   alignment = 0;
    size_t   block_size = 0;
    size_t   cbuf_size = 0;
    jlong   *theArray;
    jboolean isCopy;
    if (info == NULL) {
        h5nullArgument(env, "H5Pget_fapl_direct:  info input array is NULL");
    } /* end if */
    else {
        if (ENVPTR->GetArrayLength(ENVPAR info) < 3) {
            h5badArgument( env, "H5Pget_fapl_direct:  info input array < 4");
        } /* end if */
        else {
            theArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR info, &isCopy);
            if (theArray == NULL) {
                h5JNIFatalError(env, "H5Pget_fapl_direct:  info not pinned");
            } /* end if */
            else {
                retVal = H5Pget_fapl_direct((hid_t)fapl_id, &alignment, &block_size, &cbuf_size);
                if(retVal < 0) {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR info, theArray, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else {
                    theArray[0] = (jlong)alignment;
                    theArray[1] = (jlong)block_size;
                    theArray[2] = (jlong)cbuf_size;
                    ENVPTR->ReleaseLongArrayElements(ENVPAR info, theArray, 0);
                } /* end else */
            } /* end else */
        } /* end else */
   } /* end else */
#else
    if (retVal < 0)
        h5libraryError(env);
#endif

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1direct */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_sec2
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1sec2
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = -1;

    retVal = H5Pset_fapl_sec2((hid_t) fapl_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1sec2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_stdio
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1stdio
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = -1;

    retVal = H5Pset_fapl_stdio((hid_t) fapl_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1stdio */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_windows
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1windows
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = -1;

#ifdef H5_HAVE_WINDOWS
    retVal = H5Pset_fapl_windows((hid_t) fapl_id);
#endif
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1windows */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_muti
 * Signature: (J[I[J[Ljava/lang/String;[J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi
    (JNIEnv *env, jclass clss, jlong tid, jintArray memb_map,
        jlongArray memb_fapl, jobjectArray memb_name, jlongArray memb_addr)
{
    herr_t   status = -1;
    int      i;
    jint    *themapArray = NULL;
    jlong   *thefaplArray = NULL;
    jlong   *theaddrArray = NULL;
    char   **mName = NULL;
    jstring  str;
    jboolean isCopy;
    int relax = 0;

    if (memb_map) {
        themapArray = (jint*)ENVPTR->GetIntArrayElements(ENVPAR memb_map, &isCopy);
        if (themapArray == NULL) {
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_map not pinned");
            return 0;
        } /* end if */
    } /* end if */

    if (memb_fapl) {
        thefaplArray = (jlong*)ENVPTR->GetLongArrayElements(ENVPAR memb_fapl, &isCopy);
        if (thefaplArray == NULL) {
            if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_fapl not pinned");
            return 0;
        } /* end if */
    } /* end if */

    if (memb_addr) {
        theaddrArray = (jlong*)ENVPTR->GetLongArrayElements(ENVPAR memb_addr, &isCopy);
        if (theaddrArray == NULL) {
            if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
            if (memb_fapl) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, JNI_ABORT);
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_addr not pinned");
            return 0;
        } /* end if */
    } /* end if */

    if (memb_name)
        mName = (char**)HDcalloc(H5FD_MEM_NTYPES, sizeof (*mName));

    status = H5Pget_fapl_multi((hid_t)tid, (H5FD_mem_t*)themapArray, (hid_t*)thefaplArray, mName, (haddr_t*)theaddrArray, (hbool_t*)&relax);

    if (status < 0) {
        if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
        if (memb_fapl) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, JNI_ABORT);
        if (memb_addr) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_addr, theaddrArray, JNI_ABORT);
        if (memb_name) h5str_array_free(mName, H5FD_MEM_NTYPES);
        h5libraryError(env);
    } /* end if */
    else {
        if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, 0);
        if (memb_fapl) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, 0);
        if (memb_addr) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_addr, theaddrArray, 0);

        if (memb_name) {
            if (mName) {
                for (i = 0; i < H5FD_MEM_NTYPES; i++) {
                    if (*(mName + i)) {
                        str = ENVPTR->NewStringUTF(ENVPAR *(mName+i));
                        ENVPTR->SetObjectArrayElement(ENVPAR memb_name, i, (jobject)str);
                    } /* end if */
                } /* for (i=0; i<n; i++)*/
            } /* end if */
            h5str_array_free(mName, H5FD_MEM_NTYPES);
        } /* end if */
    } /* end else */

    return (relax!=0);
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_muti
 * Signature: (J[I[J[Ljava/lang/String;[JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi
    (JNIEnv *env, jclass clss, jlong tid, jintArray memb_map,
        jlongArray memb_fapl, jobjectArray memb_name, jlongArray memb_addr, jboolean relax)
{
    herr_t       status = -1;
    jint        *themapArray = NULL;
    jlong       *thefaplArray = NULL;
    jlong       *theaddrArray = NULL;
    jboolean     isCopy;
    jclass       Sjc;
    jstring      rstring;
    jobject      o;
    jboolean     bb;
    const char **mName = NULL;
    char        *member_name[H5FD_MEM_NTYPES];

    if (memb_map) {
        themapArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR memb_map, &isCopy);
        if (themapArray == NULL) {
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_map not pinned");
            return;
        } /* end if */
    } /* end if */

    if (memb_fapl) {
        thefaplArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR memb_fapl, &isCopy);
        if (thefaplArray == NULL) {
            if (memb_map)
                ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_fapl not pinned");
            return;
        } /* end if */
    } /* end if */

    if (memb_addr) {
        theaddrArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR memb_addr, &isCopy);
        if (theaddrArray == NULL) {
            if (memb_map)
                ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
            if (memb_fapl)
                ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, JNI_ABORT);
            h5JNIFatalError(env, "H5Pget_fapl_muti:  memb_addr not pinned");
            return;
        } /* end if */
    } /* end if */

    HDmemset(member_name, 0, H5FD_MEM_NTYPES * sizeof(char*));
    if (memb_name) {
        int i;
        for (i = 0; i < H5FD_MEM_NTYPES; i++) {
            jstring obj = (jstring) ENVPTR->GetObjectArrayElement(ENVPAR (jobjectArray) memb_name, i);
            if (obj != 0) {
                jsize length = ENVPTR->GetStringUTFLength(ENVPAR obj);
                const char *utf8 = ENVPTR->GetStringUTFChars(ENVPAR obj, 0);

                if (utf8) {
                    member_name[i] = (char*)HDmalloc(strlen(utf8) + 1);
                    if (member_name[i]) {
                        strcpy(member_name[i], utf8);
                    } /* end if */
                } /* end if */

                ENVPTR->ReleaseStringUTFChars(ENVPAR obj, utf8);
                ENVPTR->DeleteLocalRef(ENVPAR obj);
            } /* end if */
        } /* end for */
        mName = (const char **)member_name;
    } /* end if */

    status = H5Pset_fapl_multi((hid_t)tid, (const H5FD_mem_t *)themapArray, (const hid_t *)thefaplArray, mName, (const haddr_t *)theaddrArray, (hbool_t)relax);

    if (status < 0) {
        if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, JNI_ABORT);
        if (memb_fapl) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, JNI_ABORT);
        if (memb_addr) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_addr, theaddrArray, JNI_ABORT);
        if (memb_name) {
            int i;
            for (i = 0; i < H5FD_MEM_NTYPES; i++)
                HDfree(member_name[i]);
        } /* end if */
        h5libraryError(env);
    } /* end if */
    else {
        if (memb_map) ENVPTR->ReleaseIntArrayElements(ENVPAR memb_map, themapArray, 0);
        if (memb_fapl) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_fapl, thefaplArray, 0);
        if (memb_addr) ENVPTR->ReleaseLongArrayElements(ENVPAR memb_addr, theaddrArray, 0);
        if (memb_name) {
            if (mName != NULL) {
                int i;
                Sjc = ENVPTR->FindClass(ENVPAR  "java/lang/String");
                if (Sjc != NULL) {
                    for (i = 0; i < H5FD_MEM_NTYPES; i++) {
                        rstring = ENVPTR->NewStringUTF(ENVPAR member_name[i]);
                        o = ENVPTR->GetObjectArrayElement(ENVPAR memb_name, i);
                        if (o != NULL) {
                            bb = ENVPTR->IsInstanceOf(ENVPAR o, Sjc);
                            if (bb == JNI_TRUE) {
                                ENVPTR->SetObjectArrayElement(ENVPAR memb_name, i, (jobject)rstring);
                            } /* end if */
                            ENVPTR->DeleteLocalRef(ENVPAR o);
                        } /* end if */
                        HDfree(member_name[i]);
                    } /* end for */
                } /* end if */
            } /* end if */
        } /* end if */
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_split
 * Signature: (JLjava/lang/String;JLjava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1split
    (JNIEnv *env, jclass clss, jlong fapl_id, jstring metaext, jlong meta_pl_id, jstring rawext, jlong raw_pl_id)
{
    herr_t      retVal = -1;
    const char *mstr;
    const char *rstr;

    PIN_JAVA_STRING_TWO(metaext, mstr, rawext, rstr);
    if (mstr != NULL && rstr != NULL) {
        retVal = H5Pset_fapl_split((hid_t)fapl_id, mstr, (hid_t)meta_pl_id, rstr, (hid_t)raw_pl_id);

        UNPIN_JAVA_STRING_TWO(metaext, mstr, rawext, rstr);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1split */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_meta_block_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1meta_1block_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    long sz = (long)size;

    if (H5Pset_meta_block_size((hid_t)plist, (hsize_t)sz) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1meta_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_meta_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1meta_1block_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    hsize_t  s;

    if (H5Pget_meta_block_size((hid_t)plist, &s) < 0)
        h5libraryError(env);

    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1meta_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sieve_buf_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sieve_1buf_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    size_t sz = (size_t)size;

    if (H5Pset_sieve_buf_size((hid_t)plist, (size_t)sz) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1sieve_1buf_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sieve_buf_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sieve_1buf_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    size_t  s;

    if (H5Pget_sieve_buf_size((hid_t)plist, &s) < 0)
        h5libraryError(env);

    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1sieve_1buf_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_file_cache_size
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1file_1cache_1size
    (JNIEnv *env, jclass clss, jlong plist, jint size)
{
    unsigned  sz = (unsigned)size;

    if (H5Pset_elink_file_cache_size((hid_t)plist, (unsigned)sz) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1file_1cache_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_file_cache_size
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1file_1cache_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    unsigned  s;

    if (H5Pget_elink_file_cache_size((hid_t)plist, &s) < 0)
        h5libraryError(env);

    return (jint)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1file_1cache_1size */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_config
 * Signature: (J)Lhdf/hdf5lib/structs/H5AC_cache_config_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5AC_cache_config_t cacheinfo;
    herr_t     status = -1;
    jvalue     args[30];
    jstring    j_str = NULL;
    jobject    ret_obj = NULL;

    HDmemset(&cacheinfo, 0, sizeof(H5AC_cache_config_t));
    cacheinfo.version = H5AC__CURR_CACHE_CONFIG_VERSION;

    status = H5Pget_mdc_config((hid_t)plist, &cacheinfo);

    if (status < 0) {
       h5libraryError(env);
    } /* end if */
    else {
        args[0].i = cacheinfo.version;
        args[1].z = cacheinfo.rpt_fcn_enabled;
        args[2].z = cacheinfo.open_trace_file;
        args[3].z = cacheinfo.close_trace_file;
        if (cacheinfo.trace_file_name != NULL) {
            j_str = ENVPTR->NewStringUTF(ENVPAR cacheinfo.trace_file_name);
        } /* end if */
        args[4].l = j_str;
        args[5].z = cacheinfo.evictions_enabled;
        args[6].z = cacheinfo.set_initial_size;
        args[7].j = (jlong)cacheinfo.initial_size;
        args[8].d = cacheinfo.min_clean_fraction;
        args[9].j = (jlong)cacheinfo.max_size;
        args[10].j = (jlong)cacheinfo.min_size;
        args[11].j = cacheinfo.epoch_length;
        args[12].i = cacheinfo.incr_mode;
        args[13].d = cacheinfo.lower_hr_threshold;
        args[14].d = cacheinfo.increment;
        args[15].z = cacheinfo.apply_max_increment;
        args[16].j = (jlong)cacheinfo.max_increment;
        args[17].i = cacheinfo.flash_incr_mode;
        args[18].d = cacheinfo.flash_multiple;
        args[19].d = cacheinfo.flash_threshold;
        args[20].i = cacheinfo.decr_mode;
        args[21].d = cacheinfo.upper_hr_threshold;
        args[22].d = cacheinfo.decrement;
        args[23].z = cacheinfo.apply_max_decrement;
        args[24].j = (jlong)cacheinfo.max_decrement;
        args[25].i = cacheinfo.epochs_before_eviction;
        args[26].z = cacheinfo.apply_empty_reserve;
        args[27].d = cacheinfo.empty_reserve;
        args[28].j = (jlong)cacheinfo.dirty_bytes_threshold;
        args[29].i = cacheinfo.metadata_write_strategy;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5AC_cache_config_t", "(IZZZLjava/lang/String;ZZJDJJJIDDZJIDDIDDZJIZDJI)V", args);
    } /* end else */
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_config
 * Signature: (JLhdf/hdf5lib/structs/H5AC_cache_config_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config
    (JNIEnv *env, jclass clss, jlong plist, jobject cache_config)
{
    herr_t      status = -1;
    jclass      cls;
    jfieldID    fid;
    jstring     j_str;
    const char *str;
    H5AC_cache_config_t cacheinfo;

    cls = ENVPTR->GetObjectClass(ENVPAR cache_config);
    fid = ENVPTR->GetFieldID(ENVPAR cls, "version", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  version");
        return;
    } /* end if */
    cacheinfo.version = ENVPTR->GetIntField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading version failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "rpt_fcn_enabled", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  rpt_fcn_enabled");
        return;
    } /* end if */
    cacheinfo.rpt_fcn_enabled = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading rpt_fcn_enabled failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "open_trace_file", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  open_trace_file");
        return;
    } /* end if */
    cacheinfo.open_trace_file = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading open_trace_file failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "close_trace_file", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  close_trace_file");
        return;
    } /* end if */
    cacheinfo.close_trace_file = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading close_trace_file failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "trace_file_name", "Ljava/lang/String;");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  trace_file_name");
        return;
    } /* end if */
    j_str = (jstring)ENVPTR->GetObjectField(ENVPAR cache_config, fid);
    str = ENVPTR->GetStringUTFChars(ENVPAR j_str, NULL);
    if (str == NULL) {
        h5JNIFatalError(env, "H5Pset_mdc_config: out of memory trace_file_name");
        return;
    } /* end if */
    strncpy(cacheinfo.trace_file_name, str, 1025);
    ENVPTR->ReleaseStringUTFChars(ENVPAR j_str, str);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading trace_file_name failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "evictions_enabled", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  evictions_enabled");
        return;
    } /* end if */
    cacheinfo.evictions_enabled = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading evictions_enabled failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "set_initial_size", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  set_initial_size");
        return;
    } /* end if */
    cacheinfo.set_initial_size = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading set_initial_size failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "initial_size", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  initial_size");
        return;
    } /* end if */
    cacheinfo.initial_size = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading initial_size failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "min_clean_fraction", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  min_clean_fraction");
        return;
    } /* end if */
    cacheinfo.min_clean_fraction = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading min_clean_fraction failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "max_size", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  max_size");
        return;
    } /* end if */
    cacheinfo.max_size = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading max_size failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "min_size", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  min_size");
        return;
    } /* end if */
    cacheinfo.min_size = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading min_size failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "epoch_length", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  epoch_length");
        return;
    }
    cacheinfo.epoch_length = (long int)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading epoch_length failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "incr_mode", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  incr_mode");
        return;
    } /* end if */
    cacheinfo.incr_mode = ENVPTR->GetIntField(ENVPAR cache_config, fid); /*(enum H5C_cache_incr_mode) */
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading incr_mode failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "lower_hr_threshold", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  lower_hr_threshold");
        return;
    } /* end if */
    cacheinfo.lower_hr_threshold = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading lower_hr_threshold failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "increment", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  increment");
        return;
    } /* end if */
    cacheinfo.increment = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading increment failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "apply_max_increment", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  apply_max_increment");
        return;
    } /* end if */
    cacheinfo.apply_max_increment = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading apply_max_increment failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "max_increment", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  max_increment");
        return;
    } /* end if */
    cacheinfo.max_increment = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading max_increment failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "flash_incr_mode", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  flash_incr_mode");
        return;
    } /* end if */
    cacheinfo.flash_incr_mode = ENVPTR->GetIntField(ENVPAR cache_config, fid); /*(enum H5C_cache_flash_incr_mode) */
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading flash_incr_mode failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "flash_multiple", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  flash_multiple");
        return;
    } /* end if */
    cacheinfo.flash_multiple = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading flash_multiple failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "flash_threshold", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  flash_threshold");
        return;
    } /* end if */
    cacheinfo.flash_threshold = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading flash_threshold failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "decr_mode", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  decr_mode");
        return;
    } /* end if */
    cacheinfo.decr_mode = ENVPTR->GetIntField(ENVPAR cache_config, fid); /*(enum H5C_cache_decr_mode) */
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading decr_mode failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "upper_hr_threshold", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  upper_hr_threshold");
        return;
    } /* end if */
    cacheinfo.upper_hr_threshold = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading upper_hr_threshold failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "decrement", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  decrement");
        return;
    } /* end if */
    cacheinfo.decrement = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading decrement failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "apply_max_decrement", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  apply_max_decrement");
        return;
    } /* end if */
    cacheinfo.apply_max_decrement = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading apply_max_decrement failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "max_decrement", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  max_decrement");
        return;
    } /* end if */
    cacheinfo.max_decrement = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading max_decrement failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "epochs_before_eviction", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  epochs_before_eviction");
        return;
    } /* end if */
    cacheinfo.epochs_before_eviction = ENVPTR->GetIntField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading epochs_before_eviction failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "apply_empty_reserve", "Z");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  apply_empty_reserve");
        return;
    } /* end if */
    cacheinfo.apply_empty_reserve = ENVPTR->GetBooleanField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading apply_empty_reserve failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "empty_reserve", "D");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  empty_reserve");
        return;
    } /* end if */
    cacheinfo.empty_reserve = ENVPTR->GetDoubleField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading empty_reserve failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "dirty_bytes_threshold", "J");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  dirty_bytes_threshold");
        return;
    } /* end if */
    cacheinfo.dirty_bytes_threshold = (size_t)ENVPTR->GetLongField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading dirty_bytes_threshold failed");
        return;
    } /* end if */

    fid = ENVPTR->GetFieldID(ENVPAR cls, "metadata_write_strategy", "I");
    if(fid == 0) {
        h5badArgument(env, "H5Pset_mdc_config:  metadata_write_strategy");
        return;
    } /* end if */
    cacheinfo.metadata_write_strategy = ENVPTR->GetIntField(ENVPAR cache_config, fid);
    if(ENVPTR->ExceptionOccurred(ENVONLY)) {
        h5JNIFatalError(env, "H5Pset_mdc_config: loading metadata_write_strategy failed");
    } /* end if */
    else {
        status = H5Pset_mdc_config((hid_t)plist, &cacheinfo);

        if (status < 0)
        h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk_cache
 * Signature: (JJJD)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk_1cache
    (JNIEnv *env, jclass clss, jlong dapl, jlong rdcc_nslots,
        jlong rdcc_nbytes, jdouble rdcc_w0)
{
    if (H5Pset_chunk_cache((hid_t)dapl, (size_t)rdcc_nslots, (size_t)rdcc_nbytes, (double) rdcc_w0) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1chunk_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk_cache
 * Signature: (J[J[J[D)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk_1cache
    (JNIEnv *env, jclass clss, jlong dapl, jlongArray rdcc_nslots,
        jlongArray rdcc_nbytes, jdoubleArray rdcc_w0)
{
    herr_t   status = -1;
    jint     mode;
    jdouble *w0Array = (jdouble *)NULL;
    jlong   *rdcc_nslotsArray = (jlong *)NULL;
    jlong   *nbytesArray = (jlong *)NULL;
    jboolean isCopy;

    if (rdcc_w0 != NULL) {
        w0Array = (jdouble *)ENVPTR->GetDoubleArrayElements(ENVPAR rdcc_w0, &isCopy);
        if (w0Array == NULL) {
            h5JNIFatalError(env, "H5Pget_chunk_cache:  w0_array array not pinned");
            return;
        } /* end if */
    } /* end else */

    if (rdcc_nslots != NULL) {
        rdcc_nslotsArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR rdcc_nslots, &isCopy);
        if (rdcc_nslotsArray == NULL) {
            /* exception -- out of memory */
            if (w0Array != NULL) {
                ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, JNI_ABORT);
            } /* end if */
            h5JNIFatalError(env, "H5Pget_chunk_cache:  rdcc_nslots array not pinned");
            return;
        } /* end if */
    } /* end else */

    if (rdcc_nbytes != NULL) {
        nbytesArray = (jlong *)ENVPTR->GetLongArrayElements(ENVPAR rdcc_nbytes, &isCopy);
        if (nbytesArray == NULL) {
            if (w0Array != NULL) {
                ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, JNI_ABORT);
            } /* end if */
            if (rdcc_nslotsArray != NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nslots, rdcc_nslotsArray, JNI_ABORT);
            } /* end if */
            h5JNIFatalError(env, "H5Pget_chunk_cache:  nbytesArray array not pinned");
            return;
        } /* end if */
    } /* end else */

    { /* direct cast (size_t *)variable fails on 32-bit environment */
        long long rdcc_nslots_temp = *(rdcc_nslotsArray);
        size_t rdcc_nslots_t = (size_t)rdcc_nslots_temp;
        long long nbytes_temp = *(nbytesArray);
        size_t nbytes_t = (size_t)nbytes_temp;

        status = H5Pget_chunk_cache((hid_t)dapl, &rdcc_nslots_t, &nbytes_t, (double *)w0Array);

        *rdcc_nslotsArray = (jlong)rdcc_nslots_t;
        *nbytesArray = (jlong)nbytes_t;
    } /* end direct cast special handling */

    if (status < 0) {
        mode = JNI_ABORT;
    } /* end if */
    else {
        mode = 0; /* commit and free */
    } /* end else */

    if (rdcc_nslotsArray != NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nslots, rdcc_nslotsArray, mode);
    }
    /* end if */
    if (nbytesArray != NULL) {
        ENVPTR->ReleaseLongArrayElements(ENVPAR rdcc_nbytes, nbytesArray, mode);
    } /* end if */

    if (w0Array != NULL) {
        ENVPTR->ReleaseDoubleArrayElements(ENVPAR rdcc_w0, w0Array, mode);
    } /* end if */

    if (status < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pget_1chunk_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_obj_track_times
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1obj_1track_1times
    (JNIEnv *env, jclass clss, jlong objplid)
{
    hbool_t  track_times;

    if (H5Pget_obj_track_times((hid_t)objplid, &track_times) < 0) {
        h5libraryError(env);
        return JNI_FALSE;
    } /* end if */
    if (track_times == 1) {
        return JNI_TRUE;
    } /* end if */
    return JNI_FALSE;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1obj_1track_1times */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_obj_track_times
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1obj_1track_1times
    (JNIEnv *env, jclass clss, jlong objplid, jboolean track_times)
{
    hbool_t  track;

    if (track_times == JNI_TRUE) {
        track = 1;
    } /* end if */
    else {
        track = 0;
    } /* end else */

    if (H5Pset_obj_track_times((hid_t)objplid, track) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1obj_1track_1times */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_char_encoding
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding
    (JNIEnv *env, jclass clss, jlong acpl)
{
    H5T_cset_t  encoding;

    if (H5Pget_char_encoding((hid_t)acpl, &encoding) < 0)
        h5libraryError(env);

    return encoding;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_char_encoding
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding
    (JNIEnv *env, jclass clss, jlong acpl, jint encoding)
{
    if (H5Pset_char_encoding((hid_t)acpl, (H5T_cset_t)encoding) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual
 * Signature: (JJLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong vspace_id,
        jstring src_file_name, jstring src_dset_name, jlong src_space_id)
{
    herr_t      retVal = -1;
    const char *fstr;
    const char *dstr;

    PIN_JAVA_STRING_TWO(src_file_name, fstr, src_dset_name, dstr);
    if (fstr != NULL && dstr != NULL) {
        retVal = H5Pset_virtual((hid_t)dcpl_id, (hid_t)vspace_id, fstr, dstr, (hid_t)src_space_id);

        UNPIN_JAVA_STRING_TWO(src_file_name, fstr, src_dset_name, dstr);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_count
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1count
    (JNIEnv *env, jclass clss, jlong dcpl_id)
{
    size_t  s;

    if (H5Pget_virtual_count((hid_t)dcpl_id, &s) < 0)
        h5libraryError(env);

    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1count */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_vspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    hid_t space_id = -1;

    space_id = H5Pget_virtual_vspace((hid_t)dcpl_id, (size_t)index);
    if (space_id < 0)
        h5libraryError(env);

    return (jlong)space_id;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_srcspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1srcspace
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    hid_t space_id = -1;

    space_id = H5Pget_virtual_srcspace((hid_t)dcpl_id, (size_t)index);
    if (space_id < 0)
        h5libraryError(env);

    return (jlong)space_id;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1srcspace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_filename
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    char    *fname;
    ssize_t  buf_size;
    ssize_t  status;
    jstring  str = NULL;

    /* get the length of the filename */
    buf_size = H5Pget_virtual_filename((hid_t)dcpl_id, (size_t)index, NULL, 0);
    if (buf_size < 0) {
        h5badArgument( env, "H5Pget_virtual_filename:  buf_size < 0");
    } /* end if */
    else if (buf_size >= 0) {
        buf_size++; /* add extra space for the null terminator */
        fname = (char *)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (fname == NULL) {
            h5outOfMemory( env, "H5Pget_virtual_filename:  malloc failed");
        } /* end if */
        else {
            status = H5Pget_virtual_filename((hid_t)dcpl_id, (size_t)index, fname, (size_t)buf_size);

            if (status >= 0) {
                str = ENVPTR->NewStringUTF(ENVPAR fname);
                HDfree(fname);
                if (str == NULL)
                    h5JNIFatalError( env, "H5Pget_virtual_filename:  return string not allocated");
            } /* end if */
            else {
                HDfree(fname);
                h5libraryError(env);
            } /* end else */
        } /* end else */
    } /* end else if */

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_dsetname
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1dsetname
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    char    *dname;
    ssize_t  buf_size;
    ssize_t  status;
    jstring  str = NULL;

    /* get the length of the filename */
    buf_size = H5Pget_virtual_dsetname((hid_t)dcpl_id, (size_t)index, NULL, 0);
    if (buf_size < 0) {
        h5badArgument( env, "H5Pget_virtual_dsetname:  buf_size < 0");
    } /* end if */
    else if (buf_size > 0) {
        buf_size++; /* add extra space for the null terminator */
        dname = (char *)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (dname == NULL) {
            h5outOfMemory( env, "H5Pget_virtual_dsetname:  malloc failed");
        } /* end if */
        else {
            status = H5Pget_virtual_dsetname((hid_t)dcpl_id, (size_t)index, dname, (size_t)buf_size);

            if (status >= 0) {
                str = ENVPTR->NewStringUTF(ENVPAR dname);
                HDfree(dname);
                if (str == NULL)
                    h5JNIFatalError( env, "H5Pget_virtual_dsetname:  return string not allocated");
            } /* end if */
            else {
                HDfree(dname);
                h5libraryError(env);
            } /* end else */
        } /* end else */
    } /* end else if */
    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1dsetname */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_view
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1view
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    H5D_vds_view_t virtual_view;

    if (H5Pget_virtual_view((hid_t)dapl_id, &virtual_view) < 0)
        h5libraryError(env);

    return (jint)virtual_view;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1view */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_view
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1view
    (JNIEnv *env, jclass clss, jlong dapl_id, jint view)
{
    if (H5Pset_virtual_view((hid_t)dapl_id, (H5D_vds_view_t)view) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual_1view */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_printf_gap
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1printf_1gap
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    hsize_t gap_size;

    if (H5Pget_virtual_printf_gap((hid_t)dapl_id, &gap_size) < 0)
        h5libraryError(env);

    return (jlong)gap_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1printf_1gap */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_printf_gap
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1printf_1gap
    (JNIEnv *env, jclass clss, jlong dapl_id, jlong gap_size)
{
    if (H5Pset_virtual_printf_gap((hid_t)dapl_id, (hsize_t)gap_size) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual_1printf_1gap */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space
 * Signature: (J[I[J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space
    (JNIEnv *env, jclass clss, jlong fcpl_id, jintArray strategy, jlongArray threshold)
{
    herr_t   status = -1;
    jint    *thestrategyArray = NULL;
    jlong   *thethresholdArray = NULL;
    jboolean isCopy;

    if (strategy) {
        thestrategyArray = (jint*)ENVPTR->GetIntArrayElements(ENVPAR strategy, &isCopy);
        if (thestrategyArray == NULL) {
            h5JNIFatalError(env, "H5Pget_file_space:  strategy not pinned");
            return;
        }
    }

    if (threshold) {
        thethresholdArray = (jlong*)ENVPTR->GetLongArrayElements(ENVPAR threshold, &isCopy);
        if (thethresholdArray == NULL) {
            if (strategy) ENVPTR->ReleaseIntArrayElements(ENVPAR strategy, thestrategyArray, JNI_ABORT);
            h5JNIFatalError(env, "H5Pget_file_space:  threshold not pinned");
            return;
        } /* end if */
    } /* end if */

    status = H5Pget_file_space((hid_t)fcpl_id, (H5F_file_space_type_t*)thestrategyArray, (hsize_t*)thethresholdArray);

    if (status < 0) {
        if (strategy) ENVPTR->ReleaseIntArrayElements(ENVPAR strategy, thestrategyArray, JNI_ABORT);
        if (threshold) ENVPTR->ReleaseLongArrayElements(ENVPAR threshold, thethresholdArray, JNI_ABORT);
        h5libraryError(env);
    } /* end if */
    else {
        if (strategy) ENVPTR->ReleaseIntArrayElements(ENVPAR strategy, thestrategyArray, 0);
        if (threshold) ENVPTR->ReleaseLongArrayElements(ENVPAR threshold, thethresholdArray, 0);
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Pget_1file_1space */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_file_space
 * Signature: (JIJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1file_1space
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint strategy, jlong threshold)
{
    if (H5Pset_file_space((hid_t)fcpl_id, (H5F_file_space_type_t)strategy, (hsize_t)threshold) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1file_1space */


static herr_t
H5P_cls_create_cb
    (hid_t prop_id, void *create_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR create_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLhdf/hdf5lib/callbacks/H5P_cls_create_func_t;)I");
            if (mid != 0) {
                /* fprintf(stderr, "JNI H5P_cls_create_func_cb execute\n"); fflush(stderr); */
                status = CBENVPTR->CallIntMethod(CBENVPAR create_callback, mid, prop_id, create_data);
                /* fprintf(stderr, "\nJNI H5P_cls_create_func_cb status: %d\n", status); fflush(stderr); */
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_cls_create_cb */

static herr_t
H5P_cls_copy_cb
    (hid_t new_prop_id, hid_t old_prop_id, void *copy_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR copy_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JJLhdf/hdf5lib/callbacks/H5P_cls_copy_func_t;)I");
            if (mid != 0) {
                status = CBENVPTR->CallIntMethod(CBENVPAR copy_callback, mid, new_prop_id, old_prop_id, copy_data);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_cls_ccopy_cb */

static herr_t
H5P_cls_close_cb
    (hid_t prop_id, void *close_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR close_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLhdf/hdf5lib/callbacks/H5P_cls_close_func_t;)I");
            if (mid != 0) {
                status = CBENVPTR->CallIntMethod(CBENVPAR close_callback, mid, prop_id, close_data);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_cls_close_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_log_options
 * Signature: (JZLjava/lang/String;Z)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1log_1options
    (JNIEnv *env, jclass clss, jlong fapl_id, jboolean is_enabled, jstring location, jboolean start_on_access)
{
    herr_t      retVal = -1;
    const char *lstr;

    PIN_JAVA_STRING(location, lstr);

    retVal = H5Pset_mdc_log_options((hid_t)fapl_id, (hbool_t)is_enabled, lstr, (hbool_t)start_on_access);

    UNPIN_JAVA_STRING(location, lstr);

    if (retVal < 0) {
        h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pset_1mdc_1log_1options */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_log_options
 * Signature: (J[Z)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1log_1options
    (JNIEnv *env, jclass clss, jlong fapl_id, jbooleanArray mdc_log_options)
{
    hbool_t    is_enabled;
    hbool_t    start_on_access;
    jboolean  *mdc_log_options_ptr;
    char      *lname;
    size_t     location_size;
    ssize_t    status;
    jstring    str = NULL;
    jint       size;
    jboolean   isCopy;

    if (mdc_log_options == NULL) {
        h5nullArgument(env, "H5Fget_mdc_log_options:  mdc_log_options is NULL");
    } /* end if */
    else {
        size = (int)ENVPTR->GetArrayLength(ENVPAR mdc_log_options);
        if (size < 2) {
            h5badArgument(env, "H5Fget_mdc_log_options:  length of mdc_log_options < 2.");
        } /* end if */
        else {
            /* get the length of the filename */
            H5Pget_mdc_log_options((hid_t)fapl_id, &is_enabled, NULL, &location_size, &start_on_access);
            if (location_size < 0) {
                h5badArgument(env, "H5Pget_mdc_log_options:  location_size < 0");
            }/* end if */
            else if (location_size > 0) {
                location_size++; /* add extra space for the null terminator */
                lname = (char *)HDmalloc(sizeof(char) * location_size);
                if (lname == NULL) {
                    h5outOfMemory(env, "H5Pget_mdc_log_options:  malloc failed");
                } /* end if */
                else {
                    status = H5Pget_mdc_log_options((hid_t)fapl_id, &is_enabled, lname, &location_size, &start_on_access);

                    if (status < 0) {
                        HDfree(lname);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        str = ENVPTR->NewStringUTF(ENVPAR lname);
                        HDfree(lname);
                        if (str == NULL) {
                            h5JNIFatalError(env, "H5Pget_mdc_log_options:  return string not allocated");
                        } /* end if */
                        else {
                            mdc_log_options_ptr = ENVPTR->GetBooleanArrayElements(ENVPAR mdc_log_options, &isCopy);
                            mdc_log_options_ptr[0] = (jboolean)is_enabled;
                            mdc_log_options_ptr[1] = (jboolean)start_on_access;
                            ENVPTR->ReleaseBooleanArrayElements(ENVPAR mdc_log_options, mdc_log_options_ptr, 0);
                        } /* end else */
                    } /* end else */
                } /* end else */
            } /* end else if*/
        } /* end else */
    } /* end else */

    return (jstring)str;
} /* end if */

static herr_t
H5D_append_cb
    (hid_t dataset_id, hsize_t *cur_dims, void *op_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jlongArray cur_dimsArray;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) != 0) {
        JVMPTR->DetachCurrentThread(JVMPAR);
        return -1;
    } /* end if */
    cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
    if (cls != 0) {
        mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(J[JLhdf/hdf5lib/callbacks/H5D_append_t;)I");
        if (mid != 0) {
            if (cur_dims != NULL) {
                cur_dimsArray = CBENVPTR->NewLongArray(CBENVPAR 2);
                if (cur_dimsArray != NULL) {
                    CBENVPTR->SetLongArrayRegion(CBENVPAR cur_dimsArray, 0, 2, (const jlong *)cur_dims);

                    status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, dataset_id, cur_dims, op_data);
                }
            }
        }
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5D_append_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_append_flush
 * Signature: (JI[JLjava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1append_1flush
    (JNIEnv *env, jclass clss, jlong plist_id, jint ndims, jlongArray boundary, jobject callback_op, jobject op_data)
{
    herr_t   status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if (op_data == NULL) {
        h5nullArgument(env, "H5Ovisit:  op_data is NULL");
    } /* end if */
    else if (callback_op == NULL) {
        h5nullArgument(env, "H5Ovisit:  callback_op is NULL");
    } /* end if */
    else {
        status = H5Pset_append_flush((hid_t)plist_id, (unsigned)ndims, (const hsize_t*)boundary, (H5D_append_cb_t)H5D_append_cb, (void*)op_data);

        if (status < 0)
            h5libraryError(env);
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Pset_1append_1flush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class_nocb
 * Signature: (JLjava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class_1nocb
    (JNIEnv *env, jclass clss, jlong parent_class, jstring name)
{
    hid_t class_id = -1;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        class_id = H5Pcreate_class((hid_t)parent_class, cstr,  NULL, NULL, NULL, NULL, NULL, NULL);

        UNPIN_JAVA_STRING(name, cstr);

        if (class_id < 0)
            h5libraryError(env);
    }

    return (jlong)class_id;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate_1class_1nocb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class
 * Signature: (JLjava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class
    (JNIEnv *env, jclass clss, jlong parent_class, jstring name, jobject create_op,
        jobject create_data, jobject copy_op, jobject copy_data, jobject close_op, jobject close_data)
{
    hid_t class_id = -1;
    const char *cstr;
    copy_callback = copy_op;
    close_callback = close_op;
    create_callback = create_op;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        class_id = H5Pcreate_class((hid_t)parent_class, cstr, (H5P_cls_create_func_t)H5P_cls_create_cb, (void*) create_data,
                (H5P_cls_copy_func_t)H5P_cls_copy_cb, (void*) copy_data, (H5P_cls_close_func_t)H5P_cls_close_cb, (void*) close_data);

        UNPIN_JAVA_STRING(name, cstr);

        if (class_id < 0)
            h5libraryError(env);
    }

    return (jlong)class_id;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate_1class */

static herr_t
H5P_prp_create_cb
    (const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR create_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(Ljava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR create_callback, mid, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_create_cb */

static herr_t
H5P_prp_copy_cb
    (const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR copy_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(Ljava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR copy_callback, mid, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_copy_cb */

static herr_t
H5P_prp_close_cb
    (const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR close_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(Ljava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR close_callback, mid, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_close_cb */

static int
H5P_prp_compare_cb
    (void *value1, void *value2, size_t size)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR compare_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "([B[BJ)I");
            if (mid != 0) {
                status = CBENVPTR->CallIntMethod(CBENVPAR compare_callback, mid, value1, value2, size);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_compare_cb */

static herr_t
H5P_prp_get_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR get_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR get_callback, mid, prop_id, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_get_cb */

static herr_t
H5P_prp_set_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR set_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR set_callback, mid, prop_id, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_set_cb */

static herr_t
H5P_prp_delete_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR delete_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;J[B)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);
                status = CBENVPTR->CallIntMethod(CBENVPAR delete_callback, mid, prop_id, str, size, value);
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5P_prp_delete_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2_1nocb
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size, jbyteArray def_value)
{
    herr_t   status = -1;
    jbyte   *buffP;
    jboolean isCopy2;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        buffP = ENVPTR->GetByteArrayElements(ENVPAR def_value, &isCopy2);
        if (buffP == NULL) {
            h5JNIFatalError(env, "H5Pregister2:  buf not pinned");
        } /* end if */
        else {
            status = H5Pregister2((hid_t)cls_id, cstr, (size_t)prp_size, (void*)buffP, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, 0);
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, cstr);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pregister2_1nocb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size,
        jbyteArray def_value, jobject prp_create, jobject prp_set, jobject prp_get, jobject prp_delete,
        jobject prp_copy, jobject prp_cmp, jobject prp_close)
{
    herr_t   status = -1;
    jbyte   *buffP;
    jboolean isCopy2;
    const char *cstr;
    copy_callback = prp_copy;
    close_callback = prp_close;
    create_callback = prp_create;
    compare_callback = prp_cmp;
    set_callback = prp_set;
    get_callback = prp_get;
    delete_callback = prp_delete;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        buffP = ENVPTR->GetByteArrayElements(ENVPAR def_value, &isCopy2);
        if (buffP == NULL) {
            h5JNIFatalError(env, "H5Pregister2:  buf not pinned");
        } /* end if */
        else {
            status = H5Pregister2((hid_t)cls_id, cstr, (size_t)prp_size, (void*)buffP, (H5P_prp_create_func_t)H5P_prp_create_cb,
                (H5P_prp_set_func_t)H5P_prp_set_cb, (H5P_prp_get_func_t)H5P_prp_get_cb, (H5P_prp_delete_func_t)H5P_prp_delete_cb,
                (H5P_prp_copy_func_t)H5P_prp_copy_cb, (H5P_prp_compare_func_t)H5P_prp_compare_cb, (H5P_prp_close_func_t)H5P_prp_close_cb);

            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, 0);
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, cstr);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pregister2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2_1nocb
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size, jbyteArray def_value)
{
    herr_t   status = -1;
    jbyte   *buffP;
    jboolean isCopy2;
    const char *cstr;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        buffP = ENVPTR->GetByteArrayElements(ENVPAR def_value, &isCopy2);
        if (buffP == NULL) {
            h5JNIFatalError(env, "H5Pinsert2:  buf not pinned");
        } /* end if */
        else {
            status = H5Pinsert2((hid_t)cls_id, cstr, (size_t)prp_size, (void*)buffP, NULL, NULL, NULL, NULL, NULL, NULL);

            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, 0);
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, cstr);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pinsert2 */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size,
        jbyteArray def_value, jobject prp_set, jobject prp_get, jobject prp_delete,
        jobject prp_copy, jobject prp_cmp, jobject prp_close)
{
    herr_t   status = -1;
    jbyte   *buffP;
    jboolean isCopy2;
    const char *cstr;
    copy_callback = prp_copy;
    close_callback = prp_close;
    compare_callback = prp_cmp;
    set_callback = prp_set;
    get_callback = prp_get;
    delete_callback = prp_delete;

    PIN_JAVA_STRING(name, cstr);
    if (cstr != NULL) {
        buffP = ENVPTR->GetByteArrayElements(ENVPAR def_value, &isCopy2);
        if (buffP == NULL) {
            UNPIN_JAVA_STRING(name, cstr);
            h5JNIFatalError(env, "H5Pinsert2:  buf not pinned");
        } /* end if */
        else {
            status = H5Pinsert2((hid_t)cls_id, cstr, (size_t)prp_size, (void*)buffP,
                (H5P_prp_set_func_t)H5P_prp_set_cb, (H5P_prp_get_func_t)H5P_prp_get_cb, (H5P_prp_delete_func_t)H5P_prp_delete_cb,
                (H5P_prp_copy_func_t)H5P_prp_copy_cb, (H5P_prp_compare_func_t)H5P_prp_compare_cb, (H5P_prp_close_func_t)H5P_prp_close_cb);

            if (status < 0) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            else {
                ENVPTR->ReleaseByteArrayElements(ENVPAR def_value, buffP, 0);
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, cstr);
    }
} /* end Java_hdf_hdf5lib_H5_H5Pinsert2 */

static herr_t
H5P_iterate_cb
    (hid_t prop_id, const char *name, void *op_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;

    /* fprintf(stderr, "\nJNI H5P_iterate_cb entered\n"); fflush(stderr); */
    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) != 0) {
        /* fprintf(stderr, "\nJNI H5P_iterate_cb error: AttachCurrentThread failed\n"); fflush(stderr); */
        JVMPTR->DetachCurrentThread(JVMPAR);
    } /* end if */
    else {
        cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
        if (cls == 0) {
            /* fprintf(stderr, "\nJNI H5P_iterate_cb error: GetObjectClass failed\n"); fflush(stderr); */
            JVMPTR->DetachCurrentThread(JVMPAR);
        } /* end if */
        else {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/callbacks/H5P_iterate_t;)I");
            if (mid == 0) {
                /* fprintf(stderr, "\nJNI H5P_iterate_cb error: GetMethodID failed\n"); fflush(stderr); */
                JVMPTR->DetachCurrentThread(JVMPAR);
            } /* end if */
            else {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);

                /* fprintf(stderr, "JNI H5P_iterate_cb execute\n"); fflush(stderr); */
                status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, prop_id, str, op_data);
                /* fprintf(stderr, "\nJNI H5P_iterate_cb status: %d\n", status); fflush(stderr); */
            } /* end else */
        } /* end else */
    } /* end else */

    JVMPTR->DetachCurrentThread(JVMPAR);
    /* fprintf(stderr, "\nJNI H5P_iterate_cb leave\n"); fflush(stderr); */

    return status;
} /* end H5P_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Piterate
 * Signature: (J[ILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Piterate
    (JNIEnv *env, jclass clss, jlong prop_id, jintArray idx, jobject callback_op, jobject op_data)
{
    herr_t   status = -1;
    jint    *theArray = NULL;
    jboolean isCopy;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if (op_data == NULL) {
        h5nullArgument(env, "H5Piterate:  op_data is NULL");
    } /* end if */
    else if (callback_op == NULL) {
        h5nullArgument(env, "H5Piterate:  callback_op is NULL");
    } /* end else if */
    else {
        if (idx == NULL) {
            status = H5Piterate((hid_t)prop_id, NULL, (H5P_iterate_t)H5P_iterate_cb, (void*)op_data);
        } /* end if */
        else {
            theArray = (jint *)ENVPTR->GetIntArrayElements(ENVPAR idx, &isCopy);
            if (theArray == NULL) {
                h5JNIFatalError(env, "H5Piterate:  idx not pinned");
            } /* end if */
            else
                status = H5Piterate((hid_t)prop_id, (int*)&theArray[0], (H5P_iterate_t)H5P_iterate_cb, (void*)op_data);
        } /* end else */

        if (status < 0) {
            if(idx)
                ENVPTR->ReleaseIntArrayElements(ENVPAR idx, theArray, JNI_ABORT);
            h5libraryError(env);
        } /* end if */
        else if (idx)
            ENVPTR->ReleaseIntArrayElements(ENVPAR idx, theArray, 0);
    } /* end else */

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Piterate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_metadata_read_attempts
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1metadata_1read_1attempts
    (JNIEnv *env, jclass clss, jlong plist_id)
{
    unsigned attempts;
    if (H5Pget_metadata_read_attempts((hid_t)plist_id, &attempts) < 0)
        h5libraryError(env);

    return (jlong) attempts;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1metadata_1read_1attempts */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_metadata_read_attempts
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts
    (JNIEnv *env, jclass clss, jlong plist_id, jlong attempts)
{
     if (attempts <= 0) {
         h5badArgument(env, "H5Pset_metadata_read_attempts:  attempts <= 0");
     } /* end if */
     else {
        if(H5Pset_metadata_read_attempts((hid_t)plist_id, (unsigned)attempts) < 0)
            h5libraryError(env);
     } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
