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
#include "h5pFCPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_version
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1version
    (JNIEnv *env, jclass clss, jlong plist, jintArray version_info)
{
    jboolean  isCopy;
    jsize     arrLen;
    jint     *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == version_info)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_version: version_info input array is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, version_info)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_version: version_info array length < 0");
    }
    if (arrLen < 4)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_version: version_info input array length < 4");

    PIN_INT_ARRAY(ENVONLY, version_info, theArray, &isCopy, "H5Pget_version: version_info not pinned");

    if ((status = H5Pget_version((hid_t)plist, (unsigned *)&(theArray[0]),
            (unsigned *)&(theArray[1]), (unsigned *)&(theArray[2]), (unsigned *)&(theArray[3]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, version_info, theArray, (status < 0) ? JNI_ABORT : 0);

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
    long   sz = (long) size;
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_userblock((hid_t)plist, (hsize_t)sz)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    hsize_t   s;
    jlong    *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_userblock: size is NULL");

    PIN_LONG_ARRAY(ENVONLY, size, theArray, &isCopy, "H5Pget_userblock: size not pinned");

    if ((status = H5Pget_userblock((hid_t)plist, &s)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jlong)s;

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, size, theArray, (status < 0) ? JNI_ABORT : 0);

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
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_sizes((hid_t)plist, (size_t)sizeof_addr, (size_t)sizeof_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    jlong    *theArray = NULL;
    jsize     arrLen;
    size_t    ss;
    size_t    sa;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_sizes: size is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, size)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_sizes: size array length < 0");
    }
    if (arrLen < 2)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_sizes: size input array < 2 elements");

    PIN_LONG_ARRAY(ENVONLY, size, theArray, &isCopy, "H5Pget_sizes: size not pinned");

    if ((status = H5Pget_sizes((hid_t)plist, &sa, &ss)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jlong)sa;
    theArray[1] = (jlong)ss;

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, size, theArray, (status < 0) ? JNI_ABORT : 0);

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
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_sym_k((hid_t)plist, (unsigned)ik, (unsigned)lk)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    jsize     arrLen;
    jint     *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_sym_k: size is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, size)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_sym_k: size array length < 0");
    }
    if (arrLen < 2)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_sym_k: size < 2 elements");

    PIN_INT_ARRAY(ENVONLY, size, theArray, &isCopy, "H5Pget_sym_k: size not pinned");

    if ((status = H5Pget_sym_k((hid_t)plist, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, size, theArray, (status < 0) ? JNI_ABORT : 0);

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
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_istore_k((hid_t)plist, (unsigned)ik)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == ik)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_store_k: ik is NULL");

    PIN_INT_ARRAY(ENVONLY, ik, theArray, &isCopy, "H5Pget_store_k: size not pinned");

    if ((status = H5Pget_istore_k((hid_t)plist, (unsigned *)&(theArray[0]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, ik, theArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1istore_1k */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_file_space_page_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1file_1space_1page_1size
    (JNIEnv *env, jclass clss, jlong fcpl_id, jlong fsp_size)
{
    UNUSED(clss);

    if (H5Pset_file_space_page_size((hid_t)fcpl_id, (hsize_t)fsp_size) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1file_1space_1page_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space_page_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1page_1size
    (JNIEnv *env, jclass clss, jlong fcpl_id)
{
    hsize_t fsp_size = 0;

    UNUSED(clss);

    if (H5Pget_file_space_page_size((hid_t)fcpl_id, &fsp_size) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)fsp_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1page_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_file_space_strategy
 * Signature: (JIZJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1file_1space_1strategy
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint strategy, jboolean persist, jlong threshold)
{
    UNUSED(clss);

    if (H5Pset_file_space_strategy((hid_t)fcpl_id, (H5F_fspace_strategy_t)strategy, (hbool_t)persist, (hsize_t)threshold) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_file_space_strategy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space_strategy
 * Signature: (J[Z[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1strategy
    (JNIEnv *env, jclass clss, jlong fcpl_id, jbooleanArray persist, jlongArray threshold)
{
    H5F_fspace_strategy_t  thestrategy = H5F_FSPACE_STRATEGY_FSM_AGGR; /* Library default */
    jboolean               isCopy;
    jboolean              *persistArray = NULL;
    jlong                 *thresholdArray = NULL;
    herr_t                 status = FAIL;

    UNUSED(clss);

    if (persist)
        PIN_BOOL_ARRAY(ENVONLY, persist, persistArray, &isCopy, "H5Pget_file_space: persist not pinned");
    if (threshold)
        PIN_LONG_ARRAY(ENVONLY, threshold, thresholdArray, &isCopy, "H5Pget_file_space: threshold not pinned");

    if ((status = H5Pget_file_space_strategy((hid_t)fcpl_id, &thestrategy, (hbool_t *)persistArray, (hsize_t *)thresholdArray)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (thresholdArray)
        UNPIN_LONG_ARRAY(ENVONLY, threshold, thresholdArray, (status < 0) ? JNI_ABORT : 0);
    if (persistArray)
        UNPIN_BOOL_ARRAY(ENVONLY, persist, persistArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)thestrategy;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1strategy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space_strategy_persist
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1strategy_1persist
    (JNIEnv *env, jclass clss, jlong fcpl_id)
{
    hbool_t persist = FALSE;
    herr_t  status = FAIL;

    UNUSED(clss);

    if ((status = H5Pget_file_space_strategy((hid_t)fcpl_id, NULL, &persist, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jboolean)persist;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1strategy_1persist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_file_space_strategy_threshold
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1strategy_1threshold
    (JNIEnv *env, jclass clss, jlong fcpl_id)
{
    hsize_t threshold = 0;
    herr_t  status = FAIL;

    UNUSED(clss);

    if ((status = H5Pget_file_space_strategy((hid_t)fcpl_id, NULL, NULL, &threshold)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)threshold;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1file_1space_1threshold */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_nindexes
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1nindexes
    (JNIEnv *env, jclass clss, jlong plist_id, jint nindexes)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if (nindexes > H5O_SHMESG_MAX_NINDEXES)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_nindexes: number of indexes is greater than H5O_SHMESG_MAX_NINDEXES");

    if ((retVal = H5Pset_shared_mesg_nindexes((hid_t)plist_id, (unsigned)nindexes)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1nindexes */

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

    UNUSED(clss);

    if (H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)nindexes;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1nindexes */

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
    unsigned nindexes; /* Number of SOHM indexes */
    herr_t   retVal = FAIL;

    UNUSED(clss);

    /* Check arguments */
    if ((unsigned) mesg_type_flags > H5O_SHMESG_ALL_FLAG)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_index: unrecognized flags in mesg_type_flags");

    /* Read the current number of indexes */
    if (H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Range check */
    if ((unsigned) index_num >= nindexes)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_index: index_num is too large; no such index");

    if ((retVal = H5Pset_shared_mesg_index((hid_t)fcpl_id, (unsigned)index_num, (unsigned) mesg_type_flags, (unsigned) min_mesg_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    unsigned  nindexes; /* Number of SOHM indexes */
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == mesg_info)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_shared_mesg_index: mesg_info is NULL");

    /* Read the current number of indexes */
    if (H5Pget_shared_mesg_nindexes((hid_t)fcpl_id, &nindexes) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Range check */
    if ((unsigned) index_num >= nindexes)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_shared_mesg_index: index_num is too large; no such index");

    PIN_INT_ARRAY(ENVONLY, mesg_info, theArray, &isCopy, "H5Pget_shared_mesg_index: input not pinned");

    if ((retVal = H5Pget_shared_mesg_index((hid_t)fcpl_id, (unsigned)index_num, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, mesg_info, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1index */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shared_mesg_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1phase_1change
    (JNIEnv *env, jclass clss, jlong fcpl_id, jint max_list, jint min_btree)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    /* Check that values are sensible.  The min_btree value must be no greater
     * than the max list plus one.
     *
     * Range check to make certain they will fit into encoded form.
     */

    if (max_list + 1 < min_btree)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_phase_change: minimum B-tree value is greater than maximum list value");
    if (max_list > H5O_SHMESG_MAX_LIST_SIZE)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_phase_change: max list value is larger than H5O_SHMESG_MAX_LIST_SIZE");
    if (min_btree > H5O_SHMESG_MAX_LIST_SIZE)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_shared_mesg_phase_change: min btree value is larger than H5O_SHMESG_MAX_LIST_SIZE");

    if ((retVal = H5Pset_shared_mesg_phase_change((hid_t)fcpl_id, (unsigned)max_list, (unsigned)min_btree)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shared_1mesg_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_shared_mesg_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1phase_1change
    (JNIEnv *env, jclass clss, jlong fcpl_id, jintArray size)
{
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_shared_mesg_phase_change: size is NULL");

    PIN_INT_ARRAY(ENVONLY, size, theArray, &isCopy, "H5Pget_shared_mesg_phase_change: input not pinned");

    if ((retVal = H5Pget_shared_mesg_phase_change((hid_t)fcpl_id, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, size, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1shared_1mesg_1phase_1change */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
