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
#include "h5pGCPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_local_heap_size_hint
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1local_1heap_1size_1hint
    (JNIEnv *env, jclass clss, jlong gcpl_id, jlong size_hint)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_local_heap_size_hint((hid_t)gcpl_id, (size_t)size_hint)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    size_t size_hint = 0;

    UNUSED(clss);

    if (H5Pget_local_heap_size_hint((hid_t)gcpl_id, &size_hint) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)size_hint;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1local_1heap_1size_1hint */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint crt_order_flags)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_link_creation_order((hid_t)gcpl_id, (unsigned)crt_order_flags)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1link_1creation_1order */

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

    UNUSED(clss);

    if (H5Pget_link_creation_order((hid_t)gcpl_id, &crt_order_flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)crt_order_flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1link_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_est_link_info
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1est_1link_1info
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint est_num_entries, jint est_name_len)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    /* Range check values */
    if ((est_num_entries > 65535) || (est_name_len > 65535))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_est_link_info: est. name length or number of entries must be < 65536");

    if ((retVal = H5Pset_est_link_info((hid_t)gcpl_id, (unsigned)est_num_entries, (unsigned)est_name_len)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == link_info)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_est_link_info: link_info is NULL");

    PIN_INT_ARRAY(ENVONLY, link_info, theArray, &isCopy, "H5Pget_est_link_info: input not pinned");

    if ((retVal = H5Pget_est_link_info((hid_t)gcpl_id, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, link_info, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1est_1link_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_link_phase_change
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1link_1phase_1change
    (JNIEnv *env, jclass clss, jlong gcpl_id, jint max_compact, jint min_dense)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if (max_compact < min_dense)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_link_phase_change: max compact value must be >= min dense value");
    if (max_compact > 65535)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_link_phase_change: max compact value must be < 65536");
    if (min_dense > 65535)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_link_phase_change: min dense value must be < 65536");

    if ((retVal = H5Pset_link_phase_change((hid_t)gcpl_id, (unsigned)max_compact, (unsigned)min_dense)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == links)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_link_phase_change: links is NULL");

    PIN_INT_ARRAY(ENVONLY, links, theArray, &isCopy, "H5Pget_link_phase_change: input not pinned");

    if ((retVal = H5Pget_link_phase_change((hid_t)gcpl_id, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, links, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1link_1phase_1change */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
