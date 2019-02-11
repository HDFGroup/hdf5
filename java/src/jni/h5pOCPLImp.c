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
#include "h5pOCPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_create_intermediate_group
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1create_1intermediate_1group
    (JNIEnv *env, jclass clss, jlong lcpl_id, jboolean crt_intermed_group)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_create_intermediate_group((hid_t)lcpl_id, (unsigned)crt_intermed_group)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    unsigned crt_intermed_group = 0;

    UNUSED(clss);

    if (H5Pget_create_intermediate_group((hid_t)lcpl_id, &crt_intermed_group) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jboolean)crt_intermed_group;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1create_1intermediate_1group */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_obj_track_times
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1obj_1track_1times
    (JNIEnv *env, jclass clss, jlong objplid, jboolean track_times)
{
    hbool_t track = FALSE;

    UNUSED(clss);

    track = (JNI_TRUE == track_times) ? 1 : 0;

    if (H5Pset_obj_track_times((hid_t)objplid, track) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1obj_1track_1times */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_obj_track_times
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1obj_1track_1times
    (JNIEnv *env, jclass clss, jlong objplid)
{
    hbool_t  track_times = FALSE;
    jboolean retVal = JNI_FALSE;

    UNUSED(clss);

    if (H5Pget_obj_track_times((hid_t)objplid, &track_times) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    retVal = (track_times == TRUE) ? JNI_TRUE : JNI_FALSE;

done:
    return retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1obj_1track_1times */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_phase_change
 * Signature: (JII)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1phase_1change
    (JNIEnv *env, jclass clss, jlong ocpl_id, jint max_compact, jint min_dense)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_attr_phase_change((hid_t)ocpl_id, (unsigned)max_compact, (unsigned)min_dense)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1attr_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_phase_change
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1phase_1change
    (JNIEnv *env, jclass clss, jlong ocpl_id, jintArray attributes)
{
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == attributes)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_attr_phase_change: attributes is NULL");

    PIN_INT_ARRAY(ENVONLY, attributes, theArray, &isCopy, "H5Pget_attr_phase_change: input not pinned");

    if ((retVal = H5Pget_attr_phase_change((hid_t)ocpl_id, (unsigned *)&(theArray[0]), (unsigned *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, attributes, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1attr_1phase_1change */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_attr_creation_order
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1attr_1creation_1order
    (JNIEnv *env, jclass clss, jlong ocpl_id, jint crt_order_flags)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_attr_creation_order((hid_t)ocpl_id, (unsigned)crt_order_flags)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1attr_1creation_1order */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_attr_creation_order
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1attr_1creation_1order
    (JNIEnv *env, jclass clss, jlong ocpl_id)
{
    unsigned crt_order_flags = 0;

    UNUSED(clss);

    if (H5Pget_attr_creation_order((hid_t)ocpl_id, &crt_order_flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)crt_order_flags;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1attr_1creation_1order */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
