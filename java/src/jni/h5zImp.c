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

#include "hdf5.h"
#include <jni.h>
#include <stdlib.h>
#include "h5jni.h"
#include "h5zImp.h"

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zunregister
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zunregister
    (JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zunregister((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zunregister */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zfilter_avail
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zfilter_1avail
    (JNIEnv *env, jclass clss, jint filter)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Zfilter_avail((H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Zfilter_1avail */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Zget_filter_info
 * Signature: (I)I
 */

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Zget_1filter_1info
    (JNIEnv *env, jclass clss, jint filter)
{
    unsigned int flags = 0;

    UNUSED(clss);

    if (H5Zget_filter_info((H5Z_filter_t) filter, (unsigned *) &flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Zget_1filter_1info */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
