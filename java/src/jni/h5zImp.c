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
    herr_t retValue = H5Zunregister((H5Z_filter_t)filter);

    if (retValue < 0)
        h5libraryError(env);

    return (jint)retValue;
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
    herr_t retValue = H5Zfilter_avail((H5Z_filter_t)filter);

    if (retValue < 0)
        h5libraryError(env);

    return (jint)retValue;
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

    if (H5Zget_filter_info ((H5Z_filter_t) filter, (unsigned *) &flags) < 0)
        h5libraryError(env);

    return (jint)flags;
} /* end Java_hdf_hdf5lib_H5_H5Zget_1filter_1info */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
