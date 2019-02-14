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
#include "h5pOCpyPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_copy_object
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1copy_1object
    (JNIEnv *env, jclass clss, jlong ocp_plist_id, jint copy_options)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_copy_object((hid_t)ocp_plist_id, (unsigned)copy_options)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
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
    unsigned copy_options = 0;

    UNUSED(clss);

    if (H5Pget_copy_object((hid_t)ocp_plist_id, &copy_options) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)copy_options;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1copy_1object */

/*
 * TODO: H5Padd_merge_committed_dtype_path
 */

/*
 * TODO: H5Pfree_merge_committed_dtype_paths
 */

/*
 * TODO: H5Pset_mcdt_search_cb
 */

/*
 * TODO: H5Pget_mcdt_search_cb
 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
