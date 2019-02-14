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
#include "h5pStrCPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_char_encoding
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding
    (JNIEnv *env, jclass clss, jlong acpl, jint encoding)
{
    UNUSED(clss);

    if (H5Pset_char_encoding((hid_t)acpl, (H5T_cset_t)encoding) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1char_1encoding */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_char_encoding
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding
    (JNIEnv *env, jclass clss, jlong acpl)
{
    H5T_cset_t encoding = H5T_CSET_ERROR;

    UNUSED(clss);

    if (H5Pget_char_encoding((hid_t)acpl, &encoding) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return encoding;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1char_1encoding */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
