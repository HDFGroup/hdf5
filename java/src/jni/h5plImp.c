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
#include <stdlib.h>
#include "h5jni.h"
#include "h5plImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLset_loading_state
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLset_1loading_1state
    (JNIEnv *env, jclass clss, jint plugin_flags)
{
    if (H5PLset_loading_state((unsigned int)plugin_flags) < 0) {
        h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5PLset_1loading_1state */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLget_loading_state
 * Signature: (V)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5PLget_1loading_1state
    (JNIEnv *env, jclass clss)
{
    unsigned int plugin_type = 0;
    if (H5PLget_loading_state(&plugin_type) < 0) {
        h5libraryError(env);
    }
    return (jint)plugin_type;
} /* end Java_hdf_hdf5lib_H5_H5PLget_1loading_1state */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
