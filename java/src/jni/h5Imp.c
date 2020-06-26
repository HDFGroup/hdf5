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

/*
 *  This code is the C-interface called by Java programs to access the
 *  general library functions of the HDF5 library.
 *
 *  Each routine wraps a single HDF entry point, generally with the
 *  analogous arguments and return codes.
 *
 */

#include "hdf5.h"
#include <jni.h>
#include "h5jni.h"
#include "h5Imp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5open
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5open
    (JNIEnv *env, jclass clss)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5open()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5open */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5close
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5close
    (JNIEnv *env, jclass clss)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5close()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5close */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5dont_atexit
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5dont_1atexit
    (JNIEnv *env, jclass clss)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5dont_atexit()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5dont_1atexit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5get_libversion
 * Signature: ([I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5get_1libversion
    (JNIEnv *env, jclass clss, jintArray libversion)
{
    jboolean  libversionArrayIsCopy;
    int      *libversionArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (libversion == NULL)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5get_libversion: libversion is NULL");

    PIN_INT_ARRAY(ENVONLY, libversion, libversionArray, &libversionArrayIsCopy, "H5get_libversion: libversion input not pinned");

    if ((status = H5get_libversion((unsigned *) &(libversionArray[0]), (unsigned *) &(libversionArray[1]), (unsigned *) &(libversionArray[2]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (libversionArray)
        UNPIN_INT_ARRAY(ENVONLY, libversion, libversionArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5get_1libversion */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5check_version
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5check_1version
    (JNIEnv *env, jclass clss, jint majnum, jint minnum, jint relnum)
{
    UNUSED(env);
    UNUSED(clss);

    return (jint)H5check_version((unsigned)majnum, (unsigned)minnum, (unsigned)relnum);
} /* end Java_hdf_hdf5lib_H5_H5check_1version */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5garbage_collect
 * Signature: ()I
 *
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5garbage_1collect
    (JNIEnv *env, jclass clss)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5garbage_collect()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5garbage_1collect */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5set_free_list_limits
 * Signature: (IIIIII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5set_1free_1list_1limits
    (JNIEnv *env, jclass clss, jint reg_global_lim, jint reg_list_lim,
        jint arr_global_lim, jint arr_list_lim, jint blk_global_lim, jint blk_list_lim )
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5set_free_list_limits((int)reg_global_lim, (int)reg_list_lim,
            (int)arr_global_lim, (int)arr_list_lim, (int)blk_global_lim, (int)blk_list_lim)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5set_1free_1list_1limits */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5is_library_threadsafe
 * Signature: ()Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5is_1library_1threadsafe
    (JNIEnv *env, jclass clss)
{
    hbool_t is_ts = false;

    UNUSED(clss);

    if (H5is_library_threadsafe(&is_ts) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jboolean)is_ts;
} /* end Java_hdf_hdf5lib_H5_H5is_1library_1threadsafe */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
