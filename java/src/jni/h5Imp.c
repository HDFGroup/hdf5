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

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5open
 * Signature: ()I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5open
    (JNIEnv *env, jclass clss)
{
    herr_t retVal = H5open();
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = H5close();
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = H5dont_atexit();
    if (retVal < 0)
        h5libraryError(env);

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
    unsigned *theArray = NULL;
    herr_t    status = -1;
    jboolean  isCopy;

    if (libversion == NULL) {
        h5nullArgument(env, "H5get_version:  libversion is NULL");
    } /* end if */
    else {
        theArray = (unsigned*)ENVPTR->GetIntArrayElements(ENVPAR libversion, &isCopy);
        if (theArray == NULL) {
            h5JNIFatalError( env, "H5get_libversion:  input not pinned");
        } /* end if */
        else {
            status = H5get_libversion(&(theArray[0]), &(theArray[1]), &(theArray[2]));

            if (status < 0) {
                ENVPTR->ReleaseIntArrayElements(ENVPAR libversion, (jint*)theArray, JNI_ABORT);
                h5libraryError(env);
            } /* end if */
            ENVPTR->ReleaseIntArrayElements(ENVPAR libversion, (jint*)theArray,0);
        } /* end else */
    } /* end else */
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
    herr_t retVal = H5garbage_collect();
    if (retVal < 0)
        h5libraryError(env);

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
    herr_t retVal = H5set_free_list_limits((int)reg_global_lim, (int)reg_list_lim,
        (int)arr_global_lim, (int)arr_list_lim, (int)blk_global_lim, (int)blk_list_lim);
    if (retVal < 0)
        h5libraryError(env);

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
    H5is_library_threadsafe(&is_ts);
    return (jboolean)is_ts;
} /* end Java_hdf_hdf5lib_H5_H5is_1library_1threadsafe */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
