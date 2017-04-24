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

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLappend
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLappend
  (JNIEnv *env, jclass clss, jobjectArray plugin_path)
{
    char *aName;
    herr_t retVal = -1;

    PIN_JAVA_STRING(plugin_path, aName);
    if (aName != NULL) {
        retVal = H5PLappend(aName);

        UNPIN_JAVA_STRING(plugin_path, aName);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5PLappend */
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLprepend
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLprepend
  (JNIEnv *env, jclass clss, jobjectArray plugin_path)
{
    char *aName;
    herr_t retVal = -1;

    PIN_JAVA_STRING(plugin_path, aName);
    if (aName != NULL) {
        retVal = H5PLprepend(aName);

        UNPIN_JAVA_STRING(plugin_path, aName);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5PLprepend */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLreplace
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLreplace
  (JNIEnv *env, jclass clss, jobjectArray plugin_path, jint index)
{
    char *aName;
    herr_t retVal = -1;

    PIN_JAVA_STRING(plugin_path, aName);
    if (aName != NULL) {
        retVal = H5PLreplace(aName, index);

        UNPIN_JAVA_STRING(plugin_path, aName);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5PLreplace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLinsert
 * Signature: (Ljava/lang/String;I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLinsert
  (JNIEnv *env, jclass clss, jobjectArray plugin_path, jint index)
{
    char *aName;
    herr_t retVal = -1;

    PIN_JAVA_STRING(plugin_path, aName);
    if (aName != NULL) {
        retVal = H5PLinsert(aName, index);

        UNPIN_JAVA_STRING(plugin_path, aName);

        if (retVal < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5PLinsert */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLremove
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLremove
  (JNIEnv *env, jclass clss, jint index)
{
    if (H5PLremove(index) < 0)
      h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5PLremove */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLget
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5PLget
  (JNIEnv *env, jclass clss, jint index)
{
    char *aName;
    jstring  str = NULL;
    ssize_t  buf_size;

    /* get the length of the name */
    buf_size = H5PLget(index, NULL, 0);

    if (buf_size <= 0) {
        h5badArgument(env, "H5PLget:  buf_size <= 0");
    } /* end if */
    else {
        buf_size++; /* add extra space for the null terminator */
        aName = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (aName == NULL) {
            h5outOfMemory(env, "H5PLget:  malloc failed");
        } /* end if */
        else {
            buf_size = H5PLget(index, aName, (size_t)buf_size);
            if (buf_size < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                str = ENVPTR->NewStringUTF(ENVPAR aName);
            }
            HDfree(aName);
        }
    }
    return str;
} /* end Java_hdf_hdf5lib_H5_H5PLget */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLsize
 * Signature: (V)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5PLsize
  (JNIEnv *env, jclass clss)
{
    unsigned int listsize = 0;
    if (H5PLsize(&listsize) < 0) {
        h5libraryError(env);
    }
    return (jint)listsize;
} /* end Java_hdf_hdf5lib_H5_H5PLsize */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
