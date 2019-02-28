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

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5PLset_loading_state
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5PLset_1loading_1state
    (JNIEnv *env, jclass clss, jint plugin_flags)
{
    UNUSED(clss);

    if (H5PLset_loading_state((unsigned int)plugin_flags) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
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

    UNUSED(clss);

    if (H5PLget_loading_state(&plugin_type) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
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
    const char *newPath = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == plugin_path)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5PLappend: new path is NULL");

    PIN_JAVA_STRING(ENVONLY, plugin_path, newPath, NULL, "H5PLappend: new path not pinned");

    if ((retVal = H5PLappend(newPath)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newPath)
        UNPIN_JAVA_STRING(ENVONLY, plugin_path, newPath);
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
    const char *newPath = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == plugin_path)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5PLprepend: new path is NULL");

    PIN_JAVA_STRING(ENVONLY, plugin_path, newPath, NULL, "H5PLprepend: new path not pinned");

    if ((retVal = H5PLprepend(newPath)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newPath)
        UNPIN_JAVA_STRING(ENVONLY, plugin_path, newPath);
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
    const char *newPath = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == plugin_path)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5PLreplace: new path is NULL");

    if (index < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5PLreplace: index < 0");

    PIN_JAVA_STRING(ENVONLY, plugin_path, newPath, NULL, "H5PLreplace: new path not pinned");

    if ((retVal = H5PLreplace(newPath, (unsigned) index)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newPath)
        UNPIN_JAVA_STRING(ENVONLY, plugin_path, newPath);
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
    const char *newPath = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == plugin_path)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5PLinsert: new path is NULL");

    if (index < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5PLinsert: index < 0");

    PIN_JAVA_STRING(ENVONLY, plugin_path, newPath, NULL, "H5PLinsert: new path not pinned");

    if ((retVal = H5PLinsert(newPath, (unsigned) index)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (newPath)
        UNPIN_JAVA_STRING(ENVONLY, plugin_path, newPath);
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
    UNUSED(clss);

    if (index < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5PLremove: index < 0");

    if (H5PLremove((unsigned) index) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
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
    jstring  str = NULL;
    ssize_t  buf_size;
    char    *aName = NULL;

    UNUSED(clss);

    if (index < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5PLget: index < 0");

    /* Get the length of the name */
    if ((buf_size = H5PLget((unsigned) index, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5PLget: failed to allocate plugin name buffer");

    if ((H5PLget((unsigned) index, aName, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    aName[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (aName)
        HDfree(aName);

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

    UNUSED(clss);

    if (H5PLsize(&listsize) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)listsize;
} /* end Java_hdf_hdf5lib_H5_H5PLsize */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
