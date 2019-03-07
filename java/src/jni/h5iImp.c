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
#include <jni.h>
#include "h5jni.h"
#include "h5iImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_type
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1type
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    H5I_type_t retVal = H5I_BADID;

    UNUSED(clss);

    if (H5I_BADID == (retVal = H5Iget_type((hid_t)obj_id)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_name_long
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1name_1long
    (JNIEnv *env, jclass clss, jlong obj_id, jobjectArray name, jlong buf_size)
{
    ssize_t  size = -1;
    jstring  str;
    char    *aName = NULL;

    UNUSED(clss);

    if (buf_size < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Iget_name_long: buf_size < 0");

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Iget_name_long: malloc failed");

    if ((size = H5Iget_name((hid_t)obj_id, aName, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    aName[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (aName)
        HDfree(aName);

    return (jlong)size;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1name */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1name
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    jstring  str = NULL;
    ssize_t  buf_size = -1;
    char    *aName = NULL;

    UNUSED(clss);

    /* Get the length of the name */
    if ((buf_size = H5Iget_name((hid_t)obj_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Iget_name: malloc failed");

    if (H5Iget_name((hid_t)obj_id, aName, (size_t)buf_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    aName[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (aName)
        HDfree(aName);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1ref
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Iget_ref((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iinc_ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iinc_1ref
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Iinc_ref((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Iinc_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idec_1ref
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Idec_1ref
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Idec_ref((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Idec_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_file_id
 * Signature: (J)J
 */

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1file_1id
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    hid_t file_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((file_id = H5Iget_file_id((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) file_id;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1file_1id */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iget_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iget_1type_1ref
    (JNIEnv *env, jclass clss, jint type)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Iget_type_ref((H5I_type_t)type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Iget_1type_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idec_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Idec_1type_1ref
    (JNIEnv *env, jclass clss, jint type)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Idec_type_ref((H5I_type_t)type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Idec_1type_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iinc_type_ref
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Iinc_1type_1ref
    (JNIEnv *env, jclass clss, jint type)
{
    int retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Iinc_type_ref((H5I_type_t)type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Iinc_1type_1ref */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Inmembers
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Inmembers
    (JNIEnv *env, jclass clss, jint type)
{
    hsize_t num_members = 0;

    UNUSED(clss);

    if (H5Inmembers((H5I_type_t)type, &num_members) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)num_members;
} /* end Java_hdf_hdf5lib_H5_H5Inmembers */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iis_valid
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Iis_1valid
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Iis_valid((hid_t)obj_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Iis_1valid */
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Itype_exists
 * Signature: (I)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Itype_1exists
    (JNIEnv *env, jclass clss, jint type)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Itype_exists((H5I_type_t)type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end else Java_hdf_hdf5lib_H5_H5Itype_1exists */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Iclear_type
 * Signature: (IZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Iclear_1type
    (JNIEnv *env, jclass clss, jint type, jboolean force)
{
    UNUSED(clss);

    if (H5Iclear_type((H5I_type_t)type, (hbool_t)force) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Iclear_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Idestroy_type
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Idestroy_1type
    (JNIEnv *env, jclass clss, jint type)
{
    UNUSED(clss);

    if (H5Idestroy_type((H5I_type_t)type) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Idestroy_1type */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
