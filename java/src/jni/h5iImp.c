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
#include <jni.h>
#include "h5jni.h"
#include "h5iImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

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

    retVal =  H5Iget_type((hid_t)obj_id);
    if (retVal == H5I_BADID)
        h5libraryError(env);

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
    char *aName;
    jstring str;
    hssize_t size = -1;
    long bs;

    bs = (long)buf_size;
    if (bs <= 0) {
        h5badArgument(env, "H5Iget_name:  buf_size <= 0");
    } /* end if */
    else {
        aName = (char*)HDmalloc(sizeof(char) * (size_t)bs);
        if (aName == NULL) {
            h5outOfMemory(env, "H5Iget_name:  malloc failed");
        } /* end if */
        else {
            size = H5Iget_name((hid_t)obj_id, aName, (size_t)buf_size);
            if (size < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                str = ENVPTR->NewStringUTF(ENVPAR aName);
                ENVPTR->SetObjectArrayElement(ENVPAR name, 0, str);
            }
            HDfree(aName);
        }
    }
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
    char *aName;
    jstring  str = NULL;
    ssize_t  buf_size;

    /* get the length of the name */
    buf_size = H5Iget_name((hid_t)obj_id, NULL, 0);

    if (buf_size <= 0) {
        h5badArgument(env, "H5Iget_name:  buf_size <= 0");
    } /* end if */
    else {
        buf_size++; /* add extra space for the null terminator */
        aName = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (aName == NULL) {
            h5outOfMemory(env, "H5Iget_name:  malloc failed");
        } /* end if */
        else {
            buf_size = H5Iget_name((hid_t)obj_id, aName, (size_t)buf_size);
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
    int retVal = -1;

    retVal = H5Iget_ref((hid_t)obj_id);
    if (retVal < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Iinc_ref((hid_t)obj_id);
    if (retVal < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Idec_ref((hid_t)obj_id);
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t file_id = -1;

    file_id = H5Iget_file_id((hid_t)obj_id);
    if (file_id < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Iget_type_ref((H5I_type_t)type);
    if (retVal < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Idec_type_ref((H5I_type_t)type);
    if (retVal < 0)
        h5libraryError(env);

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
    int retVal = -1;

    retVal = H5Iinc_type_ref((H5I_type_t)type);
    if (retVal < 0)
        h5libraryError(env);

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
    hsize_t num_members;

    if (H5Inmembers((H5I_type_t)type, &num_members) < 0)
        h5libraryError(env);

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

    bval = H5Iis_valid((hid_t)obj_id);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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

    bval = H5Itype_exists((H5I_type_t)type);
    if (bval > 0)
        bval = JNI_TRUE;
    else if (bval < 0)
        h5libraryError(env);

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
     if (H5Iclear_type((H5I_type_t)type, (hbool_t)force) < 0)
       h5libraryError(env);
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
     if (H5Idestroy_type((H5I_type_t)type) < 0)
       h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Idestroy_1type */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
