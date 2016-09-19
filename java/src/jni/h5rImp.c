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
#include "h5rImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rcreate
 * Signature: ([BJLjava/lang/String;IJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rcreate
    (JNIEnv *env, jclass clss, jbyteArray ref, jlong loc_id, jstring name, jint ref_type, jlong space_id)
{
    const char *rName;
    herr_t      status = -1;
    jbyte      *refP;
    jboolean    isCopy2;

    PIN_JAVA_STRING(name, rName);
    if (rName != NULL) {
        if (ref == NULL) {
            h5nullArgument( env, "H5Rcreate:  ref is NULL");
        } /* end if */
        else {
            if ((ref_type == H5R_OBJECT) && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_OBJ_REF_BUF_SIZE) {
                h5badArgument( env, "H5Rcreate:  ref input array != H5R_OBJ_REF_BUF_SIZE");
            } /* end if */
            else if ((ref_type == H5R_DATASET_REGION) && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_DSET_REG_REF_BUF_SIZE) {
                h5badArgument( env, "H5Rcreate:  region ref input array != H5R_DSET_REG_REF_BUF_SIZE");
            } /* end else if */
            else if ((ref_type != H5R_OBJECT) && (ref_type != H5R_DATASET_REGION)) {
                h5badArgument( env, "H5Rcreate:  ref_type unknown type ");
            } /* end else if */
            else {
                refP = (jbyte*)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy2);
                if (refP == NULL) {
                    h5JNIFatalError(env,  "H5Rcreate:  ref not pinned");
                } /* end if */
                else {
                    status = H5Rcreate(refP, (hid_t)loc_id, rName, (H5R_type_t)ref_type, (hid_t)space_id);

                    if (status < 0) {
                        ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, 0);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, rName);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Rcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Rdereference
 * Signature: (JJI[B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Rdereference
    (JNIEnv *env, jclass clss, jlong dataset, jlong access_list, jint ref_type, jbyteArray ref)
{
    jboolean isCopy;
    jbyte   *refP;
    hid_t    retVal = -1;

    if (ref == NULL) {
        h5nullArgument( env, "H5Rdereference:  ref is NULL");
    } /* end if */
    else if ((ref_type == H5R_OBJECT) && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_OBJ_REF_BUF_SIZE) {
        h5badArgument( env, "H5Rdereference:  obj ref input array != H5R_OBJ_REF_BUF_SIZE");
    } /* end else if */
    else if ((ref_type == H5R_DATASET_REGION) && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_DSET_REG_REF_BUF_SIZE) {
        h5badArgument( env, "H5Rdereference:  region ref input array != H5R_DSET_REG_REF_BUF_SIZE");
    } /* end else if */
    else {
        refP = (jbyte*)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy);
        if (refP == NULL) {
            h5JNIFatalError(env,  "H5Rderefernce:  ref not pinned");
        } /* end if */
        else {
            retVal = H5Rdereference2((hid_t)dataset, (hid_t)access_list, (H5R_type_t)ref_type, refP);

            ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);

            if (retVal < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Rdereference */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_region
 * Signature: (JI[B)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Rget_1region
    (JNIEnv *env, jclass clss, jlong dataset, jint ref_type, jbyteArray ref)
{
    hid_t    retVal = -1;
    jboolean isCopy;
    jbyte   *refP;

    if (ref_type != H5R_DATASET_REGION)  {
        h5badArgument( env, "H5Rget_region:  bad ref_type ");
    } /* end if */
    else if (ref == NULL) {
        h5nullArgument( env, "H5Rget_region:  ref is NULL");
    } /* end if */
    else if ( ENVPTR->GetArrayLength(ENVPAR ref) != H5R_DSET_REG_REF_BUF_SIZE) {
        h5badArgument( env, "H5Rget_region:  region ref input array != H5R_DSET_REG_REF_BUF_SIZE");
    } /* end if */
    else {
        refP = (jbyte*)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy);
        if (refP == NULL) {
            h5JNIFatalError(env,  "H5Rget_region:  ref not pinned");
        } /* end if */
        else {
            retVal = H5Rget_region((hid_t)dataset, (H5R_type_t)ref_type, refP);

            ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);

            if (retVal < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Rget_1region */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5G_obj_t H5Rget_obj_type
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1type
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref)
{
    int      retVal =-1;
    jboolean isCopy;
    jbyte   *refP;
    H5O_type_t object_info;


    if (ref == NULL) {
        h5nullArgument( env, "H5Rget_object_type:  ref is NULL");
    } /* end if */
    else {
        refP = (jbyte*)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy);
        if (refP == NULL) {
            h5JNIFatalError(env,  "H5Rget_object_type:  ref not pinned");
        } /* end if */
        else {
            retVal = H5Rget_obj_type2((hid_t)loc_id, (H5R_type_t)ref_type, refP, &object_info);
            if(retVal >= 0)
                retVal = object_info;

            ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);

            if (retVal < 0)
                h5libraryError(env);
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1type */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    int H5Rget_obj_type2
 * Signature: (JI[B[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1obj_1type2
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref, jintArray ref_obj)
{

    jint     status;
    jboolean isCopy;
    jbyte   *refP;
    jint    *ref_objP;
    int      retVal = -1;


    if (ref == NULL) {
        h5nullArgument( env, "H5Rget_object_type:  ref is NULL");
    } /* end if */
    else if (ref_obj == NULL) {
        h5nullArgument( env, "H5Rget_object_type:  ref_obj is NULL");
    } /* end else if */
    else {
        refP = (jbyte *)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy);
        if (refP == NULL) {
            h5JNIFatalError(env,  "H5Rget_object_type:  ref not pinned");
        } /* end if */
        else {
            ref_objP = (jint *)ENVPTR->GetIntArrayElements(ENVPAR ref_obj, &isCopy);
            if (ref_objP == NULL) {
                ENVPTR->ReleaseByteArrayElements(ENVPAR ref,refP,0);
                h5JNIFatalError(env,  "H5Rget_object_type:  ref_obj not pinned");
            } /* end if */
            else {
                status = H5Rget_obj_type2((hid_t)loc_id, (H5R_type_t)ref_type, refP, (H5O_type_t*)ref_objP);
                retVal = ref_objP[0];

                ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);
                if (status < 0) {
                    ENVPTR->ReleaseIntArrayElements(ENVPAR ref_obj,ref_objP, JNI_ABORT);
                    h5libraryError(env);
                } /* end if */
                else {
                    ENVPTR->ReleaseIntArrayElements(ENVPAR ref_obj, ref_objP, 0);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1obj_1type2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Rget_name
 * Signature: (JI[B[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Rget_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jint ref_type, jbyteArray ref, jobjectArray name, jlong size)
{
    jlong    ret_val = -1;
    jbyte   *refP;
    jboolean isCopy;
    char    *aName = NULL;
    jstring  str;
    size_t   bs;

    bs = (size_t)size;
    if (bs <= 0) {
        h5badArgument(env, "H5Rget_name:  size <= 0");
    } /* end if */
    else if (ref == NULL) {
        h5nullArgument(env, "H5Rget_name:  ref is NULL");
    } /* end else if */
    else {
        if ((ref_type == H5R_OBJECT) && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_OBJ_REF_BUF_SIZE) {
            h5badArgument(env, "H5Rget_name:  obj ref input array != H5R_OBJ_REF_BUF_SIZE");
        } /* end if */
        else if ((ref_type == H5R_DATASET_REGION)
                && ENVPTR->GetArrayLength(ENVPAR ref) != H5R_DSET_REG_REF_BUF_SIZE) {
            h5badArgument(env, "H5Rget_name:  region ref input array != H5R_DSET_REG_REF_BUF_SIZE");
        } /* end else if */
        else {
            refP = (jbyte *)ENVPTR->GetByteArrayElements(ENVPAR ref, &isCopy);
            if (refP == NULL) {
                h5JNIFatalError(env,  "H5Rget_name:  ref not pinned");
            } /* end if */
            else {
                aName = (char*)HDmalloc(sizeof(char)*bs);
                if (aName == NULL) {
                    ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);
                    h5outOfMemory(env, "H5Rget_name:  malloc failed");
                } /* end if */
                else {
                    ret_val = (jlong)H5Rget_name((hid_t)loc_id, (H5R_type_t)ref_type, refP, aName, bs) ;

                    ENVPTR->ReleaseByteArrayElements(ENVPAR ref, refP, JNI_ABORT);
                    if (ret_val < 0) {
                        HDfree(aName);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        str = ENVPTR->NewStringUTF(ENVPAR aName);
                        ENVPTR->SetObjectArrayElement(ENVPAR name, 0, str);

                        HDfree(aName);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Rget_1name */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
