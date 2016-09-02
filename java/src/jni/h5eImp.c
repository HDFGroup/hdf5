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
 *  For details of the HDF libraries, see the HDF Documentation at:
 *   http://www.hdfgroup.org/HDF5/doc/
 *
 */

#include <jni.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5eImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

#ifdef __cplusplus
  #define CBENVPTR (cbenv)
  #define CBENVPAR
  #define JVMPTR (jvm)
  #define JVMPAR
  #define JVMPAR2
#else /* __cplusplus */
  #define CBENVPTR (*cbenv)
  #define CBENVPAR cbenv,
  #define JVMPTR (*jvm)
  #define JVMPAR jvm
  #define JVMPAR2 jvm,
#endif /* __cplusplus */

/********************/
/* Local Prototypes */
/********************/

static herr_t H5E_walk_cb(int nindx, const H5E_error2_t *info, void *op_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eauto_is_v2
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Eauto_1is_1v2
    (JNIEnv *env, jclass cls, jlong stk_id)
{
    unsigned int is_stack = 0;

    if (stk_id < 0) {
        h5badArgument(env, "H5Eauto_is_v2: invalid argument");
    } /* end if */
    else if (H5Eauto_is_v2((hid_t)stk_id, &is_stack) < 0)
        h5libraryError(env);

    return (jboolean)is_stack;
} /* end Java_hdf_hdf5lib_H5_H5Eauto_1is_1v2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eregister_class
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eregister_1class
    (JNIEnv *env, jclass cls, jstring cls_name, jstring lib_name, jstring version)
{
    hid_t       ret_val = -1;
    const char* the_cls_name;
    const char* the_lib_name;
    const char* the_version;

    PIN_JAVA_STRING_THREE(cls_name, the_cls_name, lib_name, the_lib_name, version, the_version);
    if (the_cls_name != NULL && the_lib_name != NULL && the_version != NULL) {
        ret_val = H5Eregister_class(the_cls_name, the_lib_name, the_version);

        UNPIN_JAVA_STRING_THREE(cls_name, the_cls_name, lib_name, the_lib_name, version, the_version);

        if (ret_val < 0)
            h5libraryError(env);
    }

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eregister_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eunregister_class
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eunregister_1class
    (JNIEnv *env, jclass cls, jlong cls_id)
{
    if (cls_id < 0) {
        h5badArgument(env, "H5Eunregister_class: invalid argument");
    } /* end if */
    else if (H5Eunregister_class((hid_t)cls_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Eunregister_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_msg
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1msg
    (JNIEnv *env, jclass cls, jlong err_id)
{
    if (err_id < 0) {
        h5badArgument(env, "H5Eclose_msg: invalid argument");
    } /* end if */
    else if (H5Eclose_msg((hid_t)err_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Eclose_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_msg
 * Signature: (JILjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1msg
    (JNIEnv *env, jclass cls, jlong err_id, jint msg_type, jstring err_msg)
{
    hid_t       ret_val = -1;
    const char *the_err_msg;
    H5E_type_t  error_msg_type = (H5E_type_t)msg_type;

    if (err_id < 0) {
        h5badArgument(env, "H5Ecreate_msg: invalid argument");
    } /* end if */
    else {
        PIN_JAVA_STRING(err_msg, the_err_msg);
        if (the_err_msg != NULL) {
            ret_val = H5Ecreate_msg((hid_t)err_id, error_msg_type, the_err_msg);

            UNPIN_JAVA_STRING(err_msg, the_err_msg);

            if (ret_val < 0)
                h5libraryError(env);
        }
    } /* end else */

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Ecreate_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1stack
    (JNIEnv *env, jclass cls)
{
    hid_t ret_val = -1;

    ret_val = H5Ecreate_stack();
    if (ret_val < 0)
        h5libraryError(env);

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Ecreate_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_current_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1current_1stack
    (JNIEnv *env, jclass cls)
{
    hid_t ret_val = -1;

    ret_val = H5Eget_current_stack();
    if (ret_val < 0)
        h5libraryError(env);

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1current_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1stack
    (JNIEnv *env, jclass cls, jlong stk_id)
{
    if (stk_id < 0) {
        h5badArgument(env, "H5Eclose_stack: invalid argument");
    } /* end if */
    else if (H5Eclose_stack((hid_t)stk_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Eclose_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eprint2
 * Signature: (JLjava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eprint2
    (JNIEnv *env, jclass cls, jlong stk_id, jobject stream_obj)
{
    herr_t ret_val = -1;

    if (stk_id < 0) {
        h5badArgument(env, "H5Eprint2: invalid argument");
    } /* end if */
    else {
        if(!stream_obj)
            ret_val = H5Eprint2((hid_t)stk_id, stdout);
        else
            ret_val = H5Eprint2((hid_t)stk_id, (FILE*)stream_obj);

        if (ret_val < 0)
            h5libraryError(env);
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Eprint2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1class_1name
    (JNIEnv *env, jclass cls, jlong cls_id)
{
    char   *namePtr;
    jstring str = NULL;
    ssize_t buf_size;

    if (cls_id < 0) {
        h5badArgument(env, "H5Eget_class_name: invalid argument");
    } /* end if */
    else {
        /* get the length of the name */
        buf_size = H5Eget_class_name((hid_t)cls_id, NULL, 0);

        if (buf_size < 0) {
            h5badArgument( env, "H5Eget_class_name:  buf_size < 0");
        } /* end if */
        else if (buf_size == 0) {
            h5badArgument( env, "H5Eget_class_name:  No class name");
        } /* end else if */
        else {
            buf_size++; /* add extra space for the null terminator */
            namePtr = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
            if (namePtr == NULL) {
                h5outOfMemory( env, "H5Eget_class_name:  malloc failed");
            } /* end if */
            else {
                buf_size = H5Eget_class_name((hid_t)cls_id, (char *)namePtr, (size_t)buf_size);

                if (buf_size < 0) {
                    HDfree(namePtr);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR namePtr);
                    HDfree(namePtr);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */
    return str;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1class_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eset_current_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eset_1current_1stack
    (JNIEnv *env, jclass cls, jlong stk_id)
{
    if (stk_id < 0) {
        h5badArgument(env, "H5Eset_current_stack: invalid argument");
    } /* end if */
    else if (H5Eset_current_stack((hid_t)stk_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Eset_1current_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epop
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epop
    (JNIEnv *env, jclass cls, jlong stk_id, jlong count)
{
    if (stk_id < 0) {
        h5badArgument(env, "H5Epop: invalid argument");
    } /* end if */
    else if (H5Epop((hid_t)stk_id, (size_t)count) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Epop */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epush2
 * Signature: (JLjava/lang/String;Ljava/lang/String;IJJJLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epush2
    (JNIEnv *env, jclass cls, jlong stk_id, jstring filename, jstring funcname,
        jint linenumber, jlong class_id, jlong major_id, jlong minor_id, jstring err_desc)
{
    herr_t      ret_val = -1;
    const char* fName;
    const char* fncName;
    const char* errMsg;

    if (stk_id < 0) {
        h5badArgument(env, "H5Epush: invalid argument");
    } /* end if */
    else if (class_id < 0) {
        h5badArgument(env, "H5Epush: invalid class_id argument");
    } /* end else if */
    else if (major_id < 0) {
        h5badArgument(env, "H5Epush: invalid major_id argument");
    } /* end else if */
    else if (minor_id < 0) {
        h5badArgument(env, "H5Epush: invalid minor_id argument");
    } /* end else if */
    else {
        PIN_JAVA_STRING_THREE(filename, fName, funcname, fncName, err_desc, errMsg);
        if (fName != NULL && fncName != NULL && errMsg != NULL) {
            ret_val = H5Epush2((hid_t)stk_id, fName, fncName, (unsigned)linenumber, (hid_t)class_id,
                    (hid_t)major_id, (hid_t)minor_id, errMsg);

            UNPIN_JAVA_STRING_THREE(filename, fName, funcname, fncName, err_desc, errMsg);

            if (ret_val < 0)
                h5libraryError(env);
        }
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Epush2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclear2
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclear2
    (JNIEnv *env, jclass cls, jlong stk_id)
{
    if (stk_id < 0) {
        h5badArgument(env, "H5Eclear2: invalid argument");
    } /* end if */
    else if (H5Eclear2((hid_t)stk_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Eclear2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_msg
 * Signature: (J[I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1msg
    (JNIEnv *env, jclass cls, jlong msg_id, jintArray error_msg_type_list)
{
    char      *namePtr;
    jstring    str = NULL;
    jboolean   isCopy;
    ssize_t    buf_size;
    jint      *theArray;
    H5E_type_t error_msg_type;

    if (msg_id < 0) {
        h5badArgument(env, "H5Eget_msg: invalid argument");
    } /* end if */
    else if (error_msg_type_list == NULL) {
        h5nullArgument(env, "H5Eget_msg:  error_msg_type_list is NULL");
    } /* end if */
    else {
        /* get the length of the name */
        buf_size = H5Eget_msg((hid_t)msg_id, NULL, NULL, 0);

        if ((buf_size < 0) || (buf_size == 0)) {
            h5JNIFatalError(env, "H5Eget_msg:  Invalid message");
        } /* end if */
        else {
            buf_size++; /* add extra space for the null terminator */
            namePtr = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
            if (namePtr == NULL) {
                h5outOfMemory(env, "H5Eget_msg:  malloc failed");
            } /* end if */
            else {
                theArray = (jint*)ENVPTR->GetIntArrayElements(ENVPAR error_msg_type_list, &isCopy);
                if (theArray == NULL) {
                    HDfree(namePtr);
                    h5JNIFatalError(env, "H5Eget_msg:  error_msg_type_list not pinned");
                } /* end if */
                else {
                    buf_size = H5Eget_msg((hid_t)msg_id, &error_msg_type, (char *)namePtr, (size_t)buf_size);

                    if (buf_size < 0) {
                        HDfree(namePtr);
                        ENVPTR->ReleaseIntArrayElements(ENVPAR error_msg_type_list, theArray, JNI_ABORT);
                        h5libraryError(env);
                    } /* end if */
                    else {
                        theArray[0] = error_msg_type;
                        ENVPTR->ReleaseIntArrayElements(ENVPAR error_msg_type_list, theArray, 0);

                        str = ENVPTR->NewStringUTF(ENVPAR namePtr);
                        HDfree(namePtr);
                    } /* end else */
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_num
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1num
    (JNIEnv *env, jclass cls, jlong stk_id)
{
    ssize_t ret_val = -1;

    if (stk_id < 0) {
        h5badArgument(env, "H5Eget_num: invalid argument");
    } /* end if */
    else {
        ret_val = H5Eget_num((hid_t)stk_id);
        if (ret_val < 0)
            h5libraryError(env);
    } /* end else */
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1num */

static herr_t
H5E_walk_cb
    (int nindx, const H5E_error2_t *info, void *op_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str1, str2, str3;
    jmethodID  constructor;
    jvalue     args[7];
    jobject    cb_info_t = NULL;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(ILhdf/hdf5lib/structs/H5E_error2_t;Lhdf/hdf5lib/callbacks/H5E_walk_t;)I");
            if (mid != 0) {
                args[0].j = info->cls_id;
                args[1].j = info->maj_num;
                args[2].j = info->min_num;
                args[3].i = (jint)info->line;
                str1 = CBENVPTR->NewStringUTF(CBENVPAR info->func_name);
                args[4].l = str1;
                str2 = CBENVPTR->NewStringUTF(CBENVPAR info->file_name);
                args[5].l = str2;
                str3 = CBENVPTR->NewStringUTF(CBENVPAR info->desc);
                args[6].l = str3;
                // get a reference to your class if you don't have it already
                cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5E_error2_t");
                if (cls != 0) {
                    // get a reference to the constructor; the name is <init>
                    constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(JJJILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V");
                    if (constructor != 0) {
                        cb_info_t = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);

                        status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, nindx, cb_info_t, op_data);
                    } /* end if (constructor != 0) */
                } /* end if(cls != 0) */
            } /* end if (mid != 0) */
        } /* end if (cls != 0) */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return (herr_t)status;
} /* end H5E_walk_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ewalk2
 * Signature: (JJLjava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ewalk2
    (JNIEnv *env, jclass cls, jlong stk_id, jlong direction, jobject callback_op, jobject op_data)
{
    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Ewalk2:  op_data or callback_op is NULL");
    } /* end if */
    else if (H5Ewalk2(stk_id, (H5E_direction_t)direction, (H5E_walk2_t)H5E_walk_cb, (void*)op_data) < 0)
            h5libraryError(env);
} /* end iJava_hdf_hdf5lib_H5_H5Ewalk2f */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
