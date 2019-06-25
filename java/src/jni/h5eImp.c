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
 *  For details of the HDF libraries, see the HDF Documentation at:
 *   http://www.hdfgroup.org/HDF5/doc/
 *
 */

#include <jni.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5eImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
extern JavaVM *jvm;

typedef struct _cb_wrapper {
    jobject visit_callback;
    jobject op_data;
} cb_wrapper;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5E_walk_cb(int nindx, const H5E_error2_t *info, void *cb_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eauto_is_v2
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Eauto_1is_1v2
    (JNIEnv *env, jclass clss, jlong stk_id)
{
    unsigned int is_stack = 0;

    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eauto_is_v2: invalid stack ID");

    if (H5Eauto_is_v2((hid_t)stk_id, &is_stack) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jboolean)is_stack;
} /* end Java_hdf_hdf5lib_H5_H5Eauto_1is_1v2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eregister_class
 * Signature: (Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eregister_1class
    (JNIEnv *env, jclass clss, jstring cls_name, jstring lib_name, jstring version)
{
    const char* the_cls_name = NULL;
    const char* the_lib_name = NULL;
    const char* the_version = NULL;
    hid_t       ret_val = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == cls_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Eregister_class: class name is NULL");
    if (NULL == lib_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Eregister_class: lib name is NULL");
    if (NULL == version)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Eregister_class: version string is NULL");

    PIN_JAVA_STRING(ENVONLY, cls_name, the_cls_name, NULL, "H5Eregister_class: class name not pinned");
    PIN_JAVA_STRING(ENVONLY, lib_name, the_lib_name, NULL, "H5Eregister_class: lib name not pinned");
    PIN_JAVA_STRING(ENVONLY, version, the_version, NULL, "H5Eregister_class: version string not pinned");

    if ((ret_val = H5Eregister_class(the_cls_name, the_lib_name, the_version)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (the_version)
        UNPIN_JAVA_STRING(ENVONLY, version, the_version);
    if (the_lib_name)
        UNPIN_JAVA_STRING(ENVONLY, lib_name, the_lib_name);
    if (the_cls_name)
        UNPIN_JAVA_STRING(ENVONLY, cls_name, the_cls_name);

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eregister_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eunregister_class
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eunregister_1class
    (JNIEnv *env, jclass clss, jlong cls_id)
{
    UNUSED(clss);

    if (cls_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eunregister_class: invalid error class ID");

    if (H5Eunregister_class((hid_t)cls_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eunregister_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_msg
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1msg
    (JNIEnv *env, jclass clss, jlong err_id)
{
    UNUSED(clss);

    if (err_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eclose_msg: invalid error message ID");

    if (H5Eclose_msg((hid_t)err_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eclose_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_msg
 * Signature: (JILjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1msg
    (JNIEnv *env, jclass clss, jlong err_id, jint msg_type, jstring err_msg)
{
    H5E_type_t  error_msg_type = (H5E_type_t)msg_type;
    const char *the_err_msg = NULL;
    hid_t       ret_val = H5I_INVALID_HID;

    UNUSED(clss);

    if (err_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Ecreate_msg: invalid error class ID");
    if (NULL == err_msg)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ecreate_msg: error message string is NULL");

    PIN_JAVA_STRING(ENVONLY, err_msg, the_err_msg, NULL, "H5Ecreate_msg: error message string not pinned");

    if ((ret_val = H5Ecreate_msg((hid_t)err_id, error_msg_type, the_err_msg)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (the_err_msg)
        UNPIN_JAVA_STRING(ENVONLY, err_msg, the_err_msg);

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Ecreate_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ecreate_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Ecreate_1stack
    (JNIEnv *env, jclass clss)
{
    hid_t ret_val = H5I_INVALID_HID;

    UNUSED(clss);

    if ((ret_val = H5Ecreate_stack()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Ecreate_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_current_stack
 * Signature: ()J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1current_1stack
    (JNIEnv *env, jclass clss)
{
    hid_t ret_val = H5I_INVALID_HID;

    UNUSED(clss);

    if ((ret_val = H5Eget_current_stack()) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1current_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclose_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclose_1stack
    (JNIEnv *env, jclass clss, jlong stk_id)
{
    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eclose_stack: invalid error stack ID");

    if (H5Eclose_stack((hid_t)stk_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eclose_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eprint2
 * Signature: (JLjava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eprint2
    (JNIEnv *env, jclass clss, jlong stk_id, jobject stream_obj)
{
    herr_t ret_val = FAIL;

    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eprint2: invalid error stack ID");

    if (!stream_obj) {
        if ((ret_val = H5Eprint2((hid_t)stk_id, stdout)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }
    else {
        if ((ret_val = H5Eprint2((hid_t)stk_id, (FILE *)stream_obj)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eprint2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1class_1name
    (JNIEnv *env, jclass clss, jlong cls_id)
{
    jstring  str = NULL;
    ssize_t  buf_size;
    char    *namePtr = NULL;

    UNUSED(clss);

    if (cls_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eget_class_name: invalid error class ID");

    /* Get the length of the name */
    if ((buf_size = H5Eget_class_name((hid_t)cls_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!buf_size)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eget_class_name: no class name");

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Eget_class_name: malloc failed");

    if ((H5Eget_class_name((hid_t)cls_id, (char *)namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (namePtr)
        HDfree(namePtr);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1class_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eset_current_stack
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eset_1current_1stack
    (JNIEnv *env, jclass clss, jlong stk_id)
{
    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eset_current_stack: invalid error stack ID");

    if (H5Eset_current_stack((hid_t)stk_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eset_1current_1stack */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epop
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epop
    (JNIEnv *env, jclass clss, jlong stk_id, jlong count)
{
    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Epop: invalid error stack ID");

    if (H5Epop((hid_t)stk_id, (size_t)count) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Epop */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Epush2
 * Signature: (JLjava/lang/String;Ljava/lang/String;IJJJLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Epush2
    (JNIEnv *env, jclass clss, jlong stk_id, jstring filename, jstring funcname,
        jint linenumber, jlong class_id, jlong major_id, jlong minor_id, jstring err_desc)
{
    const char *fName = NULL;
    const char *fncName = NULL;
    const char *errMsg = NULL;
    herr_t      ret_val = FAIL;

    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Epush2: invalid error stack ID");
    if (class_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Epush2: invalid error class ID");
    if (major_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Epush2: invalid major error class ID");
    if (minor_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Epush2: invalid minor error class ID");
    if (NULL == filename)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Epush2: filename is NULL");
    if (NULL == funcname)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Epush2: function name is NULL");
    if (NULL == err_desc)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Epush2: error message is NULL");

    PIN_JAVA_STRING(ENVONLY, filename, fName, NULL, "H5Epush2: filename not pinned");
    PIN_JAVA_STRING(ENVONLY, funcname, fncName, NULL, "H5Epush2: function name not pinned");
    PIN_JAVA_STRING(ENVONLY, err_desc, errMsg, NULL, "H5Epush2: error message not pinned");

    if ((ret_val = H5Epush2((hid_t)stk_id, fName, fncName, (unsigned)linenumber, (hid_t)class_id,
            (hid_t)major_id, (hid_t)minor_id, errMsg)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (errMsg)
        UNPIN_JAVA_STRING(ENVONLY, err_desc, errMsg);
    if (fncName)
        UNPIN_JAVA_STRING(ENVONLY, funcname, fncName);
    if (fName)
        UNPIN_JAVA_STRING(ENVONLY, filename, fName);
} /* end Java_hdf_hdf5lib_H5_H5Epush2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eclear2
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Eclear2
    (JNIEnv *env, jclass clss, jlong stk_id)
{
    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eclear2: invalid error stack ID");

    if (H5Eclear2((hid_t)stk_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Eclear2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_msg
 * Signature: (J[I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1msg
    (JNIEnv *env, jclass clss, jlong msg_id, jintArray error_msg_type_list)
{
    H5E_type_t  error_msg_type;
    jstring     str = NULL;
    ssize_t     buf_size;
    jint       *theArray = NULL;
    char       *namePtr = NULL;

    UNUSED(clss);

    if (msg_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eget_msg: invalid error message ID");
    if (NULL == error_msg_type_list)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Eget_msg: error_msg_type_list is NULL");

    /* Get the length of the name */
    if ((buf_size = H5Eget_msg((hid_t)msg_id, NULL, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!buf_size)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Eget_msg: invalid message");

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Eget_msg: malloc failed");

    PIN_INT_ARRAY(ENVONLY, error_msg_type_list, theArray, NULL, "H5Eget_msg: error_msg_type_list not pinned");

    if ((H5Eget_msg((hid_t)msg_id, &error_msg_type, (char *)namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    theArray[0] = error_msg_type;

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, error_msg_type_list, theArray, 0);
    if (namePtr)
        HDfree(namePtr);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1msg */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Eget_num
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Eget_1num
    (JNIEnv *env, jclass clss, jlong stk_id)
{
    ssize_t ret_val = -1;

    UNUSED(clss);

    if (stk_id < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Eget_num: invalid error stack ID");

    if ((ret_val = H5Eget_num((hid_t)stk_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Eget_1num */

static herr_t
H5E_walk_cb
    (int nindx, const H5E_error2_t *info, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jmethodID   constructor;
    jmethodID   mid;
    jobject     visit_callback = wrapper->visit_callback;
    jstring     str1, str2, str3;
    jobject     cb_info_t = NULL;
    jvalue      args[7];
    JNIEnv     *cbenv = NULL;
    jclass      cls;
    void       *op_data = (void *)wrapper->op_data;
    jint        status = FAIL;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5E_walk_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(ILhdf/hdf5lib/structs/H5E_error2_t;Lhdf/hdf5lib/callbacks/H5E_walk_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[0].j = info->cls_id;
    args[1].j = info->maj_num;
    args[2].j = info->min_num;
    args[3].i = (jint)info->line;

    if (NULL == (str1 = CBENVPTR->NewStringUTF(CBENVONLY, info->func_name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[4].l = str1;

    if (NULL == (str2 = CBENVPTR->NewStringUTF(CBENVONLY, info->file_name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[5].l = str2;

    if (NULL == (str3 = CBENVPTR->NewStringUTF(CBENVONLY, info->desc)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[6].l = str3;

    /* Get a reference to your class if you don't have it already */
    if (NULL == (cls = CBENVPTR->FindClass(CBENVONLY, "hdf/hdf5lib/structs/H5E_error2_t")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    /* get a reference to the constructor; the name is <init> */
    if (NULL == (constructor = CBENVPTR->GetMethodID(CBENVONLY, cls, "<init>", "(JJJILjava/lang/String;Ljava/lang/String;Ljava/lang/String;)V")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (cb_info_t = CBENVPTR->NewObjectA(CBENVONLY, cls, constructor, args)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, nindx, cb_info_t, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (CBENVONLY)
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
    (JNIEnv *env, jclass clss, jlong stk_id, jlong direction, jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = { callback_op, op_data };

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ewalk2: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ewalk2: callback_op is NULL");

    if (H5Ewalk2(stk_id, (H5E_direction_t)direction, (H5E_walk2_t)H5E_walk_cb, (void *)&wrapper) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end iJava_hdf_hdf5lib_H5_H5Ewalk2f */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
