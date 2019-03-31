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

#include <stdlib.h>
#include "hdf5.h"
#include "h5util.h"
#include "h5pImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
extern JavaVM *jvm;

extern jobject copy_callback;
extern jobject close_callback;
extern jobject create_callback;
extern jobject compare_callback;
extern jobject get_callback;
extern jobject set_callback;
extern jobject delete_callback;

typedef struct _cb_wrapper {
    jobject visit_callback;
    jobject op_data;
} cb_wrapper;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5P_cls_create_cb(hid_t prop_id, void *create_data);
static herr_t H5P_cls_copy_cb(hid_t new_prop_id, hid_t old_prop_id, void *copy_data);
static herr_t H5P_cls_close_cb(hid_t prop_id, void *close_data);
static herr_t H5P_prp_create_cb(const char *name, size_t size, void *value);
static herr_t H5P_prp_copy_cb(const char *name, size_t size, void *value);
static herr_t H5P_prp_close_cb(const char *name, size_t size, void *value);
static int H5P_prp_compare_cb(void *value1, void *value2, size_t size);
static herr_t H5P_prp_get_cb(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P_prp_set_cb(hid_t prop_id, const char *name, size_t size, void *value);
static herr_t H5P_prp_delete_cb(hid_t prop_id, const char *name, size_t size, void *value);

static herr_t H5P_iterate_cb(hid_t prop_id, const char *name, void *cb_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcreate
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate
    (JNIEnv *env, jclass clss, jlong type)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pcreate((hid_t) type)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pget_class((hid_t) plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /*
     * if (retVal == H5P_ROOT)
     *     H5_LIBRARY_ERROR(ENVONLY);
     */

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcopy
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pcopy((hid_t) plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pcopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if (plist >= 0)
        if ((retVal = H5Pclose((hid_t) plist)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pclose */

/*
 * TODO: H5Pencode
 */

/*
 * TODO: H5Pdecode
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class
 * Signature: (JLjava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class
    (JNIEnv *env, jclass clss, jlong parent_class, jstring name, jobject create_op,
        jobject create_data, jobject copy_op, jobject copy_data, jobject close_op, jobject close_data)
{
    const char *cstr = NULL;
    hid_t       class_id = H5I_INVALID_HID;

    UNUSED(clss);

    create_callback = create_op;
    close_callback = close_op;
    copy_callback = copy_op;

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "_H5Pcreate_class: class name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "_H5Pcreate_class: class name not pinned");

    if ((class_id = H5Pcreate_class((hid_t)parent_class, cstr, (H5P_cls_create_func_t)H5P_cls_create_cb, (void *) create_data,
            (H5P_cls_copy_func_t)H5P_cls_copy_cb, (void *) copy_data, (H5P_cls_close_func_t)H5P_cls_close_cb, (void *) close_data)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jlong)class_id;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pcreate_class_nocb
 * Signature: (JLjava/lang/String;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Pcreate_1class_1nocb
    (JNIEnv *env, jclass clss, jlong parent_class, jstring name)
{
    const char *cstr = NULL;
    hid_t       class_id = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "_H5Pcreate_class_nocb: class name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "_H5Pcreate_class_nocb: class name not pinned");

    if ((class_id = H5Pcreate_class((hid_t)parent_class, cstr,  NULL, NULL, NULL, NULL, NULL, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jlong)class_id;
} /* end Java_hdf_hdf5lib_H5__1H5Pcreate_1class_1nocb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size,
        jbyteArray def_value, jobject prp_create, jobject prp_set, jobject prp_get, jobject prp_delete,
        jobject prp_copy, jobject prp_cmp, jobject prp_close)
{
    const char *cstr = NULL;
    jboolean    isCopy;
    herr_t      status = FAIL;
    jbyte      *propValBuf = NULL;

    UNUSED(clss);

    copy_callback = prp_copy;
    close_callback = prp_close;
    create_callback = prp_create;
    compare_callback = prp_cmp;
    set_callback = prp_set;
    get_callback = prp_get;
    delete_callback = prp_delete;

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pregister2: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pregister2: property name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, &isCopy, "H5Pregister2: default property value buffer not pinned");

    if ((status = H5Pregister2((hid_t)cls_id, cstr, (size_t)prp_size, (void *)propValBuf, (H5P_prp_create_func_t)H5P_prp_create_cb,
            (H5P_prp_set_func_t)H5P_prp_set_cb, (H5P_prp_get_func_t)H5P_prp_get_cb, (H5P_prp_delete_func_t)H5P_prp_delete_cb,
            (H5P_prp_copy_func_t)H5P_prp_copy_cb, (H5P_prp_compare_func_t)H5P_prp_compare_cb, (H5P_prp_close_func_t)H5P_prp_close_cb)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (propValBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, (status < 0) ? JNI_ABORT : 0);
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);
} /* end Java_hdf_hdf5lib_H5_H5Pregister2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pregister2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pregister2_1nocb
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size, jbyteArray def_value)
{
    const char *cstr = NULL;
    jboolean    isCopy;
    herr_t      status = FAIL;
    jbyte      *propValBuf = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pregister2_nocb: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pregister2_nocb: property name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, &isCopy, "H5Pregister2_nocb: default property value buffer not pinned");

    if ((status = H5Pregister2((hid_t)cls_id, cstr, (size_t)prp_size, (void *)propValBuf, NULL, NULL, NULL, NULL, NULL, NULL, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (propValBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, (status < 0) ? JNI_ABORT : 0);
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);
} /* end Java_hdf_hdf5lib_H5_H5Pregister2_1nocb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2
 * Signature: (JLjava/lang/String;J[BLjava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size,
        jbyteArray def_value, jobject prp_set, jobject prp_get, jobject prp_delete,
        jobject prp_copy, jobject prp_cmp, jobject prp_close)
{
    const char *cstr = NULL;
    jboolean    isCopy;
    herr_t      status = FAIL;
    jbyte      *propValBuf = NULL;

    UNUSED(clss);

    copy_callback = prp_copy;
    close_callback = prp_close;
    compare_callback = prp_cmp;
    set_callback = prp_set;
    get_callback = prp_get;
    delete_callback = prp_delete;

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pinsert2: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pinsert2: property name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, &isCopy, "H5Pinsert2: property value buffer not pinned");

    if ((status = H5Pinsert2((hid_t)cls_id, cstr, (size_t)prp_size, (void *)propValBuf,
            (H5P_prp_set_func_t)H5P_prp_set_cb, (H5P_prp_get_func_t)H5P_prp_get_cb, (H5P_prp_delete_func_t)H5P_prp_delete_cb,
            (H5P_prp_copy_func_t)H5P_prp_copy_cb, (H5P_prp_compare_func_t)H5P_prp_compare_cb, (H5P_prp_close_func_t)H5P_prp_close_cb)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (propValBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, (status < 0) ? JNI_ABORT : 0);
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);
} /* end Java_hdf_hdf5lib_H5_H5Pinsert2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pinsert2_nocb
 * Signature: (JLjava/lang/String;J[B)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pinsert2_1nocb
    (JNIEnv *env, jclass clss, jlong cls_id, jstring name, jlong prp_size, jbyteArray def_value)
{
    const char *cstr = NULL;
    jboolean    isCopy;
    herr_t      status = FAIL;
    jbyte      *propValBuf = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pinsert2_nocb: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pinsert2_nocb: property name not pinned");

    PIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, &isCopy, "H5Pinsert2_nocb: property value buffer not pinned");

    if ((status = H5Pinsert2((hid_t)cls_id, cstr, (size_t)prp_size, (void *)propValBuf, NULL, NULL, NULL, NULL, NULL, NULL)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (propValBuf)
        UNPIN_BYTE_ARRAY(ENVONLY, def_value, propValBuf, (status < 0) ? JNI_ABORT : 0);
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);
} /* end Java_hdf_hdf5lib_H5_H5Pinsert2_1nocb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset
 * Signature: (JLjava/lang/String;I)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pset
    (JNIEnv *env, jclass clss, jlong plid, jstring name, jint val)
{
    const char *cstr = NULL;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pset: property name not pinned");

    if ((retVal = H5Pset((hid_t)plid, cstr, &val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pexist
 * Signature: (JLjava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pexist
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    const char *cstr = NULL;
    htri_t      bval = JNI_FALSE;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pexist: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pexist: property name not pinned");

    if ((bval = H5Pexist((hid_t)plid, cstr)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Pexist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_size
 * Signature: (JLjava/lang/String;)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1size
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    const char *cstr = NULL;
    size_t      size = 0;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_size: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pget_size: property name not pinned");

    if ((status = H5Pget_size((hid_t)plid, cstr, &size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jlong) size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nprops
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nprops
    (JNIEnv *env, jclass clss, jlong plid)
{
    size_t nprops = 0;

    UNUSED(clss);

    if (H5Pget_nprops((hid_t)plid, &nprops) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)nprops;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nprops */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1name
    (JNIEnv *env, jclass clss, jlong plid)
{
    char    *c_str = NULL;
    jstring  j_str = NULL;

    UNUSED(clss);

    if (NULL == (c_str = H5Pget_class_name((hid_t)plid)))
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (j_str = ENVPTR->NewStringUTF(ENVONLY, c_str))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_class_name: out of memory - unable to construct string from UTF characters");
    }

done:
    if (c_str)
        H5free_memory(c_str);

    return j_str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_class_parent
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1class_1parent
    (JNIEnv *env, jclass clss, jlong plid)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pget_class_parent((hid_t)plid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1class_1parent */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pisa_class
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pisa_1class
    (JNIEnv *env, jclass clss, jlong plid, jlong pcls)
{
    htri_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pisa_class((hid_t)plid, (hid_t)pcls)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pisa_1class */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    const char *cstr = NULL;
    jint        val;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pget: property name not pinned");

    if ((status = H5Pget((hid_t)plid, cstr, &val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jint)val;
} /* end Java_hdf_hdf5lib_H5_H5Pget */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pequal
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pequal
    (JNIEnv *env, jclass clss, jlong plid1, jlong plid2)
{
    htri_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pequal((hid_t)plid1, (hid_t)plid2)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pequal */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Piterate
 * Signature: (J[ILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Piterate
    (JNIEnv *env, jclass clss, jlong prop_id, jintArray idx, jobject callback_op, jobject op_data)
{
    cb_wrapper  wrapper = { callback_op, op_data };
    jboolean    isCopy;
    jint       *theArray = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Piterate: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Piterate: callback_op is NULL");

    if (NULL == idx) {
        if ((status = H5Piterate((hid_t)prop_id, NULL, (H5P_iterate_t)H5P_iterate_cb, (void *)&wrapper)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }
    else {
        PIN_INT_ARRAY(ENVONLY, idx, theArray, &isCopy, "H5Piterate: idx not pinned");

        if ((status = H5Piterate((hid_t)prop_id, (int *)&theArray[0], (H5P_iterate_t)H5P_iterate_cb, (void *)&wrapper)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, idx, theArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Piterate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pcopy_prop
 * Signature: (JJLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pcopy_1prop
    (JNIEnv *env, jclass clss, jlong dst_plid, jlong src_plid, jstring name)
{
    const char *cstr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pcopy_prop: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Pcopy_prop: property name not pinned");

    if ((retVal = H5Pcopy_prop((hid_t)dst_plid, (hid_t)src_plid, cstr)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pcopy_1prop */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Premove
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    const char *cstr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Premove: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Premove: property name not pinned");

    if ((retVal = H5Premove((hid_t)plid, cstr)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Premove */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Punregister
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Punregister
    (JNIEnv *env, jclass clss, jlong plid, jstring name)
{
    const char *cstr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Punregister: property name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, cstr, NULL, "H5Punregister: property name not pinned");

    if ((retVal = H5Punregister((hid_t)plid, cstr)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cstr)
        UNPIN_JAVA_STRING(ENVONLY, name, cstr);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Punregister */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Pclose_class
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Pclose_1class
    (JNIEnv *env, jclass clss, jlong plid)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pclose_class((hid_t)plid)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Pclose_1class */

static herr_t
H5P_cls_create_cb
    (hid_t prop_id, void *create_data)
{
    jmethodID  mid;
    JNIEnv    *cbenv = NULL;
    jclass     cls;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_cls_create_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, create_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLhdf/hdf5lib/callbacks/H5P_cls_create_func_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, create_callback, mid, prop_id, create_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_cls_create_cb */

static herr_t
H5P_cls_copy_cb
    (hid_t new_prop_id, hid_t old_prop_id, void *copy_data)
{
    jmethodID  mid;
    JNIEnv    *cbenv = NULL;
    jclass     cls;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_cls_copy_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, copy_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JJLhdf/hdf5lib/callbacks/H5P_cls_copy_func_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, copy_callback, mid, new_prop_id, old_prop_id, copy_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_cls_copy_cb */

static herr_t
H5P_cls_close_cb
    (hid_t prop_id, void *close_data)
{
    jmethodID  mid;
    JNIEnv    *cbenv = NULL;
    jclass     cls;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_cls_close_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, close_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLhdf/hdf5lib/callbacks/H5P_cls_close_func_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, close_callback, mid, prop_id, close_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_cls_close_cb */

static herr_t
H5P_prp_create_cb
    (const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_create_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, create_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(Ljava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, create_callback, mid, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_create_cb */

static herr_t
H5P_prp_set_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_set_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, set_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, set_callback, mid, prop_id, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_set_cb */

static herr_t
H5P_prp_get_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_get_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, get_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, get_callback, mid, prop_id, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_get_cb */

static herr_t
H5P_prp_delete_cb
    (hid_t prop_id, const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_delete_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, delete_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, delete_callback, mid, prop_id, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_delete_cb */

static herr_t
H5P_prp_copy_cb
    (const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_copy_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, copy_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(Ljava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, copy_callback, mid, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_copy_cb */

static int
H5P_prp_compare_cb
    (void *value1, void *value2, size_t size)
{
    jmethodID  mid;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_compare_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, compare_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "([B[BJ)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, compare_callback, mid, value1, value2, size);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_compare_cb */

static herr_t
H5P_prp_close_cb
    (const char *name, size_t size, void *value)
{
    jmethodID  mid;
    jstring    str;
    jclass     cls;
    JNIEnv    *cbenv = NULL;
    jint       status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_prp_close_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, close_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(Ljava/lang/String;J[B)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, close_callback, mid, str, size, value);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5P_prp_close_cb */

static herr_t
H5P_iterate_cb
    (hid_t prop_id, const char *name, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *) cb_data;
    jmethodID   mid;
    jobject     visit_callback = wrapper->visit_callback;
    jstring     str;
    jclass      cls;
    JNIEnv     *cbenv = NULL;
    void       *op_data = (void *) wrapper->op_data;
    jint        status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void**)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5P_iterate_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/callbacks/H5P_iterate_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, prop_id, str, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return status;
} /* end H5P_iterate_cb */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
