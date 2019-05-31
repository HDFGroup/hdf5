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

#include <jni.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5fImp.h"
#include "h5util.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fopen
 * Signature: (Ljava/lang/String;IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fopen
    (JNIEnv *env, jclass clss, jstring name, jint flags, jlong access_id)
{
    const char *fileName = NULL;
    hid_t       status = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fopen: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Fopen: file name not pinned");

    if ((status = H5Fopen(fileName, (unsigned)flags, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Fopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fcreate
 * Signature: (Ljava/lang/String;IJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fcreate
    (JNIEnv *env, jclass clss, jstring name, jint flags, jlong create_id, jlong access_id)
{
    const char *fileName = NULL;
    hid_t       status = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fcreate: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Fcreate: file name not pinned");

    if ((status = H5Fcreate(fileName, (unsigned)flags, create_id, access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Fcreate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fflush
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fflush
    (JNIEnv *env, jclass clss, jlong object_id, jint scope)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Fflush((hid_t)object_id, (H5F_scope_t)scope)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Fflush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1name
    (JNIEnv *env, jclass clss, jlong file_id)
{
    jstring  str = NULL;
    ssize_t  buf_size;
    char    *namePtr = NULL;

    UNUSED(clss);

    /* Get the length of the name */
    if ((buf_size = H5Fget_name((hid_t)file_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (namePtr = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Fget_name: malloc failed");

    if ((H5Fget_name((hid_t)file_id, namePtr, (size_t)buf_size + 1)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    namePtr[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, namePtr)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (namePtr)
        HDfree(namePtr);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fis_hdf5
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Fis_1hdf5
    (JNIEnv *env, jclass clss, jstring name)
{
    const char *fileName = NULL;
    htri_t      bval = JNI_FALSE;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fis_hdf5: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Fis_hdf5: file name not pinned");

    if ((bval = H5Fis_hdf5(fileName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Fis_1hdf5 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fget_1create_1plist
    (JNIEnv *env, jclass clss, jlong file_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Fget_create_plist((hid_t)file_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Fget_1create_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_access_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fget_1access_1plist
    (JNIEnv *env, jclass clss, jlong file_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Fget_access_plist((hid_t)file_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Fget_1access_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_intent
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1intent
    (JNIEnv *env, jclass clss, jlong file_id)
{
    unsigned intent = 0;

    UNUSED(clss);

    if (H5Fget_intent((hid_t)file_id, &intent) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)intent;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1intent */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Fclose
    (JNIEnv *env, jclass clss, jlong file_id)
{
    herr_t status = FAIL;

    UNUSED(clss);

    if ((status = H5Fclose((hid_t)file_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)status;
} /* end Java_hdf_hdf5lib_H5__1H5Fclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fmount
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fmount
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong child_id, jlong plist_id)
{
    const char *fileName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fmount: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Fmount: file name not pinned");

    if ((status = H5Fmount((hid_t)loc_id, fileName, (hid_t)child_id, (hid_t)plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Fmount */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Funmount
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Funmount
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name)
{
    const char *fileName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Funmount: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Funmount: file name not pinned");

    if ((status = H5Funmount((hid_t)loc_id, fileName)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Funmount */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_freespace
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1freespace
    (JNIEnv *env, jclass clss, jlong file_id)
{
    hssize_t ret_val = -1;

    UNUSED(clss);

    if ((ret_val = H5Fget_freespace((hid_t)file_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1freespace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Freopen
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Freopen
    (JNIEnv *env, jclass clss, jlong file_id)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Freopen((hid_t)file_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Freopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_ids
 * Signature: (JIJ[J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1ids
    (JNIEnv *env, jclass clss, jlong file_id, jint types, jlong maxObjs,
          jlongArray obj_id_list)
{
    jboolean  isCopy;
    ssize_t   ret_val = -1;
    size_t    i;
    jsize     rank;
    jlong    *obj_id_listP = NULL;
    hid_t    *id_list = NULL;

    UNUSED(clss);

    if (NULL == obj_id_list)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fget_obj_ids: obj_id_list is NULL");

    PIN_LONG_ARRAY(ENVONLY, obj_id_list, obj_id_listP, &isCopy, "H5Fget_obj_ids: obj_id_list not pinned");

    if ((rank = ENVPTR->GetArrayLength(ENVONLY, obj_id_list)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Fget_obj_ids: obj_id_list length < 0");
    }

    if (NULL == (id_list = (hid_t *) HDmalloc((size_t)rank * sizeof(hid_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Fget_obj_ids: malloc failed");

    if ((ret_val = H5Fget_obj_ids((hid_t)file_id, (unsigned int)types, (size_t)maxObjs, id_list)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (size_t)rank; i++) {
        obj_id_listP[i] = (jlong)id_list[i];
    } /* end for */

done:
    if (id_list)
        HDfree(id_list);
    if (obj_id_listP)
        UNPIN_LONG_ARRAY(ENVONLY, obj_id_list, obj_id_listP, (ret_val < 0) ? JNI_ABORT : 0);

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1obj_1ids */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_count
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1count
    (JNIEnv *env, jclass clss, jlong file_id, jint types)
{
    ssize_t ret_val = -1;

    UNUSED(clss);

    if ((ret_val = H5Fget_obj_count((hid_t)file_id, (unsigned int)types)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1obj_1count_1long */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_filesize
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1filesize
    (JNIEnv *env, jclass clss, jlong file_id)
{
    hsize_t size = 0;

    UNUSED(clss);

    if (H5Fget_filesize((hid_t)file_id, &size) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) size;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1filesize */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_hit_rate
 * Signature: (J)D
 */
JNIEXPORT jdouble JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1hit_1rate
    (JNIEnv *env, jclass clss, jlong file_id)
{
    double rate = 0.0;

    UNUSED(clss);

    if (H5Fget_mdc_hit_rate((hid_t)file_id, &rate) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jdouble)rate;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1mdc_1hit_1rate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1size
    (JNIEnv *env, jclass clss, jlong file_id, jlongArray metadata_cache)
{
    jboolean  isCopy;
    size_t    max_size = 0, min_clean_size = 0, cur_size = 0;
    jlong    *metadata_cache_ptr = NULL;
    jsize     size = 0;
    int       cur_num_entries = -1;

    UNUSED(clss);

    if (NULL == metadata_cache)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_size: metadata cache array is NULL");

    if ((size = ENVPTR->GetArrayLength(ENVONLY, metadata_cache)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_size: metadata_cache length < 0");
    }

    if (size < 3)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_size: length of metadata_cache array < 3");

    if (H5Fget_mdc_size((hid_t)file_id, &max_size, &min_clean_size, &cur_size, &cur_num_entries) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    PIN_LONG_ARRAY(ENVONLY, metadata_cache, metadata_cache_ptr, &isCopy, "H5Fget_mdc_size: metadata_cache array not pinned");

    metadata_cache_ptr[0] = (jlong)max_size;
    metadata_cache_ptr[1] = (jlong)min_clean_size;
    metadata_cache_ptr[2] = (jlong)cur_size;

done:
    if (metadata_cache_ptr)
        UNPIN_LONG_ARRAY(ENVONLY, metadata_cache, metadata_cache_ptr, 0);

    return (jint)cur_num_entries;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1mdc_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5F_info2_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1info
    (JNIEnv *env, jclass clss, jlong obj_id)
{
    H5F_info2_t finfo;
    jobject     ihinfobuf;
    jvalue      args[9];
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (H5Fget_info2((hid_t)obj_id, &finfo) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].j = (jlong)finfo.sohm.msgs_info.index_size;
    args[1].j = (jlong)finfo.sohm.msgs_info.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);

    ihinfobuf = ret_obj;

    args[0].i = (jint)finfo.super.version;
    args[1].j = (jlong)finfo.super.super_size;
    args[2].j = (jlong)finfo.super.super_ext_size;
    args[3].i = (jint)finfo.free.version;
    args[4].j = (jlong)finfo.free.meta_size;
    args[5].j = (jlong)finfo.free.tot_space;
    args[6].j = (jint)finfo.sohm.version;
    args[7].j = (jlong)finfo.sohm.hdr_size;
    args[8].l = ihinfobuf;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5F_info2_t", "(IJJIJJIJLhdf/hdf5lib/structs/H5_ih_info_t;)V", args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Freset_mdc_hit_rate_stats
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Freset_1mdc_1hit_1rate_1stats
    (JNIEnv *env, jclass clss, jlong file_id)
{
    UNUSED(clss);

    if (H5Freset_mdc_hit_rate_stats((hid_t)file_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Freset_1mdc_1hit_1rate_1stats */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fclear_elink_file_cache
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fclear_1elink_1file_1cache
    (JNIEnv *env, jclass clss, jlong file_id)
{
    UNUSED(clss);

    if (H5Fclear_elink_file_cache((hid_t)file_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Fclear_1elink_1file_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_swmr_write
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstart_1swmr_1write
    (JNIEnv *env, jclass clss, jlong file_id)
{
    UNUSED(clss);

    if (H5Fstart_swmr_write((hid_t)file_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Fstart_1swmr_1write */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstart_1mdc_1logging
    (JNIEnv *env, jclass clss, jlong file_id)
{
    UNUSED(clss);

    if (H5Fstart_mdc_logging((hid_t)file_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Fstart_1mdc_1logging */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstop_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstop_1mdc_1logging
    (JNIEnv *env, jclass clss, jlong file_id)
{
    UNUSED(clss);

    if (H5Fstop_mdc_logging((hid_t)file_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Fstop_1mdc_1logging */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_logging_status
 * Signature: (J[Z)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1logging_1status
    (JNIEnv *env, jclass clss, jlong file_id, jbooleanArray mdc_logging_status)
{
    jboolean  *mdc_logging_status_ptr = NULL;
    jboolean   isCopy;
    hbool_t    is_enabled;
    hbool_t    is_currently_logging;
    jsize      size;

    UNUSED(clss);

    if (NULL == mdc_logging_status)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_logging_status: mdc_logging_status is NULL");

    if ((size = ENVPTR->GetArrayLength(ENVONLY, mdc_logging_status)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_logging_status: mdc_logging_status length < 0");
    }

    if (size < 2)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Fget_mdc_logging_status: length of mdc_logging_status < 2");

    if (H5Fget_mdc_logging_status((hid_t)file_id, &is_enabled, &is_currently_logging) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    PIN_BOOL_ARRAY(ENVONLY, mdc_logging_status, mdc_logging_status_ptr, &isCopy, "H5Fget_mdc_logging_status: mdc_logging_status array not pinned");

    mdc_logging_status_ptr[0] = (jboolean)is_enabled;
    mdc_logging_status_ptr[1] = (jboolean)is_currently_logging;

done:
    if (mdc_logging_status_ptr)
        UNPIN_BOOL_ARRAY(ENVONLY, mdc_logging_status, mdc_logging_status_ptr, 0);

    return;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1mdc_1logging_1status */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fset_dset_no_attrs_hint
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fset_1dset_1no_1attrs_1hint
(JNIEnv *env, jclass clss, jlong file_id, jboolean minimize)
{
    hbool_t minimize_val;
    herr_t  retVal = FAIL;

    UNUSED(clss);

    minimize_val = (minimize == JNI_TRUE) ? TRUE : FALSE;

    if ((retVal = H5Fset_dset_no_attrs_hint((hid_t)file_id, (hbool_t)minimize_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_dset_no_attrs_hint
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1dset_1no_1attrs_1hint
(JNIEnv *env, jclass clss, jlong file_id)
{
    jboolean bval = JNI_FALSE;
    hbool_t  minimize = FALSE;

    UNUSED(clss);

    if (H5Fget_dset_no_attrs_hint((hid_t)file_id, (hbool_t *)&minimize) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (minimize == TRUE)
        bval =  JNI_TRUE;

done:
    return bval;
}

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fset_libver_bounds
 * Signature: (JII)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fset_1libver_1bounds
    (JNIEnv *env, jclass clss, jlong file_id, jint low, jint high)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Fset_libver_bounds((hid_t)file_id, (H5F_libver_t)low, (H5F_libver_t)high)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Fset_1libver_1bounds */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
