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

#include <jni.h>
#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5fImp.h"
#include "h5util.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fopen
 * Signature: (Ljava/lang/String;IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fopen
    (JNIEnv *env, jclass clss, jstring name, jint flags, jlong access_id)
{
    hid_t       status = -1;
    const char *fileName;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        status = H5Fopen(fileName, (unsigned)flags, (hid_t)access_id );

        UNPIN_JAVA_STRING(name, fileName);

        if (status < 0)
            h5libraryError(env);
    }

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
    hid_t       status = -1;
    const char *fileName;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        status = H5Fcreate(fileName, (unsigned)flags, create_id, access_id);

        UNPIN_JAVA_STRING(name, fileName);

        if (status < 0)
            h5libraryError(env);
    }

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
    herr_t retVal = -1;

    retVal =  H5Fflush((hid_t)object_id, (H5F_scope_t)scope );
    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Fflush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1name
    (JNIEnv *env, jclass cls, jlong file_id)
{
    char   *namePtr;
    jstring str = NULL;
    ssize_t buf_size;

    /* get the length of the name */
    buf_size = H5Fget_name((hid_t)file_id, NULL, 0);

    if (buf_size <= 0) {
        h5badArgument(env, "H5Fget_name:  buf_size <= 0");
    } /* end if */
    else {
        buf_size++; /* add extra space for the null terminator */
        namePtr = (char*)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (namePtr == NULL) {
            h5outOfMemory(env, "H5Fget_name:  malloc failed");
        } /* end if */
        else {
            buf_size = H5Fget_name((hid_t)file_id, namePtr, (size_t)buf_size);
            if (buf_size < 0) {
                HDfree(namePtr);
                h5libraryError(env);
            } /* end if */
            else {
                str = ENVPTR->NewStringUTF(ENVPAR namePtr);
                HDfree(namePtr);
            }/* end else */
        }/* end else */
    }/* end else */

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
    htri_t      bval = JNI_FALSE;
    const char *fileName;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        bval = H5Fis_hdf5(fileName);

        UNPIN_JAVA_STRING(name, fileName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

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
    hid_t retVal = -1;

    retVal =  H5Fget_create_plist((hid_t)file_id );
    if (retVal < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal =  H5Fget_access_plist((hid_t)file_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Fget_1access_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_intent
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1intent
    (JNIEnv *env, jclass cls, jlong file_id)
{
    unsigned intent = 0;

    if (H5Fget_intent((hid_t)file_id, &intent) < 0)
        h5libraryError(env);

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
    herr_t status = -1;

    if (file_id > 0)
        status = H5Fclose((hid_t)file_id );

    if (status < 0)
        h5libraryError(env);

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
    herr_t      status = -1;
    const char *fileName;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        status = H5Fmount((hid_t)loc_id, fileName, (hid_t)child_id, (hid_t)plist_id);

        UNPIN_JAVA_STRING(name, fileName);

        if (status < 0)
            h5libraryError(env);
    }

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
    herr_t      status = -1;
    const char *fileName;

    PIN_JAVA_STRING(name, fileName);
    if (fileName != NULL) {
        status = H5Funmount((hid_t)loc_id, fileName);

        UNPIN_JAVA_STRING(name, fileName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Funmount */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_freespace
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1freespace
    (JNIEnv *env, jclass cls, jlong file_id)
{
    hssize_t ret_val = H5Fget_freespace((hid_t)file_id);

    if (ret_val < 0)
        h5libraryError(env);

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
    hid_t retVal = -1;

    retVal = H5Freopen((hid_t)file_id);
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Freopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_ids
 * Signature: (JIJ[J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1ids
    (JNIEnv *env, jclass cls, jlong file_id, jint types, jlong maxObjs,
          jlongArray obj_id_list)
{
    ssize_t  ret_val = -1;
    jlong   *obj_id_listP;
    jboolean isCopy;
    hid_t   *id_list;
    size_t   rank;
    size_t   i;

    if (obj_id_list == NULL) {
        h5nullArgument(env, "H5Fget_obj_ids:  obj_id_list is NULL");
    } /* end if */
    else {
        obj_id_listP = ENVPTR->GetLongArrayElements(ENVPAR obj_id_list, &isCopy);
        if (obj_id_listP == NULL) {
            h5JNIFatalError(env, "H5Fget_obj_ids:  obj_id_list not pinned");
        } /* end if */
        else {
            rank = (size_t)ENVPTR->GetArrayLength(ENVPAR obj_id_list);

            id_list = (hid_t *)HDmalloc(rank * sizeof(hid_t));
            if (id_list == NULL) {
                ENVPTR->ReleaseLongArrayElements(ENVPAR obj_id_list, obj_id_listP, JNI_ABORT);
                h5JNIFatalError(env, "H5Fget_obj_ids:  obj_id_list not converted to hid_t");
            } /* end if */
            else {
                ret_val = H5Fget_obj_ids((hid_t)file_id, (unsigned int)types, (size_t)maxObjs, id_list);

                if (ret_val < 0) {
                    ENVPTR->ReleaseLongArrayElements(ENVPAR obj_id_list, obj_id_listP, JNI_ABORT);
                    HDfree(id_list);
                    h5libraryError(env);
                } /* end if */
                else {
                    for (i = 0; i < rank; i++) {
                        obj_id_listP[i] = (jlong)id_list[i];
                    } /* end for */
                    HDfree(id_list);
                    ENVPTR->ReleaseLongArrayElements(ENVPAR obj_id_list, obj_id_listP, 0);
                } /* end else */
            } /* end else */
        } /* end else */
    } /* end else */

    return (jlong)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1obj_1ids */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_count
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1count
    (JNIEnv *env, jclass cls, jlong file_id, jint types)
{
    ssize_t ret_val = H5Fget_obj_count((hid_t)file_id, (unsigned int)types);

    if (ret_val < 0)
        h5libraryError(env);

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

    if (H5Fget_filesize ((hid_t)file_id, &size) < 0)
        h5libraryError(env);

    return (jlong) size;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1filesize */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_hit_rate
 * Signature: (J)D
 */
JNIEXPORT jdouble JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1hit_1rate
    (JNIEnv *env, jclass cls, jlong file_id)
{
    double rate = 0.0;

    if (H5Fget_mdc_hit_rate((hid_t)file_id, &rate) < 0)
        h5libraryError(env);

    return (jdouble)rate;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1mdc_1hit_1rate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1size
    (JNIEnv *env, jclass cls, jlong file_id, jlongArray metadata_cache)
{
    jint     size = 0;
    jlong   *metadata_cache_ptr;
    size_t   max_size = 0, min_clean_size = 0, cur_size = 0;
    int      cur_num_entries = -1;
    jboolean isCopy;

    if (metadata_cache == NULL) {
        h5nullArgument(env, "H5Fget_mdc_size:  metadata_cache is NULL");
    } /* end if */
    else {
        size = (int)ENVPTR->GetArrayLength(ENVPAR metadata_cache);
        if (size < 3) {
            h5badArgument(env, "H5Fget_mdc_size:  length of metadata_cache < 3.");
        } /* end if */
        else {
            if (H5Fget_mdc_size((hid_t)file_id, &max_size, &min_clean_size, &cur_size, &cur_num_entries) < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                metadata_cache_ptr = ENVPTR->GetLongArrayElements(ENVPAR metadata_cache, &isCopy);
                metadata_cache_ptr[0] = (jlong)max_size;
                metadata_cache_ptr[1] = (jlong)min_clean_size;
                metadata_cache_ptr[2] = (jlong)cur_size;
                ENVPTR->ReleaseLongArrayElements(ENVPAR metadata_cache, metadata_cache_ptr, 0);
            } /* end else */
        } /* end else */
    } /* end else */

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
    H5F_info2_t  finfo;
    jvalue      args[9];
    jobject     ihinfobuf;
    jobject     ret_obj = NULL;

    if (H5Fget_info2((hid_t)obj_id, &finfo) < 0) {
       h5libraryError(env);
    } /* end if */
    else {
        args[0].j = (jlong)finfo.sohm.msgs_info.index_size;
        args[1].j = (jlong)finfo.sohm.msgs_info.heap_size;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
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
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5F_info2_t", "(IJJIJJIJLhdf/hdf5lib/structs/H5_ih_info_t;)V", args);
    } /* end else */
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Fget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Freset_mdc_hit_rate_stats
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Freset_1mdc_1hit_1rate_1stats
    (JNIEnv *env, jclass cls, jlong file_id)
{
    if (H5Freset_mdc_hit_rate_stats((hid_t)file_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Freset_1mdc_1hit_1rate_1stats */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fclear_elink_file_cache
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fclear_1elink_1file_1cache
    (JNIEnv *env, jclass cls, jlong file_id)
{
    if (H5Fclear_elink_file_cache((hid_t)file_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Fclear_1elink_1file_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_swmr_write
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstart_1swmr_1write
	(JNIEnv *env, jclass cls, jlong file_id)
{
    if (H5Fstart_swmr_write((hid_t)file_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Fstart_1swmr_1write */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstart_1mdc_1logging
	(JNIEnv *env, jclass cls, jlong file_id)
{
    if (H5Fstart_mdc_logging((hid_t)file_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Fstart_1mdc_1logging */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstop_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fstop_1mdc_1logging
    (JNIEnv *env, jclass cls, jlong file_id)
{
    if (H5Fstop_mdc_logging((hid_t)file_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Fstop_1mdc_1logging */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_logging_status
 * Signature: (J[Z)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1logging_1status
	(JNIEnv *env, jclass cls, jlong file_id, jbooleanArray mdc_logging_status)
{
    hbool_t    is_enabled;
    hbool_t    is_currently_logging;
    jboolean  *mdc_logging_status_ptr;
    jint       size;
    jboolean   isCopy;

    if (mdc_logging_status == NULL) {
        h5nullArgument(env, "H5Fget_mdc_logging_status:  mdc_logging_status is NULL");
    } /* end if */
    else {
        size = (int)ENVPTR->GetArrayLength(ENVPAR mdc_logging_status);
        if (size < 2) {
            h5badArgument(env, "H5Fget_mdc_logging_status:  length of mdc_logging_status < 2.");
        } /* end if */
        else {
            if (H5Fget_mdc_logging_status((hid_t)file_id, &is_enabled, &is_currently_logging) < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                mdc_logging_status_ptr = ENVPTR->GetBooleanArrayElements(ENVPAR mdc_logging_status, &isCopy);
                mdc_logging_status_ptr[0] = (jboolean)is_enabled;
                mdc_logging_status_ptr[1] = (jboolean)is_currently_logging;
                ENVPTR->ReleaseBooleanArrayElements(ENVPAR mdc_logging_status, mdc_logging_status_ptr, 0);
            } /* end else */
        } /* end else */
    } /* end else */
} /* end Java_hdf_hdf5lib_H5_H5Fget_1mdc_1logging_1status */



#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
