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
#include <string.h>
#include "hdf5.h"
#include "h5util.h"
#include "h5gImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Gclose
    (JNIEnv *env, jclass clss, jlong group_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Gclose((hid_t)group_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Gclose */

/*
 * Create a java object of hdf.h5.structs.H5G_info_t
 * public class H5G_info_t {
 *   public H5G_STORAGE_TYPE  storage_type; // Type of storage for links in group
 *   public long     nlinks;       // Number of links in group
 *   public long     max_corder;   // Current max. creation order value for group
 *   public int      mounted;      // Whether group has a file mounted on it
 * }
 *
 */
jobject
create_H5G_info_t
    (JNIEnv *env, H5G_info_t group_info)
{
    jfieldID fid_storage_type, fid_nlinks, fid_max_corder, fid_mounted;
    jboolean jmounted;
    jclass   cls;
    jint     storage_type;
    jobject  obj = NULL;

    if (NULL == (cls = ENVPTR->FindClass(ENVONLY, "hdf/hdf5lib/structs/H5G_info_t")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (obj = ENVPTR->AllocObject(ENVONLY, cls)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid_storage_type = ENVPTR->GetFieldID(ENVONLY, cls, "storage_type", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid_nlinks = ENVPTR->GetFieldID(ENVONLY, cls, "nlinks", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid_max_corder = ENVPTR->GetFieldID(ENVONLY, cls, "max_corder", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid_mounted = ENVPTR->GetFieldID(ENVONLY, cls, "mounted", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    jmounted = (group_info.mounted == 0) ? JNI_FALSE : JNI_TRUE;
    storage_type = (jint)group_info.storage_type;

    ENVPTR->SetIntField(ENVONLY, obj, fid_storage_type, (jint)storage_type);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetLongField(ENVONLY, obj, fid_nlinks, (jlong)group_info.nlinks);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetLongField(ENVONLY, obj, fid_max_corder, (jlong)group_info.max_corder);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    ENVPTR->SetBooleanField(ENVONLY, obj, fid_mounted, jmounted);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    return obj;
} /* end create_H5G_info_t */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gcreate2
 * Signature: (JLjava/lang/String;JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Gcreate2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name,
          jlong link_plist_id, jlong create_plist_id, jlong access_plist_id)
{
    const char *grpName = NULL;
    hid_t       group_id = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gcreate2: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Gcreate2: group name not pinned");

    if ((group_id = H5Gcreate2((hid_t)loc_id, grpName, (hid_t)link_plist_id, (hid_t)create_plist_id, (hid_t)access_plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return (jlong)group_id;
} /* end Java_hdf_hdf5lib_H5__1H5Gcreate2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gcreate_anon
 * Signature: (JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Gcreate_1anon
    (JNIEnv *env, jclass clss, jlong loc_id, jlong gcpl_id, jlong gapl_id)
{
    hid_t group_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((group_id = H5Gcreate_anon((hid_t)loc_id, (hid_t)gcpl_id, (hid_t)gapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)group_id;
} /* end Java_hdf_hdf5lib_H5__1H5Gcreate_1anon */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gopen2
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Gopen2
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist_id)
{
    const char *grpName = NULL;
    hid_t       group_id = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gopen2: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Gopen2: group name not pinned");

    if ((group_id = H5Gopen2((hid_t)loc_id, grpName, (hid_t)access_plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return (jlong)group_id;
} /* end Java_hdf_hdf5lib_H5__1H5Gopen2 */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1create_1plist
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    hid_t plist_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((plist_id = H5Gget_create_plist((hid_t)loc_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)plist_id;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1create_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    H5G_info_t group_info;
    jobject    obj = NULL;

    UNUSED(clss);

    if (H5Gget_info((hid_t)loc_id, &group_info) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (obj = create_H5G_info_t(env, group_info)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Gget_info: unable to create H5G_info_t object");

done:
    return obj;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info_by_name
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong lapl_id)
{
    H5G_info_t  group_info;
    const char *grpName = NULL;
    jobject     obj = NULL;
    herr_t      ret_val = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_info_by_name: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Gget_info_by_name: group name not pinned");

    if ((ret_val = H5Gget_info_by_name((hid_t)loc_id, grpName, &group_info, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (obj = create_H5G_info_t(env, group_info))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Gget_info_by_name: unable to create H5G_info_t object");
    }

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return obj;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint index_type,
          jint order, jlong n, jlong lapl_id)
{
    H5_iter_order_t  corder = (H5_iter_order_t)order;
    H5_index_t       cindex_type = (H5_index_t)index_type;
    H5G_info_t       group_info;
    const char      *grpName = NULL;
    jobject          obj = NULL;
    herr_t           ret_val = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Gget_info_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Gget_info_by_idx: group name not pinned");

    if ((ret_val = H5Gget_info_by_idx((hid_t)loc_id, grpName, cindex_type,
            corder, (hsize_t)n, &group_info, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (obj = create_H5G_info_t(env, group_info))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Gget_info_by_idx: unable to create H5G_info_t object");
    }

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return obj;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Gflush
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Gflush((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Gflush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Grefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Grefresh
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Grefresh((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Grefresh */



#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
