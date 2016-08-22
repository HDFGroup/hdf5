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
#include <string.h>
#include "hdf5.h"
#include "h5util.h"
#include "h5gImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Gclose
    (JNIEnv *env, jclass clss, jlong group_id)
{
    herr_t retVal = -1;

    retVal =  H5Gclose((hid_t)group_id);
    if (retVal < 0)
        h5libraryError(env);

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
    jclass   cls;
    jboolean jmounted;
    jint     storage_type;
    jobject  obj = NULL;
    jfieldID fid_storage_type, fid_nlinks, fid_max_corder, fid_mounted;

    cls = ENVPTR->FindClass(ENVPAR "hdf/hdf5lib/structs/H5G_info_t");
    if (cls != NULL) {
        obj = ENVPTR->AllocObject(ENVPAR cls);
        if (obj != NULL) {
            if ((fid_storage_type = ENVPTR->GetFieldID(ENVPAR cls, "storage_type", "I")) != NULL) {
                if ((fid_nlinks = ENVPTR->GetFieldID(ENVPAR cls, "nlinks", "J")) != NULL) {
                    if ((fid_max_corder = ENVPTR->GetFieldID(ENVPAR cls, "max_corder", "J")) != NULL) {
                        if ((fid_mounted = ENVPTR->GetFieldID(ENVPAR cls, "mounted", "Z")) != NULL) {
                            jmounted = (group_info.mounted==0) ? JNI_FALSE : JNI_TRUE;
                            storage_type = (jint)group_info.storage_type;

                            ENVPTR->SetIntField(ENVPAR obj, fid_storage_type, (jint)storage_type);
                            ENVPTR->SetLongField(ENVPAR obj, fid_nlinks, (jlong)group_info.nlinks);
                            ENVPTR->SetLongField(ENVPAR obj, fid_max_corder, (jlong)group_info.max_corder);
                            ENVPTR->SetBooleanField(ENVPAR obj, fid_mounted, jmounted);
                        }
                    }
                }
            }
        }
    }
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
    hid_t       group_id = -1;
    const char *gName;

    PIN_JAVA_STRING(name, gName);
    if (gName != NULL) {
        group_id = H5Gcreate2((hid_t)loc_id, gName, (hid_t)link_plist_id, (hid_t)create_plist_id, (hid_t)access_plist_id );

        UNPIN_JAVA_STRING(name, gName);
        if (group_id < 0)
            h5libraryError(env);
    }

    return (jlong)group_id;
} /* end Java_hdf_hdf5lib_H5__1H5Gcreate2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Gcreate_anon
 * Signature: (JJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Gcreate_1anon
    (JNIEnv *env, jclass cls, jlong loc_id, jlong gcpl_id, jlong gapl_id)
{
    hid_t group_id = -1;

    group_id = H5Gcreate_anon((hid_t)loc_id, (hid_t)gcpl_id, (hid_t)gapl_id);
    if (group_id < 0)
        h5libraryError(env);

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
    hid_t group_id = -1;
    const char *gName;

    PIN_JAVA_STRING(name, gName);
    if (gName != NULL) {
        group_id = H5Gopen2((hid_t)loc_id, gName, (hid_t)access_plist_id );

        UNPIN_JAVA_STRING(name, gName);

        if (group_id < 0)
            h5libraryError(env);
    }

    return (jlong)group_id;
} /* end Java_hdf_hdf5lib_H5__1H5Gopen2 */


/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1create_1plist
    (JNIEnv *env, jclass cls, jlong loc_id)
{
    hid_t plist_id = H5Gget_create_plist((hid_t)loc_id);

    if (plist_id < 0)
        h5libraryError(env);

    return (jlong)plist_id;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1create_1plist */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info
    (JNIEnv *env, jclass cls, jlong loc_id)
{
    jobject    obj = NULL;
    H5G_info_t group_info;

    if (H5Gget_info((hid_t)loc_id, &group_info) < 0)
        h5libraryError(env);
    else
        obj = create_H5G_info_t(env, group_info);

    return obj;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info_by_name
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1name
    (JNIEnv *env, jclass cls, jlong loc_id, jstring name, jlong lapl_id)
{
    jobject     obj = NULL;
    herr_t      ret_val = -1;
    const char *gName;
    H5G_info_t  group_info;

    PIN_JAVA_STRING(name, gName);
    if (gName != NULL) {
        ret_val = H5Gget_info_by_name((hid_t)loc_id, gName, &group_info, (hid_t)lapl_id);

        UNPIN_JAVA_STRING(name, gName);

        if (ret_val < 0)
            h5libraryError(env);
        else
            obj = create_H5G_info_t(env, group_info);
    }

    return obj;
} /* end Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Gget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5G_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Gget_1info_1by_1idx
    (JNIEnv *env, jclass cls, jlong loc_id, jstring name, jint index_type,
          jint order, jlong n, jlong lapl_id)
{
    jobject         obj = NULL;
    herr_t          ret_val = -1;
    const char     *gName;
    H5G_info_t      group_info;
    H5_index_t      cindex_type = (H5_index_t)index_type;
    H5_iter_order_t corder = (H5_iter_order_t)order;

    PIN_JAVA_STRING(name, gName);
    if (gName != NULL) {
        ret_val = H5Gget_info_by_idx((hid_t)loc_id, gName, cindex_type,
                corder, (hsize_t)n, &group_info, (hid_t)lapl_id);

        UNPIN_JAVA_STRING(name, gName);

        if (ret_val < 0)
            h5libraryError(env);
        else
            obj = create_H5G_info_t(env, group_info);
    }

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
    if (H5Gflush((hid_t)loc_id) < 0)
        h5libraryError(env);
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
    if (H5Grefresh((hid_t)loc_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Grefresh */



#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
