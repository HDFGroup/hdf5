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
#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5lImp.h"

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

static herr_t H5L_iterate_cb(hid_t g_id, const char *name, const H5L_info_t *info, void *cb_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcopy
    (JNIEnv *env, jclass clss, jlong cur_loc_id, jstring cur_name, jlong dst_loc_id,
        jstring dst_name, jlong create_id, jlong access_id)
{
    const char *lCurName = NULL;
    const char *lDstName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcopy: src name is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcopy: dest name is NULL");

    PIN_JAVA_STRING(ENVONLY, cur_name, lCurName, NULL, "H5Lcopy: src name not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, lDstName, NULL, "H5Lcopy: dest name not pinned");

    if ((status = H5Lcopy((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (lDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, lDstName);
    if (lCurName)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, lCurName);
} /* end Java_hdf_hdf5lib_H5_H5Lcopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_external
 * Signature: (Ljava/lang/String;Ljava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1external
    (JNIEnv *env, jclass clss, jstring file_name, jstring cur_name,
        jlong dst_loc_id, jstring dst_name, jlong create_id, jlong access_id)
{
    const char *lFileName = NULL;
    const char *lCurName = NULL;
    const char *lDstName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == file_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_external: file name is NULL");
    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_external: object name is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_external: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, file_name, lFileName, NULL, "H5Lcreate_external: file name not pinned");
    PIN_JAVA_STRING(ENVONLY, cur_name, lCurName, NULL, "H5Lcreate_external: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, lDstName, NULL, "H5Lcreate_external: link name not pinned");

    if ((status = H5Lcreate_external(lFileName, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (lDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, lDstName);
    if (lCurName)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, lCurName);
    if (lFileName)
        UNPIN_JAVA_STRING(ENVONLY, file_name, lFileName);
} /* end Java_hdf_hdf5lib_H5_H5Lcreate_1external */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_hard
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1hard
    (JNIEnv *env, jclass clss, jlong cur_loc_id, jstring cur_name,
        jlong dst_loc_id, jstring dst_name, jlong create_id, jlong access_id)
{
    const char *lCurName = NULL;
    const char *lDstName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_hard: object name is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_hard: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, cur_name, lCurName, NULL, "H5Lcreate_hard: object name not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, lDstName, NULL, "H5Lcreate_hard: link name not pinned");

    if ((status = H5Lcreate_hard((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (lDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, lDstName);
    if (lCurName)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, lCurName);
} /* end Java_hdf_hdf5lib_H5_H5Lcreate_1hard */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lcreate_soft
 * Signature: (Ljava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lcreate_1soft
    (JNIEnv *env, jclass clss, jstring cur_name, jlong dst_loc_id,
        jstring dst_name, jlong create_id, jlong access_id)
{
    const char *linkTarget = NULL;
    const char *linkName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_soft: link target is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lcreate_soft: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, cur_name, linkTarget, NULL, "H5Lcreate_soft: link target not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, linkName, NULL, "H5Lcreate_soft: link name not pinned");

    if ((status = H5Lcreate_soft(linkTarget, (hid_t)dst_loc_id, linkName, (hid_t)create_id, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (linkName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, linkName);
    if (linkTarget)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, linkTarget);
} /* end Java_hdf_hdf5lib_H5_H5Lcreate_1soft */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ldelete
 * Signature: (JLjava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ldelete
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    const char *linkName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ldelete: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, linkName, NULL, "H5Ldelete: link name not pinned");

    if ((status = H5Ldelete((hid_t)loc_id, linkName, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (linkName)
        UNPIN_JAVA_STRING(ENVONLY, name, linkName);
} /* end Java_hdf_hdf5lib_H5_H5Ldelete */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ldelete_by_idx
 * Signature: (JLjava/lang/String;IIJJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ldelete_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name,
        jint index_field, jint order, jlong link_n, jlong access_id)
{
    const char *groupName = NULL;
    hsize_t     n = (hsize_t)link_n;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ldelete_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, groupName, NULL, "H5Ldelete_by_idx: group name not pinned");

    if ((status = H5Ldelete_by_idx((hid_t)loc_id, groupName, (H5_index_t)index_field, (H5_iter_order_t)order, n, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (groupName)
        UNPIN_JAVA_STRING(ENVONLY, name, groupName);
} /* end Java_hdf_hdf5lib_H5_H5Ldelete_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lexists
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Lexists
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    const char *linkName = NULL;
    htri_t      bval = JNI_FALSE;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lexists: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, linkName, NULL, "H5Lexists: link name not pinned");

    if ((bval = H5Lexists((hid_t)loc_id, linkName, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (linkName)
        UNPIN_JAVA_STRING(ENVONLY, name, linkName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Lexists */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_info
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5L_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1info
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    H5L_info_t  infobuf;
    const char *linkName = NULL;
    jvalue      args[5];
    herr_t      status = FAIL;
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lget_info: link name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, linkName, NULL, "H5Lget_info: link name not pinned");

    if ((status = H5Lget_info((hid_t)loc_id, linkName, &infobuf, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = infobuf.type;
    args[1].z = infobuf.corder_valid;
    args[2].j = infobuf.corder;
    args[3].i = infobuf.cset;
    args[4].j = (infobuf.type == H5L_TYPE_HARD) ? (jlong) infobuf.u.address : (jlong) infobuf.u.val_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5L_info_t", "(IZJIJ)V", args, ret_obj);

done:
    if (linkName)
        UNPIN_JAVA_STRING(ENVONLY, name, linkName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Lget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5L_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1info_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name,
        jint index_field, jint order, jlong link_n, jlong access_id)
{
    H5L_info_t  infobuf;
    const char *groupName = NULL;
    jvalue      args[5];
    herr_t      status = FAIL;
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lget_info_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, groupName, NULL, "H5Lget_info_by_idx: group name not pinned");

    if ((status = H5Lget_info_by_idx((hid_t)loc_id, groupName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, &infobuf, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = infobuf.type;
    args[1].z = infobuf.corder_valid;
    args[2].j = infobuf.corder;
    args[3].i = infobuf.cset;
    args[4].j = (infobuf.type == H5L_TYPE_HARD) ? (jlong) infobuf.u.address : (jlong) infobuf.u.val_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5L_info_t", "(IZJIJ)V", args, ret_obj);

done:
    if (groupName)
        UNPIN_JAVA_STRING(ENVONLY, name, groupName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Lget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_name_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1name_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name,
        jint index_field, jint order, jlong link_n, jlong access_id)
{
    const char *groupName = NULL;
    jstring     str = NULL;
    ssize_t     status_size = -1;
    char       *linkName = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lget_name_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, groupName, NULL, "H5Lget_name_by_idx: group name not pinned");

    /* Get the length of the link name */
    if ((status_size = H5Lget_name_by_idx((hid_t)loc_id, groupName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (char *)NULL, (size_t)0, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* add extra space for the null terminator */
    if (NULL == (linkName = (char *) HDmalloc(sizeof(char) * (size_t)status_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_name_by_idx: failed to allocate buffer for link name");

    if ((H5Lget_name_by_idx((hid_t)loc_id, groupName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (char *)linkName, (size_t)status_size + 1, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    linkName[status_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, linkName)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (linkName)
        HDfree(linkName);
    if (groupName)
        UNPIN_JAVA_STRING(ENVONLY, name, groupName);

    return str;
} /* end Java_hdf_hdf5lib_H5_H5Lget_1name_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_value
 * Signature: (JLjava/lang/String;[Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1value
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jobjectArray link_value, jlong access_id)
{
    H5L_info_t  infobuf;
    const char *file_name = NULL;
    const char *obj_name = NULL;
    const char *linkName = NULL;
    jstring     str;
    herr_t      status;
    char       *linkValue = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lget_value: link name is NULL");

    infobuf.type = H5L_TYPE_ERROR;

    PIN_JAVA_STRING(ENVONLY, name, linkName, NULL, "H5Lget_value: link name not pinned");

    /* Get the length of the link value */
    if ((status = H5Lget_info((hid_t)loc_id, linkName, &infobuf, H5P_DEFAULT)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5L_TYPE_HARD == infobuf.type)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val: hard links are unsupported");

    if (NULL == (linkValue = (char *) HDmalloc(sizeof(char) * infobuf.u.val_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val: failed to allocate buffer for link value");

    if ((status = H5Lget_val((hid_t)loc_id, linkName, (void *)linkValue, infobuf.u.val_size + 1, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    linkValue[infobuf.u.val_size] = '\0';

    switch (infobuf.type) {
        case H5L_TYPE_SOFT:
        {
            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, linkValue)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 0, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            break;
        }

        case H5L_TYPE_EXTERNAL:
        {
            if ((status = H5Lunpack_elink_val(linkValue, (size_t)infobuf.u.val_size, (unsigned *)NULL, &file_name, &obj_name)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, obj_name)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 0, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, file_name)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 1, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            break;
        }

        case H5L_TYPE_ERROR:
        case H5L_TYPE_MAX:
        case H5L_TYPE_HARD:
        default:
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val: invalid link type");
            break;
    }

done:
    if (linkValue)
        HDfree(linkValue);
    if (linkName)
        UNPIN_JAVA_STRING(ENVONLY, name, linkName);

    return infobuf.type;
} /* end Java_hdf_hdf5lib_H5_H5Lget_1val */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lget_value_by_idx
 * Signature: (JLjava/lang/String;IIJ[Ljava/lang/String;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lget_1value_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint index_field, jint order,
        jlong link_n, jobjectArray link_value, jlong access_id)
{
    H5L_info_t  infobuf;
    const char *file_name = NULL;
    const char *obj_name = NULL;
    const char *grpName = NULL;
    jstring     str;
    herr_t      status;
    void       *linkValue = NULL;

    UNUSED(clss);

    infobuf.type = H5L_TYPE_ERROR;

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lget_val_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Lget_val_by_idx: group name not pinned");

    /* Get the length of the link value */
    if ((status = H5Lget_info_by_idx((hid_t)loc_id, grpName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, &infobuf, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (H5L_TYPE_HARD == infobuf.type)
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val_by_idx: hard links are unsupported")

    if (!infobuf.u.val_size)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (linkValue = (void *) HDmalloc(infobuf.u.val_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val_by_idx: failed to allocate buffer for link value");

    if ((status = H5Lget_val_by_idx((hid_t)loc_id, grpName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (void *)linkValue, infobuf.u.val_size + 1, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    ((char *) linkValue)[infobuf.u.val_size] = '\0';

    switch (infobuf.type) {
        case H5L_TYPE_SOFT:
        {
            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, (char *)linkValue)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 0, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            break;
        }

        case H5L_TYPE_EXTERNAL:
        {
            if ((status = H5Lunpack_elink_val((char *)linkValue, (size_t)infobuf.u.val_size, (unsigned *)NULL, (const char **)&file_name, (const char**)&obj_name)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, obj_name)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 0, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, file_name)))
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            ENVPTR->SetObjectArrayElement(ENVONLY, link_value, 1, str);
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

            break;
        }

        case H5L_TYPE_ERROR:
        case H5L_TYPE_MAX:
        case H5L_TYPE_HARD:
        default:
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Lget_val_by_idx: invalid link type");
            break;
    }

done:
    if (linkValue)
        HDfree(linkValue);
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return infobuf.type;
} /* end Java_hdf_hdf5lib_H5_H5Lget_1val_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lmove
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lmove
    (JNIEnv *env, jclass clss, jlong cur_loc_id, jstring cur_name,
        jlong dst_loc_id, jstring dst_name, jlong create_id, jlong access_id)
{
    const char *lCurName = NULL;
    const char *lDstName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lmove: src name is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lmove: dest name is NULL");

    PIN_JAVA_STRING(ENVONLY, cur_name, lCurName, NULL, "H5Lmove: src name not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, lDstName, NULL, "H5Lmove: dest name not pinned");

    if ((status = H5Lmove((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (lDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, lDstName);
    if (lCurName)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, lCurName);
} /* end Java_hdf_hdf5lib_H5_H5Lmove */

static herr_t
H5L_iterate_cb
    (hid_t g_id, const char *name, const H5L_info_t *info, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jmethodID   constructor, mid;
    jobject     cb_info_t = NULL;
    jobject     visit_callback = wrapper->visit_callback;
    jstring     str;
    JNIEnv     *cbenv = NULL;
    jclass      cls;
    jvalue      args[5];
    void       *op_data = (void *)wrapper->op_data;
    jint        status = -1;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5L_iterate_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5L_info_t;Lhdf/hdf5lib/callbacks/H5L_iterate_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[0].i = info->type;
    args[1].z = info->corder_valid;
    args[2].j = info->corder;
    args[3].i = info->cset;
    args[4].j = (info->type == H5L_TYPE_HARD) ? (jlong)info->u.address : (jlong)info->u.val_size;

    /* Get a reference to your class if you don't have it already */
    if (NULL == (cls = CBENVPTR->FindClass(CBENVONLY, "hdf/hdf5lib/structs/H5L_info_t")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    /* Get a reference to the constructor; the name is <init> */
    if (NULL == (constructor = CBENVPTR->GetMethodID(CBENVONLY, cls, "<init>", "(IZJIJ)V")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (cb_info_t = CBENVPTR->NewObjectA(CBENVONLY, cls, constructor, args))) {
        HDprintf("FATAL ERROR: hdf/hdf5lib/structs/H5L_info_t: Creation failed\n");
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);
    }

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, g_id, str, cb_info_t, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5L_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lvisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lvisit
    (JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
        jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = { callback_op, op_data };
    herr_t     status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lvisit: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lvisit: callback_op is NULL");

    if ((status = H5Lvisit((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5L_iterate_t)H5L_iterate_cb, (void *)&wrapper)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return status;
} /* end Java_hdf_hdf5lib_H5_H5Lvisit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lvisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lvisit_1by_1name
    (JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type, jint order,
        jobject callback_op, jobject op_data, jlong access_id)
{
    cb_wrapper  wrapper = { callback_op, op_data };
    const char *grpName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lvisit_by_name: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lvisit_by_name: callback_op is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Lvisit_by_name: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Lvisit_by_name: group name not pinned");

    if ((status = H5Lvisit_by_name((hid_t)grp_id, grpName, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5L_iterate_t)H5L_iterate_cb, (void *)&wrapper, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return status;
} /* end Java_hdf_hdf5lib_H5_H5Lvisit_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Literate
 * Signature: (JIIJLjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Literate
    (JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
        jlong idx, jobject callback_op, jobject op_data)
{
    cb_wrapper wrapper = { callback_op, op_data };
    hsize_t    start_idx = (hsize_t)idx;
    herr_t     status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Literate: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Literate: callback_op is NULL");

    if ((status = H5Literate((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t *)&start_idx, (H5L_iterate_t)H5L_iterate_cb, (void *)&wrapper)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return status;
} /* end Java_hdf_hdf5lib_H5_H5Literate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Literate_by_name
 * Signature: (JLjava/lang/String;IIJLjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Literate_1by_1name
    (JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type, jint order,
        jlong idx, jobject callback_op, jobject op_data, jlong access_id)
{
    cb_wrapper  wrapper = { callback_op, op_data };
    const char *groupName = NULL;
    hsize_t     start_idx = (hsize_t)idx;
    herr_t      status = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Literate_by_name: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Literate_by_name: callback_op is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Literate_by_name: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, groupName, NULL, "H5Literate_by_name: group name not pinned");

    if ((status = H5Literate_by_name((hid_t)grp_id, groupName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5L_iterate_t)H5L_iterate_cb, (void*)&wrapper, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (groupName)
        UNPIN_JAVA_STRING(ENVONLY, name, groupName);

    return status;
} /* end Java_hdf_hdf5lib_H5_H5Literate_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lis_registered
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Lis_1registered
    (JNIEnv *env, jclass clss, jint link_cls_id)
{
    htri_t ret_val = FAIL;

    UNUSED(clss);

    if ((ret_val = H5Lis_registered((H5L_type_t)link_cls_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (int)ret_val;
} /* end Java_hdf_hdf5lib_H5_H5Lis_1registered */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Lunregister
 * Signature: (I)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Lunregister
    (JNIEnv *env, jclass clss, jint link_cls_id)
{
    UNUSED(clss);

    if (H5Lunregister((H5L_type_t)link_cls_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Lunregister */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
