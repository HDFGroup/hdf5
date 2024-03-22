/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 *  For details of the HDF libraries, see the HDF Documentation at:
 *    https://portal.hdfgroup.org/documentation/index.html
 *
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5oImp.h"

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

static herr_t H5O_iterate_cb(hid_t g_id, const char *name, const H5O_info2_t *info, void *cb_data);

/*
 * Create a java object of hdf.hdf5lib.structs.H5O_token_t.
 */
jobject
create_H5O_token_t(JNIEnv *envptr, const H5O_token_t *token, bool is_critical_pinning)
{
    jbyteArray tokenByteBuf;
    jboolean   token_buf_is_copy;
    jvalue     constructor_args[1];
    jbyte     *token_buf     = NULL;
    jobject    ret_token_obj = NULL;

    if (NULL == (tokenByteBuf = (*envptr)->NewByteArray(envptr, H5O_MAX_TOKEN_SIZE)))
        CHECK_JNI_EXCEPTION(envptr, JNI_FALSE);

    if (is_critical_pinning)
        PIN_BYTE_ARRAY_CRITICAL(envptr, tokenByteBuf, token_buf, &token_buf_is_copy,
                                "create_H5O_token_t: object token buffer not pinned");
    else
        PIN_BYTE_ARRAY(envptr, tokenByteBuf, token_buf, &token_buf_is_copy,
                       "create_H5O_token_t: object token buffer not pinned");

    memcpy(token_buf, token, sizeof(H5O_token_t));

    if (is_critical_pinning)
        UNPIN_ARRAY_CRITICAL(envptr, tokenByteBuf, token_buf, 0);
    else
        UNPIN_BYTE_ARRAY(envptr, tokenByteBuf, token_buf, 0);

    token_buf = NULL;

    constructor_args[0].l = tokenByteBuf;
    CALL_CONSTRUCTOR(envptr, "hdf/hdf5lib/structs/H5O_token_t", "([B)V", constructor_args, ret_token_obj);

    /*
     * If critical pinning is being used, this routine is probably being
     * called within a loop, so we'll clean up local references.
     */
    if (is_critical_pinning)
        (*envptr)->DeleteLocalRef(envptr, tokenByteBuf);

done:
    if (token_buf) {
        if (is_critical_pinning)
            UNPIN_ARRAY_CRITICAL(envptr, tokenByteBuf, token_buf, JNI_ABORT);
        else
            UNPIN_BYTE_ARRAY(envptr, tokenByteBuf, token_buf, JNI_ABORT);
    } /* end if */

    return ret_token_obj;
} /* end create_H5O_token_t */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist_id)
{
    const char *objName = NULL;
    hid_t       status  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oopen: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oopen: object name not pinned");

    if ((status = H5Oopen((hid_t)loc_id, objName, (hid_t)access_plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Oclose(JNIEnv *env, jclass clss, jlong object_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Oclose((hid_t)object_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ocopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ocopy(JNIEnv *env, jclass clss, jlong cur_loc_id, jstring cur_name, jlong dst_loc_id,
                            jstring dst_name, jlong create_id, jlong access_id)
{
    const char *lCurName = NULL;
    const char *lDstName = NULL;
    herr_t      status   = FAIL;

    UNUSED(clss);

    if (NULL == cur_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ocopy: src name is NULL");
    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ocopy: dst name is NULL");

    PIN_JAVA_STRING(ENVONLY, cur_name, lCurName, NULL, "H5Ocopy: src name not pinned");
    PIN_JAVA_STRING(ENVONLY, dst_name, lDstName, NULL, "H5Ocopy: dest name not pinned");

    if ((status = H5Ocopy((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id,
                          (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (lDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, lDstName);
    if (lCurName)
        UNPIN_JAVA_STRING(ENVONLY, cur_name, lCurName);
} /* end Java_hdf_hdf5lib_H5_H5Ocopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info
 * Signature: (JI)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info(JNIEnv *env, jclass clss, jlong loc_id, jint fields)
{
    H5O_info2_t infobuf;
    jobject     token;
    jvalue      args[9];
    herr_t      status  = FAIL;
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if ((status = H5Oget_info3((hid_t)loc_id, &infobuf, (unsigned)fields)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (token = create_H5O_token_t(ENVONLY, &infobuf.token, false)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    args[0].j = (jlong)infobuf.fileno;
    args[1].l = token;
    args[2].i = infobuf.type;
    args[3].i = (jint)infobuf.rc;
    args[4].j = infobuf.atime;
    args[5].j = infobuf.mtime;
    args[6].j = infobuf.ctime;
    args[7].j = infobuf.btime;
    args[8].j = (jlong)infobuf.num_attrs;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_info_t",
                     "(JLhdf/hdf5lib/structs/H5O_token_t;IIJJJJJ)V", args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_name
 * Signature: (JLjava/lang/String;IJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint fields,
                                           jlong access_id)
{
    H5O_info2_t infobuf;
    const char *objName = NULL;
    jobject     token;
    jvalue      args[9];
    herr_t      status  = FAIL;
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oget_info_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oget_info_by_name: object name not pinned");

    if ((status =
             H5Oget_info_by_name3((hid_t)loc_id, objName, &infobuf, (unsigned)fields, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Create an H5O_token_t object */
    if (NULL == (token = create_H5O_token_t(ENVONLY, &infobuf.token, false)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    args[0].j = (jlong)infobuf.fileno;
    args[1].l = token;
    args[2].i = infobuf.type;
    args[3].i = (jint)infobuf.rc;
    args[4].j = infobuf.atime;
    args[5].j = infobuf.mtime;
    args[6].j = infobuf.ctime;
    args[7].j = infobuf.btime;
    args[8].j = (jlong)infobuf.num_attrs;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_info_t",
                     "(JLhdf/hdf5lib/structs/H5O_token_t;IIJJJJJ)V", args, ret_obj);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_idx
 * Signature: (JLjava/lang/String;IIJIJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx(JNIEnv *env, jclass clss, jlong loc_id, jstring name,
                                          jint index_field, jint order, jlong link_n, jint fields,
                                          jlong access_id)
{
    H5O_info2_t infobuf;
    const char *grpName = NULL;
    jobject     token;
    jvalue      args[9];
    herr_t      status  = FAIL;
    jobject     ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oget_info_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Oget_info_by_idx: group name not pinned");

    if ((status = H5Oget_info_by_idx3((hid_t)loc_id, grpName, (H5_index_t)index_field, (H5_iter_order_t)order,
                                      (hsize_t)link_n, &infobuf, (unsigned)fields, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    /* Create an H5O_token_t object */
    if (NULL == (token = create_H5O_token_t(ENVONLY, &infobuf.token, false)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    args[0].j = (jlong)infobuf.fileno;
    args[1].l = token;
    args[2].i = infobuf.type;
    args[3].i = (jint)infobuf.rc;
    args[4].j = infobuf.atime;
    args[5].j = infobuf.mtime;
    args[6].j = infobuf.ctime;
    args[7].j = infobuf.btime;
    args[8].j = (jlong)infobuf.num_attrs;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_info_t",
                     "(JLhdf/hdf5lib/structs/H5O_token_t;IIJJJJJ)V", args, ret_obj);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info
 * Signature: (JI)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1native_1info(JNIEnv *env, jclass clss, jlong loc_id, jint fields)
{
    H5O_native_info_t infobuf;
    jobject           hdrinfobuf;
    jobject           obj_ihinfobuf;
    jobject           attr_ihinfobuf;
    jvalue            args[10];
    herr_t            status  = FAIL;
    jobject           ret_obj = NULL;

    UNUSED(clss);

    if ((status = H5Oget_native_info((hid_t)loc_id, &infobuf, (unsigned)fields)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = (jint)infobuf.hdr.version;
    args[1].i = (jint)infobuf.hdr.nmesgs;
    args[2].i = (jint)infobuf.hdr.nchunks;
    args[3].i = (jint)infobuf.hdr.flags;
    args[4].j = (jlong)infobuf.hdr.space.total;
    args[5].j = (jlong)infobuf.hdr.space.meta;
    args[6].j = (jlong)infobuf.hdr.space.mesg;
    args[7].j = (jlong)infobuf.hdr.space.free;
    args[8].j = (jlong)infobuf.hdr.mesg.present;
    args[9].j = (jlong)infobuf.hdr.mesg.shared;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args, ret_obj);
    hdrinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.obj.index_size;
    args[1].j = (jlong)infobuf.meta_size.obj.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    obj_ihinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.attr.index_size;
    args[1].j = (jlong)infobuf.meta_size.attr.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    attr_ihinfobuf = ret_obj;

    args[0].l = hdrinfobuf;
    args[1].l = obj_ihinfobuf;
    args[2].l = attr_ihinfobuf;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_native_info_t",
                     "(Lhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/"
                     "structs/H5_ih_info_t;)V",
                     args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1native_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info_by_name
 * Signature: (JLjava/lang/String;IJ)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1name(JNIEnv *env, jclass clss, jlong loc_id, jstring name,
                                                   jint fields, jlong access_id)
{
    H5O_native_info_t infobuf;
    const char       *objName = NULL;
    jobject           hdrinfobuf;
    jobject           obj_ihinfobuf;
    jobject           attr_ihinfobuf;
    jvalue            args[10];
    herr_t            status  = FAIL;
    jobject           ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oget_native_info_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oget_native_info_by_name: object name not pinned");

    if ((status = H5Oget_native_info_by_name((hid_t)loc_id, objName, &infobuf, (unsigned)fields,
                                             (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = (jint)infobuf.hdr.version;
    args[1].i = (jint)infobuf.hdr.nmesgs;
    args[2].i = (jint)infobuf.hdr.nchunks;
    args[3].i = (jint)infobuf.hdr.flags;
    args[4].j = (jlong)infobuf.hdr.space.total;
    args[5].j = (jlong)infobuf.hdr.space.meta;
    args[6].j = (jlong)infobuf.hdr.space.mesg;
    args[7].j = (jlong)infobuf.hdr.space.free;
    args[8].j = (jlong)infobuf.hdr.mesg.present;
    args[9].j = (jlong)infobuf.hdr.mesg.shared;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args, ret_obj);
    hdrinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.obj.index_size;
    args[1].j = (jlong)infobuf.meta_size.obj.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    obj_ihinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.attr.index_size;
    args[1].j = (jlong)infobuf.meta_size.attr.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    attr_ihinfobuf = ret_obj;

    args[0].l = hdrinfobuf;
    args[1].l = obj_ihinfobuf;
    args[2].l = attr_ihinfobuf;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_native_info_t",
                     "(Lhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/"
                     "structs/H5_ih_info_t;)V",
                     args, ret_obj);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_native_info_by_idx
 * Signature: (JLjava/lang/String;IIJIJ)Lhdf/hdf5lib/structs/H5O_native_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1idx(JNIEnv *env, jclass clss, jlong loc_id, jstring name,
                                                  jint index_field, jint order, jlong link_n, jint fields,
                                                  jlong access_id)
{
    H5O_native_info_t infobuf;
    const char       *grpName = NULL;
    jobject           hdrinfobuf;
    jobject           obj_ihinfobuf;
    jobject           attr_ihinfobuf;
    jvalue            args[10];
    herr_t            status  = FAIL;
    jobject           ret_obj = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oget_native_info_by_idx: group name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Oget_native_info_by_idx: group name not pinned");

    if ((status = H5Oget_native_info_by_idx((hid_t)loc_id, grpName, (H5_index_t)index_field,
                                            (H5_iter_order_t)order, (hsize_t)link_n, &infobuf,
                                            (unsigned)fields, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = (jint)infobuf.hdr.version;
    args[1].i = (jint)infobuf.hdr.nmesgs;
    args[2].i = (jint)infobuf.hdr.nchunks;
    args[3].i = (jint)infobuf.hdr.flags;
    args[4].j = (jlong)infobuf.hdr.space.total;
    args[5].j = (jlong)infobuf.hdr.space.meta;
    args[6].j = (jlong)infobuf.hdr.space.mesg;
    args[7].j = (jlong)infobuf.hdr.space.free;
    args[8].j = (jlong)infobuf.hdr.mesg.present;
    args[9].j = (jlong)infobuf.hdr.mesg.shared;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args, ret_obj);
    hdrinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.obj.index_size;
    args[1].j = (jlong)infobuf.meta_size.obj.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    obj_ihinfobuf = ret_obj;

    args[0].j = (jlong)infobuf.meta_size.attr.index_size;
    args[1].j = (jlong)infobuf.meta_size.attr.heap_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args, ret_obj);
    attr_ihinfobuf = ret_obj;

    args[0].l = hdrinfobuf;
    args[1].l = obj_ihinfobuf;
    args[2].l = attr_ihinfobuf;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5O_native_info_t",
                     "(Lhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/"
                     "structs/H5_ih_info_t;)V",
                     args, ret_obj);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1native_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Olink
 * Signature: (JJLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Olink(JNIEnv *env, jclass clss, jlong cur_loc_id, jlong dst_loc_id, jstring dst_name,
                            jlong create_id, jlong access_id)
{
    const char *linkDstName = NULL;
    herr_t      status      = FAIL;

    UNUSED(clss);

    if (NULL == dst_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Olink: link destination name is NULL");

    PIN_JAVA_STRING(ENVONLY, dst_name, linkDstName, NULL, "H5Olink: link destination name not pinned");

    if ((status = H5Olink((hid_t)cur_loc_id, (hid_t)dst_loc_id, linkDstName, (hid_t)create_id,
                          (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (linkDstName)
        UNPIN_JAVA_STRING(ENVONLY, dst_name, linkDstName);
} /* end Java_hdf_hdf5lib_H5_H5Olink */

static herr_t
H5O_iterate_cb(hid_t g_id, const char *name, const H5O_info2_t *info, void *cb_data)
{
    cb_wrapper *wrapper = (cb_wrapper *)cb_data;
    jmethodID   mid;
    jobject     cb_info_t      = NULL;
    jobject     visit_callback = wrapper->visit_callback;
    jobject     token;
    jstring     str;
    JNIEnv     *cbenv = NULL;
    jclass      cbcls;
    jvalue      args[12];
    void       *op_data = (void *)wrapper->op_data;
    jint        status  = FAIL;

    if (JVMPTR->AttachCurrentThread(JVMPAR, (void **)&cbenv, NULL) < 0) {
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(CBENVONLY, "H5O_iterate_cb: failed to attach current thread to JVM");
    }

    if (NULL == (cbcls = CBENVPTR->GetObjectClass(CBENVONLY, visit_callback)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (mid = CBENVPTR->GetMethodID(CBENVONLY, cbcls, "callback",
                                             "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5O_info_t;Lhdf/"
                                             "hdf5lib/callbacks/H5O_iterate_opdata_t;)I")))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    if (NULL == (str = CBENVPTR->NewStringUTF(CBENVONLY, name)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    /* Create an H5O_token_t object */
    if (NULL == (token = create_H5O_token_t(CBENVONLY, &info->token, false)))
        CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

    args[0].j = (jlong)info->fileno;
    args[1].l = token;
    args[2].i = info->type;
    args[3].i = (jint)info->rc;
    args[4].j = info->atime;
    args[5].j = info->mtime;
    args[6].j = info->ctime;
    args[7].j = info->btime;
    args[8].j = (jlong)info->num_attrs;

    /* Get a reference to the H5O_info_t class */
    CALL_CONSTRUCTOR(CBENVONLY, "hdf/hdf5lib/structs/H5O_info_t",
                     "(JLhdf/hdf5lib/structs/H5O_token_t;IIJJJJJ)V", args, cb_info_t);

    status = CBENVPTR->CallIntMethod(CBENVONLY, visit_callback, mid, g_id, str, cb_info_t, op_data);
    CHECK_JNI_EXCEPTION(CBENVONLY, JNI_FALSE);

done:
    if (cbenv)
        JVMPTR->DetachCurrentThread(JVMPAR);

    return (herr_t)status;
} /* end H5O_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit(JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
                             jobject callback_op, jobject op_data, jint fields)
{
    cb_wrapper wrapper = {callback_op, op_data};
    herr_t     status  = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ovisit: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ovisit: callback_op is NULL");

    if ((status = H5Ovisit3((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, H5O_iterate_cb,
                            (void *)&wrapper, (unsigned)fields)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return status;
} /* end Java_hdf_hdf5lib_H5_H5Ovisit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;IJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name(JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type,
                                       jint order, jobject callback_op, jobject op_data, jint fields,
                                       jlong access_id)
{
    cb_wrapper  wrapper = {callback_op, op_data};
    const char *objName = NULL;
    herr_t      status  = FAIL;

    UNUSED(clss);

    ENVPTR->GetJavaVM(ENVONLY, &jvm);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == op_data)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ovisit_by_name: op_data is NULL");
    if (NULL == callback_op)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ovisit_by_name: callback_op is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Ovisit_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Ovisit_by_name: object name not pinned");

    if ((status = H5Ovisit_by_name3((hid_t)grp_id, objName, (H5_index_t)idx_type, (H5_iter_order_t)order,
                                    H5O_iterate_cb, (void *)&wrapper, (unsigned)fields, (hid_t)access_id)) <
        0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return status;
} /* end Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment(JNIEnv *env, jclass clss, jlong loc_id, jstring comment)
{
    const char *oComment = NULL;
    herr_t      status   = FAIL;

    UNUSED(clss);

    if (NULL != comment)
        PIN_JAVA_STRING(ENVONLY, comment, oComment, NULL, "H5Oset_comment: object comment not pinned");

    if ((status = H5Oset_comment((hid_t)loc_id, oComment)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (oComment)
        UNPIN_JAVA_STRING(ENVONLY, comment, oComment);
} /* end Java_hdf_hdf5lib_H5_H5Oset_1comment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name(JNIEnv *env, jclass clss, jlong loc_id, jstring name,
                                              jstring comment, jlong access_id)
{
    const char *objName    = NULL;
    const char *objComment = NULL;
    jboolean    isCopy;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oset_comment_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oset_comment_by_name: object name not pinned");

    if (NULL != comment)
        PIN_JAVA_STRING(ENVONLY, comment, objComment, &isCopy,
                        "H5Oset_comment_by_name: object comment not pinned");

    if ((status = H5Oset_comment_by_name((hid_t)loc_id, objName, objComment, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (objComment)
        UNPIN_JAVA_STRING(ENVONLY, comment, objComment);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);
} /* end Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment(JNIEnv *env, jclass clss, jlong loc_id)
{
    jstring str = NULL;
    ssize_t buf_size;
    ssize_t status   = -1;
    char   *oComment = NULL;

    UNUSED(clss);

    /* Get the length of the comment */
    if ((buf_size = H5Oget_comment((hid_t)loc_id, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (buf_size) {
        if (NULL == (oComment = (char *)malloc(sizeof(char) * (size_t)buf_size + 1)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY, "H5Oget_comment: failed to allocate object comment buffer");

        if ((status = H5Oget_comment((hid_t)loc_id, oComment, (size_t)buf_size + 1)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
        oComment[buf_size] = '\0';

        if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, oComment)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (oComment)
        free(oComment);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1comment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment_by_name
 * Signature: (JLjava/lang/String;J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name(JNIEnv *env, jclass clss, jlong loc_id, jstring name,
                                              jlong access_id)
{
    const char *objName = NULL;
    jstring     str     = NULL;
    ssize_t     buf_size;
    ssize_t     status;
    char       *objComment = NULL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oget_comment_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oget_comment_by_name: object name not pinned");

    /* Get the length of the comment */
    if ((buf_size = H5Oget_comment_by_name((hid_t)loc_id, objName, NULL, 0, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (buf_size) {
        if (NULL == (objComment = (char *)malloc(sizeof(char) * (size_t)buf_size + 1)))
            H5_OUT_OF_MEMORY_ERROR(ENVONLY,
                                   "H5Oget_comment_by_name: failed to allocate buffer for object comment");

        if ((status = H5Oget_comment_by_name((hid_t)loc_id, objName, objComment, (size_t)buf_size + 1,
                                             (hid_t)access_id)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
        objComment[buf_size] = '\0';

        if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, objComment)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (objComment)
        free(objComment);
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oexists_by_name
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Oexists_1by_1name(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    const char *objName = NULL;
    htri_t      bval    = JNI_FALSE;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oexists_by_name: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, objName, NULL, "H5Oexists_by_name: object name not pinned");

    if ((bval = H5Oexists_by_name((hid_t)loc_id, objName, (hid_t)access_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    if (objName)
        UNPIN_JAVA_STRING(ENVONLY, name, objName);

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Oexists_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odecr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Odecr_1refcount(JNIEnv *env, jclass clss, jlong object_id)
{
    UNUSED(clss);

    if (H5Odecr_refcount((hid_t)object_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
}
/* end Java_hdf_hdf5lib_H5_H5Odecr_1refcount */
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oincr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oincr_1refcount(JNIEnv *env, jclass clss, jlong object_id)
{
    UNUSED(clss);

    if (H5Oincr_refcount((hid_t)object_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Oincr_1refcount */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_token
 * Signature: (JLhdf/hdf5lib/structs/H5O_token_t;)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1token(JNIEnv *env, jclass clss, jlong loc_id, jobject token)
{
    H5O_token_t obj_token;
    jboolean    token_buf_is_copy;
    jfieldID    token_data_field_id;
    jclass      token_cls;
    jbyte      *token_buf = NULL;
    jobject     token_data;
    hid_t       retVal = H5I_INVALID_HID;

    UNUSED(clss);

    token_cls = ENVPTR->GetObjectClass(ENVONLY, token);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (token_data_field_id = ENVPTR->GetFieldID(ENVONLY, token_cls, "data", "[B")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    token_data = ENVPTR->GetObjectField(ENVONLY, token, token_data_field_id);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    PIN_BYTE_ARRAY(ENVONLY, (jbyteArray)token_data, token_buf, &token_buf_is_copy,
                   "H5Oopen_by_token: token buffer not pinned");
    memcpy(&obj_token, token_buf, sizeof(H5O_token_t));
    UNPIN_BYTE_ARRAY(ENVONLY, (jbyteArray)token_data, token_buf, JNI_ABORT);
    token_buf = NULL;

    if ((retVal = H5Oopen_by_token((hid_t)loc_id, obj_token)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (token_buf)
        UNPIN_BYTE_ARRAY(ENVONLY, (jbyteArray)token_data, token_buf, (retVal < 0) ? JNI_ABORT : 0);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen_1by_1token */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_idx
 * Signature: (JLjava/lang/String;IIJJ)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx(JNIEnv *env, jclass clss, jlong loc_id, jstring name, jint index_field,
                                       jint order, jlong link_n, jlong lapl_id)
{
    const char *grpName = NULL;
    hid_t       retVal  = H5I_INVALID_HID;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Oopen_by_idx: object name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, grpName, NULL, "H5Oopen_by_idx: object name not pinned");

    if ((retVal = H5Oopen_by_idx((hid_t)loc_id, grpName, (H5_index_t)index_field, (H5_iter_order_t)order,
                                 (hsize_t)link_n, (hid_t)lapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (grpName)
        UNPIN_JAVA_STRING(ENVONLY, name, grpName);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oflush(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Oflush((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Oflush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Orefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Orefresh(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Orefresh((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Orefresh */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odisable_mdc_flushes
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Odisable_1mdc_1flushes(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Odisable_mdc_flushes((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Odisable_1mdc_1flushes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oenable_mdc_flushes
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oenable_1mdc_1flushes(JNIEnv *env, jclass clss, jlong loc_id)
{
    UNUSED(clss);

    if (H5Oenable_mdc_flushes((hid_t)loc_id) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Oenable_1mdc_1flushes */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oare_mdc_flushes_disabled
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Oare_1mdc_1flushes_1disabled(JNIEnv *env, jclass clss, jlong loc_id)
{
    jboolean bval        = JNI_FALSE;
    bool     is_disabled = false;

    UNUSED(clss);

    if (H5Oare_mdc_flushes_disabled((hid_t)loc_id, &is_disabled) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (is_disabled == true)
        bval = JNI_TRUE;

done:
    return bval;
} /* end Java_hdf_hdf5lib_H5_H5Oare_1mdc_1flushes_1disabled */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
