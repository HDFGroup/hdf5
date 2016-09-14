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
#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5lImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5L_iterate_cb(hid_t g_id, const char *name, const H5L_info_t *info, void *op_data);

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
    herr_t      status = -1;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);
    if (lCurName != NULL && lDstName != NULL) {
        status = H5Lcopy((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
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
    herr_t      status = -1;
    const char *lFileName;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_THREE(file_name, lFileName, cur_name, lCurName, dst_name, lDstName);
    if (lFileName != NULL && lCurName != NULL && lDstName != NULL) {
        status = H5Lcreate_external(lFileName, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_THREE(file_name, lFileName, cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
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
    herr_t      status = -1;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);
    if (lCurName != NULL && lDstName != NULL) {
        status = H5Lcreate_hard((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
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
    herr_t      status = -1;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);
    if (lCurName != NULL && lDstName != NULL) {
        status = H5Lcreate_soft(lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
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
    herr_t      status = -1;
    const char *lName;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Ldelete((hid_t)loc_id, lName, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0)
            h5libraryError(env);
    }
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
    hsize_t     n = (hsize_t)link_n;
    herr_t      status;
    const char *lName;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Ldelete_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, n, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0)
            h5libraryError(env);
    }
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
    htri_t   bval = JNI_FALSE;
    const char *lName;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        bval = H5Lexists((hid_t)loc_id, lName, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

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
    jobject     ret_obj = NULL;
    jvalue      args[5];
    herr_t      status;
    H5L_info_t  infobuf;
    const char *lName;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Lget_info((hid_t)loc_id, lName, &infobuf, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            args[0].i = infobuf.type;
            args[1].z = infobuf.corder_valid;
            args[2].j = infobuf.corder;
            args[3].i = infobuf.cset;
            if(infobuf.type==0)
                args[4].j = (jlong)infobuf.u.address;
            else
                args[4].j = (jlong)infobuf.u.val_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5L_info_t", "(IZJIJ)V", args);
        } /* end else */
    } /* end if */
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
    jobject     ret_obj = NULL;
    jvalue      args[5];
    herr_t      status;
    H5L_info_t  infobuf;
    const char *lName;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Lget_info_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, &infobuf, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            args[0].i = infobuf.type;
            args[1].z = infobuf.corder_valid;
            args[2].j = infobuf.corder;
            args[3].i = infobuf.cset;
            if(infobuf.type==0)
                args[4].j = (jlong)infobuf.u.address;
            else
                args[4].j = (jlong)infobuf.u.val_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5L_info_t", "(IZJIJ)V", args);
        } /* end els */
    }
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
    jlong       status_size;
    jstring     str = NULL;
    size_t      buf_size;
    const char *lName;
    char       *lValue;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        /* get the length of the link name */
        status_size = H5Lget_name_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (char*)NULL, (size_t)0, H5P_DEFAULT);
        if(status_size < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            buf_size = (size_t)status_size + 1;/* add extra space for the null terminator */

            lValue = (char*)HDmalloc(sizeof(char) * buf_size);
            if (lValue == NULL) {
                h5outOfMemory(env, "H5Lget_name_by_idx:  malloc failed ");
            } /* end if */
            else {
                status_size = H5Lget_name_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (char*)lValue, (size_t)buf_size, (hid_t)access_id);

                if (status_size < 0) {
                    HDfree(lValue);
                    h5libraryError(env);
                } /* end if */
                else {
                    str = ENVPTR->NewStringUTF(ENVPAR lValue);
                    HDfree(lValue);
                    if (str == NULL)
                        h5JNIFatalError(env, "H5Lget_name_by_idx:  return string not created");
                } /* end else */
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, lName);
    }

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
    size_t      buf_size;
    herr_t      status;
    H5L_info_t  infobuf;
    const char *lName;
    char       *lValue;
    const char *file_name;
    const char *obj_name;
    jstring     str;

    infobuf.type = -1;
    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        /* get the length of the link val */
        status = H5Lget_info((hid_t)loc_id, lName, &infobuf, H5P_DEFAULT);
        if(status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            buf_size = infobuf.u.val_size + 1;/* add extra space for the null terminator */

            if(infobuf.type == H5L_TYPE_HARD) {
                h5JNIFatalError(env, "H5Lget_val:  link is hard type");
            } /* end if */
            else {
                lValue = (char*)HDmalloc(sizeof(char) * buf_size);
                if (lValue == NULL) {
                    h5outOfMemory(env, "H5Lget_val:  malloc failed");
                } /* end if */
                else {
                    status = H5Lget_val((hid_t)loc_id, lName, (void*)lValue, buf_size, (hid_t)access_id);

                    if (status < 0) {
                        h5libraryError(env);
                    } /* end if */
                    else if(infobuf.type == H5L_TYPE_EXTERNAL) {
                        status = H5Lunpack_elink_val(lValue, (size_t)infobuf.u.val_size, (unsigned*)NULL, &file_name, &obj_name);
                        if (status < 0) {
                            h5libraryError(env);
                        } /* end if */
                        else {
                            str = ENVPTR->NewStringUTF(ENVPAR obj_name);
                            if (str == NULL) {
                                h5JNIFatalError(env, "H5Lget_val:  return string not created");
                            } /* end if */
                            else {
                                ENVPTR->SetObjectArrayElement(ENVPAR link_value, 0, str);

                                str = ENVPTR->NewStringUTF(ENVPAR file_name);
                                if (str == NULL) {
                                    h5JNIFatalError(env, "H5Lget_val:  return string not created");
                                } /* end if */
                                else {
                                    ENVPTR->SetObjectArrayElement(ENVPAR link_value, 1, str);
                                } /* end else */
                            } /* end else */
                        } /* end else */
                    } /* end else if */
                    else {
                        str = ENVPTR->NewStringUTF(ENVPAR lValue);
                        if (str == NULL) {
                            /* exception -- fatal JNI error */
                            h5JNIFatalError(env, "H5Lget_val:  return string not created");
                        } /* end if */
                        else {
                            ENVPTR->SetObjectArrayElement(ENVPAR link_value, 0, str);
                        } /* end else */
                    } /* end else */
                    HDfree(lValue);
                } /* end else */
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, lName);
    }

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
    herr_t      status;
    size_t      buf_size;
    H5L_info_t  infobuf;
    const char *lName;
    void       *lValue;
    const char *file_name;
    const char *obj_name;
    jstring     str;

    infobuf.type = -1;
    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        /* get the length of the link valuee */
        status = H5Lget_info_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, &infobuf, (hid_t)access_id);
        if(status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
            buf_size = infobuf.u.val_size;
            if(buf_size < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                lValue = (void*)HDmalloc(buf_size);
                if (lValue == NULL) {
                    h5outOfMemory(env, "H5Lget_val_by_idx:  malloc failed ");
                } /* end if */
                else {
                    status = H5Lget_val_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (void*)lValue, (size_t)buf_size, (hid_t)access_id);

                    if (status < 0) {
                        h5libraryError(env);
                    } /* end if */
                    else if(infobuf.type == H5L_TYPE_EXTERNAL) {
                        status = H5Lunpack_elink_val((char*)lValue, (size_t)infobuf.u.val_size, (unsigned*)NULL, (const char**)&file_name, (const char**)&obj_name);
                        if (status < 0) {
                            h5libraryError(env);
                        } /* end if */
                        else {
                            str = ENVPTR->NewStringUTF(ENVPAR obj_name);
                            if (str == NULL) {
                                h5JNIFatalError(env, "H5Lget_val_by_idx:  return string not created");
                            } /* end if */
                            else {
                                ENVPTR->SetObjectArrayElement(ENVPAR link_value, 0, str);

                                str = ENVPTR->NewStringUTF(ENVPAR file_name);
                                if (str == NULL) {
                                    h5JNIFatalError(env, "H5Lget_val_by_idx:  return string not created");
                                } /* end if */
                                else {
                                    ENVPTR->SetObjectArrayElement(ENVPAR link_value, 1, str);
                                } /* end else */
                            } /* end else */
                        } /* end else */
                    } /* end else if */
                    else {
                        str = ENVPTR->NewStringUTF(ENVPAR (char *)lValue);
                        if (str == NULL) {
                            h5JNIFatalError(env, "H5Lget_val_by_idx:  return string not created");
                        } /* end if */
                        else {
                            ENVPTR->SetObjectArrayElement(ENVPAR link_value, 0, str);
                        } /* end else */
                    } /* end else */
                    HDfree(lValue);
                } /* end else */
            } /* end else */
        } /* end else */
        UNPIN_JAVA_STRING(name, lName);
    }

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
    herr_t   status = -1;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);
    if (lCurName != NULL && lDstName != NULL) {
        status = H5Lmove((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }

} /* end Java_hdf_hdf5lib_H5_H5Lmove */

static herr_t
H5L_iterate_cb
    (hid_t g_id, const char *name, const H5L_info_t *info, void *op_data)
{
    JNIEnv    *cbenv;
    jint       status;
    jclass     cls;
    jmethodID  mid;
    jstring    str;
    jmethodID  constructor;
    jvalue     args[5];
    jobject    cb_info_t = NULL;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) == 0) {
        cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
        if (cls != 0) {
            mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5L_info_t;Lhdf/hdf5lib/callbacks/H5L_iterate_t;)I");
            if (mid != 0) {
                str = CBENVPTR->NewStringUTF(CBENVPAR name);

                args[0].i = info->type;
                args[1].z = info->corder_valid;
                args[2].j = info->corder;
                args[3].i = info->cset;
                if(info->type==0)
                    args[4].j = (jlong)info->u.address;
                else
                    args[4].j = (jlong)info->u.val_size;
                // get a reference to your class if you don't have it already
                cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5L_info_t");
                if (cls != 0) {
                    // get a reference to the constructor; the name is <init>
                    constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(IZJIJ)V");
                    if (constructor != 0) {
                        cb_info_t = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);

                        status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, g_id, str, cb_info_t, op_data);
                    } /* end if */
                } /* end if */
            } /* end if */
        } /* end if */
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);
    return status;
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
    herr_t status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Lvisit:  op_data or callback_op is NULL");
    } /* end if */
    else {
        status = H5Lvisit((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5L_iterate_t)H5L_iterate_cb, (void*)op_data);
        if (status < 0)
            h5libraryError(env);
    } /* end else */

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
    herr_t        status = -1;
    const char   *lName;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Lvisit_by_name:  op_data or callback_op is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, lName);
        if (lName != NULL) {
            status = H5Lvisit_by_name((hid_t)grp_id, lName, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5L_iterate_t)H5L_iterate_cb, (void*)op_data, (hid_t)access_id);

            UNPIN_JAVA_STRING(name, lName);

            if (status < 0)
                h5libraryError(env);
        }
    } /* end else */

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
    hsize_t       start_idx = (hsize_t)idx;
    herr_t        status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Literate:  op_data or callback_op is NULL");
    } /* end if */
    else {
        status = H5Literate((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5L_iterate_t)H5L_iterate_cb, (void*)op_data);

        if (status < 0)
            h5libraryError(env);
    } /* end else */

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
    hsize_t       start_idx = (hsize_t)idx;
    herr_t        status = -1;
    const char   *lName;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if ((op_data == NULL) || (callback_op == NULL)) {
        h5nullArgument(env,  "H5Literate_by_name:  op_data or callback_op is NULL");
    } /* end if */
    else {
        PIN_JAVA_STRING(name, lName);
        if (lName != NULL) {
            status = H5Literate_by_name((hid_t)grp_id, lName, (H5_index_t)idx_type, (H5_iter_order_t)order, (hsize_t*)&start_idx, (H5L_iterate_t)H5L_iterate_cb, (void*)op_data, (hid_t)access_id);

            UNPIN_JAVA_STRING(name, lName);

            if (status < 0)
                h5libraryError(env);
        }
    } /* end else */

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
    htri_t ret_val = H5Lis_registered((H5L_type_t)link_cls_id);

    if (ret_val < 0)
        h5libraryError(env);

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
    if (H5Lunregister((H5L_type_t)link_cls_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Lunregister */


#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
