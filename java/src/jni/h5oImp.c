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

#include <stdlib.h>
#include "hdf5.h"
#include "h5jni.h"
#include "h5oImp.h"

extern JavaVM *jvm;
extern jobject visit_callback;

/********************/
/* Local Prototypes */
/********************/

static herr_t H5O_iterate_cb(hid_t g_id, const char *name, const H5O_info_t *info, void *op_data);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen
 * Signature: (JLjava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_plist_id)
{
    hid_t       status = -1;
    const char *oName;

    PIN_JAVA_STRING(name, oName);
    if (oName != NULL) {
        status = H5Oopen((hid_t)loc_id, oName, (hid_t)access_plist_id );

        UNPIN_JAVA_STRING(name, oName);

        if (status < 0)
            h5libraryError(env);
    }

    return (jlong)status;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Oclose
    (JNIEnv *env, jclass clss, jlong object_id)
{
    herr_t retVal = H5Oclose((hid_t)object_id);

    if (retVal < 0)
        h5libraryError(env);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oclose */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ocopy
 * Signature: (JLjava/lang/String;JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Ocopy
    (JNIEnv *env, jclass clss, jlong cur_loc_id, jstring cur_name,
        jlong dst_loc_id, jstring dst_name, jlong create_id, jlong access_id)
{
    herr_t      status = -1;
    const char *lCurName;
    const char *lDstName;

    PIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);
    if (lCurName != NULL && lDstName != NULL) {
        status = H5Ocopy((hid_t)cur_loc_id, lCurName, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING_TWO(cur_name, lCurName, dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Ocopy */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    herr_t      status = -1;
    H5O_info_t  infobuf;
    jvalue      args[12];
    jobject     hdrinfobuf;
    jobject     ihinfobuf1;
    jobject     ihinfobuf2;
    jobject     ret_obj = NULL;

    status = H5Oget_info((hid_t)loc_id, &infobuf);

    if (status < 0) {
        h5libraryError(env);
    } /* end if */
    else {
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
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args);
        hdrinfobuf = ret_obj;

        args[0].j = (jlong)infobuf.meta_size.obj.index_size;
        args[1].j = (jlong)infobuf.meta_size.obj.heap_size;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
        ihinfobuf1 = ret_obj;
        args[0].j = (jlong)infobuf.meta_size.attr.index_size;
        args[1].j = (jlong)infobuf.meta_size.attr.heap_size;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
        ihinfobuf2 = ret_obj;

        args[0].j = (jlong)infobuf.fileno;
        args[1].j = (jlong)infobuf.addr;
        args[2].i = infobuf.type;
        args[3].i = (jint)infobuf.rc;
        args[4].j = (jlong)infobuf.num_attrs;
        args[5].j = infobuf.atime;
        args[6].j = infobuf.mtime;
        args[7].j = infobuf.ctime;
        args[8].j = infobuf.btime;
        args[9].l = hdrinfobuf;
        args[10].l = ihinfobuf1;
        args[11].l = ihinfobuf2;
        CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_info_t", "(JJIIJJJJJLhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;)V", args);
    }

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_name
 * Signature: (JLjava/lang/String;J)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    const char *lName;
    herr_t      status = -1;
    H5O_info_t  infobuf;
    jvalue      args[12];
    jobject     hdrinfobuf;
    jobject     ihinfobuf1;
    jobject     ihinfobuf2;
    jobject     ret_obj = NULL;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Oget_info_by_name((hid_t)loc_id, lName, &infobuf, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
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
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args);
            hdrinfobuf = ret_obj;

            args[0].j = (jlong)infobuf.meta_size.obj.index_size;
            args[1].j = (jlong)infobuf.meta_size.obj.heap_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
            ihinfobuf1 = ret_obj;
            args[0].j = (jlong)infobuf.meta_size.attr.index_size;
            args[1].j = (jlong)infobuf.meta_size.attr.heap_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
            ihinfobuf2 = ret_obj;

            args[0].j = (jlong)infobuf.fileno;
            args[1].j = (jlong)infobuf.addr;
            args[2].i = infobuf.type;
            args[3].i = (jint)infobuf.rc;
            args[4].j = (jlong)infobuf.num_attrs;
            args[5].j = infobuf.atime;
            args[6].j = infobuf.mtime;
            args[7].j = infobuf.ctime;
            args[8].j = infobuf.btime;
            args[9].l = hdrinfobuf;
            args[10].l = ihinfobuf1;
            args[11].l = ihinfobuf2;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_info_t", "(JJIIJJJJJLhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;)V", args);
        }
    }

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_info_by_idx
 * Signature: (JLjava/lang/String;IIJJ)Lhdf/hdf5lib/structs/H5O_info_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id,
        jstring name, jint index_field, jint order, jlong link_n, jlong access_id)
{
    const char *lName;
    herr_t      status;
    H5O_info_t  infobuf;
    jvalue      args[12];
    jobject     hdrinfobuf;
    jobject     ihinfobuf1;
    jobject     ihinfobuf2;
    jobject     ret_obj = NULL;

    PIN_JAVA_STRING(name, lName);
    if (lName != NULL) {
        status = H5Oget_info_by_idx((hid_t)loc_id, lName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, &infobuf, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, lName);

        if (status < 0) {
            h5libraryError(env);
        } /* end if */
        else {
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
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_hdr_info_t", "(IIIIJJJJJJ)V", args);
            hdrinfobuf = ret_obj;

            args[0].j = (jlong)infobuf.meta_size.obj.index_size;
            args[1].j = (jlong)infobuf.meta_size.obj.heap_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
            ihinfobuf1 = ret_obj;
            args[0].j = (jlong)infobuf.meta_size.attr.index_size;
            args[1].j = (jlong)infobuf.meta_size.attr.heap_size;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5_ih_info_t", "(JJ)V", args);
            ihinfobuf2 = ret_obj;

            args[0].j = (jlong)infobuf.fileno;
            args[1].j = (jlong)infobuf.addr;
            args[2].i = infobuf.type;
            args[3].i = (jint)infobuf.rc;
            args[4].j = (jlong)infobuf.num_attrs;
            args[5].j = infobuf.atime;
            args[6].j = infobuf.mtime;
            args[7].j = infobuf.ctime;
            args[8].j = infobuf.btime;
            args[9].l = hdrinfobuf;
            args[10].l = ihinfobuf1;
            args[11].l = ihinfobuf2;
            CALL_CONSTRUCTOR("hdf/hdf5lib/structs/H5O_info_t", "(JJIIJJJJJLhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;)V", args);
        }
    }

    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1info_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Olink
 * Signature: (JJLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Olink
    (JNIEnv *env, jclass clss, jlong cur_loc_id, jlong dst_loc_id,
        jstring dst_name, jlong create_id, jlong access_id)
{
    herr_t      status = -1;
    const char *lDstName;

    PIN_JAVA_STRING(dst_name, lDstName);
    if (lDstName != NULL) {
        status = H5Olink((hid_t)cur_loc_id, (hid_t)dst_loc_id, lDstName, (hid_t)create_id, (hid_t)access_id);

        UNPIN_JAVA_STRING(dst_name, lDstName);

        if (status < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Olink */

static herr_t
H5O_iterate_cb
    (hid_t g_id, const char *name, const H5O_info_t *info, void *op_data)
{
    JNIEnv    *cbenv;
    jint       status = -1;
    jclass     cls;
    jmethodID  mid;
    jstring    str;
    jmethodID  constructor;
    jvalue     args[12];
    jobject    hdrinfobuf;
    jobject    ihinfobuf1;
    jobject    ihinfobuf2;
    jobject    cb_info_t = NULL;

    if(JVMPTR->AttachCurrentThread(JVMPAR2 (void**)&cbenv, NULL) != 0) {
        /* printf("JNI H5O_iterate_cb error: AttachCurrentThread failed\n"); */
        JVMPTR->DetachCurrentThread(JVMPAR);
        return -1;
    } /* end if */
    cls = CBENVPTR->GetObjectClass(CBENVPAR visit_callback);
    if (cls != 0) {
        mid = CBENVPTR->GetMethodID(CBENVPAR cls, "callback", "(JLjava/lang/String;Lhdf/hdf5lib/structs/H5O_info_t;Lhdf/hdf5lib/callbacks/H5O_iterate_t;)I");
        if (mid != 0) {
            str = CBENVPTR->NewStringUTF(CBENVPAR name);

            args[0].i = (jint)info->hdr.version;
            args[1].i = (jint)info->hdr.nmesgs;
            args[2].i = (jint)info->hdr.nchunks;
            args[3].i = (jint)info->hdr.flags;
            args[4].j = (jlong)info->hdr.space.total;
            args[5].j = (jlong)info->hdr.space.meta;
            args[6].j = (jlong)info->hdr.space.mesg;
            args[7].j = (jlong)info->hdr.space.free;
            args[8].j = (jlong)info->hdr.mesg.present;
            args[9].j = (jlong)info->hdr.mesg.shared;
            // get a reference to the H5_hdr_info_t class
            cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5O_hdr_info_t");
            if (cls != 0) {
                // get a reference to the constructor; the name is <init>
                constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(IIIIJJJJJJ)V");
                if (constructor != 0) {
                    hdrinfobuf = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);

                    args[0].j = (jlong)info->meta_size.obj.index_size;
                    args[1].j = (jlong)info->meta_size.obj.heap_size;
                    // get a reference to the H5_ih_info_t class
                    cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5_ih_info_t");
                    if (cls != 0) {
                        // get a reference to the constructor; the name is <init>
                        constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(JJ)V");
                        if (constructor != 0) {
                            ihinfobuf1 = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);
                            args[0].j = (jlong)info->meta_size.attr.index_size;
                            args[1].j = (jlong)info->meta_size.attr.heap_size;
                            ihinfobuf2 = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);

                            args[0].j = (jlong)info->fileno;
                            args[1].j = (jlong)info->addr;
                            args[2].i = info->type;
                            args[3].i = (jint)info->rc;
                            args[4].j = (jlong)info->num_attrs;
                            args[5].j = info->atime;
                            args[6].j = info->mtime;
                            args[7].j = info->ctime;
                            args[8].j = info->btime;
                            args[9].l = hdrinfobuf;
                            args[10].l = ihinfobuf1;
                            args[11].l = ihinfobuf2;
                            // get a reference to the H5O_info_t class
                            cls = CBENVPTR->FindClass(CBENVPAR "hdf/hdf5lib/structs/H5O_info_t");
                            if (cls != 0) {
                                // get a reference to the constructor; the name is <init>
                                constructor = CBENVPTR->GetMethodID(CBENVPAR cls, "<init>", "(JJIIJJJJJLhdf/hdf5lib/structs/H5O_hdr_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;Lhdf/hdf5lib/structs/H5_ih_info_t;)V");
                                if (constructor != 0) {
                                    cb_info_t = CBENVPTR->NewObjectA(CBENVPAR cls, constructor, args);

                                    status = CBENVPTR->CallIntMethod(CBENVPAR visit_callback, mid, g_id, str, cb_info_t, op_data);
                                }
                            }
                        }
                    }
                }
            }
        }
    } /* end if */
    JVMPTR->DetachCurrentThread(JVMPAR);

    return status;
} /* end H5O_iterate_cb */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit
 * Signature: (JIILjava/lang/Object;Ljava/lang/Object;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit
    (JNIEnv *env, jclass clss, jlong grp_id, jint idx_type, jint order,
        jobject callback_op, jobject op_data)
{
    herr_t   status = -1;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if (op_data == NULL) {
        h5nullArgument(env, "H5Ovisit:  op_data is NULL");
    } /* end if */
    else if (callback_op == NULL) {
        h5nullArgument(env, "H5Ovisit:  callback_op is NULL");
    } /* end if */
    else {
        status = H5Ovisit((hid_t)grp_id, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5O_iterate_t)H5O_iterate_cb, (void*)op_data);

        if (status < 0)
            h5libraryError(env);
    }

    return status;
} /* end Java_hdf_hdf5lib_H5_H5Ovisit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Ovisit_by_name
 * Signature: (JLjava/lang/String;IILjava/lang/Object;Ljava/lang/Object;J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name
    (JNIEnv *env, jclass clss, jlong grp_id, jstring name, jint idx_type, jint order,
        jobject callback_op, jobject op_data, jlong access_id)
{
    herr_t        status = -1;
    const char   *lName;

    ENVPTR->GetJavaVM(ENVPAR &jvm);
    visit_callback = callback_op;

    if (op_data == NULL) {
        h5nullArgument(env, "H5Ovisit_by_name:  op_data is NULL");
        return -1;
    } /* end if */
    else if (callback_op == NULL) {
        h5nullArgument(env, "H5Ovisit_by_name:  callback_op is NULL");
        return -1;
    } /* end if */
    else {
        PIN_JAVA_STRING(name, lName);
        if (lName != NULL) {
            status = H5Ovisit_by_name((hid_t)grp_id, lName, (H5_index_t)idx_type, (H5_iter_order_t)order, (H5O_iterate_t)H5O_iterate_cb, (void*)op_data, (hid_t)access_id);

            UNPIN_JAVA_STRING(name, lName);

            if (status < 0)
                h5libraryError(env);
        }
    }

    return status;
} /* end Java_hdf_hdf5lib_H5_H5Ovisit_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment
    (JNIEnv *env, jclass clss, jlong loc_id, jstring comment)
{
    herr_t      status = -1;
    const char *oComment = NULL;
    jboolean    isCopy;

    if (comment == NULL) {
        status = H5Oset_comment((hid_t)loc_id, oComment);
    } /* end if */
    else {
        oComment = ENVPTR->GetStringUTFChars(ENVPAR comment, &isCopy);
        if (oComment != NULL) {
            status = H5Oset_comment((hid_t)loc_id, oComment);

            ENVPTR->ReleaseStringUTFChars(ENVPAR comment, oComment);
        }
    } /* end else */

    if (status < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Oset_1comment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oset_comment_by_name
 * Signature: (JLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id,
        jstring name, jstring comment, jlong access_id)
{
    herr_t      status = -1;
    const char *oName;
    const char *oComment;

    PIN_JAVA_STRING(name, oName);
    if (oName != NULL) {
        if (comment == NULL) {
            status = H5Oset_comment_by_name((hid_t)loc_id, oName, NULL, (hid_t)access_id);
        } /* end if */
        else {
            jboolean    isCopy;
            oComment = ENVPTR->GetStringUTFChars(ENVPAR comment, &isCopy);
            if (oComment != NULL) {
                status = H5Oset_comment_by_name((hid_t)loc_id, oName, oComment, (hid_t)access_id);
                ENVPTR->ReleaseStringUTFChars(ENVPAR comment, oComment);
            } /* end if */
        } /* end else */

        UNPIN_JAVA_STRING(name, oName);

        if (status < 0)
            h5libraryError(env);
    }
} /* end Java_hdf_hdf5lib_H5_H5Oset_1comment_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    char    *oComment;
    ssize_t  buf_size;
    ssize_t  status;
    jstring  str = NULL;

    /* get the length of the comment */
    buf_size = H5Oget_comment((hid_t)loc_id, NULL, 0);
    if (buf_size < 0) {
        h5badArgument( env, "H5Oget_comment:  buf_size < 0");
    } /* end if */
    else if (buf_size > 0) {
        buf_size++; /* add extra space for the null terminator */
        oComment = (char *)HDmalloc(sizeof(char) * (size_t)buf_size);
        if (oComment == NULL) {
            /* exception -- out of memory */
            h5outOfMemory( env, "H5Oget_comment:  malloc failed");
        } /* end if */
        else {
            status = H5Oget_comment((hid_t)loc_id, oComment, (size_t)buf_size);

            if (status < 0) {
                h5libraryError(env);
            } /* end if */
            else {
                /*  may throw OutOfMemoryError */
                str = ENVPTR->NewStringUTF(ENVPAR oComment);
                if (str == NULL) {
                    h5JNIFatalError( env, "H5Oget_comment:  return string not allocated");
                } /* end if */
            } /* end else */
            HDfree(oComment);
        }
    } /* end else if */

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1comment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oget_comment_by_name
 * Signature: (JLjava/lang/String;J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    char       *oComment;
    const char *oName;
    ssize_t     buf_size;
    ssize_t     status;
    jstring     str = NULL;

    PIN_JAVA_STRING(name, oName);
    if (oName != NULL) {
        /* get the length of the comment */
        buf_size = H5Oget_comment_by_name((hid_t)loc_id, oName, NULL, 0, (hid_t)access_id);
        if (buf_size < 0) {
            h5badArgument( env, "H5Oget_comment_by_name:  buf_size < 0");
        } /* end if */
        else if (buf_size > 0) {
            buf_size++; /* add extra space for the null terminator */
            oComment = (char *)HDmalloc(sizeof(char) * (size_t)buf_size);
            if (oComment == NULL) {
                h5outOfMemory( env, "H5Oget_comment_by_name:  malloc failed");
            } /* end if */
            else {
                status = H5Oget_comment_by_name((hid_t)loc_id, oName, oComment, (size_t)buf_size, (hid_t)access_id);

                if (status < 0) {
                    h5libraryError(env);
                } /* end if */
                else {
                    /*  may throw OutOfMemoryError */
                    str = ENVPTR->NewStringUTF(ENVPAR oComment);
                    if (str == NULL) {
                        h5JNIFatalError( env, "H5Oget_comment_by_name:  return string not allocated");
                    } /* end if */
                } /* end else */
                HDfree(oComment);
            }
        } /* end if */
        UNPIN_JAVA_STRING(name, oName);
    }

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Oget_1comment_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oexists_by_name
 * Signature: (JLjava/lang/String;J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Oexists_1by_1name
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name, jlong access_id)
{
    htri_t      bval = JNI_FALSE;
    const char *oName;

    PIN_JAVA_STRING(name, oName);
    if (oName != NULL) {
        bval = H5Oexists_by_name((hid_t)loc_id, oName, (hid_t)access_id);

        UNPIN_JAVA_STRING(name, oName);

        if (bval > 0)
            bval = JNI_TRUE;
        else if (bval < 0)
            h5libraryError(env);
    }

    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Oexists_1by_1name */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Odecr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Odecr_1refcount
    (JNIEnv *env, jclass clss, jlong object_id)
{
    if (H5Odecr_refcount((hid_t)object_id) < 0)
        h5libraryError(env);
}
/* end Java_hdf_hdf5lib_H5_H5Odecr_1refcount */
/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oincr_refcount
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oincr_1refcount
    (JNIEnv *env, jclass clss, jlong object_id)
{
    if (H5Oincr_refcount((hid_t)object_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Oincr_1refcount */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_addr
 * Signature: (JJ)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1addr
    (JNIEnv *env, jclass clss, jlong loc_id, jlong addr)
{
    hid_t retVal = -1;

    retVal = H5Oopen_by_addr((hid_t)loc_id, (haddr_t)addr );
    if (retVal < 0)
        h5libraryError(env);

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen_1by_1addr */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    _H5Oopen_by_idx
 * Signature: (JLjava/lang/String;IIJJ)J;
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx
    (JNIEnv *env, jclass clss, jlong loc_id, jstring name,
        jint index_field, jint order, jlong link_n, jlong lapl_id)
{
    hid_t       retVal = -1;
    const char *oName;

    PIN_JAVA_STRING(name, oName);
    if (oName != NULL) {
        retVal = H5Oopen_by_idx((hid_t)loc_id, oName, (H5_index_t)index_field, (H5_iter_order_t)order, (hsize_t)link_n, (hid_t)lapl_id );

        UNPIN_JAVA_STRING(name, oName);

        if (retVal < 0)
            h5libraryError(env);
    }

    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5__1H5Oopen_1by_1idx */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Oflush
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Oflush
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    if (H5Oflush((hid_t)loc_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Oflush */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Orefresh
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Orefresh
    (JNIEnv *env, jclass clss, jlong loc_id)
{
    if (H5Orefresh((hid_t)loc_id) < 0)
        h5libraryError(env);
} /* end Java_hdf_hdf5lib_H5_H5Orefresh */



#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
