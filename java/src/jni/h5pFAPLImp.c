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
#include "h5jni.h"
#include "h5util.h"
#include "h5pFAPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * TODO: H5Pset_driver
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_driver
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1driver
    (JNIEnv *env, jclass clss, jlong plist)
{
    hid_t retVal = H5I_INVALID_HID;

    UNUSED(clss);

    if ((retVal = H5Pget_driver((hid_t) plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1driver */

/*
 * TODO: H5Pget_driver_info
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fclose_degree
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fclose_1degree
    (JNIEnv *env, jclass clss, jlong plist, jint fc_degree)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fclose_degree((hid_t)plist, (H5F_close_degree_t)fc_degree)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fclose_1degree */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fclose_degree
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fclose_1degree
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5F_close_degree_t degree = H5F_CLOSE_DEFAULT;

    UNUSED(clss);

    if (H5Pget_fclose_degree((hid_t)plist, &degree) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)degree;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fclose_1degree */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_core
 * Signature: (JJZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1core
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong increment, jboolean backing_store)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fapl_core((hid_t)fapl_id, (size_t)increment, (hbool_t)backing_store)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1core */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_core
 * Signature: (J[J[Z)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1core
    (JNIEnv *env, jclass clss, jlong fapl_id, jlongArray increment, jbooleanArray backing_store)
{
    jboolean  isCopy;
    jboolean *backArray = NULL;
    jlong    *incArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == increment)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_fapl_core: increment is NULL");
    if (NULL == backing_store)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_fapl_core: backing_store is NULL");

    PIN_LONG_ARRAY(ENVONLY, increment, incArray, &isCopy, "H5Pget_fapl_core: incArray not pinned");
    PIN_BOOL_ARRAY(ENVONLY, backing_store, backArray, &isCopy, "H5Pget_fapl_core: backArray not pinned");

    {
        /* Direct cast (size_t *)variable fails on 32-bit environment */
        long long inc_temp = *incArray;
        size_t    inc_t = (size_t) inc_temp;

        if ((status = H5Pget_fapl_core((hid_t)fapl_id, &inc_t, (hbool_t *)backArray)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        *incArray = (jlong)inc_t;
    }

done:
    if (backArray)
        UNPIN_BOOL_ARRAY(ENVONLY, backing_store, backArray, (status < 0) ? JNI_ABORT : 0);
    if (incArray)
        UNPIN_LONG_ARRAY(ENVONLY, increment, incArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1core */

/*
 * TODO: H5Pset_core_write_tracking
 */

/*
 * TODO: H5Pget_core_write_tracking
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_direct
 * Signature: (JJJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1direct
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong alignment,
        jlong block_size, jlong cbuf_size)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

#ifdef H5_HAVE_DIRECT
    if ((retVal = H5Pset_fapl_direct((hid_t)fapl_id, (size_t)alignment, (size_t)block_size, (size_t)cbuf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
#else
    UNUSED(env);
    UNUSED(fapl_id);
    UNUSED(alignment);
    UNUSED(block_size);
    UNUSED(cbuf_size);
#endif

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1direct */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_direct
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1direct
    (JNIEnv *env, jclass clss, jlong fapl_id, jlongArray info)
{
#ifdef H5_HAVE_DIRECT
    jboolean  isCopy;
    jlong    *theArray = NULL;
    size_t    alignment = 0;
    size_t    block_size = 0;
    size_t    cbuf_size = 0;
    jsize     arrLen;
#endif
    herr_t    retVal = FAIL;

    UNUSED(clss);

#ifdef H5_HAVE_DIRECT
    if (NULL == info)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_fapl_direct: info input array is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, info)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_fapl_direct: info array length < 0");
    }
    if (arrLen < 3)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_fapl_direct: info input array < 3");

    PIN_LONG_ARRAY(ENVONLY, info, theArray, &isCopy, "H5Pget_fapl_direct: info not pinned");

    if ((retVal = H5Pget_fapl_direct((hid_t)fapl_id, &alignment, &block_size, &cbuf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jlong)alignment;
    theArray[1] = (jlong)block_size;
    theArray[2] = (jlong)cbuf_size;

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, info, theArray, (retVal < 0) ? JNI_ABORT : 0);
#else
    UNUSED(env);
    UNUSED(fapl_id);
    UNUSED(info);
#endif

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1direct */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_family
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1family
    (JNIEnv *env, jclass clss, jlong plist, jlong memb_size, jlong memb_plist)
{
    long   ms = (long)memb_size;
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fapl_family((hid_t)plist, (hsize_t)ms, (hid_t)memb_plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1family */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_family
 * Signature: (J[J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1family
    (JNIEnv *env, jclass clss, jlong tid, jlongArray memb_size, jlongArray memb_plist)
{
    jboolean  isCopy;
    hsize_t  *sa = NULL;
    size_t    i;
    jlong    *sizeArray = NULL;
    jlong    *plistArray = NULL;
    jsize     rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == memb_size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_family: memb_size is NULL");
    if (NULL == memb_plist)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_family: memb_plist is NULL");

    PIN_LONG_ARRAY(ENVONLY, memb_size, sizeArray, &isCopy, "H5Pget_family: sizeArray not pinned");

    if ((rank = ENVPTR->GetArrayLength(ENVONLY, memb_size)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_family: memb_size array length < 0");
    }

    if (NULL == (sa = (hsize_t *) HDmalloc((size_t) rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_family: memory allocation failed");

    PIN_LONG_ARRAY(ENVONLY, memb_plist, plistArray, &isCopy, "H5Pget_family: plistArray not pinned");

    if ((status = H5Pget_fapl_family((hid_t)tid, sa, (hid_t *)plistArray)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < (size_t) rank; i++)
        sizeArray[i] = (jlong)sa[i];

done:
    if (plistArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_plist, plistArray, (status < 0) ? JNI_ABORT : 0);
    if (sa)
        HDfree(sa);
    if (sizeArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_size, sizeArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1family */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_family_offset
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1family_1offset
    (JNIEnv *env, jclass clss, jlong fapl_id, jlong offset)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_family_offset((hid_t)fapl_id, (hsize_t)offset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1family_1offset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_family_offset
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1family_1offset
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    hsize_t offset = 0;
    herr_t  retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pget_family_offset((hid_t)fapl_id, &offset)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)offset;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1family_1offset */

/* Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_hdfs
 * Signature: (J)Lhdf/hdf5lib/structs/H5FD_hdfs_fapl_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1hdfs
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
#ifdef H5_HAVE_LIBHDFS
    H5FD_hdfs_fapl_t fa;
    jvalue           args[5];
    jint             j_namenode_port      = 0;
    jstring          j_namenode_name      = NULL;
    jstring          j_user_name          = NULL;
    jstring          j_kerb_cache_path    = NULL;
    jint             j_stream_buffer_size = 0;
#endif /* H5_HAVE_LIBHDFS */
    jobject          ret_obj              = NULL;

    UNUSED(clss);

#ifdef H5_HAVE_LIBHDFS
    if (H5Pget_fapl_hdfs((hid_t)fapl_id, &fa) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL != fa.namenode_name) {
        if (NULL == (j_namenode_name = ENVPTR->NewStringUTF(ENVONLY, fa.namenode_name))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_hdfs: out of memory - can't create namenode_name string");
        }
    }
    args[0].l = j_namenode_name;

    args[1].i = (jint)fa.namenode_port;

    if (NULL != fa.user_name) {
        if (NULL == (j_user_name = ENVPTR->NewStringUTF(ENVONLY, fa.user_name))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_hdfs: out of memory - can't create user_name string");
        }
    }
    args[2].l = j_user_name;

    if (NULL != fa.kerberos_ticket_cache) {
        if (NULL == (j_kerb_cache_path = ENVPTR->NewStringUTF(ENVONLY, fa.kerberos_ticket_cache))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_hdfs: out of memory - can't create kerberos_ticket_cache string");
        }
    }
    args[3].l = j_kerb_cache_path;

    args[4].i = (jint)fa.stream_buffer_size;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5FD_hdfs_fapl_t", "(Ljava/lang/String;ILjava/lang/String;Ljava/lang/String;I)V", args, ret_obj);
#else
    H5_UNIMPLEMENTED(ENVONLY, "H5Pget_fapl_hdfs: not implemented");
#endif /* H5_HAVE_LIBHDFS */

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1hdfs */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_hdfs
 * Signature: (JLhdf/hdf5lib/structs/H5FD_hdfs_fapl_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1hdfs
    (JNIEnv *env, jclass clss, jlong fapl_id, jobject fapl_config)
{
#ifdef H5_HAVE_LIBHDFS
    H5FD_hdfs_fapl_t  instance;
    const char       *str = NULL;
    jfieldID          fid;
    jstring           j_str;
    jclass            cls;
#endif /* H5_HAVE_LIBHDFS */

    UNUSED(clss);

#ifdef H5_HAVE_LIBHDFS
    HDmemset(&instance, 0, sizeof(H5FD_hdfs_fapl_t));

    if (NULL == (cls = ENVPTR->GetObjectClass(ENVONLY, fapl_config)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "version", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    instance.version = ENVPTR->GetIntField(ENVONLY, fapl_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "namenode_name", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5FDset_fapl_hdfs: fapl_config namenode_name not pinned");

        HDstrncpy(instance.namenode_name, str, H5FD__HDFS_NODE_NAME_SPACE + 1);
        instance.namenode_name[H5FD__HDFS_NODE_NAME_SPACE] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.namenode_name, 0, H5FD__HDFS_NODE_NAME_SPACE + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "namenode_port", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    instance.namenode_port = ENVPTR->GetIntField(ENVONLY, fapl_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "user_name", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5FDset_fapl_hdfs: fapl_config user_name not pinned");

        HDstrncpy(instance.user_name, str, H5FD__HDFS_USER_NAME_SPACE + 1);
        instance.user_name[H5FD__HDFS_USER_NAME_SPACE] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.user_name, 0, H5FD__HDFS_USER_NAME_SPACE + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "kerberos_ticket_cache", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5FDset_fapl_hdfs: fapl_config kerberos_ticket_cache not pinned");

        HDstrncpy(instance.kerberos_ticket_cache, str, H5FD__HDFS_KERB_CACHE_PATH_SPACE + 1);
        instance.kerberos_ticket_cache[H5FD__HDFS_KERB_CACHE_PATH_SPACE] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.kerberos_ticket_cache, 0, H5FD__HDFS_KERB_CACHE_PATH_SPACE + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "stream_buffer_size", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    instance.stream_buffer_size = ENVPTR->GetIntField(ENVONLY, fapl_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (H5Pset_fapl_hdfs((hid_t)fapl_id, &instance) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
#else
    H5_UNIMPLEMENTED(ENVONLY, "H5Pset_fapl_hdfs: not implemented");
#endif /* H5_HAVE_LIBHDFS */

done:
    /* NOP */;
#ifdef H5_HAVE_LIBHDFS
    if (str)
        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
#endif /* H5_HAVE_LIBHDFS */
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1hdfs */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_log
 * Signature: (JLjava/lang/String;JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log
    (JNIEnv *env, jclass clss, jlong fapl_id, jstring logfile, jlong flags, jlong buf_size)
{
    const char *pLogfile = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == logfile)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_fapl_log: log file name is NULL");

    PIN_JAVA_STRING(ENVONLY, logfile, pLogfile, NULL, "H5Pset_fapl_log: log file name not pinned");

    if ((retVal = H5Pset_fapl_log((hid_t)fapl_id, pLogfile, (unsigned long long)flags, (size_t)buf_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (pLogfile)
        UNPIN_JAVA_STRING(ENVONLY, logfile, pLogfile);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1log */

/*
 * TODO: H5Pset_fapl_mpio
 */

/*
 * TODO: H5Pget_fapl_mpio
 */

/*
 * TODO: H5Pset_fapl_mpiposix
 */

/*
 * TODO: H5Pget_fapl_mpiposix
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_multi
 * Signature: (J[I[J[Ljava/lang/String;[JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi
    (JNIEnv *env, jclass clss, jlong tid, jintArray memb_map,
        jlongArray memb_fapl, jobjectArray memb_name, jlongArray memb_addr, jboolean relax)
{
    const char * const *mName = NULL;
    const char         *utf8 = NULL;
    jboolean            isCopy;
    jboolean            bb;
    jobject             o;
    jstring             rstring;
    jstring             obj;
    jclass              Sjc;
    size_t              i;
    jlong              *thefaplArray = NULL;
    jlong              *theaddrArray = NULL;
    jint               *themapArray = NULL;
    char               *member_name[H5FD_MEM_NTYPES];
    herr_t              status = FAIL;

    UNUSED(clss);

    HDmemset(member_name, 0, H5FD_MEM_NTYPES * sizeof(char *));

    if (memb_map)
        PIN_INT_ARRAY(ENVONLY, memb_map, themapArray, &isCopy, "H5Pset_fapl_multi: memb_map not pinned");
    if (memb_fapl)
        PIN_LONG_ARRAY(ENVONLY, memb_fapl, thefaplArray, &isCopy, "H5Pset_fapl_multi: memb_fapl not pinned");
    if (memb_addr)
        PIN_LONG_ARRAY(ENVONLY, memb_addr, theaddrArray, &isCopy, "H5Pset_fapl_multi: memb_addr not pinned");

    if (memb_name) {
        for (i = 0; i < H5FD_MEM_NTYPES; i++) {
            size_t str_len;

            if (NULL == (obj = (jstring) ENVPTR->GetObjectArrayElement(ENVONLY, (jobjectArray) memb_name, (jsize) i))) {
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                /*
                 * If the string object was NULL, skip it.
                 */
                member_name[i] = NULL;
                continue;
            }

            /*
             * length = ENVPTR->GetStringUTFLength(ENVONLY, obj);
             * CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
             */

            PIN_JAVA_STRING(ENVONLY, obj, utf8, NULL, "H5Pset_fapl_multi: string not pinned");

            str_len = HDstrlen(utf8);

            if (NULL == (member_name[i] = (char *) HDmalloc(str_len + 1)))
                H5_JNI_FATAL_ERROR(ENVONLY, "H5Pset_fapl_multi: memory allocation failed");

            HDstrncpy(member_name[i], utf8, str_len + 1);
            (member_name[i])[str_len] = '\0';

            UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
            utf8 = NULL;

            ENVPTR->DeleteLocalRef(ENVONLY, obj);
        }

        mName = (const char * const *)member_name;
    }

    if ((status = H5Pset_fapl_multi((hid_t)tid, (const H5FD_mem_t *)themapArray, (const hid_t *)thefaplArray,
            mName, (const haddr_t *)theaddrArray, (hbool_t)relax)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (memb_name && mName) {
        if (NULL == (Sjc = ENVPTR->FindClass(ENVONLY, "java/lang/String")))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

        for (i = 0; i < H5FD_MEM_NTYPES; i++) {
            if (!member_name[i]) continue;

            if (NULL == (rstring = ENVPTR->NewStringUTF(ENVONLY, member_name[i]))) {
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                H5_JNI_FATAL_ERROR(ENVONLY, "H5Pset_fapl_multi: out of memory - unable to construct string from UTF characters");
            }

            if (NULL == (o = ENVPTR->GetObjectArrayElement(ENVONLY, memb_name, (jsize) i))) {
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                /*
                 * If the string object was NULL, skip it.
                 */
                ENVPTR->DeleteLocalRef(ENVONLY, rstring);
                continue;
            }

            if (JNI_TRUE == (bb = ENVPTR->IsInstanceOf(ENVONLY, o, Sjc))) {
                ENVPTR->SetObjectArrayElement(ENVONLY, memb_name, (jsize) i, (jobject)rstring);
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
            }

            ENVPTR->DeleteLocalRef(ENVONLY, o);
            ENVPTR->DeleteLocalRef(ENVONLY, rstring);

            HDfree(member_name[i]);
            member_name[i] = NULL;
        }
    }

done:
    for (i = 0; i < H5FD_MEM_NTYPES; i++) {
        if (member_name[i])
            HDfree(member_name[i]);
    }
    if (utf8)
        UNPIN_JAVA_STRING(ENVONLY, obj, utf8);
    if (theaddrArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_addr, theaddrArray, (status < 0) ? JNI_ABORT : 0);
    if (thefaplArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_fapl, thefaplArray, (status < 0) ? JNI_ABORT : 0);
    if (themapArray)
        UNPIN_INT_ARRAY(ENVONLY, memb_map, themapArray, (status < 0) ? JNI_ABORT : 0);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1multi */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fapl_multi
 * Signature: (J[I[J[Ljava/lang/String;[J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi
    (JNIEnv *env, jclass clss, jlong tid, jintArray memb_map,
        jlongArray memb_fapl, jobjectArray memb_name, jlongArray memb_addr)
{
    jboolean   isCopy;
    jstring    str;
    herr_t     status = FAIL;
    size_t     i;
    jlong     *thefaplArray = NULL;
    jlong     *theaddrArray = NULL;
    jint      *themapArray = NULL;
    char     **mName = NULL;
    int        relax = 0;
    int        retVal = 0;

    UNUSED(clss);

    if (memb_map)
        PIN_INT_ARRAY(ENVONLY, memb_map, themapArray, &isCopy, "H5Pget_fapl_multi: memb_map not pinned");
    if (memb_fapl)
        PIN_LONG_ARRAY(ENVONLY, memb_fapl, thefaplArray, &isCopy, "H5Pget_fapl_multi: memb_fapl not pinned");
    if (memb_addr)
        PIN_LONG_ARRAY(ENVONLY, memb_addr, theaddrArray, &isCopy, "H5Pget_fapl_multi: memb_addr not pinned");
    if (memb_name)
        if (NULL == (mName = (char **) HDcalloc(H5FD_MEM_NTYPES, sizeof(*mName))))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_multi: memory allocation failed");

    if ((status = H5Pget_fapl_multi((hid_t)tid, (H5FD_mem_t *)themapArray, (hid_t *)thefaplArray,
            mName, (haddr_t *)theaddrArray, (hbool_t *)&relax)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (memb_name && mName) {
        for (i = 0; i < H5FD_MEM_NTYPES; i++) {
            if (mName[i]) {
                if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, mName[i]))) {
                    CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
                    H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_multi: out of memory - unable to construct string from UTF characters");
                }

                ENVPTR->SetObjectArrayElement(ENVONLY, memb_name, (jsize) i, (jobject)str);
                CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

                ENVPTR->DeleteLocalRef(ENVONLY, str);
            }
        }
    }

    retVal = (relax != 0);

done:
    h5str_array_free(mName, H5FD_MEM_NTYPES);
    if (theaddrArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_addr, theaddrArray, (status < 0) ? JNI_ABORT : 0);
    if (thefaplArray)
        UNPIN_LONG_ARRAY(ENVONLY, memb_fapl, thefaplArray, (status < 0) ? JNI_ABORT : 0);
    if (themapArray)
        UNPIN_INT_ARRAY(ENVONLY, memb_map, themapArray, (status < 0) ? JNI_ABORT : 0);

    return (jboolean)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1multi */

/*
 * TODO: H5Pset_multi_type
 */

/*
 * TODO: H5Pget_multi_type
 */

/*
 * Class:     hdf5_hdf5lib_H5
 * Method:    H5Pget_fapl_ros3
 * Signature: (J)Lhdf/hdf5lib/structs/H5FD_ros3_fapl_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fapl_1ros3
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
#ifdef H5_HAVE_ROS3_VFD
    H5FD_ros3_fapl_t fa;
    jvalue           args[3];
    jstring          j_aws   = NULL;
    jstring          j_id    = NULL;
    jstring          j_key   = NULL;
#endif /* H5_HAVE_ROS3_VFD */
    jobject          ret_obj = NULL;

    UNUSED(clss);

#ifdef H5_HAVE_ROS3_VFD
    /* pass fapl and fapl_t instance into library get_fapl */
    /* store fapl details in ros3_fapl_t instance `fa`          */
    if (H5Pget_fapl_ros3((hid_t)fapl_id, &fa) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL != fa.aws_region) {
        if (NULL == (j_aws = ENVPTR->NewStringUTF(ENVONLY, fa.aws_region))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_ros3: out of memory - can't create aws_region string");
        }
    }
    args[0].l = j_aws;

    if (NULL != fa.secret_id) {
        if (NULL == (j_id = ENVPTR->NewStringUTF(ENVONLY, fa.secret_id))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_ros3: out of memory - can't create secret_id string");
        }
    }
    args[1].l = j_id;

    if (NULL != fa.secret_key) {
        if (NULL == (j_key = ENVPTR->NewStringUTF(ENVONLY, fa.secret_key))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_fapl_ros3: out of memory - can't create secret_key string");
        }
    }
    args[2].l = j_key;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5FD_ros3_fapl_t", "(Ljava/lang/String;Ljava/lang/String;Ljava/lang/String;)V", args, ret_obj);
#else
    H5_UNIMPLEMENTED(ENVONLY, "H5Pget_fapl_ros3: not implemented");
#endif /* H5_HAVE_ROS3_VFD */

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fapl_1ros3 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_ros3
 * Signature: (JLhdf/hdf5lib/structs/H5FD_ros3_fapl_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1ros3
    (JNIEnv *env, jclass clss, jlong fapl_id, jobject fapl_config)
{
#ifdef H5_HAVE_ROS3_VFD
    H5FD_ros3_fapl_t  instance;
    const char       *str = NULL;
    jfieldID          fid;
    jstring           j_str;
    jclass            cls;
#endif /* H5_HAVE_ROS3_VFD */

    UNUSED(clss);

#ifdef H5_HAVE_ROS3_VFD
    HDmemset(&instance, 0, sizeof(H5FD_ros3_fapl_t));

    if (NULL == (cls = ENVPTR->GetObjectClass(ENVONLY, fapl_config)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "version", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    instance.version = ENVPTR->GetIntField(ENVONLY, fapl_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "aws_region", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5Pset_fapl_ros3: fapl_config aws_region not pinned");

        HDstrncpy(instance.aws_region, str, H5FD_ROS3_MAX_REGION_LEN + 1);
        instance.aws_region[H5FD_ROS3_MAX_REGION_LEN] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.aws_region, 0, H5FD_ROS3_MAX_REGION_LEN + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "secret_id", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5Pset_fapl_ros3: fapl_config secret_id not pinned");

        HDstrncpy(instance.secret_id, str, H5FD_ROS3_MAX_SECRET_ID_LEN + 1);
        instance.secret_id[H5FD_ROS3_MAX_SECRET_ID_LEN] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.secret_id, 0, H5FD_ROS3_MAX_SECRET_ID_LEN + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "secret_key", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, fapl_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5Pset_fapl_ros3: fapl_config secret_key not pinned");

        HDstrncpy(instance.secret_key, str, H5FD_ROS3_MAX_SECRET_KEY_LEN + 1);
        instance.secret_key[H5FD_ROS3_MAX_SECRET_KEY_LEN] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(instance.secret_key, 0, H5FD_ROS3_MAX_SECRET_KEY_LEN + 1);

    if (instance.aws_region[0] != '\0' && instance.secret_id[0] !='\0' && instance.secret_key[0] !='\0')
        instance.authenticate = TRUE;

    if (H5Pset_fapl_ros3((hid_t)fapl_id, &instance) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
#else
    H5_UNIMPLEMENTED(ENVONLY, "H5Pset_fapl_ros3: not implemented");
#endif /* H5_HAVE_ROS3_VFD */

done:
    /* NOP */;
#ifdef H5_HAVE_ROS3_VFD
    if (str)
        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
#endif /* H5_HAVE_LIBHDFS */
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1ros3 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_split
 * Signature: (JLjava/lang/String;JLjava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1split
    (JNIEnv *env, jclass clss, jlong fapl_id, jstring metaext, jlong meta_pl_id, jstring rawext, jlong raw_pl_id)
{
    const char *mstr = NULL;
    const char *rstr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == metaext)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_fapl_split: metaext is NULL");
    if (NULL == rawext)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_fapl_split: rawext is NULL");

    PIN_JAVA_STRING(ENVONLY, metaext, mstr, NULL, "H5Pset_fapl_split: metaext not pinned");
    PIN_JAVA_STRING(ENVONLY, rawext, rstr, NULL, "H5Pset_fapl_split: rawext not pinned");

    if ((retVal = H5Pset_fapl_split((hid_t)fapl_id, mstr, (hid_t)meta_pl_id, rstr, (hid_t)raw_pl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (rstr)
        UNPIN_JAVA_STRING(ENVONLY, rawext, rstr);
    if (mstr)
        UNPIN_JAVA_STRING(ENVONLY, metaext, mstr);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1split */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_sec2
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1sec2
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fapl_sec2((hid_t) fapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1sec2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_stdio
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1stdio
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fapl_stdio((hid_t) fapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1stdio */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fapl_windows
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fapl_1windows
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

#ifdef H5_HAVE_WINDOWS
    if ((retVal = H5Pset_fapl_windows((hid_t) fapl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
#else
    UNUSED(env);
    UNUSED(fapl_id);
#endif
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fapl_1windows */

/*
 * TODO: H5Pset_file_image
 */

/*
 * TODO: H5Pget_file_image
 */

/*
 * TODO: H5Pset_file_image_callbacks
 */

/*
 * TODO: H5Pget_file_image_callbacks
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_meta_block_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1meta_1block_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    long sz = (long)size;

    UNUSED(clss);

    if (H5Pset_meta_block_size((hid_t)plist, (hsize_t)sz) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1meta_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_meta_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1meta_1block_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    hsize_t s = 0;

    UNUSED(clss);

    if (H5Pget_meta_block_size((hid_t)plist, &s) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1meta_1block_1size */

/*
 * TODO: H5Pset_page_buffer_size
 */

/*
 * TODO: H5Pget_page_buffer_size
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_sieve_buf_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1sieve_1buf_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    size_t sz = (size_t)size;

    UNUSED(clss);

    if (H5Pset_sieve_buf_size((hid_t)plist, (size_t)sz) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1sieve_1buf_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_sieve_buf_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1sieve_1buf_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    size_t s = 0;

    UNUSED(clss);

    if (H5Pget_sieve_buf_size((hid_t)plist, &s) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1sieve_1buf_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_alignment
 * Signature: (JJJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alignment
    (JNIEnv *env, jclass clss, jlong plist, jlong threshold, jlong alignment)
{
    long   thr = (long)threshold;
    long   align = (long)alignment;
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_alignment((hid_t)plist, (hsize_t)thr, (hsize_t)align)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1alignment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alignment
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alignment
    (JNIEnv *env, jclass clss, jlong plist, jlongArray alignment)
{
    jboolean  isCopy;
    hsize_t   t = 0;
    hsize_t   a = 0;
    jsize     arrLen;
    jlong    *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == alignment)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_alignment: input alignment is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, alignment)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_alignment: alignment array length < 0");
    }
    if (arrLen < 2)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_alignment: alignment input array < 2");

    PIN_LONG_ARRAY(ENVONLY, alignment, theArray, &isCopy, "H5Pget_alignment: input array not pinned");

    if ((status = H5Pget_alignment((hid_t)plist, &t, &a)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jlong)t;
    theArray[1] = (jlong)a;

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, alignment, theArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1alignment */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_cache
 * Signature: (JIJJD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1cache
    (JNIEnv *env, jclass clss, jlong plist, jint mdc_nelmts, jlong rdcc_nelmts,
  jlong rdcc_nbytes, jdouble rdcc_w0)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_cache((hid_t)plist, (int)mdc_nelmts, (size_t)rdcc_nelmts,
            (size_t)rdcc_nbytes, (double) rdcc_w0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_cache
 * Signature: (J[I[J[J[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1cache
    (JNIEnv *env, jclass clss, jlong plist, jintArray mdc_nelmts,
        jlongArray rdcc_nelmts, jlongArray rdcc_nbytes, jdoubleArray rdcc_w0)
{
    jboolean  isCopy;
    jdouble  *w0Array = NULL;
    jlong    *rdcc_nelmtsArray = NULL;
    jlong    *nbytesArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);
    UNUSED(mdc_nelmts);

    if (NULL != rdcc_w0)
        PIN_DOUBLE_ARRAY(ENVONLY, rdcc_w0, w0Array, &isCopy, "H5Pget_cache: rdcc_w0 array not pinned");
    if (NULL != rdcc_nelmts)
        PIN_LONG_ARRAY(ENVONLY, rdcc_nelmts, rdcc_nelmtsArray, &isCopy, "H5Pget_cache: rdcc_nelmts array not pinned");
    if (NULL != rdcc_nbytes)
        PIN_LONG_ARRAY(ENVONLY, rdcc_nbytes, nbytesArray, &isCopy, "H5Pget_cache: nbytesArray array not pinned");

    {
        /* direct cast (size_t *)variable fails on 32-bit environment */
        long long rdcc_nelmts_temp = *rdcc_nelmtsArray;
        long long nbytes_temp = *nbytesArray;
        size_t    rdcc_nelmts_t = (size_t) rdcc_nelmts_temp;
        size_t    nbytes_t = (size_t) nbytes_temp;

        if ((status = H5Pget_cache((hid_t)plist, (int *)NULL, &rdcc_nelmts_t,
                &nbytes_t, (double *)w0Array)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        *rdcc_nelmtsArray = (jlong)rdcc_nelmts_t;
        *nbytesArray = (jlong)nbytes_t;
    } /* end direct cast special */

done:
    if (nbytesArray)
        UNPIN_LONG_ARRAY(ENVONLY, rdcc_nbytes, nbytesArray, (status < 0) ? JNI_ABORT : 0);
    if (rdcc_nelmtsArray)
        UNPIN_LONG_ARRAY(ENVONLY, rdcc_nelmts, rdcc_nelmtsArray, (status < 0) ? JNI_ABORT : 0);
    if (w0Array)
        UNPIN_DOUBLE_ARRAY(ENVONLY, rdcc_w0, w0Array, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1cache */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_elink_file_cache_size
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1elink_1file_1cache_1size
    (JNIEnv *env, jclass clss, jlong plist, jint size)
{
    unsigned sz = (unsigned)size;

    UNUSED(clss);

    if (H5Pset_elink_file_cache_size((hid_t)plist, (unsigned)sz) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1elink_1file_1cache_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_elink_file_cache_size
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1elink_1file_1cache_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    unsigned s = 0;

    UNUSED(clss);

    if (H5Pget_elink_file_cache_size((hid_t)plist, &s) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1elink_1file_1cache_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_evict_on_close
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1evict_1on_1close
    (JNIEnv *env, jclass clss, jlong fapl_id, jboolean evict_on_close)
{
    hbool_t evict_on_close_val = FALSE;
    herr_t  retVal = FAIL;

    UNUSED(clss);

    evict_on_close_val = (evict_on_close == JNI_TRUE) ? TRUE : FALSE;

    if ((retVal = H5Pset_evict_on_close((hid_t)fapl_id, (hbool_t)evict_on_close_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1evict_1on_1close */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_evict_on_close
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1evict_1on_1close
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    hbool_t  evict_on_close_val = FALSE;
    jboolean bval = JNI_FALSE;

    UNUSED(clss);

    if (H5Pget_evict_on_close((hid_t)fapl_id, (hbool_t *)&evict_on_close_val) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (evict_on_close_val == TRUE) ? JNI_TRUE : JNI_FALSE;

done:
    return bval;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1evict_1on_1close */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_metadata_read_attempts
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts
    (JNIEnv *env, jclass clss, jlong plist_id, jlong attempts)
{
    UNUSED(clss);

    if (attempts <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_metadata_read_attempts: attempts <= 0");

    if (H5Pset_metadata_read_attempts((hid_t)plist_id, (unsigned)attempts) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1metadata_1read_1attempts */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_metadata_read_attempts
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1metadata_1read_1attempts
    (JNIEnv *env, jclass clss, jlong plist_id)
{
    unsigned attempts = 0;

    UNUSED(clss);

    if (H5Pget_metadata_read_attempts((hid_t)plist_id, &attempts) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong) attempts;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1metadata_1read_1attempts */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_config
 * Signature: (JLhdf/hdf5lib/structs/H5AC_cache_config_t;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config
    (JNIEnv *env, jclass clss, jlong plist, jobject cache_config)
{
    H5AC_cache_config_t  cacheinfo;
    const char          *str = NULL;
    jfieldID             fid;
    jstring              j_str;
    jclass               cls;
    jint                 flash_incr_mode;
    jint                 incr_mode;
    jint                 decr_mode;
    herr_t               status = FAIL;

    UNUSED(clss);

    HDmemset(&cacheinfo, 0, sizeof(H5AC_cache_config_t));

    if (NULL == (cls = ENVPTR->GetObjectClass(ENVONLY, cache_config)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "version", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.version = ENVPTR->GetIntField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "rpt_fcn_enabled", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.rpt_fcn_enabled = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "open_trace_file", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.open_trace_file = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "close_trace_file", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.close_trace_file = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "trace_file_name", "Ljava/lang/String;")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (j_str = (jstring)ENVPTR->GetObjectField(ENVONLY, cache_config, fid)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (j_str) {
        PIN_JAVA_STRING(ENVONLY, j_str, str, NULL, "H5Pset_mdc_config: cache_config not pinned");

        HDstrncpy(cacheinfo.trace_file_name, str, H5AC__MAX_TRACE_FILE_NAME_LEN + 1);
        cacheinfo.trace_file_name[H5AC__MAX_TRACE_FILE_NAME_LEN] = '\0';

        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
        str = NULL;
    }
    else
        HDmemset(cacheinfo.trace_file_name, 0, H5AC__MAX_TRACE_FILE_NAME_LEN + 1);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "evictions_enabled", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.evictions_enabled = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "set_initial_size", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.set_initial_size = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "initial_size", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.initial_size = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "min_clean_fraction", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.min_clean_fraction = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "max_size", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.max_size = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "min_size", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.min_size = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "epoch_length", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.epoch_length = (long int)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "incr_mode", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    incr_mode = ENVPTR->GetIntField(ENVONLY, cache_config, fid); /*(enum H5C_cache_incr_mode) */
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    cacheinfo.incr_mode = (enum H5C_cache_incr_mode) incr_mode;

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "lower_hr_threshold", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.lower_hr_threshold = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "increment", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.increment = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "apply_max_increment", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.apply_max_increment = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "max_increment", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.max_increment = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "flash_incr_mode", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    flash_incr_mode = ENVPTR->GetIntField(ENVONLY, cache_config, fid); /*(enum H5C_cache_flash_incr_mode) */
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    cacheinfo.flash_incr_mode = (enum H5C_cache_flash_incr_mode) flash_incr_mode;

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "flash_multiple", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.flash_multiple = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "flash_threshold", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.flash_threshold = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "decr_mode", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    decr_mode = ENVPTR->GetIntField(ENVONLY, cache_config, fid); /*(enum H5C_cache_decr_mode) */
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    cacheinfo.decr_mode = (enum H5C_cache_decr_mode) decr_mode;

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "upper_hr_threshold", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.upper_hr_threshold = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "decrement", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.decrement = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "apply_max_decrement", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.apply_max_decrement = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "max_decrement", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.max_decrement = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "epochs_before_eviction", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.epochs_before_eviction = ENVPTR->GetIntField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "apply_empty_reserve", "Z")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.apply_empty_reserve = ENVPTR->GetBooleanField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "empty_reserve", "D")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.empty_reserve = ENVPTR->GetDoubleField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "dirty_bytes_threshold", "J")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.dirty_bytes_threshold = (size_t)ENVPTR->GetLongField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if (NULL == (fid = ENVPTR->GetFieldID(ENVONLY, cls, "metadata_write_strategy", "I")))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    cacheinfo.metadata_write_strategy = ENVPTR->GetIntField(ENVONLY, cache_config, fid);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    if ((status = H5Pset_mdc_config((hid_t)plist, &cacheinfo)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (str)
        UNPIN_JAVA_STRING(ENVONLY, j_str, str);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1mdc_1config */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_config
 * Signature: (J)Lhdf/hdf5lib/structs/H5AC_cache_config_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5AC_cache_config_t cacheinfo;
    jstring             j_str = NULL;
    jvalue              args[30];
    herr_t              status = FAIL;
    jobject             ret_obj = NULL;

    UNUSED(clss);

    HDmemset(&cacheinfo, 0, sizeof(H5AC_cache_config_t));
    cacheinfo.version = H5AC__CURR_CACHE_CONFIG_VERSION;

    if ((status = H5Pget_mdc_config((hid_t)plist, &cacheinfo)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    args[0].i = cacheinfo.version;
    args[1].z = cacheinfo.rpt_fcn_enabled;
    args[2].z = cacheinfo.open_trace_file;
    args[3].z = cacheinfo.close_trace_file;

    if (NULL != cacheinfo.trace_file_name) {
        if (NULL == (j_str = ENVPTR->NewStringUTF(ENVONLY, cacheinfo.trace_file_name))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_mdc_config: out of memory - unable to construct string from UTF characters");
        }
    }

    args[4].l = j_str;
    args[5].z = cacheinfo.evictions_enabled;
    args[6].z = cacheinfo.set_initial_size;
    args[7].j = (jlong)cacheinfo.initial_size;
    args[8].d = cacheinfo.min_clean_fraction;
    args[9].j = (jlong)cacheinfo.max_size;
    args[10].j = (jlong)cacheinfo.min_size;
    args[11].j = cacheinfo.epoch_length;
    args[12].i = cacheinfo.incr_mode;
    args[13].d = cacheinfo.lower_hr_threshold;
    args[14].d = cacheinfo.increment;
    args[15].z = cacheinfo.apply_max_increment;
    args[16].j = (jlong)cacheinfo.max_increment;
    args[17].i = cacheinfo.flash_incr_mode;
    args[18].d = cacheinfo.flash_multiple;
    args[19].d = cacheinfo.flash_threshold;
    args[20].i = cacheinfo.decr_mode;
    args[21].d = cacheinfo.upper_hr_threshold;
    args[22].d = cacheinfo.decrement;
    args[23].z = cacheinfo.apply_max_decrement;
    args[24].j = (jlong)cacheinfo.max_decrement;
    args[25].i = cacheinfo.epochs_before_eviction;
    args[26].z = cacheinfo.apply_empty_reserve;
    args[27].d = cacheinfo.empty_reserve;
    args[28].j = (jlong)cacheinfo.dirty_bytes_threshold;
    args[29].i = cacheinfo.metadata_write_strategy;

    CALL_CONSTRUCTOR(ENVONLY, "hdf/hdf5lib/structs/H5AC_cache_config_t", "(IZZZLjava/lang/String;ZZJDJJJIDDZJIDDIDDZJIZDJI)V", args, ret_obj);

done:
    return ret_obj;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1mdc_1config */

/*
 * TODO: H5Pset_mdc_image_config
 */

/*
 * TODO: H5Pget_mdc_image_config
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_mdc_log_options
 * Signature: (JZLjava/lang/String;Z)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1mdc_1log_1options
    (JNIEnv *env, jclass clss, jlong fapl_id, jboolean is_enabled, jstring location, jboolean start_on_access)
{
    const char *locStr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == location)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_mdc_log_options: location string is NULL");

    PIN_JAVA_STRING(ENVONLY, location, locStr, NULL, "H5Pset_mdc_log_options: location string not pinned");

    if ((retVal = H5Pset_mdc_log_options((hid_t)fapl_id, (hbool_t)is_enabled, locStr, (hbool_t)start_on_access)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (locStr)
        UNPIN_JAVA_STRING(ENVONLY, location, locStr);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1mdc_1log_1options */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_mdc_log_options
 * Signature: (J[Z)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1mdc_1log_1options
    (JNIEnv *env, jclass clss, jlong fapl_id, jbooleanArray mdc_log_options)
{
    jboolean   isCopy;
    jboolean  *mdc_log_options_ptr = NULL;
    hbool_t    is_enabled;
    hbool_t    start_on_access;
    ssize_t    status = -1;
    size_t     location_size;
    jsize      arrLen;
    char      *lname = NULL;
    jstring    str = NULL;

    UNUSED(clss);

    if (NULL == mdc_log_options)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_mdc_log_options: mdc_log_options is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, mdc_log_options)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_mdc_log_options: mdc_log_options array length < 0");
    }
    if (arrLen < 2)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_mdc_log_options: length of mdc_log_options < 2");

    PIN_BOOL_ARRAY(ENVONLY, mdc_log_options, mdc_log_options_ptr, &isCopy, "H5Pget_mdc_log_options: mdc_log_options array not pinned");

    /* get the length of the filename */
    if (H5Pget_mdc_log_options((hid_t)fapl_id, &is_enabled, NULL, &location_size, &start_on_access) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (!location_size)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_mdc_log_options: location_size is 0");

    location_size++; /* add extra space for the null terminator */
    if (NULL == (lname = (char *) HDmalloc(sizeof(char) * location_size)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_mdc_log_options: memory allocation failed");

    if ((status = H5Pget_mdc_log_options((hid_t)fapl_id, &is_enabled, lname, &location_size, &start_on_access)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    lname[location_size - 1] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, lname))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_mdc_log_options: out of memory - unable to construct string from UTF characters");
    }

    mdc_log_options_ptr[0] = (jboolean)is_enabled;
    mdc_log_options_ptr[1] = (jboolean)start_on_access;

done:
    if (lname)
        HDfree(lname);
    if (mdc_log_options_ptr)
        UNPIN_BOOL_ARRAY(ENVONLY, mdc_log_options, mdc_log_options_ptr, (status < 0) ? JNI_ABORT : 0);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1mdc_1log_1options */

/*
 * TODO: H5Pset_all_coll_metadata_ops
 */

/*
 * TODO: H5Pget_all_coll_metadata_ops
 */

/*
 * TODO: H5Pset_coll_metadata_write
 */

/*
 * TODO: H5Pget_coll_metadata_write
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_gc_references
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1gc_1references
    (JNIEnv *env, jclass clss, jlong fapl_id, jboolean gc_ref)
{
    unsigned gc_ref_val = 0;
    herr_t   retVal = FAIL;

    UNUSED(clss);

    gc_ref_val = (gc_ref == JNI_TRUE) ? 1 : 0;

    if ((retVal = H5Pset_gc_references((hid_t)fapl_id, gc_ref_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1gc_1references */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_gc_references
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1gc_1references
    (JNIEnv *env, jclass clss, jlong fapl_id)
{
    unsigned gc_ref_val = 0;
    jboolean bval = JNI_FALSE;

    UNUSED(clss);

    if (H5Pget_gc_references((hid_t)fapl_id, (unsigned *)&gc_ref_val) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (gc_ref_val == 1) ? JNI_TRUE : JNI_FALSE;

done:
    return bval;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1gc_1references */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_small_data_block_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1small_1data_1block_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    long   sz = (long)size;
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_small_data_block_size((hid_t)plist, (hsize_t)sz)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1small_1data_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_small_data_block_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1small_1data_1block_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    hsize_t s = 0;

    UNUSED(clss);

    if (H5Pget_small_data_block_size((hid_t)plist, &s) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1small_1data_1block_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_libver_bounds
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds
    (JNIEnv *env, jclass clss, jlong fapl_id, jint low, jint high)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_libver_bounds((hid_t)fapl_id, (H5F_libver_t)low, (H5F_libver_t)high)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1libver_1bounds */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_libver_bounds
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds
    (JNIEnv *env, jclass clss, jlong fapl_id, jintArray libver)
{
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == libver)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_libver_bounds: libversion bounds is NULL");

    PIN_INT_ARRAY(ENVONLY, libver, theArray, &isCopy, "H5Pget_libver_bounds: input not pinned");

    if ((retVal = H5Pget_libver_bounds((hid_t)fapl_id, (H5F_libver_t *)&(theArray[0]), (H5F_libver_t *)&(theArray[1]))) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, libver, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1libver_1bounds */

/*
 * TODO: H5Pset_object_flush_cb
 */

/*
 * TODO: H5Pget_object_flush_cb
 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
