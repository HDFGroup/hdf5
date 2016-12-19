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

#include <jni.h>
/* Header for class hdf_hdf5lib_H5_H5F */

#ifndef _Included_hdf_hdf5lib_H5_H5F
#define _Included_hdf_hdf5lib_H5_H5F

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fopen
 * Signature: (Ljava/lang/String;IJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fopen
  (JNIEnv*, jclass, jstring, jint, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fcreate
 * Signature: (Ljava/lang/String;IJJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fcreate
  (JNIEnv*, jclass, jstring, jint, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fflush
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fflush
  (JNIEnv*, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_name
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1name
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fis_hdf5
 * Signature: (Ljava/lang/String;)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Fis_1hdf5
  (JNIEnv*, jclass, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_create_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fget_1create_1plist
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_access_plist
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Fget_1access_1plist
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_intent
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1intent
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fclose
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5__1H5Fclose
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fmount
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fmount
  (JNIEnv*, jclass, jlong, jstring, jlong, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Funmount
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Funmount
  (JNIEnv*, jclass, jlong, jstring);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_freespace
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1freespace
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Freopen
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5__1H5Freopen
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_ids
 * Signature: (JIJ[J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1ids
  (JNIEnv*, jclass, jlong, jint, jlong, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_obj_count
 * Signature: (JI)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1obj_1count
  (JNIEnv*, jclass, jlong, jint);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_filesize
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1filesize
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_hit_rate
 * Signature: (J)D
 */
JNIEXPORT jdouble JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1hit_1rate
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1size
  (JNIEnv*, jclass, jlong, jlongArray);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Freset_mdc_hit_rate_stats
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Freset_1mdc_1hit_1rate_1stats
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_info
 * Signature: (J)Lhdf/hdf5lib/structs/H5F_info2_t;
 */
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_H5_H5Fget_1info
  (JNIEnv*, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fclear_elink_file_cache
 * Signature: (J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Fclear_1elink_1file_1cache
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_swmr_write
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Fstart_1swmr_1write
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstart_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Fstart_1mdc_1logging
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fstop_mdc_logging
 * Signature: (J)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Fstop_1mdc_1logging
  (JNIEnv *, jclass, jlong);

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Fget_mdc_logging_status
 * Signature: (J[Z)V
 */
JNIEXPORT void JNICALL 
Java_hdf_hdf5lib_H5_H5Fget_1mdc_1logging_1status
  (JNIEnv *, jclass, jlong, jbooleanArray);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_hdf_hdf5lib_H5_H5F */
