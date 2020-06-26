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

#include <jni.h>
#include "H5version.h"
#include <string.h>
#include "H5private.h"

#ifndef _Included_h5jni
#define _Included_h5jni

#ifdef __cplusplus
  #define ENVPTR (env)
  #define ENVONLY
  #define CBENVPTR (cbenv)
  #define CBENVONLY
  #define JVMPTR (jvm)
  #define JVMPAR
#else /* __cplusplus */
  #define ENVPTR (*env)
  #define ENVONLY env
  #define CBENVPTR (*cbenv)
  #define CBENVONLY cbenv
  #define JVMPTR (*jvm)
  #define JVMPAR jvm
#endif /* __cplusplus */

/*
 * Used to silence compiler when a particular
 * function parameter is not used.
 */
#define UNUSED(o) (void) o

/*
 * Macro to check for a JNI exception after a JNI method is called.
 * If an exception occurred, the value of 'clearException' will determine
 * whether or not the exception will be cleared in order for the native
 * method to do its own error handling.
 *
 * If the exception does not get cleared, this macro will skip to the
 * cleanup+return section of the native method, since at that point
 * cleaning up and returning is the only safe thing that can be done.
 */
#define CHECK_JNI_EXCEPTION(envptr, clearException)         \
do {                                                        \
    if (JNI_TRUE == (*envptr)->ExceptionCheck(envptr)) {    \
        if (JNI_TRUE == clearException)                     \
            (*envptr)->ExceptionClear(envptr);              \
        else                                                \
            goto done;                                      \
    }                                                       \
} while(0)

/* Macros for class access */
/* Calling code must define ret_obj as jobject */
#define CALL_CONSTRUCTOR(envptr, classname, classsig, args, ret_obj)                         \
do {                                                                                         \
    jmethodID constructor;                                                                   \
    jclass    cls;                                                                           \
                                                                                             \
    if (NULL == (cls = (*envptr)->FindClass(envptr, (classname)))) {                         \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                               \
        H5_JNI_FATAL_ERROR(envptr, "JNI error: GetObjectClass");                             \
    }                                                                                        \
    if (NULL == (constructor = (*envptr)->GetMethodID(envptr, cls, "<init>", (classsig)))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                               \
        H5_JNI_FATAL_ERROR(envptr, "JNI error: GetMethodID failed");                         \
    }                                                                                        \
    if (NULL == (ret_obj = (*envptr)->NewObjectA(envptr, cls, constructor, (args)))) {       \
        HDprintf("FATAL ERROR: %s: Creation failed\n", classname);                           \
        CHECK_JNI_EXCEPTION(envptr, JNI_FALSE);                                              \
    }                                                                                        \
} while(0)

/*
 * Macros for pinning/unpinning objects.
 */
#define PIN_BYTE_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                      \
    if (NULL == (outBuf = (*envptr)->GetByteArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                            \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                           \
    }                                                                                     \
} while(0)

#define PIN_BYTE_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                     \
    if (NULL == (outBuf = (jbyte *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                           \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                          \
    }                                                                                                    \
} while(0)

#define UNPIN_BYTE_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                           \
do {                                                                                            \
    (*envptr)->ReleaseByteArrayElements(envptr, pinnedArray, (jbyte *) bufToRelease, freeMode); \
} while(0)

#define PIN_SHORT_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                       \
    if (NULL == (outBuf = (*envptr)->GetShortArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                             \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                            \
    }                                                                                      \
} while(0)

#define PIN_SHORT_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                      \
    if (NULL == (outBuf = (jshort *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                            \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                           \
    }                                                                                                     \
} while(0)

#define UNPIN_SHORT_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                            \
do {                                                                                              \
    (*envptr)->ReleaseShortArrayElements(envptr, pinnedArray, (jshort *) bufToRelease, freeMode); \
} while(0)

#define PIN_INT_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                     \
    if (NULL == (outBuf = (*envptr)->GetIntArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                           \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                          \
    }                                                                                    \
} while(0)

#define PIN_INT_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                    \
    if (NULL == (outBuf = (jint *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                          \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                         \
    }                                                                                                   \
} while(0)

#define UNPIN_INT_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                          \
do {                                                                                          \
    (*envptr)->ReleaseIntArrayElements(envptr, pinnedArray, (jint *) bufToRelease, freeMode); \
} while(0)

#define PIN_LONG_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                      \
    if (NULL == (outBuf = (*envptr)->GetLongArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                            \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                           \
    }                                                                                     \
} while(0)

#define PIN_LONG_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                     \
    if (NULL == (outBuf = (jlong *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                           \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                          \
    }                                                                                                    \
} while(0)

#define UNPIN_LONG_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                           \
do {                                                                                            \
    (*envptr)->ReleaseLongArrayElements(envptr, pinnedArray, (jlong *) bufToRelease, freeMode); \
} while(0)

#define PIN_FLOAT_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                       \
    if (NULL == (outBuf = (*envptr)->GetFloatArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                             \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                            \
    }                                                                                      \
} while(0)

#define PIN_FLOAT_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                      \
    if (NULL == (outBuf = (jfloat *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                            \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                           \
    }                                                                                                     \
} while(0)

#define UNPIN_FLOAT_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                            \
do {                                                                                              \
    (*envptr)->ReleaseFloatArrayElements(envptr, pinnedArray, (jfloat *) bufToRelease, freeMode); \
} while(0)

#define PIN_DOUBLE_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                    \
do {                                                                                        \
    if (NULL == (outBuf = (*envptr)->GetDoubleArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                              \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                             \
    }                                                                                       \
} while(0)

#define PIN_DOUBLE_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                          \
do {                                                                                                       \
    if (NULL == (outBuf = (jdouble *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                             \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                            \
    }                                                                                                      \
} while(0)

#define UNPIN_DOUBLE_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                             \
do {                                                                                                \
    (*envptr)->ReleaseDoubleArrayElements(envptr, pinnedArray, (jdouble *) bufToRelease, freeMode); \
} while(0)

#define PIN_BOOL_ARRAY(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                       \
do {                                                                                         \
    if (NULL == (outBuf = (*envptr)->GetBooleanArrayElements(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                               \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                              \
    }                                                                                        \
} while(0)

#define PIN_BOOL_ARRAY_CRITICAL(envptr, arrayToPin, outBuf, isCopy, failErrMsg)                             \
do {                                                                                                        \
    if (NULL == (outBuf = (jboolean *) (*envptr)->GetPrimitiveArrayCritical(envptr, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                                              \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                                             \
    }                                                                                                       \
} while(0)

#define UNPIN_BOOL_ARRAY(envptr, pinnedArray, bufToRelease, freeMode)                                 \
do {                                                                                                  \
    (*envptr)->ReleaseBooleanArrayElements(envptr, pinnedArray, (jboolean *) bufToRelease, freeMode); \
} while(0)

#define UNPIN_ARRAY_CRITICAL(envptr, pinnedArray, bufToRelease, freeMode)                  \
do {                                                                                       \
    (*envptr)->ReleasePrimitiveArrayCritical(envptr, pinnedArray, bufToRelease, freeMode); \
} while(0)

/* Macros for string access */
#define PIN_JAVA_STRING(envptr, stringToPin, outString, isCopy, failErrMsg)                \
do {                                                                                       \
    if (NULL == (outString = (*envptr)->GetStringUTFChars(envptr, stringToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(envptr, JNI_TRUE);                                             \
        H5_JNI_FATAL_ERROR(envptr, failErrMsg);                                            \
    }                                                                                      \
} while(0)

#define UNPIN_JAVA_STRING(envptr, pinnedString, stringToRelease)             \
do {                                                                         \
    (*envptr)->ReleaseStringUTFChars(envptr, pinnedString, stringToRelease); \
} while(0)

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern jboolean h5JNIFatalError(JNIEnv *env, const char *);
extern jboolean h5nullArgument(JNIEnv *env, const char *);
extern jboolean h5badArgument(JNIEnv *env, const char *);
extern jboolean h5outOfMemory(JNIEnv *env, const char *);
extern jboolean h5libraryError(JNIEnv *env);
extern jboolean h5raiseException(JNIEnv *env, const char *, const char *);
extern jboolean h5unimplemented( JNIEnv *env, const char *functName);

/*
 * The following macros are to facilitate immediate cleanup+return
 * from a native JNI method when an exception is to be thrown.
 * Since, in general, the "cleanup" methods are the only safe JNI
 * methods to call once an exception has occurred, we want to immediately
 * cleanup and return instead of letting the native method continue.
 *
 * Note that a native method can clear the exception when one occurs and
 * then do its own error handling, but we instead opt to immediately return.
 */
#define H5_JNI_FATAL_ERROR(env, message) \
do {                                     \
    h5JNIFatalError(env, message);       \
    goto done;                           \
} while(0)

#define H5_NULL_ARGUMENT_ERROR(env, message) \
do {                                         \
    h5nullArgument(env, message);            \
    goto done;                               \
} while(0)

#define H5_BAD_ARGUMENT_ERROR(env, message) \
do {                                        \
    h5badArgument(env, message);            \
    goto done;                              \
} while(0)

#define H5_OUT_OF_MEMORY_ERROR(env, message) \
do {                                         \
    h5outOfMemory(env, message);             \
    goto done;                               \
} while(0)

#define H5_LIBRARY_ERROR(env) \
do {                          \
    h5libraryError(env);      \
    goto done;                \
} while(0)

#define H5_RAISE_EXCEPTION(env, message, exception) \
do {                                                \
    h5raiseException(env, message, exception);      \
    goto done;                                      \
} while(0)

#define H5_UNIMPLEMENTED(env, message) \
do {                                   \
    h5unimplemented(env, message);     \
    goto done;                         \
} while(0)

/* implemented at H5.c */
extern jint get_enum_value(JNIEnv *env, jobject enum_obj);
extern jobject get_enum_object(JNIEnv *env, const char* enum_class_name,
    jint enum_val, const char* enum_field_desc);

/* implemented at H5G.c */
extern jobject create_H5G_info_t(JNIEnv *env, H5G_info_t group_info);

/* implemented at h5oimp.c */
extern jobject create_H5O_token_t(JNIEnv *env, const H5O_token_t *token, hbool_t is_critical_pinning);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_h5jni */
