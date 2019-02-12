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

/* Macros for class access */
/* Calling code must define ret_obj as jobject */
#define CALL_CONSTRUCTOR(env, classname, classsig, args, ret_obj)                          \
{                                                                                          \
    jmethodID constructor;                                                                 \
    jclass    cls;                                                                         \
                                                                                           \
    if (NULL == (cls = ENVPTR->FindClass(env, (classname)))) {                             \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                \
        H5_JNI_FATAL_ERROR(env, "JNI error: GetObjectClass");                              \
    }                                                                                      \
    if (NULL == (constructor = ENVPTR->GetMethodID(ENVONLY, cls, "<init>", (classsig)))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                \
        H5_JNI_FATAL_ERROR(env, "JNI error: GetMethodID failed");                          \
    }                                                                                      \
    if (NULL == (ret_obj = ENVPTR->NewObjectA(ENVONLY, cls, constructor, (args)))) {       \
        CHECK_JNI_EXCEPTION(env, JNI_FALSE);                                               \
    }                                                                                      \
}

/*
 * Macros for pinning/unpinning objects.
 */
#define PIN_BYTE_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                   \
    if (NULL == (outBuf = ENVPTR->GetByteArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                         \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                        \
    }                                                                               \
}

#define PIN_BYTE_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                  \
    if (NULL == (outBuf = (jbyte *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                        \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                       \
    }                                                                                              \
}

#define UNPIN_BYTE_ARRAY(env, pinnedArray, bufToRelease, freeMode)                        \
{                                                                                         \
    ENVPTR->ReleaseByteArrayElements(env, pinnedArray, (jbyte *) bufToRelease, freeMode); \
}

#define PIN_SHORT_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                    \
    if (NULL == (outBuf = ENVPTR->GetShortArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                          \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                         \
    }                                                                                \
}

#define PIN_SHORT_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                   \
    if (NULL == (outBuf = (jshort *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                         \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                        \
    }                                                                                               \
}

#define UNPIN_SHORT_ARRAY(env, pinnedArray, bufToRelease, freeMode)                         \
{                                                                                           \
    ENVPTR->ReleaseShortArrayElements(env, pinnedArray, (jshort *) bufToRelease, freeMode); \
}

#define PIN_INT_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                  \
    if (NULL == (outBuf = ENVPTR->GetIntArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                        \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                       \
    }                                                                              \
}

#define PIN_INT_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                 \
    if (NULL == (outBuf = (jint *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                       \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                      \
    }                                                                                             \
}

#define UNPIN_INT_ARRAY(env, pinnedArray, bufToRelease, freeMode)                       \
{                                                                                       \
    ENVPTR->ReleaseIntArrayElements(env, pinnedArray, (jint *) bufToRelease, freeMode); \
}

#define PIN_LONG_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                   \
    if (NULL == (outBuf = ENVPTR->GetLongArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                         \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                        \
    }                                                                               \
}

#define PIN_LONG_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                  \
    if (NULL == (outBuf = (jlong *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                        \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                       \
    }                                                                                              \
}

#define UNPIN_LONG_ARRAY(env, pinnedArray, bufToRelease, freeMode)                        \
{                                                                                         \
    ENVPTR->ReleaseLongArrayElements(env, pinnedArray, (jlong *) bufToRelease, freeMode); \
}

#define PIN_FLOAT_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                    \
    if (NULL == (outBuf = ENVPTR->GetFloatArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                          \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                         \
    }                                                                                \
}

#define PIN_FLOAT_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                   \
    if (NULL == (outBuf = (jfloat *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                         \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                        \
    }                                                                                               \
}

#define UNPIN_FLOAT_ARRAY(env, pinnedArray, bufToRelease, freeMode)                         \
{                                                                                           \
    ENVPTR->ReleaseFloatArrayElements(env, pinnedArray, (jfloat *) bufToRelease, freeMode); \
}

#define PIN_DOUBLE_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                 \
{                                                                                     \
    if (NULL == (outBuf = ENVPTR->GetDoubleArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                           \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                          \
    }                                                                                 \
}

#define PIN_DOUBLE_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                       \
{                                                                                                    \
    if (NULL == (outBuf = (jdouble *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                          \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                         \
    }                                                                                                \
}

#define UNPIN_DOUBLE_ARRAY(env, pinnedArray, bufToRelease, freeMode)                          \
{                                                                                             \
    ENVPTR->ReleaseDoubleArrayElements(env, pinnedArray, (jdouble *) bufToRelease, freeMode); \
}

#define PIN_BOOL_ARRAY(env, arrayToPin, outBuf, isCopy, failErrMsg)                    \
{                                                                                      \
    if (NULL == (outBuf = ENVPTR->GetBooleanArrayElements(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                            \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                           \
    }                                                                                  \
}

#define PIN_BOOL_ARRAY_CRITICAL(env, arrayToPin, outBuf, isCopy, failErrMsg)                          \
{                                                                                                     \
    if (NULL == (outBuf = (jboolean *) ENVPTR->GetPrimitiveArrayCritical(env, arrayToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                                           \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                                          \
    }                                                                                                 \
}

#define UNPIN_BOOL_ARRAY(env, pinnedArray, bufToRelease, freeMode)                              \
{                                                                                               \
    ENVPTR->ReleaseBooleanArrayElements(env, pinnedArray, (jboolean *) bufToRelease, freeMode); \
}

#define UNPIN_ARRAY_CRITICAL(env, pinnedArray, bufToRelease, freeMode)               \
{                                                                                    \
    ENVPTR->ReleasePrimitiveArrayCritical(env, pinnedArray, bufToRelease, freeMode); \
}

/* Macros for string access */
#define PIN_JAVA_STRING(env, stringToPin, outString, isCopy, failErrMsg)             \
{                                                                                    \
    if (NULL == (outString = ENVPTR->GetStringUTFChars(env, stringToPin, isCopy))) { \
        CHECK_JNI_EXCEPTION(env, JNI_TRUE);                                          \
        H5_JNI_FATAL_ERROR(env, failErrMsg);                                         \
    }                                                                                \
}

#define UNPIN_JAVA_STRING(env, pinnedString, stringToRelease)          \
{                                                                      \
    ENVPTR->ReleaseStringUTFChars(env, pinnedString, stringToRelease); \
}

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
#define CHECK_JNI_EXCEPTION(env, clearException)   \
{                                                  \
    if (JNI_TRUE == (*env)->ExceptionCheck(env)) { \
        if (JNI_TRUE == clearException)            \
            (*env)->ExceptionClear(env);           \
        else                                       \
            goto done;                             \
    }                                              \
}

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
{                                        \
    h5JNIFatalError(env, message);       \
    goto done;                           \
}

#define H5_NULL_ARGUMENT_ERROR(env, message) \
{                                            \
    h5nullArgument(env, message);            \
    goto done;                               \
}

#define H5_BAD_ARGUMENT_ERROR(env, message) \
{                                           \
    h5badArgument(env, message);            \
    goto done;                              \
}

#define H5_OUT_OF_MEMORY_ERROR(env, message) \
{                                            \
    h5outOfMemory(env, message);             \
    goto done;                               \
}

#define H5_LIBRARY_ERROR(env) \
{                             \
    h5libraryError(env);      \
    goto done;                \
}

#define H5_RAISE_EXCEPTION(env, message, exception) \
{                                                   \
    h5raiseException(env, message, exception);      \
    goto done;                                      \
}

#define H5_UNIMPLEMENTED(env, message) \
{                                      \
    h5unimplemented(env, message);     \
    goto done;                         \
}

/* implemented at H5.c */
extern jint get_enum_value(JNIEnv *env, jobject enum_obj);
extern jobject get_enum_object(JNIEnv *env, const char* enum_class_name,
    jint enum_val, const char* enum_field_desc);

/* implemented at H5G.c */
extern jobject create_H5G_info_t(JNIEnv *env, H5G_info_t group_info);

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

#endif /* _Included_h5jni */
