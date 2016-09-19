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

#include <jni.h>
#include "H5version.h"
#include <string.h>
#include "H5private.h"

#ifndef _Included_h5jni
#define _Included_h5jni

#ifdef __cplusplus
  #define ENVPTR (env)
  #define ENVPAR
  #define ENVONLY
  #define CBENVPTR (cbenv)
  #define CBENVPAR
  #define JVMPTR (jvm)
  #define JVMPAR
  #define JVMPAR2
#else /* __cplusplus */
  #define ENVPTR (*env)
  #define ENVPAR env,
  #define ENVONLY env
  #define CBENVPTR (*cbenv)
  #define CBENVPAR cbenv,
  #define JVMPTR (*jvm)
  #define JVMPAR jvm
  #define JVMPAR2 jvm,
#endif /* __cplusplus */

/* Macros for class access */
/* Calling code must define ret_obj as jobject */
#define CALL_CONSTRUCTOR(classname,classsig,args) {                              \
    jclass     cls;                                                              \
    jmethodID  constructor;                                                      \
    cls = ENVPTR->FindClass(ENVPAR (classname));                                 \
    if (cls == 0) {                                                              \
        h5JNIFatalError(env, "JNI error: GetObjectClass\n");                     \
        ret_obj = NULL;                                                          \
    }                                                                            \
    constructor = ENVPTR->GetMethodID(ENVPAR cls, "<init>", (classsig));         \
    if (constructor == 0) {                                                      \
        h5JNIFatalError(env, "JNI error: GetMethodID failed\n");                 \
        ret_obj = NULL;                                                          \
    }                                                                            \
    ret_obj = ENVPTR->NewObjectA(ENVPAR cls, constructor, (args));               \
}


/* Macros for string access */
#define PIN_JAVA_STRING(javastr,localstr) {                                     \
    jboolean isCopy;                                                             \
    (localstr) = NULL;                                                      \
    if ((javastr) == NULL) {                                                     \
        h5nullArgument(env, "java string is NULL");                              \
    }                                                                            \
    else {                                                                       \
        (localstr) = ENVPTR->GetStringUTFChars(ENVPAR (javastr), &isCopy);           \
        if ((localstr) == NULL) {                                                    \
            h5JNIFatalError(env, "local c string is not pinned");                    \
        }                                                                        \
    }                                                                            \
}

#define UNPIN_JAVA_STRING(javastr,localstr) {                                      \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));                \
}

#define PIN_JAVA_STRING_TWO(javastr,localstr,java2str,local2str) {              \
    jboolean isCopy;                                                             \
    (localstr) = NULL;                                                      \
    (local2str) = NULL;                                                      \
    if ((javastr) == NULL) {                                                     \
        h5nullArgument(env, "java string is NULL");                              \
    }                                                                            \
    else if ((java2str) == NULL) {                                                    \
        h5nullArgument(env, "second java string is NULL");                       \
    }                                                                            \
    else {                                                                       \
        (localstr) = ENVPTR->GetStringUTFChars(ENVPAR (javastr), &isCopy);           \
        if ((localstr) == NULL) {                                                    \
            h5JNIFatalError(env, "local c string is not pinned");                    \
        }                                                                            \
        else {                                                                   \
            (local2str) = ENVPTR->GetStringUTFChars(ENVPAR (java2str), &isCopy);         \
            if ((local2str) == NULL) {                                                   \
                ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));             \
                h5JNIFatalError(env, "second local c string is not pinned");             \
            }                                                                    \
        }                                                                        \
    }                                                                            \
}

#define UNPIN_JAVA_STRING_TWO(javastr,localstr,java2str,local2str) {             \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));                \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (java2str), (local2str));              \
}

#define PIN_JAVA_STRING_THREE(javastr,localstr,java2str,local2str,java3str,local3str) {       \
    jboolean isCopy;                                                             \
    (localstr) = NULL;                                                      \
    (local2str) = NULL;                                                      \
    (local3str) = NULL;                                                      \
    if ((javastr) == NULL) {                                                     \
        h5nullArgument(env, "java string is NULL");                              \
    }                                                                            \
    else if ((java2str) == NULL) {                                                    \
        h5nullArgument(env, "second java string is NULL");                       \
    }                                                                            \
    else if ((java3str) == NULL) {                                                    \
        h5nullArgument(env, "third java string is NULL");                        \
    }                                                                            \
    else {                                                                       \
        (localstr) = ENVPTR->GetStringUTFChars(ENVPAR (javastr), &isCopy);           \
        if ((localstr) == NULL) {                                                    \
            h5JNIFatalError(env, "local c string is not pinned");                    \
        }                                                                            \
        else {                                                                   \
            (local2str) = ENVPTR->GetStringUTFChars(ENVPAR (java2str), &isCopy);         \
            if ((local2str) == NULL) {                                                   \
                ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));             \
                h5JNIFatalError(env, "second local c string is not pinned");             \
            }                                                                            \
            else {                                                               \
                (local3str) = ENVPTR->GetStringUTFChars(ENVPAR (java3str), &isCopy);         \
                if ((local3str) == NULL) {                                                   \
                    ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));             \
                    ENVPTR->ReleaseStringUTFChars(ENVPAR (java2str), (local2str));           \
                    h5JNIFatalError(env, "third local c string is not pinned");              \
                }                                                                \
            }                                                                    \
        }                                                                        \
    }                                                                            \
}

#define UNPIN_JAVA_STRING_THREE(javastr,localstr,java2str,local2str,java3str,local3str) {        \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (javastr), (localstr));                \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (java2str), (local2str));              \
     ENVPTR->ReleaseStringUTFChars(ENVPAR (java3str), (local3str));              \
}

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

extern jboolean h5JNIFatalError(JNIEnv *, const char *);
extern jboolean h5nullArgument(JNIEnv *, const char *);
extern jboolean h5badArgument (JNIEnv *, const char *);
extern jboolean h5outOfMemory (JNIEnv *, const char *);
extern jboolean h5libraryError(JNIEnv *env );
extern jboolean h5raiseException(JNIEnv *, const char *, const char *);
extern jboolean h5unimplemented( JNIEnv *env, const char *functName);

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
