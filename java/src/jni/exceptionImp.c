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


/***********/
/* Headers */
/***********/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "jni.h"
#include "h5jni.h"
#include "exceptionImp.h"


/*******************/
/* Local Variables */
/*******************/

/*  These types are copied from H5Eprivate.h
 *  They should be moved to a public include file, and deleted from
 *  here.
 */

#define H5E_NSLOTS      32      /*number of slots in an error stack */

/*
 * The list of error messages in the system is kept as an array of
 * error_code/message pairs, one for major error numbers and another for
 * minor error numbers.
 */
typedef struct H5E_major_mesg_t {
    hid_t error_code;
    const char  *str;
} H5E_major_mesg_t;

typedef struct H5E_minor_mesg_t {
    hid_t error_code;
    const char  *str;
} H5E_minor_mesg_t;

/* major and minor error numbers */
typedef struct H5E_num_t {
    hid_t maj_num;
    hid_t min_num;
} H5E_num_t;

/********************/
/* Local Macros     */
/********************/

#define THROWEXCEPTION(className,args) {                                    \
    jclass     jc;                                                          \
    jmethodID  jm;                                                          \
    jobject    ex;                                                          \
    jc = ENVPTR->FindClass(ENVPAR (className));                             \
    if (jc == NULL) {                                                       \
        return JNI_FALSE;                                                   \
    }                                                                       \
    jm = ENVPTR->GetMethodID(ENVPAR jc, "<init>", "(Ljava/lang/String;)V"); \
    if (jm == NULL) {                                                       \
        return JNI_FALSE;                                                   \
    }                                                                       \
    ex = ENVPTR->NewObjectA (ENVPAR jc, jm, (jvalue*)(args));               \
    if (ENVPTR->Throw(ENVPAR (jthrowable)ex) < 0) {                         \
        printf("FATAL ERROR:  %s: Throw failed\n", (className));            \
        return JNI_FALSE;                                                   \
    }                                                                       \
    return JNI_TRUE;                                                        \
}

/********************/
/* Local Prototypes */
/********************/

static const char *defineHDF5LibraryException(hid_t maj_num);
static jboolean H5JNIErrorClass(JNIEnv *env, const char *message, const char *className);

/* get the major and minor error numbers on the top of the error stack */
static herr_t
walk_error_callback
    (unsigned n, const H5E_error2_t *err_desc, void *_err_nums)
{
    H5E_num_t *err_nums = (H5E_num_t *)_err_nums;

    if(err_desc) {
        err_nums->maj_num = err_desc->maj_num;
        err_nums->min_num = err_desc->min_num;
    } /* end if */

    return 0;
} /* end walk_error_callback() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDF5Library
 * Method:    H5error_off
 * Signature: ()I
 *
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5error_1off
    (JNIEnv *env, jclass clss)
{
    H5Eset_auto2(H5E_DEFAULT, NULL, NULL);
    return 0;
} /* end Java_hdf_hdf5lib_H5_H5error_1off() */


/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    printStackTrace0
 * Signature: (Ljava/lang/Object;)V
 *
 *  Call the HDF-5 library to print the HDF-5 error stack to 'file_name'.
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_printStackTrace0
    (JNIEnv *env, jobject obj, jstring file_name)
{
    FILE       *stream = NULL;
    const char *file = NULL;

    if(file_name == NULL) {
        H5Eprint2(H5E_DEFAULT, stderr);
    } /* end if */
    else {
        file = ENVPTR->GetStringUTFChars(ENVPAR file_name, 0);
        stream = HDfopen(file, "a+");
        if(stream) {
            H5Eprint2(H5E_DEFAULT, stream);
            HDfclose(stream);
        } /* end if */
        ENVPTR->ReleaseStringUTFChars(ENVPAR file_name, file);
    } /* end else */
} /* end  Java_hdf_hdf5lib_exceptions_HDF5LibraryException_printStackTrace0() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMajorErrorNumber
 * Signature: ()J
 *
 *  Extract the HDF-5 major error number from the HDF-5 error stack.
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMajorErrorNumber
    (JNIEnv *env, jobject obj)
{
    H5E_num_t err_nums;
    err_nums.maj_num = 0;
    err_nums.min_num = 0;

    H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, &err_nums);

    return err_nums.maj_num;
} /* end Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMajorErrorNumber() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    getMinorErrorNumber
 * Signature: ()J
 *
 *  Extract the HDF-5 minor error number from the HDF-5 error stack.
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMinorErrorNumber
    (JNIEnv *env, jobject obj)
{
    H5E_num_t err_nums;
    err_nums.maj_num = 0;
    err_nums.min_num = 0;

    H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, &err_nums);

    return err_nums.min_num;
} /* end Java_hdf_hdf5lib_exceptions_HDF5LibraryException_getMinorErrorNumber() */

/*
 *  Routine to raise particular Java exceptions from C
 */
static jboolean
H5JNIErrorClass
    (JNIEnv *env, const char *message, const char *className)
{
    char *args[2];
    jstring str = ENVPTR->NewStringUTF(ENVPAR message);
    args[0] = (char *)str;
    args[1] = 0;

    THROWEXCEPTION(className, args);
} /* end H5JNIErrorClass() */

/*
 *  Create and throw an 'outOfMemoryException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5outOfMemory
    (JNIEnv *env, const char *functName)
{
    return H5JNIErrorClass(env, functName, "java/lang/OutOfMemoryError");
} /* end h5outOfMemory() */


/*
 *  A fatal error in a JNI call
 *  Create and throw an 'InternalError'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5JNIFatalError
    (JNIEnv *env, const char *functName)
{
    return H5JNIErrorClass(env, functName, "java/lang/InternalError");
} /* end h5JNIFatalError() */

/*
 *  A NULL argument in an HDF5 call
 *  Create and throw an 'NullPointerException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5nullArgument
    (JNIEnv *env, const char *functName)
{
    return H5JNIErrorClass(env, functName, "java/lang/NullPointerException");
} /* end h5nullArgument() */

/*
 *  A bad argument in an HDF5 call
 *  Create and throw an 'IllegalArgumentException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5badArgument
    (JNIEnv *env, const char *functName)
{
    return H5JNIErrorClass(env, functName, "java/lang/IllegalArgumentException");
} /* end h5badArgument() */

/*
 *  Some feature Not implemented yet
 *  Create and throw an 'UnsupportedOperationException'
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5unimplemented
    (JNIEnv *env, const char *functName)
{
    return H5JNIErrorClass(env, functName, "java/lang/UnsupportedOperationException");
} /* end h5unimplemented() */

/*  h5raiseException().  This routine is called to generate
 *  an arbitrary Java exception with a particular message.
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5raiseException
    (JNIEnv *env, const char *exception, const char *message)
{
    return H5JNIErrorClass(env, message, exception);
} /* end h5raiseException() */

/*
 *  h5libraryError()   determines the HDF-5 major error code
 *  and creates and throws the appropriate sub-class of
 *  HDF5LibraryException().  This routine should be called
 *  whenever a call to the HDF-5 library fails, i.e., when
 *  the return is -1.
 *
 *  Note:  This routine never returns from the 'throw',
 *  and the Java native method immediately raises the
 *  exception.
 */
jboolean
h5libraryError
    (JNIEnv *env)
{
    char       *args[2];
    const char *exception = NULL;
    char       *msg_str = NULL;
    int         num_errs = 0;
    hid_t       min_num;
    hid_t       maj_num;
    ssize_t     msg_size = 0;
    H5E_type_t  error_msg_type;
    jstring     str = NULL;
    hid_t       stk_id = -1;
    H5E_num_t   exceptionNumbers;

    exceptionNumbers.maj_num = 0;
    exceptionNumbers.min_num = 0;

    /* Save current stack contents for future use */
    stk_id = H5Eget_current_stack(); /* This will clear current stack  */
    if(stk_id >= 0)
        H5Ewalk2(stk_id, H5E_WALK_DOWNWARD, walk_error_callback, &exceptionNumbers);
    maj_num = exceptionNumbers.maj_num;
    min_num = exceptionNumbers.min_num;

    exception = defineHDF5LibraryException(maj_num);

    /* get the length of the name */
    msg_size = H5Eget_msg(min_num, NULL, NULL, 0);
    if(msg_size > 0) {
        msg_size++; /* add extra space for the null terminator */
        msg_str = (char*)HDcalloc((size_t)msg_size, sizeof(char));
        if(msg_str) {
            msg_size = H5Eget_msg(min_num, &error_msg_type, (char *)msg_str, (size_t)msg_size);
            str = ENVPTR->NewStringUTF(ENVPAR msg_str);
            HDfree(msg_str);
        } /* end if */
    } /* end if */
    else
        str = NULL;
    if(stk_id >= 0)
        H5Eset_current_stack(stk_id);

    args[0] = (char *)str;
    args[1] = 0;
    THROWEXCEPTION(exception, args);
} /* end h5libraryError() */


/*
 *  defineHDF5LibraryException()  returns the name of the sub-class
 *  which goes with an HDF-5 error code.
 */
static const char *
defineHDF5LibraryException
    (hid_t maj_num)
{
    hid_t err_num = maj_num;

    if (H5E_ARGS == err_num)
        return "hdf/hdf5lib/exceptions/HDF5FunctionArgumentException";
    else if (H5E_RESOURCE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5ResourceUnavailableException";
    else if (H5E_INTERNAL == err_num)
        return "hdf/hdf5lib/exceptions/HDF5InternalErrorException";
    else if (H5E_FILE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5FileInterfaceException";
    else if (H5E_IO == err_num)
        return "hdf/hdf5lib/exceptions/HDF5LowLevelIOException";
    else if (H5E_FUNC == err_num)
        return "hdf/hdf5lib/exceptions/HDF5FunctionEntryExitException";
    else if (H5E_ATOM == err_num)
        return "hdf/hdf5lib/exceptions/HDF5AtomException";
    else if (H5E_CACHE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5MetaDataCacheException";
    else if (H5E_BTREE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5BtreeException";
    else if (H5E_SYM == err_num)
        return "hdf/hdf5lib/exceptions/HDF5SymbolTableException";
    else if (H5E_HEAP == err_num)
        return "hdf/hdf5lib/exceptions/HDF5HeapException";
    else if (H5E_OHDR == err_num)
        return "hdf/hdf5lib/exceptions/HDF5ObjectHeaderException";
    else if (H5E_DATATYPE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5DatatypeInterfaceException";
    else if (H5E_DATASPACE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5DataspaceInterfaceException";
    else if (H5E_DATASET == err_num)
        return "hdf/hdf5lib/exceptions/HDF5DatasetInterfaceException";
    else if (H5E_STORAGE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5DataStorageException";
    else if (H5E_PLIST == err_num)
        return "hdf/hdf5lib/exceptions/HDF5PropertyListInterfaceException";
    else if (H5E_ATTR == err_num)
        return "hdf/hdf5lib/exceptions/HDF5AttributeException";
    else if (H5E_PLINE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5DataFiltersException";
    else if (H5E_EFL == err_num)
        return "hdf/hdf5lib/exceptions/HDF5ExternalFileListException";
    else if (H5E_REFERENCE == err_num)
        return "hdf/hdf5lib/exceptions/HDF5ReferenceException";

    return "hdf/hdf5lib/exceptions/HDF5LibraryException";
} /* end  defineHDF5LibraryException() */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */

