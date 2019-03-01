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


/***********/
/* Headers */
/***********/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "jni.h"
#include "h5jni.h"
#include "exceptionImp.h"

extern H5E_auto2_t  efunc;
extern void        *edata;


/*******************/
/* Local Variables */
/*******************/

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

#define THROWEXCEPTION(className, args)                                                       \
{                                                                                             \
    jmethodID jm;                                                                             \
    jclass    jc;                                                                             \
    jobject   ex;                                                                             \
                                                                                              \
    if (NULL == (jc = ENVPTR->FindClass(ENVONLY, (className))))                               \
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);                                              \
                                                                                              \
    if (NULL == (jm = ENVPTR->GetMethodID(ENVONLY, jc, "<init>", "(Ljava/lang/String;)V"))) { \
        HDprintf("THROWEXCEPTION FATAL ERROR: GetMethodID failed\n");                         \
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);                                              \
    }                                                                                         \
                                                                                              \
    if (NULL == (ex = ENVPTR->NewObjectA(ENVONLY, jc, jm, (jvalue *)(args)))) {              \
        HDprintf("THROWEXCEPTION FATAL ERROR: Class %s: Creation failed\n", (className));     \
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);                                              \
    }                                                                                         \
                                                                                              \
    if (ENVPTR->Throw(ENVONLY, (jthrowable)ex) < 0) {                                         \
        HDprintf("THROWEXCEPTION FATAL ERROR: Class %s: Throw failed\n", (className));        \
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);                                              \
    }                                                                                         \
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

    UNUSED(n);

    if (err_desc) {
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
    UNUSED(env);
    UNUSED(clss);

    if (H5Eget_auto2(H5E_DEFAULT, &efunc, &edata) < 0)
        return -1;

    if (H5Eset_auto2(H5E_DEFAULT, NULL, NULL) < 0)
        return -1;

    return 0;
} /* end Java_hdf_hdf5lib_H5_H5error_1off() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDF5Library
 * Method:    H5error_on
 * Signature: ()V
 *
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5error_1on
    (JNIEnv *env, jclass clss)
{
    UNUSED(env);
    UNUSED(clss);

    H5Eset_auto2(H5E_DEFAULT, efunc, edata);
} /* end Java_hdf_hdf5lib_H5_H5error_1on() */

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

    UNUSED(obj);

    if (NULL == file_name) {
        H5Eprint2(H5E_DEFAULT, stderr);
    }
    else {
        PIN_JAVA_STRING(ENVONLY, file_name, file, NULL, "printStackTrace0: file name not pinned");

        if ((stream = HDfopen(file, "a+"))) {
            H5Eprint2(H5E_DEFAULT, stream);
            HDfclose(stream);
        }
    }

done:
    if (file)
        UNPIN_JAVA_STRING(ENVONLY, file_name, file);

    return;
} /* end  Java_hdf_hdf5lib_exceptions_HDF5LibraryException_printStackTrace0() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    _getMajorErrorNumber
 * Signature: ()J
 *
 *  Extract the HDF-5 major error number from the HDF-5 error stack.
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException__1getMajorErrorNumber
    (JNIEnv *env, jobject obj)
{
    H5E_num_t err_nums;

    UNUSED(env);
    UNUSED(obj);

    err_nums.maj_num = 0;
    err_nums.min_num = 0;

    if (H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, &err_nums) < 0)
        return -1;

    return err_nums.maj_num;
} /* end Java_hdf_hdf5lib_exceptions_HDF5LibraryException__1getMajorErrorNumber() */

/*
 * Class:     hdf_hdf5lib_exceptions_HDFLibraryException
 * Method:    _getMinorErrorNumber
 * Signature: ()J
 *
 *  Extract the HDF-5 minor error number from the HDF-5 error stack.
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_exceptions_HDF5LibraryException__1getMinorErrorNumber
    (JNIEnv *env, jobject obj)
{
    H5E_num_t err_nums;

    UNUSED(env);
    UNUSED(obj);

    err_nums.maj_num = 0;
    err_nums.min_num = 0;

    if (H5Ewalk2(H5E_DEFAULT, H5E_WALK_DOWNWARD, walk_error_callback, &err_nums) < 0)
        return -1;

    return err_nums.min_num;
} /* end Java_hdf_hdf5lib_exceptions_HDF5LibraryException__1getMinorErrorNumber() */

/*
 *  Routine to raise particular Java exceptions from C.
 */
static jboolean
H5JNIErrorClass
    (JNIEnv *env, const char *message, const char *className)
{
    jstring   str;
    char     *args[2];
    jboolean  retVal = JNI_FALSE;

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, message)))
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

    args[0] = (char *) str;
    args[1] = 0;

    THROWEXCEPTION(className, args);

    retVal = JNI_TRUE;

done:
    return retVal;
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
    (JNIEnv *env, const char *message, const char *exception)
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
    const char *exception = NULL;
    H5E_type_t  error_msg_type;
    H5E_num_t   exceptionNumbers;
    jstring     str = NULL;
    ssize_t     msg_size = 0;
    hid_t       min_num;
    hid_t       maj_num;
    hid_t       stk_id = H5I_INVALID_HID;
    char       *args[2];
    char       *msg_str = NULL;
    jboolean    retVal = JNI_FALSE;

    exceptionNumbers.maj_num = 0;
    exceptionNumbers.min_num = 0;

    /* Save current stack contents for future use */
    if ((stk_id = H5Eget_current_stack()) >= 0)
        /* This will clear current stack */
        if (H5Ewalk2(stk_id, H5E_WALK_DOWNWARD, walk_error_callback, &exceptionNumbers) < 0)
            goto done;

    maj_num = exceptionNumbers.maj_num;
    min_num = exceptionNumbers.min_num;

    /*
     * TODO:  handle < 0 case.
     */

    /*
     * No error detected in HDF5 error stack.
     */
    if (!maj_num && !min_num)
        goto done;

    exception = defineHDF5LibraryException(maj_num);

    /* get the length of the name */
    if ((msg_size = H5Eget_msg(min_num, NULL, NULL, 0)) < 0)
        goto done;

    if (msg_size > 0) {
        if (NULL == (msg_str = (char *) HDcalloc((size_t)msg_size + 1, sizeof(char))))
            H5_JNI_FATAL_ERROR(ENVONLY, "h5libraryerror: failed to allocate buffer for error message");

        if ((msg_size = H5Eget_msg(min_num, &error_msg_type, msg_str, (size_t)msg_size + 1)) < 0)
            goto done;
        msg_str[msg_size] = '\0';

        if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, msg_str)))
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }
    else
        str = NULL;

    if (stk_id >= 0)
        H5Eset_current_stack(stk_id);

    args[0] = (char *) str;
    args[1] = 0;

    THROWEXCEPTION(exception, args);

    retVal = JNI_TRUE;

done:
    if (msg_str)
        HDfree(msg_str);

    return retVal;
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

