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
#include "h5pDXPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer
 * Signature: (JJ[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer
    (JNIEnv *env, jclass clss, jlong plist, jlong size, jbyteArray tconv, jbyteArray bkg)
{
#ifdef notdef
    jboolean  isCopy;
    jbyte    *tconvP = NULL;
    jbyte    *bkgP = NULL;
#endif
    herr_t    status = FAIL;

    UNUSED(clss);
#ifndef notdef
    UNUSED(plist);
    UNUSED(size);
    UNUSED(tconv);
    UNUSED(bkg);
#endif

    H5_UNIMPLEMENTED(ENVONLY, "H5Pset_buffer: not implemented");

    /* NOTE: DON'T IMPLEMENT THIS!!! */
#ifdef notdef
    if (NULL != tconv)
        PIN_BYTE_ARRAY(ENVONLY, tconv, tconvP, &isCopy, "H5Pset_buffer: tconv not pinned");
    if (NULL != bkg)
        PIN_BYTE_ARRAY(ENVONLY, bkg, bkgP, &isCopy, "H5Pset_buffer: bkg not pinned");

    if ((status = H5Pset_buffer((hid_t)plist, (size_t)size, tconvP, bkgP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
#endif

done:
#ifdef notdef
    if (bkgP)
        UNPIN_BYTE_ARRAY(ENVONLY, bkg, bkgP, (status < 0) ? JNI_ABORT : 0);
    if (tconvP)
        UNPIN_BYTE_ARRAY(ENVONLY, tconv, tconvP, (status < 0) ? JNI_ABORT : 0);
#endif

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1buffer */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer
 * Signature: (J[B[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer
    (JNIEnv *env, jclass clss, jlong plist, jbyteArray tconv, jbyteArray bkg)
{
#ifdef notdef
    jboolean  isCopy;
    jbyte    *tconvP = NULL;
    jbyte    *bkgP = NULL;
#endif
    herr_t    status = FAIL;

    UNUSED(clss);
#ifndef notdef
    UNUSED(plist);
    UNUSED(tconv);
    UNUSED(bkg);
#endif

    H5_UNIMPLEMENTED(ENVONLY, "H5Pget_buffer: not implemented");

    /* NOTE: DON'T IMPLEMENT THIS!!! */
#ifdef notdef
    if (NULL == tconv)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_buffer: tconv input array is NULL");
    if (NULL == bkg)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_buffer: bkg array is NULL");

    PIN_BYTE_ARRAY(ENVONLY, tconv, tconvP, &isCopy, "H5Pget_buffer: tconv not pinned");
    PIN_BYTE_ARRAY(ENVONLY, bkg, bkgP, &isCopy, "H5Pget_buffer: bkg not pinned");

    if ((status = H5Pget_buffer((hid_t)plist, tconvP, bkgP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
#endif

done:
#ifdef notdef
    if (bkgP)
        UNPIN_BYTE_ARRAY(ENVONLY, bkg, bkgP, (status < 0) ? JNI_ABORT : 0);
    if (tconvP)
        UNPIN_BYTE_ARRAY(ENVONLY, tconv, tconvP, (status < 0) ? JNI_ABORT : 0);
#endif

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1buffer */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_buffer_size
 * Signature: (JJ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1buffer_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong size)
{
    UNUSED(clss);

    if (H5Pset_buffer((hid_t)plist, (size_t)size, NULL, NULL) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1buffer_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_buffer_size
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1buffer_1size
    (JNIEnv *env, jclass clss, jlong plist)
{
    size_t size = 0;

    UNUSED(clss);

    if (!(size = H5Pget_buffer((hid_t)plist, NULL, NULL)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1buffer_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_preserve
 * Signature: (JZ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1preserve
    (JNIEnv *env, jclass clss, jlong plist, jboolean status)
{
    hbool_t st = JNI_FALSE;
    herr_t  retVal = FAIL;

    UNUSED(clss);

    if (JNI_TRUE == status)
        st = TRUE;
    else if (JNI_FALSE == status)
        st = false;
    else
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_preserve: status not TRUE or FALSE");

    if ((retVal = H5Pset_preserve((hid_t)plist, st)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1preserve */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_preserve
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1preserve
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pget_preserve((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1preserve */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_edc_check
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1edc_1check
    (JNIEnv *env, jclass clss, jlong plist, jint check)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_edc_check((hid_t)plist, (H5Z_EDC_t)check)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1edc_1check */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_edc_check
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1edc_1check
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5Z_EDC_t retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Pget_edc_check((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1edc_1check */

/*
 * TODO: H5Pset_filter_callback
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_data_transform
 * Signature: (JLjava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1data_1transform
    (JNIEnv *env, jclass clss, jlong plist_id, jstring expression)
{
    const char *express = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == expression)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_data_transform: data transform expression is NULL");

    PIN_JAVA_STRING(ENVONLY, expression, express, NULL, "H5Pset_data_transform: data transform expression not pinned");

    if ((retVal = H5Pset_data_transform((hid_t)plist_id, express)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (express)
        UNPIN_JAVA_STRING(ENVONLY, expression, express);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1data_1transform */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_data_transform
 * Signature: (J[Ljava/lang/String;J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1data_1transform
    (JNIEnv *env, jclass clss, jlong plist_id, jobjectArray expression, jlong size)
{
    jstring  str = NULL;
    char    *express = NULL;
    ssize_t  express_size = -1;

    UNUSED(clss);

    if (size <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_data_transform: size <= 0");

    if ((express_size = H5Pget_data_transform((hid_t)plist_id, (char *)NULL, (size_t)size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (express = (char *) HDmalloc(sizeof(char) * (size_t)express_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_data_transform: memory allocation failed");

    if (H5Pget_data_transform((hid_t)plist_id, express, (size_t)express_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    express[express_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, express))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_data_transform: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, expression, 0, str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (express)
        HDfree(express);

    return (jlong)express_size;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1data_1transform */

/*
 * TODO: H5Pset_type_conv_cb
 */

/*
 * TODO: H5Pget_type_conv_cb
 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_hyper_vector_size
 * Signature: (JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1hyper_1vector_1size
    (JNIEnv *env, jclass clss, jlong plist, jlong vector_size)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_hyper_vector_size((hid_t)plist, (size_t)vector_size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1hyper_1vector_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_hyper_vector_size
 * Signature: (J[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1hyper_1vector_1size
    (JNIEnv *env, jclass clss, jlong plist, jlongArray vector_size)
{
    jboolean  isCopy;
    size_t    size;
    jlong    *theArray = NULL;
    herr_t    retVal = FAIL;

    UNUSED(clss);

    if (NULL == vector_size)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_hyper_vector_size: vector_size is NULL");

    PIN_LONG_ARRAY(ENVONLY, vector_size, theArray, &isCopy, "H5Pget_hyper_vector_size: vector_size not pinned");

    if ((retVal =  H5Pget_hyper_vector_size((hid_t)plist, &size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jlong)size;

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, vector_size, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1hyper_1vector_1size */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_btree_ratios
 * Signature: (JDDD)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1btree_1ratios
    (JNIEnv *env, jclass clss, jlong plist_id, jdouble left, jdouble middle, jdouble right)
{
    herr_t status = FAIL;

    UNUSED(clss);

    if ((status = H5Pset_btree_ratios((hid_t)plist_id, (double)left,(double)middle, (double)right)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1btree_1ratios */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_btree_ratios
 * Signature: (J[D[D[D)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1btree_1ratios
    (JNIEnv *env, jclass clss, jlong plist_id, jdoubleArray left,
        jdoubleArray middle, jdoubleArray right)
{
    jboolean  isCopy;
    jdouble  *leftP = NULL;
    jdouble  *middleP = NULL;
    jdouble  *rightP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == left)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_btree_ratios: left input array is NULL");
    if (NULL == middle)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_btree_ratios: middle input array is NULL");
    if (NULL == right)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_btree_ratios: right input array is NULL");

    PIN_DOUBLE_ARRAY(ENVONLY, left, leftP, &isCopy, "H5Pget_btree_ratios: left array not pinned");
    PIN_DOUBLE_ARRAY(ENVONLY, middle, middleP, &isCopy, "H5Pget_btree_ratios: middle array not pinned");
    PIN_DOUBLE_ARRAY(ENVONLY, right, rightP, &isCopy, "H5Pget_btree_ratios: right array not pinned");

    if ((status = H5Pget_btree_ratios((hid_t)plist_id, (double *)leftP,
            (double *)middleP, (double *)rightP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (rightP)
        UNPIN_DOUBLE_ARRAY(ENVONLY, right, rightP, (status < 0) ? JNI_ABORT : 0);
    if (middleP)
        UNPIN_DOUBLE_ARRAY(ENVONLY, middle, middleP, (status < 0) ? JNI_ABORT : 0);
    if (leftP)
        UNPIN_DOUBLE_ARRAY(ENVONLY, left, leftP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1btree_1ratios */

/*
 * TODO: H5Pset_vlen_mem_manager
 */

/*
 * TODO: H5Pget_vlen_mem_manager
 */

/*
 * TODO: H5Pset_dxpl_mpio
 */

/*
 * TODO: H5Pget_dxpl_mpio
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt_num
 */

/*
 * TODO: H5Pset_dxpl_mpio_chunk_opt_ratio
 */

/*
 * TODO: H5Pset_dxpl_mpio_collective_opt
 */

/*
 * TODO: H5Pget_mpio_actual_chunk_opt_mode
 */

/*
 * TODO: H5Pget_mpio_actual_io_mode
 */

/*
 * TODO: H5Pget_mpio_no_collective_cause
 */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
