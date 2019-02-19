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
#include "h5pDCPLImp.h"

/*
 * Pointer to the JNI's Virtual Machine; used for callback functions.
 */
/* extern JavaVM *jvm; */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_layout
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1layout
    (JNIEnv *env, jclass clss, jlong plist, jint layout)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_layout((hid_t)plist, (H5D_layout_t)layout)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1layout */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_layout
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1layout
    (JNIEnv *env, jclass clss, jlong plist)
{
    H5D_layout_t retVal = H5D_LAYOUT_ERROR;

    UNUSED(clss);

    if (H5D_LAYOUT_ERROR == (retVal = H5Pget_layout((hid_t)plist)))
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1layout */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk
 * Signature: (JI[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk
    (JNIEnv *env, jclass clss, jlong plist, jint ndims, jbyteArray dim)
{
    jboolean  isCopy;
    hsize_t  *da = NULL;
    hsize_t  *lp = NULL;
    size_t    i;
    size_t    rank;
    jsize     arrLen;
    jbyte    *theArray = NULL;
    jlong    *jlp = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (ndims < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_chunk: ndims < 0");
    if (NULL == dim)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_chunk: dim array is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, dim)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_chunk: dim array length < 0");
    }

    rank = (size_t) arrLen / sizeof(jlong);
    if (rank < (size_t) ndims)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_chunk: dims array rank < ndims");

    PIN_BYTE_ARRAY(ENVONLY, dim, theArray, &isCopy, "H5Pset_chunk: dim array not pinned");

    if (NULL == (da = lp = (hsize_t *) HDmalloc(rank * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pset_chunk: memory allocation failed");

    jlp = (jlong *)theArray;
    for (i = 0; i < rank; i++) {
        *lp = (hsize_t)*jlp;
        lp++;
        jlp++;
    } /* end if */

    if ((status = H5Pset_chunk((hid_t)plist, (int)ndims, da)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (da)
        HDfree(da);
    if (theArray)
        UNPIN_BYTE_ARRAY(ENVONLY, dim, theArray, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1chunk */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk
 * Signature: (JI[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk
    (JNIEnv *env, jclass clss, jlong plist, jint max_ndims, jlongArray dims)
{
    jboolean  isCopy;
    hsize_t  *da = NULL;
    jlong    *theArray = NULL;
    jsize     arrLen;
    int       i;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == dims)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_chunk: dims is NULL");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, dims)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_chunk: dims array length < 0");
    }
    if (arrLen < max_ndims)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_chunk: dims array < max_ndims");

    PIN_LONG_ARRAY(ENVONLY, dims, theArray, &isCopy, "H5Pget_chunk: input dims not pinned");

    if (NULL == (da = (hsize_t *) HDmalloc((size_t)max_ndims * sizeof(hsize_t))))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_chunk: memory allocation failed");

    if ((status = H5Pget_chunk((hid_t)plist, (int)max_ndims, da)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    for (i = 0; i < max_ndims; i++)
        theArray[i] = (jlong)da[i];

done:
    if (da)
        HDfree(da);
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, dims, theArray, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1chunk */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_chunk_opts
 * Signature: (JI)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1chunk_1opts
    (JNIEnv *env, jclass clss, jlong dcpl_id, jint opts)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_chunk_opts((hid_t)dcpl_id, (unsigned)opts)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1chunk_1opts */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_chunk_opts
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1chunk_1opts
    (JNIEnv *env, jclass clss, jlong dcpl_id)
{
    unsigned opts = 0;

    UNUSED(clss);

    if (H5Pget_chunk_opts((hid_t)dcpl_id, &opts) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)opts;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1chunk_1opts */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_dset_no_attrs_hint
 * Signature: (JZ)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1dset_1no_1attrs_1hint
(JNIEnv *env, jclass clss, jlong dcpl_id, jboolean minimize)
{
    hbool_t minimize_val;
    herr_t  retVal = FAIL;

    UNUSED(clss);

    minimize_val = (JNI_TRUE == minimize) ? TRUE : FALSE;

    if ((retVal = H5Pset_dset_no_attrs_hint((hid_t)dcpl_id, (hbool_t)minimize_val)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1dset_1no_1attrs_1hint */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_dset_no_attrs_hint
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1dset_1no_1attrs_1hint
(JNIEnv *env, jclass clss, jlong dcpl_id)
{
    hbool_t  minimize = FALSE;
    jboolean bval = JNI_FALSE;

    UNUSED(clss);

    if (H5Pget_dset_no_attrs_hint((hid_t)dcpl_id, (hbool_t *)&minimize) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (minimize == TRUE)
        bval = JNI_TRUE;

done:
    return bval;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1dset_1no_1attrs_1hint */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_deflate
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1deflate
    (JNIEnv *env, jclass clss, jlong plist, jint level)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_deflate((hid_t)plist, (unsigned)level)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1deflate */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1value
    (JNIEnv *env, jclass clss, jlong plist_id, jlong type_id, jbyteArray value)
{
    jboolean  isCopy;
    jbyte    *byteP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == value) {
        if ((status = H5Pset_fill_value((hid_t)plist_id, (hid_t)type_id, byteP)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }
    else {
        PIN_BYTE_ARRAY(ENVONLY, value, byteP, &isCopy, "H5Pget_fill_value: value array not pinned");

        if ((status = H5Pset_fill_value((hid_t)plist_id, (hid_t)type_id, byteP)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }

done:
    if (byteP)
        UNPIN_BYTE_ARRAY(ENVONLY, value, byteP, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fill_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_value
 * Signature: (JJ[B)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1value
    (JNIEnv *env, jclass clss, jlong plist_id, jlong type_id, jbyteArray value)
{
    jboolean  isCopy;
    herr_t    status = FAIL;
    jbyte    *byteP = NULL;

    UNUSED(clss);

    if (NULL == value)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_fill_value: value is NULL");

    PIN_BYTE_ARRAY(ENVONLY, value, byteP, &isCopy, "H5Pget_fill_value: value array not pinned");

    if ((status = H5Pget_fill_value((hid_t)plist_id, (hid_t)type_id, byteP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (byteP)
        UNPIN_BYTE_ARRAY(ENVONLY, value, byteP, (status < 0) ? JNI_ABORT : 0);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fill_1value */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pfill_value_defined
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined
    (JNIEnv *env, jclass clss, jlong plist, jintArray status)
{
    H5D_fill_value_t  value = H5D_FILL_VALUE_ERROR;
    jboolean          isCopy;
    jint             *theArray = NULL;
    herr_t            retVal = FAIL;

    UNUSED(clss);

    if (NULL == status)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pfill_value_defined: status is NULL");

    PIN_INT_ARRAY(ENVONLY, status, theArray, &isCopy, "H5Pfill_value_defined: status not pinned");

    if ((retVal = H5Pfill_value_defined((hid_t)plist, &value)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jint)value;

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, status, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pfill_1value_1defined */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fill_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fill_1time
    (JNIEnv *env, jclass clss, jlong plist, jint fill_time)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fill_time((hid_t)plist, (H5D_fill_time_t)fill_time)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fill_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_fill_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1fill_1time
    (JNIEnv *env, jclass clss, jlong plist, jintArray fill_time)
{
    H5D_fill_time_t  time = H5D_FILL_TIME_ERROR;
    jboolean         isCopy;
    jint            *theArray = NULL;
    herr_t           retVal = FAIL;

    UNUSED(clss);

    if (NULL == fill_time)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_fill_time: fill_time is NULL");

    PIN_INT_ARRAY(ENVONLY, fill_time, theArray, &isCopy, "H5Pget_fill_time: fill_time not pinned");

    if ((retVal = H5Pget_fill_time((hid_t)plist, &time)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = (jint)time;

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, fill_time, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1fill_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_alloc_time
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1alloc_1time
    (JNIEnv *env, jclass clss, jlong plist, jint alloc_time)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_alloc_time((hid_t)plist, (H5D_alloc_time_t)alloc_time)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1alloc_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_alloc_time
 * Signature: (J[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1alloc_1time
    (JNIEnv *env, jclass clss, jlong plist, jintArray alloc_time)
{
    H5D_alloc_time_t  time = H5D_ALLOC_TIME_ERROR;
    jboolean          isCopy;
    jint             *theArray = NULL;
    herr_t            retVal = FAIL;

    UNUSED(clss);

    if (NULL == alloc_time)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_alloc_time: alloc_time is NULL");

    PIN_INT_ARRAY(ENVONLY, alloc_time, theArray, &isCopy, "H5Pget_alloc_time: alloc_time not pinned");

    if ((retVal =  H5Pget_alloc_time((hid_t)plist, &time)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    theArray[0] = time;

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, alloc_time, theArray, (retVal < 0) ? JNI_ABORT : 0);

    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1alloc_1time */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter, jint flags,
  jlong cd_nelmts, jintArray cd_values)
{
    jboolean  isCopy;
    jint     *theArray = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == cd_values) {
        if ((status = H5Pset_filter((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int)flags, (size_t)cd_nelmts, NULL)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }
    else {
        PIN_INT_ARRAY(ENVONLY, cd_values, theArray, &isCopy, "H5Pset_filter: input array not pinned");

        if ((status = H5Pset_filter((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int)flags, (size_t)cd_nelmts, (const unsigned int *)theArray)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);
    }

done:
    if (theArray)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, theArray, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pall_filters_avail
 * Signature: (J)Z
 */
JNIEXPORT jboolean JNICALL
Java_hdf_hdf5lib_H5_H5Pall_1filters_1avail
    (JNIEnv *env, jclass clss, jlong dcpl_id)
{
    htri_t bval = JNI_FALSE;

    UNUSED(clss);

    if ((bval = H5Pall_filters_avail((hid_t)dcpl_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    bval = (bval > 0) ? JNI_TRUE : JNI_FALSE;

done:
    return (jboolean)bval;
} /* end Java_hdf_hdf5lib_H5_H5Pall_1filters_1avail */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_nfilters
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1nfilters
    (JNIEnv *env, jclass clss, jlong plist)
{
    int retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Pget_nfilters((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1nfilters */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter_number, jintArray flags,
        jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name)
{
    jboolean  isCopy;
    jstring   str;
    jint     *flagsArray = NULL;
    jlong    *cd_nelmtsArray = NULL;
    jint     *cd_valuesArray = NULL;
    char     *filter = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (namelen <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter: namelen <= 0");
    if (NULL == flags)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter: flags is NULL");
    if (NULL == cd_nelmts)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter: cd_nelmts is NULL");
    if (NULL == cd_values)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter: cd_values is NULL");

    if (NULL == (filter = (char *) HDmalloc(sizeof(char) * (size_t)namelen)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter: memory allocation failed");

    PIN_INT_ARRAY(ENVONLY, flags, flagsArray, &isCopy, "H5Pget_filter: flags array not pinned");
    PIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, &isCopy, "H5Pget_filter: nelmts array not pinned");
    PIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, &isCopy, "H5Pget_filter: elmts array not pinned");


    /* direct cast (size_t *)variable fails on 32-bit environment */
    {
        unsigned int filter_config;
        long long    cd_nelmts_temp = *cd_nelmtsArray;
        size_t       cd_nelmts_t = (size_t) cd_nelmts_temp;

        if ((status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                (size_t)namelen, filter, &filter_config)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        filter[namelen - 1] = '\0';

        *cd_nelmtsArray = (jlong)cd_nelmts_t;
    }

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, filter))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (cd_valuesArray)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, (status < 0) ? JNI_ABORT : 0);
    if (cd_nelmtsArray)
        UNPIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, (status < 0) ? JNI_ABORT : 0);
    if (flagsArray)
        UNPIN_INT_ARRAY(ENVONLY, flags, flagsArray, (status < 0) ? JNI_ABORT : 0)
    if (filter)
        HDfree(filter);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter2
    (JNIEnv *env, jclass clss, jlong plist, jint filter_number,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen,
        jobjectArray name, jintArray filter_config)
{
    jboolean  isCopy;
    jstring   str;
    jint     *flagsArray = NULL;
    jlong    *cd_nelmtsArray = NULL;
    jint     *cd_valuesArray = NULL;
    jint     *filter_configArray = NULL;
    char     *filter = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (namelen <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter2: namelen <= 0");
    if (NULL == flags)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter2: flags is NULL");
    if (NULL == cd_nelmts)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter2: cd_nelmts is NULL");
    if (NULL == filter_config)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter2: filter_config is NULL");

    if (NULL == (filter = (char *) HDmalloc(sizeof(char) * (size_t)namelen)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter2: memory allocation failed");

    PIN_INT_ARRAY(ENVONLY, flags, flagsArray, &isCopy, "H5Pget_filter2: flags array not pinned");
    PIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, &isCopy, "H5Pget_filter2: nelmts array not pinned");
    PIN_INT_ARRAY(ENVONLY, filter_config, filter_configArray, &isCopy, "H5Pget_filter2: filter_config array not pinned");

    if (NULL == cd_values && *cd_nelmtsArray == 0) {
        /* direct cast (size_t *)variable fails on 32-bit environment */
        long long cd_nelmts_temp = 0;
        size_t cd_nelmts_t = (size_t) cd_nelmts_temp;

        if ((status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                (unsigned int *)flagsArray, &cd_nelmts_t, NULL,
                (size_t)namelen, filter, (unsigned int *)filter_configArray)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        filter[namelen - 1] = '\0';

        *cd_nelmtsArray = (jlong)cd_nelmts_t;
    }
    else {
        if (NULL == cd_values)
            H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter2: cd_values is NULL");

        PIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, &isCopy, "H5Pget_filter2: elmts array not pinned");

        {
            /* direct cast (size_t *)variable fails on 32-bit environment */
            long long cd_nelmts_temp = *cd_nelmtsArray;
            size_t    cd_nelmts_t = (size_t) cd_nelmts_temp;

            if ((status = H5Pget_filter2((hid_t)plist, (unsigned)filter_number,
                    (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                    (size_t)namelen, filter, (unsigned int *)filter_configArray)) < 0)
                H5_LIBRARY_ERROR(ENVONLY);

            filter[namelen - 1] = '\0';

            *cd_nelmtsArray = (jlong)cd_nelmts_t;
        } /* end direct cast special */
    }

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, filter))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter2: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (cd_valuesArray)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, (status < 0) ? JNI_ABORT : 0);
    if (filter_configArray)
        UNPIN_INT_ARRAY(ENVONLY, filter_config, filter_configArray, (status < 0) ? JNI_ABORT : 0);
    if (cd_nelmtsArray)
        UNPIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, (status < 0) ? JNI_ABORT : 0);
    if (flagsArray)
        UNPIN_INT_ARRAY(ENVONLY, flags, flagsArray, (status < 0) ? JNI_ABORT : 0);
    if (filter)
        HDfree(filter);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id
 * Signature: (JI[I[J[IJ[Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name)
{
    jboolean  isCopy;
    jstring   str;
    jlong    *cd_nelmtsArray = NULL;
    jint     *cd_valuesArray = NULL;
    jint     *flagsArray = NULL;
    char     *aName = NULL;
    long      bs;
    int       rank;
    herr_t    status = FAIL;

    UNUSED(clss);

    bs = (long)namelen;

    if (bs <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: namelen <= 0");
    if (NULL == flags)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: flags is NULL");
    if (NULL == cd_nelmts)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: cd_nelmts is NULL");
    if (NULL == cd_values)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: cd_values is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: name is NULL");

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)bs)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter_by_id: memory allocation failed");

    PIN_INT_ARRAY(ENVONLY, flags, flagsArray, &isCopy, "H5Pget_filter_by_id: flags not pinned");
    PIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, &isCopy, "H5Pget_filter_by_id: cd_nelms not pinned");
    PIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, &isCopy, "H5Pget_filter_by_id: cd_values array not pinned");

    if ((rank = ENVPTR->GetArrayLength(ENVONLY, cd_values)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id: cd_values array length < 0");
    }

    {
        /* direct cast (size_t *)variable fails on 32-bit environment */
        unsigned int filter_config;
        long long    cd_nelmts_temp = *cd_nelmtsArray;
        size_t       cd_nelmts_t = (size_t) cd_nelmts_temp;

        if ((status = H5Pget_filter_by_id2((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                (size_t)namelen, (char *)aName, &filter_config)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        aName[bs - 1] = '\0';

        *cd_nelmtsArray = (jlong)cd_nelmts_t;
    } /* end direct cast special */

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter_by_id: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (cd_valuesArray)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, (status < 0) ? JNI_ABORT : 0);
    if (cd_nelmtsArray)
        UNPIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, (status < 0) ? JNI_ABORT : 0);
    if (flagsArray)
        UNPIN_INT_ARRAY(ENVONLY, flags, flagsArray, (status < 0) ? JNI_ABORT : 0);
    if (aName)
        HDfree(aName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_filter_by_id2
 * Signature: (JI[I[J[IJ[Ljava/lang/String;[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jintArray flags, jlongArray cd_nelmts, jintArray cd_values, jlong namelen, jobjectArray name, jintArray filter_config)
{
    jboolean  isCopy;
    jstring   str;
    jlong    *cd_nelmtsArray = NULL;
    jint     *cd_valuesArray = NULL;
    jint     *flagsArray = NULL;
    jint     *filter_configArray = NULL;
    long      bs;
    char     *aName = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    bs = (long)namelen;

    if (bs <= 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: namelen <= 0");
    if (NULL == flags)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: flags is NULL");
    if (NULL == cd_nelmts)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: cd_nelmts is NULL");
    if (NULL == cd_values)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: cd_values is NULL");
    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: name is NULL");
    if (NULL == filter_config)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pget_filter_by_id2: filter_config is NULL");

    if (NULL == (aName = (char *) HDmalloc(sizeof(char) * (size_t)bs)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter_by_id2: memory allocation failed");

    PIN_INT_ARRAY(ENVONLY, flags, flagsArray, &isCopy, "H5Pget_filter_by_id2: flags not pinned");
    PIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, &isCopy, "H5Pget_filter_by_id2: cd_nelms not pinned");
    PIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, &isCopy, "H5Pget_filter_by_id2: cd_values array not pinned");
    PIN_INT_ARRAY(ENVONLY, filter_config, filter_configArray, &isCopy, "H5Pget_filter_by_id2: filter_config not pinned");

    {
        /* direct cast (size_t *)variable fails on 32-bit environment */
        long long cd_nelmts_temp = *cd_nelmtsArray;
        size_t    cd_nelmts_t = (size_t) cd_nelmts_temp;

        if ((status = H5Pget_filter_by_id2((hid_t)plist, (H5Z_filter_t)filter,
                (unsigned int *)flagsArray, &cd_nelmts_t, (unsigned int *)cd_valuesArray,
                (size_t)namelen, (char *)aName, (unsigned int *)filter_configArray)) < 0)
            H5_LIBRARY_ERROR(ENVONLY);

        aName[bs - 1] = '\0';

        *cd_nelmtsArray = (jlong)cd_nelmts_t;
    } /* end direct cast special handling */

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, aName))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_filter_by_id2: out of memory - unable to construct string from UTF characters");
    }

    ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
    CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);

done:
    if (filter_configArray)
        UNPIN_INT_ARRAY(ENVONLY, filter_config, filter_configArray, (status < 0) ? JNI_ABORT : 0);
    if (cd_valuesArray)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesArray, (status < 0) ? JNI_ABORT : 0);
    if (cd_nelmtsArray)
        UNPIN_LONG_ARRAY(ENVONLY, cd_nelmts, cd_nelmtsArray, (status < 0) ? JNI_ABORT : 0);
    if (flagsArray)
        UNPIN_INT_ARRAY(ENVONLY, flags, flagsArray, (status < 0) ? JNI_ABORT : 0);
    if (aName)
        HDfree(aName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1filter_1by_1id2 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pmodify_filter
 * Signature: (JIIJ[I)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pmodify_1filter
    (JNIEnv *env, jclass clss, jlong plist, jint filter,
        jint flags, jlong cd_nelmts, jintArray cd_values)
{
    jboolean  isCopy;
    jint     *cd_valuesP = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (NULL == cd_values)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pmodify_filter: cd_values is NULL");

    PIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesP, &isCopy, "H5Pmodify_filter: cd_values not pinned");

    if ((status = H5Pmodify_filter((hid_t)plist, (H5Z_filter_t)filter, (const unsigned int)flags,
            (size_t)cd_nelmts, (unsigned int *)cd_valuesP)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (cd_valuesP)
        UNPIN_INT_ARRAY(ENVONLY, cd_values, cd_valuesP, JNI_ABORT);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pmodify_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Premove_filter
 * Signature: (JI)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5P1remove_1filter
    (JNIEnv *env, jclass clss, jlong obj_id, jint filter)
{
    herr_t status = FAIL;

    UNUSED(clss);

    if ((status = H5Premove_filter((hid_t)obj_id, (H5Z_filter_t)filter)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5P1remove_1filter */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_fletcher32
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1fletcher32
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_fletcher32((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1fletcher32 */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_nbit
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1nbit
    (JNIEnv *env, jclass clss, jlong plist_id)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_nbit((hid_t)plist_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1nbit */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_scaleoffset
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1scaleoffset
    (JNIEnv *env, jclass clss, jlong plist_id, jint scale_type, jint scale_factor)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if (scale_factor < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_scaleoffset: scale factor must be >= 0");
    if (scale_type != H5Z_SO_FLOAT_DSCALE && scale_type != H5Z_SO_FLOAT_ESCALE && scale_type != H5Z_SO_INT)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pset_scaleoffset: invalid scale type");

    if ((retVal = H5Pset_scaleoffset((hid_t)plist_id, (H5Z_SO_scale_type_t)scale_type, scale_factor)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1scaleoffset */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_shuffle
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1shuffle
    (JNIEnv *env, jclass clss, jlong plist)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_shuffle((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1shuffle */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_szip
 * Signature: (JII)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1szip
    (JNIEnv *env, jclass clss, jlong plist, jint options_mask, jint pixels_per_block)
{
    herr_t retVal = FAIL;

    UNUSED(clss);

    if ((retVal = H5Pset_szip((hid_t)plist, (unsigned int)options_mask, (unsigned int)pixels_per_block)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1szip */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_external
 * Signature: (JLjava/lang/String;JJ)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1external
    (JNIEnv *env, jclass clss, jlong plist, jstring name, jlong offset, jlong size)
{
    const char *fileName = NULL;
    herr_t      status = FAIL;

    UNUSED(clss);

    if (NULL == name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_external: file name is NULL");

    PIN_JAVA_STRING(ENVONLY, name, fileName, NULL, "H5Pset_external: file name not pinned");

    if ((status = H5Pset_external((hid_t)plist, fileName, (off_t)offset, (hsize_t)size)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (fileName)
        UNPIN_JAVA_STRING(ENVONLY, name, fileName);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pset_1external */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external
 * Signature: (JIJ[Ljava/lang/String;[J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external
    (JNIEnv *env, jclass clss, jlong plist, jint idx, jlong name_size,
        jobjectArray name, jlongArray size)
{
    jboolean  isCopy;
    jstring   str;
    hsize_t   s;
    off_t     o;
    jsize     arrLen;
    jlong    *theArray = NULL;
    char     *file = NULL;
    herr_t    status = FAIL;

    UNUSED(clss);

    if (name_size < 0)
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_external: name_size < 0");

    if ((arrLen = ENVPTR->GetArrayLength(ENVONLY, size)) < 0) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_external: size array length < 0");
    }
    if ((size != NULL) && (arrLen < 2))
        H5_BAD_ARGUMENT_ERROR(ENVONLY, "H5Pget_external: size input array < 2");

    if (name_size > 0)
        if (NULL == (file = (char *) HDmalloc(sizeof(char) * (size_t)name_size)))
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_external: memory allocation failed");

    if ((status = H5Pget_external((hid_t) plist, (unsigned)idx, (size_t)name_size,
            file, (off_t *)&o, (hsize_t *)&s)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL != size) {
        PIN_LONG_ARRAY(ENVONLY, size, theArray, &isCopy, "H5Pget_external: size array not pinned");

        theArray[0] = o;
        theArray[1] = (jlong)s;
    }

    if (NULL != file) {
        file[name_size - 1] = '\0';

        if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, file))) {
            CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
            H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_external: out of memory - unable to construct string from UTF characters");
        }

        ENVPTR->SetObjectArrayElement(ENVONLY, name, 0, (jobject)str);
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_FALSE);
    }

done:
    if (theArray)
        UNPIN_LONG_ARRAY(ENVONLY, size, theArray, (status < 0) ? JNI_ABORT : 0);
    if (file)
        HDfree(file);

    return (jint)status;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1external */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_external_count
 * Signature: (J)I
 */
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1external_1count
    (JNIEnv *env, jclass clss, jlong plist)
{
    int retVal = -1;

    UNUSED(clss);

    if ((retVal = H5Pget_external_count((hid_t)plist)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jint)retVal;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1external_1count */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual
 * Signature: (JJLjava/lang/String;Ljava/lang/String;J)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong vspace_id,
        jstring src_file_name, jstring src_dset_name, jlong src_space_id)
{
    const char *fstr = NULL;
    const char *dstr = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == src_file_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_virtual: src_file_name is NULL");
    if (NULL == src_dset_name)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_virtual: src_dset_name is NULL");

    PIN_JAVA_STRING(ENVONLY, src_file_name, fstr, NULL, "H5Pset_virtual: src_file_name not pinned");
    PIN_JAVA_STRING(ENVONLY, src_dset_name, dstr, NULL, "H5Pset_virtual: src_dset_name not pinned");

    if ((retVal = H5Pset_virtual((hid_t)dcpl_id, (hid_t)vspace_id, fstr, dstr, (hid_t)src_space_id)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (dstr)
        UNPIN_JAVA_STRING(ENVONLY, src_dset_name, dstr);
    if (fstr)
        UNPIN_JAVA_STRING(ENVONLY, src_file_name, fstr);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_count
 * Signature: (J)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1count
    (JNIEnv *env, jclass clss, jlong dcpl_id)
{
    size_t s = 0;

    UNUSED(clss);

    if (H5Pget_virtual_count((hid_t)dcpl_id, &s) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)s;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1count */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_dsetname
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1dsetname
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    ssize_t  buf_size;
    char    *dname = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    /* get the length of the filename */
    if ((buf_size = H5Pget_virtual_dsetname((hid_t)dcpl_id, (size_t)index, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (dname = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_dsetname: memory allocation failed");

    if (H5Pget_virtual_dsetname((hid_t)dcpl_id, (size_t)index, dname, (size_t)buf_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    dname[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, dname))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_dsetname: out of memory - unable to construct string from UTF characters");
    }

done:
    if (dname)
        HDfree(dname);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1dsetname */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_filename
 * Signature: (JJ)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    ssize_t  buf_size;
    char    *fname = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    /* get the length of the filename */
    if ((buf_size = H5Pget_virtual_filename((hid_t)dcpl_id, (size_t)index, NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (fname = (char *) HDmalloc(sizeof(char) * (size_t)buf_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_filename: memory allocation failed");

    if (H5Pget_virtual_filename((hid_t)dcpl_id, (size_t)index, fname, (size_t)buf_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    fname[buf_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, fname))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_filename: out of memory - unable to construct string from UTF characters");
    }

done:
    if (fname)
        HDfree(fname);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1filename */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_srcspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1srcspace
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    hid_t space_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((space_id = H5Pget_virtual_srcspace((hid_t)dcpl_id, (size_t)index)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)space_id;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1srcspace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_vspace
 * Signature: (JJ)J
 */
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace
    (JNIEnv *env, jclass clss, jlong dcpl_id, jlong index)
{
    hid_t space_id = H5I_INVALID_HID;

    UNUSED(clss);

    if ((space_id = H5Pget_virtual_vspace((hid_t)dcpl_id, (size_t)index)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    return (jlong)space_id;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1vspace */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pset_virtual_prefix
 * Signature: (JLjava/lang/String;)V
 */
JNIEXPORT void JNICALL
Java_hdf_hdf5lib_H5_H5Pset_1virtual_1prefix
    (JNIEnv *env, jclass clss, jlong dapl_id, jstring prefix)
{
    const char *virtPrefix = NULL;
    herr_t      retVal = FAIL;

    UNUSED(clss);

    if (NULL == prefix)
        H5_NULL_ARGUMENT_ERROR(ENVONLY, "H5Pset_virtual_prefix: virtual prefix is NULL");

    PIN_JAVA_STRING(ENVONLY, prefix, virtPrefix, NULL, "H5Pset_virtual_prefix: virtual prefix not pinned");

    if ((retVal = H5Pset_virtual_prefix((hid_t)dapl_id, virtPrefix)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

done:
    if (virtPrefix)
        UNPIN_JAVA_STRING(ENVONLY, prefix, virtPrefix);
} /* end Java_hdf_hdf5lib_H5_H5Pset_1virtual_1prefix */

/*
 * Class:     hdf_hdf5lib_H5
 * Method:    H5Pget_virtual_prefix
 * Signature: (J)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL
Java_hdf_hdf5lib_H5_H5Pget_1virtual_1prefix
    (JNIEnv *env, jclass clss, jlong dapl_id)
{
    ssize_t  prefix_size = -1;
    char    *pre = NULL;
    jstring  str = NULL;

    UNUSED(clss);

    if ((prefix_size = H5Pget_virtual_prefix((hid_t)dapl_id, (char *)NULL, 0)) < 0)
        H5_LIBRARY_ERROR(ENVONLY);

    if (NULL == (pre = (char *) HDmalloc(sizeof(char) * (size_t) prefix_size + 1)))
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_prefix: memory allocation failed");

    if (H5Pget_virtual_prefix((hid_t)dapl_id, (char *)pre, (size_t) prefix_size + 1) < 0)
        H5_LIBRARY_ERROR(ENVONLY);
    pre[prefix_size] = '\0';

    if (NULL == (str = ENVPTR->NewStringUTF(ENVONLY, pre))) {
        CHECK_JNI_EXCEPTION(ENVONLY, JNI_TRUE);
        H5_JNI_FATAL_ERROR(ENVONLY, "H5Pget_virtual_prefix: out of memory - unable to construct string from UTF characters");
    }

done:
    if (pre)
        HDfree(pre);

    return (jstring)str;
} /* end Java_hdf_hdf5lib_H5_H5Pget_1virtual_1prefix */

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
