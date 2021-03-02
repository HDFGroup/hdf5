/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
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

#include "hdf5.h"
#include <jni.h>
#include <stdlib.h>
#include "h5jni.h"

H5_GCC_DIAG_OFF("missing-prototypes")
H5_GCC_DIAG_OFF("unused-parameter")

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1QUARTER_1HADDR_1MAX(JNIEnv *env, jclass cls)
{
    return (hsize_t)HADDR_MAX / 4;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1SZIP_1ALLOW_1K13_1OPTION_1MASK(JNIEnv *env, jclass cls)
{
    return H5_SZIP_ALLOW_K13_OPTION_MASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1SZIP_1CHIP_1OPTION_1MASK(JNIEnv *env, jclass cls)
{
    return H5_SZIP_CHIP_OPTION_MASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1SZIP_1EC_1OPTION_1MASK(JNIEnv *env, jclass cls)
{
    return H5_SZIP_EC_OPTION_MASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1SZIP_1MAX_1PIXELS_1PER_1BLOCK(JNIEnv *env, jclass cls)
{
    return H5_SZIP_MAX_PIXELS_PER_BLOCK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1SZIP_1NN_1OPTION_1MASK(JNIEnv *env, jclass cls)
{
    return H5_SZIP_NN_OPTION_MASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1INDEX_1UNKNOWN(JNIEnv *env, jclass cls)
{
    return H5_INDEX_UNKNOWN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1INDEX_1NAME(JNIEnv *env, jclass cls)
{
    return H5_INDEX_NAME;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1INDEX_1CRT_1ORDER(JNIEnv *env, jclass cls)
{
    return H5_INDEX_CRT_ORDER;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1INDEX_1N(JNIEnv *env, jclass cls)
{
    return H5_INDEX_N;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1ITER_1UNKNOWN(JNIEnv *env, jclass cls)
{
    return H5_ITER_UNKNOWN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1ITER_1INC(JNIEnv *env, jclass cls)
{
    return H5_ITER_INC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1ITER_1DEC(JNIEnv *env, jclass cls)
{
    return H5_ITER_DEC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1ITER_1NATIVE(JNIEnv *env, jclass cls)
{
    return H5_ITER_NATIVE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1ITER_1N(JNIEnv *env, jclass cls)
{
    return H5_ITER_N;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5AC_1CURR_1CACHE_1CONFIG_1VERSION(JNIEnv *env, jclass cls)
{
    return H5AC__CURR_CACHE_CONFIG_VERSION;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5AC_1MAX_1TRACE_1FILE_1NAME_1LEN(JNIEnv *env, jclass cls)
{
    return H5AC__MAX_TRACE_FILE_NAME_LEN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5AC_1METADATA_1WRITE_1STRATEGY_1PROCESS_1ZERO_1ONLY(JNIEnv *env, jclass cls)
{
    return H5AC_METADATA_WRITE_STRATEGY__PROCESS_0_ONLY;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5AC_1METADATA_1WRITE_1STRATEGY_1DISTRIBUTED(JNIEnv *env, jclass cls)
{
    return H5AC_METADATA_WRITE_STRATEGY__DISTRIBUTED;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1incr_1off(JNIEnv *env, jclass cls)
{
    return H5C_incr__off;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1incr_1threshold(JNIEnv *env, jclass cls)
{
    return H5C_incr__threshold;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1flash_1incr_1off(JNIEnv *env, jclass cls)
{
    return H5C_flash_incr__off;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1flash_1incr_1add_1space(JNIEnv *env, jclass cls)
{
    return H5C_flash_incr__add_space;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1decr_1off(JNIEnv *env, jclass cls)
{
    return H5C_decr__off;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1decr_1threshold(JNIEnv *env, jclass cls)
{
    return H5C_decr__threshold;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1decr_1age_1out(JNIEnv *env, jclass cls)
{
    return H5C_decr__age_out;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5C_1decr_1age_1out_1with_1threshold(JNIEnv *env, jclass cls)
{
    return H5C_decr__age_out_with_threshold;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1CHUNK_1IDX_1BTREE(JNIEnv *env, jclass cls)
{
    return H5D_CHUNK_IDX_BTREE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1ALLOC_1TIME_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5D_ALLOC_TIME_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1ALLOC_1TIME_1EARLY(JNIEnv *env, jclass cls)
{
    return H5D_ALLOC_TIME_EARLY;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1ALLOC_1TIME_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_ALLOC_TIME_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1ALLOC_1TIME_1INCR(JNIEnv *env, jclass cls)
{
    return H5D_ALLOC_TIME_INCR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1ALLOC_1TIME_1LATE(JNIEnv *env, jclass cls)
{
    return H5D_ALLOC_TIME_LATE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1TIME_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_FILL_TIME_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1TIME_1ALLOC(JNIEnv *env, jclass cls)
{
    return H5D_FILL_TIME_ALLOC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1TIME_1NEVER(JNIEnv *env, jclass cls)
{
    return H5D_FILL_TIME_NEVER;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1TIME_1IFSET(JNIEnv *env, jclass cls)
{
    return H5D_FILL_TIME_IFSET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1VALUE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5D_FILL_VALUE_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1VALUE_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_FILL_VALUE_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1VALUE_1UNDEFINED(JNIEnv *env, jclass cls)
{
    return H5D_FILL_VALUE_UNDEFINED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1FILL_1VALUE_1USER_1DEFINED(JNIEnv *env, jclass cls)
{
    return H5D_FILL_VALUE_USER_DEFINED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1LAYOUT_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_LAYOUT_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1CHUNKED(JNIEnv *env, jclass cls)
{
    return H5D_CHUNKED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1COMPACT(JNIEnv *env, jclass cls)
{
    return H5D_COMPACT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1CONTIGUOUS(JNIEnv *env, jclass cls)
{
    return H5D_CONTIGUOUS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1VIRTUAL(JNIEnv *env, jclass cls)
{
    return H5D_VIRTUAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1NLAYOUTS(JNIEnv *env, jclass cls)
{
    return H5D_NLAYOUTS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1SPACE_1STATUS_1ALLOCATED(JNIEnv *env, jclass cls)
{
    return H5D_SPACE_STATUS_ALLOCATED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1SPACE_1STATUS_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_SPACE_STATUS_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1SPACE_1STATUS_1NOT_1ALLOCATED(JNIEnv *env, jclass cls)
{
    return H5D_SPACE_STATUS_NOT_ALLOCATED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1SPACE_1STATUS_1PART_1ALLOCATED(JNIEnv *env, jclass cls)
{
    return H5D_SPACE_STATUS_PART_ALLOCATED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1VDS_1ERROR(JNIEnv *env, jclass cls)
{
    return H5D_VDS_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1VDS_1FIRST_1MISSING(JNIEnv *env, jclass cls)
{
    return H5D_VDS_FIRST_MISSING;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1VDS_1LAST_1AVAILABLE(JNIEnv *env, jclass cls)
{
    return H5D_VDS_LAST_AVAILABLE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5D_1CHUNK_1DONT_1FILTER_1PARTIAL_1CHUNKS(JNIEnv *env, jclass cls)
{
    return H5D_CHUNK_DONT_FILTER_PARTIAL_CHUNKS;
}

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ALIGNMENT(JNIEnv *env, jclass cls)
{
    return H5E_ALIGNMENT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ALREADYEXISTS(JNIEnv *env, jclass cls)
{
    return H5E_ALREADYEXISTS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ALREADYINIT(JNIEnv *env, jclass cls)
{
    return H5E_ALREADYINIT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ARGS(JNIEnv *env, jclass cls)
{
    return H5E_ARGS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ID(JNIEnv *env, jclass cls)
{
    return H5E_ID;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ATTR(JNIEnv *env, jclass cls)
{
    return H5E_ATTR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADID(JNIEnv *env, jclass cls)
{
    return H5E_BADID;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADFILE(JNIEnv *env, jclass cls)
{
    return H5E_BADFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADGROUP(JNIEnv *env, jclass cls)
{
    return H5E_BADGROUP;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADITER(JNIEnv *env, jclass cls)
{
    return H5E_BADITER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADMESG(JNIEnv *env, jclass cls)
{
    return H5E_BADMESG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADRANGE(JNIEnv *env, jclass cls)
{
    return H5E_BADRANGE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADSELECT(JNIEnv *env, jclass cls)
{
    return H5E_BADSELECT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADSIZE(JNIEnv *env, jclass cls)
{
    return H5E_BADSIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADTYPE(JNIEnv *env, jclass cls)
{
    return H5E_BADTYPE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BADVALUE(JNIEnv *env, jclass cls)
{
    return H5E_BADVALUE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1BTREE(JNIEnv *env, jclass cls)
{
    return H5E_BTREE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CACHE(JNIEnv *env, jclass cls)
{
    return H5E_CACHE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CALLBACK(JNIEnv *env, jclass cls)
{
    return H5E_CALLBACK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANAPPLY(JNIEnv *env, jclass cls)
{
    return H5E_CANAPPLY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTALLOC(JNIEnv *env, jclass cls)
{
    return H5E_CANTALLOC;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTAPPEND(JNIEnv *env, jclass cls)
{
    return H5E_CANTAPPEND;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTATTACH(JNIEnv *env, jclass cls)
{
    return H5E_CANTATTACH;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCLEAN(JNIEnv *env, jclass cls)
{
    return H5E_CANTCLEAN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCLIP(JNIEnv *env, jclass cls)
{
    return H5E_CANTCLIP;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCLOSEFILE(JNIEnv *env, jclass cls)
{
    return H5E_CANTCLOSEFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCLOSEOBJ(JNIEnv *env, jclass cls)
{
    return H5E_CANTCLOSEOBJ;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCOMPARE(JNIEnv *env, jclass cls)
{
    return H5E_CANTCOMPARE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCOMPUTE(JNIEnv *env, jclass cls)
{
    return H5E_CANTCOMPUTE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCONVERT(JNIEnv *env, jclass cls)
{
    return H5E_CANTCONVERT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCOPY(JNIEnv *env, jclass cls)
{
    return H5E_CANTCOPY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCORK(JNIEnv *env, jclass cls)
{
    return H5E_CANTCORK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCOUNT(JNIEnv *env, jclass cls)
{
    return H5E_CANTCOUNT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTCREATE(JNIEnv *env, jclass cls)
{
    return H5E_CANTCREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDEC(JNIEnv *env, jclass cls)
{
    return H5E_CANTDEC;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDECODE(JNIEnv *env, jclass cls)
{
    return H5E_CANTDECODE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDELETE(JNIEnv *env, jclass cls)
{
    return H5E_CANTDELETE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDELETEFILE(JNIEnv *env, jclass cls)
{
    return H5E_CANTDELETEFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDEPEND(JNIEnv *env, jclass cls)
{
    return H5E_CANTDEPEND;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTDIRTY(JNIEnv *env, jclass cls)
{
    return H5E_CANTDIRTY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTFILTER(JNIEnv *env, jclass cls)
{
    return H5E_CANTFILTER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTENCODE(JNIEnv *env, jclass cls)
{
    return H5E_CANTENCODE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTEXPUNGE(JNIEnv *env, jclass cls)
{
    return H5E_CANTEXPUNGE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTEXTEND(JNIEnv *env, jclass cls)
{
    return H5E_CANTEXTEND;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTFLUSH(JNIEnv *env, jclass cls)
{
    return H5E_CANTFLUSH;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTFREE(JNIEnv *env, jclass cls)
{
    return H5E_CANTFREE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTGATHER(JNIEnv *env, jclass cls)
{
    return H5E_CANTGATHER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTGC(JNIEnv *env, jclass cls)
{
    return H5E_CANTGC;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTGET(JNIEnv *env, jclass cls)
{
    return H5E_CANTGET;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTGETSIZE(JNIEnv *env, jclass cls)
{
    return H5E_CANTGETSIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTINC(JNIEnv *env, jclass cls)
{
    return H5E_CANTINC;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTINIT(JNIEnv *env, jclass cls)
{
    return H5E_CANTINIT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTINS(JNIEnv *env, jclass cls)
{
    return H5E_CANTINS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTINSERT(JNIEnv *env, jclass cls)
{
    return H5E_CANTINSERT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTLIST(JNIEnv *env, jclass cls)
{
    return H5E_CANTLIST;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTLOAD(JNIEnv *env, jclass cls)
{
    return H5E_CANTLOAD;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTLOCK(JNIEnv *env, jclass cls)
{
    return H5E_CANTLOCK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTLOCKFILE(JNIEnv *env, jclass cls)
{
    return H5E_CANTLOCKFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMARKCLEAN(JNIEnv *env, jclass cls)
{
    return H5E_CANTMARKCLEAN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMARKDIRTY(JNIEnv *env, jclass cls)
{
    return H5E_CANTMARKDIRTY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMARKSERIALIZED(JNIEnv *env, jclass cls)
{
    return H5E_CANTMARKSERIALIZED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMARKUNSERIALIZED(JNIEnv *env, jclass cls)
{
    return H5E_CANTMARKUNSERIALIZED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMERGE(JNIEnv *env, jclass cls)
{
    return H5E_CANTMERGE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMODIFY(JNIEnv *env, jclass cls)
{
    return H5E_CANTMODIFY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTMOVE(JNIEnv *env, jclass cls)
{
    return H5E_CANTMOVE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTNEXT(JNIEnv *env, jclass cls)
{
    return H5E_CANTNEXT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTNOTIFY(JNIEnv *env, jclass cls)
{
    return H5E_CANTNOTIFY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTOPENFILE(JNIEnv *env, jclass cls)
{
    return H5E_CANTOPENFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTOPENOBJ(JNIEnv *env, jclass cls)
{
    return H5E_CANTOPENOBJ;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTOPERATE(JNIEnv *env, jclass cls)
{
    return H5E_CANTOPERATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTPACK(JNIEnv *env, jclass cls)
{
    return H5E_CANTPACK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTPIN(JNIEnv *env, jclass cls)
{
    return H5E_CANTPIN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTPROTECT(JNIEnv *env, jclass cls)
{
    return H5E_CANTPROTECT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRECV(JNIEnv *env, jclass cls)
{
    return H5E_CANTRECV;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTREDISTRIBUTE(JNIEnv *env, jclass cls)
{
    return H5E_CANTREDISTRIBUTE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTREGISTER(JNIEnv *env, jclass cls)
{
    return H5E_CANTREGISTER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRELEASE(JNIEnv *env, jclass cls)
{
    return H5E_CANTRELEASE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTREMOVE(JNIEnv *env, jclass cls)
{
    return H5E_CANTREMOVE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRENAME(JNIEnv *env, jclass cls)
{
    return H5E_CANTRENAME;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRESET(JNIEnv *env, jclass cls)
{
    return H5E_CANTRESET;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRESIZE(JNIEnv *env, jclass cls)
{
    return H5E_CANTRESIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTRESTORE(JNIEnv *env, jclass cls)
{
    return H5E_CANTRESTORE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTREVIVE(JNIEnv *env, jclass cls)
{
    return H5E_CANTREVIVE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSHRINK(JNIEnv *env, jclass cls)
{
    return H5E_CANTSHRINK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSELECT(JNIEnv *env, jclass cls)
{
    return H5E_CANTSELECT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSET(JNIEnv *env, jclass cls)
{
    return H5E_CANTSET;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSERIALIZE(JNIEnv *env, jclass cls)
{
    return H5E_CANTSERIALIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSORT(JNIEnv *env, jclass cls)
{
    return H5E_CANTSORT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSPLIT(JNIEnv *env, jclass cls)
{
    return H5E_CANTSPLIT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTSWAP(JNIEnv *env, jclass cls)
{
    return H5E_CANTSWAP;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTTAG(JNIEnv *env, jclass cls)
{
    return H5E_CANTTAG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNCORK(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNCORK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNDEPEND(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNDEPEND;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNLOCK(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNLOCK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNLOCKFILE(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNLOCKFILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNPIN(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNPIN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNPROTECT(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNPROTECT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUNSERIALIZE(JNIEnv *env, jclass cls)
{
    return H5E_CANTUNSERIALIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CANTUPDATE(JNIEnv *env, jclass cls)
{
    return H5E_CANTUPDATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CLOSEERROR(JNIEnv *env, jclass cls)
{
    return H5E_CLOSEERROR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1COMPLEN(JNIEnv *env, jclass cls)
{
    return H5E_COMPLEN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1CONTEXT(JNIEnv *env, jclass cls)
{
    return H5E_CONTEXT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1DATASET(JNIEnv *env, jclass cls)
{
    return H5E_DATASET;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1DATASPACE(JNIEnv *env, jclass cls)
{
    return H5E_DATASPACE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1DATATYPE(JNIEnv *env, jclass cls)
{
    return H5E_DATATYPE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5E_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1DUPCLASS(JNIEnv *env, jclass cls)
{
    return H5E_DUPCLASS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1EARRAY(JNIEnv *env, jclass cls)
{
    return H5E_EARRAY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1EFL(JNIEnv *env, jclass cls)
{
    return H5E_EFL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1ERROR(JNIEnv *env, jclass cls)
{
    return H5E_ERROR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1EXISTS(JNIEnv *env, jclass cls)
{
    return H5E_EXISTS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FARRAY(JNIEnv *env, jclass cls)
{
    return H5E_FARRAY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FCNTL(JNIEnv *env, jclass cls)
{
    return H5E_FCNTL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FILE(JNIEnv *env, jclass cls)
{
    return H5E_FILE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FILEEXISTS(JNIEnv *env, jclass cls)
{
    return H5E_FILEEXISTS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FILEOPEN(JNIEnv *env, jclass cls)
{
    return H5E_FILEOPEN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FSPACE(JNIEnv *env, jclass cls)
{
    return H5E_FSPACE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1FUNC(JNIEnv *env, jclass cls)
{
    return H5E_FUNC;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1HEAP(JNIEnv *env, jclass cls)
{
    return H5E_HEAP;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1INCONSISTENTSTATE(JNIEnv *env, jclass cls)
{
    return H5E_INCONSISTENTSTATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1INTERNAL(JNIEnv *env, jclass cls)
{
    return H5E_INTERNAL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1IO(JNIEnv *env, jclass cls)
{
    return H5E_IO;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1LINK(JNIEnv *env, jclass cls)
{
    return H5E_LINK;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1LINKCOUNT(JNIEnv *env, jclass cls)
{
    return H5E_LINKCOUNT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1LOGGING(JNIEnv *env, jclass cls)
{
    return H5E_LOGGING;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MAJOR(JNIEnv *env, jclass cls)
{
    return H5E_MAJOR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MAP(JNIEnv *env, jclass cls)
{
    return H5E_MAP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MINOR(JNIEnv *env, jclass cls)
{
    return H5E_MINOR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MOUNT(JNIEnv *env, jclass cls)
{
    return H5E_MOUNT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MPI(JNIEnv *env, jclass cls)
{
    return H5E_MPI;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1MPIERRSTR(JNIEnv *env, jclass cls)
{
    return H5E_MPIERRSTR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NLINKS(JNIEnv *env, jclass cls)
{
    return H5E_NLINKS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NO_1INDEPENDENT(JNIEnv *env, jclass cls)
{
    return H5E_NO_INDEPENDENT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOENCODER(JNIEnv *env, jclass cls)
{
    return H5E_NOENCODER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOFILTER(JNIEnv *env, jclass cls)
{
    return H5E_NOFILTER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOIDS(JNIEnv *env, jclass cls)
{
    return H5E_NOIDS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NONE_1MAJOR(JNIEnv *env, jclass cls)
{
    return H5E_NONE_MAJOR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NONE_1MINOR(JNIEnv *env, jclass cls)
{
    return H5E_NONE_MINOR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOSPACE(JNIEnv *env, jclass cls)
{
    return H5E_NOSPACE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOTCACHED(JNIEnv *env, jclass cls)
{
    return H5E_NOTCACHED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOTFOUND(JNIEnv *env, jclass cls)
{
    return H5E_NOTFOUND;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOTHDF5(JNIEnv *env, jclass cls)
{
    return H5E_NOTHDF5;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1NOTREGISTERED(JNIEnv *env, jclass cls)
{
    return H5E_NOTREGISTERED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1OBJOPEN(JNIEnv *env, jclass cls)
{
    return H5E_OBJOPEN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1OHDR(JNIEnv *env, jclass cls)
{
    return H5E_OHDR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1OPENERROR(JNIEnv *env, jclass cls)
{
    return H5E_OPENERROR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1OVERFLOW(JNIEnv *env, jclass cls)
{
    return H5E_OVERFLOW;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PAGEBUF(JNIEnv *env, jclass cls)
{
    return H5E_PAGEBUF;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PATH(JNIEnv *env, jclass cls)
{
    return H5E_PATH;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PLINE(JNIEnv *env, jclass cls)
{
    return H5E_PLINE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PLIST(JNIEnv *env, jclass cls)
{
    return H5E_PLIST;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PLUGIN(JNIEnv *env, jclass cls)
{
    return H5E_PLUGIN;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1PROTECT(JNIEnv *env, jclass cls)
{
    return H5E_PROTECT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1READERROR(JNIEnv *env, jclass cls)
{
    return H5E_READERROR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1REFERENCE(JNIEnv *env, jclass cls)
{
    return H5E_REFERENCE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1RESOURCE(JNIEnv *env, jclass cls)
{
    return H5E_RESOURCE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1RS(JNIEnv *env, jclass cls)
{
    return H5E_RS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SEEKERROR(JNIEnv *env, jclass cls)
{
    return H5E_SEEKERROR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SETDISALLOWED(JNIEnv *env, jclass cls)
{
    return H5E_SETDISALLOWED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SETLOCAL(JNIEnv *env, jclass cls)
{
    return H5E_SETLOCAL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SLIST(JNIEnv *env, jclass cls)
{
    return H5E_SLIST;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SOHM(JNIEnv *env, jclass cls)
{
    return H5E_SOHM;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1STORAGE(JNIEnv *env, jclass cls)
{
    return H5E_STORAGE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SYM(JNIEnv *env, jclass cls)
{
    return H5E_SYM;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SYSERRSTR(JNIEnv *env, jclass cls)
{
    return H5E_SYSERRSTR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1SYSTEM(JNIEnv *env, jclass cls)
{
    return H5E_SYSTEM;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1TRAVERSE(JNIEnv *env, jclass cls)
{
    return H5E_TRAVERSE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1TRUNCATED(JNIEnv *env, jclass cls)
{
    return H5E_TRUNCATED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1TST(JNIEnv *env, jclass cls)
{
    return H5E_TST;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1UNINITIALIZED(JNIEnv *env, jclass cls)
{
    return H5E_UNINITIALIZED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1UNSUPPORTED(JNIEnv *env, jclass cls)
{
    return H5E_UNSUPPORTED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1VERSION(JNIEnv *env, jclass cls)
{
    return H5E_VERSION;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1VFL(JNIEnv *env, jclass cls)
{
    return H5E_VFL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1VOL(JNIEnv *env, jclass cls)
{
    return H5E_VOL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1WALK_1DOWNWARD(JNIEnv *env, jclass cls)
{
    return H5E_WALK_DOWNWARD;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1WALK_1UPWARD(JNIEnv *env, jclass cls)
{
    return H5E_WALK_UPWARD;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5E_1WRITEERROR(JNIEnv *env, jclass cls)
{
    return H5E_WRITEERROR;
}

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5ES_1STATUS_1IN_1PROGRESS(JNIEnv *env, jclass cls)
{
    return H5ES_STATUS_IN_PROGRESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5ES_1STATUS_1SUCCEED(JNIEnv *env, jclass cls)
{
    return H5ES_STATUS_SUCCEED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5ES_1STATUS_1FAIL(JNIEnv *env, jclass cls)
{
    return H5ES_STATUS_FAIL;
}

/* Java does not have unsigned native types */
H5_GCC_DIAG_OFF("sign-conversion")
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1CREAT(JNIEnv *env, jclass cls)
{
    return H5F_ACC_CREAT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1EXCL(JNIEnv *env, jclass cls)
{
    return H5F_ACC_EXCL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1RDONLY(JNIEnv *env, jclass cls)
{
    return H5F_ACC_RDONLY;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1RDWR(JNIEnv *env, jclass cls)
{
    return H5F_ACC_RDWR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1TRUNC(JNIEnv *env, jclass cls)
{
    return H5F_ACC_TRUNC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5F_ACC_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1SWMR_1READ(JNIEnv *env, jclass cls)
{
    return H5F_ACC_SWMR_READ;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1ACC_1SWMR_1WRITE(JNIEnv *env, jclass cls)
{
    return H5F_ACC_SWMR_WRITE;
}
H5_GCC_DIAG_ON("sign-conversion")

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1CLOSE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5F_CLOSE_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1CLOSE_1SEMI(JNIEnv *env, jclass cls)
{
    return H5F_CLOSE_SEMI;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1CLOSE_1STRONG(JNIEnv *env, jclass cls)
{
    return H5F_CLOSE_STRONG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1CLOSE_1WEAK(JNIEnv *env, jclass cls)
{
    return H5F_CLOSE_WEAK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1ERROR(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1EARLIEST(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_EARLIEST;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1V18(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_V18;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1V110(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_V110;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1V112(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_V112;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1V114(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_V114;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1NBOUNDS(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_NBOUNDS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1LIBVER_1LATEST(JNIEnv *env, jclass cls)
{
    return H5F_LIBVER_LATEST;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1ALL(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_ALL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1ATTR(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_ATTR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1DATASET(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_DATASET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1DATATYPE(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_DATATYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1FILE(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_FILE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1GROUP(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_GROUP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1OBJ_1LOCAL(JNIEnv *env, jclass cls)
{
    return H5F_OBJ_LOCAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1SCOPE_1GLOBAL(JNIEnv *env, jclass cls)
{
    return H5F_SCOPE_GLOBAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1SCOPE_1LOCAL(JNIEnv *env, jclass cls)
{
    return H5F_SCOPE_LOCAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1UNLIMITED(JNIEnv *env, jclass cls)
{
    return (jint)H5F_UNLIMITED;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1FSPACE_1STRATEGY_1FSM_1AGGR(JNIEnv *env, jclass cls)
{
    return H5F_FSPACE_STRATEGY_FSM_AGGR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1FSPACE_1STRATEGY_1AGGR(JNIEnv *env, jclass cls)
{
    return H5F_FSPACE_STRATEGY_AGGR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1FSPACE_1STRATEGY_1PAGE(JNIEnv *env, jclass cls)
{
    return H5F_FSPACE_STRATEGY_PAGE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1FSPACE_1STRATEGY_1NONE(JNIEnv *env, jclass cls)
{
    return H5F_FSPACE_STRATEGY_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5F_1FSPACE_1STRATEGY_1NTYPES(JNIEnv *env, jclass cls)
{
    return H5F_FSPACE_STRATEGY_NTYPES;
}

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1CORE(JNIEnv *env, jclass cls)
{
    return H5FD_CORE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1DIRECT(JNIEnv *env, jclass cls)
{
#ifdef H5_HAVE_DIRECT
    return H5FD_DIRECT;
#else
    return H5I_INVALID_HID;
#endif
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1FAMILY(JNIEnv *env, jclass cls)
{
    return H5FD_FAMILY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1HDFS(JNIEnv *env, jclass cls)
{
#ifdef H5_HAVE_LIBHDFS
    return H5FD_HDFS;
#else
    return H5I_INVALID_HID;
#endif
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG(JNIEnv *env, jclass cls)
{
    return H5FD_LOG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MPIO(JNIEnv *env, jclass cls)
{
    return H5FD_MPIO;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MULTI(JNIEnv *env, jclass cls)
{
    return H5FD_MULTI;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1SEC2(JNIEnv *env, jclass cls)
{
    return H5FD_SEC2;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1ROS3(JNIEnv *env, jclass cls)
{
#ifdef H5_HAVE_ROS3_VFD
    return H5FD_ROS3;
#else
    return H5I_INVALID_HID;
#endif
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1STDIO(JNIEnv *env, jclass cls)
{
    return H5FD_STDIO;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1WINDOWS(JNIEnv *env, jclass cls)
{
#ifdef H5_HAVE_WINDOWS
    return H5FD_DIRECT;
#else
    return H5I_INVALID_HID;
#endif
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1LOC_1READ(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_LOC_READ;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1LOC_1WRITE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_LOC_WRITE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1LOC_1SEEK(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_LOC_SEEK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1LOC_1IO(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_LOC_IO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1FILE_1READ(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_FILE_READ;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1FILE_1WRITE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_FILE_WRITE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1FILE_1IO(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_FILE_IO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1FLAVOR(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_FLAVOR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1NUM_1READ(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_NUM_READ;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1NUM_1WRITE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_NUM_WRITE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1NUM_1SEEK(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_NUM_SEEK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1NUM_1TRUNCATE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_NUM_TRUNCATE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1NUM_1IO(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_NUM_IO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1OPEN(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_OPEN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1STAT(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_STAT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1READ(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_READ;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1WRITE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_WRITE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1SEEK(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_SEEK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1CLOSE(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_CLOSE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1TIME_1IO(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_TIME_IO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1ALLOC(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_ALLOC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1LOG_1ALL(JNIEnv *env, jclass cls)
{
    return H5FD_LOG_ALL;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1NOLIST(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_NOLIST;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1SUPER(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_SUPER;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1BTREE(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_BTREE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DRAW(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_DRAW;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1GHEAP(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_GHEAP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1LHEAP(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_LHEAP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1OHDR(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_OHDR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1NTYPES(JNIEnv *env, jclass cls)
{
    return H5FD_MEM_NTYPES;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1DEFAULT_1HADDR_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(HADDR_MAX / H5FD_MEM_NTYPES);
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)0;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1SUPER_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)0;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1BTREE_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(1 * (HADDR_MAX / (H5FD_MEM_NTYPES - 1)));
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1DRAW_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(2 * (HADDR_MAX / (H5FD_MEM_NTYPES - 1)));
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1GHEAP_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(3 * (HADDR_MAX / (H5FD_MEM_NTYPES - 1)));
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1LHEAP_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(4 * (HADDR_MAX / (H5FD_MEM_NTYPES - 1)));
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5FD_1MEM_1DEFAULT_1OHDR_1SIZE(JNIEnv *env, jclass cls)
{
    return (hsize_t)(5 * (HADDR_MAX / (H5FD_MEM_NTYPES - 1)));
}

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1DATASET(JNIEnv *env, jclass cls)
{
    return H5G_DATASET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1GROUP(JNIEnv *env, jclass cls)
{
    return H5G_GROUP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1LINK(JNIEnv *env, jclass cls)
{
    return H5G_LINK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1UDLINK(JNIEnv *env, jclass cls)
{
    return H5G_UDLINK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1RESERVED_15(JNIEnv *env, jclass cls)
{
    return H5G_RESERVED_5;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1RESERVED_16(JNIEnv *env, jclass cls)
{
    return H5G_RESERVED_6;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1RESERVED_17(JNIEnv *env, jclass cls)
{
    return H5G_RESERVED_7;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1TYPE(JNIEnv *env, jclass cls)
{
    return H5G_TYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1UNKNOWN(JNIEnv *env, jclass cls)
{
    return H5G_UNKNOWN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1LINK_1ERROR(JNIEnv *env, jclass cls)
{
    return H5G_LINK_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1LINK_1HARD(JNIEnv *env, jclass cls)
{
    return H5G_LINK_HARD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1LINK_1SOFT(JNIEnv *env, jclass cls)
{
    return H5G_LINK_SOFT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1NLIBTYPES(JNIEnv *env, jclass cls)
{
    return H5G_NLIBTYPES;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1NTYPES(JNIEnv *env, jclass cls)
{
    return H5G_NTYPES;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1NUSERTYPES(JNIEnv *env, jclass cls)
{
    return H5G_NUSERTYPES;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1SAME_1LOC(JNIEnv *env, jclass cls)
{
    return H5G_SAME_LOC;
}

#endif /* H5_NO_DEPRECATED_SYMBOLS */

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1STORAGE_1TYPE_1UNKNOWN(JNIEnv *env, jclass cls)
{
    return H5G_STORAGE_TYPE_UNKNOWN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1STORAGE_1TYPE_1SYMBOL_1TABLE(JNIEnv *env, jclass cls)
{
    return H5G_STORAGE_TYPE_SYMBOL_TABLE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1STORAGE_1TYPE_1COMPACT(JNIEnv *env, jclass cls)
{
    return H5G_STORAGE_TYPE_COMPACT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5G_1STORAGE_1TYPE_1DENSE(JNIEnv *env, jclass cls)
{
    return H5G_STORAGE_TYPE_DENSE;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1UNINIT(JNIEnv *env, jclass cls)
{
    return H5I_UNINIT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1BADID(JNIEnv *env, jclass cls)
{
    return H5I_BADID;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1FILE(JNIEnv *env, jclass cls)
{
    return H5I_FILE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1GROUP(JNIEnv *env, jclass cls)
{
    return H5I_GROUP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1DATATYPE(JNIEnv *env, jclass cls)
{
    return H5I_DATATYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1DATASPACE(JNIEnv *env, jclass cls)
{
    return H5I_DATASPACE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1DATASET(JNIEnv *env, jclass cls)
{
    return H5I_DATASET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1ATTR(JNIEnv *env, jclass cls)
{
    return H5I_ATTR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1VFL(JNIEnv *env, jclass cls)
{
    return H5I_VFL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1VOL(JNIEnv *env, jclass cls)
{
    return H5I_VOL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1INVALID_1HID(JNIEnv *env, jclass cls)
{
    return H5I_INVALID_HID;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1GENPROP_1CLS(JNIEnv *env, jclass cls)
{
    return H5I_GENPROP_CLS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1GENPROP_1LST(JNIEnv *env, jclass cls)
{
    return H5I_GENPROP_LST;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1ERROR_1CLASS(JNIEnv *env, jclass cls)
{
    return H5I_ERROR_CLASS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1ERROR_1MSG(JNIEnv *env, jclass cls)
{
    return H5I_ERROR_MSG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1ERROR_1STACK(JNIEnv *env, jclass cls)
{
    return H5I_ERROR_STACK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5I_1NTYPES(JNIEnv *env, jclass cls)
{
    return H5I_NTYPES;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5L_1TYPE_1ERROR(JNIEnv *env, jclass cls)
{
    return H5L_TYPE_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5L_1TYPE_1HARD(JNIEnv *env, jclass cls)
{
    return H5L_TYPE_HARD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5L_1TYPE_1SOFT(JNIEnv *env, jclass cls)
{
    return H5L_TYPE_SOFT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5L_1TYPE_1EXTERNAL(JNIEnv *env, jclass cls)
{
    return H5L_TYPE_EXTERNAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5L_1TYPE_1MAX(JNIEnv *env, jclass cls)
{
    return H5L_TYPE_MAX;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1SHALLOW_1HIERARCHY_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_SHALLOW_HIERARCHY_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1EXPAND_1SOFT_1LINK_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_EXPAND_SOFT_LINK_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1EXPAND_1EXT_1LINK_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_EXPAND_EXT_LINK_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1EXPAND_1REFERENCE_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_EXPAND_REFERENCE_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1WITHOUT_1ATTR_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_WITHOUT_ATTR_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1COPY_1PRESERVE_1NULL_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_COPY_PRESERVE_NULL_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1INFO_1BASIC(JNIEnv *env, jclass cls)
{
    return H5O_INFO_BASIC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1INFO_1TIME(JNIEnv *env, jclass cls)
{
    return H5O_INFO_TIME;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1INFO_1NUM_1ATTRS(JNIEnv *env, jclass cls)
{
    return H5O_INFO_NUM_ATTRS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1INFO_1ALL(JNIEnv *env, jclass cls)
{
    return H5O_INFO_ALL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1NATIVE_1INFO_1HDR(JNIEnv *env, jclass cls)
{
    return H5O_NATIVE_INFO_HDR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1NATIVE_1INFO_1META_1SIZE(JNIEnv *env, jclass cls)
{
    return H5O_NATIVE_INFO_META_SIZE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1NATIVE_1INFO_1ALL(JNIEnv *env, jclass cls)
{
    return H5O_NATIVE_INFO_ALL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1NONE_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_NONE_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1SDSPACE_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_SDSPACE_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1DTYPE_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_DTYPE_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1FILL_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_FILL_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1PLINE_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_PLINE_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1ATTR_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_ATTR_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1SHMESG_1ALL_1FLAG(JNIEnv *env, jclass cls)
{
    return H5O_SHMESG_ALL_FLAG;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TYPE_1UNKNOWN(JNIEnv *env, jclass cls)
{
    return H5O_TYPE_UNKNOWN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TYPE_1GROUP(JNIEnv *env, jclass cls)
{
    return H5O_TYPE_GROUP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TYPE_1DATASET(JNIEnv *env, jclass cls)
{
    return H5O_TYPE_DATASET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TYPE_1NAMED_1DATATYPE(JNIEnv *env, jclass cls)
{
    return H5O_TYPE_NAMED_DATATYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TYPE_1NTYPES(JNIEnv *env, jclass cls)
{
    return H5O_TYPE_NTYPES;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1MAX_1TOKEN_1SIZE(JNIEnv *env, jclass cls)
{
    return H5O_MAX_TOKEN_SIZE;
}
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5O_1TOKEN_1UNDEF(JNIEnv *env, jclass cls)
{
    H5O_token_t undef_token = H5O_TOKEN_UNDEF;

    /* TODO: Can be optimized by keeping a global reference to the undefined token class */
    return create_H5O_token_t(env, &undef_token, FALSE);
}

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1ROOT(JNIEnv *env, jclass cls)
{
    return H5P_ROOT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1OBJECT_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_OBJECT_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_FILE_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_FILE_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1XFER(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_XFER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1MOUNT(JNIEnv *env, jclass cls)
{
    return H5P_FILE_MOUNT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1GROUP_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_GROUP_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1GROUP_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_GROUP_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATATYPE_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_DATATYPE_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATATYPE_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_DATATYPE_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1STRING_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_STRING_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1ATTRIBUTE_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_ATTRIBUTE_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1ATTRIBUTE_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_ATTRIBUTE_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1OBJECT_1COPY(JNIEnv *env, jclass cls)
{
    return H5P_OBJECT_COPY;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1LINK_1CREATE(JNIEnv *env, jclass cls)
{
    return H5P_LINK_CREATE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1LINK_1ACCESS(JNIEnv *env, jclass cls)
{
    return H5P_LINK_ACCESS;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1VOL_1INITIALIZE(JNIEnv *env, jclass cls)
{
    return H5P_VOL_INITIALIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_FILE_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_FILE_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATASET_1XFER_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DATASET_XFER_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1FILE_1MOUNT_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_FILE_MOUNT_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1GROUP_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_GROUP_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1GROUP_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_GROUP_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATATYPE_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DATATYPE_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DATATYPE_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DATATYPE_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1ATTRIBUTE_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_ATTRIBUTE_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1ATTRIBUTE_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_ATTRIBUTE_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1OBJECT_1COPY_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_OBJECT_COPY_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1LINK_1CREATE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_LINK_CREATE_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1LINK_1ACCESS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_LINK_ACCESS_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1VOL_1INITIALIZE_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_VOL_INITIALIZE_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1CRT_1ORDER_1TRACKED(JNIEnv *env, jclass cls)
{
    return H5P_CRT_ORDER_TRACKED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1CRT_1ORDER_1INDEXED(JNIEnv *env, jclass cls)
{
    return H5P_CRT_ORDER_INDEXED;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5P_DEFAULT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5P_1NO_1CLASS(JNIEnv *env, jclass cls)
{
    return H5P_ROOT;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1TYPE_1ERROR(JNIEnv *env, jclass cls)
{
    return H5PL_TYPE_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1TYPE_1FILTER(JNIEnv *env, jclass cls)
{
    return H5PL_TYPE_FILTER;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1TYPE_1VOL(JNIEnv *env, jclass cls)
{
    return H5PL_TYPE_VOL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1TYPE_1NONE(JNIEnv *env, jclass cls)
{
    return H5PL_TYPE_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1FILTER_1PLUGIN(JNIEnv *env, jclass cls)
{
    return H5PL_FILTER_PLUGIN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1VOL_1PLUGIN(JNIEnv *env, jclass cls)
{
    return H5PL_VOL_PLUGIN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5PL_1ALL_1PLUGIN(JNIEnv *env, jclass cls)
{
    return H5PL_ALL_PLUGIN;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1BADTYPE(JNIEnv *env, jclass cls)
{
    return H5R_BADTYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1MAXTYPE(JNIEnv *env, jclass cls)
{
    return H5R_MAXTYPE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1REF_1BUF_1SIZE(JNIEnv *env, jclass cls)
{
    return H5R_REF_BUF_SIZE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1OBJ_1REF_1BUF_1SIZE(JNIEnv *env, jclass cls)
{
    return H5R_OBJ_REF_BUF_SIZE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1DSET_1REG_1REF_1BUF_1SIZE(JNIEnv *env, jclass cls)
{
    return H5R_DSET_REG_REF_BUF_SIZE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1ATTR(JNIEnv *env, jclass cls)
{
    return H5R_ATTR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1OBJECT(JNIEnv *env, jclass cls)
{
    return H5R_OBJECT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1OBJECT1(JNIEnv *env, jclass cls)
{
    return H5R_OBJECT1;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1OBJECT2(JNIEnv *env, jclass cls)
{
    return H5R_OBJECT2;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1DATASET_1REGION(JNIEnv *env, jclass cls)
{
    return H5R_DATASET_REGION;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1DATASET_1REGION1(JNIEnv *env, jclass cls)
{
    return H5R_DATASET_REGION1;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5R_1DATASET_1REGION2(JNIEnv *env, jclass cls)
{
    return H5R_DATASET_REGION2;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1ALL(JNIEnv *env, jclass cls)
{
    return H5S_ALL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1MAX_1RANK(JNIEnv *env, jclass cls)
{
    return H5S_MAX_RANK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1NO_1CLASS(JNIEnv *env, jclass cls)
{
    return H5S_NO_CLASS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1NULL(JNIEnv *env, jclass cls)
{
    return H5S_NULL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SCALAR(JNIEnv *env, jclass cls)
{
    return H5S_SCALAR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1ALL(JNIEnv *env, jclass cls)
{
    return H5S_SEL_ALL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1ERROR(JNIEnv *env, jclass cls)
{
    return H5S_SEL_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1HYPERSLABS(JNIEnv *env, jclass cls)
{
    return H5S_SEL_HYPERSLABS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1N(JNIEnv *env, jclass cls)
{
    return H5S_SEL_N;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1NONE(JNIEnv *env, jclass cls)
{
    return H5S_SEL_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SEL_1POINTS(JNIEnv *env, jclass cls)
{
    return H5S_SEL_POINTS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1AND(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_AND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1APPEND(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_APPEND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1INVALID(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_INVALID;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1NOOP(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_NOOP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1NOTA(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_NOTA;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1NOTB(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_NOTB;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1OR(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_OR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1PREPEND(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_PREPEND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1SET(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_SET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SELECT_1XOR(JNIEnv *env, jclass cls)
{
    return H5S_SELECT_XOR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1SIMPLE(JNIEnv *env, jclass cls)
{
    return H5S_SIMPLE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5S_1UNLIMITED(JNIEnv *env, jclass cls)
{
    return (jint)H5S_UNLIMITED;
}

JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1B16(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_B16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1B32(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_B32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1B64(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_B64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1B8(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_B8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1F32(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_F32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1F64(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_F64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1I16(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_I16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1I32(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_I32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1I64(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_I64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1I8(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_I8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1U16(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_U16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1U32(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_U32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1U64(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_U64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ALPHA_1U8(JNIEnv *env, jclass cls)
{
    return H5T_ALPHA_U8;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ARRAY(JNIEnv *env, jclass cls)
{
    return H5T_ARRAY;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1BITFIELD(JNIEnv *env, jclass cls)
{
    return H5T_BITFIELD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1BKG_1NO(JNIEnv *env, jclass cls)
{
    return H5T_BKG_NO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1BKG_1YES(JNIEnv *env, jclass cls)
{
    return H5T_BKG_YES;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1C_1S1(JNIEnv *env, jclass cls)
{
    return H5T_C_S1;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1COMPOUND(JNIEnv *env, jclass cls)
{
    return H5T_COMPOUND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CONV_1CONV(JNIEnv *env, jclass cls)
{
    return H5T_CONV_CONV;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CONV_1FREE(JNIEnv *env, jclass cls)
{
    return H5T_CONV_FREE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CONV_1INIT(JNIEnv *env, jclass cls)
{
    return H5T_CONV_INIT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_CSET_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1ASCII(JNIEnv *env, jclass cls)
{
    return H5T_CSET_ASCII;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1UTF8(JNIEnv *env, jclass cls)
{
    return H5T_CSET_UTF8;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_110(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_10;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_111(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_11;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_112(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_12;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_113(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_13;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_114(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_14;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_115(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_15;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_12(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_2;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_13(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_3;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_14(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_4;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_15(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_5;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_16(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_6;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_17(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_7;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_18(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_8;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1CSET_1RESERVED_19(JNIEnv *env, jclass cls)
{
    return H5T_CSET_RESERVED_9;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1DIR_1ASCEND(JNIEnv *env, jclass cls)
{
    return H5T_DIR_ASCEND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1DIR_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5T_DIR_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1DIR_1DESCEND(JNIEnv *env, jclass cls)
{
    return H5T_DIR_DESCEND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ENUM(JNIEnv *env, jclass cls)
{
    return H5T_ENUM;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1FLOAT(JNIEnv *env, jclass cls)
{
    return H5T_FLOAT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1FORTRAN_1S1(JNIEnv *env, jclass cls)
{
    return H5T_FORTRAN_S1;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1IEEE_1F32BE(JNIEnv *env, jclass cls)
{
    return H5T_IEEE_F32BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1IEEE_1F32LE(JNIEnv *env, jclass cls)
{
    return H5T_IEEE_F32LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1IEEE_1F64BE(JNIEnv *env, jclass cls)
{
    return H5T_IEEE_F64BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1IEEE_1F64LE(JNIEnv *env, jclass cls)
{
    return H5T_IEEE_F64LE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEGER(JNIEnv *env, jclass cls)
{
    return H5T_INTEGER;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1B16(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_B16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1B32(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_B32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1B64(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_B64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1B8(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_B8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1F32(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_F32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1F64(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_F64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1I16(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_I16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1I32(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_I32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1I64(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_I64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1I8(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_I8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1U16(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_U16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1U32(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_U32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1U64(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_U64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1INTEL_1U8(JNIEnv *env, jclass cls)
{
    return H5T_INTEL_U8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1B16(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_B16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1B32(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_B32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1B64(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_B64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1B8(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_B8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1F32(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_F32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1F64(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_F64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1I16(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_I16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1I32(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_I32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1I64(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_I64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1I8(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_I8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1U16(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_U16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1U32(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_U32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1U64(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_U64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1MIPS_1U8(JNIEnv *env, jclass cls)
{
    return H5T_MIPS_U8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1B16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_B16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1B32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_B32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1B64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_B64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1B8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_B8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1CHAR(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_CHAR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1DOUBLE(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_DOUBLE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1FLOAT(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_FLOAT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1HADDR(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_HADDR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1HBOOL(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_HBOOL;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1HERR(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_HERR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1HSIZE(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_HSIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1HSSIZE(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_HSSIZE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1FAST16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_FAST16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1FAST32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_FAST32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1FAST64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_FAST64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1FAST8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_FAST8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1LEAST16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_LEAST16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1LEAST32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_LEAST32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1LEAST64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_LEAST64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT_1LEAST8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT_LEAST8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1INT8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_INT8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1LDOUBLE(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_LDOUBLE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1LLONG(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_LLONG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1LONG(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_LONG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1OPAQUE(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_OPAQUE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1SCHAR(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_SCHAR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1SHORT(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_SHORT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UCHAR(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UCHAR;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1FAST16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_FAST16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1FAST32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_FAST32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1FAST64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_FAST64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1FAST8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_FAST8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1LEAST16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_LEAST16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1LEAST32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_LEAST32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1LEAST64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_LEAST64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT_1LEAST8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT_LEAST8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT16(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT16;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT32(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT32;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT64(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT64;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1UINT8(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_UINT8;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1ULLONG(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_ULLONG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1ULONG(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_ULONG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NATIVE_1USHORT(JNIEnv *env, jclass cls)
{
    return H5T_NATIVE_USHORT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NCLASSES(JNIEnv *env, jclass cls)
{
    return H5T_NCLASSES;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NO_1CLASS(JNIEnv *env, jclass cls)
{
    return H5T_NO_CLASS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NORM_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_NORM_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NORM_1IMPLIED(JNIEnv *env, jclass cls)
{
    return H5T_NORM_IMPLIED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NORM_1MSBSET(JNIEnv *env, jclass cls)
{
    return H5T_NORM_MSBSET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NORM_1NONE(JNIEnv *env, jclass cls)
{
    return H5T_NORM_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NPAD(JNIEnv *env, jclass cls)
{
    return H5T_NPAD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1NSGN(JNIEnv *env, jclass cls)
{
    return H5T_NSGN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1OPAQUE(JNIEnv *env, jclass cls)
{
    return H5T_OPAQUE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1OPAQUE_1TAG_1MAX(JNIEnv *env, jclass cls)
{
    return H5T_OPAQUE_TAG_MAX;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ORDER_1BE(JNIEnv *env, jclass cls)
{
    return H5T_ORDER_BE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ORDER_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_ORDER_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ORDER_1LE(JNIEnv *env, jclass cls)
{
    return H5T_ORDER_LE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ORDER_1NONE(JNIEnv *env, jclass cls)
{
    return H5T_ORDER_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1ORDER_1VAX(JNIEnv *env, jclass cls)
{
    return H5T_ORDER_VAX;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PAD_1BACKGROUND(JNIEnv *env, jclass cls)
{
    return H5T_PAD_BACKGROUND;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PAD_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_PAD_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PAD_1ONE(JNIEnv *env, jclass cls)
{
    return H5T_PAD_ONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PAD_1ZERO(JNIEnv *env, jclass cls)
{
    return H5T_PAD_ZERO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PERS_1DONTCARE(JNIEnv *env, jclass cls)
{
    return H5T_PERS_DONTCARE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PERS_1HARD(JNIEnv *env, jclass cls)
{
    return H5T_PERS_HARD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1PERS_1SOFT(JNIEnv *env, jclass cls)
{
    return H5T_PERS_SOFT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1REFERENCE(JNIEnv *env, jclass cls)
{
    return H5T_REFERENCE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1SGN_12(JNIEnv *env, jclass cls)
{
    return H5T_SGN_2;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1SGN_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_SGN_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1SGN_1NONE(JNIEnv *env, jclass cls)
{
    return H5T_SGN_NONE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B16BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B16BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B16LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B16LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B32BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B32BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B32LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B32LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B64BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B64BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B64LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B64LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B8BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B8BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1B8LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_B8LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I16BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I16BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I16LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I16LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I32BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I32BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I32LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I32LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I64BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I64BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I64LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I64LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I8BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I8BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1I8LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_I8LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1REF_1DSETREG(JNIEnv *env, jclass cls)
{
    return H5T_STD_REF_DSETREG;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1REF_1OBJ(JNIEnv *env, jclass cls)
{
    return H5T_STD_REF_OBJ;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1REF(JNIEnv *env, jclass cls)
{
    return H5T_STD_REF;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U16BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U16BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U16LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U16LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U32BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U32BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U32LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U32LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U64BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U64BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U64LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U64LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U8BE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U8BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STD_1U8LE(JNIEnv *env, jclass cls)
{
    return H5T_STD_U8LE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1ERROR(JNIEnv *env, jclass cls)
{
    return H5T_STR_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1NULLPAD(JNIEnv *env, jclass cls)
{
    return H5T_STR_NULLPAD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1NULLTERM(JNIEnv *env, jclass cls)
{
    return H5T_STR_NULLTERM;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_110(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_10;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_111(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_11;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_112(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_12;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_113(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_13;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_114(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_14;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_115(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_15;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_13(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_3;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_14(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_4;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_15(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_5;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_16(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_6;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_17(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_7;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_18(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_8;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1RESERVED_19(JNIEnv *env, jclass cls)
{
    return H5T_STR_RESERVED_9;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STR_1SPACEPAD(JNIEnv *env, jclass cls)
{
    return H5T_STR_SPACEPAD;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1STRING(JNIEnv *env, jclass cls)
{
    return H5T_STRING;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1TIME(JNIEnv *env, jclass cls)
{
    return H5T_TIME;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1UNIX_1D32BE(JNIEnv *env, jclass cls)
{
    return H5T_UNIX_D32BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1UNIX_1D32LE(JNIEnv *env, jclass cls)
{
    return H5T_UNIX_D32LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1UNIX_1D64BE(JNIEnv *env, jclass cls)
{
    return H5T_UNIX_D64BE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1UNIX_1D64LE(JNIEnv *env, jclass cls)
{
    return H5T_UNIX_D64LE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1VARIABLE(JNIEnv *env, jclass cls)
{
    return (int)H5T_VARIABLE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1VLEN(JNIEnv *env, jclass cls)
{
    return H5T_VLEN;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5T_1VL_1T(JNIEnv *env, jclass cls)
{
    return sizeof(hvl_t);
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1CAP_1FLAG_1NONE(JNIEnv *env, jclass cls)
{
    return H5VL_CAP_FLAG_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1CAP_1FLAG_1THREADSAFE(JNIEnv *env, jclass cls)
{
    return H5VL_CAP_FLAG_THREADSAFE;
}
JNIEXPORT jlong JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1NATIVE(JNIEnv *env, jclass cls)
{
    return H5VL_NATIVE;
}
JNIEXPORT jobject JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1NATIVE_1NAME(JNIEnv *env, jclass cls)
{
    return (jstring)ENVPTR->NewStringUTF(ENVONLY, H5VL_NATIVE_NAME);
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1NATIVE_1VALUE(JNIEnv *env, jclass cls)
{
    return H5VL_NATIVE_VALUE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5VL_1NATIVE_1VERSION(JNIEnv *env, jclass cls)
{
    return H5VL_NATIVE_VERSION;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1VOL_1INVALID(JNIEnv *env, jclass cls)
{
    return H5_VOL_INVALID;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1VOL_1NATIVE(JNIEnv *env, jclass cls)
{
    return H5_VOL_NATIVE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1VOL_1RESERVED(JNIEnv *env, jclass cls)
{
    return H5_VOL_RESERVED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5_1VOL_1MAX(JNIEnv *env, jclass cls)
{
    return H5_VOL_MAX;
}

JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1CB_1CONT(JNIEnv *env, jclass cls)
{
    return H5Z_CB_CONT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1CB_1ERROR(JNIEnv *env, jclass cls)
{
    return H5Z_CB_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1CB_1FAIL(JNIEnv *env, jclass cls)
{
    return H5Z_CB_FAIL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1CB_1NO(JNIEnv *env, jclass cls)
{
    return H5Z_CB_NO;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1DISABLE_1EDC(JNIEnv *env, jclass cls)
{
    return H5Z_DISABLE_EDC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1ENABLE_1EDC(JNIEnv *env, jclass cls)
{
    return H5Z_ENABLE_EDC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1ERROR_1EDC(JNIEnv *env, jclass cls)
{
    return H5Z_ERROR_EDC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1CONFIG_1DECODE_1ENABLED(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_CONFIG_DECODE_ENABLED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1CONFIG_1ENCODE_1ENABLED(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_CONFIG_ENCODE_ENABLED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1DEFLATE(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_DEFLATE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1ERROR(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_ERROR;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1FLETCHER32(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_FLETCHER32;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1MAX(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_MAX;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1NBIT(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_NBIT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1NONE(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_NONE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1RESERVED(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_RESERVED;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1SCALEOFFSET(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_SCALEOFFSET;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1SHUFFLE(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_SHUFFLE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1SZIP(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_SZIP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1DEFMASK(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_DEFMASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1INVMASK(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_INVMASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1MANDATORY(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_MANDATORY;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1OPTIONAL(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_OPTIONAL;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1REVERSE(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_REVERSE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FLAG_1SKIP_1EDC(JNIEnv *env, jclass cls)
{
    return H5Z_FLAG_SKIP_EDC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1MAX_1NFILTERS(JNIEnv *env, jclass cls)
{
    return H5Z_MAX_NFILTERS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1NO_1EDC(JNIEnv *env, jclass cls)
{
    return H5Z_NO_EDC;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SO_1INT_1MINBITS_1DEFAULT(JNIEnv *env, jclass cls)
{
    return H5Z_SO_INT_MINBITS_DEFAULT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SO_1FLOAT_1DSCALE(JNIEnv *env, jclass cls)
{
    return H5Z_SO_FLOAT_DSCALE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SO_1FLOAT_1ESCALE(JNIEnv *env, jclass cls)
{
    return H5Z_SO_FLOAT_ESCALE;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SO_1INT(JNIEnv *env, jclass cls)
{
    return H5Z_SO_INT;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SHUFFLE_1USER_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_SHUFFLE_USER_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SHUFFLE_1TOTAL_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_SHUFFLE_TOTAL_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1USER_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_USER_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1TOTAL_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_TOTAL_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1PARM_1MASK(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_PARM_MASK;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1PARM_1PPB(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_PARM_PPB;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1PARM_1BPP(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_PARM_BPP;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SZIP_1PARM_1PPS(JNIEnv *env, jclass cls)
{
    return H5Z_SZIP_PARM_PPS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1NBIT_1USER_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_NBIT_USER_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1SCALEOFFSET_1USER_1NPARMS(JNIEnv *env, jclass cls)
{
    return H5Z_SCALEOFFSET_USER_NPARMS;
}
JNIEXPORT jint JNICALL
Java_hdf_hdf5lib_HDF5Constants_H5Z_1FILTER_1ALL(JNIEnv *env, jclass cls)
{
    return H5Z_FILTER_ALL;
}

H5_GCC_DIAG_ON("missing-prototypes")
H5_GCC_DIAG_ON("unused-parameter")

#ifdef __cplusplus
} /* end extern "C" */
#endif /* __cplusplus */
