/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * Purpose:	The public header file for the native VOL connector.
 */

#ifndef _H5VLnative_H
#define _H5VLnative_H

/* Private headers needed by this file */
#include "H5VLprivate.h"        /* Virtual Object Layer                 */

/* Identifier for the native VOL connector */
#define H5VL_NATIVE     (H5VL_native_register())

/* Characteristics of the native VOL connector */
#define H5VL_NATIVE_NAME        "native"
#define H5VL_NATIVE_VALUE       H5_VOL_NATIVE   /* enum value */
#define H5VL_NATIVE_VERSION     0

/* Typedef and values for native VOL connector attribute optional VOL operations */
typedef int H5VL_native_attr_optional_t;
#ifndef H5_NO_DEPRECATED_SYMBOLS
#define H5VL_NATIVE_ATTR_ITERATE_OLD    0   /* H5Aiterate (deprecated routine) */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/* Typedef and values for native VOL connector dataset optional VOL operations */
typedef int H5VL_native_dataset_optional_t;
#define H5VL_NATIVE_DATASET_FORMAT_CONVERT          0   /* H5Dformat_convert (internal) */
#define H5VL_NATIVE_DATASET_GET_CHUNK_INDEX_TYPE    1   /* H5Dget_chunk_index_type      */
#define H5VL_NATIVE_DATASET_GET_CHUNK_STORAGE_SIZE  2   /* H5Dget_chunk_storage_size    */
#define H5VL_NATIVE_DATASET_GET_NUM_CHUNKS          3   /* H5Dget_num_chunks            */
#define H5VL_NATIVE_DATASET_GET_CHUNK_INFO_BY_IDX   4   /* H5Dget_chunk_info            */
#define H5VL_NATIVE_DATASET_GET_CHUNK_INFO_BY_COORD 5   /* H5Dget_chunk_info_by_coord   */
#define H5VL_NATIVE_DATASET_CHUNK_READ              6   /* H5Dchunk_read                */
#define H5VL_NATIVE_DATASET_CHUNK_WRITE             7   /* H5Dchunk_write               */

/* Typedef and values for native VOL connector file optional VOL operations */
typedef int H5VL_native_file_optional_t;
#define H5VL_NATIVE_FILE_CLEAR_ELINK_CACHE              0   /* H5Fclear_elink_file_cache            */
#define H5VL_NATIVE_FILE_GET_FILE_IMAGE                 1   /* H5Fget_file_image                    */
#define H5VL_NATIVE_FILE_GET_FREE_SECTIONS              2   /* H5Fget_free_sections                 */
#define H5VL_NATIVE_FILE_GET_FREE_SPACE                 3   /* H5Fget_freespace                     */
#define H5VL_NATIVE_FILE_GET_INFO                       4   /* H5Fget_info1/2                       */
#define H5VL_NATIVE_FILE_GET_MDC_CONF                   5   /* H5Fget_mdc_config                    */
#define H5VL_NATIVE_FILE_GET_MDC_HR                     6   /* H5Fget_mdc_hit_rate                  */
#define H5VL_NATIVE_FILE_GET_MDC_SIZE                   7   /* H5Fget_mdc_size                      */
#define H5VL_NATIVE_FILE_GET_SIZE                       8   /* H5Fget_filesize                      */
#define H5VL_NATIVE_FILE_GET_VFD_HANDLE                 9   /* H5Fget_vfd_handle                    */
#define H5VL_NATIVE_FILE_GET_FILE_ID                    10  /* H5Fget_file_id                       */
#define H5VL_NATIVE_FILE_RESET_MDC_HIT_RATE             11  /* H5Freset_mdc_hit_rate_stats          */
#define H5VL_NATIVE_FILE_SET_MDC_CONFIG                 12  /* H5Fset_mdc_config                    */
#define H5VL_NATIVE_FILE_GET_METADATA_READ_RETRY_INFO   13  /* H5Fget_metadata_read_retry_info      */
#define H5VL_NATIVE_FILE_START_SWMR_WRITE               14  /* H5Fstart_swmr_write                  */
#define H5VL_NATIVE_FILE_START_MDC_LOGGING              15  /* H5Fstart_mdc_logging                 */
#define H5VL_NATIVE_FILE_STOP_MDC_LOGGING               16  /* H5Fstop_mdc_logging                  */
#define H5VL_NATIVE_FILE_GET_MDC_LOGGING_STATUS         17  /* H5Fget_mdc_logging_status            */
#define H5VL_NATIVE_FILE_FORMAT_CONVERT                 18  /* H5Fformat_convert                    */
#define H5VL_NATIVE_FILE_RESET_PAGE_BUFFERING_STATS     19  /* H5Freset_page_buffering_stats        */
#define H5VL_NATIVE_FILE_GET_PAGE_BUFFERING_STATS       20  /* H5Fget_page_buffering_stats          */
#define H5VL_NATIVE_FILE_GET_MDC_IMAGE_INFO             21  /* H5Fget_mdc_image_info                */
#define H5VL_NATIVE_FILE_GET_EOA                        22  /* H5Fget_eoa                           */
#define H5VL_NATIVE_FILE_INCR_FILESIZE                  23  /* H5Fincrement_filesize                */
#define H5VL_NATIVE_FILE_SET_LIBVER_BOUNDS              24  /* H5Fset_latest_format/libver_bounds   */
#define H5VL_NATIVE_FILE_GET_MIN_DSET_OHDR_FLAG         25  /* H5Fget_dset_no_attrs_hint            */
#define H5VL_NATIVE_FILE_SET_MIN_DSET_OHDR_FLAG         26  /* H5Fset_dset_no_attrs_hint            */

/* Typedef and values for native VOL connector group optional VOL operations */
typedef int H5VL_native_group_optional_t;
#ifndef H5_NO_DEPRECATED_SYMBOLS
#define H5VL_NATIVE_GROUP_ITERATE_OLD   0   /* HG5Giterate (deprecated routine) */
#define H5VL_NATIVE_GROUP_GET_OBJINFO   1   /* HG5Gget_objinfo (deprecated routine) */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/* Typedef and values for native VOL connector object optional VOL operations */
typedef int H5VL_native_object_optional_t;
#define H5VL_NATIVE_OBJECT_GET_COMMENT      0   /* H5G|H5Oget_comment, H5Oget_comment_by_name   */
#define H5VL_NATIVE_OBJECT_GET_INFO         1   /* H5Oget_info(_by_idx, _by_name)(2)            */
#define H5VL_NATIVE_OBJECT_SET_COMMENT      2   /* H5G|H5Oset_comment, H5Oset_comment_by_name   */

#ifdef __cplusplus
extern "C" {
#endif

/* Private functions */
H5_DLL hid_t H5VL_native_register(void);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLnative_H */

