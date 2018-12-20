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
#define H5VL_NATIVE_DATASET_CHUNK_READ              3   /* H5Dchunk_read                */
#define H5VL_NATIVE_DATASET_CHUNK_WRITE             4   /* H5Dchunk_write               */

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

/* Atrribute callbacks */
H5_DLL void *H5VL__native_attr_create(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
void *H5VL__native_attr_open(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_specific(void *obj, const H5VL_loc_params_t *loc_params, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_close(void *attr, hid_t dxpl_id, void **req);

/* Dataset callbacks */
H5_DLL void *H5VL__native_dataset_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VL__native_dataset_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, void **req);
H5_DLL herr_t H5VL__native_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
H5_DLL herr_t H5VL__native_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_dataset_specific(void *dset, H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_dataset_optional(void *dset, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_dataset_close(void *dset, hid_t dxpl_id, void **req);

/* File callbacks */
H5_DLL void *H5VL__native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VL__native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_file_get(void *file, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_file_specific(void *file, H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_file_optional(void *file, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_file_close(void *file, hid_t dxpl_id, void **req);

/* Group callbacks */
H5_DLL void *H5VL__native_group_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VL__native_group_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_specific(void *obj, H5VL_group_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
H5_DLL herr_t H5VL__native_link_create(H5VL_link_create_type_t create_type, void *obj, const H5VL_loc_params_t *loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_link_copy(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj, const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_link_move(void *src_obj, const H5VL_loc_params_t *loc_params1, void *dst_obj, const H5VL_loc_params_t *loc_params2, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_link_get(void *obj, const H5VL_loc_params_t *loc_params, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_link_specific(void *obj, const H5VL_loc_params_t *loc_params, H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);

/* Object callbacks */
H5_DLL void *H5VL__native_object_open(void *obj, const H5VL_loc_params_t *loc_params, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_object_copy(void *src_obj, const H5VL_loc_params_t *loc_params1, const char *src_name, void *dst_obj, const H5VL_loc_params_t *loc_params2, const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_object_get(void *obj, const H5VL_loc_params_t *loc_params, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_object_specific(void *obj, const H5VL_loc_params_t *loc_params, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_object_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);

/* Datatype callbacks */
H5_DLL void *H5VL__native_datatype_commit(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VL__native_datatype_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_datatype_get(void *dt, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_datatype_specific(void *dt, H5VL_datatype_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_datatype_close(void *dt, hid_t dxpl_id, void **req);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLnative_H */

