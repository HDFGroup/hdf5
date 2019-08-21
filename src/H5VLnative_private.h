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
 * Purpose:	The private header file for the native VOL connector.
 */

#ifndef _H5VLnative_private_H
#define _H5VLnative_private_H

/* Private headers needed by this file */
#include "H5VLnative.h"             /* Native VOL connector                 */


/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

#ifdef __cplusplus
extern "C" {
#endif

/* Attribute callbacks */
H5_DLL void *H5VL__native_attr_create(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name, hid_t type_id, hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
void *H5VL__native_attr_open(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_specific(void *obj, const H5VL_loc_params_t *loc_params, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_attr_close(void *attr, hid_t dxpl_id, void **req);

/* Dataset callbacks */
H5_DLL void *H5VL__native_dataset_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t lcpl_id, hid_t type_id, hid_t space_id, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
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
H5_DLL void *H5VL__native_group_create(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t lcpl_id, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VL__native_group_open(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL__native_group_get(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_specific(void *obj, H5VL_group_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_optional(void *obj, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VL__native_group_close(void *grp, hid_t dxpl_id, void **req);

/* Link callbacks */
H5_DLL herr_t H5VL__native_link_create(H5VL_link_create_type_t create_type, void *obj, const H5VL_loc_params_t *loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req, va_list arguments);
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

/* Blob callbacks */
H5_DLL herr_t H5VL__native_blob_put(void *blob, size_t size, void *ctx, void *id);
H5_DLL herr_t H5VL__native_blob_get(const void *id, void *ctx, void *buf);
H5_DLL herr_t H5VL__native_blob_specific(void *id, H5VL_blob_specific_t specific_type, va_list arguments);
H5_DLL herr_t H5VL__native_blob_optional(void *id, va_list arguments);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLnative_private_H */

