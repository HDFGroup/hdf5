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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2011
 */
#ifndef _H5VLprivate_H
#define _H5VLprivate_H

#include "H5VLpublic.h"

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/
#define H5_REQUEST_NULL -1

#define H5VL_NUM_OPS 50

/* Enum contiaing all possible VOL Operations */
typedef enum H5VL_op_type_t {
    H5VL_FILE_CREATE,
    H5VL_FILE_OPEN,
    H5VL_FILE_FLUSH,
    H5VL_FILE_CLOSE,
    H5VL_ATTR_CREATE,
    H5VL_ATTR_OPEN,
    H5VL_ATTR_READ,
    H5VL_ATTR_WRITE,
    H5VL_ATTR_REMOVE,
    H5VL_ATTR_CLOSE,
    H5VL_DSET_CREATE,
    H5VL_DSET_OPEN,
    H5VL_DSET_SET_EXTENT,
    H5VL_DSET_READ,
    H5VL_DSET_WRITE,
    H5VL_DSET_CLOSE,
    H5VL_DTYPE_COMMIT,
    H5VL_DTYPE_OPEN,
    H5VL_DTYPE_CLOSE,
    H5VL_GROUP_CREATE,
    H5VL_GROUP_OPEN,
    H5VL_GROUP_CLOSE,
    H5VL_LINK_CREATE,
    H5VL_LINK_MOVE,
    H5VL_LINK_REMOVE,
    H5VL_ALLOC,
    H5VL_GET_EOA,
    H5VL_SET_EOA
} H5VL_op_type_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/

/* Forward declarations for prototype arguments */
struct H5P_genplist_t;
struct H5F_t;

H5_DLL int H5VL_term_interface(void);
H5_DLL H5VL_class_t *H5VL_get_class(hid_t id);
H5_DLL hid_t  H5VL_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL hid_t H5VL_object_register(void *obj, H5I_type_t obj_type, H5VL_t *vol_plugin, hbool_t app_ref);
H5_DLL ssize_t H5VL_get_plugin_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL void *H5VL_get_object(hid_t id);
H5_DLL herr_t H5VL_close(H5VL_class_t *vol_plugin);

H5_DLL void *H5VL_attr_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
H5_DLL void *H5VL_attr_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t aapl_id, hid_t req);
H5_DLL herr_t H5VL_attr_read(void *attr, H5VL_t *vol_plugin, hid_t dtype_id, void *buf, hid_t req);
H5_DLL herr_t H5VL_attr_write(void *attr, H5VL_t *vol_plugin, hid_t dtype_id, const void *buf, hid_t req);
H5_DLL herr_t H5VL_attr_get(void *attr, H5VL_t *vol_plugin, H5VL_attr_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_attr_remove(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin,  const char *attr_name, hid_t req);
H5_DLL herr_t H5VL_attr_close(void *attr, H5VL_t *vol_plugin, hid_t req);

H5_DLL void *H5VL_dataset_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
H5_DLL void *H5VL_dataset_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t dapl_id, hid_t req);
H5_DLL herr_t H5VL_dataset_read(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
H5_DLL herr_t H5VL_dataset_write(void *dset, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
H5_DLL herr_t H5VL_dataset_set_extent(void *dset, H5VL_t *vol_plugin, const hsize_t size[], hid_t req);
H5_DLL herr_t H5VL_dataset_get(void *dset, H5VL_t *vol_plugin, H5VL_dataset_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_dataset_close(void *dset, H5VL_t *vol_plugin, hid_t req);

H5_DLL void *H5VL_datatype_commit(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
H5_DLL void *H5VL_datatype_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t tapl_id, hid_t req);
H5_DLL ssize_t H5VL_datatype_get_binary(void *obj, H5VL_t *vol_plugin, unsigned char *buf, size_t size, hid_t req);
H5_DLL herr_t H5VL_datatype_close(void *dt, H5VL_t *vol_plugin, hid_t req);

H5_DLL void *H5VL_file_create(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
H5_DLL void *H5VL_file_open(H5VL_t **vol_plugin, const char *name, unsigned flags, hid_t fapl_id, hid_t req);
H5_DLL herr_t H5VL_file_flush(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5F_scope_t scope, hid_t req);
H5_DLL herr_t H5VL_file_misc(void *file, H5VL_t *vol_plugin, H5VL_file_misc_t misc_type, hid_t req, ...);
H5_DLL herr_t H5VL_file_optional(void *file, H5VL_t *vol_plugin, H5VL_file_optional_t optional_type, hid_t req, ...);
H5_DLL herr_t H5VL_file_get(void *file, H5VL_t *vol_plugin, H5VL_file_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_file_close(void *file, H5VL_t *vol_plugin, hid_t req);

H5_DLL void *H5VL_group_create(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
H5_DLL void *H5VL_group_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, const char *name, hid_t gapl_id, hid_t req);
H5_DLL herr_t H5VL_group_close(void *grp, H5VL_t *vol_plugin, hid_t req);
H5_DLL herr_t H5VL_group_get(void *obj, H5VL_t *vol_plugin, H5VL_group_get_t get_type, hid_t req, ...);

H5_DLL herr_t H5VL_link_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VL_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                             void *dst_obj, H5VL_loc_params_t loc_params2, H5VL_t *vol_plugin,
                             hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VL_link_iterate(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, 
                                hbool_t recursive, H5_index_t idx_type, H5_iter_order_t order, 
                                hsize_t *idx, H5L_iterate_t op, void *op_data, hid_t req);
H5_DLL herr_t H5VL_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_link_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_link_remove(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t req);

H5_DLL void *H5VL_object_open(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5I_type_t *opened_type, hid_t req);
H5_DLL herr_t H5VL_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, H5VL_t *vol_plugin1, const char *src_name, 
                               void *dst_obj, H5VL_loc_params_t loc_params2, H5VL_t *vol_plugin2, const char *dst_name, 
                               hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
H5_DLL herr_t H5VL_object_visit(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5_index_t idx_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
H5_DLL herr_t H5VL_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_object_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_object_misc_t misc_type, hid_t req, ...);
H5_DLL herr_t H5VL_object_optional(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, H5VL_object_misc_t optional_type, hid_t req, ...);
//H5_DLL herr_t H5VL_object_lookup(hid_t uid, H5VL_loc_type_t lookup_type, void **location, hid_t req, ...);
//H5_DLL herr_t H5VL_object_free_loc(hid_t loc_id, void *location, hid_t req);
H5_DLL herr_t H5VL_object_close(void *obj, H5VL_loc_params_t loc_params, H5VL_t *vol_plugin, hid_t req);

H5_DLL herr_t H5VL_fapl_open(struct H5P_genplist_t *plist, H5VL_class_t *vol_cls, const void *vol_info);
H5_DLL herr_t H5VL_fapl_copy(H5VL_class_t *vol_cls, const void *vol_info, void **copied_info);
H5_DLL herr_t H5VL_fapl_close(H5VL_class_t *vol_cls, void *vol_info);

H5_DLL herr_t H5F_close_file(void *file, H5VL_t *vol_plugin);
H5_DLL herr_t H5A_close_attr(void *attr, H5VL_t *vol_plugin);
H5_DLL herr_t H5D_close_dataset(void *dset, H5VL_t *vol_plugin);
H5_DLL herr_t H5G_close_group(void *grp, H5VL_t *vol_plugin);
H5_DLL herr_t H5T_close_datatype(void *dt, H5VL_t *vol_plugin);

/* Native routines that are useful in other library plugins */
H5_DLL H5F_t *H5VL_native_get_file(void *obj, H5I_type_t type);

/* encode & decode prototypes */
H5_DLL herr_t H5VL__encode_loc_params(H5VL_loc_params_t loc_params, void *buf, size_t *size);
H5_DLL herr_t H5VL__decode_loc_params(const void *buf, H5VL_loc_params_t *loc_params);

H5_DLL herr_t H5VL__encode_file_create_params(void *buf, size_t *nalloc, const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
H5_DLL herr_t H5VL__decode_file_create_params(void *buf, char **name, unsigned *flags, hid_t *fcpl_id, hid_t *fapl_id);
H5_DLL herr_t H5VL__encode_file_open_params(void *buf, size_t *nalloc, const char *name, unsigned flags, hid_t fapl_id);
H5_DLL herr_t H5VL__decode_file_open_params(void *buf, char **name, unsigned *flags, hid_t *fapl_id);
H5_DLL herr_t H5VL__encode_file_flush_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, H5F_scope_t scope);
H5_DLL herr_t H5VL__decode_file_flush_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, H5F_scope_t *scope);
H5_DLL herr_t H5VL__encode_file_close_params(void *buf, size_t *nalloc, hid_t obj_id);
H5_DLL herr_t H5VL__decode_file_close_params(void *buf, hid_t *obj_id);

H5_DLL herr_t H5VL__encode_attr_create_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t acpl_id, hid_t aapl_id, hid_t type_id, hid_t space_id);
H5_DLL herr_t H5VL__decode_attr_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *acpl_id, hid_t *aapl_id, hid_t *type_id, hid_t *space_id);
H5_DLL herr_t H5VL__encode_attr_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t aapl_id);
H5_DLL herr_t H5VL__decode_attr_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *aapl_id);
H5_DLL herr_t H5VL__encode_attr_read_params(void *buf, size_t *nalloc, hid_t obj_id, hid_t type_id, size_t buf_size);
H5_DLL herr_t H5VL__decode_attr_read_params(void *buf, hid_t *obj_id, hid_t *type_id, size_t *buf_size);
H5_DLL herr_t H5VL__encode_attr_write_params(void *buf, size_t *nalloc, hid_t obj_id, hid_t type_id, const void *attr_buf, size_t buf_size);
H5_DLL herr_t H5VL__decode_attr_write_params(void *buf, hid_t *obj_id, hid_t *type_id, void **attr_buf, size_t *buf_size);
H5_DLL herr_t H5VL__encode_attr_remove_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name);
H5_DLL herr_t H5VL__decode_attr_remove_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name);
H5_DLL herr_t H5VL__encode_attr_close_params(void *buf, size_t *nalloc, hid_t obj_id);
H5_DLL herr_t H5VL__decode_attr_close_params(void *buf, hid_t *obj_id);

H5_DLL herr_t H5VL__encode_dataset_create_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t type_id, hid_t space_id, hid_t lcpl_id);
H5_DLL herr_t H5VL__decode_dataset_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *dcpl_id, hid_t *dapl_id, hid_t *type_id, hid_t *space_id, hid_t *lcpl_id);
H5_DLL herr_t H5VL__encode_dataset_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id);
H5_DLL herr_t H5VL__decode_dataset_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *dapl_id);
H5_DLL herr_t H5VL__encode_dataset_set_extent_params(void *buf, size_t *nalloc, hid_t obj_id, int rank, const hsize_t *size);
H5_DLL herr_t H5VL__decode_dataset_set_extent_params(void *buf, hid_t *obj_id, int *rank, hsize_t **size);
H5_DLL herr_t H5VL__encode_dataset_close_params(void *buf, size_t *nalloc, hid_t obj_id);
H5_DLL herr_t H5VL__decode_dataset_close_params(void *buf, hid_t *obj_id);

H5_DLL herr_t H5VL__encode_datatype_commit_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id);
H5_DLL herr_t H5VL__decode_datatype_commit_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *type_id, hid_t *lcpl_id, hid_t *tcpl_id, hid_t *tapl_id);
H5_DLL herr_t H5VL__encode_datatype_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id);
H5_DLL herr_t H5VL__decode_datatype_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *tapl_id);
H5_DLL herr_t H5VL__encode_datatype_close_params(void *buf, size_t *nalloc, hid_t obj_id);
H5_DLL herr_t H5VL__decode_datatype_close_params(void *buf, hid_t *obj_id);

H5_DLL herr_t H5VL__encode_group_create_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t lcpl_id);
H5_DLL herr_t H5VL__decode_group_create_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *gcpl_id, hid_t *gapl_id, hid_t *lcpl_id);
H5_DLL herr_t H5VL__encode_group_open_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id);
H5_DLL herr_t H5VL__decode_group_open_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params, char **name, hid_t *gapl_id);
H5_DLL herr_t H5VL__encode_group_close_params(void *buf, size_t *nalloc, hid_t obj_id);
H5_DLL herr_t H5VL__decode_group_close_params(void *buf, hid_t *obj_id);

H5_DLL herr_t H5VL__encode_link_create_params(void *buf, size_t *nalloc, H5VL_link_create_type_t create_type, hid_t obj_id, H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, ...);
H5_DLL herr_t H5VL__decode_link_create_params(void *buf, H5VL_link_create_type_t *create_type, hid_t *obj_id, H5VL_loc_params_t *loc_params, hid_t *lcpl_id, hid_t *lapl_id);
H5_DLL herr_t H5VL__encode_link_move_params(void *buf, size_t *nalloc, hid_t src_id, H5VL_loc_params_t loc_params1, hid_t dst_id, H5VL_loc_params_t loc_params2, hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id);
H5_DLL herr_t H5VL__decode_link_move_params(void *buf, hid_t *src_id, H5VL_loc_params_t *loc_params1, hid_t *dst_id, H5VL_loc_params_t *loc_params2, hbool_t *copy_flag, hid_t *lcpl_id, hid_t *lapl_id);
H5_DLL herr_t H5VL__encode_link_remove_params(void *buf, size_t *nalloc, hid_t obj_id, H5VL_loc_params_t loc_params);
H5_DLL herr_t H5VL__decode_link_remove_params(void *buf, hid_t *obj_id, H5VL_loc_params_t *loc_params);

/* Atrribute callbacks */
void *H5VL_native_attr_create(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
void *H5VL_native_attr_open(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
herr_t H5VL_native_attr_read(void *attr, hid_t dtype_id, void *buf, hid_t req);
herr_t H5VL_native_attr_write(void *attr, hid_t dtype_id, const void *buf, hid_t req);
herr_t H5VL_native_attr_get(void *obj, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_attr_remove(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
herr_t H5VL_native_attr_close(void *attr, hid_t req);

/* Datatype callbacks */
void *H5VL_native_datatype_commit(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
void *H5VL_native_datatype_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t tapl_id, hid_t req);
ssize_t H5VL_native_datatype_get_binary(void *obj, unsigned char *buf, size_t size, hid_t req);
herr_t H5VL_native_datatype_close(void *dt, hid_t req);

/* Dataset callbacks */
void *H5VL_native_dataset_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
void *H5VL_native_dataset_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
herr_t H5VL_native_dataset_read(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                       hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
herr_t H5VL_native_dataset_write(void *dset, hid_t mem_type_id, hid_t mem_space_id,
                                        hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
herr_t H5VL_native_dataset_set_extent(void *dset, const hsize_t size[], hid_t req);
herr_t H5VL_native_dataset_get(void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_dataset_close(void *dset, hid_t req);

/* File callbacks */
void *H5VL_native_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
void *H5VL_native_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
herr_t H5VL_native_file_flush(void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
herr_t H5VL_native_file_get(void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_file_misc(void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
herr_t H5VL_native_file_optional(void *file, H5VL_file_optional_t optional_type, hid_t req, va_list arguments);
herr_t H5VL_native_file_close(void *file, hid_t req);

/* Group callbacks */
void *H5VL_native_group_create(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
void *H5VL_native_group_open(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
herr_t H5VL_native_group_get(void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_group_close(void *grp, hid_t req);

/* Link callbacks */
herr_t H5VL_native_link_create(H5VL_link_create_type_t create_type, void *obj, 
                                      H5VL_loc_params_t loc_params, hid_t lcpl_id, hid_t lapl_id, hid_t req);
herr_t H5VL_native_link_move(void *src_obj, H5VL_loc_params_t loc_params1,
                                    void *dst_obj, H5VL_loc_params_t loc_params2,
                                    hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
herr_t H5VL_native_link_iterate(void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                                       H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                                       H5L_iterate_t op, void *op_data, hid_t req);
herr_t H5VL_native_link_get(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_link_remove(void *obj, H5VL_loc_params_t loc_params, hid_t req);

/* Object callbacks */
void *H5VL_native_object_open(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t req);
herr_t H5VL_native_object_copy(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                                      void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name, 
                                      hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
herr_t H5VL_native_object_visit(void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type, 
                                       H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
//herr_t H5VL_native_object_lookup(hid_t loc_id, H5VL_loc_type_t lookup_type, void **location, hid_t req, va_list arguments);
//herr_t H5VL_native_object_free_loc(void *location, hid_t req);
herr_t H5VL_native_object_get(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, va_list arguments);
herr_t H5VL_native_object_misc(void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, va_list arguments);
herr_t H5VL_native_object_optional(void *obj, H5VL_loc_params_t loc_params, H5VL_object_optional_t optional_type, hid_t req, va_list arguments);
herr_t H5VL_native_object_close(void *obj, H5VL_loc_params_t loc_params, hid_t req);

#endif /* _H5VLprivate_H */
