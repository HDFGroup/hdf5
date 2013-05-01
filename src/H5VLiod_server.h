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
 * This file contains private information about the H5VL iod server module
 */
#ifndef _H5VLiod_server_H
#define _H5VLiod_server_H

#include "H5VLiod_common.h"

#ifdef H5_HAVE_EFF

typedef struct H5VL_iod_file_create_input_t {
    /* Do NOT change the order of the parameters */
    const char *name;
    unsigned flags;
    hid_t fapl_id;
    hid_t fcpl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_create_input_t;

typedef struct H5VL_iod_file_open_input_t {
    /* Do NOT change the order of the parameters */
    const char *name;
    unsigned flags;
    hid_t fapl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_open_input_t;

typedef struct H5VL_iod_file_flush_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    H5F_scope_t scope;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_flush_input_t;

typedef struct H5VL_iod_file_close_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t root_oh;
    iod_obj_id_t root_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_close_input_t;

typedef struct H5VL_iod_attr_create_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    hid_t acpl_id;
    hid_t type_id;
    hid_t space_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_create_input_t;

typedef struct H5VL_iod_attr_open_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_open_input_t;

typedef struct H5VL_iod_attr_io_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_handle_t scratch_oh;
    hid_t type_id;
    hg_bulk_t bulk_handle;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_io_input_t;

typedef struct H5VL_iod_attr_op_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_op_input_t;

typedef struct H5VL_iod_attr_close_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_close_input_t;

typedef struct H5VL_iod_group_create_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gapl_id;
    hid_t gcpl_id;
    hid_t lcpl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_group_create_input_t;

typedef struct H5VL_iod_group_open_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gapl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_group_open_input_t;

typedef struct H5VL_iod_group_close_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_group_close_input_t;

typedef struct H5VL_iod_dset_create_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t dapl_id;
    hid_t dcpl_id;
    hid_t lcpl_id;
    hid_t type_id;
    hid_t space_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_create_input_t;

typedef struct H5VL_iod_dset_open_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t dapl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_open_input_t;

typedef struct H5VL_iod_dset_io_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_handle_t scratch_oh;
    hid_t space_id;
    hid_t dxpl_id;
    uint32_t checksum;
    hg_bulk_t bulk_handle;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_io_input_t;

typedef struct H5VL_iod_dset_set_extent_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    struct dims_t dims;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_set_extent_input_t;

typedef struct H5VL_iod_dset_close_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_close_input_t;

typedef struct H5VL_iod_dtype_commit_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t tapl_id;
    hid_t tcpl_id;
    hid_t lcpl_id;
    hid_t type_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dtype_commit_input_t;

typedef struct H5VL_iod_dtype_open_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t tapl_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dtype_open_input_t;

typedef struct H5VL_iod_dtype_close_input_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    AXE_task_t axe_id;
    hg_handle_t hg_handle;
} H5VL_iod_dtype_close_input_t;

H5_DLL int H5VL_iod_server_eff_init(hg_handle_t handle);
H5_DLL int H5VL_iod_server_eff_finalize(hg_handle_t handle);
H5_DLL int H5VL_iod_server_file_create(hg_handle_t handle);
H5_DLL int H5VL_iod_server_file_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_file_flush(hg_handle_t handle);
H5_DLL int H5VL_iod_server_file_close(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_create(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_read(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_write(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_exists(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_remove(hg_handle_t handle);
H5_DLL int H5VL_iod_server_attr_close(hg_handle_t handle);
H5_DLL int H5VL_iod_server_group_create(hg_handle_t handle);
H5_DLL int H5VL_iod_server_group_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_group_close(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_create(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_read(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_write(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_set_extent(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dset_close(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dtype_commit(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dtype_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_dtype_close(hg_handle_t handle);

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_server_H */
