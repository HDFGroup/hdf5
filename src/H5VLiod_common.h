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
 * This file contains private information about the H5VL module
 */
#ifndef _H5VLiod_common_H
#define _H5VLiod_common_H

#include "H5VLpublic.h"
#include "H5VLiod.h"         /* Iod VOL plugin			*/

/* types for different ways that objects are located in an HDF5 container */
typedef enum H5VL_function_type_t {
    H5VL_FILE_CREATE_ID,
    H5VL_FILE_OPEN_ID,
    H5VL_FILE_CLOSE_ID,
    H5VL_GROUP_CREATE_ID,
    H5VL_GROUP_OPEN_ID,
    H5VL_GROUP_CLOSE_ID,
    H5VL_DSET_CREATE_ID,
    H5VL_DSET_OPEN_ID,
    H5VL_DSET_CLOSE_ID
} H5VL_function_type_t;

/* struct that contains the information about the IOD container */
typedef struct H5VL_iod_remote_file_t {
    iod_handle_t coh;
    iod_handle_t root_oh;
    iod_obj_id_t root_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t fcpl_id;
} H5VL_iod_remote_file_t;

/* struct that contains the information about the IOD group */
typedef struct H5VL_iod_remote_group_t {
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t gcpl_id;
} H5VL_iod_remote_group_t;

/* struct that contains the information about the IOD dset */
typedef struct H5VL_iod_remote_dset_t {
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t dcpl_id;
    hid_t space_id;
    hid_t type_id;
} H5VL_iod_remote_dset_t;

typedef struct H5VL_iod_file_create_input_t {
    const char *name;
    unsigned flags;
    hid_t fcpl_id;
    hid_t fapl_id;
    fs_handle_t fs_handle;
} H5VL_iod_file_create_input_t;

typedef struct H5VL_iod_file_open_input_t {
    const char *name;
    unsigned flags;
    hid_t fapl_id;
    fs_handle_t fs_handle;
} H5VL_iod_file_open_input_t;

typedef struct H5VL_iod_group_create_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gcpl_id;
    hid_t gapl_id;
    hid_t lcpl_id;
    fs_handle_t fs_handle;
} H5VL_iod_group_create_input_t;

typedef struct H5VL_iod_group_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gapl_id;
    fs_handle_t fs_handle;
} H5VL_iod_group_open_input_t;

typedef struct H5VL_iod_dset_create_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t dcpl_id;
    hid_t dapl_id;
    hid_t lcpl_id;
    hid_t type_id;
    hid_t space_id;
    fs_handle_t fs_handle;
} H5VL_iod_dset_create_input_t;

typedef struct H5VL_iod_dset_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t dapl_id;
    fs_handle_t fs_handle;
} H5VL_iod_dset_open_input_t;

/* Define fs_proc_iod_handle_t */
static inline int fs_proc_iod_handle_t(fs_proc_t proc, void *data)
{
    int ret = S_SUCCESS;
    iod_handle_t *struct_data = (iod_handle_t *)data;

    ret = fs_proc_uint64_t(proc, &struct_data->cookie);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    return ret;
}

/* Define fs_proc_iod_obj_id_t */
static inline int fs_proc_iod_obj_id_t(fs_proc_t proc, void *data)
{
    int ret = S_SUCCESS;
    iod_obj_id_t *struct_data = (iod_obj_id_t *)data;

    ret = fs_proc_uint64_t(proc, &struct_data->oid_hi);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }

    ret = fs_proc_uint64_t(proc, &struct_data->oid_lo);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }

    return ret;
}

static inline int fs_proc_remote_file_t(fs_proc_t proc, void *data)
{
    int ret = S_SUCCESS;
    H5VL_iod_remote_file_t *struct_data = (H5VL_iod_remote_file_t *) data;

    ret = fs_proc_iod_handle_t(proc, &struct_data->coh);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_handle_t(proc, &struct_data->root_oh);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_obj_id_t(proc, &struct_data->root_id);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    return ret;
}

static inline int fs_proc_remote_object_t(fs_proc_t proc, void *data)
{
    int ret = S_SUCCESS;
    H5VL_iod_remote_group_t *struct_data = (H5VL_iod_remote_group_t *) data;

    ret = fs_proc_iod_handle_t(proc, &struct_data->iod_oh);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_handle_t(proc, &struct_data->iod_id);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_handle_t(proc, &struct_data->scratch_oh);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    ret = fs_proc_iod_obj_id_t(proc, &struct_data->scratch_id);
    if (ret != S_SUCCESS) {
        S_ERROR_DEFAULT("Proc error");
        ret = S_FAIL;
        return ret;
    }
    return ret;
}


#endif /* _H5VLiod_common_H */
