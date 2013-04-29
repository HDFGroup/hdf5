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

#include "H5Pprivate.h"		/* Property lists			*/
#include "H5VLpublic.h"
#include "H5VLiod.h"            /* Iod VOL plugin			*/

#ifdef H5_HAVE_EFF

#define NA_UNDEFINED NULL

/* struct that contains the information about the IOD container */
typedef struct H5VL_iod_remote_file_t {
    iod_handle_t coh;
    iod_handle_t root_oh;
    iod_obj_id_t root_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t fcpl_id;
    hg_handle_t hg_handle;
} H5VL_iod_remote_file_t;

/* struct that contains the information about the IOD attr */
typedef struct H5VL_iod_remote_attr_t {
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t acpl_id;
    hid_t space_id;
    hid_t type_id;
    hg_handle_t hg_handle;
} H5VL_iod_remote_attr_t;

/* struct that contains the information about the IOD group */
typedef struct H5VL_iod_remote_group_t {
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t gcpl_id;
    hg_handle_t hg_handle;
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
    hg_handle_t hg_handle;
} H5VL_iod_remote_dset_t;

/* struct that contains the information about the IOD dtype */
typedef struct H5VL_iod_remote_dtype_t {
    iod_handle_t iod_oh;
    iod_obj_id_t iod_id;
    iod_handle_t scratch_oh;
    iod_obj_id_t scratch_id;
    hid_t tcpl_id;
    hid_t type_id;
    hg_handle_t hg_handle;
} H5VL_iod_remote_dtype_t;

typedef struct H5VL_iod_file_create_input_t {
    const char *name;
    unsigned flags;
    hid_t fcpl_id;
    hid_t fapl_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_create_input_t;

typedef struct H5VL_iod_file_open_input_t {
    const char *name;
    unsigned flags;
    hid_t fapl_id;
    hg_handle_t hg_handle;
} H5VL_iod_file_open_input_t;

typedef struct H5VL_iod_file_flush_input_t {
    iod_handle_t coh;
    H5F_scope_t scope;
    hg_handle_t hg_handle;
} H5VL_iod_file_flush_input_t;

typedef struct H5VL_iod_attr_create_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    hid_t acpl_id;
    hid_t type_id;
    hid_t space_id;
    hg_handle_t hg_handle;
} H5VL_iod_attr_create_input_t;

typedef struct H5VL_iod_attr_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    hg_handle_t hg_handle;
} H5VL_iod_attr_open_input_t;

typedef struct H5VL_iod_attr_io_input_t {
    iod_handle_t iod_oh;
    iod_handle_t scratch_oh;
    hid_t type_id;
    hg_bulk_t bulk_handle;
    hg_handle_t hg_handle;
} H5VL_iod_attr_io_input_t;

typedef struct H5VL_iod_attr_op_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *path;
    const char *attr_name;
    hg_handle_t hg_handle;
} H5VL_iod_attr_op_input_t;

typedef struct H5VL_iod_group_create_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gcpl_id;
    hid_t gapl_id;
    hid_t lcpl_id;
    hg_handle_t hg_handle;
} H5VL_iod_group_create_input_t;

typedef struct H5VL_iod_group_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t gapl_id;
    hg_handle_t hg_handle;
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
    hg_handle_t hg_handle;
} H5VL_iod_dset_create_input_t;

typedef struct H5VL_iod_dset_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t dapl_id;
    hg_handle_t hg_handle;
} H5VL_iod_dset_open_input_t;

typedef struct H5VL_iod_dset_io_input_t {
    iod_handle_t iod_oh;
    iod_handle_t scratch_oh;
    hid_t space_id;
    hid_t dxpl_id;
    uint32_t checksum;
    hg_bulk_t bulk_handle;
    hg_handle_t hg_handle;
} H5VL_iod_dset_io_input_t;

typedef struct dims_t {
    int rank;
    hsize_t *size;
} dims_t;

typedef struct H5VL_iod_dset_set_extent_input_t {
    iod_handle_t iod_oh;
    //int rank;
    //hsize_t *size;
    dims_t dims;
    hg_handle_t hg_handle;
} H5VL_iod_dset_set_extent_input_t;

typedef struct H5VL_iod_read_status_t {
    int ret;
    uint32_t cs;
} H5VL_iod_read_status_t;

typedef struct H5VL_iod_dtype_commit_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t tcpl_id;
    hid_t tapl_id;
    hid_t lcpl_id;
    hid_t type_id;
    hg_handle_t hg_handle;
} H5VL_iod_dtype_commit_input_t;

typedef struct H5VL_iod_dtype_open_input_t {
    iod_handle_t coh;
    iod_handle_t loc_oh;
    iod_obj_id_t loc_id;
    const char *name;
    hid_t tapl_id;
    hg_handle_t hg_handle;
} H5VL_iod_dtype_open_input_t;

H5_DLL int hg_proc_ret_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_hid_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_htri_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_obj_id_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_handle_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dims_t(hg_proc_t proc, void *data);


MERCURY_GEN_PROC(eff_init_in_t, ((uint32_t)(proc_num)))

MERCURY_GEN_PROC(file_create_in_t, ((hg_string_t)(name)) ((uint32_t)(flags)) 
                 ((hid_t)(fapl_id)) ((hid_t)(fcpl_id)))
MERCURY_GEN_PROC(file_create_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((iod_obj_id_t)(root_id)) ((iod_handle_t)(scratch_oh)) 
                 ((iod_obj_id_t)(scratch_id)))
MERCURY_GEN_PROC(file_open_in_t, ((hg_string_t)(name)) ((uint32_t)(flags)) 
                 ((hid_t)(fapl_id)))
MERCURY_GEN_PROC(file_open_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((iod_obj_id_t)(root_id)) ((iod_handle_t)(scratch_oh)) 
                 ((iod_obj_id_t)(scratch_id)) ((hid_t)(fcpl_id)))
MERCURY_GEN_PROC(file_flush_in_t, ((int32_t)(scope)) ((iod_handle_t)(coh)))
MERCURY_GEN_PROC(file_close_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((iod_obj_id_t)(root_id)) ((iod_handle_t)(scratch_oh)) 
                 ((iod_obj_id_t)(scratch_id)))

MERCURY_GEN_PROC(attr_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(path))
                 ((hg_string_t)(attr_name)) ((hid_t)(acpl_id)) 
                 ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(attr_create_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))
MERCURY_GEN_PROC(attr_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(path)) 
                 ((hg_string_t)(attr_name)))
MERCURY_GEN_PROC(attr_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id))
                 ((hid_t)(acpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(attr_op_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(path)) ((hg_string_t)(attr_name)))
MERCURY_GEN_PROC(attr_io_in_t, ((iod_handle_t)(iod_oh)) ((iod_handle_t)(scratch_oh)) 
                 ((hid_t)(type_id)) ((hg_bulk_t)(bulk_handle)))
MERCURY_GEN_PROC(attr_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))

MERCURY_GEN_PROC(group_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(gapl_id)) ((hid_t)(gcpl_id)) ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(group_create_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))
MERCURY_GEN_PROC(group_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(gapl_id)))
MERCURY_GEN_PROC(group_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id))
                 ((hid_t)(gcpl_id)))
MERCURY_GEN_PROC(group_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))

MERCURY_GEN_PROC(dset_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(dapl_id)) ((hid_t)(dcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(dset_create_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))
MERCURY_GEN_PROC(dset_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(dapl_id)))
MERCURY_GEN_PROC(dset_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id))
                 ((hid_t)(dcpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(dset_set_extent_in_t, ((iod_handle_t)(iod_oh)) ((dims_t)(dims)))
MERCURY_GEN_PROC(dset_io_in_t, ((iod_handle_t)(iod_oh)) ((iod_handle_t)(scratch_oh)) 
                 ((hid_t)(space_id)) ((hid_t)(dxpl_id)) ((uint32_t)(checksum))
                 ((hg_bulk_t)(bulk_handle)))
MERCURY_GEN_PROC(dset_read_out_t, ((int32_t)(ret)) ((uint32_t)(cs)))
MERCURY_GEN_PROC(dset_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))

MERCURY_GEN_PROC(dtype_commit_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(tapl_id)) ((hid_t)(tcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)))
MERCURY_GEN_PROC(dtype_commit_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))
MERCURY_GEN_PROC(dtype_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_string_t)(name))
                 ((hid_t)(tapl_id)))
MERCURY_GEN_PROC(dtype_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id))
                 ((hid_t)(tcpl_id)) ((hid_t)(type_id)))
MERCURY_GEN_PROC(dtype_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((iod_handle_t)(scratch_oh)) ((iod_obj_id_t)(scratch_id)))

#if 0
H5_DLL int hg_proc_eff_init_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_eff_fin_in_t(hg_proc_t proc, void *data);

H5_DLL int hg_proc_file_create_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_file_create_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_file_open_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_file_open_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_file_flush_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_file_close_in_t(hg_proc_t proc, void *data);

H5_DLL int hg_proc_group_create_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_group_create_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_group_open_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_group_open_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_group_close_in_t(hg_proc_t proc, void *data);

H5_DLL int hg_proc_dset_create_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_create_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_open_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_open_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_set_extent_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_io_in_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_read_out_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dset_close_in_t(hg_proc_t proc, void *data);
#endif

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_common_H */
