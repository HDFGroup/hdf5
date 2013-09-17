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

#include "H5public.h"
#include "H5FFpublic.h"
#include "H5VLpublic.h"
#include "H5VLiod.h"            /* Iod VOL plugin			*/

#ifdef H5_HAVE_EFF

#define NA_UNDEFINED NULL
#define IOD_OH_UNDEFINED ((int)(-128))//(pow(2.0,64.0) - 1)
#define IOD_ID_UNDEFINED ((uint64_t)(-1))//(pow(2.0,64.0) - 1)
#define IOD_COUNT_UNDEFINED ((uint64_t)(-1))//(pow(2.0,64.0) - 1)
#define H5VL_IOD_DEBUG 1

typedef enum H5VL_iod_state_t {
    H5VL_IOD_PENDING,
    H5VL_IOD_COMPLETED,
    H5VL_IOD_CANCELLED
} H5VL_iod_state_t;

typedef struct H5VL_iod_read_status_t {
    int ret;
    uint32_t cs;
    size_t buf_size;
} H5VL_iod_read_status_t;

typedef struct dims_t {
    int rank;
    hsize_t *size;
} dims_t;

typedef struct name_t {
    size_t size;
    ssize_t *value_size;
    char *value;
} name_t;

typedef struct binary_buf_t {
    size_t buf_size;
    void *buf;
} binary_buf_t;

typedef struct value_t {
    size_t val_size;
    void *val;
} value_t;

typedef struct axe_t {
    AXE_task_t axe_id;
    AXE_task_t start_range;
    size_t count;
    size_t num_parents;
    AXE_task_t *parent_axe_ids;
} axe_t;

H5_DLL int hg_proc_ret_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_axe_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_size_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_hid_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_htri_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_hbool_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_obj_id_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_handle_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dims_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_axe_ids_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_name_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_value_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_binary_buf_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_linfo_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_oinfo_t(hg_proc_t proc, void *data);

MERCURY_GEN_PROC(eff_init_in_t, ((uint32_t)(proc_num)))

MERCURY_GEN_PROC(file_create_in_t, ((axe_t)(axe_info)) 
                 ((hg_const_string_t)(name)) ((uint32_t)(flags)) 
                 ((iod_obj_id_t)(root_id)) ((uint32_t)(num_peers))
                 ((hid_t)(fapl_id)) ((hid_t)(fcpl_id)))
MERCURY_GEN_PROC(file_create_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((uint64_t)(kv_oid_index)) ((uint64_t)(array_oid_index)) 
                 ((uint64_t)(blob_oid_index)))
MERCURY_GEN_PROC(file_open_in_t, ((axe_t)(axe_info)) 
                 ((hg_const_string_t)(name)) ((uint32_t)(flags)) 
                 ((hbool_t)(acquire)) ((hid_t)(fapl_id)))
MERCURY_GEN_PROC(file_open_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((uint64_t)(kv_oid_index)) ((uint64_t)(array_oid_index)) 
                 ((uint64_t)(blob_oid_index))
                 ((iod_obj_id_t)(root_id)) ((uint64_t)(c_version))
                 ((hid_t)(fcpl_id)))
MERCURY_GEN_PROC(file_close_in_t, ((axe_t)(axe_info)) 
                 ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((iod_obj_id_t)(root_id)))

MERCURY_GEN_PROC(attr_create_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(attr_id))
                 ((hg_const_string_t)(path))
                 ((hg_const_string_t)(attr_name)) ((hid_t)(acpl_id)) 
                 ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(attr_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(attr_open_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_const_string_t)(path)) 
                 ((hg_const_string_t)(attr_name)))
MERCURY_GEN_PROC(attr_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(acpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(attr_op_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(path)) ((hg_const_string_t)(attr_name)))
MERCURY_GEN_PROC(attr_rename_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(path)) ((hg_const_string_t)(old_attr_name)) 
                 ((hg_const_string_t)(new_attr_name)))
MERCURY_GEN_PROC(attr_io_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((hid_t)(space_id))
                 ((hid_t)(type_id)) ((hg_bulk_t)(bulk_handle)))
MERCURY_GEN_PROC(attr_close_in_t, ((axe_t)(axe_info)) 
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)))

MERCURY_GEN_PROC(group_create_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(grp_id))
                 ((hg_const_string_t)(name)) ((hid_t)(gapl_id)) 
                 ((hid_t)(gcpl_id)) ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(group_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(group_open_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((hg_const_string_t)(name))
                 ((hid_t)(gapl_id)))
MERCURY_GEN_PROC(group_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(gcpl_id)))
MERCURY_GEN_PROC(group_close_in_t, ((axe_t)(axe_info)) 
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)))

MERCURY_GEN_PROC(map_create_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(map_id))
                 ((hg_const_string_t)(name)) 
                 ((hid_t)(keytype_id)) ((hid_t)(valtype_id)) 
                 ((hid_t)(mapl_id)) ((hid_t)(mcpl_id)) 
                 ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(map_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(map_open_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(name)) ((hid_t)(mapl_id)))
MERCURY_GEN_PROC(map_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(keytype_id)) ((hid_t)(valtype_id)) ((hid_t)(mcpl_id)))
MERCURY_GEN_PROC(map_set_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh))
                 ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(key_maptype_id)) ((hid_t)(key_memtype_id)) ((binary_buf_t)(key))
                 ((hid_t)(val_maptype_id)) ((hid_t)(val_memtype_id)) ((binary_buf_t)(val))
                 ((hid_t)(dxpl_id)))
MERCURY_GEN_PROC(map_get_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh))
                 ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(key_maptype_id)) ((hid_t)(key_memtype_id)) ((binary_buf_t)(key))
                 ((hid_t)(val_maptype_id)) ((hid_t)(val_memtype_id)) ((hbool_t)(val_is_vl))
                 ((size_t)(val_size)) ((hid_t)(dxpl_id)))
MERCURY_GEN_PROC(map_get_out_t, ((int32_t)(ret)) ((size_t)(val_size)) ((value_t)(val)))
MERCURY_GEN_PROC(map_get_count_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh))
                 ((iod_obj_id_t)(iod_id)))
MERCURY_GEN_PROC(map_op_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh))
                 ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(key_maptype_id)) ((hid_t)(key_memtype_id)) ((binary_buf_t)(key)))
MERCURY_GEN_PROC(map_close_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)))

MERCURY_GEN_PROC(dset_create_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(dset_id)) 
                 ((hg_const_string_t)(name))
                 ((hid_t)(dapl_id)) ((hid_t)(dcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(dset_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(dset_open_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(name)) ((hid_t)(dapl_id)))
MERCURY_GEN_PROC(dset_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(dcpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(dset_set_extent_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((dims_t)(dims)))
MERCURY_GEN_PROC(dset_io_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id))
                 ((hid_t)(dset_type_id)) ((hid_t)(mem_type_id))
                 ((hid_t)(space_id)) ((hid_t)(dxpl_id)) ((uint32_t)(checksum))
                 ((hg_bulk_t)(bulk_handle)))
MERCURY_GEN_PROC(dset_get_vl_size_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((hid_t)(mem_type_id))
                 ((hid_t)(space_id)) ((hid_t)(dxpl_id)))
MERCURY_GEN_PROC(dset_read_out_t, ((int32_t)(ret)) ((uint32_t)(cs)) ((size_t)(buf_size)))
MERCURY_GEN_PROC(dset_close_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)))

MERCURY_GEN_PROC(dtype_commit_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(dtype_id)) 
                 ((hg_const_string_t)(name))
                 ((hid_t)(tapl_id)) ((hid_t)(tcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)))
MERCURY_GEN_PROC(dtype_commit_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(dtype_open_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(name)) ((hid_t)(tapl_id)))
MERCURY_GEN_PROC(dtype_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(tcpl_id)) ((hid_t)(type_id)))
MERCURY_GEN_PROC(dtype_close_in_t, ((axe_t)(axe_info)) 
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)))

MERCURY_GEN_PROC(link_create_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((int8_t)(create_type)) ((iod_handle_t)(coh)) 
                 ((iod_handle_t)(loc_oh)) ((iod_obj_id_t)(loc_id))
                 ((hg_const_string_t)(loc_name))
                 ((iod_handle_t)(target_loc_oh)) ((iod_obj_id_t)(target_loc_id))
                 ((hg_const_string_t)(target_name))
                 ((hg_const_string_t)(link_value))
                 ((hid_t)(lapl_id)) ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(link_move_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((hbool_t)(copy_flag)) ((iod_handle_t)(coh)) 
                 ((iod_handle_t)(src_loc_oh)) ((iod_obj_id_t)(src_loc_id))
                 ((hg_const_string_t)(src_loc_name))
                 ((iod_handle_t)(dst_loc_oh)) ((iod_obj_id_t)(dst_loc_id))
                 ((hg_const_string_t)(dst_loc_name))
                 ((hid_t)(lapl_id)) ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(link_op_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh)) 
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(path)))
MERCURY_GEN_PROC(link_get_val_in_t, ((axe_t)(axe_info))
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh)) 
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(path)) ((uint64_t)(length)))
MERCURY_GEN_PROC(link_get_val_out_t, ((int32_t)(ret)) ((value_t)(value)))

MERCURY_GEN_PROC(object_op_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh)) 
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(loc_name)))
MERCURY_GEN_PROC(object_open_out_t, ((int32_t)(obj_type))
                 ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(cpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(object_copy_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) 
                 ((iod_handle_t)(src_loc_oh)) ((iod_obj_id_t)(src_loc_id))
                 ((hg_const_string_t)(src_loc_name))
                 ((iod_handle_t)(dst_loc_oh)) ((iod_obj_id_t)(dst_loc_id))
                 ((hg_const_string_t)(dst_loc_name))
                 ((hid_t)(ocpypl_id)) ((hid_t)(lcpl_id)))
MERCURY_GEN_PROC(object_set_comment_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num)) ((uint64_t)(trans_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh)) 
                 ((iod_obj_id_t)(loc_id)) 
                 ((hg_const_string_t)(path)) ((hg_const_string_t)(comment)))
MERCURY_GEN_PROC(object_get_comment_in_t, ((axe_t)(axe_info)) 
                 ((uint64_t)(rcxt_num))
                 ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh)) 
                 ((iod_obj_id_t)(loc_id))
                 ((hg_const_string_t)(path)) ((uint64_t)(length)))
MERCURY_GEN_PROC(object_get_comment_out_t, ((int32_t)(ret)) ((name_t)(name)))

MERCURY_GEN_PROC(rc_acquire_in_t, ((axe_t)(axe_info)) 
                 ((iod_handle_t)(coh)) ((uint64_t)(c_version))
                 ((hid_t)(rcapl_id)))
MERCURY_GEN_PROC(rc_acquire_out_t, ((int32_t)(ret)) ((uint64_t)(c_version)))
MERCURY_GEN_PROC(rc_release_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(c_version)))
MERCURY_GEN_PROC(rc_persist_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(c_version)))
MERCURY_GEN_PROC(rc_snapshot_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((hg_const_string_t)(snapshot_name)) 
                 ((uint64_t)(c_version)))

MERCURY_GEN_PROC(tr_start_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(trans_num))
                 ((hid_t)(trspl_id)))
MERCURY_GEN_PROC(tr_finish_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(trans_num))
                 ((hbool_t)(acquire)) ((hid_t)(trfpl_id)))
MERCURY_GEN_PROC(tr_set_depend_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(child_trans_num))
                 ((uint64_t)(parent_trans_num)))
MERCURY_GEN_PROC(tr_skip_in_t, ((axe_t)(axe_info)) ((iod_handle_t)(coh)) 
                 ((uint64_t)(start_trans_num)) ((uint64_t)(count)))
MERCURY_GEN_PROC(tr_abort_in_t, ((axe_t)(axe_info))
                 ((iod_handle_t)(coh)) ((uint64_t)(trans_num)))

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_common_H */
