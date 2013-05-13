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
#define IOD_OH_UNDEFINED (pow(2,64) - 1)
#define IOD_ID_UNDEFINED (pow(2,64) - 1)
#define H5VL_IOD_DEBUG 1

typedef enum H5VL_iod_state_t {
    H5VL_IOD_PENDING,
    H5VL_IOD_COMPLETED,
    H5VL_IOD_CANCELLED
} H5VL_iod_state_t;

typedef struct H5VL_iod_read_status_t {
    int ret;
    uint32_t cs;
} H5VL_iod_read_status_t;

typedef struct dims_t {
    int rank;
    hsize_t *size;
} dims_t;

H5_DLL int hg_proc_ret_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_hid_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_htri_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_obj_id_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_iod_handle_t(hg_proc_t proc, void *data);
H5_DLL int hg_proc_dims_t(hg_proc_t proc, void *data);


MERCURY_GEN_PROC(eff_init_in_t, ((uint32_t)(proc_num)))

MERCURY_GEN_PROC(file_create_in_t, ((hg_string_t)(name)) ((uint32_t)(flags)) 
                 ((iod_obj_id_t)(root_id))
                 ((hid_t)(fapl_id)) ((hid_t)(fcpl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(file_create_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((uint64_t)(kv_oid_index)) ((uint64_t)(array_oid_index)) 
                 ((uint64_t)(blob_oid_index)))
MERCURY_GEN_PROC(file_open_in_t, ((hg_string_t)(name)) ((uint32_t)(flags)) 
                 ((hid_t)(fapl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(file_open_out_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((uint64_t)(kv_oid_index)) ((uint64_t)(array_oid_index)) 
                 ((uint64_t)(blob_oid_index))
                 ((iod_obj_id_t)(root_id)) ((hid_t)(fcpl_id)))
MERCURY_GEN_PROC(file_flush_in_t, ((iod_handle_t)(coh)) ((int32_t)(scope))
                 ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(file_close_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(root_oh))
                 ((iod_obj_id_t)(root_id)) ((uint64_t)(axe_id)))

MERCURY_GEN_PROC(attr_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(attr_id))
                 ((uint64_t)(parent_axe_id)) ((hg_string_t)(path))
                 ((hg_string_t)(attr_name)) ((hid_t)(acpl_id)) 
                 ((hid_t)(type_id)) ((hid_t)(space_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(attr_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(attr_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((uint64_t)(parent_axe_id)) ((hg_string_t)(path)) 
                 ((hg_string_t)(attr_name)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(attr_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(acpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(attr_op_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((uint64_t)(parent_axe_id)) 
                 ((hg_string_t)(path)) ((hg_string_t)(attr_name)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(attr_io_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((uint64_t)(parent_axe_id))
                 ((hid_t)(type_id)) ((hg_bulk_t)(bulk_handle)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(attr_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((uint64_t)(parent_axe_id)) ((uint64_t)(axe_id)))

MERCURY_GEN_PROC(group_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(grp_id))
                 ((uint64_t)(parent_axe_id)) ((hg_string_t)(name)) ((hid_t)(gapl_id)) 
                 ((hid_t)(gcpl_id)) ((hid_t)(lcpl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(group_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(group_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((uint64_t)(parent_axe_id)) ((hg_string_t)(name))
                 ((hid_t)(gapl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(group_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(gcpl_id)))
MERCURY_GEN_PROC(group_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((uint64_t)(parent_axe_id)) ((uint64_t)(axe_id)))

MERCURY_GEN_PROC(dset_create_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(dset_id)) 
                 ((uint64_t)(parent_axe_id)) ((hg_string_t)(name))
                 ((hid_t)(dapl_id)) ((hid_t)(dcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)) ((hid_t)(space_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dset_create_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(dset_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((uint64_t)(parent_axe_id)) 
                 ((hg_string_t)(name)) ((hid_t)(dapl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dset_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(dcpl_id)) ((hid_t)(type_id)) ((hid_t)(space_id)))
MERCURY_GEN_PROC(dset_set_extent_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((uint64_t)(parent_axe_id)) 
                 ((dims_t)(dims)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dset_io_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(iod_oh)) 
                 ((iod_obj_id_t)(iod_id)) ((uint64_t)(parent_axe_id))
                 ((hid_t)(space_id)) ((hid_t)(dxpl_id)) ((uint32_t)(checksum))
                 ((hg_bulk_t)(bulk_handle)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dset_read_out_t, ((int32_t)(ret)) ((uint32_t)(cs)))
MERCURY_GEN_PROC(dset_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((uint64_t)(parent_axe_id)) ((uint64_t)(axe_id)))

MERCURY_GEN_PROC(dtype_commit_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((iod_obj_id_t)(dtype_id)) 
                 ((uint64_t)(parent_axe_id)) ((hg_string_t)(name))
                 ((hid_t)(tapl_id)) ((hid_t)(tcpl_id)) ((hid_t)(lcpl_id))
                 ((hid_t)(type_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dtype_commit_out_t, ((iod_handle_t)(iod_oh)))
MERCURY_GEN_PROC(dtype_open_in_t, ((iod_handle_t)(coh)) ((iod_handle_t)(loc_oh))
                 ((iod_obj_id_t)(loc_id)) ((uint64_t)(parent_axe_id)) 
                 ((hg_string_t)(name)) ((hid_t)(tapl_id)) ((uint64_t)(axe_id)))
MERCURY_GEN_PROC(dtype_open_out_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((hid_t)(tcpl_id)) ((hid_t)(type_id)))
MERCURY_GEN_PROC(dtype_close_in_t, ((iod_handle_t)(iod_oh)) ((iod_obj_id_t)(iod_id)) 
                 ((uint64_t)(parent_axe_id)) ((uint64_t)(axe_id)))

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
