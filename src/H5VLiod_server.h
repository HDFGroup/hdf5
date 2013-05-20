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

typedef struct op_data_t {
    void *input;
    hg_handle_t hg_handle;
} op_data_t;

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
H5_DLL int H5VL_iod_server_cancel_op(hg_handle_t handle);
H5_DLL int H5VL_iod_server_link_create(hg_handle_t handle);
H5_DLL int H5VL_iod_server_link_move(hg_handle_t handle);
H5_DLL int H5VL_iod_server_link_exists(hg_handle_t handle);
H5_DLL int H5VL_iod_server_link_remove(hg_handle_t handle);
H5_DLL int H5VL_iod_server_link_iterate(hg_handle_t handle);

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_server_H */
