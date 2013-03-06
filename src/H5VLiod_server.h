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

#include "H5VLiod.h"         /* Iod VOL plugin                  */
#include "H5VLiod_common.h"

H5_DLL int H5VL_iod_server_file_create(fs_handle_t handle);
H5_DLL int H5VL_iod_server_file_open(fs_handle_t handle);
H5_DLL int H5VL_iod_server_group_create(fs_handle_t handle);
H5_DLL int H5VL_iod_server_group_open(fs_handle_t handle);
H5_DLL int H5VL_iod_server_dset_create(fs_handle_t handle);
H5_DLL int H5VL_iod_server_dset_open(fs_handle_t handle);

H5_DLL herr_t H5VL_server_encode_file_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_file_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_encode_file_open(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_file_open(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_encode_group_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_group_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_encode_group_open(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_group_open(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_encode_dset_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_dset_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_encode_dset_open(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_server_decode_dset_open(fs_proc_t proc, void *_input);


#endif /* _H5VLiod_server_H */
