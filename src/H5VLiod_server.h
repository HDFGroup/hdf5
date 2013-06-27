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

#define EEXISTS 1
#define H5_DO_NATIVE 0

/* the AXE op data strucutre stored with every operation */
typedef struct op_data_t {
    void *input;
    hg_handle_t hg_handle;
} op_data_t;

/* the IOD scratch pad type */
typedef struct scratch_pad_t {
    iod_obj_id_t mdkv_id;      /* IOD ID of the KV store holding the metadata about an object */
    iod_obj_id_t attr_id;      /* IOD ID of the KV store holding the attribute ID attached to object */
    iod_obj_id_t filler1_id;   /* filler value - not used */
    iod_obj_id_t filler2_id;   /* filler value - not used */
} scratch_pad_t;

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
H5_DLL int H5VL_iod_server_attr_rename(hg_handle_t handle);
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
H5_DLL int H5VL_iod_server_object_open(hg_handle_t handle);
H5_DLL int H5VL_iod_server_object_copy(hg_handle_t handle);
H5_DLL int H5VL_iod_server_object_visit(hg_handle_t handle);
H5_DLL int H5VL_iod_server_object_exists(hg_handle_t handle);
H5_DLL int H5VL_iod_server_object_set_comment(hg_handle_t handle);
H5_DLL int H5VL_iod_server_object_get_comment(hg_handle_t handle);

H5_DLL void H5VL_iod_server_file_create_cb(AXE_engine_t axe_engine, 
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_file_open_cb(AXE_engine_t axe_engine,  
                                         size_t num_n_parents, AXE_task_t n_parents[], 
                                         size_t num_s_parents, AXE_task_t s_parents[], 
                                         void *op_data);
H5_DLL void H5VL_iod_server_file_close_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_file_flush_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_attr_create_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_attr_open_cb(AXE_engine_t axe_engine,  
                                         size_t num_n_parents, AXE_task_t n_parents[], 
                                         size_t num_s_parents, AXE_task_t s_parents[], 
                                         void *op_data);
H5_DLL void H5VL_iod_server_attr_read_cb(AXE_engine_t axe_engine,  
                                         size_t num_n_parents, AXE_task_t n_parents[], 
                                         size_t num_s_parents, AXE_task_t s_parents[], 
                                         void *op_data);
H5_DLL void H5VL_iod_server_attr_write_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_attr_exists_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_attr_rename_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_attr_remove_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_attr_close_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_group_create_cb(AXE_engine_t axe_engine,  
                                            size_t num_n_parents, AXE_task_t n_parents[], 
                                            size_t num_s_parents, AXE_task_t s_parents[], 
                                            void *op_data);
H5_DLL void H5VL_iod_server_group_open_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_group_close_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_dset_create_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_dset_open_cb(AXE_engine_t axe_engine,  
                                         size_t num_n_parents, AXE_task_t n_parents[], 
                                         size_t num_s_parents, AXE_task_t s_parents[], 
                                         void *op_data);
H5_DLL void H5VL_iod_server_dset_read_cb(AXE_engine_t axe_engine,  
                                         size_t num_n_parents, AXE_task_t n_parents[], 
                                         size_t num_s_parents, AXE_task_t s_parents[], 
                                         void *op_data);
H5_DLL void H5VL_iod_server_dset_write_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_dset_set_extent_cb(AXE_engine_t axe_engine,  
                                               size_t num_n_parents, AXE_task_t n_parents[], 
                                               size_t num_s_parents, AXE_task_t s_parents[], 
                                               void *op_data);
H5_DLL void H5VL_iod_server_dset_close_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_dtype_commit_cb(AXE_engine_t axe_engine,  
                                            size_t num_n_parents, AXE_task_t n_parents[], 
                                            size_t num_s_parents, AXE_task_t s_parents[], 
                                            void *op_data);
H5_DLL void H5VL_iod_server_dtype_open_cb(AXE_engine_t axe_engine,  
                                          size_t num_n_parents, AXE_task_t n_parents[], 
                                          size_t num_s_parents, AXE_task_t s_parents[], 
                                          void *op_data);
H5_DLL void H5VL_iod_server_dtype_close_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_link_create_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_link_move_cb(AXE_engine_t axe_engine, 
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_link_exists_cb(AXE_engine_t axe_engine, 
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_link_remove_cb(AXE_engine_t axe_engine, 
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);

H5_DLL void H5VL_iod_server_object_open_cb(AXE_engine_t axe_engine,  
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_object_copy_cb(AXE_engine_t axe_engine, 
                                           size_t num_n_parents, AXE_task_t n_parents[], 
                                           size_t num_s_parents, AXE_task_t s_parents[], 
                                           void *op_data);
H5_DLL void H5VL_iod_server_object_exists_cb(AXE_engine_t axe_engine, 
                                             size_t num_n_parents, AXE_task_t n_parents[], 
                                             size_t num_s_parents, AXE_task_t s_parents[], 
                                             void *op_data);
H5_DLL void H5VL_iod_server_object_set_comment_cb(AXE_engine_t axe_engine, 
                                                  size_t num_n_parents, AXE_task_t n_parents[], 
                                                  size_t num_s_parents, AXE_task_t s_parents[], 
                                                  void *op_data);
H5_DLL void H5VL_iod_server_object_get_comment_cb(AXE_engine_t UNUSED axe_engine, 
                                                  size_t num_n_parents, AXE_task_t n_parents[], 
                                                  size_t num_s_parents, AXE_task_t s_parents[], 
                                                  void *_op_data);

H5_DLL herr_t H5VL_iod_server_traverse(iod_handle_t coh, iod_obj_id_t loc_id, iod_handle_t loc_handle, 
                                       const char *path, hbool_t create_interm_grps,
                                       char **last_comp, iod_obj_id_t *iod_id, iod_handle_t *iod_oh);

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_server_H */
