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
 * This file contains private information about the H5VL iod client module
 */
#ifndef _H5VLiod_client_H
#define _H5VLiod_client_H

#include "H5VLiod.h"         /* Iod VOL plugin			*/
#include "H5VLiod_common.h"

/* forward declaration of file struct */
struct H5VL_iod_file_t;

/* types for requests */
typedef enum H5RQ_type_t {
    FS_FILE_CREATE,
    FS_FILE_OPEN,
    FS_FILE_CLOSE,
    FS_GROUP_CREATE,
    FS_GROUP_OPEN,
    FS_GROUP_CLOSE,
    FS_DSET_CREATE,
    FS_DSET_OPEN,
    FS_DSET_CLOSE
} H5RQ_type_t;

/* the client IOD VOL request struct */
typedef struct H5VL_iod_request_t {
    H5RQ_type_t type;
    void *data;
    void *req;
    struct H5VL_iod_request_t *prev;
    struct H5VL_iod_request_t *next;
} H5VL_iod_request_t;

/* a common strcut between all client side objects */
typedef struct H5VL_iod_object_t {
    H5I_type_t obj_type;
    char *obj_name;
    H5VL_iod_request_t *request;
    struct H5VL_iod_file_t *file;
} H5VL_iod_object_t;

/* the client side file struct */
typedef struct H5VL_iod_file_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_file_t remote_file;
    char *file_name;
    unsigned flags;
    hid_t fapl_id;
    unsigned nopen_objs;
    H5VL_iod_request_t *request_list_head;
    H5VL_iod_request_t *request_list_tail;
} H5VL_iod_file_t;

/* the client side group struct */
typedef struct H5VL_iod_group_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_group_t remote_group;
    hid_t gapl_id;
} H5VL_iod_group_t;

/* the client side dataset struct */
typedef struct H5VL_iod_dset_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_dset_t remote_dset;
    hid_t dapl_id;
} H5VL_iod_dset_t;

H5_DLL herr_t H5VL_iod_request_delete(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_add(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_wait(H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_local_traverse(H5VL_iod_object_t *obj, H5VL_loc_params_t loc_params, 
                                      const char *name, iod_obj_id_t *id, iod_handle_t *oh, 
                                      char **new_name);

H5_DLL herr_t H5VL_iod_client_encode_file_create(fs_proc_t proc, void *_input);
H5_DLL herr_t H5VL_iod_client_decode_file_create(fs_proc_t proc, void *_output);


#endif /* _H5VLiod_client_H */
