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

#include "H5FFprivate.h"     /* FastForward wrappers            */
#include "H5VLiod_common.h"

#ifdef H5_HAVE_EFF

/* forward declaration of file struct */
struct H5VL_iod_file_t;
struct H5VL_iod_object_t;

/* types for requests */
typedef enum H5RQ_type_t {
    HG_FILE_CREATE,
    HG_FILE_OPEN,
    HG_FILE_FLUSH,
    HG_FILE_CLOSE,
    HG_GROUP_CREATE,
    HG_GROUP_OPEN,
    HG_GROUP_CLOSE,
    HG_DSET_CREATE,
    HG_DSET_OPEN,
    HG_DSET_READ,
    HG_DSET_WRITE,
    HG_DSET_SET_EXTENT,
    HG_DSET_CLOSE
} H5RQ_type_t;

typedef enum H5VL_iod_state_t {
    H5VL_IOD_PENDING,
    H5VL_IOD_COMPLETED
} H5VL_iod_state_t;

/* the client IOD VOL request struct */
typedef struct H5VL_iod_request_t {
    H5RQ_type_t type;
    void *data;
    void *req;
    struct H5VL_iod_object_t *obj;
    H5VL_iod_state_t state;
    H5_status_t status;
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

/* information about a dataset read/write request */
typedef struct H5VL_iod_io_info_t {
    void *status;
    hg_bulk_t *bulk_handle;
    uint32_t checksum;
} H5VL_iod_io_info_t;

H5_DLL herr_t H5VL_iod_request_delete(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_add(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_wait(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_wait_all(H5VL_iod_file_t *file);
H5_DLL herr_t H5VL_iod_request_wait_some(H5VL_iod_file_t *file, const void *object);
H5_DLL herr_t H5VL_iod_request_complete(H5VL_iod_file_t *file, H5VL_iod_request_t *req);
H5_DLL herr_t H5VL_iod_local_traverse(H5VL_iod_object_t *obj, H5VL_loc_params_t loc_params, 
                                      const char *name, iod_obj_id_t *id, iod_handle_t *oh, 
                                      char **new_name);

#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_client_H */
