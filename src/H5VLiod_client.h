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
#include "H5Mpublic.h"
#include "H5RCprivate.h"     /* Read contexts */
#include "H5TRprivate.h"     /* Transactions */
#include "H5VLiod_common.h"

#ifdef H5_HAVE_EFF

/* forward declaration of file struct */
struct H5VL_iod_file_t;
/* forward declaration of object struct */
struct H5VL_iod_object_t;

/* enum for types of requests */
typedef enum H5RQ_type_t {
    HG_ANALYSIS_EXECUTE,
    HG_FILE_CREATE,
    HG_FILE_OPEN,
    HG_FILE_CLOSE,
    HG_ATTR_CREATE,
    HG_ATTR_OPEN,
    HG_ATTR_READ,
    HG_ATTR_WRITE,
    HG_ATTR_EXISTS,
    HG_ATTR_RENAME,
    HG_ATTR_REMOVE,
    HG_ATTR_CLOSE,
    HG_GROUP_CREATE,
    HG_GROUP_OPEN,
    HG_GROUP_CLOSE,
    HG_DSET_CREATE,
    HG_DSET_OPEN,
    HG_DSET_READ,
    HG_DSET_WRITE,
    HG_DSET_GET_VL_SIZE,
    HG_DSET_SET_EXTENT,
    HG_DSET_CLOSE,
    HG_DTYPE_COMMIT,
    HG_DTYPE_OPEN,
    HG_DTYPE_CLOSE,
    HG_LINK_CREATE,
    HG_LINK_MOVE,
    HG_LINK_ITERATE,
    HG_LINK_EXISTS,
    HG_LINK_GET_INFO,
    HG_LINK_GET_VAL,
    HG_LINK_REMOVE,
    HG_MAP_CREATE,
    HG_MAP_OPEN,
    HG_MAP_SET,
    HG_MAP_GET,
    HG_MAP_GET_COUNT,
    HG_MAP_EXISTS,
    HG_MAP_ITERATE,
    HG_MAP_DELETE,
    HG_MAP_CLOSE,
    HG_OBJECT_OPEN_BY_TOKEN,
    HG_OBJECT_OPEN,
    HG_OBJECT_COPY,
    HG_OBJECT_VISIT,
    HG_OBJECT_EXISTS,
    HG_OBJECT_SET_COMMENT,
    HG_OBJECT_GET_COMMENT,
    HG_OBJECT_GET_INFO,
    HG_RC_ACQUIRE,
    HG_RC_RELEASE,
    HG_RC_PERSIST,
    HG_RC_SNAPSHOT,
    HG_TR_START,
    HG_TR_FINISH,
    HG_TR_SET_DEPEND,
    HG_TR_SKIP,
    HG_TR_ABORT
} H5RQ_type_t;

/* the client IOD VOL request struct */
typedef struct H5VL_iod_request_t {
    H5RQ_type_t type; /* The operation type of this request */
    void *data; /* data associated with request (usually used at completion time) */
    void *req; /* the request pointer correponding to the Mercury request */
    struct H5VL_iod_object_t *obj; /* The object pointer that this request is associated with */
    H5VL_iod_state_t state; /* current internal state of the request */
    H5ES_status_t status; /* external status given to use of request */
    uint64_t axe_id; /* The AXE ID this request was assigned */
    unsigned ref_count; /* reference count to know when this request can be freed. */
    H5VL_iod_req_info_t *trans_info; /* pointer to transaction or read context struct for this request */

    size_t num_parents; /* Number of parents this request has (in AXE) */
    struct H5VL_iod_request_t **parent_reqs; /* an array of the parent request pointers */

    /* Linked list pointers for the container this request was generated. */
    struct H5VL_iod_request_t *file_prev; 
    struct H5VL_iod_request_t *file_next;

    /* Linked list pointers for all IOD VOL requests in the library */
    struct H5VL_iod_request_t *global_prev;
    struct H5VL_iod_request_t *global_next;

    /* Linked list pointers for the transaction this request belong to */
    struct H5VL_iod_request_t *trans_prev;
    struct H5VL_iod_request_t *trans_next;
} H5VL_iod_request_t;

/* struct that contains the information about the IOD container */
typedef struct H5VL_iod_remote_file_t {
    /* Do NOT change the order of the parameters */
    iod_handle_t coh;
    iod_handles_t root_oh;
    uint64_t kv_oid_index;
    uint64_t array_oid_index;
    uint64_t blob_oid_index;
    iod_obj_id_t root_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    uint64_t c_version;
    hid_t fcpl_id;
} H5VL_iod_remote_file_t;

/* struct that contains the information about the IOD attr */
typedef struct H5VL_iod_remote_attr_t {
    /* Do NOT change the order of the parameters */
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    hid_t acpl_id;
    hid_t type_id;
    hid_t space_id;
} H5VL_iod_remote_attr_t;

/* struct that contains the information about the IOD group */
typedef struct H5VL_iod_remote_group_t {
    /* Do NOT change the order of the parameters */
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    hid_t gcpl_id;
} H5VL_iod_remote_group_t;

/* struct that contains the information about the IOD map */
typedef struct H5VL_iod_remote_map_t {
    /* Do NOT change the order of the parameters */
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    hid_t keytype_id;
    hid_t valtype_id;
    hid_t mcpl_id;
} H5VL_iod_remote_map_t;

/* struct that contains the information about the IOD dset */
typedef struct H5VL_iod_remote_dset_t {
    /* Do NOT change the order of the parameters */
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    hid_t dcpl_id;
    hid_t type_id;
    hid_t space_id;
} H5VL_iod_remote_dset_t;

/* struct that contains the information about the IOD dtype */
typedef struct H5VL_iod_remote_dtype_t {
    /* Do NOT change the order of the parameters */
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    hid_t tcpl_id;
    hid_t type_id;
} H5VL_iod_remote_dtype_t;

/* struct that contains the information about a generic IOD object */
typedef struct H5VL_iod_remote_object_t {
    /* Do NOT change the order of the parameters */
    H5I_type_t obj_type;
    iod_handles_t iod_oh;
    iod_obj_id_t iod_id;
    iod_obj_id_t mdkv_id;
    iod_obj_id_t attrkv_id;
    hid_t cpl_id;
    hid_t type_id;
    hid_t space_id;
} H5VL_iod_remote_object_t;

/* a common strcut between all client side objects */
typedef struct H5VL_iod_object_t {
    H5I_type_t obj_type;
    char *obj_name;
    char *comment;
    H5VL_iod_request_t *request;
    struct H5VL_iod_file_t *file;
} H5VL_iod_object_t;

/* the client side file struct */
typedef struct H5VL_iod_file_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_file_t remote_file;
    char *file_name;
    unsigned flags;
    uint32_t md_integrity_scope;
    hid_t fapl_id;
    MPI_Comm comm;
    MPI_Info info;
    int my_rank;
    int num_procs;
    size_t num_req;
    unsigned nopen_objs;
    H5VL_iod_request_t *request_list_head;
    H5VL_iod_request_t *request_list_tail;
} H5VL_iod_file_t;

/* the client side attribute struct */
typedef struct H5VL_iod_attr_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_attr_t remote_attr;
    char *loc_name;
} H5VL_iod_attr_t;

/* the client side group struct */
typedef struct H5VL_iod_group_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_group_t remote_group;
    hid_t gapl_id;
} H5VL_iod_group_t;

/* the client side map struct */
typedef struct H5VL_iod_map_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_map_t remote_map;
    hid_t mapl_id;
} H5VL_iod_map_t;

/* the client side dataset struct */
typedef struct H5VL_iod_dset_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_dset_t remote_dset;
    hid_t dapl_id;
} H5VL_iod_dset_t;

/* the client side datatype struct */
typedef struct H5VL_iod_dtype_t {
    H5VL_iod_object_t common; /* must be first */
    H5VL_iod_remote_dtype_t remote_dtype;
    hid_t tapl_id;
} H5VL_iod_dtype_t;

/* information about a dataset read/write request */
typedef struct H5VL_iod_io_info_t {
    /* read & write params */
    void *status;
    hg_bulk_t *bulk_handle;

    /* write params */
    size_t *vl_string_len;

    /* read params */
    void *buf_ptr;
    size_t nelmts;
    size_t type_size;
    struct H5S_t *space;
    uint32_t *cs_ptr;
    uint32_t raw_cs_scope;
    hid_t file_space_id;
    hid_t mem_type_id;
    hid_t dxpl_id;
    uint64_t axe_id;
    na_addr_t peer;
    hg_id_t read_id;

} H5VL_iod_io_info_t;

typedef struct H5VL_iod_map_set_info_t {
    void *status;
    hg_bulk_t *value_handle;
} H5VL_iod_map_set_info_t;

/* information about a map get request */
typedef struct H5VL_iod_map_io_info_t {
    /* read & write params */
    map_get_out_t *output; /* this must be first */
    void *val_ptr;
    void *read_buf;
    hg_bulk_t *value_handle;
    size_t val_size;
    uint32_t raw_cs_scope;
    uint32_t *val_cs_ptr;
    hid_t val_mem_type_id;
    hid_t key_mem_type_id;
    hid_t dxpl_id;
    hbool_t val_is_vl;
    hid_t rcxt_id;
    binary_buf_t key;
    na_addr_t peer;
    hg_id_t map_get_id;
} H5VL_iod_map_io_info_t;

/* information about a read context acquire request*/
typedef struct H5VL_iod_rc_info_t {
    rc_acquire_out_t result;
    H5RC_t *read_cxt;
    uint64_t *c_version_ptr;
} H5VL_iod_rc_info_t;

/* information about an exists request*/
typedef struct H5VL_iod_exists_info_t {
    hbool_t *user_bool; /* pointer to the user provided hbool_t */
    htri_t server_ret; /* the return value from the server */
} H5VL_iod_exists_info_t;

H5_DLL herr_t H5VL_iod_request_delete(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_add(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_wait(H5VL_iod_file_t *file, H5VL_iod_request_t *request);
H5_DLL herr_t H5VL_iod_request_wait_all(H5VL_iod_file_t *file);
H5_DLL herr_t H5VL_iod_request_wait_some(H5VL_iod_file_t *file, const void *object);
H5_DLL herr_t H5VL_iod_request_complete(H5VL_iod_file_t *file, H5VL_iod_request_t *req);
H5_DLL herr_t H5VL_iod_request_cancel(H5VL_iod_file_t *file, H5VL_iod_request_t *req);
H5_DLL herr_t H5VL_iod_request_decr_rc(H5VL_iod_request_t *request);

H5_DLL herr_t H5VL_iod_get_parent_requests(H5VL_iod_object_t *obj, H5VL_iod_req_info_t *req_info, 
                                           H5VL_iod_request_t **parent_reqs, size_t *num_parents);
H5_DLL  herr_t H5VL_iod_get_loc_info(H5VL_iod_object_t *obj, iod_obj_id_t *iod_id, 
                                     iod_handles_t *iod_oh, iod_obj_id_t *mdkv_oh, 
                                     iod_obj_id_t *attrkv_oh);
H5_DLL herr_t H5VL_iod_get_obj_requests(H5VL_iod_object_t *obj, /*IN/OUT*/ size_t *count, 
                                        /*OUT*/ H5VL_iod_request_t **parent_reqs);
H5_DLL herr_t H5VL__iod_create_and_forward(hg_id_t op_id, H5RQ_type_t op_type, 
                                           H5VL_iod_object_t *request_obj, htri_t track,
                                           size_t num_parents, H5VL_iod_request_t **parent_reqs,
                                           H5VL_iod_req_info_t *req_info,
                                           void *input, void *output, void *data, void **req);

H5_DLL herr_t H5VL_iod_map_dtype_info(hid_t type_id, /*out*/ hbool_t *is_vl, /*out*/size_t *size);
H5_DLL herr_t H5VL_iod_map_get_size(hid_t type_id, const void *buf, 
                                    /*out*/uint32_t *checksum, 
                                    /*out*/size_t *size, /*out*/H5T_class_t *dt_class);
H5_DLL herr_t H5VL_iod_gen_obj_id(int myrank, int nranks, uint64_t cur_index, 
                                  iod_obj_type_t type, uint64_t *id);
H5_DLL herr_t H5VL_iod_pre_write(hid_t type_id, hid_t space_id, const void *buf, 
                                 /*out*/uint32_t *_checksum, 
                                 /*out*/hg_bulk_t *bulk_handle,
                                 /*out*/size_t **vl_str_len);
H5_DLL herr_t H5VL_iod_pre_read(hid_t type_id, hid_t space_id, const void *buf, 
                                /*out*/hg_bulk_t *bulk_handle, hbool_t *is_vl_data);

/* private routines for map objects */
H5_DLL herr_t H5M_init(void);
H5_DLL void *H5VL_iod_map_create(void *obj, H5VL_loc_params_t loc_params, const char *name, 
                                 hid_t keytype, hid_t valtype, hid_t lcpl_id, hid_t mcpl_id, 
                                 hid_t mapl_id, hid_t trans_id, void **req);
H5_DLL void *H5VL_iod_map_open(void *obj, H5VL_loc_params_t loc_params,
                               const char *name, hid_t mapl_id, hid_t rcxt_id, void **req);
H5_DLL herr_t H5VL_iod_map_set(void *map, hid_t key_mem_type_id, const void *key, 
                               hid_t val_mem_type_id, const void *value, hid_t dxpl_id, 
                               hid_t trans_id, void **req);
H5_DLL herr_t H5VL_iod_map_get(void *map, hid_t key_mem_type_id, const void *key, 
                               hid_t val_mem_type_id, void *value, hid_t dxpl_id, 
                               hid_t rcxt_id, void **req);
H5_DLL herr_t H5VL_iod_map_get_types(void *map, hid_t *key_type_id, hid_t *val_type_id, 
                                     hid_t rcxt_id, void **req);
H5_DLL herr_t H5VL_iod_map_get_count(void *map, hsize_t *count, hid_t rcxt_id, void **req);
H5_DLL herr_t H5VL_iod_map_exists(void *map, hid_t key_mem_type_id, const void *key, 
                                  hbool_t *exists, hid_t rcxt_id, void **req);
H5_DLL herr_t H5VL_iod_map_iterate(void *map, hid_t key_mem_type_id, hid_t value_mem_type_id, 
                                   H5M_iterate_func_t callback_func, void *context);
H5_DLL herr_t H5VL_iod_map_delete(void *map, hid_t key_mem_type_id, const void *key, 
                                  hid_t trans_id, void **req);
H5_DLL herr_t H5VL_iod_map_close(void *map, void **req);

H5_DLL void * H5VL_iod_obj_open_token(const void *token, H5RC_t *rc, 
                                      H5I_type_t *opened_type, void **req);

/* private routines for RC */
H5_DLL herr_t H5VL_iod_rc_acquire(H5VL_iod_file_t *file, H5RC_t *rc, 
                                  uint64_t *c_version, hid_t rcapl_id, void **req);
H5_DLL herr_t H5VL_iod_rc_release(H5RC_t *rc, void **req);
H5_DLL herr_t H5VL_iod_rc_persist(H5RC_t *rc, void **req);
H5_DLL herr_t H5VL_iod_rc_snapshot(H5RC_t *rc, const char *snapshot_name, void **req);

/* private routines for TR */
H5_DLL herr_t H5VL_iod_tr_start(H5TR_t *tr, hid_t trspl_id, void **req);
H5_DLL herr_t H5VL_iod_tr_finish(H5TR_t *tr, hbool_t acquire, hid_t trfpl_id, void **req);
H5_DLL herr_t H5VL_iod_tr_set_dependency(H5TR_t *tr, uint64_t trans_num, void **req);
H5_DLL herr_t H5VL_iod_tr_skip(H5VL_iod_file_t *file, uint64_t start_trans_num, 
                               uint64_t count, void **req);
H5_DLL herr_t H5VL_iod_tr_abort(H5TR_t *tr, void **req);

H5_DLL herr_t H5VL_iod_analysis_execute(const char *file_name, const char *obj_name, 
                                        hid_t query_id, void **req);
#endif /* H5_HAVE_EFF */
#endif /* _H5VLiod_client_H */
