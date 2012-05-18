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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2011
 */
#ifndef _H5VLprivate_H
#define _H5VLprivate_H

#include "H5VLpublic.h"

/**************************/
/* Library Private Macros */
/**************************/

/****************************/
/* Library Private Typedefs */
/****************************/
#define H5_REQUEST_NULL -1

/* type of the ID passed to users */
typedef struct H5VL_id_wrapper_t {
    hid_t obj_id;  /* actual id for object */
    H5VL_class_t *vol_plugin;  /* pointer to the VOL structure */
} H5VL_id_wrapper_t;

/*****************************/
/* Library Private Variables */
/*****************************/

/******************************/
/* Library Private Prototypes */
/******************************/

/* Forward declarations for prototype arguments */
struct H5P_genplist_t;
struct H5F_t;

H5_DLL int H5VL_term_interface(void);
H5_DLL H5VL_class_t *H5VL_get_class(hid_t id);
//H5_DLL hsize_t H5VL_sb_size(H5F_t *file);
H5_DLL hid_t  H5VL_register(const void *cls, size_t size, hbool_t app_ref);

H5_DLL herr_t H5VL_replace_with_uids(hid_t *oid_list, ssize_t num_ids);
H5_DLL int H5VL_inc_ref_uid(hid_t fid, hbool_t app_ref);

H5_DLL hid_t H5VL_attr_create(hid_t loc_id, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
H5_DLL hid_t H5VL_attr_open(hid_t loc_id, void *location, const char *name, hid_t aapl_id, hid_t req);
H5_DLL herr_t H5VL_attr_read(hid_t attr_id, hid_t dtype_id, void *buf, hid_t req);
H5_DLL herr_t H5VL_attr_write(hid_t attr_id, hid_t dtype_id, const void *buf, hid_t req);
H5_DLL herr_t H5VL_attr_get(hid_t id, H5VL_attr_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_attr_delete(hid_t loc_id, void *location, const char *attr_name, hid_t req);
H5_DLL herr_t H5VL_attr_close(hid_t attr_id, hid_t req);

H5_DLL hid_t H5VL_dataset_create(hid_t uid, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
H5_DLL hid_t H5VL_dataset_open(hid_t uid, const char *name, hid_t dapl_id, hid_t req);
H5_DLL herr_t H5VL_dataset_close(hid_t uid, hid_t req);
H5_DLL herr_t H5VL_dataset_read(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
H5_DLL herr_t H5VL_dataset_write(hid_t dset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
H5_DLL herr_t H5VL_dataset_set_extent(hid_t uid, const hsize_t size[], hid_t req);
H5_DLL herr_t H5VL_dataset_get(hid_t uid, H5VL_dataset_get_t get_type, hid_t req, ...);

H5_DLL herr_t H5VL_datatype_commit(hid_t loc_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
H5_DLL hid_t H5VL_datatype_open(hid_t loc_id, const char *name, hid_t tapl_id, hid_t req);
H5_DLL herr_t H5VL_datatype_close(hid_t type_id, hid_t req);

H5_DLL hid_t  H5VL_file_open(const char *name, unsigned flags, hid_t fapl_id, hid_t req);
H5_DLL hid_t  H5VL_file_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
H5_DLL herr_t H5VL_file_close(hid_t file_id, hid_t req);
H5_DLL herr_t H5VL_file_flush(hid_t file_id, H5F_scope_t scope, hid_t req);
H5_DLL herr_t H5VL_file_generic(hid_t loc_id, H5VL_file_generic_t generic_type, hid_t req, ...);
H5_DLL herr_t H5VL_file_get(hid_t uid, H5VL_file_get_t get_type, hid_t req, ...);

H5_DLL hid_t H5VL_group_create(hid_t uid, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
H5_DLL hid_t H5VL_group_open(hid_t uid, const char *name, hid_t gapl_id, hid_t req);
H5_DLL herr_t H5VL_group_close(hid_t uid, hid_t req);
H5_DLL herr_t H5VL_group_get(hid_t uid, H5VL_group_get_t get_type, hid_t req, ...);

H5_DLL herr_t H5VL_link_create(H5VL_link_create_type_t create_type, hid_t loc_id, 
                               const char *link_name, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VL_link_move(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id,
                             const char *dst_name, hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VL_link_get(hid_t loc_id, H5VL_link_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_link_delete(hid_t loc_id, const char *name, void *udata, hid_t lapl_id, hid_t req);

H5_DLL hid_t H5VL_object_open_by_loc(hid_t uid, void *obj_loc, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VL_object_copy(hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, 
                               const char *dst_name, hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
H5_DLL herr_t H5VL_object_get(hid_t uid, H5VL_object_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VL_object_generic(hid_t id, H5VL_object_generic_t generic_type, hid_t req, ...);
H5_DLL herr_t H5VL_object_lookup(hid_t uid, H5VL_object_lookup_t lookup_type, hid_t req, ...);
H5_DLL herr_t H5VL_object_free_loc(hid_t loc_id, void *location, hid_t req);
H5_DLL herr_t H5VL_object_close(hid_t uid, hid_t req);

H5_DLL herr_t H5VL_fapl_open(struct H5P_genplist_t *plist, H5VL_class_t *vol_cls);
H5_DLL herr_t H5VL_fapl_close(H5VL_class_t *vol_cls);

#endif /* _H5VLprivate_H */
