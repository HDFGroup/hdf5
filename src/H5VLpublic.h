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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.org>
 *              January 23, 2012
 */
#ifndef _H5VLpublic_H
#define _H5VLpublic_H

#include "H5public.h"
#include "H5Fpublic.h"
#include "H5Lpublic.h"

#define H5VL_VOL_DEFAULT 0   /* Default VOL plugin value */

/* general routines common to all HDF5 objects */
typedef struct H5VL_general_class_t {
    hid_t (*open) (hid_t obj_id, const char * object_name, hid_t plist_id);
    hid_t (*create) (hid_t obj_id, const char *object_name, hid_t type_id, size_t size_hint, hid_t space_id,
                     hid_t plist_id1, hid_t plist_id2, hid_t plist_id3);
    htri_t (*exists) (hid_t id, const char *obj_name, const char *name, hid_t lapl_id);
    herr_t (*close) (hid_t obj_id);
} H5VL_general_class_t;

/* H5F routines */
typedef struct H5VL_file_class_t {
    H5F_t  *(*open) (const char *name, unsigned flags, hid_t fcpl_id, 
                     hid_t fapl_id, hid_t dxpl_id);
    herr_t (*close) (H5F_t *file);
    H5F_t  *(*create) (const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id);
    herr_t (*flush) (hid_t object_id, H5F_scope_t scope);
    htri_t (*is_hdf5) (const char *name);
    herr_t (*mount) (hid_t loc_id, const char *name, hid_t child_id, hid_t plist_id);
    herr_t (*unmount) (hid_t loc_id, const char *name);
    hid_t  (*reopen) (hid_t file_id);
} H5VL_file_class_t;

/* H5D routines */
typedef struct H5VL_dataset_class_t {
    herr_t (*read) (hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, 
                    hid_t xfer_plist_id, void * buf );
    herr_t (*write) (hid_t dataset_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, 
                     hid_t xfer_plist_id, const void * buf );
    herr_t (*set_extent) (hid_t dset_id, const hsize_t size[] );
} H5VL_dataset_class_t;

/* H5A routines */
typedef struct H5VL_attribute_class_t {
    hid_t (*create_by_name) (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t type_id, 
                             hid_t space_id, hid_t acpl_id, hid_t aapl_id, hid_t lapl_id);
    herr_t (*delete) (hid_t loc_id, const char *attr_name);
    herr_t (*delete_by_idx) (hid_t loc_id, const char *obj_name, H5_index_t idx_type, 
                             H5_iter_order_t order, hsize_t n, hid_t lapl_id );
    herr_t (*delete_by_name) (hid_t loc_id, const char *obj_name, const char *attr_name, hid_t lapl_id);
    hid_t (*open_by_idx) (hid_t loc_id, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, 
                          hsize_t n, hid_t aapl_id, hid_t lapl_id );
    hid_t (*open_by_name) (hid_t loc_id, const char *obj_name, const char *attr_name, 
                           hid_t aapl_id, hid_t lapl_id );
    hid_t (*open_idx) (hid_t loc_id, unsigned int idx );
    herr_t (*read) (hid_t attr_id, hid_t mem_type_id, void *buf );
    herr_t (*write) (hid_t attr_id, hid_t mem_type_id, const void *buf );
    herr_t (*rename) (hid_t loc_id, const char *obj_name, const char *old_attr_name, 
                      const char *new_attr_name, hid_t lapl_id);
} H5VL_attribute_class_t;

/* H5T routines*/
typedef struct H5VL_datatype_class_t {
    hid_t (*commit) (hid_t loc_id, const char *name, hid_t type_id,
                     hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id);
}H5VL_datatype_class_t;

/* H5L routines */
typedef struct H5VL_link_class_t {
    herr_t (*create) (hid_t obj_id, const char *name, hid_t loc_id, const char *link_name,
                      hid_t lcpl_id, hid_t lapl_id);
    herr_t (*delete) (hid_t loc_id, const char *name, hid_t lapl_id);
    herr_t (*move) (hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dest_name,
                    hid_t lcpl, hid_t lapl);
    herr_t (*copy) (hid_t src_loc_id, const char *src_name, hid_t dest_loc_id, const char *dest_name, 
                    hid_t lcpl_id, hid_t lapl_id);
    herr_t (*delete_by_idx) (hid_t loc_id, const char *group_name, H5_index_t index_field, 
                             H5_iter_order_t order, hsize_t n, hid_t lapl_id);
    herr_t (*create_external) (const char *target_file_name, const char *target_obj_name, 
                               hid_t link_loc_id, const char *link_name, hid_t lcpl_id, hid_t lapl_id);
    herr_t (*create_ud) (hid_t link_loc_id, const char *link_name, H5L_type_t link_type, 
                         const char *udata, size_t udata_size, hid_t lcpl_id, hid_t lapl_id);

} H5VL_link_class_t;

/* H5O routines */
typedef struct H5VL_object_class_t {
    herr_t (*set_comment) (hid_t object_id, const char *name, const char *comment, hid_t lapl_id);
    hid_t (*open_by_addr) (hid_t loc_id, haddr_t addr );
    hid_t (*open_by_idx) (hid_t loc_id, const char *group_name, H5_index_t index_type, 
                          H5_iter_order_t order, hsize_t n, hid_t lapl_id );
    herr_t (*copy) (hid_t src_loc_id, const char *src_name, hid_t dst_loc_id, const char *dst_name, 
                    hid_t ocpypl_id, hid_t lcpl_id );
    herr_t (*incr_refcount) (hid_t object_id );
    herr_t (*decr_refcount) (hid_t object_id );
} H5VL_object_class_t;

/* Class information for each VOL driver */
typedef struct H5VL_class_t {
    const char *name;
    herr_t  (*terminate)(void);
    size_t  fapl_size;
    void *  (*fapl_get)(H5F_t *file);
    void *  (*fapl_copy)(const void *fapl);
    herr_t  (*fapl_free)(void *fapl);
    H5VL_general_class_t       general_cls;
    H5VL_file_class_t          file_cls;
    H5VL_dataset_class_t       dataset_cls;
    H5VL_attribute_class_t     attribute_cls;
    H5VL_datatype_class_t      datatype_cls;
    H5VL_link_class_t          link_cls;
    H5VL_object_class_t        object_cls;
} H5VL_class_t;


/* Function prototypes */
H5_DLL hid_t H5VLregister(const H5VL_class_t *cls);
H5_DLL herr_t H5VLunregister(hid_t driver_id);

#endif
