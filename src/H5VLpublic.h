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

#include "stdarg.h"

#include "H5public.h"
#include "H5Fpublic.h"
#include "H5Lpublic.h"
#include "H5Opublic.h"
#include "H5Rpublic.h"

/* Dataset creation property names */
#define H5VL_DSET_TYPE_ID        "dataset_type_id"
#define H5VL_DSET_SPACE_ID       "dataset_space_id"
#define H5VL_DSET_LCPL_ID        "dataset_lcpl_id"

/* Attribute creation property names */
#define H5VL_ATTR_TYPE_ID        "attr_type_id"
#define H5VL_ATTR_SPACE_ID       "attr_space_id"
#define H5VL_ATTR_LOC_PARAMS     "attr_location"

/* Link creation property names */
#define H5VL_LINK_TARGET             "target_location_object"
#define H5VL_LINK_TARGET_LOC_PARAMS  "target_params"
#define H5VL_LINK_TARGET_NAME        "target_name"
#define H5VL_LINK_TYPE               "link type"
#define H5VL_LINK_UDATA              "udata"
#define H5VL_LINK_UDATA_SIZE         "udata size"

/* Group creation property names */
#define H5VL_GRP_LCPL_ID "group_lcpl_id"

/* types for all attr get API routines */
typedef enum H5VL_attr_get_t {
    H5VL_ATTR_EXISTS              = 0,         /* H5Aexists                          */
    H5VL_ATTR_GET_SPACE           = 1,         /* dataspace                           */
    H5VL_ATTR_GET_TYPE            = 2,         /* datatype                            */
    H5VL_ATTR_GET_ACPL            = 3,         /* creation property list              */
    H5VL_ATTR_GET_NAME            = 4,         /* access property list                */
    H5VL_ATTR_GET_STORAGE_SIZE    = 5,         /* storage size                        */
    H5VL_ATTR_GET_INFO            = 6          /* offset                              */
} H5VL_attr_get_t;

/* types for all dataset get API routines */
typedef enum H5VL_dataset_get_t {
    H5VL_DATASET_GET_SPACE           = 0,         /* dataspace                           */
    H5VL_DATASET_GET_SPACE_STATUS    = 1,         /* space  status                       */
    H5VL_DATASET_GET_TYPE            = 2,         /* datatype                            */
    H5VL_DATASET_GET_DCPL            = 3,         /* creation property list              */
    H5VL_DATASET_GET_DAPL            = 4,         /* access property list                */
    H5VL_DATASET_GET_STORAGE_SIZE    = 5,         /* storage size                        */
    H5VL_DATASET_GET_OFFSET          = 6          /* offset                              */
} H5VL_dataset_get_t;

/* types for all file get API routines */
typedef enum H5VL_file_get_t {
    H5VL_FILE_GET_FAPL	            = 0, 	/*file access property list		*/
    H5VL_FILE_GET_FCPL	            = 1,	/*file creation property list		*/
    H5VL_FILE_GET_INTENT	    = 2,	/*file intent           		*/
    H5VL_FILE_GET_NAME	            = 3,	/*file name             		*/
    H5VL_FILE_GET_OBJ_COUNT	    = 4,	/*object count in file	        	*/
    H5VL_FILE_GET_OBJ_IDS	    = 5,	/*object ids in file     		*/
    H5VL_OBJECT_GET_FILE            = 6
} H5VL_file_get_t;

/* types for all file misc operations */
typedef enum H5VL_file_misc_t {
    H5VL_FILE_MOUNT                 = 0,
    H5VL_FILE_UNMOUNT               = 1,
    H5VL_FILE_IS_ACCESSIBLE         = 2
} H5VL_file_misc_t;

/* types for all file optional operations */
typedef enum H5VL_file_optional_t {
    H5VL_FILE_CLEAR_ELINK_CACHE     = 0,
    H5VL_FILE_GET_FILE_IMAGE        = 1,        /*file image */
    H5VL_FILE_GET_FREE_SECTIONS     = 2,        /*file free selections                  */
    H5VL_FILE_GET_FREE_SPACE	    = 3,	/*file freespace         		*/
    H5VL_FILE_GET_INFO	            = 4,	/*file info             		*/
    H5VL_FILE_GET_MDC_CONF	    = 5,	/*file metadata cache configuration	*/
    H5VL_FILE_GET_MDC_HR	    = 6,	/*file metadata cache hit rate		*/
    H5VL_FILE_GET_MDC_SIZE	    = 7,        /*file metadata cache size		*/
    H5VL_FILE_GET_SIZE	            = 8,	/*file size             		*/
    H5VL_FILE_GET_VFD_HANDLE	    = 9, 	/*file VFD handle       		*/
    H5VL_FILE_REOPEN                = 10,
    H5VL_FILE_RESET_MDC_HIT_RATE    = 11,
    H5VL_FILE_SET_MDC_CONFIG        = 12
} H5VL_file_optional_t;

/* types for all group get API routines */
typedef enum H5VL_group_get_t {
    H5VL_GROUP_GET_GCPL	    = 0,	/*group creation property list		*/
    H5VL_GROUP_GET_INFO	    = 1 	/*group info             		*/
} H5VL_group_get_t;

/* link create types for VOL */
typedef enum H5VL_link_create_type_t {
    H5VL_LINK_CREATE_HARD           = 0,
    H5VL_LINK_CREATE_SOFT	    = 1,
    H5VL_LINK_CREATE_UD	            = 2
} H5VL_link_create_type_t;

/* types for all link get API routines */
typedef enum H5VL_link_get_t {
    H5VL_LINK_EXISTS        = 0,        /*link existence                        */
    H5VL_LINK_GET_INFO	    = 1, 	/*link info             		*/
    H5VL_LINK_GET_NAME	    = 2,	/*link name                             */
    H5VL_LINK_GET_VAL       = 3         /*link value                            */
} H5VL_link_get_t;

/* types for all object general operations */
typedef enum H5VL_object_misc_t {
    H5VL_ATTR_RENAME                = 0,        /* H5Arename                          */
    H5VL_OBJECT_CHANGE_REF_COUNT    = 1,        /* H5Oincr/decr_refcount              */
    H5VL_OBJECT_SET_COMMENT         = 2,        /* H5Oset_comment(_by_name)           */
    H5VL_REF_CREATE                 = 3         /* H5Rcreate                          */
} H5VL_object_misc_t;

/* types for all object general operations */
typedef enum H5VL_object_optional_t {
    H5VL_OPTIONAL = 0
} H5VL_object_optional_t;

/* types for all object get API routines */
typedef enum H5VL_object_get_t {
    H5VL_OBJECT_EXISTS              = 0,        /* H5Oexists_by_name)                */
    H5VL_OBJECT_GET_INFO	    = 1,	/*object info	                	*/
    H5VL_OBJECT_GET_COMMENT	    = 2, 	/*object comment            		*/
    H5VL_REF_GET_REGION             = 3,        /*dataspace of region                   */
    H5VL_REF_GET_TYPE               = 4,        /*type of object                        */
    H5VL_REF_GET_NAME               = 5         /*object name                           */
} H5VL_object_get_t;

/* types for different ways that objects are located in an HDF5 container */
typedef enum H5VL_loc_type_t {
    H5VL_OBJECT_BY_SELF     = 0,
    H5VL_OBJECT_BY_NAME	    = 1,
    H5VL_OBJECT_BY_IDX	    = 2,
    H5VL_OBJECT_BY_ADDR	    = 3,
    H5VL_OBJECT_BY_REF      = 4
} H5VL_loc_type_t;

struct H5VL_loc_by_name {
    const char *name;
    hid_t plist_id;
};

struct H5VL_loc_by_idx {
    const char *name;
    H5_index_t idx_type;
    H5_iter_order_t order;
    hsize_t n;
    hid_t plist_id;
};

struct H5VL_loc_by_addr {
    haddr_t addr;
};

struct H5VL_loc_by_ref {
    H5R_type_t ref_type;
    const void *_ref;
    hid_t plist_id;
};

/* Structure to hold parameters for object locations. 
   either: BY_ID, BY_NAME, BY_IDX, BY_ADDR, BY_REF */
typedef struct H5VL_loc_params_t {
    H5I_type_t      obj_type;
    H5VL_loc_type_t type;
    union{
        struct H5VL_loc_by_name loc_by_name;
        struct H5VL_loc_by_idx  loc_by_idx;
        struct H5VL_loc_by_addr loc_by_addr;
        struct H5VL_loc_by_ref  loc_by_ref;
    }loc_data;
} H5VL_loc_params_t;

#define H5VL_VOL_DEFAULT 0   /* Default VOL plugin value */

/* Forward declaration */
typedef struct H5VL_t H5VL_t;

/* H5A routines */
typedef struct H5VL_attr_class_t {
    void  *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t req);
    void  *(*open)  (void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t aapl_id, hid_t req);
    herr_t (*read)  (void *attr, hid_t mem_type_id, void *buf, hid_t req);
    herr_t (*write) (void *attr, hid_t mem_type_id, const void *buf, hid_t req);
    herr_t (*get)   (void *attr, H5VL_attr_get_t get_type, hid_t req, va_list arguments);
    herr_t (*remove)(void *obj, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
    herr_t (*close) (void *attr, hid_t req);
} H5VL_attr_class_t;

/* H5T routines*/
typedef struct H5VL_datatype_class_t {
    void  *(*commit)(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id, 
                     hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
    void  *(*open)  (void *obj, H5VL_loc_params_t loc_params, const char * name, 
                     unsigned char *buf, size_t nalloc, hid_t tapl_id, hid_t req);
    ssize_t (*get_size) (void *obj, H5VL_loc_params_t loc_params, const char *name, 
                         hid_t tapl_id, hid_t req);
    herr_t (*close) (void *dt, hid_t req);
}H5VL_datatype_class_t;

/* H5D routines */
typedef struct H5VL_dataset_class_t {
    void  *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
    void  *(*open)  (void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t dapl_id, hid_t req);
    herr_t (*read)  (void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                     hid_t xfer_plist_id, void * buf, hid_t req);
    herr_t (*write) (void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                     hid_t xfer_plist_id, const void * buf, hid_t req);
    herr_t (*set_extent) (void *dset, const hsize_t size[], hid_t req);
    herr_t (*get)   (void *dset, H5VL_dataset_get_t get_type, hid_t req, va_list arguments);
    herr_t (*close) (void *dset, hid_t req);
} H5VL_dataset_class_t;

/* H5F routines */
typedef struct H5VL_file_class_t {
    void  *(*create)(const char *name, unsigned flags, hid_t fcpl_id, 
                     hid_t fapl_id, hid_t req);
    void  *(*open)  (const char *name, unsigned flags, hid_t fapl_id, hid_t req);
    herr_t (*flush) (void *obj, H5VL_loc_params_t loc_params, H5F_scope_t scope, hid_t req);
    herr_t (*get)   (void *file, H5VL_file_get_t get_type, hid_t req, va_list arguments);
    herr_t (*misc)  (void *file, H5VL_file_misc_t misc_type, hid_t req, va_list arguments);
    herr_t (*optional)(void *file, H5VL_file_optional_t op_type, hid_t req, va_list arguments);
    herr_t (*close) (void *file, hid_t req);
} H5VL_file_class_t;

/* H5G routines */
typedef struct H5VL_group_class_t {
    void  *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
    void  *(*open)  (void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t gapl_id, hid_t req);
    herr_t (*get)   (void *obj, H5VL_group_get_t get_type, hid_t req, va_list arguments);
    herr_t (*close) (void *grp, hid_t req);
} H5VL_group_class_t;

/* H5L routines */
typedef struct H5VL_link_class_t {
    herr_t (*create)(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, 
                     hid_t lcpl_id, hid_t lapl_id, hid_t req);
    herr_t (*move)  (void *src_obj, H5VL_loc_params_t loc_params1, 
                     void *dst_obj, H5VL_loc_params_t loc_params2,
                     hbool_t copy_flag, hid_t lcpl, hid_t lapl, hid_t req);
    herr_t (*iterate) (void *obj, H5VL_loc_params_t loc_params, hbool_t recursive, 
                       H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, 
                       H5L_iterate_t op, void *op_data, hid_t req);
    herr_t (*get)   (void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type, hid_t req, va_list arguments);
    herr_t (*remove)(void *obj, H5VL_loc_params_t loc_params, hid_t req);
} H5VL_link_class_t;

/* H5O routines */
typedef struct H5VL_object_class_t {
    void  *(*open)  (void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type, hid_t req);
    herr_t (*copy)  (void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name, 
                     void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name,
                     hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
    herr_t (*visit) (void *obj, H5VL_loc_params_t loc_params, H5_index_t idx_type,
                     H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t req);
    //herr_t (*lookup)(hid_t loc_id, H5VL_loc_type_t lookup_type, void **location, hid_t req, va_list arguments);
    //herr_t (*free_loc)(void *location, hid_t req);
    herr_t (*get)   (void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type, hid_t req, va_list arguments);
    herr_t (*misc)  (void *obj, H5VL_loc_params_t loc_params, H5VL_object_misc_t misc_type, hid_t req, va_list arguments);
    herr_t (*optional)(void *obj, H5VL_loc_params_t loc_params, H5VL_object_optional_t op_type, hid_t req, va_list arguments);
    herr_t (*close) (void *obj, H5VL_loc_params_t loc_params, hid_t req);
} H5VL_object_class_t;

/* Class information for each VOL driver */
typedef struct H5VL_class_t {
    const char *name;
    herr_t  (*initialize)(void);
    herr_t  (*terminate)(void);
    H5VL_attr_class_t          attr_cls;
    H5VL_datatype_class_t      datatype_cls;
    H5VL_dataset_class_t       dataset_cls;
    H5VL_file_class_t          file_cls;
    H5VL_group_class_t         group_cls;
    H5VL_link_class_t          link_cls;
    H5VL_object_class_t        object_cls;
} H5VL_class_t;

/*
 * The main datatype for each plugin. Public fields common to all
 * plugins are declared here and the plugin appends private fields in
 * memory.
 */
struct H5VL_t {
    const H5VL_class_t *cls;            /*constant class info       */
    const char         *container_name; /* name of the underlying storage container */
    unsigned long       feature_flags;  /* VOL Driver feature Flags */
    int                 nrefs;          /* number of references by objects using this struct */
};

#if 0

/* FILE OBJECT ROUTINES */
H5_DLL void *H5VLfile_create(H5VL_t *vol_plugin, const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t req);
H5_DLL void *H5VLfile_open(H5VL_t *vol_plugin, const char *name, unsigned flags, hid_t fapl_id, hid_t req);
H5_DLL herr_t H5VLfile_flush(void *file, H5VL_t *vol_plugin, H5F_scope_t scope, hid_t req);
H5_DLL herr_t H5VLfile_misc(void *file, H5VL_t *vol_plugin, H5VL_file_misc_t misc_type, hid_t req, ...);
H5_DLL herr_t H5VLfile_optional(void *file, H5VL_t *vol_plugin, H5VL_file_optional_t optional_type, hid_t req, ...);
H5_DLL herr_t H5VLfile_get(void *file, H5VL_t *vol_plugin, H5VL_file_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLfile_close(void *file, H5VL_t *vol_plugin, hid_t req);

/* ATTRIBUTE OBJECT ROUTINES */
H5_DLL void *H5VLattr_create(void *obj, H5VL_t *vol_plugin, const char *attr_name, hid_t acpl, hid_t aapl, hid_t req);
H5_DLL void *H5VLattr_open(void *obj, H5VL_t *vol_plugin, H5VL_loc_params_t loc_params, const char *name, hid_t aapl, hid_t req);
H5_DLL herr_t H5VLattr_read(void *obj, H5VL_t *vol_plugin, hid_t dtype_id, void *buf, hid_t req);
H5_DLL herr_t H5VLattr_write(void *obj, H5VL_t *vol_plugin, hid_t dtype_id, const void *buf, hid_t req);
H5_DLL herr_t H5VLattr_get(void *obj, H5VL_t *vol_plugin, H5VL_attr_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLattr_remove(void *obj, H5VL_t *vol_plugin, H5VL_loc_params_t loc_params, const char *attr_name, hid_t req);
H5_DLL herr_t H5VLattr_close(void *obj, H5VL_t *vol_plugin, hid_t req);

/* DATASET OBJECT ROUTINES */
H5_DLL void *H5VLdataset_create(void *obj, H5VL_t *vol_plugin, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t req);
H5_DLL void *H5VLdataset_open(void *obj, H5VL_t *vol_plugin, const char *name, hid_t dapl_id, hid_t req);
H5_DLL herr_t H5VLdataset_read(void *obj, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, hid_t req);
H5_DLL herr_t H5VLdataset_write(void *obj, H5VL_t *vol_plugin, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, hid_t req);
H5_DLL herr_t H5VLdataset_set_extent(void *obj, H5VL_t *vol_plugin, const hsize_t size[], hid_t req);
H5_DLL herr_t H5VLdataset_get(void *obj, H5VL_t *vol_plugin, H5VL_dataset_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLdataset_close(void *obj, H5VL_t *vol_plugin, hid_t req);

/* DATATYPE OBJECT ROUTINES */
H5_DLL void *H5VLdatatype_commit(void *obj, H5VL_t *vol_plugin, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t req);
H5_DLL size_t H5VLdatatype_get_serial_size(void *obj, H5VL_t *vol_plugin,  const char *name, hid_t tapl_id, hid_t req);
H5_DLL void *H5VLdatatype_open(void *obj, H5VL_t *vol_plugin, void *serialized_type, const char *name, hid_t tapl_id, hid_t req);
H5_DLL herr_t H5VLdatatype_close(void *obj, H5VL_t *vol_plugin, hid_t req);

/* GROUP OBJECT ROUTINES */
H5_DLL H5VL_t *H5VLgroup_create(void *obj, H5VL_t *vol_plugin, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t req);
H5_DLL H5VL_t *H5VLgroup_open(void *obj, H5VL_t *vol_plugin, const char *name, hid_t gapl_id, hid_t req);
H5_DLL herr_t H5VLgroup_get(void *obj, H5VL_t *vol_plugin, H5VL_group_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLgroup_close(void *obj, H5VL_t *vol_plugin, hid_t req);

/* LINK ROUTINES */
H5_DLL herr_t H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, H5VL_t *vol_plugin, const char *link_name, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VLlink_move(void *src_obj, H5VL_t *src_plugin, const char *src_name, 
                            void *dst_obj, H5VL_t *dst_plugin, const char *dst_name, 
                            hbool_t copy_flag, hid_t lcpl_id, hid_t lapl_id, hid_t req);
H5_DLL herr_t H5VLlink_iterate(void *obj, H5VL_t *vol_plugin, const char *name, hbool_t recursive, H5_index_t idx_type, 
                               H5_iter_order_t order, hsize_t *idx, H5L_iterate_t op, void *op_data, 
                               hid_t lapl_id);
H5_DLL herr_t H5VLlink_get(void *obj, H5VL_t *vol_plugin, H5VL_link_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLlink_remove(void *obj, H5VL_t *vol_plugin, const char *name, void *udata, hid_t lapl_id, hid_t req);

/* OBJECT ROUTINES */
H5_DLL void *H5VLobject_open(void *obj, H5VL_t *vol_plugin, H5VL_loc_params_t params, hid_t req);
H5_DLL herr_t H5VLobject_copy(void *src_obj, H5VL_t *src_plugin, const char *src_name, 
                              void *dst_obj, H5VL_t *dst_plugin, const char *dst_name, 
                              hid_t ocpypl_id, hid_t lcpl_id, hid_t req);
H5_DLL herr_t H5VLobject_visit(void *obj, H5VL_t *vol_plugin, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, H5O_iterate_t op, void *op_data, hid_t lapl_id);
H5_DLL herr_t H5VLobject_get(void *obj, H5VL_t *vol_plugin, H5VL_object_get_t get_type, hid_t req, ...);
H5_DLL herr_t H5VLobject_misc(void *obj, H5VL_t *vol_plugin, H5VL_object_misc_t misc_type, hid_t req, ...);
H5_DLL herr_t H5VLobject_optional(void *obj, H5VL_t *vol_plugin, H5VL_object_misc_t optional_type, hid_t req, ...);
H5_DLL herr_t H5VLobject_close(void *obj, H5VL_t *vol_plugin, hid_t req);
/*
H5_DLL herr_t H5VLobject_lookup(void *obj, H5VL_t *vol_plugin, H5VL_loc_type_t lookup_type, void **location, hid_t req, ...);
H5_DLL herr_t H5VLobject_free_loc(void *obj, H5VL_t *vol_plugin, void *location, hid_t req);
*/

#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes */
H5_DLL hid_t H5VLregister(const H5VL_class_t *cls);
H5_DLL herr_t H5VLunregister(hid_t plugin_id);
H5_DLL htri_t H5VLis_registered(hid_t id);
H5_DLL ssize_t H5VLget_plugin_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL hid_t H5VLregister_object(void *obj, H5I_type_t obj_type, const H5VL_class_t *cls);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLpublic_H */
