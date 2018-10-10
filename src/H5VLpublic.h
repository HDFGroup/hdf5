/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains public declarations for the H5VL (VOL) module.
 */

#ifndef _H5VLpublic_H
#define _H5VLpublic_H

/* Public headers needed by this file */
#include "H5public.h"           /* Generic Functions                    */
#include "H5Apublic.h"          /* Attributes                           */
#include "H5ESpublic.h"         /* Event Stack                          */
#include "H5Fpublic.h"          /* Files                                */
#include "H5Lpublic.h"          /* Links                                */
#include "H5Opublic.h"          /* Objects                              */
#include "H5Rpublic.h"          /* References                           */


/*****************/
/* Public Macros */
/*****************/

/* Dataset creation property names */
#define H5VL_PROP_DSET_TYPE_ID              "dataset_type_id"
#define H5VL_PROP_DSET_SPACE_ID             "dataset_space_id"
#define H5VL_PROP_DSET_LCPL_ID              "dataset_lcpl_id"

/* Attribute creation property names */
#define H5VL_PROP_ATTR_TYPE_ID              "attr_type_id"
#define H5VL_PROP_ATTR_SPACE_ID             "attr_space_id"
#define H5VL_PROP_ATTR_LOC_PARAMS           "attr_location"

/* Link creation property names */
#define H5VL_PROP_LINK_TARGET               "target_location_object"
#define H5VL_PROP_LINK_TARGET_LOC_PARAMS    "target_params"
#define H5VL_PROP_LINK_TARGET_NAME          "target_name"
#define H5VL_PROP_LINK_TYPE                 "link type"
#define H5VL_PROP_LINK_UDATA                "udata"
#define H5VL_PROP_LINK_UDATA_SIZE           "udata size"

/* Group creation property names */
#define H5VL_PROP_GRP_LCPL_ID               "group_lcpl_id"

/* Default VOL driver value */
#define H5VL_VOL_DEFAULT                    0


/*******************/
/* Public Typedefs */
/*******************/

/* types for attribute GET callback */
typedef enum H5VL_attr_get_t {
    H5VL_ATTR_GET_ACPL,                     /* creation property list              */
    H5VL_ATTR_GET_INFO,                     /* info                                */
    H5VL_ATTR_GET_NAME,                     /* access property list                */
    H5VL_ATTR_GET_SPACE,                    /* dataspace                           */
    H5VL_ATTR_GET_STORAGE_SIZE,             /* storage size                        */
    H5VL_ATTR_GET_TYPE                      /* datatype                            */
} H5VL_attr_get_t;

/* types for attribute SPECFIC callback */
typedef enum H5VL_attr_specific_t {
    H5VL_ATTR_DELETE,                       /* H5Adelete(_by_name/idx)             */
    H5VL_ATTR_EXISTS,                       /* H5Aexists(_by_name)                 */
    H5VL_ATTR_ITER,                         /* H5Aiterate(_by_name)                */
    H5VL_ATTR_RENAME                        /* H5Arename(_by_name)                 */
} H5VL_attr_specific_t;

/* types for dataset GET callback */
typedef enum H5VL_dataset_get_t {
    H5VL_DATASET_GET_DAPL,                  /* access property list                */
    H5VL_DATASET_GET_DCPL,                  /* creation property list              */
    H5VL_DATASET_GET_OFFSET,                /* offset                              */
    H5VL_DATASET_GET_SPACE,                 /* dataspace                           */
    H5VL_DATASET_GET_SPACE_STATUS,          /* space  status                       */
    H5VL_DATASET_GET_STORAGE_SIZE,          /* storage size                        */
    H5VL_DATASET_GET_TYPE                   /* datatype                            */
} H5VL_dataset_get_t;

/* types for dataset SPECFIC callback */
typedef enum H5VL_dataset_specific_t {
    H5VL_DATASET_SET_EXTENT,                /* H5Dset_extent                       */
    H5VL_DATASET_FLUSH,                     /* H5Dflush                            */
    H5VL_DATASET_REFRESH                    /* H5Drefresh                          */
} H5VL_dataset_specific_t;

/* types for datatype GET callback */
typedef enum H5VL_datatype_get_t {
    H5VL_DATATYPE_GET_BINARY,               /* get serialized form of transient type */
    H5VL_DATATYPE_GET_TCPL	                /* datatype creation property list	   */
} H5VL_datatype_get_t;

/* types for datatype SPECFIC callback */
typedef enum H5VL_datatype_specific_t {
    H5VL_DATATYPE_FLUSH,
    H5VL_DATATYPE_REFRESH
} H5VL_datatype_specific_t;

/* types for file GET callback */
typedef enum H5VL_file_get_t {
    H5VL_FILE_GET_FAPL,                     /* file access property list	*/
    H5VL_FILE_GET_FCPL,	                    /* file creation property list	*/
    H5VL_FILE_GET_INTENT,	                /* file intent           		*/
    H5VL_FILE_GET_NAME,	                    /* file name             		*/
    H5VL_FILE_GET_OBJ_COUNT,	            /* object count in file	       	*/
    H5VL_FILE_GET_OBJ_IDS,	                /* object ids in file     		*/
    H5VL_OBJECT_GET_FILE                    /* retrieve or resurrect file of object */
} H5VL_file_get_t;

/* types for file SPECIFIC callback */
typedef enum H5VL_file_specific_t {
    H5VL_FILE_FLUSH,                        /* Flush file                       */
    H5VL_FILE_IS_ACCESSIBLE,                /* Check if a file is accessible    */
    H5VL_FILE_MOUNT,                        /* Mount a file                     */
    H5VL_FILE_UNMOUNT                       /* Un-Mount a file                  */
} H5VL_file_specific_t;

/* types for group GET callback */
typedef enum H5VL_group_get_t {
    H5VL_GROUP_GET_GCPL,	            /* group creation property list	*/
    H5VL_GROUP_GET_INFO 	            /* group info             		*/
} H5VL_group_get_t;

/* types for group SPECFIC callback */
typedef enum H5VL_group_specific_t {
    H5VL_GROUP_FLUSH,
    H5VL_GROUP_REFRESH
} H5VL_group_specific_t;

/* link create types for VOL */
typedef enum H5VL_link_create_type_t {
    H5VL_LINK_CREATE_HARD,
    H5VL_LINK_CREATE_SOFT,
    H5VL_LINK_CREATE_UD
} H5VL_link_create_type_t;

/* types for link GET callback */
typedef enum H5VL_link_get_t {
    H5VL_LINK_GET_INFO,        /* link info         		    */
    H5VL_LINK_GET_NAME,	       /* link name                         */
    H5VL_LINK_GET_VAL          /* link value                        */
} H5VL_link_get_t;

/* types for link SPECIFIC callback */
typedef enum H5VL_link_specific_t {
    H5VL_LINK_DELETE,          /* H5Ldelete(_by_idx)                */
    H5VL_LINK_EXISTS,          /* link existence                    */
    H5VL_LINK_ITER             /* H5Literate/visit(_by_name)              */
} H5VL_link_specific_t;

/* types for object GET callback */
typedef enum H5VL_object_get_t {
    H5VL_REF_GET_NAME,                 /* object name                       */
    H5VL_REF_GET_REGION,               /* dataspace of region               */
    H5VL_REF_GET_TYPE                  /* type of object                    */
} H5VL_object_get_t;

/* types for object SPECIFIC callback */
typedef enum H5VL_object_specific_t {
    H5VL_OBJECT_CHANGE_REF_COUNT,      /* H5Oincr/decr_refcount              */
    H5VL_OBJECT_EXISTS,                /* H5Oexists_by_name                  */
    H5VL_OBJECT_VISIT,                 /* H5Ovisit(_by_name)                 */
    H5VL_REF_CREATE,                   /* H5Rcreate                          */
    H5VL_OBJECT_FLUSH,
    H5VL_OBJECT_REFRESH
} H5VL_object_specific_t;

/* types for different ways that objects are located in an HDF5 container */
typedef enum H5VL_loc_type_t {
    H5VL_OBJECT_BY_SELF,
    H5VL_OBJECT_BY_NAME,
    H5VL_OBJECT_BY_IDX,
    H5VL_OBJECT_BY_ADDR,
    H5VL_OBJECT_BY_REF
} H5VL_loc_type_t;

struct H5VL_loc_by_name {
    const char *name;
    hid_t lapl_id;
};

struct H5VL_loc_by_idx {
    const char *name;
    H5_index_t idx_type;
    H5_iter_order_t order;
    hsize_t n;
    hid_t lapl_id;
};

struct H5VL_loc_by_addr {
    haddr_t addr;
};

struct H5VL_loc_by_ref {
    H5R_type_t ref_type;
    const void *_ref;
    hid_t lapl_id;
};

/* Structure to hold parameters for object locations. 
 * either: BY_ADDR, BY_ID, BY_NAME, BY_IDX, BY_REF
 *
 * Note: Leave loc_by_addr as the first union member so we
 *       can perform the simplest initialization of the struct
 *       without raising warnings.
 */
typedef struct H5VL_loc_params_t {
    H5I_type_t      obj_type;
    H5VL_loc_type_t type;
    union{
        struct H5VL_loc_by_addr loc_by_addr;
        struct H5VL_loc_by_name loc_by_name;
        struct H5VL_loc_by_idx  loc_by_idx;
        struct H5VL_loc_by_ref  loc_by_ref;
    } loc_data;
} H5VL_loc_params_t;

/* H5A routines */
typedef struct H5VL_attr_class_t {
    void *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *attr_name,
                    hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, H5VL_loc_params_t loc_params, const char *attr_name,
                  hid_t aapl_id, hid_t dxpl_id, void **req);
    herr_t (*read)(void *attr, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req);
    herr_t (*write)(void *attr, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_loc_params_t loc_params, H5VL_attr_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *attr, hid_t dxpl_id, void **req);
} H5VL_attr_class_t;

/* H5D routines */
typedef struct H5VL_dataset_class_t {
    void *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *name,
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, H5VL_loc_params_t loc_params, const char *name,
                  hid_t dapl_id, hid_t dxpl_id, void **req);
    herr_t (*read)(void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                   hid_t xfer_plist_id, void * buf, void **req);
    herr_t (*write)(void *dset, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id,
                    hid_t xfer_plist_id, const void * buf, void **req);
    herr_t (*get)(void *obj, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_dataset_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *dset, hid_t dxpl_id, void **req);
} H5VL_dataset_class_t;

/* H5T routines*/
typedef struct H5VL_datatype_class_t {
    void *(*commit)(void *obj, H5VL_loc_params_t loc_params, const char *name, hid_t type_id,
                    hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, H5VL_loc_params_t loc_params, const char * name,
                  hid_t tapl_id, hid_t dxpl_id, void **req);
    herr_t (*get)   (void *obj, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_datatype_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *dt, hid_t dxpl_id, void **req);
} H5VL_datatype_class_t;

/* H5F routines */
typedef struct H5VL_file_class_t {
    void *(*create)(const char *name, unsigned flags, hid_t fcpl_id,
                    hid_t fapl_id, hid_t dxpl_id, void **req);
    void *(*open)(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_file_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *file, hid_t dxpl_id, void **req);
} H5VL_file_class_t;

/* H5G routines */
typedef struct H5VL_group_class_t {
    void *(*create)(void *obj, H5VL_loc_params_t loc_params, const char *name,
                    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, H5VL_loc_params_t loc_params, const char *name,
                  hid_t gapl_id, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_group_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *grp, hid_t dxpl_id, void **req);
} H5VL_group_class_t;

/* H5L routines */
typedef struct H5VL_link_class_t {
    herr_t (*create)(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params,
                     hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
    herr_t (*copy)(void *src_obj, H5VL_loc_params_t loc_params1,
                   void *dst_obj, H5VL_loc_params_t loc_params2,
                   hid_t lcpl, hid_t lapl, hid_t dxpl_id, void **req);
    herr_t (*move)(void *src_obj, H5VL_loc_params_t loc_params1,
                   void *dst_obj, H5VL_loc_params_t loc_params2,
                   hid_t lcpl, hid_t lapl, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_loc_params_t loc_params, H5VL_link_get_t get_type,
                  hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_loc_params_t loc_params, H5VL_link_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
} H5VL_link_class_t;

/* H5O routines */
typedef struct H5VL_object_class_t {
    void *(*open)(void *obj, H5VL_loc_params_t loc_params, H5I_type_t *opened_type,
                  hid_t dxpl_id, void **req);
    herr_t (*copy)(void *src_obj, H5VL_loc_params_t loc_params1, const char *src_name,
                   void *dst_obj, H5VL_loc_params_t loc_params2, const char *dst_name,
                   hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_loc_params_t loc_params, H5VL_object_get_t get_type,
                  hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_loc_params_t loc_params, H5VL_object_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
} H5VL_object_class_t;

/* H5AO routines */
typedef struct H5VL_async_class_t {
    herr_t (*cancel)(void **, H5ES_status_t *);
    herr_t (*test)  (void **, H5ES_status_t *);
    herr_t (*wait)  (void **, H5ES_status_t *);
} H5VL_async_class_t;

/* VOL category (internal, external, etc.)
 * XXX: This is intended to replace the H5VL_class_value_t struct, which seems
 *      difficult to manage. It's currently unused so I don't break struct
 *      compatibility with existing VOLs.
 */
typedef enum H5VL_category_t {
    H5VL_INTERNAL,      /* Internal VOL driver */
    H5VL_EXTERNAL       /* External VOL driver (plugin) */
} H5VL_category_t;

/* enum value to identify the class of a VOL driver (mostly for comparison purposes) */
typedef enum H5VL_class_value_t {
    H5_VOL_NATIVE = 0,              /* This should be first */
    H5_VOL_MAX_LIB_VALUE = 128      /* This should be last */
} H5VL_class_value_t;

/* Class information for each VOL driver */
/* XXX: We should consider adding a UUID/GUID field to this struct
 *      as well as a H5VLregister_by_uuid() API call for people who
 *      really care about getting a particular VOL driver.
 * XXX: We should also consider adding enough information so that
 *      files can be opened without specifying the VOL driver.
 *      e.g.: If we stored a UUID and version, we could search for
 *      a matching VOL driver so a user did not have to make any
 *      H5VL calls.
 */
typedef struct H5VL_class_t {
                                                    /* XXX: How do we identify unique VOL drivers?
                                                     *      This is unclear, but for now we'll keep
                                                     *      all the ID fields from the original VOL
                                                     *      branch.
                                                     */
    unsigned int version;                           /* VOL driver version #
                                                     * XXX: Is this supposed to be a VOL driver
                                                     *      version number or a VOL API version
                                                     *      number? Maybe we need both?
                                                     */
    H5VL_class_value_t value;                       /* value to identify driver                     */
    const char *name;                               /* Plugin name (MUST be unique!)                */
    herr_t  (*initialize)(hid_t vipl_id);           /* Plugin initialization callback               */
    herr_t  (*terminate)(hid_t vtpl_id);            /* Plugin termination callback                  */
    size_t  fapl_size;                              /* size of the vol info in the fapl property    */
    void *  (*fapl_copy)(const void *info);         /* callback to create a copy of the vol info    */
    herr_t  (*fapl_free)(void *info);               /* callback to release the vol info copy        */

    /* Data Model */
    H5VL_attr_class_t          attr_cls;            /* attribute class callbacks    */
    H5VL_dataset_class_t       dataset_cls;         /* dataset class callbacks      */
    H5VL_datatype_class_t      datatype_cls;        /* datatype class callbacks     */
    H5VL_file_class_t          file_cls;            /* file class callbacks         */
    H5VL_group_class_t         group_cls;           /* group class callbacks        */
    H5VL_link_class_t          link_cls;            /* link class callbacks         */
    H5VL_object_class_t        object_cls;          /* object class callbacks       */

    /* Services */
    H5VL_async_class_t         async_cls;         /* asynchronous class callbacks */
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments); /* Optional callback */
} H5VL_class_t;


/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* VOL Driver/Plugin Functionality */
H5_DLL herr_t H5VLinitialize(hid_t driver_id, hid_t vipl_id);
H5_DLL herr_t H5VLterminate(hid_t driver_id, hid_t vtpl_id);
H5_DLL herr_t H5VLclose(hid_t driver_id);
H5_DLL hid_t H5VLregister(const H5VL_class_t *cls);
H5_DLL hid_t H5VLregister_by_name(const char *driver_name);
H5_DLL herr_t H5VLunregister(hid_t driver_id);
H5_DLL htri_t H5VLis_registered(const char *name);
H5_DLL hid_t H5VLget_driver_id(const char *name);
H5_DLL ssize_t H5VLget_driver_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL hid_t H5VLobject_register(void *obj, H5I_type_t obj_type, hid_t driver_id);
H5_DLL void *H5VLobject(hid_t id);
H5_DLL herr_t H5VLget_object(hid_t obj_id, void **obj);

/* Attributes */
H5_DLL void *H5VLattr_create(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLattr_open(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_read(void *attr, hid_t driver_id, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_write(void *attr, hid_t driver_id, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_get(void *obj, hid_t driver_id, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_specific(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_close(void *attr, hid_t driver_id, hid_t dxpl_id, void **req);

/* Datasets */
H5_DLL void *H5VLdataset_create(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLdataset_open(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLdataset_read(void *dset, hid_t driver_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, void **req);
H5_DLL herr_t H5VLdataset_write(void *dset, hid_t driver_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
H5_DLL herr_t H5VLdataset_get(void *dset, hid_t driver_id, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_specific(void *obj, hid_t driver_id, H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_close(void *dset, hid_t driver_id, hid_t dxpl_id, void **req);

/* Files */
H5_DLL void *H5VLfile_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLfile_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLfile_get(void *file, hid_t driver_id, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_specific(void *obj, hid_t driver_id, H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_close(void *file, hid_t driver_id, hid_t dxpl_id, void **req);

/* Groups */
H5_DLL void *H5VLgroup_create(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLgroup_open(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLgroup_get(void *obj, hid_t driver_id, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_specific(void *obj, hid_t driver_id, H5VL_group_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_close(void *grp, hid_t driver_id, hid_t dxpl_id, void **req);

/* Links */
H5_DLL herr_t H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_copy(void *src_obj, H5VL_loc_params_t loc_params1,
                             void *dst_obj, H5VL_loc_params_t loc_params2, hid_t driver_id,
                             hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_move(void *src_obj, H5VL_loc_params_t loc_params1,
                             void *dst_obj, H5VL_loc_params_t loc_params2, hid_t driver_id,
                             hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_get(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLlink_specific(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLlink_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);

/* Objects */
H5_DLL void *H5VLobject_open(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLobject_copy(void *src_obj, H5VL_loc_params_t loc_params1, hid_t driver_id1, const char *src_name,
                               void *dst_obj, H5VL_loc_params_t loc_params2, hid_t driver_id2, const char *dst_name,
                               hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLobject_get(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLobject_specific(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLobject_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);

/* Datatypes */
H5_DLL void *H5VLdatatype_commit(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLdatatype_open(void *obj, H5VL_loc_params_t loc_params, hid_t driver_id, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLdatatype_get(void *dt, hid_t driver_id, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_specific(void *obj, hid_t driver_id, H5VL_datatype_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_optional(void *obj, hid_t driver_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_close(void *dt, hid_t driver_id, hid_t dxpl_id, void **req);

/* Asynchronous Requests */
H5_DLL herr_t H5VLrequest_cancel(void **req, hid_t driver_id, H5ES_status_t *status);
H5_DLL herr_t H5VLrequest_test(void **req, hid_t driver_id, H5ES_status_t *status);
H5_DLL herr_t H5VLrequest_wait(void **req, hid_t driver_id, H5ES_status_t *status);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLpublic_H */
