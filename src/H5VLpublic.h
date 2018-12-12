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

/* Default VOL connector value */
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
    H5VL_FILE_GET_INTENT,	            /* file intent           		*/
    H5VL_FILE_GET_NAME,	                    /* file name             		*/
    H5VL_FILE_GET_OBJ_COUNT,	            /* object count in file	       	*/
    H5VL_FILE_GET_OBJ_IDS,	            /* object ids in file     		*/
    H5VL_FILE_GET_MIN_DSET_OHDR_FLAG        /* minimize dataset object headers? */
} H5VL_file_get_t;

/* types for file SPECIFIC callback */
typedef enum H5VL_file_specific_t {
    H5VL_FILE_FLUSH,                        /* Flush file                       */
    H5VL_FILE_REOPEN,                       /* Reopen the file                  */
    H5VL_FILE_MOUNT,                        /* Mount a file                     */
    H5VL_FILE_UNMOUNT,                      /* Unmount a file                   */
    H5VL_FILE_IS_ACCESSIBLE,                /* Check if a file is accessible    */
    H5VL_FILE_CACHE_VOL_CONN                /* Cache VOL connector ID & info    */
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
    H5VL_REF_GET_NAME,                 /* object name, for reference        */
    H5VL_REF_GET_REGION,               /* dataspace of region               */
    H5VL_REF_GET_TYPE,                 /* type of object                    */
    H5VL_ID_GET_NAME                   /* object name, for hid_t            */
} H5VL_object_get_t;

/* types for object SPECIFIC callback */
typedef enum H5VL_object_specific_t {
    H5VL_OBJECT_CHANGE_REF_COUNT,       /* H5Oincr/decr_refcount             */
    H5VL_OBJECT_EXISTS,                 /* H5Oexists_by_name                 */
    H5VL_OBJECT_VISIT,                  /* H5Ovisit(_by_name)                */
    H5VL_REF_CREATE,                    /* H5Rcreate                         */
    H5VL_OBJECT_FLUSH,                  /* H5{D|G|O|T}flush                  */
    H5VL_OBJECT_REFRESH                 /* H5{D|G|O|T}refresh                */
} H5VL_object_specific_t;

/* types for async request SPECIFIC callback */
typedef enum H5VL_request_specific_t {
    H5VL_REQUEST_WAITANY,               /* Wait until any request completes */
    H5VL_REQUEST_WAITSOME,              /* Wait until at least one requesst completes */
    H5VL_REQUEST_WAITALL                /* Wait until all requests complete */
} H5VL_request_specific_t;

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
    void *(*create)(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name,
                    hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, const H5VL_loc_params_t *loc_params, const char *attr_name,
                  hid_t aapl_id, hid_t dxpl_id, void **req);
    herr_t (*read)(void *attr, hid_t mem_type_id, void *buf, hid_t dxpl_id, void **req);
    herr_t (*write)(void *attr, hid_t mem_type_id, const void *buf, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, const H5VL_loc_params_t *loc_params, H5VL_attr_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *attr, hid_t dxpl_id, void **req);
} H5VL_attr_class_t;

/* H5D routines */
typedef struct H5VL_dataset_class_t {
    void *(*create)(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                    hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
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
    void *(*commit)(void *obj, const H5VL_loc_params_t *loc_params, const char *name, hid_t type_id,
                    hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, const H5VL_loc_params_t *loc_params, const char * name,
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
    void *(*create)(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                    hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
    void *(*open)(void *obj, const H5VL_loc_params_t *loc_params, const char *name,
                  hid_t gapl_id, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, H5VL_group_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*close) (void *grp, hid_t dxpl_id, void **req);
} H5VL_group_class_t;

/* H5L routines */
typedef struct H5VL_link_class_t {
    herr_t (*create)(H5VL_link_create_type_t create_type, void *obj, const H5VL_loc_params_t *loc_params,
                     hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
    herr_t (*copy)(void *src_obj, const H5VL_loc_params_t *loc_params1,
                   void *dst_obj, const H5VL_loc_params_t *loc_params2,
                   hid_t lcpl, hid_t lapl, hid_t dxpl_id, void **req);
    herr_t (*move)(void *src_obj, const H5VL_loc_params_t *loc_params1,
                   void *dst_obj, const H5VL_loc_params_t *loc_params2,
                   hid_t lcpl, hid_t lapl, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, const H5VL_loc_params_t *loc_params, H5VL_link_get_t get_type,
                  hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, const H5VL_loc_params_t *loc_params, H5VL_link_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
} H5VL_link_class_t;

/* H5O routines */
typedef struct H5VL_object_class_t {
    void *(*open)(void *obj, const H5VL_loc_params_t *loc_params, H5I_type_t *opened_type,
                  hid_t dxpl_id, void **req);
    herr_t (*copy)(void *src_obj, const H5VL_loc_params_t *loc_params1, const char *src_name,
                   void *dst_obj, const H5VL_loc_params_t *loc_params2, const char *dst_name,
                   hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
    herr_t (*get)(void *obj, const H5VL_loc_params_t *loc_params, H5VL_object_get_t get_type,
                  hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*specific)(void *obj, const H5VL_loc_params_t *loc_params, H5VL_object_specific_t specific_type,
                       hid_t dxpl_id, void **req, va_list arguments);
    herr_t (*optional)(void *obj, hid_t dxpl_id, void **req, va_list arguments);
} H5VL_object_class_t;

/* Asynchronous request 'notify' callback */
typedef herr_t (*H5VL_request_notify_t)(void *ctx, H5ES_status_t status);

/* Async request operation routines */
typedef struct H5VL_request_class_t {
    herr_t (*wait)(void *req, uint64_t timeout, H5ES_status_t *status);
    herr_t (*notify)(void *req, H5VL_request_notify_t cb, void *ctx);
    herr_t (*cancel)(void *req);
    herr_t (*specific)(void *req, H5VL_request_specific_t specific_type, va_list arguments);
    herr_t (*optional)(void *req, va_list arguments);
    herr_t (*free)(void *req);
} H5VL_request_class_t;

/*
 * VOL connector identifiers.  Values 0 through 255 are for connectors defined
 * by the HDF5 library.  Values 256 through 511 are available for testing new
 * filters.  Subsequent values should be obtained from the HDF5 development
 * team at help@hdfgroup.org.
 */
typedef int H5VL_class_value_t;

/* VOL connector identifier values */
#define H5_VOL_INVALID  (-1)    /* Invalid ID for VOL connector iD */
#define H5_VOL_NATIVE   0       /* Native HDF5 file formnat VOL connector */
#define H5_VOL_RESERVED 256     /* VOL connector IDs below this value are reserved for library use */
#define H5_VOL_MAX	65535	/* Maximum VOL connector ID */

/* Capability flags for connector */
#define H5VL_CAP_FLAG_NONE              0       /* No special connector capabilities */
#define H5VL_CAP_FLAG_THREADSAFE        0x01    /* Connector is threadsafe */

/* Class information for each VOL connector */
typedef struct H5VL_class_t {
    unsigned int version;                           /* VOL connector class struct version #         */
    H5VL_class_value_t value;                       /* Value to identify connector                  */
    const char *name;                               /* Connector name (MUST be unique!)             */
    unsigned cap_flags;                             /* Capability flags for connector               */
    herr_t  (*initialize)(hid_t vipl_id);           /* Connector initialization callback            */
    herr_t  (*terminate)(void);                     /* Connector termination callback               */
    size_t  info_size;                              /* Size of the VOL info                         */
    void *  (*info_copy)(const void *info);         /* Callback to create a copy of the VOL info    */
    herr_t  (*info_cmp)(int *cmp_value, const void *info1, const void *info2); /* Callback to compare VOL info */
    herr_t  (*info_free)(void *info);               /* Callback to release the VOL info copy        */
    herr_t  (*info_to_str)(const void *info, char **str); /* Callback to serialize connector's info into a string */
    herr_t  (*str_to_info)(const char *str, void **info); /* Callback to deserialize a string into connector's info */
    void *  (*get_object)(const void *obj);         /* Callback to retrieve underlying object       */
    herr_t  (*get_wrap_ctx)(const void *obj, void **wrap_ctx); /* Callback to retrieve the object wrapping context for the connector */
    void*   (*wrap_object)(void *obj, void *wrap_ctx); /* Callback to wrap a library object */
    herr_t  (*free_wrap_ctx)(void *wrap_ctx);       /* Callback to release the object wrapping context for the connector */

    /* Data Model */
    H5VL_attr_class_t          attr_cls;            /* attribute class callbacks    */
    H5VL_dataset_class_t       dataset_cls;         /* dataset class callbacks      */
    H5VL_datatype_class_t      datatype_cls;        /* datatype class callbacks     */
    H5VL_file_class_t          file_cls;            /* file class callbacks         */
    H5VL_group_class_t         group_cls;           /* group class callbacks        */
    H5VL_link_class_t          link_cls;            /* link class callbacks         */
    H5VL_object_class_t        object_cls;          /* object class callbacks       */

    /* Services */
    H5VL_request_class_t       request_cls;         /* asynchronous request class callbacks */

    /* Catch-all */
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

/* VOL Connector Functionality */
H5_DLL hid_t H5VLregister_connector(const H5VL_class_t *cls, hid_t vipl_id);
H5_DLL hid_t H5VLregister_connector_by_name(const char *connector_name, hid_t vipl_id);
H5_DLL hid_t H5VLregister_connector_by_value(H5VL_class_value_t connector_value, hid_t vipl_id);
H5_DLL htri_t H5VLis_connector_registered(const char *name);
H5_DLL hid_t H5VLget_connector_id(const char *name);
H5_DLL ssize_t H5VLget_connector_name(hid_t id, char *name/*out*/, size_t size);
H5_DLL herr_t H5VLclose(hid_t connector_id);
H5_DLL herr_t H5VLunregister_connector(hid_t connector_id);


/*****************************************************************************
 * VOL callback wrappers and helper routines, for _VOL_connector_ authors only! *
 * (Not part of the public API for _application_ developers)                 *
 *****************************************************************************/

/* Helper routines for VOL connector authors */
H5_DLL herr_t H5VLcmp_connector_cls(int *cmp, hid_t connector_id1, hid_t connector_id2);


/* Public wrappers for generic callbacks */
H5_DLL herr_t H5VLinitialize(hid_t connector_id, hid_t vipl_id);
H5_DLL herr_t H5VLterminate(hid_t connector_id);
H5_DLL herr_t H5VLget_cap_flags(hid_t connector_id, unsigned *cap_flags);
H5_DLL herr_t H5VLget_value(hid_t connector_id, H5VL_class_value_t *conn_value);
H5_DLL herr_t H5VLcopy_connector_info(hid_t connector_id, void **dst_vol_info, void *src_vol_info);
H5_DLL herr_t H5VLcmp_connector_info(int *cmp, hid_t connector_id, const void *info1,
    const void *info2);
H5_DLL herr_t H5VLfree_connector_info(hid_t connector_id, void *vol_info);
H5_DLL herr_t H5VLconnector_info_to_str(const void *info, hid_t connector_id, char **str);
H5_DLL herr_t H5VLconnector_str_to_info(const char *str, hid_t connector_id, void **info);
H5_DLL void *H5VLget_object(void *obj, hid_t connector_id);
H5_DLL herr_t H5VLget_wrap_ctx(void *obj, hid_t connector_id, void **wrap_ctx);
H5_DLL void *H5VLwrap_object(void *obj, hid_t connector_id, void *wrap_ctx);
H5_DLL herr_t H5VLfree_wrap_ctx(void *wrap_ctx, hid_t connector_id);

/* Public wrappers for attribute callbacks */
H5_DLL void *H5VLattr_create(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *attr_name, hid_t acpl_id, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLattr_open(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t aapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_read(void *attr, hid_t connector_id, hid_t dtype_id, void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_write(void *attr, hid_t connector_id, hid_t dtype_id, const void *buf, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLattr_get(void *obj, hid_t connector_id, H5VL_attr_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_specific(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5VL_attr_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLattr_close(void *attr, hid_t connector_id, hid_t dxpl_id, void **req);

/* Public wrappers for dataset callbacks */
H5_DLL void *H5VLdataset_create(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t dcpl_id, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLdataset_open(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t dapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLdataset_read(void *dset, hid_t connector_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, void *buf, void **req);
H5_DLL herr_t H5VLdataset_write(void *dset, hid_t connector_id, hid_t mem_type_id, hid_t mem_space_id, hid_t file_space_id, hid_t plist_id, const void *buf, void **req);
H5_DLL herr_t H5VLdataset_get(void *dset, hid_t connector_id, H5VL_dataset_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_specific(void *obj, hid_t connector_id, H5VL_dataset_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdataset_close(void *dset, hid_t connector_id, hid_t dxpl_id, void **req);

/* Public wrappers for file callbacks */
H5_DLL void *H5VLfile_create(const char *name, unsigned flags, hid_t fcpl_id, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLfile_open(const char *name, unsigned flags, hid_t fapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLfile_get(void *file, hid_t connector_id, H5VL_file_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_specific(void *obj, hid_t connector_id, H5VL_file_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLfile_close(void *file, hid_t connector_id, hid_t dxpl_id, void **req);

/* Public wrappers for group callbacks */
H5_DLL void *H5VLgroup_create(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t gcpl_id, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLgroup_open(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t gapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLgroup_get(void *obj, hid_t connector_id, H5VL_group_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_specific(void *obj, hid_t connector_id, H5VL_group_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLgroup_close(void *grp, hid_t connector_id, hid_t dxpl_id, void **req);

/* Public wrappers for link callbacks */
H5_DLL herr_t H5VLlink_create(H5VL_link_create_type_t create_type, void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_copy(void *src_obj, const H5VL_loc_params_t *loc_params1,
                             void *dst_obj, const H5VL_loc_params_t *loc_params2, hid_t connector_id,
                             hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_move(void *src_obj, const H5VL_loc_params_t *loc_params1,
                             void *dst_obj, const H5VL_loc_params_t *loc_params2, hid_t connector_id,
                             hid_t lcpl_id, hid_t lapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLlink_get(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5VL_link_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLlink_specific(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5VL_link_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLlink_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);

/* Public wrappers for object callbacks */
H5_DLL void *H5VLobject_open(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5I_type_t *opened_type, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLobject_copy(void *src_obj, const H5VL_loc_params_t *loc_params1, const char *src_name,
                               void *dst_obj, const H5VL_loc_params_t *loc_params2, const char *dst_name,
                               hid_t connector_id, hid_t ocpypl_id, hid_t lcpl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLobject_get(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5VL_object_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLobject_specific(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, H5VL_object_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLobject_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);

/* Public wrappers for named datatype callbacks */
H5_DLL void *H5VLdatatype_commit(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t type_id, hid_t lcpl_id, hid_t tcpl_id, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL void *H5VLdatatype_open(void *obj, const H5VL_loc_params_t *loc_params, hid_t connector_id, const char *name, hid_t tapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VLdatatype_get(void *dt, hid_t connector_id, H5VL_datatype_get_t get_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_specific(void *obj, hid_t connector_id, H5VL_datatype_specific_t specific_type, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_optional(void *obj, hid_t connector_id, hid_t dxpl_id, void **req, va_list arguments);
H5_DLL herr_t H5VLdatatype_close(void *dt, hid_t connector_id, hid_t dxpl_id, void **req);

/* Public wrappers for asynchronous request callbacks */
H5_DLL herr_t H5VLrequest_wait(void *req, hid_t connector_id, uint64_t timeout, H5ES_status_t *status);
H5_DLL herr_t H5VLrequest_notify(void *req, hid_t connector_id, H5VL_request_notify_t cb, void *ctx);
H5_DLL herr_t H5VLrequest_cancel(void *req, hid_t connector_id);
H5_DLL herr_t H5VLrequest_specific(void *req, hid_t connector_id, H5VL_request_specific_t specific_type, va_list arguments);
H5_DLL herr_t H5VLrequest_optional(void *req, hid_t connector_id, va_list arguments);
H5_DLL herr_t H5VLrequest_free(void *req, hid_t connector_id);

#ifdef __cplusplus
}
#endif

#endif /* _H5VLpublic_H */

