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
#include "H5public.h"  /* Generic Functions                    */
#include "H5Ipublic.h" /* IDs                                  */

/*****************/
/* Public Macros */
/*****************/

/* VOL connector identifier values
 * These are H5VL_class_value_t values, NOT hid_t values!
 */
#define H5_VOL_INVALID  (-1)  /* Invalid ID for VOL connector ID */
#define H5_VOL_NATIVE   0     /* Native HDF5 file format VOL connector */
#define H5_VOL_RESERVED 256   /* VOL connector IDs below this value are reserved for library use */
#define H5_VOL_MAX      65535 /* Maximum VOL connector ID */

/* Flags to return from H5VLquery_optional API and 'opt_query' callbacks */
/* Note: Operations which access multiple objects' data or metadata in a
 *      container should be registered as file-level optional operations.
 *      (e.g. "H5Dwrite_multi" takes a list of datasets to write data to, so
 *      a VOL connector that implemented it should register it as an optional
 *      file operation, and pass-through VOL connectors that are stacked above
 *      the connector that registered it should assume that dataset elements
 *      for _any_ dataset in the file could be written to)
 */
#define H5VL_OPT_QUERY_SUPPORTED        0x0001  /* VOL connector supports this operation */
#define H5VL_OPT_QUERY_READ_DATA        0x0002  /* Operation reads data for object */
#define H5VL_OPT_QUERY_WRITE_DATA       0x0004  /* Operation writes data for object */
#define H5VL_OPT_QUERY_QUERY_METADATA   0x0008  /* Operation reads metadata for object */
#define H5VL_OPT_QUERY_MODIFY_METADATA  0x0010  /* Operation modifies metadata for object */
#define H5VL_OPT_QUERY_COLLECTIVE       0x0020  /* Operation is collective (operations without this flag are assumed to be independent) */
#define H5VL_OPT_QUERY_NO_ASYNC         0x0040  /* Operation may NOT be executed asynchronously */
#define H5VL_OPT_QUERY_MULTI_OBJ        0x0080  /* Operation involves multiple objects */

/*******************/
/* Public Typedefs */
/*******************/

/*
 * VOL connector identifiers.  Values 0 through 255 are for connectors defined
 * by the HDF5 library.  Values 256 through 511 are available for testing new
 * connectors. Subsequent values should be obtained from the HDF5 development
 * team at help@hdfgroup.org.
 */
typedef int H5VL_class_value_t;

/* Enum type for each VOL subclass */
/* (Used for various queries, etc) */
typedef enum H5VL_subclass_t {
    H5VL_SUBCLS_NONE,     /* Operations outside of a subclass */
    H5VL_SUBCLS_INFO,     /* 'Info' subclass */
    H5VL_SUBCLS_WRAP,     /* 'Wrap' subclass */
    H5VL_SUBCLS_ATTR,     /* 'Attribute' subclass */
    H5VL_SUBCLS_DATASET,  /* 'Dataset' subclass */
    H5VL_SUBCLS_DATATYPE, /* 'Named datatype' subclass */
    H5VL_SUBCLS_FILE,     /* 'File' subclass */
    H5VL_SUBCLS_GROUP,    /* 'Group' subclass */
    H5VL_SUBCLS_LINK,     /* 'Link' subclass */
    H5VL_SUBCLS_OBJECT,   /* 'Object' subclass */
    H5VL_SUBCLS_REQUEST,  /* 'Request' subclass */
    H5VL_SUBCLS_BLOB,     /* 'Blob' subclass */
    H5VL_SUBCLS_TOKEN     /* 'Token' subclass */
                          /* NOTE: if more operations are added, the
                           * H5VL_opt_vals_g[] array size should be updated.
                           */
} H5VL_subclass_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t   H5VLregister_connector_by_name(const char *connector_name, hid_t vipl_id);
H5_DLL hid_t   H5VLregister_connector_by_value(H5VL_class_value_t connector_value, hid_t vipl_id);
H5_DLL htri_t  H5VLis_connector_registered_by_name(const char *name);
H5_DLL htri_t  H5VLis_connector_registered_by_value(H5VL_class_value_t connector_value);
H5_DLL hid_t   H5VLget_connector_id(hid_t obj_id);
H5_DLL hid_t   H5VLget_connector_id_by_name(const char *name);
H5_DLL hid_t   H5VLget_connector_id_by_value(H5VL_class_value_t connector_value);
H5_DLL ssize_t H5VLget_connector_name(hid_t id, char *name /*out*/, size_t size);
H5_DLL herr_t  H5VLclose(hid_t connector_id);
H5_DLL herr_t  H5VLunregister_connector(hid_t connector_id);
H5_DLL herr_t H5VLquery_optional(hid_t obj_id, H5VL_subclass_t subcls, int opt_type, uint64_t *flags);

#ifdef __cplusplus
}
#endif

/* Semi-public headers mainly for VOL connector authors */
#include "H5VLconnector.h" /* VOL connector author routines */
#include "H5VLconnector_passthru.h" /* Pass-through VOL connector author routines */
#include "H5VLnative.h" /* Native VOL connector macros, for VOL connector authors */

#endif /* _H5VLpublic_H */
