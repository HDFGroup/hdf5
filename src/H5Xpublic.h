/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
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
 * This file contains function prototypes for each exported function in the
 * H5X module.
 */
#ifndef _H5Xpublic_H
#define _H5Xpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"
#include "H5Qpublic.h"
#include "H5Rpublic.h"

/*****************/
/* Public Macros */
/*****************/

#define H5X_CLASS_T_VERS (1)

/* Plugin IDs */
#define H5X_PLUGIN_ERROR        (-1)    /* no plugin                    */
#define H5X_PLUGIN_NONE         0       /* reserved indefinitely        */
#define H5X_PLUGIN_DUMMY        1       /* dummy                        */
#define H5X_PLUGIN_FASTBIT      2       /* fastbit                      */
#define H5X_PLUGIN_ALACRITY     3       /* alacrity                     */

#define H5X_PLUGIN_META_DUMMY   4       /* metadata dummy               */

#define H5X_PLUGIN_RESERVED     64      /* plugin ids below this value reserved */

#define H5X_PLUGIN_MAX          256     /* maximum plugin id            */
#define H5X_MAX_NPLUGINS        16      /* Maximum number of plugins allowed in a pipeline */

/*******************/
/* Public Typedefs */
/*******************/

/* Index type */
typedef enum {
    H5X_TYPE_DATA,        /* Data index */
    H5X_TYPE_METADATA     /* Metadata index */
} H5X_type_t;

typedef struct {
    hid_t field_datatype_id;
    unsigned plugin_id;
} H5X_info_t;

/* Data index class */
typedef struct {
    void *(*create)(hid_t dataset_id, hid_t xcpl_id, hid_t xapl_id,
        size_t *metadata_size, void **metadata); /* TODO pass datatype id */
    herr_t (*remove)(hid_t file_id, size_t metadata_size, void *metadata);
    void *(*open)(hid_t dataset_id, hid_t xapl_id, size_t metadata_size,
        void *metadata);
    herr_t (*close)(void *idx_handle);
    herr_t (*pre_update)(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);
    herr_t (*post_update)(void *idx_handle, const void *buf, hid_t dataspace_id,
        hid_t xxpl_id);
    hid_t  (*query)(void *idx_handle, hid_t dataspace_id, hid_t query_id,
        hid_t xxpl_id);
    herr_t (*refresh)(void *idx_handle, size_t *metadata_size, void **metadata);
    herr_t (*copy)(hid_t src_file_id, hid_t dest_file_id, hid_t xcpl_id,
            hid_t xapl_id, size_t src_metadata_size, void *src_metadata,
            size_t *dest_metadata_size, void **dest_metadata);
    herr_t (*get_size)(void *idx_handle, hsize_t *idx_size);
} H5X_data_class_t;

/* Metadata index class */
typedef struct {
    void *(*create)(hid_t loc_id, hid_t xcpl_id, hid_t xapl_id,
        size_t *metadata_size, void **metadata);
    herr_t (*remove)(hid_t loc_id, size_t metadata_size, void *metadata);
    void *(*open)(hid_t loc_id, hid_t xapl_id, size_t metadata_size,
        void *metadata);
    herr_t (*close)(void *idx_handle);
    herr_t (*insert_entry)(void *idx_handle, hid_t obj_id, H5Q_type_t key_type,
        H5Q_elem_t *key, hid_t xxpl_id);
    herr_t (*remove_entry)(void *idx_handle, hid_t obj_id, H5Q_type_t key_type,
        H5Q_elem_t *key, hid_t xxpl_id);
    herr_t (*query)(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        size_t *ref_count, href_t *refs[]);
    herr_t (*get_size)(void *idx_handle, hsize_t *idx_size);
} H5X_metadata_class_t;

typedef struct {
    unsigned version;     /* Version number of the index plugin class struct */
                          /* (Should always be set to H5X_CLASS_VERSION, which
                           *  may vary between releases of HDF5 library) */
    unsigned id;          /* Index ID (assigned by The HDF Group, for now) */
    const char *idx_name; /* Index name (for debugging only, currently) */
    H5X_type_t type;      /* Type of data indexed by this plugin */

    /* Callbacks */
    union {
        /* Union of callback index structures */
        H5X_data_class_t data_class;
        H5X_metadata_class_t metadata_class;
    } idx_class;
} H5X_class_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/
#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes */
H5_DLL herr_t H5Xregister(const H5X_class_t *idx_class);
H5_DLL herr_t H5Xunregister(unsigned plugin_id);
H5_DLL herr_t H5Xcreate(hid_t loc_id, unsigned plugin_id, hid_t xcpl_id);
H5_DLL herr_t H5Xremove(hid_t loc_id, unsigned idx /* Index n to be removed */);
H5_DLL herr_t H5Xget_count(hid_t loc_id, hsize_t *idx_count);
H5_DLL herr_t H5Xget_info(hid_t loc_id, unsigned idx, H5X_info_t *info);
H5_DLL hsize_t H5Xget_size(hid_t loc_id);

/*
H5_DLL herr_t H5Xget_type(hid_t object_id, hsize_t index_idx,
        unsigned *plugin_id);
*/

#ifdef __cplusplus
}
#endif

#endif /* _H5Xpublic_H */
