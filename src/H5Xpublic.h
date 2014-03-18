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

/*****************/
/* Public Macros */
/*****************/

#define H5X_CLASS_T_VERS (1)

/* Plugin IDs */
#define H5X_PLUGIN_ERROR    (-1) /* no plugin             */
#define H5X_PLUGIN_NONE     0    /* reserved indefinitely */
#define H5X_PLUGIN_DUMMY    1    /* dummy                 */
#define H5X_PLUGIN_FASTBIT  2    /* fastbit               */
#define H5X_PLUGIN_ALACRIT  3    /* alacrity              */

#define H5X_PLUGIN_RESERVED 64   /* plugin ids below this value reserved */

#define H5X_PLUGIN_MAX      256  /* maximum plugin id     */
#define H5X_MAX_NPLUGINS    16   /* Maximum number of plugins allowed in a pipeline */

/*******************/
/* Public Typedefs */
/*******************/

/* Index type */
typedef enum {
    H5X_TYPE_LINK_NAME,   /* Link name index */
    H5X_TYPE_ATTR_NAME,   /* Attribute name index */
    H5X_TYPE_DATA_ELEM,   /* Dataset element index */
    H5X_TYPE_MAP_VALUE    /* Map value index */
} H5X_type_t;

typedef struct {
    unsigned version;     /* Version number of the index plugin class struct */
                          /* (Should always be set to H5X_CLASS_VERSION, which
                           *  may vary between releases of HDF5 library) */
    unsigned id;          /* Index ID (assigned by The HDF Group, for now) */
    const char *idx_name; /* Index name (for debugging only, currently) */
    H5X_type_t type;      /* Type of data indexed by this plugin */

    /* Callbacks, described above */
    void *(*create)(hid_t file_id, hid_t dataset_id, hid_t xcpl_id,
        hid_t xapl_id, size_t *metadata_size, void **metadata);
    herr_t (*remove)(hid_t file_id, hid_t dataset_id, size_t metadata_size,
        void *metadata);
    void *(*open)(hid_t file_id, hid_t dataset_id, hid_t xapl_id,
        size_t metadata_size, void *metadata);
    herr_t (*close)(void *idx_handle);
    herr_t (*pre_update)(void *idx_handle, hid_t dataspace_id, hid_t xxpl_id);
    herr_t (*post_update)(void *idx_handle, const void *buf, hid_t dataspace_id,
            hid_t xxpl_id);
    herr_t (*query)(void *idx_handle, hid_t query_id, hid_t xxpl_id,
        hid_t *dataspace_id);
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

H5_DLL herr_t H5Xcreate(hid_t file_id, unsigned plugin_id, hid_t scope_id,
        hid_t xcpl_id);
H5_DLL herr_t H5Xcreate_ff(hid_t file_id, unsigned plugin_id, hid_t scope_id,
        hid_t xcpl_id, hid_t trans_id, hid_t estack_id);

H5_DLL herr_t H5Xremove(hid_t file_id, unsigned plugin_id, hid_t scope_id);
H5_DLL herr_t H5Xremove_ff(hid_t file_id, unsigned plugin_id, hid_t scope_id,
        hid_t trans_id, hid_t estack_id);

H5_DLL herr_t H5Xget_count(hid_t scope_id, hsize_t *idx_count);
H5_DLL herr_t H5Xget_count_ff(hid_t scope_id, hsize_t *idx_count, hid_t rcxt_id,
        hid_t estack_id);

/*
H5_DLL herr_t H5Xget_type(hid_t object_id, hsize_t index_idx,
        unsigned *plugin_id);
H5_DLL herr_t H5Xget_type_ff(hid_t object_id, hsize_t index_idx,
        unsigned *plugin_id, hid_t rcxt_id, hid_t event_stack_id);
*/
H5_DLL herr_t H5Pget_xapl_transaction(hid_t xapl_id, hid_t *trans_id);

#ifdef __cplusplus
}
#endif

#endif /* _H5Xpublic_H */
