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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.org>
 *              September, 2016
 *
 * Purpose:	The private header file for the DAOS-M VOL plugin.
 */
#ifndef H5VLdaosm_H
#define H5VLdaosm_H

#define H5_HAVE_EFF 1 /* DSMINC */

/* Include package's public header */
#include "H5VLdaosm_public.h"

#ifdef H5_HAVE_EFF

#include "daos.h"

#define HDF5_VOL_DAOSM_VERSION_1	1	/* Version number of IOD VOL plugin */

#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

/* FAPL property to tell the VOL plugin to open a saved snapshot when opening a
 * file */
#define H5VL_DAOSM_SNAP_OPEN_ID "daosm_snap_open"

/* Common object and attribute information */
typedef struct H5VL_daosm_item_t {
    H5I_type_t type;
    struct H5VL_daosm_file_t *file;
    int rc;
} H5VL_daosm_item_t;

/* Common object information */
typedef struct H5VL_daosm_obj_t {
    H5VL_daosm_item_t item; /* Must be first */
    daos_obj_id_t oid;
    daos_handle_t obj_oh;
} H5VL_daosm_obj_t;

/* The file struct */
typedef struct H5VL_daosm_file_t {
    H5VL_daosm_item_t item; /* Must be first */
    daos_handle_t coh;
    daos_epoch_t epoch;
    int snap_epoch;
    char *file_name;
    uuid_t uuid;
    unsigned flags;
    daos_handle_t glob_md_oh;
    struct H5VL_daosm_group_t *root_grp;
    uint64_t max_oid;
    hbool_t max_oid_dirty;
    hid_t fcpl_id;
    hid_t fapl_id;
    MPI_Comm comm;
    MPI_Info info;
    int my_rank;
    int num_procs;
    hbool_t collective;
} H5VL_daosm_file_t;

/* The group struct */
typedef struct H5VL_daosm_group_t {
    H5VL_daosm_obj_t obj; /* Must be first */
    hid_t gcpl_id;
    hid_t gapl_id;
} H5VL_daosm_group_t;

/* The dataset struct */
typedef struct H5VL_daosm_dset_t {
    H5VL_daosm_obj_t obj; /* Must be first */
    hid_t type_id;
    hid_t space_id;
    hid_t dcpl_id;
    hid_t dapl_id;
} H5VL_daosm_dset_t;

/* The datatype struct */
/* Note we could speed things up a bit by caching the serialized datatype.  We
 * may also not need to keep the type_id around.  -NAF */
typedef struct H5VL_daosm_dtype_t {
    H5VL_daosm_obj_t obj; /* Must be first */
    hid_t type_id;
    hid_t tcpl_id;
    hid_t tapl_id;
} H5VL_daosm_dtype_t;

/* The map struct */
typedef struct H5VL_daosm_map_t {
    H5VL_daosm_obj_t obj; /* Must be first */
    hid_t ktype_id;
    hid_t vtype_id;
} H5VL_daosm_map_t;

/* The attribute struct */
typedef struct H5VL_daosm_attr_t {
    H5VL_daosm_item_t item; /* Must be first */
    H5VL_daosm_obj_t *parent;
    char *name;
    hid_t type_id;
    hid_t space_id;
} H5VL_daosm_attr_t;

/* The link value struct */
typedef struct H5VL_daosm_link_val_t {
    H5L_type_t type;
    union {
        daos_obj_id_t hard;
        char *soft;
    } target;
} H5VL_daosm_link_val_t;

extern hid_t H5VL_DAOSM_g;

H5_DLL herr_t H5VL_daosm_init(void);

H5_DLL void * H5VL_daosm_map_create(void *_item, H5VL_loc_params_t loc_params, const char *name,
				    hid_t ktype_id, hid_t vtype_id, hid_t mcpl_id, hid_t mapl_id,
				    hid_t dxpl_id, void **req);
H5_DLL void * H5VL_daosm_map_open(void *_item, H5VL_loc_params_t loc_params, const char *name,
				  hid_t mapl_id, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL_daosm_map_set(void *_map, hid_t key_mem_type_id, const void *key, 
				 hid_t val_mem_type_id, const void *value, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL_daosm_map_get(void *_map, hid_t key_mem_type_id, const void *key, 
				 hid_t val_mem_type_id, void *value, hid_t dxpl_id, void **req);
H5_DLL herr_t H5VL_daosm_map_get_types(void *_map, hid_t *key_type_id, hid_t *val_type_id, void **req);
H5_DLL herr_t H5VL_daosm_map_get_count(void *_map, hsize_t *count, void **req);
H5_DLL herr_t H5VL_daosm_map_exists(void *_map, hid_t key_mem_type_id, const void *key, 
				    hbool_t *exists, void **req);
H5_DLL herr_t H5VL_daosm_map_close(void *_map, hid_t dxpl_id, void **req);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* H5VLdaosm_H */
