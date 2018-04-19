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
 *              December, 2017
 *
 * Purpose:	The private header file for the RADOS VOL plugin.
 */
#ifndef H5VLrados_H
#define H5VLrados_H

/* Include package's public header */
#include "H5VLrados_public.h"

#define HDF5_VOL_RADOS_VERSION_1	1	/* Version number of RADOS VOL plugin */

#ifdef __cplusplus
extern "C" {
#endif

/* FAPL property to tell the VOL plugin to open a saved snapshot when opening a
 * file */
#define H5VL_RADOS_SNAP_OPEN_ID "rados_snap_open"

/* Common object and attribute information */
typedef struct H5VL_rados_item_t {
    H5I_type_t type;
    struct H5VL_rados_file_t *file;
    int rc;
} H5VL_rados_item_t;

/* Common object information */
typedef struct H5VL_rados_obj_t {
    H5VL_rados_item_t item; /* Must be first */
    uint64_t bin_oid;
    char *oid;
} H5VL_rados_obj_t;

/* The file struct */
typedef struct H5VL_rados_file_t {
    H5VL_rados_item_t item; /* Must be first */
    char *file_name;
    size_t file_name_len;
    unsigned flags;
    char *glob_md_oid;
    struct H5VL_rados_group_t *root_grp;
    uint64_t max_oid;
    hbool_t max_oid_dirty;
    hid_t fcpl_id;
    hid_t fapl_id;
    MPI_Comm comm;
    MPI_Info info;
    int my_rank;
    int num_procs;
    hbool_t collective;
} H5VL_rados_file_t;

/* The group struct */
typedef struct H5VL_rados_group_t {
    H5VL_rados_obj_t obj; /* Must be first */
    hid_t gcpl_id;
    hid_t gapl_id;
} H5VL_rados_group_t;

/* The dataset struct */
typedef struct H5VL_rados_dset_t {
    H5VL_rados_obj_t obj; /* Must be first */
    hid_t type_id;
    hid_t space_id;
    hid_t dcpl_id;
    hid_t dapl_id;
} H5VL_rados_dset_t;

/* The datatype struct */
/* Note we could speed things up a bit by caching the serialized datatype.  We
 * may also not need to keep the type_id around.  -NAF */
typedef struct H5VL_rados_dtype_t {
    H5VL_rados_obj_t obj; /* Must be first */
    hid_t type_id;
    hid_t tcpl_id;
    hid_t tapl_id;
} H5VL_rados_dtype_t;

/* The attribute struct */
typedef struct H5VL_rados_attr_t {
    H5VL_rados_item_t item; /* Must be first */
    H5VL_rados_obj_t *parent;
    char *name;
    hid_t type_id;
    hid_t space_id;
} H5VL_rados_attr_t;

/* The link value struct */
typedef struct H5VL_rados_link_val_t {
    H5L_type_t type;
    union {
        uint64_t hard;
        char *soft;
    } target;
} H5VL_rados_link_val_t;

extern hid_t H5VL_RADOS_g;

H5_DLL herr_t H5VL_rados_init(void);

#ifdef __cplusplus
}
#endif

#endif /* H5VLrados_H */
