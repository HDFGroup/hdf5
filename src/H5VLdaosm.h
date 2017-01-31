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
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.gov>
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

#define H5VL_DAOSM	(H5VL_daosm_init())
#define HDF5_VOL_DAOSM_VERSION_1	1	/* Version number of IOD VOL plugin */
#else
#define H5VL_DAOSM	(-1)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

/* FAPL property to tell the VOL plugin to open a saved snapshot when opening a
 * file */
#define H5VL_DAOSM_SNAP_OPEN_ID "daosm_snap_open"

/* Common object information */
typedef struct H5VL_daosm_obj_t {
    H5I_type_t type;
    struct H5VL_daosm_file_t *file;
    int rc;
} H5VL_daosm_obj_t;

/* The file struct */
typedef struct H5VL_daosm_file_t {
    H5VL_daosm_obj_t common; /* Must be first */
    daos_handle_t poh;
    daos_handle_t coh;
    daos_epoch_t epoch;
    hbool_t snap_epoch;
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
} H5VL_daosm_file_t;

/* The group struct */
typedef struct H5VL_daosm_group_t {
    H5VL_daosm_obj_t common; /* Must be first */
    daos_obj_id_t oid;
    daos_handle_t obj_oh;
    hid_t gcpl_id;
    hid_t gapl_id;
} H5VL_daosm_group_t;

/* The dataset struct */
typedef struct H5VL_daosm_dset_t {
    H5VL_daosm_obj_t common; /* Must be first */
    daos_obj_id_t oid;
    daos_handle_t obj_oh;
    hid_t type_id;
    hid_t space_id;
    hid_t dcpl_id;
    hid_t dapl_id;
} H5VL_daosm_dset_t;

extern hid_t H5VL_DAOSM_g;

H5_DLL hid_t H5VL_daosm_init(void);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* H5VLdaosm_H */
