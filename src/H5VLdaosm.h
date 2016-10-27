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
 * Purpose:	The public header file for the DAOS-M VOL plugin.
 */
#ifndef H5VLdaosm_H
#define H5VLdaosm_H

#define H5_HAVE_EFF 1 /* DSMINC */

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

/* the file struct */
typedef struct H5VL_daosm_file_t {
    daos_handle_t poh;
    daos_handle_t coh;
    //daos_pool_info_t pool_info;
    //daos_co_info_t co_info;
    char *file_name;
    uuid_t uuid;
    unsigned flags;
    daos_handle_t glob_md_oh;
    daos_handle_t root_oh;
    daos_obj_id_t max_oid;
    hid_t fcpl_id;
    hid_t fapl_id;
    MPI_Comm comm;
    MPI_Info info;
    int my_rank;
    int num_procs;
} H5VL_daosm_file_t;

extern hid_t H5VL_DAOSM_g;

H5_DLL hid_t H5VL_daosm_init(void);
H5_DLL herr_t H5Pset_fapl_daosm(hid_t fapl_id, MPI_Comm comm, MPI_Info info,
    uuid_t pool_uuid, char *pool_grp);
//H5_DLL herr_t EFF_init(void); DSMINC
//H5_DLL herr_t EFF_finalize(void); DSMINC

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* H5VLdaosm_H */
