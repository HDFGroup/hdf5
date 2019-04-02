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

/* This source code was developed under the Mochi project
 * (https://www.mcs.anl.gov/research/projects/mochi), supported by the U.S.
 * Department of Energy, Office of Science, under contract DE-AC02-06CH11357.
 */

/*
 * Programmer:  Neil Fortner <nfortne2@hdfgroup.gov>
 *              December, 2016
 *
 * Purpose:	The public header file for the RADOS VOL plugin.
 */
#ifndef H5VLrados_public_H
#define H5VLrados_public_H

/* External headers needed by this file */
#ifdef HDF5_USE_MOBJECT
#include <librados-mobject-store.h>
#else
#include <rados/librados.h>
#endif

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Ipublic.h"

#define H5VL_RADOS_VERSION      1
#define H5VL_RADOS_VALUE        268
#define H5VL_RADOS_NAME         "rados_vol_connector"

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL herr_t H5VLrados_init(rados_t rados_cluster, const char *rados_pool);
H5_DLL herr_t H5VLrados_term(void);
H5_DLL herr_t H5Pset_fapl_rados(hid_t fapl_id, MPI_Comm comm, MPI_Info info);

#ifdef __cplusplus
}
#endif

#endif /* H5VLrados_public_H */
