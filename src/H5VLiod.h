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
 * Programmer:  Mohamad Chaarawi <chaarawi@hdfgroup.gov>
 *              January, 2012
 *
 * Purpose:	The public header file for the IOD VOL plugin.
 */
#ifndef H5VLiod_H
#define H5VLiod_H

#ifdef H5_HAVE_EFF

#include "axe.h"
#include "iod_api.h"
#include "iod_types.h"
#include "mercury.h"
#include "mercury_handler.h"
#include "mercury_macros.h"
#include "mercury_proc_string.h"
#include "na_mpi.h"

#define H5VL_IOD	(H5VL_iod_init())
#else
#define H5VL_IOD	(-1)
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef H5_HAVE_EFF

#define H5D_XFER_INJECT_CORRUPTION_NAME "inject_corruption"
#define H5D_XFER_CHECKSUM_NAME "checksum"
#define H5D_XFER_CHECKSUM_PTR_NAME "checksum_ptr"
#define H5D_CRT_APPEND_ONLY_NAME "append_only"
#define H5O_CRT_ENABLE_CHECKSUM_NAME "enable_obj_checksum"
#define H5O_ACS_REPLICA_ID_NAME "replica_id"

H5_DLL H5VL_class_t *H5VL_iod_init(void);
H5_DLL herr_t H5Pset_fapl_iod(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_start_server(MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_init(MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_finalize(void);

H5_DLL herr_t H5Pset_read_replica(hid_t dxpl_id, hrpl_t replica_id);
H5_DLL herr_t H5Pget_read_replica(hid_t dxpl_id, hrpl_t *replica_id);
H5_DLL herr_t H5Pset_evict_replica(hid_t lapl_id, hrpl_t replica_id);
H5_DLL herr_t H5Pget_evict_replica(hid_t lapl_id, hrpl_t *replica_id);
H5_DLL herr_t H5Pset_ocpl_enable_checksum(hid_t ocpl_id, hbool_t flag);
H5_DLL herr_t H5Pget_ocpl_enable_checksum(hid_t ocpl_id, hbool_t *flag);
H5_DLL herr_t H5Pset_dxpl_checksum(hid_t dxpl_id, uint64_t value);
H5_DLL herr_t H5Pget_dxpl_checksum(hid_t dxpl_id, uint64_t *value);
H5_DLL herr_t H5Pset_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t *value);
H5_DLL herr_t H5Pget_dxpl_checksum_ptr(hid_t dxpl_id, uint64_t **value);
H5_DLL herr_t H5Pset_metadata_integrity_scope(hid_t fapl_id, uint32_t scope);
H5_DLL herr_t H5Pget_metadata_integrity_scope(hid_t fapl_id, uint32_t *scope);
H5_DLL herr_t H5Pset_rawdata_integrity_scope(hid_t dxpl_id, uint32_t scope);
H5_DLL herr_t H5Pget_rawdata_integrity_scope(hid_t dxpl_id, uint32_t *scope);
H5_DLL herr_t H5Pset_dxpl_inject_corruption(hid_t dxpl_id, hbool_t flag);
H5_DLL herr_t H5Pget_dxpl_inject_corruption(hid_t dxpl_id, hbool_t *flag);
H5_DLL herr_t H5Pset_dcpl_append_only(hid_t dcpl_id, hbool_t flag);
H5_DLL herr_t H5Pget_dcpl_append_only(hid_t dcpl_id, hbool_t *flag);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* _H5VLiod_H */
