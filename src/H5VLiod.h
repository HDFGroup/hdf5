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
#ifdef H5_HAVE_IOD_CORRUPT_TOOL
#include "iod_corrupt_tool.h"
#endif
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

#define H5D_CRT_DIMS_ORDER_NAME "dims_order"
#define H5D_CRT_STRIPE_COUNT_NAME "stripe_count"
#define H5D_CRT_STRIPE_SIZE_NAME "stripe_size"
#define H5D_CRT_APPEND_ONLY_NAME "append_only"

#define H5O_CRT_ENABLE_CHECKSUM_NAME "enable_obj_checksum"

#define H5O_XFER_REPLICA_ID_NAME "replica_id"
#define H5O_XFER_LAYOUT_TYPE_NAME "layout_type"
#define H5O_XFER_SELECTION_NAME "prefetched_selection"
#define H5O_XFER_KEY_TYPE_NAME "prefetched_keytype"
#define H5O_XFER_LOW_KEY_BUF_NAME "low_key_range"
#define H5O_XFER_HIGH_KEY_BUF_NAME "high_key_range"

#define HDF5_VOL_IOD_VERSION_1	1	/* Version number of IOD VOL plugin */

typedef enum H5FF_layout_t {
    H5_DEFAULT_LAYOUT = 0,
    H5_LOCAL_NODE,
    H5_CONTROLLED_LAYOUT
} H5FF_layout_t;

typedef enum H5FF_dset_dim_layout_t {
    H5D_ROW_MAJOR = 0,
    H5D_COL_MAJOR
} H5FF_dset_dim_layout_t;

extern hid_t H5VL_IOD_g;

H5_DLL hid_t H5VL_iod_init(void);
H5_DLL herr_t H5Pset_fapl_iod(hid_t fapl_id, MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_start_server(MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_init(MPI_Comm comm, MPI_Info info);
H5_DLL herr_t EFF_finalize(void);

H5_DLL herr_t H5Pset_dxpl_replica(hid_t dxpl_id, hrpl_t replica_id);
H5_DLL herr_t H5Pget_dxpl_replica(hid_t dxpl_id, hrpl_t *replica_id);
H5_DLL herr_t H5Pset_prefetch_layout(hid_t dxpl_id, H5FF_layout_t layout);
H5_DLL herr_t H5Pget_prefetch_layout(hid_t dxpl_id, H5FF_layout_t *layout);
H5_DLL herr_t H5Pset_prefetch_selection(hid_t dxpl_id, hid_t file_space);
    //H5_DLL herr_t H5Pget_prefetch_selection(hid_t dxpl_id, hid_t *file_space);
H5_DLL herr_t H5Pset_prefetch_range(hid_t dxpl_id, hid_t keymem_type, 
                                    const void *low_key, const void *high_key);
    //H5_DLL herr_t H5Pget_prefetch_range(hid_t dxpl_id, hid_t *keymem_type, 
    //void **low_key, void **high_key);

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

H5_DLL herr_t H5Pset_dcpl_dim_layout(hid_t dcpl_id, H5FF_dset_dim_layout_t dims_layout);
H5_DLL herr_t H5Pget_dcpl_dim_layout(hid_t dcpl_id, H5FF_dset_dim_layout_t *dims_layout);
H5_DLL herr_t H5Pset_dcpl_stripe_count(hid_t dcpl_id, size_t stripe_count);
H5_DLL herr_t H5Pget_dcpl_stripe_count(hid_t dcpl_id, size_t *stripe_count);
H5_DLL herr_t H5Pset_dcpl_stripe_size(hid_t dcpl_id, size_t stripe_size);
H5_DLL herr_t H5Pget_dcpl_stripe_size(hid_t dcpl_id, size_t *stripe_size);

H5_DLL hid_t H5VLiod_get_file_id(const char *filename, iod_handle_t coh, 
                                 hid_t fapl_id, hid_t *rcxt_id);
H5_DLL herr_t H5VLiod_close_file_id(hid_t file_id);
H5_DLL hid_t H5Dquery_ff(hid_t dset_id, hid_t query_id, hid_t scope_id, 
                         hid_t rcxt_id);
H5_DLL herr_t H5VLiod_query_map(hid_t obj_id, iod_trans_id_t rtid, 
                                iod_obj_map_t **obj_map);
H5_DLL herr_t H5VLiod_close_map(hid_t obj_id, iod_obj_map_t *obj_map);

#endif /* H5_HAVE_EFF */

#ifdef __cplusplus
}
#endif

#endif /* _H5VLiod_H */
