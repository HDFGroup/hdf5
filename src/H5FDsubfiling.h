/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, August  2, 1999
 *
 * Purpose:	The public header file for the subfiling driver.
 */
#ifndef H5FDsubfiling_H
#define H5FDsubfiling_H

#define H5FD_SUBFILING	(H5FD_subfiling_init())

/****************************************************************************
 *
 * Structure: H5FD_subfiling_fapl_t
 *
 * Purpose:
 *
 *     H5FD_subfiling_fapl_t is a public structure that is used to pass 
 *     subfiling configuration data to the appropriate subfiling VFD via 
 *     the FAPL.  A pointer to an instance of this structure is a parameter 
 *     to H5Pset_fapl_subfiling() and H5Pget_fapl_subfiling().
 *
 * `version` (int32_t)
 *
 *     Version number of the H5FD_subfiling_fapl_t structure.  Any instance 
 *     passed to the above calls must have a recognized version number, or 
 *     an error will be flagged.
 *
 *     This field should be set to H5FD_CURR_SUBFILING_FAPL_T_VERSION.
 *
 *
 *     Add fields needed to configure the subfiling VFD here.
 *
 *     Note that we have to be able to copy FAPL entries -- thus use of 
 *     variable size fields (i.e. pointers to strings, etc) will complicate
 *     matters.
 *
 ****************************************************************************/

#define H5FD_CURR_SUBFILING_FAPL_T_VERSION     1

typedef struct H5FD_subfiling_fapl_t {

    int32_t version;

    /* add configuration fields here */
    
} H5FD_subfiling_fapl_t;


#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_subfiling_init(void);
H5_DLL herr_t H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_fapl_t *fa_out);
H5_DLL herr_t H5Pset_fapl_subfiling(hid_t fapl_id,  H5FD_subfiling_fapl_t *fa);
H5_DLL herr_t H5FD__get_file_ino(const char *name, uint64_t *st_ino);
H5_DLL herr_t H5FD__dataset_write_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
											 int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id, hid_t mem_space_id,
											 hid_t file_space_id, hid_t plist_id, const void *buf);
H5_DLL herr_t H5FD__dataset_read_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent, 
											int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id, hid_t mem_space_id,
											hid_t file_space_id, hid_t plist_id, void *buf);

/* Copied from:: H5FDsubfile_private.h */
H5_DLL int sf_open_subfiles(hid_t context_id, char *filename, char *prefix, int flags);
H5_DLL int sf_close_subfiles(hid_t context_id);
H5_DLL int sf_read_independent(hid_t sf_fid, int64_t offset, int64_t elements, int dtype_extent, void *data);
H5_DLL int sf_write_independent(hid_t sf_fid, int64_t offset, int64_t elements, int dtype_extent, const void *data);
H5_DLL herr_t sf_read_vector(hid_t h5_fid, hssize_t count, hsize_t addrs[], hsize_t sizes[], void *bufs[] /* out */);
H5_DLL herr_t sf_write_vector(hid_t h5_fid, hssize_t count, hsize_t addrs[], hsize_t sizes[], void *bufs[] /* in */);
H5_DLL int sf_truncate(hid_t h5_fid, haddr_t addr);


#ifdef __cplusplus
}
#endif

#endif

