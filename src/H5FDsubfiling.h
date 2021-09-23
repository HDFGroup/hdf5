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

#include "H5FDsubfiling_priv.h"

#define H5FD_SUBFILING (H5FD_subfiling_init())

#ifndef H5FD_SUBFILING_FAPL_T_MAGIC
#define H5FD_CURR_SUBFILING_FAPL_T_VERSION 1
#define H5FD_SUBFILING_FAPL_T_MAGIC        0xFED01331
#endif

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
 * `magic`   (uint32_t)
 *
 *     Magic is a somewhat unique number which identifies this VFD from
 *     other VFDs.  Used in combination with a version number, we can
 *     validate a user generated file access property list (fapl).
 *     This field should be set to H5FD_SUBFILING_FAPL_T_MAGIC.
 *
 * `version` (uint32_t)
 *
 *     Version number of the H5FD_subfiling_fapl_t structure.  Any instance
 *     passed to the above calls must have a recognized version number, or
 *     an error will be flagged.
 *
 *     This field should be set to H5FD_CURR_SUBFILING_FAPL_T_VERSION.
 *
 ***   IO Concentrator Info ***
 ***   These fields will be replicated in the stacked IOC VFD which
 ***   provides the extended support for aggregating reads and writes
 ***   and allows global file access to node-local storage containers.
 *
 * `stripe_count` (int32_t)
 *
 *     The integer value which identifies the total number of
 *     subfiles that have been algorthmically been selected to
 *     to contain the segments of raw data which make up an HDF5
 *     file.  This value is used to implement the RAID-0 functionality
 *     when reading or writing datasets.
 *
 * `stripe_depth` (int64_t)
 *
 *     The stripe depth defines a limit on the maximum number of contiguous
 *     bytes that can be read or written in a single operation on any
 *     selected subfile.  Larger IO operations can exceed this limit
 *     by utilizing MPI derived types to construct an IO request which
 *     gathers additional data segments from memory for the IO request.
 *
 * `ioc_selection` (enum io_selection datatype)
 *
 *     The io_selection_t defines a specific algorithm by which IO
 *     concentrators (IOCs) and sub-files are identified.  The available
 *     algorthms are: SELECT_IOC_ONE_PER_NODE, SELECT_IOC_EVERY_NTH_RANK,
 *     SELECT_IOC_WITH_CONFIG, and SELECT_IOC_TOTAL.
 *
 ***   STACKING and other VFD support
 ***   i.e. FAPL caching
 ***
 *
 * `ioc_fapl_id` (hid_t)
 *
 *     A valid file access property list (fapl) is cached on each
 *     process and thus enables selection of an alternative provider
 *     for subsequent file operations.
 *     By defalt, Sub-filing employs an additional support VFD that
 *     provides file IO proxy capabilities to all MPI ranks in a
 *     distributed parallel application.  This IO indirection
 *     thus allows application access all sub-files even while
 *     these may actually be node-local and thus not directly
 *     accessable to remote ranks.
 *
 ***   Subfiling file Info
 *
 * `subfile_dir`  char[]
 *
 *     A file directory name where subfiling files should be
 *     placed. Under normal circumstances, this directory name
 *     should match the directory path of the user defined HDF5
 *     file.
 *
 * `subfile_path` char[]
 *
 *     The full pathname of the user HDF5 file.
 *

#define H5FD_SUBFILING_PATH_MAX 4096

typedef struct config_common_t {
    uint32_t        magic;
    uint32_t        version;
    int32_t         stripe_count;
    int64_t         stripe_depth;
    ioc_selection_t ioc_selection;
        hid_t           ioc_fapl_id;
    char            subfile_dir[H5FD_SUBFILING_PATH_MAX +1];
        char            subfile_path[H5FD_SUBFILING_PATH_MAX +1];
} config_common_t;

 ****************************************************************************/

/*
 * In addition to the common configuration fields, we can have
 * VFD specific fields.  Here's one for the subfiling VFD.
 *
 * `require_ioc` (hbool_t)
 *
 *     Require_IOC is a boolean flag with a default value of TRUE.
 *     This flag indicates that the stacked H5FDioc VFD should be
 *     employed for sub-filing operations.  The default flag can be
 *     overriden with an environment variable: H5_REQUIRE_IOC=0
 *
 */

typedef struct H5FD_subfiling_config_t {
    config_common_t common;
    hbool_t         require_ioc;
} H5FD_subfiling_config_t;

#ifdef __cplusplus
extern "C" {
#endif

extern FILE *sf_logfile;
extern FILE *client_log;

H5_DLL hid_t  H5FD_subfiling_init(void);
H5_DLL herr_t H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *config_out);
H5_DLL herr_t H5Pset_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *vfd_config);
H5_DLL herr_t H5FD__get_file_ino(const char *name, uint64_t *st_ino);
H5_DLL char * H5FD__get_file_directory(void *h5file);
H5_DLL herr_t H5FD__dataset_write_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                                             int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
                                             hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
                                             const void *buf);
H5_DLL herr_t H5FD__dataset_read_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                                            int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
                                            hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
                                            void *buf);

H5_DLL char *get_ioc_selection_criteria(ioc_selection_t *);
H5_DLL void *get__subfiling_object(int64_t object_id);
H5_DLL hid_t fid_map_to_context(hid_t h5_fid);

/* return arguments are vector of vectors - function return is the length
 * (depth) of the sub vectors. Note that we don't need to include the
 * MPI_Datatype return argument!
 */
H5_DLL int init__indep_io(void *_sf_context, size_t depth, int ioc_total, int64_t *sf_source_data_offset,
                          int64_t *sf_datasize, int64_t *f_offset, int *first_index, int *n_containers,
                          int64_t offset, int64_t elements, int dtype_extent);

H5_DLL int    H5FD__open_subfiles(void *_config_info, int64_t inode_id, int fd, int flags);
H5_DLL int    H5FD__close_subfiles(hid_t context_id);
H5_DLL int    H5FD__read_independent(hid_t H5FD__fid, int64_t offset, int64_t elements, int dtype_extent,
                                     void *data);
H5_DLL int    H5FD__write_independent(hid_t H5FD__fid, int64_t offset, int64_t elements, int dtype_extent,
                                      const void *data);
H5_DLL herr_t H5FD__read_vector(hid_t h5_fid, hssize_t count, haddr_t *addrs, hsize_t sizes[],
                                void *bufs[] /* in */);
H5_DLL herr_t H5FD__write_vector(hid_t h5_fid, hssize_t count, haddr_t *addrs, hsize_t sizes[],
                                 void *bufs[] /* in */);
H5_DLL int    H5FD__truncate(hid_t h5_fid, haddr_t addr);
H5_DLL int    H5FD__shutdown_local_ioc(hid_t fid);
H5_DLL void   manage_client_logfile(int client_rank, int flag_value);
H5_DLL int    initialize_ioc_threads(void *sf_context);

#ifdef __cplusplus
}
#endif

#endif
