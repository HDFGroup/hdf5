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

/*
 * Purpose:	The public header file for the "io concentrator" driver.
 * This provides a similar functionality to that of the subfiling driver
 * but introduces the necessary file access functionality via a multi-
 * threading MPI service
 */

#ifndef H5FDioc_H
#define H5FDioc_H

#include "H5FDsubfiling_priv.h"

#define H5FD_IOC (H5FD_ioc_init())

#ifndef H5FD_IOC_FAPL_T_MAGIC
#define H5FD_CURR_IOC_FAPL_T_VERSION 1
#define H5FD_IOC_FAPL_T_MAGIC        0xFED21331
#endif

/* Maximum length of a filename/path string in the Write-Only channel,
 * including the NULL-terminator.
 */
#define H5FD_IOC_PATH_MAX         4096
#define H5FD_IOC_THREAD_POOL_SIZE 4

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
 * VFD specific fields.  Here's one for the IO Concentrator VFD.
 *
 * thread_pool_count (int32_t)
 *      Indicate the number of helper threads that we want for
 *      creating a thread pool
 *
 * ----------------------------------------------------------------------------
 */
typedef struct H5FD_ioc_config_t {
    config_common_t common;
    int32_t         thread_pool_count;
} H5FD_ioc_config_t;

#ifdef __cplusplus
extern "C" {
#endif
H5_DLL hid_t  H5FD_ioc_init(void);
H5_DLL herr_t H5Pset_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_ptr);
H5_DLL herr_t H5Pget_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_ptr);
H5_DLL void   H5FD_ioc_set_shutdown_flag(int flag);
H5_DLL void   H5FD_ioc_wait_thread_main(void);
H5_DLL void   H5FD_ioc_finalize_threads(void);

#ifdef __cplusplus
}
#endif

#endif
