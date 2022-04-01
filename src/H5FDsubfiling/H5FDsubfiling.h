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

/* Purpose: The public header file for the subfiling driver. */
#ifndef H5FDsubfiling_H
#define H5FDsubfiling_H

#include "H5FDioc.h"

#ifdef H5_HAVE_SUBFILING_VFD
#define H5FD_SUBFILING (H5FDperform_init(H5FD_subfiling_init))
#else
#define H5FD_SUBFILING (H5I_INVALID_HID)
#endif

#define H5FD_SUBFILING_NAME "subfiling"

#ifndef H5FD_SUBFILING_FAPL_MAGIC
#define H5FD_CURR_SUBFILING_FAPL_VERSION 1
#define H5FD_SUBFILING_FAPL_MAGIC        0xFED01331
#endif

#define H5FD_SUBFILING_PATH_MAX 4096

/****************************************************************************
 *
 * Structure: H5FD_subfiling_config_t
 *
 * Purpose:
 *
 *     H5FD_subfiling_config_t is a public structure that is used to pass
 *     subfiling configuration data to the appropriate subfiling VFD via
 *     the FAPL.  A pointer to an instance of this structure is a parameter
 *     to H5Pset_fapl_subfiling() and H5Pget_fapl_subfiling().
 *
 * `magic`   (uint32_t)
 *
 *     Magic is a somewhat unique number which identifies this VFD from
 *     other VFDs.  Used in combination with a version number, we can
 *     validate a user generated file access property list (fapl).
 *     This field should be set to H5FD_SUBFILING_FAPL_MAGIC.
 *
 * `version` (uint32_t)
 *
 *     Version number of the H5FD_subfiling_config_t structure.  Any instance
 *     passed to the above calls must have a recognized version number, or
 *     an error will be flagged.
 *
 *     This field should be set to H5FD_CURR_SUBFILING_FAPL_VERSION.
 *
 ***   IO Concentrator Info ***
 ***   These fields will be replicated in the stacked IOC VFD which
 ***   provides the extended support for aggregating reads and writes
 ***   and allows global file access to node-local storage containers.
 *
 * `stripe_count` (int32_t)
 *
 *     The integer value which identifies the total number of
 *     subfiles that have been algorithmically been selected to
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
 *     algorithms are: SELECT_IOC_ONE_PER_NODE, SELECT_IOC_EVERY_NTH_RANK,
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
 *     By default, Sub-filing employs an additional support VFD that
 *     provides file IO proxy capabilities to all MPI ranks in a
 *     distributed parallel application.  This IO indirection
 *     thus allows application access all sub-files even while
 *     these may actually be node-local and thus not directly
 *     accessible to remote ranks.
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
 *     overridden with an environment variable: H5_REQUIRE_IOC=0
 *
 */

//! <!-- [H5FD_subfiling_config_t_snip] -->
/**
 * Configuration structure for H5Pset_fapl_subfiling() / H5Pget_fapl_subfiling()
 */
typedef struct H5FD_subfiling_config_t {
    uint32_t        magic;                              /* set to H5FD_SUBFILING_FAPL_MAGIC */
    uint32_t        version;                            /* set to H5FD_CURR_SUBFILING_FAPL_VERSION */
    int32_t         stripe_count;                       /* How many io concentrators */
    int64_t         stripe_depth;                       /* Max # of bytes in contiguous IO to an IOC */
    ioc_selection_t ioc_selection;                      /* Method to select IO Concentrators */
    hid_t           ioc_fapl_id;                        /* The hid_t value of the stacked VFD  */
    int64_t         context_id;                         /* The value used to lookup an IOC context */
    char            file_dir[H5FD_SUBFILING_PATH_MAX];  /* Directory where we find files */
    char            file_path[H5FD_SUBFILING_PATH_MAX]; /* The user defined filename */
    hbool_t         require_ioc;
} H5FD_subfiling_config_t;
//! <!-- [H5FD_subfiling_config_t_snip] -->

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL hid_t H5FD_subfiling_init(void);
/**
 * \ingroup FAPL
 *
 * \brief Modifies the file access property list to use the #H5FD_SUBFILING driver
 *
 * \fapl_id
 * \param[in] vfd_config #H5FD_SUBFILING driver specific properties. If NULL, then
 *            the IO concentrator VFD will be used.
 * \returns \herr_t
 *
 * \details H5Pset_fapl_core() modifies the file access property list to use the
 *          #H5FD_SUBFILING driver.
 *
 *          \todo Expand details!
 *
 * \since 1.14.0
 *
 */
H5_DLL herr_t H5Pset_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *vfd_config);
/**
 * \ingroup FAPL
 *
 * \brief Queries subfiling file driver properties
 *
 * \fapl_id
 * \param[out] config_out The subfiling fapl data.
 *
 * \returns \herr_t
 *
 * \details H5Pget_fapl_subfiling() queries the #H5FD_SUBFILING driver properties as set
 *          by H5Pset_fapl_subfiling(). If the #H5FD_SUBFILING driver has not been set on
 *          the File Access Property List, a default configuration is returned.
 *
 * \since 1.14.0
 *
 */
H5_DLL herr_t H5Pget_fapl_subfiling(hid_t fapl_id, H5FD_subfiling_config_t *config_out);

#ifdef __cplusplus
}
#endif

#endif /* H5FDsubfiling_H */
