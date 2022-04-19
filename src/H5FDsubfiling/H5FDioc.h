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

#ifdef H5_HAVE_IOC_VFD
#define H5FD_IOC (H5FDperform_init(H5FD_ioc_init))
#else
#define H5FD_IOC (H5I_INVALID_HID)
#endif

#define H5FD_IOC_NAME "ioc"

#ifndef H5FD_IOC_FAPL_MAGIC
#define H5FD_CURR_IOC_FAPL_VERSION 1
#define H5FD_IOC_FAPL_MAGIC        0xFED21331
#endif

/* Maximum length of a filename/path string in the Write-Only channel,
 * including the NULL-terminator.
 */
#define H5FD_IOC_PATH_MAX         4096
#define H5FD_IOC_THREAD_POOL_SIZE 4

/*
 * Environment variables interpreted by the IOC VFD
 */
#define H5_IOC_THREAD_POOL_COUNT "H5_IOC_THREAD_POOL_COUNT"

/*
 * Define the various constants to allow different allocations
 * of subfile ranks.  The choices are self explanatory, starting
 * with the default of one IO Concentrator (IOC) per node and
 * lastly, defining a fixed number.
 */
typedef enum {
    SELECT_IOC_ONE_PER_NODE = 0, /* Default */
    SELECT_IOC_EVERY_NTH_RANK,   /* Starting at rank 0, select-next += N */
    SELECT_IOC_WITH_CONFIG,      /* NOT IMPLEMENTED: Read-from-file       */
    SELECT_IOC_TOTAL,            /* Starting at rank 0, mpi_size / total   */
    ioc_selection_options        /* (Uses same selection as every Nth rank) */
} ioc_selection_t;

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
    uint32_t        magic;                            /* set to H5FD_IOC_FAPL_MAGIC */
    uint32_t        version;                          /* set to H5FD_CURR_IOC_FAPL_VERSION */
    int32_t         stripe_count;                     /* How many io concentrators */
    int64_t         stripe_depth;                     /* Max # of bytes in contiguous IO to an IOC */
    ioc_selection_t ioc_selection;                    /* Method to select IO Concentrators */
    hid_t           ioc_fapl_id;                      /* The hid_t value of the stacked VFD  */
    int64_t         context_id;                       /* The value used to lookup an IOC context */
    char            file_dir[H5FD_IOC_PATH_MAX + 1];  /* Directory where we find files */
    char            file_path[H5FD_IOC_PATH_MAX + 1]; /* The user defined filename */
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
H5_DLL void   begin_thread_exclusive(void);
H5_DLL void   end_thread_exclusive(void);
H5_DLL void   ioc__wait_for_serialize(void *msg);
H5_DLL void   ioc__release_dependency(int qid);

#ifdef __cplusplus
}
#endif

#endif
