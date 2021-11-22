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

#define H5FD_IOC       (H5FD_ioc_init())
#define H5FD_IOC_VALUE H5_VFD_IOC

#ifndef H5FD_IOC_FAPL_T_MAGIC
#define H5FD_CURR_IOC_FAPL_T_VERSION 1
#define H5FD_IOC_FAPL_T_MAGIC        0xFED21331
#endif

/* Maximum length of a filename/path string in the Write-Only channel,
 * including the NULL-terminator.
 */
#define H5FD_IOC_PATH_MAX         4096
#define H5FD_IOC_THREAD_POOL_SIZE 4

/*
 *    Define the various constants to allow different allocations
 *    of subfile ranks.  The choices are self explanatory, starting
 *    with the default of one IO Concentrator (IOC) per node and
 *    lastly, defining a fixed number.
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
#define H5FD_SUBFILING_PATH_MAX 4096

typedef struct config_common_t {
    uint32_t        magic;                                  /* set to H5FD_SUBFILING_FAPL_T_MAGIC */
    uint32_t        version;                                /* set to H5FD_CURR_SUBFILING_FAPL_T_VERSION */
    int32_t         stripe_count;                           /* How many io concentrators */
    int64_t         stripe_depth;                           /* Max # of bytes in contigious IO to an IOC */
    ioc_selection_t ioc_selection;                          /* Method to select IO Concentrators */
    hid_t           ioc_fapl_id;                            /* The hid_t value of the stacked VFD  */
    int64_t         context_id;                             /* The value used to lookup an IOC context */
    char            file_dir[H5FD_SUBFILING_PATH_MAX + 1];  /* Directory where we find files */
    char            file_path[H5FD_SUBFILING_PATH_MAX + 1]; /* The user defined filename */
} config_common_t;

typedef struct H5FD_ioc_config_t {
    config_common_t common;
    int32_t         thread_pool_count;
} H5FD_ioc_config_t;

/* The information of this ioc */
typedef struct H5FD_ioc_t {
    H5FD_t pub; /* public stuff, must be first    */
    int    fd;  /* the filesystem file descriptor */

    H5FD_ioc_config_t fa; /* driver-specific file access properties */
    int               mpi_rank;
    int               mpi_size;
    H5FD_t *          ioc_file; /* native HDF5 file pointer (sec2) */

#ifndef H5_HAVE_WIN32_API
    /* On most systems the combination of device and i-node number uniquely
     * identify a file.  Note that Cygwin, MinGW and other Windows POSIX
     * environments have the stat function (which fakes inodes)
     * and will use the 'device + inodes' scheme as opposed to the
     * Windows code further below.
     */
    dev_t device; /* file device number   */
    ino_t inode;  /* file i-node number   */
#else
    /* Files in windows are uniquely identified by the volume serial
     * number and the file index (both low and high parts).
     *
     * There are caveats where these numbers can change, especially
     * on FAT file systems.  On NTFS, however, a file should keep
     * those numbers the same until renamed or deleted (though you
     * can use ReplaceFile() on NTFS to keep the numbers the same
     * while renaming).
     *
     * See the MSDN "BY_HANDLE_FILE_INFORMATION Structure" entry for
     * more information.
     *
     * http://msdn.microsoft.com/en-us/library/aa363788(v=VS.85).aspx
     */
    DWORD nFileIndexLow;
    DWORD nFileIndexHigh;
    DWORD dwVolumeSerialNumber;

    HANDLE hFile; /* Native windows file handle */
#endif /* H5_HAVE_WIN32_API */
    int hdf_fd_dup;
} H5FD_ioc_t;

#ifdef __cplusplus
extern "C" {
#endif
H5_DLL hid_t  H5FD_ioc_init(void);
H5_DLL herr_t H5Pset_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_ptr);
H5_DLL herr_t H5Pget_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_ptr);
H5_DLL void   H5FD_ioc_set_shutdown_flag(int flag);
H5_DLL void   H5FD_ioc_wait_thread_main(void);
H5_DLL void   H5FD_ioc_finalize_threads(void);
H5_DLL int    initialize_ioc_threads(void *_sf_context);
H5_DLL int    tpool_add_work(void *work);
H5_DLL void   begin_thread_exclusive(void);
H5_DLL void   end_thread_exclusive(void);
H5_DLL void   ioc__wait_for_serialize(void *msg);
H5_DLL void   ioc__release_dependency(int qid);

#ifdef __cplusplus
}
#endif

#endif
