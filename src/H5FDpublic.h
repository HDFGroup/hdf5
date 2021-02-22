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
 * Programmer:  Robb Matzke
 *              Monday, July 26, 1999
 */
#ifndef _H5FDpublic_H
#define _H5FDpublic_H

/* Public headers needed by this file */
#include "H5public.h"  /* Generic Functions */
#include "H5Fpublic.h" /* Files */

/*****************/
/* Public Macros */
/*****************/

#define H5FD_VFD_DEFAULT 0 /* Default VFL driver value */

/* Define VFL driver features that can be enabled on a per-driver basis */
/* These are returned with the 'query' function pointer in H5FD_class_t */
/*
 * Defining H5FD_FEAT_AGGREGATE_METADATA for a VFL driver means that
 * the library will attempt to allocate a larger block for metadata and
 * then sub-allocate each metadata request from that larger block.
 */
#define H5FD_FEAT_AGGREGATE_METADATA 0x00000001
/*
 * Defining H5FD_FEAT_ACCUMULATE_METADATA for a VFL driver means that
 * the library will attempt to cache metadata as it is written to the file
 * and build up a larger block of metadata to eventually pass to the VFL
 * 'write' routine.
 *
 * Distinguish between updating the metadata accumulator on writes and
 * reads.  This is particularly (perhaps only, even) important for MPI-I/O
 * where we guarantee that writes are collective, but reads may not be.
 * If we were to allow the metadata accumulator to be written during a
 * read operation, the application would hang.
 */
#define H5FD_FEAT_ACCUMULATE_METADATA_WRITE 0x00000002
#define H5FD_FEAT_ACCUMULATE_METADATA_READ  0x00000004
#define H5FD_FEAT_ACCUMULATE_METADATA                                                                        \
    (H5FD_FEAT_ACCUMULATE_METADATA_WRITE | H5FD_FEAT_ACCUMULATE_METADATA_READ)
/*
 * Defining H5FD_FEAT_DATA_SIEVE for a VFL driver means that
 * the library will attempt to cache raw data as it is read from/written to
 * a file in a "data seive" buffer.  See Rajeev Thakur's papers:
 *  http://www.mcs.anl.gov/~thakur/papers/romio-coll.ps.gz
 *  http://www.mcs.anl.gov/~thakur/papers/mpio-high-perf.ps.gz
 */
#define H5FD_FEAT_DATA_SIEVE 0x00000008
/*
 * Defining H5FD_FEAT_AGGREGATE_SMALLDATA for a VFL driver means that
 * the library will attempt to allocate a larger block for "small" raw data
 * and then sub-allocate "small" raw data requests from that larger block.
 */
#define H5FD_FEAT_AGGREGATE_SMALLDATA 0x00000010
/*
 * Defining H5FD_FEAT_IGNORE_DRVRINFO for a VFL driver means that
 * the library will ignore the driver info that is encoded in the file
 * for the VFL driver.  (This will cause the driver info to be eliminated
 * from the file when it is flushed/closed, if the file is opened R/W).
 */
#define H5FD_FEAT_IGNORE_DRVRINFO 0x00000020
/*
 * Defining the H5FD_FEAT_DIRTY_DRVRINFO_LOAD for a VFL driver means that
 * the library will mark the driver info dirty when the file is opened
 * R/W.  This will cause the driver info to be re-encoded when the file
 * is flushed/closed.
 */
#define H5FD_FEAT_DIRTY_DRVRINFO_LOAD 0x00000040
/*
 * Defining H5FD_FEAT_POSIX_COMPAT_HANDLE for a VFL driver means that
 * the handle for the VFD (returned with the 'get_handle' callback) is
 * of type 'int' and is compatible with POSIX I/O calls.
 */
#define H5FD_FEAT_POSIX_COMPAT_HANDLE 0x00000080
/*
 * Defining H5FD_FEAT_HAS_MPI for a VFL driver means that
 * the driver makes use of MPI communication and code may retrieve
 * communicator/rank information from it
 */
#define H5FD_FEAT_HAS_MPI 0x00000100
/*
 * Defining the H5FD_FEAT_ALLOCATE_EARLY for a VFL driver will force
 * the library to use the H5D_ALLOC_TIME_EARLY on dataset create
 * instead of the default H5D_ALLOC_TIME_LATE
 */
#define H5FD_FEAT_ALLOCATE_EARLY 0x00000200
/*
 * Defining H5FD_FEAT_ALLOW_FILE_IMAGE for a VFL driver means that
 * the driver is able to use a file image in the fapl as the initial
 * contents of a file.
 */
#define H5FD_FEAT_ALLOW_FILE_IMAGE 0x00000400
/*
 * Defining H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS for a VFL driver
 * means that the driver is able to use callbacks to make a copy of the
 * image to store in memory.
 */
#define H5FD_FEAT_CAN_USE_FILE_IMAGE_CALLBACKS 0x00000800
/*
 * Defining H5FD_FEAT_SUPPORTS_SWMR_IO for a VFL driver means that the
 * driver supports the single-writer/multiple-readers I/O pattern.
 */
#define H5FD_FEAT_SUPPORTS_SWMR_IO 0x00001000
/*
 * Defining H5FD_FEAT_USE_ALLOC_SIZE for a VFL driver
 * means that the library will just pass the allocation size to the
 * the driver's allocation callback which will eventually handle alignment.
 * This is specifically used for the multi/split driver.
 */
#define H5FD_FEAT_USE_ALLOC_SIZE 0x00002000
/*
 * Defining H5FD_FEAT_PAGED_AGGR for a VFL driver
 * means that the driver needs special file space mapping for paged aggregation.
 * This is specifically used for the multi/split driver.
 */
#define H5FD_FEAT_PAGED_AGGR 0x00004000
/*
 * Defining H5FD_FEAT_DEFAULT_VFD_COMPATIBLE for a VFL driver
 * that creates a file which is compatible with the default VFD.
 * Generally, this means that the VFD creates a single file that follows
 * the canonical HDF5 file format.
 * Regarding the Splitter VFD specifically, only drivers with this flag
 * enabled may be used as the Write-Only (W/O) channel driver.
 */
#define H5FD_FEAT_DEFAULT_VFD_COMPATIBLE 0x00008000

/*******************/
/* Public Typedefs */
/*******************/

/* Types of allocation requests: see H5Fpublic.h  */
typedef enum H5F_mem_t H5FD_mem_t;

/* Define enum for the source of file image callbacks */
typedef enum {
    H5FD_FILE_IMAGE_OP_NO_OP,
    H5FD_FILE_IMAGE_OP_PROPERTY_LIST_SET,
    H5FD_FILE_IMAGE_OP_PROPERTY_LIST_COPY,
    H5FD_FILE_IMAGE_OP_PROPERTY_LIST_GET,
    H5FD_FILE_IMAGE_OP_PROPERTY_LIST_CLOSE,
    H5FD_FILE_IMAGE_OP_FILE_OPEN,
    H5FD_FILE_IMAGE_OP_FILE_RESIZE,
    H5FD_FILE_IMAGE_OP_FILE_CLOSE
} H5FD_file_image_op_t;

/* Define structure to hold file image callbacks */
typedef struct {
    void *(*image_malloc)(size_t size, H5FD_file_image_op_t file_image_op, void *udata);
    void *(*image_memcpy)(void *dest, const void *src, size_t size, H5FD_file_image_op_t file_image_op,
                          void *udata);
    void *(*image_realloc)(void *ptr, size_t size, H5FD_file_image_op_t file_image_op, void *udata);
    herr_t (*image_free)(void *ptr, H5FD_file_image_op_t file_image_op, void *udata);
    void *(*udata_copy)(void *udata);
    herr_t (*udata_free)(void *udata);
    void *udata;
} H5FD_file_image_callbacks_t;

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/* Function prototypes */
/* Allows querying a VFD ID for features before the file is opened */
H5_DLL herr_t H5FDdriver_query(hid_t driver_id, unsigned long *flags /*out*/);

#ifdef __cplusplus
}
#endif
#endif
