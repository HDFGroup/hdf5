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
 * Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Monday, July 26, 1999
 */
#ifndef _H5FDprivate_H
#define _H5FDprivate_H

/* Include package's public header */
#include "H5FDpublic.h"

/* Private headers needed by this file */

/*
 * The MPI drivers are needed because there are
 * places where we check for things that aren't handled by these drivers.
 */
#include "H5FDmpi.h"            /* MPI-based file drivers		*/


/**************************/
/* Library Private Macros */
/**************************/

/* Length of filename buffer */
#define H5FD_MAX_FILENAME_LEN      1024

#define H5FD_AIO_OP__UNDEFINED                     0
#define H5FD_AIO_OP__READ                          1
#define H5FD_AIO_OP__WRITE                         2
#define H5FD_AIO_OP__FSYNC                         3
#define H5FD_AIO_OP__MAX_OP                        3

#define H5FD__H5FD_STATS_T_MAGIC 0x0FD05354

#define H5FD_UPDATE_STATS__AIO_OP_ATTEMPTED(stats_ptr, op) {	           \
    if((op)==H5FD_AIO_OP__READ)                                            \
        ((stats_ptr)->aio_reads_attempted)++;                              \
    else if((op)==H5FD_AIO_OP__WRITE)                                      \
        ((stats_ptr)->aio_writes_attempted)++;                             \
    else if((op)==H5FD_AIO_OP__FSYNC)                                      \
        ((stats_ptr)->aio_fsyncs_attempted)++; }

#define H5FD_UPDATE_STATS__AIO_OP_COMPLETED_SUCCESSFULLY(stats_ptr, op) {     \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_reads_completed_successfully)++;                    \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_writes_completed_successfully)++;                   \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsyncs_completed_successfully)++; }

#define H5FD_UPDATE_STATS__AIO_OP_QUEUE_ATTEMPTED(stats_ptr, op) {	      \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_read_queue_attempts)++;                             \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_write_queue_attempts)++;                            \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsync_queue_attempts)++; }

#define H5FD_UPDATE_STATS__AIO_OP_QUEUE_ATTEMPT_FAILURES(stats_ptr, op) {     \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_read_queue_attempt_failures)++;                     \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_write_queue_attempt_failures)++;                    \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsync_queue_attempt_failures)++; }

#define H5FD_UPDATE_STATS__AIO_OP_CONVERTED_TO_SIO(stats_ptr, op) {           \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_reads_converted_to_sio)++;                          \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_writes_converted_to_sio)++;                         \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsyncs_converted_to_sio)++; }

#define H5FD_UPDATE_STATS__AIO_OP_FAILURES(stats_ptr, op) {                   \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_read_failures)++;                                   \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_write_failures)++;                                  \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsync_failures)++; }

#define H5FD_UPDATE_STATS__AIO_OP_FAILURES_RECOVERED_VIA_SIO(stats_ptr, op) { \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_read_failures_recovered_via_sio)++;                 \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_write_failures_recovered_via_sio)++;                \
    else if((op)==H5FD_AIO_OP__FSYNC)                                         \
        ((stats_ptr)->aio_fsync_failures_recovered_via_sio)++; }

#define H5FD_UPDATE_STATS__AIO_OP_PARTIAL(stats_ptr, op) {                    \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_partial_reads)++;                                   \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_partial_writes)++; }

#define H5FD_UPDATE_STATS__AIO_OP_PARTIAL_RECOVERED_VIA_SIO(stats_ptr, op) {  \
    if((op)==H5FD_AIO_OP__READ)                                               \
        ((stats_ptr)->aio_partial_reads_recovered_via_sio)++;                 \
    else if((op)==H5FD_AIO_OP__WRITE)                                         \
        ((stats_ptr)->aio_partial_reads_recovered_via_sio)++; }



/****************************/
/* Library Private Typedefs */
/****************************/

typedef struct H5FD_stats_t {
    int32_t magic;
    hbool_t defined;
    hbool_t complete;
    uint64_t aio_reads_attempted;                       /* number of aio reads requested  */
    uint64_t aio_reads_completed_successfully;          /* number of aio reads requested, */
                                                        /* queued, and completed via aio  */
                                                        /* with all desired data read.    */
    uint64_t aio_read_queue_attempts;                   /* number of times we have tried  */
                                                        /* to queue an aio read.          */
    uint64_t aio_read_queue_attempt_failures;           /* number of times an attempt to  */
                                                        /* queue an aio read has failed   */
    uint64_t aio_reads_converted_to_sio;                /* number of aio read requests    */
                                                        /* that have been converted to    */
                                                        /* sio due to the inability to    */
                                                        /* queue the aio request.         */
    uint64_t aio_read_failures;                         /* number of aio reads that have  */
                                                        /* failed beyond recovery.        */
    uint64_t aio_read_failures_recovered_via_sio;       /* number of aio read failures    */
                                                        /* recovered by converting the    */
                                                        /* read to sio.                   */
    uint64_t aio_partial_reads;                         /* number of aio read requests    */
                                                        /* that were successful, but      */
                                                        /* returned less than the         */
                                                        /* requested amount of data.      */
    uint64_t aio_partial_reads_recovered_via_sio;       /* number of aio read requests    */
                                                        /* that returned less than the    */
                                                        /* requested amount of data, and  */
                                                        /* that were completed by         */
                                                        /* converting the unfulfilled     */
                                                        /* part of the read to sio.       */

    uint64_t aio_writes_attempted;                      /* number of aio writes requested */
    uint64_t aio_writes_completed_successfully;         /* number of aio writes requested,*/
                                                        /* queued, and completed with all */
                                                        /* desired data read.             */
    uint64_t aio_write_queue_attempts;                  /* number of times we have tried  */
                                                        /* to queue an aio write.         */
    uint64_t aio_write_queue_attempt_failures;          /* number of times an attempt to  */
                                                        /* queue an aio write has failed  */
    uint64_t aio_writes_converted_to_sio;               /* number of aio write requests   */
                                                        /* that have been converted to    */
                                                        /* sio due to the inability to    */
                                                        /* queue the aio request.         */
    uint64_t aio_write_failures;                        /* number of aio writes that have */
                                                        /* failed beyond recovery.        */
    uint64_t aio_write_failures_recovered_via_sio;      /* number of aio write failures   */
                                                        /* recovered by converting the    */
                                                        /* read to sio.                   */
    uint64_t aio_partial_writes;                        /* number of aio write requests   */
                                                        /* that were successful, but      */
                                                        /* wrote less than the requested  */
                                                        /* amount of data.                */
    uint64_t aio_partial_writes_recovered_via_sio;      /* number of aio write requests   */
                                                        /* that wrote less than the       */
                                                        /* requested amount of data, and  */
                                                        /* that were completed by         */
                                                        /* converting the unfulfilled     */
                                                        /* part of the write to sio.      */

    uint64_t aio_fsyncs_attempted;                      /* number of aio fsyncs requested */
    uint64_t aio_fsyncs_completed_successfully;         /* number of aio fsyncs requested,*/
                                                        /* queued, and completed with out */
                                                        /* error.                         */
    uint64_t aio_fsync_queue_attempts;                  /* number of times we have tried  */
                                                        /* to queue an aio fsync.         */
    uint64_t aio_fsync_queue_attempt_failures;          /* number of times an attempt to  */
                                                        /* queue an aio fsync has failed  */
    uint64_t aio_fsyncs_converted_to_sio;               /* number of aio fsync requests   */
                                                        /* that have been converted to    */
                                                        /* sio due to the inability to    */
                                                        /* queue the aio request.         */
    uint64_t aio_fsync_failures;                        /* number of aio fsyncs that have */
                                                        /* failed beyond recovery.        */
    uint64_t aio_fsync_failures_recovered_via_sio;      /* number of aio fsync failures   */
                                                        /* recovered by converting the    */
                                                        /* read to sio.                   */
} H5FD_stats_t;

/* File operations */
typedef enum {
    OP_UNKNOWN = 0,             /* Unknown last file operation */
    OP_READ = 1,                /* Last file I/O operation was a read */
    OP_WRITE = 2                /* Last file I/O operation was a write */
} H5FD_file_op_t;

/* private extended verion of H5FD_class_t */
#define H5FD__H5FD_PRIVATE_CLASS_T__MAGIC 0x48357063	/* 'H5pc' */
typedef struct H5FD_private_class_t {
    struct H5FD_class_t pub;
    int32_t magic;					/* Must be set to                    */
                                                        /* H5FD__H5FD_PRIVATE_CLASS_T__MAGIC */
    herr_t  (*get_stats)(H5FD_t *file, H5FD_stats_t *stats_ptr);
    herr_t  (*reset_stats)(H5FD_t *file);
} H5FD_private_class_t;


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

/* Forward declarations for prototype arguments */
struct H5P_genplist_t;
struct H5F_t;

H5_DLL int H5FD_term_interface(void);
H5_DLL H5FD_class_t *H5FD_get_class(hid_t id);
H5_DLL hsize_t H5FD_sb_size(H5FD_t *file);
H5_DLL herr_t H5FD_sb_encode(H5FD_t *file, char *name/*out*/, uint8_t *buf);
H5_DLL herr_t H5FD_sb_decode(H5FD_t *file, const char *name, const uint8_t *buf);
H5_DLL void *H5FD_fapl_get(H5FD_t *file);
H5_DLL herr_t H5FD_fapl_open(struct H5P_genplist_t *plist, hid_t driver_id, const void *driver_info);
H5_DLL herr_t H5FD_fapl_copy(hid_t driver_id, const void *fapl, void **copied_fapl);
H5_DLL herr_t H5FD_fapl_close(hid_t driver_id, void *fapl);
H5_DLL herr_t H5FD_dxpl_open(struct H5P_genplist_t *plist, hid_t driver_id, const void *driver_info);
H5_DLL herr_t H5FD_dxpl_copy(hid_t driver_id, const void *dxpl, void **copied_dxpl);
H5_DLL herr_t H5FD_dxpl_close(hid_t driver_id, void *dxpl);
H5_DLL hid_t H5FD_register(const void *cls, size_t size, hbool_t app_ref);
H5_DLL H5FD_t *H5FD_open(const char *name, unsigned flags, hid_t fapl_id,
		  haddr_t maxaddr);
H5_DLL herr_t H5FD_close(H5FD_t *file);
H5_DLL int H5FD_cmp(const H5FD_t *f1, const H5FD_t *f2);
H5_DLL int H5FD_query(const H5FD_t *f, unsigned long *flags/*out*/);
H5_DLL haddr_t H5FD_alloc(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, struct H5F_t *f,
    hsize_t size, haddr_t *align_addr, hsize_t *align_size);
H5_DLL herr_t H5FD_free(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type, struct H5F_t *f,
    haddr_t addr, hsize_t size);
H5_DLL htri_t H5FD_try_extend(H5FD_t *file, H5FD_mem_t type, struct H5F_t *f,
    haddr_t blk_end, hsize_t extra_requested);
H5_DLL haddr_t H5FD_get_eoa(const H5FD_t *file, H5FD_mem_t type);
H5_DLL herr_t H5FD_set_eoa(H5FD_t *file, H5FD_mem_t type, haddr_t addr);
H5_DLL haddr_t H5FD_get_eof(const H5FD_t *file);
H5_DLL haddr_t H5FD_get_maxaddr(const H5FD_t *file);
H5_DLL herr_t H5FD_get_feature_flags(const H5FD_t *file, unsigned long *feature_flags);
H5_DLL herr_t H5FD_get_fs_type_map(const H5FD_t *file, H5FD_mem_t *type_map);
H5_DLL herr_t H5FD_read(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type,
    haddr_t addr, size_t size, void *buf/*out*/);
H5_DLL herr_t H5FD_write(H5FD_t *file, hid_t dxpl_id, H5FD_mem_t type,
    haddr_t addr, size_t size, const void *buf);
H5_DLL herr_t H5FD_flush(H5FD_t *file, hid_t dxpl_id, hbool_t closing);
H5_DLL herr_t H5FD_truncate(H5FD_t *file, hid_t dxpl_id, hbool_t closing);
H5_DLL herr_t H5FD_get_fileno(const H5FD_t *file, unsigned long *filenum);
H5_DLL herr_t H5FD_get_vfd_handle(H5FD_t *file, hid_t fapl, void** file_handle);
H5_DLL herr_t H5FD_set_base_addr(H5FD_t *file, haddr_t base_addr);
H5_DLL haddr_t H5FD_get_base_addr(const H5FD_t *file);
H5_DLL herr_t H5FD_aio_read(H5FD_t *file, H5FD_mem_t type, hid_t dxpl, haddr_t addr, size_t size, void *buffer, void **ctlblk_ptr_ptr);
H5_DLL herr_t H5FD_aio_write(H5FD_t *file, H5FD_mem_t type, hid_t dxpl, haddr_t addr, size_t size, void *buffer, void **ctlblk_ptr_ptr);
H5_DLL herr_t H5FD_aio_test(H5FD_t *file, hbool_t *done_ptr, void *ctlblk_ptr);
H5_DLL herr_t H5FD_aio_wait(H5FD_t *file, void *ctlblk_ptr);
H5_DLL herr_t H5FD_aio_finish(H5FD_t *file, int *errno_ptr, void *ctlblk_ptr);
H5_DLL herr_t H5FD_aio_fsync(H5FD_t *file, void **ctlblk_ptr_ptr);
H5_DLL herr_t H5FD_aio_cancel(H5FD_t *file, void *ctlblk_ptr);
H5_DLL herr_t H5FD_fsync(H5FD_t *file, hid_t dxpl);
H5_DLL herr_t H5FD_aggregate_stats(H5FD_stats_t *base_stats_ptr, H5FD_stats_t *new_stats_ptr);
H5_DLL herr_t H5FD_copy_stats(H5FD_stats_t *dest_stats_ptr, H5FD_stats_t *src_stats_ptr);
H5_DLL herr_t H5FD_dump_stats(FILE * out_stream,H5FD_stats_t *stats_ptr,const char *label_ptr);
H5_DLL herr_t H5FD_get_stats(H5FD_t *file, H5FD_stats_t *stats_ptr);
H5_DLL herr_t H5FD_initialize_stats(H5FD_stats_t *stats_ptr);
H5_DLL herr_t H5FD_reset_stats(H5FD_t *file);

#endif /* !_H5FDprivate_H */

