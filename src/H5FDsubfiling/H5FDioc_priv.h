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
 * Private definitions for HDF5 IOC VFD
 */

#ifndef H5FDioc_priv_H
#define H5FDioc_priv_H

/********************/
/* Standard Headers */
/********************/

/* TODO: review needed headers */
#include <assert.h>
#include <libgen.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mpi.h"

/**************/
/* H5 Headers */
/**************/

#include "H5private.h"   /* Generic Functions                        */
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5FDioc.h"     /* IOC VFD                                  */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Pprivate.h"  /* Property lists                           */

#include "subfiling_common.h"

#if 1 /* JRM */ /* For now, H5FDioc_priv.h needs mercury.  Since the code that needs it will                 \
                 * move to its own header, just hack it for now.                                             \
                 */
#include "mercury_thread.h"
#include "mercury_thread_mutex.h"
#include "mercury_thread_pool.h"
#endif /* JRM */

#define H5FD_IOC__COLLECT_STATS TRUE

/****************************************************************************
 *
 * IOC I/O Queue management macros:
 *
 * The following macros perform the necessary operations on the IOC I/O
 * Queue, which is implemented as a doubly linked list of instances of
 * H5FD_ioc_io_queue_entry_t.
 *
 * WARNING: q_ptr->q_mutex must be held when these macros are executed..
 *
 * At present, the necessary operations are append (insert an entry at the
 * end of the queue), and delete (remove an entry from the queue).
 *
 * At least initially, all sanity checking is done with asserts, as the
 * the existing I/O concentrator code is not well integrated into the HDF5
 * error reporting system.  This will have to be revisited for a production
 * version, but it should be sufficient for now.
 *
 *                                                 JRM -- 11/2/21
 *
 ****************************************************************************/

#define H5FD_IOC__IO_Q_ENTRY_MAGIC 0x1357

/* clang-format off */

#define H5FD_IOC__Q_APPEND(q_ptr, entry_ptr)                                                      \
do {                                                                                              \
    HDassert(q_ptr);                                                                              \
    HDassert((q_ptr)->magic == H5FD_IOC__IO_Q_MAGIC);                                             \
    HDassert((((q_ptr)->q_len == 0) && ((q_ptr)->q_head == NULL) && ((q_ptr)->q_tail == NULL)) || \
             (((q_ptr)->q_len > 0) && ((q_ptr)->q_head != NULL) && ((q_ptr)->q_tail != NULL)));   \
    HDassert(entry_ptr);                                                                          \
    HDassert((entry_ptr)->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);                                   \
    HDassert((entry_ptr)->next == NULL);                                                          \
    HDassert((entry_ptr)->prev == NULL);                                                          \
    HDassert((entry_ptr)->in_progress == FALSE);                                                  \
                                                                                                  \
    if ( ((q_ptr)->q_head) == NULL )                                                              \
    {                                                                                             \
       ((q_ptr)->q_head) = (entry_ptr);                                                           \
       ((q_ptr)->q_tail) = (entry_ptr);                                                           \
    }                                                                                             \
    else                                                                                          \
    {                                                                                             \
       ((q_ptr)->q_tail)->next = (entry_ptr);                                                     \
       (entry_ptr)->prev = ((q_ptr)->q_tail);                                                     \
       ((q_ptr)->q_tail) = (entry_ptr);                                                           \
    }                                                                                             \
    ((q_ptr)->q_len)++;                                                                           \
} while ( FALSE ) /* H5FD_IOC__Q_APPEND() */

#define H5FD_IOC__Q_REMOVE(q_ptr, entry_ptr)                                                                         \
do {                                                                                                                 \
    HDassert(q_ptr);                                                                                                 \
    HDassert((q_ptr)->magic == H5FD_IOC__IO_Q_MAGIC);                                                                \
    HDassert((((q_ptr)->q_len == 1) && ((q_ptr)->q_head ==((q_ptr)->q_tail)) && ((q_ptr)->q_head == (entry_ptr))) || \
             (((q_ptr)->q_len > 0) && ((q_ptr)->q_head != NULL) && ((q_ptr)->q_tail != NULL)));                      \
    HDassert(entry_ptr);                                                                                             \
    HDassert((entry_ptr)->magic == H5FD_IOC__IO_Q_ENTRY_MAGIC);                                                      \
    HDassert((((q_ptr)->q_len == 1) && ((entry_ptr)->next == NULL) && ((entry_ptr)->prev == NULL)) ||                \
             (((q_ptr)->q_len > 1) && (((entry_ptr)->next != NULL) || ((entry_ptr)->prev != NULL))));                \
    HDassert((entry_ptr)->in_progress == TRUE);                                                                      \
                                                                                                                     \
    {                                                                                                                \
       if ( (((q_ptr)->q_head)) == (entry_ptr) )                                                                     \
       {                                                                                                             \
          (((q_ptr)->q_head)) = (entry_ptr)->next;                                                                   \
          if ( (((q_ptr)->q_head)) != NULL )                                                                         \
             (((q_ptr)->q_head))->prev = NULL;                                                                       \
       }                                                                                                             \
       else                                                                                                          \
       {                                                                                                             \
          (entry_ptr)->prev->next = (entry_ptr)->next;                                                               \
       }                                                                                                             \
       if (((q_ptr)->q_tail) == (entry_ptr) )                                                                        \
       {                                                                                                             \
          ((q_ptr)->q_tail) = (entry_ptr)->prev;                                                                     \
          if ( ((q_ptr)->q_tail) != NULL )                                                                           \
             ((q_ptr)->q_tail)->next = NULL;                                                                         \
       }                                                                                                             \
       else                                                                                                          \
       {                                                                                                             \
          (entry_ptr)->next->prev = (entry_ptr)->prev;                                                               \
       }                                                                                                             \
       (entry_ptr)->next = NULL;                                                                                     \
       (entry_ptr)->prev = NULL;                                                                                     \
       ((q_ptr)->q_len)--;                                                                                           \
    }                                                                                                                \
} while ( FALSE ) /* H5FD_IOC__Q_REMOVE() */

/* clang-format on */

/****************************************************************************
 *
 * structure H5FD_ioc_io_queue_entry
 *
 * magic:  Unsigned 32 bit integer always set to H5FD_IOC__IO_Q_ENTRY_MAGIC.
 *         This field is used to validate pointers to instances of
 *         H5FD_ioc_io_queue_entry_t.
 *
 * next:   Next pointer in the doubly linked list used to implement
 *         the IOC I/O Queue.  This field points to the next entry
 *         in the queue, or NULL if there is no next entry.
 *
 * prev:   Prev pointer in the doubly linked list used to implement
 *         the IOC I/O Queue.  This field points to the previous entry
 *         in the queue, or NULL if there is no previous entry.
 *
 * in_progress: Boolean flag that must be FALSE when the entry is inserted
 *         into the IOC I/O Queue, and set to TRUE when the entry is dispatched
 *         to the worker thread pool for execution.
 *
 *         When in_progress is FALS, the entry is said to be pending.
 *
 * counter: uint32_t containing a serial number assigned to this IOC
 *         I/O Queue entry.  Note that this will roll over on long
 *         computations, and thus is not in general unique.
 *
 *         The counter fields is used to construct a tag to distinguish
 *         multiple concurrent I/O requests from a give rank, and thus
 *         this should not be a problem as long as there is sufficient
 *         time between roll overs.  As only the lower bits of the counter
 *         are used in tag construction, this is more frequent than the
 *         size of the counter field would suggest -- albeit hopefully
 *         still infrequent enough.
 *
 * wk_req: Instance of sf_work_request_t.  Replace with individual
 *         fields when convenient.
 *
 *
 * Statistics:
 *
 * The following fields are only defined if H5FD_IOC__COLLECT_STATS is TRUE.
 * They are intended to allow collection of basic statistics on the
 * behaviour of the IOC I/O Queue for purposes of debugging and performance
 * optimization.
 *
 * q_time:      uint64_t containing the time the entry was place on the
 *              IOC I/O Queue in usec after the UNIX epoch.
 *
 *              This value is used to compute the queue wait time, and the
 *              total processing time for the entry.
 *
 * dispatch_time:  uint64_t containing the time the entry is dispatched in
 *              usec after the UNIX epoch.  This field is undefined if the
 *              entry is pending.
 *
 *              This value is used to compute the execution time for the
 *              entry.
 *
 ****************************************************************************/

typedef struct H5FD_ioc_io_queue_entry {

    uint32_t                        magic;
    struct H5FD_ioc_io_queue_entry *next;
    struct H5FD_ioc_io_queue_entry *prev;
    hbool_t                         in_progress;
    uint32_t                        counter;

    /* rework these fields */ /* JRM */
    sf_work_request_t     wk_req;
    struct hg_thread_work thread_wk;

    /* statistics */
#if H5FD_IOC__COLLECT_STATS

    uint64_t q_time;
    uint64_t dispatch_time;

#endif /* H5FD_IOC__COLLECT_STATS */

} H5FD_ioc_io_queue_entry_t;

#if 0 /* JRM */ /* keep this copy for convenience for now */
typedef struct {
    /* {Datasize, Offset, FileID} */
    int64_t header[3];        /* The basic RPC input plus       */
    int     tag;              /* the supplied OPCODE tag        */
    int     source;           /* Rank of who sent the message   */
    int     subfile_rank;     /* The IOC rank                   */
    hid_t   context_id;       /* context to be used to complete */
    double  start_time;       /* the request, + time of receipt */
                              /* from which we calc Time(queued) */
    void *buffer;             /* for writes, we keep the buffer */
                              /* around for awhile...           */
    volatile int in_progress; /* Not used!               */
    volatile int serialize;   /* worker thread needs to wait while true */
    volatile int dependents;  //* If current work item has dependents */
    int          depend_id;   /* work queue index of the dependent */
} sf_work_request_t;

struct hg_thread_work {
    hg_thread_func_t func;
    void *           args;
    HG_QUEUE_ENTRY(hg_thread_work) entry; /* Internal */
};

#endif /* JRM */

/****************************************************************************
 *
 * structure H5FD_ioc_io_queue
 *
 * This is a temporary structure -- its fields should be moved to an I/O
 * concentrator Catchall structure eventually.
 *
 * The fields of this structure support the io queue used to receive and
 * sequence I/O requests for execution by the worker threads.  The rules
 * for sequencing are as follows:
 *
 * 1) Non-overlaping I/O requests must be fed to the worker threads in
 *    the order received, and may execute concurrently
 *
 * 2) Overlapping read requests must be fed to the worker threads in
 *    the order received, but may execute concurrently.
 *
 * 3) If any pair of I/O requests overlap, and at least one is a write
 *    request, they must be executed in strict arrival order, and the
 *    first must complete before the second starts.
 *
 * Due to the strict ordering requirement in rule 3, entries must be
 * inserted at the tail of the queue in receipt order, and retained on
 * the queue until completed.  Entries in the queue are marked pending
 * when inserted on the queue, in progress when handed to a worker
 * thread, and deleted from the queue when completed.
 *
 * The dispatch algorithm is as follows:
 *
 * 1) Set X equal to the element at the head of the queue.
 *
 * 2) If X is pending, and there exists no prior element (i.e. between X
 *    and the head of the queue) that intersects with X, goto 5).
 *
 * 3) If X is pending, X is a read, and all prior intersecting elements
 *    are reads, goto 5).
 *
 * 4) If X is in progress, or if any prior intersecting element is a
 *    write, or if X is a write, set X equal to its successor in the
 *    queue (i.e. the next element further down the queue from the head)
 *    and goto 2)  If there is no next element, exit without dispatching
 *    any I/O request.
 *
 * 5) If we get to 5, X must be pending.  Mark it in progress, and
 *    dispatch it.  If the number of in progress entries is less than
 *    the number of worker threads, and X has a successor in the queue,
 *    set X equal to its predecessor, and goto 2).  Otherwise exit without
 *    dispatching further I/O requests.
 *
 * Note that the above dispatch algorithm doesn't address collective
 * I/O requests -- this should be OK for now, but it will have to
 * addressed prior to production release.
 *
 * On I/O request completion, worker threads must delete their assigned
 * I/O requests from the queue, check to see if there are any pending
 * requests, and trigger the dispatch algorithm if there are.
 *
 * The fields in the structure are discussed individually below.
 *
 * magic:  Unsigned 32 bit integer always set to H5FD_IOC__IO_Q_MAGIC.
 *         This field is used to validate pointers to instances of
 *         H5C_t.
 *
 * q_head: Pointer to the head of the doubly linked list of entries in
 *         the I/O queue.
 *
 *         This field is NULL if the I/O queue is empty.
 *
 * q_tail: Pointer to the tail of the doubly linked list of entries in
 *         the I/O queue.
 *
 *         This field is NULL if the I/O queue is empty.
 *
 * num_pending:  Number of I/O request pending on the I/O queue.
 *
 * num_in_progress: Number of I/O requests in progress on the I/O queue.
 *
 * q_len:  Number of I/O requests on the I/O queue.  Observe that q_len
 *         must equal (num_pending + num_in_progress).
 *
 * req_counter: unsigned 16 bit integer used to provide a "unique" tag for
 *         each I/O request.  This value is incremented by 1, and then
 *         passed to the worker thread where its lower bits are incorporated
 *         into the tag used to disambiguate multiple, concurrent I/O
 *         requests from a single rank.  The value is 32 bits, as MPI tags
 *         are limited to 32 bits.  The value is unsigned as it is expected
 *         to wrap around once its maximum value is reached.
 *
 * q_mutex: Mutex used to ensure that only one thread accesses the IOC I/O
 *         Queue at once.  This mutex must be held to access of modify
 *         all fields of the
 *
 *
 * Statistics:
 *
 * The following fields are only defined if H5FD_IOC__COLLECT_STATS is TRUE.
 * They are intended to allow collection of basic statistics on the
 * behaviour of the IOC I/O Queue for purposes of debugging and performance
 * optimization.
 *
 * max_q_len: Maximum number of requests residing on the IOC I/O Queue at
 *         any point in time in the current run.
 *
 * max_num_pending: Maximum number of pending requests residing on the IOC
 *         I/O Queue at any point in time in the current run.
 *
 * max_num_in_progress: Maximum number of in progress requests residing on
 *         the IOC I/O Queue at any point in time in the current run.
 *
 * ind_read_requests:  Number of independent read requests received by the
 *          IOC to date.
 *
 * ind_write_requests Number of independent write requests received by the
 *          IOC to date.
 *
 * truncate_requests:  Number of truncate requests received by the IOC to
 *           date.
 *
 * get_eof_requests: Number fo get EOF request received by the IO to date.
 *
 * requests_queued: Number of I/O requests received and placed on the IOC
 *          I/O queue.
 *
 * requests_dispatched: Number of I/O requests dispatched for execution by
 *          the worker threads.
 *
 * requests_completed: Number of I/O requests completed by the worker threads.
 *          Observe that on file close, requests_queued, requests_dispatched,
 *          and requests_completed should be equal.
 *
 ****************************************************************************/

#define H5FD_IOC__IO_Q_MAGIC 0x2468

typedef struct H5FD_ioc_io_queue {

    uint32_t                   magic;
    H5FD_ioc_io_queue_entry_t *q_head;
    H5FD_ioc_io_queue_entry_t *q_tail;
    int32_t                    num_pending;
    int32_t                    num_in_progress;
    int32_t                    q_len;
    uint32_t                   req_counter;
    hg_thread_mutex_t          q_mutex;

    /* statistics */
#if H5FD_IOC__COLLECT_STATS
    int32_t max_q_len;
    int32_t max_num_pending;
    int32_t max_num_in_progress;
    int64_t ind_read_requests;
    int64_t ind_write_requests;
    int64_t truncate_requests;
    int64_t get_eof_requests;
    int64_t requests_queued;
    int64_t requests_dispatched;
    int64_t requests_completed;
#endif /* H5FD_IOC__COLLECT_STATS */

} H5FD_ioc_io_queue_t;

/*
 * Structure definitions to enable async io completions
 * We first define a structure which contains the basic
 * input arguments for the functions which were originally
 * invoked.  See below.
 */
typedef struct _client_io_args {
    int         ioc;        /* ID of the IO Concentrator handling this IO.   */
    hid_t       context_id; /* The context id provided for the read or write */
    int64_t     offset;     /* The file offset for the IO operation          */
    int64_t     elements;   /* How many bytes                                */
    void *      data;       /* A pointer to the (contiguous) data segment    */
    MPI_Request io_req;     /* An MPI request to allow the code to loop while */
                            /* making progress on multiple IOs               */
} io_args_t;

typedef struct _client_io_func {
    int (*io_function)(void *this_io); /* pointer to a completion function */
    io_args_t io_args;                 /* arguments passed to the completion function   */
    int       pending;                 /* The function is complete (0) or pending (1)?  */
} io_func_t;

typedef struct _io_req {
    struct _io_req *prev;            /* A simple list structure containing completion */
    struct _io_req *next;            /* functions. These should get removed as IO ops */
    io_func_t       completion_func; /* are completed */
} io_req_t;

extern H5FD_ioc_io_queue_t io_queue_g;
extern atomic_int          sf_work_pending;
extern atomic_int          sf_io_ops_pending;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL int ioc_main(int64_t context_id);

H5_DLL char * H5FD__get_file_directory(void *h5file);
H5_DLL herr_t H5FD__dataset_write_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                                             int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
                                             hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
                                             const void *buf);
H5_DLL herr_t H5FD__dataset_read_contiguous(hid_t h5_file_id, haddr_t dataset_baseAddr, size_t dtype_extent,
                                            int mpi_rank, int mpi_size, void *_dset, hid_t mem_type_id,
                                            hid_t mem_space_id, hid_t file_space_id, hid_t plist_id,
                                            void *buf);

/* return arguments are vector of vectors - function return is the length
 * (depth) of the sub vectors. Note that we don't need to include the
 * MPI_Datatype return argument!
 */
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
#if 0  /* JRM */
H5_DLL int    initialize_ioc_threads(void *sf_context);
#endif /* JRM */
H5_DLL herr_t H5FD__write_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                                          const void *bufs[] /* data_in */);

H5_DLL herr_t H5FD__read_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                                         void *bufs[] /* data_out */);
#if 0  /* JRM */
H5_DLL int queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
#else  /* JRM */
H5_DLL int queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm,
                             uint32_t counter);
#endif /* JRM */

H5_DLL int queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);

H5_DLL int sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank);

H5_DLL int sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank);

H5_DLL int sf_truncate(int fd, int64_t length, int subfile_rank);

H5_DLL int report_sf_eof(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);

H5_DLL int write__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset,
                                    int64_t elements, int dtype_extent, const void *data, io_req_t **io_req);
H5_DLL int read__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                                   int dtype_extent, void *data, io_req_t **io_req);

H5_DLL H5FD_ioc_io_queue_entry_t *H5FD_ioc__alloc_io_q_entry(void);
H5_DLL void                       H5FD_ioc__complete_io_q_entry(H5FD_ioc_io_queue_entry_t *entry_ptr);
H5_DLL void                       H5FD_ioc__dispatch_elegible_io_q_entries(void);
H5_DLL void                       H5FD_ioc__free_io_q_entry(H5FD_ioc_io_queue_entry_t *q_entry_ptr);
H5_DLL void                       H5FD_ioc__queue_io_q_entry(sf_work_request_t *wk_req_ptr);

H5_DLL void manage_client_logfile(int client_rank, int flag_value);

#ifdef __cplusplus
}
#endif

#endif /* H5FDioc_priv_H */
