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
 * Purpose: Public, shared definitions for Mirror VFD & remote Writer.
 */

#ifndef H5FDsubfiling_priv_H
#define H5FDsubfiling_priv_H

/********************/
/* Standard Headers */
/********************/

#include <assert.h>
#include <libgen.h>
#include <stdatomic.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/**************/
/* H5 Headers */
/**************/
#include "H5CXprivate.h" /* API Contexts                             */
#include "H5Dprivate.h"  /* Datasets                                 */
#include "H5Eprivate.h"  /* Error handling                           */
#include "H5Iprivate.h"  /* IDs                                      */
#include "H5Ipublic.h"
#include "H5MMprivate.h" /* Memory management                        */
#include "H5Pprivate.h"  /* Property lists                           */
#include "H5private.h"   /* Generic Functions                        */
#include "H5FDioc.h"

#include "mpi.h"

#ifdef __cplusplus
extern "C" {
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

#ifndef H5FD_SUBFILING_FAPL_T_MAGIC
#define H5FD_CURR_SUBFILING_FAPL_T_VERSION 1
#define H5FD_SUBFILING_FAPL_T_MAGIC        0xFED01331
#endif

#ifndef H5FD_IOC_FAPL_T_MAGIC
#define H5FD_CURR_IOC_FAPL_T_VERSION 1
#define H5FD_IOC_FAPL_T_MAGIC        0xFED21331
#endif

#define DRIVER_INFO_MESSAGE_MAX_INFO   65536
#define DRIVER_INFO_MESSAGE_MAX_LENGTH 65552 /* MAX_INFO + sizeof(info_header_t) */

#define K(n)                      ((n)*1024)
#define M(n)                      ((n) * (1024 * 1024))
#define H5FD_DEFAULT_STRIPE_DEPTH M(32)

typedef struct stat_record {
    int64_t op_count; /* How many ops in total */
    double  min;      /* minimum (time)         */
    double  max;      /* maximum (time)        */
    double  total;    /* average (time)        */
} stat_record_t;

typedef enum stat_category { /* Stat (OP) Categories  */
                             WRITE_STAT = 0,
                             WRITE_WAIT,
                             READ_STAT,
                             READ_WAIT,
                             FOPEN_STAT,
                             FCLOSE_STAT,
                             QUEUE_STAT,
                             TOTAL_STAT_COUNT
} stat_category_t;

typedef struct _info_header { /* Header for a driver info message */
    uint8_t version;
    uint8_t unused_1;
    uint8_t unused_2;
    uint8_t unused_3;    /* Actual info message length, but  */
    int32_t info_length; /* CANNOT exceed 64k (65552) bytes  */
    char    vfd_key[8];  /* 's' 'u' 'b' 'f' 'i' 'l' 'i' 'n'  */
} info_header_t;

/* THE following definitions are used between H5FDsubfile_mpi.c
 * and H5FDioc_threads.c
 *
 * MPI Tags are 32 bits, we treat them as unsigned
 * to allow the use of the available bits for RPC
 * selections, i.e. a message from the VFD read or write functions
 * to an IO Concentrator.  The messages themselves are in general
 * ONLY 3 int64_t values which define a) the data size to be read
 * or written, b) the file offset where the data will be read from
 * or stored, and c) the context_id allows the IO concentrator to
 * locate the IO context for the new IO transaction.
 *
 *    0000
 *    0001 READ_OP  (Independent)
 *    0010 WRITE_OP (Independent)
 *    0011 /////////
 *    0100 CLOSE_OP (Independent)
 *    -----
 *    1000
 *    1001 COLLECTIVE_READ
 *    1010 COLLECTIVE_WRITE
 *    1011 /////////
 *    1100 COLLECTIVE_CLOSE
 *
 *   31    28      24      20      16      12       8       4       0|
 *   +-------+-------+-------+-------+-------+-------+-------+-------+
 *   |       |       |              ACKS             |      OP       |
 *   +-------+-------+-------+-------+-------+-------+-------+-------+
 *
 */

/* Bit 3 SET indicates collectives */
#define COLL_FUNC (0x1 << 3)

#if 0 /* JRM */ /* original version */

#define ACK_PART  (0x0acc << 8)
#define DATA_PART (0xd8da << 8)
#define READY     (0xfeed << 8)
#define COMPLETED (0xfed1 << 8)

#else /* JRM */ /* reduce size to make space for counters to disambiguate multiple concurrent requests from  \
                   same rank */

#define ACK_PART  (0x01 << 8)
#define DATA_PART (0x02 << 8)
#define READY     (0x04 << 8)
#define COMPLETED (0x08 << 8)

#endif /* JRM */ /* reduce size to make space for counters to disambiguate multiple concurrent requests from \
                    same rank */

#define READ_INDEP  (READ_OP)
#define READ_COLL   (COLL_FUNC | READ_OP)
#define WRITE_INDEP (WRITE_OP)
#define WRITE_COLL  (COLL_FUNC | WRITE_OP)

#define WRITE_INDEP_ACK  (ACK_PART | WRITE_OP)
#define WRITE_INDEP_DATA (DATA_PART | WRITE_OP)

#define READ_INDEP_DATA (DATA_PART | READ_OP)

#define GET_EOF_COMPLETED (COMPLETED | GET_EOF_OP)

#define SET_LOGGING (LOGGING_OP)

#define INT32_MASK 0x07FFFFFFFFFFFFFFF

/* The following are the basic 'op codes' used when
 * constructing a RPC message for IO Concentrators.
 * These are defined in the low 8 bits of the
 * message.
 *
 * We currently ONLY use READ_OP and WRITE_OP
 *
 * Added TRUNC_OP 12/15/21 -- JRM
 *
 * Added GET_EOF_OP 12/28/21 -- JRM
 */
typedef enum io_ops {
    READ_OP    = 1,
    WRITE_OP   = 2,
    OPEN_OP    = 3,
    CLOSE_OP   = 4,
    TRUNC_OP   = 5,
    GET_EOF_OP = 6,
    FINI_OP    = 8,
    LOGGING_OP = 16
} io_op_t;

/* Here are the basic key values to be used when accessing
 * the cache of stored topologies or contexts.
 */
typedef enum {
    SF_BADID    = (-1),
    SF_TOPOLOGY = 1,
    SF_CONTEXT  = 2,
    SF_NTYPES /* number of subfiling object types, MUST BE LAST */
} sf_obj_type_t;

/* Every application rank will record their MPI rank
 * and hostid as a structure.  These eventually get
 * communicated to MPI rank zero(0) and sorted before
 * being broadcast. The resulting sorted vector
 * provides a basis for determining which MPI ranks
 * will host an IO Concentrator (IOC), e.g. For
 * default behavior, we choose the first vector entry
 * associated with a "new" hostid.
 */
typedef struct {
    long rank;
    long hostid;
} layout_t;

/* This typedef defines a fixed process layout which
 * can be reused for any number of file open operations
 */
typedef struct app_layout_t {
    long      hostid;      /* value returned by gethostid()  */
    layout_t *layout;      /* Vector of {rank,hostid} values */
    int *     node_ranks;  /* ranks extracted from sorted layout */
    int       node_count;  /* Total nodes (different hostids) */
    int       node_index;  /* My node: index into node_ranks */
    int       local_peers; /* How may local peers on my node */
    int       world_rank;  /* My MPI rank                    */
    int       world_size;  /* Total number of MPI ranks      */
} app_layout_t;

/*  This typedef defines things related to IOC selections */
typedef struct topology {
    app_layout_t *  app_layout;         /* Pointer to our layout struct   */
    bool            rank_is_ioc;        /* Indicates that we host an IOC  */
    int             subfile_rank;       /* Valid only if rank_is_ioc      */
    int             n_io_concentrators; /* Number of IO concentrators  */
    int *           io_concentrator;    /* Vector of ranks which are IOCs */
    int *           subfile_fd;         /* file descriptor (if IOC)       */
    ioc_selection_t selection_type;     /* Cache our IOC selection criteria */
} sf_topology_t;

typedef struct {
    hid_t          sf_context_id;           /* Generated context ID which embeds the cache index */
    uint64_t       h5_file_id;              /* GUID (basically the inode value) */
    int            sf_fid;                  /* value returned by open(file,..)  */
    size_t         sf_write_count;          /* Statistics: write_count  */
    size_t         sf_read_count;           /* Statistics: read_count  */
    haddr_t        sf_eof;                  /* File eof */
    int64_t        sf_stripe_size;          /* Stripe-depth */
    int64_t        sf_blocksize_per_stripe; /* Stripe-depth X n_IOCs  */
    int64_t        sf_base_addr;            /* For an IOC, our base address   */
    MPI_Comm       sf_msg_comm;             /* MPI comm used to send RPC msg  */
    MPI_Comm       sf_data_comm;            /* MPI comm used to move data     */
    MPI_Comm       sf_group_comm;           /* Not used: for IOC collectives  */
    MPI_Comm       sf_intercomm;            /* Not used: for msgs to all IOC  */
    int            sf_group_size;           /* IOC count (in sf_group_comm)   */
    int            sf_group_rank;           /* IOC rank  (in sf_group_comm)   */
    int            sf_intercomm_root;       /* Not used: for IOC comms        */
    char *         subfile_prefix;          /* If subfiles are node-local     */
    char *         sf_filename;             /* A generated subfile name       */
    char *         h5_filename;             /* The user supplied file name    */
    sf_topology_t *topology;                /* pointer to our topology        */

} subfiling_context_t;

/* The following is a somewhat augmented input (by the IOC) which captures
 * the basic RPC from a 'source'.   The fields are filled out to allow
 * an easy gathering of statistics by the IO Concentrator.
 */
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

typedef struct {            /* Format of a context map entry  */
    uint64_t h5_file_id;    /* key value (linear search of the cache) */
    hid_t    sf_context_id; /* The return value if matching h5_file_id */
} file_map_to_context_t;

/*
 * CAUTION::
 * Do we want or need this?
 * Unfortunately, this structure is ONLY defined
 * in the H5FDsec2.c source file...
 * I'm only using it to access the file descriptor to
 * allow me to get the inode info.
 */
typedef struct H5FD_sec2_t {
    H5FD_t pub; /* public stuff, must be first      */
    int    fd;  /* the filesystem file descriptor   */
} H5FD_sec2_t;

extern int        sf_verbose_flag;
extern atomic_int sf_work_pending;
extern atomic_int sf_file_open_count;
extern atomic_int sf_file_close_count;
extern atomic_int sf_shutdown_flag;
extern atomic_int sf_io_ops_pending;
extern atomic_int sf_ioc_ready;

#if 1 /* JRM */ /* this belongs in an IOC private header file */

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

#define H5FD_IOC__IO_Q_ENTRY_MAGIC 0x1357

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

H5_DLL void H5FD_ioc_take_down_thread_pool(void);

H5_DLL H5FD_ioc_io_queue_entry_t *H5FD_ioc__alloc_io_q_entry(void);
H5_DLL void                       H5FD_ioc__complete_io_q_entry(H5FD_ioc_io_queue_entry_t *entry_ptr);
H5_DLL void                       H5FD_ioc__dispatch_elegible_io_q_entries(void);
H5_DLL void                       H5FD_ioc__free_io_q_entry(H5FD_ioc_io_queue_entry_t *q_entry_ptr);
H5_DLL void                       H5FD_ioc__queue_io_q_entry(sf_work_request_t *wk_req_ptr);

#endif /* JRM */

#ifdef __cplusplus
}
#endif

#endif /* H5FDsubfiling_priv_H */
