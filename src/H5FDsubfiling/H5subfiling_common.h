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
 * Header file for shared code between the HDF5 Subfiling VFD and IOC VFD
 */

#ifndef H5_SUBFILING_COMMON_H
#define H5_SUBFILING_COMMON_H

#include <stdatomic.h>

#include "H5private.h"
#include "H5Iprivate.h"

/* TODO: needed for ioc_selection_t, which also needs to be public */
#include "H5FDioc.h"

/* The following is our basic template for a subfile filename.
 * Note that eventually we shouldn't use 0_of_N since we
 * intend to use the user defined HDF5 filename for a
 * zeroth subfile as well as for all metadata.
 */
#define SF_FILENAME_TEMPLATE ".subfile_%ld_%0*d_of_%d"

/*
 * Environment variables interpreted by the HDF5 subfiling feature
 */
#define H5_IOC_COUNT_PER_NODE "H5_IOC_COUNT_PER_NODE"
#define H5_IOC_STRIPE_SIZE    "H5_IOC_STRIPE_SIZE"
#define H5_IOC_SUBFILE_PREFIX "H5_IOC_SUBFILE_PREFIX"

#define H5FD_DEFAULT_STRIPE_DEPTH (32 * 1024 * 1024)

/*
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

#define ACK_PART  (0x01 << 8)
#define DATA_PART (0x02 << 8)
#define READY     (0x04 << 8)
#define COMPLETED (0x08 << 8)

#define INT32_MASK 0x07FFFFFFFFFFFFFFF

#define READ_INDEP  (READ_OP)
#define READ_COLL   (COLL_FUNC | READ_OP)
#define WRITE_INDEP (WRITE_OP)
#define WRITE_COLL  (COLL_FUNC | WRITE_OP)

#define WRITE_INDEP_ACK  (ACK_PART | WRITE_OP)
#define WRITE_INDEP_DATA (DATA_PART | WRITE_OP)

#define READ_INDEP_DATA (DATA_PART | READ_OP)

#define GET_EOF_COMPLETED (COMPLETED | GET_EOF_OP)

#define SET_LOGGING (LOGGING_OP)

/*
 * Object type definitions for subfiling objects.
 * Used when generating a new subfiling object ID
 * or accessing the cache of stored subfiling
 * objects.
 */
typedef enum {
    SF_BADID    = (-1),
    SF_TOPOLOGY = 1,
    SF_CONTEXT  = 2,
    SF_NTYPES /* number of subfiling object types, MUST BE LAST */
} sf_obj_type_t;

/* The following are the basic 'op codes' used when
 * constructing a RPC message for IO Concentrators.
 * These are defined in the low 8 bits of the
 * message.
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
    int *           io_concentrators;   /* Vector of ranks which are IOCs */
    int *           subfile_fd;         /* file descriptor (if IOC)       */
    ioc_selection_t selection_type;     /* Cache our IOC selection criteria */
} sf_topology_t;

typedef struct {
    int64_t        sf_context_id;           /* Generated context ID which embeds the cache index */
    uint64_t       h5_file_id;              /* GUID (basically the inode value) */
    int            sf_fid;                  /* value returned by open(file,..)  */
    size_t         sf_write_count;          /* Statistics: write_count  */
    size_t         sf_read_count;           /* Statistics: read_count  */
    haddr_t        sf_eof;                  /* File eof */
    int64_t        sf_stripe_size;          /* Stripe-depth */
    int64_t        sf_blocksize_per_stripe; /* Stripe-depth X n_IOCs  */
    int64_t        sf_base_addr;            /* For an IOC, our base address      */
    MPI_Comm       sf_file_comm;            /* MPI comm the file was opened with */
    MPI_Comm       sf_msg_comm;             /* MPI comm used to send RPC msg     */
    MPI_Comm       sf_data_comm;            /* MPI comm used to move data        */
    MPI_Comm       sf_group_comm;           /* Not used: for IOC collectives     */
    MPI_Comm       sf_intercomm;            /* Not used: for msgs to all IOC     */
    int            sf_group_size;           /* IOC count (in sf_group_comm)      */
    int            sf_group_rank;           /* IOC rank  (in sf_group_comm)      */
    int            sf_intercomm_root;       /* Not used: for IOC comms           */
    char *         subfile_prefix;          /* If subfiles are node-local        */
    char *         sf_filename;             /* A generated subfile name          */
    char *         h5_filename;             /* The user supplied file name       */
    sf_topology_t *topology;                /* pointer to our topology           */
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
    int64_t context_id;       /* context to be used to complete */
    double  start_time;       /* the request, + time of receipt */
                              /* from which we calc Time(queued) */
    void *buffer;             /* for writes, we keep the buffer */
                              /* around for awhile...           */
    volatile int in_progress; /* Not used!               */
    volatile int serialize;   /* worker thread needs to wait while true */
    volatile int dependents;  //* If current work item has dependents */
    int          depend_id;   /* work queue index of the dependent */
} sf_work_request_t;

extern FILE *sf_logfile;
extern FILE *client_log;

extern int        sf_verbose_flag;
extern atomic_int sf_file_open_count;
extern atomic_int sf_shutdown_flag;

#ifdef __cplusplus
extern "C" {
#endif

H5_DLL herr_t H5_open_subfiles(const char *base_filename, uint64_t h5_file_id,
                               ioc_selection_t ioc_selection_type, int file_acc_flags, MPI_Comm file_comm,
                               int64_t *context_id_out);
H5_DLL herr_t H5_close_subfiles(int64_t subfiling_context_id);

H5_DLL int64_t H5_new_subfiling_object_id(sf_obj_type_t obj_type, int64_t index_val);
H5_DLL void *  H5_get_subfiling_object(int64_t object_id);
H5_DLL int64_t H5_subfile_fid_to_context(uint64_t h5_fid);
H5_DLL herr_t  H5_free_subfiling_object(int64_t object_id);

H5_DLL void H5FD_ioc_take_down_thread_pool(void);

void set_verbose_flag(int subfile_rank, int new_value);

#ifdef __cplusplus
}
#endif

#endif /* H5_SUBFILING_COMMON_H */
