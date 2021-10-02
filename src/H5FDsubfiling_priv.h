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
    double  min;      /* minium (time)         */
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

#define ACK_PART  (0x0acc << 8)
#define DATA_PART (0xd8da << 8)
#define READY     (0xfeed << 8)
#define COMPLETED (0xfed1 << 8)

#define READ_INDEP  (READ_OP)
#define READ_COLL   (COLL_FUNC | READ_OP)
#define WRITE_INDEP (WRITE_OP)
#define WRITE_COLL  (COLL_FUNC | WRITE_OP)

#define WRITE_INDEP_ACK  (ACK_PART | WRITE_OP)
#define WRITE_INDEP_DATA (DATA_PART | WRITE_OP)

#define READ_INDEP_DATA (DATA_PART | READ_OP)
#define SET_LOGGING     (LOGGING_OP)

#define INT32_MASK 0x07FFFFFFFFFFFFFFF

/* The following are the basic 'op codes' used when
 * constructing a RPC message for IO Concentrators.
 * These are defined in the low 8 bits of the
 * message.
 *
 * We currently ONLY use READ_OP and WRITE_OP
 */
typedef enum io_ops {
    READ_OP    = 1,
    WRITE_OP   = 2,
    OPEN_OP    = 3,
    CLOSE_OP   = 4,
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
    int       node_count;  /* Total nodes (differnt hostids) */
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

#ifdef __cplusplus
}
#endif

#endif /* H5FDsubfiling_priv_H */
