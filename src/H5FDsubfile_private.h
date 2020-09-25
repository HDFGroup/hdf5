/********************/
/* Standard Headers */
/********************/

#include <assert.h>
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

#include "mpi.h"

#ifndef _H5FDsubfile_private_H
#    define _H5FDsubfile_private_H

typedef int (*file_close_cb)(int, int *, MPI_Comm);

typedef enum io_ops {
    READ_OP = 1,
    WRITE_OP = 2,
    OPEN_OP = 3,
    CLOSE_OP = 4,
    FINI_OP = 8,
    LOGGING_OP = 16
} io_op_t;

typedef enum {
    SF_BADID = (-1),
    SF_TOPOLOGY = 1,
    SF_CONTEXT = 2,
    SF_NTYPES /* number of subfiling object types, MUST BE LAST */
} sf_obj_type_t;

typedef enum {
    SELECT_IOC_ONE_PER_NODE = 0, /* Default */
    SELECT_IOC_EVERY_NTH_RANK,
    SELECT_IOC_WITH_CONFIG,
    ioc_selection_options
} sf_ioc_selection_t;

typedef struct {
    long rank;
    long hostid;
} layout_t;

typedef struct topology {
    long             hostid;
    layout_t *       layout;
    int *            node_ranks;
    int              node_count;
    int              node_index;
    int              local_peers;
    int              subfile_rank;
    int              world_rank;
    int              world_size;
    bool             rank_is_ioc;
    int              n_io_concentrators;
    int *            io_concentrator;
    sf_ioc_selection_t selection_type;
} sf_topology_t;

typedef struct {
    hid_t  sf_context_id;
    hid_t  h5_file_id;
    int    sf_fid;
    size_t sf_write_count;
    size_t sf_read_count;
    size_t sf_eof;
    /* Copy of the HDF5 File 'serial' number */
    unsigned long  fileno;
    int64_t        sf_stripe_size;
    int64_t        sf_blocksize_per_stripe;
    MPI_Comm       sf_msg_comm;
    MPI_Comm       sf_data_comm;
    MPI_Comm       sf_group_comm;
    MPI_Comm       sf_intercomm;
    int            sf_group_size;
    int            sf_group_rank;
    int            sf_intercomm_root;
    char *         subfile_prefix;
	char *         filename;
    sf_topology_t *topology;
} subfiling_context_t;

typedef struct {
    /* {Datasize, Offset, FileID} */
    int64_t header[3];
    int     tag;
    int     source;
    int     subfile_rank;
    hid_t   context_id;
} sf_work_request_t;

typedef struct {
    hid_t h5_file_id;
    hid_t sf_context_id;
} file_map_to_context_t;

#    define K(n) ((n) *1024)
#    define DEFAULT_STRIPE_SIZE K(256) /* (1024*1024) */
#    define MAX_DEPTH 1024

/* MPI Tags are 32 bits, we treat them as unsigned
 * to allow the use of the available bits for RPC
 * selections:
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
#    define COLL_FUNC (0x1 << 3)

#    define ACK_PART (0x0acc << 8)
#    define DATA_PART (0xd8da << 8)
#    define READY (0xfeed << 8)
#    define COMPLETED (0xfed1 << 8)

#    define READ_INDEP (READ_OP)
#    define READ_COLL (COLL_FUNC | READ_OP)
#    define WRITE_INDEP (WRITE_OP)
#    define WRITE_COLL (COLL_FUNC | WRITE_OP)

#    define WRITE_INDEP_ACK (ACK_PART | WRITE_OP)
#    define WRITE_INDEP_DATA (DATA_PART | WRITE_OP)

#    define READ_INDEP_DATA (DATA_PART | READ_OP)
#    define SET_LOGGING (LOGGING_OP)

#    define INT32_MASK 0x07FFFFFFFFFFFFFFF

extern int sf_shutdown_flag;

extern atomic_int sf_workinprogress;
extern atomic_int sf_work_pending;
extern atomic_int sf_file_close_count;
extern atomic_int sf_file_refcount;
extern int        sf_verbose_flag;

#    ifndef NDEBUG
extern FILE *sf_logfile;
#    endif

#    ifdef __cplusplus
extern "C" {
#    endif

/* clang-format off */
H5_DLL herr_t  H5FDsubfiling_init(sf_ioc_selection_t ioc_select_method, char *ioc_select_option, int64_t *context);
H5_DLL herr_t  H5FDsubfiling_finalize(int64_t subfile_id);
H5_DLL int     H5FD__determine_ioc_count(int world_size, int world_rank,
                   sf_ioc_selection_t ioc_select_method, char *ioc_select_option, sf_topology_t **thisapp);
H5_DLL int     H5FD__init_subfile_context(sf_topology_t *thisApp, int n_iocs, int world_rank,
                   subfiling_context_t *newContext);
H5_DLL int64_t record_subfiling_object(sf_obj_type_t type, void *obj);
H5_DLL void *  get_subfiling_object(int64_t object_id);
H5_DLL herr_t  sf_free_context(subfiling_context_t **sf_context);
H5_DLL int     initialize_ioc_threads(subfiling_context_t *sf_context);
H5_DLL int     tpool_add_work(sf_work_request_t *);
H5_DLL bool    tpool_is_empty(void);
H5_DLL int     ioc_main(int64_t context_id);
H5_DLL int     queue_write_coll( sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int     queue_read_coll( sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int     queue_write_indep( sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int     queue_read_indep( sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int     subfiling_close_file(int subfile_rank, int *subfile_fid, MPI_Comm comm);
H5_DLL int     subfiling_shutdown(int subfile_rank, int *subfile_fid, MPI_Comm comm);
H5_DLL int     subfiling_open_file( sf_work_request_t *msg, const char *prefix, int subfile_rank, int flags);
H5_DLL int     queue_file_open( sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int     decrement_file_ref_counts(sf_work_request_t *msg, int subfile_rank, int source,
                   MPI_Comm comm, file_close_cb callback_ftn);
H5_DLL int     increment_ioc_fini_counts(sf_work_request_t *msg, int subfile_rank, int source,
                   MPI_Comm comm, file_close_cb callback_ftn);
H5_DLL int     sf_open_subfiles(hid_t context_id, char *filename, char *prefix, int flags);
H5_DLL int     sf_close_subfiles(hid_t context_id);
H5_DLL int     sf_notify_shutdown(hid_t context_id);
H5_DLL int     sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
                   int subfile_rank);
H5_DLL int     sf_read_independent(hid_t sf_fid, int64_t offset, int64_t elements,
                   int dtype_extent, void *data);
H5_DLL int     sf_write_independent(hid_t sf_fid, int64_t offset, int64_t elements,
                   int dtype_extent, const void *data);
H5_DLL int     sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size,
                   int subfile_rank);
H5_DLL herr_t  sf_read_vector(hid_t h5_fid, hssize_t count, haddr_t addrs[], hsize_t sizes[],
                   void *bufs[] /* in */);
H5_DLL herr_t  sf_write_vector(hid_t h5_fid, hssize_t count, haddr_t addrs[], hsize_t sizes[],
                   void *bufs[] /* in */);
H5_DLL int     sf_truncate(hid_t h5_fid, haddr_t addr);
H5_DLL void    delete_subfiling_context(hid_t context_id);
H5_DLL void    finalize_ioc_threads(void);
H5_DLL int     begin_thread_exclusive(void);
H5_DLL int     end_thread_exclusive(void);
H5_DLL int     wait_for_thread_main(void);
H5_DLL int     finalize_subfile_close(void);
H5_DLL char *  get_ioc_selection_criteria(sf_ioc_selection_t *selection_criteria);
H5_DLL int     active_map_entries(void);
H5_DLL void    clear_fid_map_entry(hid_t sf_fid);
H5_DLL hid_t   fid_map_to_context(hid_t sf_fid);
H5_DLL void    set_verbose_flag(int subfile_rank, int new_value);
H5_DLL int     sf_get_mpi_rank(hid_t fid, int *rank);
H5_DLL int     sf_get_mpi_size(hid_t fid, int *size);
H5_DLL int     sf_get_group_comm(hid_t fid, MPI_Comm *comm);
H5_DLL int     sf_subfile_set_logging(hid_t sf_fid, int ioc_rank, int flag);

/* clang-format on */

#    ifdef __cplusplus
}
#    endif

#endif
