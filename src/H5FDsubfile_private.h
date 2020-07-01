/********************/
/* Standard Headers */
/********************/

#include <assert.h>
#include <stdatomic.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/**************/
/* H5 Headers */
/**************/
#include "H5private.h"          /* Generic Functions                        */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Ipublic.h"
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Pprivate.h"         /* Property lists                           */


#include "mpi.h"

#ifndef _H5FDsubfile_private_H
#define _H5FDsubfile_private_H

typedef int (*file_close_cb)(int,MPI_Comm);

typedef struct {
	int64_t   sf_stripe_size;
	int64_t   sf_blocksize_per_stripe;
	MPI_Comm  sf_msg_comm;
	MPI_Comm  sf_data_comm;
	MPI_Comm  sf_group_comm;
	MPI_Comm  sf_intercomm;
	int       sf_group_size;
	int       sf_group_rank;
	int       sf_intercomm_root;
	char      *subfile_prefix;
} subfiling_context_t;

typedef struct {
    /* {Datasize, Offset} */
    int64_t   header[2];
    int       tag;
    int       source;
    int       subfile_rank;
} sf_work_request_t;



typedef struct {
	long      rank;
	long      hostid;
} layout_t;

typedef struct {
	long      hostid;
	layout_t *topology;
	int      *node_ranks;
	int       node_count;
	int       node_index;
	int       local_peers;
	int       subfile_rank;
    int       world_rank;
	int       world_size;
	bool      rank_is_ioc;
} sf_topology_t;

#define K(n) ((n)*1024)
#define DEFAULT_STRIPE_SIZE K(256) /* (1024*1024) */
#define MAX_DEPTH 256

typedef enum io_ops {
    READ_OP  = 1,
    WRITE_OP = 2,
	OPEN_OP  = 3,
	CLOSE_OP = 4,
    INCR_OP  = 8,
	DECR_OP  = 16,
} io_op_t;
	    
typedef enum {
    SF_BADID = (-1),
	SF_TOPOLOGY = 1,
	SF_CONTEXT,
	SF_NTYPES					/* number of subfiling object types, MUST BE LAST */
} SF_OBJ_TYPE;
	


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
#define COLL_FUNC        (0x1 << 3)

#define ACK_PART         (0x0acc << 8)
#define DATA_PART        (0xd8da << 8)
#define READY            (0xfeed << 8)
#define COMPLETED        (0xfed1 << 8)

#define READ_INDEP       (READ_OP)
#define READ_COLL        (COLL_FUNC | READ_OP)
#define WRITE_INDEP      (WRITE_OP)
#define WRITE_COLL       (COLL_FUNC | WRITE_OP)

#define WRITE_INDEP_ACK  (ACK_PART  | WRITE_OP)
#define WRITE_INDEP_DATA (DATA_PART | WRITE_OP)

#define READ_INDEP_DATA  (DATA_PART | READ_OP)

#define INT32_MASK       0x07FFFFFFFFFFFFFFF

extern int sf_verbose_flag;
extern int sf_shutdown_flag;

extern atomic_int sf_workinprogress;
extern atomic_int sf_work_pending;
extern atomic_int sf_file_close_count;
extern atomic_int sf_file_refcount;

/*
-------------
Messages IN 
-------------
*/
extern MPI_Comm sf_msg_comm;

/* 
-------------
Messages OUT 
-------------
*/
extern MPI_Comm sf_data_comm;



H5_DLL int H5FD__determine_ioc_count(int world_size, int world_rank, sf_topology_t **thisapp);
H5_DLL int H5FD__init_subfile_context(subfiling_context_t **newContext, int n_iocs, int world_size, int world_rank, bool rank_is_ioc);
H5_DLL void * get_subfiling_object(int64_t object_id);
H5_DLL hid_t get_subfiling_context(void);
H5_DLL int initialize_ioc_threads(subfiling_context_t *sf_context);
H5_DLL int tpool_add_work(sf_work_request_t *);
H5_DLL bool tpool_is_empty(void);
H5_DLL int ioc_main(subfiling_context_t *context);
H5_DLL int queue_write_coll(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int queue_read_coll(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int subfiling_close_file(int subfile_rank, MPI_Comm comm);
H5_DLL int subfiling_open_file(const char *prefix, int subfile_rank, MPI_Comm comm);
H5_DLL int queue_file_open(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm);
H5_DLL int decrement_file_ref_counts( int subfile_rank, int source, MPI_Comm comm, file_close_cb callback_ftn);
H5_DLL int sf_open_subfiles(hid_t context_id, char *prefix, int flags);
H5_DLL int sf_close_subfiles(hid_t context_id);
H5_DLL int sf_write_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank);
H5_DLL int sf_read_independent(hid_t context_id, int64_t offset, int64_t elements, int dtype_extent, void *data);
H5_DLL int sf_write_independent(hid_t context_id, int64_t offset, int64_t elements, int dtype_extent, void *data);
H5_DLL int sf_read_data(int fd, int64_t file_offset, void *data_buffer, int64_t data_size, int subfile_rank);
H5_DLL void delete_subfiling_context(hid_t context_id);
H5_DLL void finalize_ioc_threads(void);
H5_DLL int begin_thread_exclusive(void);
H5_DLL int end_thread_exclusive(void);
H5_DLL int wait_for_thread_main(void);
H5_DLL int finalize_subfile_close(void);

#endif
