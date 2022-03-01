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

#include "H5FDsubfiling.h"

static int sf_close_file_count      = 0;
static int sf_ops_after_first_close = 0;
static int sf_enable_directIO       = 0;

static int    sf_write_ops       = 0;
static double sf_pwrite_time     = 0.0;
static double sf_write_wait_time = 0.0;

static int    sf_read_ops         = 0;
static double sf_pread_time       = 0.0;
static double sf_read_wait_time   = 0.0;
static double sf_queue_delay_time = 0.0;

/* The following is our basic template for a subfile filename.
 * Note that eventually we shouldn't use 0_of_N since we
 * intend to use the user defined HDF5 filename for a
 * zeroth subfile as well as for all metadata.
 */
#define SF_FILENAME_TEMPLATE ".subfile_%ld_%0*d_of_%d"
static int *request_count_per_rank = NULL;

atomic_int sf_workinprogress    = 0;
atomic_int sf_work_pending      = 0;
atomic_int sf_file_open_count   = 0;
atomic_int sf_file_close_count  = 0;
atomic_int sf_file_refcount     = 0;
atomic_int sf_ioc_fini_refcount = 0;
atomic_int sf_ioc_ready         = 0;
atomic_int sf_shutdown_flag     = 0;
#if 1 /* JRM */
/* sf_io_ops_pending is use to track the number of I/O operations pending so that we can wait
 * until all I/O operations have been serviced before shutting down the worker thread pool.
 * The value of this variable must always be non-negative.
 */
atomic_int sf_io_ops_pending = 0;
#endif /* JRM */

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

/* pre-define */
typedef struct _client_io_func io_func_t;

struct _client_io_func {
    int (*io_function)(void *this_io); /* pointer to a completion function */
    io_args_t io_args;                 /* arguments passed to the completion function   */
    int       pending;                 /* The function is complete (0) or pending (1)?  */
};

typedef struct _io_req {
    struct _io_req *prev;            /* A simple list structure containing completion */
    struct _io_req *next;            /* functions. These should get removed as IO ops */
    io_func_t       completion_func; /* are completed */
} io_req_t;

int      n_io_pending = 0;
io_req_t pending_io_requests;

typedef struct _client_xfer_info {
    int64_t offset;
    int64_t length;
    int     ioc_targets;
    io_op_t op;
} client_xfer_info_t;

typedef struct _xfer_info {
    int64_t offset;
    int64_t length;
} xfer_info_t;

#define STAT_BLOCKSIZE 1024
typedef struct _ioc_stats {
    int          read_index;
    int          read_size;
    xfer_info_t *read_info;
    int          write_index;
    int          write_size;
    xfer_info_t *write_info;
} ioc_stats_t;

static ioc_stats_t ioc_xfer_records;

int                 client_op_index = 0;
int                 client_op_size  = 0;
client_xfer_info_t *client_ops      = NULL;

#if 1 /* JRM */ /* Find a better place for this */
H5FD_ioc_io_queue_t io_queue_g = {
    /* magic               = */ H5FD_IOC__IO_Q_MAGIC,
    /* q_head              = */ NULL,
    /* q_tail              = */ NULL,
    /* num_pending         = */ 0,
    /* num_in_progress     = */ 0,
    /* q_len               = */ 0,
    /* req_counter         = */ 0,
    /* q_mutex             = */
    PTHREAD_MUTEX_INITIALIZER
#if H5FD_IOC__COLLECT_STATS
    /* comma to allow further initializers */,
    /* max_q_len           = */ 0,
    /* max_num_pending     = */ 0,
    /* max_num_in_progress = */ 0,
    /* ind_read_requests   = */ 0,
    /* ind_write_requests  = */ 0,
    /* truncate_requests   = */ 0,
    /* requests_queued     = */ 0,
    /* requests_dispatched = */ 0,
    /* requests_completed  = */ 0
#endif /* H5FD_IOC__COLLECT_STATS */
};
#endif /* JRM */ /* Find a better place for this */

/* const char *sf_subfile_prefix = "."; */

#if 0 /* JRM */
#define MAX_WORK_PER_RANK 2
#else                       /* JRM */
#define MAX_WORK_PER_RANK 4 /* just to see if this changes anything */
#endif                      /* JRM */
#define K(n)                ((n)*1024)
#define M(n)                ((n) * (1024 * 1024))
#define DEFAULT_STRIPE_SIZE M(32)
#define MAX_DEPTH           1024

/*
=========================================
Private functions
=========================================
*/

static inline void *
cast_to_void(const void *data)
{
    union {
        const void *const_ptr_to_data;
        void *      ptr_to_data;
    } eliminate_const_warning;
    eliminate_const_warning.const_ptr_to_data = data;
    return eliminate_const_warning.ptr_to_data;
}
static char *get_ioc_subfile_path(int ioc, int ioc_count, subfiling_context_t *sf_context);
static int   async_completion(void *arg);

static int
numDigits(int n)
{
    if (n < 0)
        n = (n == INT_MIN) ? INT_MAX : -n;
    if (n < 10)
        return 1;
    if (n < 100)
        return 2;
    if (n < 1000)
        return 3;
    if (n < 10000)
        return 4;
    if (n < 100000)
        return 5;
    if (n < 1000000)
        return 6;
    if (n < 10000000)
        return 7;
    if (n < 100000000)
        return 8;
    if (n < 1000000000)
        return 9;
    return 10;
}

/* ===================================================================== */
/* MPI_Datatype Creation functions.
 * These are categorized by usage patterns, i.e. when data is sent to or
 * received from and IOC, the initial data offset provided by the user
 * may or may NOT start on a stripe boundary.  Because this, the initial
 * data segment to the selected IOC will often be less than 'stripe_size'
 * in length.  The purpose of these Datatype creation functions is to
 * enable the gathering of all data from this client to the IOC target
 * into a single MPI message.  The MPI datatype will the be utilized by
 * the sending function to pack data into a contiguous block of memory
 * which enables the IOC to write to disk in an effective manner.
 * ===================================================================== */

/*-------------------------------------------------------------------------
 * Function:    H5FD__create_first_mpi_type
 *
 * Purpose:     Return an appropriate MPI datatype to represent the initial
 *              IO operation when reading or writing data to or from an IO
 *              Concentrator (IOC).
 *
 *              If the 'first_io' is sufficient to complete the IO to the
 *              IOC, then the returned MPI datatype will simply be MPI_BYTE.
 *              For all other non-zero length IO operations, we create a
 *              derived MPI datatype using MPI_Type_indexed. The 'ioc_depth'
 *              input will define the number of blocks/disps pairs that are
 *              required to represent the desired IO operation.
 *
 * Return:      The MPI_Datatype that will be used to send or receive data.
 * Errors:      MPI_Type_NULL if for any reason, the MPI_Datatype creation
 *              fails.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

/* Fill the output vectors 'io_offset', 'io_datasize' and 'io_f_offset'
 * All calculations are in terms of bytes.
 */
static void
H5FD__create_first_mpi_type(subfiling_context_t *context, int ioc_depth, int64_t src_offset,
                            int64_t target_datasize, int64_t f_offset, int64_t *io_offset,
                            int64_t *io_datasize, int64_t *io_f_offset, int64_t first_io)
{
    int64_t stripe_size          = context->sf_stripe_size;
    int64_t blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t offset_in_stripe     = f_offset % stripe_size;
    int64_t next_offset          = blocksize_per_stripe - offset_in_stripe;
    int64_t total_bytes          = first_io;

    io_offset[0]   = src_offset;
    io_datasize[0] = first_io;
    io_f_offset[0] = f_offset;
#ifdef VERBOSE
    printf("[%s] 0: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, src_offset, first_io, f_offset);
    fflush(stdout);
#endif
    if (first_io == target_datasize) {
        return;
    }
    if (first_io) {
        int k;
        f_offset += (blocksize_per_stripe - offset_in_stripe);
        for (k = 1; k <= ioc_depth; k++) {
            io_offset[k]   = next_offset;
            io_datasize[k] = stripe_size;
            io_f_offset[k] = f_offset;
            total_bytes += stripe_size;
#ifdef VERBOSE
            printf("[%s] %d: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, k, next_offset,
                   stripe_size, f_offset);
            fflush(stdout);
#endif
            f_offset += context->sf_blocksize_per_stripe;
            next_offset += context->sf_blocksize_per_stripe;
        }
        if (total_bytes != target_datasize) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n", __func__, total_bytes,
                   target_datasize);
        }
    }
    return;
} /* end H5FD__create_first_mpi_type() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__create_final_mpi_type
 *
 * Purpose:     Return an appropriate MPI datatype to represent the final
 *              IO operation when reading or writing data to or from an IO
 *              Concentrator (IOC).
 *
 *              The data that we're sending to an IO concentrator (IOC)
 *              contains the final collection of bytes. Other than that detail,
 *              this is pretty much like the typical' IO case, i.e. all block
 *              sizes are identical (execpt for the very last block).
 *Furthermore, they all start at relative stripe offset of 0, in other words on
 *a 'stripe_size' boundary.
 *
 * Return:      The MPI_Datatype that will be used to send or receive data.
 * Errors:      MPI_Type_NULL if for any reason, the MPI_Datatype creation
 *              fails.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

/* Fill the output vectors 'io_offset', 'io_datasize' and 'io_f_offset'
 * All calculations are in terms of bytes.
 */
static void
H5FD__create_final_mpi_type(subfiling_context_t *context, int ioc_depth, int64_t src_offset,
                            int64_t target_datasize, int64_t f_offset, int64_t *io_offset,
                            int64_t *io_datasize, int64_t *io_f_offset, int64_t last_io)
{
    int64_t stripe_size          = context->sf_stripe_size;
    int64_t blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t next_offset          = src_offset;
    int64_t total_bytes          = 0;

    if (last_io == target_datasize) {
        io_offset[0]   = src_offset;
        io_f_offset[0] = f_offset;
        io_datasize[0] = last_io;
#ifdef VERBOSE
        printf("[%s] 0: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, src_offset, last_io,
               f_offset);
        fflush(stdout);
#endif
        return;
    }

    if (last_io) {
        int i, k;
        for (k = 0, i = 1; i < ioc_depth; i++) {
            io_offset[k]   = next_offset;
            io_datasize[k] = stripe_size;
            io_f_offset[k] = f_offset;
#ifdef VERBOSE
            printf("[%s] %d: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, k, next_offset,
                   stripe_size, f_offset);
            fflush(stdout);
#endif
            k++;
            total_bytes += stripe_size;
            f_offset += blocksize_per_stripe;
            next_offset += context->sf_blocksize_per_stripe;
        }

        io_datasize[k] = last_io;
        io_offset[k]   = next_offset;
        io_f_offset[k] = f_offset;
        total_bytes += last_io;

        if (total_bytes != target_datasize) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n", __func__, total_bytes,
                   target_datasize);
        }
    }
    return;
} /* end H5FD__create_final_mpi_type() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__create_f_l_mpi_type
 *
 * Purpose:     Return an appropriate MPI datatype which includes both the
 *              first and final IO data segments.
 *
 *              A special case where the current IOC has both the first and
 *              final write blocks. This function is basically a merge of
 *              the first_mpi_type and final_mpi_type functions.
 *
 * Return:      The MPI_Datatype that will be used to send or receive data.
 * Errors:      MPI_Type_NULL if for any reason, the MPI_Datatype creation
 *              fails.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

static void
H5FD__create_f_l_mpi_type(subfiling_context_t *context, int ioc_depth, int64_t src_offset,
                          int64_t target_datasize, int64_t f_offset, int64_t *io_offset, int64_t *io_datasize,
                          int64_t *io_f_offset, int64_t first_io, int64_t last_io)
{
    int64_t stripe_size          = context->sf_stripe_size;
    int64_t blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t offset_in_stripe     = f_offset % stripe_size;
    int64_t next_offset          = blocksize_per_stripe - offset_in_stripe;
    int64_t total_bytes          = first_io;

    io_offset[0]   = src_offset;
    io_datasize[0] = first_io;
    io_f_offset[0] = f_offset;

#ifdef VERBOSE
    printf("[%s] 0: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, src_offset, first_io, f_offset);
    fflush(stdout);
#endif
    if (total_bytes == target_datasize) {
        return;
    }

    if (total_bytes) {
        int k;
        f_offset += (blocksize_per_stripe - offset_in_stripe);
        for (k = 1; k < ioc_depth; k++) {
            io_offset[k]   = next_offset;
            io_datasize[k] = stripe_size;
            io_f_offset[k] = f_offset;
            total_bytes += stripe_size;
#ifdef VERBOSE
            printf("[%s] %d: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, k, next_offset,
                   stripe_size, f_offset);
            fflush(stdout);
#endif
            f_offset += blocksize_per_stripe;
            next_offset += blocksize_per_stripe;
        }
        io_datasize[ioc_depth] = last_io;
        io_f_offset[ioc_depth] = f_offset;
        io_offset[ioc_depth]   = next_offset;
#ifdef VERBOSE
        printf("[%s] %d: mem_offset=%ld, datasize=%ld, f_offset=%ld\n", __func__, k, next_offset, last_io,
               f_offset);
        fflush(stdout);
#endif
        total_bytes += last_io;

        if (total_bytes != target_datasize) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n", __func__, total_bytes,
                   target_datasize);
        }
    }
    return;
} /* end H5FD__create_f_l_mpi_type() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__create_mpi_uniform_type
 *
 * Purpose:     Return an appropriate MPI datatype to represent the typical
 *              IO operation when reading or writing data to or from an IO
 *              Concentrator (IOC).
 *
 *              Each data segment is of 'stripe_size' length and will be
 *              separated from a previous or following segment by
 *              'sf_blocksize_per_stripe' bytes of data.
 *
 * Return:      The MPI_Datatype that will be used to send or receive data.
 * Errors:      MPI_Type_NULL if for any reason, the MPI_Datatype creation
 *              fails.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static void
H5FD__create_mpi_uniform_type(subfiling_context_t *context, int ioc_depth, int64_t src_offset,
                              int64_t target_datasize, int64_t f_offset, int64_t *io_offset,
                              int64_t *io_datasize, int64_t *io_f_offset)
{
    int64_t stripe_size          = context->sf_stripe_size;
    int64_t blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t next_offset          = src_offset + blocksize_per_stripe;
    int64_t total_bytes          = 0;

    io_offset[0]   = src_offset;
    io_datasize[0] = stripe_size;
    io_f_offset[0] = f_offset;
    if (target_datasize == 0) {
#if 0
    printf("[%s] 0: datasize=0\n", __func__);
    fflush(stdout);
#endif
        io_datasize[0] = 0;
        return;
    }

#if 0
  printf("[%s] 0: mem_offset=%ld, datasize=%ld, f_offset=%ld\n",
         __func__, src_offset, stripe_size, f_offset);
  fflush(stdout);
#endif

    f_offset += blocksize_per_stripe;
    total_bytes = stripe_size;

    if (target_datasize > stripe_size) {
        int k;
        for (k = 1; k < ioc_depth; k++) {
            io_offset[k]   = next_offset;
            io_datasize[k] = stripe_size;
            io_f_offset[k] = f_offset;
#if 0
      printf("[%s] %d: mem_offset=%ld, datasize=%ld, f_offset=%ld\n",
             __func__, k, next_offset, stripe_size, f_offset);
      fflush(stdout);
#endif
            total_bytes += stripe_size;
            f_offset += blocksize_per_stripe;
            next_offset += blocksize_per_stripe;
        }

        if (total_bytes != target_datasize) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n", __func__, total_bytes,
                   target_datasize);
        }
    }
    return;
} /* end H5FD__create_mpi_uniform_type() */

/*-------------------------------------------------------------------------
 * Function:    init__indep_io
 *
 * Purpose:     Utility function to initialize the set of IO transactions
 *              used to communicate with IO concentrators for read and write
 *              IO operations.
 *
 * Return:      A filled set of vectors.  As a consequence of not allowing
 *              use of MPI derived datatypes in the VFD layer, we need to
 *              accommodate the possibility that large IO transactions will
 *              be required to use multiple IOs per IOC.
 *
 *              Example: Using 4 IOCs, each with 1M stripe-depth; when
 *              presented an IO request for 8MB then at a minimum each IOC
 *              will require 2 IOs of 1MB each.  Depending on the starting
 *              file offset, the 2 IOs can instead be 3...
 *
 *              To fully describe the IO transactions for read and writes we
 *              we thus use a return type where each IOC vector element is
 *              instead a vector itself and has a vector length of which
 *              corresponds to the max number of IO transactions per IOC.
 *              In the example above, these vector lengths can be 2 or 3.
 *              The actual length is determined by the 'container_depth'
 *              variable.
 *
 *              For IO operations which involve a subset of IO concentrators,
 *              the vector entries for the unused IOCs will have lengths of
 *              zero and MPI NULL datatypes.  The 'container_depth' in this
 *              case will always be 1.
 *
 * Return value: The vector "depth" or max number of IOs per IOC.
 *
 * Errors:      Cannot fail.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

int
init__indep_io(void *_sf_context, size_t maxdepth, int H5_ATTR_PARALLEL_UNUSED ioc_total,
               int64_t *sf_source_data_offset, int64_t *sf_datasize, int64_t *sf_offset, int *first_index,
               int *n_containers, int64_t offset, int64_t elements, int dtype_extent)
{
    subfiling_context_t *sf_context      = _sf_context;
    int                  container_count = sf_context->topology->n_io_concentrators;
    int64_t              stripe_size     = sf_context->sf_stripe_size;
    int64_t              data_size       = elements * dtype_extent;

    int64_t start_id         = offset / stripe_size;
    int64_t offset_in_stripe = offset % sf_context->sf_blocksize_per_stripe;
    int64_t container_offset = offset % stripe_size;
    int64_t start_length     = MIN(data_size, (stripe_size - container_offset));
    int64_t start_row        = start_id / container_count;
    int64_t ioc_start        = start_id % container_count;
    int64_t final_offset     = offset + data_size;
    int64_t final_id         = final_offset / stripe_size;
    int64_t final_length     = (start_length == data_size ? 0 : final_offset % stripe_size);
    int64_t ioc_final        = final_id % container_count;
    int64_t container_bytes = 0, total_bytes = 0;
    int64_t source_offset = 0;

    int     row_id_start = (int)(start_id - ioc_start);
    int     row_id_final = (int)(final_id - ioc_final);
    int     i, k, depth = ((row_id_final - row_id_start) / container_count) + 1;
    int     container_id = (int)start_id;
    int64_t row_offset   = (int64_t)(start_row * stripe_size);

    *first_index = (int)ioc_start;

    /* Given the IO parameters, we loop thru the set of IOCs
     * to determine the various vector components for each.
     * Those IOCs whose datasize is zero (0), will not have
     * IO requests passed to them.
     */

    for (i = 0, k = (int)ioc_start; i < container_count; i++) {
        /* We use 'output_offset' as an index into a linear
         * version of a 2D array. In 'C' the last subscript
         * is the one that varies most rapidly.
         * In our case, the 2D array is represented as
         * array[ container_count ][ maxdepth ]
         */
        size_t depthsize       = maxdepth * sizeof(int64_t); /* ONLY used for memset */
        size_t output_offset   = (size_t)(k)*maxdepth;
        int    container_depth = depth;

        hbool_t  is_first = false, is_last = false;
        int64_t *__sf_source_data_offset = sf_source_data_offset + output_offset;
        int64_t *__sf_datasize           = sf_datasize + output_offset;
        int64_t *__sf_offset             = sf_offset + output_offset;

        memset(__sf_source_data_offset, 0, depthsize);
        memset(__sf_datasize, 0, depthsize);
        memset(__sf_offset, 0, depthsize);

        container_bytes = 0;

        if (total_bytes == data_size) {
            *n_containers = i;
            return depth + 1;
        }
        if (total_bytes < data_size) {
            if (k == ioc_start) {
                is_first        = true;
                container_bytes = start_length;
                container_depth--; /* Account for the start_length */
                if (ioc_final < ioc_start) {
                    container_depth--;
                    depth--;
                }
            }
            if (k == ioc_final) {
                is_last = true;
                container_bytes += final_length;
                if (container_depth)
                    container_depth--; /* Account for the final_length */
                if (depth)
                    depth--;
            }
            container_bytes += container_depth * stripe_size;
            total_bytes += container_bytes;
        }

        __sf_source_data_offset[0] = source_offset;
        __sf_datasize[0]           = container_bytes;
        __sf_offset[0]             = row_offset + offset_in_stripe;

        if (container_count == 1) {
        }
        else {
            /* Fill the IO datatypes */
            if (is_first) {
                if (is_last) { /* First + Last */
                    H5FD__create_f_l_mpi_type(sf_context, container_depth + 1, source_offset, container_bytes,
                                              row_offset + offset_in_stripe, __sf_source_data_offset,
                                              __sf_datasize, __sf_offset, start_length, final_length);
                }
                else { /* First ONLY */
                    H5FD__create_first_mpi_type(sf_context, container_depth, source_offset, container_bytes,
                                                row_offset + offset_in_stripe, __sf_source_data_offset,
                                                __sf_datasize, __sf_offset, start_length);
                }
                /* Move the memory pointer to the starting location
                 * for next IOC request.
                 */
                source_offset += start_length;
            }
            else if (is_last) { /* Last ONLY */
                H5FD__create_final_mpi_type(sf_context, container_depth, source_offset, container_bytes,
                                            row_offset + offset_in_stripe, __sf_source_data_offset,
                                            __sf_datasize, __sf_offset, final_length);
                /* Probably not needed... */
                source_offset += stripe_size;
            }
            else { /* Everything else (uniform) */
                H5FD__create_mpi_uniform_type(sf_context, container_depth, source_offset, container_bytes,
                                              row_offset + offset_in_stripe, __sf_source_data_offset,
                                              __sf_datasize, __sf_offset);
                source_offset += stripe_size;
            }
        }

        k++;
        offset_in_stripe += __sf_datasize[0];
        container_id++;

        if (k == container_count) {
            k                = 0;
            offset_in_stripe = 0;
            depth            = ((row_id_final - container_id) / container_count) + 1;
            row_offset += sf_context->sf_blocksize_per_stripe;
        }
    }
    if (total_bytes != data_size) {
        printf("Error: total_bytes != data_size\n");
    }

    *n_containers = container_count;
    return depth + 1;
} /* end init__indep_io() */

/*-------------------------------------------------------------------------
 * Function:    Internal read__independent_async
 *
 * Purpose:     The IO operations can be striped across a selection of
 *              IO concentrators.  The read and write independent calls
 *              compute the group of 1 or more IOCs and further create
 *              derived MPI datatypes when required by the size of the
 *              contiguous read or write requests.
 *
 *              IOC(0) contains the logical data storage for file offset
 *              zero and all offsets that reside within modulo range of
 *              the subfiling stripe_size.
 *
 *              We cycle through all 'n_io_conentrators' and send a
 *              descriptor to each IOC that has a non-zero sized IO
 *              request to fulfill.
 *
 *              Sending descriptors to an IOC usually gets an ACK or
 *              NACK in response.  For the read operations, we post
 *              asynch READs to receive the file data and wait until
 *              all pending operations have completed.
 *
 * Return:      Success (0) or Faiure (non-zero)
 * Errors:      If MPI operations fail for some reason.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
#define WORLD_SIZE(ctx) ((ctx)->topology->app_layout->world_size)
#define WORLD_RANK(ctx) ((ctx)->topology->app_layout->world_size)

static int
read__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                        int H5_ATTR_PARALLEL_UNUSED dtype_extent, void *data, io_req_t **io_req)
{
    int       status = 0;
    int64_t   stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *     io_concentrator = NULL;
    io_req_t *sf_io_request   = NULL;
    int64_t   msg[3]          = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);

    ioc_start = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], READ_INDEP, sf_context->sf_msg_comm);

    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send request header (%ld) "
               "bytes to %d returned an error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }

    /* At this point in the new implementation, we should queue
     * the async recv so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = data;
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

    status = MPI_Irecv(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], READ_INDEP_DATA,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Irecv must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }

    return status;
} /* end read__independent_async() */

/*-------------------------------------------------------------------------
 * Function:    get_ioc_subfile_path
 *
 * Purpose:     We provide a utility function to generate a subfiling
 *              filename from a template.  While the user provides a
 *              name which will serve as the HDF5 file name, sub-filing
 *              files are related to the user filename via the filesystem
 *              inode identifier.  The inode id can be utilized as a
 *              global unique identifier (GUID) which provides a
 *              grouping ID to easily distinguish subfiles.
 *
 *              The inode_id is contained in the 'sf_context' structure.
 *
 * Return:      A full filepath which should be copied, e.g. using strdup
 *-------------------------------------------------------------------------
 */
static char *
get_ioc_subfile_path(int ioc, int ioc_count, subfiling_context_t *sf_context)
{
    static char filepath[PATH_MAX];
    char *      subfile_dir = NULL;
    char *      prefix      = sf_context->subfile_prefix;

    int numD = numDigits(ioc_count);
    if (prefix != NULL) {
        sprintf(filepath, "%s/" SF_FILENAME_TEMPLATE, prefix, sf_context->h5_file_id, numD, ioc, ioc_count);
    }
    else {
        strcpy(filepath, sf_context->h5_filename);
        subfile_dir = strrchr(filepath, '/');
        assert(subfile_dir);
        sprintf(subfile_dir + 1, SF_FILENAME_TEMPLATE, sf_context->h5_file_id, numD, ioc, ioc_count);
    }
    return filepath;
} /* end get_ioc_subfile_path() */

/*-------------------------------------------------------------------------
 * Utility functions in support of a first pass attempt at handling async
 * IO.  The working assumption is that reads and writes to a collection
 * of IO Concentrators (IOCs) will proceed by stages.  In the first stage,
 * each MPI rank will get their individual IOs started by preping the IOC
 * with a message which indicates (via the MPI tag) what operation is
 * starting, along with the file offset, data size, and a context_id.
 * The latter will be used to access the actual open file descriptor.
 *
 *-------------------------------------------------------------------------
 * Function:    progress_this_pending_io
 *
 * Purpose:     In this initial example, we can progress an individual
 *              IO request which is described by the io_req_t input arg.
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
progress_this_pending_io(io_req_t *this_req)
{
    assert(this_req);
    assert(this_req->completion_func.io_function);
    return (*this_req->completion_func.io_function)(&this_req->completion_func);
}

/*-------------------------------------------------------------------------
 * Function:    write_data
 *
 * Purpose:     Given a io_func_t structure containing the function pointer
 *              and it's input arguments, we write the supplied data out
 *              asynchronous using MPI_Isend, to the appropriate IOC.
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
write_data(io_func_t *this_func)
{
    int                  ioc, status;
    int64_t              elements;
    void *               data;
    int *                io_concentrator = NULL;
    subfiling_context_t *sf_context      = NULL;
    assert(this_func);

    sf_context = get__subfiling_object(this_func->io_args.context_id);

    assert(sf_context);

    io_concentrator = sf_context->topology->io_concentrator;
    ioc             = this_func->io_args.ioc;

    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc], WRITE_INDEP_DATA,
                       sf_context->sf_data_comm, &this_func->io_args.io_req);
    return status;
}

/*-------------------------------------------------------------------------
 * Function:    async_completion
 *
 * Purpose:     Given a single io_func_t structure containing the function
 *              pointer and it's input arguments and a single MPI_Request
 *              argument which needs to be completed, we make progress
 *              by calling MPI_Test.  In this initial example, we loop
 *              until the request is completed as indicated by a non-zero
 *              flag variable.
 *
 *              As we go further with the implementation, we anticipate that
 *              rather than testing a single request variable, we will
 *              deal with a collection of all pending IO requests (on
 *              this rank).
 *
 * Return:      an integer status.  Zero(0) indicates success. Negative
 *              values (-1) indicates an error.
 *-------------------------------------------------------------------------
 */
static int
async_completion(void *arg)
{
    struct async_arg {
        int          n_reqs;
        MPI_Request *sf_reqs;
    } *in_progress = (struct async_arg *)arg;

    assert(arg);
    int        status, errors = 0;
    int        count     = in_progress->n_reqs;
    int        n_waiting = count;
    int        indices[count];
    MPI_Status stats[count];
    useconds_t delay = 5;

    while (n_waiting) {
        int i, ready = 0;
        status = MPI_Testsome(count, in_progress->sf_reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%s] MPI_ERROR! MPI_Testsome returned an error(%s)\n", __func__, estring);
            fflush(stdout);
            errors++;
            return -1;
        }

        if (ready == 0) {
            usleep(delay);
        }

        for (i = 0; i < ready; i++) {
            n_waiting--;
        }
    }
    return errors;
}

/*-------------------------------------------------------------------------
 * Function:    Internal write__independent_async.
 *
 * Purpose:     The IO operations can be striped across a selection of
 *              IO concentrators.  The read and write independent calls
 *              compute the group of 1 or more IOCs and further create
 *              derived MPI datatypes when required by the size of the
 *              contiguous read or write requests.
 *
 *              IOC(0) contains the logical data storage for file offset
 *              zero and all offsets that reside within modulo range of
 *              the subfiling stripe_size.
 *
 *              We cycle through all 'n_io_conentrators' and send a
 *              descriptor to each IOC that has a non-zero sized IO
 *              request to fulfill.
 *
 *              Sending descriptors to an IOC usually gets an ACK or
 *              NACK in response.  For the write operations, we post
 *              asynch READs to receive ACKs from IOC ranks that have
 *              allocated memory receive the data to write to the
 *              subfile.  Upon receiving an ACK, we send the actual
 *              user data to the IOC.
 *
 * Return:      Success (0) or Faiure (non-zero)
 * Errors:      If MPI operations fail for some reason.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
static int
write__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                         int H5_ATTR_PARALLEL_UNUSED dtype_extent, const void *data, io_req_t **io_req)
{

    int         ack = 0, active_sends = 0, n_waiting = 0, status = 0;
    int64_t     stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *       io_concentrator = NULL;
    io_req_t *  sf_io_request   = NULL;
    MPI_Request ackrequest;
    int64_t     msg[3] = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);
    ioc_start  = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset.
     * (see above: Calculate the IOC))
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], WRITE_INDEP, sf_context->sf_msg_comm);
    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }
    else
        active_sends++;
    /*
     * We wait for memory to be allocated on the target IOC so that we can
     * start sending user data. Once memory is allocated, we will receive
     * an ACK (or NACK) message from the IOC to allow us to proceed.
     */
    status = MPI_Irecv(&ack, 1, MPI_INT, io_concentrator[ioc_start], WRITE_INDEP_ACK,
                       sf_context->sf_data_comm, &ackrequest);

    if (status != MPI_SUCCESS) {
        printf("[%d %s] MPI_Irecv failed\n", WORLD_RANK(sf_context), __func__);
        fflush(stdout);
        return -1;
    }

    n_waiting = active_sends;

    while (n_waiting) {
        int flag = 0;
        status   = MPI_Test(&ackrequest, &flag, MPI_STATUS_IGNORE);
        if (status == MPI_SUCCESS) {
            if (flag == 0)
                usleep(0);
            else {
                n_waiting--;
                if (ack == 0) { /* NACK */
                    printf("%s - Received NACK!\n", __func__);
                }
            }
        }
    }

    /* At this point in the new implementation, we should queue
     * the async write so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = cast_to_void(data);
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

#if 1 /* JRM */ /* experiment with MPI_Issend() */
    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
#if 1 /* JRM */ /* experiment with MPI_Send */
    status = MPI_Issend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
    status = MPI_Send(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm);
#endif          /* JRM */
#endif          /* JRM */

    /* When we actually have the async IO support,
     * the request should be queued before we
     * return to the caller.
     * Having queued the IO operation, we might want to
     * get additional work started before allowing the
     * queued IO requests to make further progress and/or
     * to complete, so we just return to the caller.
     */

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Isend must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }
    return status;
} /* end write__independent_async() */

#else /* JRM */ /* modified to use IOC supplied tag for data send */

static int
write__independent_async(int n_io_concentrators, hid_t context_id, int64_t offset, int64_t elements,
                         int H5_ATTR_PARALLEL_UNUSED dtype_extent, const void *data, io_req_t **io_req)
{

    int         ack = 0, active_sends = 0, n_waiting = 0, status = 0;
    int64_t     stripe_size, ioc_row, start_id, ioc_start, ioc_offset;
    int *       io_concentrator = NULL;
    io_req_t *  sf_io_request   = NULL;
    MPI_Request ackrequest;
    int64_t     msg[3] = {
        0,
    };

    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);

    /* Calculate the IOC that we'll send the IO request to */
    stripe_size = sf_context->sf_stripe_size;

    start_id   = offset / stripe_size;
    ioc_row    = start_id / n_io_concentrators;
    ioc_offset = (offset % stripe_size) + (ioc_row * stripe_size);
    ioc_start  = start_id % n_io_concentrators;

    io_concentrator = sf_context->topology->io_concentrator;
    assert(io_concentrator != NULL);

    /* Make sure that we can return a request structure
     * if everything is working correctly
     */
    assert(io_req);

    /* Prepare an IO request.
     * This gets sent to the ioc identified by the file offset.
     * (see above: Calculate the IOC))
     */
    msg[0] = elements;
    msg[1] = ioc_offset;
    msg[2] = context_id;
#ifdef VERBOSE
    printf("[%s ioc(%ld)] elements=%ld, offset=%ld, file_offset=%ld\n", __func__, ioc_start, elements, offset,
           ioc_offset);
    fflush(stdout);
#endif
    status = MPI_Send(msg, 3, MPI_INT64_T, io_concentrator[ioc_start], WRITE_INDEP, sf_context->sf_msg_comm);
    if (status != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(status, estring, &len);
        printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               WORLD_RANK(sf_context), sizeof(msg), io_concentrator[ioc_start], estring);
        fflush(stdout);
        return -1;
    }
    else
        active_sends++;
    /*
     * We wait for memory to be allocated on the target IOC so that we can
     * start sending user data. Once memory is allocated, we will receive
     * an ACK (or NACK) message from the IOC to allow us to proceed.
     */
    /* On ACK, IOC will send tag to be used for data send -- need this to
     * distinguish between multiple concurrent writes from a single rank.
     */
    status = MPI_Irecv(&ack, 1, MPI_INT, io_concentrator[ioc_start], WRITE_INDEP_ACK,
                       sf_context->sf_data_comm, &ackrequest);

    if (status != MPI_SUCCESS) {
        printf("[%d %s] MPI_Irecv failed\n", WORLD_RANK(sf_context), __func__);
        fflush(stdout);
        return -1;
    }

    n_waiting = active_sends;

    while (n_waiting) {
        int flag = 0;
        status   = MPI_Test(&ackrequest, &flag, MPI_STATUS_IGNORE);
        if (status == MPI_SUCCESS) {
            if (flag == 0)
                usleep(0);
            else {
                n_waiting--;
                if (ack == 0) { /* NACK */
                    printf("%s - Received NACK!\n", __func__);
                }
            }
        }
    }

    /* At this point in the new implementation, we should queue
     * the async write so that when the top level VFD tells us
     * to complete all pending IO requests, we have all the info
     * we need to accomplish that.
     */
    sf_io_request = (io_req_t *)malloc(sizeof(io_req_t));
    assert(sf_io_request);

    sf_io_request->completion_func.io_args.ioc        = (int)ioc_start;
    sf_io_request->completion_func.io_args.context_id = context_id;
    sf_io_request->completion_func.io_args.offset     = offset;
    sf_io_request->completion_func.io_args.elements   = elements;
    sf_io_request->completion_func.io_args.data       = cast_to_void(data);
    sf_io_request->completion_func.io_args.io_req     = MPI_REQUEST_NULL;
    sf_io_request->completion_func.io_function        = async_completion;
    sf_io_request->completion_func.pending            = 0;

    sf_io_request->prev = sf_io_request->next = NULL;
    /* Start the actual data transfer */

#if 1 /* JRM */ /* experiment with MPI_Issend() */
    /* use ack from IOC as the tag for the send */
    status = MPI_Isend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], ack,
                       sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
#if 1 /* JRM */ /* experiment with MPI_Send */
    status = MPI_Issend(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm, &sf_io_request->completion_func.io_args.io_req);
#else           /* JRM */
    status = MPI_Send(data, (int)elements, MPI_BYTE, io_concentrator[ioc_start], WRITE_INDEP_DATA,
                      sf_context->sf_data_comm);
#endif          /* JRM */
#endif          /* JRM */

    /* When we actually have the async IO support,
     * the request should be queued before we
     * return to the caller.
     * Having queued the IO operation, we might want to
     * get additional work started before allowing the
     * queued IO requests to make further progress and/or
     * to complete, so we just return to the caller.
     */

    if (status == MPI_SUCCESS) {
        sf_io_request->completion_func.pending = 1;
        *io_req                                = sf_io_request;
    }
    else {
        puts("MPI_Isend must have failed!");
        free(sf_io_request);
        *io_req = NULL;
    }
    return status;
} /* end write__independent_async() */

#endif /* JRM */ /* modified to use IOC supplied tag for data send */

/*
 * Function:   H5FD__write_vector_internal
 *
 * Purpose:    This function takes 'count' vector entries
 *             and initiates an asynch write operation for each.
 *             By asynchronous, we mean that MPI_Isends are utilized
 *             to communicate the write operations to the 'count'
 *             IO Concentrators.  The calling function will have
 *             decomposed the actual user IO request into the
 *             component segments, each IO having a maximum size
 *             of "stripe_depth", which is recorded in the
 *             subfiling_context_t 'sf_context' structure.
 *
 * Return:     SUCCEED if no errors, FAIL otherwise.
 */
herr_t
H5FD__write_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                            const void *bufs[] /* in */)
{
    herr_t               ret_value = SUCCEED;
    hssize_t             status = 0, k = 0;
    hid_t                sf_context_id = fid_map_to_context((uint64_t)h5_fid);
    subfiling_context_t *sf_context    = NULL;
    io_req_t **          sf_async_reqs = NULL;
    MPI_Request *        active_reqs   = NULL;
    struct __mpi_req {
        int          n_reqs;
        MPI_Request *active_reqs;
    } *mpi_reqs = NULL;

    sf_context = get__subfiling_object(sf_context_id);
    assert(sf_context != NULL);

    active_reqs = (MPI_Request *)calloc((size_t)(count + 2), sizeof(struct __mpi_req));
    assert(active_reqs);

    sf_async_reqs = (io_req_t **)calloc((size_t)count, sizeof(void *));
    assert(sf_async_reqs);

    /*
     * Note: We allocated extra space in the active_requests (above).
     * The extra should be enough for an integer plus a pointer.
     */
    mpi_reqs              = (struct __mpi_req *)&active_reqs[count];
    mpi_reqs->n_reqs      = (int)count;
    mpi_reqs->active_reqs = active_reqs;

    /* Each pass thru the following should queue an MPI write
     * to a new IOC. Both the IOC selection and offset within the
     * particular subfile are based on the combinatation of striping
     * factors and the virtual file offset (addrs[k]).
     */
    for (k = 0; k < count; k++) {
        if (sizes[k] == 0) {
            puts("Something wrong with the size argument: size is 0!");
            fflush(stdout);
        }
        status =
            write__independent_async(sf_context->topology->n_io_concentrators, sf_context_id,
                                     (int64_t)addrs[k], (int64_t)sizes[k], 1, bufs[k], &sf_async_reqs[k]);
        if (status < 0) {
            printf("%s - encountered an internal error!\n", __func__);
            goto errors;
        }
        else {
            mpi_reqs->active_reqs[k] = sf_async_reqs[k]->completion_func.io_args.io_req;
        }
    }

    /* Here, we should have queued 'count' async requests.
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
#if 1 /* JRM */ /* experiment with synchronous send */
    if (sf_async_reqs[0]->completion_func.io_function)
        ret_value = (*sf_async_reqs[0]->completion_func.io_function)(mpi_reqs);
#endif /* JRM */

    if (active_reqs)
        free(active_reqs);

    if (sf_async_reqs) {
        for (k = 0; k < count; k++) {
            if (sf_async_reqs[k]) {
                free(sf_async_reqs[k]);
            }
        }
        free(sf_async_reqs);
    }
    return ret_value;

errors:
    return FAIL;
}

/*
 * Refactored version of the original sf_read_vector() function.
 * The H5FD__ioc_read_vector VFD call included additional 'hid_t dxpl'
 * and 'H5FD_mem_t types[]'. These are now removed.
 */
herr_t
H5FD__read_vector_internal(hid_t h5_fid, hssize_t count, haddr_t addrs[], size_t sizes[],
                           void *bufs[] /* out */)
{
    herr_t               ret_value = SUCCEED;
    hssize_t             status = 0, k = 0;
    hid_t                sf_context_id = fid_map_to_context((uint64_t)h5_fid);
    subfiling_context_t *sf_context    = NULL;
    io_req_t **          sf_async_reqs = NULL;
    MPI_Request *        active_reqs   = NULL;
    struct __mpi_req {
        int          n_reqs;
        MPI_Request *active_reqs;
    } *mpi_reqs = NULL;

    sf_context = get__subfiling_object(sf_context_id);
    assert(sf_context != NULL);

    active_reqs = (MPI_Request *)calloc((size_t)(count + 2), sizeof(struct __mpi_req));
    assert(active_reqs);

    sf_async_reqs = (io_req_t **)calloc((size_t)count, sizeof(void *));
    assert(sf_async_reqs);

    /*
     * Note: We allocated extra space in the active_requests (above).
     * The extra should be enough for an integer plus a pointer.
     */
    mpi_reqs              = (struct __mpi_req *)&active_reqs[count];
    mpi_reqs->n_reqs      = (int)count;
    mpi_reqs->active_reqs = active_reqs;

    for (k = 0; k < count; k++) {
        status = read__independent_async(sf_context->topology->n_io_concentrators, sf_context_id,
                                         (int64_t)addrs[k], (int64_t)sizes[k], 1, bufs[k], &sf_async_reqs[k]);
        if (status < 0) {
            printf("%s - encountered an internal error!\n", __func__);
            goto errors;
        }
        else {
            mpi_reqs->active_reqs[k] = sf_async_reqs[k]->completion_func.io_args.io_req;
        }
    }
    /* Here, we should have queued 'count' async requests
     * (one to each required IOC).
     *
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
    if (sf_async_reqs[0]->completion_func.io_function)
        ret_value = (*sf_async_reqs[0]->completion_func.io_function)(mpi_reqs);

    if (active_reqs)
        free(active_reqs);

    if (sf_async_reqs) {
        for (k = 0; k < count; k++) {
            if (sf_async_reqs[k]) {
                free(sf_async_reqs[k]);
            }
        }
        free(sf_async_reqs);
    }
    return ret_value;

errors:
    return FAIL;
}

#if 0 /* JRM */  /* delete this -- superseded version of sf_truncate */
int
sf_truncate(hid_t h5_fid, haddr_t H5_ATTR_PARALLEL_UNUSED addr)
{
    hid_t                sf_context_id = fid_map_to_context((uint64_t)h5_fid);
    subfiling_context_t *sf_context    = get__subfiling_object(sf_context_id);

    assert(sf_context != NULL);
    return 0;
}
#endif /* JRM */ /* delete this */

#if 1 /* JRM */ /* delete this if all goes well */
int
sf_shutdown_local_ioc(hid_t fid)
{
    hid_t                context_id = fid_map_to_context((uint64_t)fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);
    if (sf_context->topology->rank_is_ioc) {
        atomic_fetch_add(&sf_shutdown_flag, 1);
    }
    return 0;
}
#else /* JRM */

/*-------------------------------------------------------------------------
 * Function:    sf_shutdown_local_ioc()
 *
 * Purpose:     Set the sf_shutdown_flag, and wait until the local
 *              I/O Concentrator shuts down.
 *
 * Return:      Void
 *
 * Errors:      None
 *
 * Programmer:  JRM -- 10/26/21
 *
 * Changes:     None.
 *
 *-------------------------------------------------------------------------
 */
void
sf_shutdown_local_ioc(hid_t fid)
{
    hid_t                context_id = fid_map_to_context((uint64_t)fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);
    if (sf_context->topology->rank_is_ioc) {
        atomic_fetch_add(&sf_shutdown_flag, 1);
    }
    return;

} /* sf_shutdown_local_ioc() */

#endif /* JRM */

#if 0 /* JRM */ /* original version of ioc_main() */
/*-------------------------------------------------------------------------
 * Function:    Public/IOC ioc_main
 *
 * Purpose:     This is the principal function run by the IO Concentrator
 *              main thread.  It remains within a loop until allowed to
 *              exit by means of setting the 'sf_shutdown_flag'.   This
 *              usually accomplished as part of the file close operation.
 *
 *              The function implements an asynchronous polling approach
 *              for incoming messages. These messages can be thought of
 *              as a primitive RPC which utilizes MPI TAGs to code and
 *              implement the desired subfiling functionality.
 *
 *              As each incoming message is received, it get added to
 *              a queue for processing by a thread_pool thread.
 *              The message handlers are dispatched via the
 *              "handle_work_request" ftn (see H5FDsubfile_thread.c)

 *              Subfiling is effectively a software RAID-0 implementation
 *              where having multiple IO Concentrators and independent
 *              subfiles is equated to the multiple disks and a true
 *              hardware base RAID implementation.
 *
 *              IO Concentrators are ordered according to their MPI rank.
 *              In the simplest interpretation, IOC(0) will always contain
 *              the initial bytes of the logical disk image.  Byte 0 of
 *              IOC(1) will contain the byte written to the logical disk
 *              offset "stripe_size" X IOC(number).
 *
 *              Example: If the stripe size is defined to be 256K, then
 *              byte 0 of subfile(1) is at logical offset 262144 of the
 *              file.   Similarly, byte 0 of subfile(2) represents the
 *              logical file offset = 524288.   For logical files larger
 *              than 'N' X stripe_size, we simply "wrap around" back to
 *              subfile(0).  The following shows the mapping of 30
 *              logical blocks of data over 3 subfiles:
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(0 )| blk(1) | blk(2 )| blk(3 )| blk(4 )| blk(5 )|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(6 )| blk(7) | blk(8 )| blk(9 )| blk(10)| blk(11)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(12)| blk(13)| blk(14)| blk(15)| blk(16)| blk(17)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(18)| blk(19)| blk(20)| blk(21)| blk(22)| blk(23)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *              | blk(24)| blk(25)| blk(26)| blk(27)| blk(28)| blk(29)|
 *              | IOC(0) | IOC(1) | IOC(2) | IOC(0) | IOC(1) | IOC(2) |
 *              +--------+--------+--------+--------+--------+--------+
 *
 * Return:      None
 * Errors:      None
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
int
ioc_main(int64_t context_id)
{
    int                  subfile_rank;
    int                  flag, ret;
    int                  max_work_depth;
    int                  shutdown_requested;
    MPI_Status           status, msg_status;
    sf_work_request_t *  incoming_requests = NULL;
    useconds_t           delay             = 20;
    subfiling_context_t *context           = get__subfiling_object(context_id);
    double               queue_start_time;

    assert(context != NULL);
    /* We can't have opened any files at this point..
     * The file open approach has changed so that the normal
     * application rank (hosting this thread) does the file open.
     * We can simply utilize the file descriptor (which should now
     * represent an open file).
     */

    subfile_rank = context->sf_group_rank;

    if (request_count_per_rank == NULL) {
        request_count_per_rank = (int *)calloc((size_t)WORLD_SIZE(context), sizeof(int));
        assert(request_count_per_rank != NULL);
    }

    max_work_depth    = MAX(8, WORLD_SIZE(context) * MAX_WORK_PER_RANK);
    incoming_requests = (sf_work_request_t *)calloc((size_t)(max_work_depth + 1), sizeof(sf_work_request_t));

    /* Validate that the allocation succeeded */
    assert(incoming_requests != NULL);

    /* Initialize atomic vars */
    atomic_init(&sf_workinprogress, 0);
    atomic_init(&sf_work_pending, 0);
    atomic_init(&sf_file_close_count, 0);
    atomic_init(&sf_file_refcount, 0);
    atomic_init(&sf_ioc_fini_refcount, 0);
    atomic_init(&sf_shutdown_flag, 0);
    atomic_init(&sf_ioc_ready, 1);
#if 1           /* JRM */
    /* this variable is incremented by tpool_add_work(), and decremented when the 
     * received I/O request is completed.
     *
     * On shutdown, we must wait until this field is decremented to zero before 
     * taking down the thread pool.
     */
    atomic_init(&sf_io_ops_pending, 0);
#endif          /* JRM */
    shutdown_requested = 0;

#if 0  /* JRM */
    while (!shutdown_requested || sf_work_pending) {
#else  /* JRM */
    while ( ( ! shutdown_requested ) || ( 0 < atomic_load(&sf_io_ops_pending) ) || sf_work_pending) {
#endif /* JRM */
        flag = 0;
        ret  = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int                count;
            int                index        = 0;
            int                request_size = (int)sizeof(sf_work_request_t);
            int                source       = status.MPI_SOURCE;
            int                tag          = status.MPI_TAG;

#if 1  /* JRM */ 
            if ( ( tag != READ_INDEP ) && ( tag != WRITE_INDEP ) ) {

                HDprintf("\n\nioc_main: received non READ_INDEP / WRITE_INDEP mssg. tag = %d.\n\n", tag);
                HDfflush(stdout);
            }
#endif /* JRM */

            MPI_Get_count(&status, MPI_BYTE, &count);
            if (count > request_size) {
                msg = (sf_work_request_t *)malloc((size_t)count);
                ret = MPI_Recv(msg, count, MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);
            }
            else {
                index = atomic_load(&sf_workinprogress);
                ret = MPI_Recv(&incoming_requests[index], count, MPI_BYTE, source, tag, context->sf_msg_comm,
                               &msg_status);
                if (MPI_SUCCESS == ret) {
                    int howmany = 0;
                    MPI_Get_count(&msg_status, MPI_BYTE, &howmany);
                    if (howmany != count) {
                        printf("%s: MPI_Recv completed %d bytes of %d\n", __func__, howmany, count);
                        fflush(stdout);
                    }
                }
            }
            queue_start_time = MPI_Wtime();
            if (ret == MPI_SUCCESS) {
                if (msg) {
                    printf("%s: non-std msg=(%p) from %d\n", __func__, (void *)msg, source);
                    fflush(stdout);

                    msg->source       = source;
                    msg->subfile_rank = subfile_rank;
                    msg->context_id   = context->sf_context_id;
                    msg->start_time   = queue_start_time;
                    tpool_add_work(msg);
                }
                else {
                    incoming_requests[index].tag          = tag;
                    incoming_requests[index].source       = source;
                    incoming_requests[index].subfile_rank = subfile_rank;
                    incoming_requests[index].start_time   = queue_start_time;
                    incoming_requests[index].buffer       = NULL;
                    tpool_add_work(&incoming_requests[index]);
                    if (index == max_work_depth - 1) {
                        atomic_init(&sf_workinprogress, 0);
                    }
                    else {
                        atomic_fetch_add(&sf_workinprogress, 1); // atomic
                    }
                }
            }
        }
        else {
            usleep(delay);
        }
        shutdown_requested = atomic_load(&sf_shutdown_flag);
    }

    if (incoming_requests) {
        free(incoming_requests);
    }

    /* Reset the shutdown flag */
    atomic_init(&sf_shutdown_flag, 0);

    return 0;
}

#else /* JRM */ /* re-written version of ioc_main() */

int
ioc_main(int64_t context_id)
{
    int                  subfile_rank;
    int                  flag, ret;
    int                  max_work_depth;
    int                  shutdown_requested;
    MPI_Status           status, msg_status;
    sf_work_request_t    wk_req;
    useconds_t           delay   = 20;
    subfiling_context_t *context = get__subfiling_object(context_id);
    double               queue_start_time;

#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: entering.\n\n");
    HDfflush(stdout);
#endif /* JRM */

    assert(context != NULL);
    /* We can't have opened any files at this point..
     * The file open approach has changed so that the normal
     * application rank (hosting this thread) does the file open.
     * We can simply utilize the file descriptor (which should now
     * represent an open file).
     */

    subfile_rank = context->sf_group_rank;

    /* zero out the wk_req, since the received message will typically be smaller
     * than sizeof(sf_work_request_t).
     */
    HDmemset(&wk_req, 0, sizeof(sf_work_request_t));

    /* Initialize atomic vars */
    /* JRM */ /* delete most of these? */
    atomic_init(&sf_workinprogress, 0);
#if 1  /* JRM */
    atomic_init(&sf_work_pending, 0);
#endif /* JRM */
    atomic_init(&sf_file_close_count, 0);
    atomic_init(&sf_file_refcount, 0);
    atomic_init(&sf_ioc_fini_refcount, 0);
    atomic_init(&sf_shutdown_flag, 0);
#if 1  /* JRM */
    /* this variable is incremented by H5FD_ioc__queue_io_q_entry() when work
     * is added to the I/O request queue, and decremented by H5FD_ioc__complete_io_q_entry()
     * when an I/O request is completed and removed from the queue..
     *
     * On shutdown, we must wait until this field is decremented to zero before
     * taking down the thread pool.
     *
     * Note that this is a convenience variable -- we could use io_queue_g.q_len instead.
     * However, accessing this field requires locking io_queue_g.q_mutex.
     */
#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: setting sf_io_ops_pending to zero.  sf_io_ops_pending = %d.\n\n",
             atomic_load(&sf_io_ops_pending));
    HDfflush(stdout);
#endif /* JRM */
    atomic_init(&sf_io_ops_pending, 0);
#endif /* JRM */
    /* tell initialize_ioc_threads() that ioc_main() is ready to enter its main loop */
    atomic_init(&sf_ioc_ready, 1);
    shutdown_requested = 0;

    while ((!shutdown_requested) || (0 < atomic_load(&sf_io_ops_pending))
#if 1  /* JRM */
           || (0 < atomic_load(&sf_work_pending))
#endif /* JRM */
    ) {
        flag = 0;
        ret  = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int                count;
            int                index        = 0;
            int                request_size = (int)sizeof(sf_work_request_t);
            int                source       = status.MPI_SOURCE;
            int                tag          = status.MPI_TAG;

#if 1  /* JRM */
            if ((tag != READ_INDEP) && (tag != WRITE_INDEP) && (tag != TRUNC_OP) && (tag != GET_EOF_OP)) {

                HDprintf("\n\nioc_main: received non READ_INDEP / WRITE_INDEP / TRUNC_OP / GET_EOF_OP mssg. "
                         "tag = %d.\n\n",
                         tag);
                HDfflush(stdout);
            }
#endif /* JRM */

            MPI_Get_count(&status, MPI_BYTE, &count);

            /* convert this assert to a proper error message once we decide how to handle error
             * reporting from the I/O concentrator.
             */
            HDassert(count <= sizeof(sf_work_request_t));

            /* zero out the wk_req, since the received message will typically be smaller
             * than sizeof(sf_work_request_t).
             */
            HDmemset(&wk_req, 0, sizeof(sf_work_request_t));

            ret = MPI_Recv(&wk_req, count, MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);

            if (MPI_SUCCESS == ret) {

                int howmany = 0;

                MPI_Get_count(&msg_status, MPI_BYTE, &howmany);

                if (howmany != count) {
                    printf("%s: MPI_Recv completed %d bytes of %d\n", __func__, howmany, count);
                    fflush(stdout);
                }
            }

            queue_start_time = MPI_Wtime();

            if (ret == MPI_SUCCESS) {

                int curr_io_ops_pending;

                wk_req.tag          = tag;
                wk_req.source       = source;
                wk_req.subfile_rank = subfile_rank;
                wk_req.start_time   = queue_start_time;
                wk_req.buffer       = NULL;

                H5FD_ioc__queue_io_q_entry(&wk_req);

                HDassert(atomic_load(&sf_io_ops_pending) >= 0);

                H5FD_ioc__dispatch_elegible_io_q_entries();
            }
        }
        else {
            usleep(delay);
        }
        shutdown_requested = atomic_load(&sf_shutdown_flag);
    }

    /* Reset the shutdown flag */
    atomic_init(&sf_shutdown_flag, 0);

#if 0  /* JRM */
    HDfprintf(stdout, "\n\nioc_main: exiting.\n\n");
    HDfflush(stdout);
#endif /* JRM */

    return 0;

} /* ioc_main() */

#endif /* JRM */ /* re-written version of ioc_main() */

/*
=========================================
Private helper functions
=========================================
*/

#if 0 /* JRM */ /* original version */
static int
send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int ack = 1;
    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending ACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;
}
#else /* JRM */ /* version modified to send expected data send tag */

static int
send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm, int ack)
{

    HDassert(ack > 0);

    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending ACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;

} /* send_ack__() */

#endif /* JRM */ /* version modified to send expected data send tag */

static int
send_nack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int nack = 0;
    int ret  = MPI_Send(&nack, 1, MPI_INT, target, tag, comm);

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending NACK to MPI_rank(%d)\n", subfile_rank, target);
        }
    }
#endif
    return ret;
}

/*
=========================================
queue_xxx functions that should be run
from the thread pool threads...
=========================================
*/

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_write_indep
 *
 * Purpose:     Implement the IOC independent write function.  The
 *              function is invoked as a result of the IOC receiving the
 *              "header"/RPC.  What remains is to allocate memory for the
 *              data sent by the client and then write the data to our
 *              subfile.  We utilize pwrite for the actual file writing.
 *              File flushing is done at file close.
 *
 * Return:      The integer status returned by the Internal read_independent
 *              function.  Successful operations will return 0.
 * Errors:      An MPI related error value.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
int
queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
#if 1           /* JRM */
    int                  actual_bytes_received;
#endif          /* JRM */
    char *               recv_buffer = NULL;
    int                  ret         = MPI_SUCCESS;
    MPI_Status           msg_status;
    int64_t              data_size       = msg->header[0];
    int64_t              file_offset     = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    double               t_start, t_end;
    double               t_write, t_wait, t_queue_delay;
    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    int64_t              stripe_id  = file_offset + data_size;
    haddr_t              sf_eof;
    assert(sf_context != NULL);

    sf_eof = (haddr_t)(stripe_id % sf_context->sf_stripe_size);
    stripe_id /= sf_context->sf_stripe_size;
    sf_eof += (haddr_t)((stripe_id * sf_context->sf_blocksize_per_stripe) + sf_context->sf_base_addr);

    /* flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;
    /* For debugging performance */
    sf_write_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                    "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld, "
                    "queue_delay = %lf seconds\n",
                    subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
        }
    }
#endif

    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *)malloc((size_t)data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
    ret = MPI_Recv(recv_buffer, (int)data_size, MPI_BYTE, source, WRITE_INDEP_DATA, comm, &msg_status);

#if 1  /* JRM */
    if ( MPI_SUCCESS != MPI_Get_count(&msg_status, MPI_BYTE, &actual_bytes_received) ) {

        HDprintf("\n\nqueue_write_indep(): can't get actual bytes receive.\n\n");
        HDfflush(stdout);

    } else if ( actual_bytes_received != data_size ) {

        HDprintf("\n\nqueue_write_indep(): message size mismatch -- expected = %ld, actual = %d.\n\n", 
                 data_size, actual_bytes_received);
        HDfflush(stdout);

    }
#endif /* JRM */

    t_end  = MPI_Wtime();
    t_wait = t_end - t_start;
    sf_write_wait_time += t_wait;
    t_start = t_end;
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n", subfile_rank,
                    __func__, data_size, source, ret);
        }
    }
#endif

    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d "
               "returned an error(%s)\n",
               subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source, estring);
        fflush(stdout);
        return ret;
    }

    if (msg->serialize)
        ioc__wait_for_serialize(msg);

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d)] WARNING: %s called while subfile_fid = %d (closed)\n", subfile_rank, __func__, fd);
        fflush(stdout);
    }
    else {
        if (sf_write_data(fd, file_offset, recv_buffer, data_size, subfile_rank) < 0) {
            free(recv_buffer);
            recv_buffer = NULL;
            printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank, __func__);
            fflush(stdout);
            return -1;
        }
        t_end   = MPI_Wtime();
        t_write = t_end - t_start;
        sf_pwrite_time += t_write;
    }

    sf_queue_delay_time += t_queue_delay;

    /* Done... */
    if (sf_eof > sf_context->sf_eof)
        sf_context->sf_eof = sf_eof;

#ifdef VERBOSE
    printf("[ioc(%d)] %s local sf_eof = %ld sf_context=%p\n", subfile_rank, __func__, sf_context->sf_eof,
           (void *)sf_context);
    fflush(stdout);
#endif
    if (recv_buffer) {
        free(recv_buffer);
    }
    return 0;
}

#else /* JRM */ /* version modified for new dispatch code */

int
queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm, int counter)
{
    int                  fd;
#if 1           /* JRM */
    int                  actual_bytes_received;
#endif          /* JRM */
    int                  rcv_tag     = ((counter & 0xFFFF) << 12) | WRITE_INDEP_DATA;
    char *               recv_buffer = NULL;
    int                  ret         = MPI_SUCCESS;
    MPI_Status           msg_status;
    int64_t              data_size       = msg->header[0];
    int64_t              file_offset     = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    double               t_start, t_end;
    double               t_write, t_wait, t_queue_delay;
    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    int64_t              stripe_id  = file_offset + data_size;
    haddr_t              sf_eof;
    assert(sf_context != NULL);

    sf_eof = (haddr_t)(stripe_id % sf_context->sf_stripe_size);
    stripe_id /= sf_context->sf_stripe_size;
    sf_eof += (haddr_t)((stripe_id * sf_context->sf_blocksize_per_stripe) + sf_context->sf_base_addr);

    /* flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;
    /* For debugging performance */
    sf_write_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                    "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld, "
                    "queue_delay = %lf seconds\n",
                    subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
        }
    }
#endif

    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *)malloc((size_t)data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm, rcv_tag);
    ret = MPI_Recv(recv_buffer, (int)data_size, MPI_BYTE, source, rcv_tag, comm, &msg_status);

#if 1  /* JRM */
    if (MPI_SUCCESS != MPI_Get_count(&msg_status, MPI_BYTE, &actual_bytes_received)) {

        HDprintf("\n\nqueue_write_indep(): can't get actual bytes receive.\n\n");
        HDfflush(stdout);
    }
    else if (actual_bytes_received != data_size) {

        HDprintf("\n\nqueue_write_indep(): message size mismatch -- expected = %ld, actual = %d.\n\n",
                 data_size, actual_bytes_received);
        HDfflush(stdout);
    }
#endif /* JRM */

    t_end  = MPI_Wtime();
    t_wait = t_end - t_start;
    sf_write_wait_time += t_wait;
    t_start = t_end;
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n", subfile_rank,
                    __func__, data_size, source, ret);
        }
    }
#endif

    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d "
               "returned an error(%s)\n",
               subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source, estring);
        fflush(stdout);
        return ret;
    }

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d)] WARNING: %s called while subfile_fid = %d (closed)\n", subfile_rank, __func__, fd);
        fflush(stdout);
    }
    else {
        if (sf_write_data(fd, file_offset, recv_buffer, data_size, subfile_rank) < 0) {
            free(recv_buffer);
            recv_buffer = NULL;
            printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank, __func__);
            fflush(stdout);
            return -1;
        }
        t_end   = MPI_Wtime();
        t_write = t_end - t_start;
        sf_pwrite_time += t_write;
    }

    sf_queue_delay_time += t_queue_delay;

    /* Done... */
    if (sf_eof > sf_context->sf_eof)
        sf_context->sf_eof = sf_eof;

#ifdef VERBOSE
    printf("[ioc(%d)] %s local sf_eof = %ld sf_context=%p\n", subfile_rank, __func__, sf_context->sf_eof,
           (void *)sf_context);
    fflush(stdout);
#endif
    if (recv_buffer) {
        free(recv_buffer);
    }
    return 0;

} /* queue_write_indep() */

#endif /* JRM */ /* version modified for new dispatch code */

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_read_indep
 *
 * Purpose:     Implement the IOC independent read function.  The
 *              function is invoked as a result of the IOC receiving the
 *              "header"/RPC.  What remains is to allocate memory for
 *              reading the data and then to send this to the client.
 *              We utilize pread for the actual file reading.
 *
 * Return:      The integer status returned by the Internal read_independent
 *              function.  Successful operations will return 0.
 * Errors:      An MPI related error value.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
#if 0 /* JRM */ /* original version */
int
queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int     fd;
    char *  send_buffer     = NULL;
    int     ret             = MPI_SUCCESS;
    int64_t data_size       = msg->header[0];
    int64_t file_offset     = msg->header[1];
    int64_t file_context_id = msg->header[2];
    double  t_start, t_end;
    double  t_read, t_queue_delay;

    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    assert(sf_context != NULL);

    sf_context->sf_read_count++;
    /* For debugging performance */
    sf_read_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

    fd = sf_context->sf_fid;
    if (fd < 0) {
        printf("[ioc(%d) %s] subfile(%d) file descriptor not valid\n", subfile_rank, __func__, fd);
        return -1;
    }

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile,
                "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld "
                "queue_delay=%lf seconds\n",
                subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
    }
#endif
    if ((send_buffer = (char *)malloc((size_t)data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(fd, file_offset, send_buffer, data_size, subfile_rank) < 0) {
        printf("[%d] %s - sf_read_data fd=%d for source(%d) returned an error!\n", subfile_rank, __func__, fd,
               source);
        fflush(stdout);
        /*
         * Should send a zero(0) byte message to the client to prevent
         * it from hanging...
         */
        MPI_Send(send_buffer, 0, MPI_BYTE, source, READ_INDEP_DATA, comm);
        free(send_buffer);
        return -1;
    }

    ret = MPI_Send(send_buffer, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d)] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               subfile_rank, data_size, source, estring);
        fflush(stdout);
        return ret;
    }
    t_end  = MPI_Wtime();
    t_read = t_end - t_start;
    sf_pread_time += t_read;
    sf_queue_delay_time += t_queue_delay;

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n", subfile_rank, source);
    }
#endif

    if (send_buffer) {
        free(send_buffer);
        send_buffer = NULL;
    }

    return 0;
} /* end queue_read_indep() */

#else /* JRM */ /* version modified for new dispatch code */

int
queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int     fd;
    char *  send_buffer     = NULL;
    int     ret             = MPI_SUCCESS;
    int64_t data_size       = msg->header[0];
    int64_t file_offset     = msg->header[1];
    int64_t file_context_id = msg->header[2];
    double  t_start, t_end;
    double  t_read, t_queue_delay;

    subfiling_context_t *sf_context = get__subfiling_object(file_context_id);
    assert(sf_context != NULL);

    sf_context->sf_read_count++;
    /* For debugging performance */
    sf_read_ops++;

    t_start       = MPI_Wtime();
    t_queue_delay = t_start - msg->start_time;

    fd = sf_context->sf_fid;
    if (fd < 0) {
        printf("[ioc(%d) %s] subfile(%d) file descriptor not valid\n", subfile_rank, __func__, fd);
        return -1;
    }

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile,
                "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld "
                "queue_delay=%lf seconds\n",
                subfile_rank, __func__, source, data_size, file_offset, t_queue_delay);
    }
#endif
    if ((send_buffer = (char *)malloc((size_t)data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(fd, file_offset, send_buffer, data_size, subfile_rank) < 0) {
        printf("[%d] %s - sf_read_data fd=%d for source(%d) returned an error!\n", subfile_rank, __func__, fd,
               source);
        fflush(stdout);
        /*
         * Should send a zero(0) byte message to the client to prevent
         * it from hanging...
         */
        MPI_Send(send_buffer, 0, MPI_BYTE, source, READ_INDEP_DATA, comm);
        free(send_buffer);
        return -1;
    }

    ret = MPI_Send(send_buffer, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d)] ERROR! MPI_Send of %ld bytes to %d returned an "
               "error(%s)\n",
               subfile_rank, data_size, source, estring);
        fflush(stdout);
        return ret;
    }
    t_end  = MPI_Wtime();
    t_read = t_end - t_start;
    sf_pread_time += t_read;
    sf_queue_delay_time += t_queue_delay;

#ifndef NDEBUG
    if (sf_verbose_flag && (sf_logfile != NULL)) {
        fprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n", subfile_rank, source);
    }
#endif

    if (send_buffer) {
        free(send_buffer);
        send_buffer = NULL;
    }

    return 0;
} /* end queue_read_indep() */

#endif /* JRM */ /* version modified for new dispatch code */

/* ---------------------------------------------------
 * Helper function for subfiling_open_file() see below
 * Subfiles should be located in the same directory
 * as the HDF5 file unless the user has provided
 * an alternate directory name as indicated by the
 * sf_context->subfile_prefix argument.
 * ---------------------------------------------------*/
static void
get__subfile_name(subfiling_context_t *sf_context, int64_t h5_file_id, int subfile_rank, char **_basename,
                  char **_subfile_dir, char *filepath)
{
    char *prefix = NULL, *subfile_dir = NULL;
    char *base               = NULL;
    int   n_io_concentrators = sf_context->topology->n_io_concentrators;

    /* We require this to be non-null */
    HDassert(sf_context);

    prefix = (char *)malloc(PATH_MAX);
    HDassert(prefix);

    /* Under normal operation, we co-locate subfiles
     * with the HDF5 file
     */
    strcpy(prefix, sf_context->h5_filename);
    base       = basename(prefix);
    *_basename = strdup(base);

    if (sf_context->subfile_prefix == NULL) {
        subfile_dir   = dirname(prefix);
        *_subfile_dir = strdup(subfile_dir);
    }
    else {
        /* Note: Users may specify a directory name which is inaccessible
         * from where the current is running.  In particular, "node-local"
         * storage is not uniformly available to all processes.
         * We would like to check if the user pathname unavailable and
         * if so, we could default to creating the subfiles in the
         * current directory. (?)
         */
        *_subfile_dir = strdup(sf_context->subfile_prefix);
    }

    /* The subfile naming should produce files of the following form:
     * If we assume the HDF5 file is named ABC.h5, then subfiles
     * will have names:
     *   ABC.h5.subfile_<file-number>_00_of_20,
     *   ABC.h5.subfile_<file-number>_01_of_20, and
     *   ABC.h5.subfile_<file-number>.config
     */
    int numD = numDigits(n_io_concentrators);
    sprintf(filepath, "%s/%s" SF_FILENAME_TEMPLATE, subfile_dir, base, h5_file_id, numD, subfile_rank,
            n_io_concentrators);
    if (prefix)
        HDfree(prefix);
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC subfiling_open_file
 *
 * Purpose:     This function gets called when a client invokes a OPEN_OP.
 *              The HDF5 file opening protocol actually attempts to open
 *              a file; first without any truncate other flags which would
 *              modify the file state if it already exists.  A file close
 *              and then the second file open using the user supplied open
 *              flags is invoked.   The OPEN_OP provides the user flags as
 *              part of the RPC message.  The file prefix info doesn't
 *              transmitted as part of the RPC since it is available as
 *              part of the client context which can be utilized by the
 *              IOC thread.  We access the sf_context by reading the
 *              cache of contexts at the index provided with the RPC msg.
 *
 * Return:      The integer status returned by the Internal read_independent
 *              function.  Successful operations will return 0.
 * Errors:      An MPI related error value.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
int
subfiling_open_file(sf_work_request_t *msg, int subfile_rank, int flags)
{
    int  errors = 0;
    char filepath[PATH_MAX];
    char linebuf[PATH_MAX];

    char * temp        = NULL;
    char * prefix      = NULL;
    char * subfile_dir = NULL;
    char * base        = NULL;
    mode_t mode        = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;

    double t_start = 0.0, t_end = 0.0;
    /* Only the real IOCs open the subfiles
     * Once a file is opened, all additional file open requests
     * can return immediately.
     */

    t_start = MPI_Wtime();
    /* Only allow the actual IO concentrator ranks to create sub-files */
    if (subfile_rank >= 0) {
        int                  k, retries = 2;
        int64_t              h5_file_id      = msg->header[1];
        int64_t              file_context_id = msg->header[2];
        subfiling_context_t *sf_context      = get__subfiling_object(file_context_id);
        assert(sf_context != NULL);

        memset(filepath, 0, PATH_MAX);

        begin_thread_exclusive();
        /* Check to see whether we need to create the subfile
         * and possibly (IFF our subfile_rank is 0) a config file.
         */

        get__subfile_name(sf_context, h5_file_id, subfile_rank, &base, &subfile_dir, filepath);
        sf_context->sf_filename = strdup(filepath);

        assert(sf_context->sf_filename);

        /* Check if we need to create the subfiles */
        if (sf_context->sf_fid == -2) {
            int  n_io_concentrators = sf_context->topology->n_io_concentrators;
            int *io_concentrator    = sf_context->topology->io_concentrator;
            for (k = 0; k < retries; k++) {
                int fd;
                if ((fd = HDopen(filepath, O_CREAT | O_RDWR | O_TRUNC, mode)) > 0) {
                    sf_context->sf_fid = fd;
                    sf_context->sf_eof = 0;
                    break;
                }
            }
            if (sf_context->sf_fid < 0) {
                end_thread_exclusive();
                perror("subfiling_open_file/open");
                HDprintf("[%d %s] file create(%s) failed!\n", subfile_rank, __func__, filepath);
                HDfflush(stdout);

#ifndef NDEBUG
                if (sf_verbose_flag) {
                    printf("[%d %s] file create(%s) failed!\n", subfile_rank, __func__, filepath);
                    fflush(stdout);
                }
#endif
                errors++;
                goto done;
            }
            sprintf(filepath, "%s/%s.subfile_%ld.config", subfile_dir, base, h5_file_id);
            /* SUBFILE rank 0 does the work creating a config file */
            if ((subfile_rank == 0) && (flags & O_CREAT)) {
                FILE *f = NULL;
                /* If a config file already exists, AND
                 * the user wants to truncate subfiles (if they exist),
                 * then we should also truncate an existing config file.
                 */
                if (access(filepath, flags) == 0) {
                    truncate(filepath, 0);
                }
                f = HDfopen(filepath, "w+");
                if (f != NULL) {
                    sprintf(linebuf, "stripe_size=%ld\n", sf_context->sf_stripe_size);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    sprintf(linebuf, "aggregator_count=%d\n", n_io_concentrators);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    sprintf(linebuf, "hdf5_file=%s\n", sf_context->h5_filename);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    sprintf(linebuf, "subfile_dir=%s\n", subfile_dir);

                    int numD = numDigits(n_io_concentrators);
                    for (k = 0; k < n_io_concentrators; k++) {
                        sprintf(linebuf, "%s" SF_FILENAME_TEMPLATE "\n", base, h5_file_id, numD, k,
                                n_io_concentrators);
                        HDfwrite(linebuf, 1, strlen(linebuf), f);
                    }

                    fclose(f);
                }
                else {
                    perror("fopen(config)");
                    errors++;
                    goto done;
                }
            }

#ifndef NDEBUG
            if (sf_verbose_flag) {
                if (sf_logfile) {
                    HDfprintf(sf_logfile, "[ioc:%d] Opened subfile %s\n", subfile_rank, filepath);
                }
            }
#endif
        }
        else {
            for (k = 0; k < retries; k++) {
                int fd;
                if ((fd = HDopen(filepath, O_CREAT | O_RDWR, mode)) > 0) {
                    sf_context->sf_fid = fd;
                    break;
                }
            }
            if (sf_context->sf_fid < 0) {
                end_thread_exclusive();
                perror("subfiling_open_file/open");
                HDprintf("[%d %s] file open(%s) failed!\n", subfile_rank, __func__, filepath);
                HDfflush(stdout);

#ifndef NDEBUG
                if (sf_verbose_flag) {
                    HDprintf("[%d %s] file open(%s) failed!\n", subfile_rank, __func__, filepath);
                    HDfflush(stdout);
                }
#endif
                errors++;
                goto done;
            }
        }
        end_thread_exclusive();
    }

done:
    t_end = MPI_Wtime();
    if (base)
        HDfree(base);
    if (subfile_dir)
        HDfree(subfile_dir);

#ifndef NDEBUG
    if (sf_verbose_flag) {
        printf("[%s %d] open completed in %lf seconds with %d errors\n", __func__, subfile_rank,
               (t_end - t_start), errors);
        fflush(stdout);
    }
#endif
    return errors;
} /* end subfiling_open_file() */

/*-------------------------------------------------------------------------
 * Function:    UTILITY FUNCTIONS:
 *
 *              sf_get_mpi_rank  - (not used) retrieves the MPI rank of the
 *                                 calling process.  Was used when pairing
 *                                 the subfiling VFD with the SUBFILING VFD.
 *
 *              sf_get_mpi_size  - (not used) retrieves the MPI size of the
 *                                 communicator associated with the open
 *                                 file.
 *
 *              sf_get_group_com - (not used) retrieves the MPI Comm object
 *                                 associated with the open file/sf_context.
 *
 *              sf_subfile_set_logging - (not used) informs one or all IOC
 *                                 instances to set the verbose/logging flag
 *                                 to the value provided by the user.
 *
 * Return:      none
 * Errors:      none
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

int
sf_get_mpi_rank(hid_t fid, int *rank)
{
    hid_t                context_id = fid_map_to_context((uint64_t)fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(rank != NULL);
    *rank = sf_context->sf_group_rank;
    return 0;
}

int
sf_get_mpi_size(hid_t fid, int *size)
{
    hid_t                context_id = fid_map_to_context((uint64_t)fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(size != NULL);
    *size = sf_context->sf_group_size;
    return 0;
}

int
sf_get_group_comm(hid_t fid, MPI_Comm *comm)
{
    hid_t                context_id = fid_map_to_context((uint64_t)fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(comm != NULL);
    *comm = sf_context->sf_group_comm;
    return 0;
}

int
sf_subfile_set_logging(hid_t sf_fid, int ioc_rank, int flag)
{
    int                  ioc;
    int                  status     = 0;
    hid_t                context_id = fid_map_to_context((uint64_t)sf_fid);
    subfiling_context_t *sf_context = get__subfiling_object(context_id);
    int                  n_io_concentrators;
    int *                io_concentrator = NULL;
    int64_t              lflag           = (int64_t)(flag & 0xFF);
    int64_t              msg[3];

    assert(sf_context != NULL);

    msg[0] = lflag;
    msg[1] = 0;
    msg[2] = sf_context->sf_context_id;

    n_io_concentrators = sf_context->topology->n_io_concentrators;
    io_concentrator    = sf_context->topology->io_concentrator;

    for (ioc = 0; ioc < n_io_concentrators; ioc++) {
        if ((flag < 0) || (flag == ioc_rank)) {
            status =
                MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[ioc], LOGGING_OP, sf_context->sf_msg_comm);
        }
    }
    return status;
}

/*-------------------------------------------------------------------------
 * Function:    report_sf_eof
 *
 * Purpose:     Determine the target sub-file's eof and report this value
 *              to the requesting rank.
 *
 *              Notes: This function will have to be reworked once we solve
 *                     the IOC error reporting problem.
 *
 *                     This function mixes functionality that should be
 *                     in two different VFDs.
 *
 * Return:      0 if successful, 1 or an MPI error code on failure.
 *
 * Programmer:  John Mainzer
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */

int
report_sf_eof(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
    int                  mpi_ret;
    int64_t              eof_req_reply[3];
    int64_t              file_context_id;
    subfiling_context_t *sf_context = NULL;
    h5_stat_t            sb;

    HDassert(msg);

    /* first get the EOF of the target file. */

    file_context_id = msg->header[2];

    if (NULL == (sf_context = get__subfiling_object(file_context_id))) {

        HDfprintf(stdout, "report_sf_eof: get__subfiling_object() failed.\n");
        HDfflush(stdout);
        return (1);
    }

    fd = sf_context->sf_fid;

    if (HDfstat(fd, &sb) < 0) {

        HDfprintf(stdout, "report_sf_eof: get__subfiling_object() failed.\n");
        HDfflush(stdout);
        return (1);
    }

    eof_req_reply[0] = (int64_t)subfile_rank;
    eof_req_reply[1] = (int64_t)(sb.st_size);
    eof_req_reply[2] = 0; /* not used */

    /* return the subfile EOF to the querying rank */
    if (MPI_SUCCESS != (mpi_ret = MPI_Send(eof_req_reply, 3, MPI_INT64_T, source, GET_EOF_COMPLETED, comm))) {

        HDfprintf(stdout, "report_sf_eof: MPI_Send failed -- return code = %d.\n", mpi_ret);
        HDfflush(stdout);
        return (mpi_ret);
    }

    return 0;

} /* report_sf_eof() */
