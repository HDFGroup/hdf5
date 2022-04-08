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

#include "H5FDsubfiling_priv.h"

static int sf_close_file_count      = 0;
static int sf_ops_after_first_close = 0;
static int sf_enable_directIO       = 0;

/* The following is our basic template for a subfile filename.
 * Note that eventually we shouldn't use 0_of_N since we
 * intend to use the user defined HDF5 filename for a
 * zeroth subfile as well as for all metadata.
 */
#define SF_FILENAME_TEMPLATE ".subfile_%ld_%0*d_of_%d"
static int *request_count_per_rank = NULL;

atomic_int sf_file_open_count = 0;
atomic_int sf_ioc_ready       = 0;
atomic_int sf_shutdown_flag   = 0;

int n_io_pending = 0;

#if 0 /* TODO */
typedef struct _client_xfer_info {
    int64_t offset;
    int64_t length;
    int     ioc_targets;
    io_op_t op;
} client_xfer_info_t;
#endif

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

int client_op_index = 0;
int client_op_size  = 0;
#if 0 /* TODO */
client_xfer_info_t *client_ops      = NULL;
#endif

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

static char *get_ioc_subfile_path(int ioc, int ioc_count, subfiling_context_t *sf_context);

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
        HDsnprintf(filepath, sizeof(filepath), "%s/" SF_FILENAME_TEMPLATE, prefix, sf_context->h5_file_id, numD, ioc, ioc_count);
    }
    else {
        strcpy(filepath, sf_context->h5_filename);
        subfile_dir = strrchr(filepath, '/');
        assert(subfile_dir);
        sprintf(subfile_dir + 1, SF_FILENAME_TEMPLATE, sf_context->h5_file_id, numD, ioc, ioc_count);
    }
    return filepath;
} /* end get_ioc_subfile_path() */

#if 1 /* JRM */ /* delete this if all goes well */
static int
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

/*
=========================================
queue_xxx functions that should be run
from the thread pool threads...
=========================================
*/

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
    int    errors = 0;
    char   filepath[PATH_MAX];
    char   linebuf[PATH_MAX];
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
            int n_io_concentrators = sf_context->topology->n_io_concentrators;
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
            HDsnprintf(filepath, sizeof(filepath), "%s/%s.subfile_%ld.config", subfile_dir, base, h5_file_id);
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
                    HDsnprintf(linebuf, sizeof(linebuf), "stripe_size=%ld\n", sf_context->sf_stripe_size);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    HDsnprintf(linebuf, sizeof(linebuf), "aggregator_count=%d\n", n_io_concentrators);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    HDsnprintf(linebuf, sizeof(linebuf), "hdf5_file=%s\n", sf_context->h5_filename);
                    HDfwrite(linebuf, 1, strlen(linebuf), f);
                    HDsnprintf(linebuf, sizeof(linebuf), "subfile_dir=%s\n", subfile_dir);

                    int numD = numDigits(n_io_concentrators);
                    for (k = 0; k < n_io_concentrators; k++) {
                        HDsnprintf(linebuf, sizeof(linebuf), "%s" SF_FILENAME_TEMPLATE "\n", base,
                                h5_file_id, numD, k, n_io_concentrators);
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

static int
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
