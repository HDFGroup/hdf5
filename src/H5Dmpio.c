/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  rky 980813
 * KY 2005 revised the code and made the change to support and optimize
 * collective IO support.
 * Purpose:    Functions to read/write directly between app buffer and file.
 *
 *         Beware of the ifdef'ed print statements.
 *         I didn't make them portable.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h" /* This source code file is part of the H5D module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions */
#include "H5CXprivate.h" /* API Contexts      */
#include "H5Dpkg.h"      /* Datasets          */
#include "H5Eprivate.h"  /* Error handling    */
#include "H5Fprivate.h"  /* File access       */
#include "H5FDprivate.h" /* File drivers      */
#include "H5FLprivate.h" /* Free Lists        */
#include "H5Iprivate.h"  /* IDs               */
#include "H5MMprivate.h" /* Memory management */
#include "H5Oprivate.h"  /* Object headers    */
#include "H5Pprivate.h"  /* Property lists    */
#include "H5Sprivate.h"  /* Dataspaces        */
#include "H5VMprivate.h" /* Vector            */

#ifdef H5_HAVE_PARALLEL

/****************/
/* Local Macros */
/****************/

/* Macros to represent different IO options */
#define H5D_ONE_LINK_CHUNK_IO          0
#define H5D_MULTI_CHUNK_IO             1
#define H5D_ONE_LINK_CHUNK_IO_MORE_OPT 2
#define H5D_MULTI_CHUNK_IO_MORE_OPT    3

/***** Macros for One linked collective IO case. *****/
/* The default value to do one linked collective IO for all chunks.
   If the average number of chunks per process is greater than this value,
      the library will create an MPI derived datatype to link all chunks to do collective IO.
      The user can set this value through an API. */

/* Macros to represent options on how to obtain chunk address for one linked-chunk IO case */
#define H5D_OBTAIN_ONE_CHUNK_ADDR_IND 0
#define H5D_OBTAIN_ALL_CHUNK_ADDR_COL 2

/* Macros to define the default ratio of obtaining all chunk addresses for one linked-chunk IO case */
#define H5D_ALL_CHUNK_ADDR_THRES_COL     30
#define H5D_ALL_CHUNK_ADDR_THRES_COL_NUM 10000

/***** Macros for multi-chunk collective IO case. *****/
/* The default value of the threshold to do collective IO for this chunk.
   If the average number of processes per chunk is greater than the default value,
   collective IO is done for this chunk.
*/

/* Macros to represent different IO modes(NONE, Independent or collective)for multiple chunk IO case */
#define H5D_CHUNK_IO_MODE_COL 1

/* Macros to represent the regularity of the selection for multiple chunk IO case. */
#define H5D_CHUNK_SELECT_REG 1

/*
 * Macro to initialize a H5D_chk_idx_info_t
 * structure, given a pointer to a H5D_io_info_t
 * structure
 */
#define H5D_MPIO_INIT_CHUNK_IDX_INFO(index_info, io_info_ptr)                                                \
    do {                                                                                                     \
        index_info.f       = (io_info_ptr)->dset->oloc.file;                                                 \
        index_info.pline   = &((io_info_ptr)->dset->shared->dcpl_cache.pline);                               \
        index_info.layout  = &((io_info_ptr)->dset->shared->layout.u.chunk);                                 \
        index_info.storage = &((io_info_ptr)->dset->shared->layout.storage.u.chunk);                         \
    } while (0)

/*
 * Macro to initialize a H5D_chunk_ud_t structure
 * given a pointer to a H5D_chk_idx_info_t structure
 */
#define H5D_MPIO_INIT_CHUNK_UD_INFO(chunk_ud, index_info_ptr)                                                \
    do {                                                                                                     \
        HDmemset(&chunk_ud, 0, sizeof(H5D_chunk_ud_t));                                                      \
        chunk_ud.common.layout  = (index_info_ptr)->layout;                                                  \
        chunk_ud.common.storage = (index_info_ptr)->storage;                                                 \
    } while (0)

/******************/
/* Local Typedefs */
/******************/

/* Combine chunk address and chunk info into a struct for better performance. */
typedef struct H5D_chunk_addr_info_t {
    haddr_t          chunk_addr;
    H5D_chunk_info_t chunk_info;
} H5D_chunk_addr_info_t;

/* Rank 0 Bcast values */
typedef enum H5D_mpio_no_rank0_bcast_cause_t {
    H5D_MPIO_RANK0_BCAST            = 0x00,
    H5D_MPIO_RANK0_NOT_H5S_ALL      = 0x01,
    H5D_MPIO_RANK0_NOT_CONTIGUOUS   = 0x02,
    H5D_MPIO_RANK0_NOT_FIXED_SIZE   = 0x04,
    H5D_MPIO_RANK0_GREATER_THAN_2GB = 0x08
} H5D_mpio_no_rank0_bcast_cause_t;

/*
 * Information necessary for re-allocating file space for a chunk
 * during a parallel write of a chunked dataset with filters
 * applied.
 */
typedef struct H5D_chunk_alloc_info_t {
    H5F_block_t chunk_current;
    H5F_block_t chunk_new;
    hsize_t     chunk_idx;
} H5D_chunk_alloc_info_t;

/*
 * Information for a chunk pertaining to the dataset's chunk
 * index entry for the chunk
 */
typedef struct H5D_chunk_index_info_t {
    hsize_t  chunk_idx;
    unsigned filter_mask;
    hbool_t  need_insert;
} H5D_chunk_index_info_t;

/*
 * Information about a single chunk when performing collective filtered I/O. All
 * of the fields of one of these structs are initialized at the start of collective
 * filtered I/O in the function H5D__mpio_collective_filtered_chunk_io_setup(). This
 * struct's fields are as follows:
 *
 * index_info - A structure containing the information needed when collectively
 *              re-inserting the chunk into the dataset's chunk index. The structure
 *              is distributed to all ranks during the re-insertion operation. Its fields
 *              are as follows:
 *
 *     chunk_idx - The index of the chunk in the dataset's chunk index.
 *
 *     filter_mask - A bit-mask that indicates which filters are to be applied to the
 *                   chunk. Each filter in a chunk's filter pipeline has a bit position
 *                   that can be masked to disable that particular filter for the chunk.
 *                   This filter mask is saved alongside the chunk in the file.
 *
 *     need_insert - A flag which determines whether or not a chunk needs to be re-inserted into
 *                   the chunk index after the write operation.
 *
 * chunk_info - A pointer to the chunk's H5D_chunk_info_t structure, which contains useful
 *              information like the dataspaces containing the selection in the chunk.
 *
 * chunk_current - The address in the file and size of this chunk before the filtering
 *                 operation. When reading a chunk from the file, this field is used to
 *                 read the correct amount of bytes. It is also used when redistributing
 *                 shared chunks among MPI ranks and as a parameter to the chunk file
 *                 space reallocation function.
 *
 * chunk_new - The address in the file and size of this chunk after the filtering
 *             operation. This field is relevant when collectively re-allocating space
 *             in the file for all of the chunks written to in the I/O operation, as
 *             their sizes may have changed after their data has been filtered.
 *
 * need_read - A flag which determines whether or not a chunk needs to be read from the
 *             file. During writes, if a chunk is being fully overwritten (the entire extent
 *             is selected in its file dataspace), then it is not necessary to read the chunk
 *             from the file. However, if the chunk is not being fully overwritten, it has to
 *             be read from the file in order to update the chunk without trashing the parts
 *             of the chunk that are not selected. During reads, this field should always be
 *             true.
 *
 * skip_filter_pline - A flag which determines whether to skip calls to the filter pipeline
 *                     for this chunk. This flag is mostly useful for correct handling of
 *                     partial edge chunks when the "don't filter partial edge chunks" flag
 *                     is set on the dataset's DCPL.
 *
 * io_size - The total size of I/O to this chunk. This field is an accumulation of the size of
 *           I/O to the chunk from each MPI rank which has the chunk selected and is used to
 *           determine the value for the previous `full_overwrite` flag.
 *
 * chunk_buf_size - The size in bytes of the data buffer allocated for the chunk
 *
 * orig_owner - The MPI rank which originally had this chunk selected at the beginning of
 *              the collective filtered I/O operation. This field is currently used when
 *              redistributing shared chunks among MPI ranks.
 *
 * new_owner - The MPI rank which has been selected to perform the modifications to this chunk.
 *
 * num_writers - The total number of MPI ranks writing to this chunk. This field is used when
 *               the new owner of a chunk is receiving messages from other MPI ranks that
 *               contain their selections in the chunk and the data to update the chunk with.
 *               The new owner must know how many MPI ranks it should expect messages from so
 *               that it can post an equal number of receive calls.
 *
 * buf - A pointer which serves the dual purpose of holding either the chunk data which is to be
 *       written to the file or the chunk data which has been read from the file.
 *
 * async_info - In the case of dataset writes only, this struct is used by the owning MPI rank of
 *              the chunk in order to manage the MPI send and receive calls made between it and all
 *              of the other MPI ranks which have this chunk selected in the I/O operation. Its
 *              fields are as follows:
 *
 *     receive_requests_array - An array containing one MPI_Request for each of the asynchronous
 *                              MPI receive calls the owning MPI rank of this chunk makes to another
 *                              MPI rank in order to receive that rank's chunk modification data and
 *                              selection in the chunk.
 *
 *     receive_buffer_array - An array of buffers into which the owning MPI rank of this chunk
 *                            will store chunk modification data and the selection in the chunk
 *                            received from another MPI rank.
 *
 *     num_receive_requests - The number of entries in the `receive_request_array` and
 *                            `receive_buffer_array` fields.
 */
typedef struct H5D_filtered_collective_io_info_t {
    H5D_chunk_index_info_t index_info;

    H5D_chunk_info_t *chunk_info;
    H5F_block_t       chunk_current;
    H5F_block_t       chunk_new;
    hbool_t           need_read;
    hbool_t           skip_filter_pline;
    size_t            io_size;
    size_t            chunk_buf_size;
    int               orig_owner;
    int               new_owner;
    int               num_writers;
    void *            buf;

    struct {
        MPI_Request *   receive_requests_array;
        unsigned char **receive_buffer_array;
        int             num_receive_requests;
    } async_info;
} H5D_filtered_collective_io_info_t;

/*
 * Information necessary for redistributing shared chunks during
 * a parallel write of a chunked dataset with filters applied.
 */
typedef struct H5D_chunk_redistribute_info_t {
    H5F_block_t chunk_block;
    hsize_t     chunk_idx;
    int         orig_owner;
    int         new_owner;
    int         num_writers;
} H5D_chunk_redistribute_info_t;

/*
 * Information used when re-inserting a chunk into a dataset's
 * chunk index during a parallel write of a chunked dataset with
 * filters applied.
 */
typedef struct H5D_chunk_insert_info_t {
    H5F_block_t            chunk_block;
    H5D_chunk_index_info_t index_info;
} H5D_chunk_insert_info_t;

/********************/
/* Local Prototypes */
/********************/
static herr_t H5D__chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                       H5D_chunk_map_t *fm);
static herr_t H5D__multi_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                             H5D_chunk_map_t *fm, int mpi_rank, int mpi_size);
static herr_t H5D__multi_chunk_filtered_collective_io(H5D_io_info_t *        io_info,
                                                      const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
                                                      int mpi_rank, int mpi_size);
static herr_t H5D__link_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                            H5D_chunk_map_t *fm, int sum_chunk, int mpi_rank, int mpi_size);
static herr_t H5D__link_chunk_filtered_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                                     H5D_chunk_map_t *fm, int mpi_rank, int mpi_size);
static herr_t H5D__inter_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                       const H5S_t *file_space, const H5S_t *mem_space);
static herr_t H5D__final_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                       hsize_t nelmts, MPI_Datatype mpi_file_type, MPI_Datatype mpi_buf_type);
static herr_t H5D__sort_chunk(H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
                              H5D_chunk_addr_info_t chunk_addr_info_array[], int many_chunk_opt, int mpi_rank,
                              int mpi_size);
static herr_t H5D__obtain_mpio_mode(H5D_io_info_t *io_info, H5D_chunk_map_t *fm, uint8_t assign_io_mode[],
                                    haddr_t chunk_addr[], int mpi_rank, int mpi_size);
static herr_t H5D__mpio_get_sum_chunk(const H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
                                      int *sum_chunkf);
static herr_t H5D__mpio_collective_filtered_chunk_io_setup(const H5D_io_info_t *               io_info,
                                                           const H5D_type_info_t *             type_info,
                                                           const H5D_chunk_map_t *             fm,
                                                           H5D_filtered_collective_io_info_t **chunk_list,
                                                           size_t *num_entries, int mpi_rank);
static herr_t H5D__mpio_redistribute_shared_chunks(H5D_filtered_collective_io_info_t *chunk_list,
                                                  size_t chunk_list_num_entries, const H5D_io_info_t *io_info,
                                                  const H5D_chunk_map_t *fm, int mpi_rank, int mpi_size,
                                                  size_t **rank_chunks_assigned_map);
static herr_t H5D__mpio_redistribute_shared_chunks_p0(H5D_filtered_collective_io_info_t *chunk_list,
                                                      size_t *num_chunks_assigned_map, const H5D_io_info_t *io_info,
                                                      const H5D_chunk_map_t *fm, int mpi_rank, int mpi_size);
static herr_t H5D__mpio_share_chunk_modification_data(H5D_filtered_collective_io_info_t *chunk_list,
                                                      size_t *chunk_list_num_entries, H5D_io_info_t *io_info,
                                                      const H5D_type_info_t *type_info, int mpi_rank,
                                                      int mpi_size);
static herr_t H5D__mpio_collective_filtered_chunk_common_io(H5D_filtered_collective_io_info_t *chunk_list,
                                                            size_t                 chunk_list_num_entries,
                                                            const H5D_io_info_t *  io_info,
                                                            const H5D_type_info_t *type_info);
static herr_t H5D__mpio_collective_filtered_chunk_read(H5D_filtered_collective_io_info_t *chunk_list,
                                                       size_t                 chunk_list_num_entries,
                                                       const H5D_io_info_t *  io_info,
                                                       const H5D_type_info_t *type_info, int mpi_rank);
static herr_t H5D__mpio_collective_filtered_chunk_update(H5D_filtered_collective_io_info_t *chunk_list,
                                                         size_t                 chunk_list_num_entries,
                                                         const H5D_io_info_t *  io_info,
                                                         const H5D_type_info_t *type_info, int mpi_rank);
static herr_t H5D__mpio_collective_filtered_chunk_reallocate(H5D_filtered_collective_io_info_t *chunk_list,
                                                             size_t              chunk_list_num_entries,
                                                             H5D_io_info_t *     io_info,
                                                             H5D_chk_idx_info_t *idx_info, int mpi_rank,
                                                             int mpi_size);
static herr_t H5D__mpio_collective_filtered_chunk_reinsert(H5D_filtered_collective_io_info_t *chunk_list,
                                                           size_t              chunk_list_num_entries,
                                                           H5D_io_info_t *     io_info,
                                                           H5D_chk_idx_info_t *idx_info, int mpi_rank,
                                                           int mpi_size);
static herr_t H5D__mpio_get_chunk_redistribute_info_types(MPI_Datatype *contig_type,
                                                          hbool_t *     contig_type_derived,
                                                          MPI_Datatype *resized_type,
                                                          hbool_t *     resized_type_derived);
static herr_t H5D__mpio_get_chunk_alloc_info_types(MPI_Datatype *contig_type, hbool_t *contig_type_derived,
                                                    MPI_Datatype *resized_type,
                                                    hbool_t *     resized_type_derived);
static herr_t H5D__mpio_get_chunk_insert_info_types(MPI_Datatype *contig_type, hbool_t *contig_type_derived,
                                                    MPI_Datatype *resized_type, hbool_t *resized_type_derived);
static herr_t H5D__mpio_collective_filtered_io_type(H5D_filtered_collective_io_info_t *chunk_list,
                                                    size_t num_entries, H5D_io_op_type_t op_type,
                                                    MPI_Datatype *new_mem_type, hbool_t *mem_type_derived,
                                                    MPI_Datatype *new_file_type, hbool_t *file_type_derived);
static int    H5D__cmp_chunk_addr(const void *chunk_addr_info1, const void *chunk_addr_info2);
static int    H5D__cmp_filtered_collective_io_info_entry(const void *filtered_collective_io_info_entry1,
                                                         const void *filtered_collective_io_info_entry2);
static int H5D__cmp_chunk_redistribute_info(const void *entry1, const void *entry2);
static int H5D__cmp_chunk_redistribute_info_orig_owner(const void *entry1, const void *entry2);

#ifdef H5Dmpio_DEBUG
static herr_t H5D__mpio_debug_init(void);
static herr_t H5D__mpio_dump_collective_filtered_chunk_list(H5D_filtered_collective_io_info_t *chunk_list,
                                                            size_t chunk_list_num_entries, int mpi_rank);
#endif

/*********************/
/* Package Variables */
/*********************/

/*******************/
/* Local Variables */
/*******************/

/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

#ifdef H5Dmpio_DEBUG

/* Flags to control debug actions in this file.
 * (Meant to be indexed by characters)
 *
 * These flags can be set with either (or both) the environment variable
 *      "H5D_mpio_Debug" set to a string containing one or more characters
 *      (flags) or by setting them as a string value for the
 *      "H5D_mpio_debug_key" MPI Info key.
 *
 * Supported characters in 'H5D_mpio_Debug' string:
 *      't' trace function entry and exit
 *      'f' log to file rather than debugging stream
 *      'm' show (rough) memory usage statistics
 *      'c' show critical timing information
 *
 *      To only show output from a particular MPI rank, specify its rank
 *      number as a character, e.g.:
 *
 *      '0' only show output from rank 0
 *
 *      To only show output from a particular range (up to 8 ranks supported
 *      between 0-9) of MPI ranks, specify the start and end ranks separated
 *      by a hyphen, e.g.:
 *
 *      '0-7' only show output from ranks 0 through 7
 *
 */
static int               H5D_mpio_debug_flags_s[256];
static int               H5D_mpio_debug_rank_s[8] = {-1, -1, -1, -1, -1, -1, -1, -1};
static hbool_t           H5D_mpio_debug_inited    = FALSE;
static const char *const trace_in_pre             = "-> ";
static const char *const trace_out_pre            = "<- ";
static int               debug_indent             = 0;
static FILE *            debug_stream             = NULL;

/* Determine if this rank should output debugging info */
#define H5D_MPIO_DEBUG_THIS_RANK(rank)                                                                       \
    (H5D_mpio_debug_rank_s[0] < 0 || rank == H5D_mpio_debug_rank_s[0] || rank == H5D_mpio_debug_rank_s[1] || \
     rank == H5D_mpio_debug_rank_s[2] || rank == H5D_mpio_debug_rank_s[3] ||                                 \
     rank == H5D_mpio_debug_rank_s[4] || rank == H5D_mpio_debug_rank_s[5] ||                                 \
     rank == H5D_mpio_debug_rank_s[6] || rank == H5D_mpio_debug_rank_s[7])

/* Print some debugging string */
#define H5D_MPIO_DEBUG(rank, string)                                                                         \
    do {                                                                                                     \
        if (debug_stream && H5D_MPIO_DEBUG_THIS_RANK(rank)) {                                                \
            HDfprintf(debug_stream, "%*s(Rank %d) " string "\n", debug_indent, "", rank);                    \
            fflush(debug_stream);                                                                            \
        }                                                                                                    \
    } while (0)

/* Print some debugging string with printf-style arguments */
#define H5D_MPIO_DEBUG_VA(rank, string, ...)                                                                 \
    do {                                                                                                     \
        if (debug_stream && H5D_MPIO_DEBUG_THIS_RANK(rank)) {                                                \
            HDfprintf(debug_stream, "%*s(Rank %d) " string "\n", debug_indent, "", rank, __VA_ARGS__);       \
            fflush(debug_stream);                                                                            \
        }                                                                                                    \
    } while (0)

#define H5D_MPIO_TRACE_ENTER(rank)                                                                           \
    do {                                                                                                     \
        hbool_t trace_flag = H5D_mpio_debug_flags_s[(int)'t'];                                               \
                                                                                                             \
        if (trace_flag) {                                                                                    \
            H5D_MPIO_DEBUG_VA(rank, "%s%s", trace_in_pre, __func__);                                         \
            debug_indent += (int)strlen(trace_in_pre);                                                       \
        }                                                                                                    \
    } while (0)

#define H5D_MPIO_TRACE_EXIT(rank)                                                                            \
    do {                                                                                                     \
        hbool_t trace_flag = H5D_mpio_debug_flags_s[(int)'t'];                                               \
                                                                                                             \
        if (trace_flag) {                                                                                    \
            debug_indent -= (int)strlen(trace_out_pre);                                                      \
            H5D_MPIO_DEBUG_VA(rank, "%s%s", trace_out_pre, __func__);                                        \
        }                                                                                                    \
    } while (0)

#define H5D_MPIO_TIME_START(rank, op_name)                                                                   \
    {                                                                                                        \
        hbool_t           time_flag  = H5D_mpio_debug_flags_s[(int)'c'];                                     \
        double            start_time = 0.0, end_time = 0.0;                                                  \
        const char *const op = op_name;                                                                      \
                                                                                                             \
        if (time_flag) {                                                                                     \
            start_time = MPI_Wtime();                                                                        \
        }

#define H5D_MPIO_TIME_STOP(rank)                                                                             \
    if (time_flag) {                                                                                         \
        end_time = MPI_Wtime();                                                                              \
        H5D_MPIO_DEBUG_VA(rank, "'%s' took %f seconds", op, (end_time - start_time));                        \
    }                                                                                                        \
    }

/*---------------------------------------------------------------------------
 * Function:    H5D__mpio_parse_debug_str
 *
 * Purpose:     Parse a string for H5Dmpio-related debugging flags
 *
 * Returns:     N/A
 *
 *---------------------------------------------------------------------------
 */
static void
H5D__mpio_parse_debug_str(const char *s)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(s);

    while (*s) {
        int c = (int)(*s);

        if (c >= (int)'0' && c <= (int)'9') {
            hbool_t range = FALSE;

            if (*(s + 1) && *(s + 2))
                range = (int)*(s + 1) == '-' && (int)*(s + 2) >= (int)'0' && (int)*(s + 2) <= (int)'9';

            if (range) {
                int start_rank = c - (int)'0';
                int end_rank   = (int)*(s + 2) - '0';
                int num_ranks  = end_rank - start_rank + 1;
                int i;

                if (num_ranks > 8) {
                    end_rank  = start_rank + 7;
                    num_ranks = 8;
                }

                for (i = 0; i < num_ranks; i++)
                    H5D_mpio_debug_rank_s[i] = start_rank++;

                s += 3;
            }
            else
                H5D_mpio_debug_rank_s[0] = c - (int)'0';
        }
        else
            H5D_mpio_debug_flags_s[c]++;

        s++;
    }

    FUNC_LEAVE_NOAPI_VOID
}

static herr_t
H5D__mpio_debug_init(void)
{
    const char *debug_str;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_STATIC_NOERR

    HDassert(!H5D_mpio_debug_inited);

    /* Clear the debug flag buffer */
    HDmemset(H5D_mpio_debug_flags_s, 0, sizeof(H5D_mpio_debug_flags_s));

    /* Retrieve and parse the H5Dmpio debug string */
    debug_str = HDgetenv("H5D_mpio_Debug");
    if (debug_str)
        H5D__mpio_parse_debug_str(debug_str);

    if (H5DEBUG(D))
        debug_stream = H5DEBUG(D);

    H5D_mpio_debug_inited = TRUE;

    FUNC_LEAVE_NOAPI(ret_value)
}

#endif

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_opt_possible
 *
 * Purpose:     Checks if an direct I/O transfer is possible between memory and
 *                  the file.
 *
 * Return:      Success:   Non-negative: TRUE or FALSE
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, April 3, 2002
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5D__mpio_opt_possible(const H5D_io_info_t *io_info, const H5S_t *file_space, const H5S_t *mem_space,
                       const H5D_type_info_t *type_info)
{
    H5FD_mpio_xfer_t io_xfer_mode;            /* MPI I/O transfer mode */
    unsigned         local_cause[2] = {0, 0}; /* [0] Local reason(s) for breaking collective mode */
                                              /* [1] Flag if dataset is both: H5S_ALL and small */
    unsigned global_cause[2] = {0, 0};        /* Global reason(s) for breaking collective mode */
    htri_t   is_vl_storage;       /* Whether the dataset's datatype is stored in a variable-length form */
    htri_t   ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(io_info);
    HDassert(mem_space);
    HDassert(file_space);
    HDassert(type_info);

    /* For independent I/O, get out quickly and don't try to form consensus */
    if (H5CX_get_io_xfer_mode(&io_xfer_mode) < 0)
        /* Set error flag, but keep going */
        local_cause[0] |= H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE;
    if (io_xfer_mode == H5FD_MPIO_INDEPENDENT)
        local_cause[0] |= H5D_MPIO_SET_INDEPENDENT;

    /* Optimized MPI types flag must be set */
    /* (based on 'HDF5_MPI_OPT_TYPES' environment variable) */
    if (!H5FD_mpi_opt_types_g)
        local_cause[0] |= H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED;

    /* Don't allow collective operations if datatype conversions need to happen */
    if (!type_info->is_conv_noop)
        local_cause[0] |= H5D_MPIO_DATATYPE_CONVERSION;

    /* Don't allow collective operations if data transform operations should occur */
    if (!type_info->is_xform_noop)
        local_cause[0] |= H5D_MPIO_DATA_TRANSFORMS;

    /* Check whether these are both simple or scalar dataspaces */
    if (!((H5S_SIMPLE == H5S_GET_EXTENT_TYPE(mem_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(mem_space)) &&
          (H5S_SIMPLE == H5S_GET_EXTENT_TYPE(file_space) || H5S_SCALAR == H5S_GET_EXTENT_TYPE(file_space))))
        local_cause[0] |= H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES;

    /* Dataset storage must be contiguous or chunked */
    if (!(io_info->dset->shared->layout.type == H5D_CONTIGUOUS ||
          io_info->dset->shared->layout.type == H5D_CHUNKED))
        local_cause[0] |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

    /* check if external-file storage is used */
    if (io_info->dset->shared->dcpl_cache.efl.nused > 0)
        local_cause[0] |= H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET;

        /* The handling of memory space is different for chunking and contiguous
         *  storage.  For contiguous storage, mem_space and file_space won't change
         *  when it it is doing disk IO.  For chunking storage, mem_space will
         *  change for different chunks. So for chunking storage, whether we can
         *  use collective IO will defer until each chunk IO is reached.
         */

#ifndef H5_HAVE_PARALLEL_FILTERED_WRITES
    /* Don't allow writes to filtered datasets if the functionality is disabled */
    if (io_info->op_type == H5D_IO_OP_WRITE && io_info->dset->shared->dcpl_cache.pline.nused > 0)
        local_cause[0] |= H5D_MPIO_PARALLEL_FILTERED_WRITES_DISABLED;
#endif

    /* Check if we are able to do a MPI_Bcast of the data from one rank
     * instead of having all the processes involved in the collective I/O call.
     */

    /* Check to see if the process is reading the entire dataset */
    if (H5S_GET_SELECT_TYPE(file_space) != H5S_SEL_ALL)
        local_cause[1] |= H5D_MPIO_RANK0_NOT_H5S_ALL;
    /* Only perform this optimization for contiguous datasets, currently */
    else if (H5D_CONTIGUOUS != io_info->dset->shared->layout.type)
        /* Flag to do a MPI_Bcast of the data from one proc instead of
         * having all the processes involved in the collective I/O.
         */
        local_cause[1] |= H5D_MPIO_RANK0_NOT_CONTIGUOUS;
    else if ((is_vl_storage = H5T_is_vl_storage(type_info->dset_type)) < 0)
        local_cause[0] |= H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE;
    else if (is_vl_storage)
        local_cause[1] |= H5D_MPIO_RANK0_NOT_FIXED_SIZE;
    else {
        size_t type_size; /* Size of dataset's datatype */

        /* Retrieve the size of the dataset's datatype */
        if (0 == (type_size = H5T_GET_SIZE(type_info->dset_type)))
            local_cause[0] |= H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE;
        else {
            hssize_t snelmts; /* [Signed] # of elements in dataset's dataspace */

            /* Retrieve the size of the dataset's datatype */
            if ((snelmts = H5S_GET_EXTENT_NPOINTS(file_space)) < 0)
                local_cause[0] |= H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE;
            else {
                hsize_t dset_size;

                /* Determine dataset size */
                dset_size = ((hsize_t)snelmts) * type_size;

                /* If the size of the dataset is less than 2GB then do an MPI_Bcast
                 * of the data from one process instead of having all the processes
                 * involved in the collective I/O.
                 */
                if (dset_size > ((hsize_t)(2.0F * H5_GB) - 1))
                    local_cause[1] |= H5D_MPIO_RANK0_GREATER_THAN_2GB;
            } /* end else */
        }     /* end else */
    }         /* end else */

    /* Check for independent I/O */
    if (local_cause[0] & H5D_MPIO_SET_INDEPENDENT)
        global_cause[0] = local_cause[0];
    else {
        int mpi_code; /* MPI error code */

        /* Form consensus opinion among all processes about whether to perform
         * collective I/O
         */
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allreduce(local_cause, global_cause, 2, MPI_UNSIGNED, MPI_BOR, io_info->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)
    } /* end else */

    /* Set the local & global values of no-collective-cause in the API context */
    H5CX_set_mpio_local_no_coll_cause(local_cause[0]);
    H5CX_set_mpio_global_no_coll_cause(global_cause[0]);

    /* Set read-with-rank0-and-bcast flag if possible */
    if (global_cause[0] == 0 && global_cause[1] == 0) {
        H5CX_set_mpio_rank0_bcast(TRUE);
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
        H5CX_test_set_mpio_coll_rank0_bcast(TRUE);
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */
    }  /* end if */

    /* Set the return value, based on the global cause */
    ret_value = global_cause[0] > 0 ? FALSE : TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_opt_possible() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_no_coll_cause_strings
 *
 * Purpose:     When collective I/O is broken internally, it can be useful
 *              for users to see a representative string for the reason(s)
 *              why it was broken. This routine inspects the current
 *              "cause" flags from the API context and prints strings into
 *              the caller's buffers for the local and global reasons that
 *              collective I/O was broken.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_get_no_coll_cause_strings(char *local_cause, size_t local_cause_len, char *global_cause,
                                    size_t global_cause_len)
{
    H5D_mpio_no_collective_cause_t cause;
    uint32_t                       local_no_coll_cause;
    uint32_t                       global_no_coll_cause;
    size_t                         local_cause_bytes_written  = 0;
    size_t                         global_cause_bytes_written = 0;
    size_t                         idx;
    size_t                         num_cause_strings;
    herr_t                         ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    HDassert((local_cause && local_cause_len > 0) || (global_cause && global_cause_len > 0));

    /* Ensure things are in order for our cause string table */
    HDcompile_assert(H5D_MPIO_SET_INDEPENDENT == (H5D_mpio_no_collective_cause_t)1);
    HDcompile_assert(H5D_MPIO_DATATYPE_CONVERSION == (H5D_mpio_no_collective_cause_t)2);
    HDcompile_assert(H5D_MPIO_DATA_TRANSFORMS == (H5D_mpio_no_collective_cause_t)4);
    HDcompile_assert(H5D_MPIO_MPI_OPT_TYPES_ENV_VAR_DISABLED == (H5D_mpio_no_collective_cause_t)8);
    HDcompile_assert(H5D_MPIO_NOT_SIMPLE_OR_SCALAR_DATASPACES == (H5D_mpio_no_collective_cause_t)16);
    HDcompile_assert(H5D_MPIO_NOT_CONTIGUOUS_OR_CHUNKED_DATASET == (H5D_mpio_no_collective_cause_t)32);
    HDcompile_assert(H5D_MPIO_PARALLEL_FILTERED_WRITES_DISABLED == (H5D_mpio_no_collective_cause_t)64);
    HDcompile_assert(H5D_MPIO_ERROR_WHILE_CHECKING_COLLECTIVE_POSSIBLE ==
                     (H5D_mpio_no_collective_cause_t)128);
    HDcompile_assert(H5D_MPIO_NO_COLLECTIVE_MAX_CAUSE == (H5D_mpio_no_collective_cause_t)256);

    /* Initialize output buffers */
    if (local_cause && local_cause_len > 0)
        *local_cause = '\0';
    if (global_cause && global_cause_len > 0)
        *global_cause = '\0';

    /* Retrieve the local and global cause flags from the API context */
    if (H5CX_get_mpio_local_no_coll_cause(&local_no_coll_cause) < 0)
        HGOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL, "unable to get local no collective cause value")
    if (H5CX_get_mpio_global_no_coll_cause(&global_no_coll_cause) < 0)
        HGOTO_ERROR(H5E_CONTEXT, H5E_CANTGET, FAIL, "unable to get global no collective cause value")

    {
        const char *cause_strings[8] = {"independent I/O was requested",
                                        "datatype conversions were required",
                                        "data transforms needed to be applied",
                                        "optimized MPI types flag wasn't set",
                                        "one of the dataspaces was neither simple nor scalar",
                                        "dataset was not contiguous or chunked",
                                        "parallel writes to filtered datasets are disabled",
                                        "an error occurred while checking if collective I/O was possible"};

        num_cause_strings = sizeof(cause_strings) / sizeof(cause_strings[0]);

        /*
         * Append each of the "reason for breaking collective I/O" error messages
         * to the local and global cause string buffers
         */
        for (cause = H5D_MPIO_SET_INDEPENDENT, idx = 0;
             (cause < H5D_MPIO_NO_COLLECTIVE_MAX_CAUSE) && (idx < num_cause_strings); cause <<= 1, idx++) {
            size_t buf_space_left;

            if (local_cause && (cause & local_no_coll_cause)) {
                buf_space_left = local_cause_len - local_cause_bytes_written;

                /*
                 * Check if there were any previous error messages included. If
                 * so, prepend a semicolon to separate the messages.
                 */
                if (buf_space_left && local_cause_bytes_written) {
                    HDstrncat(local_cause, "; ", buf_space_left);
                    local_cause_bytes_written += MIN(buf_space_left, 2);
                    buf_space_left -= MIN(buf_space_left, 2);
                }

                if (buf_space_left) {
                    HDstrncat(local_cause, cause_strings[idx], buf_space_left);
                    local_cause_bytes_written += MIN(buf_space_left, HDstrlen(cause_strings[idx]));
                }
            }

            if (global_cause && (cause & global_no_coll_cause)) {
                buf_space_left = global_cause_len - global_cause_bytes_written;

                /*
                 * Check if there were any previous error messages included. If
                 * so, prepend a semicolon to separate the messages.
                 */
                if (buf_space_left && global_cause_bytes_written) {
                    HDstrncat(global_cause, "; ", buf_space_left);
                    global_cause_bytes_written += MIN(buf_space_left, 2);
                    buf_space_left -= MIN(buf_space_left, 2);
                }

                if (buf_space_left) {
                    HDstrncat(global_cause, cause_strings[idx], buf_space_left);
                    global_cause_bytes_written += MIN(buf_space_left, HDstrlen(cause_strings[idx]));
                }
            }
        }
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_no_coll_cause_strings() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_read
 *
 * Purpose:     MPI-IO function to read directly from app buffer to file.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Programmer:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_read(const H5D_io_info_t *io_info, const H5D_type_info_t H5_ATTR_UNUSED *type_info,
                      hsize_t mpi_buf_count, H5S_t H5_ATTR_UNUSED *file_space,
                      H5S_t H5_ATTR_UNUSED *mem_space)
{
    const H5D_contig_storage_t *store_contig =
        &(io_info->store->contig); /* Contiguous storage info for this I/O operation */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if (H5F_shared_block_read(io_info->f_sh, H5FD_MEM_DRAW, store_contig->dset_addr, (size_t)mpi_buf_count,
                              io_info->u.rbuf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "can't finish collective parallel read")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_select_write
 *
 * Purpose:     MPI-IO function to write directly from app buffer to file.
 *
 * Return:      non-negative on success, negative on failure.
 *
 * Programmer:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__mpio_select_write(const H5D_io_info_t *io_info, const H5D_type_info_t H5_ATTR_UNUSED *type_info,
                       hsize_t mpi_buf_count, H5S_t H5_ATTR_UNUSED *file_space,
                       H5S_t H5_ATTR_UNUSED *mem_space)
{
    const H5D_contig_storage_t *store_contig =
        &(io_info->store->contig); /* Contiguous storage info for this I/O operation */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE

    /*OKAY: CAST DISCARDS CONST QUALIFIER*/
    H5_CHECK_OVERFLOW(mpi_buf_count, hsize_t, size_t);
    if (H5F_shared_block_write(io_info->f_sh, H5FD_MEM_DRAW, store_contig->dset_addr, (size_t)mpi_buf_count,
                               io_info->u.wbuf) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't finish collective parallel write")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_select_write() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_sum_chunk
 *
 * Purpose:     Routine for obtaining total number of chunks to cover
 *              hyperslab selection selected by all processors.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_sum_chunk(const H5D_io_info_t *io_info, const H5D_chunk_map_t *fm, int *sum_chunkf)
{
    int    num_chunkf; /* Number of chunks to iterate over */
    size_t ori_num_chunkf;
    int    mpi_code; /* MPI return code */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Get the number of chunks to perform I/O on */
    num_chunkf     = 0;
    ori_num_chunkf = H5SL_count(fm->sel_chunks);
    H5_CHECKED_ASSIGN(num_chunkf, int, ori_num_chunkf, size_t);

    /* Determine the summation of number of chunks for all processes */
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Allreduce(&num_chunkf, sum_chunkf, 1, MPI_INT, MPI_SUM, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_sum_chunk() */

/*-------------------------------------------------------------------------
 * Function:    H5D__contig_collective_read
 *
 * Purpose:     Reads directly from contiguous data in file into application
 *              memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__contig_collective_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                            hsize_t H5_ATTR_UNUSED nelmts, H5S_t *file_space, H5S_t *mem_space,
                            H5D_chunk_map_t H5_ATTR_UNUSED *fm)
{
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
    herr_t                    ret_value      = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(H5FD_MPIO == H5F_DRIVER_ID(io_info->dset->oloc.file));

    /* Call generic internal collective I/O routine */
    if (H5D__inter_collective_io(io_info, type_info, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't finish shared collective MPI-IO")

    /* Set the actual I/O mode property. internal_collective_io will not break to
     * independent I/O, so we set it here.
     */
    H5CX_set_mpio_actual_io_mode(actual_io_mode);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__contig_collective_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__contig_collective_write
 *
 * Purpose:     Write directly to contiguous data in file from application
 *              memory using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__contig_collective_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                             hsize_t H5_ATTR_UNUSED nelmts, H5S_t *file_space, H5S_t *mem_space,
                             H5D_chunk_map_t H5_ATTR_UNUSED *fm)
{
    H5D_mpio_actual_io_mode_t actual_io_mode = H5D_MPIO_CONTIGUOUS_COLLECTIVE;
    herr_t                    ret_value      = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(H5FD_MPIO == H5F_DRIVER_ID(io_info->dset->oloc.file));

    /* Call generic internal collective I/O routine */
    if (H5D__inter_collective_io(io_info, type_info, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't finish shared collective MPI-IO")

    /* Set the actual I/O mode property. internal_collective_io will not break to
     * independent I/O, so we set it here.
     */
    H5CX_set_mpio_actual_io_mode(actual_io_mode);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__contig_collective_write() */

/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_io
 *
 * Purpose:     Routine for
 *              1) choose an IO option:
 *                    a) One collective IO defined by one MPI derived datatype to link through all chunks
 *              or    b) multiple chunk IOs,to do MPI-IO for each chunk, the IO mode may be adjusted
 *                       due to the selection pattern for each chunk.
 *              For option a)
 *                      1. Sort the chunk address, obtain chunk info according to the sorted chunk address
 *                      2. Build up MPI derived datatype for each chunk
 *                      3. Build up the final MPI derived datatype
 *                      4. Set up collective IO property list
 *                      5. Do IO
 *              For option b)
 *                      1. Use MPI_gather and MPI_Bcast to obtain information of *collective/independent/none*
 *                         IO mode for each chunk of the selection
 *                      2. Depending on whether the IO mode is collective or independent or none,
 *                         Create either MPI derived datatype for each chunk to do collective IO or
 *                         just do independent IO or independent IO with file set view
 *                      3. Set up collective IO property list for collective mode
 *                      4. DO IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 * Modification:
 *  - Refctore to remove multi-chunk-without-opimization feature and update for
 *    multi-chunk-io accordingly
 * Programmer: Jonathan Kim
 * Date: 2012-10-10
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info, H5D_chunk_map_t *fm)
{
    H5FD_mpio_chunk_opt_t chunk_opt_mode;
#ifdef H5Dmpio_DEBUG
    hbool_t log_file_flag  = FALSE;
    FILE *  debug_log_file = NULL;
#endif
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    htri_t temp_not_link_io = FALSE;
#endif
    int    io_option = H5D_MULTI_CHUNK_IO_MORE_OPT;
    int    sum_chunk = -1;
    int    mpi_rank;
    int    mpi_size;
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(io_info);
    HDassert(io_info->using_mpi_vfd);
    HDassert(type_info);
    HDassert(fm);

    /* Obtain the current rank of the process and the number of ranks */
    if ((mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain MPI rank")
    if ((mpi_size = H5F_mpi_get_size(io_info->dset->oloc.file)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain MPI size")

#ifdef H5Dmpio_DEBUG
    /* Initialize file-level debugging if not initialized */
    if (!H5D_mpio_debug_inited && H5D__mpio_debug_init() < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize H5Dmpio debugging")

    /* Open file for debugging if necessary */
    log_file_flag = H5D_mpio_debug_flags_s[(int)'f'];
    if (log_file_flag) {
        char   debug_log_filename[1024];
        time_t time_now;

        HDsnprintf(debug_log_filename, 1024, "H5Dmpio_debug.rank%d", mpi_rank);

        if (NULL == (debug_log_file = HDfopen(debug_log_filename, "a")))
            HGOTO_ERROR(H5E_IO, H5E_OPENERROR, FAIL, "couldn't open debugging log file")

        /* Print a short header for this I/O operation */
        time_now = time(NULL);
        HDfprintf(debug_log_file, "##### %s", asctime(localtime(&time_now)));

        debug_stream = debug_log_file;
    }
#endif

    /* Check the optional property list for the collective chunk IO optimization option */
    if (H5CX_get_mpio_chunk_opt_mode(&chunk_opt_mode) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "couldn't get chunk optimization option")

    if (H5FD_MPIO_CHUNK_ONE_IO == chunk_opt_mode)
        io_option = H5D_ONE_LINK_CHUNK_IO; /*no opt*/
    /* direct request to multi-chunk-io */
    else if (H5FD_MPIO_CHUNK_MULTI_IO == chunk_opt_mode)
        io_option = H5D_MULTI_CHUNK_IO;
    /* via default path. branch by num threshold */
    else {
        unsigned one_link_chunk_io_threshold; /* Threshold to use single collective I/O for all chunks */

        if (H5D__mpio_get_sum_chunk(io_info, fm, &sum_chunk) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL,
                        "unable to obtain the total chunk number of all processes");

        /* Get the chunk optimization option threshold */
        if (H5CX_get_mpio_chunk_opt_num(&one_link_chunk_io_threshold) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                        "couldn't get chunk optimization option threshold value")

        /* step 1: choose an IO option */
        /* If the average number of chunk per process is greater than a threshold, we will do one link chunked
         * IO. */
        if ((unsigned)sum_chunk / (unsigned)mpi_size >= one_link_chunk_io_threshold)
            io_option = H5D_ONE_LINK_CHUNK_IO_MORE_OPT;
#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
        else
            temp_not_link_io = TRUE;
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */
    }  /* end else */

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    {
        /*** Set collective chunk user-input optimization APIs. ***/
        if (H5D_ONE_LINK_CHUNK_IO == io_option) {
            if (H5CX_test_set_mpio_coll_chunk_link_hard(0) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
        else if (H5D_MULTI_CHUNK_IO == io_option) {
            if (H5CX_test_set_mpio_coll_chunk_multi_hard(0) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end else-if */
        else if (H5D_ONE_LINK_CHUNK_IO_MORE_OPT == io_option) {
            if (H5CX_test_set_mpio_coll_chunk_link_num_true(0) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
        else if (temp_not_link_io) {
            if (H5CX_test_set_mpio_coll_chunk_link_num_false(0) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
        } /* end if */
    }
#endif /* H5_HAVE_INSTRUMENTED_LIBRARY */

    /* step 2:  Go ahead to do IO.*/
    switch (io_option) {
        case H5D_ONE_LINK_CHUNK_IO:
        case H5D_ONE_LINK_CHUNK_IO_MORE_OPT:
            /* Check if there are any filters in the pipeline */
            if (io_info->dset->shared->dcpl_cache.pline.nused > 0) {
                if (H5D__link_chunk_filtered_collective_io(io_info, type_info, fm, mpi_rank, mpi_size) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish filtered linked chunk MPI-IO")
            } /* end if */
            else
                /* Perform unfiltered link chunk collective IO */
                if (H5D__link_chunk_collective_io(io_info, type_info, fm, sum_chunk, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish linked chunk MPI-IO")
            break;

        case H5D_MULTI_CHUNK_IO: /* direct request to do multi-chunk IO */
        default:                 /* multiple chunk IO via threshold */
            /* Check if there are any filters in the pipeline */
            if (io_info->dset->shared->dcpl_cache.pline.nused > 0) {
                if (H5D__multi_chunk_filtered_collective_io(io_info, type_info, fm, mpi_rank, mpi_size) < 0)
                    HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL,
                                "couldn't finish optimized multiple filtered chunk MPI-IO")
            } /* end if */
            else
                /* Perform unfiltered multi chunk collective IO */
                if (H5D__multi_chunk_collective_io(io_info, type_info, fm, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish optimized multiple chunk MPI-IO")
            break;
    } /* end switch */

done:
#ifdef H5Dmpio_DEBUG
    /* Close debugging log file */
    if (debug_log_file) {
        HDfprintf(debug_log_file, "##############\n\n");
        if (EOF == HDfclose(debug_log_file))
            HDONE_ERROR(H5E_IO, H5E_CLOSEERROR, FAIL, "couldn't close debugging log file")
        debug_stream = H5DEBUG(D);
    }
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_io */

/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_read
 *
 * Purpose:     Reads directly from chunks in file into application memory
 *              using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_collective_read(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                           hsize_t H5_ATTR_UNUSED nelmts, H5S_t H5_ATTR_UNUSED *file_space,
                           H5S_t H5_ATTR_UNUSED *mem_space, H5D_chunk_map_t *fm)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if (H5D__chunk_collective_io(io_info, type_info, fm) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__chunk_collective_write
 *
 * Purpose:     Write directly to chunks in file from application memory
 *              using collective I/O.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  4, 2008
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__chunk_collective_write(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                            hsize_t H5_ATTR_UNUSED nelmts, H5S_t H5_ATTR_UNUSED *file_space,
                            H5S_t H5_ATTR_UNUSED *mem_space, H5D_chunk_map_t *fm)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    if (H5D__chunk_collective_io(io_info, type_info, fm) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__chunk_collective_write() */

/*-------------------------------------------------------------------------
 * Function:    H5D__link_chunk_collective_io
 *
 * Purpose:     Routine for one collective IO with one MPI derived datatype to link with all chunks
 *
 *                      1. Sort the chunk address and chunk info
 *                      2. Build up MPI derived datatype for each chunk
 *                      3. Build up the final MPI derived datatype
 *                      4. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__link_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
                              int sum_chunk, int mpi_rank, int mpi_size)
{
    H5D_chunk_addr_info_t *chunk_addr_info_array = NULL;
    MPI_Datatype           chunk_final_mtype; /* Final memory MPI datatype for all chunks with selection */
    hbool_t                chunk_final_mtype_is_derived = FALSE;
    MPI_Datatype           chunk_final_ftype; /* Final file MPI datatype for all chunks with selection */
    hbool_t                chunk_final_ftype_is_derived = FALSE;
    H5D_storage_t          ctg_store; /* Storage info for "fake" contiguous dataset */
    size_t                 total_chunks;
    MPI_Datatype *         chunk_mtype          = NULL;
    MPI_Datatype *         chunk_ftype          = NULL;
    MPI_Aint *             chunk_disp_array     = NULL;
    MPI_Aint *             chunk_mem_disp_array = NULL;
    hbool_t *              chunk_mft_is_derived_array =
        NULL; /* Flags to indicate each chunk's MPI file datatype is derived */
    hbool_t *chunk_mbt_is_derived_array =
        NULL;                            /* Flags to indicate each chunk's MPI memory datatype is derived */
    int *  chunk_mpi_file_counts = NULL; /* Count of MPI file datatype for each chunk */
    int *  chunk_mpi_mem_counts  = NULL; /* Count of MPI memory datatype for each chunk */
    int    mpi_code;                     /* MPI return code */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Set the actual-chunk-opt-mode property. */
    H5CX_set_mpio_actual_chunk_opt(H5D_MPIO_LINK_CHUNK);

    /* Set the actual-io-mode property.
     * Link chunk I/O does not break to independent, so can set right away */
    H5CX_set_mpio_actual_io_mode(H5D_MPIO_CHUNK_COLLECTIVE);

    /* Get the sum # of chunks, if not already available */
    if (sum_chunk < 0) {
        if (H5D__mpio_get_sum_chunk(io_info, fm, &sum_chunk) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL,
                        "unable to obtain the total chunk number of all processes");
    } /* end if */

    /* Retrieve total # of chunks in dataset */
    H5_CHECKED_ASSIGN(total_chunks, size_t, fm->layout->u.chunk.nchunks, hsize_t);

    /* Handle special case when dataspace dimensions only allow one chunk in
     *  the dataset.  [This sometimes is used by developers who want the
     *  equivalent of compressed contiguous datasets - QAK]
     */
    if (total_chunks == 1) {
        H5SL_node_t *chunk_node; /* Pointer to chunk node for selection */
        H5S_t *      fspace;     /* Dataspace describing chunk & selection in it */
        H5S_t *      mspace;     /* Dataspace describing selection in memory corresponding to this chunk */

        /* Check for this process having selection in this chunk */
        chunk_node = H5SL_first(fm->sel_chunks);

        if (chunk_node == NULL) {
            /* Set the dataspace info for I/O to NULL, this process doesn't have any I/O to perform */
            fspace = mspace = NULL;

            /* Initialize chunk address */
            ctg_store.contig.dset_addr = 0;
        } /* end if */
        else {
            H5D_chunk_ud_t    udata;      /* User data for querying chunk info */
            H5D_chunk_info_t *chunk_info; /* Info for chunk in skiplist */

            /* Get the chunk info, for the selection in the chunk */
            if (NULL == (chunk_info = (H5D_chunk_info_t *)H5SL_item(chunk_node)))
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk info from skip list")

            /* Set the dataspace info for I/O */
            fspace = chunk_info->fspace;
            mspace = chunk_info->mspace;

            /* Look up address of chunk */
            if (H5D__chunk_lookup(io_info->dset, chunk_info->scaled, &udata) < 0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk address")
            ctg_store.contig.dset_addr = udata.chunk_block.offset;
        } /* end else */

        /* Set up the base storage address for this chunk */
        io_info->store = &ctg_store;

#ifdef H5Dmpio_DEBUG
        H5D_MPIO_DEBUG(mpi_rank, "before inter_collective_io for total chunk = 1");
#endif

        /* Perform I/O */
        if (H5D__inter_collective_io(io_info, type_info, fspace, mspace) < 0)
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
    } /* end if */
    else {
        hsize_t mpi_buf_count; /* Number of MPI types */
        size_t  num_chunk;     /* Number of chunks for this process */
        size_t  u;             /* Local index variable */

        /* Get the number of chunks with a selection */
        num_chunk = H5SL_count(fm->sel_chunks);
        H5_CHECK_OVERFLOW(num_chunk, size_t, int);

#ifdef H5Dmpio_DEBUG
        H5D_MPIO_DEBUG_VA(mpi_rank, "total_chunks = %zu, num_chunk = %zu", total_chunks, num_chunk);
#endif

        /* Set up MPI datatype for chunks selected */
        if (num_chunk) {
            /* Allocate chunking information */
            if (NULL == (chunk_addr_info_array =
                             (H5D_chunk_addr_info_t *)H5MM_malloc(num_chunk * sizeof(H5D_chunk_addr_info_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk array buffer")
            if (NULL == (chunk_mtype = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk memory datatype buffer")
            if (NULL == (chunk_ftype = (MPI_Datatype *)H5MM_malloc(num_chunk * sizeof(MPI_Datatype))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file datatype buffer")
            if (NULL == (chunk_disp_array = (MPI_Aint *)H5MM_malloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk file displacement buffer")
            if (NULL == (chunk_mem_disp_array = (MPI_Aint *)H5MM_calloc(num_chunk * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk memory displacement buffer")
            if (NULL == (chunk_mpi_mem_counts = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk memory counts buffer")
            if (NULL == (chunk_mpi_file_counts = (int *)H5MM_calloc(num_chunk * sizeof(int))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk file counts buffer")
            if (NULL == (chunk_mbt_is_derived_array = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk memory is derived datatype flags buffer")
            if (NULL == (chunk_mft_is_derived_array = (hbool_t *)H5MM_calloc(num_chunk * sizeof(hbool_t))))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk file is derived datatype flags buffer")

#ifdef H5Dmpio_DEBUG
            H5D_MPIO_DEBUG(mpi_rank, "before sorting chunk addresses");
#endif

            /* Sort the chunk address */
            if (H5D__sort_chunk(io_info, fm, chunk_addr_info_array, sum_chunk, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSWAP, FAIL, "unable to sort chunk address")
            ctg_store.contig.dset_addr = chunk_addr_info_array[0].chunk_addr;

#ifdef H5Dmpio_DEBUG
            H5D_MPIO_DEBUG(mpi_rank, "after sorting chunk addresses");
#endif

            /* Obtain MPI derived datatype from all individual chunks */
            for (u = 0; u < num_chunk; u++) {
                hsize_t *permute_map = NULL; /* array that holds the mapping from the old,
                                                out-of-order displacements to the in-order
                                                displacements of the MPI datatypes of the
                                                point selection of the file space */
                hbool_t is_permuted = FALSE;

                /* Obtain disk and memory MPI derived datatype */
                /* NOTE: The permute_map array can be allocated within H5S_mpio_space_type
                 *              and will be fed into the next call to H5S_mpio_space_type
                 *              where it will be freed.
                 */
                if (H5S_mpio_space_type(chunk_addr_info_array[u].chunk_info.fspace, type_info->src_type_size,
                                        &chunk_ftype[u],                  /* OUT: datatype created */
                                        &chunk_mpi_file_counts[u],        /* OUT */
                                        &(chunk_mft_is_derived_array[u]), /* OUT */
                                        TRUE,                             /* this is a file space,
                                                                             so permute the
                                                                             datatype if the point
                                                                             selections are out of
                                                                             order */
                                        &permute_map,                     /* OUT: a map to indicate the
                                                                             permutation of points
                                                                             selected in case they
                                                                             are out of order */
                                        &is_permuted /* OUT */) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI file type")
                /* Sanity check */
                if (is_permuted)
                    HDassert(permute_map);
                if (H5S_mpio_space_type(chunk_addr_info_array[u].chunk_info.mspace, type_info->dst_type_size,
                                        &chunk_mtype[u], &chunk_mpi_mem_counts[u],
                                        &(chunk_mbt_is_derived_array[u]), FALSE, /* this is a memory
                                                                                    space, so if the file
                                                                                    space is not
                                                                                    permuted, there is no
                                                                                    need to permute the
                                                                                    datatype if the point
                                                                                    selections are out of
                                                                                    order*/
                                        &permute_map,                            /* IN: the permutation map
                                                                                    generated by the
                                                                                    file_space selection
                                                                                    and applied to the
                                                                                    memory selection */
                                        &is_permuted /* IN */) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI buf type")
                /* Sanity check */
                if (is_permuted)
                    HDassert(!permute_map);

                /* Chunk address relative to the first chunk */
                chunk_addr_info_array[u].chunk_addr -= ctg_store.contig.dset_addr;

                /* Assign chunk address to MPI displacement */
                /* (assume MPI_Aint big enough to hold it) */
                chunk_disp_array[u] = (MPI_Aint)chunk_addr_info_array[u].chunk_addr;
            } /* end for */

            /* Create final MPI derived datatype for the file */
            if (MPI_SUCCESS !=
                (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_file_counts, chunk_disp_array,
                                                   chunk_ftype, &chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
            if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_ftype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            chunk_final_ftype_is_derived = TRUE;

            /* Create final MPI derived datatype for memory */
            if (MPI_SUCCESS !=
                (mpi_code = MPI_Type_create_struct((int)num_chunk, chunk_mpi_mem_counts, chunk_mem_disp_array,
                                                   chunk_mtype, &chunk_final_mtype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
            if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(&chunk_final_mtype)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            chunk_final_mtype_is_derived = TRUE;

            /* Free the file & memory MPI datatypes for each chunk */
            for (u = 0; u < num_chunk; u++) {
                if (chunk_mbt_is_derived_array[u])
                    if (MPI_SUCCESS != (mpi_code = MPI_Type_free(chunk_mtype + u)))
                        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

                if (chunk_mft_is_derived_array[u])
                    if (MPI_SUCCESS != (mpi_code = MPI_Type_free(chunk_ftype + u)))
                        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            } /* end for */

            /* We have a single, complicated MPI datatype for both memory & file */
            mpi_buf_count = (hsize_t)1;
        }      /* end if */
        else { /* no selection at all for this process */
            ctg_store.contig.dset_addr = 0;

            /* Set the MPI datatype */
            chunk_final_ftype = MPI_BYTE;
            chunk_final_mtype = MPI_BYTE;

            /* No chunks selected for this process */
            mpi_buf_count = (hsize_t)0;
        } /* end else */

#ifdef H5Dmpio_DEBUG
        H5D_MPIO_DEBUG(mpi_rank, "before coming to final collective I/O");
#endif

        /* Set up the base storage address for this chunk */
        io_info->store = &ctg_store;

        /* Perform final collective I/O operation */
        if (H5D__final_collective_io(io_info, type_info, mpi_buf_count, chunk_final_ftype,
                                     chunk_final_mtype) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")
    } /* end else */

done:
#ifdef H5Dmpio_DEBUG
    H5D_MPIO_DEBUG_VA(mpi_rank, "before freeing memory inside H5D_link_collective_io ret_value = %d",
                      ret_value);
#endif

    /* Release resources */
    if (chunk_addr_info_array)
        H5MM_xfree(chunk_addr_info_array);
    if (chunk_mtype)
        H5MM_xfree(chunk_mtype);
    if (chunk_ftype)
        H5MM_xfree(chunk_ftype);
    if (chunk_disp_array)
        H5MM_xfree(chunk_disp_array);
    if (chunk_mem_disp_array)
        H5MM_xfree(chunk_mem_disp_array);
    if (chunk_mpi_mem_counts)
        H5MM_xfree(chunk_mpi_mem_counts);
    if (chunk_mpi_file_counts)
        H5MM_xfree(chunk_mpi_file_counts);
    if (chunk_mbt_is_derived_array)
        H5MM_xfree(chunk_mbt_is_derived_array);
    if (chunk_mft_is_derived_array)
        H5MM_xfree(chunk_mft_is_derived_array);

    /* Free the MPI buf and file types, if they were derived */
    if (chunk_final_mtype_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_final_mtype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (chunk_final_ftype_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_final_ftype)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__link_chunk_collective_io */

/*-------------------------------------------------------------------------
 * Function:    H5D__link_chunk_filtered_collective_io
 *
 * Purpose:     Performs collective I/O on filtered chunks by creating a
 *              single MPI derived datatype to link with all filtered
 *              chunks. The general algorithm is as follows:
 *
 *              1. Construct a list of selected chunks in the collective
 *                 I/O operation
 *              2. If the operation is a read operation
 *                 A. Ensure that the list of chunks is sorted in
 *                    monotonically non-decreasing order of chunk offset
 *                    in the file
 *                 B. Participate in a collective read of chunks from
 *                    the file
 *                 C. Loop through each selected chunk, unfiltering it and
 *                    scattering the data to the application's read buffer
 *              3. If the operation is a write operation
 *                 A. Redistribute any chunks being written by more than 1
 *                    MPI rank, such that the chunk is only owned by 1 MPI
 *                    rank. The rank writing to the chunk which currently
 *                    has the least amount of chunks assigned to it becomes
 *                    the new owner (in the case of ties, the lowest MPI
 *                    rank becomes the new owner)
 *                 B. Participate in a collective read of chunks from the
 *                    file
 *                 C. Loop through each chunk selected in the operation
 *                    and for each chunk:
 *                    I. If we actually read the chunk from the file (if
 *                       a chunk is being fully overwritten, we skip
 *                       reading it), pass the chunk through the filter
 *                       pipeline in reverse order (unfilter the chunk)
 *                    II. Update the chunk data with the modifications from
 *                        the owning MPI rank
 *                    III. Receive any modification data from other
 *                         ranks and update the chunk data with those
 *                         modifications
 *                    IV. Filter the chunk
 *                 D. Contribute the modified chunks to an array gathered
 *                    by all ranks which contains information for
 *                    re-allocating space in the file for every chunk
 *                    modified. Then, each rank collectively re-allocates
 *                    each chunk from the gathered array with their new
 *                    sizes after the filter operation
 *                 E. Proceed with the collective write operation for all
 *                    the modified chunks
 *                 F. Contribute the modified chunks to an array gathered
 *                    by all ranks which contains information for
 *                    re-inserting every chunk modified into the chunk
 *                    index. Then, each rank collectively re-inserts each
 *                    chunk from the gathered array into the chunk index
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__link_chunk_filtered_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                       H5D_chunk_map_t *fm, int mpi_rank, int mpi_size)
{
    H5D_filtered_collective_io_info_t *chunk_list = NULL; /* The list of chunks being read/written */
    H5D_storage_t                      ctg_store; /* Chunk storage information as contiguous dataset */
    MPI_Datatype                       mem_type             = MPI_BYTE;
    MPI_Datatype                       file_type            = MPI_BYTE;
    hbool_t                            mem_type_is_derived  = FALSE;
    hbool_t                            file_type_is_derived = FALSE;
    size_t *                           rank_chunks_assigned_map = NULL;
    size_t                             chunk_list_num_entries;
    size_t                             i;
    int                                mpi_code;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(io_info);
    HDassert(type_info);
    HDassert(fm);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_DEBUG_VA(mpi_rank, "Performing Linked-chunk I/O (%s) with MPI Comm size of %d",
                      io_info->op_type == H5D_IO_OP_WRITE ? "write" : "read", mpi_size);
    H5D_MPIO_TIME_START(mpi_rank, "Linked-chunk I/O");
#endif

    /* Set the actual-chunk-opt-mode property. */
    H5CX_set_mpio_actual_chunk_opt(H5D_MPIO_LINK_CHUNK);

    /* Set the actual-io-mode property.
     * Link chunk filtered I/O does not break to independent, so can set right away
     */
    H5CX_set_mpio_actual_io_mode(H5D_MPIO_CHUNK_COLLECTIVE);

    /* Build a list of selected chunks in the collective io operation */
    if (H5D__mpio_collective_filtered_chunk_io_setup(io_info, type_info, fm, &chunk_list,
                                                     &chunk_list_num_entries, mpi_rank) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't construct filtered I/O info list")

    if (io_info->op_type == H5D_IO_OP_READ) { /* Filtered collective read */
        if (H5D__mpio_collective_filtered_chunk_read(chunk_list, chunk_list_num_entries, io_info, type_info,
                                                     mpi_rank) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "couldn't read filtered chunks")
    }
    else { /* Filtered collective write */
        H5D_chk_idx_info_t index_info;
        hsize_t            mpi_buf_count;

        H5D_MPIO_INIT_CHUNK_IDX_INFO(index_info, io_info);

        /* Redistribute any shared chunks being written to and
         * send around chunk modification data as necessary
         */
        if (mpi_size > 1) {
            if (H5D__mpio_redistribute_shared_chunks(chunk_list, chunk_list_num_entries,
                                                     io_info, fm, mpi_rank, mpi_size,
                                                     &rank_chunks_assigned_map) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to redistribute shared chunks")

            /* Send chunk modification data */
            if (H5D__mpio_share_chunk_modification_data(chunk_list, &chunk_list_num_entries, io_info,
                                                        type_info, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                            "unable to send chunk modification data between MPI ranks")

            /* Make sure the local chunk list was updated correctly */
            HDassert(chunk_list_num_entries == rank_chunks_assigned_map[mpi_rank]);
        }

        /* Proceed to update all the chunks this rank owns with its own
         * modification data and data from other ranks, before re-filtering
         * the chunks.
         */
        if (H5D__mpio_collective_filtered_chunk_update(chunk_list, chunk_list_num_entries, io_info, type_info,
                                                       mpi_rank) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "couldn't update modified chunks")

        /* All ranks now collectively re-allocate file space for all chunks */
        if (H5D__mpio_collective_filtered_chunk_reallocate(chunk_list, chunk_list_num_entries, io_info,
                                                           &index_info, mpi_rank, mpi_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                        "couldn't collectively re-allocate file space for chunks")

        /* If this rank has any chunks selected, create a MPI type for collectively
         * writing out the chunks to file. Otherwise, the rank contributes to the
         * collective write with a none type.
         */
        if (H5D__mpio_collective_filtered_io_type(chunk_list, chunk_list_num_entries, io_info->op_type,
                                                  &mem_type, &mem_type_is_derived, &file_type,
                                                  &file_type_is_derived) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                        "couldn't create MPI type for writing filtered chunks")

        mpi_buf_count = (file_type_is_derived || mem_type_is_derived) ? 1 : 0;

        /* Setup contig storage info for I/O operation */
        if (chunk_list_num_entries) {
            /*
             * Override the write buffer to point to the first
             * chunk's data buffer
             */
            io_info->u.wbuf = chunk_list[0].buf;

            /*
             * Setup the base storage address for this operation
             * to be the first chunk's file address
             */
            ctg_store.contig.dset_addr = chunk_list[0].chunk_new.offset;
        }
        else
            ctg_store.contig.dset_addr = 0;

        /* Perform I/O */
        io_info->store = &ctg_store;
        if (H5D__final_collective_io(io_info, type_info, mpi_buf_count, file_type, mem_type) < 0)
            HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")

        /* Free up resources in anticipation of following collective operation */
        for (i = 0; i < chunk_list_num_entries; i++) {
            if (chunk_list[i].buf) {
                H5MM_free(chunk_list[i].buf);
                chunk_list[i].buf = NULL;
            }
        }

        /* Participate in the collective re-insertion of all chunks modified
         * into the chunk index
         */
        if (H5D__mpio_collective_filtered_chunk_reinsert(chunk_list, chunk_list_num_entries, io_info,
                                                         &index_info, mpi_rank, mpi_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                        "couldn't collectively re-insert modified chunks into chunk index")
    }

done:
    /* Free the MPI buf and file types, if they were derived */
    if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    /* Free resources used by a rank which had some selection */
    if (chunk_list) {
        for (i = 0; i < chunk_list_num_entries; i++)
            if (chunk_list[i].buf)
                H5MM_free(chunk_list[i].buf);

        H5MM_free(chunk_list);
    } /* end if */

    if (rank_chunks_assigned_map)
        H5MM_free(rank_chunks_assigned_map);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__link_chunk_filtered_collective_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__multi_chunk_collective_io
 *
 * Purpose:     To do IO per chunk according to IO mode(collective/independent/none)
 *
 *              1. Use MPI_gather and MPI_Bcast to obtain IO mode in each chunk(collective/independent/none)
 *              2. Depending on whether the IO mode is collective or independent or none,
 *                 Create either MPI derived datatype for each chunk or just do independent IO
 *              3. Use common collective IO routine to do MPI-IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__multi_chunk_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info, H5D_chunk_map_t *fm,
                               int mpi_rank, int mpi_size)
{
    H5D_io_info_t              ctg_io_info; /* Contiguous I/O info object */
    H5D_storage_t              ctg_store;   /* Chunk storage information as contiguous dataset */
    H5D_io_info_t              cpt_io_info; /* Compact I/O info object */
    H5D_storage_t              cpt_store;   /* Chunk storage information as compact dataset */
    hbool_t                    cpt_dirty;   /* Temporary placeholder for compact storage "dirty" flag */
    uint8_t *                  chunk_io_option = NULL;
    haddr_t *                  chunk_addr      = NULL;
    H5D_storage_t              store; /* union of EFL and chunk pointer in file space */
    H5FD_mpio_collective_opt_t last_coll_opt_mode =
        H5FD_MPIO_COLLECTIVE_IO; /* Last parallel transfer with independent IO or collective IO with this mode
                                  */
    size_t                    total_chunk; /* Total # of chunks in dataset */
    size_t                    u;           /* Local index variable */
    H5D_mpio_actual_io_mode_t actual_io_mode =
        H5D_MPIO_NO_COLLECTIVE; /* Local variable for tracking the I/O mode used. */
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Set the actual chunk opt mode property */
    H5CX_set_mpio_actual_chunk_opt(H5D_MPIO_MULTI_CHUNK);

    /* Retrieve total # of chunks in dataset */
    H5_CHECKED_ASSIGN(total_chunk, size_t, fm->layout->u.chunk.nchunks, hsize_t);
    HDassert(total_chunk != 0);

    /* Allocate memories */
    chunk_io_option = (uint8_t *)H5MM_calloc(total_chunk);
    chunk_addr      = (haddr_t *)H5MM_calloc(total_chunk * sizeof(haddr_t));

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_DEBUG_VA(mpi_rank, "total_chunk %zu", total_chunk);
#endif

    /* Obtain IO option for each chunk */
    if (H5D__obtain_mpio_mode(io_info, fm, chunk_io_option, chunk_addr, mpi_rank, mpi_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTRECV, FAIL, "unable to obtain MPIO mode")

    /* Set up contiguous I/O info object */
    H5MM_memcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store      = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;

    /* Set up compact I/O info object */
    H5MM_memcpy(&cpt_io_info, io_info, sizeof(cpt_io_info));
    cpt_io_info.store      = &cpt_store;
    cpt_io_info.layout_ops = *H5D_LOPS_COMPACT;

    /* Initialize temporary compact storage info */
    cpt_store.compact.dirty = &cpt_dirty;

    /* Set dataset storage for I/O info */
    io_info->store = &store;

    /* Loop over _all_ the chunks */
    for (u = 0; u < total_chunk; u++) {
        H5D_chunk_info_t *chunk_info; /* Chunk info for current chunk */
        H5S_t *           fspace;     /* Dataspace describing chunk & selection in it */
        H5S_t *           mspace; /* Dataspace describing selection in memory corresponding to this chunk */

#ifdef H5Dmpio_DEBUG
        H5D_MPIO_DEBUG_VA(mpi_rank, "mpi_rank = %d, chunk index = %zu", mpi_rank, u);
#endif
        /* Get the chunk info for this chunk, if there are elements selected */
        chunk_info = fm->select_chunk[u];

        /* Set the storage information for chunks with selections */
        if (chunk_info) {
            HDassert(chunk_info->index == u);

            /* Pass in chunk's coordinates in a union. */
            store.chunk.scaled = chunk_info->scaled;
        } /* end if */

        /* Collective IO for this chunk,
         * Note: even there is no selection for this process, the process still
         *      needs to contribute MPI NONE TYPE.
         */
        if (chunk_io_option[u] == H5D_CHUNK_IO_MODE_COL) {
#ifdef H5Dmpio_DEBUG
            H5D_MPIO_DEBUG_VA(mpi_rank, "inside collective chunk IO mpi_rank = %d, chunk index = %zu",
                              mpi_rank, u);
#endif

            /* Set the file & memory dataspaces */
            if (chunk_info) {
                fspace = chunk_info->fspace;
                mspace = chunk_info->mspace;

                /* Update the local variable tracking the actual io mode property.
                 *
                 * Note: H5D_MPIO_COLLECTIVE_MULTI | H5D_MPIO_INDEPENDENT = H5D_MPIO_MIXED
                 *      to ease switching between to mixed I/O without checking the current
                 *      value of the property. You can see the definition in H5Ppublic.h
                 */
                actual_io_mode = (H5D_mpio_actual_io_mode_t)(actual_io_mode | H5D_MPIO_CHUNK_COLLECTIVE);

            } /* end if */
            else {
                fspace = mspace = NULL;
            } /* end else */

            /* Switch back to collective I/O */
            if (last_coll_opt_mode != H5FD_MPIO_COLLECTIVE_IO) {
                if (H5CX_set_mpio_coll_opt(H5FD_MPIO_COLLECTIVE_IO) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't switch to collective I/O")
                last_coll_opt_mode = H5FD_MPIO_COLLECTIVE_IO;
            } /* end if */

            /* Initialize temporary contiguous storage address */
            ctg_store.contig.dset_addr = chunk_addr[u];

            /* Perform the I/O */
            if (H5D__inter_collective_io(&ctg_io_info, type_info, fspace, mspace) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
        }      /* end if */
        else { /* possible independent IO for this chunk */
#ifdef H5Dmpio_DEBUG
            H5D_MPIO_DEBUG_VA(mpi_rank, "inside independent IO mpi_rank = %d, chunk index = %zu", mpi_rank,
                              u);
#endif

            HDassert(chunk_io_option[u] == 0);

            /* Set the file & memory dataspaces */
            if (chunk_info) {
                fspace = chunk_info->fspace;
                mspace = chunk_info->mspace;

                /* Update the local variable tracking the actual io mode. */
                actual_io_mode = (H5D_mpio_actual_io_mode_t)(actual_io_mode | H5D_MPIO_CHUNK_INDEPENDENT);
            } /* end if */
            else {
                fspace = mspace = NULL;
            } /* end else */

            /* Using independent I/O with file setview.*/
            if (last_coll_opt_mode != H5FD_MPIO_INDIVIDUAL_IO) {
                if (H5CX_set_mpio_coll_opt(H5FD_MPIO_INDIVIDUAL_IO) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't switch to individual I/O")
                last_coll_opt_mode = H5FD_MPIO_INDIVIDUAL_IO;
            } /* end if */

            /* Initialize temporary contiguous storage address */
            ctg_store.contig.dset_addr = chunk_addr[u];

            /* Perform the I/O */
            if (H5D__inter_collective_io(&ctg_io_info, type_info, fspace, mspace) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish shared collective MPI-IO")
#ifdef H5Dmpio_DEBUG
            H5D_MPIO_DEBUG(mpi_rank, "after inter collective IO");
#endif
        } /* end else */
    }     /* end for */

    /* Write the local value of actual io mode to the API context. */
    H5CX_set_mpio_actual_io_mode(actual_io_mode);

done:
    if (chunk_io_option)
        H5MM_xfree(chunk_io_option);
    if (chunk_addr)
        H5MM_xfree(chunk_addr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__multi_chunk_collective_io */

/*-------------------------------------------------------------------------
 * Function:    H5D__multi_chunk_filtered_collective_io
 *
 * Purpose:     Performs collective I/O on filtered chunks iteratively to
 *              save on memory and potentially get better performance
 *              depending on the average number of chunks per rank. While
 *              linked-chunk I/O will construct and work on a list of all
 *              of the chunks selected in the I/O operation at once, this
 *              function works iteratively on a set of chunks at a time; at
 *              most one chunk per rank per iteration.  The general
 *              algorithm is as follows:
 *
 *              1. Construct a list of selected chunks in the collective
 *                 I/O operation
 *              2. If the operation is a read operation, loop an amount of
 *                 times equal to the maximum number of chunks selected on
 *                 any particular rank and on each iteration:
 *                 A. Participate in a collective read of chunks from
 *                    the file (ranks that run out of chunks still need
 *                    to participate)
 *                 B. Unfilter the chunk that was read (if any)
 *                 C. Scatter the read chunk's data to the application's
 *                    read buffer
 *              3. If the operation is a write operation, redistribute any
 *                 chunks being written to by more than 1 MPI rank, such
 *                 that the chunk is only owned by 1 MPI rank. The rank
 *                 writing to the chunk which currently has the least
 *                 amount of chunks assigned to it becomes the new owner
 *                 (in the case of ties, the lowest MPI rank becomes the
 *                 new owner). Then, loop an amount of times equal to the
 *                 maximum number of chunks selected on any particular
 *                 rank and on each iteration:
 *                 A. Participate in a collective read of chunks from
 *                    the file (ranks that run out of chunks still need
 *                    to participate)
 *                    I. If we actually read a chunk from the file (if
 *                       a chunk is being fully overwritten, we skip
 *                       reading it), pass the chunk through the filter
 *                       pipeline in reverse order (unfilter the chunk)
 *                 B. Update the chunk data with the modifications from
 *                    the owning rank
 *                 C. Receive any modification data from other ranks and
 *                    update the chunk data with those modifications
 *                 D. Filter the chunk
 *                 E. Contribute the chunk to an array gathered by
 *                    all ranks which contains information for
 *                    re-allocating space in the file for every chunk
 *                    modified in this iteration (up to one chunk per
 *                    rank; some ranks may not have a selection/may have
 *                    less chunks to work on than other ranks). Then,
 *                    each rank collectively re-allocates each chunk
 *                    from the gathered array with their new sizes
 *                    after the filter operation
 *                 F. Proceed with the collective write operation
 *                    for the chunks modified on this iteration
 *                 G. Contribute the chunk to an array gathered by
 *                    all ranks which contains information for
 *                    re-inserting every chunk modified on this
 *                    iteration into the chunk index. Then, each rank
 *                    collectively re-inserts each chunk from the
 *                    gathered array into the chunk index
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__multi_chunk_filtered_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                        H5D_chunk_map_t *fm, int mpi_rank, int mpi_size)
{
    H5D_filtered_collective_io_info_t *chunk_list = NULL; /* The list of chunks being read/written */
    H5D_io_info_t                      ctg_io_info;       /* Contiguous I/O info object */
    H5D_storage_t                      ctg_store; /* Chunk storage information as contiguous dataset */
    MPI_Datatype                       mem_type             = MPI_BYTE;
    MPI_Datatype                       file_type            = MPI_BYTE;
    hbool_t                            mem_type_is_derived  = FALSE;
    hbool_t                            file_type_is_derived = FALSE;
    hbool_t                            have_chunk_to_process;
    size_t *                           rank_chunks_assigned_map = NULL;
    size_t                             chunk_list_num_entries;
    size_t                             i;
    size_t                             max_num_chunks;
    int                                mpi_code;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(io_info);
    HDassert(type_info);
    HDassert(fm);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_DEBUG_VA(mpi_rank, "Performing Multi-chunk I/O (%s) with MPI Comm size of %d",
                      io_info->op_type == H5D_IO_OP_WRITE ? "write" : "read", mpi_size);
    H5D_MPIO_TIME_START(mpi_rank, "Multi-chunk I/O");
#endif

    /* Set the actual chunk opt mode property */
    H5CX_set_mpio_actual_chunk_opt(H5D_MPIO_MULTI_CHUNK);

    /* Set the actual_io_mode property.
     * Multi chunk I/O does not break to independent, so can set right away
     */
    H5CX_set_mpio_actual_io_mode(H5D_MPIO_CHUNK_COLLECTIVE);

    /* Build a list of selected chunks in the collective IO operation */
    if (H5D__mpio_collective_filtered_chunk_io_setup(io_info, type_info, fm, &chunk_list,
                                                     &chunk_list_num_entries, mpi_rank) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't construct filtered I/O info list")

    /* Retrieve the maximum number of chunks selected for any rank */
    if (MPI_SUCCESS != (mpi_code = MPI_Allreduce(&chunk_list_num_entries, &max_num_chunks, 1,
                                                 MPI_UNSIGNED_LONG_LONG, MPI_MAX, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allreduce failed", mpi_code)

    /* If no one has anything selected at all, end the operation */
    if (0 == max_num_chunks)
        HGOTO_DONE(SUCCEED);

    /* Set up contiguous I/O info object */
    H5MM_memcpy(&ctg_io_info, io_info, sizeof(ctg_io_info));
    ctg_io_info.store      = &ctg_store;
    ctg_io_info.layout_ops = *H5D_LOPS_CONTIG;

    /* Initialize temporary contiguous storage info */
    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;

    if (io_info->op_type == H5D_IO_OP_READ) { /* Filtered collective read */
        for (i = 0; i < max_num_chunks; i++) {
            /* Check if this rank has a chunk to work on for this iteration */
            have_chunk_to_process = (i < chunk_list_num_entries);

            if (H5D__mpio_collective_filtered_chunk_read(have_chunk_to_process ? &chunk_list[i] : NULL,
                                                         have_chunk_to_process ? 1 : 0, io_info, type_info,
                                                         mpi_rank) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "couldn't read filtered chunks")

            if (have_chunk_to_process && chunk_list[i].buf) {
                H5MM_free(chunk_list[i].buf);
                chunk_list[i].buf = NULL;
            }
        }
    }
    else { /* Filtered collective write */
        H5D_chk_idx_info_t index_info;
        hsize_t            mpi_buf_count;

        /* Construct chunked index info */
        H5D_MPIO_INIT_CHUNK_IDX_INFO(index_info, io_info);

        /* Redistribute any shared chunks being written to and
         * send around chunk modification data as necessary
         */
        if (mpi_size > 1) {
            if (H5D__mpio_redistribute_shared_chunks(chunk_list, chunk_list_num_entries,
                                                     io_info, fm, mpi_rank, mpi_size,
                                                     &rank_chunks_assigned_map) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "unable to redistribute shared chunks")

            /* Send chunk modification data */
            if (H5D__mpio_share_chunk_modification_data(chunk_list, &chunk_list_num_entries, io_info,
                                                        type_info, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                            "unable to send chunk modification data between MPI ranks")

            /* Make sure the local chunk list was updated correctly */
            HDassert(chunk_list_num_entries == rank_chunks_assigned_map[mpi_rank]);
        }

        /* Iterate over the max number of chunks among all ranks, as this rank could
         * have no chunks left to work on, but it still needs to participate in the
         * collective re-allocation and re-insertion of chunks modified by other ranks.
         */
        for (i = 0; i < max_num_chunks; i++) {
            /* Check if this rank has a chunk to work on for this iteration */
            have_chunk_to_process = (i < chunk_list_num_entries) && (mpi_rank == chunk_list[i].new_owner);

            /* Proceed to update the chunk this rank owns (if any left) with its
             * own modification data and data from other ranks, before re-filtering
             * the chunks. As chunk reads are done collectively here, all ranks
             * must participate.
             */
            if (H5D__mpio_collective_filtered_chunk_update(have_chunk_to_process ? &chunk_list[i] : NULL,
                                                           have_chunk_to_process ? 1 : 0, io_info, type_info,
                                                           mpi_rank) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "couldn't update modified chunks")

            /* All ranks now collectively re-allocate file space for all chunks */
            if (H5D__mpio_collective_filtered_chunk_reallocate(have_chunk_to_process ? &chunk_list[i] : NULL,
                                                               have_chunk_to_process ? 1 : 0, io_info,
                                                               &index_info, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                            "couldn't collectively re-allocate file space for chunks")

            /*
             * If this rank has a chunk to work on, create a MPI type
             * for writing out the chunk. Otherwise, the rank will
             * use MPI_BYTE for the file and memory type and specify
             * a count of 0.
             */
            if (H5D__mpio_collective_filtered_io_type(
                    have_chunk_to_process ? &chunk_list[i] : NULL, have_chunk_to_process ? 1 : 0,
                    io_info->op_type, &mem_type, &mem_type_is_derived, &file_type, &file_type_is_derived) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                            "couldn't create MPI type for writing filtered chunks")

            mpi_buf_count = (file_type_is_derived || mem_type_is_derived) ? 1 : 0;

            /* Override the write buffer to point to the chunk data buffer */
            if (have_chunk_to_process) {
                /*
                 * Override the write buffer to point to the
                 * chunk's data buffer
                 */
                ctg_io_info.u.wbuf = chunk_list[i].buf;

                /*
                 * Setup the base storage address for this
                 * operation to be the chunk's file address
                 */
                ctg_store.contig.dset_addr = chunk_list[i].chunk_new.offset;
            }
            else
                ctg_store.contig.dset_addr = 0;

            /* Perform the I/O */
            if (H5D__final_collective_io(&ctg_io_info, type_info, mpi_buf_count, file_type, mem_type) < 0)
                HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish MPI-IO")

            /* Free up resources in anticipation of following collective operation */
            if (have_chunk_to_process && chunk_list[i].buf) {
                H5MM_free(chunk_list[i].buf);
                chunk_list[i].buf = NULL;
            }

            /* Participate in the collective re-insertion of all chunks modified
             * in this iteration into the chunk index
             */
            if (H5D__mpio_collective_filtered_chunk_reinsert(have_chunk_to_process ? &chunk_list[i] : NULL,
                                                             have_chunk_to_process ? 1 : 0, io_info,
                                                             &index_info, mpi_rank, mpi_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL,
                            "couldn't collectively re-insert modified chunks into chunk index")

            /* Free the MPI types, if they were derived */
            if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            mem_type_is_derived = FALSE;
            if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            file_type_is_derived = FALSE;
        } /* end for */
    }

done:
    /* Free the MPI buf and file types, if they were derived */
    if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    /* Free resources used by a rank which had some selection */
    if (chunk_list) {
        for (i = 0; i < chunk_list_num_entries; i++)
            if (chunk_list[i].buf)
                H5MM_free(chunk_list[i].buf);

        H5MM_free(chunk_list);
    } /* end if */

    if (rank_chunks_assigned_map)
        H5MM_free(rank_chunks_assigned_map);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__multi_chunk_filtered_collective_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__inter_collective_io
 *
 * Purpose:     Routine for the shared part of collective IO between multiple chunk
 *              collective IO and contiguous collective IO
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__inter_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info, const H5S_t *file_space,
                         const H5S_t *mem_space)
{
    int          mpi_buf_count; /* # of MPI types */
    hbool_t      mbt_is_derived = FALSE;
    hbool_t      mft_is_derived = FALSE;
    MPI_Datatype mpi_file_type, mpi_buf_type;
    int          mpi_code; /* MPI return code */
#ifdef H5Dmpio_DEBUG
    int mpi_rank;
#endif
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_STATIC

#ifdef H5Dmpio_DEBUG
    mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file);
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Inter collective I/O");
    if (mpi_rank < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain MPI rank")
#endif

    if ((file_space != NULL) && (mem_space != NULL)) {
        int      mpi_file_count;     /* Number of file "objects" to transfer */
        hsize_t *permute_map = NULL; /* array that holds the mapping from the old,
                                        out-of-order displacements to the in-order
                                        displacements of the MPI datatypes of the
                                        point selection of the file space */
        hbool_t is_permuted = FALSE;

        /* Obtain disk and memory MPI derived datatype */
        /* NOTE: The permute_map array can be allocated within H5S_mpio_space_type
         *              and will be fed into the next call to H5S_mpio_space_type
         *              where it will be freed.
         */
        if (H5S_mpio_space_type(file_space, type_info->src_type_size, &mpi_file_type, &mpi_file_count,
                                &mft_is_derived, /* OUT: datatype created */
                                TRUE,            /* this is a file space, so
                                                    permute the datatype if the
                                                    point selection is out of
                                                    order */
                                &permute_map,    /* OUT: a map to indicate
                                                    the permutation of
                                                    points selected in
                                                    case they are out of
                                                    order */
                                &is_permuted /* OUT */) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI file type")
        /* Sanity check */
        if (is_permuted)
            HDassert(permute_map);
        if (H5S_mpio_space_type(mem_space, type_info->src_type_size, &mpi_buf_type, &mpi_buf_count,
                                &mbt_is_derived, /* OUT: datatype created */
                                FALSE,           /* this is a memory space, so if
                                                    the file space is not
                                                    permuted, there is no need to
                                                    permute the datatype if the
                                                    point selections are out of
                                                    order*/
                                &permute_map     /* IN: the permutation map
                                                    generated by the
                                                    file_space selection
                                                    and applied to the
                                                    memory selection */
                                ,
                                &is_permuted /* IN */) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADTYPE, FAIL, "couldn't create MPI buffer type")
        /* Sanity check */
        if (is_permuted)
            HDassert(!permute_map);
    } /* end if */
    else {
        /* For non-selection, participate with a none MPI derived datatype, the count is 0.  */
        mpi_buf_type   = MPI_BYTE;
        mpi_file_type  = MPI_BYTE;
        mpi_buf_count  = 0;
        mbt_is_derived = FALSE;
        mft_is_derived = FALSE;
    } /* end else */

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_DEBUG(mpi_rank, "before final collective I/O");
#endif

    /* Perform final collective I/O operation */
    if (H5D__final_collective_io(io_info, type_info, (hsize_t)mpi_buf_count, mpi_file_type, mpi_buf_type) < 0)
        HGOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "couldn't finish collective MPI-IO")

done:
    /* Free the MPI buf and file types, if they were derived */
    if (mbt_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mpi_buf_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (mft_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mpi_file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_DEBUG_VA(mpi_rank, "before leaving inter_collective_io ret_value = %d", ret_value);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__inter_collective_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__final_collective_io
 *
 * Purpose:     Routine for the common part of collective IO with different storages.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__final_collective_io(H5D_io_info_t *io_info, const H5D_type_info_t *type_info, hsize_t mpi_buf_count,
                         MPI_Datatype mpi_file_type, MPI_Datatype mpi_buf_type)
{
#ifdef H5Dmpio_DEBUG
    int mpi_rank;
#endif
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_STATIC

#ifdef H5Dmpio_DEBUG
    mpi_rank = H5F_mpi_get_rank(io_info->dset->oloc.file);
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Final collective I/O");
    if (mpi_rank < 0)
        HGOTO_ERROR(H5E_IO, H5E_MPI, FAIL, "unable to obtain MPI rank")
#endif

    /* Pass buf type, file type to the file driver.  */
    if (H5CX_set_mpi_coll_datatypes(mpi_buf_type, mpi_file_type) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "can't set MPI-I/O collective I/O datatypes")

    if (io_info->op_type == H5D_IO_OP_WRITE) {
        if ((io_info->io_ops.single_write)(io_info, type_info, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "optimized write failed")
    } /* end if */
    else {
        if ((io_info->io_ops.single_read)(io_info, type_info, mpi_buf_count, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "optimized read failed")
    } /* end else */

done:
#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_DEBUG_VA(mpi_rank, "ret_value before leaving final_collective_io=%d", ret_value);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__final_collective_io */

/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_chunk_addr
 *
 * Purpose:     Routine to compare chunk addresses
 *
 * Description: Callback for qsort() to compare chunk addresses
 *
 * Return:      -1, 0, 1
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_chunk_addr(const void *chunk_addr_info1, const void *chunk_addr_info2)
{
    haddr_t addr1 = HADDR_UNDEF, addr2 = HADDR_UNDEF;

    FUNC_ENTER_STATIC_NOERR

    addr1 = ((const H5D_chunk_addr_info_t *)chunk_addr_info1)->chunk_addr;
    addr2 = ((const H5D_chunk_addr_info_t *)chunk_addr_info2)->chunk_addr;

    FUNC_LEAVE_NOAPI(H5F_addr_cmp(addr1, addr2))
} /* end H5D__cmp_chunk_addr() */

/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_filtered_collective_io_info_entry
 *
 * Purpose:     Routine to compare filtered collective chunk io info
 *              entries
 *
 * Description: Callback for qsort() to compare filtered collective chunk
 *              io info entries
 *
 * Return:      -1, 0, 1
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_filtered_collective_io_info_entry(const void *filtered_collective_io_info_entry1,
                                           const void *filtered_collective_io_info_entry2)
{
    const H5D_filtered_collective_io_info_t *entry1;
    const H5D_filtered_collective_io_info_t *entry2;
    haddr_t                                  addr1 = HADDR_UNDEF;
    haddr_t                                  addr2 = HADDR_UNDEF;
    int                                      ret_value;

    FUNC_ENTER_STATIC_NOERR

    entry1 = (const H5D_filtered_collective_io_info_t *)filtered_collective_io_info_entry1;
    entry2 = (const H5D_filtered_collective_io_info_t *)filtered_collective_io_info_entry2;

    addr1 = entry1->chunk_new.offset;
    addr2 = entry2->chunk_new.offset;

    /*
     * If both chunk addresses are defined, H5F_addr_cmp is safe to use.
     * Otherwise, if both addresses aren't defined, compared chunk
     * entries based on their chunk index. Finally, if only one chunk
     * address is defined, return the appropriate value based on which
     * is defined.
     */
    if (H5F_addr_defined(addr1) && H5F_addr_defined(addr2)) {
        ret_value = H5F_addr_cmp(addr1, addr2);
    }
    else if (!H5F_addr_defined(addr1) && !H5F_addr_defined(addr2)) {
        hsize_t chunk_idx1 = entry1->index_info.chunk_idx;
        hsize_t chunk_idx2 = entry2->index_info.chunk_idx;

        ret_value = (chunk_idx1 > chunk_idx2) - (chunk_idx1 < chunk_idx2);
    }
    else
        ret_value = H5F_addr_defined(addr1) ? 1 : -1;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__cmp_filtered_collective_io_info_entry() */

/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_chunk_redistribute_info
 *
 * Purpose:     Routine to compare two H5D_chunk_redistribute_info_t
 *              structures
 *
 * Description: Callback for qsort() to compare two
 *              H5D_chunk_redistribute_info_t structures
 *
 * Return:      -1, 0, 1
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_chunk_redistribute_info(const void *_entry1, const void *_entry2)
{
    const H5D_chunk_redistribute_info_t *entry1;
    const H5D_chunk_redistribute_info_t *entry2;
    hsize_t                              chunk_index1;
    hsize_t                              chunk_index2;
    int                                  ret_value;

    FUNC_ENTER_STATIC_NOERR

    entry1 = (const H5D_chunk_redistribute_info_t *)_entry1;
    entry2 = (const H5D_chunk_redistribute_info_t *)_entry2;

    chunk_index1 = entry1->chunk_idx;
    chunk_index2 = entry2->chunk_idx;

    if (chunk_index1 == chunk_index2) {
        int orig_owner1 = entry1->orig_owner;
        int orig_owner2 = entry2->orig_owner;

        ret_value = (orig_owner1 > orig_owner2) - (orig_owner1 < orig_owner2);
    }
    else
        ret_value = (chunk_index1 > chunk_index2) - (chunk_index1 < chunk_index2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__cmp_chunk_redistribute_info() */

/*-------------------------------------------------------------------------
 * Function:    H5D__cmp_chunk_redistribute_info_orig_owner
 *
 * Purpose:     Routine to compare the original owning MPI rank for two
 *              H5D_chunk_redistribute_info_t structures
 *
 * Description: Callback for qsort() to compare the original owning MPI
 *              rank for two H5D_chunk_redistribute_info_t
 *              structures
 *
 * Return:      -1, 0, 1
 *
 *-------------------------------------------------------------------------
 */
static int
H5D__cmp_chunk_redistribute_info_orig_owner(const void *_entry1, const void *_entry2)
{
    const H5D_chunk_redistribute_info_t *entry1;
    const H5D_chunk_redistribute_info_t *entry2;
    int                                  owner1 = -1;
    int                                  owner2 = -1;
    int                                  ret_value;

    FUNC_ENTER_STATIC_NOERR

    entry1 = (const H5D_chunk_redistribute_info_t *)_entry1;
    entry2 = (const H5D_chunk_redistribute_info_t *)_entry2;

    owner1 = entry1->orig_owner;
    owner2 = entry2->orig_owner;

    if (owner1 == owner2) {
        haddr_t addr1 = entry1->chunk_block.offset;
        haddr_t addr2 = entry2->chunk_block.offset;

        /*
         * If both chunk addresses are defined, H5F_addr_cmp is safe to use.
         * Otherwise, if both addresses aren't defined, compared chunk
         * entries based on their chunk index. Finally, if only one chunk
         * address is defined, return the appropriate value based on which
         * is defined.
         */
        if (H5F_addr_defined(addr1) && H5F_addr_defined(addr2)) {
            ret_value = H5F_addr_cmp(addr1, addr2);
        }
        else if (!H5F_addr_defined(addr1) && !H5F_addr_defined(addr2)) {
            hsize_t chunk_idx1 = entry1->chunk_idx;
            hsize_t chunk_idx2 = entry2->chunk_idx;

            ret_value = (chunk_idx1 > chunk_idx2) - (chunk_idx1 < chunk_idx2);
        }
        else
            ret_value = H5F_addr_defined(addr1) ? 1 : -1;
    }
    else
        ret_value = (owner1 > owner2) - (owner1 < owner2);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__cmp_chunk_redistribute_info_orig_owner() */

/*-------------------------------------------------------------------------
 * Function:    H5D__sort_chunk
 *
 * Purpose:     Routine to sort chunks in increasing order of chunk address
 *              Each chunk address is also obtained.
 *
 * Description:
 *              For most cases, the chunk address has already been sorted in increasing order.
 *              The special sorting flag is used to optimize this common case.
 *              quick sort is used for necessary sorting.
 *
 * Parameters:
 *              Input: H5D_io_info_t* io_info,
 *                      H5D_chunk_map_t *fm(global chunk map struct)
 *              Input/Output:  H5D_chunk_addr_info_t chunk_addr_info_array[]   : array to store chunk address
 *and information many_chunk_opt                         : flag to optimize the way to obtain chunk addresses
 *                                                              for many chunks
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__sort_chunk(H5D_io_info_t *io_info, const H5D_chunk_map_t *fm,
                H5D_chunk_addr_info_t chunk_addr_info_array[], int sum_chunk, int mpi_rank, int mpi_size)
{
    H5SL_node_t *           chunk_node;           /* Current node in chunk skip list */
    H5D_chunk_info_t *      chunk_info;           /* Current chunking info. of this node. */
    haddr_t                 chunk_addr;           /* Current chunking address of this node */
    haddr_t *               total_chunk_addr_array = NULL; /* The array of chunk address for the total number of chunk */
    H5P_coll_md_read_flag_t md_reads_file_flag;
    hbool_t                 md_reads_context_flag;
    hbool_t                 restore_md_reads_state = FALSE;
    hbool_t                 do_sort                = FALSE; /* Whether the addresses need to be sorted */
    int                     bsearch_coll_chunk_threshold;
    int                     many_chunk_opt = H5D_OBTAIN_ONE_CHUNK_ADDR_IND;
    int                     mpi_code;            /* MPI return code */
    int                     i;                   /* Local index variable */
    herr_t                  ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Calculate the actual threshold to obtain all chunk addresses collectively
     *  The bigger this number is, the more possible the use of obtaining chunk
     * address collectively.
     */
    /* For non-optimization one-link IO, actual bsearch threshold is always
     *   0, we would always want to obtain the chunk addresses individually
     *   for each process.
     */
    bsearch_coll_chunk_threshold = (sum_chunk * 100) / ((int)fm->layout->u.chunk.nchunks * mpi_size);
    if ((bsearch_coll_chunk_threshold > H5D_ALL_CHUNK_ADDR_THRES_COL) &&
        ((sum_chunk / mpi_size) >= H5D_ALL_CHUNK_ADDR_THRES_COL_NUM))
        many_chunk_opt = H5D_OBTAIN_ALL_CHUNK_ADDR_COL;

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_DEBUG_VA(mpi_rank, "many_chunk_opt = %d", many_chunk_opt);
#endif

    /* If we need to optimize the way to obtain the chunk address */
    if (many_chunk_opt != H5D_OBTAIN_ONE_CHUNK_ADDR_IND) {
#ifdef H5Dmpio_DEBUG
        H5D_MPIO_DEBUG(mpi_rank, "Coming inside H5D_OBTAIN_ALL_CHUNK_ADDR_COL");
#endif
        /* Allocate array for chunk addresses */
        if (NULL == (total_chunk_addr_array =
                         (haddr_t *)H5MM_malloc(sizeof(haddr_t) * (size_t)fm->layout->u.chunk.nchunks)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "unable to allocate memory chunk address array")

        if (mpi_rank == 0) {
            herr_t result;

            /*
             * If enabled, disable collective metadata reads here.
             * Since the chunk address mapping is done on rank 0
             * only here, it will cause problems if collective
             * metadata reads are enabled.
             */
            if (H5F_get_coll_metadata_reads(io_info->dset->oloc.file)) {
                md_reads_file_flag    = H5P_FORCE_FALSE;
                md_reads_context_flag = FALSE;
                H5F_set_coll_metadata_reads(io_info->dset->oloc.file, &md_reads_file_flag,
                                            &md_reads_context_flag);
                restore_md_reads_state = TRUE;
            }

            result = H5D__chunk_addrmap(io_info, total_chunk_addr_array);

            /* Ensure that we restore the old collective metadata reads state */
            if (restore_md_reads_state) {
                H5F_set_coll_metadata_reads(io_info->dset->oloc.file, &md_reads_file_flag,
                                            &md_reads_context_flag);
                restore_md_reads_state = FALSE;
            }

            if (result < 0) {
                size_t u;

                /* Clear total chunk address array */
                for (u = 0; u < (size_t)fm->layout->u.chunk.nchunks; u++)
                    total_chunk_addr_array[u] = HADDR_UNDEF;

                /* Push error, but still participate in following MPI_Bcast */
                HDONE_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")
            }
        } /* end if */

        /* Broadcasting the MPI_IO option info. and chunk address info. */
        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(total_chunk_addr_array,
                                                 (int)(sizeof(haddr_t) * fm->layout->u.chunk.nchunks),
                                                 MPI_BYTE, (int)0, io_info->comm)))
            HMPI_GOTO_ERROR(FAIL, "MPI_BCast failed", mpi_code)
    } /* end if */

    /* Start at first node in chunk skip list */
    i = 0;
    if (NULL == (chunk_node = H5SL_first(fm->sel_chunks)))
        HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk node from skipped list")

    /* Iterate over all chunks for this process */
    while (chunk_node) {
        if (NULL == (chunk_info = (H5D_chunk_info_t *)H5SL_item(chunk_node)))
            HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk info from skipped list")

        if (many_chunk_opt == H5D_OBTAIN_ONE_CHUNK_ADDR_IND) {
            H5D_chunk_ud_t udata; /* User data for querying chunk info */

            /* Get address of chunk */
            if (H5D__chunk_lookup(io_info->dset, chunk_info->scaled, &udata) < 0)
                HGOTO_ERROR(H5E_STORAGE, H5E_CANTGET, FAIL, "couldn't get chunk info from skipped list")
            chunk_addr = udata.chunk_block.offset;
        } /* end if */
        else
            chunk_addr = total_chunk_addr_array[chunk_info->index];

        /* Check if chunk addresses are not in increasing order in the file */
        if (i > 0 && chunk_addr < chunk_addr_info_array[i - 1].chunk_addr)
            do_sort = TRUE;

        /* Set the address & info for this chunk */
        chunk_addr_info_array[i].chunk_addr = chunk_addr;
        chunk_addr_info_array[i].chunk_info = *chunk_info;

        /* Advance to next chunk in list */
        i++;
        chunk_node = H5SL_next(chunk_node);
    } /* end while */

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_DEBUG(mpi_rank, "before Qsort");
#endif

    if (do_sort) {
        size_t num_chunks = H5SL_count(fm->sel_chunks);

        HDqsort(chunk_addr_info_array, num_chunks, sizeof(chunk_addr_info_array[0]), H5D__cmp_chunk_addr);
    } /* end if */

done:
    /* Re-enable collective metadata reads if we disabled them */
    if (restore_md_reads_state)
        H5F_set_coll_metadata_reads(io_info->dset->oloc.file, &md_reads_file_flag, &md_reads_context_flag);

    if (total_chunk_addr_array)
        H5MM_xfree(total_chunk_addr_array);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__sort_chunk() */

/*-------------------------------------------------------------------------
 * Function:    H5D__obtain_mpio_mode
 *
 * Purpose:     Routine to obtain each io mode(collective,independent or none) for each chunk;
 *              Each chunk address is also obtained.
 *
 * Description:
 *
 *              1) Each process provides two piece of information for all chunks having selection
 *                 a) chunk index
 *                 b) whether this chunk is regular(for MPI derived datatype not working case)
 *
 *              2) Gather all the information to the root process
 *
 *              3) Root process will do the following:
 *                 a) Obtain chunk addresses for all chunks in this dataspace
 *                 b) With the consideration of the user option, calculate IO mode for each chunk
 *                 c) Build MPI derived datatype to combine "chunk address" and "assign_io" information
 *                      in order to do MPI Bcast only once
 *                 d) MPI Bcast the IO mode and chunk address information for each chunk.
 *              4) Each process then retrieves IO mode and chunk address information to assign_io_mode and
 *chunk_addr.
 *
 * Parameters:
 *
 *              Input: H5D_io_info_t* io_info,
 *                      H5D_chunk_map_t *fm,(global chunk map struct)
 *              Output: uint8_t assign_io_mode[], : IO mode, collective, independent or none
 *                      haddr_t chunk_addr[],     : chunk address array for each chunk
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Muqun Yang
 *              Monday, Feb. 13th, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__obtain_mpio_mode(H5D_io_info_t *io_info, H5D_chunk_map_t *fm, uint8_t assign_io_mode[],
                      haddr_t chunk_addr[], int mpi_rank, int mpi_size)
{
    size_t                  total_chunks;
    unsigned                percent_nproc_per_chunk, threshold_nproc_per_chunk;
    uint8_t *               io_mode_info      = NULL;
    uint8_t *               recv_io_mode_info = NULL;
    uint8_t *               mergebuf          = NULL;
    uint8_t *               tempbuf;
    H5SL_node_t *           chunk_node;
    H5D_chunk_info_t *      chunk_info;
    H5P_coll_md_read_flag_t md_reads_file_flag;
    hbool_t                 md_reads_context_flag;
    hbool_t                 restore_md_reads_state = FALSE;
    MPI_Comm                comm;
    int                     root;
    size_t                  ic;
    int                     mpi_code;
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    /* Assign the rank 0 to the root */
    root = 0;
    comm = io_info->comm;

    /* Setup parameters */
    H5_CHECKED_ASSIGN(total_chunks, size_t, fm->layout->u.chunk.nchunks, hsize_t);
    if (H5CX_get_mpio_chunk_opt_ratio(&percent_nproc_per_chunk) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "couldn't get percent nproc per chunk")
    /* if ratio is 0, perform collective io */
    if (0 == percent_nproc_per_chunk) {
        if (H5D__chunk_addrmap(io_info, chunk_addr) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address");
        for (ic = 0; ic < total_chunks; ic++)
            assign_io_mode[ic] = H5D_CHUNK_IO_MODE_COL;

        HGOTO_DONE(SUCCEED)
    } /* end if */

    threshold_nproc_per_chunk = (unsigned)mpi_size * percent_nproc_per_chunk / 100;

    /* Allocate memory */
    if (NULL == (io_mode_info = (uint8_t *)H5MM_calloc(total_chunks)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate I/O mode info buffer")
    if (NULL == (mergebuf = (uint8_t *)H5MM_malloc((sizeof(haddr_t) + 1) * total_chunks)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate mergebuf buffer")
    tempbuf = mergebuf + total_chunks;
    if (mpi_rank == root)
        if (NULL == (recv_io_mode_info = (uint8_t *)H5MM_malloc(total_chunks * (size_t)mpi_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate recv I/O mode info buffer")

    /* Obtain the regularity and selection information for all chunks in this process. */
    chunk_node = H5SL_first(fm->sel_chunks);
    while (chunk_node) {
        chunk_info = (H5D_chunk_info_t *)H5SL_item(chunk_node);

        io_mode_info[chunk_info->index] = H5D_CHUNK_SELECT_REG; /* this chunk is selected and is "regular" */
        chunk_node                      = H5SL_next(chunk_node);
    } /* end while */

    /* Gather all the information */
    H5_CHECK_OVERFLOW(total_chunks, size_t, int)
    if (MPI_SUCCESS != (mpi_code = MPI_Gather(io_mode_info, (int)total_chunks, MPI_BYTE, recv_io_mode_info,
                                              (int)total_chunks, MPI_BYTE, root, comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Gather failed", mpi_code)

    /* Calculate the mode for IO(collective, independent or none) at root process */
    if (mpi_rank == root) {
        size_t    nproc;
        unsigned *nproc_per_chunk;

        /*
         * If enabled, disable collective metadata reads here.
         * Since the chunk address mapping is done on rank 0
         * only here, it will cause problems if collective
         * metadata reads are enabled.
         */
        if (H5F_get_coll_metadata_reads(io_info->dset->oloc.file)) {
            md_reads_file_flag    = H5P_FORCE_FALSE;
            md_reads_context_flag = FALSE;
            H5F_set_coll_metadata_reads(io_info->dset->oloc.file, &md_reads_file_flag,
                                        &md_reads_context_flag);
            restore_md_reads_state = TRUE;
        }

        /* pre-computing: calculate number of processes and
            regularity of the selection occupied in each chunk */
        if (NULL == (nproc_per_chunk = (unsigned *)H5MM_calloc(total_chunks * sizeof(unsigned))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate nproc_per_chunk buffer")

        /* calculating the chunk address */
        if (H5D__chunk_addrmap(io_info, chunk_addr) < 0) {
            H5MM_free(nproc_per_chunk);
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get chunk address")
        } /* end if */

        /* checking for number of process per chunk and regularity of the selection*/
        for (nproc = 0; nproc < (size_t)mpi_size; nproc++) {
            uint8_t *tmp_recv_io_mode_info = recv_io_mode_info + (nproc * total_chunks);

            /* Calculate the number of process per chunk and adding irregular selection option */
            for (ic = 0; ic < total_chunks; ic++, tmp_recv_io_mode_info++) {
                if (*tmp_recv_io_mode_info != 0) {
                    nproc_per_chunk[ic]++;
                } /* end if */
            }     /* end for */
        }         /* end for */

        /* Calculating MPIO mode for each chunk (collective, independent, none) */
        for (ic = 0; ic < total_chunks; ic++) {
            if (nproc_per_chunk[ic] > MAX(1, threshold_nproc_per_chunk)) {
                assign_io_mode[ic] = H5D_CHUNK_IO_MODE_COL;
            } /* end if */
        }     /* end for */

        /* merge buffer io_mode info and chunk addr into one */
        H5MM_memcpy(mergebuf, assign_io_mode, total_chunks);
        H5MM_memcpy(tempbuf, chunk_addr, sizeof(haddr_t) * total_chunks);

        H5MM_free(nproc_per_chunk);
    } /* end if */

    /* Broadcasting the MPI_IO option info. and chunk address info. */
    if ((sizeof(haddr_t) + 1) * total_chunks > INT_MAX)
        HGOTO_ERROR(H5E_DATASET, H5E_BADVALUE, FAIL, "result overflow")
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Bcast(mergebuf, (int)((sizeof(haddr_t) + 1) * total_chunks), MPI_BYTE, root, comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_BCast failed", mpi_code)

    H5MM_memcpy(assign_io_mode, mergebuf, total_chunks);
    H5MM_memcpy(chunk_addr, tempbuf, sizeof(haddr_t) * total_chunks);

#ifdef H5_HAVE_INSTRUMENTED_LIBRARY
    {
        hbool_t coll_op = FALSE;

        for (ic = 0; ic < total_chunks; ic++)
            if (assign_io_mode[ic] == H5D_CHUNK_IO_MODE_COL) {
                if (H5CX_test_set_mpio_coll_chunk_multi_ratio_coll(0) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
                coll_op = TRUE;
                break;
            } /* end if */

        if (!coll_op)
            if (H5CX_test_set_mpio_coll_chunk_multi_ratio_ind(0) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, FAIL, "unable to set property value")
    }
#endif

done:
    /* Re-enable collective metadata reads if we disabled them */
    if (restore_md_reads_state)
        H5F_set_coll_metadata_reads(io_info->dset->oloc.file, &md_reads_file_flag, &md_reads_context_flag);

    if (io_mode_info)
        H5MM_free(io_mode_info);
    if (mergebuf)
        H5MM_free(mergebuf);
    if (recv_io_mode_info) {
        HDassert(mpi_rank == root);
        H5MM_free(recv_io_mode_info);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__obtain_mpio_mode() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_io_setup
 *
 * Purpose:     Constructs a list of entries which contain the necessary
 *              information for inter-process communication when performing
 *              collective io on filtered chunks. This list is used by
 *              each MPI rank when performing I/O on locally selected
 *              chunks and also in operations that must be collectively
 *              done on every chunk, such as chunk re-allocation, insertion
 *              of chunks into the chunk index, etc.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_io_setup(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
                                             const H5D_chunk_map_t *             fm,
                                             H5D_filtered_collective_io_info_t **chunk_list,
                                             size_t *num_entries, int mpi_rank)
{
    H5D_filtered_collective_io_info_t *local_info_array = NULL;
    hbool_t                            filter_partial_edge_chunks;
    size_t                             num_chunks_selected;
    size_t                             i;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(io_info);
    HDassert(type_info);
    HDassert(fm);
    HDassert(chunk_list);
    HDassert(num_entries);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Filtered Collective I/O Setup");
#endif

    /* Each rank builds a local list of the chunks they have selected */
    if ((num_chunks_selected = H5SL_count(fm->sel_chunks))) {
        H5D_chunk_info_t *chunk_info;
        H5D_chunk_ud_t    udata;
        H5SL_node_t *     chunk_node;
        hsize_t           select_npoints;
        hbool_t           need_sort = FALSE;

        /* Determine whether partial edge chunks should be filtered */
        filter_partial_edge_chunks = !(io_info->dset->shared->layout.u.chunk.flags & H5O_LAYOUT_CHUNK_DONT_FILTER_PARTIAL_BOUND_CHUNKS);

        if (NULL == (local_info_array = H5MM_malloc(num_chunks_selected * sizeof(*local_info_array))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate local io info array buffer")

        chunk_node = H5SL_first(fm->sel_chunks);
        for (i = 0; chunk_node; i++) {
            chunk_info = (H5D_chunk_info_t *)H5SL_item(chunk_node);

            /* Obtain this chunk's address */
            if (H5D__chunk_lookup(io_info->dset, chunk_info->scaled, &udata) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "error looking up chunk address")

            /* Initialize rank-local chunk info */
            local_info_array[i].chunk_info     = chunk_info;
            local_info_array[i].chunk_buf_size = 0;
            local_info_array[i].num_writers    = 0;
            local_info_array[i].orig_owner     = mpi_rank;
            local_info_array[i].new_owner      = mpi_rank;
            local_info_array[i].buf            = NULL;

            select_npoints              = H5S_GET_SELECT_NPOINTS(chunk_info->fspace);
            local_info_array[i].io_size = (size_t)select_npoints * type_info->dst_type_size;

            if (io_info->op_type == H5D_IO_OP_READ)
                local_info_array[i].need_read = TRUE;
            else {
                /* Currently the full overwrite status of a chunk is only obtained on a per-rank
                 * basis. This means that if the total selection in the chunk, as determined by
                 * the combination of selections of all of the ranks interested in the chunk,
                 * covers the entire chunk, the performance optimization of not reading the chunk
                 * from the file is still valid, but is not applied in the current implementation.
                 * Something like an appropriately placed MPI_Allreduce or a running total of the
                 * number of chunk points selected during chunk redistribution should suffice for
                 * implementing this case - JTH.
                 */
                local_info_array[i].need_read =
                    local_info_array[i].io_size < (size_t)io_info->dset->shared->layout.u.chunk.size;
            }

            local_info_array[i].skip_filter_pline = FALSE;
            if (!filter_partial_edge_chunks) {
                /*
                 * If this is a partial edge chunk and the "don't filter partial edge
                 * chunks" flag is set, make sure not to apply filters to the chunk.
                 */
                if (H5D__chunk_is_partial_edge_chunk(io_info->dset->shared->ndims,
                                                     io_info->dset->shared->layout.u.chunk.dim,
                                                     chunk_info->scaled, io_info->dset->shared->curr_dims))
                    local_info_array[i].skip_filter_pline = TRUE;
            }

            /* Initialize the chunk's shared info */
            local_info_array[i].chunk_current = udata.chunk_block;
            local_info_array[i].chunk_new     = udata.chunk_block;

            /*
             * Check if the list is not in ascending order of offset in the file
             * or has unallocated chunks. In either case, the list should get
             * sorted.
             */
            if (i) {
                haddr_t curr_chunk_offset = local_info_array[i].chunk_current.offset;
                haddr_t prev_chunk_offset = local_info_array[i - 1].chunk_current.offset;

                if (!H5F_addr_defined(prev_chunk_offset) || !H5F_addr_defined(curr_chunk_offset)
                        || (curr_chunk_offset < prev_chunk_offset))
                    need_sort = TRUE;
            }

            /*
             * Extensible arrays may calculate a chunk's index a little differently
             * than normal when the dataset's unlimited dimension is not the
             * slowest-changing dimension, so set the index here based on what the
             * extensible array code calculated instead of what was calculated
             * in the chunk file mapping.
             */
            if (io_info->dset->shared->layout.u.chunk.idx_type == H5D_CHUNK_IDX_EARRAY)
                local_info_array[i].index_info.chunk_idx = udata.chunk_idx;
            else
                local_info_array[i].index_info.chunk_idx = chunk_info->index;

            local_info_array[i].index_info.filter_mask = udata.filter_mask;
            local_info_array[i].index_info.need_insert = FALSE;

            /* Finally, initialize the info for non-blocking MPI calls */
            local_info_array[i].async_info.receive_requests_array = NULL;
            local_info_array[i].async_info.receive_buffer_array   = NULL;
            local_info_array[i].async_info.num_receive_requests   = 0;

            chunk_node = H5SL_next(chunk_node);
        }

        /* Ensure the chunk list is sorted in ascending order of offset in the file */
        if (need_sort)
            HDqsort(local_info_array, num_chunks_selected, sizeof(H5D_filtered_collective_io_info_t),
                    H5D__cmp_filtered_collective_io_info_entry);

#ifdef H5Dmpio_DEBUG
        H5D__mpio_dump_collective_filtered_chunk_list(local_info_array, num_chunks_selected, mpi_rank);
#endif
    }

    *chunk_list  = local_info_array;
    *num_entries = num_chunks_selected;

done:
#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_chunk_io_setup() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_redistribute_shared_chunks
 *
 * Purpose:     When performing a parallel write on a chunked Dataset with
 *              filters applied, we must ensure that any particular chunk
 *              is only written to by a single MPI rank in order to avoid
 *              potential data races on the chunk. This function is used to
 *              redistribute (by assigning ownership to a single rank) any
 *              chunks which are selected by more than one MPI rank.
 *
 *              An initial Allgather is performed to determine how many
 *              chunks each rank has selected in the write operation and
 *              then that number is compared against a threshold value to
 *              determine whether chunk redistribution should be done on
 *              MPI rank 0 only, or on all MPI ranks.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_redistribute_shared_chunks(H5D_filtered_collective_io_info_t *chunk_list,
                                     size_t chunk_list_num_entries, const H5D_io_info_t *io_info,
                                     const H5D_chunk_map_t *fm, int mpi_rank, int mpi_size,
                                     size_t **rank_chunks_assigned_map)
{
    size_t *num_chunks_map = NULL;
    size_t  coll_chunk_list_size = 0;
    size_t  i;
    int     mpi_code;
    herr_t  ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(fm);
    HDassert(mpi_size > 1); /* No chunk sharing is possible for MPI Comm size of 1 */
    HDassert(rank_chunks_assigned_map);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Redistribute shared chunks");
#endif

    /*
     * Allocate an array for each rank to keep track of the number of
     * chunks assigned to any other rank in order to cut down on future
     * MPI communication.
     */
    if (NULL == (num_chunks_map = H5MM_malloc((size_t)mpi_size * sizeof(*num_chunks_map))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't allocate assigned chunks array")

    /* Perform initial Allgather to determine the collective chunk list size */
    if (MPI_SUCCESS != (mpi_code = MPI_Allgather(&chunk_list_num_entries, 1, H5_SIZE_T_AS_MPI_TYPE,
            num_chunks_map, 1, H5_SIZE_T_AS_MPI_TYPE, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code)

    for (i = 0; i < (size_t)mpi_size; i++)
        coll_chunk_list_size += num_chunks_map[i];

    /* Relatively large number of chunks. Redistribute on rank 0 only */
    if (H5D__mpio_redistribute_shared_chunks_p0(chunk_list, num_chunks_map, io_info,
                                                fm, mpi_rank, mpi_size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTREDISTRIBUTE, FAIL, "can't redistribute shared chunks on rank 0")

    *rank_chunks_assigned_map = num_chunks_map;

done:
    if (ret_value < 0) {
        num_chunks_map = H5MM_xfree(num_chunks_map);
    }

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_redistribute_shared_chunks() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_redistribute_shared_chunks_p0
 *
 * Purpose:     Routine to perform redistribution of shared chunks during
 *              parallel writes to datasets with filters applied. This
 *              implementation has MPI rank 0 do all of the chunk
 *              redistribution, which adds a bit of MPI communication
 *              overhead, but should reduce total memory usage as only rank
 *              0 needs to keep a copy of the total chunk list across all
 *              ranks. To this end, this strategy is employed when the
 *              number of chunks being written to is relatively large.
 *
 *              This implementation follows this 2-phase process:
 *
 *              - All MPI ranks send their list of selected chunks to rank
 *                0, then rank 0 sorts this new list in order of chunk
 *                index.
 *
 *              - Rank 0 scans the list looking for matching runs of chunk
 *                index values (corresponding to a shared chunk which
 *                has been selected by more than one rank in the I/O
 *                operation) and for each shared chunk, it redistributes
 *                the chunk to the MPI rank writing to the chunk which
 *                currently has the least amount of chunks assigned to it.
 *                It does this by modifying the "new_owner" field in each
 *                of the list entries corresponding to that chunk. Rank 0
 *                then re-sorts the list in order of previous owner so that
 *                each rank will get back the array that they contributed
 *                to the redistribution operation, with the "new_owner"
 *                field of each chunk they are modifying having possibly
 *                been modified. Rank 0 finally scatters each segment of
 *                the list back to its corresponding rank.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_redistribute_shared_chunks_p0(H5D_filtered_collective_io_info_t *chunk_list,
                                        size_t *num_chunks_assigned_map, const H5D_io_info_t *io_info,
                                        const H5D_chunk_map_t *fm, int mpi_rank, int mpi_size)
{
    MPI_Datatype struct_type;
    MPI_Datatype packed_type;
    hbool_t      struct_type_derived = FALSE;
    hbool_t      packed_type_derived = FALSE;
    size_t       i;
    size_t       coll_chunk_list_num_entries = 0;
    void *       coll_chunk_list         = NULL;
    int *        recv_counts_disps_array = NULL;
    int *        rank_info               = NULL;
    int *        send_counts_ptr         = NULL;
    int *        displacements_ptr       = NULL;
    int *        num_chunks_assigned_ptr = NULL;
    int          num_chunks_int;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(num_chunks_assigned_map);
    HDassert(chunk_list || 0 == num_chunks_assigned_map[mpi_rank]);
    HDassert(io_info);
    HDassert(fm);
    HDassert(mpi_size > 1);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Redistribute shared chunks (p0 only)");
#endif

    /*
     * Make sure it's safe to cast this rank's number
     * of chunks to be sent into an int for MPI
     */
    H5_CHECKED_ASSIGN(num_chunks_int, int, num_chunks_assigned_map[mpi_rank], size_t);

    /*
     * Phase 1 - Participate in collective gathering of every rank's
     * list of chunks to rank 0 to allow it to perform the redistribution
     * operation.
     */

    if (mpi_rank == 0) {
        /*
         * Allocate array to store the receive counts of each rank, as well as
         * the displacements into the final array where each rank will place
         * their data. The first half of the array contains the receive counts
         * (in rank order), while the latter half contains the displacements
         * (also in rank order).
         */
        if (NULL == (recv_counts_disps_array = H5MM_malloc(2 * (size_t)mpi_size * sizeof(*recv_counts_disps_array)))) {
            /* Push an error, but still participate in collective gather operation */
            HDONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                        "couldn't allocate receive counts and displacements array")
        }
        else {
            /* Set the receive counts from the assigned chunks map */
            for (i = 0; i < (size_t)mpi_size; i++)
                H5_CHECKED_ASSIGN(recv_counts_disps_array[i], int, num_chunks_assigned_map[i], size_t);

            /* Set the displacements into the receive buffer for the gather operation */
            displacements_ptr = &recv_counts_disps_array[mpi_size];

            *displacements_ptr = 0;
            for (i = 1; i < (size_t)mpi_size; i++)
                displacements_ptr[i] = displacements_ptr[i - 1] + recv_counts_disps_array[i - 1];
        }
    }

    /*
     * Construct MPI derived types for extracting information
     * necessary for MPI communication
     */
    if (H5D__mpio_get_chunk_redistribute_info_types(&packed_type, &packed_type_derived, &struct_type,
                                                    &struct_type_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                    "can't create derived datatypes for chunk redistribution info")

    /* Perform gather operation */
    if (H5_mpio_gatherv_alloc(chunk_list, num_chunks_int, struct_type, recv_counts_disps_array,
                              &recv_counts_disps_array[mpi_size], packed_type, FALSE, 0,
                              io_info->comm, mpi_rank, mpi_size, &coll_chunk_list,
                              &coll_chunk_list_num_entries) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGATHER, FAIL, "can't gather chunk redistribution info to rank 0")

    /*
     * Phase 2 - Rank 0 now redistributes any shared chunks to new
     * owners as necessary.
     */

    if (mpi_rank == 0) {
        /* Sort collective chunk list according to chunk index */
        HDqsort(coll_chunk_list, coll_chunk_list_num_entries,
                sizeof(H5D_chunk_redistribute_info_t),
                H5D__cmp_chunk_redistribute_info);

        /* Allocate an array to hold info about other ranks for rank 0's bookkeeping.
         * This array contains the amount of data to send back to each rank, the
         * displacements in the total array for each rank's piece of contributed data
         * and the number of chunks assigned to a particular rank. It can be visualized
         * as three consecutive arrays in one, with each sub-array ordered by increasing
         * MPI rank value, e.g.:
         *
         * [send_count0][...][send_countN][displacement0][...][displacementN][num_chunks0][...][num_chunksN]
         */
        if (NULL == (rank_info = H5MM_calloc(3 * (size_t)mpi_size * sizeof(*rank_info)))) {
            /* Push an error, but still participate in following collective Scatterv call */
            HDONE_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                        "unable to allocate MPI rank info bookkeeping array")
        }
        else {
            H5D_chunk_redistribute_info_t *chunk_entry;
            hsize_t                        curr_chunk_idx;
            size_t                         set_begin_index;
            int                            num_writers;
            int                            new_chunk_owner;

            /* Setup indexing into rank info bookkeeping array */
            send_counts_ptr         = &rank_info[0];
            displacements_ptr       = &rank_info[mpi_size];
            num_chunks_assigned_ptr = &rank_info[2 * mpi_size];

            /* Process all chunks in the collective chunk list */
            chunk_entry = &((H5D_chunk_redistribute_info_t *)coll_chunk_list)[0];
            for (i = 0; i < coll_chunk_list_num_entries;) {
                /* Set chunk's initial new owner to its original owner */
                new_chunk_owner = chunk_entry->orig_owner;

                /*
                 * Set the current chunk index so we know when we've processed
                 * all duplicate entries for a particular shared chunk
                 */
                curr_chunk_idx = chunk_entry->chunk_idx;

                /* Reset the initial number of writers to this chunk */
                num_writers = 0;

                /* Set index for the beginning of this section of duplicate chunk entries */
                set_begin_index = i;

                /*
                 * Process each chunk entry in the set for the current
                 * (possibly shared) chunk
                 */
                do {
                    /*
                     * The new owner of the chunk is determined by the rank
                     * writing to the chunk which currently has the least amount
                     * of chunks assigned to it
                     */
                    if (num_chunks_assigned_ptr[chunk_entry->orig_owner] <
                        num_chunks_assigned_ptr[new_chunk_owner])
                        new_chunk_owner = chunk_entry->orig_owner;

                    /* Update the amount of data being sent back to the rank that owns this chunk entry */
                    send_counts_ptr[chunk_entry->orig_owner]++;

                    /* Update the number of writers to this particular chunk */
                    num_writers++;

                    chunk_entry++;
                } while (++i < coll_chunk_list_num_entries &&
                        chunk_entry->chunk_idx == curr_chunk_idx);

                /* We should never have more writers to a chunk than the number of MPI ranks */
                HDassert(num_writers <= mpi_size);

                /* Set all processed chunk entries' "new_owner" and "num_writers" fields */
                for (; set_begin_index < i; set_begin_index++) {
                    H5D_chunk_redistribute_info_t *entry;

                    entry = &((H5D_chunk_redistribute_info_t *)coll_chunk_list)[set_begin_index];

                    entry->new_owner   = new_chunk_owner;
                    entry->num_writers = num_writers;
                }

                /* Update the number of chunks assigned to the MPI rank that now owns this chunk */
                num_chunks_assigned_ptr[new_chunk_owner]++;
            }

            /* Update the mapping from rank number -> number of assigned chunks */
            for (i = 0; i < (size_t)mpi_size; i++)
                num_chunks_assigned_map[i] = (size_t)num_chunks_assigned_ptr[i];

            /*
             * Re-sort the collective chunk list in order of previous owner so
             * that each original owner of a chunk entry gets that entry back,
             * with the possibly newly-modified "new_owner" and "num_writers"
             * fields.
             */
            HDqsort(coll_chunk_list, coll_chunk_list_num_entries,
                    sizeof(H5D_chunk_redistribute_info_t),
                    H5D__cmp_chunk_redistribute_info_orig_owner);

            /*
             * Setup displacements for MPI_Scatterv to scatter each
             * rank's contributed piece of the array back to them
             */
            displacements_ptr[0] = 0;
            for (i = 1; i < (size_t)mpi_size; i++)
                displacements_ptr[i] = displacements_ptr[i - 1] + send_counts_ptr[i - 1];
        }
    }

    /* Scatter the segments of the collective chunk list back to each rank */
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Scatterv(coll_chunk_list, send_counts_ptr, displacements_ptr, packed_type,
                                 chunk_list, num_chunks_int, struct_type, 0, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "unable to scatter shared chunks info buffer", mpi_code)

    /* Distribute rank number -> assigned chunks mapping back to all ranks */
    if (MPI_SUCCESS != (mpi_code = MPI_Bcast(num_chunks_assigned_map, mpi_size, H5_SIZE_T_AS_MPI_TYPE,
                                             0, io_info->comm)))
        HMPI_GOTO_ERROR(FAIL, "couldn't broadcast chunk mapping to other ranks", mpi_code)

#ifdef H5Dmpio_DEBUG
    H5D__mpio_dump_collective_filtered_chunk_list(chunk_list, num_chunks_assigned_map[mpi_rank], mpi_rank);
#endif

done:
    H5MM_free(rank_info);
    H5MM_free(coll_chunk_list);

    if (struct_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&struct_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (packed_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&packed_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

    H5MM_free(recv_counts_disps_array);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_redistribute_shared_chunks_p0() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_share_chunk_modification_data
 *
 * Purpose:     When performing a parallel write on a chunked dataset with
 *              filters applied, we must first ensure that any particular
 *              chunk is only written to by a single MPI rank in order to
 *              avoid potential data races on the chunk. Once dataset
 *              chunks have been redistributed in a suitable manner, each
 *              MPI rank must scan through its list of selected chunks and
 *              do one of the following:
 *
 *               * If a chunk in the MPI rank's chunk list is no longer
 *                 owned by that rank, the rank must send the data it
 *                 wishes to update the chunk with to the MPI rank that
 *                 now has ownership of that chunk. To do this, it encodes
 *                 its selection in the chunk and its modification data
 *                 into a buffer and then posts a non-blocking MPI_Isend.
 *               * Accordingly, if a chunk in the MPI rank's chunk list is
 *                 owned by that rank, the rank must receive any chunk
 *                 modification data from other MPI ranks that are updating
 *                 that chunk. To do this, it probes for messages to
 *                 receive using MPI_Mprobe with the chunk index as the
 *                 tag. Then, for each message to be received it allocates
 *                 a buffer for the message and posts a non-blocking
 *                 MPI_Imrecv for that message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_share_chunk_modification_data(H5D_filtered_collective_io_info_t *chunk_list,
                                        size_t *chunk_list_num_entries, H5D_io_info_t *io_info,
                                        const H5D_type_info_t *type_info, int mpi_rank, int mpi_size)
{
#if MPI_VERSION >= 3
    H5S_sel_iter_t *mem_iter          = NULL;
    unsigned char **chunk_mod_data    = NULL;
    MPI_Request *   send_requests     = NULL;
    hbool_t         mem_iter_init     = FALSE;
    size_t          num_send_requests = 0;
    size_t          last_assigned_idx;
    size_t          i;
    int             mpi_code;
    herr_t          ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list_num_entries);
    HDassert(chunk_list || 0 == *chunk_list_num_entries);
    HDassert(io_info);
    HDassert(type_info);
    HDassert(mpi_size > 1);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Share chunk modification data");
#endif

    /* Set to latest format for encoding dataspace */
    H5CX_set_libver_bounds(NULL);

    /* Allocate a selection iterator for iterating over chunk dataspaces */
    if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate dataspace selection iterator")

    if (*chunk_list_num_entries) {
        /* Allocate an array of buffers to hold the modification data for
         * each chunk that this rank is sending out data for
         */
        if (NULL == (chunk_mod_data = H5MM_malloc(*chunk_list_num_entries * sizeof(*chunk_mod_data))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                        "unable to allocate chunk modification data buffer array")

        /* Allocate an array to hold the MPI_Request objects for each non-blocking
         * send request made
         */
        if (NULL == (send_requests = H5MM_malloc(*chunk_list_num_entries * sizeof(*send_requests))))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate send requests buffer")
    }

    /* Perform all the sends on the chunks that this rank doesn't own.
     * Here, the sends and recvs are posted in two separate loops to
     * avoid a deadlock
     */
    for (i = 0; i < *chunk_list_num_entries; i++) {
        H5D_filtered_collective_io_info_t *chunk_entry = &chunk_list[i];

        if (mpi_rank != chunk_entry->new_owner) {
            H5D_chunk_info_t *chunk_info = chunk_entry->chunk_info;
            unsigned char *   mod_data_p = NULL;
            hsize_t           iter_nelmts;
            size_t            mod_data_size;

            /* Determine size of serialized chunk file dataspace, plus the
             * size of the data being written
             */
            if (H5S_encode(chunk_info->fspace, &mod_data_p, &mod_data_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "unable to get encoded dataspace size")

            iter_nelmts = H5S_GET_SELECT_NPOINTS(chunk_info->mspace);

            H5_CHECK_OVERFLOW(iter_nelmts, hsize_t, size_t);
            mod_data_size += (size_t)iter_nelmts * type_info->src_type_size;

            if (NULL == (chunk_mod_data[num_send_requests] = H5MM_malloc(mod_data_size)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                            "couldn't allocate chunk modification send buffer")

            /* Serialize the chunk's file dataspace into the buffer */
            mod_data_p = chunk_mod_data[num_send_requests];
            if (H5S_encode(chunk_info->fspace, &mod_data_p, &mod_data_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTENCODE, FAIL, "unable to encode dataspace")

            /* Initialize iterator for memory selection */
            if (H5S_select_iter_init(mem_iter, chunk_info->mspace, type_info->src_type_size,
                                     H5S_SEL_ITER_SHARE_WITH_DATASPACE) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                            "unable to initialize memory selection information")
            mem_iter_init = TRUE;

            /* Collect the modification data into the buffer */
            if (0 == H5D__gather_mem(io_info->u.wbuf, mem_iter, (size_t)iter_nelmts, mod_data_p))
                HGOTO_ERROR(H5E_IO, H5E_CANTGATHER, FAIL, "couldn't gather from write buffer")

            /* Send modification data to new owner */
            H5_CHECK_OVERFLOW(mod_data_size, size_t, int)
            H5_CHECK_OVERFLOW(chunk_entry->index_info.chunk_idx, hsize_t, int)
            if (MPI_SUCCESS !=
                (mpi_code = MPI_Isend(chunk_mod_data[num_send_requests], (int)mod_data_size, MPI_BYTE,
                                      chunk_entry->new_owner, (int)chunk_entry->index_info.chunk_idx,
                                      io_info->comm, &send_requests[num_send_requests])))
                HMPI_GOTO_ERROR(FAIL, "MPI_Isend failed", mpi_code)

            if (H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "couldn't release memory selection iterator")
            mem_iter_init = FALSE;

            num_send_requests++;
        }
    }

    /* Perform all the recvs on the chunks this rank DOES own */
    for (i = 0, last_assigned_idx = 0; i < *chunk_list_num_entries; i++) {
        H5D_filtered_collective_io_info_t *chunk_entry = &chunk_list[i];

        if (mpi_rank == chunk_entry->new_owner) {
            if (chunk_entry->num_writers > 1) {
                MPI_Message message;
                MPI_Status  status;
                size_t      j;

                /* Allocate all necessary buffers for a non-blocking receive operation */
                chunk_entry->async_info.num_receive_requests = chunk_entry->num_writers - 1;
                if (NULL == (chunk_entry->async_info.receive_requests_array = H5MM_malloc(
                                 (size_t)chunk_entry->async_info.num_receive_requests * sizeof(MPI_Request))))
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate MPI requests array")

                if (NULL ==
                    (chunk_entry->async_info.receive_buffer_array = H5MM_malloc(
                         (size_t)chunk_entry->async_info.num_receive_requests * sizeof(unsigned char *))))
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate receive buffers")

                for (j = 0; j < (size_t)chunk_entry->num_writers - 1; j++) {
                    int count = 0;

                    /* Probe for a particular message for this chunk from any rank, removing
                     * that message from the receive queue for the rank and allocating that
                     * much memory for the non-blocking receive
                     */
                    if (MPI_SUCCESS !=
                        (mpi_code = MPI_Mprobe(MPI_ANY_SOURCE, (int)chunk_entry->index_info.chunk_idx,
                                               io_info->comm, &message, &status)))
                        HMPI_GOTO_ERROR(FAIL, "MPI_Mprobe failed", mpi_code)

                    if (MPI_SUCCESS != (mpi_code = MPI_Get_count(&status, MPI_BYTE, &count)))
                        HMPI_GOTO_ERROR(FAIL, "MPI_Get_count failed", mpi_code)

                    HDassert(count >= 0);
                    if (NULL == (chunk_entry->async_info.receive_buffer_array[j] =
                                     H5MM_malloc((size_t)count * sizeof(char))))
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL,
                                    "unable to allocate modification data receive buffer")

                    if (MPI_SUCCESS != (mpi_code = MPI_Imrecv(
                                            chunk_entry->async_info.receive_buffer_array[j], count, MPI_BYTE,
                                            &message, &chunk_entry->async_info.receive_requests_array[j])))
                        HMPI_GOTO_ERROR(FAIL, "MPI_Imrecv failed", mpi_code)
                }
            }

            /* Overwrite chunk entries this rank doesn't own with entries that it
             * does own, since it has sent the necessary data and is no longer
             * interested in the chunks it doesn't own.
             */
            chunk_list[last_assigned_idx++] = chunk_list[i];
        }
    }

    /* Set the new number of locally-selected chunks */
    *chunk_list_num_entries = last_assigned_idx;

done:
    /* Wait for all non-blocking send requests to complete before returning */
    if (num_send_requests) {
        H5_CHECK_OVERFLOW(num_send_requests, size_t, int);
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Waitall((int)num_send_requests, send_requests, MPI_STATUSES_IGNORE)))
            HMPI_DONE_ERROR(FAIL, "MPI_Waitall failed", mpi_code)
    } /* end if */

    if (send_requests)
        H5MM_free(send_requests);

    /* Now that all send requests have completed, free up the send
     * buffers used in the non-blocking operations
     */
    if (chunk_mod_data) {
        for (i = 0; i < num_send_requests; i++) {
            if (chunk_mod_data[i])
                H5MM_free(chunk_mod_data[i]);
        }

        H5MM_free(chunk_mod_data);
    }

    if (mem_iter) {
        if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "couldn't release dataspace selection iterator")
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);
    }

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
#else
    FUNC_ENTER_STATIC
    HERROR(H5E_DATASET, H5E_WRITEERROR,
           "unable to send chunk modification data between MPI ranks - MPI version < 3 (MPI_Mprobe and "
           "MPI_Imrecv missing)")
    FUNC_LEAVE_NOAPI(FAIL)
#endif
} /* end H5D__mpio_share_chunk_modification_data() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_common_io
 *
 * Purpose:     This routine performs the common part of collective I/O
 *              when reading or writing filtered chunks collectively.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_common_io(H5D_filtered_collective_io_info_t *chunk_list,
                                              size_t chunk_list_num_entries, const H5D_io_info_t *io_info,
                                              const H5D_type_info_t *type_info)
{
    H5D_io_info_t coll_io_info;
    H5D_storage_t ctg_store;
    MPI_Datatype  file_type            = MPI_DATATYPE_NULL;
    MPI_Datatype  mem_type             = MPI_DATATYPE_NULL;
    hbool_t       mem_type_is_derived  = FALSE;
    hbool_t       file_type_is_derived = FALSE;
    hsize_t       mpi_buf_count;
    haddr_t       base_read_offset = HADDR_UNDEF;
    size_t        num_chunks;
    size_t        i;
    char          fake_buf; /* Used as a fake buffer for ranks with no chunks, thus a NULL buf pointer */
    int           mpi_code;
    herr_t        ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(type_info);

    /* Initialize temporary I/O info */
    coll_io_info = *io_info;

    /*
     * Construct MPI derived datatype for collective I/O on chunks
     */
    if (H5D__mpio_collective_filtered_io_type(chunk_list, chunk_list_num_entries, io_info->op_type, &mem_type,
                                              &mem_type_is_derived, &file_type, &file_type_is_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_BADTYPE, FAIL, "couldn't create MPI I/O type for chunk I/O")

    /*
     * For reads, determine how many chunks are actually being read.
     * Note that if this is a read during a write operation
     * (read chunk -> unfilter -> modify -> write back), some
     * chunks may not need to be read if they're being fully
     * overwritten during a write operation.
     */
    if (io_info->op_type == H5D_IO_OP_READ) {
        for (i = 0, num_chunks = 0; i < chunk_list_num_entries; i++) {
            HDassert(chunk_list[i].buf);

            if (chunk_list[i].need_read) {
                if (!H5F_addr_defined(base_read_offset))
                    base_read_offset = chunk_list[i].chunk_current.offset;

                num_chunks++;
            }
        }
    }
    else
        num_chunks = chunk_list_num_entries;

    /*
     * If this rank doesn't have a selection, it probably has a
     * NULL buf pointer, so override to a fake buffer since our
     * write/read function expects one.
     */
    if (num_chunks == 0) {
        if (io_info->op_type == H5D_IO_OP_WRITE)
            coll_io_info.u.wbuf = &fake_buf;
        else
            coll_io_info.u.rbuf = &fake_buf;
    }

    /*
     * Setup for I/O operation
     */

    mpi_buf_count = (num_chunks) ? 1 : 0;

    if (num_chunks) {
        /*
         * Setup the base storage address for this operation
         * to be the first chunk's file address
         */
        if (io_info->op_type == H5D_IO_OP_WRITE)
            ctg_store.contig.dset_addr = chunk_list[0].chunk_new.offset;
        else
            ctg_store.contig.dset_addr = base_read_offset;
    }
    else
        ctg_store.contig.dset_addr = 0;

    ctg_store.contig.dset_size = (hsize_t)io_info->dset->shared->layout.u.chunk.size;
    coll_io_info.store         = &ctg_store;

    /* Perform I/O */
    if (H5D__final_collective_io(&coll_io_info, type_info, mpi_buf_count, file_type, mem_type) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't finish MPI I/O")

done:
    /* Free the MPI buf and file types, if they were derived */
    if (mem_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&mem_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    if (file_type_is_derived && MPI_SUCCESS != (mpi_code = MPI_Type_free(&file_type)))
        HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_chunk_common_io() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_read
 *
 * Purpose:     This routine coordinates a collective read across all ranks
 *              of the chunks they have selected. Each rank will then go
 *              and
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_read(H5D_filtered_collective_io_info_t *chunk_list,
                                         size_t chunk_list_num_entries, const H5D_io_info_t *io_info,
                                         const H5D_type_info_t *type_info, int mpi_rank)
{
    H5D_fill_buf_info_t fb_info;
    H5D_chunk_info_t *  chunk_info = NULL;
    H5D_io_info_t       coll_io_info;
    H5Z_EDC_t           err_detect; /* Error detection info */
    H5Z_cb_t            filter_cb;  /* I/O filter callback function */
    hsize_t             file_chunk_size = 0;
    hsize_t             iter_nelmts; /* Number of points to iterate over for the chunk IO operation */
    hbool_t             should_fill  = FALSE;
    hbool_t             fb_info_init = FALSE;
    size_t              i;
    H5S_t *             fill_space    = NULL;
    void *              base_read_buf = NULL;
    herr_t              ret_value     = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(type_info);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Filtered collective chunk read");
#else
    (void)mpi_rank;
#endif

    /* Initialize temporary I/O info */
    coll_io_info = *io_info;
    coll_io_info.u.rbuf = NULL;

    if (chunk_list_num_entries) {
        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info")
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function")

        /* Set size of full chunks in dataset */
        file_chunk_size = io_info->dset->shared->layout.u.chunk.size;

        /* Determine if fill values should be "read" for unallocated chunks */
        should_fill = (io_info->dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC)
                || ( (io_info->dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET) &&
                        io_info->dset->shared->dcpl_cache.fill.fill_defined);

        if (should_fill) {
            hsize_t chunk_dims[H5S_MAX_RANK];

            HDassert(io_info->dset->shared->ndims == io_info->dset->shared->layout.u.chunk.ndims - 1);
            for (i = 0; i < io_info->dset->shared->layout.u.chunk.ndims - 1; i++)
                chunk_dims[i] = (hsize_t)io_info->dset->shared->layout.u.chunk.dim[i];

            /* Get a dataspace for filling chunk memory buffers */
            if (NULL == (fill_space = H5S_create_simple(io_info->dset->shared->layout.u.chunk.ndims - 1,
                    chunk_dims, NULL)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create chunk fill dataspace")

            /* Initialize fill value buffer */
            if (H5D__fill_init(&fb_info, NULL, (H5MM_allocate_t)H5D__chunk_mem_alloc,
                    (void *)&io_info->dset->shared->dcpl_cache.pline, (H5MM_free_t)H5D__chunk_mem_free,
                    (void *)&io_info->dset->shared->dcpl_cache.pline, &io_info->dset->shared->dcpl_cache.fill,
                    io_info->dset->shared->type, io_info->dset->shared->type_id, 0, file_chunk_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize fill value buffer")

            fb_info_init = TRUE;
        }
    }

    /*
     * Allocate memory buffers for all chunks being read. Chunk data buffers are of
     * the largest size between the chunk's current filtered size and the chunk's true
     * size, as calculated by the number of elements in the chunk's file space extent
     * multiplied by the datatype size. This tries to ensure that:
     *
     *  * If we're reading the chunk and the filter normally reduces the chunk size,
     *    the unfiltering operation won't need to grow the buffer.
     *  * If we're reading the chunk and the filter normally grows the chunk size,
     *    we make sure to read into a buffer of size equal to the filtered chunk's
     *    size; reading into a (smaller) buffer of size equal to the unfiltered
     *    chunk size would of course be bad.
     */
    for (i = 0; i < chunk_list_num_entries; i++) {
        HDassert(chunk_list[i].need_read);

        chunk_list[i].chunk_buf_size = MAX(chunk_list[i].chunk_current.length, file_chunk_size);

        if (NULL == (chunk_list[i].buf = H5MM_malloc(chunk_list[i].chunk_buf_size))) {
            /* Push an error, but participate in collective read */
            HDONE_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk data buffer")
            break;
        }

        /*
         * Check if chunk is currently allocated. If not, don't try to
         * read it from the file. Instead, just fill the chunk buffer
         * with the fill value if necessary.
         */
        if (H5F_addr_defined(chunk_list[i].chunk_current.offset)) {
            /* Set first read buffer */
            if (!base_read_buf)
                base_read_buf = chunk_list[i].buf;

            /* Set chunk's new length for eventual filter pipeline calls */
            if (chunk_list[i].skip_filter_pline)
                chunk_list[i].chunk_new.length = file_chunk_size;
            else
                chunk_list[i].chunk_new.length = chunk_list[i].chunk_current.length;
        }
        else {
            chunk_list[i].need_read = FALSE;

            /* Set chunk's new length for eventual filter pipeline calls */
            chunk_list[i].chunk_new.length = file_chunk_size;

            if (should_fill) {
                /* Write fill value to memory buffer */
                HDassert(fb_info.fill_buf);
                if (H5D__fill(fb_info.fill_buf, io_info->dset->shared->type, chunk_list[i].buf,
                        type_info->mem_type, fill_space) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't fill chunk buffer with fill value")
            }
        }
    }

    /*
     * Override the read buffer to point to the address of
     * the first chunk data buffer being read into
     */
    if (base_read_buf)
        coll_io_info.u.rbuf = base_read_buf;

    /* Perform collective chunk read */
    if (H5D__mpio_collective_filtered_chunk_common_io(chunk_list, chunk_list_num_entries, &coll_io_info,
                                                      type_info) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't finish collective filtered chunk read")

    /*
     * Iterate through all the read chunks, unfiltering them and scattering their
     * data out to the application's read buffer.
     */
    for (i = 0; i < chunk_list_num_entries; i++) {
        chunk_info = chunk_list[i].chunk_info;

        /* Unfilter the chunk, unless we didn't read it from the file */
        if (chunk_list[i].need_read && !chunk_list[i].skip_filter_pline) {
            if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, H5Z_FLAG_REVERSE,
                             &(chunk_list[i].index_info.filter_mask), err_detect, filter_cb,
                             (size_t *)&chunk_list[i].chunk_new.length, &chunk_list[i].chunk_buf_size,
                             &chunk_list[i].buf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "couldn't unfilter chunk for modifying")
        }

        /* Scatter the chunk data to the read buffer */
        iter_nelmts = H5S_GET_SELECT_NPOINTS(chunk_info->fspace);

        if (H5D_select_io_mem(io_info->u.rbuf, chunk_info->mspace, chunk_list[i].buf, chunk_info->fspace,
                              type_info->src_type_size, (size_t)iter_nelmts) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_READERROR, FAIL, "couldn't copy chunk data to read buffer")
    }

done:
    /* Free all resources used by entries in the chunk list */
    for (i = 0; i < chunk_list_num_entries; i++) {
        if (chunk_list[i].buf) {
            H5MM_free(chunk_list[i].buf);
            chunk_list[i].buf = NULL;
        }
    }

    /* Release the fill buffer info, if it's been initialized */
    if (fb_info_init && H5D__fill_term(&fb_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release fill buffer info")
    if (fill_space && (H5S_close(fill_space) < 0))
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "can't close fill space")

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_chunk_read() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_update
 *
 * Purpose:     When performing a parallel write on a chunked dataset with
 *              filters applied, all ranks must update their owned chunks
 *              with their own modification data and data from other ranks.
 *              This routine is responsible for coordinating that process.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_update(H5D_filtered_collective_io_info_t *chunk_list,
                                           size_t chunk_list_num_entries, const H5D_io_info_t *io_info,
                                           const H5D_type_info_t *type_info, int mpi_rank)
{
    H5D_fill_buf_info_t fb_info;
    H5D_chunk_info_t *  chunk_info = NULL;
    H5S_sel_iter_t *    sel_iter   = NULL; /* Dataspace selection iterator for H5D__scatter_mem */
    H5D_io_info_t       coll_io_info;
    H5Z_EDC_t           err_detect; /* Error detection info */
    H5Z_cb_t            filter_cb;  /* I/O filter callback function */
    hsize_t             file_chunk_size = 0;
    hsize_t             iter_nelmts; /* Number of points to iterate over for the chunk IO operation */
    hbool_t             should_fill   = FALSE;
    hbool_t             fb_info_init  = FALSE;
    hbool_t             sel_iter_init = FALSE;
    size_t              i, j;
    H5S_t *             dataspace     = NULL;
    H5S_t *             fill_space    = NULL;
    void *              base_read_buf = NULL;
    int                 mpi_code;
    herr_t              ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(type_info);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Filtered collective chunk update");
#endif

    /* Initialize temporary I/O info */
    coll_io_info         = *io_info;
    coll_io_info.op_type = H5D_IO_OP_READ;

    if (chunk_list_num_entries) {
        /* Retrieve filter settings from API context */
        if (H5CX_get_err_detect(&err_detect) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get error detection info")
        if (H5CX_get_filter_cb(&filter_cb) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't get I/O filter callback function")

        /* Set size of full chunks in dataset */
        file_chunk_size = io_info->dset->shared->layout.u.chunk.size;

        /* Determine if fill values should be written to chunks */
        should_fill = (io_info->dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_ALLOC)
                || ( (io_info->dset->shared->dcpl_cache.fill.fill_time == H5D_FILL_TIME_IFSET) &&
                        io_info->dset->shared->dcpl_cache.fill.fill_defined);

        if (should_fill) {
            hsize_t chunk_dims[H5S_MAX_RANK];

            HDassert(io_info->dset->shared->ndims == io_info->dset->shared->layout.u.chunk.ndims - 1);
            for (i = 0; i < io_info->dset->shared->layout.u.chunk.ndims - 1; i++)
                chunk_dims[i] = (hsize_t)io_info->dset->shared->layout.u.chunk.dim[i];

            /* Get a dataspace for filling chunk memory buffers */
            if (NULL == (fill_space = H5S_create_simple(io_info->dset->shared->layout.u.chunk.ndims - 1,
                    chunk_dims, NULL)))
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to create chunk fill dataspace")

            /* Initialize fill value buffer */
            if (H5D__fill_init(&fb_info, NULL, (H5MM_allocate_t)H5D__chunk_mem_alloc,
                    (void *)&io_info->dset->shared->dcpl_cache.pline, (H5MM_free_t)H5D__chunk_mem_free,
                    (void *)&io_info->dset->shared->dcpl_cache.pline, &io_info->dset->shared->dcpl_cache.fill,
                    io_info->dset->shared->type, io_info->dset->shared->type_id, 0, file_chunk_size) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize fill value buffer")

            fb_info_init = TRUE;
        }
    }

    /*
     * Allocate memory buffers for all owned chunks. Chunk data buffers are of the
     * largest size between the chunk's current filtered size and the chunk's true
     * size, as calculated by the number of elements in the chunk's file space extent
     * multiplied by the datatype size. This tries to ensure that:
     *
     *  * If we're fully overwriting the chunk and the filter normally reduces the
     *    chunk size, we simply have the exact buffer size required to hold the
     *    unfiltered chunk data.
     *  * If we're fully overwriting the chunk and the filter normally grows the
     *    chunk size (e.g., fletcher32 filter), the final filtering operation
     *    (hopefully) won't need to grow the buffer.
     *  * If we're reading the chunk and the filter normally reduces the chunk size,
     *    the unfiltering operation won't need to grow the buffer.
     *  * If we're reading the chunk and the filter normally grows the chunk size,
     *    we make sure to read into a buffer of size equal to the filtered chunk's
     *    size; reading into a (smaller) buffer of size equal to the unfiltered
     *    chunk size would of course be bad.
     */
    for (i = 0; i < chunk_list_num_entries; i++) {
        if (mpi_rank == chunk_list[i].new_owner) {
            chunk_list[i].chunk_buf_size =
                MAX(chunk_list[i].chunk_current.length, file_chunk_size);

            /*
             * If this chunk hasn't been allocated yet and we aren't writing
             * out fill values to it, make sure to 0-fill its memory buffer
             * so we don't use uninitialized memory.
             */
            if (!H5F_addr_defined(chunk_list[i].chunk_current.offset) && !should_fill)
                chunk_list[i].buf = H5MM_calloc(chunk_list[i].chunk_buf_size);
            else
                chunk_list[i].buf = H5MM_malloc(chunk_list[i].chunk_buf_size);

            if (NULL == chunk_list[i].buf) {
                /* Push an error, but participate in collective read */
                HDONE_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate chunk data buffer")
                break;
            }

            /* Set chunk's new length for eventual filter pipeline calls */
            if (chunk_list[i].need_read) {
                /*
                 * Check if chunk is currently allocated. If not, don't try to
                 * read it from the file. Instead, just fill the chunk buffer
                 * with the fill value if fill values are to be written.
                 */
                if (H5F_addr_defined(chunk_list[i].chunk_current.offset)) {
                    /* Set first read buffer */
                    if (!base_read_buf)
                        base_read_buf = chunk_list[i].buf;

                    /* Set chunk's new length for eventual filter pipeline calls */
                    if (chunk_list[i].skip_filter_pline)
                        chunk_list[i].chunk_new.length = file_chunk_size;
                    else
                        chunk_list[i].chunk_new.length = chunk_list[i].chunk_current.length;
                }
                else {
                    chunk_list[i].need_read = FALSE;

                    /* Set chunk's new length for eventual filter pipeline calls */
                    chunk_list[i].chunk_new.length = file_chunk_size;

                    if (should_fill) {
                        /* Write fill value to memory buffer */
                        HDassert(fb_info.fill_buf);
                        if (H5D__fill(fb_info.fill_buf, io_info->dset->shared->type, chunk_list[i].buf,
                                type_info->mem_type, fill_space) < 0)
                            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "couldn't fill chunk buffer with fill value")
                    }
                }
            }
            else
                chunk_list[i].chunk_new.length = file_chunk_size;
        }
    }

    /*
     * Setup for I/O operation
     */

    /* Override the read buffer to point to the address of the first
     * chunk data buffer being read into
     */
    if (base_read_buf)
        coll_io_info.u.rbuf = base_read_buf;

    if (H5D__mpio_collective_filtered_chunk_common_io(chunk_list, chunk_list_num_entries, &coll_io_info,
                                                      type_info) < 0)
        HGOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't finish collective filtered chunk read")

    /* Allocate iterator for memory selection */
    if (NULL == (sel_iter = H5FL_MALLOC(H5S_sel_iter_t)))
        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "couldn't allocate memory iterator")

    /*
     * Now that all owned chunks have been read, iterate through them
     * and update them along the way.
     */
    for (i = 0; i < chunk_list_num_entries; i++) {
        if (mpi_rank == chunk_list[i].new_owner) {
            chunk_info = chunk_list[i].chunk_info;

            /* If this chunk wasn't being fully overwritten, we read it from
             * the file, so we need to unfilter it
             */
            if (chunk_list[i].need_read && !chunk_list[i].skip_filter_pline) {
                if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, H5Z_FLAG_REVERSE,
                                 &(chunk_list[i].index_info.filter_mask), err_detect, filter_cb,
                                 (size_t *)&chunk_list[i].chunk_new.length,
                                 &chunk_list[i].chunk_buf_size, &chunk_list[i].buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFILTER, FAIL, "couldn't unfilter chunk for modifying")
            }

            /* Update the chunk with this rank's modification data */
            iter_nelmts = H5S_GET_SELECT_NPOINTS(chunk_info->mspace);

            if (H5D_select_io_mem(chunk_list[i].buf, chunk_info->fspace, io_info->u.wbuf, chunk_info->mspace,
                                  type_info->dst_type_size, (size_t)iter_nelmts) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "couldn't copy chunk data to write buffer")

            /* Wait for all outstanding requests for chunk data receives to complete */
            if (MPI_SUCCESS != (mpi_code = MPI_Waitall(chunk_list[i].async_info.num_receive_requests,
                                                       chunk_list[i].async_info.receive_requests_array,
                                                       MPI_STATUSES_IGNORE)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Waitall failed", mpi_code)

            /* For each non-blocking receive call previously posted, receive the chunk
             * modification buffer from another rank and update the chunk data
             */
            for (j = 0; j < (size_t)chunk_list[i].async_info.num_receive_requests; j++) {
                const unsigned char *mod_data_p;

                /* Decode the rank's chunk file dataspace */
                mod_data_p = chunk_list[i].async_info.receive_buffer_array[j];
                if (NULL == (dataspace = H5S_decode(&mod_data_p)))
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTDECODE, FAIL, "unable to decode dataspace")

                if (H5S_select_iter_init(sel_iter, dataspace, type_info->dst_type_size,
                                         H5S_SEL_ITER_SHARE_WITH_DATASPACE) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL,
                                "unable to initialize memory selection information")
                sel_iter_init = TRUE;

                iter_nelmts = H5S_GET_SELECT_NPOINTS(dataspace);

                /* Update the chunk data with the received modification data */
                if (H5D__scatter_mem(mod_data_p, sel_iter, (size_t)iter_nelmts, chunk_list[i].buf) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_WRITEERROR, FAIL, "couldn't scatter to write buffer")

                if (H5S_SELECT_ITER_RELEASE(sel_iter) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "couldn't release selection iterator")
                sel_iter_init = FALSE;

                if (dataspace) {
                    if (H5S_close(dataspace) < 0)
                        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace")
                    dataspace = NULL;
                }

                H5MM_free(chunk_list[i].async_info.receive_buffer_array[j]);
            }

            /* Finally, filter the chunk */
            if (!chunk_list[i].skip_filter_pline) {
                if (H5Z_pipeline(&io_info->dset->shared->dcpl_cache.pline, 0,
                                 &(chunk_list[i].index_info.filter_mask), err_detect, filter_cb,
                                 (size_t *)&chunk_list[i].chunk_new.length,
                                 &chunk_list[i].chunk_buf_size, &chunk_list[i].buf) < 0)
                    HGOTO_ERROR(H5E_PLINE, H5E_CANTFILTER, FAIL, "output pipeline failed")
            }

#if H5_SIZEOF_SIZE_T > 4
            /* Check for the chunk expanding too much to encode in a 32-bit value */
            if (chunk_list[i].chunk_new.length > ((size_t)0xffffffff))
                HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "chunk too large for 32-bit length")
#endif

            /* Free up resources used by the chunk entry */
            if (chunk_list[i].async_info.receive_buffer_array) {
                H5MM_free(chunk_list[i].async_info.receive_buffer_array);
                chunk_list[i].async_info.receive_buffer_array = NULL;
            }
            if (chunk_list[i].async_info.receive_requests_array) {
                H5MM_free(chunk_list[i].async_info.receive_requests_array);
                chunk_list[i].async_info.receive_requests_array = NULL;
            }
        }
    }

done:
    if (sel_iter) {
        if (sel_iter_init && H5S_SELECT_ITER_RELEASE(sel_iter) < 0)
            HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "couldn't release selection iterator")
        sel_iter = H5FL_FREE(H5S_sel_iter_t, sel_iter);
    }
    if (dataspace && (H5S_close(dataspace) < 0))
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTFREE, FAIL, "can't close dataspace")
    if (fill_space && (H5S_close(fill_space) < 0))
        HDONE_ERROR(H5E_DATASET, H5E_CLOSEERROR, FAIL, "can't close fill space")

    /* Release the fill buffer info, if it's been initialized */
    if (fb_info_init && H5D__fill_term(&fb_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release fill buffer info")

    /* On failure, try to free all resources used by entries in the chunk list */
    if (ret_value < 0) {
        for (i = 0; i < chunk_list_num_entries; i++) {
            if (mpi_rank == chunk_list[i].new_owner) {
                if (chunk_list[i].buf) {
                    H5MM_free(chunk_list[i].buf);
                    chunk_list[i].buf = NULL;
                }
                if (chunk_list[i].async_info.receive_buffer_array) {
                    for (j = 0; j < (size_t)chunk_list[i].async_info.num_receive_requests; j++) {
                        if (chunk_list[i].async_info.receive_buffer_array[j]) {
                            H5MM_free(chunk_list[i].async_info.receive_buffer_array[j]);
                            chunk_list[i].async_info.receive_buffer_array[j] = NULL;
                        }
                    }

                    H5MM_free(chunk_list[i].async_info.receive_buffer_array);
                    chunk_list[i].async_info.receive_buffer_array = NULL;
                }
                if (chunk_list[i].async_info.receive_requests_array) {
                    H5MM_free(chunk_list[i].async_info.receive_requests_array);
                    chunk_list[i].async_info.receive_requests_array = NULL;
                }
            }
        }
    }

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_chunk_update() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_reallocate
 *
 * Purpose:     When performing a parallel write on a chunked dataset with
 *              filters applied, all ranks must eventually get together and
 *              perform a collective reallocation of space in the file for
 *              all chunks that were modified on all ranks. This routine is
 *              responsible for coordinating that process.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_reallocate(H5D_filtered_collective_io_info_t *chunk_list,
                                               size_t chunk_list_num_entries, H5D_io_info_t *io_info,
                                               H5D_chk_idx_info_t *idx_info, int mpi_rank, int mpi_size)
{
    H5D_chunk_alloc_info_t *collective_list = NULL;
    MPI_Datatype            send_type;
    MPI_Datatype            recv_type;
    hbool_t                 send_type_derived      = FALSE;
    hbool_t                 recv_type_derived      = FALSE;
    size_t                  collective_num_entries = 0;
    size_t                  i;
    void *                  gathered_array = NULL;
    int                     mpi_code;
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(idx_info);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Reallocation of chunk file space");
#endif

    /* Create derived datatypes for the chunk file space info needed */
    if (H5D__mpio_get_chunk_alloc_info_types(&recv_type, &recv_type_derived, &send_type,
                                              &send_type_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                    "can't create derived datatypes for chunk file space info")

    /* Gather the new chunk sizes to all ranks for a collective reallocation
     * of the chunks in the file.
     */
    H5_CHECK_OVERFLOW(chunk_list_num_entries, size_t, int);
    if (H5_mpio_gatherv_alloc_simple(chunk_list, (int)chunk_list_num_entries, send_type, recv_type, TRUE, 0,
                                     io_info->comm, mpi_rank, mpi_size, &gathered_array,
                                     &collective_num_entries) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGATHER, FAIL, "can't gather chunk file space info to/from ranks")
    collective_list = (H5D_chunk_alloc_info_t *)gathered_array;

    if (collective_num_entries) {
        H5D_chunk_alloc_info_t *coll_entry;
        hbool_t                 need_insert;
        hbool_t                 need_sort = FALSE;

        /* Collectively re-allocate the modified chunks (from each rank) in the file */
        HDassert(idx_info->storage->idx_type != H5D_CHUNK_IDX_NONE);
        for (i = 0; i < collective_num_entries; i++) {
            coll_entry = &collective_list[i];

            /*
             * Once we find the section of chunks that belong to this rank,
             * do the file space re-allocation for each chunk and keep the
             * local chunk list updated alongside the allocations. This saves
             * a little on post-processing and memory usage.
             */
            if (chunk_list_num_entries &&
                coll_entry->chunk_idx == chunk_list[0].index_info.chunk_idx) {
                size_t num_processed = 0;

                do {
                    coll_entry = &collective_list[i];

                    if (H5D__chunk_file_alloc(idx_info, &coll_entry->chunk_current, &coll_entry->chunk_new,
                                              &chunk_list[num_processed].index_info.need_insert, NULL) < 0)
                        HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")

                    chunk_list[num_processed].chunk_new = coll_entry->chunk_new;

                    /*
                     * Since chunk reallocation can move chunks around, check if
                     * the local chunk list is still in ascending offset of order
                     * in the file
                     */
                    if (num_processed) {
                        haddr_t curr_chunk_offset = chunk_list[num_processed].chunk_new.offset;
                        haddr_t prev_chunk_offset = chunk_list[num_processed - 1].chunk_new.offset;

                        HDassert(H5F_addr_defined(prev_chunk_offset) && H5F_addr_defined(curr_chunk_offset));
                        if (curr_chunk_offset < prev_chunk_offset)
                            need_sort = TRUE;
                    }
                } while ((++num_processed < chunk_list_num_entries) && (++i < collective_num_entries));

                continue;
            }

            /*
             * Re-allocate this chunk, but throw away the "need_insert" status
             * since it's not a chunk we own and therefore the status won't be
             * persistent after this operation. The owning rank will update
             * their local chunk list above, so when we get back together for
             * the collective chunk re-insertion, everyone will get the correct
             * status for each chunk. We could keep a persistent array around
             * to track the status for every chunk on every rank, but there's
             * no need to pressure system memory more in that way.
             */
            if (H5D__chunk_file_alloc(idx_info, &coll_entry->chunk_current, &coll_entry->chunk_new,
                                      &need_insert, NULL) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "unable to allocate chunk")
        }

        /* Ensure the chunk list is sorted in ascending order of offset in the file */
        if (need_sort)
            HDqsort(chunk_list, chunk_list_num_entries, sizeof(H5D_filtered_collective_io_info_t),
                    H5D__cmp_filtered_collective_io_info_entry);
    }

done:
    if (gathered_array)
        H5MM_free(gathered_array);
    if (send_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&send_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (recv_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&recv_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5D__mpio_collective_filtered_chunk_reallocate() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_chunk_reinsert
 *
 * Purpose:     When performing a parallel write on a chunked dataset with
 *              filters applied, all ranks must eventually get together and
 *              perform a collective reinsertion into the dataset's chunk
 *              index of chunks that were modified. This routine is
 *              responsible for coordinating that process.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_chunk_reinsert(H5D_filtered_collective_io_info_t *chunk_list,
                                             size_t chunk_list_num_entries, H5D_io_info_t *io_info,
                                             H5D_chk_idx_info_t *idx_info, int mpi_rank, int mpi_size)
{
    MPI_Datatype send_type;
    MPI_Datatype recv_type;
    hbool_t      send_type_derived = FALSE;
    hbool_t      recv_type_derived = FALSE;
    hsize_t      scaled_coords[H5O_LAYOUT_NDIMS];
    size_t       collective_num_entries = 0;
    size_t       i;
    void *       gathered_array = NULL;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == chunk_list_num_entries);
    HDassert(io_info);
    HDassert(idx_info);

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TRACE_ENTER(mpi_rank);
    H5D_MPIO_TIME_START(mpi_rank, "Reinsertion of modified chunks into chunk index");
#endif

    /* Only re-insert chunks if index has an insert method */
    if (idx_info->storage->ops->insert) {
        H5D_chunk_ud_t chunk_ud;

        H5D_MPIO_INIT_CHUNK_UD_INFO(chunk_ud, idx_info);

        /* Create derived datatypes for the chunk re-insertion info needed */
        if (H5D__mpio_get_chunk_insert_info_types(&recv_type, &recv_type_derived, &send_type, &send_type_derived) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL,
                        "can't create derived datatypes for chunk re-insertion info")

        /*
         * Gather information to all ranks for a collective re-insertion
         * of the modified chunks into the chunk index
         */
        H5_CHECK_OVERFLOW(chunk_list_num_entries, size_t, int);
        if (H5_mpio_gatherv_alloc_simple(chunk_list, (int)chunk_list_num_entries, send_type, recv_type, TRUE,
                                         0, io_info->comm, mpi_rank, mpi_size, &gathered_array,
                                         &collective_num_entries) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGATHER, FAIL,
                        "can't gather chunk index re-insertion info to/from ranks")

        for (i = 0; i < collective_num_entries; i++) {
            H5D_chunk_insert_info_t *coll_entry = &((H5D_chunk_insert_info_t *)gathered_array)[i];

            /*
             * We only need to reinsert this chunk if we had to actually
             * allocate or reallocate space in the file for it
             */
            if (coll_entry->index_info.need_insert) {
                chunk_ud.chunk_block   = coll_entry->chunk_block;
                chunk_ud.chunk_idx     = coll_entry->index_info.chunk_idx;
                chunk_ud.filter_mask   = coll_entry->index_info.filter_mask;
                chunk_ud.common.scaled = scaled_coords;

                /* Calculate scaled coordinates for the chunk */
                if (idx_info->layout->idx_type == H5D_CHUNK_IDX_EARRAY &&
                    idx_info->layout->u.earray.unlim_dim > 0) {
                    /*
                     * Extensible arrays where the unlimited dimension is not
                     * the slowest-changing dimension "swizzle" the coordinates
                     * to move the unlimited dimension value to offset 0. Therefore,
                     * we use the "swizzled" down chunks to calculate the "swizzled"
                     * scaled coordinates and then we undo the "swizzle" operation.
                     */
                    H5VM_array_calc_pre(chunk_ud.chunk_idx, io_info->dset->shared->ndims,
                                        idx_info->layout->u.earray.swizzled_down_chunks, scaled_coords);

                    H5VM_unswizzle_coords(hsize_t, scaled_coords, idx_info->layout->u.earray.unlim_dim);
                }
                else {
                    H5VM_array_calc_pre(chunk_ud.chunk_idx, io_info->dset->shared->ndims,
                                        io_info->dset->shared->layout.u.chunk.down_chunks, scaled_coords);
                }

                scaled_coords[io_info->dset->shared->ndims] = 0;

#ifndef NDEBUG
                /*
                 * If a matching local chunk entry is found, compare
                 * the calculated scaled coordinates against the chunk's
                 * actual scaled coordinates to make sure they match.
                 */
                for (size_t dbg_idx = 0; dbg_idx < chunk_list_num_entries; dbg_idx++) {
                    if (coll_entry->index_info.chunk_idx == chunk_list[dbg_idx].index_info.chunk_idx) {
                        hbool_t coords_match =
                            !HDmemcmp(scaled_coords, chunk_list[dbg_idx].chunk_info->scaled,
                                      io_info->dset->shared->ndims * sizeof(hsize_t));

                        HDassert(coords_match && "Calculated scaled coordinates for chunk didn't match "
                                                 "chunk's actual scaled coordinates!");
                        break;
                    }
                }
#endif

                if ((idx_info->storage->ops->insert)(idx_info, &chunk_ud, io_info->dset) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTINSERT, FAIL,
                                "unable to insert chunk address into index")
            }
        }
    }

done:
    if (gathered_array)
        H5MM_free(gathered_array);
    if (send_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&send_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (recv_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&recv_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

#ifdef H5Dmpio_DEBUG
    H5D_MPIO_TIME_STOP(mpi_rank);
    H5D_MPIO_TRACE_EXIT(mpi_rank);
#endif

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_chunk_reinsert() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_chunk_redistribute_info_types
 *
 * Purpose:     Constructs MPI derived datatypes for communicating the
 *              info from a H5D_filtered_collective_io_info_t structure
 *              that is necessary for redistributing shared chunks during a
 *              collective write of filtered chunks.
 *
 *              The datatype returned through `contig_type` has an extent
 *              equal to the size of an H5D_chunk_redistribute_info_t
 *              structure and is suitable for communicating that structure
 *              type.
 *
 *              The datatype returned through `resized_type` has an extent
 *              equal to the size of an H5D_filtered_collective_io_info_t
 *              structure. This makes it suitable for sending an array of
 *              those structures, while extracting out just the info
 *              necessary for the chunk redistribution operation during
 *              communication.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_chunk_redistribute_info_types(MPI_Datatype *contig_type, hbool_t *contig_type_derived,
                                            MPI_Datatype *resized_type, hbool_t *resized_type_derived)
{
    MPI_Datatype struct_type              = MPI_DATATYPE_NULL;
    hbool_t      struct_type_derived      = FALSE;
    MPI_Datatype chunk_block_type         = MPI_DATATYPE_NULL;
    hbool_t      chunk_block_type_derived = FALSE;
#if MPI_VERSION >= 3
    MPI_Count chunk_block_type_size;
#else
    int chunk_block_type_size;
#endif
    MPI_Datatype types[5];
    MPI_Aint     displacements[5];
    int          block_lengths[5];
    int          field_count;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(contig_type);
    HDassert(contig_type_derived);
    HDassert(resized_type);
    HDassert(resized_type_derived);

    *contig_type_derived  = FALSE;
    *resized_type_derived = FALSE;

    /* Create struct type for the inner H5F_block_t structure */
    if (H5F_mpi_get_file_block_type(FALSE, &chunk_block_type, &chunk_block_type_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't create derived type for chunk file description")

        /* Retrieve the size of the chunk block type */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size_x(chunk_block_type, &chunk_block_type_size)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size(chunk_block_type, &chunk_block_type_size)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_size(_x) failed", mpi_code)

#if MPI_VERSION >= 3
    H5_CHECK_OVERFLOW(chunk_block_type_size, MPI_Count, MPI_Aint);
#endif

    field_count = 5;
    HDassert(field_count == (sizeof(types) / sizeof(MPI_Datatype)));

    /*
     * Create structure type to pack chunk H5F_block_t structure
     * next to chunk_idx, orig_owner, new_owner and num_writers
     * fields
     */
    block_lengths[0] = 1;
    block_lengths[1] = 1;
    block_lengths[2] = 1;
    block_lengths[3] = 1;
    block_lengths[4] = 1;
    displacements[0] = offsetof(H5D_chunk_redistribute_info_t, chunk_block);
    displacements[1] = offsetof(H5D_chunk_redistribute_info_t, chunk_idx);
    displacements[2] = offsetof(H5D_chunk_redistribute_info_t, orig_owner);
    displacements[3] = offsetof(H5D_chunk_redistribute_info_t, new_owner);
    displacements[4] = offsetof(H5D_chunk_redistribute_info_t, num_writers);
    types[0]         = chunk_block_type;
    types[1]         = HSIZE_AS_MPI_TYPE;
    types[2]         = MPI_INT;
    types[3]         = MPI_INT;
    types[4]         = MPI_INT;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    *contig_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

    /* Create struct type to extract the chunk_current, chunk_idx, orig_owner,
     * new_owner and num_writers fields from a H5D_filtered_collective_io_info_t
     * structure
     */
    block_lengths[0] = 1;
    block_lengths[1] = 1;
    block_lengths[2] = 1;
    block_lengths[3] = 1;
    block_lengths[4] = 1;
    displacements[0] = offsetof(H5D_filtered_collective_io_info_t, chunk_current);
    displacements[1] = offsetof(H5D_filtered_collective_io_info_t, index_info.chunk_idx);
    displacements[2] = offsetof(H5D_filtered_collective_io_info_t, orig_owner);
    displacements[3] = offsetof(H5D_filtered_collective_io_info_t, new_owner);
    displacements[4] = offsetof(H5D_filtered_collective_io_info_t, num_writers);
    types[0]         = chunk_block_type;
    types[1]         = HSIZE_AS_MPI_TYPE;
    types[2]         = MPI_INT;
    types[3]         = MPI_INT;
    types[4]         = MPI_INT;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, &struct_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    struct_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_create_resized(
                            struct_type, 0, sizeof(H5D_filtered_collective_io_info_t), resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_resized failed", mpi_code)
    *resized_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

done:
    if (struct_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&struct_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (chunk_block_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_block_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

    if (ret_value < 0) {
        if (*resized_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(resized_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *resized_type_derived = FALSE;
        }
        if (*contig_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(contig_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *contig_type_derived = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_chunk_redistribute_info_types() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_chunk_alloc_info_types
 *
 * Purpose:     Constructs MPI derived datatypes for communicating the info
 *              from a H5D_filtered_collective_io_info_t structure that is
 *              necessary for re-allocating file space during a collective
 *              write of filtered chunks.
 *
 *              The datatype returned through `contig_type` has an extent
 *              equal to the size of an H5D_chunk_alloc_info_t structure
 *              and is suitable for communicating that structure type.
 *
 *              The datatype returned through `resized_type` has an extent
 *              equal to the size of an H5D_filtered_collective_io_info_t
 *              structure. This makes it suitable for sending an array of
 *              those structures, while extracting out just the info
 *              necessary for the chunk file space reallocation operation
 *              during communication.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_chunk_alloc_info_types(MPI_Datatype *contig_type, hbool_t *contig_type_derived,
                                     MPI_Datatype *resized_type, hbool_t *resized_type_derived)
{
    MPI_Datatype struct_type               = MPI_DATATYPE_NULL;
    hbool_t      struct_type_derived       = FALSE;
    MPI_Datatype chunk_block_type          = MPI_DATATYPE_NULL;
    hbool_t      chunk_block_type_derived  = FALSE;
#if MPI_VERSION >= 3
    MPI_Count chunk_block_type_size;
#else
    int chunk_block_type_size;
#endif
    MPI_Datatype types[3];
    MPI_Aint     displacements[3];
    int          block_lengths[3];
    int          field_count;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(contig_type);
    HDassert(contig_type_derived);
    HDassert(resized_type);
    HDassert(resized_type_derived);

    *contig_type_derived  = FALSE;
    *resized_type_derived = FALSE;

    /* Create struct type for the inner H5F_block_t structure */
    if (H5F_mpi_get_file_block_type(FALSE, &chunk_block_type, &chunk_block_type_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't create derived type for chunk file description")

        /* Retrieve the size of the chunk block type */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size_x(chunk_block_type, &chunk_block_type_size)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size(chunk_block_type, &chunk_block_type_size)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_size(_x) failed", mpi_code)

#if MPI_VERSION >= 3
    H5_CHECK_OVERFLOW(chunk_block_type_size, MPI_Count, MPI_Aint);
#endif

    field_count = 3;
    HDassert(field_count == (sizeof(types) / sizeof(MPI_Datatype)));

    /*
     * Create structure type to pack both chunk H5F_block_t structures
     * next to chunk_idx field
     */
    block_lengths[0] = 1;
    block_lengths[1] = 1;
    block_lengths[2] = 1;
    displacements[0] = offsetof(H5D_chunk_alloc_info_t, chunk_current);
    displacements[1] = offsetof(H5D_chunk_alloc_info_t, chunk_new);
    displacements[2] = offsetof(H5D_chunk_alloc_info_t, chunk_idx);
    types[0]         = chunk_block_type;
    types[1]         = chunk_block_type;
    types[2]         = HSIZE_AS_MPI_TYPE;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    *contig_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

    /*
     * Create struct type to extract the chunk_current, chunk_new and chunk_idx
     * fields from a H5D_filtered_collective_io_info_t structure
     */
    block_lengths[0] = 1;
    block_lengths[1] = 1;
    block_lengths[2] = 1;
    displacements[0] = offsetof(H5D_filtered_collective_io_info_t, chunk_current);
    displacements[1] = offsetof(H5D_filtered_collective_io_info_t, chunk_new);
    displacements[2] = offsetof(H5D_filtered_collective_io_info_t, index_info.chunk_idx);
    types[0]         = chunk_block_type;
    types[1]         = chunk_block_type;
    types[2]         = HSIZE_AS_MPI_TYPE;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, &struct_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    struct_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_create_resized(
                            struct_type, 0, sizeof(H5D_filtered_collective_io_info_t), resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_resized failed", mpi_code)
    *resized_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

done:
    if (struct_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&struct_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (chunk_block_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_block_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

    if (ret_value < 0) {
        if (*resized_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(resized_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *resized_type_derived = FALSE;
        }
        if (*contig_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(contig_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *contig_type_derived = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_chunk_alloc_info_types() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_get_chunk_insert_info_types
 *
 * Purpose:     Constructs MPI derived datatypes for communicating the
 *              information necessary when reinserting chunks into a
 *              dataset's chunk index. This includes the chunk's new offset
 *              and size (H5F_block_t) and the inner `index_info` structure
 *              of a H5D_filtered_collective_io_info_t structure.
 *
 *              The datatype returned through `contig_type` has an extent
 *              equal to the size of an H5D_chunk_insert_info_t structure
 *              and is suitable for communicating that structure type.
 *
 *              The datatype returned through `resized_type` has an extent
 *              equal to the size of the encompassing
 *              H5D_filtered_collective_io_info_t structure. This makes it
 *              suitable for sending an array of
 *              H5D_filtered_collective_io_info_t structures, while
 *              extracting out just the information needed during
 *              communication.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_get_chunk_insert_info_types(MPI_Datatype *contig_type, hbool_t *contig_type_derived,
                                      MPI_Datatype *resized_type, hbool_t *resized_type_derived)
{
    MPI_Datatype struct_type              = MPI_DATATYPE_NULL;
    hbool_t      struct_type_derived      = FALSE;
    MPI_Datatype chunk_block_type         = MPI_DATATYPE_NULL;
    hbool_t      chunk_block_type_derived = FALSE;
    MPI_Aint     contig_type_extent;
#if MPI_VERSION >= 3
    MPI_Count chunk_block_type_size;
#else
    int chunk_block_type_size;
#endif
    MPI_Datatype types[4];
    MPI_Aint     displacements[4];
    int          block_lengths[4];
    int          field_count;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(contig_type);
    HDassert(contig_type_derived);
    HDassert(resized_type);
    HDassert(resized_type_derived);

    *contig_type_derived  = FALSE;
    *resized_type_derived = FALSE;

    /* Create struct type for an H5F_block_t structure */
    if (H5F_mpi_get_file_block_type(FALSE, &chunk_block_type, &chunk_block_type_derived) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't create derived type for chunk file description")

        /* Retrieve the size of the chunk block type */
#if MPI_VERSION >= 3
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size_x(chunk_block_type, &chunk_block_type_size)))
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Type_size(chunk_block_type, &chunk_block_type_size)))
#endif
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_size(_x) failed", mpi_code)

#if MPI_VERSION >= 3
    H5_CHECK_OVERFLOW(chunk_block_type_size, MPI_Count, MPI_Aint);
#endif

    field_count = 4;
    HDassert(field_count == (sizeof(types) / sizeof(MPI_Datatype)));

    /*
     * Create struct type to pack information into memory as follows:
     *
     * Chunk's new Offset/Size (H5F_block_t) ->
     * Chunk Index Info (H5D_chunk_index_info_t)
     */
    block_lengths[0] = 1;
    block_lengths[1] = 1;
    block_lengths[2] = 1;
    block_lengths[3] = 1;
    displacements[0] = offsetof(H5D_chunk_insert_info_t, chunk_block);
    displacements[1] = offsetof(H5D_chunk_insert_info_t, index_info.chunk_idx);
    displacements[2] = offsetof(H5D_chunk_insert_info_t, index_info.filter_mask);
    displacements[3] = offsetof(H5D_chunk_insert_info_t, index_info.need_insert);
    types[0]         = chunk_block_type;
    types[1]         = HSIZE_AS_MPI_TYPE;
    types[2]         = MPI_UNSIGNED;
    types[3]         = MPI_C_BOOL;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, &struct_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    struct_type_derived = TRUE;

    contig_type_extent = (MPI_Aint)(sizeof(H5F_block_t) + sizeof(H5D_chunk_index_info_t));

    if (MPI_SUCCESS != (mpi_code = MPI_Type_create_resized(struct_type, 0, contig_type_extent, contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_resized failed", mpi_code)
    *contig_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(contig_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

    struct_type_derived = FALSE;
    if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&struct_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_free failed", mpi_code)

    /*
     * Create struct type to correctly extract all needed
     * information from a H5D_filtered_collective_io_info_t
     * structure.
     */
    displacements[0] = offsetof(H5D_filtered_collective_io_info_t, chunk_new);
    displacements[1] = offsetof(H5D_filtered_collective_io_info_t, index_info.chunk_idx);
    displacements[2] = offsetof(H5D_filtered_collective_io_info_t, index_info.filter_mask);
    displacements[3] = offsetof(H5D_filtered_collective_io_info_t, index_info.need_insert);
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, &struct_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    struct_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_create_resized(
                            struct_type, 0, sizeof(H5D_filtered_collective_io_info_t), resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_resized failed", mpi_code)
    *resized_type_derived = TRUE;

    if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(resized_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

done:
    if (struct_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&struct_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }
    if (chunk_block_type_derived) {
        if (MPI_SUCCESS != (mpi_code = MPI_Type_free(&chunk_block_type)))
            HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
    }

    if (ret_value < 0) {
        if (*resized_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(resized_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *resized_type_derived = FALSE;
        }
        if (*contig_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(contig_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *contig_type_derived = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_get_chunk_insert_info_types() */

/*-------------------------------------------------------------------------
 * Function:    H5D__mpio_collective_filtered_io_type
 *
 * Purpose:     Constructs a MPI derived datatype for both the memory and
 *              the file for a collective I/O operation on filtered chunks.
 *              The datatype contains the chunk offsets and lengths in the
 *              file and the locations of the chunk data buffers to read
 *              into/write from.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__mpio_collective_filtered_io_type(H5D_filtered_collective_io_info_t *chunk_list, size_t num_entries,
                                      H5D_io_op_type_t op_type, MPI_Datatype *new_mem_type,
                                      hbool_t *mem_type_derived, MPI_Datatype *new_file_type,
                                      hbool_t *file_type_derived)
{
    MPI_Aint *io_buf_array      = NULL; /* Relative displacements of filtered chunk data buffers */
    MPI_Aint *file_offset_array = NULL; /* Chunk offsets in the file */
    int *     length_array      = NULL; /* Filtered Chunk lengths */
    int       mpi_code;
    herr_t    ret_value = SUCCEED;

    FUNC_ENTER_STATIC

    HDassert(chunk_list || 0 == num_entries);
    HDassert(new_mem_type);
    HDassert(mem_type_derived);
    HDassert(new_file_type);
    HDassert(file_type_derived);

    *mem_type_derived  = FALSE;
    *file_type_derived = FALSE;
    *new_mem_type      = MPI_BYTE;
    *new_file_type     = MPI_BYTE;

    if (num_entries > 0) {
        size_t i, last_valid_idx = 0;
        int    chunk_count;

        /*
         * Determine number of chunks for I/O operation and
         * setup for derived datatype creation if I/O operation
         * includes multiple chunks
         */
        if (num_entries == 1) {
            /* Set last valid index to 0 for contiguous datatype creation */
            last_valid_idx = 0;

            if (op_type == H5D_IO_OP_WRITE)
                chunk_count = 1;
            else
                chunk_count = chunk_list[0].need_read ? 1 : 0;
        }
        else {
            MPI_Aint chunk_buf;
            MPI_Aint base_buf;
            haddr_t  base_offset        = HADDR_UNDEF;
            hbool_t  base_read_vars_set = FALSE;

            H5_CHECK_OVERFLOW(num_entries, size_t, int);

            /* Allocate arrays */
            if (NULL == (length_array = H5MM_malloc((size_t)num_entries * sizeof(int))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory allocation failed for filtered collective I/O length array")
            if (NULL == (io_buf_array = H5MM_malloc((size_t)num_entries * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory allocation failed for filtered collective I/O buf length array")
            if (NULL == (file_offset_array = H5MM_malloc((size_t)num_entries * sizeof(MPI_Aint))))
                HGOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                            "memory allocation failed for filtered collective I/O offset array")

            /*
             * If doing a write, we can set the base chunk offset
             * and base chunk data buffer right away
             */
            if (op_type == H5D_IO_OP_WRITE) {
#if MPI_VERSION >= 3
                if (MPI_SUCCESS != (mpi_code = MPI_Get_address(chunk_list[0].buf, &base_buf)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Get_address failed", mpi_code)
#else
                base_buf = (MPI_Aint)chunk_list[0].buf;
#endif

                base_offset = chunk_list[0].chunk_new.offset;
            }

            for (i = 0, chunk_count = 0; i < num_entries; i++) {
                if (op_type == H5D_IO_OP_READ) {
                    /*
                     * If this is a read operation that is occurring as
                     * part of a dataset write (read-modify-write cycle),
                     * don't add the chunk to the list of addresses being
                     * read if the chunk is being fully overwritten
                     */
                    if (!chunk_list[i].need_read)
                        continue;
                    else {
                        /* Otherwise, set our base chunk offset and base chunk
                         * data read buffer if we haven't already
                         */
                        if (!base_read_vars_set) {
#if MPI_VERSION >= 3
                            if (MPI_SUCCESS != (mpi_code = MPI_Get_address(chunk_list[i].buf, &base_buf)))
                                HMPI_GOTO_ERROR(FAIL, "MPI_Get_address failed", mpi_code)
#else
                            base_buf = (MPI_Aint)chunk_list[i].buf;
#endif

                            base_offset = chunk_list[i].chunk_current.offset;

                            base_read_vars_set = TRUE;
                        }
                    }
                }

                /*
                 * Set the current chunk entry's offset in the file, relative to
                 * the first chunk entry
                 */
                if (op_type == H5D_IO_OP_READ) {
                    HDassert(H5F_addr_defined(chunk_list[i].chunk_current.offset));
                    file_offset_array[chunk_count] =
                        (MPI_Aint)(chunk_list[i].chunk_current.offset - base_offset);
                }
                else {
                    HDassert(H5F_addr_defined(chunk_list[i].chunk_new.offset));
                    file_offset_array[chunk_count] =
                        (MPI_Aint)(chunk_list[i].chunk_new.offset - base_offset);
                }

                /*
                 * Ensure the chunk list is sorted in ascending ordering of
                 * offset in the file
                 */
                if (chunk_count)
                    HDassert(file_offset_array[chunk_count] > file_offset_array[chunk_count - 1]);

                /* Set the current chunk entry's size for the I/O operation */
                if (op_type == H5D_IO_OP_READ) {
                    H5_CHECK_OVERFLOW(chunk_list[i].chunk_current.length, hsize_t, int);
                    length_array[chunk_count] = (int)chunk_list[i].chunk_current.length;
                }
                else {
                    H5_CHECK_OVERFLOW(chunk_list[i].chunk_new.length, hsize_t, int);
                    length_array[chunk_count] = (int)chunk_list[i].chunk_new.length;
                }

                /*
                 * Set the displacement of the chunk entry's chunk data buffer,
                 * relative to the first entry's data buffer
                 */
#if MPI_VERSION >= 3 && MPI_SUBVERSION >= 1
                if (MPI_SUCCESS != (mpi_code = MPI_Get_address(chunk_list[i].buf, &chunk_buf)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Get_address failed", mpi_code)

                io_buf_array[chunk_count] = MPI_Aint_diff(chunk_buf, base_buf);
#else
                chunk_buf                 = (MPI_Aint)chunk_list[i].buf;
                io_buf_array[chunk_count] = chunk_buf - base_buf;
#endif

                /*
                 * Set last valid index in case only a single chunk will
                 * be involved in the I/O operation
                 */
                last_valid_idx = i;

                chunk_count++;
            } /* end for */
        }

        /*
         * Create derived datatypes for the chunk list if this
         * rank has any chunks to work on
         */
        if (chunk_count > 0) {
            if (chunk_count == 1) {
                int chunk_len;

                /* Single chunk - use a contiguous type for both memory and file */

                if (op_type == H5D_IO_OP_READ) {
                    H5_CHECKED_ASSIGN(chunk_len, int, chunk_list[last_valid_idx].chunk_current.length,
                                      hsize_t);
                }
                else {
                    H5_CHECKED_ASSIGN(chunk_len, int, chunk_list[last_valid_idx].chunk_new.length, hsize_t);
                }

                if (MPI_SUCCESS != (mpi_code = MPI_Type_contiguous(chunk_len, MPI_BYTE, new_file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_contiguous failed", mpi_code)
                *new_mem_type = *new_file_type;

                /*
                 * Since we use the same datatype for both memory and file, only
                 * mark the file type as derived so the caller doesn't try to
                 * free the same type twice
                 */
                *mem_type_derived  = FALSE;
                *file_type_derived = TRUE;

                if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            }
            else {
                HDassert(file_offset_array);
                HDassert(length_array);
                HDassert(io_buf_array);

                /* Multiple chunks - use an hindexed type for both memory and file */

                /* Create memory MPI type */
                if (MPI_SUCCESS != (mpi_code = MPI_Type_create_hindexed(
                                        chunk_count, length_array, io_buf_array, MPI_BYTE, new_mem_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
                *mem_type_derived = TRUE;

                if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_mem_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

                /* Create file MPI type */
                if (MPI_SUCCESS !=
                    (mpi_code = MPI_Type_create_hindexed(chunk_count, length_array, file_offset_array,
                                                         MPI_BYTE, new_file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_hindexed failed", mpi_code)
                *file_type_derived = TRUE;

                if (MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_file_type)))
                    HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)
            }
        }
    } /* end if */

done:
    if (file_offset_array)
        H5MM_free(file_offset_array);
    if (io_buf_array)
        H5MM_free(io_buf_array);
    if (length_array)
        H5MM_free(length_array);

    if (ret_value < 0) {
        if (*file_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(new_file_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *file_type_derived = FALSE;
        }
        if (*mem_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(new_mem_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *mem_type_derived = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_collective_filtered_io_type() */

#ifdef H5Dmpio_DEBUG

static herr_t
H5D__mpio_dump_collective_filtered_chunk_list(H5D_filtered_collective_io_info_t *chunk_list,
                                              size_t chunk_list_num_entries, int mpi_rank)
{
    H5D_filtered_collective_io_info_t *chunk_entry;
    size_t                             i;
    herr_t                             ret_value = SUCCEED;

    FUNC_ENTER_STATIC_NOERR

    H5D_MPIO_DEBUG(mpi_rank, "CHUNK LIST: [");
    for (i = 0; i < chunk_list_num_entries; i++) {
        unsigned chunk_rank;

        chunk_entry = &chunk_list[i];

        HDassert(chunk_entry->chunk_info);
        chunk_rank = (unsigned)H5S_GET_EXTENT_NDIMS(chunk_entry->chunk_info->fspace);

        H5D_MPIO_DEBUG(mpi_rank, " {");
        H5D_MPIO_DEBUG_VA(mpi_rank, "   - Entry %zu -", i);

        H5D_MPIO_DEBUG(mpi_rank, "   - Chunk Fspace Info -");
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Current Info: { Offset: %llu, Length: %llu }",
                (unsigned long long) chunk_entry->chunk_current.offset,
                (unsigned long long) chunk_entry->chunk_current.length);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk New Info: { Offset: %llu, Length: %llu }",
                (unsigned long long) chunk_entry->chunk_new.offset,
                (unsigned long long) chunk_entry->chunk_new.length);

        H5D_MPIO_DEBUG(mpi_rank, "   - Chunk Insert Info -");
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Scaled Coords (4-d): { %llu, %llu, %llu, %llu }",
                chunk_rank < 1 ? 0 : (unsigned long long) chunk_entry->chunk_info->scaled[0],
                chunk_rank < 2 ? 0 : (unsigned long long) chunk_entry->chunk_info->scaled[1],
                chunk_rank < 3 ? 0 : (unsigned long long) chunk_entry->chunk_info->scaled[2],
                chunk_rank < 4 ? 0 : (unsigned long long) chunk_entry->chunk_info->scaled[3]);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Index: %llu",
                (unsigned long long) chunk_entry->index_info.chunk_idx);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Filter Mask: %u", chunk_entry->index_info.filter_mask);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Need Insert: %s",
                chunk_entry->index_info.need_insert ? "YES" : "NO");

        H5D_MPIO_DEBUG(mpi_rank, "   - Other Info -");
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Info Ptr: %p", (void *)chunk_entry->chunk_info);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Need Read: %s", chunk_entry->need_read ? "YES" : "NO");
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk I/O Size: %zu", chunk_entry->io_size);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Buffer Size: %zu", chunk_entry->chunk_buf_size);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Original Owner: %d", chunk_entry->orig_owner);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     New Owner: %d", chunk_entry->new_owner);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     # of Writers: %d", chunk_entry->num_writers);
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Chunk Data Buffer Ptr: %p", (void *)chunk_entry->buf);

        H5D_MPIO_DEBUG(mpi_rank, "   - MPI Request Info -");
        H5D_MPIO_DEBUG_VA(mpi_rank, "     Num Receive Requests: %d", chunk_entry->async_info.num_receive_requests);

        H5D_MPIO_DEBUG(mpi_rank, " }");
    }
    H5D_MPIO_DEBUG(mpi_rank, "]");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__mpio_dump_collective_filtered_chunk_list() */

#endif

#endif /* H5_HAVE_PARALLEL */
