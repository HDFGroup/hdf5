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
 * Generic code for integrating an HDF5 VFD with the subfiling feature
 */

#include <libgen.h>

#include "H5subfiling_common.h"
#include "H5subfiling_err.h"

#include "H5MMprivate.h"

typedef struct {           /* Format of a context map entry  */
    void   *file_handle;   /* key value (linear search of the cache) */
    int64_t sf_context_id; /* The return value if matching file_handle */
} file_map_to_context_t;

/* Identifiers for HDF5's error API */
hid_t H5subfiling_err_stack_g = H5I_INVALID_HID;
hid_t H5subfiling_err_class_g = H5I_INVALID_HID;
char  H5subfiling_mpi_error_str[MPI_MAX_ERROR_STRING];
int   H5subfiling_mpi_error_str_len;

static subfiling_context_t *sf_context_cache  = NULL;
static sf_topology_t       *sf_topology_cache = NULL;

static size_t sf_context_cache_limit  = 16;
static size_t sf_topology_cache_limit = 4;

static int sf_topology_cache_size = 0;

static file_map_to_context_t *sf_open_file_map = NULL;
static int                    sf_file_map_size = 0;
#define DEFAULT_FILE_MAP_ENTRIES 8

static herr_t H5_free_subfiling_object_int(subfiling_context_t *sf_context);
static herr_t H5_free_subfiling_topology(sf_topology_t *topology);

static herr_t init_subfiling(H5FD_subfiling_shared_config_t *subfiling_config, MPI_Comm comm,
                             int64_t *context_id_out);
static herr_t init_app_topology(H5FD_subfiling_ioc_select_t ioc_selection_type, MPI_Comm comm,
                                MPI_Comm intra_comm, sf_topology_t **app_topology_out);
static herr_t init_subfiling_context(subfiling_context_t            *sf_context,
                                     H5FD_subfiling_shared_config_t *subfiling_config,
                                     sf_topology_t *app_topology, MPI_Comm file_comm);
static herr_t open_subfile_with_context(subfiling_context_t *sf_context, int file_acc_flags);
static herr_t record_fid_to_subfile(void *file_handle, int64_t subfile_context_id, int *next_index);
static herr_t ioc_open_file(int64_t file_context_id, int file_acc_flags);
static herr_t generate_subfile_name(subfiling_context_t *sf_context, int file_acc_flags, char *filename_out,
                                    size_t filename_out_len, char **filename_basename_out,
                                    char **subfile_dir_out);
static herr_t create_config_file(subfiling_context_t *sf_context, const char *base_filename,
                                 const char *subfile_dir, hbool_t truncate_if_exists);
static herr_t open_config_file(subfiling_context_t *sf_context, const char *base_filename,
                               const char *subfile_dir, const char *mode, FILE **config_file_out);

static int         get_next_fid_map_index(void);
static void        clear_fid_map_entry(void *file_handle, int64_t sf_context_id);
static int         compare_hostid(const void *h1, const void *h2);
static herr_t      get_ioc_selection_criteria_from_env(H5FD_subfiling_ioc_select_t *ioc_selection_type,
                                                       char                       **ioc_sel_info_str);
static int         count_nodes(sf_topology_t *info, MPI_Comm comm);
static herr_t      find_cached_topology_info(MPI_Comm comm, H5FD_subfiling_ioc_select_t ioc_selection_type,
                                             sf_topology_t **app_topology);
static herr_t      gather_topology_info(sf_topology_t *info, MPI_Comm comm, MPI_Comm intra_comm);
static int         identify_ioc_ranks(sf_topology_t *info, int node_count, int iocs_per_node);
static inline void assign_ioc_ranks(sf_topology_t *app_topology, int ioc_count, int rank_multiple);

static int
get_next_fid_map_index(void)
{
    int index = 0;

    HDassert(sf_open_file_map || (sf_file_map_size == 0));

    for (int i = 0; i < sf_file_map_size; i++) {
        if (sf_open_file_map[i].file_handle == NULL) {
            index = i;
            break;
        }
    }

    /* A valid index should always be found here */
    HDassert(index >= 0);
    HDassert((sf_file_map_size == 0) || (index < sf_file_map_size));

    return index;
}

/*-------------------------------------------------------------------------
 * Function:    clear_fid_map_entry
 *
 * Purpose:     Remove the map entry associated with the file->inode.
 *              This is done at file close.
 *
 * Return:      None
 * Errors:      Cannot fail.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static void
clear_fid_map_entry(void *file_handle, int64_t sf_context_id)
{
    if (sf_open_file_map) {
        for (int i = 0; i < sf_file_map_size; i++) {
            if ((sf_open_file_map[i].file_handle == file_handle) &&
                (sf_open_file_map[i].sf_context_id == sf_context_id)) {
                sf_open_file_map[i].file_handle   = NULL;
                sf_open_file_map[i].sf_context_id = -1;
                return;
            }
        }
    }
} /* end clear_fid_map_entry() */

/*
 * ---------------------------------------------------
 * Topology discovery related functions for choosing
 * I/O Concentrator (IOC) ranks.
 * Currently, the default approach for assigning an IOC
 * is select the lowest MPI rank on each node.
 *
 * The approach collectively generates N tuples
 * consisting of the MPI rank and hostid. This
 * collection is then sorted by hostid and scanned
 * to identify the IOC ranks.
 *
 * As time permits, addition assignment methods will
 * be implemented, e.g. 1-per-Nranks or via a config
 * option.  Additional selection methodologies can
 * be included as users get more experience using the
 * subfiling implementation.
 * ---------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Function:    compare_hostid
 *
 * Purpose:     qsort sorting function.
 *              Compares tuples of 'layout_t'. The sorting is based on
 *              the long hostid values.
 *
 * Return:      result of: (hostid1 > hostid2)
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static int
compare_hostid(const void *h1, const void *h2)
{
    const layout_t *host1 = (const layout_t *)h1;
    const layout_t *host2 = (const layout_t *)h2;
    return (host1->hostid > host2->hostid);
}

/*
-------------------------------------------------------------------------
  Programmer:  Richard Warren
  Purpose:     Return a character string which represents either the
               default selection method: SELECT_IOC_ONE_PER_NODE; or
               if the user has selected a method via the environment
               variable (H5FD_SUBFILING_IOC_SELECTION_CRITERIA), we
               return that along with any optional qualifier with for
               that method.

  Errors:      None.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
static herr_t
get_ioc_selection_criteria_from_env(H5FD_subfiling_ioc_select_t *ioc_selection_type, char **ioc_sel_info_str)
{
    char  *opt_value = NULL;
    char  *env_value = HDgetenv(H5FD_SUBFILING_IOC_SELECTION_CRITERIA);
    herr_t ret_value = SUCCEED;

    HDassert(ioc_selection_type);
    HDassert(ioc_sel_info_str);

    *ioc_sel_info_str = NULL;

    if (env_value) {
        long check_value;

        /*
         * For non-default options, the environment variable
         * should have the following form:  integer:[integer|string]
         * In particular, EveryNthRank == 1:64 or every 64 ranks assign an IOC
         * or WithConfig == 2:/<full_path_to_config_file>
         */
        if ((opt_value = HDstrchr(env_value, ':')))
            *opt_value++ = '\0';

        errno       = 0;
        check_value = HDstrtol(env_value, NULL, 0);

        if (errno == ERANGE)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                        "couldn't parse value from " H5FD_SUBFILING_IOC_SELECTION_CRITERIA
                                        " environment variable");

        if ((check_value < 0) || (check_value >= ioc_selection_options))
            H5_SUBFILING_GOTO_ERROR(
                H5E_VFL, H5E_BADVALUE, FAIL,
                "invalid IOC selection type value %ld from " H5FD_SUBFILING_IOC_SELECTION_CRITERIA
                " environment variable",
                check_value);

        *ioc_selection_type = (H5FD_subfiling_ioc_select_t)check_value;
        *ioc_sel_info_str   = opt_value;
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    count_nodes
 *
 * Purpose:     Initializes the sorted collection of hostid+mpi_rank
 *              tuples.  After initialization, the collection is scanned
 *              to determine the number of unique hostid entries.  This
 *              value will determine the number of actual I/O concentrators
 *              that available to the application.  A side effect is to
 *              identify the 'node_index' of the current process.
 *
 * Return:      The number of unique hostid's (nodes).
 * Errors:      MPI_Abort if memory cannot be allocated.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static int
count_nodes(sf_topology_t *info, MPI_Comm comm)
{
    app_layout_t *app_layout = NULL;
    long          nextid;
    int           node_count;
    int           hostid_index = -1;
    int           my_rank;
    int           mpi_code;
    int           ret_value = 0;

    HDassert(info);
    HDassert(info->app_layout);
    HDassert(info->app_layout->layout);
    HDassert(info->app_layout->node_ranks);
    HDassert(MPI_COMM_NULL != comm);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &my_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(-1, "MPI_Comm_rank failed", mpi_code);

    app_layout = info->app_layout;
    node_count = app_layout->node_count;

    nextid = app_layout->layout[0].hostid;
    /* Possibly record my hostid_index */
    if (app_layout->layout[0].rank == my_rank) {
        hostid_index = 0;
    }

    app_layout->node_ranks[0] = 0; /* Add index */
    node_count                = 1;

    /* Recall that the topology array has been sorted! */
    for (int k = 1; k < app_layout->world_size; k++) {
        /* Possibly record my hostid_index */
        if (app_layout->layout[k].rank == my_rank)
            hostid_index = k;
        if (app_layout->layout[k].hostid != nextid) {
            nextid = app_layout->layout[k].hostid;
            /* Record the index of new hostid */
            app_layout->node_ranks[node_count++] = k;
        }
    }

    /* Mark the end of the node_ranks */
    app_layout->node_ranks[node_count] = app_layout->world_size;
    /* Save the index where we first located my hostid */
    app_layout->node_index = hostid_index;

    app_layout->node_count = node_count;

    ret_value = node_count;

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    find_cached_topology_info
 *
 * Purpose:     Given an MPI communicator and IOC selection strategy,
 *              checks the subfiling topology cached to see if any matching
 *              topology objects have been cached.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
static herr_t
find_cached_topology_info(MPI_Comm comm, H5FD_subfiling_ioc_select_t ioc_selection_type,
                          sf_topology_t **app_topology)
{
    herr_t ret_value = SUCCEED;

    for (int i = 0; i < sf_topology_cache_size; i++) {
        sf_topology_t *cached_topology = &sf_topology_cache[i];
        int            result;
        int            mpi_code;

        if (ioc_selection_type != cached_topology->selection_type)
            continue;

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_compare(comm, cached_topology->app_comm, &result)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_compare failed", mpi_code);

        if (MPI_IDENT == result || MPI_CONGRUENT == result) {
            *app_topology = cached_topology;
            break;
        }
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    gather_topology_info
 *
 * Purpose:     Collectively generate a sorted collection of hostid+mpi_rank
 *              tuples.  The result is returned in the 'topology' field
 *              of the sf_topology_t structure.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
gather_topology_info(sf_topology_t *info, MPI_Comm comm, MPI_Comm intra_comm)
{
    app_layout_t *app_layout       = NULL;
    layout_t     *hostinfo_partial = NULL;
    layout_t      my_hostinfo;
    MPI_Comm      aggr_comm = MPI_COMM_NULL;
    long          hostid;
    int          *recv_counts = NULL;
    int          *recv_displs = NULL;
    int           sf_world_size;
    int           sf_world_rank;
    herr_t        ret_value = SUCCEED;

    HDassert(info);
    HDassert(info->app_layout);
    HDassert(info->app_layout->layout);
    HDassert(MPI_COMM_NULL != comm);

    app_layout    = info->app_layout;
    sf_world_size = app_layout->world_size;
    sf_world_rank = app_layout->world_rank;

    hostid = gethostid();

    my_hostinfo.hostid = hostid;
    my_hostinfo.rank   = sf_world_rank;

    app_layout->hostid                = hostid;
    app_layout->layout[sf_world_rank] = my_hostinfo;

    if (sf_world_size > 1) {
        int mpi_code;

#ifdef H5_SUBFILING_PREFER_ALLGATHER_TOPOLOGY
        (void)intra_comm;

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allgather(&my_hostinfo, 2, MPI_LONG, app_layout->layout, 2, MPI_LONG, comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code);

        HDqsort(app_layout->layout, (size_t)sf_world_size, sizeof(layout_t), compare_hostid);
#else
        int node_local_rank = 0;
        int node_local_size = 0;
        int aggr_comm_size  = 0;

        HDassert(MPI_COMM_NULL != intra_comm);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(intra_comm, &node_local_rank)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(intra_comm, &node_local_size)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

        /* Split the file communicator into a sub-group of one rank per node */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_split(comm, node_local_rank, sf_world_rank, &aggr_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_split failed", mpi_code);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(aggr_comm, &aggr_comm_size)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

        /* Allocate a partial hostinfo array to aggregate into from node-local ranks */
        if (node_local_rank == 0) {
            if (NULL == (hostinfo_partial = HDmalloc((size_t)node_local_size * sizeof(*hostinfo_partial))))
                /* Push error, but participate in gather operation */
                H5_SUBFILING_DONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate hostinfo array");
        }

        /* Gather node-local hostinfo to single master rank on each node */
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Gather(&my_hostinfo, 2, MPI_LONG, hostinfo_partial, 2, MPI_LONG, 0, intra_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Gather failed", mpi_code);

        /* Gather total hostinfo from/to each master rank on each node */
        if (node_local_rank == 0) {
            int send_size = 2 * node_local_size;

            if (NULL == (recv_counts = HDmalloc((size_t)aggr_comm_size * sizeof(*recv_counts))))
                H5_SUBFILING_DONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate receive counts array");
            if (NULL == (recv_displs = HDmalloc((size_t)aggr_comm_size * sizeof(*recv_displs))))
                H5_SUBFILING_DONE_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                        "can't allocate receive displacements array");

            if (MPI_SUCCESS !=
                (mpi_code = MPI_Allgather(&send_size, 1, MPI_INT, recv_counts, 1, MPI_INT, aggr_comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Allgather failed", mpi_code);

            recv_displs[0] = 0;
            for (int i = 1; i < aggr_comm_size; i++)
                recv_displs[i] = recv_displs[i - 1] + recv_counts[i - 1];

            if (MPI_SUCCESS !=
                (mpi_code = MPI_Allgatherv(hostinfo_partial, send_size, MPI_LONG, app_layout->layout,
                                           recv_counts, recv_displs, MPI_LONG, aggr_comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Allgatherv failed", mpi_code);

            HDfree(recv_displs);
            HDfree(recv_counts);
            recv_displs = NULL;
            recv_counts = NULL;

            HDqsort(app_layout->layout, (size_t)sf_world_size, sizeof(layout_t), compare_hostid);
        }

        /*
         * Each master rank on each node distributes the total
         * hostinfo back to other node-local ranks
         */
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Bcast(app_layout->layout, 2 * sf_world_size, MPI_LONG, 0, intra_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
#endif
    }

done:
    HDfree(recv_displs);
    HDfree(recv_counts);
    HDfree(hostinfo_partial);

    if (H5_mpi_comm_free(&aggr_comm) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI communicator");

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    identify_ioc_ranks
 *
 * Purpose:     We've already identified the number of unique nodes and
 *              have a sorted list layout_t structures.  Under normal
 *              conditions, we only utilize a single IOC per node. Under
 *              that circumstance, we only need to fill the io_concentrator
 *              vector from the node_ranks array (which contains the index
 *              into the layout array of lowest MPI rank on each node) into
 *              the io_concentrator vector;
 *              Otherwise, while determining the number of local_peers per
 *              node, we can also select one or more additional IOCs.
 *
 *              As a side effect, we fill the 'ioc_concentrator' vector
 *              and set the 'rank_is_ioc' flag to TRUE if our rank is
 *              identified as owning an I/O Concentrator (IOC).
 *
 *-------------------------------------------------------------------------
 */
static int
identify_ioc_ranks(sf_topology_t *info, int node_count, int iocs_per_node)
{
    app_layout_t *app_layout      = NULL;
    int           total_ioc_count = 0;

    HDassert(info);
    HDassert(info->app_layout);

    app_layout = info->app_layout;

    for (int n = 0; n < node_count; n++) {
        int node_index       = app_layout->node_ranks[n];
        int local_peer_count = app_layout->node_ranks[n + 1] - app_layout->node_ranks[n];

        info->io_concentrators[total_ioc_count++] = (int)(app_layout->layout[node_index++].rank);

        if (app_layout->layout[node_index - 1].rank == app_layout->world_rank) {
            info->subfile_rank = total_ioc_count - 1;
            info->rank_is_ioc  = TRUE;
        }

        for (int k = 1; k < iocs_per_node; k++) {
            if (k < local_peer_count) {
                if (app_layout->layout[node_index].rank == app_layout->world_rank) {
                    info->rank_is_ioc  = TRUE;
                    info->subfile_rank = total_ioc_count;
                }
                info->io_concentrators[total_ioc_count++] = (int)(app_layout->layout[node_index++].rank);
            }
        }
    }

    info->n_io_concentrators = total_ioc_count;

    return total_ioc_count;
} /* end identify_ioc_ranks() */

static inline void
assign_ioc_ranks(sf_topology_t *app_topology, int ioc_count, int rank_multiple)
{
    app_layout_t *app_layout       = NULL;
    int          *io_concentrators = NULL;

    HDassert(app_topology);
    HDassert(app_topology->app_layout);
    HDassert(app_topology->io_concentrators);

    app_layout       = app_topology->app_layout;
    io_concentrators = app_topology->io_concentrators;

    /* fill the io_concentrators values based on the application layout */
    if (io_concentrators) {
        int ioc_index;
        for (int k = 0, ioc_next = 0; ioc_next < ioc_count; ioc_next++) {
            ioc_index                  = rank_multiple * k++;
            io_concentrators[ioc_next] = (int)(app_layout->layout[ioc_index].rank);
            if (io_concentrators[ioc_next] == app_layout->world_rank) {
                app_topology->subfile_rank = ioc_next;
                app_topology->rank_is_ioc  = TRUE;
            }
        }
        app_topology->n_io_concentrators = ioc_count;
    }
} /* end assign_ioc_ranks() */

/*-------------------------------------------------------------------------
 * Function:    H5_new_subfiling_object_id
 *
 * Purpose:     Given a subfiling object type and an index value, generates
 *              a new subfiling object ID.
 *
 * Return:      Non-negative object ID on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
int64_t
H5_new_subfiling_object_id(sf_obj_type_t obj_type, int64_t index_val)
{
    if (obj_type != SF_CONTEXT && obj_type != SF_TOPOLOGY)
        return -1;
    if (index_val < 0)
        return -1;

    return (((int64_t)obj_type << 32) | index_val);
}

/*-------------------------------------------------------------------------
 * Function:    H5_get_subfiling_object
 *
 * Purpose:     Given a subfiling object ID, returns a pointer to the
 *              underlying object, which can be either a subfiling context
 *              object (subfiling_context_t) or a subfiling topology
 *              object (sf_topology_t).
 *
 *              A subfiling object ID contains the object type in the upper
 *              32 bits and an index value in the lower 32 bits.
 *
 *              Subfiling contexts are 1 per open file. If only one file is
 *              open at a time, then only a single subfiling context cache
 *              entry will be used.
 *
 * Return:      Pointer to underlying subfiling object if subfiling object
 *              ID is valid
 *
 *              NULL if subfiling object ID is invalid or an internal
 *              failure occurs
 *
 *-------------------------------------------------------------------------
 */
/* TODO: no way of freeing caches on close currently */
void *
H5_get_subfiling_object(int64_t object_id)
{
    int64_t obj_type  = (object_id >> 32) & 0x0FFFF;
    int64_t obj_index = object_id & 0x0FFFF;
    void   *ret_value = NULL;

    if (obj_index < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL,
                                "invalid object index for subfiling object ID %" PRId64, object_id);

    if (obj_type == SF_CONTEXT) {
        /* Contexts provide information principally about
         * the application and how the data layout is managed
         * over some number of sub-files.  The important
         * parameters are the number of subfiles (or in the
         * context of IOCs, the MPI ranks and counts of the
         * processes which host an I/O Concentrator.  We
         * also provide a map of IOC rank to MPI rank
         * to facilitate the communication of I/O requests.
         */

        /* Create subfiling context cache if it doesn't exist */
        if (!sf_context_cache) {
            if (NULL == (sf_context_cache = HDcalloc(sf_context_cache_limit, sizeof(subfiling_context_t))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL,
                                        "couldn't allocate space for subfiling context cache");
        }

        /* Make more space in context cache if needed */
        if ((size_t)obj_index == sf_context_cache_limit) {
            size_t old_num_entries;
            void  *tmp_realloc;

            old_num_entries = sf_context_cache_limit;

            sf_context_cache_limit *= 2;

            if (NULL == (tmp_realloc = HDrealloc(sf_context_cache,
                                                 sf_context_cache_limit * sizeof(subfiling_context_t))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL,
                                        "couldn't allocate space for subfiling context cache");

            sf_context_cache = tmp_realloc;

            /* Clear newly-allocated entries */
            HDmemset(&sf_context_cache[obj_index], 0,
                     (sf_context_cache_limit - old_num_entries) * sizeof(subfiling_context_t));
        }

        /* Return direct pointer to the context cache entry */
        return (void *)&sf_context_cache[obj_index];
    }
    else if (obj_type == SF_TOPOLOGY) {
        /* Create subfiling topology cache if it doesn't exist */
        if (!sf_topology_cache) {
            if (NULL == (sf_topology_cache = HDcalloc(sf_topology_cache_limit, sizeof(sf_topology_t))))
                H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, NULL,
                                        "couldn't allocate space for subfiling topology cache");
        }

        /* We will likely only cache a single topology
         * which is that of the original parallel application.
         * In that context, we will identify the number of
         * nodes along with the number of MPI ranks on a node.
         */
        if ((size_t)obj_index >= sf_topology_cache_limit)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL,
                                    "invalid object index for subfiling topology object ID");

        /* Return direct pointer to the topology cache entry */
        return (void *)&sf_topology_cache[obj_index];
    }

#ifdef H5_SUBFILING_DEBUG
    HDprintf("%s: Unknown subfiling object type for ID %" PRId64 "\n", __func__, object_id);
#endif

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_free_subfiling_object
 *
 * Purpose:     Frees the underlying subfiling object for a given subfiling
 *              object ID.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_free_subfiling_object(int64_t object_id)
{
    subfiling_context_t *sf_context = NULL;
    int64_t              obj_type   = (object_id >> 32) & 0x0FFFF;
    herr_t               ret_value  = SUCCEED;

    if (obj_type != SF_CONTEXT)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid subfiling object type for ID %" PRId64,
                                object_id);

    if (NULL == (sf_context = H5_get_subfiling_object(object_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                "couldn't get subfiling context for subfiling object ID");

    if (H5_free_subfiling_object_int(sf_context) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "couldn't free subfiling object");

done:
    H5_SUBFILING_FUNC_LEAVE;
}

static herr_t
H5_free_subfiling_object_int(subfiling_context_t *sf_context)
{
    HDassert(sf_context);

#ifdef H5_SUBFILING_DEBUG
    if (sf_context->sf_logfile) {
        struct tm *tm = NULL;
        time_t     cur_time;

        cur_time = time(NULL);
        tm       = localtime(&cur_time);

        H5_subfiling_log(sf_context->sf_context_id, "\n-- LOGGING FINISH - %s", asctime(tm));

        HDfclose(sf_context->sf_logfile);
        sf_context->sf_logfile = NULL;
    }
#endif

    sf_context->sf_context_id           = -1;
    sf_context->h5_file_id              = UINT64_MAX;
    sf_context->h5_file_handle          = NULL;
    sf_context->sf_fid                  = -1;
    sf_context->sf_write_count          = 0;
    sf_context->sf_read_count           = 0;
    sf_context->sf_eof                  = HADDR_UNDEF;
    sf_context->sf_stripe_size          = -1;
    sf_context->sf_blocksize_per_stripe = -1;
    sf_context->sf_base_addr            = -1;

    if (sf_context->sf_msg_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_msg_comm) < 0)
            return FAIL;
        sf_context->sf_msg_comm = MPI_COMM_NULL;
    }
    if (sf_context->sf_data_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_data_comm) < 0)
            return FAIL;
        sf_context->sf_data_comm = MPI_COMM_NULL;
    }
    if (sf_context->sf_eof_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_eof_comm) < 0)
            return FAIL;
        sf_context->sf_eof_comm = MPI_COMM_NULL;
    }
    if (sf_context->sf_intra_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_intra_comm) < 0)
            return FAIL;
        sf_context->sf_intra_comm = MPI_COMM_NULL;
    }
    if (sf_context->sf_group_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_group_comm) < 0)
            return FAIL;
        sf_context->sf_group_comm = MPI_COMM_NULL;
    }

    sf_context->sf_group_size = -1;
    sf_context->sf_group_rank = -1;

    HDfree(sf_context->subfile_prefix);
    sf_context->subfile_prefix = NULL;

    HDfree(sf_context->sf_filename);
    sf_context->sf_filename = NULL;

    HDfree(sf_context->h5_filename);
    sf_context->h5_filename = NULL;

    if (H5_free_subfiling_topology(sf_context->topology) < 0)
        return FAIL;
    sf_context->topology = NULL;

    return SUCCEED;
}

static herr_t
H5_free_subfiling_topology(sf_topology_t *topology)
{
    herr_t ret_value = SUCCEED;

    HDassert(topology);

    /* Subfiling currently keeps topologies in cache until application exit */
    for (int i = 0; i < sf_topology_cache_size; i++)
        if (topology == &sf_topology_cache[i])
            return SUCCEED;

    topology->subfile_rank       = -1;
    topology->n_io_concentrators = 0;

    if (topology->app_layout) {
        HDfree(topology->app_layout->layout);
        topology->app_layout->layout = NULL;

        HDfree(topology->app_layout->node_ranks);
        topology->app_layout->node_ranks = NULL;

        HDfree(topology->app_layout);
    }

    topology->app_layout = NULL;

    HDfree(topology->io_concentrators);
    topology->io_concentrators = NULL;

    if (H5_mpi_comm_free(&topology->app_comm) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI communicator");

    HDfree(topology);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_open_subfiles
 *
 * Purpose:     Wrapper for the internal 'open__subfiles' function
 *              Similar to the other public wrapper functions, we
 *              discover (via the sf_context) the number of io concentrators
 *              and pass that to the internal function so that vector
 *              storage arrays can be stack based rather than explicitly
 *              allocated and freed.
 *
 *              The Internal function is responsible for sending all IOC
 *              instances, the (sub)file open requests.
 *
 *              Prior to calling the internal open function, we initialize
 *              a new subfiling context that contains topology info and
 *              new MPI communicators that facilitate messaging between
 *              HDF5 clients and the IOCs.
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
/* TODO: revise description */
herr_t
H5_open_subfiles(const char *base_filename, void *file_handle,
                 H5FD_subfiling_shared_config_t *subfiling_config, int file_acc_flags, MPI_Comm file_comm,
                 int64_t *context_id_out)
{
    subfiling_context_t *sf_context = NULL;
    int64_t              context_id = -1;
    int                  mpi_code;
    herr_t               ret_value = SUCCEED;

    if (!base_filename)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "invalid subfiling base filename");

    if (!subfiling_config)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "invalid subfiling configuration");

    if (!context_id_out)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "invalid subfiling context ID pointer");

    /* Initialize new subfiling context ID based on configuration information */
    if (init_subfiling(subfiling_config, file_comm, &context_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "couldn't initialize subfiling context");

    /* Retrieve the subfiling object for the newly-created context ID */
    if (NULL == (sf_context = H5_get_subfiling_object(context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "couldn't get subfiling object from context ID");

    /* Save some basic things in the new subfiling context */
    sf_context->h5_file_handle = file_handle;

    if (NULL == (sf_context->h5_filename = HDstrdup(base_filename)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for subfiling filename");

    /*
     * If we're actually using the IOCs, we will
     * start the service threads on the identified
     * ranks as part of the subfile opening.
     */
    if (open_subfile_with_context(sf_context, file_acc_flags) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "couldn't open subfiling subfiles");

#ifdef H5_SUBFILING_DEBUG
    {
        struct tm *tm = NULL;
        time_t     cur_time;
        int        mpi_rank;
        int        mpi_code;

        /* Open debugging logfile */

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_comm, &mpi_rank)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

        HDsnprintf(sf_context->sf_logfile_name, PATH_MAX, "%s.log.%d", sf_context->h5_filename, mpi_rank);

        if (NULL == (sf_context->sf_logfile = HDfopen(sf_context->sf_logfile_name, "a")))
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                        "couldn't open subfiling debug logfile");

        cur_time = time(NULL);
        tm       = localtime(&cur_time);

        H5_subfiling_log(context_id, "-- LOGGING BEGIN - %s", asctime(tm));
    }
#endif

    *context_id_out = context_id;

done:
    /*
     * Form consensus on whether opening subfiles was
     * successful
     */
    {
        int mpi_size   = -1;
        int err_result = (ret_value < 0);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_comm, &mpi_size)))
            H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

        if (mpi_size > 1) {
            if (MPI_SUCCESS !=
                (mpi_code = MPI_Allreduce(MPI_IN_PLACE, &err_result, 1, MPI_INT, MPI_MAX, file_comm)))
                H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Allreduce failed", mpi_code);
        }

        if (err_result)
            H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL,
                                    "one or more IOC ranks couldn't open subfiles");
    }

    if (ret_value < 0) {
        clear_fid_map_entry(file_handle, context_id);

        if (context_id >= 0 && H5_free_subfiling_object(context_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "couldn't free subfiling object");

        *context_id_out = -1;
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*
-------------------------------------------------------------------------
  Programmer:  Richard Warren
  Purpose:     Called as part of a file open operation, we initialize a
               subfiling context which includes the application topology
               along with other relevant info such as the MPI objects
               (communicators) for communicating with IO concentrators.
               We also identify which MPI ranks will have IOC threads
               started on them.

               We return a context ID via the 'sf_context' variable.

  Errors:      returns an error if we detect any initialization errors,
               including malloc failures or any resource allocation
               problems.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
static herr_t
init_subfiling(H5FD_subfiling_shared_config_t *subfiling_config, MPI_Comm comm, int64_t *context_id_out)
{
    subfiling_context_t *new_context  = NULL;
    sf_topology_t       *app_topology = NULL;
    MPI_Comm             intra_comm   = MPI_COMM_NULL;
    int64_t              context_id   = -1;
    int                  file_index   = -1;
    herr_t               ret_value    = SUCCEED;

    HDassert(context_id_out);

    file_index = get_next_fid_map_index();
    HDassert(file_index >= 0);

    /* Use the file's index to create a new subfiling context ID */
    if ((context_id = H5_new_subfiling_object_id(SF_CONTEXT, file_index)) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "couldn't create new subfiling context ID");

    /* Create a new subfiling context object with the created context ID */
    if (NULL == (new_context = H5_get_subfiling_object(context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "couldn't create new subfiling object");

#ifndef H5_SUBFILING_PREFER_ALLGATHER_TOPOLOGY
    {
        int mpi_rank;
        int mpi_code;

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &mpi_rank)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

        /* Create an MPI sub-communicator for intra-node communications */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_split_type(comm, MPI_COMM_TYPE_SHARED, mpi_rank,
                                                           MPI_INFO_NULL, &intra_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_split_type failed", mpi_code);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(intra_comm, MPI_ERRORS_RETURN)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_set_errhandler failed", mpi_code);
    }
#endif

    /*
     * Setup the application topology information, including the computed
     * number and distribution map of the set of I/O concentrators
     */
    if (init_app_topology(subfiling_config->ioc_selection, comm, intra_comm, &app_topology) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "couldn't initialize application topology");

    new_context->sf_context_id = context_id;

    if (init_subfiling_context(new_context, subfiling_config, app_topology, comm) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL,
                                "couldn't initialize subfiling application topology object");
    new_context->sf_intra_comm = intra_comm;

    new_context->sf_base_addr = 0;
    if (new_context->topology->rank_is_ioc) {
        new_context->sf_base_addr =
            (int64_t)(new_context->topology->subfile_rank * new_context->sf_stripe_size);
    }

    *context_id_out = context_id;

done:
    if (ret_value < 0) {
        HDfree(app_topology);

        if (H5_mpi_comm_free(&intra_comm) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "couldn't free MPI communicator");

        if (context_id >= 0 && H5_free_subfiling_object(context_id) < 0)
            H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "couldn't free subfiling object");
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    init_app_topology
 *
 * Purpose:     Once a sorted collection of hostid/mpi_rank tuples has been
 *              created and the number of unique hostids (nodes) has
 *              been determined, we may modify this "default" value for
 *              the number of IO Concentrators for this application.
 *
 *              The default of one(1) IO concentrator per node can be
 *              changed (principally for testing) by environment variable.
 *              if IOC_COUNT_PER_NODE is defined, then that integer value
 *              is utilized as a multiplier to modify the set of
 *              IO Concentrator ranks.
 *
 *              The cached results will be replicated within the
 *              subfiling_context_t structure and is utilized as a map from
 *              io concentrator rank to MPI communicator rank for message
 *              sends and receives.
 *
 * Return:      The number of IO Concentrator ranks. We also cache
 *              the MPI ranks in the 'io_concentrator' vector variable.
 *              The length of this vector is cached as 'n_io_concentrators'.
 * Errors:      MPI_Abort if memory cannot be allocated.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     - Initial Version/None.
 *              - Updated the API to allow a variety of methods for
 *                determining the number and MPI ranks that will have
 *                IO Concentrators.  The default approach will define
 *                a single IOC per node.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
init_app_topology(H5FD_subfiling_ioc_select_t ioc_selection_type, MPI_Comm comm, MPI_Comm intra_comm,
                  sf_topology_t **app_topology_out)
{
    sf_topology_t *app_topology   = NULL;
    app_layout_t  *app_layout     = NULL;
    int64_t        topology_id    = -1;
    char          *env_value      = NULL;
    char          *ioc_sel_str    = NULL;
    long           ioc_select_val = -1;
    long           iocs_per_node  = 1;
    int            ioc_count      = 0;
    int            comm_rank;
    int            comm_size;
    int            mpi_code;
    herr_t         ret_value = SUCCEED;

    HDassert(MPI_COMM_NULL != comm);
    HDassert(app_topology_out);
    HDassert(!*app_topology_out);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &comm_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &comm_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

    /* Check if an IOC selection type was specified by environment variable */
    if (get_ioc_selection_criteria_from_env(&ioc_selection_type, &ioc_sel_str) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                "couldn't get IOC selection type from environment");

    /* Sanity checking on different IOC selection strategies */
    switch (ioc_selection_type) {
        case SELECT_IOC_EVERY_NTH_RANK: {
            errno = 0;

            ioc_select_val = 1;
            if (ioc_sel_str) {
                ioc_select_val = HDstrtol(ioc_sel_str, NULL, 0);
                if ((ERANGE == errno) || (ioc_select_val <= 0)) {
                    HDprintf("invalid IOC selection strategy string '%s' for strategy "
                             "SELECT_IOC_EVERY_NTH_RANK; defaulting to SELECT_IOC_ONE_PER_NODE\n",
                             ioc_sel_str);
                    ioc_select_val     = 1;
                    ioc_selection_type = SELECT_IOC_ONE_PER_NODE;
                }
            }

            break;
        }

        case SELECT_IOC_WITH_CONFIG:
            HDprintf("SELECT_IOC_WITH_CONFIG IOC selection strategy not supported yet; defaulting to "
                     "SELECT_IOC_ONE_PER_NODE\n");
            ioc_selection_type = SELECT_IOC_ONE_PER_NODE;
            break;

        case SELECT_IOC_TOTAL: {
            errno = 0;

            ioc_select_val = 1;
            if (ioc_sel_str) {
                ioc_select_val = HDstrtol(ioc_sel_str, NULL, 0);
                if ((ERANGE == errno) || (ioc_select_val <= 0) || (ioc_select_val >= comm_size)) {
                    HDprintf("invalid IOC selection strategy string '%s' for strategy SELECT_IOC_TOTAL; "
                             "defaulting to SELECT_IOC_ONE_PER_NODE\n",
                             ioc_sel_str);
                    ioc_select_val     = 1;
                    ioc_selection_type = SELECT_IOC_ONE_PER_NODE;
                }
            }

            break;
        }

        default:
            break;
    }

    /*
     * TODO: A different IOC selection string from the environment than what was
     *       used originally will cause the IOCs to be assigned differently than
     *       expected. While this generally shouldn't cause issues (other than
     *       for the SELECT_IOC_TOTAL case), this should still be dealt with
     *       eventually.
     */
    /* Check the subfiling topology cache to see if there's a matching object */
    if (find_cached_topology_info(comm, ioc_selection_type, &app_topology) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                "can't check for cached subfiling topology object");
    HDassert(!app_topology || (app_topology->selection_type == ioc_selection_type));

    if (!app_topology) {
        /* Generate an ID for the application topology object */
        if ((topology_id = H5_new_subfiling_object_id(SF_TOPOLOGY, (int64_t)sf_topology_cache_size)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get ID for subfiling topology object");
        sf_topology_cache_size++;

        /* Get a new application topology object from the cache */
        if (NULL == (app_topology = H5_get_subfiling_object(topology_id)))
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get subfiling topology object");
        app_topology->app_layout         = NULL;
        app_topology->app_comm           = MPI_COMM_NULL;
        app_topology->rank_is_ioc        = FALSE;
        app_topology->subfile_rank       = -1;
        app_topology->n_io_concentrators = -1;
        app_topology->io_concentrators   = NULL;
        app_topology->selection_type     = ioc_selection_type;

        if (H5_mpi_comm_dup(comm, &app_topology->app_comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTCOPY, FAIL, "can't duplicate MPI communicator");

        if (NULL == (app_topology->io_concentrators = HDcalloc((size_t)comm_size, sizeof(int))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't allocate array of I/O concentrator ranks");

        if (NULL == (app_layout = HDcalloc(1, sizeof(*app_layout))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't allocate application layout structure");
        app_layout->world_size = comm_size;
        app_layout->world_rank = comm_rank;

        if (NULL == (app_layout->node_ranks = HDcalloc(1, ((size_t)comm_size + 1) * sizeof(int))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't allocate application layout node rank array");

        if (NULL == (app_layout->layout = HDcalloc(1, ((size_t)comm_size + 1) * sizeof(layout_t))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't allocate application layout array");

        app_topology->app_layout = app_layout;

        if (gather_topology_info(app_topology, comm, intra_comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL, "can't gather application topology info");

        /*
         * Determine which ranks are I/O concentrator ranks, based on the
         * given IOC selection strategy and MPI information.
         */
        switch (ioc_selection_type) {
            case SELECT_IOC_ONE_PER_NODE: {
                int node_count;

                app_topology->selection_type = SELECT_IOC_ONE_PER_NODE;

                if ((node_count = count_nodes(app_topology, comm)) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                            "couldn't determine number of nodes used");

                /* Check for an IOC-per-node value set in the environment */
                if ((env_value = HDgetenv(H5FD_SUBFILING_IOC_PER_NODE))) {
                    errno          = 0;
                    ioc_select_val = HDstrtol(env_value, NULL, 0);
                    if ((ERANGE == errno)) {
                        HDprintf("invalid value '%s' for " H5FD_SUBFILING_IOC_PER_NODE "\n", env_value);
                        ioc_select_val = 1;
                    }

                    if (ioc_select_val > 0)
                        iocs_per_node = ioc_select_val;
                }

                H5_CHECK_OVERFLOW(iocs_per_node, long, int);
                ioc_count = identify_ioc_ranks(app_topology, node_count, (int)iocs_per_node);

                break;
            }

            case SELECT_IOC_EVERY_NTH_RANK: {
                /*
                 * User specifies a rank multiple value. Selection starts
                 * with rank 0 and then the user-specified stride is applied\
                 * to identify other IOC ranks.
                 */

                H5_CHECK_OVERFLOW(ioc_select_val, long, int);
                ioc_count = (comm_size / (int)ioc_select_val);

                if ((comm_size % ioc_select_val) != 0) {
                    ioc_count++;
                }

                assign_ioc_ranks(app_topology, ioc_count, (int)ioc_select_val);

                break;
            }

            case SELECT_IOC_TOTAL: {
                int rank_multiple = 0;

                /*
                 * User specifies a total number of I/O concentrators.
                 * Starting with rank 0, a stride of (mpi_size / total)
                 * is applied to identify other IOC ranks.
                 */

                H5_CHECK_OVERFLOW(ioc_select_val, long, int);
                ioc_count = (int)ioc_select_val;

                rank_multiple = (comm_size / ioc_count);

                assign_ioc_ranks(app_topology, ioc_count, rank_multiple);

                break;
            }

            case SELECT_IOC_WITH_CONFIG:
            default:
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid IOC selection strategy");
                break;
        }

        HDassert(ioc_count > 0);
        app_topology->n_io_concentrators = ioc_count;
    }

    *app_topology_out = app_topology;

done:
    if (ret_value < 0) {
        if (app_layout) {
            HDfree(app_layout->layout);
            HDfree(app_layout->node_ranks);
            HDfree(app_layout);
        }
        if (topology_id >= 0) {
            sf_topology_cache_size--;

            if (app_topology) {
                HDfree(app_topology->io_concentrators);
                if (H5_mpi_comm_free(&app_topology->app_comm) < 0)
                    H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "can't free MPI communicator");
                HDfree(app_topology);
            }
        }
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    init_subfile_context
 *
 * Purpose:     Called as part of the HDF5 file + subfiling opening.
 *              This initializes the subfiling context and associates
 *              this context with the specific HDF5 file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
static herr_t
init_subfiling_context(subfiling_context_t *sf_context, H5FD_subfiling_shared_config_t *subfiling_config,
                       sf_topology_t *app_topology, MPI_Comm file_comm)
{
    char  *env_value = NULL;
    int    comm_rank;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    HDassert(sf_context);
    HDassert(sf_context->topology == NULL);
    HDassert(subfiling_config);
    HDassert(app_topology);
    HDassert(app_topology->n_io_concentrators > 0);
    HDassert(MPI_COMM_NULL != file_comm);

    sf_context->topology       = app_topology;
    sf_context->sf_msg_comm    = MPI_COMM_NULL;
    sf_context->sf_data_comm   = MPI_COMM_NULL;
    sf_context->sf_eof_comm    = MPI_COMM_NULL;
    sf_context->sf_intra_comm  = MPI_COMM_NULL;
    sf_context->sf_group_comm  = MPI_COMM_NULL;
    sf_context->sf_stripe_size = H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
    sf_context->sf_write_count = 0;
    sf_context->sf_read_count  = 0;
    sf_context->sf_eof         = HADDR_UNDEF;
    sf_context->h5_file_handle = NULL;
    sf_context->sf_fid         = -1;
    sf_context->sf_group_size  = 1;
    sf_context->sf_group_rank  = 0;
    sf_context->h5_filename    = NULL;
    sf_context->sf_filename    = NULL;
    sf_context->subfile_prefix = NULL;
    sf_context->ioc_data       = NULL;

#ifdef H5_SUBFILING_DEBUG
    sf_context->sf_logfile = NULL;
#endif

    /*
     * Set IOC stripe size from subfiling configuration, then check
     * for a setting from the environment
     */
    if (subfiling_config->stripe_size > 0)
        sf_context->sf_stripe_size = subfiling_config->stripe_size;

    if ((env_value = HDgetenv(H5FD_SUBFILING_STRIPE_SIZE))) {
        long long stripe_size = -1;

        errno = 0;

        stripe_size = HDstrtoll(env_value, NULL, 0);
        if (ERANGE == errno)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                                        "invalid stripe size setting for " H5FD_SUBFILING_STRIPE_SIZE);

        if (stripe_size > 0) {
            sf_context->sf_stripe_size = (int64_t)stripe_size;
        }
    }

    /*
     * Set blocksize per stripe value after possibly adjusting
     * for user-specified subfile stripe size
     */
    sf_context->sf_blocksize_per_stripe = sf_context->sf_stripe_size * app_topology->n_io_concentrators;

    /* Check for a subfile name prefix setting in the environment */
    if ((env_value = HDgetenv(H5FD_SUBFILING_SUBFILE_PREFIX))) {
        if (NULL == (sf_context->subfile_prefix = HDstrdup(env_value)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't copy subfile prefix value");
    }

    /*
     * Set up various MPI sub-communicators for MPI operations
     * to/from IOC ranks
     */

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_comm, &comm_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_msg_comm)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_msg_comm, MPI_ERRORS_RETURN)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_set_errhandler failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_data_comm)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_data_comm, MPI_ERRORS_RETURN)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_set_errhandler failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_eof_comm)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_dup failed", mpi_code);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_eof_comm, MPI_ERRORS_RETURN)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_set_errhandler failed", mpi_code);

    /* Create an MPI sub-communicator for IOC ranks */
    if (app_topology->n_io_concentrators > 1) {
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_split(file_comm, app_topology->rank_is_ioc, comm_rank,
                                                      &sf_context->sf_group_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_split failed", mpi_code);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(sf_context->sf_group_comm, &sf_context->sf_group_rank)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(sf_context->sf_group_comm, &sf_context->sf_group_size)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);
    }

done:
    if (ret_value < 0) {
        H5_free_subfiling_object_int(sf_context);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    open_subfile_with_context
 *
 * Purpose:     While we cannot know a priori, whether an HDF client will
 *              need to access data across the entirety of a file, e.g.
 *              an individual MPI rank may read or write only small
 *              segments of the entire file space; this function sends
 *              a file OPEN_OP to every IO concentrator.
 *
 *              Prior to opening any subfiles, the H5FDopen will have
 *              created an HDF5 file with the user specified naming.
 *              A path prefix will be selected and is available as
 *              an input argument.
 *
 *              The opened HDF5 file handle will contain device and
 *              inode values, these being constant for all processes
 *              opening the shared file.  The inode value is utilized
 *              as a key value and is associated with the sf_context
 *              which we receive as one of the input arguments.
 *
 *              IO Concentrator threads will be initialized on MPI ranks
 *              which have been identified via application toplogy
 *              discovery.  The number and mapping of IOC to MPI_rank
 *              is part of the sf_context->topology structure.
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
static herr_t
open_subfile_with_context(subfiling_context_t *sf_context, int file_acc_flags)
{
    herr_t ret_value = SUCCEED;

    HDassert(sf_context);

    /*
     * Save the HDF5 file ID (fid) to subfile context mapping.
     * There shouldn't be any issue, but check the status and
     * return if there was a problem.
     */
    if (record_fid_to_subfile(sf_context->h5_file_handle, sf_context->sf_context_id, NULL) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, FAIL,
                                "couldn't record HDF5 file ID to subfile context mapping");

    /*
     * If this rank is an I/O concentrator, actually open
     * the subfile belonging to this IOC rank
     */
    if (sf_context->topology->rank_is_ioc) {
        if (sf_context->sf_group_rank == 0) {
            h5_stat_t st;

            HDcompile_assert(sizeof(uint64_t) >= sizeof(ino_t));

            /* Retrieve Inode value for HDF5 stub file and broadcast to other IOCs */
            if (HDstat(sf_context->h5_filename, &st) < 0) {
                sf_context->h5_file_id = UINT64_MAX;
                H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                        "couldn't stat HDF5 stub file, errno = %d, error message = '%s'",
                                        errno, strerror(errno));
            }
            else
                sf_context->h5_file_id = (uint64_t)st.st_ino;
        }

        if (sf_context->sf_group_size > 1) {
            int mpi_code;

            if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&sf_context->h5_file_id, 1, MPI_UINT64_T, 0,
                                                     sf_context->sf_group_comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
        }

        if (sf_context->h5_file_id == UINT64_MAX)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL,
                                    "couldn't get inode value for HDF5 stub file");

        if (ioc_open_file(sf_context->sf_context_id, file_acc_flags) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL, "IOC couldn't open subfile");
    }

done:
    if (ret_value < 0) {
        clear_fid_map_entry(sf_context->h5_file_handle, sf_context->sf_context_id);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    record_fid_to_subfile
 *
 * Purpose:     Every opened HDF5 file will have (if utilizing subfiling)
 *              a subfiling context associated with it. It is important that
 *              the HDF5 file index is a constant rather than utilizing a
 *              posix file handle since files can be opened multiple times
 *              and with each file open, a new file handle will be assigned.
 *              Note that in such a case, the actual filesystem id will be
 *              retained.
 *
 *              We utilize that filesystem id (ino_t inode) so that
 *              irrespective of what process opens a common file, the
 *              subfiling system will generate a consistent context for this
 *              file across all parallel ranks.
 *
 *              This function simply records the filesystem handle to
 *              subfiling context mapping.
 *
 * Return:      SUCCEED or FAIL.
 * Errors:      FAILs ONLY if storage for the mapping entry cannot
 *              be allocated.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
record_fid_to_subfile(void *file_handle, int64_t subfile_context_id, int *next_index)
{
    int    index;
    herr_t ret_value = SUCCEED;

    if (sf_file_map_size == 0) {
        if (NULL ==
            (sf_open_file_map = HDmalloc((size_t)DEFAULT_FILE_MAP_ENTRIES * sizeof(*sf_open_file_map))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't allocate open file mapping");

        sf_file_map_size = DEFAULT_FILE_MAP_ENTRIES;
        for (int i = 0; i < sf_file_map_size; i++) {
            sf_open_file_map[i].file_handle   = NULL;
            sf_open_file_map[i].sf_context_id = -1;
        }
    }

    for (index = 0; index < sf_file_map_size; index++) {
        if (sf_open_file_map[index].file_handle == file_handle)
            goto done;

        if (sf_open_file_map[index].file_handle == NULL) {
            sf_open_file_map[index].file_handle   = file_handle;
            sf_open_file_map[index].sf_context_id = subfile_context_id;

            if (next_index) {
                *next_index = index;
            }

            goto done;
        }
    }

    if (index == sf_file_map_size) {
        void *tmp_realloc;

        if (NULL == (tmp_realloc = HDrealloc(sf_open_file_map,
                                             ((size_t)(sf_file_map_size * 2) * sizeof(*sf_open_file_map)))))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't reallocate open file mapping");

        sf_open_file_map = tmp_realloc;
        sf_file_map_size *= 2;

        for (int i = index; i < sf_file_map_size; i++) {
            sf_open_file_map[i].file_handle = NULL;
        }

        if (next_index) {
            *next_index = index;
        }

        sf_open_file_map[index].file_handle     = file_handle;
        sf_open_file_map[index++].sf_context_id = subfile_context_id;
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    ioc_open_file
 *
 * Purpose:     This function is called by an I/O concentrator in order to
 *              open the subfile it is responsible for.
 *
 *              The name of the subfile to be opened is generated based on
 *              values from either:
 *
 *              - The corresponding subfiling configuration file, if one
 *                exists and the HDF5 file isn't being truncated
 *              - The current subfiling context object for the file, if a
 *                subfiling configuration file doesn't exist or the HDF5
 *                file is being truncated
 *
 *              After the subfile has been opened, a subfiling
 *              configuration file will be created if this is a file
 *              creation operation. If the truncate flag is specified, the
 *              subfiling configuration file will be re-created in order to
 *              account for any possible changes in the subfiling
 *              configuration.
 *
 *              Note that the HDF5 file opening protocol may attempt to
 *              open a file twice. A first open attempt is made without any
 *              truncate or other flags which would modify the file state
 *              if it already exists. Then, if this tentative open wasn't
 *              sufficient, the file is closed and a second file open using
 *              the user supplied open flags is invoked.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
ioc_open_file(int64_t file_context_id, int file_acc_flags)
{
    subfiling_context_t *sf_context  = NULL;
    mode_t               mode        = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    char                *filepath    = NULL;
    char                *subfile_dir = NULL;
    char                *base        = NULL;
    int                  fd          = -1;
    herr_t               ret_value   = SUCCEED;

    if (NULL == (sf_context = H5_get_subfiling_object(file_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL,
                                "couldn't get subfiling object from context ID");

    /* Only IOC ranks should be here */
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->subfile_rank >= 0);

    if (NULL == (filepath = HDcalloc(1, PATH_MAX)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for subfile filename");

    /* Generate the name of the subfile that this IOC rank will open */
    if (generate_subfile_name(sf_context, file_acc_flags, filepath, PATH_MAX, &base, &subfile_dir) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTOPENFILE, FAIL, "couldn't generate name for subfile");

    if (NULL == (sf_context->sf_filename = HDstrdup(filepath)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't copy subfile name");

    /* Attempt to create/open the subfile for this IOC rank */
    if ((fd = HDopen(filepath, file_acc_flags, mode)) < 0)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "failed to open subfile");

    sf_context->sf_fid = fd;
    if (file_acc_flags & O_CREAT)
        sf_context->sf_eof = 0;

    /*
     * If subfiles were created (rather than simply opened),
     * check if we also need to create a config file.
     */
    if ((file_acc_flags & O_CREAT) && (sf_context->topology->subfile_rank == 0)) {
        if (create_config_file(sf_context, base, subfile_dir, (file_acc_flags & O_TRUNC)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTCREATE, FAIL,
                                    "couldn't create subfiling configuration file");
    }

done:
    if (ret_value < 0) {
        if (sf_context) {
            HDfree(sf_context->sf_filename);
            sf_context->sf_filename = NULL;

            if (sf_context->sf_fid >= 0) {
                HDclose(sf_context->sf_fid);
                sf_context->sf_fid = -1;
            }
        }
    }

    H5MM_free(base);
    H5MM_free(subfile_dir);
    HDfree(filepath);

    H5_SUBFILING_FUNC_LEAVE;
}

/*
 * Generate the name of the subfile this IOC rank will open,
 * based on available information.
 *
 * This may include:
 * - the subfiling configuration (from a subfiling configuration
 *   file if one exists, or from the subfiling context object
 *   otherwise)
 * - the base file's name and ID (inode or similar)
 * - the IOC's rank value within the set of I/O concentrators
 * - an optional filename prefix specified by the user
 *
 * This routine is collective across the set of I/O concentrator
 * ranks, as some information may be read from the subfiling
 * configuration file and we don't want to do that from all
 * ranks.
 */
static herr_t
generate_subfile_name(subfiling_context_t *sf_context, int file_acc_flags, char *filename_out,
                      size_t filename_out_len, char **filename_basename_out, char **subfile_dir_out)
{
    FILE  *config_file = NULL;
    char  *subfile_dir = NULL;
    char  *prefix      = NULL;
    char  *base        = NULL;
    int    n_io_concentrators;
    int    num_digits;
    herr_t ret_value = SUCCEED;

    HDassert(sf_context);
    HDassert(sf_context->h5_filename);
    HDassert(filename_out);
    HDassert(filename_basename_out);
    HDassert(subfile_dir_out);

    *filename_basename_out = NULL;
    *subfile_dir_out       = NULL;

    /*
     * Initially use the number of I/O concentrators specified in the
     * subfiling context. However, if there's an existing subfiling
     * configuration file (and we aren't truncating it) we will use
     * the number specified there instead, as that should be the actual
     * number that the subfile names were originally generated with.
     * The current subfiling context may have a different number of I/O
     * concentrators specified; e.g. a simple serial file open for
     * reading purposes (think h5dump) might only be using 1 I/O
     * concentrator, whereas the file was created with several I/O
     * concentrators.
     */
    n_io_concentrators = sf_context->topology->n_io_concentrators;

    if (NULL == (prefix = HDmalloc(PATH_MAX)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for subfile prefix");

    /* Under normal operation, we co-locate subfiles with the HDF5 file */
    HDstrncpy(prefix, sf_context->h5_filename, PATH_MAX - 1);
    prefix[PATH_MAX - 1] = '\0';

    if (H5_basename(prefix, &base) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't get subfile basename");

    if (sf_context->subfile_prefix) {
        /* Note: Users may specify a directory name which is inaccessible
         * from where the current is running.  In particular, "node-local"
         * storage is not uniformly available to all processes.
         * We would like to check if the user pathname unavailable and
         * if so, we could default to creating the subfiles in the
         * current directory. (?)
         */
        if (NULL == (subfile_dir = H5MM_strdup(sf_context->subfile_prefix)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't copy subfile prefix");
    }
    else {
        if (H5_dirname(prefix, &subfile_dir) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "couldn't get subfile prefix");
    }

    /*
     * Open the file's subfiling configuration file, if it exists and
     * we aren't truncating the file.
     */
    if (0 == (file_acc_flags & O_TRUNC)) {
        int n_iocs_from_config_file = 0;

        if (sf_context->sf_group_rank == 0) {
            if (open_config_file(sf_context, base, subfile_dir, "r", &config_file) < 0)
                n_iocs_from_config_file = -1;
            else if (!config_file)
                n_iocs_from_config_file = -2; /* No config file; use setting from context */
            else {
                /*
                 * If a subfiling configuration file exists and we aren't truncating
                 * it, read the number of I/O concentrators used at file creation time
                 * in order to generate the correct subfile names.
                 */
                if (H5_get_num_iocs_from_config_file(config_file, &n_iocs_from_config_file) < 0)
                    n_iocs_from_config_file = -1;
            }
        }

        if (sf_context->sf_group_size > 1) {
            int mpi_code;

            if (MPI_SUCCESS !=
                (mpi_code = MPI_Bcast(&n_iocs_from_config_file, 1, MPI_INT, 0, sf_context->sf_group_comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
        }

        if (n_iocs_from_config_file > 0)
            n_io_concentrators = n_iocs_from_config_file;
        else if (n_iocs_from_config_file == -1)
            H5_SUBFILING_GOTO_ERROR(
                H5E_FILE, H5E_CANTOPENFILE, FAIL,
                "couldn't read the number of I/O concentrators from subfiling configuration file");
    }

    /*
     * Generate the name of the subfile. The subfile naming should
     * produce files of the following form:
     * If we assume the HDF5 file is named ABC.h5, and 20 I/O
     * concentrators are used, then the subfiles will have names:
     *   ABC.h5.subfile_<file-number>_01_of_20,
     *   ABC.h5.subfile_<file-number>_02_of_20, etc.
     *
     * and the configuration file will be named:
     *   ABC.h5.subfile_<file-number>.config
     */
    num_digits = (int)(HDlog10(n_io_concentrators) + 1);
    HDsnprintf(filename_out, filename_out_len, "%s/" H5FD_SUBFILING_FILENAME_TEMPLATE, subfile_dir, base,
               sf_context->h5_file_id, num_digits, sf_context->topology->subfile_rank + 1,
               n_io_concentrators);

    *filename_basename_out = base;
    *subfile_dir_out       = subfile_dir;

done:
    if (config_file && (EOF == HDfclose(config_file)))
        H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL,
                                "couldn't close subfiling configuration file");

    if (ret_value < 0) {
        H5MM_free(subfile_dir);
        H5MM_free(base);

        if (*filename_basename_out) {
            H5MM_free(*filename_basename_out);
            *filename_basename_out = NULL;
        }
        if (*subfile_dir_out) {
            H5MM_free(*subfile_dir_out);
            *subfile_dir_out = NULL;
        }
    }

    HDfree(prefix);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    create_config_file
 *
 * Purpose:     Creates a configuration file that contains
 *              subfiling-related information for a file. This file
 *              includes information such as:
 *
 *              - the stripe size for the file's subfiles
 *              - the number of I/O concentrators used for I/O to the file's subfiles
 *              - the base HDF5 filename
 *              - the optional directory prefix where the file's subfiles are placed
 *              - the names of each of the file's subfiles
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
create_config_file(subfiling_context_t *sf_context, const char *base_filename, const char *subfile_dir,
                   hbool_t truncate_if_exists)
{
    hbool_t config_file_exists = FALSE;
    FILE   *config_file        = NULL;
    char   *config_filename    = NULL;
    char   *line_buf           = NULL;
    int     ret                = 0;
    herr_t  ret_value          = SUCCEED;

    HDassert(sf_context);
    HDassert(base_filename);
    HDassert(subfile_dir);

    if (sf_context->h5_file_id == UINT64_MAX)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid HDF5 file ID %" PRIu64,
                                sf_context->h5_file_id);
    if (*base_filename == '\0')
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid base HDF5 filename '%s'",
                                base_filename);
    if (*subfile_dir == '\0')
        subfile_dir = ".";

    if (NULL == (config_filename = HDmalloc(PATH_MAX)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for subfiling configuration filename");

    HDsnprintf(config_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, subfile_dir,
               base_filename, sf_context->h5_file_id);

    /* Determine whether a subfiling configuration file exists */
    errno = 0;
    ret   = HDaccess(config_filename, F_OK);

    config_file_exists = (ret == 0) || ((ret < 0) && (ENOENT != errno));

    if (config_file_exists && (ret != 0))
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                    "couldn't check existence of subfiling configuration file");

    /*
     * If a config file doesn't exist, create one. If a
     * config file does exist, don't touch it unless the
     * O_TRUNC flag was specified. In this case, truncate
     * the existing config file and create a new one.
     */
    /* TODO: if truncating, consider removing old stale config files. */
    if (!config_file_exists || truncate_if_exists) {
        int n_io_concentrators = sf_context->topology->n_io_concentrators;
        int num_digits;

        if (NULL == (config_file = HDfopen(config_filename, "w+")))
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                        "couldn't open subfiling configuration file");

        if (NULL == (line_buf = HDmalloc(PATH_MAX)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "couldn't allocate buffer for writing to subfiling configuration file");

        /* Write the subfiling stripe size to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "stripe_size=%" PRId64 "\n", sf_context->sf_stripe_size);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                                        "failed to write to subfiling configuration file");

        /* Write the number of I/O concentrators to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "aggregator_count=%d\n", n_io_concentrators);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                                        "failed to write to subfiling configuration file");

        /* Write the base HDF5 filename to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "hdf5_file=%s\n", sf_context->h5_filename);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                                        "failed to write to subfiling configuration file");

        /* Write the optional subfile directory prefix to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "subfile_dir=%s\n", subfile_dir);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                                        "failed to write to subfiling configuration file");

        /* Write out each subfile name to the configuration file */
        num_digits = (int)(HDlog10(n_io_concentrators) + 1);
        for (int k = 0; k < n_io_concentrators; k++) {
            HDsnprintf(line_buf, PATH_MAX, H5FD_SUBFILING_FILENAME_TEMPLATE "\n", base_filename,
                       sf_context->h5_file_id, num_digits, k + 1, n_io_concentrators);

            if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1)
                H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_WRITEERROR, FAIL,
                                            "failed to write to subfiling configuration file");
        }
    }

done:
    if (config_file) {
        if (EOF == HDfclose(config_file))
            H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL,
                                    "couldn't close subfiling configuration file");
    }

    HDfree(line_buf);
    HDfree(config_filename);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    open_config_file
 *
 * Purpose:     Opens the subfiling configuration file for a given HDF5
 *              file and sets `config_file_out`, if a configuration file
 *              exists. Otherwise, `config_file_out` is set to NULL.
 *
 *              It is the caller's responsibility to check
 *              `config_file_out` on success and close an opened file as
 *              necessary.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
open_config_file(subfiling_context_t *sf_context, const char *base_filename, const char *subfile_dir,
                 const char *mode, FILE **config_file_out)
{
    hbool_t config_file_exists = FALSE;
    FILE   *config_file        = NULL;
    char   *config_filename    = NULL;
    int     ret                = 0;
    herr_t  ret_value          = SUCCEED;

    HDassert(sf_context);
    HDassert(base_filename);
    HDassert(subfile_dir);
    HDassert(mode);
    HDassert(config_file_out);

    *config_file_out = NULL;

    if (sf_context->h5_file_id == UINT64_MAX)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid HDF5 file ID %" PRIu64,
                                sf_context->h5_file_id);
    if (*base_filename == '\0')
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL, "invalid base HDF5 filename '%s'",
                                base_filename);
    if (*subfile_dir == '\0')
        subfile_dir = ".";

    if (NULL == (config_filename = HDmalloc(PATH_MAX)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for subfiling configuration filename");

    HDsnprintf(config_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE, subfile_dir,
               base_filename, sf_context->h5_file_id);

    /* Determine whether a subfiling configuration file exists */
    errno = 0;
    ret   = HDaccess(config_filename, F_OK);

    config_file_exists = (ret == 0) || ((ret < 0) && (ENOENT != errno));

    if (!config_file_exists)
        goto done;

    if (config_file_exists && (ret != 0))
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                    "couldn't check existence of subfiling configuration file");

    if (NULL == (config_file = HDfopen(config_filename, mode)))
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                    "couldn't open subfiling configuration file");

    *config_file_out = config_file;

done:
    if (ret_value < 0) {
        if (config_file && (EOF == HDfclose(config_file)))
            H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL,
                                    "couldn't close subfiling configuration file");
    }

    HDfree(config_filename);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_get_num_iocs_from_config_file
 *
 * Purpose:     Reads a Subfiling configuration file to get the number of
 *              I/O concentrators used for the logical HDF5 file.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_get_num_iocs_from_config_file(FILE *config_file, int *n_io_concentrators)
{
    char  *config_buf      = NULL;
    char  *ioc_substr      = NULL;
    long   config_file_len = 0;
    int    read_n_io_concs = 0;
    herr_t ret_value       = SUCCEED;

    HDassert(config_file);
    HDassert(n_io_concentrators);

    if (HDfseek(config_file, 0, SEEK_END) < 0)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL,
                                    "couldn't seek to end of subfiling configuration file");

    if ((config_file_len = HDftell(config_file)) < 0)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL,
                                    "couldn't get size of subfiling configuration file");

    if (HDfseek(config_file, 0, SEEK_SET) < 0)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_SEEKERROR, FAIL,
                                    "couldn't seek to beginning of subfiling configuration file");

    if (NULL == (config_buf = HDmalloc((size_t)config_file_len + 1)))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                "couldn't allocate space for reading from subfiling configuration file");

    if (HDfread(config_buf, (size_t)config_file_len, 1, config_file) != 1)
        H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL,
                                    "couldn't read from subfiling configuration file");

    config_buf[config_file_len] = '\0';

    if (NULL == (ioc_substr = HDstrstr(config_buf, "aggregator_count")))
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                                "malformed subfiling configuration file - no aggregator count entry");

    if (EOF == HDsscanf(ioc_substr, "aggregator_count=%d", &read_n_io_concs))
        H5_SUBFILING_SYS_GOTO_ERROR(
            H5E_FILE, H5E_CANTGET, FAIL,
            "couldn't get number of I/O concentrators from subfiling configuration file");

    if (read_n_io_concs <= 0)
        H5_SUBFILING_GOTO_ERROR(
            H5E_FILE, H5E_BADVALUE, FAIL,
            "invalid number of I/O concentrators (%d) read from subfiling configuration file",
            read_n_io_concs);

    *n_io_concentrators = read_n_io_concs;

done:
    HDfree(config_buf);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_resolve_pathname
 *
 * Purpose:     Simple wrapper routine around realpath(3) to fully resolve
 *              a given filepath. Collective across the specified MPI
 *              communicator in order to minimize file system contention
 *              between MPI ranks.
 *
 *              The resolved filepath returned through `resolved_filepath`
 *              must be freed by the caller with HDfree.
 *
 * Return       Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5_resolve_pathname(const char *filepath, MPI_Comm comm, char **resolved_filepath)
{
    hsize_t path_len         = HSIZE_UNDEF;
    hbool_t bcasted_path_len = FALSE;
    hbool_t bcasted_path     = FALSE;
    char   *resolved_path    = NULL;
    char   *file_basename    = NULL;
    char   *cwd              = NULL;
    int     mpi_rank;
    int     mpi_size;
    int     mpi_code;
    herr_t  ret_value = SUCCEED;

    HDassert(filepath);
    HDassert(resolved_filepath);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &mpi_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

    if (mpi_rank == 0) {
        if (NULL == (resolved_path = HDrealpath(filepath, NULL))) {
            if (ENOENT == errno) {
                if (NULL == (resolved_path = HDmalloc(PATH_MAX)))
                    H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for filepath");
                if (H5_basename(filepath, &file_basename) < 0)
                    H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't get file basename");
                if (NULL == (cwd = HDmalloc(PATH_MAX)))
                    H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate buffer for CWD");

                if (NULL == HDgetcwd(cwd, PATH_MAX))
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                            "can't get current working directory, errno = %d, error message = '%s'", errno,
                                            HDstrerror(errno));

                HDsnprintf(resolved_path, PATH_MAX, "%s/%s", cwd, file_basename);
            }
            else
                H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL,
                                        "can't resolve subfile path, errno = %d, error message = '%s'", errno,
                                        HDstrerror(errno));
        }

        if (resolved_path) {
            H5_CHECKED_ASSIGN(path_len, hsize_t, (HDstrlen(resolved_path) + 1), size_t);
        }
        else
            path_len = HSIZE_UNDEF;
    }

    /* Broadcast the size of the resolved filepath string to other ranks */
    bcasted_path_len = TRUE;
    if (mpi_size > 1) {
        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&path_len, 1, HSIZE_AS_MPI_TYPE, 0, comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
    }

    if (path_len == HSIZE_UNDEF)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "couldn't resolve filepath");

    if (mpi_rank != 0) {
        if (NULL == (resolved_path = HDmalloc(path_len)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate file name buffer");
    }

    /* Broadcast the resolved filepath to other ranks */
    bcasted_path = TRUE;
    if (mpi_size > 1) {
        H5_CHECK_OVERFLOW(path_len, hsize_t, int);
        if (MPI_SUCCESS != (mpi_code = MPI_Bcast(resolved_path, (int)path_len, MPI_CHAR, 0, comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
    }

    *resolved_filepath = resolved_path;

done:
    HDfree(cwd);
    H5MM_free(file_basename);

    if (ret_value < 0) {
        if (!bcasted_path_len) {
            if (MPI_SUCCESS != (mpi_code = MPI_Bcast(&path_len, 1, HSIZE_AS_MPI_TYPE, 0, comm)))
                H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
        }
        if (!bcasted_path) {
            H5_CHECK_OVERFLOW(path_len, hsize_t, int);
            if (MPI_SUCCESS != (mpi_code = MPI_Bcast(resolved_path, (int)path_len, MPI_CHAR, 0, comm)))
                H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Bcast failed", mpi_code);
        }

        HDfree(resolved_path);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_close_subfiles
 *
 * Purpose:     This is a simple wrapper function for the internal version
 *              which actually manages all subfile closing via commands
 *              to the set of IO Concentrators.
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
/*-------------------------------------------------------------------------
 * Function:    Internal close__subfiles
 *
 * Purpose:     When closing and HDF5 file, we need to close any associated
 *              subfiles as well.  This function cycles through all known
 *              IO Concentrators to send a file CLOSE_OP command.
 *
 *              This function is collective across all MPI ranks which
 *              have opened HDF5 file which associated with the provided
 *              sf_context.  Once the request has been issued by all
 *              ranks, the subfile at each IOC will be closed and an
 *              completion ACK will be received.
 *
 *              Once the subfiles are closed, we initiate a teardown of
 *              the IOC and associated thread_pool threads.
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
herr_t
H5_close_subfiles(int64_t subfiling_context_id, MPI_Comm file_comm)
{
    subfiling_context_t *sf_context  = NULL;
    MPI_Request          barrier_req = MPI_REQUEST_NULL;
    int                  mpi_size;
    int                  mpi_code;
    herr_t               ret_value = SUCCEED;

    if (NULL == (sf_context = H5_get_subfiling_object(subfiling_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "couldn't get subfiling object from context ID");

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_comm, &mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

    /* We make the subfile close operation collective.
     * Otherwise, there may be a race condition between
     * our closing the subfiles and the user application
     * moving ahead and possibly re-opening a file.
     *
     * If we can, we utilize an async barrier which gives
     * us the opportunity to reduce the CPU load due to
     * MPI spinning while waiting for the barrier to
     * complete.  This is especially important if there
     * is heavy thread utilization due to subfiling
     * activities, i.e. the thread pool might be
     * extremely busy servicing I/O requests from all
     * HDF5 application ranks.
     */
    if (mpi_size > 1) {
#if MPI_VERSION > 3 || (MPI_VERSION == 3 && MPI_SUBVERSION >= 1)
        int barrier_complete = 0;

        if (MPI_SUCCESS != (mpi_code = MPI_Ibarrier(file_comm, &barrier_req)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Ibarrier failed", mpi_code);

        while (!barrier_complete) {
            useconds_t t_delay = 5;
            usleep(t_delay);

            if (MPI_SUCCESS != (mpi_code = MPI_Test(&barrier_req, &barrier_complete, MPI_STATUS_IGNORE)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Test failed", mpi_code);
        }
#else
        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
#endif
    }

    /* The map from file handle to subfiling context can now be cleared */
    if (sf_context->h5_file_handle != NULL) {
        clear_fid_map_entry(sf_context->h5_file_handle, sf_context->sf_context_id);
    }

    if (sf_context->topology->rank_is_ioc) {
        if (sf_context->sf_fid >= 0) {
            errno = 0;
            if (HDclose(sf_context->sf_fid) < 0)
                H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "couldn't close subfile");

            sf_context->sf_fid = -1;
        }
    }

    /*
     * Run another barrier to prevent some ranks from running ahead,
     * and opening another file before this file is completely closed
     * down.
     */
    if (mpi_size > 1) {
#if MPI_VERSION > 3 || (MPI_VERSION == 3 && MPI_SUBVERSION >= 1)
        int barrier_complete = 0;

        if (MPI_SUCCESS != (mpi_code = MPI_Ibarrier(file_comm, &barrier_req)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Ibarrier failed", mpi_code);

        while (!barrier_complete) {
            useconds_t t_delay = 5;
            usleep(t_delay);

            if (MPI_SUCCESS != (mpi_code = MPI_Test(&barrier_req, &barrier_complete, MPI_STATUS_IGNORE)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Test failed", mpi_code);
        }
#else
        if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file_comm)))
            H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
#endif
    }

done:
    if (sf_context && H5_free_subfiling_object_int(sf_context) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTFREE, FAIL, "couldn't free subfiling context object");

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5_subfile_fhandle_to_context
 *
 * Purpose:     This is a basic lookup function which returns the subfiling
 *              context id associated with the specified file handle.
 *
 * Return:      Non-negative subfiling context ID if the context exists
 *              Negative on failure or if the subfiling context doesn't
 *                exist
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
int64_t
H5_subfile_fhandle_to_context(void *file_handle)
{
    int64_t ret_value = -1;

    if (!sf_open_file_map)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, -1, "open file map is NULL");

    for (int i = 0; i < sf_file_map_size; i++) {
        if (sf_open_file_map[i].file_handle == file_handle) {
            return sf_open_file_map[i].sf_context_id;
        }
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5_subfile_fhandle_to_context() */

#ifdef H5_SUBFILING_DEBUG
void
H5_subfiling_log(int64_t sf_context_id, const char *fmt, ...)
{
    subfiling_context_t *sf_context = NULL;
    va_list              log_args;

    va_start(log_args, fmt);

    /* Retrieve the subfiling object for the newly-created context ID */
    if (NULL == (sf_context = H5_get_subfiling_object(sf_context_id))) {
        HDprintf("%s: couldn't get subfiling object from context ID\n", __func__);
        goto done;
    }

    H5FD_ioc_begin_thread_exclusive();

    if (sf_context->sf_logfile) {
        HDvfprintf(sf_context->sf_logfile, fmt, log_args);
        HDfputs("\n", sf_context->sf_logfile);
        HDfflush(sf_context->sf_logfile);
    }
    else {
        HDvprintf(fmt, log_args);
        HDputs("");
        HDfflush(stdout);
    }

    H5FD_ioc_end_thread_exclusive();

done:
    va_end(log_args);

    return;
}
#endif
