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

typedef struct {            /* Format of a context map entry  */
    uint64_t h5_file_id;    /* key value (linear search of the cache) */
    int64_t  sf_context_id; /* The return value if matching h5_file_id */
} file_map_to_context_t;

typedef struct stat_record {
    int64_t op_count; /* How many ops in total */
    double  min;      /* minimum (time)         */
    double  max;      /* maximum (time)        */
    double  total;    /* average (time)        */
} stat_record_t;

/* Stat (OP) Categories  */
typedef enum stat_category {
    WRITE_STAT = 0,
    WRITE_WAIT,
    READ_STAT,
    READ_WAIT,
    FOPEN_STAT,
    FCLOSE_STAT,
    QUEUE_STAT,
    TOTAL_STAT_COUNT
} stat_category_t;

static subfiling_context_t *sf_context_cache  = NULL;
static sf_topology_t *      sf_topology_cache = NULL;

static size_t sf_context_cache_limit  = 16;
static size_t sf_topology_cache_limit = 4;

static app_layout_t *sf_app_layout = NULL;

static file_map_to_context_t *sf_open_file_map = NULL;
static int                    sf_file_map_size = 0;
#define DEFAULT_FILE_MAP_ENTRIES 8

atomic_int sf_file_open_count = 0;
atomic_int sf_shutdown_flag   = 0;

/* Definitions for recording subfiling statistics */
static stat_record_t subfiling_stats[TOTAL_STAT_COUNT];
#define SF_WRITE_OPS       (subfiling_stats[WRITE_STAT].op_count)
#define SF_WRITE_TIME      (subfiling_stats[WRITE_STAT].total / (double)subfiling_stats[WRITE_STAT].op_count)
#define SF_WRITE_WAIT_TIME (subfiling_stats[WRITE_WAIT].total / (double)subfiling_stats[WRITE_WAIT].op_count)
#define SF_READ_OPS        (subfiling_stats[READ_STAT].op_count)
#define SF_READ_TIME       (subfiling_stats[READ_STAT].total / (double)subfiling_stats[READ_STAT].op_count)
#define SF_READ_WAIT_TIME  (subfiling_stats[READ_WAIT].total / (double)subfiling_stats[READ_WAIT].op_count)
#define SF_QUEUE_DELAYS    (subfiling_stats[QUEUE_STAT].total)

int sf_verbose_flag = 0;

#ifdef H5_SUBFILING_DEBUG
char  sf_logile_name[PATH_MAX];
FILE *sf_logfile = NULL;

static int sf_open_file_count = 0;
#endif

static herr_t H5_free_subfiling_object_int(subfiling_context_t *sf_context);
static herr_t H5_free_subfiling_topology(sf_topology_t *topology);

static herr_t init_subfiling(ioc_selection_t ioc_selection_type, MPI_Comm comm, int64_t *context_id_out);
static herr_t init_app_topology(ioc_selection_t ioc_selection_type, MPI_Comm comm,
                                sf_topology_t **app_topology_out);
static herr_t init_subfiling_context(subfiling_context_t *sf_context, sf_topology_t *app_topology,
                                     MPI_Comm file_comm);
static herr_t open_subfile_with_context(subfiling_context_t *sf_context, int file_acc_flags);
static herr_t record_fid_to_subfile(uint64_t h5_file_id, int64_t subfile_context_id, int *next_index);
static herr_t ioc_open_file(sf_work_request_t *msg, int file_acc_flags);
static herr_t generate_subfile_name(subfiling_context_t *sf_context, int file_acc_flags, char *filename_out,
                                    size_t filename_out_len, char **filename_basename_out,
                                    char **subfile_dir_out);
static herr_t create_config_file(subfiling_context_t *sf_context, const char *base_filename,
                                 const char *subfile_dir, hbool_t truncate_if_exists);
static herr_t open_config_file(subfiling_context_t *sf_context, const char *base_filename,
                               const char *subfile_dir, const char *mode, FILE **config_file_out);

static void        initialize_statistics(void);
static int         numDigits(int n);
static int         active_file_map_entries(void);
static void        clear_fid_map_entry(uint64_t sf_fid);
static int         compare_hostid(const void *h1, const void *h2);
static herr_t      get_ioc_selection_criteria_from_env(ioc_selection_t *ioc_selection_type,
                                                       char **          ioc_sel_info_str);
static int         count_nodes(sf_topology_t *info, MPI_Comm comm);
static herr_t      gather_topology_info(sf_topology_t *info, MPI_Comm comm);
static int         identify_ioc_ranks(sf_topology_t *info, int node_count, int iocs_per_node);
static inline void assign_ioc_ranks(sf_topology_t *app_topology, int ioc_count, int rank_multiple);

static void
initialize_statistics(void)
{
    HDmemset(subfiling_stats, 0, sizeof(subfiling_stats));
}

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

/*-------------------------------------------------------------------------
 * Function:    set_verbose_flag
 *
 * Purpose:     For debugging purposes, I allow a verbose setting to
 *              have printing of relevant information into an IOC specific
 *              file that is opened as a result of enabling the flag
 *              and closed when the verbose setting is disabled.
 *
 * Return:      None
 * Errors:      None
 *
 * Programmer:  Richard Warren
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
void
set_verbose_flag(int subfile_rank, int new_value)
{
#ifdef H5_SUBFILING_DEBUG
    sf_verbose_flag = (int)(new_value & 0x0FF);
    if (sf_verbose_flag) {
        char logname[64];
        HDsnprintf(logname, sizeof(logname), "ioc_%d.log", subfile_rank);
        if (sf_open_file_count > 1)
            sf_logfile = fopen(logname, "a+");
        else
            sf_logfile = fopen(logname, "w+");
    }
    else if (sf_logfile) {
        fclose(sf_logfile);
        sf_logfile = NULL;
    }
#else
    (void)subfile_rank;
    (void)new_value;
#endif

    return;
}

/*-------------------------------------------------------------------------
 * Function:    active_file_map_entries
 *
 * Purpose:     Count the number of entries that have valid h5_file_id
 *              values.
 *
 * Return:      The number of active file map entries (can be zero).
 * Errors:      Cannot fail.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static int
active_file_map_entries(void)
{
    int map_entries = 0;
    for (int i = 0; i < sf_file_map_size; i++) {
        if (sf_open_file_map[i].h5_file_id != UINT64_MAX) {
            map_entries++;
        }
    }
    return map_entries;
} /* end active_map_entries() */

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
clear_fid_map_entry(uint64_t sf_fid)
{
    if (sf_open_file_map) {
        int i;
        for (i = 0; i < sf_file_map_size; i++) {
            if (sf_open_file_map[i].h5_file_id == sf_fid) {
                sf_open_file_map[i].h5_file_id    = UINT64_MAX;
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
               variable (H5_IOC_SELECTION_CRITERIA), we return that
               along with any optional qualifier with for that method.

  Errors:      None.

  Revision History -- Initial implementation
-------------------------------------------------------------------------
*/
static herr_t
get_ioc_selection_criteria_from_env(ioc_selection_t *ioc_selection_type, char **ioc_sel_info_str)
{
    char *opt_value = NULL;
    char *env_value = HDgetenv(H5_IOC_SELECTION_CRITERIA);

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

        if (errno == ERANGE) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't parse value from " H5_IOC_SELECTION_CRITERIA " environment variable\n",
                     __func__);
#endif

            return FAIL;
        }

        if ((check_value < 0) || (check_value >= ioc_selection_options)) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: invalid IOC selection type value %ld from " H5_IOC_SELECTION_CRITERIA
                     " environment variable\n",
                     __func__, check_value);
#endif

            return FAIL;
        }

        *ioc_selection_type = (ioc_selection_t)check_value;
        *ioc_sel_info_str   = opt_value;
    }

    return SUCCEED;
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

    HDassert(info);
    HDassert(info->app_layout);
    HDassert(info->app_layout->layout);
    HDassert(info->app_layout->node_ranks);
    HDassert(MPI_COMM_NULL != comm);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &my_rank))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get MPI communicator rank; rc = %d\n", __func__, mpi_code);
#endif

        return -1;
    }

    app_layout = info->app_layout;
    node_count = app_layout->node_count;

    if (node_count == 0)
        gather_topology_info(info, comm);

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

    return node_count;
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
gather_topology_info(sf_topology_t *info, MPI_Comm comm)
{
    app_layout_t *app_layout = NULL;
    layout_t      my_hostinfo;
    long          hostid;
    int           sf_world_size;
    int           sf_world_rank;

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

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allgather(&my_hostinfo, 2, MPI_LONG, app_layout->layout, 2, MPI_LONG, comm))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: MPI_Allgather failed with rc %d\n", __func__, mpi_code);
#endif

            return FAIL;
        }

        qsort(app_layout->layout, (size_t)sf_world_size, sizeof(layout_t), compare_hostid);
    }

    return SUCCEED;
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
    int *         io_concentrators = NULL;

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
            if (io_concentrators[ioc_next] == app_layout->world_rank)
                app_topology->rank_is_ioc = TRUE;
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
 *              Topologies are static, e.g. for any one I/O concentrator
 *              allocation strategy, the results should always be the same.
 *
 *              TODO: The one exception to this being the 1 IOC per N MPI
 *              ranks strategy. The value of N can be changed on a per-file
 *              basis, so we need to address that at some point.
 *
 * Return:      Pointer to underlying subfiling object if subfiling object
 *              ID is valid
 *
 *              NULL if subfiling object ID is invalid or an internal
 *              failure occurs
 *
 *-------------------------------------------------------------------------
 */
/*
 * TODO: we don't appear to ever use this for retrieving a subfile topology
 *       object. Might be able to refactor to just return a subfile context
 *       object.
 */
/* TODO: no way of freeing caches on close currently */
void *
H5_get_subfiling_object(int64_t object_id)
{
    int64_t obj_type  = (object_id >> 32) & 0x0FFFF;
    int64_t obj_index = object_id & 0x0FFFF;

    if (obj_index < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid object index for subfiling object ID %" PRId64 "\n", __func__, object_id);
#endif

        return NULL;
    }

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
            if (NULL == (sf_context_cache = HDcalloc(sf_context_cache_limit, sizeof(subfiling_context_t)))) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: couldn't allocate space for subfiling context cache\n", __func__);
#endif

                return NULL;
            }
        }

        /* Make more space in context cache if needed */
        if ((size_t)obj_index == sf_context_cache_limit) {
            size_t old_num_entries;
            void * tmp_realloc;

            old_num_entries = sf_context_cache_limit;

            sf_context_cache_limit *= 2;

            if (NULL == (tmp_realloc = HDrealloc(sf_context_cache,
                                                 sf_context_cache_limit * sizeof(subfiling_context_t)))) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: couldn't allocate space for subfiling context cache\n", __func__);
#endif

                return NULL;
            }

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
            if (NULL == (sf_topology_cache = HDcalloc(sf_topology_cache_limit, sizeof(sf_topology_t)))) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: couldn't allocate space for subfiling topology cache\n", __func__);
#endif

                return NULL;
            }
        }

        /* We will likely only cache a single topology
         * which is that of the original parallel application.
         * In that context, we will identify the number of
         * nodes along with the number of MPI ranks on a node.
         */
        if ((size_t)obj_index >= sf_topology_cache_limit) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: invalid object index for subfiling topology object ID\n", __func__);
#endif

            return NULL;
        }

        /* Return direct pointer to the topology cache entry */
        return (void *)&sf_topology_cache[obj_index];
    }

#ifdef H5_SUBFILING_DEBUG
    HDprintf("%s: Unknown subfiling object type for ID %" PRId64 "\n", __func__, object_id);
#endif

    return NULL;
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

    if (obj_type != SF_CONTEXT) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid subfiling object type for ID %" PRId64 "\n", __func__, object_id);
#endif

        return FAIL;
    }

    sf_context = H5_get_subfiling_object(object_id);
    if (!sf_context)
        return FAIL;

    if (H5_free_subfiling_object_int(sf_context) < 0)
        return FAIL;

    return SUCCEED;
}

static herr_t
H5_free_subfiling_object_int(subfiling_context_t *sf_context)
{
    HDassert(sf_context);

    sf_context->sf_context_id           = -1;
    sf_context->h5_file_id              = UINT64_MAX;
    sf_context->sf_fid                  = -1;
    sf_context->sf_write_count          = 0;
    sf_context->sf_read_count           = 0;
    sf_context->sf_eof                  = HADDR_UNDEF;
    sf_context->sf_stripe_size          = -1;
    sf_context->sf_blocksize_per_stripe = -1;
    sf_context->sf_base_addr            = -1;

    /*
     * NOTE: assumes context's file communicator is not
     * duplicated, but simply used directly
     */
    sf_context->sf_file_comm = MPI_COMM_NULL;

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
    if (sf_context->sf_group_comm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_group_comm) < 0)
            return FAIL;
        sf_context->sf_group_comm = MPI_COMM_NULL;
    }
    if (sf_context->sf_intercomm != MPI_COMM_NULL) {
        if (H5_mpi_comm_free(&sf_context->sf_intercomm) < 0)
            return FAIL;
        sf_context->sf_intercomm = MPI_COMM_NULL;
    }

    sf_context->sf_group_size     = -1;
    sf_context->sf_group_rank     = -1;
    sf_context->sf_intercomm_root = -1;

    HDfree(sf_context->subfile_prefix);
    sf_context->subfile_prefix = NULL;

    HDfree(sf_context->sf_filename);
    sf_context->sf_filename = NULL;

    HDfree(sf_context->h5_filename);
    sf_context->h5_filename = NULL;

    if (H5_free_subfiling_topology(sf_context->topology) < 0)
        return FAIL;
    sf_context->topology = NULL;

#ifdef H5_SUBFILING_DEBUG
    if (sf_context->sf_logfile) {
        HDfclose(sf_context->sf_logfile);
        sf_context->sf_logfile = NULL;
    }
#endif

    return SUCCEED;
}

static herr_t
H5_free_subfiling_topology(sf_topology_t *topology)
{
    HDassert(topology);

    HDfree(topology->subfile_fd);

    /*
     * NOTE: This assumes that a single allocation is used
     * to allocate space for the app layout structure and
     * its contents. This will need to be revised if that
     * changes.
     */
    HDfree(topology->app_layout);
    topology->app_layout = NULL;

    /* TODO: */
    sf_app_layout = NULL;

    HDfree(topology->io_concentrators);
    HDfree(topology);

    return SUCCEED;
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
H5_open_subfiles(const char *base_filename, uint64_t h5_file_id, ioc_selection_t ioc_selection_type,
                 int file_acc_flags, MPI_Comm file_comm, int64_t *context_id_out)
{
    subfiling_context_t *sf_context = NULL;
    int64_t              context_id = -1;
    herr_t               ret_value  = SUCCEED;

    if (!base_filename) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid base filename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (!context_id_out) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: context_id_out is NULL\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    initialize_statistics();

#if 0 /* TODO */
    /* Maybe set the verbose flag for more debugging info */
    envValue = HDgetenv("H5_SF_VERBOSE_FLAG");
    if (envValue != NULL) {
        int check_value = atoi(envValue);
        if (check_value > 0)
            sf_verbose_flag = 1;
    }
#endif

    /* Initialize new subfiling context ID based on configuration information */
    if (init_subfiling(ioc_selection_type, file_comm, &context_id) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't initialize subfiling context\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Retrieve the subfiling object for the newly-created context ID */
    if (NULL == (sf_context = H5_get_subfiling_object(context_id))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get subfiling object from context ID\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Save some basic things in the new subfiling context */
    sf_context->h5_file_id = h5_file_id;

    if (NULL == (sf_context->h5_filename = HDstrdup(base_filename))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't copy base HDF5 filename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /*
     * Ensure that the IOC service won't exit
     * as we prepare to start up
     */
#if 0 /* JRM */ /* delete if all goes well */
    H5FD_ioc_set_shutdown_flag(0);
#else
    atomic_init(&sf_shutdown_flag, 0);
#endif

    /*
     * If we're actually using the IOCs, we will
     * start the service threads on the identified
     * ranks as part of the subfile opening.
     */
    if (open_subfile_with_context(sf_context, file_acc_flags) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't open subfiles\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

#ifdef H5_SUBFILING_DEBUG
    {
        int mpi_rank;

        /* Open debugging logfile */

        if (MPI_SUCCESS != MPI_Comm_rank(sf_context->sf_file_comm, &mpi_rank)) {
            HDprintf("%s: couldn't get MPI rank\n", __func__);
            ret_value = FAIL;
            goto done;
        }

        HDsnprintf(sf_context->sf_logfile_name, PATH_MAX, "%s.log.%d", sf_context->h5_filename, mpi_rank);

        if (NULL == (sf_context->sf_logfile = HDfopen(sf_context->sf_logfile_name, "w"))) {
            HDprintf("%s: couldn't open subfiling debug logfile\n", __func__);
            ret_value = FAIL;
            goto done;
        }
    }
#endif

    *context_id_out = context_id;

done:
    if (ret_value < 0) {
        if (context_id >= 0 && H5_free_subfiling_object(context_id) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't free subfiling object\n", __func__);
#endif
        }
    }

    return ret_value;
}

/* TODO: revise */
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
init_subfiling(ioc_selection_t ioc_selection_type, MPI_Comm comm, int64_t *context_id_out)
{
    subfiling_context_t *new_context  = NULL;
    sf_topology_t *      app_topology = NULL;
    int64_t              context_id   = -1;
    int                  file_index   = -1;
    herr_t               ret_value    = SUCCEED;

    HDassert(context_id_out);

    file_index = active_file_map_entries();
    HDassert(file_index >= 0);

    /* Use the file's index to create a new subfiling context ID */
    if ((context_id = H5_new_subfiling_object_id(SF_CONTEXT, file_index)) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create new subfiling context ID\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Create a new subfiling context object with the created context ID */
    if (NULL == (new_context = H5_get_subfiling_object(context_id))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create new subfiling object\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /*
     * Setup the application topology information, including the computed
     * number and distribution map of the set of I/O concentrators
     */
    if (init_app_topology(ioc_selection_type, comm, &app_topology) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't initialize application topology\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    new_context->sf_context_id = context_id;

    if (init_subfiling_context(new_context, app_topology, comm) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't initialize subfiling topology object\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    new_context->sf_base_addr = 0;
    if (new_context->topology->rank_is_ioc) {
        new_context->sf_base_addr =
            (int64_t)(new_context->topology->subfile_rank * new_context->sf_stripe_size);
    }

    *context_id_out = context_id;

done:
    if (ret_value < 0) {
        HDfree(app_topology);

        if (context_id >= 0 && H5_free_subfiling_object(context_id) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't free subfiling object\n", __func__);
#endif
        }
    }

    return ret_value;
}

/* TODO: revise */
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
init_app_topology(ioc_selection_t ioc_selection_type, MPI_Comm comm, sf_topology_t **app_topology_out)
{
    sf_topology_t *app_topology     = NULL;
    app_layout_t * app_layout       = sf_app_layout;
    char *         env_value        = NULL;
    char *         ioc_sel_str      = NULL;
    int *          io_concentrators = NULL;
    long           ioc_select_val   = -1;
    long           iocs_per_node    = 1;
    int            ioc_count        = 0;
    int            comm_rank;
    int            comm_size;
    int            mpi_code;
    herr_t         ret_value = SUCCEED;

    HDassert(MPI_COMM_NULL != comm);
    HDassert(app_topology_out);
    HDassert(!*app_topology_out);

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &comm_rank))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get MPI communicator rank; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &comm_size))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get MPI communicator size; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Check if an IOC selection type was specified by environment variable */
    if (get_ioc_selection_criteria_from_env(&ioc_selection_type, &ioc_sel_str) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get IOC selection type from environment\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

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

    /* Allocate new application topology information object */
    if (NULL == (app_topology = HDcalloc(1, sizeof(*app_topology)))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create new subfiling topology object\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    app_topology->selection_type = ioc_selection_type;

    if (NULL == (app_topology->io_concentrators = HDcalloc((size_t)comm_size, sizeof(int)))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate array of I/O concentrator ranks\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    io_concentrators = app_topology->io_concentrators;
    HDassert(io_concentrators);

    if (!app_layout) {
        size_t node_rank_size = ((size_t)comm_size + 1) * sizeof(int);
        size_t layout_size    = ((size_t)comm_size + 1) * sizeof(layout_t);
        size_t alloc_size     = sizeof(app_layout_t) + node_rank_size + layout_size;

        /* TODO: this is dangerous if a new comm size is greater than what
         * was allocated. Can't reuse app layout.
         */

        /*
         * Use single allocation to encompass the app layout
         * structure and all of its elements
         */
        if (NULL == (app_layout = HDcalloc(1, alloc_size))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't allocate application layout structure\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        app_layout->node_ranks = (int *)&app_layout[1];
        app_layout->layout     = (layout_t *)&app_layout->node_ranks[comm_size + 1];
    }

    app_layout->world_size = comm_size;
    app_layout->world_rank = comm_rank;

    /*
     * Once the application layout has been filled once, any additional
     * file open operations won't be required to gather that information.
     */
    app_topology->app_layout = app_layout;

    /* TODO */
    sf_app_layout = app_layout;

    /*
     * Determine which ranks are I/O concentrator ranks, based on the
     * given IOC selection strategy and MPI information.
     */
    switch (ioc_selection_type) {
        case SELECT_IOC_ONE_PER_NODE: {
            int node_count;

            app_topology->selection_type = SELECT_IOC_ONE_PER_NODE;

            if ((node_count = count_nodes(app_topology, comm)) < 0) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: couldn't determine number of nodes used\n", __func__);
#endif

                ret_value = FAIL;
                goto done;
            }

            /* Check for an IOC-per-node value set in the environment */
            /* TODO: should this env. var. be interpreted for other selection types? */
            if ((env_value = HDgetenv(H5_IOC_COUNT_PER_NODE))) {
                errno          = 0;
                ioc_select_val = HDstrtol(env_value, NULL, 0);
                if ((ERANGE == errno)) {
                    HDprintf("invalid value '%s' for " H5_IOC_COUNT_PER_NODE "\n", env_value);
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
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: invalid IOC selection strategy\n", __func__);
#endif
            ret_value = FAIL;
            goto done;
            break;
    }

    HDassert(ioc_count > 0);
    app_topology->n_io_concentrators = ioc_count;

    /*
     * Create a vector of "potential" file descriptors
     * which can be indexed by the IOC ID
     */
    if (NULL == (app_topology->subfile_fd = HDcalloc((size_t)ioc_count, sizeof(int)))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate subfile file descriptor array\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    *app_topology_out = app_topology;

done:
    if (ret_value < 0) {
        HDfree(app_layout);
        if (app_topology) {
            HDfree(app_topology->subfile_fd);
            HDfree(app_topology->io_concentrators);
            HDfree(app_topology);
        }
    }

    return ret_value;
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
/* TODO: revise description */
static herr_t
init_subfiling_context(subfiling_context_t *sf_context, sf_topology_t *app_topology, MPI_Comm file_comm)
{
    char * env_value = NULL;
    int    comm_rank;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    HDassert(sf_context);
    HDassert(sf_context->topology == NULL);
    HDassert(app_topology);
    HDassert(app_topology->n_io_concentrators > 0);
    HDassert(MPI_COMM_NULL != file_comm);

    sf_context->topology       = app_topology;
    sf_context->sf_file_comm   = file_comm;
    sf_context->sf_msg_comm    = MPI_COMM_NULL;
    sf_context->sf_data_comm   = MPI_COMM_NULL;
    sf_context->sf_eof_comm    = MPI_COMM_NULL;
    sf_context->sf_group_comm  = MPI_COMM_NULL;
    sf_context->sf_intercomm   = MPI_COMM_NULL;
    sf_context->sf_stripe_size = H5FD_DEFAULT_STRIPE_DEPTH;
    sf_context->sf_write_count = 0;
    sf_context->sf_read_count  = 0;
    sf_context->sf_eof         = HADDR_UNDEF;
    sf_context->sf_fid         = -1;
    sf_context->sf_group_size  = 1;
    sf_context->sf_group_rank  = 0;
    sf_context->h5_filename    = NULL;
    sf_context->sf_filename    = NULL;
    sf_context->subfile_prefix = NULL;

#ifdef H5_SUBFILING_DEBUG
    sf_context->sf_logfile = NULL;
#endif

    /* Check for an IOC stripe size setting in the environment */
    if ((env_value = HDgetenv(H5_IOC_STRIPE_SIZE))) {
        long long stripe_size = -1;

        errno = 0;

        stripe_size = HDstrtoll(env_value, NULL, 0);
        if (ERANGE == errno) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: invalid stripe size setting '%s' for " H5_IOC_STRIPE_SIZE "\n", __func__,
                     env_value);
#endif

            ret_value = FAIL;
            goto done;
        }

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
    if ((env_value = HDgetenv(H5_IOC_SUBFILE_PREFIX))) {
        if (NULL == (sf_context->subfile_prefix = HDstrdup(env_value))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't copy subfile prefix value\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }
    }

    /*
     * Set up various MPI sub-communicators for MPI operations
     * to/from IOC ranks
     */

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_comm, &comm_rank))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get MPI communicator rank; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_msg_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create sub-communicator for IOC messages; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_msg_comm, MPI_ERRORS_RETURN))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't set MPI error handler on IOC message sub-communicator; rc = %d\n", __func__,
                 mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_data_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create sub-communicator for IOC data; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_data_comm, MPI_ERRORS_RETURN))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't set MPI error handler on IOC data sub-communicator; rc = %d\n", __func__,
                 mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_dup(file_comm, &sf_context->sf_eof_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't create sub-communicator for IOC EOF; rc = %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (MPI_SUCCESS != (mpi_code = MPI_Comm_set_errhandler(sf_context->sf_eof_comm, MPI_ERRORS_RETURN))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't set MPI error handler on IOC EOF sub-communicator; rc = %d\n", __func__,
                 mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Create an MPI sub-communicator for IOC ranks */
    if (app_topology->n_io_concentrators > 1) {
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_split(file_comm, app_topology->rank_is_ioc, comm_rank,
                                                      &sf_context->sf_group_comm))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't create sub-communicator for IOC ranks; rc = %d\n", __func__, mpi_code);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Comm_rank(sf_context->sf_group_comm, &sf_context->sf_group_rank))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't get MPI rank from IOC rank sub-communicator; rc = %d\n", __func__,
                     mpi_code);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (MPI_SUCCESS !=
            (mpi_code = MPI_Comm_size(sf_context->sf_group_comm, &sf_context->sf_group_size))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't get MPI comm size from IOC rank sub-communicator; rc = %d\n", __func__,
                     mpi_code);
#endif

            ret_value = FAIL;
            goto done;
        }

        /*
         * TODO: There may be additional functionality we need for the IOCs...
         *       If so, then can probably initialize those things here!
         */
    }

done:
    if (ret_value < 0) {
        H5_free_subfiling_object_int(sf_context);
    }

    return ret_value;
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
/* TODO: revise description */
static herr_t
open_subfile_with_context(subfiling_context_t *sf_context, int file_acc_flags)
{
    double start_time;
    int    l_errors = 0;
    int    g_errors = 0;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    HDassert(sf_context);

    start_time = MPI_Wtime();

    /*
     * Save the HDF5 file ID (fid) to subfile context mapping.
     * There shouldn't be any issue, but check the status and
     * return if there was a problem.
     */
    if (record_fid_to_subfile(sf_context->h5_file_id, sf_context->sf_context_id, NULL) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't record HDF5 file ID to subfile context mapping\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /*
     * If this rank is an I/O concentrator, actually open
     * the subfile belonging to this IOC rank
     */
    if (sf_context->topology->rank_is_ioc) {
        sf_work_request_t msg = {{file_acc_flags, (int64_t)sf_context->h5_file_id, sf_context->sf_context_id},
                                 OPEN_OP,
                                 sf_context->topology->app_layout->world_rank,
                                 sf_context->topology->subfile_rank,
                                 sf_context->sf_context_id,
                                 start_time,
                                 NULL,
                                 0,
                                 0,
                                 0,
                                 0};

        if (ioc_open_file(&msg, file_acc_flags) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("[%s %d]: couldn't open subfile\n", __func__,
                     sf_context->topology->app_layout->world_rank);
#endif

            ret_value = FAIL;
            goto done;
        }
    }

done:
    if (ret_value < 0) {
        l_errors = 1;
    }

    /*
     * Form consensus on whether opening subfiles was
     * successful across the IOC ranks
     */
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Allreduce(&l_errors, &g_errors, 1, MPI_INT, MPI_SUM, sf_context->sf_file_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("[%s %d]: MPI_Allreduce failed with rc %d\n", __func__,
                 sf_context->topology->app_layout->world_rank, mpi_code);
#endif

        ret_value = FAIL;
    }

    if (g_errors > 0) {
#ifdef H5_SUBFILING_DEBUG
        if (sf_context->topology->app_layout->world_rank == 0) {
            HDprintf("%s: one or more IOC ranks couldn't open subfiles\n", __func__);
        }
#endif

        ret_value = FAIL;
    }

    if (ret_value < 0) {
        clear_fid_map_entry(sf_context->h5_file_id);
    }

    return ret_value;
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
record_fid_to_subfile(uint64_t h5_file_id, int64_t subfile_context_id, int *next_index)
{
    int    index;
    herr_t ret_value = SUCCEED;

    if (sf_file_map_size == 0) {
        if (NULL ==
            (sf_open_file_map = HDmalloc((size_t)DEFAULT_FILE_MAP_ENTRIES * sizeof(*sf_open_file_map)))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't allocate open file map\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        sf_file_map_size = DEFAULT_FILE_MAP_ENTRIES;
        for (int i = 0; i < sf_file_map_size; i++) {
            sf_open_file_map[i].h5_file_id    = UINT64_MAX;
            sf_open_file_map[i].sf_context_id = -1;
        }
    }

    for (index = 0; index < sf_file_map_size; index++) {
        if (sf_open_file_map[index].h5_file_id == UINT64_MAX) {
            sf_open_file_map[index].h5_file_id    = h5_file_id;
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
                                             ((size_t)(sf_file_map_size * 2) * sizeof(*sf_open_file_map))))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't reallocate open file map\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        sf_open_file_map = tmp_realloc;
        sf_file_map_size *= 2;

        for (int i = index; i < sf_file_map_size; i++) {
            sf_open_file_map[i].h5_file_id = UINT64_MAX;
        }

        if (next_index) {
            *next_index = index;
        }

        sf_open_file_map[index].h5_file_id      = h5_file_id;
        sf_open_file_map[index++].sf_context_id = subfile_context_id;
    }

done:
    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    ioc_open_file
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
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
/* TODO: revise description */
static herr_t
ioc_open_file(sf_work_request_t *msg, int file_acc_flags)
{
    subfiling_context_t *sf_context = NULL;
    int64_t              file_context_id;
#ifdef H5_SUBFILING_DEBUG
    double t_start = 0.0;
    double t_end   = 0.0;
#endif
    mode_t mode        = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
    char * filepath    = NULL;
    char * subfile_dir = NULL;
    char * base        = NULL;
    int    fd          = -1;
    herr_t ret_value   = SUCCEED;

    HDassert(msg);

#ifdef H5_SUBFILING_DEBUG
    t_start = MPI_Wtime();
#endif

    /* Retrieve subfiling context ID from RPC message */
    file_context_id = msg->header[2];

    if (NULL == (sf_context = H5_get_subfiling_object(file_context_id))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get subfiling object from context ID\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Only IOC ranks should be here */
    HDassert(sf_context->topology);
    HDassert(sf_context->topology->subfile_rank >= 0);

    if (NULL == (filepath = HDcalloc(1, PATH_MAX))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate space for subfile filename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Generate the name of the subfile that this IOC rank will open */
    if (generate_subfile_name(sf_context, file_acc_flags, filepath, PATH_MAX, &base, &subfile_dir) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't generate name for subfile\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (NULL == (sf_context->sf_filename = HDstrdup(filepath))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't copy subfile name\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    begin_thread_exclusive();

    /* Attempt to create/open the subfile for this IOC rank */
    if ((fd = HDopen(filepath, file_acc_flags, mode)) < 0) {
        end_thread_exclusive(); /* TODO: try to only unlock in one place? */

        HDperror("ioc_open_file - file open failed");

#ifdef H5_SUBFILING_DEBUG
        if (sf_verbose_flag) {
            HDprintf("[%d %s] file open(%s) failed!\n", sf_context->topology->subfile_rank, __func__,
                     filepath);
        }
#endif

        ret_value = FAIL;
        goto done;
    }

    sf_context->sf_fid = fd;
    if (file_acc_flags & O_CREAT)
        sf_context->sf_eof = 0;

#ifdef H5_SUBFILING_DEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            HDfprintf(sf_logfile, "[ioc:%d] Opened subfile %s\n", sf_context->topology->subfile_rank,
                      filepath);
        }
    }
#endif

    /*
     * If subfiles were created (rather than simply opened),
     * check if we also need to create a config file.
     */
    if ((file_acc_flags & O_CREAT) && (sf_context->topology->subfile_rank == 0)) {
        if (create_config_file(sf_context, base, subfile_dir, (file_acc_flags & O_TRUNC)) < 0) {
            end_thread_exclusive(); /* TODO: try to only unlock in one place? */

#ifdef H5_SUBFILING_DEBUG
            HDprintf("[%d %s]: couldn't create subfiling configuration file\n",
                     sf_context->topology->subfile_rank, __func__);
#endif

            ret_value = FAIL;
            goto done;
        }
    }

    end_thread_exclusive(); /* TODO: try to only unlock in one place? */

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

    HDfree(base);
    HDfree(subfile_dir);
    HDfree(filepath);

#ifdef H5_SUBFILING_DEBUG
    t_end = MPI_Wtime();
    if (sf_verbose_flag) {
        HDprintf("[%s %d] open completed in %lf seconds\n", __func__, sf_context->topology->subfile_rank,
                 (t_end - t_start));
    }
#endif

    return ret_value;
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
 */
static herr_t
generate_subfile_name(subfiling_context_t *sf_context, int file_acc_flags, char *filename_out,
                      size_t filename_out_len, char **filename_basename_out, char **subfile_dir_out)
{
    FILE * config_file = NULL;
    char * config_buf  = NULL;
    char * subfile_dir = NULL;
    char * prefix      = NULL;
    char * base        = NULL;
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

    if (NULL == (prefix = HDmalloc(PATH_MAX))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate space for subfile prefix\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /* Under normal operation, we co-locate subfiles with the HDF5 file */
    HDstrncpy(prefix, sf_context->h5_filename, PATH_MAX);

    base = basename(prefix);

    if (NULL == (*filename_basename_out = HDstrdup(base))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate space for subfile basename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    if (sf_context->subfile_prefix) {
        /* Note: Users may specify a directory name which is inaccessible
         * from where the current is running.  In particular, "node-local"
         * storage is not uniformly available to all processes.
         * We would like to check if the user pathname unavailable and
         * if so, we could default to creating the subfiles in the
         * current directory. (?)
         */
        if (NULL == (*subfile_dir_out = HDstrdup(sf_context->subfile_prefix))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't copy subfile prefix\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        subfile_dir = *subfile_dir_out;
    }
    else {
        subfile_dir = dirname(prefix);

        if (NULL == (*subfile_dir_out = HDstrdup(subfile_dir))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't copy subfile prefix\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }
    }

    /*
     * Open the file's subfiling configuration file, if it exists and
     * we aren't truncating the file.
     */
    if (0 == (file_acc_flags & O_TRUNC)) {
        if (open_config_file(sf_context, base, subfile_dir, "r", &config_file) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't open existing subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }
    }

    /*
     * If a subfiling configuration file exists and we aren't truncating
     * it, read the number of I/O concentrators used at file creation time
     * in order to generate the correct subfile names.
     */
    if (config_file) {
        char *ioc_substr      = NULL;
        long  config_file_len = 0;

        if (HDfseek(config_file, 0, SEEK_END) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't seek to end of subfiling configuration file; errno = %d\n", __func__,
                     errno);
#endif

            ret_value = FAIL;
            goto done;
        }

        if ((config_file_len = HDftell(config_file)) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't get size of subfiling configuration file; errno = %d\n", __func__, errno);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (HDfseek(config_file, 0, SEEK_SET) < 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't seek to end of subfiling configuration file; errno = %d\n", __func__,
                     errno);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (NULL == (config_buf = HDmalloc((size_t)config_file_len + 1))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't allocate space for reading subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (HDfread(config_buf, (size_t)config_file_len, 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't read from subfiling configuration file; errno = %d\n", __func__, errno);
#endif

            ret_value = FAIL;
            goto done;
        }

        config_buf[config_file_len] = '\0';

        if (NULL == (ioc_substr = HDstrstr(config_buf, "aggregator_count"))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: malformed subfiling configuration file - no aggregator_count entry\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (EOF == HDsscanf(ioc_substr, "aggregator_count=%d", &n_io_concentrators)) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't get number of I/O concentrators from subfiling configuration file\n",
                     __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        if (n_io_concentrators <= 0) {
            HDprintf("%s: invalid number of I/O concentrators (%d) read from subfiling configuration file\n",
                     __func__, n_io_concentrators);
            ret_value = FAIL;
            goto done;
        }
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
    num_digits = numDigits(n_io_concentrators);
    HDsnprintf(filename_out, filename_out_len, "%s/%s" SF_FILENAME_TEMPLATE, subfile_dir, base,
               sf_context->h5_file_id, num_digits, sf_context->topology->subfile_rank + 1,
               n_io_concentrators);

done:
    if (config_file && (EOF == HDfclose(config_file))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: fclose failed to close subfiling configuration file\n", __func__);
#endif

        ret_value = FAIL;
    }

    if (ret_value < 0) {
        if (*filename_basename_out) {
            HDfree(*filename_basename_out);
            *filename_basename_out = NULL;
        }
        if (*subfile_dir_out) {
            HDfree(*subfile_dir_out);
            *subfile_dir_out = NULL;
        }
    }

    HDfree(config_buf);
    HDfree(prefix);

    return ret_value;
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
    FILE *  config_file        = NULL;
    char *  config_filename    = NULL;
    char *  line_buf           = NULL;
    int     ret                = 0;
    herr_t  ret_value          = SUCCEED;

    HDassert(sf_context);
    HDassert(base_filename);
    HDassert(subfile_dir);

    if (sf_context->h5_file_id == UINT64_MAX) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid HDF5 file ID %" PRIu64 "\n", __func__, sf_context->h5_file_id);
#endif

        ret_value = FAIL;
        goto done;
    }
    if (*base_filename == '\0') {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid base HDF5 filename %s\n", __func__, base_filename);
#endif

        ret_value = FAIL;
        goto done;
    }
    if (*subfile_dir == '\0')
        subfile_dir = ".";

    if (NULL == (config_filename = HDmalloc(PATH_MAX))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate space for subfiling configuration file filename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    HDsnprintf(config_filename, PATH_MAX, "%s/%s" SF_CONFIG_FILENAME_TEMPLATE, subfile_dir, base_filename,
               sf_context->h5_file_id);

    /* Determine whether a subfiling configuration file exists */
    errno = 0;
    ret   = HDaccess(config_filename, F_OK);

    config_file_exists = (ret == 0) || ((ret < 0) && (ENOENT != errno));

    if (config_file_exists && (ret != 0)) {
#ifdef H5_SUBFILING_DEBUG
        HDperror("couldn't check existence of configuration file");
#endif

        ret_value = FAIL;
        goto done;
    }

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

        if (NULL == (config_file = HDfopen(config_filename, "w+"))) {
#ifdef H5_SUBFILING_DEBUG
            HDperror("couldn't open subfiling configuration file");
#endif

            ret_value = FAIL;
            goto done;
        }

        if (NULL == (line_buf = HDmalloc(PATH_MAX))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: couldn't allocate buffer for writing to subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        /* Write the subfiling stripe size to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "stripe_size=%" PRId64 "\n", sf_context->sf_stripe_size);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fwrite failed to write to subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        /* Write the number of I/O concentrators to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "aggregator_count=%d\n", n_io_concentrators);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fwrite failed to write to subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        /* Write the base HDF5 filename to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "hdf5_file=%s\n", sf_context->h5_filename);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fwrite failed to write to subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        /* Write the optional subfile directory prefix to the configuration file */
        HDsnprintf(line_buf, PATH_MAX, "subfile_dir=%s\n", subfile_dir);
        if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fwrite failed to write to subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
            goto done;
        }

        /* Write out each subfile name to the configuration file */
        num_digits = numDigits(n_io_concentrators);
        for (int k = 0; k < n_io_concentrators; k++) {
            HDsnprintf(line_buf, PATH_MAX, "%s" SF_FILENAME_TEMPLATE "\n", base_filename,
                       sf_context->h5_file_id, num_digits, k + 1, n_io_concentrators);

            if (HDfwrite(line_buf, HDstrlen(line_buf), 1, config_file) != 1) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: fwrite failed to write to subfiling configuration file\n", __func__);
#endif

                ret_value = FAIL;
                goto done;
            }
        }
    }

done:
    if (config_file) {
        if (EOF == HDfclose(config_file)) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fclose failed to close subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
        }
    }

    HDfree(line_buf);
    HDfree(config_filename);

    return ret_value;
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
    FILE *  config_file        = NULL;
    char *  config_filename    = NULL;
    int     ret                = 0;
    herr_t  ret_value          = SUCCEED;

    HDassert(sf_context);
    HDassert(base_filename);
    HDassert(subfile_dir);
    HDassert(mode);
    HDassert(config_file_out);

    *config_file_out = NULL;

    if (sf_context->h5_file_id == UINT64_MAX) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid HDF5 file ID %" PRIu64 "\n", __func__, sf_context->h5_file_id);
#endif

        ret_value = FAIL;
        goto done;
    }
    if (*base_filename == '\0') {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: invalid base HDF5 filename %s\n", __func__, base_filename);
#endif

        ret_value = FAIL;
        goto done;
    }
    if (*subfile_dir == '\0')
        subfile_dir = ".";

    if (NULL == (config_filename = HDmalloc(PATH_MAX))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't allocate space for subfiling configuration file filename\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    HDsnprintf(config_filename, PATH_MAX, "%s/%s" SF_CONFIG_FILENAME_TEMPLATE, subfile_dir, base_filename,
               sf_context->h5_file_id);

    /* Determine whether a subfiling configuration file exists */
    errno = 0;
    ret   = HDaccess(config_filename, F_OK);

    config_file_exists = (ret == 0) || ((ret < 0) && (ENOENT != errno));

    if (!config_file_exists)
        goto done;

    if (config_file_exists && (ret != 0)) {
#ifdef H5_SUBFILING_DEBUG
        HDperror("couldn't check existence of configuration file");
#endif

        ret_value = FAIL;
        goto done;
    }

    if (NULL == (config_file = HDfopen(config_filename, mode))) {
#ifdef H5_SUBFILING_DEBUG
        HDperror("couldn't open subfiling configuration file");
#endif

        ret_value = FAIL;
        goto done;
    }

    *config_file_out = config_file;

done:
    if (ret_value < 0) {
        if (config_file && (EOF == HDfclose(config_file))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: fclose failed to close subfiling configuration file\n", __func__);
#endif

            ret_value = FAIL;
        }
    }

    HDfree(config_filename);

    return ret_value;
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
/* TODO: revise description */
herr_t
H5_close_subfiles(int64_t subfiling_context_id)
{
    subfiling_context_t *sf_context  = NULL;
    MPI_Request          barrier_req = MPI_REQUEST_NULL;
    MPI_Comm             file_comm   = MPI_COMM_NULL;
#ifdef H5_SUBFILING_DEBUG
    double t_finalize_threads = 0.0;
    double t_main_exit        = 0.0;
    double t0                 = 0.0;
    double t1                 = 0.0;
    double t2                 = 0.0;
#endif
    int    errors        = 0;
    int    global_errors = 0;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

#ifdef H5_SUBFILING_DEBUG
    t0 = MPI_Wtime();
#endif

    if (NULL == (sf_context = H5_get_subfiling_object(subfiling_context_id))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't get subfiling object from context ID\n", __func__);
#endif

        ret_value = FAIL;
        goto done;
    }

    /*
     * Store a reference to the subfiling context's file communicator
     * for later use, as we will still need it after freeing the
     * underlying subfiling context. Note that this assumes the
     * subfiling context is holding a direct reference to the file's
     * communicator, rather than a duplicated one. This logic will
     * need to be refactored if that changes.
     */
    file_comm = sf_context->sf_file_comm;

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
#if MPI_VERSION > 3 || (MPI_VERSION == 3 && MPI_SUBVERSION >= 1)
    {
        int barrier_complete = 0;

        if (MPI_SUCCESS != (mpi_code = MPI_Ibarrier(sf_context->sf_file_comm, &barrier_req))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: MPI_Ibarrier failed with rc %d\n", __func__, mpi_code);
#endif

            ret_value = FAIL;
            goto done;
        }

        while (!barrier_complete) {
            useconds_t t_delay = 5;
            usleep(t_delay);

            if (MPI_SUCCESS != (mpi_code = MPI_Test(&barrier_req, &barrier_complete, MPI_STATUS_IGNORE))) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: MPI_Test failed with rc %d\n", __func__, mpi_code);
#endif

                ret_value = FAIL;
                goto done;
            }
        }
    }
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Barrier(sf_context->sf_file_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: MPI_Barrier failed with rc %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }
#endif

    /* The map from FID to subfiling context can now be cleared */
    if (sf_context->h5_file_id != UINT64_MAX) {
        clear_fid_map_entry(sf_context->h5_file_id);
    }

    if (sf_context->topology->rank_is_ioc) {
        int file_open_count;

        file_open_count = atomic_load(&sf_file_open_count);
        atomic_fetch_sub(&sf_file_open_count, 1);

        /* If there's only a single file that is
         * currently open, we can shutdown the IO concentrator
         * as part of the file close.
         */
#if 0 /* JRM */ /* delete this if all goes well */
        if (file_open_count == 1) {
            /* Shutdown the main IOC thread */
            H5FD_ioc_set_shutdown_flag(1);
            /* Allow ioc_main to exit.*/
            usleep(20);

            t1 = MPI_Wtime();
            H5FD_ioc_wait_thread_main();
            t2          = MPI_Wtime();
            t1          = t2;
            t_main_exit = t2 - t1;
            H5FD_ioc_finalize_threads();

            t2 = MPI_Wtime();
        }
#else
        if (file_open_count == 1) {

            HDassert(0 == atomic_load(&sf_shutdown_flag));

            /* Shutdown the main IOC thread */
            atomic_store(&sf_shutdown_flag, 1);

            /* Allow ioc_main to exit.*/
            do {

                usleep(20);

            } while (0 != atomic_load(&sf_shutdown_flag));

#ifdef H5_SUBFILING_DEBUG
            t1 = MPI_Wtime();
#endif

            H5FD_ioc_wait_thread_main();

#ifdef H5_SUBFILING_DEBUG
            t2          = MPI_Wtime();
            t1          = t2;
            t_main_exit = t2 - t1;
#endif

            H5FD_ioc_take_down_thread_pool();

#ifdef H5_SUBFILING_DEBUG
            t2 = MPI_Wtime();
#endif
        }
#endif

#ifdef H5_SUBFILING_DEBUG
        t_finalize_threads = t2 - t1;
#endif

        if (sf_context->sf_fid >= 0) {
            errno = 0;
            if (HDclose(sf_context->sf_fid) < 0) {
                HDperror("H5_close_subfiles - couldn't close subfile");

#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: couldn't close subfile\n", __func__);
#endif

                ret_value = FAIL;
                goto done;
            }

            sf_context->sf_fid = -1;
        }

#ifdef H5_SUBFILING_DEBUG
        /* FIXME: If we've had multiple files open, our statistics
         * will be messed up!
         */
        if (sf_verbose_flag) {
            t1 = t2;
            if (sf_logfile != NULL) {
                HDfprintf(sf_logfile, "[%d] main_exit=%lf, finalize_threads=%lf\n", sf_context->sf_group_rank,
                          t_main_exit, t_finalize_threads);
                if (SF_WRITE_OPS > 0)
                    HDfprintf(
                        sf_logfile,
                        "[%d] pwrite perf: wrt_ops=%ld wait=%lf pwrite=%lf IOC_shutdown = %lf seconds\n",
                        sf_context->sf_group_rank, SF_WRITE_OPS, SF_WRITE_WAIT_TIME, SF_WRITE_TIME,
                        (t1 - t0));
                if (SF_READ_OPS > 0)
                    HDfprintf(sf_logfile,
                              "[%d] pread perf: read_ops=%ld wait=%lf pread=%lf IOC_shutdown = %lf seconds\n",
                              sf_context->sf_group_rank, SF_READ_OPS, SF_READ_WAIT_TIME, SF_READ_TIME,
                              (t1 - t0));

                HDfprintf(sf_logfile, "[%d] Avg queue time=%lf seconds\n", sf_context->sf_group_rank,
                          SF_QUEUE_DELAYS / (double)(SF_WRITE_OPS + SF_READ_OPS));

                HDfflush(sf_logfile);

                HDfclose(sf_logfile);
                sf_logfile = NULL;
            }
        }
#endif
    }

    /*
     * Run another barrier to prevent some ranks from running ahead,
     * and opening another file before this file is completely closed
     * down.
     */
#if MPI_VERSION > 3 || (MPI_VERSION == 3 && MPI_SUBVERSION >= 1)
    {
        int barrier_complete = 0;

        if (MPI_SUCCESS != (mpi_code = MPI_Ibarrier(sf_context->sf_file_comm, &barrier_req))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: MPI_Ibarrier failed with rc %d\n", __func__, mpi_code);
#endif

            ret_value = FAIL;
            goto done;
        }

        while (!barrier_complete) {
            useconds_t t_delay = 5;
            usleep(t_delay);

            if (MPI_SUCCESS != (mpi_code = MPI_Test(&barrier_req, &barrier_complete, MPI_STATUS_IGNORE))) {
#ifdef H5_SUBFILING_DEBUG
                HDprintf("%s: MPI_Test failed with rc %d\n", __func__, mpi_code);
#endif

                ret_value = FAIL;
                goto done;
            }
        }
    }
#else
    if (MPI_SUCCESS != (mpi_code = MPI_Barrier(sf_context->sf_file_comm))) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: MPI_Barrier failed with rc %d\n", __func__, mpi_code);
#endif

        ret_value = FAIL;
        goto done;
    }
#endif

done:
    if (sf_context && H5_free_subfiling_object_int(sf_context) < 0) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: couldn't free subfiling context object\n", __func__);
#endif

        ret_value = FAIL;
    }

    if (ret_value < 0) {
        errors = 1;
    }

    if (file_comm != MPI_COMM_NULL) {
        /*
         * Form consensus on whether closing subfiles was
         * successful across the IOC ranks. Note the use
         * of the previously-stored file_comm communicator,
         * since we have already freed the subfiling context
         * but still need a valid communicator to form a
         * consensus on whether any IOC ranks failed.
         */
        if (MPI_SUCCESS !=
            (mpi_code = MPI_Allreduce(&errors, &global_errors, 1, MPI_INT, MPI_SUM, file_comm))) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: MPI_Allreduce failed with rc %d\n", __func__, mpi_code);
#endif

            ret_value = FAIL;
        }

        if (global_errors > 0) {
#ifdef H5_SUBFILING_DEBUG
            HDprintf("%s: one or more IOC ranks couldn't close subfiles\n", __func__);
#endif

            ret_value = FAIL;
        }
    }

    return ret_value;
}

/*-------------------------------------------------------------------------
 * Function:    H5_subfile_fid_to_context
 *
 * Purpose:     This is a basic lookup function which returns the subfiling
 *              context id associated with the specified file->inode.
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
H5_subfile_fid_to_context(uint64_t sf_fid)
{
    if (!sf_open_file_map) {
#ifdef H5_SUBFILING_DEBUG
        HDprintf("%s: open file map is invalid\n", __func__);
#endif

        return -1;
    }

    for (int i = 0; i < sf_file_map_size; i++) {
        if (sf_open_file_map[i].h5_file_id == sf_fid) {
            return sf_open_file_map[i].sf_context_id;
        }
    }

    return -1;
} /* end H5_subfile_fid_to_context() */

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
        return;
    }

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

done:
    va_end(log_args);

    return;
}
#endif
