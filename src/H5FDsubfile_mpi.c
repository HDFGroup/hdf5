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

#include "H5FDsubfile_private.h"

static int sf_world_rank = -1;
static int sf_world_size = -1;
static int sf_open_file_count = 0;
static int sf_close_file_count = 0;
static int sf_ops_after_first_close = 0;

static int *request_count_per_rank = NULL;

atomic_int sf_workinprogress = 0;
atomic_int sf_work_pending = 0;
atomic_int sf_file_close_count = 0;
atomic_int sf_file_refcount = 0;
atomic_int sf_ioc_fini_refcount = 0;

#ifndef NDEBUG
FILE *sf_logfile = NULL;
#endif

int sf_shutdown_flag = 0;

const char *sf_subfile_prefix = ".";

#define MAX_WORK_PER_RANK 2

/*
=========================================
Private functions
=========================================
*/

/*
 * ---------------------------------------------------
 * Topology discovery related functions for choosing
 * IO Concentrator (IOC) ranks.
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
    const layout_t *host1 = (const layout_t *) h1;
    const layout_t *host2 = (const layout_t *) h2;
    return (host1->hostid > host2->hostid);
}

/*-------------------------------------------------------------------------
 * Function:    gather_topology_info
 *
 * Purpose:     Collectively generate a sorted collection of hostid+mpi_rank
 *              tuples.  The result is returned in the 'topology' field
 *              of the sf_topology_t structure.
 *
 * Return:      Sorted array of hostid/mpi_rank tuples.
 * Errors:      MPI_Abort if memory cannot be allocated.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
static void
gather_topology_info(sf_topology_t *info)
{
    sf_world_size = info->world_size;
    sf_world_rank = info->world_rank;

    if (info->layout)
        return;

    if (sf_world_size) {
        long      hostid = gethostid();
        layout_t  my_hostinfo;
        layout_t *layout =
            (layout_t *) calloc((size_t) sf_world_size + 1, sizeof(layout_t));
        if (layout == NULL) {
            perror("calloc failure!");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
        info->hostid = hostid;
        info->layout = layout;
        my_hostinfo.rank = sf_world_rank;
        my_hostinfo.hostid = hostid;
        info->layout[sf_world_rank] = my_hostinfo;
        if (sf_world_size > 1) {
            if (MPI_Allgather(&my_hostinfo, 2, MPI_LONG, info->layout, 2,
                    MPI_LONG, MPI_COMM_WORLD) == MPI_SUCCESS) {
                qsort(info->layout, (size_t) sf_world_size, sizeof(layout_t),
                    compare_hostid);
            }
        }
    }
}

/*-------------------------------------------------------------------------
 * Function:    count_nodes
 *
 * Purpose:     Initializes the sorted collection of hostid+mpi_rank
 *              tuples.  After initialization, the collection is scanned
 *              to determine the number of unique hostid entries.  This
 *              value will determine the number of actual IO concentrators
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
count_nodes(sf_topology_t *info)
{
    int  k, node_count, hostid_index = -1;
    long nextid;

    assert(info != NULL);
    if (info->layout == NULL)
        gather_topology_info(info);

    nextid = info->layout[0].hostid;
    info->node_ranks =
        (int *) calloc((size_t)(info->world_size + 1), sizeof(int));
    assert(info->node_ranks != NULL);

    if (nextid == info->hostid)
        hostid_index = 0;

    node_count = 1;
    /* Recall that the topology array has been sorted! */
    for (k = 1; k < info->world_size; k++) {
        if (info->layout[k].hostid != nextid) {
            nextid = info->layout[k].hostid;
            if (hostid_index < 0) {
                if (nextid == info->hostid)
                    hostid_index = k;
            }
            /* Record the index of new hostid */
            info->node_ranks[node_count++] = k;
        }
    }

    /* Mark the end of the node_ranks */
    info->node_ranks[node_count] = info->world_size;
    /* Save the index where we first located my hostid */
    info->node_index = hostid_index;
    return info->node_count = node_count;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__determine_ioc_count
 *
 * Purpose:     Once a sorted collection of hostid/mpi_rank tuples has been
 *              created and the number of unique hostids (nodes) has
 *              been determined, we may modify this "default" value for
 *              the number of IO Concentrators for this application.
 *
 *              The default of one(1) IO concentrator per node can be
 *              changed (principally for testing) by environment variable.
 *              if IOC_COUNT_PER_NODE is defined, then that integer value
 *              is utilized as a mulitiplier to modify the set of
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
int
H5FD__determine_ioc_count(int world_size, int world_rank,
    sf_ioc_selection_t ioc_select_method, char *ioc_select_option,
    sf_topology_t **thisapp)
{
    static int              ioc_count = 0;
    static int64_t          topology_id = 0;
    static sf_ioc_selection_t ioc_selection = ioc_selection_options;
    sf_topology_t *         app_topology = NULL;

    assert(thisapp != NULL);

    if (!ioc_count || (ioc_selection != ioc_select_method)) {
        int     k, node;
        int     node_index;
        int     iocs_per_node = 1;
        char *  envValue = NULL;
        int *   io_concentrator = NULL;
        int     index = (int) ioc_select_method;
        int64_t tag = (int64_t) SF_TOPOLOGY;
        topology_id = (int64_t)((tag << 32) | index);

        app_topology = (sf_topology_t *) get_subfiling_object(topology_id);
        assert(app_topology != NULL);
        app_topology->world_size = world_size;
        app_topology->world_rank = world_rank;
        if (app_topology->io_concentrator == NULL) {
            app_topology->io_concentrator = io_concentrator =
                (int *) malloc(((size_t) world_size * sizeof(int)));
        }
        assert(io_concentrator != NULL);
        app_topology->selection_type = ioc_selection = ioc_select_method;

        if (ioc_select_method == SELECT_IOC_ONE_PER_NODE) {
            ioc_count = count_nodes(app_topology);
            /* FIXME: This should ONLY be used for testing!
             * For production, we should probably limit the
             * number to a single IOC per node...
             * (based on performance numbers)
             */
            if ((envValue = getenv("IOC_COUNT_PER_NODE")) != NULL) {
                int value_check = atoi(envValue);
                if (value_check > 0) {
                    iocs_per_node = value_check;
                }
            }

            /* 'node_ranks' contain the index of the first instance of a hostid
             *  in the sorted sf_topology array. Our own index is 'node_index'.
             */
            node_index = app_topology->node_index;
            app_topology->local_peers =
                app_topology->node_ranks[node_index + 1] -
                app_topology->node_ranks[node_index];
            if (app_topology->layout[node_index].rank == world_rank) {
                app_topology->rank_is_ioc = true;
                app_topology->subfile_rank = node_index;
            }
            /* FIXME: This should ONLY be used for testing!
             * NOTE: The app_topology->local_peers is ONLY valid
             * for the current NODE.  There is no guarantee that
             * the application layout defines a uniform number of
             * MPI ranks per node...
             * Because this is only for testing purposes (at this time)
             * we can live with the assumption that if we define the
             * IOC_COUNT_PER_NODE environment variable, then each
             * node will have *at-least* that many MPI ranks assigned.
             * See above!
             */
            else if ((app_topology->local_peers > 1) && (iocs_per_node > 1)) {
                if (iocs_per_node > app_topology->local_peers)
                    iocs_per_node = app_topology->local_peers;
                for (k = 1; k < iocs_per_node; k++) {
                    if (app_topology->layout[node_index + k].rank ==
                        world_rank) {
                        app_topology->rank_is_ioc = true;
                        app_topology->subfile_rank = node_index + k;
                        break;
                    }
                }
            }
            /* More hacks for testing */
            if (io_concentrator) {
                int n_iocs = 0;
                for (node = 0; node < ioc_count; node++) {
                    for (k = 0; k < iocs_per_node; k++) {
                        node_index = app_topology->node_ranks[node];
                        io_concentrator[n_iocs++] =
                            (int) (app_topology->layout[node_index + k].rank);
                    }
                }
                ioc_count = n_iocs;
            }

            if (ioc_count > 0) {
                app_topology->n_io_concentrators = ioc_count;
                *thisapp = app_topology;
                // topology_id = (hid_t)record_subfiling_object(SF_TOPOLOGY,
                // app_topology);
            }
        } else {
            if (world_rank == 0) {
                printf("[%d - %s] IOC_selection(%d) with option(%s) is not "
                       "supported\n",
                    world_rank, __func__, (int) ioc_select_method,
                    ioc_select_option);
            }
        }
    } else {
        app_topology = (sf_topology_t *) get_subfiling_object(topology_id);
        *thisapp = app_topology;
    }
    return ioc_count;
}

/* ===================================================================== */
/* MPI_Datatype Creation functions.
 * These are catagorized by usage paterns, i.e. when data is sent to or
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
static MPI_Datatype
H5FD__create_first_mpi_type(subfiling_context_t *context, int ioc_depth,
    int64_t offset, int64_t target_write_bytes, int64_t first_io)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
    int64_t      stripe_size = context->sf_stripe_size;
    int64_t      blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t      offset_in_stripe = offset % stripe_size;
    int64_t      next_offset = blocksize_per_stripe - offset_in_stripe;
    int64_t      total_bytes = first_io;

    if (first_io == target_write_bytes) {
        if (first_io > 0) {
            return MPI_BYTE;
        }
    }
    if (first_io) {
        int  k;
        int  temp_blocks[64];
        int  temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (blocks == NULL) {
                perror("calloc");
                return newType;
            }
            disps = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (disps == NULL) {
                perror("calloc");
                return newType;
            }
        }
        blocks[0] = (int) first_io;
        disps[0] = (int) 0;
        for (k = 1; k <= ioc_depth; k++) {
            disps[k] = (int) next_offset;
            blocks[k] = (int) stripe_size;
            total_bytes += stripe_size;
            next_offset += context->sf_blocksize_per_stripe;
        }
        if (total_bytes != target_write_bytes) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
                __func__, total_bytes, target_write_bytes);
        }

        if (MPI_Type_indexed(k, blocks, disps, MPI_BYTE, &newType) !=
            MPI_SUCCESS) {
            perror("MPI_Type_indexed failed!");
            return newType;
        }
        MPI_Type_commit(&newType);
        if (1) {
            int type_size;
            MPI_Type_size(newType, &type_size);
            if (type_size != target_write_bytes) {
                printf("%s: type_size=%d should be: %ld\n", __func__, type_size,
                    target_write_bytes);
            }
        }
        if (ioc_depth > 64) {
            if (blocks != temp_blocks) {
                free(blocks);
                blocks = NULL;
            }
            if (disps != temp_disps) {
                free(disps);
                disps = NULL;
            }
        }
    }
    return newType;
}

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
static MPI_Datatype
H5FD__create_final_mpi_type(subfiling_context_t *context, int ioc_depth,
    int64_t target_write_bytes, int64_t last_write)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
    int64_t      stripe_size = context->sf_stripe_size;
    int64_t      depth_in_bytes = (stripe_size * ioc_depth) + last_write;
    int64_t      total_bytes = last_write;

    if (depth_in_bytes == target_write_bytes) {
        if (depth_in_bytes > 0) {
            return MPI_BYTE;
        }
    }

    if (depth_in_bytes) {
        int  k;
        int  temp_blocks[64];
        int  temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (blocks == NULL) {
                perror("calloc");
                return newType;
            }
            disps = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (disps == NULL) {
                perror("calloc");
                return newType;
            }
        }

        for (k = 0; k < ioc_depth; k++) {
            disps[k] = (int) (k * context->sf_blocksize_per_stripe);
            blocks[k] = (int) stripe_size;
            total_bytes += stripe_size;
        }
        blocks[k - 1] = (int) last_write;
        if (total_bytes != target_write_bytes) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
                __func__, total_bytes, target_write_bytes);
        }

        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &newType) !=
            MPI_SUCCESS) {
            return MPI_DATATYPE_NULL;
        }
        MPI_Type_commit(&newType);
        if (ioc_depth > 64) {
            if (blocks != temp_blocks) {
                free(blocks);
                blocks = NULL;
            }
            if (disps != temp_disps) {
                free(disps);
                disps = NULL;
            }
        }
    }
    return newType;
}

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
static MPI_Datatype
H5FD__create_f_l_mpi_type(subfiling_context_t *context, int ioc_depth,
    int64_t offset, int64_t target_write_bytes, int64_t first_write,
    int64_t last_write)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
    int64_t      stripe_size = context->sf_stripe_size;
    int64_t      blocksize_per_stripe = context->sf_blocksize_per_stripe;
    int64_t      offset_in_stripe = offset % stripe_size;
    int64_t      next_offset = blocksize_per_stripe - offset_in_stripe;
    int64_t      total_bytes = first_write + last_write;

    /* We might actaully check that the 'target_write_bytes'
     * input variable exceeds 2Gb.  If so, then we should
     * always create a derived type.
     */
    if ((total_bytes == target_write_bytes) &&
        (context->topology->n_io_concentrators == 1)) {
        return MPI_BYTE;
    } else if (first_write) {
        int  k;
        int  temp_blocks[64];
        int  temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (blocks == NULL) {
                perror("calloc");
                return newType;
            }
            disps = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (disps == NULL) {
                perror("calloc");
                return newType;
            }
        }

        blocks[0] = (int) first_write;
        disps[0] = 0;

        for (k = 1; k < ioc_depth; k++) {
            blocks[k] = (int) stripe_size;
            disps[k] = (int) next_offset;
            next_offset += context->sf_blocksize_per_stripe;
            total_bytes += stripe_size;
        }
        if (k == 1) {
            disps[k] = (int) next_offset;
        }
        blocks[k] = (int) last_write;

        if (total_bytes != target_write_bytes) {
            printf("[%d] Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
                sf_world_rank, __func__, total_bytes, target_write_bytes);
        }

        if (MPI_Type_indexed(k + 1, blocks, disps, MPI_BYTE, &newType) !=
            MPI_SUCCESS) {
            perror("MPI_Type_indexed failed!");
            return MPI_DATATYPE_NULL;
        }

        MPI_Type_commit(&newType);
        if (ioc_depth > 64) {
            if (blocks != temp_blocks) {
                free(blocks);
                blocks = NULL;
            }
            if (disps != temp_disps) {
                free(disps);
                disps = NULL;
            }
        }
    }
    return newType;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__create_mpi_uniform_type
 *
 * Purpose:     Return an appropriate MPI datatype to represent the typical
 *              IO operation when reading or writing data to or from an IO
 *              Concentrator (IOC).
 *
 *              Each data segment is of 'stripe_size' length and will be
 *              seperated from a previous or following segment by
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
static MPI_Datatype
H5FD__create_mpi_uniform_type(
    subfiling_context_t *context, int ioc_depth, int64_t target_write_bytes)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
    int64_t      stripe_size = context->sf_stripe_size;
    int64_t      check_depth = stripe_size * ioc_depth;
    int64_t      total_bytes = 0;

    if (check_depth == stripe_size) {
        if (target_write_bytes > 0) {
            return MPI_BYTE;
        }
    }

    if (target_write_bytes) {
        int  k;
        int  temp_blocks[64];
        int  temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (blocks == NULL) {
                perror("calloc");
                return newType;
            }
            disps = (int *) calloc((size_t) ioc_depth, sizeof(int));
            if (disps == NULL) {
                perror("calloc");
                return newType;
            }
        }
        for (k = 0; k < ioc_depth; k++) {
            disps[k] = (int) (k * context->sf_blocksize_per_stripe);
            blocks[k] = (int) (stripe_size);
            total_bytes += stripe_size;
        }

        if (total_bytes != target_write_bytes) {
            printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
                __func__, total_bytes, target_write_bytes);
        }

        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &newType) !=
            MPI_SUCCESS) {
            perror("MPI_Type_indexed failed!");
            return MPI_DATATYPE_NULL;
        }
        MPI_Type_commit(&newType);
        if (1) {
            int type_size;
            MPI_Type_size(newType, &type_size);
            if (type_size != target_write_bytes) {
                printf("%s: type_size=%d should be: %ld\n", __func__, type_size,
                    target_write_bytes);
            }
        }

        if (ioc_depth > 64) {
            if (blocks != temp_blocks) {
                free(blocks);
                blocks = NULL;
            }
            if (disps != temp_disps) {
                free(disps);
                disps = NULL;
            }
        }
    }
    return newType;
}

static file_map_to_context_t *sf_open_file_map = NULL;
static int                    sf_file_map_size = 0;
#define DEFAULT_MAP_ENTRIES 8

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
record_fid_to_subfile(hid_t fid, hid_t subfile_context_id, int *next_index)
{
    herr_t status = SUCCEED;
    int    index;
    if (sf_file_map_size == 0) {
        int i;
        sf_open_file_map = (file_map_to_context_t *) malloc(
            (size_t) DEFAULT_MAP_ENTRIES * sizeof(file_map_to_context_t));
        if (sf_open_file_map == NULL) {
            perror("calloc");
            return FAIL;
        }
        sf_file_map_size = DEFAULT_MAP_ENTRIES;
        for (i = 0; i < sf_file_map_size; i++) {
            sf_open_file_map[i].h5_file_id = H5I_INVALID_HID;
        }
    }
    for (index = 0; index < sf_file_map_size; index++) {
        if (sf_open_file_map[index].h5_file_id == H5I_INVALID_HID) {
            sf_open_file_map[index].h5_file_id = fid;
            sf_open_file_map[index].sf_context_id = subfile_context_id;
            if (next_index) {
                *next_index = index;
            }
            return status;
        }
    }
    if (index == sf_file_map_size) {
        int i;
        sf_open_file_map = realloc(sf_open_file_map,
           ((size_t)(sf_file_map_size * 2) * sizeof(file_map_to_context_t)));
        if (sf_open_file_map == NULL) {
            perror("realloc");
            return FAIL;
        }
        sf_file_map_size *= 2;
        for (i = index; i < sf_file_map_size; i++) {
            sf_open_file_map[i].h5_file_id = H5I_INVALID_HID;
        }

        if (next_index) {
            *next_index = index;
        }

        sf_open_file_map[index].h5_file_id = fid;
        sf_open_file_map[index++].sf_context_id = subfile_context_id;
    }
    return status;
}

/*-------------------------------------------------------------------------
 * Function:    fid_map_to_context
 *
 * Purpose:     This is a basic lookup function which returns the subfiling
 *              context id associated with the specified file->inode.
 *
 * Return:      The Subfiling context ID if it exists.
 * Errors:      H5I_INVALID_HID if the inode to context map is not found.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *
 *-------------------------------------------------------------------------
 */
hid_t
fid_map_to_context(hid_t sf_fid)
{
    if (sf_open_file_map) {
        int i;
        for (i = 0; i < sf_file_map_size; i++) {
            if (sf_open_file_map[i].h5_file_id == sf_fid) {
                return sf_open_file_map[i].sf_context_id;
            }
        }
    }
    return H5I_INVALID_HID;
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
void
clear_fid_map_entry(hid_t sf_fid)
{
    if (sf_open_file_map) {
        int i;
        for (i = 0; i < sf_file_map_size; i++) {
            if (sf_open_file_map[i].h5_file_id == sf_fid) {
                sf_open_file_map[i].h5_file_id = H5I_INVALID_HID;
                sf_open_file_map[i].sf_context_id = H5I_INVALID_HID;
                return;
            }
        }
    }
}

/*-------------------------------------------------------------------------
 * Function:    active_map_entries
 *
 * Purpose:     Count the number of entries that have valid h5_file_id
 *              values.
 *
 * Return:      The number of active map entries (can be zero).
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
active_map_entries(void)
{
    int i, map_entries = 0;
    for (i = 0; i < sf_file_map_size; i++) {
        if (sf_open_file_map[i].h5_file_id != H5I_INVALID_HID) {
            map_entries++;
        }
    }
    return map_entries;
}

/*-------------------------------------------------------------------------
 * Function:    init__indep_io
 *
 * Purpose:     Utility function to initialize the set of IO transactions
 *              used to communicate with IO concentrators for read and write
 *              IO operations.
 *
 * Return:      A filled set of vectors (1 entry per IO concentrator) which
 *              fully describe the IO transactions for read and writes.
 *              At most, every IO concentrator will have a descriptor which
 *              identifies the local memory offset, the virtual FILE offset,
 *              and the total length of the IO which will be sent to or
 *              received from the individual IOCs.
 *
 *              For IO operations which involve a subset of IO concentrators,
 *              the vector entries for the unused IOCs will have lengths of
 *              zero and MPI NULL datatypes.
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
static int
init__indep_io(subfiling_context_t *sf_context, int64_t *sf_source_data_offset,
    int64_t *sf_datasize, int64_t *sf_offset, MPI_Datatype *sf_dtype,
    int64_t offset, int64_t elements, int dtype_extent)
{

    int     container_count = sf_context->topology->n_io_concentrators;
    int64_t stripe_size = sf_context->sf_stripe_size;
    int64_t data_size = elements * dtype_extent;

    int64_t start_id = offset / stripe_size;
    int64_t offset_in_stripe = offset % stripe_size;
    int64_t start_length = MIN(data_size, (stripe_size - offset_in_stripe));
    int64_t start_row = start_id / container_count;
    int64_t ioc_start = start_id % container_count;

    int64_t final_offset = offset + data_size;
    int64_t final_id = final_offset / stripe_size;
    int64_t final_length =
        (start_length == data_size ? 0 : final_offset % stripe_size);
    int64_t ioc_final = final_id % container_count;
    int64_t container_bytes, total_bytes = 0;
    int64_t source_offset = 0;

    int     row_id_start = (int) (start_id - ioc_start);
    int     row_id_final = (int) (final_id - ioc_final);
    int     i, k, depth = ((row_id_final - row_id_start) / container_count) + 1;
    int     container_id = (int) start_id;
    int64_t row_offset = (int64_t)(start_row * stripe_size);

    for (i = 0, k = (int) ioc_start; i < container_count; i++) {
        int     container_depth = depth;
        hbool_t is_first = false, is_last = false;
        container_bytes = 0;
        sf_datasize[k] = container_bytes;
        if (total_bytes < data_size) {
            if (k == ioc_start) {
                is_first = true;
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

        sf_source_data_offset[k] = source_offset;
        sf_datasize[k] = container_bytes;
        sf_offset[k] = row_offset + offset_in_stripe;

        if (container_count == 1) {
            sf_dtype[k] = MPI_BYTE;
        } else {
            /* Fill the IO datatypes */
            if (is_first) {
                if (is_last) { /* First + Last */
                    sf_dtype[k] = H5FD__create_f_l_mpi_type(sf_context,
                        container_depth + 1, sf_offset[k], container_bytes,
                        start_length, final_length);
                } else { /* First ONLY */
                    sf_dtype[k] =
                        H5FD__create_first_mpi_type(sf_context, container_depth,
                            sf_offset[k], container_bytes, start_length);
                }
                source_offset += start_length;
                offset_in_stripe = 0;
            } else if (is_last) { /* Last ONLY */
                source_offset += stripe_size;
                sf_dtype[k] = H5FD__create_final_mpi_type(
                    sf_context, container_depth, container_bytes, final_length);
            } else { /* Everything else (uniform) */
                source_offset += stripe_size;
                sf_dtype[k] = H5FD__create_mpi_uniform_type(
                    sf_context, container_depth, container_bytes);
            }
        }
        k++;
        container_id++;

        if (k == container_count) {
            k = 0;
            depth = ((row_id_final - container_id) / container_count) + 1;
            row_offset += stripe_size;
        }
    }
    if (total_bytes != data_size) {
        printf("Error: total_bytes != data_size\n");
    }

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__init_subfile_context
 *
 * Purpose:     Called as part of the HDF5 file + subfiling opening.
 *              This initializes the subfiling context and associates
 *              this context with the specific HDF5 file.
 *
 * Return:      Success (0) or Faiure (-1)
 * Errors:      If MPI operations fail for some reason.
 *
 * Programmer:  Richard Warren
 *              7/17/2020
 *
 * Changes:     Initial Version/None.
 *-------------------------------------------------------------------------
 */
int
H5FD__init_subfile_context(sf_topology_t *thisApp, int n_iocs, int world_rank,
    subfiling_context_t *newContext)
{
    static MPI_Comm sf_msg_comm = MPI_COMM_NULL;
    static MPI_Comm sf_data_comm = MPI_COMM_NULL;

    assert(newContext != NULL);

    if (newContext->topology != thisApp) {
        int   status;
        char *envValue = NULL;

        newContext->topology = thisApp;
        newContext->sf_msg_comm = sf_msg_comm;
        newContext->sf_data_comm = sf_data_comm;
        newContext->sf_group_comm = MPI_COMM_NULL;
        newContext->sf_intercomm = MPI_COMM_NULL;
        newContext->sf_stripe_size = DEFAULT_STRIPE_SIZE;
        newContext->sf_write_count = 0;
        newContext->sf_read_count = 0;
        newContext->sf_eof = 0;
        if ((envValue = getenv("IOC_STRIPE_SIZE")) != NULL) {
            long value_check = atol(envValue);
            if (value_check > 0) {
                newContext->sf_stripe_size = (int64_t) value_check;
            }
        }
        if ((envValue = getenv("SUBFILE_PREFIX")) != NULL) {
            char temp[PATH_MAX];
            sprintf(temp, "%s", envValue);
            newContext->subfile_prefix = strdup(temp);
            sf_subfile_prefix = strdup(temp);
        }

        newContext->sf_blocksize_per_stripe =
            newContext->sf_stripe_size * n_iocs;
        if (sf_msg_comm == MPI_COMM_NULL) {
            status = MPI_Comm_dup(MPI_COMM_WORLD, &newContext->sf_msg_comm);
            if (status != MPI_SUCCESS)
                goto err_exit;
            status = MPI_Comm_set_errhandler(
                newContext->sf_msg_comm, MPI_ERRORS_RETURN);
            if (status != MPI_SUCCESS)
                goto err_exit;
            sf_msg_comm = newContext->sf_msg_comm;
        }
        if (sf_data_comm == MPI_COMM_NULL) {
            status = MPI_Comm_dup(MPI_COMM_WORLD, &newContext->sf_data_comm);
            if (status != MPI_SUCCESS)
                goto err_exit;
            status = MPI_Comm_set_errhandler(
                newContext->sf_data_comm, MPI_ERRORS_RETURN);
            if (status != MPI_SUCCESS)
                goto err_exit;
            sf_data_comm = newContext->sf_data_comm;
        }
        if (n_iocs > 1) {
            status = MPI_Comm_split(MPI_COMM_WORLD, thisApp->rank_is_ioc,
                world_rank, &newContext->sf_group_comm);
            if (status != MPI_SUCCESS)
                goto err_exit;
            status = MPI_Comm_size(
                newContext->sf_group_comm, &newContext->sf_group_size);
            if (status != MPI_SUCCESS)
                goto err_exit;
            status = MPI_Comm_rank(
                newContext->sf_group_comm, &newContext->sf_group_rank);
            if (status != MPI_SUCCESS)
                goto err_exit;
            /*
             * There may be additional functionality we need for the IOCs...
             * If so, then can probably initialize those things here!
             */
        } else {
            newContext->sf_group_comm = MPI_COMM_SELF;
            newContext->sf_group_size = 1;
            newContext->sf_group_rank = 0;
        }
    }
    return 0;

err_exit:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    Internal read__independent.
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
 *              request to fullfill.
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
static int
read__independent(int n_io_concentrators, hid_t context_id, int64_t offset,
    int64_t elements, int dtype_extent, void *data)
{
    int          i, ioc, n_waiting = 0, status = 0;
    int *        io_concentrator = NULL;
    int          indices[n_io_concentrators];
    MPI_Request  reqs[n_io_concentrators];
    MPI_Status   stats[n_io_concentrators];
    int64_t      source_data_offset[n_io_concentrators];
    int64_t      ioc_read_datasize[n_io_concentrators];
    int64_t      ioc_read_offset[n_io_concentrators];
    MPI_Datatype ioc_read_type[n_io_concentrators];
    useconds_t   delay = 100;

    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);


    /* Note that the sf_write_count is only tracked by an IOC rank */
    if (sf_context->sf_write_count && (sf_context->sf_fid > 0)) {
        fdatasync(sf_context->sf_fid);

        /* We can attempt to give the IOC more compute time
         * if we extend out delaying tactic when awaiting
         * responses.
         */
        delay *= sf_context->topology->world_size;
    }

    io_concentrator = sf_context->topology->io_concentrator;
    if (init__indep_io(sf_context, source_data_offset, ioc_read_datasize,
            ioc_read_offset, ioc_read_type, offset, elements,
            dtype_extent) < 0) {
        return -1;
    }
    /* Prepare the IOCs with a message which indicates the length
     * and file offset for the actual data to be provided.
     */
    for (ioc = 0; ioc < n_io_concentrators; ioc++) {
        int64_t msg[3] = {ioc_read_datasize[ioc], ioc_read_offset[ioc],
            sf_context->sf_context_id};
        char *  sourceData = (char *) data;
        int64_t sourceOffset = source_data_offset[ioc];
        int     packsize = 0;
        // printf("[%d] %s: context_id = 0x%lx\n", sf_world_rank, __func__,
        // sf_context->sf_context_id);
        /* We may not require data from this IOC...
         * or we may read the data directly from the file!
         * Check the size to verify!
         */
        reqs[ioc] = MPI_REQUEST_NULL;
        if (ioc_read_datasize[ioc] == 0) {
            continue;
        }

#ifndef NDEBUG
        if (sf_verbose_flag) {
            fprintf(stdout,
                    "[%d %s] Requesting %ld read bytes from IOC(%d): "
                    "sourceOffset=%ld subfile_offset=%ld\n",
                    sf_world_rank, __func__, msg[0], io_concentrator[ioc],
                    sourceOffset, msg[1]);
            fflush(stdout);
        }
#endif

        status = MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[ioc],
            READ_INDEP, sf_context->sf_msg_comm);
        if (status != MPI_SUCCESS) {
            printf("[%d] MPI_Send failure!", sf_world_rank);
            return status;
        } else {
            if (ioc_read_type[ioc] == MPI_BYTE) {
                int bytes = (int) ioc_read_datasize[ioc];
                status = MPI_Irecv(&sourceData[sourceOffset], bytes,
                    ioc_read_type[ioc], io_concentrator[ioc], READ_INDEP_DATA,
                    sf_context->sf_data_comm, &reqs[ioc]);
            } else {
                MPI_Pack_size(1, ioc_read_type[ioc], MPI_COMM_WORLD, &packsize);
                status = MPI_Irecv(&sourceData[sourceOffset], 1,
                    ioc_read_type[ioc], io_concentrator[ioc], READ_INDEP_DATA,
                    sf_context->sf_data_comm, &reqs[ioc]);
            }
            if (status != MPI_SUCCESS) {
                int  length = 256;
                char error_string[length];
                MPI_Error_string(status, error_string, &length);
                printf("(%s) MPI_Irecv error: %s\n", __func__, error_string);
                return status;
            }
            n_waiting++;
        }
    }
    /* We've queued all of the Async READs, now we just need to
     * complete them in any order...
     */
    while (n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int  length = 256;
            char error_string[length];
            MPI_Error_string(status, error_string, &length);
            printf("(%s) MPI_Waitsome error: %s\n", __func__, error_string);
            for (i = 0; i < n_waiting; i++) {
                printf(
                    "stats[%d].SOURCE=%d, stats.TAG=%d, stats.MPI_ERROR=%d\n",
                    i, stats[i].MPI_SOURCE, stats[i].MPI_TAG,
                    stats[i].MPI_ERROR);
                fflush(stdout);
            }
            return status;
        }

        for (i = 0; i < ready; i++) {
#ifndef NDEBUG
            if (sf_verbose_flag) {
                fprintf(stdout,
                        "[%d] READ bytes(%ld) of data from ioc_concentrator %d "
                        "complete\n",
                        sf_world_rank, ioc_read_datasize[indices[i]],
                        indices[i]);
                fflush(stdout);
            }
#endif
            if (ioc_read_type[indices[i]] != MPI_BYTE) {
                MPI_Type_free(&ioc_read_type[indices[i]]);
            }
            n_waiting--;
        }
        if (n_waiting)
            usleep(delay);
    }
    return status;
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client sf_read_independent
 *
 * Purpose:     A public function which wraps the Internal version
 *              and allows the addition of the additional 'n_io_concentrator'
 *              argument.  This is important as it allows me to skip
 *              memory allocation functions since storage for the various
 *              vector variables is on the call stack...
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
sf_read_independent(hid_t sf_fid, int64_t offset, int64_t elements,
    int dtype_extent, void *data)
{
    hid_t                sf_context_id = fid_map_to_context(sf_fid);
    subfiling_context_t *sf_context = get_subfiling_object(sf_context_id);

    assert(sf_context != NULL);
    return read__independent(sf_context->topology->n_io_concentrators,
        sf_context_id, offset, elements, dtype_extent, data);
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client sf_read_vector
 *
 * Purpose:     Another read__independent wrapper.  In this instance
 *              we simply loop over then collection of vector entries
 *              and call the sf__read_independent function.
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
herr_t
sf_read_vector(hid_t h5_fid, hssize_t count, haddr_t addrs[], hsize_t sizes[],
    void *bufs[] /* data_out */)
{
    hssize_t             k;
    herr_t               ret_value = SUCCEED;
    hid_t                sf_context_id = fid_map_to_context(h5_fid);
    subfiling_context_t *sf_context = get_subfiling_object(sf_context_id);

    assert(sf_context != NULL);

    /* Unfortunately, we cannot know whether an incoming vector represents
     * (as a whole) a contiguous block of data.  Certainly each vector entry
     * is a contiguous block of data.  There is a temptation of course to
     * attempt to merge multiple vector instances into a single MPI write
     * by utilizing MPI datatypes. At this time we don't attempt to
     * consolidate multiple vector entries and are thus forced to loop
     * over the vector, sending one a vector entry at a time.
     */
    for (k = 0; k < (int32_t) count; k++) {
        if (read__independent(sf_context->topology->n_io_concentrators,
                sf_context_id, (int64_t) addrs[k], (int64_t) sizes[k], 1,
                bufs[k]) != 0) {
            printf("%s - encountered an internal error!\n", __func__);
            goto errors;
        }
    }
    return ret_value;

errors:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    Internal write__independent.
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
 *              request to fullfill.
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
static int
write__independent(int n_io_concentrators, hid_t context_id, int64_t offset,
    int64_t elements, int dtype_extent, const void *data)
{
    int *        io_concentrator = NULL;
    int          acks[n_io_concentrators];
    int          indices[n_io_concentrators];
    MPI_Request  reqs[n_io_concentrators];
    MPI_Status   stats[n_io_concentrators];
    int64_t      source_data_offset[n_io_concentrators];
    int64_t      ioc_write_datasize[n_io_concentrators];
    int64_t      ioc_write_offset[n_io_concentrators];
    MPI_Datatype ioc_write_type[n_io_concentrators];
	int          n_waiting = 0, status = 0, errors = 0;
    int          i, target, ioc;
    useconds_t   delay = 100;

    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context);

    io_concentrator = sf_context->topology->io_concentrator;

    if (sf_context->topology->rank_is_ioc) {
        sf_context->sf_write_count++;

        /* We can attempt to give the IOC more compute time
         * if we extend out delaying tactic when awaiting
         * responses.
         */
        delay *= sf_context->topology->world_size;
    }

    /* The following function will initialize the collection of IO transfer
     * parameters, i.e. local memory (source) offsets, target file offsets,
     * target data sizes (in bytes), and a MPI Datatype for each of the
     * IO concentrator transactions.
     *
     * For small transfers, at least 1 IOC instance will have valid info.
     * For larger transfers, it is likely that the full set of
     * n_io_concentrators will be utilized.  If the total transaction size is
     * less than n_io_concentrators X stripe_size, then the MPI datatype should
     * probably be MPI_BYTE.  Larger tranactions will create MPI derived
     * datatypes to span the entire logical collection of stripes.  Said
     * differently, the largest IO requests will require a stripe depth greater
     * than one.
     */
    if (init__indep_io(sf_context, source_data_offset, ioc_write_datasize,
            ioc_write_offset, ioc_write_type, offset, elements,
            dtype_extent) < 0) {
        return -1;
    }

    /* Prepare the IOCs with a message which indicates the length
     * of the actual data to be written.  We also provide the file
     * offset so that when the IOC recieves the data (in whatever order)
     * they can lseek to the correct offset and write the data.
     *
     * NOTE: we use 'pwrite' which provides the seek functionality
     * as part of the API.
     */
    for (target = 0; target < n_io_concentrators; target++) {
        int64_t sourceOffset;
        int64_t msg[3] = {
            0,
        };
        const char *sourceData = (const char *) data;
        ioc = (sf_world_rank + target) % n_io_concentrators;

        sourceOffset = source_data_offset[ioc];
        msg[0] = ioc_write_datasize[ioc];
        msg[1] = ioc_write_offset[ioc];
        msg[2] = sf_context->sf_context_id;
        acks[ioc] = 0;
        reqs[ioc] = MPI_REQUEST_NULL;

        if (ioc_write_datasize[ioc] == 0) {
            continue;
        }

#ifndef NDEBUG
        if (sf_verbose_flag)
        {
            fprintf(stdout,
                    "[%d %s]: write_dest[ioc(%d), "
                    "sourceOffset=%ld, datasize=%ld, foffset=%ld]\n",
                    sf_world_rank, __func__, ioc, sourceOffset,
                    ioc_write_datasize[ioc], ioc_write_offset[ioc]);
            fflush(stdout);
        }
#endif


        /* Send the Message HEADER which indicates the requested IO operation
         * (via the message TAG) along with the data size and file offset.
         */

        status = MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[ioc],
            WRITE_INDEP, sf_context->sf_msg_comm);

        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an "
                   "error(%s)\n",
                sf_world_rank, sizeof(msg), io_concentrator[ioc], estring);
            fflush(stdout);
            break; /* If unable to send to an IOC, we can call it quits...  */
        }

        /* Wait for memory to be allocated on the target IOC so that we can
         * start sending user data to this IOC.
         * FIXME: We could possibly use Irecv for handling ACKs.  This could
         * potentially allow some additional overlap of posting IO requests
         * to the collection of IO Concentrators.
         */
        status = MPI_Recv(&acks[ioc], 1, MPI_INT, io_concentrator[ioc],
            WRITE_INDEP_ACK, sf_context->sf_data_comm, &stats[ioc]);

        if (status == MPI_SUCCESS) {
#ifndef NDEBUG
            if (sf_verbose_flag) {
                if (sf_logfile) {
                    fprintf(sf_logfile, "[%d] received ack(%d) from ioc(%d)\n",
                        sf_world_rank, acks[ioc], ioc);
                }
            }
#endif
            /* No errors, start sending data to the IOC.
             * If the data transfer is small enough, we don't utilize a
             * derived MPI type, i.e. we use MPI_BYTE.
             */
            if (acks[ioc] > 0) {
                if (ioc_write_type[ioc] == MPI_BYTE) {
                    int datasize = (int) (ioc_write_datasize[ioc] & INT32_MASK);
                    status = MPI_Issend(&sourceData[sourceOffset], datasize,
                        MPI_BYTE, io_concentrator[ioc], WRITE_INDEP_DATA,
                        sf_context->sf_data_comm, &reqs[ioc]);
                } else {
                    status = MPI_Issend(&sourceData[sourceOffset], 1,
                        ioc_write_type[ioc], io_concentrator[ioc],
                        WRITE_INDEP_DATA, sf_context->sf_data_comm, &reqs[ioc]);
                }
                n_waiting++;
            }
        } else {
            errors++;
            puts("ACK error!");
            fflush(stdout);
            break;
        }

        /* Check the status of our MPI_Issend... */
        if (status != MPI_SUCCESS) {
            errors++;
            printf("[%d] ERROR! Unable to Send data to ioc(%d)\n",
                sf_world_rank, ioc);
            fflush(stdout);
            break;
        }
    }

    /* Wait for the Issends to complete (in any order) */
    while (n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                sf_world_rank, __func__, estring);
            fflush(stdout);
            errors++;
        }
        for (i = 0; i < ready; i++) {
            /* One of the Issend calls has completed
             * If we used a derived type to send data, then should free
             * that datatype instance.
             */
            if (ioc_write_type[indices[i]] != MPI_BYTE) {
                MPI_Type_free(&ioc_write_type[indices[i]]);
            }
            n_waiting--;
        }
        if (n_waiting)
            usleep(delay);
    }
    if (errors)
        return -1;
    return status;
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client sf_write_independent
 *
 * Purpose:     A public function which wraps the Internal version
 *              and allows the addition of the additional 'n_io_concentrator'
 *              argument.  This is important as it allows me to skip
 *              memory allocation functions since storage for the various
 *              vector variables is on the call stack...
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
sf_write_independent(hid_t sf_fid, int64_t offset, int64_t elements,
    int dtype_extent, const void *data)
{
    hid_t                sf_context_id = fid_map_to_context(sf_fid);
    subfiling_context_t *sf_context = get_subfiling_object(sf_context_id);

    assert(sf_context != NULL);
    return write__independent(sf_context->topology->n_io_concentrators,
        sf_context_id, offset, elements, dtype_extent, data);
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client sf_write_vector
 *
 * Purpose:     Another write__independent wrapper.  As with the
 *              sf_read_vector function, we simply loop over the vector
 *              elements and call the underlying write_independent function.
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
herr_t
sf_write_vector(hid_t h5_fid, hssize_t count, haddr_t addrs[], hsize_t sizes[],
    void *bufs[] /* data_in */)
{
    hssize_t             k;
    herr_t               ret_value = SUCCEED;
    hid_t                sf_context_id = fid_map_to_context(h5_fid);
    subfiling_context_t *sf_context = get_subfiling_object(sf_context_id);

    assert(sf_context != NULL);

    /*
     * Call the underlying write function for each vector element.
     */
    for (k = 0; k < count; k++) {
        if (write__independent(sf_context->topology->n_io_concentrators,
                sf_context_id, (int64_t) addrs[k], (int64_t) sizes[k], 1,
                bufs[k]) < 0) {
            printf("%s - encountered an internal error!\n", __func__);
            goto errors;
        }
    }
    return ret_value;

errors:
    return FAIL;
}

int
sf_truncate(hid_t h5_fid, haddr_t H5_ATTR_PARALLEL_UNUSED addr)
{
    hid_t                sf_context_id = fid_map_to_context(h5_fid);
    subfiling_context_t *sf_context = get_subfiling_object(sf_context_id);

    assert(sf_context != NULL);

#if 0
    if (sf_context->topology->n_io_concentrators > 1) {
        if (MPI_Allreduce(&addr_in, &addr_max, 1, MPI_INT64_T, MPI_MAX, sf_context->sf_data_comm) != MPI_SUCCESS) {
            addr_max = (int64_t)addr;
        }
    }
    if (sf_context->topology->rank_is_ioc) {
        int container_count = sf_context->topology->n_io_concentrators;
        int64_t stripe_size = sf_context->sf_stripe_size;
        int64_t addr_max_stripe_id = addr_max / stripe_size;
        int64_t offset_in_stripe = addr_max % stripe_size;
        int max_row = (int)(addr_max_stripe_id / container_count);
        int addr_max_ioc = (int)(addr_max_stripe_id % container_count);
        /*
         * Subfiling storage can be thought of as a 2D array in which each row
         * contains N columns (containers).  The containers have a fixed width
         * so that number of bytes in any "row" is (# of containers) X stripe_size.
         *
         * Given any offset, we can identify the 'row' of the specified offset
         * as well as the offset within row and thus the specific container and
         * actual offset within that container.
         */
        int64_t row_start = max_row * stripe_size;
        int64_t container_addr_max = row_start + stripe_size;
        if (sf_context->topology->subfile_rank == addr_max_ioc) {
            container_addr_max = row_start + offset_in_stripe;
        }
        else if (sf_context->topology->subfile_rank < addr_max_ioc) {
            container_addr_max = row_start + stripe_size;
        }
        if(-1 == HDftruncate(sf_context->sf_fid, (HDoff_t)container_addr_max)) {
            puts("truncate failed!");
            return -1;
        }
    }
#endif
    return 0;
}

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
static int
close__subfiles(
    subfiling_context_t *sf_context, int n_io_concentrators, hid_t fid)
{
    int         i, status;
    int         global_errors = 0, errors = 0;
    int         n_waiting = 0;
    int         indices[n_io_concentrators];
    int         ioc_acks[n_io_concentrators];
    MPI_Request reqs[n_io_concentrators];
    int *       io_concentrator = sf_context->topology->io_concentrator;

    /* The map from fid to context can now be cleared */
    clear_fid_map_entry(fid);

    for (i = 0; i < n_io_concentrators; i++) {
        int64_t msg[3] = {0, 0, sf_context->sf_context_id};
        status = MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[i], CLOSE_OP,
            sf_context->sf_msg_comm);
        if (status == MPI_SUCCESS) {
            status = MPI_Irecv(&ioc_acks[i], 1, MPI_INT, io_concentrator[i],
                COMPLETED, sf_context->sf_data_comm, &reqs[i]);
        }
        if (status != MPI_SUCCESS) {
            printf("[%d] MPI close_subfiles failure!", sf_world_rank);
            errors++;
        } else
            n_waiting++;
    }

    while (n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(
            n_io_concentrators, reqs, &ready, indices, MPI_STATUSES_IGNORE);
        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                sf_world_rank, __func__, estring);
            fflush(stdout);
            errors++;
        }
        for (i = 0; i < ready; i++) {
            n_waiting--;
        }
    }

    if (sf_context->topology->rank_is_ioc) {
        finalize_ioc_threads();
        wait_for_thread_main();
    }

    status = MPI_Allreduce(
        &errors, &global_errors, 1, MPI_INT, MPI_SUM, sf_context->sf_data_comm);

    if (status != MPI_SUCCESS) {
        global_errors++;
    }
    return global_errors;
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client sf_close_subfiles
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
int
sf_close_subfiles(hid_t fid)
{
    hid_t                context_id = fid_map_to_context(fid);
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);
    return close__subfiles(
        sf_context, sf_context->topology->n_io_concentrators, fid);
}

/*-------------------------------------------------------------------------
 * Function:    Internal open__subfiles
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
 *              which we recieve as one of the input arguments.
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

static int
open__subfiles(subfiling_context_t *sf_context, int n_io_concentrators,
    hid_t fid, char *prefix, int flags)
{
    int         i, ret, status, n_waiting = 0;
    int *       io_concentrator = NULL;
    int         indices[n_io_concentrators];
    int         ioc_acks[n_io_concentrators];
    MPI_Request reqs[n_io_concentrators];

    assert(sf_context != NULL);

    if (prefix) {
        if (sf_context->subfile_prefix) {
            if (strcmp(sf_context->subfile_prefix, prefix) != 0) {
                sf_context->subfile_prefix = strdup(prefix);
            }
        } else {
            sf_context->subfile_prefix = strdup(prefix);
        }
        sf_subfile_prefix = sf_context->subfile_prefix;
    }

    /*
     * Save the HDF5 file id (fid) to subfile context mapping.
     * There shouldn't be any issue, but check the status and
     * return if there was a problem.
     */
    ret = record_fid_to_subfile(fid, sf_context->sf_context_id, NULL);
    if (ret != SUCCEED) {
        printf("[%d - %s] Error mapping hdf5 file to a subfiling context\n",
            sf_context->topology->world_rank, __func__);
        return -1;
    }

    /* We already know the number of IO concentrators, but
     * grab the mapping of IO concentrator to MPI ranks for our
     * messaging loop.
     */
    io_concentrator = sf_context->topology->io_concentrator;

    for (i = 0; i < n_io_concentrators; i++) {
        int64_t msg[3] = {flags, fid, sf_context->sf_context_id};

#ifndef NDEBUG
        if (sf_verbose_flag) {
            if (sf_logfile) {
                fprintf(sf_logfile, "[%d] file open request (flags = %0lx)\n",
                    sf_world_rank, msg[0]);
            }
        }
#endif
        /* Send the open_op message to an IOC */
        status = MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[i], OPEN_OP,
            sf_context->sf_msg_comm);

        /* Check for errors */
        if (status == MPI_SUCCESS) {
            /* And post a receive for the open file ACK */
            status = MPI_Irecv(&ioc_acks[i], 1, MPI_INT, io_concentrator[i],
                COMPLETED, sf_context->sf_data_comm, &reqs[i]);
        }

        if (status != MPI_SUCCESS) {
            printf("[%d] MPI close_subfiles failure!", sf_world_rank);
        } else
            n_waiting++;
    } /* END - for loop */

    /* Wait for all (n_waiting) ACK messages to be received */
    while (n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(
            n_io_concentrators, reqs, &ready, indices, MPI_STATUSES_IGNORE);
        if (status != MPI_SUCCESS) {
            int  len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                sf_world_rank, __func__, estring);
            fflush(stdout);
        }

        for (i = 0; i < ready; i++) {
            n_waiting--;
        }
    } /* END - while */

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client open_subfiles
 *
 * Purpose:     Wrapper for the internal 'open__subfiles' function
 *              Similar to the other public wrapper functions, we
 *              discover (via the sf_context) the number of io concentrators
 *              and pass that to the internal function so that vector
 *              storage arrays can be stack based rather than explicitly
 *              allocated and freed.
 *
 *              The Internal function is resposible for sending all IOC
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
int
sf_open_subfiles(hid_t fid, char *filename, char *prefix, int flags)
{
    int                  status;
    int64_t              context_id = -1;
    subfiling_context_t *sf_context = NULL;
    sf_ioc_selection_t   ioc_selection;
    char *option_arg = get_ioc_selection_criteria(&ioc_selection);

    status = H5FDsubfiling_init(ioc_selection, option_arg, &context_id);
    if (status != SUCCEED) {
        puts("H5FDsubfiling_init failed!");
        return -1;
    }

    sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);

    sf_context->sf_context_id = context_id;
    sf_context->h5_file_id = fid;
    sf_context->filename = strdup(filename);
    sf_shutdown_flag = 0;

    return open__subfiles(sf_context, sf_context->topology->n_io_concentrators,
        fid, prefix, flags);
}

/*-------------------------------------------------------------------------
 * Function:    Public/Client set_verbose_flag
 *
 * Purpose:     For debugging purposes, I allow a verbose setting to
 *              have printing of relevent information into an IOC specific
 *              file that is opened as a result of enabling the flag
 *              and closed when the verbose setting is disabled.
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
void
set_verbose_flag(int subfile_rank, int new_value)
{
#ifndef NDEBUG
    sf_verbose_flag = (int) (new_value & 0x0FF);

    if (sf_verbose_flag) {
        char logname[64];
        sprintf(logname, "ioc_%d.log", subfile_rank);
        sf_logfile = fopen(logname, "w+");
    } else if (sf_logfile) {
        fclose(sf_logfile);
        sf_logfile = NULL;
    }
#endif
    return;
}

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
    MPI_Status           status, msg_status;
    sf_work_request_t *  incoming_requests = NULL;
    useconds_t           delay = 20;
    subfiling_context_t *context = get_subfiling_object(context_id);

    assert(context != NULL);
    /* We can't have opened any files at this point.. */
    context->sf_fid = -1;

    subfile_rank = context->sf_group_rank;
    if (request_count_per_rank == NULL) {
        request_count_per_rank =
            (int *) calloc((size_t) sf_world_size, sizeof(int));
        assert(request_count_per_rank != NULL);
    }

    max_work_depth = MAX(8, sf_world_size * MAX_WORK_PER_RANK);
    incoming_requests = (sf_work_request_t *) calloc(
        (size_t)(max_work_depth + 1), sizeof(sf_work_request_t));

    /* Validate that the allocation succeeded */
    assert(incoming_requests != NULL);

    /* Initialize atomic vars */
    atomic_init(&sf_workinprogress, 0);
    atomic_init(&sf_work_pending, 0);
    atomic_init(&sf_file_close_count, 0);
    atomic_init(&sf_file_refcount, 0);
    atomic_init(&sf_ioc_fini_refcount, 0);

    sf_open_file_count = 0;   
    sf_close_file_count = 0;  
    sf_ops_after_first_close = 0;

    while (!sf_shutdown_flag || sf_work_pending) {
        flag = 0;
        ret = MPI_Iprobe(
            MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int                count;
            int                request_size = (int) sizeof(sf_work_request_t);
            int                source = status.MPI_SOURCE;
            int                tag = status.MPI_TAG;

            MPI_Get_count(&status, MPI_BYTE, &count);
            if (count > request_size) {
                msg = (sf_work_request_t *) malloc((size_t) count);
                ret = MPI_Recv(msg, count, MPI_BYTE, source, tag,
                    context->sf_msg_comm, &msg_status);
            } else {
                ret = MPI_Recv(&incoming_requests[sf_workinprogress], count,
                    MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);
            }
            if (ret == MPI_SUCCESS) {
                if (msg) {
                    msg->source = source;
                    msg->subfile_rank = subfile_rank;
                    msg->context_id = context->sf_context_id;
                    tpool_add_work(msg);
                } else {
                    int index = atomic_load(&sf_workinprogress);
                    incoming_requests[index].tag = tag;
                    incoming_requests[index].source = source;
                    incoming_requests[index].subfile_rank = subfile_rank;
                    tpool_add_work(&incoming_requests[index]);
                    if (index == max_work_depth - 1) {
                        atomic_init(&sf_workinprogress, 0);
                    } else {
                        atomic_fetch_add(&sf_workinprogress, 1); // atomic
                    }
                }
            }
        } else {
            usleep(delay);
        }
    }

#ifndef NDEBUG
    if (sf_logfile) {
        fclose(sf_logfile);
        sf_logfile = NULL;
    }
#endif

    if (incoming_requests) {
        free(incoming_requests);
    }

    /* Reset the shutdown flag */
    sf_shutdown_flag = 0;

    return 0;
}

/*
=========================================
Private helper functions
=========================================
*/

static int
send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int ack = 1;
    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending ACK to MPI_rank(%d)\n",
                subfile_rank, target);
        }
    }
#endif
    return ret;
}

static int
send_nack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int nack = 0;
    int ret = MPI_Send(&nack, 1, MPI_INT, target, tag, comm);

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d): Sending NACK to MPI_rank(%d)\n",
                subfile_rank, target);
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
 * Function:    Public/IOC queue_write_coll
 *
 * Purpose:     Collective write function (NOT currently implemented)
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
queue_write_coll(sf_work_request_t H5_ATTR_PARALLEL_UNUSED *msg,
    int H5_ATTR_PARALLEL_UNUSED subfile_rank,
    int H5_ATTR_PARALLEL_UNUSED source, MPI_Comm H5_ATTR_PARALLEL_UNUSED comm)
{
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_read_coll
 *
 * Purpose:     Collective read function (NOT currently implemented)
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
queue_read_coll(sf_work_request_t H5_ATTR_PARALLEL_UNUSED *msg,
    int H5_ATTR_PARALLEL_UNUSED subfile_rank,
    int H5_ATTR_PARALLEL_UNUSED source, MPI_Comm H5_ATTR_PARALLEL_UNUSED comm)
{
    return 0;
}

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
int
queue_write_indep(
    sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
    char *               recv_buffer = NULL;
    int                  ret = MPI_SUCCESS;
    MPI_Status           msg_status;
    int64_t              data_size = msg->header[0];
    int64_t              file_offset = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
    assert(sf_context != NULL);

    /* flag that we've attempted to write data to the file */
    sf_context->sf_write_count++;

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                "[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld\n",
                subfile_rank, __func__, source, data_size, file_offset);
        }
    }
#endif
    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *) malloc((size_t) data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm);

    ret = MPI_Recv(recv_buffer, (int) data_size, MPI_BYTE, source,
        WRITE_INDEP_DATA, comm, &msg_status);

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                "[ioc(%d) %s] MPI_Recv(%ld bytes, from = %d) status = %d\n",
                subfile_rank, __func__, data_size, source, ret);
        }
    }
#endif

    if (ret != MPI_SUCCESS) {
        int  len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d "
               "returned an error(%s)\n",
            subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source,
            estring);
        fflush(stdout);
        return ret;
    }

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d)] WARNING: %s called while subfile_fid = %d (closed)\n",
            subfile_rank, __func__, fd);
        fflush(stdout);
    } else if (sf_write_data(
                   fd, file_offset, recv_buffer, data_size, subfile_rank) < 0) {
        free(recv_buffer);
        recv_buffer = NULL;
        printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank,
            __func__);
        fflush(stdout);
        return -1;
    }
    /* Done... */
    if (recv_buffer) {
        free(recv_buffer);
    }
    return 0;
}

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
int
queue_read_indep(
    sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int                  fd;
    char *               send_buffer = NULL;
    int                  ret = MPI_SUCCESS;
    int64_t              data_size = msg->header[0];
    int64_t              file_offset = msg->header[1];
    int64_t              file_context_id = msg->header[2];
    subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
    assert(sf_context != NULL);

    sf_context->sf_read_count++;

    fd = sf_context->sf_fid;

    if (fd < 0) {
        printf("[ioc(%d) %s] subfile(%d) file descriptor not valid\n",
            subfile_rank, __func__, fd);
        return -1;
    }

    /* If there were writes to this file, we should flush the file cache
     * before attempting to read the contents.
     */
    if (sf_context->sf_write_count) {
        sf_context->sf_write_count = 0;
        fdatasync(fd);
    }

#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                "[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld\n",
                subfile_rank, __func__, source, data_size, file_offset);
        }
    }
#endif
    if ((send_buffer = (char *) malloc((size_t) data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(fd, file_offset, send_buffer, data_size, subfile_rank) <
        0) {
        printf("[%d] %s - sf_read_data for source(%d) returned an error! "
               "read_count=%ld\n",
            subfile_rank, __func__, source, sf_context->sf_read_count);
        fflush(stdout);
        return -1;
    }
    ret = MPI_Send(
        send_buffer, (int) data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
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
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile, "[ioc(%d)] MPI_Send to source(%d) completed\n",
                subfile_rank, source);
        }
    }
#endif

    if (send_buffer) {
        free(send_buffer);
        send_buffer = NULL;
    }

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC queue_file_open
 *
 * Purpose:     Implement the IOC file open function.  The
 *              function is invoked as a result of the IOC receiving the
 *              "header"/RPC.  What remains is open the subfile if it
 *              isn't already open.  This can happen if this function
 *              was invoked by another client process.
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
queue_file_open(
    sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    int ret, errors = 0;
    int flags = (int) (msg->header[0] & 0x0ffffffff);
    // int open_count;
    atomic_fetch_add(&sf_file_refcount, 1); // atomic
#ifndef NDEBUG
    if (sf_verbose_flag) {
        if (sf_logfile) {
            fprintf(sf_logfile,
                "[ioc(%d) %s] file open flags = %0x, source=%d\n", subfile_rank,
                __func__, flags, source);
        }
    }
#endif
    errors = subfiling_open_file(msg, sf_subfile_prefix, subfile_rank, flags);
    // open_count = atomic_load(&sf_file_refcount);

    ret = MPI_Send(&errors, 1, MPI_INT, source, COMPLETED, comm);
    if (ret != MPI_SUCCESS) {
        printf("[ioc(%d)] MPI_Send FILE_OPEN, COMPLETED to source(%d) FAILED\n",
               subfile_rank, source);
        fflush(stdout);
        errors++;
    }

    if (errors) {
#ifndef NDEBUG
        if (sf_verbose_flag) {
            if (sf_logfile) {
                fprintf(sf_logfile, "[ioc(%d) %s] Error opening file\n",
                    subfile_rank, __func__);
            }
        }
#endif
    }
    return errors;
}

/*
 *  The decrement is somewhat of misnomer, i.e. we check the number of file open
 *  requests to the number of file close requests.  When those values match, the
 *  actual file gets closed via the callback_ftn.  This effects a weak
 * collective on the file close operation.   File opens (*) on the other hand,
 * can occur in any random order and no collective semanitics are enforced.
 *
 *  (*) Note that on the original file open, there are collective operations
 * which take place to generate the MPI communications descriptors.
 */
int
decrement_file_ref_counts(sf_work_request_t *msg, int subfile_rank,
    int H5_ATTR_PARALLEL_UNUSED source, MPI_Comm comm,
    file_close_cb callback_ftn)
{
    int close_count, errors = 0;

    atomic_fetch_add(&sf_file_close_count, 1); // atomic
    close_count = atomic_load(&sf_file_close_count);

    if (close_count == sf_world_size) {
        int64_t              file_context_id = msg->header[2];
        subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
        assert(sf_context != NULL);

        atomic_store(&sf_file_refcount, 0);
        atomic_store(&sf_file_close_count, 0); /* Complete the reset to zeros */

        /* Wait until any queued work has finished */
        while (!tpool_is_empty()) {
            usleep(20);
        }

        if (callback_ftn(subfile_rank, &sf_context->sf_fid, comm) < 0) {
            printf("[ioc(%d) %s] callback_ftn returned an error\n",
                subfile_rank, __func__);
            fflush(stdout);
            errors++;
        } else {
            sf_context->sf_fid = -1; /* reset the actual file descriptor */
        }
    }
    return errors;
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC subfiling_close_file
 *
 * Purpose:     This function should be called ONLY when all clients
 *              have called the CLOSE_OP on this IO Concentrator.
 *              The IOC API maintains a reference count on subfiles
 *              so that once that count is decremented to zero, the
 *              decrement_file_ref_counts function will call here.
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
subfiling_close_file(int subfile_rank, int *fid, MPI_Comm comm)
{
    int errors = 0;
    int subfile_fid = *fid;

    if (subfile_fid >= 0) {
        if (fdatasync(subfile_fid) < 0) {
            perror("fdatasync");
            printf("fdatasync(%d)\n", subfile_fid);
            errors++;
        }
    }

    errors += subfiling_shutdown(subfile_rank, fid, comm);

    if (errors) {
        printf("[ioc(%d) %s] Errors detected!\n", subfile_rank, __func__);
        fflush(stdout);
    }

    return errors;
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC subfiling_shutdown
 *
 * Purpose:     This function gets called ONLY when all clients have
 *              invoked the file CLOSE_OP, which in turn decrements the
 *              file reference count maintained within the subfiling
 *              context. As a result, the subfiling_close_file call is
 *              invoked, forcing a file sync/flush and then calling
 *              function to close the local subfile and notify the
 *              clients with the close ACK to allow them to continue
 *              beyond the HDF5 file close function.
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
subfiling_shutdown(int subfile_rank, int *fid, MPI_Comm comm)
{
    int ret, source = 0;
    int subfile_fid = *fid;
    int errors = 0, flag = COMPLETED;
    if (subfile_fid >= 0) {
        if (close(subfile_fid) < 0) {
            perror("subfiling_close_file");
            printf("subfile_fid = %d\n", subfile_fid);
            errors++;
        }
        *fid = -1;
    }

    /* Shutdown the main IOC thread */
    sf_shutdown_flag = 1;
    /* Allow ioc_main to exit.*/
    usleep(40);

    /* Notify all ranks */
    for (source = 0; source < sf_world_size; source++) {
        /* Don't release our local MPI process until all
         * other ranks are released.
         */
        if (source == sf_world_rank) {
            continue;
        }
        ret = MPI_Send(&flag, 1, MPI_INT, source, COMPLETED, comm);
        if (ret != MPI_SUCCESS)
            errors++;
    }

    /* Release the local MPI process */
    ret = MPI_Send(&flag, 1, MPI_INT, sf_world_rank, COMPLETED, comm);
    if (ret != MPI_SUCCESS)
        errors++;

    if (errors) {
        printf("[ioc(%d) %s] Errors sending ioc_fini replies\n", subfile_rank,
            __func__);
        fflush(stdout);
    }

    return errors;
}

/*-------------------------------------------------------------------------
 * Function:    Public/IOC increment_ioc_fini_counts
 *
 * Purpose:     UNUSED.  Was originally implemented to manage the shutdown
 *              of IO Concentrators.  The subfiling design changed to
 *              create IOC instances as part of FILE opens and shutdowns
 *              as part of file closing.
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
increment_ioc_fini_counts(sf_work_request_t *msg, int subfile_rank,
    int H5_ATTR_PARALLEL_UNUSED source, MPI_Comm comm,
    file_close_cb callback_ftn)
{
    int close_count, errors = 0;
    atomic_fetch_add(&sf_ioc_fini_refcount, 1); // atomic
    close_count = atomic_load(&sf_ioc_fini_refcount);

    if (close_count == sf_world_size) {
        int64_t              file_context_id = msg->header[2];
        subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
        assert(sf_context != NULL);
        if (callback_ftn(subfile_rank, &sf_context->sf_fid, comm) < 0) {
            printf("[ioc(%d) %s] callback_ftn returned an error\n",
                subfile_rank, __func__);
            fflush(stdout);
        }
    }
    return errors;
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
 *              transmited as part of the RPC since it is available as
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
subfiling_open_file(
    sf_work_request_t *msg, const char *prefix, int subfile_rank, int flags)
{
    int errors = 0;

    /* Only the real IOCs open the subfiles
     * Once a file is opened, all additional file open requests
     * can return immediately.
     */
    if (subfile_rank >= 0) {
        char                 filepath[PATH_MAX];
        char                 config[PATH_MAX];
        int                  subfile_fid;
        int64_t              h5_file_id = msg->header[1];
        int64_t              file_context_id = msg->header[2];
        subfiling_context_t *sf_context = get_subfiling_object(file_context_id);
        assert(sf_context != NULL);

        begin_thread_exclusive();

        if (sf_context->sf_fid < 0) {
            int  n_io_concentrators = sf_context->topology->n_io_concentrators;
            int *io_concentrator = sf_context->topology->io_concentrator;
            const char *dotconfig = ".subfile_config";
            mode_t      mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
            if (prefix) {
                mkdir(prefix, S_IRWXU);
                sprintf(filepath, "%s/%ld_node_local_temp_%d_of_%d", prefix,
                    h5_file_id, subfile_rank, n_io_concentrators);
                sprintf(config, "%s/%ld%s", prefix, h5_file_id, dotconfig);
            } else {
                sprintf(filepath, "%ld_node_local_temp_%d_of_%d", h5_file_id,
                    subfile_rank, n_io_concentrators);
                strcpy(config, dotconfig);
            }

            if ((subfile_fid = open(filepath, flags, mode)) < 0) {
                end_thread_exclusive();
                errors++;
                goto done;
            } else {
                sf_context->sf_fid = subfile_fid;
            }

            if (flags & O_CREAT) {
                int64_t              new_context = SF_CONTEXT;
                int64_t              objtype = (new_context << 32);
                int                  context_id = (int) msg->context_id;
                size_t               bufsize = PATH_MAX + 16;
                FILE *               f = NULL;
                char                 linebuf[bufsize];
                int64_t              thisId = (int64_t)(objtype | context_id);
                subfiling_context_t *context =
                    (subfiling_context_t *) get_subfiling_object(thisId);
                /* If a config file already exists, AND
                 * the user wants to truncate subfiles (if they exist),
                 * then we should also truncate an existing config file.
                 */
                if (access(config, flags) == 0) {
                    truncate(config, 0);
                }
                f = fopen(config, "w+");
                if (f != NULL) {
                    int   k;
                    sprintf(
                        linebuf, "stripe_size=%ld\n", context->sf_stripe_size);
                    fwrite(linebuf, strlen(linebuf), 1, f);
                    sprintf(
                        linebuf, "aggregator_count=%d\n", n_io_concentrators);
                    fwrite(linebuf, strlen(linebuf), 1, f);
                    sprintf(linebuf,"hdf5_file=%s\n", context->filename);
                    fwrite(linebuf, strlen(linebuf), 1, f);

                    for (k = 0; k < n_io_concentrators; k++) {
                        if (prefix)
                            sprintf(linebuf, "%s/%ld_node_local_temp_%d_of_%d:%d", prefix,
                            h5_file_id, subfile_rank, n_io_concentrators, io_concentrator[k]);
                        else
                            sprintf(linebuf, "%ld_node_local_temp_%d_of_%d:%d", h5_file_id,
                            subfile_rank, n_io_concentrators, io_concentrator[k]);

                        fwrite(linebuf, strlen(linebuf), 1, f);
                    }

                    fclose(f);
                } else {
                    perror("fopen(config)");
                    errors++;
                    goto done;
                }
            }
#ifndef NDEBUG
            if (sf_verbose_flag) {
                if (sf_logfile) {
                    fprintf(sf_logfile, "[ioc:%d] Opened subfile %s\n",
                        subfile_rank, filepath);
                }
            }
#endif
        }
        end_thread_exclusive();
    }
done:
    return errors;
}

/*-------------------------------------------------------------------------
 * Function:    UTILITY FUNCTIONS:
 *              delete_subfiling_context - removes a context entry in the
 *                                         object cache.  Free communicators
 *                                         and zero other structure fields.
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

void
delete_subfiling_context(hid_t context_id)
{
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    if (sf_context) {
        if (sf_context->topology->n_io_concentrators > 1) {
            if (sf_context->sf_group_comm != MPI_COMM_NULL) {
                MPI_Comm_free(&sf_context->sf_group_comm);
            }
            if (sf_context->sf_intercomm != MPI_COMM_NULL) {
                MPI_Comm_free(&sf_context->sf_intercomm);
            }
        }
        free(sf_context);
    }

    return;
}

int
sf_get_mpi_rank(hid_t fid, int *rank)
{
    hid_t                context_id = fid_map_to_context(fid);
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(rank != NULL);
    *rank = sf_context->sf_group_rank;
    return 0;
}

int
sf_get_mpi_size(hid_t fid, int *size)
{
    hid_t                context_id = fid_map_to_context(fid);
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(size != NULL);
    *size = sf_context->sf_group_size;
    return 0;
}

int
sf_get_group_comm(hid_t fid, MPI_Comm *comm)
{
    hid_t                context_id = fid_map_to_context(fid);
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    assert(sf_context != NULL);
    assert(comm != NULL);
    *comm = sf_context->sf_group_comm;
    return 0;
}

int
sf_subfile_set_logging(hid_t sf_fid, int ioc_rank, int flag)
{
    int                  ioc;
    int                  status = 0;
    hid_t                context_id = fid_map_to_context(sf_fid);
    subfiling_context_t *sf_context = get_subfiling_object(context_id);
    int                  n_io_concentrators;
    int *                io_concentrator = NULL;
    int64_t              lflag = (int64_t)(flag & 0xFF);
    int64_t              msg[3];

    assert(sf_context != NULL);

    msg[0] = lflag;
    msg[1] = 0;
    msg[2] = sf_context->sf_context_id;

    n_io_concentrators = sf_context->topology->n_io_concentrators;
    io_concentrator = sf_context->topology->io_concentrator;

    for (ioc = 0; ioc < n_io_concentrators; ioc++) {
        if ((flag < 0) || (flag == ioc_rank)) {
            status = MPI_Ssend(msg, 3, MPI_INT64_T, io_concentrator[ioc],
                LOGGING_OP, sf_context->sf_msg_comm);
        }
    }
    return status;
}
