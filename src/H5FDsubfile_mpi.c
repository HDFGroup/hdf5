
#include "H5FDsubfile_private.h"

static int *io_concentrator = NULL;
static int n_io_concentrators = -1;
static int sf_world_rank = -1;
static int sf_world_size = -1;
static int subfile_fid = -1;
static int64_t sf_stripe_size = -1;
static int64_t sf_blocksize_per_stripe = 0;

static MPI_Datatype H5FD__create_f_l_mpi_type(subfiling_context_t *context,
											  int64_t target_write_bytes,
											  int64_t first_write,
											  int64_t last_write,
											  int ioc_depth);
static MPI_Datatype H5FD__create_first_mpi_type(subfiling_context_t *context,
												int64_t offset,
												int64_t target_write_bytes,
												int64_t first_write,
												int ioc_depth);
static MPI_Datatype H5FD__create_final_mpi_type(subfiling_context_t *context,
												int64_t target_write_bytes,
												int64_t last_write,
												int ioc_depth);
static MPI_Datatype H5FD__create_mpi_uniform_type(subfiling_context_t *context,
												  int64_t offset,
												  int64_t target_write_bytes,
												  int ioc_depth);

static int * request_count_per_rank = NULL;

atomic_int sf_workinprogress = 0;
atomic_int sf_work_pending = 0;
atomic_int sf_file_close_count = 0;
atomic_int sf_file_refcount = 0;

#ifdef DEBUG_TRACING
FILE *sf_logfile = NULL;
#endif

MPI_Comm sf_msg_comm = MPI_COMM_NULL;	/* Messages IN */
MPI_Comm sf_data_comm = MPI_COMM_NULL;	/* Messages OUT */

int sf_shutdown_flag = 0;

const char *sf_subfile_prefix = ".";


#define MAX_WORK_PER_RANK 2

/*
=========================================
Private functions
=========================================
*/

static int _determine_subfile_rank(int myrank)
{
    if (io_concentrator) {
        int i;
        for(i=0; i< n_io_concentrators; i++) {
            if (io_concentrator[i] == myrank)
                return i;
        }
    }
    return -1;
}

static int is_io_concentrator(int rank)
{
    int index = _determine_subfile_rank(rank);
    if (index < 0) return 0;
    return 1;  /* true */
}



static void init_io_vars(int64_t stripe_size, int64_t blocksize_per_stripe,
						 int64_t file_offset, int64_t data_extent,
						 int64_t *first_io, int64_t *first_io_offset, int64_t *last_io,
						 int *starting_ioc, int *final_ioc, int *starting_row, int *final_row)
{
    int64_t total_stripe_width = stripe_size * n_io_concentrators;
    int64_t starting_offset = file_offset % stripe_size;
    int64_t final_offset = (file_offset + data_extent -1);
    int64_t last_io_check = (starting_offset + data_extent) % stripe_size;
    *starting_row = (int)(file_offset / total_stripe_width);
    *final_row = (int)(final_offset / total_stripe_width);

    /* Maybe update how many bytes in the entire IOC collection */
    if (blocksize_per_stripe == 0)
        sf_blocksize_per_stripe = total_stripe_width;

    *starting_ioc = (int)((file_offset / stripe_size) % n_io_concentrators);
    *final_ioc = (int)((final_offset / stripe_size) % n_io_concentrators);
    *first_io_offset = starting_offset;
    *first_io = ((stripe_size - starting_offset) >= data_extent ? data_extent : (stripe_size - starting_offset));
	/* Check for just a single IO op */
	if (*first_io == data_extent) *last_io = 0;
	else *last_io = (last_io_check > 0 ? last_io_check : stripe_size);
}

static int init__indep_io(subfiling_context_t *sf_context,
						  int64_t **source_data_offset, int64_t **sf_datasize,
						  int64_t **sf_offset, MPI_Datatype **sf_dtype,
						  int64_t offset, int64_t elements, int dtype_extent)
{
    int64_t data_extent = elements * dtype_extent;
    int64_t first_io=0, last_io=0, first_io_offset=0;

	int64_t *data_offset = *source_data_offset;
	int64_t *ioc_datasize = *sf_datasize;
	int64_t *ioc_offset = *sf_offset;
    MPI_Datatype *ioc_type = *sf_dtype;
    int k, ioc_start, ioc_last, ioc_depth, starting_row, final_row;
	sf_stripe_size = sf_context->sf_stripe_size;
	sf_blocksize_per_stripe = sf_context->sf_blocksize_per_stripe;

    init_io_vars(sf_stripe_size, sf_blocksize_per_stripe, offset, data_extent,
				 &first_io, &first_io_offset, &last_io,
                 &ioc_start, &ioc_last, &starting_row, &final_row);

    if (sf_verbose_flag) {
        printf("[%d] offset=%ld,data_extent=%ld,sf_stripe_size=%ld,n_io_concentrators=%d,"
               "first_io=%ld,first_io_offset=%ld,last_io=%ld,ioc_start=%d,ioc_last=%d\n",
               sf_world_rank, offset,data_extent,sf_stripe_size,n_io_concentrators,
               first_io,first_io_offset,last_io,ioc_start,ioc_last);
        fflush(stdout);
    }

    if (data_offset == NULL) {
        data_offset = (int64_t *)calloc((size_t)n_io_concentrators, sizeof(int64_t));
		assert(data_offset != NULL);
		*source_data_offset = data_offset;
    }

	if (ioc_datasize == NULL) {
		ioc_datasize = (int64_t *)calloc((size_t)n_io_concentrators, sizeof(int64_t));
		assert(ioc_datasize != NULL);
		*sf_datasize = ioc_datasize;
	}

	if (ioc_offset == NULL) {
		ioc_offset = (int64_t *)calloc((size_t)n_io_concentrators, sizeof(int64_t));
		assert(ioc_offset != NULL);
		*sf_offset = ioc_offset;
	}

	if (ioc_type == NULL) {
		ioc_type = (MPI_Datatype *)calloc((size_t)n_io_concentrators, sizeof(MPI_Datatype));
		assert(ioc_type != NULL);
		*sf_dtype = ioc_type;
    }

    for(k=0; k < n_io_concentrators; k++) {
        ioc_datasize[k] = 0;
        ioc_offset[k] = 0;
        /* Free previously used datatypes */
        if (ioc_type[k] &&
            (ioc_type[k] != MPI_DATATYPE_NULL) &&
            (ioc_type[k] != MPI_BYTE))
            MPI_Type_free(&ioc_type[k]);
        else ioc_type[k] = MPI_DATATYPE_NULL;
    }

    if (data_extent) {
        int next_index = ioc_start;
        int64_t target_bytes;
        int64_t total_bytes_remaining = data_extent;
        int64_t row_base = starting_row * sf_stripe_size;
        int64_t subfile_offset = row_base + first_io_offset;
        int64_t source_offset = 0;
        int64_t remaining_bytes_in_row = ((n_io_concentrators - ioc_start) * sf_stripe_size) - first_io_offset;

        ioc_depth = (final_row - starting_row) +1;
        if ((ioc_start > ioc_last) && (data_extent > remaining_bytes_in_row)) ioc_depth--;
    
        while(total_bytes_remaining > 0) {
            target_bytes = 0;
            if (next_index == ioc_start) {
                target_bytes = first_io;
            }
            if (next_index == ioc_last) {
                target_bytes += last_io;
                ioc_depth--;
            }
            if (ioc_depth) { 
                if (next_index == ioc_start)
                    target_bytes += (sf_stripe_size * (ioc_depth -1));
                else target_bytes += (sf_stripe_size * ioc_depth);
            }

            data_offset[next_index] = source_offset;
            ioc_datasize[next_index] += target_bytes;
            ioc_offset[next_index] += subfile_offset;
            total_bytes_remaining -= target_bytes;
            /* 
             * With the exception of the very 1st IO, all additional
             * IO operations start on a slice_boundary (and this is
             * consistent across the collection of IOCs).
             */

            subfile_offset = row_base;

            /* 
             * Possibly Create an MPI datatype for each MPI_Send operation.
             * If the length allows writing into a single stripe on 
             * a single IOC, then we can use the MPI_BYTE datatype.
             */


            if (next_index == ioc_start) { /* First target */
                if (next_index == ioc_last) {
                    ioc_type[next_index] =
                        H5FD__create_f_l_mpi_type(sf_context, target_bytes,
									first_io, last_io, ioc_depth+1);
                } else {
					ioc_type[next_index] =
                        H5FD__create_first_mpi_type(sf_context, ioc_offset[next_index],
									target_bytes, first_io, ioc_depth);
				}
                source_offset += first_io;
            }
            else {
                if (next_index == ioc_last) {
                    ioc_type[next_index] =
                        H5FD__create_final_mpi_type(sf_context,
                                    target_bytes, last_io, ioc_depth+1);
				}  else {
                    ioc_type[next_index] =
                        H5FD__create_mpi_uniform_type(sf_context,ioc_offset[next_index],
									target_bytes, ioc_depth);
				}
                source_offset += sf_stripe_size;        
            }

            if (++next_index == n_io_concentrators) {
                next_index = 0;
                row_base += sf_stripe_size;
                subfile_offset = row_base;
            }
        }
    }
    return 0;
}


static int compare_hostid(const void *h1, const void *h2)
{
    const layout_t *host1 = (const layout_t *)h1;
    const layout_t *host2 = (const layout_t *)h2;
    return (host1->hostid > host2->hostid);
}


static void gather_topology_info(sf_topology_t *info)
{
	sf_world_size = info->world_size;
	sf_world_rank = info->world_rank;

	if (info->topology)
		return;

    if (sf_world_size > 1) {
		long hostid = gethostid();
        layout_t my_hostinfo;
		layout_t *topology = (layout_t *)calloc((size_t)sf_world_size+1, sizeof(layout_t)); 
        if (topology == NULL) {
            perror("calloc failure!");
            MPI_Abort(MPI_COMM_WORLD, 1);
        }
		info->hostid = hostid;
		info->topology = topology;
		my_hostinfo.rank = sf_world_rank;
		my_hostinfo.hostid = hostid;
        info->topology[sf_world_rank] = my_hostinfo;
        if (MPI_Allgather(&my_hostinfo, 2, MPI_LONG,
						  info->topology, 2, MPI_LONG,
						  MPI_COMM_WORLD) == MPI_SUCCESS) {
			qsort(info->topology, (size_t)sf_world_size, sizeof(layout_t), compare_hostid);
		}
    }
}

static int count_nodes(sf_topology_t *info)
{
    int k, node_count, hostid_index = -1;
    long nextid;

	assert(info != NULL);
	if (info->topology == NULL)
		gather_topology_info (info);

	nextid = info->topology[0].hostid;
    info->node_ranks = (int *)calloc((size_t)(info->world_size+1), sizeof(int));
    if (nextid == info->hostid)
		hostid_index = 0;

    node_count = 1;
	/* Recall that the topology array has been sorted! */
    for (k=1; k < info->world_size; k++) {
        if (info->topology[k].hostid != nextid) {
            nextid = info->topology[k].hostid;
            if (hostid_index < 0) {
                if (nextid == info->hostid) hostid_index = k;
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

int
H5FD__determine_ioc_count(int world_size, int world_rank, sf_topology_t **thisapp)
{
    static int ioc_count = 0;
    if (!ioc_count) {
		int k, node;
		int node_index;
		int iocs_per_node = 1;
		char *envValue = NULL;
		sf_topology_t *app_topology = (sf_topology_t *)calloc(1, sizeof(sf_topology_t));
		assert(app_topology != NULL);
		app_topology->world_size = world_size;
		app_topology->world_rank = world_rank;

		io_concentrator = (int *)calloc((size_t)world_size, sizeof(int));
		assert(io_concentrator != NULL);
        ioc_count = count_nodes (app_topology);
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
        app_topology->local_peers = app_topology->node_ranks[node_index+1] -
			app_topology->node_ranks[node_index];
        if (app_topology->topology[node_index].rank == world_rank) {
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
			for(k=1; k< iocs_per_node; k++) {
				if (app_topology->topology[node_index + k].rank == world_rank) {
					app_topology->rank_is_ioc = true;
					app_topology->subfile_rank = node_index + k;
					break;
				}
			}
		}
		/* More hacks for testing */
        if (io_concentrator) {
			int n_iocs = 0;
            for(node = 0; node < ioc_count; node++) {
				for (k=0; k < iocs_per_node; k++) {
					node_index = app_topology->node_ranks[node];
					io_concentrator[n_iocs++] = (int)(
						app_topology->topology[node_index + k].rank);
				}
			}
            ioc_count = n_io_concentrators = n_iocs;
        }

		if (ioc_count > 0) {
			*thisapp = app_topology;
		}
    }
    return ioc_count;
}

int
H5FD__init_subfile_context(subfiling_context_t **newContext, int n_iocs, int world_size, int world_rank, bool rank_is_ioc)
{
	int status;
	subfiling_context_t *next = (subfiling_context_t *)
		malloc(sizeof(subfiling_context_t));
	if (io_concentrator == NULL) {
		goto err_exit;
	}
	if (next == NULL) {
		goto err_exit;
	}
	else {
		int k;
		char *envValue = NULL;
		int ioc_leader = io_concentrator[0];
		int app_leader = 0;
		*newContext = next;
		next->sf_stripe_size = DEFAULT_STRIPE_SIZE;
		if ((envValue = getenv("IOC_STRIPE_SIZE")) != NULL) {
			long value_check = atol(envValue);
			if (value_check > 0) {
				next->sf_stripe_size = (int64_t)value_check;
			}
		}
		if ((envValue = getenv("SUBFILE_PREFIX")) != NULL) {
			char temp[PATH_MAX];
			sprintf(temp,"%s", envValue);
			next->subfile_prefix = strdup(temp);
			sf_subfile_prefix = strdup(temp);
		}

		next->sf_blocksize_per_stripe = next->sf_stripe_size * n_iocs;
		status = MPI_Comm_dup(MPI_COMM_WORLD, &next->sf_msg_comm);
		if (status != MPI_SUCCESS) goto err_exit;
		status = MPI_Comm_set_errhandler(next->sf_msg_comm, MPI_ERRORS_RETURN);
		if (status != MPI_SUCCESS) goto err_exit;
		status = MPI_Comm_dup(MPI_COMM_WORLD, &next->sf_data_comm);
		if (status != MPI_SUCCESS) goto err_exit;
		status = MPI_Comm_set_errhandler(next->sf_data_comm, MPI_ERRORS_RETURN);
		if (status != MPI_SUCCESS) goto err_exit;

		k = 0;
		while(is_io_concentrator(k))
			k++;
		app_leader = k;

		if (sf_verbose_flag && (world_rank == 0)) {
			printf("app_leader = %d and ioc_leader = %d\n", app_leader, ioc_leader);
		}

		if (n_iocs > 1) {
			status = MPI_Comm_split(MPI_COMM_WORLD, rank_is_ioc, world_rank, &next->sf_group_comm);
			if (status != MPI_SUCCESS) goto err_exit;
			status = MPI_Comm_size(next->sf_group_comm, &next->sf_group_size);
			if (status != MPI_SUCCESS) goto err_exit;
			status = MPI_Comm_rank(next->sf_group_comm, &next->sf_group_rank);
			if (status != MPI_SUCCESS) goto err_exit;
			/* 
			 * There may be additional functionality we need for the IOCs...
			 * If so, then can probably initialize those things here!
			 */
		}

		if (rank_is_ioc) {
			status = initialize_ioc_threads(next);
			if (status) goto err_exit;
		}
	}
	return 0;

err_exit:
	return -1;
}


/* 
---------------------------------------------------------------------------------
  The data that we're sending to receiving from an IO concentrator (IOC) contains
  the initial collection of bytes.  The length of this initial segment is 'first_write'.
  Note that the terminology isn't significant. We are describing an IO operation in
  terms of an MPI datatype which will either gather data from a source buffer 
  to send to an IOC or will be used to unpack data from an IOC into a user buffer.
  Subsequent IO operations which are related to the current File IO will begin on
  sf_stripe_size boundaries.
---------------------------------------------------------------------------------
*/

static MPI_Datatype H5FD__create_first_mpi_type(
	subfiling_context_t *context, int64_t offset,
	int64_t target_write_bytes, int64_t first_write, int ioc_depth)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
	int64_t stripe_size = context->sf_stripe_size;
    int64_t offset_in_stripe = offset % sf_stripe_size;
    int64_t depth_in_bytes = sf_stripe_size * ioc_depth;
    int64_t next_offset = context->sf_blocksize_per_stripe - offset_in_stripe;
	int64_t total_bytes = first_write;

    assert(ioc_depth > 0);
    if (stripe_size >= depth_in_bytes)
        return MPI_BYTE;

    if (depth_in_bytes) {
        int k;
        int temp_blocks[64];
        int temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *)calloc((size_t)ioc_depth, sizeof(int));
            disps  = (int *)calloc((size_t)ioc_depth, sizeof(int));
        } 
        blocks[0] = (int)first_write;
        disps[0] = (int) 0;
        for(k=1; k < ioc_depth; k++) {
            disps[k] = (int)next_offset;
            blocks[k] = (int)stripe_size;
			total_bytes += stripe_size;
            next_offset += context->sf_blocksize_per_stripe;
        }
		if (total_bytes != target_write_bytes) {
			printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
				   __func__, total_bytes, target_write_bytes);
		}
		
        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &newType) != MPI_SUCCESS) {
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

/* 
---------------------------------------------------------------------------------
  The data that we're sending to an IO concentrator (IOC) contains the final
  collection of bytes. Other than that detail, this is pretty much like the
  typical' case... All chunks sizes are the identical (execpt for the very
  last chunk) and all will start at relative stripe offset of 0. More precisely,
  the start offset is a multiple of the subfiling "stripe_size". 
  We can utilize MPI_Type_indexed to represent the new type.
---------------------------------------------------------------------------------
*/
static MPI_Datatype H5FD__create_final_mpi_type(subfiling_context_t *context, int64_t target_write_bytes, int64_t last_write, int ioc_depth)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
	int64_t stripe_size = context->sf_stripe_size;
    int64_t depth_in_bytes = (stripe_size * ioc_depth) + last_write;
	int64_t total_bytes = last_write;
    
    assert(ioc_depth > 0);

    if (depth_in_bytes <= stripe_size)
        return MPI_BYTE;

    if (depth_in_bytes) {
        int k;
        int temp_blocks[64];
        int temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *)calloc((size_t)ioc_depth, sizeof(int));
            disps  = (int *)calloc((size_t)ioc_depth, sizeof(int));
        } 
		
        for(k=0; k < ioc_depth; k++) {
            disps[k] = (int)(k * context->sf_blocksize_per_stripe);
            blocks[k] = (int)stripe_size;
			total_bytes += stripe_size;
        }
        blocks[k-1] = (int)last_write;
		if (total_bytes != target_write_bytes) {
			printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
				   __func__, total_bytes, target_write_bytes);
		}

        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &newType) != MPI_SUCCESS) {
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

/* 
---------------------------------------------------------------------------------
   Special case where the current IOC has both the first and final write chunks.
   This implmentation is a merge of the first_mpi_type and final_mpi_type
   functions. 
---------------------------------------------------------------------------------
*/
static MPI_Datatype H5FD__create_f_l_mpi_type(subfiling_context_t *context,
						  int64_t target_write_bytes,
						  int64_t first_write,
						  int64_t last_write, int ioc_depth)
{
    MPI_Datatype newType = MPI_DATATYPE_NULL;
	int64_t stripe_size = context->sf_stripe_size;
    int64_t depth_in_bytes = stripe_size * ioc_depth;
    int64_t offset_in_stripe = stripe_size - first_write;
    int64_t next_offset = context->sf_blocksize_per_stripe - offset_in_stripe;
	int64_t total_bytes = first_write + last_write;

    assert(ioc_depth > 0);
	if (last_write == 0) {
		newType = MPI_BYTE;
	}
	else if (depth_in_bytes) {
        int k;
        int temp_blocks[64];
        int temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *)calloc((size_t)ioc_depth, sizeof(int));
            disps  = (int *)calloc((size_t)ioc_depth, sizeof(int));
        } 
        blocks[0] = (int)first_write;
        disps[0] = 0;
        for(k=1; k < ioc_depth; k++) {
			total_bytes += stripe_size;
            blocks[k] = (int)stripe_size;
            disps[k] = (int)next_offset;
            next_offset += context->sf_blocksize_per_stripe;
        }
        blocks[k-1] = (int)last_write;

		if (total_bytes != target_write_bytes) {
			printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
				   __func__, total_bytes, target_write_bytes);
		}

        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &newType) != MPI_SUCCESS) {
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

/*
---------------------------------------------------------------------------------
 This is the 'typical' case in which the IOC has neither the first chunck nor 
 the last. All chunks sizes are the identical and start at offset = 0.
 We utilize MPI_Type_indexed to represent the new type.
---------------------------------------------------------------------------------
*/
MPI_Datatype H5FD__create_mpi_uniform_type(subfiling_context_t *context,
				   int64_t offset,
				   int64_t target_write_bytes, int ioc_depth)
{
    /* Maintain some state between function calls allow reuse of the new datatypes... */
    static MPI_Datatype uniformType = MPI_DATATYPE_NULL;
    static int64_t depth_in_bytes = 0;
    
    MPI_Datatype newType = MPI_DATATYPE_NULL;
	int64_t stripe_size = context->sf_stripe_size;
    int64_t offset_in_stripe = offset % stripe_size;
    int64_t check_depth = stripe_size * ioc_depth;
	int64_t total_bytes = 0;

    assert(offset_in_stripe == 0);
    assert(ioc_depth > 0);

    if (check_depth == stripe_size)
        return MPI_BYTE;

    if (depth_in_bytes) {
        if (depth_in_bytes != check_depth) {
            MPI_Type_free(&uniformType);
            depth_in_bytes = 0;
        }
    }    
    if (!depth_in_bytes) {
        int k;
        int temp_blocks[64];
        int temp_disps[64];
        int *blocks = temp_blocks;
        int *disps = temp_disps;
        if (ioc_depth > 64) {
            blocks = (int *)calloc((size_t)ioc_depth, sizeof(int));
            disps  = (int *)calloc((size_t)ioc_depth, sizeof(int));
        } 
        for(k=0; k < ioc_depth; k++) {
            disps[k] = (int)(k * context->sf_blocksize_per_stripe);
            blocks[k] = (int)(stripe_size);
			total_bytes += stripe_size;	
        }

		if (total_bytes != target_write_bytes) {
			printf("Warning (%s): total_SUM(%ld) != target_bytes(%ld)\n",
				   __func__, total_bytes, target_write_bytes);
		}
		
        if (MPI_Type_indexed(ioc_depth, blocks, disps, MPI_BYTE, &uniformType) != MPI_SUCCESS) {
            perror("MPI_Type_indexed failed!");
            return MPI_DATATYPE_NULL;
        }
        MPI_Type_commit(&uniformType);
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
        depth_in_bytes = check_depth;
    }
    MPI_Type_dup(uniformType, &newType);
    return newType;
}


int sf_read_independent(hid_t context_id, int64_t offset, int64_t elements, int dtype_extent, void *data)
{
    static int *acks = NULL;
    static int *indices = NULL;
    static MPI_Request *ackreqs = NULL;
    static MPI_Request *reqs = NULL;
    static MPI_Status *stats = NULL;
	static int64_t *source_data_offset = NULL;
	static int64_t *ioc_read_datasize = NULL;
	static int64_t *ioc_read_offset = NULL;
	static MPI_Datatype *ioc_read_type = NULL;

	subfiling_context_t *sf_context = get_subfiling_object(context_id);
    int i, ioc, n_waiting = 0, status = 0;

	assert(sf_context != NULL);
	
    if (acks == NULL) {
        if ((acks = (int *)calloc((size_t)n_io_concentrators*2, sizeof(int))) == NULL) {
            perror("calloc");
            return -1;
        }
        else indices = &acks[n_io_concentrators];
    }
    if (reqs == NULL) {
        if ((reqs = (MPI_Request *)calloc((size_t)n_io_concentrators, sizeof(MPI_Request))) == NULL) {
            perror("calloc");
            return -1;
        }
    }
    if (ackreqs == NULL) {
        if ((ackreqs = (MPI_Request *)calloc((size_t)n_io_concentrators, sizeof(MPI_Request))) == NULL) {
            perror("calloc");
            return -1;
        }
    }
    if (stats == NULL) {
        if ((stats = (MPI_Status *)calloc((size_t)n_io_concentrators, sizeof(MPI_Status))) == NULL) {
            perror("calloc");
            return -1;
        }
    }

    if (init__indep_io(sf_context, &source_data_offset, &ioc_read_datasize, &ioc_read_offset,
					   &ioc_read_type, offset, elements, dtype_extent) < 0) {
        return -1;
	}

	if (sf_verbose_flag) {
		for(ioc=0; ioc < n_io_concentrators; ioc++) {
			int64_t sourceOffset = source_data_offset[ioc];
			printf("[%d %s]: read_source[ioc(%d), sourceOffset=%ld, datasize=%ld, foffset=%ld]\n",
				   sf_world_rank, __func__, ioc, sourceOffset, ioc_read_datasize[ioc], ioc_read_offset[ioc] );
		}
	}

    /* Prepare the IOCs with a message which indicates the length
     * and file offset for the actual data to be provided.  
     */
    for(ioc=0; ioc < n_io_concentrators; ioc++) {
        int64_t msg[2] = {ioc_read_datasize[ioc], ioc_read_offset[ioc]};
        char *sourceData = (char *)data;
        int64_t sourceOffset = source_data_offset[ioc];

        /* We may not require data from this IOC...
         * or we may read the data directly from the file!
         * Check the size to verify!
         */
        reqs[ioc] = MPI_REQUEST_NULL;
        if (ioc_read_datasize[ioc] == 0) {
            continue;
        }

        if (sf_verbose_flag ) {
            printf("[%d %s] Requesting %ld read bytes from IOC(%d): sourceOffset=%ld\n",
                   sf_world_rank, __func__, msg[0], io_concentrator[ioc], sourceOffset );
        }

        status = MPI_Ssend(msg, 2, MPI_INT64_T, io_concentrator[ioc], READ_INDEP, sf_context->sf_msg_comm);
        if (status != MPI_SUCCESS) {
            printf("[%d] MPI_Send failure!", sf_world_rank);
            return status;
        }
        else {
            if (ioc_read_type[ioc] == MPI_BYTE) {
                int bytes = (int) ioc_read_datasize[ioc];
                status = MPI_Irecv(&sourceData[sourceOffset], bytes, ioc_read_type[ioc], io_concentrator[ioc],
                                   READ_INDEP_DATA, sf_context->sf_data_comm, &reqs[ioc]);
            } else {
                status = MPI_Irecv(&sourceData[sourceOffset], 1, ioc_read_type[ioc], io_concentrator[ioc],
                                   READ_INDEP_DATA, sf_context->sf_data_comm, &reqs[ioc]);
            }
            if (status != MPI_SUCCESS) {
                int length = 256;
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
    while(n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                   sf_world_rank, __func__, estring );
            fflush(stdout);
        }

        for(i=0; i < ready; i++) {
            ioc = io_concentrator[indices[i]];
            if (sf_verbose_flag) {
                printf("[%d] READ bytes(%ld) of data from ioc_concentrator %d complete\n",
                       sf_world_rank, ioc_read_datasize[indices[i]] , ioc);
                fflush(stdout);
            }
            n_waiting--;        
        }
    }
    return status;
}



int sf_write_independent(hid_t context_id, int64_t offset, int64_t elements, int dtype_extent, void *data)
{
    static int *acks = NULL;
    static int *indices = NULL;
    static MPI_Request *reqs = NULL;
    static MPI_Status *stats = NULL;
	static int64_t *source_data_offset = NULL;
	static int64_t *ioc_write_datasize = NULL;
	static int64_t *ioc_write_offset = NULL;
	static MPI_Datatype *ioc_write_type = NULL;

	subfiling_context_t *sf_context = get_subfiling_object(context_id);
    int i, target, ioc, n_waiting = 0, status = 0;
    int errors = 0;
    if (acks == NULL) {
        if ((acks = (int *)calloc((size_t)n_io_concentrators*2, sizeof(int))) == NULL) {
            perror("calloc");
            return -1;
        }
        else indices = &acks[n_io_concentrators];
    }
    if (reqs == NULL) {
        if ((reqs = (MPI_Request *)calloc((size_t)n_io_concentrators, sizeof(MPI_Request))) == NULL) {
            perror("calloc");
            return -1;
        }
    }
    if (stats == NULL) {
        if ((stats = (MPI_Status *)calloc((size_t)n_io_concentrators, sizeof(MPI_Status))) == NULL) {
            perror("calloc");
            return -1;
        }
    }

    if (init__indep_io(sf_context, &source_data_offset, &ioc_write_datasize, &ioc_write_offset,
					   &ioc_write_type, offset, elements, dtype_extent) < 0) {
        return -1;
	}

	if (sf_verbose_flag) {
		for(ioc=0; ioc < n_io_concentrators; ioc++) {
			int64_t sourceOffset = source_data_offset[ioc];
			printf("[%d %s]: write_dest[ioc(%d), sourceOffset=%ld, datasize=%ld, foffset=%ld]\n",
				   sf_world_rank, __func__, ioc, sourceOffset,
				   ioc_write_datasize[ioc], ioc_write_offset[ioc] );
		}
	}

    /* Prepare the IOCs with a message which indicates the length
     * of the actual data to be written.  We also provide the file
     * offset so that when the IOC recieves the data (in whatever order)
     * they can lseek to the correct offset and write the data.
     */
    for(target=0; target < n_io_concentrators; target++) {
        int64_t sourceOffset;
        int64_t msg[2] = {0,};
        char *sourceData = (char *)data;
		ioc = (sf_world_rank + target) % n_io_concentrators;

		sourceOffset = source_data_offset[ioc];
		msg[0] = ioc_write_datasize[ioc];
		msg[1] = ioc_write_offset[ioc];
        acks[ioc] = 0;
        reqs[ioc] = MPI_REQUEST_NULL;

        if (ioc_write_datasize[ioc] == 0) {
            if (sf_verbose_flag) {
                printf("[%d %s] skipping ioc(%d) send datasize = %ld\n",
                       sf_world_rank,__func__, ioc, ioc_write_datasize[ioc]);
                fflush(stdout);
            }
            continue;
        }
        if ( sf_verbose_flag ) {
            printf("[%d] Datatype(%x) Sending to ioc(%d) %ld bytes of data with file_offset=%ld\n",
                   sf_world_rank, ioc_write_type[ioc], ioc, ioc_write_datasize[ioc], ioc_write_offset[ioc]);
            fflush(stdout);
        }

        /* Send the Message HEADER which indicates the requested IO operation
         * (via the message TAG) along with the data size and file offset.
         */

        status = MPI_Ssend(msg, 2, MPI_INT64_T, io_concentrator[ioc],
                           WRITE_INDEP, sf_context->sf_msg_comm);

        if (status != MPI_SUCCESS) {
            int len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d] ERROR! MPI_Send of %ld bytes to %d returned an error(%s)\n",
				   sf_world_rank, sizeof(msg), io_concentrator[ioc], estring );
            fflush(stdout);
        }

        status = MPI_Recv(&acks[ioc], 1, MPI_INT, io_concentrator[ioc], WRITE_INDEP_ACK,
                          sf_context->sf_data_comm, &stats[ioc]);

        if (status == MPI_SUCCESS) {
            if (sf_verbose_flag) {
                printf("[%d] received ack(%d) from ioc(%d)\n",sf_world_rank, acks[ioc], ioc);
                fflush(stdout);
            }
            if (acks[ioc] > 0) {
                if (ioc_write_type[ioc] == MPI_BYTE)  {
					int datasize = (int)(ioc_write_datasize[ioc] & INT32_MASK);
                    status = MPI_Issend(&sourceData[sourceOffset], datasize,
                                        MPI_BYTE, io_concentrator[ioc], WRITE_INDEP_DATA,
                                        sf_context->sf_data_comm,&reqs[ioc]);
                }
                else {
                    status = MPI_Issend(&sourceData[sourceOffset], 1, ioc_write_type[ioc],
                                        io_concentrator[ioc], WRITE_INDEP_DATA,
                                        sf_context->sf_data_comm,&reqs[ioc]);
                }
                /* Queued another Isend which need to be completed (below) */
                n_waiting++;
            }
        } else {
            errors++;
            puts("ACK error!");
            fflush(stdout);
        }
        if (status != MPI_SUCCESS) {
            errors++;
            printf("[%d] ERROR! Unable to Send data to ioc(%d)\n",
                   sf_world_rank, ioc);
            fflush(stdout);
        }
    }

    while(n_waiting) {
        int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, stats);
        if (status != MPI_SUCCESS) {
            int len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                   sf_world_rank, __func__, estring );
            fflush(stdout);
            errors++;
        }

        for(i=0; i < ready; i++) {
            /* One of the Issend calls has completed
             * Wait for another ACK to indicate the data as been written
             * to the subfile.
             */
            acks[indices[i]] = 0;
            n_waiting--;
        }
    }
    if (errors) return -1;
    return status;
}

int sf_close_subfiles(hid_t context_id)
{
	int i, status;
	int errors = 0;
	int n_waiting = 0;
	int indices[n_io_concentrators];
	int ioc_acks[n_io_concentrators];
	MPI_Request reqs[n_io_concentrators];
	subfiling_context_t *sf_context = get_subfiling_object(context_id);

	for (i=0; i < n_io_concentrators; i++) {
		int64_t msg[2] = {0, 0};
		status = MPI_Ssend(msg, 2, MPI_INT64_T, io_concentrator[i], CLOSE_OP, sf_context->sf_msg_comm);
		if (status == MPI_SUCCESS) {
			status = MPI_Irecv(&ioc_acks[i], 1, MPI_INT, io_concentrator[i], COMPLETED, sf_context->sf_data_comm, &reqs[i]);
		}
        if (status != MPI_SUCCESS) {
            printf("[%d] MPI close_subfiles failure!", sf_world_rank);
			errors++;
        }
		else n_waiting++;
	}
	while(n_waiting) {
		int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, MPI_STATUSES_IGNORE);
		if (status != MPI_SUCCESS) {
            int len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                   sf_world_rank, __func__, estring );
            fflush(stdout);
			errors++;
		}
        for(i=0; i < ready; i++) {
			n_waiting--;
		}
	}
	return errors;
}

int sf_open_subfiles(hid_t context_id, char *prefix, int flags)
{
	int i, status;
	int n_waiting = 0;
	int indices[n_io_concentrators];
	int ioc_acks[n_io_concentrators];
	MPI_Request reqs[n_io_concentrators];
	subfiling_context_t *sf_context = get_subfiling_object(context_id);
	
	sf_stripe_size = sf_context->sf_stripe_size;

	if ((sf_context->subfile_prefix != NULL) && (prefix != NULL)) {
		if (strcmp(sf_context->subfile_prefix, prefix) != 0) {
			sf_context->subfile_prefix = strdup(prefix);
		}
	}

	for (i=0; i < n_io_concentrators; i++) {
		int64_t msg[2] = {flags, 0};
		if (sf_verbose_flag) {
			printf("[%d] file open request (flags = %0lx)\n", sf_world_rank, msg[0]);
		}
		status = MPI_Ssend(msg, 2, MPI_INT64_T, io_concentrator[i], OPEN_OP, sf_context->sf_msg_comm);
		if (status == MPI_SUCCESS) {
			status = MPI_Irecv(&ioc_acks[i], 1, MPI_INT, io_concentrator[i], COMPLETED, sf_context->sf_data_comm, &reqs[i]);
		}
        if (status != MPI_SUCCESS) {
            printf("[%d] MPI close_subfiles failure!", sf_world_rank);
        }
		else n_waiting++;
	}
	while(n_waiting) {
		int ready = 0;
        status = MPI_Waitsome(n_io_concentrators, reqs, &ready, indices, MPI_STATUSES_IGNORE);
		if (status != MPI_SUCCESS) {
            int len;
            char estring[MPI_MAX_ERROR_STRING];
            MPI_Error_string(status, estring, &len);
            printf("[%d %s] MPI_ERROR! MPI_Waitsome returned an error(%s)\n",
                   sf_world_rank, __func__, estring );
            fflush(stdout);
		}
        for(i=0; i < ready; i++) {
			n_waiting--;
		}
	}

	return 0;
}
	
int
ioc_main(subfiling_context_t *context)
{
	int subfile_rank;
	int flag, ret;
	int max_work_depth;
	MPI_Status status, msg_status;
	sf_work_request_t *incoming_requests = NULL;
	useconds_t delay = 20;

	assert(context != NULL);
	subfile_rank = context->sf_group_rank;
	if (request_count_per_rank == NULL) {
		request_count_per_rank = (int *)calloc((size_t)sf_world_size, sizeof(int));
		assert(request_count_per_rank != NULL);
	}

	max_work_depth = sf_world_size * MAX_WORK_PER_RANK;
	incoming_requests = (sf_work_request_t *)calloc((size_t)max_work_depth, sizeof(sf_work_request_t));
	assert(incoming_requests != NULL);

#ifdef DEBUG_TRACING
	char logname[64];
	sprintf(logname,"ioc_%d.log", subfile_rank);
	sf_logfile = fopen(logname, "w+");
#endif
	/* Initialize atomic vars */
	atomic_init(&sf_workinprogress, 0);
	atomic_init(&sf_work_pending, 0);
	atomic_init(&sf_file_close_count, 0);
	atomic_init(&sf_file_refcount, 0);

	sf_msg_comm = context->sf_msg_comm; /* Messages IN */
	sf_data_comm = context->sf_data_comm; /* Messages OUT */
	
    while(!sf_shutdown_flag || sf_work_pending) {
        flag = 0;
        ret = MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, context->sf_msg_comm, &flag, &status);
        if ((ret == MPI_SUCCESS) && (flag != 0)) {
            sf_work_request_t *msg = NULL;
            int count;
			int request_size = (int)sizeof(sf_work_request_t);
            int source = status.MPI_SOURCE;
            int tag = status.MPI_TAG;

            MPI_Get_count(&status, MPI_BYTE, &count);
            if (count > request_size) {
                msg = (sf_work_request_t *) malloc((size_t)count);
                ret = MPI_Recv(msg,count,MPI_BYTE, source, tag, context->sf_msg_comm, &msg_status);
            }
            else {
                ret = MPI_Recv(&incoming_requests[sf_workinprogress],count, MPI_BYTE,
                               source, tag, context->sf_msg_comm, &msg_status);
            }
            if (ret == MPI_SUCCESS) {

#ifdef DEBUG_TRACING
                printf("[[ioc(%d) msg from %d tag=%x, datasize=%ld, foffset=%ld]]\n", subfile_rank, source, tag,
					   incoming_requests[sf_workinprogress].header[0],
					   incoming_requests[sf_workinprogress].header[1]);
                fflush(stdout);
#endif
                if (msg) {
                    msg->tag = tag;
                    msg->source = source;
					msg->subfile_rank = subfile_rank;
                    tpool_add_work(msg);
                }
                else {
                    incoming_requests[sf_workinprogress].tag = tag;
                    incoming_requests[sf_workinprogress].source = source;
                    incoming_requests[sf_workinprogress].subfile_rank = subfile_rank;
                    tpool_add_work(&incoming_requests[sf_workinprogress]);
                    atomic_fetch_add(&sf_workinprogress, 1); // atomic
                    atomic_compare_exchange_strong(&sf_workinprogress, &max_work_depth, 0);
                }
            }
        }
        else usleep(delay);
    }

#ifdef DEBUG_TRACING
	fclose(sf_logfile);
#endif

	return 0;
}

/*
=========================================
Private helper functions
=========================================
*/

static int send_ack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int ack = 1;
    int ret = MPI_Send(&ack, 1, MPI_INT, target, tag, comm);
    if (sf_verbose_flag) {
        printf("[ioc(%d): Sending ACK to MPI_rank(%d)\n", subfile_rank, target);
    }
    return ret;
}

static int send_nack__(int target, int subfile_rank, int tag, MPI_Comm comm)
{
    int nack = 0;
    int ret = MPI_Send(&nack, 1, MPI_INT, target, tag, comm);
    if (sf_verbose_flag) {
        printf("[ioc(%d): Sending NACK to MPI_rank(%d)\n", subfile_rank, target);
    }
    return ret;
}

/*
=========================================
queue_xxx functions that should be run
from the thread pool threads...
=========================================
*/

int queue_write_coll(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
	return 0;
}

int queue_read_coll(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
	return 0;
}

int queue_write_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    char *recv_buffer = NULL;
    int ret = MPI_SUCCESS;
    MPI_Status msg_status;
    int64_t data_size = msg->header[0];
    int64_t file_offset = msg->header[1];

    if (sf_verbose_flag) {
        printf("[ioc(%d) %s]: msg from %d: datasize=%ld\toffset=%ld\n", subfile_rank, __func__, source, data_size, file_offset );
        fflush(stdout);
    }
    if (recv_buffer == NULL) {
        if ((recv_buffer = (char *)malloc((size_t)data_size)) == NULL) {
            perror("malloc");
            send_nack__(source, subfile_rank, WRITE_INDEP_ACK, comm);
            return -1;
        }
    }

    send_ack__(source, subfile_rank, WRITE_INDEP_ACK, comm);

    ret = MPI_Recv(recv_buffer, (int)data_size, MPI_BYTE, source, WRITE_INDEP_DATA, comm, &msg_status );
    if (ret != MPI_SUCCESS) {
        int len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d) %s] MPI_ERROR(%d)! MPI_Recv of %ld bytes from %d returned an error(%s)\n",
               subfile_rank, __func__, msg_status.MPI_ERROR, data_size, source, estring );
        fflush(stdout);
        return ret;
    } else if(sf_verbose_flag) {
        printf("[ioc(%d) %s] MPI_Recv success. Writing %ld bytes from rank %d to disk\n",
			   subfile_rank, __func__, data_size, source);
        fflush(stdout);
    }
    if (sf_write_data(subfile_fid, file_offset, recv_buffer, data_size, subfile_rank ) < 0) {
		free(recv_buffer);
		recv_buffer = NULL;
        printf("[ioc(%d) %s] sf_write_data returned an error!\n", subfile_rank, __func__);
        fflush(stdout);
        return -1;
    }
    // send_ack__(source, WRITE_COMPLETED, tinfo->comm);
    if (recv_buffer) {
		free(recv_buffer);
	}
	return 0;
}

int queue_read_indep(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
    char *send_buffer = NULL;
    int ret = MPI_SUCCESS;
    int64_t data_size = msg->header[0];
    int64_t file_offset = msg->header[1];


    if (sf_verbose_flag) {
        printf("[ioc(%d) %s] msg from %d: datasize=%ld\toffset=%ld\n", subfile_rank, __func__, source, data_size, file_offset );
        fflush(stdout);
    }
    if ((send_buffer = (char *)malloc((size_t)data_size)) == NULL) {
        perror("malloc");
        return -1;
    }

    if (sf_read_data(subfile_fid, file_offset, send_buffer, data_size, subfile_rank) < 0) {
        printf("[%d] %s - sf_read_data returned an error!\n", subfile_rank, __func__);
        fflush(stdout);
        return -1;
    }
    ret = MPI_Send(send_buffer, (int)data_size, MPI_BYTE, source, READ_INDEP_DATA, comm);
    if (ret != MPI_SUCCESS) {
        int len;
        char estring[MPI_MAX_ERROR_STRING];
        MPI_Error_string(ret, estring, &len);
        printf("[ioc(%d)] ERROR! MPI_Send of %ld bytes to %d returned an error(%s)\n",subfile_rank, data_size, source, estring );
        fflush(stdout);
        return ret;
    }

    if (send_buffer) free(send_buffer);

	return 0;
}


int queue_file_open(sf_work_request_t *msg, int subfile_rank, int source, MPI_Comm comm)
{
	int ret, req_count, errors=0;
	int prev_count;
	int flags = (int)(msg->header[0] & 0x0ffffffff);

	atomic_fetch_add(&sf_file_refcount, 1); // atomic

	if (sf_verbose_flag) {
		printf("[ioc(%d) %s] file open flags = %0x, source=%d\n", subfile_rank, __func__, flags, source);
		fflush(stdout);
	}
	if (subfile_fid < 0) {
		errors = subfiling_open_file(sf_subfile_prefix, subfile_rank, flags);
	}

	req_count = COMPLETED;
    ret = MPI_Send(&req_count, 1, MPI_INT, source, COMPLETED, comm);
	if (ret != MPI_SUCCESS) {
		errors++;
	}
	if (errors) {
		printf("[ioc(%d) %s] Error opening file\n", subfile_rank, __func__);
		fflush(stdout);
	}
    return errors;
}

/* 
 *  The decrement is somewhat of misnomer, i.e. we check the number of file open
 *  requests to the number of file close requests.  When those values match, the
 *  actual file gets closed via the callback_ftn.  The effects a weak collective
 *  on the file close operation.   File opens on the other hand, can occur in
 *  any random order and no collective semanitics are enforced.
 */
int decrement_file_ref_counts( int subfile_rank, int source, MPI_Comm comm, file_close_cb callback_ftn)
{
	int close_count, open_count;
	atomic_fetch_add(&sf_file_close_count, 1); // atomic
	close_count = atomic_load(&sf_file_close_count);
	open_count  = atomic_load(&sf_file_refcount);

	if (close_count == open_count) {
		atomic_store(&sf_file_refcount, 0);
		atomic_store(&sf_file_close_count, 0); /* Complete the reset to zeros */
        if (callback_ftn(subfile_rank, comm) < 0) {
            printf("[ioc(%d) %s] callback_ftn returned an error\n", subfile_rank, __func__ );
            fflush(stdout);
        }
    }
    return 0;
}

/* Note: This function should be called ONLY when all clients
 * have called the CLOSE_OP on this IO Concentrator.
 * The IOC API maintains a reference count on subfiles
 * so that once that count is decremented to zero, the
 * decrement_file_ref_counts function will call here.
 */
int subfiling_close_file(int subfile_rank, MPI_Comm comm)
{
    int ret, source = 0;
    int errors = 0, flag = COMPLETED;
    if (subfile_fid >= 0) {
        close(subfile_fid);
        subfile_fid = -1;
    }
    /* Notify all ranks */
    for (source = 0; source < sf_world_size; source++) {
		/* Don't release our local MPI process until all
		 * other ranks are released.
		 */
		if (source == sf_world_rank) {
			continue;
		}
        ret = MPI_Send(&flag, 1, MPI_INT, source, COMPLETED, comm);
        if (ret != MPI_SUCCESS) errors++;
    }

	/* Release the local MPI process */
	ret = MPI_Send(&flag, 1, MPI_INT, sf_world_rank, COMPLETED, comm);
	if (ret != MPI_SUCCESS) errors++;	

    if (errors) {
        printf("[ioc(%d) %s] Errors sending file close replies\n", subfile_rank, __func__);
        fflush(stdout);
    }

    return errors;
}

int subfiling_open_file(const char *prefix, int subfile_rank, int flags)
{
    int errors = 0;
    /* Only the real IOCs open the subfiles */
    if (subfile_rank >= 0) {
        const char *dotconfig = ".subfile_config";
        mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
        char filepath[PATH_MAX];
        char config[PATH_MAX];
        if (prefix) {
            mkdir(prefix, S_IRWXU);
            sprintf(filepath, "%s/node_local_temp_%d_of_%d",
                    prefix, subfile_rank, n_io_concentrators);
            sprintf(config, "%s/%s", prefix, dotconfig);
        }
        else {
            sprintf(filepath, "node_local_temp_%d_of_%d",
                    subfile_rank,n_io_concentrators);
            strcpy(config, dotconfig);
        }

        if ((subfile_fid = open(filepath, flags, mode)) < 0) {
            perror("subfile open");
            errors++;
			goto done;
        }
        else {
            /* 
             * File open and close operations are collective
             * in intent. Apart from actual IO operations, we
             * initialize the number of references to everyone
             * so that we detect when pending IO operations are
             * have all completed before we close the actual
             * subfiles.
             */
            atomic_init((atomic_int *)&sf_workinprogress, sf_world_size);
        }

        if ((subfile_rank == 0) && (flags & O_CREAT)) {
			size_t bufsize = PATH_MAX + 16;
            FILE *f = NULL;
            char linebuf[bufsize];
            /* If a config file already exists, AND
             * the user wants to truncate subfiles (if they exist), 
             * then we should also truncate an existing config file.
             */
            if (access(config, flags) == 0) {
                truncate(config, 0);
            }
            f = fopen(config, "w+");
            if (f != NULL) {
                int k;
                char *underscore = strrchr(filepath,'_');
                *underscore=0;
                strcpy(config, filepath);
                *underscore='_';
                sprintf(linebuf,"stripe_size=%ld\n", sf_stripe_size);
                fwrite(linebuf, strlen(linebuf), 1, f);
                sprintf(linebuf,"aggregator_count=%d\n",n_io_concentrators);
                fwrite(linebuf, strlen(linebuf), 1, f);
        
                for(k=0; k < n_io_concentrators; k++) {
                    snprintf(linebuf,bufsize,"%s_%d:%d\n",config, k, io_concentrator[k]);
                    fwrite(linebuf, strlen(linebuf), 1, f);
                }

				fclose(f);
            }
			else {
				perror("fopen(config)");
				errors++;
				goto done;
			}
        }
        if (sf_verbose_flag) {
            printf("[ioc:%d] Opened subfile %s\n", subfile_rank, filepath);
        }
    }
done:
	return errors;
}

void
delete_subfiling_context(hid_t context_id)
{
	subfiling_context_t *sf_context = get_subfiling_object(context_id);
	MPI_Comm_free(&sf_context->sf_msg_comm);
	MPI_Comm_free(&sf_context->sf_data_comm);
	sf_msg_comm = MPI_COMM_NULL;
	sf_data_comm = MPI_COMM_NULL;
	if (n_io_concentrators > 1) {
		MPI_Comm_free(&sf_context->sf_group_comm);
		MPI_Comm_free(&sf_context->sf_intercomm);
	}
	
	free(sf_context);
	usleep(100);

	return;
}
