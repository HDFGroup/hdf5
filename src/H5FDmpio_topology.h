
/*
 * Programmers: Richard Zamora <rzamora@anl.gov>
 *              August 2018, (last modified: December 11th, 2018)
 *
 *              Francois Tessier <ftessier@cscs.ch>
 *              August 2018
 *
 * Purpose:     This is the topology API, which can be used to select optimal
 *              aggregator ranks for collective IO opperations.
 *
 */

/*********************/
/* Define Statements */
/*********************/

/*
 * HOWTO Information:
 *
 *    To add a new system, add a new #ifdef block below. At a minimum, you must
 *    define the NETWORK_BANDWIDTH and NETWORK_LATENCY. You should also add
 *    '#include' statements for any libraries you need to implement
 *    machine-specific code in `distance_between_ranks()` and in
 *    `distance_to_io_node()`.  You will probably also need to use/modify the
 *    `rank_to_coordinates()` function to actually calculate rank-rank hop
 *    distances.
 *
 *    For systems with PMI support (e.g. #include <pmi.h> is available), follow
 *    the THETA example for calculating rank-rank distances.
 *
 */

/* Machine-specific Defs/Includes for Theta Cray XC40 (@ALCF) */
#ifdef THETA
#include <pmi.h>
#include <lustre/lustreapi.h>     /* Not used in this version of the API */
#include <lustre/lustre_user.h>   /* Not used in this version of the API */
#define LNETS_PER_OST     7
#define MAX_IONODES       392     /* 56 OSTs * 7 LNET */
#define NETWORK_BANDWIDTH 1800000
#define NETWORK_LATENCY   30

/* Machine-specific Defs/Includes for IBM BG/Q Mira/Vesta (@ALCF) */
#elif defined (BGQ)
#include <spi/include/kernel/location.h>
#include <spi/include/kernel/process.h>
#include <spi/include/kernel/memory.h>
#include <firmware/include/personality.h>
#include <hwi/include/bqc/nd_500_dcr.h>
#include <mpix.h>
#define NETWORK_BANDWIDTH 1800000
#define NETWORK_LATENCY   30

/* Default Machine Defs */
#else
#define NETWORK_BANDWIDTH 1800000
#define NETWORK_LATENCY   30

#endif

#define TMIN(a,b) (((a)<(b))?(a):(b))
#define TMAX(a,b) (((a)>(b))?(a):(b))
#define LARGE_PENALTY 10000000.0
#define SMALL_PENALTY 10000.0 /* Note: This penalty is currently arbitrary */
#define MAX_STR 1024
#define DBGRANKS 1 // Only shows ranklist on rank==0 if DBGRANKS==0
//#define topo_debug

/*
 * MPI_CHECK_H5 will display a custom error message as well as an error string
 * from the MPI_STATUS and then exit the program. This macro is borrowed
 * directly from the HPC-IOR code
 */
#define MPI_CHECK_H5(MPI_STATUS, MSG) do {                               \
    char resultString[MPI_MAX_ERROR_STRING];                             \
    int resultLength;                                                    \
                                                                         \
    if (MPI_STATUS != MPI_SUCCESS) {                                     \
        MPI_Error_string(MPI_STATUS, resultString, &resultLength);       \
        fprintf(stdout, "ior ERROR: %s, MPI %s, (%s:%d)\n",              \
                MSG, resultString, __FILE__, __LINE__);                  \
        fflush(stdout);                                                  \
        MPI_Abort(MPI_COMM_WORLD, -1);                                   \
    }                                                                    \
} while(0)


/*********************/
/* Special Type Defs */
/*********************/


typedef struct cost cost;

struct cost {
    double cost;
    int rank;
};

/*
 * AGGSelect is used to select the desired aggregation selection routine
 * DATA    -> Try to maximize data-movement bandwidth
 * SPREAD  -> Spread out aggregators using topology information
 * STRIDED -> Spread out aggregators according a given stride (using the rank IDs)
 * RANDOM  -> Use random rank selection for aggregator placement
 */
enum AGGSelect{DEFAULT, DATA, SPREAD, STRIDED, RANDOM};


/************************/
/* Function Definitions */
/************************/


/*-------------------------------------------------------------------------
 * Function:    CountProcsPerNode
 *
 * Purpose:     Count the number of mpi procs that share a host.
 *
 * Return:      The count (int)
 *
 * NOTE: This also assumes that the task count on all nodes is equal
 * to the task count on the host running MPI task 0.
 *
 *-------------------------------------------------------------------------
 */
static int CountProcsPerNode(int numTasks, int rank, MPI_Comm comm)
{
    char localhost[MAX_STR];
    char hostname0[MAX_STR];
    static int firstPass = true;
    unsigned count;
    unsigned flag;
    int rc;

    rc = gethostname(localhost, MAX_STR);
    if (rc == -1) {
        /* This node won't match task 0's hostname...except in the
         * case where ALL gethostname() calls fail, in which
         * case ALL nodes will appear to be on the same node.
         * We'll handle that later. */
        localhost[0] = '\0';
        if (rank == 0) perror("gethostname() failed");
    }

    /* send task 0's hostname to all tasks */
    if (rank == 0) strcpy(hostname0, localhost);
    MPI_CHECK_H5(MPI_Bcast(hostname0, MAX_STR, MPI_CHAR, 0, comm), "broadcast of task 0's hostname failed");
    if (strcmp(hostname0, localhost) == 0) flag = 1;
    else flag = 0;

    /* count the tasks share the same host as task 0 */
    MPI_Allreduce(&flag, &count, 1, MPI_UNSIGNED, MPI_SUM, comm);

    if (hostname0[0] == '\0') count = 1;
    return (int)count;
}

/*-------------------------------------------------------------------------
 * Function:    network_bandwidth
 *
 * Purpose:     system-dependent (hard-coded) network bandwidth (bytes/ms)
 *
 * Return:      Bandwidth (int64_t) in bytes/ms
 *
 *-------------------------------------------------------------------------
 */
int64_t network_bandwidth () {
    return NETWORK_BANDWIDTH;
}

/*-------------------------------------------------------------------------
 * Function:    network_latency
 *
 * Purpose:     system-dependent (hard-coded) network latency (ms)
 *
 * Return:      Latency (int64_t) in milliseconds
 *
 *-------------------------------------------------------------------------
 */
int64_t network_latency () {
    return NETWORK_LATENCY;
}

/*-------------------------------------------------------------------------
 * Function:    rank_to_coordinates
 *
 * Purpose:     Given the rank, return the topology coordinates
 *
 * Return:      int* coord array pointer
 *
 *-------------------------------------------------------------------------
 */
void rank_to_coordinates ( int rank, int* coord ) {
#ifdef THETA
    pmi_mesh_coord_t xyz;
    int nid;

    /* Hypothesis : PMI_rank == MPI_rank */
    PMI_Get_nid(rank, &nid);
    PMI_Get_meshcoord((pmi_nid_t) nid, &xyz);

    coord[0] = xyz.mesh_x;
    coord[1] = xyz.mesh_y;
    coord[2] = xyz.mesh_z;
    coord[3] = nid;
    coord[4] = sched_getcpu();
#elif defined (BGQ)
    MPIX_Rank2torus( rank, coord );
#endif
}

/*-------------------------------------------------------------------------
 * Function:    distance_between_ranks
 *
 * Purpose:     Given two ranks, return the number of hops a message needs
 *              to take to travel between them.
 *
 * Return:      int distance (number of hops between ranks)
 *
 *-------------------------------------------------------------------------
 */
int distance_between_ranks ( int src_rank, int dest_rank, int ppn, int pps ) {
    int distance = 0;

#ifdef THETA
    int dim = 4, d;
    int src_coord[dim], dest_coord[dim];

    rank_to_coordinates ( src_rank, src_coord );
    rank_to_coordinates ( dest_rank, dest_coord );

    for ( d = 0; d < dim; d++ ) {
        if ( src_coord[d] != dest_coord[d] )
            distance++;
    }
#elif defined (BGQ)
    int dim=6, d, hops;
    int src_coord[6], dest_coord[6];
    MPIX_Hardware_t hw;

    rank_to_coordinates ( src_rank, src_coord );
    rank_to_coordinates ( dest_rank, dest_coord );

    MPIX_Hardware( &hw );
    //dim = hw.torus_dimension; // Should return "6"

    /* Note: dont count last dimension.. it refers to cores on same node */
    for ( d = 0; d < dim-1; d++ ) {
        hops = abs ( dest_coord[d] - src_coord[d] );
        if ( hw.isTorus[d] == 1 )
            hops = TMIN ( hops, (int)hw.Size[d] - hops );
        distance += hops;
    }
#else
    /*
     * If we don't have topology information, but do know ppn & pps (per socket),
     * just assume simple rank ordering.
     * Assume 2 hops between nodes & 1 between sockets.
     */
    int same_node = 1;
    int same_soc  = 0;
    if (ppn > 0) {
        int ind_i = (src_rank / ppn) * ppn;
        int ind_f = (src_rank / ppn) * ppn + ppn;
        if ( (dest_rank < ind_i) || (dest_rank >= ind_f) ) {
            // NOT Inside same 'node'
            same_node = 0;
        }
    }
    if (same_node && (pps > 0)) {
        int ind_i = (src_rank / pps) * pps;
        int ind_m = (src_rank / pps) * pps + pps;
        if ( (dest_rank >= ind_i) && (dest_rank < ind_m) ) same_soc = 1;
    }
    if (!same_soc)  distance++;
    if (!same_node) distance++;
#endif

    return distance;
}

/***********************/
/* THETA-Specific Defs */
/***********************/

#ifdef THETA

  /*-------------------------------------------------------------------------
   * Function:    fgr_to_lnets
   *
   * Purpose:     Given a formatted OST label, return the possible lnet nodes.
   *
   * Return:      int *lnet is populated.
   *
   *-------------------------------------------------------------------------
   */
  void fgr_to_lnets ( char *fgr_id, int *lnet ) {
      int count = 0;
      FILE *fp;
      char fline[100];
      char *lnet_list, *item;

      fp = fopen("/etc/lnet/routes.conf", "r");

      if ( fp == NULL ) {
          fprintf ( stdout, "[ERROR] Error while opening routes.conf file!\n" );
          return;
      }

      while ( fgets ( fline, 100, fp ) != NULL ) {

          const char *c = strstr ( fline, fgr_id );

          if ( c != NULL )  {
              const char *b1 = strstr ( fline, "[" ) + 1;
              const char *b2 = strstr ( fline, "]" );
              lnet_list = ( char * ) malloc ( sizeof ( char ) * ( b2 - b1 + 1 ) );
              strncpy ( lnet_list, b1, b2 - b1 );
              item = strtok ( lnet_list, "," );

              while ( item ) {
                  lnet [ count ] = atoi ( item );
                  item = strtok ( 0, "," );
                  count++;
              }
          }
          count = 0;
      }

      fclose ( fp );
      return;
  }

  /*-------------------------------------------------------------------------
   * Function:    io_nodes_per_file
   *
   * Purpose:     Given a file name, determine the LNET nodes.
   *
   * Return:      Number of LNET nodes responsible for file.
   *              Populate (int *) nodesList with the LNET nodes.
   *
   * Note:        Assume 7 LNET nodes per OST
   *
   *-------------------------------------------------------------------------
   */
  int io_nodes_per_file ( char* filename, int *nodesList ) {
      int err, stripeCount, nLnets, i, idx, oid, l;
      char fgrId [20];
      int *ssuId, *ostId, *lnets;
      struct find_param param = { 0 };
      int ssu2fgr [] = { 0, 0, 0, 0,
                         2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2, 3,
                         4, 5, 4, 5, 4, 5, 4, 5, 4, 5, 4, 5, 4, 5,
                         6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7,
                         8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9, 8, 9};

      err = llapi_getstripe ( filename, &param );
      if ( err )
          fprintf ( stdout, "[ERROR] llapi_getstripe\n");

      stripeCount  = (&param)->fp_lmd->lmd_lmm.lmm_stripe_count;
      nLnets       = stripeCount * LNETS_PER_OST;

      ssuId        = (int *) malloc ( stripeCount * sizeof ( int ) );
      ostId        = (int *) malloc ( stripeCount * sizeof ( int ) );

      /* Hypothesis : OSS id == SNX - 4 */
      for ( i = 0; i < stripeCount; i++ ) {
          idx       = (&param)->fp_lmd->lmd_lmm.lmm_objects[i].l_ost_idx;
          ssuId[i] = idx + 4;
          lnets     = (int *) malloc ( LNETS_PER_OST * sizeof ( int ) );

          snprintf ( fgrId, 20, "o2ib100%d", ssu2fgr[ ssuId[i] ] );

          fgr_to_lnets ( fgrId, lnets );

          for ( l = 0; l < LNETS_PER_OST; l++ )
              nodesList [ i * LNETS_PER_OST + l ] = lnets [ l ];

          free ( lnets );
      }

      return nLnets;
  }


#endif


/*-------------------------------------------------------------------------
 * Function:    distance_to_io_node
 *
 * Purpose:     Given a rank, determine distance to the nearest IO node.
 *
 * Return:      Number of hops needed to reach the nearest IO node.
 *
 *-------------------------------------------------------------------------
 */
int distance_to_io_node ( int src_rank ) {
#ifdef THETAIO

    /*
     * On Theta/LUSTRE, each OST will be served by 7 different IO nodes. This
     * means the writing/reading of a specific stripe (on a specific OST) will
     * require the aggregator to interact with up to 7 different IO nodes
     * durring the course of a collective I/O operation.
     *
     * The following code cannot be used to actually determine the exact/avg
     * IO-node distace, but it is included in case we get ideas later. :)
     */
    int nodesList[MAX_IONODES];
    int n_lnets, i;
    n_lnets = io_nodes_per_file ( "/lus/theta-fs0/projects/datascience/rzamora/topology/1D-ARRAY-00000000.dat", nodesList );
    for ( i = 0; i < n_lnets; i++ ) {
        fprintf (stdout, "%d ", nodesList[i]);
    }

    /*
     * Note: This function needs to be improved to actually calculate the distance to IO nodes...
     * Fore now, just setting distance to 1:
     */
    return 1;

#elif defined (BGQ)
    return MPIX_IO_distance ();

#endif

    /* Default Value */
    return 1;
}

/*-------------------------------------------------------------------------
 * Function:    topology_aware_list_serial
 *
 * Purpose:     Given a `tally` array (of bytes needed to/from each of nb_aggr
 *              aggregators) on each rank, determine optimal list of ranks
 *              to act as the aggregators (agg_list).
 *
 * Return:      0 == Success. Populates int* agg_list
 *
 * Note:        "Serial" label refers to the fact that agg_list must be
 *              populated one at a time (for now).
 *
 *-------------------------------------------------------------------------
 */
int topology_aware_list_serial ( int64_t* tally, int64_t nb_aggr, int* agg_list, int ppn, int pps, MPI_Comm comm )
{
    int i, r, agg_ind, aggr_nprocs, nprocs, latency, bandwidth, distance_to_io, distance, rank;
    int agg_to_calc, aggr_comm_rank, aggr_comm_size, ranks_per_agg, ind_i, ind_m, ind_f;
    MPI_Comm_rank ( comm, &rank );
    MPI_Comm_size ( comm, &nprocs );
    int64_t *data_distribution;
    double base_cost_penalty = 1;
    int trim_thresh = 1;
    int min_stride;
    int *world_ranks;
    cost aggr_cost, min_cost;

    latency           = network_latency (); /*  */
    bandwidth         = network_bandwidth ();
    data_distribution = (int64_t *) malloc (nprocs * sizeof(int64_t));
    world_ranks       = (int *) malloc (nprocs * sizeof(int));
    min_stride        = nprocs / nb_aggr;

    /* Loop through the aggregators (this is the `serial` part) */
    for (agg_ind=0; agg_ind<nb_aggr; agg_ind++ ) {

        aggr_cost.cost  = base_cost_penalty;
        aggr_cost.rank  = rank;
        min_cost.cost  = 0.0;
        min_cost.rank  = 0;

        /* All-reduce the structures needed to calculate cost for sending data */
        MPI_Allgather ( &tally[ agg_ind ], 1, MPI_LONG_LONG, data_distribution, 1, MPI_LONG_LONG, comm );
        MPI_Allgather ( &rank, 1, MPI_INT, world_ranks, 1, MPI_INT, comm );

        /*
         * Now we can trim the data a bit.
         * Note: This is currently turned off.. (see if(0))
         */
        aggr_nprocs = nprocs;
        if (0 && trim_thresh > 0) {
            for (r = nprocs-1; r >= 0; r-- ) {
                if (data_distribution[r] < trim_thresh) {
                    aggr_nprocs--;
                    for (i = r; i < aggr_nprocs; i++) {
                        data_distribution[i] = data_distribution[i+1];
                        world_ranks[i] = world_ranks[i+1];
                    }
                }
            }
        }

        /* Compute the cost of aggregating data from the other ranks */
        for (r = 0; r < aggr_nprocs; r++ ) {
            if ( (rank != world_ranks[r]) && (data_distribution[r] > 0)) {
                distance = distance_between_ranks ( rank, world_ranks[r], ppn, pps );
                //printf("Rank %d - agg_ind = %d r = %d distance = %d \n", rank, agg_ind, r , distance);
                aggr_cost.cost += ( distance * latency + data_distribution[r] / bandwidth );
            }
        }
        distance_to_io = distance_to_io_node ( rank );
        aggr_cost.cost += distance_to_io * latency;

        /* If "this" rank was selected as the "strided" initial list, give it slight preference */
        if ( aggr_cost.rank == agg_list[ agg_ind ]) aggr_cost.cost-=latency;

        /* Determine the aggr with minimum cost */
        //printf("agg_ind = %d aggr_cost.rank = %d aggr_cost.cost = %f\n",agg_ind,aggr_cost.rank,aggr_cost.cost);
        MPI_Allreduce ( &aggr_cost, &min_cost, 1, MPI_DOUBLE_INT, MPI_MINLOC, comm );
        agg_list[ agg_ind ] = min_cost.rank;
        //if (rank == 0) printf("agg_ind = %d min_cost.rank = %d min_cost.cost = %f\n",agg_ind,min_cost.rank,min_cost.cost);

        //printf("agg_ind = %d rank = %d base_cost_penalty = %f aggr_cost.cost = %f\n",agg_ind,rank,base_cost_penalty,aggr_cost.cost);

        /*
         * Increase `base_cost_penalty` for rank that was just chosen.
         * Add smaller penalty if rank is on same node as the last selection.
         */
        if (min_cost.rank == rank)
            base_cost_penalty += LARGE_PENALTY;
        else {
            distance = distance_between_ranks ( rank, min_cost.rank, ppn, pps );
            if (distance < 1)
                base_cost_penalty += SMALL_PENALTY;
        }

    }

    free(data_distribution);
    free(world_ranks);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    add_chunk
 *
 * Purpose:     Helper function to recursively populate an array of byte
 *              quantities, with each index corresponding to the aggregator
 *              index where that quatitiy of data will be read from or written to.
 *
 * Return:      0 == Success. Populates int64_t* tally
 *
 *-------------------------------------------------------------------------
 */
int add_chunk ( int64_t datalen, int64_t offset, int64_t buffer_size, int64_t nb_aggr, int64_t* tally )
{

    int64_t agg_offset = 0;
    int64_t amount_add = 0;
    int64_t amount_left = 0;

    agg_offset = offset % (nb_aggr * buffer_size) ; /* Position of the start of the chunk wrt the round */
    agg_offset = agg_offset / buffer_size ; /* Which aggregator owns the start of this chunk */
    amount_add = buffer_size - (agg_offset % buffer_size) ; /* How many bytes belong in the starting agg */
    if (amount_add > datalen) { /* chunk ends within the same agg */
        amount_add = datalen;
    } else {
        amount_left = datalen - amount_add;
        if (amount_left > 0) {
            offset += amount_add;
            datalen-= amount_add;
            add_chunk ( datalen, offset, buffer_size, nb_aggr, tally );
        }
    }
    tally[ agg_offset ] += amount_add;
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    get_cb_config_list
 *
 * Purpose:     Generate a ROMIO cb_config_list hint string to select an
 *              optimal list of aggregator nodes.
 *
 * Return:      0 == Success. Populates char* hint_str
 *
 * Note:        This will only work if there is one rank per node, and fewer
 *              aggregators than nodes.
 *              (and if the cb_config_list hint is not ignored)
 *
 * Assumption:
 *    ---------------------------------------------------------------
 *   | agg 1 round 1 | agg 2 round 1 | agg 1 round 2 | agg 2 round 2 |
 *    ---------------------------------------------------------------
 *
 *-------------------------------------------------------------------------
 */
int get_cb_config_list ( int64_t* data_lens, int64_t* offsets, int data_len, char* hint_str, int64_t buffer_size, int64_t nb_aggr, MPI_Comm comm )
{
    int rank, nprocs, i, r, resultlen;
    int* agg_list;
    int64_t *data_to_send_per_aggr;
    char name[MPI_MAX_PROCESSOR_NAME];
    char name_buf[MPI_MAX_PROCESSOR_NAME];
    char* cb_reverse = getenv("HDF5_CB_REV");
    MPI_Comm_rank ( comm, &rank );
    MPI_Comm_size ( comm, &nprocs );
    MPI_Get_processor_name( name, &resultlen );
    int ppn = CountProcsPerNode(nprocs, rank, comm);
    int pps = ppn;

    /* Tally data quantities associated with each aggregator */
    data_to_send_per_aggr = (int64_t *) calloc (nb_aggr, sizeof (int64_t));
    for ( r = 0; r < data_len; r++ ) {
        add_chunk ( data_lens[r], offsets[r], buffer_size, nb_aggr, data_to_send_per_aggr );
    }

    /* Generate topology-aware list of aggregators */
    agg_list = (int *) calloc (nprocs, sizeof (int));
    topology_aware_list_serial( data_to_send_per_aggr, nb_aggr, agg_list, ppn, pps, comm );

    /* Reverse the order of the agg list..? */
    if ( cb_reverse && (strcmp(cb_reverse,"yes") == 0) ) {
      for (i=0, r=nb_aggr-1; i<r; i++,r--) {
        int tmp0 = agg_list[i];
        agg_list[i] = agg_list[r];
        agg_list[r] = tmp0;
      }
    }

    for ( r = 0; r < nb_aggr; r++ ) {
        if (rank==0) printf("agg_list[%d] = %d\n",r,agg_list[r]);
        strcpy(name_buf, name);
        MPI_Bcast ( name_buf, MPI_MAX_PROCESSOR_NAME, MPI_CHAR, agg_list[r], comm );
        if (r==0)
            sprintf(hint_str,"%s",name_buf);
        else
            sprintf(hint_str,"%s,%s",hint_str,name_buf);
    }
    free(agg_list);

    return 0;
}


/*-------------------------------------------------------------------------
 * Function:    get_ranklist_spread
 *
 * Purpose:     Just start with a simple rank-based spacing between aggs, then
 *              try to increase actual minimum distance between any two aggs.
 *
 * Return:      0 == Success. Populates int* agg_list
 *
 *-------------------------------------------------------------------------
 */
int get_ranklist_spread ( int64_t nb_aggr, int* agg_list, int ppn, int pps, MPI_Comm comm )
{
    int i, r, agg_ind, distance, nprocs, rank, stride;
    cost aggr_cost, max_cost;

    MPI_Comm_rank ( comm, &rank );
    MPI_Comm_size ( comm, &nprocs );

    /* Start with a simple aggregator placement */
    stride = nprocs / nb_aggr;
    for (agg_ind=0; agg_ind<nb_aggr; agg_ind++ ) {
        agg_list[ agg_ind ] = agg_ind * stride;
    }

    /*
     * Loop through the aggs and adjust each one to MAXIMUIZE the
     * minimum distance to another aggregator...
     */
    for (agg_ind=0; agg_ind<nb_aggr; agg_ind++ ) {

        aggr_cost.cost  = -1;
        aggr_cost.rank  = rank;
        max_cost.cost  = 0.0;
        max_cost.rank  = 0;
        //if ((rank == 0)) printf("starting with agg_rank = %d for agg_ind = %d.\n", agg_list[ agg_ind ], agg_ind);

        /* Compute the "cost" (min distance) to other aggs */
        for (r = 0; r < nb_aggr; r++ ) {
            if (r != agg_ind) {
                if (aggr_cost.rank == agg_list[ r ]) {
                    aggr_cost.cost = 0;
                    //printf("agg_ind = %d, rank %d cannot be chosen because its agg_ind %d already.\n", agg_ind, rank , r);
                    break;
                }
                distance = distance_between_ranks ( aggr_cost.rank, agg_list[ r ], ppn, pps );
                //printf("agg_ind = %d, rank %d is distance=%d from agg %d.\n", agg_ind, aggr_cost.rank, distance, r);
                if (aggr_cost.cost < 0)
                    aggr_cost.cost = distance;
                else
                    aggr_cost.cost = TMIN( distance, aggr_cost.cost );
            }
        }
        if (aggr_cost.rank == agg_list[ agg_ind ]) aggr_cost.cost++; // Don't change agg if we are already good...

        /* Determine the aggr with MAXIMUM cost (we want to maximize min distance) */
        MPI_Allreduce ( &aggr_cost, &max_cost, 1, MPI_DOUBLE_INT, MPI_MAXLOC, comm );
        agg_list[ agg_ind ] = max_cost.rank;
        //printf("agg_ind = %d aggr_cost.rank = %d aggr_cost.cost = %f, max_cost.rank = %d, max_cost.cost = %f \n", agg_ind, aggr_cost.rank, aggr_cost.cost, max_cost.rank, max_cost.cost);

    }

    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    get_ranklist_random
 *
 * Purpose:     Just return a random selection of ranks
 *
 * Return:      0 == Success. Populates int* agg_list
 *
 *-------------------------------------------------------------------------
 */
int get_ranklist_random ( int64_t nb_aggr, int* agg_list, MPI_Comm comm )
{
    int i, r, good, agg_ind, rank, nprocs;
    MPI_Comm_rank ( comm, &rank );
    MPI_Comm_size ( comm, &nprocs );

    /* Use rank-0 to crate a random agg placement */
    if (rank == 0) {
        srand(time(NULL));   /* Initialization, should only be called once. */
        for (agg_ind=0; agg_ind<nb_aggr; agg_ind++ ) {
            while (1) {
                good = 1;
                r = rand() % nprocs;
                for (i=0; i<agg_ind; i++) {
                    if ((r == agg_list[i]) || (r < 0) || (r > (nprocs-1))) {
                        good = 0;
                        break;
                    }
                }
                if (good == 1) break;
            }
            agg_list[ agg_ind ] = r;
        }
    }
    /* Bcast random list to other ranks */
    MPI_Bcast(&agg_list[0], nb_aggr, MPI_INT, 0, comm);
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    get_ranklist_strided
 *
 * Purpose:     Just return a strided selection of ranks
 *
 * Return:      0 == Success. Populates int* agg_list
 *
 *-------------------------------------------------------------------------
 */
int get_ranklist_strided ( int64_t nb_aggr, int* agg_list, int stride, MPI_Comm comm )
{
    int agg_ind, nprocs;
    MPI_Comm_size ( comm, &nprocs );
    if (stride < 1) stride = nprocs / nb_aggr;
    /* Use rank-0 to crate a random agg placement */
    for (agg_ind=0; agg_ind<nb_aggr; agg_ind++ ) {
        agg_list[ agg_ind ] = agg_ind * stride;
    }
    return 0;
}

/*-------------------------------------------------------------------------
 * Function:    topology_aware_ranklist
 *
 * Purpose:     Given a data patern to read or write, generate an optimal
 *              list of aggregator ranks.  Actual routine depends on the
 *              'AGGSelect select_type' argument...
 *
 *    DATA    -> Try to maximize data-movement bandwidth
 *    SPREAD  -> Spread out aggregators using topology information
 *    STRIDED -> Spread out aggregators according a given stride (using the rank IDs)
 *    RANDOM  -> Use random rank selection for aggregator placement
 *
 *
 *   fd_mapping == 0 Assumption:
 *    ---------------------------------------------------------------
 *   | agg 1 round 1 | agg 2 round 1 | agg 1 round 2 | agg 2 round 2 |
 *    ---------------------------------------------------------------
 *
 *   fd_mapping == 1 Assumption (Use contiguous file domains for each agg):
 *    ---------------------------------------------------------------
 *   | agg 1 round 1 | agg 1 round 2 | agg 2 round 1 | agg 2 round 2 |
 *    ---------------------------------------------------------------
 *
 * Return:      0 == Success. Populates int *ranklist
 *
 *-------------------------------------------------------------------------
 */
int topology_aware_ranklist ( int64_t* data_lens, int64_t* offsets, int data_len,
    int *ranklist, int64_t buffer_size, int64_t nb_aggr, int ppn, int pps,
    int stride, MPI_Comm comm, enum AGGSelect select_type, int fd_mapping )
{
    int r;
    int64_t *data_to_send_per_aggr;
    double min_off_g, max_off_g, min_off_l, max_off_l;
    int64_t min_off, max_off, off;
    int64_t st_agg, in_0, in_1;

#ifdef topo_debug
    int rank, myrank, nprocs;
    MPI_Comm_rank ( comm, &myrank );
    MPI_Comm_size ( comm, &nprocs );
#endif

    switch(select_type) {

        case DATA :
        {
            /* Tally data quantities associated with each aggregator */
            data_to_send_per_aggr = (int64_t *) calloc (nb_aggr, sizeof (int64_t));

            if (fd_mapping==1) { /* GPFS-style mapping */

                /* get local min and max offsets */

                min_off = offsets[0];
                max_off = offsets[0] + data_lens[0];
                for ( r = 1; r < data_len; r++ ) {
                    if (offsets[r] < min_off)
                        min_off = offsets[r];
                    off = offsets[r] + data_lens[r];
                    if (off > max_off)
                        max_off = off;
                }
                min_off_l = (double) min_off;
                max_off_l = (double) max_off;

                /* Use allreduce to get global min and max offsets */
                MPI_Allreduce ( &min_off_l, &min_off_g, 1, MPI_DOUBLE, MPI_MIN, comm );
                MPI_Allreduce ( &max_off_l, &max_off_g, 1, MPI_DOUBLE, MPI_MAX, comm );

                min_off = (int64_t) min_off_g;
                max_off = (int64_t) max_off_g;

#ifdef topo_debug
                if (DBGRANKS > 0) {
                    for (rank=0;rank<TMIN(nprocs,DBGRANKS);rank++) {
                        if (rank == myrank) {
                            printf("Rank %d - (min_off_l=%ld, max_off_l=%ld) (min_off=%ld, max_off=%ld):", myrank, (int64_t) min_off_l, (int64_t) max_off_l, min_off, max_off);
                            for (r=0;r<data_len;r++)
                                printf(" [%ld -> %ld]", offsets[r], offsets[r]+data_lens[r] );
                            printf("\n");
                            fflush(stdout);
                        }
                        MPI_Barrier(comm);
                    }
                }
#endif

                /* Loop through data to add counts to known file domains */
                int64_t fd_size = (max_off - min_off) / nb_aggr;
                for ( r = 0; r < data_len; r++ ) {
                    st_agg = (offsets[r] - min_off) / fd_size;
                    //in_0   = (offsets[r]-min_off) % fd_size;
                    in_0   = ((st_agg + 1) * fd_size) - (offsets[r] - min_off);
                    in_0   = TMIN ( in_0, data_lens[r] );
                    in_1   = TMAX(0, data_lens[r] - in_0);
                    data_to_send_per_aggr[ (int) st_agg ] += in_0;
                    data_to_send_per_aggr[ (int) ((st_agg+1)%nb_aggr) ] += in_1;
                }

            } else { /* LUSTRE-style mapping */
                for ( r = 0; r < data_len; r++ ) {
                    add_chunk ( data_lens[r], offsets[r], buffer_size, nb_aggr, data_to_send_per_aggr );
                }
            }

            /* Generate topology-aware list of aggregators */
            topology_aware_list_serial( data_to_send_per_aggr, nb_aggr, ranklist, ppn, pps, comm );
            break;
        }
        case SPREAD :
        {
            /* Generate spread-out list of aggregators */
            get_ranklist_spread ( nb_aggr, ranklist, ppn, pps, comm );
            break;
        }
        case STRIDED :
        {
            /* Generate constant-strided list of aggregators */
            get_ranklist_strided ( nb_aggr, ranklist, stride, comm );
            break;
        }
        case RANDOM :
        {
            /* Generate random list of aggregators */
            get_ranklist_random ( nb_aggr, ranklist, comm );
            break;
        }
        default :
        {
            /* Generate random list of aggregators */
            get_ranklist_strided ( nb_aggr, ranklist, 0, comm );
        }

    }

#ifdef topo_debug
    if (myrank == 0) {
        printf("Topology-aware CB Selection (type %d): nb_aggr is %d, and ranklist is:", select_type, nb_aggr);
        for (r=0;r<nb_aggr;r++)
            printf(" %d", ranklist[r]);
        printf("\n");
    }
    MPI_Barrier(comm);
    if ((select_type == DATA) && (DBGRANKS > 0)) {
        for (rank=0;rank<TMIN(nprocs,DBGRANKS);rank++) {
            if (rank == myrank) {
                printf("Rank %d - Data distribution: ", myrank);
                for (r=0;r<nb_aggr;r++)
                    printf(" %d", data_to_send_per_aggr[r]);
                printf("\n");
                fflush(stdout);
            }
            MPI_Barrier(comm);
        }
    }
#endif

    return 0;
}
