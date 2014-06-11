/* 
 * h5ff_timings.c: Do some timing of various write / persist / prefetch / read
 */
/*
 *
 * Creates 3 groups, each with a dataset & a map
 * In /G-logged, objects will read.
 * In /G-prefetched, objects will be evicted & prefetched & read & evicted
 * In /G-stored, objects will be evicted & read
 *    D = dataset; 2d with uint64 datatypes.   size is [ (rows_per_rank*#ranks), (cols_per_row) ]
 *    M = map; uint64 keys ranging from 0 to (rows_per_rank*#ranks*cols_per_row)-1
 *             uint64 values, set to same as dset entries
 *    each rank updates "its" dataset entries & map pairs, based on rows_per_rank variable
 *
 *
 * Notes for possible future modifications:
 * 1) Make Datasets 1D, with entries 2D vectors to mimic ACG Edge List representation
 * 2) Make Maps have values that are vectors of uint64, to mimic ACG Adjacency list representation (step 1)
 * 3) Make Maps have values that variable-length vectors of uint64, to mimic ACG Adjaceny list represenations (really)
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include "mpi.h"
#include "hdf5.h"

/* define filename for this app, and max size after username prepended */
#define FILENAME_APP "eff_timings.h5"
#define FILENAME_SIZE 256

/* define default sizing parameters - can be changed via command line arguments */
#define DEFAULT_ROWS_PER_RANK 10
#define DEFAULT_COLS_PER_ROW 5

/* macros related to error reporting */
#define STATUS (ret >= 0) ? " " : " - FAILED"
#define ASSERT_RET assert( ret >= 0 )

/* option flags */
int cols_per_row = DEFAULT_COLS_PER_ROW;
int rows_per_rank = DEFAULT_ROWS_PER_RANK;      
int num_iterations = 1;    /* Number of times to perform the commit/persist/evict/prefetch/read/evict cycle */
int verbose = 0;           /* Verbose output - defaults to no */
int time_reads = 0;        /* Time reads after container re-open */

/* global variables and macros used to make timing easier */
struct timeval tv_start;
struct timeval tv_end;
#define START_TIME gettimeofday( &tv_start, NULL )
#define END_TIME gettimeofday( &tv_end, NULL )
#define ELAPSED_TIME (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) 

/* function prototypes */
void print_container_contents( hid_t, hid_t, const char*, int );
int  parse_options( int, char**, int, int );
void usage( const char* );

/**************************************************************************************************************/
int main( int argc, char **argv ) {

   /* MPI */
   int my_rank, comm_size;
   int provided;

   /* Container */
   char *user_name;                 /* We'll prepend username to make filename unique */
   char file_name[FILENAME_SIZE];   /* Actual filename to be opened will be user_name + FILENAME_APP */
   hid_t  fapl_id;         
   hid_t  file_id;

   /* Groups */
   hid_t  grp_l_id;
   hid_t  grp_p_id;
   hid_t  grp_s_id;

   /* Dataspaces */
   hid_t    space_l_id;
   hid_t    space_p_id;
   hid_t    space_s_id;

   /* Datasets */
   hid_t    dset_l_id;
   hid_t    dset_p_id;
   hid_t    dset_s_id;
   hsize_t  dset_size[2];
   hid_t    space_id; 

   /* Maps */
   hid_t    map_l_id;
   hid_t    map_p_id;
   hid_t    map_s_id;

   /* Replicas and related properties */
   hrpl_t   dset_p_replica;
   hid_t    dxpl_p_id;
   hrpl_t   map_p_replica;
   hid_t    mxpl_p_id;

   /* Container Version & Read Contexts */
   uint64_t version, versionH;
   hid_t    rc_id, rc_idH;

   /* Transactions  */
   uint64_t tr_num;
   hid_t    tr_id;
   hid_t    trspl_id;
   int      num_tr_leaders;

   /* Memory buffer - sized to hold complete DS or Map Values; sometimes partially used */
   uint64_t *mbuf;

   /* Sizing, dataspace, hyperslab parameters set per-rank */
   hsize_t  rank_mbuf_size[1];
   hsize_t  rank_offset[2], rank_count[2];
   hid_t    rank_space_id;

   /* Variables used for reading data written by a neighbor rank */
   int      neighbor_rank;
   hid_t    neighbor_space_l_id;
   hid_t    neighbor_space_p_id;
   hid_t    neighbor_space_s_id; 
   hsize_t  neighbor_offset[2], neighbor_count[2];

   /* Misc */
   herr_t   ret;
   uint64_t u, r, c;
   uint64_t key;
   int      bytesPerCell;
   int      i;
   int      iteration;

   /****
    * Initialize and create container
    ****/ 

   /* Confirm needed MPI functionality is available */
   MPI_Init_thread( &argc, &argv, MPI_THREAD_MULTIPLE, &provided );
   if ( MPI_THREAD_MULTIPLE != provided ) {
      fprintf( stderr, "APP: ERROR: MPI does not have MPI_THREAD_MULTIPLE support\n" );
      exit( 1 );
   }

   /* Find MPI rank & communicator size */
   MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
   MPI_Comm_size( MPI_COMM_WORLD, &comm_size );
   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: Number of MPI processes = %d\n", my_rank, comm_size );
   }

   /* Parse command-line options controlling behavior */
   if ( parse_options( argc, argv, my_rank, comm_size ) != 0 ) {
      exit( 1 );
   }    
   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r0: Number of MPI processes = %d\n", comm_size );
      fprintf( stderr, "APP-r0: Datasets will have %d rows (%d per rank).\n", (rows_per_rank*comm_size), rows_per_rank );
      fprintf( stderr, "APP-r0: Datasets will have %d columns.\n", cols_per_row );
      fprintf( stderr, "APP-r0: There will be %d iterations.\n", num_iterations );
   }

   /* Initialize the EFF stack, starting FS client, registering HDF5 VOL calls, requesting IOD init */
   fprintf( stderr, "APP-r%d: Initialize EFF stack\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Specify the IOD VOL plugin should be used and create H5File (EFF container), with user name prepended */
   user_name = getenv( "USER" );
   snprintf( file_name, FILENAME_SIZE, "%s_%s", user_name, FILENAME_APP );
   fprintf( stderr, "APP-r%d: Create %s\n", my_rank, file_name );
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id >= 0 );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); ASSERT_RET;
   file_id = H5Fcreate_ff( file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );

   /* Create the memory buffer that will be used in many places - both for writing and reading */
   bytesPerCell = sizeof( uint64_t );
   mbuf = (uint64_t *) calloc( (rows_per_rank * comm_size * cols_per_row), bytesPerCell ); assert( mbuf != NULL );

   /****
    * Transaction 2: Rank 0 creates H5Objects in the container 
    ****/

   if ( my_rank == 0 ) {

      /* Acquire read context for CV 1 */
      version = 1;
      rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( rc_id >= 0); assert( version == 1 );
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

      /* Start a transaction with a single leader (the default) */
      tr_num = 2;
      fprintf( stderr, "APP-r%d: tr %lu - Start\n", my_rank, tr_num );
      tr_id = H5TRcreate( file_id, rc_id, tr_num ); assert( tr_id >= 0 );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Add updates to the transaction for Group creates */
      fprintf( stderr, "APP-r%d: tr %d - Create /G-logged /G-prefetched /G-stored\n", my_rank, tr_num );

      grp_l_id = H5Gcreate_ff( file_id, "G-logged", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_l_id >= 0 );

      grp_p_id = H5Gcreate_ff( file_id, "G-prefetched", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_p_id >= 0 );

      grp_s_id = H5Gcreate_ff( file_id, "G-stored", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_s_id >= 0 );

      /* Create Dataspace for Datasets */
      dset_size[0] = rows_per_rank * comm_size;
      dset_size[1] = cols_per_row;
      space_id = H5Screate_simple( 2, dset_size, dset_size ); assert( space_id >= 0 );

      /* Add updates to the transaction for Dataset creates */
      fprintf( stderr, "APP-r%d: tr %d - Create /G-logged/D /G-prefetched/D /G-stored/D\n", my_rank, tr_num );

      dset_l_id = H5Dcreate_ff( grp_l_id, "D", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );

      dset_p_id = H5Dcreate_ff( grp_p_id, "D", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                H5_EVENT_STACK_NULL ); assert( dset_p_id >= 0 );

      dset_s_id = H5Dcreate_ff( grp_s_id, "D", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                H5_EVENT_STACK_NULL ); assert( dset_s_id >= 0 );

      ret = H5Sclose( space_id ); ASSERT_RET;      

      /* Add updates to the transaction for Map creates */
      fprintf( stderr, "APP-r%d: tr %d - Create /G-logged/M /G-prefetched/M /G-stored/M\n", my_rank, tr_num );

      map_l_id = H5Mcreate_ff( grp_l_id, "M", H5T_NATIVE_UINT, H5T_NATIVE_UINT, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                               H5_EVENT_STACK_NULL ); assert( map_l_id >= 0 );

      map_p_id = H5Mcreate_ff( grp_p_id, "M", H5T_NATIVE_UINT, H5T_NATIVE_UINT, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                               H5_EVENT_STACK_NULL ); assert( map_p_id >= 0 );

      map_s_id = H5Mcreate_ff( grp_s_id, "M", H5T_NATIVE_UINT, H5T_NATIVE_UINT, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                               H5_EVENT_STACK_NULL ); assert( map_s_id >= 0 );

      /* Finish and commit TR and get RC for it */
      ret = H5TRfinish( tr_id, H5P_DEFAULT, &rc_idH, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      fprintf( stderr, "APP-r%d: tr %lu - Finished\n", my_rank, tr_num );

      assert( rc_idH >= 0 ); 
      versionH = tr_num;
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, versionH );

      /* Release the read handle and close read context on earlier CV */
      ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
      ret = H5RCclose( rc_id ); ASSERT_RET;
      fprintf( stderr, "APP-r%d: rc %lu - Released and Closed\n", my_rank, version );

      /* The just-acquired RC is now the primary rc_id we'll work with (for awhile) */
      rc_id = rc_idH;
      version = versionH;
   }

   /* Rank 0 notifies other ranks that transaction 2 committed and RC acquired */
   MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );

   /****
    * Read Context 2: Other ranks create RC and open HObjects created by rank 0.  
    ****/
   if ( my_rank != 0 ) {
      rc_id = H5RCcreate( file_id, version );
      assert( rc_id >= 0 ); assert ( version == 2 );
      fprintf( stderr, "APP-r%d: rc %lu - Created for rank\n", my_rank, version );

      grp_l_id = H5Gopen_ff( file_id, "G-logged", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( grp_l_id >= 0 );
      grp_p_id = H5Gopen_ff( file_id, "G-prefetched", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( grp_p_id >= 0 );
      grp_s_id = H5Gopen_ff( file_id, "G-stored", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( grp_s_id >= 0 );

      dset_l_id = H5Dopen_ff( grp_l_id, "D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );
      dset_p_id = H5Dopen_ff( grp_p_id, "D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_p_id >= 0 );
      dset_s_id = H5Dopen_ff( grp_s_id, "D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_s_id >= 0 );

      map_l_id = H5Mopen_ff( grp_l_id, "M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( map_l_id >= 0 );
      map_p_id = H5Mopen_ff( grp_p_id, "M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( map_p_id >= 0 );
      map_s_id = H5Mopen_ff( grp_s_id, "M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( map_s_id >= 0 );
   } else {
      if ( verbose ) print_container_contents( file_id, rc_id, "/", my_rank );
   }

   /* Keep track of my_rank's neighbor */
   neighbor_rank = my_rank + 1;
   if ( neighbor_rank == comm_size ) {
      neighbor_rank = 0;
   }

   /* Get the dataspaces for the three datasets and make a copy of each */
   space_l_id = H5Dget_space( dset_l_id ); assert ( space_l_id >= 0 );
   space_p_id = H5Dget_space( dset_p_id ); assert ( space_p_id >= 0 ); 
   space_s_id = H5Dget_space( dset_s_id ); assert ( space_s_id >= 0 );

   neighbor_space_l_id = H5Scopy( space_l_id );
   neighbor_space_p_id = H5Scopy( space_p_id );
   neighbor_space_s_id = H5Scopy( space_s_id );

   /* Select the hyperslab that this rank will update and the hyperslab for reading neighbor's data */
   rank_offset[0] = my_rank * rows_per_rank;
   rank_offset[1] = 0;
   rank_count[0] = rows_per_rank;
   rank_count[1] = cols_per_row;

   neighbor_offset[0] = neighbor_rank * rows_per_rank;
   neighbor_offset[1] = 0;
   neighbor_count[0] = rows_per_rank;
   neighbor_count[1] = cols_per_row;

   ret = H5Sselect_hyperslab( space_l_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( space_p_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( space_s_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

   ret = H5Sselect_hyperslab( neighbor_space_l_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( neighbor_space_p_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( neighbor_space_s_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;

   /* Create the memory dataspace for a rank - always first part of memory buffer */
   rank_mbuf_size[0] = rows_per_rank * cols_per_row;
   rank_space_id = H5Screate_simple( 1, rank_mbuf_size, rank_mbuf_size ); assert( rank_space_id >= 0 );

   /* Create property lists that will be used */
   dxpl_p_id = H5Pcreate( H5P_DATASET_XFER );                              
   mxpl_p_id = H5Pcreate( H5P_DATASET_XFER );

   for ( iteration = 0; iteration < num_iterations; iteration++ ) {

      /**** 
       *  Cycle of:
       *    Tranasction N Start / Update / Commit: All ranks update Dataset and Map objects in the 3 Groups then commit 
       *    CV N Persist: Rank 0
       *    Prefetch: Rank 0
       *
       ****/
      tr_num = tr_num + 1;
      num_tr_leaders = comm_size;

      /* Create a transaction and start it. */
      fprintf( stderr, "APP-r%d: iter %d tr %lu - Transaction Start with %d leaders\n", 
               my_rank, iteration, tr_num, num_tr_leaders );
      tr_id = H5TRcreate( file_id, rc_id, tr_num ); assert( tr_id >= 0 );
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
      ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
      ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   
      /* Set cell values (in memory) for this rank based on Row, Rank, Column, Transaction */
      u = 0;
      for ( r = 0; r < rows_per_rank; r++ ) {
         for ( c = 0; c < cols_per_row; c++ ) {
            mbuf[u] = (my_rank*rows_per_rank+r)*1000000 + c*100 + tr_num;
            u++;
         }
      }

      fprintf( stderr, "APP-r%d: iter %d tr %lu - Add updates to transaction.\n", my_rank, iteration, tr_num );

      /* Add dataset updates to transaction */
      START_TIME;
      ret = H5Dwrite_ff( dset_l_id, H5T_NATIVE_UINT64, rank_space_id, space_l_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates (%lu bytes) for /G-logged/D: %lu usec\n", 
               my_rank, iteration, u*bytesPerCell, ELAPSED_TIME );

      START_TIME;
      ret = H5Dwrite_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, space_p_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates (%lu bytes) for /G-prefetched/D: %lu usec\n", 
               my_rank, iteration, u*bytesPerCell, ELAPSED_TIME );

      START_TIME;
      ret = H5Dwrite_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, space_s_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates (%lu bytes) for /G-stored/D: %lu usec\n", 
               my_rank, iteration, u*bytesPerCell, ELAPSED_TIME );

      /* Add map updates to transaction; do it map-by-map so we can do timing */
      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mset_ff( map_l_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates ( %lu K/V pairs - %lu bytes) for /G-logged/M: %lu usec\n", 
               my_rank, iteration, u, u*bytesPerCell*2, ELAPSED_TIME );
   
      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mset_ff( map_p_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates ( %lu K/V pairs - %lu bytes) for /G-prefetched/M: %lu usec\n", 
               my_rank, iteration, u, u*bytesPerCell*2, ELAPSED_TIME );

      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mset_ff( map_s_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Time to add updates ( %lu K/V pairs - %lu bytes) for /G-stored/M: %lu usec\n", 
               my_rank, iteration, u, u*bytesPerCell*2, ELAPSED_TIME );

      /* Finish, (commit), and close transaction */
      fprintf( stderr, "APP-r%d: iter %d tr %lu - Finish and Commit\n", my_rank, iteration, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      ret = H5Pclose( trspl_id ); ASSERT_RET;

      /* After commit completes on all ranks, acquire RC on rank 0 and create local handle for RC on other ranks */
      MPI_Barrier( MPI_COMM_WORLD ); 
      versionH = tr_num;
      if ( my_rank == 0 ) {
         rc_idH = H5RCacquire( file_id, &versionH, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         assert( rc_idH >= 0 ); assert ( versionH == tr_num );
         fprintf( stderr, "APP-r%d: iter %d rc %lu - Acquired\n", my_rank, iteration, versionH );
      } else {
         rc_idH = H5RCcreate( file_id, versionH );  
         assert( rc_idH >= 0 ); assert ( versionH == tr_num );
         fprintf( stderr, "APP-r%d: iter %d rc %lu - Created\n", my_rank, iteration, versionH );
      }
      MPI_Barrier( MPI_COMM_WORLD );   /* Wait to make sure rank 0 has acquired from IOD before proceeding */

      /* Release previous Read Context */
      if ( my_rank == 0 ) {
         ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
         fprintf( stderr, "APP-r%d: iter %d rc %lu - Released\n", my_rank, iteration, version );
      }
      ret = H5RCclose( rc_id ); ASSERT_RET;
      fprintf( stderr, "APP-r%d: iter %d rc %lu - Closed\n", my_rank, iteration, version );

      /* The just-acquired RC is now the primary rc_id we'll work with (for awhile) */
      rc_id = rc_idH;
      version = versionH;

      /* Rank 0:
        - persists the container
        - evicts D and M under /G-prefetched and /G-stored 
        - prefetches D and M under /G-prefetched
      */
      if ( my_rank == 0 ) {   
         if ( verbose ) print_container_contents( file_id, rc_id, "/", my_rank );

         START_TIME;
         ret = H5RCpersist( rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Persist container: %lu usec\n", my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Devict_ff( dset_p_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict /G-prefetched/D: %lu usec\n", my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Devict_ff( dset_s_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict /G-stored/D: %lu usec\n", my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Mevict_ff( map_p_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict /G-prefetched/M (currently no-op for IOD): %lu usec\n", 
                  my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Mevict_ff( map_s_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict /G-stored/M: (currently no-op for IOD) %lu usec\n", 
                  my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Dprefetch_ff( dset_p_id, rc_id, &dset_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Prefetch /G-prefetched/D: %lu usec\n", my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Mprefetch_ff( map_p_id, rc_id, &map_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Prefetch /G-prefetched/M: %lu usec\n", my_rank, iteration, ELAPSED_TIME );
      }

      /* This Bcast will make sure no nodes go ahead to use the RC before rank 0 has acquired it */
      MPI_Bcast( &dset_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
      ret = H5Pset_dxpl_replica( dxpl_p_id, dset_p_replica ); ASSERT_RET;

      MPI_Bcast( &map_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
      ret = H5Pset_dxpl_replica( mxpl_p_id, map_p_replica );  ASSERT_RET;

      MPI_Barrier( MPI_COMM_WORLD );         /* Make sure all are here before continuing */

      /* 
       * All ranks read the same values they updated in last transaction
       */

      /* Datasets */
      START_TIME;
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, rank_space_id, space_l_id, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-logged/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /G-logged/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      START_TIME;
      ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, space_p_id, dxpl_p_id, mbuf, rc_id, H5_EVENT_STACK_NULL );
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-prefetched/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /G-prefetched/D: %09lu\n", my_rank, mbuf[0] ); 
      }

      START_TIME;
      ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, space_s_id, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-stored/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /G-stored/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      /* Maps */
      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_l_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-logged/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /G-logged/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_p_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-prefetched/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) 
         { fprintf( stderr, "APP-r%d: my first updated value in /G-prefetched/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      key = rows_per_rank * cols_per_row * my_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_s_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for my rank entries for /G-stored/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /G-stored/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      /* 
       * All ranks read the values "next rank" neighbor updated in last transaction
       */

      /* Datasets */
      START_TIME;
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, rank_space_id, neighbor_space_l_id, H5P_DEFAULT, mbuf, rc_id, 
                        H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-logged/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-logged/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      START_TIME;
      ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, neighbor_space_p_id, dxpl_p_id, mbuf, rc_id, 
                        H5_EVENT_STACK_NULL );
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-prefetched/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-prefetched/D: %09lu\n", my_rank, mbuf[0]); 
      }

      START_TIME;
      ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, neighbor_space_s_id, H5P_DEFAULT, mbuf, rc_id, 
                        H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-stored/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-stored/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      /* Maps */
      key = rows_per_rank * cols_per_row * neighbor_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_l_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-logged/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-logged/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      key = rows_per_rank * cols_per_row * neighbor_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_p_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-prefetched/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-prefetched/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      key = rows_per_rank * cols_per_row * neighbor_rank;
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row; u++ ) {
         ret = H5Mget_ff( map_s_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         key++;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for neighbor's entries for /G-stored/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /G-stored/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      /* 
       * All ranks read all the values 
       */

      /* Datasets */
      START_TIME;
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-logged/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-logged/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      START_TIME;
      ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, dxpl_p_id, mbuf, rc_id, H5_EVENT_STACK_NULL );
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-prefetched/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-prefetched/D: %09lu\n", my_rank, mbuf[0] ); 
      }

      START_TIME;
      ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-stored/D: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-stored/D: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      /* Maps */
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row * comm_size; u++ ) {
         ret = H5Mget_ff( map_l_id, H5T_NATIVE_UINT64, &u, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-logged/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-logged/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row * comm_size; u++ ) {
         ret = H5Mget_ff( map_p_id, H5T_NATIVE_UINT64, &u, H5T_NATIVE_UINT64, &mbuf[u], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-prefetched/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-prefetched/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   
      START_TIME;
      for ( u = 0; u < rows_per_rank * cols_per_row * comm_size; u++ ) {
         ret = H5Mget_ff( map_s_id, H5T_NATIVE_UINT64, &u, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
      }
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %d - Read Time for all entries for /G-stored/M: %lu usec\n", 
               my_rank, iteration, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /G-stored/M: %09lu\n", my_rank, mbuf[0] ); 
      }
   

      /* Rank 0 evicts the replicas */
      MPI_Barrier( MPI_COMM_WORLD );            /* Make sure all ranks done reading replicas before evict */
      if ( my_rank == 0 ) {
         START_TIME;
         ret = H5Devict_ff( dset_p_id, version, dxpl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict Replica of /G-prefetched/D: %lu usec\n", 
                  my_rank, iteration, ELAPSED_TIME );

         START_TIME;
         ret = H5Mevict_ff( map_p_id, version, mxpl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %d - Time to Evict Replica of /G-prefetched/M (currently no-op for IOD): %lu usec\n", 
                  my_rank, iteration, ELAPSED_TIME );
      }

   }

   /*
    * Wrap-up 
    */

   /* Print (if verbose) the final state */
   if ( my_rank == 0 ) {
      if ( verbose ) print_container_contents( file_id, rc_id, "/", my_rank );
   }

   /* Close the dataset and map property lists */
   ret = H5Pclose( dxpl_p_id ); ASSERT_RET;
   ret = H5Pclose( mxpl_p_id ); ASSERT_RET;

   /* Close the memory dataspace for the rank */
   ret = H5Sclose( rank_space_id ); ASSERT_RET;         

   fprintf( stderr, "APP-r%d: Closing H5Objects\n", my_rank );

   ret = H5Dclose_ff( dset_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Dclose_ff( dset_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Dclose_ff( dset_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   ret = H5Sclose( space_l_id ); ASSERT_RET;
   ret = H5Sclose( space_p_id ); ASSERT_RET;
   ret = H5Sclose( space_s_id ); ASSERT_RET;

   ret = H5Mclose_ff( map_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Mclose_ff( map_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Mclose_ff( map_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   ret = H5Gclose_ff( grp_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Gclose_ff( grp_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Gclose_ff( grp_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Release the read handle and close read context  */
   MPI_Barrier( MPI_COMM_WORLD );         /* Make sure all ranks done with RC */
   if ( my_rank == 0 ) {
      ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
      fprintf( stderr, "APP-r%d: rc %lu - Released\n", my_rank, version );
   }
   ret = H5RCclose( rc_id ); ASSERT_RET;
   fprintf( stderr, "APP-r%d: rc %lu - Closed\n", my_rank, version );

   /* Close H5 Objects that are still open */
   fprintf( stderr, "APP-r%d: Closing %s\n", my_rank, file_name );
   MPI_Barrier( MPI_COMM_WORLD );
   ret = H5Fclose_ff( file_id, 1, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Pclose( fapl_id ); ASSERT_RET;

   /* Perform wrap-up operations */
   fprintf( stderr, "APP-r%d: Finalize EFF stack\n", my_rank );
   MPI_Barrier( MPI_COMM_WORLD );
   EFF_finalize();
   MPI_Finalize();

   return 0;
}


/*
 * Helper function used to recursively read and print container contents
 * for container identified by "file_id"
 * in read context identified by "rc_id"
 * with path to current level in "grp_path"
 * and "my_rank" used to identify the process doing the reading / printing.
 */
void
print_container_contents( hid_t file_id, hid_t rc_id, const char* grp_path, int my_rank )
{
   herr_t ret;
   uint64_t cv;
   hbool_t exists;
   char path_to_object[1024];
   char name[30];
   int i;

   static int lvl = 0;     /* level in recursion - used to format printing */
   char preface[128];    

   /* Get the container version for the read context */
   ret = H5RCget_version( rc_id, &cv ); ASSERT_RET;

   /* Set up the preface and adding version number */
   sprintf( preface, "APP-r%d: cv %d: ", my_rank, (int)cv );

   /* Start the printing */
   if ( lvl == 0 ) {
      fprintf( stderr, "%s ----- Container Contents ------------\n", preface );
   } 

   /* Datasets */
   sprintf( path_to_object, "%s%s", grp_path, "D" );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t dset_id;
      hid_t space_id;
      int nDims;
      hsize_t current_size[2];
      hsize_t max_size[2];
      hsize_t totalSize;
      uint64_t *data;              
      uint64_t u, e;

      dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( dset_id >= 0 );

      space_id = H5Dget_space( dset_id ); assert ( space_id >= 0 );
      nDims = H5Sget_simple_extent_dims( space_id, current_size, max_size ); 
      assert( nDims == 2 );

      totalSize = current_size[0] * current_size[1];
      data = (uint64_t *)calloc( totalSize, sizeof(uint64_t) ); assert( data != NULL );

      ret = H5Dread_ff( dset_id, H5T_NATIVE_UINT64, space_id, space_id, H5P_DEFAULT, data, rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* note that by printing piece-by-piece, other output may be interspersed.  trade-off since won't know line size */
      fprintf( stderr, "%s %s data:\n", preface, path_to_object );
      e = 0;
      for ( i = 0; i < totalSize; i++ ) {
         if ( e == 0 ) fprintf( stderr, "%s\t", preface );
         fprintf( stderr, "%09lu ", data[i] );
         if ( ++e == cols_per_row ) {
            fprintf( stderr, "\n" );
            e = 0;
         }
      }

      free( data );
      sleep( 1 );       /* give output a chance to be printed before diag messages from lower layers on close */
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_id ); ASSERT_RET;
   }

   /* Maps */
   sprintf( path_to_object, "%s%s", grp_path, "M" );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t map_id;
      hsize_t totalCount;
      uint64_t *value;              
      uint64_t u, e;

      map_id = H5Mopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( map_id >= 0 );

      ret = H5Mget_count_ff( map_id, &totalCount, rc_id, H5_EVENT_STACK_NULL );  ASSERT_RET;
      value = (uint64_t *)calloc( totalCount, sizeof(uint64_t) );  assert( value != NULL );

      for ( u = 0; u < totalCount; u++ ) {
         ret = H5Mget_ff( map_id, H5T_NATIVE_UINT64, &u, H5T_NATIVE_UINT64, &value[u], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }

      /* note that by printing piece-by-piece, other output may be interspersed.  trade-off since won't know line size */
      fprintf( stderr, "%s %s key/values:\n", preface, path_to_object );
      e = 0;
      for ( u = 0; u < totalCount; u++ ) {
         if ( e == 0 ) fprintf( stderr, "%s\t", preface );
         fprintf( stderr, "%lu/%09lu ", u, value[u] );
         if ( ++e == cols_per_row ) {
            fprintf( stderr, "\n" );
            e = 0;
         }
      }

      free( value );
      sleep( 1 );       /* give output a chance to be printed before diag messages from lower layers on close */
      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   }

   /* Groups - if found, descend */
   for ( i = 1; i < 4; i++ ) {
      if ( i == 1 ) { 
         strcpy( name, "G-logged" );
      } else if ( i == 2 ) { 
         strcpy( name, "G-prefetched" ); 
      } else if ( i == 3 ) { 
         strcpy( name, "G-stored" ); 
      }

      sprintf( path_to_object, "%s%s/", grp_path, name );
      ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );
      if ( exists ) { 
         fprintf( stderr, "%s %s\n", preface, path_to_object );
         lvl++;
         print_container_contents( file_id, rc_id, path_to_object, my_rank );
         lvl--;
      } 
   }

   /* End printing */
   if ( lvl == 0 ) {
      fprintf( stderr, "%s -----------------\n", preface );
   }

   return;
}

/*
 * parse_options - parse command line options
 */
int
parse_options( int argc, char** argv, int my_rank, int comm_size ) {
   int    i, n;
   char* app = argv[0];

   while ( --argc ) {
      if ( **(++argv) != '-' ) {
         break;
      } else {
         switch( *(*argv+1) ) {
            case 'c':
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "Error: No number specified after -c option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  cols_per_row = atoi( *argv );
               }
               break;
            case 'r':
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "Error: No number specified after -r option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  rows_per_rank = atoi( *argv );
               }
               break;
            case 'i':
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "APP-r0: Error: No number specified after -i option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  num_iterations = atoi( *argv );
               }
               break;
            case 'v':   
               verbose = 1;
               break;
            default: 
               if ( my_rank == 0 ) {
                  usage( app );
               }
               return( 1 );
         }
      }
   }
   return( 0 );
}

/*
 * usage - display usage message 
 */
void
usage( const char* app ) {
   printf( "Usage: %s [-c cols_per_row] [-r rows_per_rank] [-i num_iterations] [-v] [-w]\n", app  );
   printf( "\tc: number of columns in each row of created H5Datasets\n" );
   printf( "\tr: number of rows each rank writes in created H5Datasets (total rows = this * # of ranks)\n" );
   printf( "\ti: number of iterations to do update/commit/persist/evict/prefetch/read/evict\n" );
   printf( "\tv: verbose output\n" );
}




#ifdef THIS_CODE_WONT_COMPILE_BUT_KEEP_IT_AROUND
/* 
 * This codeblock needs work to reintegrate back into main() after the iterate loop of complete overwrites.
 * Didn't want to loose it in case I decide to add back in later.
 * It allows ranks to overwrite only part of the dataset & map obects in a transaction.
 * With more work, can be used to demonstrate / time reading from 'layers' of updates & persisting only partial objects
 * for a container (where previous layers alreay persisted     -- Ruth
 */

   /**** 
    * Transaction X - All ranks update parts of the Dataset and Map objects in the 3 Groups  
    ****/
   mbuf_size[0] = cols_per_row;
   space_id = H5Screate_simple( 1, mbuf_size, mbuf_size ); assert( space_id >= 0 );

   offset[0] = my_rank * rows_per_rank;
   offset[1] = 0;
   count[0] = 1;
   count[1] = cols_per_row;

   ret = H5Sselect_hyperslab( space_l_id, H5S_SELECT_SET, offset, NULL, count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( space_p_id, H5S_SELECT_SET, offset, NULL, count, NULL );  ASSERT_RET;
   ret = H5Sselect_hyperslab( space_s_id, H5S_SELECT_SET, offset, NULL, count, NULL );  ASSERT_RET;

   tr_num = X;
   num_tr_leaders = comm_size;

   /* Create a local transaction for transaction number and start it. */
   fprintf( stderr, "APP-r%d: tr %lu - Start with %d leaders\n", my_rank, tr_num, num_tr_leaders );
   tr_id = H5TRcreate( file_id, rc_id, tr_num ); assert( tr_id >= 0 );
   trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
   ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
   ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Set cell values (in memory) based on Column, Row, Rank, Transaction */
   u = 0;
   for ( r = 0; r < 1; r++ ) {
      for ( c = 0; c < cols_per_row; c++ ) {
         mbuf[u] = (my_rank*rows_per_rank+r)*1000000 + c*100 + tr_num;
         u++;
      }
   }

   /* Add dataset updates to transaction */
   ret = H5Dwrite_ff( dset_l_id, H5T_NATIVE_UINT64, space_id, space_l_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;
   ret = H5Dwrite_ff( dset_p_id, H5T_NATIVE_UINT64, space_id, space_p_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;
   ret = H5Dwrite_ff( dset_s_id, H5T_NATIVE_UINT64, space_id, space_s_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;

   /* Add map updates to transaction */
   /* Set cell values (in memory) based on Column, Row, Rank, Transaction */
   key = rows_per_rank * cols_per_row * my_rank;
   for ( u = 0; u < cols_per_row; u++ ) {
      ret = H5Mset_ff( map_l_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      ASSERT_RET;
      ret = H5Mset_ff( map_p_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      ASSERT_RET;
      ret = H5Mset_ff( map_s_id, H5T_NATIVE_UINT64, &key, H5T_NATIVE_UINT64, &mbuf[u], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      ASSERT_RET;
      key++;
   }

   ret = H5Sclose( space_id ); ASSERT_RET;         // RUTH - likely keep this around
   free( mbuf );                                   // RUTH - likely keep this around

   /* Finish and close transaction */
   fprintf( stderr, "APP-r%d: tr %lu - Finish\n", my_rank, tr_num );
   ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5TRclose( tr_id ); ASSERT_RET;
   ret = H5Pclose( trspl_id ); ASSERT_RET;

   /* NEEDS more work if actually used at some point */
   MPI_Barrier( MPI_COMM_WORLD ); 
   if ( my_rank == 0 ) {
      version = tr_num;
      rc_idH = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      assert( rc_idH >= 0 ); assert ( version == tr_num );
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );
      if ( verbose ) print_container_contents( file_id, rc_idH, "/", my_rank );
      ret = H5RCrelease( rc_idH, H5_EVENT_STACK_NULL); ASSERT_RET;
      ret = H5RCclose( rc_idH ); ASSERT_RET;
      version = 2;
   }
   MPI_Barrier( MPI_COMM_WORLD );

#endif
