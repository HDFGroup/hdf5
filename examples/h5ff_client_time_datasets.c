/* 
 * h5ff_time_datasets.c: Do timings of various write / persist / prefetch / read operations for datasets
 * 
 *    /DL is dataset that is read back from transaction logs in BB
 *    /DP is dataset that is read back from prefetched replica in BB  [optional, based on runtime param]
 *    /DS is dataset that is read back from stored data (DAOS)        [optional, based on runtime param]
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include "mpi.h"
#include "hdf5.h"

/* define filename for this app, and max size after username prepended */
#define FILENAME_APP "eff_time_datasets.h5"
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
int prefetched_dset = 0;         /* Create DP - defaults to no */
int stored_dset = 0;             /* Create DS - defaults to no */
int num_iterations = 1;          /* Number of times to perform the commit/persist/evict/prefetch/read/evict cycle */
int verbose = 0;                 /* Verbose output - defaults to no */

/* global variables and macros used to make timing easier */
struct timeval tv_start;
struct timeval tv_end;
#define START_TIME gettimeofday( &tv_start, NULL )
#define END_TIME gettimeofday( &tv_end, NULL )
#define ELAPSED_TIME (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) 

/* function prototypes */
void print_container_contents( hid_t, hid_t, int );
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

   /* Replicas and related properties */
   hrpl_t   dset_p_replica;
   hid_t    dxpl_p_id;
   hid_t    dapl_p_id;

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
   uint64_t bytesPerCell, bytesPerRank, bytesPerDataset, bytesPerContainer;
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
      if ( prefetched_dset ) {
         fprintf( stderr, "APP-r0: /DP will be created\n" );
      }
      if ( stored_dset ) {
         fprintf( stderr, "APP-r0: /DS will be created\n" );
      }
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
   bytesPerRank = bytesPerCell * rows_per_rank * cols_per_row;          /* Bytes written by 1 rank to each dset */
   bytesPerDataset = bytesPerRank * comm_size;                          /* Bytes written by all ranks to each dset */
   bytesPerContainer = bytesPerDataset;                                 /* Bytes written by all ranks to all dsets */
   if ( prefetched_dset ) {
      bytesPerContainer += bytesPerDataset;
   }
   if ( stored_dset ) {
      bytesPerContainer += bytesPerDataset;
   }

   mbuf = (uint64_t *) calloc( (rows_per_rank * comm_size * cols_per_row), bytesPerCell );

   /****
    * Transaction 2: Rank 0 creates H5Objects in the container 
    ****/

   if ( my_rank == 0 ) {

      /* Acquire read context for CV 1 */
      version = 1;
      rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET; assert( version == 1 );
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

      /* Start a transaction with a single leader (the default) */
      tr_num = 2;
      fprintf( stderr, "APP-r%d: tr %lu - Start\n", my_rank, tr_num );
      tr_id = H5TRcreate( file_id, rc_id, tr_num ); assert( tr_id >= 0 );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Create Dataspace for Datasets */
      dset_size[0] = rows_per_rank * comm_size;
      dset_size[1] = cols_per_row;
      space_id = H5Screate_simple( 2, dset_size, dset_size ); assert( space_id >= 0 );

      /* Add updates to the transaction for Dataset creates */
      fprintf( stderr, "APP-r%d: tr %d - Create Dataset(s)\n", my_rank, tr_num );

      dset_l_id = H5Dcreate_ff( file_id, "DL", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );

      if ( prefetched_dset ) {
         dset_p_id = H5Dcreate_ff( file_id, "DP", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                   H5_EVENT_STACK_NULL ); assert( dset_p_id >= 0 );
      }

      if ( stored_dset ) {
         dset_s_id = H5Dcreate_ff( file_id, "DS", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                                   H5_EVENT_STACK_NULL ); assert( dset_s_id >= 0 );
      }

      ret = H5Sclose( space_id ); ASSERT_RET;      

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

      dset_l_id = H5Dopen_ff( file_id, "DL", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );

      if ( prefetched_dset ) {    
         dset_p_id = H5Dopen_ff( file_id, "DP", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_p_id >= 0 );
      }      
      if ( stored_dset ) {    
         dset_s_id = H5Dopen_ff( file_id, "DS", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_s_id >= 0 );
      }

   } else {
      if ( verbose ) print_container_contents( file_id, rc_id, my_rank );
   }

   /* Keep track of my_rank's neighbor */
   neighbor_rank = my_rank + 1;
   if ( neighbor_rank == comm_size ) {
      neighbor_rank = 0;
   }

   /* Get the dataspaces for the three datasets and make a copy of each */
   space_l_id = H5Dget_space( dset_l_id ); assert ( space_l_id >= 0 );
   neighbor_space_l_id = H5Scopy( space_l_id );

   if ( prefetched_dset ) { 
      space_p_id = H5Dget_space( dset_p_id ); assert ( space_p_id >= 0 ); 
      neighbor_space_p_id = H5Scopy( space_p_id );
   }
   if ( stored_dset ) {
      space_s_id = H5Dget_space( dset_s_id ); assert ( space_s_id >= 0 );
      neighbor_space_s_id = H5Scopy( space_s_id );
   }

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
   ret = H5Sselect_hyperslab( neighbor_space_l_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;

   if ( prefetched_dset ) { 
      ret = H5Sselect_hyperslab( space_p_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;
      ret = H5Sselect_hyperslab( neighbor_space_p_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;
   }

   if ( stored_dset ) {
      ret = H5Sselect_hyperslab( space_s_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;
      ret = H5Sselect_hyperslab( neighbor_space_s_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );  ASSERT_RET;
   }

   /* Create the memory dataspace for a rank - always first part of memory buffer */
   rank_mbuf_size[0] = rows_per_rank * cols_per_row;
   rank_space_id = H5Screate_simple( 1, rank_mbuf_size, rank_mbuf_size ); assert( rank_space_id >= 0 );

   /* Create property lists that will be used */
   if ( prefetched_dset ) {
      dxpl_p_id = H5Pcreate( H5P_DATASET_XFER );                              
      dapl_p_id = H5Pcreate( H5P_DATASET_ACCESS );
   }

   for ( iteration = 0; iteration < num_iterations; iteration++ ) {

      /**** 
       *  Cycle of:
       *    Tranasction N Start / Update / Commit: All ranks update Dataset(s) then commit 
       *    CV N Persist: Rank 0
       *    Prefetch: Rank 0
       *    Read rank data: All Ranks
       *    Read neighbor rank data: All Ranks
       *    Read all data: All Ranks
       ****/
      tr_num = tr_num + 1;
      num_tr_leaders = comm_size;

      /* Create a transaction and start it. */
      fprintf( stderr, "APP-r%d: iter %05d tr %lu - Transaction Start with %d leaders\n", 
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

      fprintf( stderr, "APP-r%d: iter %05d tr %lu - Add updates to transaction.\n", my_rank, iteration, tr_num );

      /* Add dataset updates to transaction */
      START_TIME;
      ret = H5Dwrite_ff( dset_l_id, H5T_NATIVE_UINT64, rank_space_id, space_l_id, H5P_DEFAULT, mbuf, tr_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %05d step 01: Time to add Write updates for /DL (%lu bytes): %lu usec\n", 
               my_rank, iteration, bytesPerRank, ELAPSED_TIME );

      if ( prefetched_dset ) {
         START_TIME;
         ret = H5Dwrite_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, space_p_id, H5P_DEFAULT, mbuf, tr_id, 
                            H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 02: Time to add Write updates for /DP (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
     }

      if ( stored_dset ) {
         START_TIME;
         ret = H5Dwrite_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, space_s_id, H5P_DEFAULT, mbuf, tr_id, 
                            H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 03: Time to add Write updates for /DS (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
      }

      /* Finish, (commit), and close transaction */
      fprintf( stderr, "APP-r%d: iter %05d tr %lu - Finish and Commit\n", my_rank, iteration, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      ret = H5Pclose( trspl_id ); ASSERT_RET;

      /* After commit completes on all ranks, acquire RC on rank 0 and create local handle for RC on other ranks */
      MPI_Barrier( MPI_COMM_WORLD ); 
      versionH = tr_num;
      if ( my_rank == 0 ) {
         rc_idH = H5RCacquire( file_id, &versionH, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         assert( rc_idH >= 0 ); assert ( versionH == tr_num );
         fprintf( stderr, "APP-r%d: iter %05d rc %lu - Acquired\n", my_rank, iteration, versionH );
      } else {
         rc_idH = H5RCcreate( file_id, versionH );  
         assert( rc_idH >= 0 ); assert ( versionH == tr_num );
         fprintf( stderr, "APP-r%d: iter %05d rc %lu - Created\n", my_rank, iteration, versionH );
      }
      MPI_Barrier( MPI_COMM_WORLD );      /* Wait to make sure rank 0 has acquired from IOD before proceeding */

      /* Release previous Read Context */
      if ( my_rank == 0 ) {
         ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
         fprintf( stderr, "APP-r%d: iter %05d rc %lu - Released\n", my_rank, iteration, version );
      }
      ret = H5RCclose( rc_id ); ASSERT_RET;
      fprintf( stderr, "APP-r%d: iter %05d rc %lu - Closed\n", my_rank, iteration, version );

      /* The just-acquired RC is now the primary rc_id we'll work with (for awhile) */
      rc_id = rc_idH;
      version = versionH;

      /* Rank 0:
        - persists the container
        - evicts DP & DS
        - prefetches DP
      */
      if ( my_rank == 0 ) {   
         if ( verbose ) print_container_contents( file_id, rc_id, my_rank );

         START_TIME;
         ret = H5RCpersist( rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 04: Time to Persist container (%lu bytes + KVs): %lu usec\n", 
                  my_rank, iteration, bytesPerContainer, ELAPSED_TIME );

         if ( prefetched_dset ) {
            START_TIME;
            ret = H5Devict_ff( dset_p_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
            END_TIME;
            fprintf( stderr, "APP-r%d: iter %05d step 05: Time to Evict /DP (%lu bytes): %lu usec\n", 
                     my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         }

         if ( stored_dset ) {
            START_TIME;
            ret = H5Devict_ff( dset_s_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
            END_TIME;
            fprintf( stderr, "APP-r%d: iter %05d step 06: Time to Evict /DS (%lu bytes): %lu usec\n", 
                     my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         }

         if ( prefetched_dset ) {
            START_TIME;
            ret = H5Dprefetch_ff( dset_p_id, rc_id, &dset_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
            END_TIME;
            fprintf( stderr, "APP-r%d: iter %05d step 07: Time to Prefetch (%lu bytes) /DP: %lu usec\n", 
                     my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         }

      } 


      if ( prefetched_dset ) { 
         MPI_Bcast( &dset_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
         ret = H5Pset_read_replica( dxpl_p_id, dset_p_replica ); ASSERT_RET;
         ret = H5Pset_evict_replica( dapl_p_id, dset_p_replica ); ASSERT_RET;
      } 

      MPI_Barrier( MPI_COMM_WORLD );      /* Make sure all are here before continuing, (esp in case of no prefetch Bcast) */

      /* 
       * All ranks read the same values they updated in last transaction
       */

      /* Datasets */
      START_TIME;
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, rank_space_id, space_l_id, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %05d step 08: Time to Read my rank entries for /DL (%lu bytes): %lu usec\n", 
               my_rank, iteration, bytesPerRank, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: my first updated value in /DL: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      if ( prefetched_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, space_p_id, dxpl_p_id, mbuf, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 09: Time to Read my rank entries for /DP (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: my first updated value in /DP: %09lu\n", my_rank, mbuf[0] ); 
         }
      }

      if ( stored_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, space_s_id, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 10: Time to Read my rank entries for /DS (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: my first updated value in /DS: %09lu\n", my_rank, mbuf[0] ); 
         }
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
      fprintf( stderr, "APP-r%d: iter %05d step 11: Time to Read neighbor's entries for /DL (%lu bytes): %lu usec\n", 
               my_rank, iteration, bytesPerRank, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: neighbor's first updated value in /DL: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      if ( prefetched_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, rank_space_id, neighbor_space_p_id, dxpl_p_id, mbuf, rc_id, 
                           H5_EVENT_STACK_NULL );
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 12: Time to Read neighbor's entries for /DP (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: neighbor's first updated value in /DP: %09lu\n", my_rank, mbuf[0]); 
         }
      }

      if ( stored_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, rank_space_id, neighbor_space_s_id, H5P_DEFAULT, mbuf, rc_id, 
                           H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 13: Time to Read neighbor's entries for /DS (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerRank, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: neighbor's first updated value in /DS: %09lu\n", my_rank, mbuf[0] ); 
         }
      }
      
      /* 
       * All ranks read all the values 
       */

      /* Datasets */
      START_TIME;
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
      END_TIME;
      fprintf( stderr, "APP-r%d: iter %05d step 14: Time to Read all entries for /DL (%lu bytes): %lu usec\n", 
               my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
      if ( verbose ) { 
         fprintf( stderr, "APP-r%d: first value in /DL: %09lu\n", my_rank, mbuf[0] ); 
      }
      
      if ( prefetched_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, dxpl_p_id, mbuf, rc_id, H5_EVENT_STACK_NULL );
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 15: Time to Read all entries for /DP (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: first value in /DP: %09lu\n", my_rank, mbuf[0] ); 
         }
      }

      if ( stored_dset ) {
         START_TIME;
         ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, H5S_ALL, H5S_ALL, H5P_DEFAULT, mbuf, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         fprintf( stderr, "APP-r%d: iter %05d step 16: Time to Read all entries for /DS (%lu bytes): %lu usec\n", 
                  my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         if ( verbose ) { 
            fprintf( stderr, "APP-r%d: first value in /DS: %09lu\n", my_rank, mbuf[0] ); 
         }
      }
         
      /* Rank 0 evicts the replica */
      MPI_Barrier( MPI_COMM_WORLD );      /* Make sure all ranks done reading the replica before evicting */
      if ( my_rank == 0 ) {
         if ( prefetched_dset ) {
            START_TIME;
            ret = H5Devict_ff( dset_p_id, version, dapl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  
            END_TIME;
            fprintf( stderr, "APP-r%d: iter %05d step 17: Time to Evict Replica of /DP (%lu bytes): %lu usec\n", 
                     my_rank, iteration, bytesPerDataset, ELAPSED_TIME );
         }
      }

   }

   /*
    * Wrap-up 
    */

   /* Print (if verbose) the final state */
   if ( my_rank == 0 ) {
      if ( verbose ) print_container_contents( file_id, rc_id, my_rank );
   }

   /* Close the dataset property lists */
   if ( prefetched_dset ) {
      ret = H5Pclose( dxpl_p_id ); ASSERT_RET;
      ret = H5Pclose( dapl_p_id ); ASSERT_RET;
   }

   /* Close the memory dataspace for the rank */
   ret = H5Sclose( rank_space_id ); ASSERT_RET;         

   fprintf( stderr, "APP-r%d: Closing H5Objects\n", my_rank );

   ret = H5Dclose_ff( dset_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Sclose( space_l_id ); ASSERT_RET;

   if ( prefetched_dset ) {
      ret = H5Dclose_ff( dset_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_p_id ); ASSERT_RET;
      ret = H5Sclose( neighbor_space_p_id ); ASSERT_RET;
   }
   if ( stored_dset ) {
      ret = H5Dclose_ff( dset_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_s_id ); ASSERT_RET;
      ret = H5Sclose( neighbor_space_s_id ); ASSERT_RET;
   }



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
 * "my_rank" used to identify the process doing the reading / printing.
 */
void
print_container_contents( hid_t file_id, hid_t rc_id, int my_rank )
{
   herr_t ret;
   uint64_t cv;
   hbool_t exists;
   char path_to_object[1024];
   char name[30];
   int i, d;

   char preface[128];    

   /* Get the container version for the read context */
   ret = H5RCget_version( rc_id, &cv ); ASSERT_RET;

   /* Set up the preface and adding version number */
   sprintf( preface, "APP-r%d: cv %d: ", my_rank, (int)cv );

   /* Start the printing */
   fprintf( stderr, "%s ----- Container Contents ------------\n", preface );

   for ( d = 0; d < 3; d++ ) {
      if ( d == 0 ) {
         sprintf( path_to_object, "%s", "/DL" );
      } else if ( d == 1 ) {
         sprintf( path_to_object, "%s", "/DP" );
      } else if ( d == 2 ) {
         sprintf( path_to_object, "%s", "/DS" );
      }

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
         data = (uint64_t *)calloc( totalSize, sizeof(uint64_t) );

         ret = H5Dread_ff( dset_id, H5T_NATIVE_UINT64, space_id, space_id, H5P_DEFAULT, data, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;

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
   }

   /* End printing */
   fprintf( stderr, "%s -----------------\n", preface );

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
            case 'p':   
               prefetched_dset = 1;
               break;
            case 's':   
               stored_dset = 1;
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
   printf( "Usage: %s [-c cols_per_row] [-r rows_per_rank] [-i num_iterations] [-p] [-s] [-v] \n", app  );
   printf( "\tc: number of columns in each row of created H5Datasets\n" );
   printf( "\tr: number of rows each rank writes in created H5Datasets (total rows = this * # of ranks)\n" );
   printf( "\ti: number of iterations to do update/commit/persist/evict/prefetch/read/evict\n" );
   printf( "\tp: create /DP dataset - data will be evicted, replica prefetched, reads from replica\n" );
   printf( "\ts: create /DS dataset - data will be evicted, reads from storage (DAOS)\n" );
   printf( "\tv: verbose output\n" );
}
