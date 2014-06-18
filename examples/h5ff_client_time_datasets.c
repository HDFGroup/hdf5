/* 
 * h5ff_time_datasets2.c: Do timings of various write / persist / prefetch / read operations for datasets
 * 
 *    /DL is dataset that is read back from transaction logs in BB
 *    /DP is dataset that is read back from prefetched replica in BB  
 *    /DS is dataset that is read back from stored data (DAOS)       
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include "mpi.h"
#include "hdf5.h"

/* define filename for this app, and max size after username prepended */
#define FILENAME_APP "eff_time_datasets2.h5"
#define FILENAME_SIZE 256

/* define default sizing parameters - can be changed via command line arguments */
//#define DEFAULT_COLS_PER_ROW 131072       /*  = 1 MiB/row => ( 1024*1024 /8 bytes per cell) */
//#define DEFAULT_ROWS_PER_WRITE 2          /*  = 2 MiB writes with 1MiB/row */
//#define DEFAULT_ROWS_PER_READ 2           /*  = 2 MiB writes with 1MiB/row */
//#define DEFAULT_ROWS_PER_RANK 5120        /*  = 5 GiB/rank with 1MiB/row */
/* Sized for testing - with verbose */
#define DEFAULT_COLS_PER_ROW 6
#define DEFAULT_ROWS_PER_WRITE 2  
#define DEFAULT_ROWS_PER_READ 1  
#define DEFAULT_ROWS_PER_RANK 4 

/* macros related to error reporting */
#define STATUS (ret >= 0) ? " " : " - FAILED"
#define ASSERT_RET assert( ret >= 0 )

/* option flags */
int cols_per_row = DEFAULT_COLS_PER_ROW;
int rows_per_write = DEFAULT_ROWS_PER_WRITE;      
int rows_per_read = DEFAULT_ROWS_PER_READ;      
int rows_per_rank = DEFAULT_ROWS_PER_RANK;      

int logged_dset = 0;             /* Create DL - defaults to no (unless neither DP nor DS are created ) */
int prefetched_dset = 0;         /* Create DP - defaults to no */
int stored_dset = 0;             /* Create DS - defaults to no */
int num_iterations = 1;          /* Number of times to perform the commit/persist/evict/prefetch/read/evict cycle */
int persist_rate = 1;            /* Rate at which transactions are persisted */
int enable_checksums = 0;        /* Enable checksums on raw data - defaults to no */
int detailed_timing = 0;         /* Print detailed timing information for each write & read - defaults to no */
int verbose = 0;                 /* Verbose output - defaults to no */

/* global variables and macros used to make timing easier */
struct timeval tv_start;
struct timeval tv_end;
#define START_TIME gettimeofday( &tv_start, NULL )
#define END_TIME gettimeofday( &tv_end, NULL )
#define ELAPSED_TIME (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) 

/* function prototypes */
void fill_stats( int*, ulong*, ulong*, ulong*, ulong );
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

   /* Dataspaces and hyperslab selection parameters */
   hid_t    space_l_id;
   hid_t    space_p_id;
   hid_t    space_s_id;
   hsize_t  rank_offset[2], rank_count[2];

   /* Datasets */
   hid_t    dset_l_id;
   hid_t    dset_p_id;
   hid_t    dset_s_id;
   hsize_t  dset_size[2];
   hid_t    space_id; 

   /* Data creation and transfer properties */
   hid_t    dcpl_id;
   hid_t    dxpl_l_id;
   hid_t    dxpl_p_id;
   hid_t    dxpl_s_id;

   /* Replicas */
   hrpl_t   dset_p_replica;

   /* Container Version & Read Contexts */
   uint64_t version, versionH;
   hid_t    rc_id, rc_idH;

   /* Transactions  */
   uint64_t tr_num;
   hid_t    tr_id;
   hid_t    trspl_id;
   int      num_tr_leaders;

   /* Memory buffer to hold data for 1 rank and memory hyperslab selection parameters */
   uint64_t *mbuf;
   uint64_t mbuf_offset;
   hsize_t  mbuf_size[1];
   hid_t    mem_space_id;
   hsize_t  mem_offset[1], mem_count[1];

   /* Variables used for reading data written by a neighbor rank */
   int      neighbor_rank;
   hid_t    neighbor_space_l_id;
   hid_t    neighbor_space_p_id;
   hid_t    neighbor_space_s_id; 
   hsize_t  neighbor_offset[2], neighbor_count[2];

   /* Misc */
   herr_t   ret;
   uint64_t u, r, c;
   uint64_t bytesPerCell, bytesPerWrite, bytesPerRead, bytesPerRank, bytesPerDataset, bytesPerContainer;
   double   megabytesPerWrite, megabytesPerRead, megabytesPerRank, megabytesPerDataset, megabytesPerContainer;
   int      rows_transferred;
   ulong    elapsed_time;
   double   rate;
   ulong    e_min, e_max, e_sum;
   int      cnt;
   
   int      i;
   int      iteration;
   int      iter_since_last_persist;

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

   /* Parse command-line options controlling behavior */
   if ( parse_options( argc, argv, my_rank, comm_size ) != 0 ) {
      exit( 1 );
   }    
   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: Number of MPI processes = %d\n", my_rank, comm_size );
      fprintf( stderr, "APP-r%d: Datasets will have %d columns.\n", my_rank, cols_per_row );
      fprintf( stderr, "APP-r%d: Datasets will be written %d rows at a time.\n", my_rank, rows_per_write );
      fprintf( stderr, "APP-r%d: Datasets will be read %d rows at a time.\n", my_rank, rows_per_read );
      fprintf( stderr, "APP-r%d: Datasets will have %d rows (%d per rank).\n", my_rank, (rows_per_rank*comm_size), rows_per_rank );
      fprintf( stderr, "APP-r%d: There will be %d iterations.\n", my_rank, num_iterations );
      fprintf( stderr, "APP-r%d: Persists will occur every %d iterations.\n", my_rank, persist_rate );
      if ( ( prefetched_dset==0 ) && ( stored_dset==0 ) ) {       /* Make sure something is created! */
         logged_dset = 1;
      }
      if ( logged_dset ) {
         fprintf( stderr, "APP-r%d: /DL will be created\n", my_rank );
      }
      if ( prefetched_dset ) {
         fprintf( stderr, "APP-r%d: /DP will be created\n", my_rank );
      }
      if ( stored_dset ) {
         fprintf( stderr, "APP-r%d: /DS will be created\n", my_rank );
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
   bytesPerWrite = bytesPerCell * rows_per_write * cols_per_row;        /* Bytes written in a single operation*/
   bytesPerRead = bytesPerCell * rows_per_read * cols_per_row;          /* Bytes read in a single operations */
   bytesPerRank = bytesPerCell * rows_per_rank * cols_per_row;          /* Bytes per rank to each dset */
   bytesPerDataset = bytesPerRank * comm_size;                          /* Bytes written by all ranks to each dset */
   bytesPerContainer = bytesPerDataset;                                 /* Bytes written by all ranks to all dsets */
   if ( prefetched_dset ) {
      bytesPerContainer += bytesPerDataset;
   }
   if ( stored_dset ) {
      bytesPerContainer += bytesPerDataset;
   }

   megabytesPerWrite = (double)bytesPerWrite/(1024*1024);
   megabytesPerRead = (double)bytesPerRead/(1024*1024);
   megabytesPerRank = (double)bytesPerRank/(1024*1024);
   megabytesPerDataset = (double)bytesPerDataset/(1024*1024);
   megabytesPerContainer = (double)bytesPerContainer/(1024*1024);

   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: Bytes per Cell = %lu\n", my_rank, bytesPerCell );
      fprintf( stderr, "APP-r%d: Bytes per Write  = %lu\n", my_rank, bytesPerWrite );
      fprintf( stderr, "APP-r%d: Bytes per Read  = %lu\n", my_rank, bytesPerRead );
      fprintf( stderr, "APP-r%d: Bytes per Rank  = %lu\n", my_rank, bytesPerRank );
      fprintf( stderr, "APP-r%d: Bytes per Dataset  = %lu\n", my_rank, bytesPerDataset );
      fprintf( stderr, "APP-r%d: Bytes per Container  = %lu\n", my_rank, bytesPerContainer );
      fprintf( stderr, "APP-r%d: MiB per Write = %f\n", my_rank, megabytesPerWrite );
      fprintf( stderr, "APP-r%d: MiB per Read = %f\n", my_rank, megabytesPerRead );
      fprintf( stderr, "APP-r%d: MiB per Rank = %f\n", my_rank, megabytesPerRank );
      fprintf( stderr, "APP-r%d: MiB per Dataset = %f\n", my_rank, megabytesPerDataset );
      fprintf( stderr, "APP-r%d: MiB per Container = %f\n", my_rank, megabytesPerContainer );
   }

   mbuf = (uint64_t *) calloc( bytesPerRank, 1 );
   if ( mbuf == NULL ) {
      fprintf( stderr, "APP-r%d: calloc failed when trying to allocate %lu bytes\n", my_rank, bytesPerRank );
      exit( -1 );
   }

   /****
    * Transaction 2: Rank 0 creates H5Objects in the container 
    ****/

   tr_num = 2;                /* Set tr_num here so all ranks can later access the value */
   if ( my_rank == 0 ) {

      /* Acquire read context for CV 1 */
      version = 1;
      rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( rc_id >= 0); assert( version == 1 );
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

      /* Start a transaction with a single leader (the default) */
      fprintf( stderr, "APP-r%d: tr %lu - Start\n", my_rank, tr_num );
      tr_id = H5TRcreate( file_id, rc_id, tr_num ); assert( tr_id >= 0 );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Set Dataset Creation Property List to control use of checksums at IOD level */
      dcpl_id = H5Pcreate( H5P_DATASET_CREATE );
      if ( enable_checksums ) {
         /* enabled by default */
      } else {
         H5Pset_ocpl_enable_checksum( dcpl_id, 0 );
      }

      /* Create Dataspace for Datasets */
      dset_size[0] = rows_per_rank * comm_size;
      dset_size[1] = cols_per_row;
      space_id = H5Screate_simple( 2, dset_size, dset_size ); assert( space_id >= 0 );

      /* Add updates to the transaction for Dataset creates */
      fprintf( stderr, "APP-r%d: tr %d - Create Dataset(s)\n", my_rank, tr_num );

      if ( logged_dset ) {
         dset_l_id = H5Dcreate_ff( file_id, "DL", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT, tr_id,
                                   H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );
      }

      if ( prefetched_dset ) {
         dset_p_id = H5Dcreate_ff( file_id, "DP", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT, tr_id,
                                   H5_EVENT_STACK_NULL ); assert( dset_p_id >= 0 );
      }

      if ( stored_dset ) {
         dset_s_id = H5Dcreate_ff( file_id, "DS", H5T_NATIVE_UINT64, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT, tr_id,
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

      if ( logged_dset ) {    
         dset_l_id = H5Dopen_ff( file_id, "DL", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_l_id >= 0 );
      }
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

   /* Get the dataspaces for the three datasets and make a copy of each for neighbor's */
   if ( logged_dset ) {
      space_l_id = H5Dget_space( dset_l_id ); assert ( space_l_id >= 0 );
      neighbor_space_l_id = H5Scopy( space_l_id );
   }
   if ( prefetched_dset ) { 
      space_p_id = H5Dget_space( dset_p_id ); assert ( space_p_id >= 0 ); 
      neighbor_space_p_id = H5Scopy( space_p_id );
   }
   if ( stored_dset ) {
      space_s_id = H5Dget_space( dset_s_id ); assert ( space_s_id >= 0 );
      neighbor_space_s_id = H5Scopy( space_s_id );
   }


   /* Create the memory dataspace for one rank worth of data */
   mbuf_size[0] = rows_per_rank * cols_per_row;
   mem_space_id = H5Screate_simple( 1, mbuf_size, mbuf_size ); assert( mem_space_id >= 0 );

   /* Create property lists that will be used */
   if ( logged_dset ) {
      dxpl_l_id = H5Pcreate( H5P_DATASET_XFER );                              
      if ( enable_checksums ) {
         ret = H5Pset_rawdata_integrity_scope( dxpl_l_id, H5_CHECKSUM_ALL ); ASSERT_RET;
      } else {
         ret = H5Pset_rawdata_integrity_scope( dxpl_l_id, H5_CHECKSUM_NONE ); ASSERT_RET;
      }
   }

   if ( prefetched_dset ) {
      dxpl_p_id = H5Pcreate( H5P_DATASET_XFER );                              
      if ( enable_checksums ) {
         ret = H5Pset_rawdata_integrity_scope( dxpl_p_id, H5_CHECKSUM_ALL ); ASSERT_RET;
      } else {
         ret = H5Pset_rawdata_integrity_scope( dxpl_p_id, H5_CHECKSUM_NONE ); ASSERT_RET;
      }
   }

   if ( stored_dset ) {
      dxpl_s_id = H5Pcreate( H5P_DATASET_XFER );                              
      if ( enable_checksums ) {
         ret = H5Pset_rawdata_integrity_scope( dxpl_s_id, H5_CHECKSUM_ALL ); ASSERT_RET;
      } else {
         ret = H5Pset_rawdata_integrity_scope( dxpl_s_id, H5_CHECKSUM_NONE ); ASSERT_RET;
      }
   }

   iter_since_last_persist = 0;

   for ( iteration = 1; iteration <= num_iterations; iteration++ ) {

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
            mbuf[u] = ( my_rank*10000 + r*1000 + c*100 + tr_num ) ;
            u++;
         }
      }
   
      fprintf( stderr, "APP-r%d: iter %05d tr %lu - Add updates to transaction.\n", my_rank, iteration, tr_num );

      /* Add dataset updates to transaction */
      if ( logged_dset ) {
         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_write * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_write;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_write)  <= rows_per_rank ) {        /* final rows left unfilled if not exact multiple */
            /* Select the hyperslabs for this write */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_l_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dwrite_ff( dset_l_id, H5T_NATIVE_UINT64, mem_space_id, space_l_id, dxpl_l_id, &mbuf[mbuf_offset], 
                               tr_id, H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerWrite/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 01: Time to add Write updates for /DL (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerWrite, elapsed_time, rate );
            }

            /* Advance in preparation for next write */
            rows_transferred += rows_per_write;
            rank_offset[0] += rows_per_write;
            mem_offset[0] += ( rows_per_write * cols_per_row );
            mbuf_offset += ( rows_per_write * cols_per_row );
         }
         if ( cnt > 0 ) {
            rate = megabytesPerWrite/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 01: Average Time to add Write updates for /DL (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerWrite, e_min, e_max, e_sum/cnt, rate );
         }
      }
   
      if ( prefetched_dset ) {
         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_write * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_write;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_write)  <= rows_per_rank ) {        /* final rows left unfilled if not exact multiple */
            /* Select the hyperslabs for this write */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_p_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dwrite_ff( dset_p_id, H5T_NATIVE_UINT64, mem_space_id, space_p_id, dxpl_p_id, &mbuf[mbuf_offset], tr_id, 
                               H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerWrite/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 02: Time to add Write updates for /DP (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerWrite, elapsed_time, rate );
            }

            /* Advance in preparation for next write */
            rows_transferred += rows_per_write;
            rank_offset[0] += rows_per_write;
            mem_offset[0] += ( rows_per_write * cols_per_row );
            mbuf_offset += ( rows_per_write * cols_per_row );
         }
         if ( cnt > 0 ) {
            rate = megabytesPerWrite/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 02: Average Time to add Write updates for /DP (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerWrite, e_min, e_max, e_sum/cnt, rate );
         }
      }
   
      if ( stored_dset ) {
         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_write * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_write;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;
   
         while ( (rows_transferred+rows_per_write)  <= rows_per_rank ) {        /* final rows left unfilled if not exact multiple */
            /* Select the hyperslabs for this write */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_s_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dwrite_ff( dset_s_id, H5T_NATIVE_UINT64, mem_space_id, space_s_id, dxpl_s_id, &mbuf[mbuf_offset], tr_id, 
                               H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerWrite/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 03: Time to add Write updates for /DS (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerWrite, elapsed_time, rate );
            }

            /* Advance in preparation for next write */
            rows_transferred += rows_per_write;
            rank_offset[0] += rows_per_write;
            mem_offset[0] += ( rows_per_write * cols_per_row );
            mbuf_offset += ( rows_per_write * cols_per_row );
         }
         if ( cnt > 0 ) {
            rate = megabytesPerWrite/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 03: Average Time to add Write updates for /DS (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerWrite, e_min, e_max, e_sum/cnt, rate );
        } 
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

      iter_since_last_persist++;             /* We've done the commit of a TR, so advance iter ctr */

      /* Rank 0:
         if this is a persist iteration:
         - persists the container
         - evicts DP & DS
         - prefetches DP
      */
      if ( my_rank == 0 ) {   
         if ( verbose ) print_container_contents( file_id, rc_id, my_rank );

         if ( iter_since_last_persist == persist_rate )  {   
            START_TIME;
            ret = H5RCpersist( rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            rate = megabytesPerContainer/((double)elapsed_time/1000000);
            fprintf( stderr, "APP-r%d: iter %05d step 04: Time to Persist container (%lu bytes + KVs): %lu usec - %f MiB/sec\n", 
                     my_rank, iteration, bytesPerContainer, elapsed_time, rate );

            if ( prefetched_dset ) {
               START_TIME;
               ret = H5Devict_ff( dset_p_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
               END_TIME;
               elapsed_time = ELAPSED_TIME;
               rate = megabytesPerDataset/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 05: Time to Evict /DP (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerDataset, elapsed_time, rate );
            }

            if ( stored_dset ) {
               START_TIME;
               ret = H5Devict_ff( dset_s_id, version, H5P_DEFAULT, H5_EVENT_STACK_NULL );  ASSERT_RET;
               END_TIME;
               elapsed_time = ELAPSED_TIME;
               rate = megabytesPerDataset/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 06: Time to Evict /DS (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerDataset, elapsed_time, rate );
            }

            if ( prefetched_dset ) {
               START_TIME;
               ret = H5Dprefetch_ff( dset_p_id, rc_id, &dset_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
               END_TIME;
               elapsed_time = ELAPSED_TIME;
               rate = megabytesPerDataset/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 07: Time to Prefetch (%lu bytes) /DP: %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerDataset, elapsed_time, rate );
            }
         } 
      }
  
      /* We only prefetch if a persist occurred */
      if ( ( prefetched_dset ) && ( iter_since_last_persist == persist_rate ) ) { 
         MPI_Bcast( &dset_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
         ret = H5Pset_dxpl_replica( dxpl_p_id, dset_p_replica ); ASSERT_RET;
      } 

      MPI_Barrier( MPI_COMM_WORLD );      /* Make sure all are here before continuing, (esp in case of no prefetch Bcast) */

      /* 
       * All ranks read 'their' part of the written dataset (s)
       * For /DL, read every iteration.
       * For /DP and DS, only read on persisted iterations.
       */

      if ( logged_dset ) {
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_read;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_l_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, mem_space_id, space_l_id, dxpl_l_id, &mbuf[mbuf_offset], rc_id, 
                              H5_EVENT_STACK_NULL );
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 08: Time to Read my entries for /DL (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: first value read in /DL: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {      /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            rank_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );
         }

         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 08: Average Time to Read my entries for /DL (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }
      }
         
      if ( ( prefetched_dset ) && ( iter_since_last_persist == persist_rate ) ) { 
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_read;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_p_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, mem_space_id, space_p_id, dxpl_p_id, &mbuf[mbuf_offset], rc_id, 
                              H5_EVENT_STACK_NULL );
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 09: Time to Read my entries for /DP (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: first value read in /DP: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {         /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            rank_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );

         }
         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 09: Average Time to Read my entries for /DP (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }
      }
         
      if ( ( stored_dset ) && ( iter_since_last_persist == persist_rate ) ) { 
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         rank_offset[0] = my_rank * rows_per_rank;
         rank_offset[1] = 0;
         rank_count[0] = rows_per_read;
         rank_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( space_s_id, H5S_SELECT_SET, rank_offset, NULL, rank_count, NULL );  ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, mem_space_id, space_s_id, dxpl_s_id, &mbuf[mbuf_offset], rc_id, 
                              H5_EVENT_STACK_NULL );
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, "APP-r%d: iter %05d step 10: Time to Read my entries for /DS (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: first value read in /DS: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {         /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            rank_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );
         }

         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 10: Average Time to Read my entries for /DS (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }
      }

      /* 
       * All ranks read the values "next rank" neighbor updated in last transaction
       * For /DL, read every iteration.
       * For /DP and DS, only read on persisted iterations.
       */

      if ( logged_dset  && ( my_rank != neighbor_rank ) ) {
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         neighbor_offset[0] = neighbor_rank * rows_per_rank;
         neighbor_offset[1] = 0;
         neighbor_count[0] = rows_per_read;
         neighbor_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( neighbor_space_l_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );
            ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_l_id, H5T_NATIVE_UINT64, mem_space_id, neighbor_space_l_id, dxpl_l_id, &mbuf[mbuf_offset], rc_id,
                              H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 11: Time to Read neighbor's entries for /DL (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: neighbor's first value read in /DL: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {         /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            neighbor_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );
         }

         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 11: Average Time to Read neighbor's entries for /DL (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }
      }
         
      if ( ( prefetched_dset ) && ( iter_since_last_persist == persist_rate )  && ( my_rank != neighbor_rank ) ) { 
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         neighbor_offset[0] = neighbor_rank * rows_per_rank;
         neighbor_offset[1] = 0;
         neighbor_count[0] = rows_per_read;
         neighbor_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( neighbor_space_p_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );
            ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_p_id, H5T_NATIVE_UINT64, mem_space_id, neighbor_space_p_id, dxpl_p_id, &mbuf[mbuf_offset], rc_id,
                              H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 12: Time to Read neighbor's entries for /DP (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: neighbor's first value read in /DP: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {         /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            neighbor_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );
         }

         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 12: Average Time to Read neighbor's entries for /DP (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }
      }
         
      if ( ( stored_dset ) && ( iter_since_last_persist == persist_rate ) && ( my_rank != neighbor_rank ) ) { 
         /* Zero the memory buf so we can be sure we are reading */
         if ( verbose ) {
            bzero( (void*) mbuf, bytesPerRank );
         }

         cnt = e_min = e_max = e_sum = 0;
         /* Set parameters that will be used to select the initial hyperslabs for this rank */
         mem_offset[0] = 0;
         mem_count[0] = rows_per_read * cols_per_row;
         mbuf_offset = 0;
         neighbor_offset[0] = neighbor_rank * rows_per_rank;
         neighbor_offset[1] = 0;
         neighbor_count[0] = rows_per_read;
         neighbor_count[1] = cols_per_row;
         rows_transferred = 0;

         while ( (rows_transferred+rows_per_read)  <= rows_per_rank ) {        /* final rows left unread if not exact multiple */
            /* Select the hyperslabs for this read */
            ret = H5Sselect_hyperslab( mem_space_id, H5S_SELECT_SET, mem_offset, NULL, mem_count, NULL ); ASSERT_RET;
            ret = H5Sselect_hyperslab( neighbor_space_s_id, H5S_SELECT_SET, neighbor_offset, NULL, neighbor_count, NULL );
            ASSERT_RET;

            START_TIME;
            ret = H5Dread_ff( dset_s_id, H5T_NATIVE_UINT64, mem_space_id, neighbor_space_s_id, dxpl_s_id, &mbuf[mbuf_offset], rc_id,
                              H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            fill_stats( &cnt, &e_min, &e_max, &e_sum, elapsed_time );
            if ( detailed_timing ) {
               rate = megabytesPerRead/((double)elapsed_time/1000000);
               fprintf( stderr, 
                        "APP-r%d: iter %05d step 13: Time to Read neighbor's entries for /DS (%lu bytes): %lu usec - %f MiB/sec\n", 
                        my_rank, iteration, bytesPerRead, elapsed_time, rate );
            }

            fprintf( stderr, "APP-r%d: neighbor's first value read in /DS: %09lu\n", my_rank, mbuf[mbuf_offset] ); 
            if ( ( verbose ) && ( my_rank == 0 ) ) {         /* print all mbuf on rank 0 to confirm correctness while developing */
               u = 0;
               for ( r = 0; r < rows_per_rank; r++ ) {
                  for ( c = 0; c < cols_per_row; c++ ) {
                     fprintf( stderr, "%lu   ", mbuf[u] );
                     u++;
                  }
                  fprintf( stderr, "\n" );
               }
            }

            /* Advance in preparation for next read */
            rows_transferred += rows_per_read;
            neighbor_offset[0] += rows_per_read;
            mem_offset[0] += ( rows_per_read * cols_per_row );
            mbuf_offset += ( rows_per_read * cols_per_row );
         }

         if ( cnt > 0 ) {
            rate = megabytesPerRead/((double)(e_sum/cnt)/1000000);
            fprintf( stderr, 
                     "APP-r%d: iter %05d step 13: Average Time to Read neighbor's entries for /DS (%lu bytes): Min: %lu usec; Max: %lu usec; Avg: %lu usec - Avg %f MiB/sec\n", 
                     my_rank, iteration, bytesPerRead, e_min, e_max, e_sum/cnt, rate );
         }

      }

      /* For persisted iterations, reset the counter & if we prefetched a replica, evict it */
      if ( iter_since_last_persist == persist_rate ) { 
         iter_since_last_persist = 0;

         MPI_Barrier( MPI_COMM_WORLD );      /* Make sure all ranks done reading the replica before evicting */

         /* Rank 0 evicts the replica */
         if  ( ( my_rank == 0 ) && ( prefetched_dset ) ) {
            START_TIME;
            ret = H5Devict_ff( dset_p_id, version, dxpl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  
            END_TIME;
            elapsed_time = ELAPSED_TIME;
            rate = megabytesPerDataset/((double)elapsed_time/1000000);
            fprintf( stderr, "APP-r%d: iter %05d step 17: Time to Evict Replica of /DP (%lu bytes): %lu usec - %f MiB/sec\n", 
                     my_rank, iteration, bytesPerDataset, elapsed_time, rate );
         }
      }
   }

   /*
    * Wrap-up 
    */

   if ( my_rank == 0 ) {
      if ( verbose ) print_container_contents( file_id, rc_id, my_rank );
   }

   fprintf( stderr, "APP-r%d: Closing H5Objects\n", my_rank );

   ret = H5Sclose( mem_space_id ); ASSERT_RET;         

   if ( logged_dset ) {
      ret = H5Dclose_ff( dset_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_l_id ); ASSERT_RET;
      ret = H5Sclose( neighbor_space_l_id ); ASSERT_RET;
      ret = H5Pclose( dxpl_l_id ); ASSERT_RET;
   }
   if ( prefetched_dset ) {
      ret = H5Dclose_ff( dset_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_p_id ); ASSERT_RET;
      ret = H5Sclose( neighbor_space_p_id ); ASSERT_RET;
      ret = H5Pclose( dxpl_p_id ); ASSERT_RET;
   }
   if ( stored_dset ) {
      ret = H5Dclose_ff( dset_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_s_id ); ASSERT_RET;
      ret = H5Sclose( neighbor_space_s_id ); ASSERT_RET;
      ret = H5Pclose( dxpl_s_id ); ASSERT_RET;
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
 * Helper function to keep track of timing info
 */
void
fill_stats( int* cnt, ulong* min, ulong* max, ulong* sum, ulong elapsed ) 
{
   if ( *min == 0 ) {
      *min = elapsed;
      *max = elapsed;
   } else {
      if ( elapsed < *min ) {
         *min = elapsed;
      }
      if ( elapsed > *max ) {
         *max = elapsed;
      }
   }
   *sum += elapsed;
   *cnt += 1;
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
         hid_t dxpl_id;
         uint64_t *data;              
         uint64_t u, e;

         dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         assert( dset_id >= 0 );

         space_id = H5Dget_space( dset_id ); assert ( space_id >= 0 );
         nDims = H5Sget_simple_extent_dims( space_id, current_size, max_size ); 
         assert( nDims == 2 );

         totalSize = current_size[0] * current_size[1];
         data = (uint64_t *)calloc( totalSize, sizeof(uint64_t) ); assert( data != NULL );

         dxpl_id = H5Pcreate( H5P_DATASET_XFER );                              
         if ( enable_checksums ) {
            ret = H5Pset_rawdata_integrity_scope( dxpl_id, H5_CHECKSUM_ALL ); ASSERT_RET;
         } else {
            ret = H5Pset_rawdata_integrity_scope( dxpl_id, H5_CHECKSUM_NONE ); ASSERT_RET;
         }

         ret = H5Dread_ff( dset_id, H5T_NATIVE_UINT64, space_id, space_id, dxpl_id, data, rc_id, H5_EVENT_STACK_NULL ); 
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
         ret = H5Pclose( dxpl_id ); ASSERT_RET;
         ret = H5Sclose( space_id ); ASSERT_RET;
         ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
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
            case 'w':
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "Error: No number specified after -w option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  rows_per_write = atoi( *argv );
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
                  rows_per_read = atoi( *argv );
               }
               break;
            case 'b':
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "Error: No number specified after -b option.\n" );
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
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "APP-r0: Error: No number specified after -p option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  persist_rate = atoi( *argv );
               }
               break;
            case 'e':   
               enable_checksums = 1;
               break;
            case 'L':   
               logged_dset = 1;
               break;
            case 'P':   
               prefetched_dset = 1;
               break;
            case 'S':   
               stored_dset = 1;
               break;
            case 'd':
               detailed_timing = 1;
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
   printf( "Usage: %s [-c cols_per_row] [-w rows_per_write] [-r rows_per_read] [-b rows_per_rank] [-i num_iterations] [-p persist_rate] [-e] [-L] [-P] [-S] [-d] [-v] \n", app  );
   printf( "\tc: number of columns per row (each cell is 8 bytes)\n" );
   printf( "\tw: number of rows per write per rank\n" );
   printf( "\tr: number of rows per read per rank\n" );
   printf( "\tb: number of rows in each rank's block (if not multiple of w or r, may not be fully written or read)\n" );
   printf( "\ti: number of iterations to do writes/commit/[persist/evict/prefetch/reads/evict]\n" );
   printf( "\tp: persist rate; for /DP and /DS, also impacts evict/prefetch/read/evict\n" );
   printf( "\te: enable checksums on raw data in H5Datasets\n" );
   printf( "\tL: create /DL dataset - data will not be evicted (created by default if no /DP nor /DS)\n" );
   printf( "\tP: create /DP dataset - data will be evicted, replica prefetched, reads from replica\n" );
   printf( "\tS: create /DS dataset - data will be evicted, reads from storage (DAOS)\n" );
   printf( "\td: print detailed timing information for each read and write operation\n" );
   printf( "\tv: verbose output of data values - intended for use only with small arrays and few iterations\n" );
}
