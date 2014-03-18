/* 
 * h5ff_client_M7.2-pep_demo.c: Demo Persist / Evict / Prefetch
 * Part of Milestone 7.2 - Functionality of Burst Buffer.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include "mpi.h"
#include "hdf5.h"

/* macros related to error reporting */
#define STATUS (ret >= 0) ? "OK" : "FAILED"
#define ASSERT_RET assert( ret >= 0 )

/* option flags */
int use_daos_lustre = 0;   // Use DAOS Lustre - defaults to no
int verbose = 0;           // Verbose output - defaults to no

/* prototypes for helper functions */
void create_group( hid_t, const char*, hid_t, const char*, int );
void create_dataset( hid_t, const char*, hid_t, const char*, int, int );
void create_map( hid_t, const char*, hid_t, const char*, int, int );
void create_committed_datatype( hid_t, const char*, hid_t, const char*, int );
void update_dataset( hid_t, const char*, hid_t, hid_t, const char*, int, int );
void update_map( hid_t, const char*, hid_t, hid_t, const char*, int, int );
void evict_group_members_updates( hid_t, hid_t, const char*, int );
void print_container_contents( hid_t, hid_t, const char*, int );
int  parse_options( int, char**, int );

/**************************************************************************************************************/
int main( int argc, char **argv ) {

   const char file_name[]="eff_pep.h5";

   int my_rank, comm_size;
   int provided;

   hid_t  fapl_id;
   hid_t  file_id;
   herr_t ret;

   hid_t         es_id;   
   H5ES_status_t status;
   size_t        num_events;

   uint64_t version;
   hid_t    rc_id1, rc_id2, rc_id3, rc_id4, rc_id5, rc_id;

   uint64_t tr_num;
   hid_t    tr_id;
   hid_t    trspl_id;
   int      num_tr_leaders;

   struct timeval tv_start;
   struct timeval tv_end;
   int    i;

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
   fprintf( stderr, "APP-r%d: Number of MPI processes = %d\n", my_rank, comm_size );

   /* Parse command-line options controlling behavior */
   if ( parse_options( argc, argv, my_rank ) != 0 ) {
      exit( 1 );
   }    

   /* Initialize the EFF stack, starting FS client, registering HDF5 VOL calls, requesting IOD init */
   fprintf( stderr, "APP-r%d: Initialize EFF stack\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Create an event stack for managing asynchronous requests. */
   es_id = H5EScreate();  assert( es_id >= 0 );

   /* Specify the IOD VOL plugin should be used and create H5File (EFF container) */
   fprintf( stderr, "APP-r%d: Create the container %s\n", my_rank, file_name );
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id >= 0 );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); ASSERT_RET;
   file_id = H5Fcreate_ff( file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );

   /* Get read context */
   version = 1;
   rc_id1 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET; assert( version == 1 );

   /****
    * Transaction 2: Rank 0 creates three H5Groups in the H5File 
    ****/

   if ( my_rank == 0 ) {
      tr_num = 2;

      /* Create a local transaction for transaction number and start it with a single tr leader (the default). */
      fprintf( stderr, "APP-r%d: Create and start tr %lu\n", my_rank, tr_num );
      tr_id = H5TRcreate( file_id, rc_id1, tr_num ); assert( tr_id >= 0 );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Add updates to the transaction */
      create_group( file_id, "G-logged", tr_id, "/", my_rank );
      create_group( file_id, "G-prefetched", tr_id, "/", my_rank );
      create_group( file_id, "G-stored", tr_id, "/", my_rank );

      /* Finish, commit, and close transaction */
      fprintf( stderr, "APP-r%d: Finish, commit, and close tr %lu\n", my_rank, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
   }

   /* Acquire a read handle for container version and create a read context. */
   version = 2;
   if (verbose) fprintf( stderr, "APP-r%d: Try to acquire read context %lu\n", my_rank, version );
   H5E_BEGIN_TRY { 
      rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      while ( rc_id2 < 0 ) {
         if (verbose) fprintf( stderr, "APP-r%d: Failed to acquire read context %lu; sleep then retry\n", my_rank, version );
         sleep( 1 );
         rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      }
   } H5E_END_TRY;
   assert( rc_id2 >= 0 ); assert ( version == 2 );
   fprintf( stderr, "APP-r%d: Acquired read context %lu\n", my_rank, version );

   /* Release the read handle and close read context on previous CV  */
   fprintf( stderr, "APP-r%d: Release read context 1\n", my_rank );
   ret = H5RCrelease( rc_id1, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id1 ); ASSERT_RET;

   if (verbose) print_container_contents( file_id, rc_id2, "/", my_rank );

   /****
    * Transaction 3 - In each of the 3 Groups, create "D"(rnk 0), "M"(rnk 1 or 0), "T" (rnk 2 or 1 or 0)
    ****/

   if ( my_rank < 3 ) {
      tr_num = 3;
      num_tr_leaders = ( (comm_size < 4) ? comm_size : 3 );

      /* Create a local transaction for transaction number and start it. */
      fprintf( stderr, "APP-r%d: Create and start tr %lu with %d transaction leaders\n", my_rank, tr_num, num_tr_leaders );
      tr_id = H5TRcreate( file_id, rc_id2, tr_num ); assert( tr_id >= 0 );
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
      ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
      ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      hid_t glog_id, gpre_id, gsto_id;
      char glog_path[128], gpre_path[128], gsto_path[128];

      /* Open the groups where objects will be created and set pathnames used in status messages */
      glog_id = H5Gopen_ff( file_id, "G-logged", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( glog_id >= 0 );
      gpre_id = H5Gopen_ff( file_id, "G-prefetched", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( gpre_id >= 0 );
      gsto_id = H5Gopen_ff( file_id, "G-stored", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( gsto_id >= 0 );
      sprintf( glog_path, "/G-logged/" );
      sprintf( gpre_path, "/G-prefetched/" );
      sprintf( gsto_path, "/G-stored/" );

      /* Add updates to the transaction */
      if ( my_rank == 0 ) {
         create_dataset( glog_id, "D", tr_id, glog_path, my_rank, 1 );
         create_dataset( gpre_id, "D", tr_id, gpre_path, my_rank, 2 );
         create_dataset( gsto_id, "D", tr_id, gsto_path, my_rank, 3 );
      }  
      if ( ( my_rank == 1 ) || ( ( my_rank == 0 ) && ( comm_size == 1 ) ) )  {
         create_map( glog_id, "M", tr_id, glog_path, my_rank, 1 );
         create_map( gpre_id, "M", tr_id, gpre_path, my_rank, 2 );
         create_map( gsto_id, "M", tr_id, gsto_path, my_rank, 3 );
      }
      if ( ( my_rank == 2 )  || ( ( my_rank == 1 ) && ( comm_size == 2 ) )  || ( ( my_rank == 0 ) && ( comm_size == 1 ) ) ) {
         create_committed_datatype( glog_id, "T", tr_id, glog_path, my_rank );
         create_committed_datatype( gpre_id, "T", tr_id, gpre_path, my_rank );
         create_committed_datatype( gsto_id, "T", tr_id, gsto_path, my_rank );
      }

      /* Close the groups */
      ret = H5Gclose_ff ( glog_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Gclose_ff ( gpre_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Gclose_ff ( gsto_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Finish, commit, and close transaction */
      fprintf( stderr, "APP-r%d: Finish, commit, and close tr %lu\n", my_rank, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      ret = H5Pclose( trspl_id ); ASSERT_RET;
   }

   /* Acquire a read handle for container version and create a read context. */
   version = 3;
   if (verbose) fprintf( stderr, "APP-r%d: Try to acquire read context %lu\n", my_rank, version );
   H5E_BEGIN_TRY { 
      rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      while ( rc_id3 < 0 ) {
         if (verbose) fprintf( stderr, "APP-r%d: Failed to acquire read context %lu; sleep then retry\n", my_rank, version );
         sleep( 1 );
         rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      }
   } H5E_END_TRY;
   assert( rc_id3 >= 0 ); assert ( version == 3 );
   fprintf( stderr, "APP-r%d: Acquired read context %lu\n", my_rank, version );

   /* Release the read handle and close read context on previous CV  */
   fprintf( stderr, "APP-r%d: Release read context 2\n", my_rank );
   ret = H5RCrelease( rc_id2, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id2 ); ASSERT_RET;

   /**** 
    * Rank 0 persists CV 3 
    *
    * Satisfies:  An application will create multiple transactions in a container, then persist them
    * from the BB to persistent storage.                           
    ****/

   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: Persist cv 3.\n", my_rank );
      print_container_contents( file_id, rc_id3, "/", my_rank );
      ret = H5RCpersist(rc_id3, H5_EVENT_STACK_NULL); ASSERT_RET; 
   }

   if (verbose) print_container_contents( file_id, rc_id3, "/", my_rank );

   /**** 
    * Transaction 4 - All ranks update Dataset and Map objects in the 3 Groups  
    ****/

   tr_num = 4;
   num_tr_leaders = comm_size;

   /* Create a local transaction for transaction number and start it. */
   fprintf( stderr, "APP-r%d: Create and start tr %lu with %d transaction leaders\n", my_rank, tr_num, num_tr_leaders );
   tr_id = H5TRcreate( file_id, rc_id3, tr_num ); assert( tr_id >= 0 );
   trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
   ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
   ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Add updates to the transaction */
   update_dataset( file_id, "G-logged/D", tr_id, rc_id3, "/", my_rank, 1 );
   update_dataset( file_id, "G-prefetched/D", tr_id, rc_id3, "/", my_rank, 2 );
   update_dataset( file_id, "G-stored/D", tr_id, rc_id3, "/", my_rank, 3 );

   update_map( file_id, "G-logged/M", tr_id, rc_id3, "/", my_rank, 1 );
   update_map( file_id, "G-prefetched/M", tr_id, rc_id3, "/", my_rank, 2 );
   update_map( file_id, "G-stored/M", tr_id, rc_id3, "/", my_rank, 3 );

   /* Finish, commit, and close transaction */
   fprintf( stderr, "APP-r%d: Finish, commit, and close tr %lu\n", my_rank, tr_num );
   ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5TRclose( tr_id ); ASSERT_RET;
   ret = H5Pclose( trspl_id ); ASSERT_RET;

   /* If verbose mode, get RC 4 and print CV 4 */
   if ( verbose ) {
      version = 4;
      fprintf( stderr, "APP-r%d: Try to acquire read context %lu\n", my_rank, version );
      H5E_BEGIN_TRY { 
         rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         while ( rc_id4 < 0 ) {
            fprintf( stderr, "APP-r%d: Failed to acquire read context %lu; sleep then retry\n", my_rank, version );
            sleep( 1 );
            rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         }
      } H5E_END_TRY;
      assert( rc_id4 >= 0 ); assert ( version == 4 );
      fprintf( stderr, "APP-r%d: Acquired read context %lu\n", my_rank, version );

      print_container_contents( file_id, rc_id4, "/", my_rank );

      /* Release the read handle and close read context on CV 4 */
      fprintf( stderr, "APP-r%d: Release read context 4\n", my_rank );
      ret = H5RCrelease( rc_id4, H5_EVENT_STACK_NULL); ASSERT_RET;
      ret = H5RCclose( rc_id4 ); ASSERT_RET;
   }

   /**** 
    * Transaction 5 (based on CV 3) - All ranks update Dataset and Map objects in the 3 Groups  
    ****/

   tr_num = 5;
   num_tr_leaders = comm_size;

   /* Create a local transaction for transaction number and start it. */
   fprintf( stderr, "APP-r%d: Create and start tr %lu with %d transaction leaders\n", my_rank, tr_num, num_tr_leaders );
   tr_id = H5TRcreate( file_id, rc_id3, tr_num ); assert( tr_id >= 0 );
   trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
   ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
   ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Add updates to the transaction */
   update_dataset( file_id, "G-logged/D", tr_id, rc_id3, "/", my_rank, 1 );
   update_dataset( file_id, "G-prefetched/D", tr_id, rc_id3, "/", my_rank, 2 );
   update_dataset( file_id, "G-stored/D", tr_id, rc_id3, "/", my_rank, 3 );

   update_map( file_id, "G-logged/M", tr_id, rc_id3, "/", my_rank, 1 );
   update_map( file_id, "G-prefetched/M", tr_id, rc_id3, "/", my_rank, 2 );
   update_map( file_id, "G-stored/M", tr_id, rc_id3, "/", my_rank, 3 );

   /* Finish, commit, and close transaction */
   fprintf( stderr, "APP-r%d: Finish, commit, and close tr %lu\n", my_rank, tr_num );
   ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5TRclose( tr_id ); ASSERT_RET;
   ret = H5Pclose( trspl_id ); ASSERT_RET;

   /* Acquire a read handle for container version and create a read context. */
   version = 5;
   if (verbose) fprintf( stderr, "APP-r%d: Try to acquire read context %lu\n", my_rank, version );
   H5E_BEGIN_TRY { 
      rc_id5 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      while ( rc_id5 < 0 ) {
         if (verbose) fprintf( stderr, "APP-r%d: Failed to acquire read context %lu; sleep then retry\n", my_rank, version );
         sleep( 1 );
         rc_id5 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      }
   } H5E_END_TRY;
   assert( rc_id5 >= 0 ); assert ( version == 5 );
   fprintf( stderr, "APP-r%d: Acquired read context %lu\n", my_rank, version );

   /* Release the read handle and close read context on CV 3 */
   fprintf( stderr, "APP-r%d: Release read context 3\n", my_rank );
   ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id3 ); ASSERT_RET;

   if (verbose) print_container_contents( file_id, rc_id5, "/", my_rank );

   /**** 
    * Highest rank persists CV 5, then evicts objects under G-prefetched and G-stored. 
    *
    * Satisfies:  An application will create multiple transactions in a container, then persist them
    * from the burst buffer storate to persistent storage, then evict several of the objects in the IOD
    * container, releasing space in the burst buffer.
    ****/

   if ( my_rank == comm_size-1 ) {
      fprintf( stderr, "APP-r%d: Persist cv 5.\n", my_rank );
      print_container_contents( file_id, rc_id5, "/", my_rank );
      ret = H5RCpersist(rc_id5, H5_EVENT_STACK_NULL); ASSERT_RET; 

      /* TODO:  Add code showing BB space in use */

      evict_group_members_updates( file_id, rc_id5, "/G-prefetched", my_rank );
      evict_group_members_updates( file_id, rc_id5, "/G-stored", my_rank );

      /* TODO:  Add code showing BB space in use */
   }
   
   /* All ranks print container here.  For rank == comm_size, this will be after objects have been evicted */
   if (verbose) print_container_contents( file_id, rc_id5, "/", my_rank ); 

   /* Release the read handle and close read context on CV 5 */
   fprintf( stderr, "APP-r%d: Release read context 5\n", my_rank );
   ret = H5RCrelease( rc_id5, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id5 ); ASSERT_RET;

   /****
    *  Close the H5File.  Reopen, pre-fetch D, M, and TODO T in /G-prefetched.  
    *
    *  Satisfies: "... then closing the container.  The application will then re-open the container and asynchronously 
    *  pre-fetch multiple objects from persistent storage to burst buffer storage.
    ****/

   /* Close the container, then barrier to make sure all ranks have closed it before continuing */
   fprintf( stderr, "APP-r%d: Close the container.\n", my_rank );
   ret = H5Fclose_ff( file_id, 1, H5_EVENT_STACK_NULL ); ASSERT_RET;
   MPI_Barrier( MPI_COMM_WORLD );

   /* Reopen the container read-only and get a read context for highest CV */
   file_id = H5Fopen_ff( file_name, H5F_ACC_RDONLY, fapl_id, &rc_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );
   H5RCget_version( rc_id, &version );
   fprintf( stderr, "APP-r%d: Container re-opened; Acquired RC\n", my_rank, version );

   /* TODO:  Add code showing BB space in use */

   /****************** STILL WORKING ON THIS PART *****************************/

   hid_t dset_l_id, dset_p_id, dset_s_id;
   hid_t space_l_id, space_p_id, space_s_id;
   int data_l[4], data_p[4], data_s[4];              
   hrpl_t dset_p_replica;
   hid_t dxpl_p_id;

   hid_t map_l_id, map_p_id, map_s_id;
   int key_l[4], key_p[4], key_s[4];              
   int value_l[4], value_p[4], value_s[4];              
   hrpl_t map_p_replica;
   hid_t mxpl_p_id;

   /* Open Datasets and then Maps in all 3 Groups, prefetching objects in the /G-prefetched group. */
   dset_l_id = H5Dopen_ff( file_id, "/G-logged/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_l_id >= 0 );
   space_l_id = H5Dget_space( dset_l_id ); assert ( space_l_id >= 0 );

   dset_p_id = H5Dopen_ff( file_id, "/G-prefetched/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_p_id >= 0 );
   space_p_id = H5Dget_space( dset_p_id ); assert ( space_p_id >= 0 ); /* Space info doesn't get prefetched, so read it here */

   if ( use_daos_lustre ) {
      /* FOR NOW:  All ranks prefetch synchronously.  TODO: change so only one - async - then share replica_id */
      fprintf( stderr, "APP-r%d: prefetch /G-prefeteched/D.\n", my_rank );
      ret = H5Dprefetch_ff( dset_p_id, rc_id, &dset_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
      dxpl_p_id = H5Pcreate( H5P_DATASET_XFER );
      ret = H5Pset_read_replica( dxpl_p_id, dset_p_replica );  ASSERT_RET;
   } else {
      dxpl_p_id = H5P_DEFAULT;
   }

   dset_s_id = H5Dopen_ff( file_id, "/G-stored/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_s_id >= 0 );
   space_s_id = H5Dget_space( dset_s_id ); assert ( space_s_id >= 0 );

   map_l_id = H5Mopen_ff( file_id, "/G-logged/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_l_id >= 0 );

   map_p_id = H5Mopen_ff( file_id, "/G-prefetched/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_p_id >= 0 );

   if ( use_daos_lustre ) {
      /* FOR NOW:  All ranks prefetch synchronously.  TODO: change so only one - async - then share replica_id */
      fprintf( stderr, "APP-r%d: prefetch /M-prefeteched/D.\n", my_rank );
      ret = H5Mprefetch_ff( map_p_id, rc_id, &map_p_replica, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
      mxpl_p_id = H5Pcreate( H5P_DATASET_XFER );
      ret = H5Pset_read_replica( mxpl_p_id, map_p_replica );  ASSERT_RET;
   } else {
      mxpl_p_id = H5P_DEFAULT;
   }

   map_s_id = H5Mopen_ff( file_id, "/G-stored/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_s_id >= 0 );

   // TODO:  Do these reads/prints multiple times, to get a better sense of variance in read times

   /* Read Datasets and then Maps in all 3 Groups, measuring time it takes to read data for each*/

   gettimeofday( &tv_start, NULL );
   ret = H5Dread_ff( dset_l_id, H5T_NATIVE_INT, space_l_id, space_l_id, H5P_DEFAULT, data_l, rc_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-logged/D: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );

   gettimeofday( &tv_start, NULL );
   ret = H5Dread_ff( dset_p_id, H5T_NATIVE_INT, space_p_id, space_p_id, dxpl_p_id, data_p, rc_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-prefetched/D: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );

   gettimeofday( &tv_start, NULL );
   ret = H5Dread_ff( dset_s_id, H5T_NATIVE_INT, space_s_id, space_s_id, H5P_DEFAULT, data_s, rc_id, H5_EVENT_STACK_NULL ); 
   ASSERT_RET;
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-stored/D: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );

   gettimeofday( &tv_start, NULL );
   for ( i = 0; i < 4; i++ ) {
      ret = H5Mget_ff( map_l_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_l[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
   }
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-logged/M: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );

   gettimeofday( &tv_start, NULL );
   for ( i = 0; i < 4; i++ ) {
      ret = H5Mget_ff( map_p_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_p[i], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
   }
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-prefetched/M: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );

   gettimeofday( &tv_start, NULL );
   for ( i = 0; i < 4; i++ ) {
      ret = H5Mget_ff( map_s_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_s[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;
   }
   gettimeofday( &tv_end, NULL );
   fprintf( stderr, "APP-r%d  Time (usec) to read /G-stored/M: %lu\n", my_rank, 
         (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) );


   /* Print Dataset and Map values for all 3 groups */
   fprintf( stderr, "APP-r%d: /G-logged/D values: %d %d %d %d\n", my_rank, data_l[0], data_l[1], data_l[2], data_l[3] );
   fprintf( stderr, "APP-r%d: /G-prefetched/D values: %d %d %d %d\n", my_rank, data_p[0], data_p[1], data_p[2], data_p[3]);
   fprintf( stderr, "APP-r%d: /G-stored/D values: %d %d %d %d\n", my_rank, data_s[0], data_s[1], data_s[2], data_s[3] );

   fprintf( stderr, "APP-r%d: /G-logged/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_l[0], value_l[1], value_l[2], value_l[3] );
   fprintf( stderr, "APP-r%d: /G-prefetched/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_p[0], value_p[1], value_p[2], value_p[3] );
   fprintf( stderr, "APP-r%d: /G-stored/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_s[0], value_s[1], value_s[2], value_s[3] );

   /* TODO:  Add barrier ( unless can show BB space per ION per rank ) */

   /* TODO:  Add code showing BB space in use */

   /* TODO:  Add code to evict replicas */

   /* Close Objects*/

   if ( use_daos_lustre ) {
      ret = H5Pclose( mxpl_p_id ); ASSERT_RET;
      ret = H5Pclose( dxpl_p_id ); ASSERT_RET;
   }

   ret = H5Dclose_ff( dset_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Dclose_ff( dset_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Dclose_ff( dset_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   ret = H5Sclose( space_l_id ); ASSERT_RET;
   ret = H5Sclose( space_p_id ); ASSERT_RET;
   ret = H5Sclose( space_s_id ); ASSERT_RET;

   ret = H5Mclose_ff( map_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Mclose_ff( map_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Mclose_ff( map_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;


   /****************** WORKING ON THIS PART *****************************/
   

   /* Release the read handle and close read context  */
   fprintf( stderr, "APP-r%d: Release read context\n", my_rank );
   ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id ); ASSERT_RET;

   /* Close H5 Objects that are still open */
   fprintf( stderr, "APP-r%d: Close all H5 objects that are still open\n", my_rank );
   ret = H5Fclose_ff( file_id, 1, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Pclose( fapl_id ); ASSERT_RET;

   fprintf( stderr, "APP-r%d: Finalize EFF stack\n", my_rank );

   /* Perform wrap-up operations */
   MPI_Barrier( MPI_COMM_WORLD );
   EFF_finalize();
   MPI_Finalize();

   return 0;
}


/*
 * Helper function used to create group "group_name"
 * in object identified by "obj_id"
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" are used in the status output.
 */
void
create_group( hid_t obj_id, const char* group_name, hid_t tr_id, const char* obj_path, int my_rank ) {
   herr_t   ret;
   uint64_t tr_num;
   hid_t    group_id;

   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;

   group_id = H5Gcreate_ff( obj_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   if ( group_id >= 0 ) {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d - %s\n", my_rank, group_name, obj_path, (int)tr_num, "OK"  );
      ret = H5Gclose_ff( group_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d - %s\n", my_rank, group_name, obj_path, (int)tr_num, "FAILED"  );
   }

   return;
}

/*
 * Helper function used to create and initialize dataset "dset_name"
 * in object identified by "obj_id"
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" are used in the status output.
 * "ordinal" is used as a multiplier in computing the cell value.
 *
 * All datasets are 1D Arrays with 4 elements.
 * Cell values are computed using the formula:
 *    data[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
 */
void
create_dataset( hid_t obj_id, const char* dset_name, hid_t tr_id, const char* obj_path, int my_rank, int ordinal ) {

   herr_t   ret;
   uint64_t tr_num;
   int      i;
   hsize_t  current_size = 4;        /* For now, we hard-code to small number */
   hsize_t  max_size = 4;
   int      data[4];             
   hid_t    space_id;
   hid_t    dtype_id;
   hid_t    dset_id;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;
   for ( i = 0; i < current_size; i++ ) {
      data[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
   }
   space_id = H5Screate_simple( 1, &current_size, &max_size ); assert( space_id >= 0 );
   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id >= 0 );

   /* Create & Populate */
   dset_id = H5Dcreate_ff( obj_id, dset_name, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                          H5_EVENT_STACK_NULL );
   if ( dset_id >= 0 ) {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d; data values will be: %d %d %d %d - %s\n",
               my_rank, dset_name, obj_path, (int)tr_num, data[0], data[1], data[2], data[3], "OK" );
      ret = H5Dwrite_ff( dset_id, dtype_id, space_id, space_id, H5P_DEFAULT, data, tr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d - %s\n", my_rank, dset_name, obj_path, (int)tr_num, "FAILED" );
   }
   ret = H5Tclose( dtype_id ); ASSERT_RET;
   ret = H5Sclose( space_id ); ASSERT_RET;
   return;
}

/*
 * Helper function used to create and initialize map "map_name"
 * in object identified by "obj_id"
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" are used in the status output.
 * "ordinal" is used as a multiplier in computing the value.
 *
 * All maps are created with 4 initial KV pairs.
 * Keys/values are computed using the formula:
 *    key=i / value=(ordinal*1000 + tr_num*100 + my_rank*10 + i)
 */
void
create_map( hid_t obj_id, const char* map_name, hid_t tr_id, const char* obj_path, int my_rank, int ordinal ) {

   herr_t   ret;
   uint64_t tr_num;
   int      i;
   int      num_kvs = 4;           /* For now, we hard-code to small number */
   int      value[4];             
   hid_t    map_id;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;
   for ( i = 0; i < num_kvs; i++ ) {
      value[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
   }

   /* Create & Populate */
   map_id = H5Mcreate_ff( obj_id, map_name, H5T_STD_I32LE, H5T_STD_I32LE, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id,
                          H5_EVENT_STACK_NULL );
   if ( map_id >= 0 ) {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d; keys/values will be: 0/%d 1/%d 2/%d 3/%d - %s\n",
               my_rank, map_name, obj_path, (int)tr_num, value[0], value[1], value[2], value[3], "OK" );
      for ( i = 0; i < num_kvs; i++ ) {
         ret = H5Mset_ff( map_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value[i], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }
      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "APP-r%d: Create %s in %s in tr %d - %s\n", my_rank, map_name, obj_path, (int)tr_num, "FAILED" );
   }
   return;
}

/*
 * Helper function used to create committed datatype "type_name"
 * in group identified by "obj_id"
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" are used in the status output.
 */
void
create_committed_datatype( hid_t obj_id, const char* type_name, hid_t tr_id, const char* obj_path, int my_rank ) {

   herr_t   ret;
   uint64_t tr_num;
   hid_t    dtype_id;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;
   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id >= 0 );

   /* Create */
   ret = H5Tcommit_ff( obj_id, type_name, dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   fprintf( stderr, "APP-r%d: Create CDT %s in %s tr %d - %s\n", my_rank, type_name, obj_path, (int)tr_num, STATUS );
   ret = H5Tclose( dtype_id ); ASSERT_RET;
   return;
}

/*
 * Helper function used to update raw data in "dset_name"
 * in object identified by "obj_id"
 * in transaction identified by "tr_id".
 * "rc_id" for "tr_id" passed in explicitly for now - later may be able to get it from info in tr_id (cv & file_id)
 * "obj_path" and "my_rank" are used in the status output.
 * "ordinal" is used as a multiplier in computing the cell value.
 *
 * Cell value to be updated is computed using the formula:
 *    data[my_rank] = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
 *
 * If my_rank >= max_size of data, then no update done in call.
 */
void
update_dataset( hid_t obj_id, const char* dset_name, hid_t tr_id, hid_t rc_id, const char* obj_path, int my_rank, int ordinal ){
   herr_t   ret;
   uint64_t tr_num;
   hid_t    dset_id;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;

   dset_id = H5Dopen_ff( obj_id, dset_name, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   if ( dset_id >= 0 ) {

      /* Dataset opened; confirm as we expect */
      hid_t filespace_id;
      int nDims;
      hsize_t current_size, max_size;

      filespace_id = H5Dget_space( dset_id ); assert ( filespace_id >= 0 );
      nDims = H5Sget_simple_extent_dims( filespace_id, &current_size, &max_size );
      assert( nDims == 1 );
      assert( current_size == 4 );
      assert( max_size == 4 );

      if ( my_rank < current_size ) {
         /* If rank in bounds of array, do the update */
         int cell_data;              
         hsize_t mem_size = 1;
         hid_t memspace_id;
         hsize_t index;

         cell_data = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;

         memspace_id = H5Screate_simple( 1, &mem_size, &mem_size ); assert( memspace_id >= 0 );
         index = (hsize_t)my_rank;
         ret = H5Sselect_elements( filespace_id, H5S_SELECT_SET, 1, &index );  ASSERT_RET;

         ret = H5Dwrite_ff( dset_id, H5T_NATIVE_INT, memspace_id, filespace_id, H5P_DEFAULT, &cell_data, tr_id,
                            H5_EVENT_STACK_NULL ); ASSERT_RET;
         fprintf( stderr, "APP-r%d: Update %s[%d] in %s in tr %d to have value %d - %s\n",
                        my_rank, dset_name, my_rank, obj_path, (int)tr_num, cell_data, STATUS );
         ret = H5Sclose( memspace_id ); ASSERT_RET;

      } else {
         fprintf( stderr, "APP-r%d: No update to %s in %s in tr %d for this rank.\n", my_rank, dset_name, obj_path, (int)tr_num );
      }

      ret = H5Sclose( filespace_id ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   } else {
      fprintf( stderr, "APP-r%d: Update %s[%d] in %s in tr %d - %s\n",
               my_rank, dset_name, my_rank, obj_path, (int)tr_num, "FAILED" );
   }

   return;
}

/*
 * Helper function used to update raw data in "dset_name"
 * in object identified by "obj_id"
 * in transaction identified by "tr_id".
 * "rc_id" for "tr_id" passed in explicitly for now - later may be able to get it from info in tr_id (cv & file_id)
 * "obj_path" and "my_rank" are used in the status output.
 * key/value set according to: 
 *    key = my_rank 
 *    value = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
 */
void
update_map( hid_t obj_id, const char* map_name, hid_t tr_id, hid_t rc_id, const char* obj_path, int my_rank, int ordinal ){
   herr_t ret;
   uint64_t tr_num;
   hid_t map_id;
   int value;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;

   /* Open the map */
   map_id = H5Mopen_ff( obj_id, map_name, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );

   if ( map_id >= 0 ) {
      value = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
      ret = H5Mset_ff( map_id, H5T_STD_I32LE, &my_rank, H5T_STD_I32LE, &value, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      fprintf( stderr, "APP-r%d: Update Map %s in %s in tr %d to have key/value = %d/%d - %s\n",
                        my_rank, map_name, obj_path, (int)tr_num, my_rank, value, STATUS );

      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   } else {
      fprintf( stderr, "APP-r%d: Update Map %s in %s in tr %d - %s\n",
               my_rank, map_name, my_rank, obj_path, (int)tr_num, "FAILED" );
   }

   return;
}

/*
 * Helper function used to recursively print container contents
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
   char line[1024];    

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
      hsize_t current_size[1];
      hsize_t max_size[1];
      hsize_t totalSize;
      int *data;              
      int i;

      dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( dset_id >= 0 );

      space_id = H5Dget_space( dset_id ); assert ( space_id >= 0 );
      nDims = H5Sget_simple_extent_dims( space_id, current_size, max_size ); 
      assert( nDims == 1 );

      totalSize = current_size[0];
      data = (int *)calloc( totalSize, sizeof(int) ); 

      ret = H5Dread_ff( dset_id, H5T_NATIVE_INT, space_id, space_id, H5P_DEFAULT, data, rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      sprintf( line, "%s %s values: ", preface, path_to_object );
      for ( i = 0; i < totalSize; i++ ) {
         sprintf( line, "%s%d ", line, data[i] );
      }
                  
      fprintf( stderr, "%s\n", line );
      free( data );

      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Sclose( space_id ); ASSERT_RET;
   }

   /* Maps */
   sprintf( path_to_object, "%s%s", grp_path, "M" );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t map_id;
      hsize_t totalCount;
      int *value;              
      int i;

      map_id = H5Mopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( map_id >= 0 );

      ret = H5Mget_count_ff( map_id, &totalCount, rc_id, H5_EVENT_STACK_NULL );  ASSERT_RET;
      value = (int *)calloc( totalCount, sizeof(int) ); 

      for ( i = 0; i < totalCount; i++ ) {
         ret = H5Mget_ff( map_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }

      sprintf( line, "%s %s key/values: ", preface, path_to_object );
      for ( i = 0; i < totalCount; i++ ) {
         sprintf( line, "%s %d/%d ", line, i, value[i] );
      }
                  
      fprintf( stderr, "%s\n", line );
      free( value );

      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   }

   /* Committed datatypes */
   sprintf( path_to_object, "%s%s", grp_path, "T" );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      fprintf( stderr, "%s %s\n", preface, path_to_object );
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
 * Helper function used to evict logged updates for (expected) members of specified group group 
 * for container identified by "file_id" 
 * in read context identified by "rc_id"
 * for group specified by "grp_path"
 * and "my_rank" used to identify the process doing the evicting
 */
void
evict_group_members_updates( hid_t file_id, hid_t rc_id, const char* grp_path, int my_rank )
{
   herr_t ret;
   uint64_t cv;
   hbool_t exists;
   char path_to_object[1024];
   int i;

   char preface[128];    

   /* Get the container version for the read context */
   ret = H5RCget_version( rc_id, &cv ); ASSERT_RET;

   /* Set up the preface and adding version number */
   sprintf( preface, "APP-r%d: cv %d: ", my_rank, (int)cv );

   /* Dataset */
   sprintf( path_to_object, "%s/%s", grp_path, "D" );
   if (verbose) fprintf( stderr, "%s: checking for existance of object %s before evicting\n", preface, path_to_object );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t dset_id;

      dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( dset_id >= 0 );

      if ( use_daos_lustre ) {
         ret = H5Devict_ff( dset_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
         fprintf( stderr, "%s: evicted updates for object %s\n", preface, path_to_object );
      } else {
         fprintf( stderr, "%s: DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
      }

      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   }

   /* Map */
   sprintf( path_to_object, "%s/%s", grp_path, "M" );
   fprintf( stderr, "%s: checking for existance of object %s before evicting\n", preface, path_to_object );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t map_id;

      map_id = H5Mopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
      assert( map_id >= 0 );

      if ( use_daos_lustre ) {
         ret = H5Mevict_ff( map_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
         fprintf( stderr, "%s: evicted updates for object %s\n", preface, path_to_object );
      } else {
         fprintf( stderr, "%s: DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
      }

      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   }

   /* Committed datatypes */
   sprintf( path_to_object, "%s/%s", grp_path, "T" );
   fprintf( stderr, "%s: checking for existance of object %s before evicting\n", preface, path_to_object );
   ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );

   if ( exists ) { 
      hid_t type_id;

      type_id = H5Topen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
      assert( type_id >= 0 );

      if ( use_daos_lustre ) {
         ret = H5Tevict_ff( type_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
         fprintf( stderr, "%s: evicted updates for object %s\n", preface, path_to_object );
      } else {
         fprintf( stderr, "%s: DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
      }

      ret = H5Tclose_ff( type_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   }

   return;
}



/*
 * parse_options - helper function to parse the command line options.
 */
int
parse_options( int argc, char** argv, int my_rank ) {
   int    i, n;
   char* app = argv[0];

   while ( --argc ) {
      if ( **(++argv) != '-' ) {
         break;
      } else {
         switch( *(*argv+1) ) {
            case 'l':   
               use_daos_lustre = 1;
               break;
            case 'v':   
               verbose = 1;
               break;
            default: 
               if ( my_rank == 0 ) {
                  printf( "Usage: %s [-lv]\n", app  );
                  printf( "\tl: use DAOS Lustre\n" );
                  printf( "\tv: verbose output\n" );
               }
               return( 1 );
         }
      }
   }
   return( 0 );
}
