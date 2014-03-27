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
#define STATUS (ret >= 0) ? " " : " - FAILED"
#define ASSERT_RET assert( ret >= 0 )

/* option flags */
int use_daos_lustre = 1;   // Use DAOS Lustre - defaults to yes
int verbose = 0;           // Verbose output - defaults to no
int pause = 0;             // Pause to allow out-of-band space check - defaults to 0.
int time_reads = 0;        // Time reads after container re-open
int workaround = 1;        // Enable Q7 workaround for race conditions (added barriers) - defaults to 1.

/* global variables and macros used to make timing easier */
struct timeval tv_start;
struct timeval tv_end;
#define START_TIME gettimeofday( &tv_start, NULL )
#define END_TIME gettimeofday( &tv_end, NULL )
#define ELAPSED_TIME (ulong)( (tv_end.tv_usec + 1000000*tv_end.tv_sec) - (tv_start.tv_usec + 1000000*tv_start.tv_sec) ) 

/* function prototypes */
void create_dataset( hid_t, const char*, hid_t, const char*, int, int );
void create_map( hid_t, const char*, hid_t, const char*, int, int );
void create_named_datatype( hid_t, const char*, hid_t, const char*, int );
void update_dataset( hid_t, const char*, hid_t, hid_t, const char*, int, int );
void update_map( hid_t, const char*, hid_t, hid_t, const char*, int, int );
void evict_group_members_updates( hid_t, hid_t, const char*, int );
void print_container_contents( hid_t, hid_t, const char*, int );
int  parse_options( int, char**, int );
void usage( const char* );

/**************************************************************************************************************/
int main( int argc, char **argv ) {

   /* MPI */
   int my_rank, comm_size;
   int provided;

   /* Container */
   const char file_name[]="eff_pep.h5";
   hid_t  fapl_id;         
   hid_t  file_id;

   /* Groups */
   hid_t  grp_l_id;
   hid_t  grp_p_id;
   hid_t  grp_s_id;

   /* Container Version & Read Contexts */
   uint64_t version;
   hid_t    rc_id1, rc_id2, rc_id3, rc_id4, rc_id5, rc_id;

   /* Transactions  */
   uint64_t tr_num;
   hid_t    tr_id;
   hid_t    trspl_id;
   int      num_tr_leaders;

   herr_t ret;
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
   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: Number of MPI processes = %d\n", my_rank, comm_size );
   }

   /* Parse command-line options controlling behavior */
   if ( parse_options( argc, argv, my_rank ) != 0 ) {
      exit( 1 );
   }    

   /* Initialize the EFF stack, starting FS client, registering HDF5 VOL calls, requesting IOD init */
   fprintf( stderr, "APP-r%d: Initialize EFF stack\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Specify the IOD VOL plugin should be used and create H5File (EFF container) */
   fprintf( stderr, "APP-r%d: Create %s\n", my_rank, file_name );
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
      fprintf( stderr, "APP-r%d: tr %lu - Start\n", my_rank, tr_num );
      tr_id = H5TRcreate( file_id, rc_id1, tr_num ); assert( tr_id >= 0 );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Add updates to the transaction for Group creates */
      fprintf( stderr, "APP-r%d: tr %d - Create /G-logged /G-prefetched /G-stored\n", my_rank, tr_num );
      grp_l_id = H5Gcreate_ff( file_id, "G-logged", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_l_id >= 0 );
      grp_p_id = H5Gcreate_ff( file_id, "G-prefetched", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_p_id >= 0 );
      grp_s_id = H5Gcreate_ff( file_id, "G-stored", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
      assert( grp_s_id >= 0 );

      /* Finish and commit TR and get RC */
      fprintf( stderr, "APP-r%d: tr %lu - Finish\n", my_rank, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, &rc_id2, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      version = 2;
      assert( rc_id2 >= 0 ); 
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, tr_num );
   }

   /* Rank 0 tells others that RC has been acquired; they create local RC object and open the Groups */
   MPI_Bcast( &version, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
   if ( my_rank != 0 ) {
      rc_id2 = H5RCcreate( file_id, version );
      assert( rc_id2 >= 0 ); assert ( version == 2 );

      grp_l_id = H5Gopen_ff( file_id, "G-logged", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( grp_l_id >= 0 );
      grp_p_id = H5Gopen_ff( file_id, "G-prefetched", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( grp_p_id >= 0 );
      grp_s_id = H5Gopen_ff( file_id, "G-stored", H5P_DEFAULT, rc_id2, H5_EVENT_STACK_NULL ); assert( grp_s_id >= 0 );
   } 

   /* Release the read handle and close read context on previous CV  */
   ret = H5RCrelease( rc_id1, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id1 ); ASSERT_RET;

   if ( verbose && ( my_rank == 0 ) ) print_container_contents( file_id, rc_id2, "/", my_rank );

   /****
    * Transaction 3 - In each of the 3 Groups, create "D"(rnk 0), "M"(rnk 1 or 0), "T" (rnk 2 or 1 or 0)
    ****/

   if ( my_rank < 3 ) {
      tr_num = 3;
      num_tr_leaders = ( (comm_size < 4) ? comm_size : 3 );

      /* Create a local transaction for transaction number and start it. */
      fprintf( stderr, "APP-r%d: tr %lu - Start with %d leaders\n", my_rank, tr_num, num_tr_leaders );
      tr_id = H5TRcreate( file_id, rc_id2, tr_num ); assert( tr_id >= 0 );
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
      ret = H5Pset_trspl_num_peers( trspl_id, num_tr_leaders ); ASSERT_RET;
      ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      char glog_path[128], gpre_path[128], gsto_path[128];

      /* Set pathnames to groups used in status messages */
      sprintf( glog_path, "/G-logged/" );
      sprintf( gpre_path, "/G-prefetched/" );
      sprintf( gsto_path, "/G-stored/" );

      /* Add updates to the transaction */
      if ( my_rank == 0 ) {
         create_dataset( grp_l_id, "D", tr_id, glog_path, my_rank, 1 );
         create_dataset( grp_p_id, "D", tr_id, gpre_path, my_rank, 2 );
         create_dataset( grp_s_id, "D", tr_id, gsto_path, my_rank, 3 );
      }  
      if ( ( my_rank == 1 ) || ( ( my_rank == 0 ) && ( comm_size == 1 ) ) )  {
         create_map( grp_l_id, "M", tr_id, glog_path, my_rank, 1 );
         create_map( grp_p_id, "M", tr_id, gpre_path, my_rank, 2 );
         create_map( grp_s_id, "M", tr_id, gsto_path, my_rank, 3 );
      }
      if ( ( my_rank == 2 )  || ( ( my_rank == 1 ) && ( comm_size == 2 ) )  || ( ( my_rank == 0 ) && ( comm_size == 1 ) ) ) {
         create_named_datatype( grp_l_id, "T", tr_id, glog_path, my_rank );
         create_named_datatype( grp_p_id, "T", tr_id, gpre_path, my_rank );
         create_named_datatype( grp_s_id, "T", tr_id, gsto_path, my_rank );
      }

      /* Finish, commit, and close transaction */
      fprintf( stderr, "APP-r%d: tr %lu - Finish\n", my_rank, tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id ); ASSERT_RET;
      ret = H5Pclose( trspl_id ); ASSERT_RET;
   }


   /* Acquire a read handle for container version and create a read context. */
   version = 3;
   if ( verbose ) fprintf( stderr, "APP-r%d: rc %lu - Try to acquire\n", my_rank, version );

   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      
   H5E_BEGIN_TRY { 
      rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      while ( rc_id3 < 0 ) {
         if ( verbose ) fprintf( stderr, "APP-r%d: rc %lu - Failed to acquire; sleep then retry\n", my_rank, version );
         sleep( 1 );
         rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      }
   } H5E_END_TRY;
   assert( rc_id3 >= 0 ); assert ( version == 3 );
   fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

   /* Release the read handle and close read context on previous CV  */
   if ( my_rank == 0 ) {
      ret = H5RCrelease( rc_id2, H5_EVENT_STACK_NULL); ASSERT_RET;
   }
   ret = H5RCclose( rc_id2 ); ASSERT_RET;
   fprintf( stderr, "APP-r%d: rc 2 - Released\n", my_rank );

   /**** 
    * Rank 0 persists CV 3 
    *
    * Satisfies:  An application will create multiple transactions in a container, then persist them
    * from the BB to persistent storage.                           
    ****/

   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      
   if ( my_rank == 0 ) {
      fprintf( stderr, "APP-r%d: cv 3 - Persist\n", my_rank );
      ret = H5RCpersist(rc_id3, H5_EVENT_STACK_NULL); ASSERT_RET; 
      print_container_contents( file_id, rc_id3, "/", my_rank );
   }
   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      

   if ( verbose ) print_container_contents( file_id, rc_id3, "/", my_rank );

   /**** 
    * Transaction 4 - All ranks update Dataset and Map objects in the 3 Groups  
    ****/

   tr_num = 4;
   num_tr_leaders = comm_size;

   /* Create a local transaction for transaction number and start it. */
   fprintf( stderr, "APP-r%d: tr %lu - Start with %d leaders\n", my_rank, tr_num, num_tr_leaders );
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

   /* Finish and close transaction */
   fprintf( stderr, "APP-r%d: tr %lu - Finish\n", my_rank, tr_num );
   ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5TRclose( tr_id ); ASSERT_RET;
   ret = H5Pclose( trspl_id ); ASSERT_RET;

   /* If verbose mode, get RC 4 and print CV 4 */
   if ( verbose ) {
      version = 4;
      fprintf( stderr, "APP-r%d: rc %lu - Try to acquire\n", my_rank, version );

      if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      
      H5E_BEGIN_TRY { 
         rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         while ( rc_id4 < 0 ) {
            fprintf( stderr, "APP-r%d: rc %lu - Failed to acquire; sleep then retry\n", my_rank, version );
            sleep( 1 );
            rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
         }
      } H5E_END_TRY;
      assert( rc_id4 >= 0 ); assert ( version == 4 );
      fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

      print_container_contents( file_id, rc_id4, "/", my_rank );

      /* Release the read handle and close read context on CV 4 */
      ret = H5RCrelease( rc_id4, H5_EVENT_STACK_NULL); ASSERT_RET;
      ret = H5RCclose( rc_id4 ); ASSERT_RET;
      fprintf( stderr, "APP-r%d: rc 4 - Released\n", my_rank );
   }

   /**** 
    * Transaction 5 (based on CV 3) - All ranks update Dataset and Map objects in the 3 Groups  
    ****/

   tr_num = 5;
   num_tr_leaders = comm_size;

   /* Create a local transaction for transaction number and start it. */
   fprintf( stderr, "APP-r%d: tr %lu - Start with %d leaders\n", my_rank, tr_num, num_tr_leaders );
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
   fprintf( stderr, "APP-r%d: tr %lu - Finish\n", my_rank, tr_num );
   ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5TRclose( tr_id ); ASSERT_RET;
   ret = H5Pclose( trspl_id ); ASSERT_RET;

   /* Acquire a read handle for container version and create a read context. */
   version = 5;
   if ( verbose ) fprintf( stderr, "APP-r%d: rc %lu - Try to acquire\n", my_rank, version );

   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      
   H5E_BEGIN_TRY { 
      rc_id5 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      while ( rc_id5 < 0 ) {
         if ( verbose ) fprintf( stderr, "APP-r%d: rc %lu - Failed to acquire; sleep then retry\n", my_rank, version );
         sleep( 1 );
         rc_id5 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL );
      }
   } H5E_END_TRY;
   assert( rc_id5 >= 0 ); assert ( version == 5 );
   fprintf( stderr, "APP-r%d: rc %lu - Acquired\n", my_rank, version );

   /* Release the read handle and close read context on CV 3 */
   ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id3 ); ASSERT_RET;
   fprintf( stderr, "APP-r%d: rc 3 - Released \n", my_rank );

   if ( verbose ) print_container_contents( file_id, rc_id5, "/", my_rank );

   /**** 
    * Highest rank persists CV 5, then evicts objects under G-prefetched and G-stored. 
    *
    * Satisfies:  An application will create multiple transactions in a container, then persist them
    * from the burst buffer storage to persistent storage, then evict several of the objects in the IOD
    * container, releasing space in the burst buffer.
    ****/

   /* Optionally, pause to show change in BB space using out-of-band check. All ranks paused. */
   if ( pause )  { 
      MPI_Barrier( MPI_COMM_WORLD );
         if ( my_rank == 0 ) {
            fprintf( stderr, ">>> PAUSED before persist/evict to allow BB space usage to be checked.\n" );
            fprintf( stderr, ">>> Press return to continue..." );
            getchar();
         }
      MPI_Barrier( MPI_COMM_WORLD );
   }

   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      
   if ( my_rank == comm_size-1 ) {
      fprintf( stderr, "APP-r%d: cv 5 - Persist\n", my_rank );
      ret = H5RCpersist(rc_id5, H5_EVENT_STACK_NULL); ASSERT_RET; 
      print_container_contents( file_id, rc_id5, "/", my_rank );

      evict_group_members_updates( file_id, rc_id5, "/G-prefetched", my_rank );
      evict_group_members_updates( file_id, rc_id5, "/G-stored", my_rank );
   }
   if ( workaround ) MPI_Barrier( MPI_COMM_WORLD );      

   /* Optionally, pause to show change in BB space using out-of-band check. All ranks paused. */
   if ( pause )  { 
      MPI_Barrier( MPI_COMM_WORLD );
      if ( my_rank == 0 ) {
         fprintf( stderr, ">>> PAUSED after persist/evict to allow BB space usage to be checked.\n" );
         fprintf( stderr, ">>> Press return to continue..." );
         getchar();
      }
      MPI_Barrier( MPI_COMM_WORLD );
   }
   
   /* All ranks print container here.  For rank == comm_size, this will be after objects have been evicted */
   if ( verbose ) print_container_contents( file_id, rc_id5, "/", my_rank ); 

   /* Release the read handle and close read context on CV 5 */
   ret = H5RCrelease( rc_id5, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id5 ); ASSERT_RET;
   fprintf( stderr, "APP-r%d: rc 5 - Released \n", my_rank );

   /****
    *  Close the H5File.  Reopen, pre-fetch D, M, and T in /G-prefetched.  
    *
    *  Satisfies: "... then closing the container.  The application will then re-open the container and asynchronously 
    *  pre-fetch multiple objects from persistent storage to burst buffer storage.
    ****/

   ret = H5Gclose_ff( grp_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Gclose_ff( grp_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Gclose_ff( grp_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Close the container, then barrier to make sure all ranks have closed it before continuing */
   fprintf( stderr, "APP-r%d: Close %s\n", my_rank, file_name );
   ret = H5Fclose_ff( file_id, 1, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Pclose( fapl_id ); ASSERT_RET;
   MPI_Barrier( MPI_COMM_WORLD );

   /* Define variables here so easier to move this into separate routine if desired (note: still rely on some defined earlier */
   hid_t dset_l_id, dset_p_id, dset_s_id;
   hid_t space_l_id, space_p_id, space_s_id;
   hrpl_t dset_p_replica;
   hid_t dxpl_p_id;
   hid_t dapl_p_id;
   int data_l[4], data_p[4], data_s[4];              

   hid_t map_l_id, map_p_id, map_s_id;
   hrpl_t map_p_replica;
   hid_t mxpl_p_id;
   hid_t mapl_p_id;
   int map_entries;                       /* know same number in all maps */
   int *value_l, *value_p, *value_s;              

   hid_t type_l_id, type_p_id, type_s_id;
   hrpl_t type_p_replica;
   hid_t tapl_p_id;

   /* Event stack */
   hid_t         d_es_id;        /* for prefetch of Dataset */
   hid_t         mt_es_id;       /* for prefetch of Map and dataType (shared event stack) */
   H5ES_status_t status;

   /* Create event stacks for managing asynchronous requests. */
   d_es_id = H5EScreate();  assert( d_es_id >= 0 );
   mt_es_id = H5EScreate();  assert( mt_es_id >= 0 );

   /* Reopen the container read-only and get a read context for highest CV */
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id >= 0 );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); ASSERT_RET;
   file_id = H5Fopen_ff( file_name, H5F_ACC_RDONLY, fapl_id, &rc_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );
   H5RCget_version( rc_id, &version );
   fprintf( stderr, "APP-r%d: Re-open %s\n", my_rank, file_name );

   /* Open Datasets, Maps and DataTypes in all 3 Groups */
   dset_l_id = H5Dopen_ff( file_id, "/G-logged/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_l_id >= 0 );
   space_l_id = H5Dget_space( dset_l_id ); assert ( space_l_id >= 0 );

   dset_p_id = H5Dopen_ff( file_id, "/G-prefetched/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_p_id >= 0 );
   space_p_id = H5Dget_space( dset_p_id ); assert ( space_p_id >= 0 ); 

   dset_s_id = H5Dopen_ff( file_id, "/G-stored/D", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( dset_s_id >= 0 );
   space_s_id = H5Dget_space( dset_s_id ); assert ( space_s_id >= 0 );

   map_l_id = H5Mopen_ff( file_id, "/G-logged/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_l_id >= 0 );

   map_p_id = H5Mopen_ff( file_id, "/G-prefetched/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_p_id >= 0 );

   map_s_id = H5Mopen_ff( file_id, "/G-stored/M", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( map_s_id >= 0 );

   type_l_id = H5Topen_ff( file_id, "G-logged/T", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( type_l_id >= 0 );

   type_p_id = H5Topen_ff( file_id, "G-prefetched/T", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( type_p_id >= 0 );

   type_s_id = H5Topen_ff( file_id, "G-stored/T", H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( type_s_id >= 0 );


   /* Optionally, pause to show change in BB space using out-of-band check. All ranks paused. */
   if ( pause )  { 
      MPI_Barrier( MPI_COMM_WORLD );
      if ( my_rank == 0 ) {
         fprintf( stderr, ">>> PAUSED before prefetch of replicas allow BB space usage to be checked.\n" );
         fprintf( stderr, ">>> Press return to continue..." );
         getchar();
      }
      MPI_Barrier( MPI_COMM_WORLD );
   }

   /* Prefetch objects in the /G-prefetched group. */
   if ( use_daos_lustre ) {

      /* Rank 0 prefetches asynchronously, using 2 different stacks */
      if ( my_rank == 0 ) {
         fprintf( stderr, "APP-r%d: Prefetch /G-prefetched/D asynchronously\n", my_rank );
         ret = H5Dprefetch_ff( dset_p_id, rc_id, &dset_p_replica, H5P_DEFAULT, d_es_id ); ASSERT_RET;

         fprintf( stderr, "APP-r%d: Prefetch /G-prefetched/M asynchronously\n", my_rank );
         ret = H5Mprefetch_ff( map_p_id, rc_id, &map_p_replica, H5P_DEFAULT, mt_es_id ); ASSERT_RET;

         fprintf( stderr, "APP-r%d: Prefetch /G-prefetched/T asynchronously\n", my_rank );
         ret = H5Tprefetch_ff( type_p_id, rc_id, &type_p_replica, H5P_DEFAULT, mt_es_id ); ASSERT_RET;
      }

      /* Note: could start some reads of non-prefetched objects here, but since I want to offer pause & read timing, I didn't */

      /* After dataset prefetch completes, bcast the replica ID and set up properties */
      if ( my_rank == 0 ) {
         H5ESwait_all( d_es_id, &status );
         fprintf( stderr, "APP-r%d: asynchronous prefetch of D completed with status %d\n", my_rank, status );
      }
      MPI_Bcast( &dset_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
      dxpl_p_id = H5Pcreate( H5P_DATASET_XFER );                              /* Used in Read */
      ret = H5Pset_read_replica( dxpl_p_id, dset_p_replica ); ASSERT_RET;
      dapl_p_id = H5Pcreate( H5P_DATASET_ACCESS );                            /* Used in Evict */
      ret = H5Pset_evict_replica( dapl_p_id, dset_p_replica ); ASSERT_RET;

      /* After both map and datatype prefetches complete, bcast the replica IDs and set up properties */
      if ( my_rank == 0 ) {
         H5ESwait_all( mt_es_id, &status );
         fprintf( stderr, "APP-r%d: asynchronous prefetch of M and T completed with status %d\n", my_rank, status );
      }
      MPI_Bcast( &map_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
      mxpl_p_id = H5Pcreate( H5P_DATASET_XFER );                              /* Used in Get */
      ret = H5Pset_read_replica( mxpl_p_id, map_p_replica );  ASSERT_RET;
      mapl_p_id = H5Pcreate( H5P_MAP_ACCESS );                                /* Used in Evict */
      ret = H5Pset_evict_replica( mapl_p_id, map_p_replica ); ASSERT_RET;

      MPI_Bcast( &type_p_replica, 1, MPI_UINT64_T, 0, MPI_COMM_WORLD );
      tapl_p_id = H5Pcreate( H5P_DATATYPE_ACCESS );                           /* Used in Evict */
      ret = H5Pset_evict_replica( tapl_p_id, type_p_replica ); ASSERT_RET;

   } else {
      /* Without daos-lustre, we don't do prefetch, so default properties */
      dxpl_p_id = H5P_DEFAULT;
      dapl_p_id = H5P_DEFAULT;
      mxpl_p_id = H5P_DEFAULT;
      mapl_p_id = H5P_DEFAULT;
      tapl_p_id = H5P_DEFAULT;
   }

   /* Optionally, pause to show change in BB space using out-of-band check. All ranks paused. */
   if ( pause )  { 
      MPI_Barrier( MPI_COMM_WORLD );
      if ( my_rank == 0 ) {
         fprintf( stderr, ">>> PAUSED after prefetch of replicas to allow BB space usage to be checked.\n" );
         fprintf( stderr, ">>> Press return to continue..." );
         getchar();
      }
      MPI_Barrier( MPI_COMM_WORLD );
   }

   /* Find entries in first Map, then use to create value vectors for all three Map objects - we know they have same size. */
   ret = H5Mget_count_ff( map_l_id, &map_entries, rc_id, H5_EVENT_STACK_NULL );  ASSERT_RET;
   value_l = (int *)calloc( map_entries, sizeof(int) ); 
   value_p = (int *)calloc( map_entries, sizeof(int) ); 
   value_s = (int *)calloc( map_entries, sizeof(int) ); 

   /* Read Datasets and then Maps in all 3 Groups, measuring time it takes to read data for each*/

   if ( time_reads ) {
      ulong usec_dl, usec_dp, usec_ds;                   // per-read elapsed time for for datasets
      ulong usec_ml, usec_mp, usec_ms;                   // per-read elapsed time for maps

      ulong all_usec_dl, all_usec_dp, all_usec_ds;       // total read time for datasets
      ulong all_usec_ml, all_usec_mp, all_usec_ms;       // total read time for maps

      all_usec_dl = all_usec_dp = all_usec_ds = 0;
      all_usec_ml = all_usec_mp = all_usec_ms = 0;

      int t;

      for ( t = 0; t < time_reads; t++ ) {

         START_TIME;
         ret = H5Dread_ff( dset_l_id, H5T_NATIVE_INT, space_l_id, space_l_id, H5P_DEFAULT, data_l, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         usec_dl = ELAPSED_TIME;
         all_usec_dl += usec_dl;
         fprintf( stderr, "APP-r%d Read Time %d for /G-logged/D: %lu usec\n", my_rank, t, usec_dl );
      
         START_TIME;
         ret = H5Dread_ff( dset_p_id, H5T_NATIVE_INT, space_p_id, space_p_id, dxpl_p_id, data_p, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         usec_dp = ELAPSED_TIME;
         all_usec_dp += usec_dp;
         fprintf( stderr, "APP-r%d Read Time %d for /G-prefetched/D: %lu usec\n", my_rank, t, usec_dp );
      
         START_TIME;
         ret = H5Dread_ff( dset_s_id, H5T_NATIVE_INT, space_s_id, space_s_id, H5P_DEFAULT, data_s, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
         END_TIME;
         usec_ds = ELAPSED_TIME;
         all_usec_ds += usec_ds;
         fprintf( stderr, "APP-r%d Read Time %d for /G-stored/D: %lu usec\n", my_rank, t, usec_ds );
      
         START_TIME;
         for ( i = 0; i < map_entries; i++ ) {
            ret = H5Mget_ff( map_l_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_l[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
         }
         END_TIME;
         usec_ml = ELAPSED_TIME;
         all_usec_ml += usec_ml;
         fprintf( stderr, "APP-r%d Read Time %d for /G-logged/M: %lu usec\n", my_rank, t, usec_ml );
      
         START_TIME;
         for ( i = 0; i < map_entries; i++ ) {
            ret = H5Mget_ff( map_p_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_p[i], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
         }
         END_TIME;
         usec_mp = ELAPSED_TIME;
         all_usec_mp += usec_mp;
         fprintf( stderr, "APP-r%d Read Time %d for /G-prefetched/M: %lu usec\n", my_rank, t, usec_mp );
      
         START_TIME;
         for ( i = 0; i < map_entries; i++ ) {
            ret = H5Mget_ff( map_s_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_s[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
            ASSERT_RET;
         }
         END_TIME;
         usec_ms = ELAPSED_TIME;
         all_usec_ms += usec_ms;
         fprintf( stderr, "APP-r%d Read Time %d for /G-stored/M: %lu usec\n", my_rank, t, usec_ms );
   
      }

      if ( time_reads > 1 ) {
         fprintf( stderr, "APP-r%d Read Time Avg for /G-logged/D: %lu usec\n", my_rank, all_usec_dl/time_reads );
         fprintf( stderr, "APP-r%d Read Time Avg for /G-prefetched/D: %lu usec\n", my_rank, all_usec_dp/time_reads );
         fprintf( stderr, "APP-r%d Read Time Avg for /G-stored/D: %lu usec\n", my_rank, all_usec_ds/time_reads );
         fprintf( stderr, "APP-r%d Read Time Avg for /G-logged/M: %lu usec\n", my_rank, all_usec_ml/time_reads );
         fprintf( stderr, "APP-r%d Read Time Avg for /G-prefetched/M: %lu usec\n", my_rank, all_usec_mp/time_reads );
         fprintf( stderr, "APP-r%d Read Time Avg for /G-stored/M: %lu usec\n", my_rank, all_usec_ms/time_reads );
      }
   
   } else {
      fprintf( stderr, "APP-r%d Read /G-logged/D\n", my_rank  );
      ret = H5Dread_ff( dset_l_id, H5T_NATIVE_INT, space_l_id, space_l_id, H5P_DEFAULT, data_l, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;

      fprintf( stderr, "APP-r%d Read /G-prefetched/D\n", my_rank  );
      ret = H5Dread_ff( dset_p_id, H5T_NATIVE_INT, space_p_id, space_p_id, dxpl_p_id, data_p, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;

      fprintf( stderr, "APP-r%d Read /G-stored/D\n", my_rank  );
      ret = H5Dread_ff( dset_s_id, H5T_NATIVE_INT, space_s_id, space_s_id, H5P_DEFAULT, data_s, rc_id, H5_EVENT_STACK_NULL ); 
      ASSERT_RET;

      fprintf( stderr, "APP-r%d Read /G-logged/M\n", my_rank  );
      for ( i = 0; i < map_entries; i++ ) {
         ret = H5Mget_ff( map_l_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_l[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }

      fprintf( stderr, "APP-r%d Read /G-prefetched/M\n", my_rank  );
      for ( i = 0; i < map_entries; i++ ) {
         ret = H5Mget_ff( map_p_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_p[i], mxpl_p_id, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }

      fprintf( stderr, "APP-r%d Read /G-stored/M\n", my_rank  );
      for ( i = 0; i < map_entries; i++ ) {
         ret = H5Mget_ff( map_s_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value_s[i], H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }
   }

   /* Print Dataset and Map values for all 3 groups */
   fprintf( stderr, "APP-r%d: /G-logged/D data: %d %d %d %d\n", my_rank, data_l[0], data_l[1], data_l[2], data_l[3] );
   fprintf( stderr, "APP-r%d: /G-prefetched/D data: %d %d %d %d\n", my_rank, data_p[0], data_p[1], data_p[2], data_p[3]);
   fprintf( stderr, "APP-r%d: /G-stored/D data: %d %d %d %d\n", my_rank, data_s[0], data_s[1], data_s[2], data_s[3] );

   if ( map_entries == 4 ) {
      fprintf( stderr, "APP-r%d: /G-logged/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_l[0], value_l[1], value_l[2], value_l[3] );
      fprintf( stderr, "APP-r%d: /G-prefetched/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_p[0], value_p[1], value_p[2], value_p[3] );
      fprintf( stderr, "APP-r%d: /G-stored/M key/values: 0/%d 1/%d 2/%d 3/%d\n", my_rank, 
                    value_s[0], value_s[1], value_s[2], value_s[3] );
   } else {
      fprintf( stderr, "APP-r%d: /G-logged/M key/values: 0/%d 1/%d 2/%d 3/%d ... %d/%d\n", my_rank, 
                    value_l[0], value_l[1], value_l[2], value_l[3], map_entries-1, value_l[map_entries-1] );
      fprintf( stderr, "APP-r%d: /G-prefetched/M key/values: 0/%d 1/%d 2/%d 3/%d ... %d/%d\n", my_rank, 
                    value_p[0], value_p[1], value_p[2], value_p[3], map_entries-1, value_p[map_entries-1] );
      fprintf( stderr, "APP-r%d: /G-stored/M key/values: 0/%d 1/%d 2/%d 3/%d ... %d/%d\n", my_rank, 
                    value_s[0], value_s[1], value_s[2], value_s[3], map_entries-1, value_s[map_entries-1] );
   }
   free( value_l );
   free( value_p );
   free( value_s );

   /* Rank 0 evicts replicas */
   if ( my_rank == 0 ) {
      if ( use_daos_lustre ) {
         fprintf( stderr, "APP-r%d: Evict replica of /G-prefetched/D\n", my_rank );
         ret = H5Devict_ff( dset_p_id, version, dapl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  /* Note: CV redundant for replica */

         fprintf( stderr, "APP-r%d: Evict replica of /G-prefetched/M (Q7 note: currently a no-op in IOD)\n", my_rank );
         ret = H5Mevict_ff( map_p_id, version, mapl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;   /* Note: CV redundant for replica */

         fprintf( stderr, "APP-r%d: Evict replica of /G-prefetched/T\n", my_rank );
         ret = H5Tevict_ff( type_p_id, version, tapl_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;  /* Note: CV redundant for replica */
      } else {
         fprintf( stderr, "APP-r%d DAOS-POSIX - No replicas to evict\n", my_rank );
      }
   }

   /* Optionally, pause to show change in BB space using out-of-band check. All ranks paused. */
   if ( pause )  { 
      MPI_Barrier( MPI_COMM_WORLD );
      if ( my_rank == 0 ) {
         fprintf( stderr, ">>> PAUSED after evict of replicas to allow BB space usage to be checked.\n" );
         fprintf( stderr, ">>> Press return to continue..." );
      getchar();
      }
      MPI_Barrier( MPI_COMM_WORLD );
   }

   /* Close Objects*/

   if ( use_daos_lustre ) {
      ret = H5Pclose( mapl_p_id ); ASSERT_RET;
      ret = H5Pclose( mxpl_p_id ); ASSERT_RET;
      ret = H5Pclose( dapl_p_id ); ASSERT_RET;
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

   ret = H5Tclose_ff( type_l_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Tclose_ff( type_p_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Tclose_ff( type_s_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Release the read handle and close read context  */
   ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id ); ASSERT_RET;

   /* Close H5 Objects that are still open */
   fprintf( stderr, "APP-r%d: Close %s\n", my_rank, file_name );
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
      fprintf( stderr, "APP-r%d: tr %lu - Create %s%s with data: %d %d %d %d\n",
               my_rank, tr_num, obj_path, dset_name, data[0], data[1], data[2], data[3] );
      ret = H5Dwrite_ff( dset_id, dtype_id, space_id, space_id, H5P_DEFAULT, data, tr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "APP-r%d: tr %lu - Create %s%s - FAILED\n", my_rank, tr_num, obj_path, dset_name );
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
   int      num_kvs = 4;           /* Start with 4 KV pairs */
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
      fprintf( stderr, "APP-r%d: tr %lu - Create %s%s with keys/values: 0/%d 1/%d 2/%d 3/%d\n",
               my_rank, tr_num, obj_path, map_name, value[0], value[1], value[2], value[3] );
      for ( i = 0; i < num_kvs; i++ ) {
         ret = H5Mset_ff( map_id, H5T_STD_I32LE, &i, H5T_STD_I32LE, &value[i], H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL ); 
         ASSERT_RET;
      }
      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "APP-r%d: tr %lu - Create %s%s - FAILED\n", my_rank, tr_num, obj_path, map_name );
   }
   return;
}

/*
 * Helper function used to create named datatype "type_name"
 * in group identified by "obj_id"
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" are used in the status output.
 */
void
create_named_datatype( hid_t obj_id, const char* type_name, hid_t tr_id, const char* obj_path, int my_rank ) {

   herr_t   ret;
   uint64_t tr_num;
   hid_t    dtype_id;

   /* Setup */
   ret = H5TRget_trans_num( tr_id, &tr_num ); ASSERT_RET;
   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id >= 0 );

   /* Create */
   ret = H5Tcommit_ff( obj_id, type_name, dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   fprintf( stderr, "APP-r%d: tr %lu - Create %s%s %s\n", my_rank, tr_num, obj_path, type_name, STATUS );
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
         fprintf( stderr, "APP-r%d: tr %lu - Update %s%s[%d] to %d %s\n", 
                  my_rank, tr_num, obj_path, dset_name, my_rank, cell_data, STATUS );
         ret = H5Sclose( memspace_id ); ASSERT_RET;

      } else {
         if ( verbose ) {
            fprintf( stderr, "APP-r%d: tr %lu - No Update to %s%s for this rank \n", my_rank, tr_num, obj_path, dset_name );
         }
      }

      ret = H5Sclose( filespace_id ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   } else {
      fprintf( stderr, "APP-r%d: tr %lu - Update %s%s - FAILED\n", my_rank, tr_num, obj_path, dset_name );
   }
   return;
}

/*
 * Helper function used to update (or add) key/value data in "map_name"
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
      if ( verbose || ( ret < 0 ) ) {
         fprintf( stderr, "APP-r%d: tr %lu - Update %s%s with key/value %d/%d %s\n", 
                  my_rank, tr_num, obj_path, map_name, my_rank, value, STATUS );
      }

      ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   } else {
      fprintf( stderr, "APP-r%d: tr %lu - Update %s%s - FAILED\n", my_rank, tr_num, obj_path, map_name );
   }
   return;
}

/*
 * evict_group_members_updates -  evict logged updates for (expected) members of specified group 
 *  parameters:
 *    file_id: identifies container
 *    rc_id: identifies read context
 *    grp_path: path that specifies group
 *    my_rank: identifies rank doing the evict
 */
void
evict_group_members_updates( hid_t file_id, hid_t rc_id, const char* grp_path, int my_rank )
{
   uint64_t cv; char preface[128];    
   char path_to_object[1024];

   hid_t dset_id, map_id, type_id;
   herr_t ret;
   int i;

   /* Get the container version for the read context and set up print preface */
   ret = H5RCget_version( rc_id, &cv ); ASSERT_RET;
   sprintf( preface, "APP-r%d: cv %lu -", my_rank, cv );

   /* Dataset */
   sprintf( path_to_object, "%s/%s", grp_path, "D" );
   dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
   assert( dset_id >= 0 );

   if ( use_daos_lustre ) {
      ret = H5Devict_ff( dset_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
      fprintf( stderr, "%s Evicted updates for %s\n", preface, path_to_object );
   } else {
      fprintf( stderr, "%s DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
   }
   ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Map */
   sprintf( path_to_object, "%s/%s", grp_path, "M" );
   map_id = H5Mopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
   assert( map_id >= 0 );

   if ( use_daos_lustre ) {
      ret = H5Mevict_ff( map_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
      fprintf( stderr, "%s Evicted updates for %s (Q7 note: currently a no-op in IOD)\n", preface, path_to_object );
   } else {
      fprintf( stderr, "%s DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
   }
   ret = H5Mclose_ff( map_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   /* Named Datatype */
   sprintf( path_to_object, "%s/%s", grp_path, "T" );
   type_id = H5Topen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL );
   assert( type_id >= 0 );

   if ( use_daos_lustre ) {
      ret = H5Tevict_ff( type_id, cv, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;
      fprintf( stderr, "%s Evicted updates for %s\n", preface, path_to_object );
   } else {
      fprintf( stderr, "%s DAOS-POSIX - Won't evict updates for object %s\n", preface, path_to_object );
   }
   ret = H5Tclose_ff( type_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   return;
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

      sprintf( line, "%s %s data: ", preface, path_to_object );
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

   /* Named Datatypes */
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
 * parse_options - parse command line options
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
               use_daos_lustre = 0;
               break;
            case 'p':
               pause = 1;
               break;
            case 't':   
               if ( ( --argc == 0 )  || (  **(argv+1) == '-' ) ) {
                  if ( my_rank == 0 ) {
                     printf( "Error: No repeat_cnt specified after -t option.\n" );
                     usage( app );
                  }
                  return( 1 );
               } else {
                  ++argv;
                  time_reads = atoi( *argv );
                  if ( my_rank == 0 ) {
                     printf( "Will repeat read sequence %d times to get average elapsed time per read.\n", time_reads );
                  }
               }
               break;
            case 'v':   
               verbose = 1;
               break;
            case 'w':   
               workaround = 0;
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
   printf( "Usage: %s [-l] [-p] [-t repeat_cnt] [-v] [-w]\n", app  );
   printf( "\tl: don't use DAOS Lustre\n" );
   printf( "\tp: pause to allow out-of-band BB space check\n" );
   printf( "\tt: time reads after container re-open, performing read sequence 'repeat_cnt' times\n" );
   printf( "\tv: verbose output\n" );
   printf( "\tw: disable barriers added as Q7 workaround to race conditions\n" );
}
