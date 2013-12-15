/*
 * M6.2-demo.c: This is the demo program for Milestone 6.2 - HDF5 and IO Dispatcher Container Versioning Demonstration.
 * This program runs on one or more compute nodes and makes calls to the HDF5 API.
 * The Function Shipper server should be running before this demo program is started.
 */

/*
 * DEFINES that affect behavior
 * SHOW_H5A_DELETE_BY_NAME_ISSUE - define this if you want to debug the issue with H5Adelete_by_name when -a flag used.
 * - EXTEND_WORKING   If defined, datasets are extended.  If not defined, only a message it printed.
 */

#define SHOW_H5A_DELETE_BY_NAME_ISSUE
//#define EXTEND_WORKING

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

#define ATTR_STR_LEN 128

#define STATUS (ret >= 0) ? "OK" : "FAILED"
#define ASSERT_RET assert( ret >= 0 )

/* option flags */
int verbose = 0;     // Verbose defaults to no
int abort3 = 0;      // Abort Transaction 3 defaults to no

/* prototypes for helper functions */
void create_string_attribute( hid_t, const char*, hid_t, const char*, int, uint64_t );
void create_group( hid_t, const char*, hid_t, const char*, int, uint64_t );
void create_dataset( hid_t, const char*, hid_t, const char*, int, uint64_t, int );
void create_committed_datatype( hid_t, const char*, hid_t, const char*, int, uint64_t );
void update_dataset( hid_t, const char*, hid_t, hid_t, const char*, int, uint64_t, int );
void extend_dataset( hid_t, const char*, hid_t, hid_t, const char*, int, uint64_t, int );
void print_container_contents( hid_t, hid_t, const char*, int );
int  parse_options( int argc, char** argv);

int main( int argc, char **argv ) {
   const char file_name[]="m6.2-eff_container.h5";

   int my_rank, comm_size;
   int provided;

   hid_t fapl_id;
   hid_t file_id;
   herr_t ret;

   uint64_t version;
   hid_t rc_id, rc_id0, rc_id1, rc_id2, rc_id3, rc_id4;

   /* Check for MPI multi-thread support */
   MPI_Init_thread( &argc, &argv, MPI_THREAD_MULTIPLE, &provided );
   if ( MPI_THREAD_MULTIPLE != provided ) {
      fprintf( stderr, "M6.2: ERROR: MPI does not have MPI_THREAD_MULTIPLE support\n" );
      exit( 1 );
   }

   /* Parse command-line options controlling behavior */
   if ( parse_options( argc, argv ) != 0 ) {
      exit( 1 );
   }

   MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
   MPI_Comm_size( MPI_COMM_WORLD, &comm_size );
   fprintf( stderr, "M6.2-r%d: Number of MPI processes = %d\n", my_rank, comm_size );

   

   /* Initialize the EFF stack. */
   fprintf( stderr, "M6.2-r%d: Initialize EFF stack (Step 1)\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Specify that the IOD VOL plugin should be used and create H5File (EFF container) */
   fprintf( stderr, "M6.2-r%d: Create the container %s (Step 2)\n", my_rank, file_name );
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id >= 0 );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); ASSERT_RET;
   file_id = H5Fcreate_ff( file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );

   /* Acquire a read handle for container version 0 and create a read context. */
   version = 0;
   fprintf( stderr, "M6.2-r%d: Acquire read context for container version %d (Step 3)\n", my_rank, (int)version );
   rc_id0 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET; assert( version == 0 );

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 1st call to print container contents (Step 4)\n", my_rank );
   if ( verbose ) print_container_contents( file_id, rc_id0, "/", my_rank );

   /* 
    * Transaction 1 is created, started, updated, and committed by rank 0.
    * Root group attributes AA and AB are created and populated.
    * Groups /GA and /GB are created.
    * Datasets /DA and /DB are created and populated.
    * Committed datatypes /TA and /TB are created.
    */
   if ( my_rank == 0 ) {

      uint64_t tr_num = 1;
      hid_t tr_id;

      /* Create a local transaction for transaction number. */
      fprintf( stderr, "M6.2-r%d: Create tr %d (Step 5 - begin) \n", my_rank, (int)tr_num );
      tr_id = H5TRcreate( file_id, rc_id0, (uint64_t)tr_num ); assert( tr_id >= 0 );

      /* Start transaction 1 with single transaction leader (the default) */
      fprintf( stderr, "M6.2-r%d: Start tr %d\n", my_rank, (int)tr_num );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Add updates to the transaction */

      /* 2 Attributes */
      create_string_attribute( file_id, "AA", tr_id, "/", my_rank, tr_num );
      create_string_attribute( file_id, "AB", tr_id, "/", my_rank, tr_num );
    
      /* 2 Datasets */
      create_dataset( file_id, "DA", tr_id, "/", my_rank, tr_num, 1 );
      create_dataset( file_id, "DB", tr_id, "/", my_rank, tr_num, 2 );

      /* 2 committed datatypes */
      create_committed_datatype( file_id, "TA", tr_id, "/", my_rank, tr_num );
      create_committed_datatype( file_id, "TB", tr_id, "/", my_rank, tr_num );

      /* 2 Groups */
      create_group( file_id, "GA", tr_id, "/", my_rank, tr_num );
      create_group( file_id, "GB", tr_id, "/", my_rank, tr_num );

      /* Finish and commit transaction 1, causing the updates to appear in container version 1. */
      fprintf( stderr, "M6.2-r%d: Finish and commit tr %d (Step 5 - end)\n", my_rank, (int)tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Close the local transaction. */
      ret = H5TRclose( tr_id ); ASSERT_RET;
   }

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 2nd call to print container contents (Step 6)\n", my_rank );
   if (verbose) print_container_contents( file_id, rc_id0, "/", my_rank );

   /* Acquire a read handle for container version 1 and create a read context. */
   version = 1;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 7)\n", my_rank, (int)version );
   rc_id1 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( rc_id1 < 0 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv %d - sleep then retry\n", my_rank, (int)version );
      sleep( 1 );
      version = 1;      
      rc_id1 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }

   assert( rc_id1 >= 0 ); assert ( version == 1 );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv 1\n", my_rank );
   fprintf( stderr, "M6.2-r%d: 3rd call to print container contents (Step 8)\n", my_rank );
   if (verbose) print_container_contents( file_id, rc_id1, "/", my_rank ); ASSERT_RET;

   /* Release the read handle and close read context on cv 0 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 0 (Step 9)\n", my_rank );
   ret = H5RCrelease( rc_id0, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id0 ); ASSERT_RET;

   /* 
    * Transaction 2 is created, started, updated, and committed by ranks 0 & 1, if 2 or more MPI processes.
    * Group /GA attributes AA and AB are created and populated by rank 0.
    * Group /GB attributes AA and AB are created and populated by rank 1.
    * Groups /GA/GA and /GA/GB are created by rank 0.
    * Groups /GB/GA and /GB/GB are created by rank 1.
    * Datasets /GA/DA and /GA/DB are created and populated by rank 0.
    * Datasets /GB/DA and /GB/DB are created and populated by rank 1.
    * Committed Datatypes /GA/AA and /GA/TB are created by rank 0.
    * Committed Datatypes /GB/AA and /GB/TB are created by rank 1.
    */
   if ( (my_rank == 0) || (my_rank == 1) ) {

      uint64_t tr_num = 2;
      hid_t tr_id;
      hid_t trspl_id;
      hid_t gr_id;
      char gr_path[128];

      /* Create a local transaction for transaction number 2. */
      tr_id = H5TRcreate( file_id, rc_id1, tr_num ); assert( tr_id >= 0 );

      /* Start transaction 2 with two transaction leaders (unless there is only 1 MPI process) */
      fprintf( stderr, "M6.2-r%d: Start tr %d (Step 10 - begin)\n", my_rank, (int)tr_num );
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
      if ( comm_size >= 2 ) {
         ret = H5Pset_trspl_num_peers( trspl_id, 2 ); ASSERT_RET;
      } else {
         ret = H5Pset_trspl_num_peers( trspl_id, 1 ); ASSERT_RET;
      }
      ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Add updates to the transaction */
      if ( my_rank == 0 ) {
         gr_id = H5Gopen_ff( file_id, "GA", H5P_DEFAULT, rc_id1, H5_EVENT_STACK_NULL ); assert( gr_id >= 0 );
         sprintf( gr_path, "/GA/" );
      } else {
         gr_id = H5Gopen_ff( file_id, "GB", H5P_DEFAULT, rc_id1, H5_EVENT_STACK_NULL ); assert( gr_id >= 0 );
         sprintf( gr_path, "/GB/" );
      }

      /* 2 Attributes in each group */
      create_string_attribute( gr_id, "AA", tr_id, gr_path, my_rank, tr_num );
      create_string_attribute( gr_id, "AB", tr_id, gr_path, my_rank, tr_num );

      /* 2 1-D Datasets */
      create_dataset( gr_id, "DA", tr_id, gr_path, my_rank, tr_num, 1 );
      create_dataset( gr_id, "DB", tr_id, gr_path, my_rank, tr_num, 2 );

      /* 2 committed datatypes */
      create_committed_datatype( gr_id, "TA", tr_id, gr_path, my_rank, tr_num );
      create_committed_datatype( gr_id, "TB", tr_id, gr_path, my_rank, tr_num );

      /* 2 Groups */
      create_group( gr_id, "GA", tr_id, gr_path, my_rank, tr_num );
      create_group( gr_id, "GB", tr_id, gr_path, my_rank, tr_num );

      /* Close the group */
      ret = H5Gclose_ff ( gr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Finish and commit transaction 2, causing the updates to appear in container version 2. */
      fprintf( stderr, "M6.2-r%d: Finish and commit tr %d (Step 10 - end)\n", my_rank, (int)tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;

      /* Close the local transaction and transaction start property list. */
      ret = H5TRclose( tr_id ); ASSERT_RET;
      ret = H5Pclose( trspl_id ); ASSERT_RET;
   }

   fprintf( stderr, "M6.2-r%d: 4th call to print container contents (Step 11)\n", my_rank );
   if (verbose) print_container_contents( file_id, rc_id1, "/", my_rank ); ASSERT_RET;

   version = 2;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 12)\n", my_rank, (int)version );
   rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( rc_id2 < 0 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 2 - sleep then retry\n", my_rank );
      sleep( 1 );
      version = 2;
      rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }
   assert( rc_id2 >= 0 ); assert( version == 2 );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv 2\n", my_rank );
   fprintf( stderr, "M6.2-r%d: 5th call to print container contents (Step 13)\n", my_rank );
   if (verbose) print_container_contents( file_id, rc_id2, "/", my_rank ); ASSERT_RET;

   /* Release the read handle and close read context on cv 1 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 1 (Step 14)\n", my_rank );
   ret = H5RCrelease( rc_id1, H5_EVENT_STACK_NULL); ASSERT_RET;
   ret = H5RCclose( rc_id1 ); ASSERT_RET;

   /* 
    * Transaction 4 is created and started by all MPI processes.
    * Transaction 3 is created and started by all MPI processes.
    * Updates are added to the transactions in an interleaved manner, 
    * with the ranks progressing independent of each other.  
    * The sequence of operations in the code is:
    *    1) /GA/GA/GB added to Tr 4 by rank 0
    *    2) /GB/GB/GB added to Tr 4 by rank 1
    *    3) AA@/ deleted in Tr 3 by rank 0
    *    4) /GA/DA deleted in Tr 3 by rank 1
    *    5) /GA/TB deleted in Tr 4 by rank 0
    *    6) /DB deleted in Tr 4 by rank 1
    *    7) /GB/GA deleted in Tr 3 by rank 0
    *    8) /GA/TA deleted in Tr 3 by rank 1
    *    9) /GA/GA/GA added in Tr 3 by rank 0
    *    10) /GB/TA deleted in Tr 3 by rank 1
    *    11) /GB/GB/DB added in Tr 3 by rank 1
    * Transaction 3 is finished and committed.
    *    12) /GB/GB/GA added in Tr 4 by rank 1
    * Transaction 4 is finished and committed.
    *
    * Bracket in {}'s to localize variables, and to later make it easier to restrict ranks participating if desired.
    * 
    * Transaction 3 may be aborted by on process before it's started on others.  
    * We switch to printing the status of the calls that start and add updates to transactions rather than assert they
    * are successful.
    */
   {
      uint64_t tr_num3 = 3;
      uint64_t tr_num4 = 4;
      hid_t tr_id3, tr_id4;
      hid_t trspl_id;
   
      /* Set up transaction start property list with the number of transaction leaders */
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id >= 0 );
      ret = H5Pset_trspl_num_peers( trspl_id, comm_size ); ASSERT_RET;
   
      /* Create & start transaction 4, based on cv 2  */
      tr_id4 = H5TRcreate( file_id, rc_id2, tr_num4 ); assert( tr_id4 >= 0 );
      ret = H5TRstart( tr_id4, trspl_id, H5_EVENT_STACK_NULL ); 
      fprintf( stderr, "M6.2-r%d: Start tr %d (Step 15a - begin) - %s\n", my_rank, (int)tr_num4, STATUS );
   
      /* Create & start transaction 3, based on cv 2  */
#ifdef SHOW_H5A_DELETE_BY_NAME_ISSUE
if (my_rank == 0 ) sleep (5);
#endif
      tr_id3 = H5TRcreate( file_id, rc_id2, tr_num3 ); assert( tr_id3 >= 0 );
      ret = H5TRstart( tr_id3, trspl_id, H5_EVENT_STACK_NULL ); 
      fprintf( stderr, "M6.2-r%d: Start tr %d (Step 15b - begin) - %s\n", my_rank, (int)tr_num3, STATUS );
   
      /* Close the transaction start property list. */
      ret = H5Pclose( trspl_id ); ASSERT_RET;

      /* Add updates to the transactions */
      /*    1) /GA/GA/GB added to Tr 4 by rank 0   */
      if ( my_rank == 0 ) {
         create_group( file_id, "/GA/GA/GB", tr_id4, "/", my_rank, tr_num4 );
      } 
      /*    2) /GB/GB/GB added to Tr 4 by rank 1   */
      if ( my_rank == 1 ) {
         create_group( file_id, "/GB/GB/GB", tr_id4, "/", my_rank, tr_num4 );
      }
      /*    3) AA@/ deleted in Tr 3 by rank 0      */
      if ( my_rank == 0 ) {
         // ISSUE:  When you run with -a and another rank aborts transaction 3 before H5Adelete_by_name_ff is called 
         // here by rank 0, the file close will fail because not all objects are closed.
         // Coded here so that call won't be made if running with -a unless we're in debug mode and 
         // SHOW_H5A_DELETE_BY_NAME_ISSUE is defined.  Clean up once issue fixed in lower layers.
         if ( abort3 == 0 ) {
            ret = H5Adelete_by_name_ff( file_id, ".", "AA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
            fprintf( stderr, "M6.2-r%d: delete AA @ / in tr %d - %s\n", my_rank, (int)tr_num3, STATUS );
         } else {
#ifdef SHOW_H5A_DELETE_BY_NAME_ISSUE
            ret = H5Adelete_by_name_ff( file_id, ".", "AA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
            fprintf( stderr, "M6.2-r%d: delete AA @ / in tr %d - %s\n", my_rank, (int)tr_num3, STATUS );
#endif
         }
      }
      /*    4) /GA/DA deleted in Tr 3 by rank 1    */
      if ( my_rank == 1 ) {
         ret = H5Ldelete_ff( file_id, "/GA/DA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /GA/DA in tr %d - %s\n", my_rank, (int)tr_num3, STATUS);
      }
      /*    5) /GA/TB deleted in Tr 4 by rank 0    */
      if ( my_rank == 0 ) {
         ret = H5Ldelete_ff( file_id, "/GA/TB", H5P_DEFAULT, tr_id4, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /GA/TB in tr %d - %s\n", my_rank, (int)tr_num4, STATUS );
      }
      /*    6) /DB deleted in Tr 4 by rank 1       */
      if ( my_rank == 1 ) {
         ret = H5Ldelete_ff( file_id, "/DB", H5P_DEFAULT, tr_id4, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /DB in tr %d - %s \n", my_rank, (int)tr_num4, STATUS );
      }
      /*    7) /GB/GA deleted in Tr 3 by rank 0    */
      if ( my_rank == 0 ) {
         ret = H5Ldelete_ff( file_id, "/GB/GA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /GB/GA in tr %d - %s\n", my_rank, (int)tr_num3, STATUS );
      }
      /*    8) /GA/TA deleted in Tr 3 by rank 1    */
      if ( my_rank == 1  ) {
         ret = H5Ldelete_ff( file_id, "/GA/TA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /GA/TA in tr %d - %s\n", my_rank, (int)tr_num3, STATUS );
      }
      /*    9) /GA/GA/GA added in Tr 3 by rank 0   */
      if ( my_rank == 0 ) {
         create_group( file_id, "/GA/GA/GA", tr_id3, "/", my_rank, tr_num3 );
      } 
      /*    10) /GB/TA deleted in Tr 3 by rank 1   */
      if ( my_rank == 1 ) {
         ret = H5Ldelete_ff( file_id, "/GB/TA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); 
         fprintf( stderr, "M6.2-r%d: delete /GB/TA in tr %d - %s\n", my_rank, (int)tr_num3, STATUS );
      }
      /*    11) /GB/GB/DB added in Tr 3 by rank 1  */
      if ( my_rank == 1 ) {
         create_dataset( file_id, "/GB/GB/DB", tr_id3, "/", my_rank, tr_num3, 2 );
      } 
      /*    12) /DA updated in Tr 3 by all ranks   */
      update_dataset( file_id, "/DA", tr_id3, rc_id2, "/", my_rank, tr_num3, 1 );

      /*    13) /GB/DA updated in Tr 3 by rank 0 if > 1 process   */
      if ( my_rank == 0 ) {
         update_dataset( file_id, "/GB/DA", tr_id3, rc_id2, "/", my_rank, tr_num3, 1 );
      }

      /*    14) /GA/DB extended in Tr 3 by rank 0  */
      if ( my_rank == 0 ) {
         extend_dataset( file_id, "/GA/DB", tr_id3, rc_id2, "/", my_rank, tr_num3, 1 );
      }

      /*    Abort - we show all ranks calling abort, but a single rank can make the call and have the same effect   */
      if ( abort3 ) {
         ret = H5TRabort( tr_id3, H5_EVENT_STACK_NULL ); 
         fprintf( stderr, "M6.2-r%d: ABORT tr %d (Step 15b - abort) - %s\n", my_rank, (int)tr_num3, STATUS );
      }

      /* Finish and commit transaction 3, causing the updates to appear in container version 3. */
      ret = H5TRfinish( tr_id3, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); 
      fprintf( stderr, "M6.2-r%d: Finish and commit tr %d (Step 15b - end) - %s\n", my_rank, (int)tr_num3, STATUS );
   
      ret = H5TRclose( tr_id3 ); ASSERT_RET;

      /* Get read context for CV 3, then print the contents of container */
      /* Limit the number of tries, because if it was aborted we'll never get it */
      int max_tries = 4;
      int current_try = 0;
      version = 3;
      fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 15c)\n", my_rank, (int)version );
      rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      while ( (rc_id3 < 0) && (current_try < max_tries) ) {
         fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 3 - sleep then retry\n", my_rank );
         sleep( 1 );
         current_try++;
         version = 3;
         rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      }
      if ( rc_id3 >= 0 ) {
         assert( version == 3 );
         fprintf( stderr, "M6.2-r%d: Acquired read context for cv 3\n", my_rank );
         fprintf( stderr, "M6.2-r%d: 6th call to print container contents (Step 15d)\n", my_rank );
         if (verbose) print_container_contents( file_id, rc_id3, "/", my_rank ); ASSERT_RET;

         /* Release read handle & close read context for CV 3 */
         fprintf( stderr, "M6.2-r%d: Release read handle on cv 3 (Step 15e)\n", my_rank );
         ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL ); ASSERT_RET;
         ret = H5RCclose( rc_id3 ); ASSERT_RET;
      } else {
         fprintf( stderr, "M6.2-r%d: Failed %d times to aquire read context for cv 3 - continuing\n", my_rank, max_tries );
      }
         
      /*    16) /GB/GB/GA added in Tr 4 by rank 1  */
      if ( my_rank == 1 ) {
         create_group( file_id, "/GB/GB/GA", tr_id4, "/", my_rank, tr_num4 );
      }

      /*    17) /DA updated in Tr 4 by all ranks */
      update_dataset( file_id, "/DA", tr_id4, rc_id2, "/", my_rank, tr_num4, 1 );

      /* Finish and commit transaction 4, causing the updates to appear in container version 4. */
      fprintf( stderr, "M6.2-r%d: Finish and commit tr %d (Step 15a - end)\n", my_rank, (int)tr_num4 );
      ret = H5TRfinish( tr_id4, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5TRclose( tr_id4 ); ASSERT_RET;

      /* Get read context for CV 4, then print the contents of the container */
      version = 4;
      fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 15f)\n", my_rank, (int)version );
      rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      while ( rc_id4 < 0 ) {
         fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 4 - sleep then retry\n", my_rank );
         sleep( 1 );
         version = 4;
         rc_id4 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      }
      assert( rc_id4 >= 0 ); assert( version == 4 );
      fprintf( stderr, "M6.2-r%d: Acquired read context for cv 4\n", my_rank );
      fprintf( stderr, "M6.2-r%d: 7th call to print container contents (Step 15g)\n", my_rank );
      if (verbose) print_container_contents( file_id, rc_id4, "/", my_rank ); ASSERT_RET;

      /* 
       * Get read context for CV 3. 
       * Should be there first time if Transaction 3 was finished & committed (because we know Transaction 4 is done)
       * If Transaction 3 was aborted, it will never be there.
       */
      version = 3;
      fprintf( stderr, "M6.2-r%d: Once again, try to acquire read context for cv %d (Step 15h)\n", my_rank, (int)version );
      rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      if ( rc_id3 < 0 ) {
         fprintf( stderr, "M6.2-r%d: Read context for cv 3 not available.\n", my_rank );
      } else {
         assert( rc_id3 >= 0 ); assert( version == 3 );
         fprintf( stderr, "M6.2-r%d: Acquired read context for cv 3\n", my_rank );
         fprintf( stderr, "M6.2-r%d: 8th call to print container contents (Step 15i)\n", my_rank );
         if (verbose) print_container_contents( file_id, rc_id3, "/", my_rank ); ASSERT_RET;

         /* Release read handle & close read context for CV 3 */
         fprintf( stderr, "M6.2-r%d: Release read handle on cv 3 (Step 15j)\n", my_rank );
         ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL ); ASSERT_RET;
         ret = H5RCclose( rc_id3 ); ASSERT_RET;
      }

      /* 
       * Note that rc_id4 was obtained in this bracketed code, but we aren't releasing it until later.
       * Inelegant but I want it again & don't want to have to acquire again. 
       * Wanted to use it in print above (15f) before 2nd print of CV3 (15g), so didn't wait until
       * bracket closed to acquire. 
       */
   
   }

   /* Release the read handle and close read context on open CVs */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 2 (Step 16)\n", my_rank );
   ret = H5RCrelease( rc_id2, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5RCclose( rc_id2 ); ASSERT_RET;
   
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 4\n", my_rank );
   ret = H5RCrelease( rc_id4, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5RCclose( rc_id4 ); ASSERT_RET;

   /* Close the file, then barrier to make sure all have closed it. */
   fprintf( stderr, "M6.2-r%d: close the container\n", my_rank );
   ret = H5Fclose_ff( file_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

   MPI_Barrier( MPI_COMM_WORLD );


   /* Now, reopen file and show contents of all available CVs */
   hid_t last_rc_id;
   uint64_t last_version, v;

   /* Reopen the file Read/Write */
   fprintf( stderr, "M6.2-r%d: open the container\n", my_rank );

   /* KEEP CODE IN CASE WANT TO Ask for explicit CV 4 Later */
   /*  file_id = H5Fopen_ff( file_name, H5F_ACC_RDWR, fapl_id, NULL, H5_EVENT_STACK_NULL ); assert( file_id >= 0 ); */

   /* Get latest CV on open */
   file_id = H5Fopen_ff( file_name, H5F_ACC_RDWR, fapl_id, &last_rc_id, H5_EVENT_STACK_NULL ); assert( file_id >= 0 );

   H5RCget_version( last_rc_id, &last_version );
   fprintf( stderr, "M6.2-r%d: Latest CV in file is cv %d\n", my_rank, (int)last_version );

   for ( v = 0; v <= last_version; v++ ) {
      version = v;
      fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d\n", my_rank, (int)version );
      rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      if ( rc_id < 0 ) {
         fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv %d\n", my_rank, (int)v );
      } else {
         assert ( version == v );
         print_container_contents( file_id, rc_id, "/", my_rank );
         ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
         ret = H5RCclose( rc_id ); ASSERT_RET;
      }
   }

   /* Release the read handle and close read context on cv obtained from H5Fopen_ff */
   ret = H5RCrelease( last_rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5RCclose( last_rc_id ); ASSERT_RET;

   /* Close 2 H5 Objects that are still open */
   fprintf( stderr, "M6.2-r%d: close all h5 objects that are still open\n", my_rank );
   ret = H5Fclose_ff( file_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   ret = H5Pclose( fapl_id ); ASSERT_RET;

   fprintf( stderr, "M6.2-r%d: Finalize EFF stack\n", my_rank );

   /* Perform wrap-up operations */
   EFF_finalize();
   MPI_Finalize();

   return 0;
}


/*
 * Helper function used to create string attribute "attr_name" 
 * for object identified by "obj_id" 
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" and "tr_num" are used to create identifiable string value
 */
void
create_string_attribute( hid_t obj_id, const char* attr_name, hid_t tr_id, const char* obj_path, int my_rank, uint64_t tr_num ){
   herr_t ret;
   hsize_t dim_cnt;
   hid_t space_id;
   hid_t dtype_id;
   char attr_val[ATTR_STR_LEN];
   hsize_t val_size;
   hid_t attr_id;

   dim_cnt = 1;
   space_id = H5Screate_simple( 1, &dim_cnt, NULL ); assert( space_id >= 0 );
   dtype_id = H5Tcopy( H5T_C_S1 ); assert( dtype_id >= 0 );

   val_size = sprintf( attr_val, "%s @ %s by rank %d in tr %d", attr_name, obj_path, my_rank, (int)tr_num );
   assert( val_size < ATTR_STR_LEN );
   ret = H5Tset_size( dtype_id, ATTR_STR_LEN ); ASSERT_RET;

   attr_id = H5Acreate_ff( obj_id, attr_name, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   if ( attr_id >= 0 ) {
      fprintf( stderr, "M6.2-r%d: Create %s with value %s - %s\n", my_rank, attr_name, attr_val, "OK"  );
      ret = H5Awrite_ff( attr_id, dtype_id, attr_val, tr_id, H5_EVENT_STACK_NULL ); 

      ret = H5Aclose_ff( attr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "M6.2-r%d: Create %s with value %s - %s\n", my_rank, attr_name, attr_val, "FAILED"  );
   }
      
   ret = H5Tclose( dtype_id ); ASSERT_RET;
   ret = H5Sclose( space_id ); ASSERT_RET;

   return;
}

/*
 * Helper function used to create group "group_name" 
 * in object identified by "obj_id" 
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" and "tr_num" are used in the status output.
 */
void
create_group( hid_t obj_id, const char* group_name, hid_t tr_id, const char* obj_path, int my_rank, uint64_t tr_num ){
   herr_t ret;
   hid_t group_id;

   group_id = H5Gcreate_ff( obj_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   if ( group_id >= 0 ) {
      fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d - %s\n", my_rank, group_name, obj_path, (int)tr_num, "OK"  );
      ret = H5Gclose_ff( group_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
   } else {
      fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d - %s\n", my_rank, group_name, obj_path, (int)tr_num, "FAILED"  );
   }

   return;
}

/*
 * Helper function used to create and initialize dataset "dset_name" 
 * in object identified by "obj_id" 
 * in transaction identified by "tr_id".
 * All datasets are 1D Arrays, start with 4 elements, and have a max of 10 elements.
 * "obj_path" and "my_rank" and "tr_num" and "ordinal" are used in the status output.
 * Cell values are computed using the formula:
 *    data[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
 * where ordinal can be set by the calling function to denote where the creation of this dataset occurs relative
 * to the creation of other datasets in "obj_id".  It is used solely to generate identifiable cell values.
 */
void
create_dataset( hid_t obj_id, const char* dset_name, hid_t tr_id, const char* obj_path, int my_rank, uint64_t tr_num, int ordinal ){
   herr_t ret;
   hsize_t current_size = 4;
   hsize_t max_size = 10;
   int data[4];              /* sized to accomodate current_size elements */
   hid_t space_id;
   hid_t dtype_id;
   hid_t dset_id;
   int i;

   /* Set cell values */
   for ( i = 0; i < current_size; i++ ) {
      data[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
   }

   space_id = H5Screate_simple( 1, &current_size, &max_size ); assert( space_id >= 0 );
   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id >= 0 );

   dset_id = H5Dcreate_ff( obj_id, dset_name, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, 
                          H5_EVENT_STACK_NULL );
   if ( dset_id >= 0 ) {
      fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d; ordinal is %d; data values are %d %d %d %d - %s\n", 
               my_rank, dset_name, obj_path, (int)tr_num, ordinal, data[0], data[1], data[2], data[3], "OK" );
      ret = H5Dwrite_ff( dset_id, dtype_id, space_id, space_id, H5P_DEFAULT, data, tr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET; 
   } else {
      fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d; ordinal is %d; data values are %d %d %d %d - %s\n", 
               my_rank, dset_name, obj_path, (int)tr_num, ordinal, data[0], data[1], data[2], data[3], "FAILED" );
   }

   ret = H5Tclose( dtype_id ); ASSERT_RET;
   ret = H5Sclose( space_id ); ASSERT_RET;

   return;
}

/*
 * Helper function used to create committed datatype "type_name" 
 * in group identified by "obj_id" 
 * in transaction identified by "tr_id".
 * "obj_path" and "my_rank" and "tr_num" are used in the status output.
 */
void
create_committed_datatype( hid_t obj_id, const char* type_name, hid_t tr_id, const char* obj_path, int my_rank, uint64_t tr_num ){
   herr_t ret;
   hid_t dtype_id;

   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id >= 0 );
   ret = H5Tcommit_ff( obj_id, type_name, dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL ); 
   fprintf( stderr, "M6.2-r%d: Create CDT %s in %s tr %d - %s\n", my_rank, type_name, obj_path, (int)tr_num, STATUS );

   ret = H5Tclose( dtype_id ); ASSERT_RET;

   return;
}

/*
 * Helper function used to update raw data in dataset "dset_name" 
 * in object identified by "obj_id" 
 * in transaction identified by "tr_id".
 * "rc_id" for "tr_id" passed in explicitly for now - later add H5 function to extract it from tr_id.
 * cell[my_rank] will be updated, using the formula
 *    value  = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
 * "obj_path" and "my_rank" and "tr_num" and "ordinal" are used in the status output.
 */
void
update_dataset( hid_t obj_id, const char* dset_name, hid_t tr_id, hid_t rc_id, const char* obj_path, int my_rank, 
                uint64_t tr_num, int ordinal ){
   herr_t ret;
   hid_t dset_id;

   /* Open the dataset, confirm number of dimensions, get current & max size */
   dset_id = H5Dopen_ff( obj_id, dset_name, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 

   if ( dset_id >= 0 ) {
      hid_t filespace_id;
      int nDims;
      hsize_t current_size, max_size;

      filespace_id = H5Dget_space( dset_id ); assert ( filespace_id >= 0 );
      nDims = H5Sget_simple_extent_dims( filespace_id, &current_size, &max_size );
      assert( nDims == 1 );

      if ( my_rank < current_size ) {
         int cell_data;              /* sized to hold data for a single cell in the dataset*/
         hsize_t mem_size = 1;
         hid_t memspace_id;

         /* set data value, create memory data space, then select the element to update */
         cell_data = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
         memspace_id = H5Screate_simple( 1, &mem_size, &mem_size ); assert( memspace_id >= 0 );

         hsize_t start, stride, count, block;
         start = (hsize_t)my_rank;
         stride = count = block = 1;
      
         ret = H5Sselect_hyperslab( filespace_id, H5S_SELECT_SET, &start, &stride, &count, &block ); ASSERT_RET;

         ret = H5Dwrite_ff( dset_id, H5T_NATIVE_INT, memspace_id, filespace_id, H5P_DEFAULT, &cell_data, tr_id, H5_EVENT_STACK_NULL );
         fprintf( stderr, "M6.2-r%d: Update %s[%d] in %s in tr %d to have value %d - %s\n", 
                        my_rank, dset_name, my_rank, obj_path, (int)tr_num, cell_data, STATUS );
         ret = H5Sclose( memspace_id ); ASSERT_RET;

      } else {
         fprintf( stderr, "M6.2-r%d: No update to %s in %s in tr %d for this rank.\n", my_rank, dset_name, obj_path, (int)tr_num );
      }

      ret = H5Sclose( filespace_id ); ASSERT_RET;
      ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET; 

   } else {
      fprintf( stderr, "M6.2-r%d: Update %s[%d] in %s in tr %d - %s\n", my_rank, dset_name, my_rank, obj_path, (int)tr_num, "FAILED" );
   }

   return;
}

/*
 * Helper function used to extend dataset "dset_name" 
 * in object identified by "obj_id" 
 * in transaction identified by "tr_id".
 * "rc_id" for "tr_id" passed in explicitly for now - later add H5 function to extract it from tr_id.
 * Dataset will be extended by 1 and value of new cell wil be updated using the formula
 *    data[i] = ordinal*1000 + tr_num*100 + my_rank*10 + i;
 * "obj_path" and "my_rank" and "tr_num" and "ordinal" are used in the status output.
 */
void
extend_dataset( hid_t obj_id, const char* dset_name, hid_t tr_id, hid_t rc_id, const char* obj_path, int my_rank, 
                uint64_t tr_num, int ordinal ){
   herr_t ret;
   hid_t dset_id;
   hid_t file_space_id;
   int nDims;
   hsize_t current_size, max_size, new_size;

   /* Open the dataset, confirm number of dimensions, get current & max size */
   dset_id = H5Dopen_ff( obj_id, dset_name, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_id >= 0 );

   file_space_id = H5Dget_space( dset_id ); assert ( file_space_id >= 0 );
   nDims = H5Sget_simple_extent_dims( file_space_id, &current_size, &max_size );
#ifdef EXTEND_WORKING
   assert( nDims == 1 );

   if ( current_size < max_size) {
      new_size = current_size + 1;

      ret = H5Dset_extent_ff( dset_id, &new_size, tr_id, H5_EVENT_STACK_NULL ); 
      fprintf( stderr, "M6.2-r%d: Extend %s in %s in tr %d to have size %d - %s\n", 
                       my_rank, dset_name, obj_path, (int)tr_num, new_size, STATUS );

   } else {
      fprintf( stderr, "M6.2-r%d: No extend to %s in %s in tr %d as max_size already .\n", 
            my_rank, dset_name, obj_path, (int)tr_num );
   }
   ret = H5Sclose( file_space_id ); ASSERT_RET;
#else
   fprintf( stderr, "M6.2-r%d: Not yet able to extend %s in %s in tr %d to have size %d\n", 
                       my_rank, dset_name, obj_path, (int)tr_num, current_size+1 );
#endif

   ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET; 

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
   hid_t obj_id;
   char name[3];
   int i;

   static int lvl = 0;     /* level in recursion - used to format printing */
   char preface[128];    
   char line[1024];    

   /* Get the container version for the read context */
   ret = H5RCget_version( rc_id, &cv ); ASSERT_RET;

   /* Set up the preface and adding version number */
   sprintf( preface, "M6.2-r%d: cv %d: ", my_rank, (int)cv );

   /* Start the printing */
   if ( lvl == 0 ) {
      fprintf( stderr, "%s ----- Container Contents ------------\n", preface );
   } 

   /* Attributes */
   for ( i = 1; i < 3; i++ ) {
      if ( i == 1 ) { 
         strcpy( name, "AA" );
      } else if ( i == 2 ) { 
         strcpy( name, "AB" ); 
      } else { 
         strcpy( name, "XX" ); 
      }
      ret = H5Aexists_by_name_ff( file_id, grp_path, name, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      if ( exists ) { 
         hid_t attr_id, atype_id;
         char attr_val[ATTR_STR_LEN];

         attr_id = H5Aopen_by_name_ff( file_id, grp_path, name, H5P_DEFAULT, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         assert( attr_id >= 0 );
   
         atype_id = H5Tcopy( H5T_C_S1 ); assert( atype_id >= 0 );
         ret = H5Tset_size( atype_id, ATTR_STR_LEN ); ASSERT_RET;

         ret = H5Aread_ff( attr_id, atype_id, attr_val, rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
         fprintf( stderr, "%s %s%s   value: %s\n", preface, grp_path, name, attr_val );

         ret = H5Tclose( atype_id ); ASSERT_RET;
         ret = H5Aclose_ff( attr_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
      }
   }

   /* Datasets */
   for ( i = 1; i < 3; i++ ) {
      if ( i == 1 ) { 
         strcpy( name, "DA" );
      } else if ( i == 2 ) { 
         strcpy( name, "DB" ); 
      } else { 
         strcpy( name, "XX" ); 
      }

      sprintf( path_to_object, "%s%s", grp_path, name );
      ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );
      if ( exists ) { 
         hid_t dset_id;
         hid_t space_id;
         int nDims;
         hsize_t current_size;
         hsize_t max_size;
         int *data;              
         int i;

         dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         assert( dset_id >= 0 );

         space_id = H5Dget_space( dset_id ); assert ( space_id >= 0 );
         nDims = H5Sget_simple_extent_dims( space_id, &current_size, &max_size ); 
         assert( nDims == 1 );

         data = (int *)calloc( current_size, sizeof(int) ); 

         ret = H5Dread_ff( dset_id, H5T_NATIVE_INT, space_id, space_id, H5P_DEFAULT, data, rc_id, H5_EVENT_STACK_NULL ); ASSERT_RET;

         sprintf( line, "%s %s   values: ", preface, path_to_object, cv );
         for ( i = 0; i < current_size; i++ ) {
            sprintf( line, "%s%d ", line, data[i] );
         }
         fprintf( stderr, "%s\n", line );
         free( data );

         ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); ASSERT_RET;
         ret = H5Sclose( space_id ); ASSERT_RET;
      } 
   }

   /* Committed datatypes */
   for ( i = 1; i < 3; i++ ) {
      if ( i == 1 ) { 
         strcpy( name, "TA" );
      } else if ( i == 2 ) { 
         strcpy( name, "TB" ); 
      } else { 
         strcpy( name, "XX" ); 
      }

      sprintf( path_to_object, "%s%s", grp_path, name );
      ret = H5Lexists_ff( file_id, path_to_object, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL );
      if ( exists ) { 
         fprintf( stderr, "%s %s\n", preface, path_to_object );
      } 
   }

   /* Groups - if found, descend */
   for ( i = 1; i < 3; i++ ) {
      if ( i == 1 ) { 
         strcpy( name, "GA" );
      } else if ( i == 2 ) { 
         strcpy( name, "GB" ); 
      } else { 
         strcpy( name, "XX" ); 
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
 * parse_options - helper function to parse the command line options.
 */
int
parse_options( int argc, char** argv ) {
   int i, n;

   while ( --argc ) {
      if ( **(++argv) != '-' ) {
         break;
      } else {
         switch( *(*argv+1) ) {
            case 'v':   
               verbose = 1;
               break;
            case 'a':
               abort3 = 1;
               break;
            default: 
               printf( "Usage: h5ff_client_M6.2_demo [-av] \n\ta: abort transaction 3\n\tv: verbose printing\n" );
               return( 1 );
         }
      }
   }
   return( 0 );
}
