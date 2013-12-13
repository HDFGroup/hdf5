/*
 * M6.2-demo.c: This is the demo program for Milestone 6.2 - HDF5 and IO Dispatcher Container Versioning Demonstration.
 * This program runs on one or more compute nodes and makes calls to the HDF5 API.
 * The Function Shipper server should be running before this demo program is started.
 */
/* NOTE FROM RUTH TO MOHAMAD:  This version is currently failing. 
 * Updates added to transaction 3 that should not be committed when transaction 3 is aborted are showing up in the
 *   printed output for CV 4.  And, when the output gets to /GA/GA/GA, it thinks the directory is there -- it should not be.
 *   Then, it seems to think /GA/GA/GA has an attribute "AA" but none was never added in any transcation.  And, things fail.
 *   
 *   Here's the client-side output from that when run with 1 MPI process:
 *   Link Exists axe 260: /GA/GA/GA/ ID 3458764513820540928
 *   Operation 260 Dependencies:
 *   Operation 260 will finish tasks 259 through 259
 *       M6.2-r0: /GA/GA/GA/ exists in cv 4.
 *       Attribute Exists loc /GA/GA/GA/ name AA, axe id 261
 *       Operation 261 Dependencies:
 *       Operation 261 will finish tasks 260 through 260
 *       EXIST OP failed at the server
 *       HDF5-DIAG: Error detected in HDF5 (1.9.167) MPI-process 0:
 *         #000: H5FF.c line 1898 in H5Aexists_by_name_ff(): unable to get attribute info
 *             major: Internal error (too specific to document in detail)
 *                 minor: Can't get value
 *                   #001: H5VLint.c line 758 in H5VL_attr_get(): get failed
 *                       major: Virtual Object Layer
 *                           minor: Can't get value
 *                             #002: H5VLiod.c line 4898 in H5VL_iod_attribute_get(): failed to create and ship attribute exists
 *                                 major: Symbol table
 *                                     minor: Unable to initialize object
 *                                     h5ff_client_M6.2_demo: ./h5ff_client_M6.2_demo.c:714: print_container_contents: Assertion `ret == 0' failed.
 *
 *
 * To see what happens when transaction 3 is not aborted, comment out the define ABORT3 line and rebuild.
 * It can also be run with 2 MPI processes (and I think more).  It fails then too.
 * This failure did not occur prior to the changes made 12/12.
 */
#define ABORT3

/*
 * TODO:  Edit to address issue that tr_start may occur after tr_abort for tid_3 when laggard nodes.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "mpi.h"
#include "hdf5.h"

#define ATTR_STR_LEN 128

void create_string_attribute( hid_t, const char*, hid_t, const char*, int, uint64_t );
void create_group( hid_t, const char*, hid_t, const char*, int, uint64_t );
void create_dataset( hid_t, const char*, hid_t, const char*, int, uint64_t, int );
void create_committed_datatype( hid_t, const char*, hid_t, const char*, int, uint64_t );
void update_dataset( hid_t, const char*, hid_t, hid_t, const char*, int, uint64_t, int );
void print_container_contents( hid_t, hid_t, const char*, int );

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
   MPI_Comm_rank( MPI_COMM_WORLD, &my_rank );
   MPI_Comm_size( MPI_COMM_WORLD, &comm_size );
   fprintf( stderr, "M6.2-r%d: Number of MPI processes = %d\n", my_rank, comm_size );

   /* Initialize the EFF stack. */
   fprintf( stderr, "M6.2-r%d: Initialize EFF stack (Step 1)\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Specify that the IOD VOL plugin should be used and create H5File (EFF container) */
   fprintf( stderr, "M6.2-r%d: Create the container %s (Step 2)\n", my_rank, file_name );
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); assert( ret == 0 );
   file_id = H5Fcreate_ff( file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL ); assert( file_id );

   /* Acquire a read handle for container version 0 and create a read context. */
   version = 0;
   fprintf( stderr, "M6.2-r%d: Acquire read context for container version %d (Step 3)\n", my_rank, (int)version );
   rc_id0 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( rc_id0 ); assert( version == 0 );

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 1st call to print container contents (Step 4)\n", my_rank );
   print_container_contents( file_id, rc_id0, "/", my_rank );

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
      fprintf( stderr, "M6.2-r%d: create transaction %d (Step 5 - begin) \n", my_rank, (int)tr_num );
      tr_id = H5TRcreate( file_id, rc_id0, (uint64_t)tr_num ); assert( tr_id );

      /* Start transaction 1 with single transaction leader (the default) */
      fprintf( stderr, "M6.2-r%d: start transaction %d\n", my_rank, (int)tr_num );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( ret == 0 );

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
      fprintf( stderr, "M6.2-r%d: finish and commit transaction %d (Step 5 - end)\n", my_rank, (int)tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Close the local transaction. */
      ret = H5TRclose( tr_id ); assert( ret == 0 );
   }

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 2nd call to print container contents (Step 6)\n", my_rank );
   print_container_contents( file_id, rc_id0, "/", my_rank );

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
   print_container_contents( file_id, rc_id1, "/", my_rank ); assert( ret == 0 );

   /* Release the read handle and close read context on cv 0 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 0 (Step 9)\n", my_rank );
   ret = H5RCrelease( rc_id0, H5_EVENT_STACK_NULL); assert( ret == 0 );
   ret = H5RCclose( rc_id0 ); assert( ret == 0 );

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
      tr_id = H5TRcreate( file_id, rc_id1, tr_num ); assert( tr_id );

      /* Start transaction 2 with two transaction leaders (unless there is only 1 MPI process) */
      fprintf( stderr, "M6.2-r%d: start transaction %d (Step 10 - begin)\n", my_rank, (int)tr_num );
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id );
      if ( comm_size >= 2 ) {
         ret = H5Pset_trspl_num_peers( trspl_id, 2 ); assert( ret == 0 ) ;
      } else {
         ret = H5Pset_trspl_num_peers( trspl_id, 1 ); assert( ret == 0 ) ;
      }
      ret = H5TRstart( tr_id, trspl_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Add updates to the transaction */
      if ( my_rank == 0 ) {
         gr_id = H5Gopen_ff( file_id, "GA", H5P_DEFAULT, rc_id1, H5_EVENT_STACK_NULL ); assert( gr_id );
         sprintf( gr_path, "/GA/" );
      } else {
         gr_id = H5Gopen_ff( file_id, "GB", H5P_DEFAULT, rc_id1, H5_EVENT_STACK_NULL ); assert( gr_id );
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
      ret = H5Gclose_ff ( gr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Finish and commit transaction 2, causing the updates to appear in container version 2. */
      fprintf( stderr, "M6.2-r%d: finish and commit transaction %d (Step 10 - end)\n", my_rank, (int)tr_num );
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Close the local transaction and transaction start property list. */
      ret = H5TRclose( tr_id ); assert( ret == 0 );
      ret = H5Pclose( trspl_id ); assert( ret == 0 );
   }

   fprintf( stderr, "M6.2-r%d: 4th call to print container contents (Step 11)\n", my_rank );
   print_container_contents( file_id, rc_id1, "/", my_rank ); assert( ret == 0 );

   version = 2;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 12)\n", my_rank, (int)version );
   rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( rc_id2 < 0 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 2 - sleep then retry\n", my_rank );
      sleep( 1 );
      version = 2;
      rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }
   assert( rc_id2 >= 0 ); assert ( version == 2 );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv 2\n", my_rank );
   fprintf( stderr, "M6.2-r%d: 5th call to print container contents (Step 13)\n", my_rank );
   print_container_contents( file_id, rc_id2, "/", my_rank ); assert( ret == 0 );

   /* Release the read handle and close read context on cv 1 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 1 (Step 14)\n", my_rank );
   ret = H5RCrelease( rc_id1, H5_EVENT_STACK_NULL); assert( ret == 0 );
   ret = H5RCclose( rc_id1 ); assert( ret == 0 );

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
    */
   {
      uint64_t tr_num3 = 3;
      uint64_t tr_num4 = 4;
      hid_t tr_id3, tr_id4;
      hid_t trspl_id;
   
      /* Set up transaction start property list with the number of transaction leaders */
      trspl_id = H5Pcreate( H5P_TR_START );  assert( trspl_id );
      ret = H5Pset_trspl_num_peers( trspl_id, comm_size ); assert( ret == 0 ) ;
   
      /* Create & start transaction 4, based on cv 2  */
      fprintf( stderr, "M6.2-r%d: start transaction %d (Step 15a - begin)\n", my_rank, (int)tr_num4 );
      tr_id4 = H5TRcreate( file_id, rc_id2, tr_num4 ); assert( tr_id4 );
      ret = H5TRstart( tr_id4, trspl_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   
      /* Create & start transaction 3, based on cv 2  */
      fprintf( stderr, "M6.2-r%d: start transaction %d (Step 15b - begin)\n", my_rank, (int)tr_num3 );
      tr_id3 = H5TRcreate( file_id, rc_id2, tr_num3 ); assert( tr_id3 );
      ret = H5TRstart( tr_id3, trspl_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   
      /* Close the transaction start property list. */
      ret = H5Pclose( trspl_id ); assert( ret == 0 );

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
         fprintf( stderr, "M6.2-r%d: delete AA @ / in tr %d \n", my_rank, (int)tr_num3 );
         ret = H5Adelete_by_name_ff( file_id, ".", "AA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    4) /GA/DA deleted in Tr 3 by rank 1    */
      if ( my_rank == 1 ) {
         fprintf( stderr, "M6.2-r%d: delete /GA/DA in tr %d \n", my_rank, (int)tr_num3 );
         ret = H5Ldelete_ff( file_id, "/GA/DA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    5) /GA/TB deleted in Tr 4 by rank 0    */
      if ( my_rank == 0 ) {
         fprintf( stderr, "M6.2-r%d: delete /GA/TB in tr %d \n", my_rank, (int)tr_num4 );
         ret = H5Ldelete_ff( file_id, "/GA/TB", H5P_DEFAULT, tr_id4, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    6) /DB deleted in Tr 4 by rank 1       */
      if ( my_rank == 1 ) {
         fprintf( stderr, "M6.2-r%d: delete /DB in tr %d \n", my_rank, (int)tr_num4 );
         ret = H5Ldelete_ff( file_id, "/DB", H5P_DEFAULT, tr_id4, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    7) /GB/GA deleted in Tr 3 by rank 0    */
      if ( my_rank == 0  && comm_size > 1 ) {
         fprintf( stderr, "M6.2-r%d: delete /GB/GA in tr %d \n", my_rank, (int)tr_num3 );
         ret = H5Ldelete_ff( file_id, "/GB/GA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    8) /GA/TA deleted in Tr 3 by rank 1    */
      if ( my_rank == 1  ) {
         fprintf( stderr, "M6.2-r%d: delete /GA/TA in tr %d \n", my_rank, (int)tr_num3 );
         ret = H5Ldelete_ff( file_id, "/GA/TA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    9) /GA/GA/GA added in Tr 3 by rank 0   */
      if ( my_rank == 0 ) {
         create_group( file_id, "/GA/GA/GA", tr_id3, "/", my_rank, tr_num3 );
      } 
      /*    10) /GB/TA deleted in Tr 3 by rank 1   */
      if ( my_rank == 1 ) {
         fprintf( stderr, "M6.2-r%d: delete /GB/TA in tr %d \n", my_rank, (int)tr_num3 );
         ret = H5Ldelete_ff( file_id, "/GB/TA", H5P_DEFAULT, tr_id3, H5_EVENT_STACK_NULL); assert( ret == 0  );
      }
      /*    11) /GB/GB/DB added in Tr 3 by rank 1  */
      if ( my_rank == 1 ) {
         create_dataset( file_id, "/GB/GB/DB", tr_id3, "/", my_rank, tr_num3, 2 );
      } 

#ifdef ABORT3
      /*    Abort   */
      MPI_Barrier( MPI_COMM_WORLD );         // TEMPORARY BARRIER UNTIL OTHER EDITS MADE BY RUTH
      fprintf( stderr, "M6.2-r%d: ABORT transaction %d (Step 15b - abort)\n", my_rank, (int)tr_num3 );
      ret = H5TRabort( tr_id3, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      MPI_Barrier( MPI_COMM_WORLD );
      ret = H5TRclose( tr_id3 ); assert( ret == 0 );
#else
      /* Finish and commit transaction 3, causing the updates to appear in container version 3. */
      fprintf( stderr, "M6.2-r%d: finish and commit transaction %d (Step 15b - end)\n", my_rank, (int)tr_num3 );
      ret = H5TRfinish( tr_id3, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );
      ret = H5TRclose( tr_id3 ); assert( ret == 0 );

      /* Get read context for CV 3, then print the contents of container */
      version = 3;
      fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d (Step 15c)\n", my_rank, (int)version );
      rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      while ( rc_id3 < 0 ) {
         fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 3 - sleep then retry\n", my_rank );
         sleep( 1 );
         version = 3;
         rc_id3 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
      }
      assert( rc_id3 >= 0 ); assert ( version == 3 );
      fprintf( stderr, "M6.2-r%d: Acquired read context for cv 3\n", my_rank );
      fprintf( stderr, "M6.2-r%d: 6th call to print container contents (Step 15d)\n", my_rank );
      print_container_contents( file_id, rc_id3, "/", my_rank ); assert( ret == 0 );

      /* Release read handle & close read context for CV 3 */
      fprintf( stderr, "M6.2-r%d: Release read handle on cv 3 (Step 15e)\n", my_rank );
      ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL ); assert( ret == 0 );
      ret = H5RCclose( rc_id3 ); assert( ret == 0 );
#endif
   
      /*    12) /GB/GB/GA added in Tr 4 by rank 1  */
      if ( my_rank == 1 ) {
         create_group( file_id, "/GB/GB/GA", tr_id4, "/", my_rank, tr_num4 );
      }
      update_dataset( file_id, "/DA", tr_id4, rc_id2, "/", my_rank, tr_num4, 1 );

      /* Finish and commit transaction 4, causing the updates to appear in container version 4. */
      fprintf( stderr, "M6.2-r%d: finish and commit transaction %d (Step 15a - end)\n", my_rank, (int)tr_num4 );
      ret = H5TRfinish( tr_id4, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );
      ret = H5TRclose( tr_id4 ); assert( ret == 0 );

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
      assert( rc_id4 >= 0 ); assert ( version == 4 );
      fprintf( stderr, "M6.2-r%d: Acquired read context for cv 4\n", my_rank );
      fprintf( stderr, "M6.2-r%d: 7th call to print container contents (Step 15g)\n", my_rank );
      print_container_contents( file_id, rc_id4, "/", my_rank ); assert( ret == 0 );

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
         assert( rc_id3 >= 0 ); assert ( version == 3 );
         fprintf( stderr, "M6.2-r%d: Acquired read context for cv 3\n", my_rank );
         fprintf( stderr, "M6.2-r%d: 8th call to print container contents (Step 15i)\n", my_rank );
         print_container_contents( file_id, rc_id3, "/", my_rank ); assert( ret == 0 );

         /* Release read handle & close read context for CV 3 */
         fprintf( stderr, "M6.2-r%d: Release read handle on cv 3 (Step 15j)\n", my_rank );
         ret = H5RCrelease( rc_id3, H5_EVENT_STACK_NULL ); assert( ret == 0 );
         ret = H5RCclose( rc_id3 ); assert( ret == 0 );
      }

      /* 
       * Note that rc_id4 was obtained in this bracket code, but we aren't releasing it until later.
       * Inelegant but I want it again & don't want to have to acquire again. 
       * Wanted to use it in print above (15f) before 2nd print of CV3 (15g), so didn't wait until
       * bracket closed to acquire. 
       */
   
   }

   /* Release the read handle and close read context on open CVs */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 2 (Step 16)\n", my_rank );
   ret = H5RCrelease( rc_id2, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5RCclose( rc_id2 ); assert( ret == 0 );
   
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 4\n", my_rank );
   ret = H5RCrelease( rc_id4, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5RCclose( rc_id4 ); assert( ret == 0 );

   /* Close the file, then barrier to make sure all have closed it. */
   fprintf( stderr, "M6.2-r%d: close the container\n", my_rank );
   ret = H5Fclose_ff( file_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );

   MPI_Barrier( MPI_COMM_WORLD );

   /* Reopen the file Read/Write */
   fprintf( stderr, "M6.2-r%d: open the container\n", my_rank );

#ifdef PRINT_LATEST
   /* Get latest CV on open */
   file_id = H5Fopen_ff( file_name, H5F_ACC_RDWR, fapl_id, &rc_id, H5_EVENT_STACK_NULL ); assert( file_id );

#else
   /* Ask for explicit CV 4  */
   file_id = H5Fopen_ff( file_name, H5F_ACC_RDWR, fapl_id, NULL, H5_EVENT_STACK_NULL ); assert( file_id );

   version = 4;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d\n", my_rank, (int)version );
   rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( rc_id < 0 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv - sleep then retry\n", my_rank );
      sleep( 1 );
      version = 4;
      rc_id = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }
   assert( rc_id >= 0 ); assert ( version == 4 );
#endif

   H5RCget_version( rc_id, &version );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv %d\n", my_rank, (int)version );

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: XXth call to print container contents\n", my_rank );
   print_container_contents( file_id, rc_id, "/", my_rank );

   /* Release the read handle and close read context on cv obtained from H5Fopen_ff */
   fprintf( stderr, "M6.2-r%d: Release read handle on version %d\n", my_rank, (int)version );
   ret = H5RCrelease( rc_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5RCclose( rc_id ); assert( ret == 0 );

   /* Close 2 H5 Objects that are still open */
   fprintf( stderr, "M6.2-r%d: close all h5 objects that are still open\n", my_rank );
   ret = H5Fclose_ff( file_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5Pclose( fapl_id ); assert( ret == 0 );

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
   space_id = H5Screate_simple( 1, &dim_cnt, NULL ); assert( space_id );
   dtype_id = H5Tcopy( H5T_C_S1 ); assert( dtype_id );

   val_size = sprintf( attr_val, "%s @ %s by rank %d in tr %d", attr_name, obj_path, my_rank, (int)tr_num );
   assert( val_size < ATTR_STR_LEN );
   ret = H5Tset_size( dtype_id, ATTR_STR_LEN ); assert( ret == 0 );

   fprintf( stderr, "M6.2-r%d: Create %s with value %s\n", my_rank, attr_name, attr_val  );
   attr_id = H5Acreate_ff( obj_id, attr_name, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   assert( attr_id );
   ret = H5Awrite_ff( attr_id, dtype_id, attr_val, tr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );

   ret = H5Aclose_ff( attr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5Tclose( dtype_id ); assert( ret == 0 );
   ret = H5Sclose( space_id ); assert( ret == 0 );

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

   fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d\n", my_rank, group_name, obj_path, (int)tr_num  );
   group_id = H5Gcreate_ff( obj_id, group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL );
   assert( group_id );
   ret = H5Gclose_ff( group_id, H5_EVENT_STACK_NULL ); assert( ret == 0 ); 

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

   space_id = H5Screate_simple( 1, &current_size, &max_size ); assert( space_id );
   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id );

   fprintf( stderr, "M6.2-r%d: Create %s in %s in tr %d; ordinal is %d; ", my_rank, dset_name, obj_path, (int)tr_num, ordinal );
   fprintf( stderr, "data values are %d %d %d %d\n", data[0], data[1], data[2], data[3] );

   dset_id = H5Dcreate_ff( obj_id, dset_name, dtype_id, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, 
                          H5_EVENT_STACK_NULL );
   assert( dset_id );
   ret = H5Dwrite_ff( dset_id, dtype_id, space_id, space_id, H5P_DEFAULT, data, tr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 ); 
   ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); assert( ret == 0 ); 

   ret = H5Tclose( dtype_id ); assert( ret == 0 );
   ret = H5Sclose( space_id ); assert( ret == 0 );

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

   fprintf( stderr, "M6.2-r%d: Create CDT %s in %s tr %d\n", my_rank, type_name, obj_path, (int)tr_num );

   dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id );
   ret = H5Tcommit_ff( obj_id, type_name, dtype_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, tr_id, H5_EVENT_STACK_NULL ); 
   assert( ret == 0 );

   ret = H5Tclose( dtype_id ); assert ( ret == 0 );

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
   hid_t file_space_id;
   int nDims;
   hsize_t current_size, max_size;

   /* Open the dataset, confirm number of dimensions, get current & max size */
   dset_id = H5Dopen_ff( obj_id, dset_name, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); assert( dset_id );

   file_space_id = H5Dget_space( dset_id ); assert ( file_space_id );
   nDims = H5Sget_simple_extent_dims( file_space_id, &current_size, &max_size );
   assert( nDims == 1 );

   if ( my_rank < current_size ) {
      int cell_data;              /* sized to hold data for a single cell in the dataset*/
      hsize_t mem_size = 1;
      hid_t mem_space_id;

      /* set data value, create memory data space, then select the element to update */
      cell_data = ordinal*1000 + tr_num*100 + my_rank*10 + my_rank;
      mem_space_id = H5Screate_simple( 1, &mem_size, &mem_size ); assert( mem_space_id );

      hsize_t start, stride, count, block;
      start = (hsize_t)my_rank;
      stride = count = block = 1;
      
      ret = H5Sselect_hyperslab( file_space_id, H5S_SELECT_SET, &start, &stride, &count, &block ); assert( ret == 0 );

      fprintf( stderr, "M6.2-r%d: Update %s[%d] in %s in tr %d to have value %d\n", 
                       my_rank, dset_name, my_rank, obj_path, (int)tr_num, cell_data );
      ret = H5Dwrite_ff( dset_id, H5T_NATIVE_INT, mem_space_id, file_space_id, H5P_DEFAULT, &cell_data, tr_id, H5_EVENT_STACK_NULL );
      assert( ret == 0 ); 
      ret = H5Sclose( mem_space_id ); assert( ret == 0 );

   } else {
      fprintf( stderr, "M6.2-r%d: No update to %s in %s in tr %d for this rank.\n", my_rank, dset_name, obj_path, (int)tr_num );
   }

   ret = H5Sclose( file_space_id ); assert( ret == 0 );
   ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); assert( ret == 0 ); 

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
   int s;
   char preface[1024];    

   /* First set up the preface, indenting 2 for each level in the recursion */
   sprintf( preface, "" );
   s = lvl * 2;
   while ( s ) { 
      strcat( preface, " " ); 
      s--; 
   }
   sprintf( preface, "%sM6.2-r%d:", preface, my_rank );

   /* Start the printing */
   ret = H5RCget_version( rc_id, &cv ); assert( ret == 0 );
   if ( lvl == 0 ) {
      fprintf( stderr, "%s -----------------\n", preface );
      fprintf( stderr, "%s Contents for container version %d\n", preface, (int)cv );
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
      sprintf( path_to_object, "%s @ %s", name, grp_path );
      ret = H5Aexists_by_name_ff( file_id, grp_path, name, H5P_DEFAULT, &exists, rc_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
      if ( exists ) { 
         hid_t attr_id, atype_id;
         char attr_val[ATTR_STR_LEN];

         attr_id = H5Aopen_by_name_ff( file_id, grp_path, name, H5P_DEFAULT, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         assert( attr_id );
   
         atype_id = H5Tcopy( H5T_C_S1 ); assert( atype_id );
         ret = H5Tset_size( atype_id, ATTR_STR_LEN ); assert( ret == 0 );

         ret = H5Aread_ff( attr_id, atype_id, attr_val, rc_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
         fprintf( stderr, "%s %s exists for cv %d and has value %s.\n", preface, path_to_object, (int) cv, attr_val );

         ret = H5Tclose( atype_id ); assert( ret == 0 );
         ret = H5Aclose_ff( attr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
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
         hsize_t current_size = 4;
         hsize_t max_size = 10;
         int data[4];              /* sized to accomodate current_size elements */
         hid_t space_id;
         hid_t dtype_id;
         hid_t dset_id;
         int i;

         dset_id = H5Dopen_ff( file_id, path_to_object, H5P_DEFAULT, rc_id, H5_EVENT_STACK_NULL ); 
         assert( dset_id );

         space_id = H5Screate_simple( 1, &current_size, &max_size ); assert( space_id );
         dtype_id = H5Tcopy( H5T_NATIVE_INT ); assert( dtype_id );
   
         ret = H5Dread_ff( dset_id, dtype_id, space_id, space_id, H5P_DEFAULT, data, rc_id, H5_EVENT_STACK_NULL ); 
         assert( ret == 0 );

         fprintf( stderr, "%s %s exists in cv %d and has values ", preface, path_to_object, cv );
         fprintf( stderr, "%d %d %d %d\n", data[0], data[1], data[2], data[3] );

         ret = H5Dclose_ff( dset_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
         ret = H5Tclose( dtype_id ); assert( ret == 0 );
         ret = H5Sclose( space_id ); assert( ret == 0 );
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
         fprintf( stderr, "%s %s exists in cv %d.\n", preface, path_to_object, (int) cv );
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
         fprintf( stderr, "%s %s exists in cv %d.\n", preface, path_to_object, (int) cv );
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


