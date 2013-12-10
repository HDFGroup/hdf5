/*
 * M6.2-demo.c: This is the demo program for Milestone 6.2 - HDF5 and IO Dispatcher Container Versioning Demonstration.
 * This program runs on one or more compute nodes and makes calls to the HDF5 API.
 * The Function Shipper server should be running before this demo program is started.
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
void print_container_contents( hid_t, hid_t, const char*, int );

int main( int argc, char **argv ) {
   const char file_name[]="m6.2-eff_container.h5";

   int my_rank, comm_size;
   int provided;

   hid_t fapl_id;
   hid_t file_id;
   herr_t ret;

   uint64_t version;
   hid_t rc_id0, rc_id1, rc_id2;

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
   fprintf( stderr, "M6.2-r%d: Initialize EFF stack\n", my_rank );
   EFF_init( MPI_COMM_WORLD, MPI_INFO_NULL );

   /* Specify that the IOD VOL plugin should be used and create H5File (EFF container) */
   fprintf( stderr, "M6.2-r%d: Create the container %s\n", my_rank, file_name );
   fapl_id = H5Pcreate( H5P_FILE_ACCESS ); assert( fapl_id );
   ret = H5Pset_fapl_iod( fapl_id, MPI_COMM_WORLD, MPI_INFO_NULL ); assert( ret == 0 );
   file_id = H5Fcreate_ff( file_name, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id, H5_EVENT_STACK_NULL ); assert( file_id );

   /* Acquire a read handle for container version 0 and create a read context. */
   version = 0;
   fprintf( stderr, "M6.2-r%d: Acquire read context for container version %d\n", my_rank, (int)version );
   rc_id0 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( rc_id0 ); assert( version == 0 );

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 1st call to print container contents\n", my_rank );
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
      fprintf( stderr, "M6.2-r%d: create transaction %d\n", my_rank, (int)tr_num );
      tr_id = H5TRcreate( file_id, rc_id0, (uint64_t)tr_num ); assert( tr_id );

      /* Start transaction 1 with single transaction leader (the default) */
      fprintf( stderr, "M6.2-r%d: start transaction %d\n", my_rank, (int)tr_num );
      ret = H5TRstart( tr_id, H5P_DEFAULT, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Add updates to the transaction */

      /* 2 Attributes */
      create_string_attribute( file_id, "AA", tr_id, "/", my_rank, tr_num );
      create_string_attribute( file_id, "AB", tr_id, "/", my_rank, tr_num );

      /* 2 Groups */
      create_group( file_id, "GA", tr_id, "/", my_rank, tr_num );
      create_group( file_id, "GB", tr_id, "/", my_rank, tr_num );
    
      /* 2 Datasets */
      create_dataset( file_id, "DA", tr_id, "/", my_rank, tr_num, 1 );
      create_dataset( file_id, "DB", tr_id, "/", my_rank, tr_num, 2 );

      /* 2 committed datatypes */
      create_committed_datatype( file_id, "TA", tr_id, "/", my_rank, tr_num );
      create_committed_datatype( file_id, "TB", tr_id, "/", my_rank, tr_num );

      /* Finish (and commit) transaction 1, causing the updates to appear in container version 1. */
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Close the local transaction. */
      ret = H5TRclose( tr_id ); assert( ret == 0 );
   }

   /* Print container contents at this point */
   fprintf( stderr, "M6.2-r%d: 2nd call to print container contents\n", my_rank );
   print_container_contents( file_id, rc_id0, "/", my_rank );

   /* Acquire a read handle for container version 1 and create a read context. */
   version = 1;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d\n", my_rank, (int)version );
   rc_id1 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( rc_id1 < 0 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv %d - sleep then retry\n", my_rank, (int)version );
      sleep( 1 );
      version = 1;      
      rc_id1 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }

   assert( rc_id1 > 0 ); assert ( version == 1 );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv 1\n", my_rank );
   fprintf( stderr, "M6.2-r%d: 3rd call to print container contents\n", my_rank );
   print_container_contents( file_id, rc_id1, "/", my_rank ); assert( ret == 0 );

   /* Release the read handle and close read context on cv 0 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 0\n", my_rank );
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

      /* 2 Groups */
      create_group( gr_id, "GA", tr_id, gr_path, my_rank, tr_num );
      create_group( gr_id, "GB", tr_id, gr_path, my_rank, tr_num );

      /* 2 1-D Datasets */
      create_dataset( gr_id, "DA", tr_id, gr_path, my_rank, tr_num, 1 );
      create_dataset( gr_id, "DB", tr_id, gr_path, my_rank, tr_num, 2 );

      /* 2 committed datatypes */
      create_committed_datatype( gr_id, "TA", tr_id, gr_path, my_rank, tr_num );
      create_committed_datatype( gr_id, "TB", tr_id, gr_path, my_rank, tr_num );

      /* Close the group */
      ret = H5Gclose_ff ( gr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );

#ifdef ABORT_WORKING 
// MOHAMAD - Here's where I tried the abort.
// in server output, saw message like 
// Transaction Abort 2
// h5ff_server: mdhim_kv.c:223: _mdhim_kv_update_record_cb_fn: Assertion `rc == 0 && oh.cookie != 0' failed.
//

MPI_Barrier( MPI_COMM_WORLD );
if ( my_rank == 1 ) {
   ret = H5TRabort( tr_id, H5_EVENT_STACK_NULL ); assert( ret == 0 );
} else {
      /* Finish (and commit) transaction 2, causing the updates to appear in container version 2. */
//      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );
}
MPI_Barrier( MPI_COMM_WORLD );
#endif

      /* Finish (and commit) transaction 2, causing the updates to appear in container version 2. */
      ret = H5TRfinish( tr_id, H5P_DEFAULT, NULL, H5_EVENT_STACK_NULL ); assert( ret == 0 );

      /* Close the local transaction. */
      ret = H5TRclose( tr_id ); assert( ret == 0 );
   }

   fprintf( stderr, "M6.2-r%d: 4th call to print container contents\n", my_rank );
   print_container_contents( file_id, rc_id1, "/", my_rank ); assert( ret == 0 );

   version = 2;
   fprintf( stderr, "M6.2-r%d: Try to acquire read context for cv %d\n", my_rank, (int)version );
   rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   while ( version != 2 ) {
      fprintf( stderr, "M6.2-r%d: Failed to acquire read context for cv 2 - sleep then retry\n", my_rank );
      sleep( 1 );
      version = 2;
      rc_id2 = H5RCacquire( file_id, &version, H5P_DEFAULT, H5_EVENT_STACK_NULL ); 
   }
   assert( rc_id2 > 0 ); assert ( version == 2 );
   fprintf( stderr, "M6.2-r%d: Acquired read context for cv 2\n", my_rank );
   fprintf( stderr, "M6.2-r%d: 5th call to print container contents\n", my_rank );
   print_container_contents( file_id, rc_id2, "/", my_rank ); assert( ret == 0 );

   /* Release the read handle and close read context on cv 1 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 1\n", my_rank );
   ret = H5RCrelease( rc_id1, H5_EVENT_STACK_NULL); assert( ret == 0 );
   ret = H5RCclose( rc_id1 ); assert( ret == 0 );

/* ADD MORE THINGS HERE */

   /* Release the read handle and close read context on cv 2 */
   fprintf( stderr, "M6.2-r%d: Release read handle on cv 2\n", my_rank );
   ret = H5RCrelease( rc_id2, H5_EVENT_STACK_NULL ); assert( ret == 0 );
   ret = H5RCclose( rc_id2 ); assert( ret == 0 );

   ret = H5Fclose( file_id ); assert( ret == 0 );
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

   /* End printing */
   if ( lvl == 0 ) {
      fprintf( stderr, "%s -----------------\n", preface );
   }

   return;
}

