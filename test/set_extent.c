
#include "hdf5.h"
#include "h5test.h"


/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extent. In the current version of the library 
 * the dataset MUST be chunked.
 *
 *-------------------------------------------------------------------------
 */

#define RANK 2


int main( void )
{
 
 hid_t   file_id;
 hid_t   dataset_id;
 hid_t   space_id;  
 hid_t   plist_id;
 hsize_t dims[RANK] = { 90, 90 };
 hsize_t dims_new[RANK] = { 70, 70 };
 hsize_t dims_chunk[RANK] = { 20, 20 };
 hsize_t dims_out[RANK];
 hsize_t maxdims[RANK] = { H5S_UNLIMITED, H5S_UNLIMITED };
 int     data[ 90 ][ 90 ];
 int     buf1[ 70 ][ 70 ];
 int     buf2[ 90 ][ 90 ];
 herr_t  status; 
 int     i, j, n = 0;
 
 
 for( i = 0; i < 90; i++ )
 {
  for( j = 0; j < 90; j++ )
  {
   data[i][j] = n++;
  }
 }
 
 
/*-------------------------------------------------------------------------
 * Test H5Dset_extent with chunks on the raw data cache
 *-------------------------------------------------------------------------
 */
 
 TESTING("extend dataset create");
 
 
 /* Create a new file using default properties. */
 file_id = H5Fcreate( "set_extent_create.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
 
 /* Create the data space with unlimited dimensions. */
 space_id = H5Screate_simple( RANK, dims, maxdims ); 
 
 /* Modify dataset creation properties, i.e. enable chunking. */
 plist_id = H5Pcreate (H5P_DATASET_CREATE);
 status = H5Pset_chunk( plist_id, RANK, dims_chunk);
 
 
/*-------------------------------------------------------------------------
 * Create and write one dataset
 *-------------------------------------------------------------------------
 */
 
 /* Create a new dataset */
 dataset_id = H5Dcreate( file_id , "Dataset", H5T_NATIVE_INT, space_id, plist_id );
 
 /* Write the data. */
 status = H5Dwrite( dataset_id , H5T_NATIVE_INT, space_id, H5S_ALL, H5P_DEFAULT, data );
 
/*-------------------------------------------------------------------------
 * Set new dimensions for the array; shrink it 
 *-------------------------------------------------------------------------
 */
 
 /* Set new dimensions for the array. */
 status = H5Dset_extent( dataset_id , dims_new );
 
 /* Get the space. */
 space_id = H5Dget_space( dataset_id );
 
 /* Get dimensions. */
 status = H5Sget_simple_extent_dims( space_id, dims_out, NULL);
 
 if ( dims_out[0] != dims_new[0] ) 
  goto out;
 
 
/*-------------------------------------------------------------------------
 * Read
 *-------------------------------------------------------------------------
 */
 
 /* Read the new dataset. */
 status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1 );
 
 
 /* Compare the read array with the original array */
 for( i = 0; i < dims_out[0]; i++ )
 {
  for( j = 0; j < dims_out[1]; j++ )
  {
   if (  buf1[i][j] != data[i][j] ) {
    goto out;
   }
  }
 }
 
 
#if 0
 
/*-------------------------------------------------------------------------
 * Set new dimensions for the array; expand it again 
 *-------------------------------------------------------------------------
 */
 
 /* Set new dimensions for the array. */
 status = H5Dset_extent( dataset_id , dims );
 
 /* Get the space. */
 space_id = H5Dget_space( dataset_id );
 
 /* Get dimensions. */
 status = H5Sget_simple_extent_dims( space_id, dims_out, NULL);
 
 if ( dims_out[0] != dims[0] ) 
  goto out;
 
 
/*-------------------------------------------------------------------------
 * Read
 *-------------------------------------------------------------------------
 */
 
 /* Read the new dataset. */
 status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2 );
 
 /* Compare the read array with the original array */
 for( i = 0; i < dims_out[0]; i++ )
 {
  for( j = 0; j < dims_out[1]; j++ )
  {
   if ( i > 70 || j > 70 )
   {
    if (  buf2[i][j] != 0 ) {
     goto out;
    }
   }
   else
   {
    if (  buf2[i][j] != data[i][j] ) {
     goto out;
    }
   }
   
  }
 }
 
#endif
 
/*-------------------------------------------------------------------------
 * Close/release resources
 *-------------------------------------------------------------------------
 */
 
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
 H5Fclose( file_id );
 
 
 
 PASSED();
 
 
/*-------------------------------------------------------------------------
 * Test H5Dset_extent with chunks written to file
 *-------------------------------------------------------------------------
 */
 
 TESTING("extend dataset read");
 
 /* Create a new file using default properties. */
 file_id = H5Fcreate( "set_extent_read.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );
 
 /* Create the data space with unlimited dimensions. */
 space_id = H5Screate_simple( RANK, dims, maxdims ); 
 
 /* Modify dataset creation properties, i.e. enable chunking. */
 plist_id = H5Pcreate (H5P_DATASET_CREATE);
 status = H5Pset_chunk( plist_id, RANK, dims_chunk);
 
 /* Create a new dataset within the file using cparms creation properties. */
 dataset_id = H5Dcreate( file_id , "Dataset", H5T_NATIVE_INT, space_id, plist_id );
 
 /* Write the data. */
 status = H5Dwrite( dataset_id , H5T_NATIVE_INT, space_id, H5S_ALL, H5P_DEFAULT, data );
 
 /* Close/release resources. */
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
 H5Fclose( file_id );
 
 
 /* Open the file */
 file_id = H5Fopen( "set_extent_read.h5", H5F_ACC_RDWR, H5P_DEFAULT );
 
 /* Open the dataset */
 dataset_id = H5Dopen( file_id , "Dataset" );
 
#if 0
 /* Read the dataset. */
 status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data );
#endif
 
 /* Set new dimensions for the array. */
 status = H5Dset_extent( dataset_id, dims_new );
 
 /* Get the space. */
 space_id = H5Dget_space( dataset_id );
 
 /* Get dimensions. */
 status = H5Sget_simple_extent_dims( space_id, dims_out, NULL);
 
 if ( dims_out[0] != dims_new[0] ) 
  goto out;
 
#if 1
 
 /* Read the new dataset. */
 status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1 );
 
 /* Compare the read array with the original array */
 for( i = 0; i < dims_out[0]; i++ )
 {
  for( j = 0; j < dims_out[1]; j++ )
  {
   if (  buf1[i][j] != data[i][j] ) {
    goto out;
   }
  }
 }
#endif
 
 
 
#if 0
 
/*-------------------------------------------------------------------------
 * Set new dimensions for the array; expand it again 
 *-------------------------------------------------------------------------
 */
 
 /* Set new dimensions for the array. */
 status = H5Dset_extent( dataset_id , dims );
 
 /* Get the space. */
 space_id = H5Dget_space( dataset_id );
 
 /* Get dimensions. */
 status = H5Sget_simple_extent_dims( space_id, dims_out, NULL);
 
 if ( dims_out[0] != dims[0] ) 
  goto out;
 
 /* Read the new dataset. */
 status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2 );
 
 /* Compare the read array with the original array */
 for( i = 0; i < dims_out[0]; i++ )
 {
  for( j = 0; j < dims_out[1]; j++ )
  {
   if ( i > 70 || j > 70 )
   {
    if (  buf2[i][j] != 0 ) {
     goto out;
    }
   }
   else
   {
    if (  buf2[i][j] != data[i][j] ) {
     goto out;
    }
   }
   
  }
 }
 
#endif
 
/*-------------------------------------------------------------------------
 * Close/release resources
 *-------------------------------------------------------------------------
 */
 
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Fclose( file_id );
 
 PASSED();
 
 return 0;
 
 
out:
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
 H5Fclose( file_id );
 H5_FAILED();
 return 1;
 
}

