

#include "hdf5.h"

/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extend. In the current version of the library 
 * the dataset MUST be chunked.
 *
 *-------------------------------------------------------------------------
 */

#define DATASETNAME "ShrinkArray" 
#define RANK         2


int main( void )
{
 
  hid_t   file_id;
  hid_t   dataset_id;
  hid_t   space_id;  
  hid_t   plist_id;
  hsize_t dims[2] = { 3, 3};
  hsize_t dims_new[2] = { 2, 2};
  hsize_t dims_chunk[2] = { 3, 3};
  hsize_t dims_out[2];
  hsize_t maxdims[1] = { H5S_UNLIMITED };
  int     data1[3][3] = { {1, 2, 3}, {4, 5, 6}, {7, 8, 9} }; 
  int     buf[2][2];
  herr_t  status; 
  int     i, j;
  
  /* Create a new file using default properties. */
  file_id = H5Fcreate( "set_extend.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT );

  /* Create the data space with unlimited dimensions. */
  space_id = H5Screate_simple( RANK, dims, maxdims ); 

  /* Modify dataset creation properties, i.e. enable chunking. */
  plist_id = H5Pcreate (H5P_DATASET_CREATE);
  status = H5Pset_chunk( plist_id, RANK, dims_chunk);

  /* Create a new dataset within the file using cparms creation properties. */
  dataset_id = H5Dcreate( file_id , DATASETNAME, H5T_NATIVE_INT, space_id, plist_id );

  /* Write the data. */
  status = H5Dwrite( dataset_id , H5T_NATIVE_INT, space_id, H5S_ALL, H5P_DEFAULT, data1 );

/*-------------------------------------------------------------------------
 * Set new dimensions for the array
 *-------------------------------------------------------------------------
 */
  
  /* Set new dimensions for the array. */
  status = H5Dset_extend( dataset_id , dims_new );

   /* Get the space. */
  space_id = H5Dget_space( dataset_id );

  /* Get dimensions. */
  status = H5Sget_simple_extent_dims( space_id, dims_out, NULL);

  for( i = 0; i < 2; i++ ) 
  {
   if (  dims_out[i] != dims_new[i] ) 
    goto out;
  }


/*-------------------------------------------------------------------------
 * Read
 *-------------------------------------------------------------------------
 */
 
  /* Read the new dataset. */
  status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf );

  /* Compare the read array with the original array */
  for( i = 0; i < dims_out[0]; i++ ) 
   for( j = 0; j < dims_out[1]; j++ )
   {
    if (  buf[i][j] != data1[i][j] ) {
     goto out;
    }
   }


    
/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */
 
 /* Close/release resources. */
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
 H5Fclose( file_id );
 printf("%s \n", "H5Dset_extend test PASSED" );

 return 0;

out:
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
 H5Fclose( file_id );
 printf("%s \n", "H5Dset_extend FAILED" );
 return 1;

}

