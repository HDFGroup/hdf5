

#include "hdf5.h"
#include "h5test.h"


/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extend. In the current version of the library 
 * the dataset MUST be chunked.
 *
 *-------------------------------------------------------------------------
 */

#define DATASETNAME "ShrinkArray" 
#define RANK         1
#define DIM          4



int main( void )
{
 
  hid_t   file_id;
  hid_t   dataset_id;
  hid_t   space_id;  
  hid_t   plist_id;
  hsize_t dims[RANK] = { DIM };
  hsize_t dims_new[RANK] = { 2 };
  hsize_t dims_chunk[RANK] = { 1 };
  hsize_t dims_out[RANK];
  hsize_t maxdims[RANK] = { H5S_UNLIMITED };
		int     data[ DIM ];
  int     buf1[ DIM ];
  int     buf2[ 3 ];
  herr_t  status; 
  int     i;

		for( i = 0; i < DIM; i++ )
			data[ i ] = i;
		

  TESTING("extend dataset");
  
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
  status = H5Dwrite( dataset_id , H5T_NATIVE_INT, space_id, H5S_ALL, H5P_DEFAULT, data );

/*-------------------------------------------------------------------------
 * Set new dimensions for the array; shrink it to (3)
 *-------------------------------------------------------------------------
 */
  
  /* Set new dimensions for the array. */
  status = H5Dset_extend( dataset_id , dims_new );

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
  status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf2 );

  /* Compare the read array with the original array */
  for( i = 0; i < dims_out[0]; i++ )
		{
   if (  buf2[i] != data[i] ) {
     goto out;
    }
   }


#if 0

/*-------------------------------------------------------------------------
 * Set new dimensions for the array; expand it again to (6)
 *-------------------------------------------------------------------------
 */
  
  /* Set new dimensions for the array. */
  status = H5Dset_extend( dataset_id , dims );

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
  status = H5Dread( dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf1 );

  /* Compare the read array with the original array */
  for( i = 0; i < dims_out[0]; i++ ) 
   {
    if ( i > 2 )
				{
     if (  buf1[i] != 0 ) 
      goto out;
				}
	   else
				{
     if (  buf1[i] != data[i] ) 
      goto out;
    }
   }

#endif
    
/*-------------------------------------------------------------------------
 * end
 *-------------------------------------------------------------------------
 */
 
 /* Close/release resources. */
 H5Dclose( dataset_id );
 H5Sclose( space_id );
 H5Pclose( plist_id  );
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

