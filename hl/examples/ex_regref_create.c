/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
    This example creates a file and writes a two dimensional integer dataset
    to it.

    It then reopens the dataset and creates two region references to hyperslabs with
    coordinates (2,5)-(4,6) and (0,0)-(2,1). It then creates a new data set composed of data
    from the hyperslabs referenced by the newly created region references. The newly
    created dataset is then read and printed to the screen.
*/

#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "File.h5"
#define dsetname "DS"     /* dataset name */

#define DIM0  8   /* dataset dimensions */
#define DIM1  7

#define rank  2  /* dataset rank */

int main(void)
{
    hid_t file_id;         /* file identifier */
    hsize_t dims[rank] =  {DIM0, DIM1};  /* dataset dimensions */
    herr_t status;
    int data[DIM0][DIM1]; /* data */
    int i, j;
    int rdata[6][2]; /* buffer to read the data into */
    hsize_t num_elem; /* number of region references to create */
    const char *path[2]; /* paths to the data for the region references */
    hsize_t block_coord[8] ={ 2, 5, 4, 6, 0, 0, 2, 1}; /* hyperslab coordinates defining region references */
    hdset_reg_ref_t ref[2]; /* region references */
    hid_t file_id_array[2]; /* file id of the region references */

    num_elem = 2;

/*********************************************************  
   This writes data to the HDF5 file.  
 *********************************************************/ 
 
    path[0] ="/DS";
    path[1] ="/DS";
    /* 
     * Data initialization. 
     */
    printf("FULL 2D DATA:");
    for (i=0; i<DIM0; i++) {
      printf("\n[ ");
      for (j=0; j<DIM1; j++) {
	data[i][j] =  10*i+j;
	printf("%02d ", data[i][j]);
      }
      printf("]");
    }
    printf("\n");

    /*
     * Create file with default file access and file creation properties.
     */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    /*
     * Create and write the dataset.
     */
    status = H5LTmake_dataset ( file_id, dsetname, rank, dims, H5T_NATIVE_INT, data);

    status = H5Fclose(file_id);

/*************************************************************  

  This creates a series of region reference

 ************************************************************/  

    /*
     * Reopen the file.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);

    /*
     *   Create an array of region references using an array of paths
     *   and an array of corresponding hyperslab descriptions.
     */
    status = H5LRcreate_region_references(file_id,
					  num_elem,
					  path,
					  block_coord,
					  ref);
    /*
     * We are creating the data set in the same file so fill the file_id path with the same file id.
     */
    for (i=0; i< (hsize_t)num_elem; i++) 
      file_id_array[i] = file_id;

    /*
     * Check the region references: (1) create a new dataset from the data pointed to by the region references 
     */

    status = H5LRmake_dataset(file_id, "/DS2a", H5T_NATIVE_INT, num_elem, file_id_array, ( const hdset_reg_ref_t *)ref);


    /* print the newly created data set */

    status = H5LTread_dataset (file_id, "/DS2a", H5T_NATIVE_INT, rdata);
    
    printf("FULL 2D DATA CREATED BY LIST OF REGION REFERENCES:");
    for (i=0; i<6; i++) {
      printf("\n[ ");
      for (j=0; j<2; j++) {
	printf("%02d ", rdata[i][j]);
      }
      printf("]");
    }
    printf("\n");


    status = H5Fclose(file_id);

    return 0;
}



