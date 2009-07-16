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
  This program shows how to create a dataset and region references associated
  with hyperslabs. It then creates a new dataset composed of data from the 
  data associated with a recursive loop over all the region references. 
  Main illustrative function: H5LRcreate_regref_to_all
*/

#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_regref_to_all.h5"
#define dsetnamev "MY_DATA" /* dataset name */
#define dsetnamer "MY_REF" /* region references name */

#define DIM0  9   /* dataset dimensions */
#define DIM1  8
#define RANK  2   /* dataset rank */


int main(void)
{
  hid_t file_id;        /* file identifier */
  hid_t space_id_ref;   /* region reference dataspace identifier */
  hid_t dset_id_ref;    /* region reference dataset identifiers*/
  hsize_t dims[RANK] =  {DIM0,DIM1}; /* dataset dimensions */
  hsize_t dims_ref[1] = {2}; /* region reference dimensions */
  herr_t status;
  hdset_reg_ref_t ref[2]; /* region references */
  int data[DIM0][DIM1];   /* data */
  int i, j;
  const char *path[2];    /* paths to the data for the region references */
  hsize_t block_coord[8] ={ 0, 0, 2, 2, 3, 3, 5, 5}; /* hyperslab coordinates defining region references */
  int rdata[18]; /* holds data from region references */
  
  /*********************************************************  
   This section writes data to the HDF5 file.  
  *********************************************************/ 
  
  path[0] ="/MY_DATA";
  path[1] ="/MY_DATA";
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

  /* Open the file. */
  
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /*
   * Create and write the dataset.
   */
  status = H5LTmake_dataset ( file_id, dsetnamev, RANK, dims, H5T_NATIVE_INT, data);
  
  /*
   * Create two region references with hyperslab coordinates: (0,0)-(2,2) and (3,3)-(5,5).
   */
  status = H5LRcreate_region_references(file_id, 2, path, block_coord, ref);
  
  /*
   * Write the region references to the file
   */
  space_id_ref = H5Screate_simple(1, dims_ref, NULL);
  dset_id_ref = H5Dcreate2(file_id, dsetnamer, H5T_STD_REF_DSETREG, space_id_ref, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  status = H5Dwrite(dset_id_ref, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);
  
  /*
   * Close all objects.
   */
  status = H5Sclose(space_id_ref);
  status = H5Dclose(dset_id_ref);
  status = H5Fclose(file_id);
  
/*************************************************************  

  This section creates a data set from region references found
  recursively in the file starting at "/" for this example.

************************************************************/  
  
  /*
   * Reopen the file.
   */
  file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
  
  /*
   * Create the "/MY_DATAc" dataset from regions of data.
   */
  
  status = H5LRcreate_regref_to_all(file_id, "/",
				    "/NEW_DATA", H5_INDEX_NAME, H5_ITER_INC, H5R_DATASET_REGION);

  return 0;
}



