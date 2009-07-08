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
    to it. Then it creates a region reference to a hyperslab.

    It then reopens the file, copies data from a dataset (pointed to by a region
    reference) to a new location and creates a new reference to it.
    Main illustrative function: H5LRcopy_references
    
*/

#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_regref_copy_references.h5"
#define dsetname "DS"         /* dataset name */
#define dsetname_ref "REF"    /* dataset name of region references */

#define DIM0  5   /* dataset dimensions */
#define DIM1  4
#define DIM_REF 1

#define rank  2    /* dataset rank */
#define rank_ref 1 /* region reference rank */


int main(void)
{
    hid_t file_id;         /* file identifier */
    hid_t space_id_ref;    /* region reference dataspace identifier */
    hid_t dset_id_ref;     /* region reference dataset identifier */
    hsize_t dims[rank] =  {DIM0, DIM1}; /* dataset dimensions */
    hsize_t dims_ref[rank_ref] =  {DIM_REF}; /* region reference dimensions */
    hsize_t block_coord_dest[4] ={1, 2, 3, 3 };  /*destination hyperslab block coordinates */ 
    herr_t status;
    size_t numelem_size; /* number of elements in hyperslab  */
    hdset_reg_ref_t ref[DIM_REF]; /* buffer of region references */
    int data[DIM0][DIM1]; /* data */
    int i, j;
    int rdata[DIM0][DIM1];  /* reading buffers */
    int rdata2[3][2];
    hdset_reg_ref_t ref_new; /* region reference */
    hsize_t block_coord[4] ={ 1, 1, 3, 2}; /* hyperslab coordinates defining region references */
    const char *path[1];   /* paths to the data for the region references */

    path[0] = dsetname;

/*********************************************************  
   This writes data to the HDF5 file.  
 *********************************************************/ 
 
    /* 
     * Data  and output buffer initialization. 
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
    status = H5LTmake_dataset ( file_id, dsetname, 2, dims, H5T_NATIVE_INT, data);

    /*
     * Create a region reference with hyperslab coordinates: (1,1)-(3,2)
     */
    status = H5LRcreate_region_references(file_id, 1, path, block_coord, ref);
    /*
     * Write the region references to the file
     */
    space_id_ref = H5Screate_simple(1, dims_ref, NULL);
    dset_id_ref = H5Dcreate2(file_id, dsetname_ref, H5T_STD_REF_DSETREG, space_id_ref, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    status = H5Dwrite(dset_id_ref, H5T_STD_REF_DSETREG, H5S_ALL, H5S_ALL, H5P_DEFAULT,ref);

    /*
     * Close/release resources.
     */
    status = H5Sclose(space_id_ref);
    status = H5Dclose(dset_id_ref);
    status = H5Fclose(file_id);

/*************************************************************  

  This copies data from a dataset to a new location and creates
  a new reference to it.

 ************************************************************/  

    /*
     * Reopen the file to read selections back.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);


    /*
     * copy the dataset pointed to by ref[0] into the /DS data
     * defined by the coordinates block_coord_dest. ref_new is the region
     * reference to this new hyperslab.
     */

    status =  H5LRcopy_references( file_id, &ref[0], filename,
				   "/DS", block_coord_dest, &ref_new);

    /* read the copied to data set and print it */
    status = H5LTread_dataset(file_id,"/DS",H5T_NATIVE_INT,rdata);

    printf("FULL 2D DATA AFTER H5LRCOPY_REFERENCES: [(%d,%d)-(%d,%d)] --> [(%d,%d)-(%d,%d)]",
	   (int)block_coord[0],(int)block_coord[1],(int)block_coord[2],(int)block_coord[3],
	   (int)block_coord_dest[0], (int)block_coord_dest[1],(int)block_coord_dest[2], (int)block_coord_dest[3]);

    for (i=0; i<DIM0; i++) {
      printf("\n[ ");
      for (j=0; j<DIM1; j++) {
	printf("%02d ", rdata[i][j]);
      }
      printf("]");
    }
    printf("\n");

    printf("DATA POINTED TO BY NEW REGION REFERENCE");
    /* print the data pointed to by the new region reference */
    status = H5LRread_region(file_id,
  			     (const hdset_reg_ref_t*)ref_new,
  			     H5T_NATIVE_INT,
  			     &numelem_size,
  			     rdata2);

    for (i = 0 ; i < (hsize_t)(block_coord_dest[2] - block_coord_dest[0] + 1); i++) {
      printf("\n[ ");
      for (j = 0; j < (hsize_t)(block_coord_dest[3] - block_coord_dest[1] + 1) ; j++) {
	printf("%02d ", rdata2[i][j]);
      }
      printf("]");
    }
    printf("\n");


    status = H5Fclose(file_id);

    return 0;
}



