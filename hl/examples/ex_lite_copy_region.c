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
    This examples creates a file and writes a two dimensional real dataset
    to it.

    It then reopens the file, and copies a region described by blocks 
    to another region described by another block.
    Main illustrative function: H5LTcopy_region 

*/

#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_lite_copy_region.h5"
#define DSETNAME "DS"     /* dataset name */

#define DIM0  6   /* dataset dimensions */
#define DIM1  7

#define rank  2  /* dataset rank */


int main(void)
{
    hid_t file_id;         /* file identifier */
    hsize_t dims[rank] =  {DIM0, DIM1}; /* dataset dimension */
    herr_t status;
    int data[DIM0][DIM1]; /* data */
    int i, j;
    hsize_t block_coord_src[4] ={ 0, 0, 1, 2}; /* hyperslab coordinates to copy from */
    hsize_t block_coord_dest[4] ={2, 3, 3, 5 }; /* destinations block coordinates to copy data to */
    int rdata[DIM0][DIM1]; /* read data buffer */
    

/*********************************************************  
   This writes data to the HDF5 file.  
 *********************************************************/ 
 
    /* 
     * Data  and output buffer initialization. 
     */
    printf("FULL 2D ARRAY:");
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
    status = H5LTmake_dataset ( file_id, DSETNAME, 2, dims, H5T_NATIVE_INT, data);

    /*
     * Close/release resources.
     */
    status = H5Fclose(file_id);

/*************************************************************  

  This copies a region described by blocks to another region 
  described by another block

 ************************************************************/  
    status = H5LTcopy_region(filename,
 			     "/DS",
 	                     block_coord_src,
 			     filename,
  			     "/DS",
  			     block_coord_dest);

    /* Read and print the destination region */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    status = H5LTread_dataset(file_id,"/DS",H5T_NATIVE_INT,rdata);
    status = H5Fclose(file_id);

    printf("2D DATA AFTER H5LTCOPY_REGION: [(%d,%d)-(%d,%d)] --> [(%d,%d)-(%d,%d)]",
	   (int)block_coord_src[0],(int)block_coord_src[1], (int)block_coord_src[2],(int)block_coord_src[3],
	   (int)block_coord_dest[0], (int)block_coord_dest[1],(int)block_coord_dest[2], (int)block_coord_dest[3]);

    for (i=0; i< DIM0; i++)
      {
	printf("\n  [ ");
	for (j=0; j< DIM1; j++) {
	  printf("%02d ", rdata[i][j]);
	}
	printf("]");
      }
    printf("\n");

    return 0;
}



