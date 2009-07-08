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

    This example creates a file and writes a two dimensional real dataset
    to it.

    It then reopens the file and reads a region of data given a set
    of corner coordinates.
    Main illustrative function: H5LTread_region
*/

#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_lite_read_region.h5"
#define DSETNAME "DS"     /* dataset name */

#define DIM0  5   /* dataset dimensions */
#define DIM1  4

#define RANK  2  /* dataset RANK */


int main(void)
{
    hid_t file_id;         /* file identifier */
    hsize_t dims[RANK] =  {DIM0, DIM1};
    herr_t status;
    float data[DIM0][DIM1];
    int i, j;
    hsize_t block_coord[4] ={ 2, 2, 4, 3};
    float rdata[3][2];
    

/*********************************************************  
   This writes data to the HDF5 file.  
 *********************************************************/ 
 
    /* 
     * Data initialization. 
     */
    printf("FULL 2D ARRAY:");
    for (i=0; i<DIM0; i++) {
      printf("\n[ ");
      for (j=0; j<DIM1; j++) {
	data[i][j] =  (float)((DIM0-1)*i+j);
	printf("%f ", data[i][j]);
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
    status = H5LTmake_dataset ( file_id, DSETNAME, 2, dims, H5T_NATIVE_FLOAT, data);
    
    status = H5Fclose(file_id);

/*************************************************************  

  This reads a region of data given corner coordinates

 ************************************************************/  

    status = H5LTread_region(filename,
			     "/DS",
	                     block_coord,
 			     H5T_NATIVE_FLOAT,
 			     rdata);
    
    printf("REGION 2D HYPERSLAB BY CORNER COORDINATES ,");
    printf(" COORDINATES (%d,%d)-(%d,%d):\n",(int)block_coord[0],(int)block_coord[1],
	   (int)block_coord[2],(int)block_coord[3]);
    

    for (i=0; i< block_coord[2] - block_coord[0] + 1; i++)
      {
	printf("\n  [ ");
	for (j=0; j< block_coord[3] - block_coord[1] + 1; j++) {
	  printf("%f ", rdata[i][j]);
	}
	printf("]");
      }
    printf("\n");
    
    return 0;
}



