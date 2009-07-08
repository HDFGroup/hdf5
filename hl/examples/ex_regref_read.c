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
    This example shows how to create, store and read data associated 
    with region references.

    It creates a file and writes a two dimensional integer dataset
    to it. Then it creates a region reference to a subset hyperslab region of 
    the data.

    It then reopens the files, obtains information about the data associated
    with the region reference, creates an array to store the data, and then
    reads the hyperslab slab subset of the data and prints it to the screen.

    Main illustrative functions: H5LRget_region_info, H5LRread_region
*/
#include <stdlib.h>
#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_regref_read.h5"
#define dsetname "/DS"     /* dataset name */
#define dsetname_ref "MY_REF"    /* dataset name of region references */

#define DIM0  5   /* dataset dimensions */
#define DIM1  4
#define DIM_REF 1

#define rank  2    /* dataset rank */
#define rank_ref 1 /* region reference rank */


int main(void)
{
    hid_t file_id;         /* file identifier */
    hid_t space_id_ref;    /* region reference dataspace identifier */
    hid_t dset_id_ref;   /* region reference dataset identifier */
    hsize_t dims[rank] =  {DIM0, DIM1};       /* dataset dimensions */
    hsize_t dims_ref[rank_ref] =  {DIM_REF};  /* region reference dimensions */
    herr_t status;
    hdset_reg_ref_t ref[DIM_REF];  /* region references */
    int data[DIM0][DIM1];          /* data */
    int i, j;
    size_t nlength;   /* size of the buffer to store the path in */
    int rank_out;     /* the number of dimensions of the dataset pointed by region reference */
    hid_t dtype;      /* datatype of the dataset pointed by the region reference */
    hsize_t *buf;     /* contains the description of the region pointed by region reference */
    hsize_t rdims[2];
    char *name;            /* full path that a region reference points to */
    size_t numelem;        /* number of coordinate blocks or selected elements */
    const char *path[1];   /* paths to the data for the region references */
    H5S_sel_type sel_type; /* type of selection (hyperslab or point) */
    int **rdata;           /* buffer to read data into */
    size_t size_ref; 
    hsize_t block_coord[4] ={ 1, 1, 3, 2}; /* hyperslab coordinates defining region references */

    path[0] = dsetname;

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

  This reads the hyperslab pointed to by a region reference

 ************************************************************/  

    /*
     * Reopen the file to read selections back.
     */
    file_id = H5Fopen(filename, H5F_ACC_RDWR,  H5P_DEFAULT);
    nlength = 0;
    rank_out = 0;

    /* Obtain information about the data associated with the region reference */

    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[0],
				 &nlength,
				 NULL,
				 &rank_out,
				 NULL,
				 &sel_type,
				 &numelem,
				 NULL);

    /* allocate the name */
    name = (char *) malloc (nlength);

    /* allocate space for hyperslab block
                            | NUMBLOCKS |   RANK    |  SIZEOF DATA TYPE | NUMBER COORDINATE POINTS | */
    buf = (hsize_t *)malloc(  numelem   * rank_out  *   sizeof(hsize_t) *          2                );

    /* Get region refererce information, hyperslab coordinates returned in buf */
    status = H5LRget_region_info(file_id,
				 (const hdset_reg_ref_t*)ref[0],
				 &nlength,
				 name,
				 &rank_out,
				 &dtype,
				 &sel_type,
				 &numelem,
				 buf );

    rdims[0] = buf[2] - buf[0] + 1;
    rdims[1] = buf[3] - buf[1] + 1;

    size_ref = H5Tget_size( dtype );

    /*
     * Allocate array of pointers to rows.
     */
    rdata = (int **) malloc (rdims[0] * size_ref);

    /*
     * Allocate space for integer data.
     */
    rdata[0] = (int *) malloc (rdims[0] * rdims[1] * size_ref);

    /*
     * Set the rest of the pointers to rows to the correct addresses.
     */
    for (i=1; i<rdims[0]; i++)
        rdata[i] = rdata[0] + i * rdims[1];

    /* Read a region of the data using a region reference and print it. */

    status = H5LRread_region(file_id,(const hdset_reg_ref_t*)ref[0], 
			     dtype,
			     &numelem, 
			     rdata[0]);
    
    printf("\nREGION REFERENCED FOR 2D HYPERSLAB WITH");
    printf(" COORDINATES (%d,%d)-(%d,%d):",
	   (int)buf[0],(int)buf[1],(int)buf[2],(int)buf[3]);

    for (i=0; i< rdims[0]; i++)
      {
	printf("\n  [ ");
	for (j=0; j< rdims[1]; j++) {
	  printf("%d ", rdata[i][j]);
	}
	printf("]");
      }
    printf("\n");

    free(rdata);
    status = H5Fclose(file_id);

    return 0;
}



