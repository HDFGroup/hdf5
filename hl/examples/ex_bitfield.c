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
  This example shows how to read and write bitfield
  datatypes to a dataset.  The program first writes bit
  fields to a dataset with a dataspace of DIM0xDIM1, then
  closes the file.  Next, it reopens the file, and extracts
  the bit field using a given starting bit and number
  of bits in the bit-field. The values are returned
  as a base-10 integer.

  Main illustrative function: H5LTread_bitfield_value

*/

#include <stdlib.h>
#include "hdf5.h"
#include "hdf5_hl.h"

#define filename "ex_bitfield.h5"
#define DIM0            4
#define DIM1            7
#define num_flags       4

int main(void)
{
  int qf_data[DIM0][DIM1][num_flags];  /* Read buffer */
  unsigned int offset[4] = {0,2,4,6};  /* Starting bits to be extracted from element */
  unsigned int length[4] = {2,2,2,2};  /* Number of bits to be extracted for each value */
  hid_t file, space; /* Handles */
  hid_t qf_dset;
  int ndims;
  herr_t status;
  int rank = 2;
  hsize_t dims[2] =  {DIM0,DIM1}; 
  hid_t file_id;
  unsigned char wdata[DIM0][DIM1]; /* Write buffer */
  int i, j, k;
  
  /*
   * Initialize data.  We will manually fill four 2-bit integers into
   * each unsigned char data element.
   */
  for (i=0; i<DIM0; i++)
    for (j=0; j<DIM1; j++) {
      wdata[i][j] = 0;
      wdata[i][j] |= (i * j - j) & 0x03;          /* Field "A" */
      wdata[i][j] |= (i & 0x03) << 2;             /* Field "B" */
      wdata[i][j] |= (j & 0x03) << 4;             /* Field "C" */
      wdata[i][j] |= ( (i + j) & 0x03 ) <<6;      /* Field "D" */
    }
  /*
   * Create file with default file access and file creation properties.
   */
  file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  /*
   * write the data.
   */
  status = H5LTmake_dataset ( file_id, "Granule 1", rank, dims, H5T_NATIVE_UCHAR, wdata );
  /*
   * close the resources.
   */
  status = H5Fclose(file_id);
  
  /*
   * Open file.
   */
  file = H5Fopen (filename, H5F_ACC_RDONLY, H5P_DEFAULT);
  /* 
   * Open the data set
   */
  qf_dset = H5Dopen (file, "Granule 1",H5P_DEFAULT );
  /*
   * Get dataspace and allocate memory for read buffer. Quality flags dataset
   * has the same dimensionality as corresponding product dataset;
   * we are using its dimensions for illustration purposes only.
   */
  space = H5Dget_space (qf_dset);
  ndims = H5Sget_simple_extent_dims (space, dims, NULL);
  
  status = H5Sclose (space);
  
  /*
   * For each element read the value that takes first two bits and 
   * store it in a char buffer. This selects all the elements (H5S_ALL)
   */
  status = H5LTread_bitfield_value(qf_dset, num_flags, offset, length,
				   H5S_ALL, qf_data);
  /* Print out the bit field */
  printf("Bit Field:\n");
  for (i = 0; i<DIM0; i++) {
    printf (" [");
    for (j = 0; j<DIM1; j++) {
      printf(" {");
      for (k = 0; k<num_flags; k++){
	printf(" %d ", qf_data[i][j][k]);
      }
      printf("} ");
    }
    printf("]\n");
  }

  return 0;
  
}
