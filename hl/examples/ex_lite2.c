
/****************************************************************************
 * NCSA HDF                                                                 *
 * Scientific Data Technologies                                             *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING f.                                                           *
 *                                                                          *
 ****************************************************************************/

#include "H5LT.h"

int main( void )
{
 hid_t       file_id; 
 int         data[6];
 hsize_t     dims[2];
 herr_t      status;
 hsize_t     i, j, nrow, n_values;
 
 /* open file from ex_lite1.c */
 file_id = H5Fopen ("ex_lite1.h5", H5F_ACC_RDONLY, H5P_DEFAULT); 

 /* read dataset */
 status = H5LTread_dataset_int(file_id,"/dset",data);

 /* get the dimensions of the dataset */
 status = H5LTget_dataset_info(file_id,"/dset",dims,NULL,NULL);
 
 /* print it by rows */
 n_values = dims[0] * dims[1];
 nrow = dims[1];
 for (i=0; i<n_values/nrow; i++ )
 {
  for (j=0; j<nrow; j++)
   printf ("  %d", data[i*nrow + j]);
  printf ("\n");
 }
   
 /* close file */
 status = H5Fclose (file_id);

 return 0;


}


