
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
#include <stdlib.h>

#define ATTR_SIZE 5

int main( void )
{
 hid_t   file_id; 
 hid_t   dset_id;
 hid_t   space_id;  
 hsize_t dims[1] = { ATTR_SIZE };
 int     data[ATTR_SIZE] = {1,2,3,4,5};
 herr_t  status; 
 int     i;
  
 /* create a file */
 file_id = H5Fcreate ("ex_lite3.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* create a dataset named "dset" */
 dset_id = H5Dcreate(file_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

 /* close */
 status = H5Dclose(dset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * example of H5LTset_attribute_int
 *-------------------------------------------------------------------------
 */
  
 /* create and write the attribute "attr1" on the dataset "dset" */
 status = H5LTset_attribute_int(file_id,"dset","attr1",data,ATTR_SIZE);

/*-------------------------------------------------------------------------
 * example of H5LTget_attribute_int
 *-------------------------------------------------------------------------
 */

 /* get the attribute "attr1" from the dataset "dset" */
 status = H5LTget_attribute_int(file_id,"dset","attr1",data);

 for (i=0; i< ATTR_SIZE; i++ )
 {
  printf ("  %d", data[i]);
 }
 printf ("\n");

 /* close file */
 status = H5Fclose(file_id);

 return 0;

}

