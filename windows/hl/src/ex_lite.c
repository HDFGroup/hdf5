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



int main( void )
{
 

 hid_t   file_id; 
 hid_t   dataset_id;
 hid_t   space_id;  
 hsize_t dims[1] = { 5 };
 int     data[5] = {1,2,3,4,5};
 herr_t  status; 

	EXAMPLE("make an attribute");
  
 /* Create a file */
 file_id = H5Fcreate ("ex_lite3.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file_id,"dset",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Example of H5LTset_attribute_int
 *-------------------------------------------------------------------------
 */
  
 /* Create and write the attribute "attr1" on the dataset "dset" */
 status = H5LTset_attribute_int(file_id,"dset","attr1",data,5);

/*-------------------------------------------------------------------------
 * Example of H5LTget_attribute_int
 *-------------------------------------------------------------------------
 */

 /* Get the attribute "attr1" from the dataset "Dataset" */
 status = H5LTget_attribute_int(file_id,"dset","attr1",data);
    

 /* Close file */
 status = H5Fclose(file_id);

	PASSED();

 return 0;

}


