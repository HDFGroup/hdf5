/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdf.ncsa.uiuc.edu/HDF5/doc/Copyright.html.  If you do not have     *
 * access to either file, you may request a copy from hdfhelp@ncsa.uiuc.edu. *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */


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


