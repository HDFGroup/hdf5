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

#include <stdio.h>
#include <stdlib.h>

#include "hdf5.h"

int do_test_files();


int main(int argc, const char *argv[])
{
 
 do_test_files();
 return 0;
}




/*-------------------------------------------------------------------------
 * do some test files 
 *-------------------------------------------------------------------------
 */

/*
dset1 dset2 h5diff_test1.h5 h5diff_test2.h5
dset1 dset2 -l h5diff_test1.h5 h5diff_test2.h5
h5diff_test1.h5 h5diff_test2.h5
dset1 dset2 -r h5diff_test1.h5 h5diff_test2.h5
dset1 dset2 -n 2 h5diff_test1.h5 h5diff_test2.h5
dset3 dset4 -d 0.01 h5diff_test1.h5 h5diff_test2.h5
dset5 dset6 -p 0.05 h5diff_test1.h5 h5diff_test2.h5
dset5 dset7 h5diff_test1.h5 h5diff_test2.h5
dset8 dset9 h5diff_test2.h5 h5diff_test2.h5
dset11 dset12 h5diff_test1.h5 h5diff_test2.h5
*/


int do_test_files()
{

 hid_t   file1_id, file2_id; 
 hid_t   dataset_id;
 hid_t   space_id;  
 hid_t   group_id, group2_id;
 hid_t   type_id;  
 hsize_t dims  [1] = { 7 };
 hsize_t dims2 [2] = { 3,2 };
 hsize_t dims3 [2] = { 3,3 };
 int     data1[7] = {1,1,1,1,1,1,1};
 int     data2[7] = {1,1,1,4,5,6,7};
 float   data3[7] = {1,1,3,4,5,6,7};
 float   data4[7] = {1,1,3.02f,4.002f,5.00002f,6,7};
 float   data5[3][2] = {1,1,3,4,5,6};
 float   data6[3][2] = {1,1.1f,3.02f,4.002f,5.00002f,6};
 float   data7[3][3] = {1,1,3,4,5,6,7,8,9};
 double  data8[3][2] = {1,1,3.40505e-9,4,5,6};
 double  data9[3][2] = {1,1,3.58911e-9,4,5,6};
 char    data10[] = {"A string"};
 long    data11[7] = {1,1,1,1,1,1,1};
 long    data12[7] = {1,1,1,4,5,6,7};
 herr_t  status;

/*-------------------------------------------------------------------------
 * Create two files
 *-------------------------------------------------------------------------
 */
  
 /* Create a file */
 file1_id = H5Fcreate ("h5diff_test1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a file */
 file2_id = H5Fcreate ("h5diff_test2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * Make dataset "dset1" on file1
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset1" */
 dataset_id = H5Dcreate(file1_id,"dset1",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data1);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset3" on file1
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset3" */
 dataset_id = H5Dcreate(file1_id,"dset3",H5T_NATIVE_FLOAT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data3);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);


/*-------------------------------------------------------------------------
 * Make dataset "g1/dset1" on file1
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a group. */
 group_id = H5Gcreate(file1_id, "g1", 0);

 /* Create a dataset "g1/dset1" */
 dataset_id = H5Dcreate(group_id,"dset1",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data1);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Gclose(group_id);
 status = H5Sclose(space_id);


/*-------------------------------------------------------------------------
 * Make dataset "dset1" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset1" */
 dataset_id = H5Dcreate(file2_id,"dset1",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data2);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);


/*-------------------------------------------------------------------------
 * Make dataset "dset2" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset1" */
 dataset_id = H5Dcreate(file2_id,"dset2",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data2);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);


/*-------------------------------------------------------------------------
 * Make dataset "g1/dset1" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a group. */
 group_id = H5Gcreate(file2_id, "g1", 0);

 /* Create a dataset "g1/dset1" */
 dataset_id = H5Dcreate(group_id,"dset1",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_INT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data2);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Gclose(group_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make group "g2/g1" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a group. */
 group_id = H5Gcreate(file2_id, "g2", 0);
 group2_id = H5Gcreate(group_id, "g1", 0);

 /* Close */
 status = H5Gclose(group_id);
 status = H5Gclose(group2_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset4" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset4",H5T_NATIVE_FLOAT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data4);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset5" on file1
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(2,dims2,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file1_id,"dset5",H5T_NATIVE_FLOAT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data5);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset6" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(2,dims2,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset6",H5T_NATIVE_FLOAT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data6);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset7" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(2,dims3,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset7",H5T_NATIVE_FLOAT,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_FLOAT,H5S_ALL,H5S_ALL,H5P_DEFAULT,data7);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset8" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(2,dims2,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset8",H5T_NATIVE_DOUBLE,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data8);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset9" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(2,dims2,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset9",H5T_NATIVE_DOUBLE,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_DOUBLE,H5S_ALL,H5S_ALL,H5P_DEFAULT,data9);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset10" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate(H5S_SCALAR);

 /* Make a string type */
 type_id = H5Tcopy(H5T_C_S1);
 status = H5Tset_size (type_id, strlen(data10));

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file2_id,"dset10",type_id,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,data10);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset11" on file1
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset1" */
 dataset_id = H5Dcreate(file1_id,"dset11",H5T_NATIVE_LONG,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_LONG,H5S_ALL,H5S_ALL,H5P_DEFAULT,data11);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Make dataset "dset12" on file2
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims,NULL);

 /* Create a dataset "dset12" */
 dataset_id = H5Dcreate(file2_id,"dset12",H5T_NATIVE_LONG,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,H5T_NATIVE_LONG,H5S_ALL,H5S_ALL,H5P_DEFAULT,data12);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

 
/*-------------------------------------------------------------------------
 * Close files
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 status = H5Fclose(file2_id);


 return 0;


}


