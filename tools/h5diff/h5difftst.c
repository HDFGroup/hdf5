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



/* diff tst*/
int do_test_files(void);
int write_dataset( hid_t file_id, int rank, hsize_t *dims, const char *dset_name,
																		 hid_t type_id, void *data );


int main(int argc, const char *argv[])
{
 
 do_test_files();
 return 0;
}


/*-------------------------------------------------------------------------
 * these command line options are tested in ./testh5diff.sh
 *-------------------------------------------------------------------------
 */

/*
# test 1.1
dset1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5
g1.1 g1.1 h5diff_test1.h5 h5diff_test2.h5
# test 1.2
compound h5diff_test1.h5 h5diff_test2.h5
enum h5diff_test1.h5 h5diff_test2.h5
# test 1.3
dset1.3 h5diff_test1.h5 h5diff_test2.h5
# test 1.4
dset1.1 dset1.4 h5diff_test1.h5 h5diff_test2.h5
# test 1.5
dset1.1 dset1.5 h5diff_test1.h5 h5diff_test2.h5
# test 1.6
dset1.1 dset1.6 h5diff_test1.h5 h5diff_test2.h5

# Different datatype sizes and different mix of options 
# test 2.1.0
dset2.1 dset2.2 h5diff_test1.h5 h5diff_test2.h5
# test 2.1.1
dset2.1 dset2.2 -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.1.2
dset2.1 dset2.2 -d 3 h5diff_test1.h5 h5diff_test2.h5


*/


int do_test_files(void)
{

 hid_t   file1_id, file2_id; 
 hid_t   dataset_id;
 hid_t   space_id;  
 hid_t   group_id;
	hid_t   type_id, type2_id;  
 herr_t  status;
	int     val;

	/* Test 1. */
	hsize_t dims1  [1] = { 7 };
	hsize_t dims1_1[1] = { 8 };
	hsize_t dims2  [2] = { 3,2 };
 char    data1_3[]  = {"A string"};
	float   data1_4[7] = {1,1,3,4,5,6,7};

	/* Test 2. */
	char    data2_1[3][2] = {{1,1},{1,1},{1,1}};
	char    data2_2[3][2] = {{1,1},{3,4},{5,6}};

/*
	float   data5[3][2]   = {{1,1},{3,4},{5,6}};
 float   data6[3][2]   = {{1,1.1f},{3.02f,4.002f},{5.00002f,6}};
 double  data8[3][2]   = {{1,1},{3.40505e-9,4},{5,6}};
 double  data9[3][2]   = {{1,1},{3.58911e-9,4},{5,6}};*/

	/* Compound datatype */
 typedef struct s_t 
	{
 	int    a;
	 float  b;
 } s_t;

	typedef enum 
	{
  E_RED,
  E_GREEN
} e_t;

/*-------------------------------------------------------------------------
 * Create two files
 *-------------------------------------------------------------------------
 */
  
 /* Create a file */
 file1_id = H5Fcreate ("h5diff_test1.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a file */
 file2_id = H5Fcreate ("h5diff_test2.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * Test 1.1
	* Objects are not the same type (e.g try to compare a group with a dataset)
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate_simple(1,dims1,NULL);

 /* Create a dataset "dset1.1" */
 dataset_id = H5Dcreate(file1_id,"dset1.1",H5T_NATIVE_INT,space_id,H5P_DEFAULT);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

 /* Create a group "g1.1" on file2 */
 group_id = H5Gcreate(file2_id, "g1.1", 0);

 /* Close */
 status = H5Gclose(group_id);

/*-------------------------------------------------------------------------
 * Test 1.2
	* Objects are of classes H5G_TYPE and H5G_GROUP and their name is supplied
 *-------------------------------------------------------------------------
 */

 /* Create a group "g1.1" on file1 */
 group_id = H5Gcreate(file1_id, "g1.1", 0);

 /* Close */
 status = H5Gclose(group_id);

	/* Create a memory compound datatype on file1 */
	type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
	/* Commit compound datatype and close it */
 H5Tcommit(file1_id, "compound", type_id);
	type2_id = H5Tcopy(type_id);
	H5Tcommit(file2_id, "compound", type2_id);
	H5Tclose(type_id);
	H5Tclose(type2_id);
	
	/* Create a memory enum datatype on file1 */
	type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
	H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
	type2_id = H5Tcopy(type_id);

	/* Commit enumeration datatype and close it */
	H5Tcommit(file1_id, "enum", type_id);
	H5Tcommit(file2_id, "enum", type2_id);
	H5Tclose(type_id);
	H5Tclose(type2_id);

/*-------------------------------------------------------------------------
 * Test 1.3
	* Check for non supported classes. Supported classes are H5T_INTEGER and H5T_FLOAT
	* Non supported classes are
	* H5T_TIME, H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_COMPOUND, H5T_REFERENCE,
	* H5T_ENUM, H5T_VLEN, H5T_ARRAY
 *-------------------------------------------------------------------------
 */

/*-------------------------------------------------------------------------
 * Write two string datatypes
 *-------------------------------------------------------------------------
 */

 /* Create a data space  */
 space_id = H5Screate(H5S_SCALAR);

 /* Make a string type */
 type_id = H5Tcopy(H5T_C_S1);
 status = H5Tset_size (type_id, strlen(data1_3));

 /* Create a dataset "dset1.3" on file 1 */
 dataset_id = H5Dcreate(file1_id,"dset1.3",type_id,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,data1_3);

 /* Close */
 status = H5Dclose(dataset_id);

 /* Create a dataset "dset1.3" on file 2 */
 dataset_id = H5Dcreate(file2_id,"dset1.3",type_id,space_id,H5P_DEFAULT);
  
 /* Write the data */
 status = H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,data1_3);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * Test 1.4
	* Datasets are not the same class type
	* Write a float dataset and compare with integer "dset1.1"
 *-------------------------------------------------------------------------
 */

 write_dataset(file2_id,1,dims1,"dset1.4",H5T_NATIVE_FLOAT,data1_4);

/*-------------------------------------------------------------------------
 * Test 1.5
	* Datasets are not the same rank
 *-------------------------------------------------------------------------
 */

 write_dataset(file2_id,2,dims2,"dset1.5",H5T_NATIVE_INT,NULL);

/*-------------------------------------------------------------------------
 * Test 1.6
	* Check for the same current dimensions. Only compare if they are the same.
 *-------------------------------------------------------------------------
 */

 write_dataset(file2_id,1,dims1_1,"dset1.6",H5T_NATIVE_INT,NULL);

/*-------------------------------------------------------------------------
 * Test 2.1
	* Check for the same current dimensions. Only compare if they are the same.
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.1",H5T_NATIVE_CHAR,data2_1);
	write_dataset(file2_id,2,dims2,"dset2.2",H5T_NATIVE_CHAR,data2_2);


 
/*-------------------------------------------------------------------------
 * Close files
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 status = H5Fclose(file2_id);


 return 0;


}

/*-------------------------------------------------------------------------
 * Function: write_dataset
 *
 * Purpose: utility function to write a dataset
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: April 7, 2003
 *
 *-------------------------------------------------------------------------
 */

int write_dataset( hid_t file_id, int rank, hsize_t *dims, const char *dset_name,
																		 hid_t type_id, void *data )
{
 hid_t   dataset_id;
 hid_t   space_id;  
	herr_t  status;

	/* Create a data space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create a dataset "dset" */
 dataset_id = H5Dcreate(file_id,dset_name,type_id,space_id,H5P_DEFAULT);
  
 /* Write the data */
	if ( data )
  status = H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

	return 0;

}




#if 0


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

#endif