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

#######################################################
# Different datatype sizes and different mix of options 
#######################################################
# test 2.1.0
dset2.1a dset2.1b h5diff_test1.h5 h5diff_test2.h5
# test 2.1.1
dset2.1a dset2.1b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.1.2
dset2.1a dset2.1b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.1.3
dset2.1a dset2.1b -p 3 h5diff_test1.h5 h5diff_test2.h5
#######################################################
# test 2.2.0
dset2.2a dset2.2b h5diff_test1.h5 h5diff_test2.h5
# test 2.2.1
dset2.2a dset2.2b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.2.2
dset2.2a dset2.2b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.2.3
dset2.2a dset2.2b -p 3 h5diff_test1.h5 h5diff_test2.h5
#######################################################
# test 2.3.0
dset2.3a dset2.3b h5diff_test1.h5 h5diff_test2.h5
# test 2.3.1
dset2.3a dset2.3b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.3.2
dset2.3a dset2.3b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.3.3
dset2.3a dset2.3b -p 3 h5diff_test1.h5 h5diff_test2.h5
#######################################################
# test 2.4.0
dset2.4a dset2.4b h5diff_test1.h5 h5diff_test2.h5
# test 2.4.1
dset2.4a dset2.4b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.4.2
dset2.4a dset2.4b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.4.3
dset2.4a dset2.4b -p 3 h5diff_test1.h5 h5diff_test2.h5
#######################################################
# test 2.5.0
dset2.5a dset2.5b h5diff_test1.h5 h5diff_test2.h5
# test 2.5.1
dset2.5a dset2.5b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.5.2
dset2.5a dset2.5b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.5.3
dset2.5a dset2.5b -p 3 h5diff_test1.h5 h5diff_test2.h5
#######################################################
# test 2.6.0
dset2.6a dset2.6b h5diff_test1.h5 h5diff_test2.h5
# test 2.6.1
dset2.6a dset2.6b -n 2 h5diff_test1.h5 h5diff_test2.h5
# test 2.6.2
dset2.6a dset2.6b -d 3 h5diff_test1.h5 h5diff_test2.h5
# test 2.6.3
dset2.6a dset2.6b -p 3 h5diff_test1.h5 h5diff_test2.h5

#######################################################
# Different combination of objects
#######################################################

# test 3.0
h5diff_test3.h5 h5diff_test4.h5

# test 3.1
dset3 dset3 h5diff_test3.h5 h5diff_test4.h5

# test 3.2
dset3 dset4 h5diff_test3.h5 h5diff_test4.h5

# test 3.3
dset6 dset3 h5diff_test3.h5 h5diff_test4.h5

# test 3.4
dset6 dset6 h5diff_test3.h5 h5diff_test4.h5

*/


int do_test_files(void)
{

 hid_t   file1_id, file2_id, file3_id, file4_id; 
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

 /* Test 2.1 */
 char    data2_1a[3][2] = {{1,1},{1,1},{1,1}};
 char    data2_1b[3][2] = {{1,1},{3,4},{5,6}};
 /* Test 2.2 */
 short   data2_2a[3][2] = {{1,1},{1,1},{1,1}};
 short   data2_2b[3][2] = {{1,1},{3,4},{5,6}};
 /* Test 2.3 */
 int    data2_3a[3][2] = {{1,1},{1,1},{1,1}};
 int    data2_3b[3][2] = {{1,1},{3,4},{5,6}};
 /* Test 2.4 */
 long   data2_4a[3][2] = {{1,1},{1,1},{1,1}};
 long   data2_4b[3][2] = {{1,1},{3,4},{5,6}};
 /* Test 2.5 */
 float  data2_5a[3][2] = {{1,1},{1,1},{1,1}};
 float  data2_5b[3][2] = {{1,1},{3,4},{5,6}};
 /* Test 2.6 */
 double  data2_6a[3][2] = {{1,1},{1,1},{1,1}};
 double  data2_6b[3][2] = {{1,1},{3,4},{5,6}};



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
 * Check H5T_NATIVE_CHAR data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.1a",H5T_NATIVE_CHAR,data2_1a);
 write_dataset(file2_id,2,dims2,"dset2.1b",H5T_NATIVE_CHAR,data2_1b);

/*-------------------------------------------------------------------------
 * Test 2.2
 * Check H5T_NATIVE_SHORT data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.2a",H5T_NATIVE_SHORT,data2_2a);
 write_dataset(file2_id,2,dims2,"dset2.2b",H5T_NATIVE_SHORT,data2_2b);

/*-------------------------------------------------------------------------
 * Test 2.3
 * Check H5T_NATIVE_INT data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.3a",H5T_NATIVE_INT,data2_3a);
 write_dataset(file2_id,2,dims2,"dset2.3b",H5T_NATIVE_INT,data2_3b);

/*-------------------------------------------------------------------------
 * Test 2.4
 * Check H5T_NATIVE_LONG data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.4a",H5T_NATIVE_LONG,data2_4a);
 write_dataset(file2_id,2,dims2,"dset2.4b",H5T_NATIVE_LONG,data2_4b);

/*-------------------------------------------------------------------------
 * Test 2.5
 * Check H5T_NATIVE_FLOAT data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.5a",H5T_NATIVE_FLOAT,data2_5a);
 write_dataset(file2_id,2,dims2,"dset2.5b",H5T_NATIVE_FLOAT,data2_5b);

/*-------------------------------------------------------------------------
 * Test 2.4
 * Check H5T_NATIVE_LONG data
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims2,"dset2.6a",H5T_NATIVE_DOUBLE,data2_6a);
 write_dataset(file2_id,2,dims2,"dset2.6b",H5T_NATIVE_DOUBLE,data2_6b);




/*-------------------------------------------------------------------------
 * Create two files
 *-------------------------------------------------------------------------
 */
  
 /* Create a file */
 file3_id = H5Fcreate ("h5diff_test3.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create a file */
 file4_id = H5Fcreate ("h5diff_test4.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * Test 3.0
 * Check for different objects
 *-------------------------------------------------------------------------
 */

 write_dataset(file3_id,1,dims1_1,"dset3",H5T_NATIVE_INT,0);
 write_dataset(file4_id,1,dims1_1,"dset3",H5T_NATIVE_INT,0);
 write_dataset(file3_id,1,dims1_1,"dset4",H5T_NATIVE_INT,0);
 write_dataset(file4_id,1,dims1_1,"dset5",H5T_NATIVE_INT,0);



 
/*-------------------------------------------------------------------------
 * Close files
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 status = H5Fclose(file2_id);
 status = H5Fclose(file3_id);
 status = H5Fclose(file4_id);


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

 /* Create a dataset */
 dataset_id = H5Dcreate(file_id,dset_name,type_id,space_id,H5P_DEFAULT);
  
 /* Write the data */
 if ( data )
  status = H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,data);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

 return 0;

}


