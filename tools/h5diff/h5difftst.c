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
#include "H5private.h" 

/*UINT_MAX Maximum value for a variable of type unsigned int. 4294967295 */
#define UIMAX 4294967295u

/* diff test*/
static int do_test_1(const char *file1, const char *file2);
static int do_test_2(const char *file1, const char *file2);
static int do_test_3(const char *file1, const char *file2);
static int do_test_4(const char *file1, const char *file2);
static int do_test_5(const char *file1, const char *file2);
static int write_dataset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
                          hid_t type_id, void *buf );


int main(int UNUSED argc, const UNUSED char *argv[])
{
 
 do_test_1("file1.h5","file2.h5");
 do_test_2("file3.h5",NULL);
 do_test_3("file4.h5",NULL);
 do_test_4("file5.h5",NULL);
 do_test_5("file6.h5",NULL);
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

static 
int write_dataset( hid_t loc_id, int rank, hsize_t *dims, const char *dset_name,
                   hid_t type_id, void *buf )
{
 hid_t   dataset_id;
 hid_t   space_id;  
 herr_t  status;

 /* Create a buf space  */
 space_id = H5Screate_simple(rank,dims,NULL);

 /* Create a dataset */
 dataset_id = H5Dcreate(loc_id,dset_name,type_id,space_id,H5P_DEFAULT);
  
 /* Write the buf */
 if ( buf )
 assert(H5Dwrite(dataset_id,type_id,H5S_ALL,H5S_ALL,H5P_DEFAULT,buf)>=0);

 /* Close */
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
 assert(status>=0);

 return status;

}

/*

# ##############################################################################
# # Common usage
# ##############################################################################


# 1.0
 -h

# 1.1
 file1.h5 file2.h5 g1/dset1 g1/dset2

# 1.2
 file1.h5 file2.h5 -n 2 g1/dset1 g1/dset2

# 1.3
 file1.h5 file2.h5 -d 5 g1/dset3 g1/dset4

# 1.4
 file1.h5 file2.h5 -p 0.05 g1/dset3 g1/dset4

# 1.5
 file1.h5 file2.h5 -r g1/dset1 g1/dset2

# 1.6
 file1.h5 file2.h5 

# ##############################################################################
# # not comparable types
# ##############################################################################

# 2.0
 file3.h5 file3.h5 dset group

# 2.1
 file3.h5 file3.h5 dset link

# 2.2
 file3.h5 file3.h5 dset type

# 2.3
 file3.h5 file3.h5 group group

# 2.4
 file3.h5 file3.h5 type type

# 2.5
 file3.h5 file3.h5 link link

# ##############################################################################
# # Class issues
# ##############################################################################


# 3.0
 file4.h5 file4.h5 string 

# 3.1
 file4.h5 file4.h5 bitfield 

# 3.2
 file4.h5 file4.h5 opaque

# 3.3
 file4.h5 file4.h5 compound

# 3.4
 file4.h5 file4.h5 ref

# 3.5
 file4.h5 file4.h5 enum

# 3.6
 file4.h5 file4.h5 vlen

# 3.7
 file4.h5 file4.h5 array

# 3.8
 file4.h5 file4.h5 integer float

# ##############################################################################
# # Dimensions
# ##############################################################################

# 4.0
 file5.h5 file5.h5 dset1 dset2

# 4.1
 file5.h5 file5.h5 dset3 dset4

# 4.2
 file5.h5 file5.h5 dset5 dset6


# ##############################################################################
# # Dataset types
# ##############################################################################

# 5.0
 file6.h5 file6.h5 dset0a dset0b

# 5.1
 file6.h5 file6.h5 dset1a dset1b

# 5.2
 file6.h5 file6.h5 dset2a dset2b

# 5.3
 file6.h5 file6.h5 dset3a dset4b

# 5.4
 file6.h5 file6.h5 dset4a dset4b

# 5.5
 file6.h5 file6.h5 dset5a dset5b

# 5.6
 file6.h5 file6.h5 dset6a dset6b

# 5.7
 file6.h5 file6.h5 dset7a dset7b

# 5.8
 file6.h5 file6.h5 dset8a dset8b

# ##############################################################################
# # Error messages
# ##############################################################################


# 6.0: Check if the command line number of arguments is less than 3
 h5diff_test1.h5 

# 6.1: Check for invalid options
 h5diff_test1.h5 h5diff_test2.h5 -x 

# ##############################################################################
# # -d 
# ##############################################################################

# 6.2: no value
 file1.h5 file2.h5 -d g1/dset3 g1/dset4

# 6.3: negative value
 file1.h5 file2.h5 -d -4 g1/dset3 g1/dset4

# 6.4: zero
 file1.h5 file2.h5 -d 0 g1/dset3 g1/dset4

# 6.5: non number
 file1.h5 file2.h5 -d u g1/dset3 g1/dset4

# 6.6: hexadecimal
 file1.h5 file2.h5 -d 0x1 g1/dset3 g1/dset4

# 6.7: string
 file1.h5 file2.h5 -d "1" g1/dset3 g1/dset4

# 6.8: repeated option
 file1.h5 file2.h5 -d 1 -d 2 g1/dset3 g1/dset4

# 6.9: number larger than biggest difference
 file1.h5 file2.h5 -d 200 g1/dset3 g1/dset4

# 6.10: number smaller than smallest difference
 file1.h5 file2.h5 -d 1 g1/dset3 g1/dset4


# ##############################################################################
# # -p
# ##############################################################################


# 6.11: no value
 file1.h5 file2.h5 -p g1/dset3 g1/dset4

# 6.12: negative value
 file1.h5 file2.h5 -p -4 g1/dset3 g1/dset4

# 6.13: zero
 file1.h5 file2.h5 -p 0 g1/dset3 g1/dset4

# 6.14: non number
 file1.h5 file2.h5 -p u g1/dset3 g1/dset4

# 6.15: hexadecimal
 file1.h5 file2.h5 -p 0x1 g1/dset3 g1/dset4

# 6.16: string
 file1.h5 file2.h5 -p "0.21" g1/dset3 g1/dset4

# 6.17: repeated option
 file1.h5 file2.h5 -p 0.21 -p 0.22 g1/dset3 g1/dset4

# 6.18: number larger than biggest difference
 file1.h5 file2.h5 -p 2 g1/dset3 g1/dset4

# 6.19: number smaller than smallest difference
 file1.h5 file2.h5 -p 0.005 g1/dset3 g1/dset4



# ##############################################################################
# # -n
# ##############################################################################


# 6.20: no value
 file1.h5 file2.h5 -n g1/dset3 g1/dset4

# 6.21: negative value
 file1.h5 file2.h5 -n -4 g1/dset3 g1/dset4

# 6.22: zero
 file1.h5 file2.h5 -n 0 g1/dset3 g1/dset4

# 6.23: non number
 file1.h5 file2.h5 -n u g1/dset3 g1/dset4

# 6.24: hexadecimal
 file1.h5 file2.h5 -n 0x1 g1/dset3 g1/dset4

# 6.25: string
 file1.h5 file2.h5 -n "2" g1/dset3 g1/dset4

# 6.26: repeated option
 file1.h5 file2.h5 -n 2 -n 3 g1/dset3 g1/dset4

# 6.27: number larger than biggest difference
 file1.h5 file2.h5 -n 200 g1/dset3 g1/dset4

# 6.28: number smaller than smallest difference
 file1.h5 file2.h5 -n 1 g1/dset3 g1/dset4

# ##############################################################################
# # non valid files
# ##############################################################################

 file1.h6 file2.h6

*/


/*-------------------------------------------------------------------------
 * Basic review tests
 *-------------------------------------------------------------------------
 */
static 
int do_test_1(const char *file1, const char *file2)
{

 hid_t   file1_id, file2_id; 
 hid_t   group1_id, group2_id, group3_id;
 herr_t  status;
 hsize_t dims[2] = { 3,2 };

 /* Test */
 double  data1[3][2] = {{1,1},{1,1},{1,1}};
 double  data2[3][2] = {{1,1.1},{1.01,1.001},{1.0001,1.000000001}};
 double  data3[3][2] = {{100,110},{100,100},{100,100}};
 double  data4[3][2] = {{110,100},{90,80},{140,200}};

/*-------------------------------------------------------------------------
 * Create two files
 *-------------------------------------------------------------------------
 */
  
 file1_id = H5Fcreate (file1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 
 file2_id = H5Fcreate (file2, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

 /* Create groups */
 group1_id = H5Gcreate(file1_id, "g1", 0);
 group2_id = H5Gcreate(file2_id, "g1", 0);
 group3_id = H5Gcreate(file2_id, "g2", 0);

 write_dataset(group1_id,2,dims,"dset1",H5T_NATIVE_DOUBLE,data1);
 write_dataset(group2_id,2,dims,"dset2",H5T_NATIVE_DOUBLE,data2);
 write_dataset(group1_id,2,dims,"dset3",H5T_NATIVE_DOUBLE,data3);
 write_dataset(group2_id,2,dims,"dset4",H5T_NATIVE_DOUBLE,data4);
 write_dataset(group2_id,2,dims,"dset1",H5T_NATIVE_DOUBLE,data2);

/*-------------------------------------------------------------------------
 * Close 
 *-------------------------------------------------------------------------
 */
 status = H5Gclose(group1_id);
 status = H5Gclose(group2_id);
 status = H5Gclose(group3_id);
 status = H5Fclose(file1_id);
 status = H5Fclose(file2_id);
 return status;
}

/*-------------------------------------------------------------------------
 * Compare different types: H5G_DATASET, H5G_TYPE, H5G_GROUP, H5G_LINK
 *-------------------------------------------------------------------------
 */
static 
int do_test_2(const char *file1, const char UNUSED *file2)
{

 hid_t   file1_id; 
 hid_t   group_id;
 hid_t   type_id;  
 herr_t  status;
 hsize_t dims[1]={1};
 /* Compound datatype */
 typedef struct s_t 
 {
  int    a;
  float  b;
 } s_t;

/*-------------------------------------------------------------------------
 * Create one file
 *-------------------------------------------------------------------------
 */
 file1_id = H5Fcreate (file1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * H5G_DATASET
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,1,dims,"dset",H5T_NATIVE_INT,0);

/*-------------------------------------------------------------------------
 * H5G_GROUP
 *-------------------------------------------------------------------------
 */
 group_id = H5Gcreate(file1_id, "group", 0);
 status = H5Gclose(group_id);

/*-------------------------------------------------------------------------
 * H5G_TYPE
 *-------------------------------------------------------------------------
 */

 /* Create a memory compound datatype */
 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
 /* Commit compound datatype and close it */
 H5Tcommit(file1_id, "type", type_id);
 H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5G_LINK
 *-------------------------------------------------------------------------
 */
 
 status = H5Glink(file1_id, H5G_LINK_SOFT, "dset", "link");

/*-------------------------------------------------------------------------
 * Close 
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 return status;
}




/*-------------------------------------------------------------------------
 * Check for non supported classes. Supported classes are H5T_INTEGER and H5T_FLOAT
 * Non supported classes are
 * H5T_TIME, H5T_STRING, H5T_BITFIELD, H5T_OPAQUE, H5T_COMPOUND, H5T_REFERENCE,
 * H5T_ENUM, H5T_VLEN, H5T_ARRAY
 *-------------------------------------------------------------------------
 */
static 
int do_test_3(const char *file1, const char UNUSED *file2)
{

 hid_t   file1_id; 
 hid_t   dataset_id;
 hid_t   space_id;  
 hid_t   type_id;  
 hsize_t dims[1]={1};
 herr_t  status;
 int     val;

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
 * Create a file
 *-------------------------------------------------------------------------
 */
 file1_id = H5Fcreate (file1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * H5T_STRING
 *-------------------------------------------------------------------------
 */
 space_id = H5Screate(H5S_SCALAR);
 type_id = H5Tcopy(H5T_C_S1);
 dataset_id = H5Dcreate(file1_id,"string",type_id,space_id,H5P_DEFAULT);
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_BITFIELD
 *-------------------------------------------------------------------------
 */

 type_id = H5Tcopy(H5T_STD_B8LE);
 write_dataset(file1_id,1,dims,"bitfield",type_id,0);
 status = H5Tclose(type_id);


/*-------------------------------------------------------------------------
 * H5T_OPAQUE
 *-------------------------------------------------------------------------
 */

 type_id = H5Tcreate(H5T_OPAQUE, 1);
 status = H5Tset_tag(type_id, "1-byte opaque type"); /* must set this */
 write_dataset(file1_id,1,dims,"opaque",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_COMPOUND
 *-------------------------------------------------------------------------
 */

 type_id = H5Tcreate (H5T_COMPOUND, sizeof(s_t));
 H5Tinsert(type_id, "a", HOFFSET(s_t, a), H5T_NATIVE_INT);
 H5Tinsert(type_id, "b", HOFFSET(s_t, b), H5T_NATIVE_FLOAT);
 write_dataset(file1_id,1,dims,"compound",type_id,0);
 status = H5Tclose(type_id);
 
/*-------------------------------------------------------------------------
 * H5T_REFERENCE
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,1,dims,"ref",H5T_STD_REF_OBJ,0);

/*-------------------------------------------------------------------------
 * H5T_ENUM
 *-------------------------------------------------------------------------
 */
 type_id = H5Tcreate(H5T_ENUM, sizeof(e_t));
 H5Tenum_insert(type_id, "RED",   (val = 0, &val));
 H5Tenum_insert(type_id, "GREEN", (val = 1, &val));
 write_dataset(file1_id,1,dims,"enum",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_VLEN
 *-------------------------------------------------------------------------
 */
 type_id = H5Tvlen_create(H5T_NATIVE_INT);
 write_dataset(file1_id,1,dims,"vlen",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_ARRAY
 *-------------------------------------------------------------------------
 */
 type_id = H5Tarray_create(H5T_NATIVE_INT,1,dims,NULL);
 write_dataset(file1_id,1,dims,"array",type_id,0);
 status = H5Tclose(type_id);

/*-------------------------------------------------------------------------
 * H5T_INTEGER vs H5T_FLOAT
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,1,dims,"integer",H5T_NATIVE_INT,0);
 write_dataset(file1_id,1,dims,"float",H5T_NATIVE_FLOAT,0);

/*-------------------------------------------------------------------------
 * Close 
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 return status;
}


/*-------------------------------------------------------------------------
 * Dimension issues
 *-------------------------------------------------------------------------
 */
static 
int do_test_4(const char *file1, const char UNUSED *file2)
{

 hid_t   file1_id; 
 hid_t   dataset_id;
 hid_t   space_id;
 hid_t   plist_id;
 herr_t  status;
 hsize_t dims1 [1]={7};
 hsize_t dims2 [4]={2,2,2,2};
 hsize_t dims21[4]={2,2,2,3};

/*-------------------------------------------------------------------------
 * Create a file
 *-------------------------------------------------------------------------
 */
 file1_id = H5Fcreate (file1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); 

/*-------------------------------------------------------------------------
 * rank
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,1,dims1,"dset1",H5T_NATIVE_INT,NULL);
 write_dataset(file1_id,4,dims2,"dset2",H5T_NATIVE_INT,NULL);

/*-------------------------------------------------------------------------
 * current dimensions
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,4,dims2,"dset3",H5T_NATIVE_INT,NULL);
 write_dataset(file1_id,4,dims21,"dset4",H5T_NATIVE_INT,NULL);

/*-------------------------------------------------------------------------
 * Check for the same maximum dimensions. Give a warning if they are different
 *-------------------------------------------------------------------------
 */

 space_id = H5Screate_simple(4,dims2,dims2);
 dataset_id = H5Dcreate(file1_id,"dset5",H5T_NATIVE_INT,space_id,H5P_DEFAULT);
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);
 space_id = H5Screate_simple(4,dims2,dims21);
 plist_id = H5Pcreate(H5P_DATASET_CREATE);
 status = H5Pset_chunk(plist_id,4,dims2);
 dataset_id = H5Dcreate(file1_id,"dset6",H5T_NATIVE_INT,space_id,plist_id);
 status = H5Dclose(dataset_id);
 status = H5Sclose(space_id);

/*-------------------------------------------------------------------------
 * Close 
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 return status;
}




/*-------------------------------------------------------------------------
 * Datasets datatypes 
 *-------------------------------------------------------------------------
 */
static 
int do_test_5(const char *file1, const char UNUSED *file2)
{

 hid_t   file1_id; 
 hsize_t dims[2]={3,2};
 herr_t  status;
 char    buf1a[3][2] = {{1,1},{1,1},{1,1}};
 char    buf1b[3][2] = {{1,1},{3,4},{5,6}};
 short   buf2a[3][2] = {{1,1},{1,1},{1,1}};
 short   buf2b[3][2] = {{1,1},{3,4},{5,6}};
 int     buf3a[3][2] = {{1,1},{1,1},{1,1}};
 int     buf3b[3][2] = {{1,1},{3,4},{5,6}};
 long    buf4a[3][2] = {{1,1},{1,1},{1,1}};
 long    buf4b[3][2] = {{1,1},{3,4},{5,6}};
 float   buf5a[3][2] = {{1,1},{1,1},{1,1}};
 float   buf5b[3][2] = {{1,1},{3,4},{5,6}};
 double  buf6a[3][2] = {{1,1},{1,1},{1,1}};
 double  buf6b[3][2] = {{1,1},{3,4},{5,6}};

 /*unsigned/signed test
   signed char -128 to 127 
   unsigned char 0 to 255 
  */
 char          buf7a[3][2] = {{-1,-128},{-1,-1},{-1,-1}};
 unsigned char buf7b[3][2] = {{1,128},{1,1},{1,1}};
 
 /* long_long test */
 long_long            buf8a[3][2] = {{1,1},{1,1},{1,1}};
 long_long            buf8b[3][2] = {{1,1},{3,4},{5,6}};
 unsigned long_long   buf9a[3][2] = {{1,1},{1,1},{1,1}};
 unsigned long_long   buf9b[3][2] = {{1,1},{3,4},{5,6}};

 unsigned int    buf10a[3][2] = {{UIMAX,1},{1,1},{1,1}};
 unsigned int    buf10b[3][2] = {{UIMAX-1,1},{3,4},{5,6}};
 
 
/*-------------------------------------------------------------------------
 * Create a file
 *-------------------------------------------------------------------------
 */
 file1_id = H5Fcreate (file1, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

/*-------------------------------------------------------------------------
 * Check for different storage order. Give a warning if they are different
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset0a",H5T_STD_I16LE,buf2a);
 write_dataset(file1_id,2,dims,"dset0b",H5T_STD_I32LE,buf3b);
 
/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_CHAR 
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,2,dims,"dset1a",H5T_NATIVE_CHAR,buf1a);
 write_dataset(file1_id,2,dims,"dset1b",H5T_NATIVE_CHAR,buf1b);

/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_SHORT 
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,2,dims,"dset2a",H5T_NATIVE_SHORT,buf2a);
 write_dataset(file1_id,2,dims,"dset2b",H5T_NATIVE_SHORT,buf2b);

/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_INT
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,2,dims,"dset3a",H5T_NATIVE_INT,buf3a);
 write_dataset(file1_id,2,dims,"dset3b",H5T_NATIVE_INT,buf3b);

/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_LONG
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,2,dims,"dset4a",H5T_NATIVE_LONG,buf4a);
 write_dataset(file1_id,2,dims,"dset4b",H5T_NATIVE_LONG,buf4b);

/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_FLOAT
 *-------------------------------------------------------------------------
 */
 write_dataset(file1_id,2,dims,"dset5a",H5T_NATIVE_FLOAT,buf5a);
 write_dataset(file1_id,2,dims,"dset5b",H5T_NATIVE_FLOAT,buf5b);

/*-------------------------------------------------------------------------
 * Check H5T_NATIVE_DOUBLE 
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset6a",H5T_NATIVE_DOUBLE,buf6a);
 write_dataset(file1_id,2,dims,"dset6b",H5T_NATIVE_DOUBLE,buf6b);

/*-------------------------------------------------------------------------
 * H5T_NATIVE_CHAR and H5T_NATIVE_UCHAR
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset7a",H5T_NATIVE_CHAR,buf7a);
 write_dataset(file1_id,2,dims,"dset7b",H5T_NATIVE_UCHAR,buf7b);

/*-------------------------------------------------------------------------
 * H5T_NATIVE_LLONG
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset8a",H5T_NATIVE_LLONG,buf8a);
 write_dataset(file1_id,2,dims,"dset8b",H5T_NATIVE_LLONG,buf8b);

/*-------------------------------------------------------------------------
 * H5T_NATIVE_ULLONG
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset9a",H5T_NATIVE_ULLONG,buf9a);
 write_dataset(file1_id,2,dims,"dset9b",H5T_NATIVE_ULLONG,buf9b);

/*-------------------------------------------------------------------------
 * H5T_NATIVE_INT
 *-------------------------------------------------------------------------
 */

 write_dataset(file1_id,2,dims,"dset10a",H5T_NATIVE_UINT,buf10a);
 write_dataset(file1_id,2,dims,"dset10b",H5T_NATIVE_UINT,buf10b);


/*-------------------------------------------------------------------------
 * Close 
 *-------------------------------------------------------------------------
 */
 status = H5Fclose(file1_id);
 return status;
}

