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

/*
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              June 29, 2004
 *
 * Purpose:     Tests the "H5Fget_name" functionality
 */

#include "testhdf5.h"

#define FILENAME "get_file_name"
#define GROUPNAME "group"
#define DSETNAME "dataset"
#define ATTRNAME "attribute"
#define DTYPENAME "compound"
#define NAME_BUF_SIZE   64

#define RANK 2
#define NX 4
#define NY 5

/* Compound datatype */
typedef struct s1_t {
    unsigned int a;
    float        b;
} s1_t;

/* Used to make certain a return name _is_ the file name */
#define VERIFY_NAME(x, val, where) do {					      \
    if (GetTestVerbosity()>=VERBO_HI) {				              \
	print_func("   Call to routine: %15s at line %4d in %s had value "    \
		   "%ld \n", (where), (int)__LINE__, __FILE__, (long)(x));    \
    }									      \
    if (strcmp(x, val)) {					              \
	TestErrPrintf("*** UNEXPECTED VALUE from %s should be %s, but is %s at line %4d " \
		   "in %s\n", where, val, x, (int)__LINE__, __FILE__);        \
	H5Eprint_stack(H5E_DEFAULT, stdout);				      \
    }									      \
} while(0)

int main( void )
{
    char    filename[NAME_BUF_SIZE];
    hid_t   fapl;
    hid_t   file_id;
    hid_t   group_id;
    hid_t   dataset_id;
    hid_t   space_id;  
    hid_t   type_id;
    hid_t   attr_id;
    hsize_t dims[RANK] = {NX, NY};
    char    name[NAME_BUF_SIZE];
    ssize_t name_len;
    herr_t  ret;

    TESTING("H5Fget_name");
         
    /* Reset the library and get the file access property list */
    h5_reset();
    fapl = h5_fileaccess();

    /* Initialize the file names */
    h5_fixname(FILENAME, fapl, filename, sizeof filename);
     
    /* Create a new file_id using default properties. */
    file_id = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl );
    CHECK(file_id, FAIL, "H5Fcreate"); 

    /* Get and verify file name */
    name_len = H5Fget_name(file_id, name, NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_NAME(name, filename, "H5Fget_name");

    /* Create a group in the root group */
    group_id = H5Gcreate(file_id, GROUPNAME, 0);
    CHECK(group_id, FAIL, "H5Gcreate"); 

    /* Get and verify file name */
    name_len = H5Fget_name(group_id, name, NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_NAME(name, filename, "H5Fget_name");

    /* Create the data space  */
    space_id = H5Screate_simple(RANK, dims, NULL);
    CHECK(space_id, FAIL, "H5Screate_simple"); 

    /* Try get file name from data space.  Supposed to fail because 
     * it's illegal operation. */
    H5E_BEGIN_TRY {
        name_len = H5Fget_name(space_id, name, NAME_BUF_SIZE);
    } H5E_END_TRY;
    VERIFY(name_len, FAIL, "H5Fget_name");

    /* Create a new dataset */
    dataset_id = H5Dcreate(file_id, DSETNAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT);
    CHECK(dataset_id, FAIL, "H5Dcreate"); 

    /* Get and verify file name */
    name_len = H5Fget_name(dataset_id, name, NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_NAME(name, filename, "H5Fget_name");

    /* Create an attribute for the dataset */
    attr_id = H5Acreate(dataset_id,ATTRNAME,H5T_NATIVE_INT,space_id,H5P_DEFAULT);
    CHECK(attr_id, FAIL, "H5Acreate");

    /* Get and verify file name */
    name_len = H5Fget_name(attr_id, name, NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_NAME(name, filename, "H5Fget_name");

    /* Create a compound datatype */
    type_id = H5Tcreate(H5T_COMPOUND, sizeof(s1_t));
    CHECK(type_id, FAIL, "H5Tcreate"); 

    /* Insert fields */
    ret = H5Tinsert (type_id, "a", HOFFSET(s1_t,a), H5T_NATIVE_INT);
    CHECK(ret, FAIL, "H5Tinsert"); 

    ret = H5Tinsert (type_id, "b", HOFFSET(s1_t,b), H5T_NATIVE_FLOAT);
    CHECK(ret, FAIL, "H5Tinsert"); 
  
    /* Save it on file */
    ret = H5Tcommit(file_id, DTYPENAME, type_id);
    CHECK(ret, FAIL, "H5Tcommit"); 

    /* Get and verify file name */
    name_len = H5Fget_name(type_id, name, NAME_BUF_SIZE);
    CHECK(name_len, FAIL, "H5Fget_name");
    VERIFY_NAME(name, filename, "H5Fget_name");

    H5Tclose(type_id);
    H5Aclose(attr_id);
    H5Dclose(dataset_id);
    H5Sclose(space_id);
    H5Gclose(group_id);
    H5Fclose(file_id);

    PASSED();
    return 0;
}
