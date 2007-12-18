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
 * Programmer:  Raymond Lu <slu@hdfgroup.org>
 *              19 November 2007
 */

#include "h5test.h"

#define STR_LEN 16
#define NX	10u
#define NY	20u
#define MX	100u
#define MY	200u
#define CPTR(VAR,CONST)	((VAR)=(CONST),&(VAR))

const char *FILENAME[] = {
    "modify_types",
    NULL
};

const char *DSET_NAME[] = {
    "integer",
    "float1",
    "float2",
    "string",
    "vlstring",
    "bitfield",
    "opaque",
    "enum",
    "array",
    NULL
};

typedef enum {
    E1_RED,
    E1_GREEN,
    E1_BLUE,
    E1_WHITE,
    E1_BLACK
} c_e1;


/*-------------------------------------------------------------------------
 * Function:	test_integer
 *
 * Purpose:	Creates a simple dataset of an integer type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_integer(hid_t file)
{
    hid_t               dset;
    hid_t               space;
    hsize_t             dims[2] = {NX, NY};
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[0], H5T_STD_U32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_STD_I16BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_STD_I32BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_IEEE_F32BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, H5T_IEEE_F64LE) < 0)
        TEST_ERROR;
 
    if(H5Dmodify_dtype(dset, H5T_STD_I64LE) < 0)
        TEST_ERROR;
     
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
  
    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_float
 *
 * Purpose:	Creates a simple dataset of a float type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_float(hid_t file)
{
    hid_t               dset;
    hid_t               space;
    hid_t               dcpl_id;
    float               fill_value = 100.123;
    double              fill_ret;
    double              data_out[NX][NY];
    hsize_t             dims[2] = {NX, NY};
    hsize_t             chunk_dims[2] = {NX/2, NY/2};
    unsigned int        i, j;
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if(H5Pset_fill_value(dcpl_id, H5T_NATIVE_FLOAT, &fill_value) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[1], H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_STD_U32BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;

    if(H5Pget_fill_value(dcpl_id, H5T_NATIVE_DOUBLE, &fill_ret) < 0)
        TEST_ERROR;

    if(!DBL_ABS_EQUAL(fill_ret, fill_value))
        TEST_ERROR; 

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    /* Create a chunk dataset with fill value and space allocation early.
     * Enable the filter for modifying dataset's datatype.  Verify the correctness
     * of the fill value. */ 
    if(H5Pset_dtype_modifiable(dcpl_id)<0) 
        TEST_ERROR;

    /* Set chunking */
    if(H5Pset_chunk(dcpl_id, 2, chunk_dims)<0)
        TEST_ERROR;

    /* Set space allocation early */
    if(H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[2], H5T_NATIVE_FLOAT, space, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Change dataset's type */
    if(H5Dmodify_dtype(dset, H5T_NATIVE_DOUBLE) < 0)
        TEST_ERROR;

    /* Verify fill value is still correct */
    if(H5Pget_fill_value(dcpl_id, H5T_NATIVE_DOUBLE, &fill_ret) < 0)
        TEST_ERROR;

    if(!DBL_ABS_EQUAL(fill_ret, fill_value))
        TEST_ERROR; 

    if(H5Dread(dset, H5T_NATIVE_DOUBLE, space, H5S_ALL, H5P_DEFAULT, data_out) < 0)
        TEST_ERROR;

    for(i=0; i<NX; i++) {
        for(j=0; j<NY; j++) {
            if(!DBL_ABS_EQUAL(fill_ret, data_out[i][j]))
                TEST_ERROR; 
        }
    }   

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Pclose(dcpl_id) < 0)
        TEST_ERROR;

    if(H5Sclose(space) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Pclose(dcpl_id);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_string
 *
 * Purpose:	Creates a simple dataset of a string type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_string(hid_t file)
{
    hid_t               dset;
    hid_t               space, type1, type2;
    hsize_t             dims[2] = {NX, NY};
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* create a fix-sized string datatype */
    if((type1 = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if(H5Tset_size(type1, STR_LEN) < 0)
        TEST_ERROR;

    if(H5Tset_strpad(type1, H5T_STR_NULLTERM) < 0)
        TEST_ERROR;

    /* Create a dataset of fix-sized string */
    if((dset=H5Dcreate2(file, DSET_NAME[3], type1, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    if(H5Tset_size(type1, STR_LEN - 1) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, type1);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Tset_size(type1, STR_LEN + 4) < 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, type1) < 0)
        TEST_ERROR;


    if(H5Tset_size(type1, H5T_VARIABLE) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, type1);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Dclose(dset) < 0)
        TEST_ERROR;

 
    /* Create a dataset of vl string */
    if((dset=H5Dcreate2(file, DSET_NAME[4], type1, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a fix-size string datatype */
    if((type2 = H5Tcopy(H5T_C_S1)) < 0)
        TEST_ERROR;

    if(H5Tset_size(type2, STR_LEN) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, type2);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Tset_size(type2, H5T_VARIABLE) < 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, type2) < 0)
        TEST_ERROR;

    if(H5Tclose(type1) < 0)
        TEST_ERROR;

    if(H5Tclose(type2) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Tclose(type1);
        H5Tclose(type2);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_bitfield
 *
 * Purpose:	Creates a simple dataset of a bitfield type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_bitfield(hid_t file)
{
    hid_t               dset;
    hid_t               space;
    hsize_t             dims[2] = {NX, NY};
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[5], H5T_STD_B32BE, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_STD_B16BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, H5T_IEEE_F32BE);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, H5T_STD_B64LE) < 0)
        TEST_ERROR;
     
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
  
    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_opaque
 *
 * Purpose:	Creates a simple dataset of an opaque type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_opaque(hid_t file)
{
    hid_t               dset;
    hid_t               space, type, new_type;
    hsize_t             dims[2] = {NX, NY};
    const char          *tag = "this is a tag";
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if((type = H5Tcreate(H5T_OPAQUE, 4)) < 0)
        TEST_ERROR;

    if(H5Tset_tag(type, tag) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[6], type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a smaller opaque type and try to change the type to it.
     * It should fail. */
    if((new_type = H5Tcreate(H5T_OPAQUE, 2)) < 0)
        TEST_ERROR;

    if(H5Tset_tag(new_type, tag) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, new_type);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    /* Create a bigger opaque type and try to change the type to it.
     * It should succeed. */
    if((new_type = H5Tcreate(H5T_OPAQUE, 6)) < 0)
        TEST_ERROR;

    if(H5Tset_tag(new_type, tag) < 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, new_type) < 0)
        TEST_ERROR;
     
    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    if(H5Tclose(type) < 0)
        TEST_ERROR;
 
    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Tclose(new_type);
        H5Tclose(type);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_enum
 *
 * Purpose:	Creates a simple dataset of an enum type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_enum(hid_t file)
{
    hid_t               dset;
    hid_t               space, type, new_type;
    hsize_t             dims[2] = {NX, NY};
    c_e1                val1;
    int                 val2;
    short               val3;
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    if ((type = H5Tcreate(H5T_ENUM, sizeof(c_e1)))<0)  TEST_ERROR;
    if (H5Tenum_insert(type, "RED",   CPTR(val1, E1_RED  ))<0) TEST_ERROR;
    if (H5Tenum_insert(type, "GREEN", CPTR(val1, E1_GREEN))<0) TEST_ERROR;
    if (H5Tenum_insert(type, "BLUE",  CPTR(val1, E1_BLUE ))<0) TEST_ERROR;
    if (H5Tenum_insert(type, "WHITE", CPTR(val1, E1_WHITE))<0) TEST_ERROR;
    if (H5Tenum_insert(type, "BLACK", CPTR(val1, E1_BLACK))<0) TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[7], type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create a new enum type with less members and try to change the type to it.
     * It should fail. */
    if ((new_type = H5Tcreate(H5T_ENUM, sizeof(int)))<0)  TEST_ERROR;
    if (H5Tenum_insert(new_type, "RED",   CPTR(val2, 10))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "GREEN", CPTR(val2, 11))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "BLUE",  CPTR(val2, 12))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "WHITE", CPTR(val2, 13))<0) TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, new_type);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    /* Create an enum type with more members type and try to change the type to it.
     * It should succeed. */
    if ((new_type = H5Tcreate(H5T_ENUM, sizeof(short)))<0)  TEST_ERROR;
    if (H5Tenum_insert(new_type, "BLUE",  CPTR(val3,  107))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "GREEN", CPTR(val3,  106))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "RED",   CPTR(val3,  105))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "WHITE", CPTR(val3,  104))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "BLACK", CPTR(val3,  103))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "YELLOW", CPTR(val3, 102))<0) TEST_ERROR;
    if (H5Tenum_insert(new_type, "CYAN", CPTR(val3,   101))<0) TEST_ERROR;

    if(H5Dmodify_dtype(dset, new_type) < 0)
        TEST_ERROR;
     
    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    if(H5Tclose(type) < 0)
        TEST_ERROR;
 
    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Tclose(new_type);
        H5Tclose(type);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	test_array
 *
 * Purpose:	Creates a simple dataset of a named array type and tries to 
 *              change the datatype.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
static int
test_array(hid_t file)
{
    hid_t               dset;
    hid_t               space, type, new_type;
    hsize_t             dims[2] = {NX, NY};
    hsize_t		tdims[] = {4,4};
    hsize_t		new_tdims[] = {2,2};
    herr_t              ret;

    if((space=H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Make a named array datatype */
    if((type = H5Tarray_create2(H5T_NATIVE_INT, 2, tdims)) < 0)
        TEST_ERROR;

    if(H5Tcommit2(file, "array type", type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if((dset=H5Dcreate2(file, DSET_NAME[8], type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create an array type with shorter dimensionality and try to change the type to it.
     * It should fail. */
    if((new_type = H5Tarray_create2(H5T_NATIVE_INT, 2, new_tdims)) < 0)
        TEST_ERROR;

    H5E_BEGIN_TRY {
        ret = H5Dmodify_dtype(dset, new_type);
    } H5E_END_TRY;

    if(ret > 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    /* Create a named array type of long int and try to change the type to it.
     * It should succeed. */
    if((new_type = H5Tarray_create2(H5T_NATIVE_LONG, 2, tdims)) < 0)
        TEST_ERROR;

    if(H5Tcommit2(file, "new array type", new_type, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT) < 0)
        TEST_ERROR;

    if(H5Dmodify_dtype(dset, new_type) < 0)
        TEST_ERROR;
     
    if(H5Dclose(dset) < 0)
        TEST_ERROR;

    if(H5Tclose(new_type) < 0)
        TEST_ERROR;

    if(H5Tclose(type) < 0)
        TEST_ERROR;
 
    PASSED();
    return 0;

error:
    H5_FAILED();
    H5E_BEGIN_TRY {
        H5Sclose(space);
        H5Dclose(dset);
        H5Tclose(new_type);
        H5Tclose(type);
    } H5E_END_TRY;
    return 1;
}


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test H5Dmodify_dtype for different kinds of datatypes.
 *              The test for compound type is in cmpd_dset.c.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Raymond Lu
 *              19 November 2007 
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    hid_t	fid, fapl_id;
    char	filename[256];
    unsigned 	nerrors = 0;

    h5_reset();

    /* Create the file */
    fapl_id = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl_id, filename, sizeof(filename));

    if ((fid = H5Fcreate (filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0)
	nerrors++;

    TESTING("dataset of integer type:");
    nerrors += test_integer(fid);

    TESTING("dataset of floating number:");
    nerrors += test_float(fid);

    TESTING("dataset of string type:");
    nerrors += test_string(fid);

    TESTING("dataset of bitfield type:");
    nerrors += test_bitfield(fid);

    TESTING("dataset of opaque type:");
    nerrors += test_opaque(fid);

    TESTING("dataset of enum type:");
    nerrors += test_enum(fid);

    TESTING("dataset of array type:");
    nerrors += test_array(fid);

    if(H5Fclose(fid) < 0)
	nerrors++;

    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }

    h5_cleanup(FILENAME, fapl_id);
    puts("All tests for changing dataset's datatype passed.");

    return 0;
}
