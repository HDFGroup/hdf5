/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Thursday, November 09, 2000
 *
 * Purpose:	Create a two datasets, one with a compound datatypes with array
 *      fields (which should be stored in the newer version (version 2)) and
 *      one with an array datatype.
 *		This program is used to create the test file `tarrnew.h5' which has a
 *      datatypes stored in the newer (version 2) style in the object headers.
 *		To build the test file, this program MUST be compiled and linked with
 *      the hdf5-1.3+ series of libraries and the generated test file must be
 *      put into the 'test' directory in the 1.2.x branch of the library.
 *      The test file should be generated on a little-endian machine with
 *      16-bit shorts, 32-bit floats, 32-bit ints and 64-bit doubles.
 */
#include "hdf5.h"

#define TESTFILE   "tarrnew.h5"

/* 1-D array datatype */
#define ARRAY1_RANK	1
#define ARRAY1_DIM1 4

/* 2-D dataset with fixed dimensions */
#define SPACE1_RANK	2
#define SPACE1_DIM1	8
#define SPACE1_DIM2	9


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Robb Matzke
 *              Monday, October 26, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    typedef struct {        /* Typedef for compound datatype */
        short i;
        float f[ARRAY1_DIM1];
        long l[ARRAY1_DIM1];
        double d;
    } s3_t;
    hid_t	file, space, type, arr_type, dset;
    hsize_t		tdims1[] = {ARRAY1_DIM1};
    hsize_t	cur_dim[SPACE1_RANK]={SPACE1_DIM1,SPACE1_DIM2};
    herr_t		ret;		/* Generic return value		*/

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file<0)
        printf("file<0!\n");

    /* Create the dataspace (for both datasets) */
    space = H5Screate_simple(SPACE1_RANK, cur_dim, NULL);
    if(space<0)
        printf("space<0!\n");

    /* Create the compound datatype with array fields */
    type = H5Tcreate(H5T_COMPOUND, sizeof(s3_t));
    if(type<0)
        printf("type<0!\n");

    /* Insert integer field */
    ret = H5Tinsert (type, "i", HOFFSET(s3_t,i), H5T_NATIVE_SHORT);
    if(ret<0)
        printf("field 1 insert<0!\n");

    /* Creat the array datatype */
    arr_type = H5Tarray_create2(H5T_NATIVE_FLOAT, ARRAY1_RANK, tdims1);
    if(arr_type < 0)
        printf("arr_type<0!\n");

    /* Insert float array field */
    ret = H5Tinsert (type, "f", HOFFSET(s3_t,f), arr_type);
    if(ret<0)
        printf("field 3 insert<0!\n");

    /* Close array datatype */
    ret = H5Tclose (arr_type);
    if(ret<0)
        printf("field 3 array close<0!\n");

    /* Creat the array datatype */
    arr_type = H5Tarray_create2(H5T_NATIVE_LONG, ARRAY1_RANK, tdims1);
    if(arr_type < 0)
        printf("arr_type<0!\n");

    /* Insert long array field */
    ret = H5Tinsert (type, "l", HOFFSET(s3_t,l), arr_type);
    if(ret<0)
        printf("field 3 insert<0!\n");

    /* Close array datatype */
    ret = H5Tclose (arr_type);
    if(ret<0)
        printf("field 3 array close<0!\n");

    /* Insert double field */
    ret = H5Tinsert (type, "d", HOFFSET(s3_t,d), H5T_NATIVE_DOUBLE);
    if(ret<0)
        printf("field 4 insert<0!\n");

    /* Create the dataset with compound array fields */
    dset = H5Dcreate2(file, "Dataset1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset<0)
        printf("dset<0!\n");
    H5Dclose(dset);

    /* Close compound datatype */
    H5Tclose(type);

    /* Create the compound datatype with array fields */
    type = H5Tarray_create2(H5T_NATIVE_INT, ARRAY1_RANK, tdims1);
    if(type < 0)
        printf("type<0!\n");

    /* Create the dataset with array datatype */
    dset = H5Dcreate2(file, "Dataset2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset<0)
        printf("dset<0!\n");
    H5Dclose(dset);

    /* Close array datatype */
    H5Tclose(type);

    H5Sclose(space);
    H5Fclose(file);

    return 0;
}

