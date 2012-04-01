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
 * Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Friday, March 30, 2012
 *
 * Purpose:	Create a simple file for use with the file image tests.
 *
 */
#include "hdf5.h"

#define TESTFILE   "file_image_core_test.h5"

/* 2-D dataset with fixed dimensions */
#define SPACE_RANK	2
#define SPACE_DIM1	128
#define SPACE_DIM2	32

/* Dataset data */
int data[SPACE_DIM1][SPACE_DIM2];


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 30, 2012
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset;
    hsize_t	dims[SPACE_RANK] = {SPACE_DIM1, SPACE_DIM2};
    size_t      i, j;            /* Local index variables */

    /* Initialize the data */
    for(i = 0; i < SPACE_DIM1; i++)
        for(j = 0; j < SPACE_DIM2; j++)
            data[i][j] = (int)(j % 5);

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file < 0)
        printf("file < 0!\n");

    /* Create the dataspace */
    space = H5Screate_simple(SPACE_RANK, dims, NULL);
    if(space < 0)
        printf("space < 0!\n");

    /* Create the compressed dataset */
    dset = H5Dcreate2(file, "Dataset1", H5T_NATIVE_INT, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset < 0)
        printf("dset < 0!\n");

    /* Write the data to the dataset */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        printf("H5Dwrite() failed!\n");

    /* Close everything */
    if(H5Dclose(dset) < 0)
        printf("H5Dclose() failed!\n");
    if(H5Sclose(space) < 0)
        printf("H5Sclose() failed!\n");
    if(H5Fclose(file) < 0)
        printf("H5Fclose() failed!\n");

    return 0;
}

