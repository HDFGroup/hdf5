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
 *              Thursday, November 14, 2002
 *
 * Purpose:	Create a dataset compressed with the deflate filter.
 *	This program is used to create the test file `tdeflate.h5' which has
 *      a dataset compressed with the "deflate" I/O filter.  This dataset will
 *      be used to verify the correct behavior of the library when a filter is
 *      not available for a dataset which requires it.
 */
#include "hdf5.h"

#define TESTFILE   "deflate.h5"

/* 2-D dataset with fixed dimensions */
#define SPACE_RANK	2
#define SPACE_DIM1	100
#define SPACE_DIM2	200
#define CHUNK_DIM1	50
#define CHUNK_DIM2	50

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
 *              Thursday, November 14, 2002
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	file, space, dset, dcpl;
    hsize_t	dims[SPACE_RANK]={SPACE_DIM1,SPACE_DIM2};
    hsize_t	chunk_dims[SPACE_RANK]={CHUNK_DIM1,CHUNK_DIM2};
    size_t      i,j;            /* Local index variables */

    /* Initialize the data */
    /* (Try for something easily compressible) */
    for(i=0; i<SPACE_DIM1; i++)
        for(j=0; j<SPACE_DIM2; j++)
            data[i][j] = (int)(j % 5);

    /* Create the file */
    file = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if(file<0)
        printf("file<0!\n");

    /* Create the dataspace */
    space = H5Screate_simple(SPACE_RANK, dims, NULL);
    if(space<0)
        printf("space<0!\n");

    /* Create the dataset creation property list */
    dcpl = H5Pcreate(H5P_DATASET_CREATE);
    if(dcpl<0)
        printf("dcpl<0!\n");

    /* Set up for deflated data */
    if(H5Pset_chunk(dcpl, 2, chunk_dims)<0)
        printf("H5Pset_chunk() failed!\n");
    if(H5Pset_deflate(dcpl, 9)<0)
        printf("H5Pset_deflate() failed!\n");

    /* Create the compressed dataset */
    dset = H5Dcreate2(file, "Dataset1", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    if(dset<0)
        printf("dset<0!\n");

    /* Write the data to the dataset */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data)<0)
        printf("H5Dwrite() failed!\n");

    /* Close everything */
    if(H5Pclose(dcpl)<0)
        printf("H5Pclose() failed!\n");
    if(H5Dclose(dset)<0)
        printf("H5Dclose() failed!\n");
    if(H5Sclose(space)<0)
        printf("H5Sclose() failed!\n");
    if(H5Fclose(file)<0)
        printf("H5Fclose() failed!\n");

    return 0;
}

