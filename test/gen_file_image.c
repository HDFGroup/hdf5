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
 * Programmer:  Quincey Koziol
 *              Friday, March 30, 2012
 *
 * Purpose:     Create a simple file for use with the file image tests.
 *
 */
#include "h5test.h"

#define TESTFILE   "file_image_core_test.h5"

/* 2-D dataset with fixed dimensions */
#define SPACE_RANK	2
#define SPACE_DIM1	128
#define SPACE_DIM2	32


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:	Quincey Koziol
 *              Friday, March 30, 2012
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t   fid = -1, sid = -1, did = -1;
    hsize_t	dims[SPACE_RANK] = {SPACE_DIM1, SPACE_DIM2};
    size_t      i,j;                    /* Local index variables */
    int        *data = NULL;            /* Dataset data */

    /* Initialize the data */
    if(NULL == (data = (int *)HDmalloc(SPACE_DIM1 * SPACE_DIM2 * sizeof(int))))
        TEST_ERROR

    for(i = 0; i < SPACE_DIM1; i++)
        for(j = 0; j < SPACE_DIM2; j++)
            data[(i * SPACE_DIM2) + j] = (int)(j % 5);

    /* Create the file */
    if((fid = H5Fcreate(TESTFILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create the dataspace */
    if((sid = H5Screate_simple(SPACE_RANK, dims, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create the compressed dataset */
    if((did = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write the data to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR

    /* Close everything */
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    return EXIT_SUCCESS;

error:
    if(data)
        HDfree(data);
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;

    return EXIT_FAILURE;
} /* end main() */

