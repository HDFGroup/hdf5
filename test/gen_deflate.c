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
 *              Thursday, November 14, 2002
 *
 * Purpose:	    Create a dataset compressed with the deflate filter.
 *              This program is used to create the test file `tdeflate.h5'
 *              which has a dataset compressed with the "deflate" I/O filter.
 *              This dataset will be used to verify the correct behavior of
 *              the library when a filter is not available for a dataset which
 *              requires it.
 */

#include "h5test.h"

#define TESTFILE   "deflate.h5"

/* 2-D dataset with fixed dimensions */
#define SPACE_RANK	2
#define SPACE_DIM1	100
#define SPACE_DIM2	200
#define CHUNK_DIM1	50
#define CHUNK_DIM2	50



/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 14, 2002
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t       fid = -1, sid = -1, did = -1, dcpl_id = -1;
    hsize_t     dims[SPACE_RANK] = {SPACE_DIM1, SPACE_DIM2};
    hsize_t	    chunk_dims[SPACE_RANK] = {CHUNK_DIM1, CHUNK_DIM2};
    size_t      i,j;                    /* Local index variables */
    int        *data = NULL;            /* Dataset data */

    /* Initialize the data */
    /* (Try for something easily compressible) */
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

    /* Create the dataset creation property list */
    if((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    /* Set up for deflated data */
    if(H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_deflate(dcpl_id, 9) < 0)
        FAIL_STACK_ERROR

    /* Create the compressed dataset */
    if((did = H5Dcreate2(fid, "Dataset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Write the data to the dataset */
    if(H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        FAIL_STACK_ERROR

    /* Close everything */
    if(H5Pclose(dcpl_id) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(did) < 0)
        FAIL_STACK_ERROR
    if(H5Sclose(sid) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0)
        FAIL_STACK_ERROR

    HDfree(data);

    return EXIT_SUCCESS;

error:
    if(data)
        HDfree(data);
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Dclose(did);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;

    return EXIT_FAILURE;
} /* end main() */

