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

#include "h5hltest.h"
#include "H5DOpublic.h"

/* This test is a minimal test to ensure that the H5DO compatibility wrappers
 * work correctly.
 */

#ifndef H5_NO_DEPRECATED_SYMBOLS

#define FILE_NAME       "h5do_compat.h5"
#define DATASET_NAME    "direct_chunk_io"

#define NX              8
#define CHUNK_NX        4


/*-------------------------------------------------------------------------
 * Function:	test_direct_chunk_write
 *
 * Purpose:	    Test the basic functionality of H5DOwrite_chunk
 *
 * Return:	    Success:    An identifer for the dataset used in the tests
 *		        Failure:	H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
static hid_t
create_dataset(hid_t fid)
{
    hid_t   did             = H5I_INVALID_HID;
    hid_t   sid             = H5I_INVALID_HID;
    hid_t   dcpl_id         = H5I_INVALID_HID;
    hsize_t dims[1]         = {NX};
    hsize_t maxdims[1]      = {H5S_UNLIMITED};
    hsize_t chunk_dims[1]   = {CHUNK_NX};
    int     data[NX];
    int     i;

    /* Create a dataspace for the new dataset */
    if ((sid = H5Screate_simple(1, dims, maxdims)) < 0)
        goto error;

    /* Set up dataset creation parameters */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        goto error;
    if (H5Pset_chunk(dcpl_id, 1, chunk_dims) < 0)
        goto error;

    /* Create a new dataset */
    if ((did = H5Dcreate2(fid, DATASET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0)
        goto error;

    /* Initialize the data */
    for (i = 0; i < NX; i++)
	    data[i] = i;

    /* Write the initialized data */
    if (H5Dwrite(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data) < 0)
        goto error;

    /* Close everything */
    if (H5Sclose(sid) < 0)
        goto error;
    if (H5Pclose(dcpl_id) < 0)
        goto error;

    return did;

 error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
        H5Sclose(sid);
        H5Pclose(dcpl_id);
    } H5E_END_TRY;

    return H5I_INVALID_HID;

} /* end create_dataset() */



/*-------------------------------------------------------------------------
 * Function:	test_direct_chunk_write
 *
 * Purpose:	    Test the basic functionality of H5DOwrite_chunk
 *
 * Return:	    Success:    0
 *		        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static int
test_direct_chunk_write(hid_t did)
{
    unsigned    filter_mask = 0;
    int         chunk_data[CHUNK_NX];
    hsize_t     offset[1];
    size_t      data_size;
    int         i;

    TESTING("H5DOwrite_chunk wrapper");

    /* Set the size of the chunk data */
    data_size = CHUNK_NX * sizeof(int);

    /* Initialize the chunk data */
    for (i = 0; i < CHUNK_NX; i++)
        chunk_data[i] = (i * 10) + i;

    /* Write the direct chunk data repeatedly to cover all the chunks in the 
     * dataset, using the direct writing function.
     */ 
    offset[0] = 0;
    for (i = 0; i < NX/CHUNK_NX; i++) {
        if (H5DOwrite_chunk(did, H5P_DEFAULT, filter_mask, offset, data_size, chunk_data) < 0)
            TEST_ERROR
        offset[0] += CHUNK_NX;
    }

    PASSED();
    return 0;

error:
    H5_FAILED();
    return 1;
} /* test_direct_chunk_write() */


/*-------------------------------------------------------------------------
 * Function:	test_direct_chunk_read
 *
 * Purpose:	    Test the basic functionality of H5DOread_chunk
 *
 * Return:	    Success:    0
 *		        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
static int
test_direct_chunk_read(hid_t did)
{
    hid_t       mem_sid         = H5I_INVALID_HID;
    hid_t       file_sid        = H5I_INVALID_HID;
    hsize_t     dims[1]         = {NX};
    hsize_t     chunk_dims[1]   = {CHUNK_NX};

    unsigned    filter_mask;
    int         chunk_data[CHUNK_NX];   /* Chunk read with H5DOread_chunk */
    int         check[CHUNK_NX];        /* Chunk read with H5Dread */
    hsize_t     offset[1];

    hsize_t     start[1];  /* Start of hyperslab */
    hsize_t     stride[1]; /* Stride of hyperslab */
    hsize_t     count[1];  /* Block count */
    hsize_t     block[1];  /* Block sizes */

    int         i,j;

    TESTING("H5DOread_chunk wrapper");

    /* Create dataspaces for reading */
    if ((mem_sid = H5Screate_simple(1, chunk_dims, NULL)) < 0)
        TEST_ERROR
    if ((file_sid = H5Screate_simple(1, dims, NULL)) < 0)
        TEST_ERROR

    /* For each chunk in the dataset, compare the result of H5Dread and H5DOread_chunk. */
    for (i = 0; i < NX/CHUNK_NX; i++) {

        /* Select hyperslab for one chunk in the file */
        start[0]  = (hsize_t)i * CHUNK_NX;
        stride[0] = 1;
        count[0]  = 1;
        block[0]  = CHUNK_NX;

        /* Hyperslab selection equals single chunk */
        if (H5Sselect_hyperslab(file_sid, H5S_SELECT_SET, start, stride, count, block) < 0)
            TEST_ERROR

        /* Read the chunk back */
        if (H5Dread(did, H5T_NATIVE_INT, mem_sid, file_sid, H5P_DEFAULT, check) < 0)
            TEST_ERROR

        /* Read the raw chunk back */
        HDmemset(chunk_data, 0, CHUNK_NX * sizeof(int));
        filter_mask = UINT_MAX;
        offset[0] = (hsize_t)i * CHUNK_NX;
        if (H5DOread_chunk(did, H5P_DEFAULT, offset, &filter_mask, chunk_data) < 0)
            TEST_ERROR

        /* Check filter mask return value */
        if (filter_mask != 0)
            TEST_ERROR

        /* Check that the values are correct */
        for (j = 0; j < CHUNK_NX; j++)
            if (chunk_data[i] != check[i])
                TEST_ERROR
    }

    /* Close */
    if (H5Sclose(mem_sid) < 0)
        TEST_ERROR
    if (H5Sclose(file_sid) < 0)
        TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(mem_sid);
        H5Sclose(file_sid);
    } H5E_END_TRY;

    H5_FAILED();
    return 1;
} /* test_direct_chunk_read() */

#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	    Test direct chunk write function H5DOwrite_chunk and
 *              chunk direct read function H5DOread_chunk
 *
 * Return:	    Success:    0
 *		        Failure:    1
 *
 *-------------------------------------------------------------------------
 */
int main( void )
{
#ifdef H5_NO_DEPRECATED_SYMBOLS

    HDputs("Direct chunk read/write wrapper tests SKIPPED.");
    HDputs("(Backward compatibility not configured)");
    return EXIT_SUCCESS;

#else

    hid_t   fid         = H5I_INVALID_HID;
    hid_t   did         = H5I_INVALID_HID;
    int     nerrors     = 0;

    if ((fid = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    if ((did = create_dataset(fid)) < 0)
        goto error;

    nerrors += test_direct_chunk_write(did);
    nerrors += test_direct_chunk_read(did);

    if (H5Dclose(did) < 0)
        goto error;
    if (H5Fclose(fid) < 0)
        goto error;

    /* check for errors */
    if (nerrors)
        goto error;

    HDputs("All direct chunk read/write wrapper tests passed.");
    return EXIT_SUCCESS;

error:
    HDputs("*** TESTS FAILED ***");
    return EXIT_FAILURE;
#endif /* H5_NO_DEPRECATED_SYMBOLS */
} /* end main() */
