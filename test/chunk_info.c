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
 * Programmer:  Pedro Vicente <pvn@hdfgroup.edu>
 *              April 7, 2008
 *
 * Purpose:     Tests chunk query API functions
 */

#include "h5test.h"

 /* #define PRINT_DATA
 */ 
#define H5FILE_NAME "chunk_info.h5"
#define DATASETNAME "2d"
#define RANK         2
#define FILENAME_BUF_SIZE       1024

/* Parameters for testing chunk querying */
#define FILENAME                "tchunk_info"
#define DSET_SIMPLE_CHUNKED     "Chunked Dataset"
#define DSET_EMPTY              "Empty Dataset"
#define NX          16
#define NY          16
#define CHUNK_NX     4
#define CHUNK_NY     4
#define CHUNK_SIZE  64
#define NUM_CHUNKS_WRITTEN  4

void reinit_vars(unsigned *read_filter_mask, haddr_t *addr, hsize_t *size);

/*-------------------------------------------------------------------------
 * Function:    read_each_chunk (helper function)
 *
 * Purpose:     Reads the chunk specified by its offset and verifies that
 *              it contains the same data as what was written.  This function
 *              is used in test_get_chunk_info.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        September 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t read_each_chunk(hid_t dset_id, hsize_t offset1, hsize_t offset2, void *direct_buf)
{
    int      read_buf[CHUNK_NX][CHUNK_NY];
    hsize_t  offset[2] = {offset1, offset2};
    unsigned read_filter_mask = 0;

    /* Read the raw chunk back */
    HDmemset(&read_buf, 0, sizeof(read_buf));

    /* Read the chunk specified by its offset */
    if (H5Dread_chunk(dset_id, H5P_DEFAULT, offset, &read_filter_mask, read_buf) < 0)
        return(FAIL);

    /* Verify that read chunk is the same as the corresponding written one */
    if (HDmemcmp(direct_buf, read_buf, CHUNK_NX*CHUNK_NY) != 0)
    {
        HDfprintf(stderr, "Read chunk differs from written chunk at offset (%d,%d)\n", offset1, offset2);
        return(FAIL);
    }

    return(SUCCEED);
}

/*-------------------------------------------------------------------------
 * Function:    reinit_vars (helper function)
 *
 * Purpose:     Helper function to wipe out variables for the next use,
 *              used in test_get_chunk_info.
 *
 * Return:      Won't fail
 *
 * Date:        September 2018
 *
 *-------------------------------------------------------------------------
 */
void reinit_vars(unsigned *read_filter_mask, haddr_t *addr, hsize_t *size)
{
    if (read_filter_mask)
        *read_filter_mask = 0;
    if (addr)
        *addr = 0;
    if (size)
        *size = 0;
}

/*-------------------------------------------------------------------------
 * Function:	test_get_chunk_info
 *
 * Purpose:	    Test getting various chunk information
 *
 * Return:	    Success:	0
 *		        Failure:	1
 *
 * Description:
 *              This function tests the new API functions added for EED-343:
 *              H5Dget_num_chunks, H5Dget_chunk_info, and H5Dget_chunk_info_by_coord.
 *
 * Date:        September 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_chunk_info(void)
{
    hid_t       chunkfile = -1, fapl = -1;
    hid_t       fspace = -1, dset = -1;
    hid_t       mspace = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    int         fillvalue = -1;
    char        filename[FILENAME_BUF_SIZE];
    unsigned    filter_mask = 0;
    int         direct_buf[16][CHUNK_NX][CHUNK_NY];
    int         out_buf[NX][NY];
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int         aggression = 9;         /* Compression aggression setting */
    unsigned    read_filter_mask = 0;   /* filter mask after direct read */
    H5F_libver_t low, high;             /* File format bounds */
    hsize_t offset[2];
    hsize_t out_offset[2] = {0, 0};
    hsize_t size = 0;
    hsize_t nchunks = 0;
    haddr_t addr = 0;
    hsize_t index = 0;
    hsize_t i, j;
    int     n; /* for use on buffer, to avoid conversion warning */

    TESTING("getting chunk information");

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR

    /* Set high bound to V18 */
    low = H5F_LIBVER_EARLIEST;
    high = H5F_LIBVER_V18;
    if (H5Pset_libver_bounds(fapl, low, high) < 0)
        TEST_ERROR;

    /* Create a file */
    h5_fixname(FILENAME, fapl, filename, sizeof filename);
    if((chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Create the data space */
    if((fspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    if((mspace = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR

    /* Modify dataset creation properties, i.e. enable chunking and compression */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    if(H5Pset_deflate(cparms, (unsigned ) aggression) < 0)
        TEST_ERROR

    if (H5Pset_fill_value(cparms, H5T_NATIVE_INT, &fillvalue) < 0)
        TEST_ERROR;

    /* Create a new dataset using cparms creation properties */
    if((dset = H5Dcreate2(chunkfile, DSET_SIMPLE_CHUNKED, H5T_NATIVE_INT, fspace,
            H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0) TEST_ERROR

    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR

    /* Indicate skipping the compression filter.     */
    filter_mask = 0x00000001;

    /* Initialize the array of chunk data, that is for all 16 chunks */
    for(n = 0; n < 16; n++)
        for(i = 0; i < CHUNK_NX; i++)
            for(j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write NUM_CHUNKS_WRITTEN chunks: (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
        {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            if (H5Dwrite_chunk(dset, dxpl, filter_mask, offset, buf_size, (void*)direct_buf[n]) < 0)
                TEST_ERROR
        }

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_SIMPLE_CHUNKED, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, mspace, &nchunks) < 0) TEST_ERROR;
    if (nchunks != NUM_CHUNKS_WRITTEN) TEST_ERROR;

    /* Read the entire dataset back */
    if(H5Dread(dset, H5T_NATIVE_INT, fspace, fspace, H5P_DEFAULT, out_buf) < 0)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;
    if (out_offset[0] != 0 || out_offset[1] != 8) TEST_ERROR;

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;
    if (out_offset[0] != 0 || out_offset[1] != 12) TEST_ERROR;

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
 
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;
    if (out_offset[0] != 4 || out_offset[1] != 8) TEST_ERROR;

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;
    if (out_offset[0] != 4 || out_offset[1] != 12) TEST_ERROR;

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF) TEST_ERROR;
    if (size != 0) TEST_ERROR;

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_filter_mask, &addr, &size) < 0) TEST_ERROR;
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_filter_mask, &addr, &size) < 0) TEST_ERROR;
    if (read_filter_mask != filter_mask) TEST_ERROR;
    if (size != CHUNK_SIZE) TEST_ERROR;

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_filter_mask, &addr, &size) < 0) TEST_ERROR;
    if (addr != HADDR_UNDEF) TEST_ERROR;
    if (size != 0) TEST_ERROR;

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_filter_mask, &addr, &size) < 0) TEST_ERROR;
    if (addr != HADDR_UNDEF) TEST_ERROR;
    if (size != 0) TEST_ERROR;

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i*CHUNK_NX, j*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Close the first dataset */
    if (H5Dclose(dset) < 0) TEST_ERROR

    /* Create an empty dataset */
    if((dset = H5Dcreate2(chunkfile, DSET_EMPTY, H5T_NATIVE_INT, fspace,
            H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0) TEST_ERROR
    if (H5Dclose(dset) < 0) TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if((dset = H5Dopen2(chunkfile, DSET_EMPTY, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify that the number of chunks is 0 */
    if (H5Dget_num_chunks(dset, mspace, &nchunks) < 0) TEST_ERROR;
    if (nchunks != 0) TEST_ERROR;

    /* Attempt to get info of a chunk from an empty dataset, verify the
       returned address and size */
    index = 0;
    reinit_vars(&read_filter_mask, &addr, &size);
    if (H5Dget_chunk_info(dset, fspace, index, out_offset, &read_filter_mask, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF) TEST_ERROR;
    if (size != 0) TEST_ERROR;

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_filter_mask, &addr, &size) < 0) TEST_ERROR;
    if (addr != HADDR_UNDEF) TEST_ERROR;
    if (size != 0) TEST_ERROR;

    /* Close/release resources. */
    if (H5Sclose(mspace) < 0) TEST_ERROR
    if (H5Sclose(fspace) < 0) TEST_ERROR
    if (H5Pclose(cparms) < 0) TEST_ERROR
    if (H5Pclose(dxpl) < 0) TEST_ERROR
    if (H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(mspace);
        H5Sclose(fspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_get_chunk_info() */

/*-------------------------------------------------------------------------
 * Function:    create_4x4_dset
 *
 * Purpose:     Tests functions related to chunk information
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Description:
 *              The code in this create_4x4_dset function was originally written by
 *              Pedro in main() that writes a 4x4 dataset by iterating on
 *              2x2 chunks at a time, with the intention of making a frame
 *              work to test H5Dget_chunk_info, which was not in the library,
 *              until now.  For the work in EED-343, the test function
 *              test_get_chunk_info was added to test the new query chunk
 *              API functions: H5Dget_num_chunk, H5Dget_chunk_info, and
 *              H5Dget_chunk_info_by_coord.  This code can be used at a
 *              later time, so it is kept here.
 *              -BMR, November 5, 2018
 *
 * Programmer:  Pedro Vicente <pvn@hdfgroup.edu>
 *              April 7, 2008
 *
 *-------------------------------------------------------------------------
 */
int
create_4x4_dset(void)
{
    hid_t   fid;      /* file ID */
    hid_t   did;      /* dataset ID */
    hid_t   f_sid;    /* file space ID */
    hid_t   m_sid;    /* memory space ID */
    hid_t   pid;      /* property list ID */
    hsize_t start[2]; /* chunk location to start writing */
    hsize_t dims[2]  = { 4, 4};
    hsize_t chunk_dims[2] = { 2, 2 };
    int     chunk_data[2][2] = { {1, 1}, {1, 1} };
    int     buf[4][4];
    int     fillvalue = 0;
    hsize_t i, j;

    /* create a new file using default properties. */
    if ((fid = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* create the file space */
    if ((f_sid = H5Screate_simple(RANK, dims, dims)) < 0) TEST_ERROR;

    /* create the memory space with chunk dimensions */
    if ((m_sid = H5Screate_simple(RANK, chunk_dims, chunk_dims)) < 0) TEST_ERROR;
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(m_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0) TEST_ERROR;

   /*-------------------------------------------------------------------------
    * create a dataset
    *-------------------------------------------------------------------------
    */

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((pid = H5Pcreate (H5P_DATASET_CREATE)) < 0) TEST_ERROR;
    if (H5Pset_chunk(pid, RANK, chunk_dims) < 0) TEST_ERROR;
    if (H5Pset_fill_value(pid, H5T_NATIVE_INT, &fillvalue) < 0) TEST_ERROR;

    /* create a new dataset */
    if((did = H5Dcreate2(fid , DATASETNAME, H5T_NATIVE_INT, f_sid, H5P_DEFAULT, pid, H5P_DEFAULT)) < 0) TEST_ERROR;


   /*-------------------------------------------------------------------------
    * write the dataset in 2x2 chunks
    *-------------------------------------------------------------------------
    */

    /* iterate in dim 0 */
    for (j = 0; j < chunk_dims[0]; j++) {
        /* reset start in dim 1 */
        start[1] = 0;

        /* iterate in dim 1 */
        for (i = 0; i < chunk_dims[1]; i++) {
            /* select file hyperslab to save a 2x2 chunk */
            if (H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0) TEST_ERROR;

            /* write the data to the hyperslab. */
            if (H5Dwrite(did, H5T_NATIVE_INT, m_sid, f_sid, H5P_DEFAULT, chunk_data) < 0) TEST_ERROR;

            /* read back and display complete dataset 4x4 */
            if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) TEST_ERROR;

#if defined (PRINT_DATA)
            printf("\n");
            printf("dataset: \n");
            for (jj = 0; jj < dims[0]; jj++) {
                for (ii = 0; ii < dims[1]; ii++) printf("%d ", buf[jj][ii]);
                printf("\n");
            }
#endif
            /* increment start in dim 1 */
            start[1] += 2;
        }
        /* increment start in dim 0 */
        start[0] += 2;
    }

   /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */
    if (H5Dclose(did) < 0) TEST_ERROR
    if (H5Sclose(f_sid) < 0) TEST_ERROR
    if (H5Sclose(m_sid) < 0) TEST_ERROR
    if (H5Pclose(pid) < 0) TEST_ERROR
    if (H5Fclose(fid) < 0) TEST_ERROR

    PASSED();

    return SUCCEED;

/* this will be removed once the existing code is moved out of main */
error:
    H5Dclose(did);
    H5Sclose(f_sid);
    H5Sclose(m_sid);
    H5Pclose(pid);
    H5Fclose(fid);

    return FAIL;
} /* end create_4x4_dset */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests functions related to chunk information
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Programmer:  Binh-Minh Ribler
 *              November 5, 2018
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int     nerrors = 0;

    /* Tests getting chunk information */
    nerrors += (test_get_chunk_info() < 0 ? 1 : 0);

    /* Create a 4x4 dataset (using the existing code to avoid compilation
       warnings for now) */
    nerrors += (create_4x4_dset() < 0 ? 1 : 0);

    if(nerrors) {
        goto error;

    } /* end if */

    HDprintf("All chunk query tests passed.\n");

    return SUCCEED;

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d QUERY CHUNK INFO TEST%s FAILED! *****\n",
              nerrors, 1 == nerrors ? "" : "S");
    return FAIL;
}



