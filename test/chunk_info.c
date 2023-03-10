/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/*
 *
 * Purpose:     Tests chunk query API functions
 *
 * Modification:
 *              Many tests were added for HDFFV-10615. -BMR, October 2018
 *
 * Test structure:
 *          main()
 *              test_basic_query()
 *              test_get_chunk_info_highest18()
 *              test_get_chunk_info_110()
 *                  test_chunk_info_single_chunk()
 *                  test_chunk_info_implicit()
 *                  test_chunk_info_fixed_array()
 *                  test_chunk_info_extensible_array()
 *                  test_chunk_info_version2_btrees()
 * Helper functions:
 *          verify_get_chunk_info()
 *          verify_get_chunk_info_by_coord()
 *          verify_empty_chunk_info()
 *
 */
#define H5D_FRIEND
#define H5D_TESTING /* to use H5D__ functions */
#include "H5Dpkg.h"

#include "testhdf5.h"

/* Test file names */
const char *FILENAME[] = {"tchunk_info_18", "tchunk_info_110", "chunk_info", NULL};

/* From original test */
#define DATASETNAME "2d"

#define BASIC_FILE "basic_query"

/* Parameters for testing chunk querying */
#define RANK                 2
#define FILENAME_BUF_SIZE    1024
#define DSET_SIMPLE_CHUNKED  "Chunked Dataset"
#define DSET_EMPTY           "Empty Dataset"
#define DSET_EMPTY_ALLOC     "Empty Dataset with ALLOC_TIME_EARLY"
#define DSET_SINGLE_CHUNK    "Single Chunk Index Dataset"
#define DSET_IMPLICIT_INDEX  "Implicit Index Dataset"
#define DSET_FIXED_ARR_INDEX "Fixed Array Index Dataset"
#define DSET_EXT_ARR_INDEX   "Extensible Array Index Dataset"
#define DSET_V2_BTREE_INDEX  "Version 2 B-Tree Index Dataset"
#define NX                   24
#define NY                   16
#define CHUNK_NX             6
#define CHUNK_NY             4
#define SINGLE_CHUNK_SIZE    (NX * NY * sizeof(int))
#define CHUNK_SIZE           96
#define EMPTY_CHK_SIZE       0
#define NUM_CHUNKS           16

/* Number of chunks that have been written */
#define NUM_CHUNKS_WRITTEN 4
#define ONE_CHUNK_WRITTEN  1
#define TWO_CHUNKS_WRITTEN 2
#define NO_CHUNK_WRITTEN   0

/* For testing invalid arguments */
#define NONEXIST_CHK_INDEX   3
#define OUTOFRANGE_CHK_INDEX 5
#define INVALID_CHK_INDEX    5

/* Artifact of original file, maybe removed */
/* #define PRINT_DATA */

/* Utility function to initialize arguments */
void reinit_vars(unsigned *read_flt_msk, haddr_t *addr, hsize_t *size);

/* Helper function containing common code that verifies indexing type
   and number of chunks */
static int verify_get_chunk_info(hid_t dset, hid_t dspace, hsize_t chk_index, hsize_t exp_chk_size,
                                 const hsize_t *exp_offset, unsigned exp_flt_msk);
static int verify_get_chunk_info_by_coord(hid_t dset, hsize_t *offset, hsize_t exp_chk_size,
                                          unsigned exp_flt_msk);
static int verify_empty_chunk_info(hid_t dset, hsize_t *offset);

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
// EIP - May be this function should take a pointer to an array of chunk dimensions
// EIP  and its size, so it is not restricted to 2 dims only?
static herr_t
read_each_chunk(hid_t dset_id, hsize_t offset1, hsize_t offset2, void *direct_buf)
{
    int      read_buf[CHUNK_NX][CHUNK_NY];
    hsize_t  offset[2]    = {offset1, offset2};
    unsigned read_flt_msk = 0;
    herr_t   ret; /* Return value */

    HDmemset(&read_buf, 0, sizeof(read_buf));

    /* Read the chunk specified by its offset */
    ret = H5Dread_chunk(dset_id, H5P_DEFAULT, offset, &read_flt_msk, read_buf);
    if (ret < 0)
        return (FAIL);

    /* Verify that read chunk is the same as the corresponding written one */
    if (HDmemcmp(direct_buf, read_buf, CHUNK_NX * CHUNK_NY) != 0) {
        HDfprintf(stderr, "Read chunk differs from written chunk at offset (%" PRIdHSIZE ",%" PRIdHSIZE ")\n",
                  offset1, offset2);
        return (FAIL);
    }

    return (SUCCEED);
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
void
reinit_vars(unsigned *read_flt_msk, haddr_t *addr, hsize_t *size)
{
    if (read_flt_msk)
        *read_flt_msk = 0;
    if (addr)
        *addr = 0;
    if (size)
        *size = 0;
}

/*-------------------------------------------------------------------------
 * Function:    verify_get_chunk_info (helper function)
 *
 * Purpose:     Verifies that H5Dget_chunk_info returns correct
 *              values for a chunk.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static int
verify_get_chunk_info(hid_t dset, hid_t dspace, hsize_t chk_index, hsize_t exp_chk_size,
                      const hsize_t *exp_offset, unsigned exp_flt_msk)
{
    unsigned read_flt_msk  = 0;      /* Read filter mask */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size          = 0;      /* Size of an allocated/written chunk */
    haddr_t  addr          = 0;      /* Address of an allocated/written chunk */

    if (H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR;

    if (HADDR_UNDEF == addr)
        FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");
    if (size != exp_chk_size)
        FAIL_PUTS_ERROR("unexpected chunk size");
    if (read_flt_msk != exp_flt_msk)
        FAIL_PUTS_ERROR("unexpected filter mask");
    if (out_offset[0] != exp_offset[0])
        FAIL_PUTS_ERROR("unexpected offset[0]");
    if (out_offset[1] != exp_offset[1])
        FAIL_PUTS_ERROR("unexpected offset[1]");

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    verify_get_chunk_info_by_coord (helper function)
 *
 * Purpose:     Verifies that H5Dget_chunk_info_by_coord returns correct
 *              values for a chunk.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static int
verify_get_chunk_info_by_coord(hid_t dset, hsize_t *offset, hsize_t exp_chk_size, unsigned exp_flt_msk)
{
    unsigned read_flt_msk = 0; /* Read filter mask */
    hsize_t  size         = 0; /* Size of an allocated/written chunk */
    haddr_t  addr         = 0; /* Address of an allocated/written chunk */

    /* Get info of the chunk at logical coordinates specified by offset */
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR;

    if (HADDR_UNDEF == addr)
        FAIL_PUTS_ERROR("address cannot be HADDR_UNDEF");
    if (size != exp_chk_size)
        FAIL_PUTS_ERROR("unexpected chunk size");
    if (read_flt_msk != exp_flt_msk)
        FAIL_PUTS_ERROR("unexpected filter mask");

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    verify_empty_chunk_info (helper function)
 *
 * Purpose:     Verifies that H5Dget_chunk_info_by_coord returns correct
 *              values for an empty chunk.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2018
 *
 *-------------------------------------------------------------------------
 */
static int
verify_empty_chunk_info(hid_t dset, hsize_t *offset)
{
    unsigned read_flt_msk = 0; /* Read filter mask */
    hsize_t  size         = 0; /* Size of an allocated/written chunk */
    haddr_t  addr         = 0; /* Address of an allocated/written chunk */

    /* Get info of the chunk at logical coordinates specified by offset */
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR;

    if (HADDR_UNDEF != addr)
        FAIL_PUTS_ERROR("address was not HADDR_UNDEF");
    if (EMPTY_CHK_SIZE != size)
        FAIL_PUTS_ERROR("size was not EMPTY_CHK_SIZE");

    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    test_get_chunk_info_highest18
 *
 * Purpose:     Test getting various chunk information
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions is
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Description:
 *              This function tests the new API functions added for EED-343:
 *              H5Dget_num_chunks, H5Dget_chunk_info, and
 *              H5Dget_chunk_info_by_coord for high bound up to 1.8.
 *
 * Date:        September 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_chunk_info_highest18(hid_t fapl)
{
    char     filename[FILENAME_BUF_SIZE];
    hid_t    chunkfile     = -1;       /* File ID */
    hid_t    dspace        = -1;       /* Dataspace ID */
    hid_t    dset          = -1;       /* Dataset ID */
    hid_t    cparms        = -1;       /* Creation plist */
    hsize_t  dims[2]       = {NX, NY}; /* Dataset dimensions */
    hsize_t  maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};             /* Chunk dimensions */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    int      out_buf[NX][NY];                                  /* Buffer to read data in */
    size_t   buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int); /* Buffer size of a chk */
    unsigned filter_mask  = 0;                                 /* Filter mask */
    unsigned read_flt_msk = 0;                                 /* Filter mask after direct read */
    int      fillvalue    = -1;                                /* Fill value */
#ifdef H5_HAVE_FILTER_DEFLATE
    int aggression = 9; /* Compression aggression setting */
#endif
    H5F_libver_t low, high;              /* File format bounds */
    hsize_t      offset[2];              /* Offset coordinates of a chunk */
    hsize_t      out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t      size          = 0;      /* Size of an allocated/written chunk */
    hsize_t      nchunks       = 0;      /* Number of chunks */
    haddr_t      addr          = 0;      /* Address of an allocated/written chunk */
    hsize_t      index         = 0;      /* Index of a chunk */
    int          n;                      /* Used on buffer, to avoid conversion warning */
    hsize_t      i, j;
    herr_t       ret;

    TESTING("getting chunk information in file with version prior to 1.10");

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Set high bound to V18 to test chunked dataset that use B-tree v1
       structures to index chunks */
    high = H5F_LIBVER_V18;

    /* Low bound can be anything below 1.10, which was when the new chunk storage
       was introduced */
    low = H5F_LIBVER_EARLIEST;

    /* Set version bounds for creating the file */
    if (H5Pset_libver_bounds(fapl, low, high) < 0)
        TEST_ERROR

    chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    if (chunkfile < 0)
        TEST_ERROR

    /* Create the file and memory dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Set dset creation properties with chunking, compression, and fillvalue */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

#ifdef H5_HAVE_FILTER_DEFLATE
    if (H5Pset_deflate(cparms, (unsigned)aggression) < 0)
        TEST_ERROR
#endif /* end H5_HAVE_FILTER_DEFLATE */

    /* Set fill value */
    if (H5Pset_fill_value(cparms, H5T_NATIVE_INT, &fillvalue) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(chunkfile, DSET_SIMPLE_CHUNKED, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Indicate skipping the compression filter */
    filter_mask = 0x00000001;

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for (n = 0; n < NUM_CHUNKS; n++)
        for (i = 0; i < CHUNK_NX; i++)
            for (j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++) {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void *)direct_buf[n]);
            if (ret < 0)
                TEST_ERROR
        }

    if (H5Fflush(dset, H5F_SCOPE_LOCAL) < 0)
        goto error;

    /* Close the dataset then... */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if ((dset = H5Dopen2(chunkfile, DSET_SIMPLE_CHUNKED, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != NUM_CHUNKS_WRITTEN)
        TEST_ERROR

    /* Read the entire dataset back */
    ret = H5Dread(dset, H5T_NATIVE_INT, dspace, dspace, H5P_DEFAULT, out_buf);
    if (ret < 0)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 12)
        TEST_ERROR

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR

    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 12)
        TEST_ERROR

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    ret       = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    ret       = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Attempt to get info of empty chunks, verify the returned addr and size */
    offset[0] = 0;
    offset[1] = 0;
    ret       = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    ret       = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i * CHUNK_NX, j * CHUNK_NY, (void *)direct_buf[n]) < 0)
                TEST_ERROR

    /* Close the first dataset */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, DSET_EMPTY, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if ((dset = H5Dopen2(chunkfile, DSET_EMPTY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is 0 */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 0)
        TEST_ERROR

    /* Attempt to get info of a chunk from an empty dataset, verify the
       returned address and size */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    ret       = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /************************************************************************
     * Test empty dataset with H5D_ALLOC_TIME_EARLY                         *
     ************************************************************************/

    /* Set space allocation to early so that chunk query functions will
       retrieve chunk information even though the dataset is empty */
    if (H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, DSET_EMPTY_ALLOC, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if ((dset = H5Dopen2(chunkfile, DSET_EMPTY_ALLOC, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is NUM_CHUNKS */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != NUM_CHUNKS)
        TEST_ERROR

    /* Attempt to get info of a chunk from an empty dataset, verify the
       returned address and size */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR

    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size == 0)
        TEST_ERROR

    index = 10;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size);
    if (ret < 0)
        TEST_ERROR

    /* Because of H5P_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size == 0)
        TEST_ERROR

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    /* Because of H5P_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size == 0)
        TEST_ERROR

    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* Close/release resources. */
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Pclose(cparms) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_get_chunk_info_highest18() */

/*-------------------------------------------------------------------------
 * Function:    test_chunk_info_single_chunk
 *
 * Purpose:     Test getting various chunk information when Single Chunk
 *              index type is used
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        November 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_info_single_chunk(char *filename, hid_t fapl)
{
    hid_t             chunkfile     = -1;       /* File ID */
    hid_t             dspace        = -1;       /* Dataspace ID */
    hid_t             dset          = -1;       /* Dataset ID */
    hid_t             cparms        = -1;       /* Creation plist */
    hsize_t           dims[2]       = {NX, NY}; /* Dataset dimensions */
    hsize_t           chunk_dims[2] = {NX, NY}; /* Chunk dimensions */
    int               in_buf[NX][NY];           /* Input buffer */
    unsigned          filter_mask  = 0;         /* Filter mask */
    unsigned          read_flt_msk = 0;         /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;                 /* Dataset chunk index type */
    hsize_t           offset[2];                /* Offset coordinates of a chunk */
    hsize_t           out_offset[2] = {0, 0};   /* Buffer to get offset coordinates */
    hsize_t           size          = 0;        /* Size of an allocated/written chunk */
    hsize_t           nchunks       = 0;        /* Number of chunks */
    haddr_t           addr          = 0;        /* Address of an allocated/written chunk */
    hsize_t           index         = 0;        /* Index of a chunk */
    int               i, j;

    TESTING("   Single Chunk index");

    /* Open the file for reading/writing */
    if ((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_SINGLE_CHUNK, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if (idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("should be using Single Chunk index type");

    /* Close the dataset */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions on a single empty
       chunk */
    if ((dset = H5Dopen2(chunkfile, DSET_SINGLE_CHUNK, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 0)
        TEST_ERROR

    /* Initialize the array of chunk data for the single chunk */
    for (i = 0; i < NX; i++)
        for (j = 0; j < NY; j++)
            in_buf[i][j] = (i * j);

    /* Write the chunk */
    if (H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, in_buf) < 0)
        TEST_ERROR

    /* Get and verify that one chunk had been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 1)
        TEST_ERROR

    /* Get and verify info of the first and only chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != SINGLE_CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 0)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,0) */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != SINGLE_CHUNK_SIZE)
        TEST_ERROR

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_single_chunk() */

/*-------------------------------------------------------------------------
 * Function:    test_chunk_info_implicit
 *
 * Purpose:     Test getting various chunk information when Implicit
 *              index type is used
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        November 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_info_implicit(char *filename, hid_t fapl)
{
    hid_t             chunkfile     = -1;                               /* File ID */
    hid_t             dspace        = -1;                               /* Dataspace ID */
    hid_t             dset          = -1;                               /* Dataset ID */
    hid_t             cparms        = -1;                               /* Creation plist */
    hsize_t           dims[2]       = {NX, NY};                         /* Dataset dimensions */
    hsize_t           chunk_dims[2] = {CHUNK_NX, CHUNK_NY};             /* Chunk dimensions */
    int               direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    int               out_buf[NX][NY];                                  /* Buffer to read data in */
    size_t            buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int); /* Buffer size of a chk */
    unsigned          filter_mask  = 0;                                 /* Filter mask */
    unsigned          read_flt_msk = 0;                                 /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;                                         /* Dataset chunk index type */
    hsize_t           offset[2];                                        /* Offset coordinates of a chunk */
    hsize_t           out_offset[2] = {0, 0};                           /* Buffer to get offset coordinates */
    hsize_t           size          = 0; /* Size of an allocated/written chunk */
    hsize_t           nchunks       = 0; /* Number of chunks */
    haddr_t           addr          = 0; /* Address of an allocated/written chunk */
    hsize_t           index         = 0; /* Index of a chunk */
    int               n;                 /* Used on buffer, to avoid conversion warning */
    hsize_t           i, j;
    herr_t            ret;

    TESTING("   Implicit index");

    /* Open the file for reading/writing */
    if ((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Set allocation time to early */
    if (H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(chunkfile, DSET_IMPLICIT_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if (idx_type != H5D_CHUNK_IDX_NONE) /* Implicit: No Index */
        FAIL_PUTS_ERROR("should be using Implicit index type");

    /* Close the dataset then... */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if ((dset = H5Dopen2(chunkfile, DSET_IMPLICIT_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR

    /* All chunks because of H5D_ALLOC_TIME_EARLY */
    if (nchunks != NUM_CHUNKS)
        TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for (n = 0; n < NUM_CHUNKS; n++)
        for (i = 0; i < CHUNK_NX; i++)
            for (j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++) {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void *)direct_buf[n]);
            if (ret < 0)
                TEST_ERROR
        }

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR

    /* Should still be all chunks, because of H5D_ALLOC_TIME_EARLY */
    if (nchunks != NUM_CHUNKS)
        TEST_ERROR

    /* Read the entire dataset back */
    if (H5Dread(dset, H5T_NATIVE_INT, dspace, dspace, H5P_DEFAULT, out_buf) < 0)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 0)
        TEST_ERROR

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 4)
        TEST_ERROR

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR

    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 12)
        TEST_ERROR

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 4)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    /* Because of H5P_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF */
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    /* Because of H5P_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF */
    if (addr == HADDR_UNDEF)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i * CHUNK_NX, j * CHUNK_NY, (void *)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_implicit() */

/*-------------------------------------------------------------------------
 * Function:    test_chunk_info_fixed_array
 *
 * Purpose:     Test getting various chunk information when Fixed Array
 *              index type is used
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        November 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_info_fixed_array(char *filename, hid_t fapl)
{
    hid_t             chunkfile     = -1;                               /* File ID */
    hid_t             dspace        = -1;                               /* Dataspace ID */
    hid_t             dset          = -1;                               /* Dataset ID */
    hid_t             cparms        = -1;                               /* Creation plist */
    hsize_t           dims[2]       = {NX, NY};                         /* Dataset dimensions */
    hsize_t           chunk_dims[2] = {CHUNK_NX, CHUNK_NY};             /* Chunk dimensions */
    int               direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    int               out_buf[NX][NY];                                  /* Buffer to read data in */
    size_t            buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int); /* Buffer size of a chk */
    unsigned          filter_mask  = 0;                                 /* Filter mask */
    unsigned          read_flt_msk = 0;                                 /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;                                         /* Dataset chunk index type */
    hsize_t           offset[2];                                        /* Offset coordinates of a chunk */
    hsize_t           out_offset[2] = {0, 0};                           /* Buffer to get offset coordinates */
    hsize_t           size          = 0; /* Size of an allocated/written chunk */
    hsize_t           nchunks       = 0; /* Number of chunks */
    haddr_t           addr          = 0; /* Address of an allocated/written chunk */
    hsize_t           index         = 0; /* Index of a chunk */
    int               n;                 /* Used on buffer, to avoid conversion warning */
    hsize_t           i, j;
    herr_t            ret;

    TESTING("   Fixed Array index");

    /* Open the file for reading/writing */
    if ((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(chunkfile, DSET_FIXED_ARR_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if (idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("should be using Fixed Array index type");

    /* Close the dataset then... */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if ((dset = H5Dopen2(chunkfile, DSET_FIXED_ARR_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 0)
        TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for (n = 0; n < NUM_CHUNKS; n++)
        for (i = 0; i < CHUNK_NX; i++)
            for (j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++) {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void *)direct_buf[n]);
            if (ret < 0)
                TEST_ERROR
        }

    /* Read the entire dataset back */
    if (H5Dread(dset, H5T_NATIVE_INT, dspace, dspace, H5P_DEFAULT, out_buf) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != NUM_CHUNKS_WRITTEN)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 12)
        TEST_ERROR

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 12)
        TEST_ERROR

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i * CHUNK_NX, j * CHUNK_NY, (void *)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_fixed_array() */

/*-------------------------------------------------------------------------
 * Function:    test_chunk_info_extensible_array
 *
 * Purpose:     Test getting various chunk information when Extensible Array
 *              index type is used
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        November 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_info_extensible_array(char *filename, hid_t fapl)
{
    hid_t             chunkfile     = H5I_INVALID_HID;                  /* File ID */
    hid_t             dspace        = H5I_INVALID_HID;                  /* Dataspace ID */
    hid_t             dset          = H5I_INVALID_HID;                  /* Dataset ID */
    hid_t             cparms        = H5I_INVALID_HID;                  /* Creation plist */
    hsize_t           dims[2]       = {NX, NY};                         /* Dataset dimensions */
    hsize_t           chunk_dims[2] = {CHUNK_NX, CHUNK_NY};             /* Chunk dimensions */
    hsize_t           maxdims[2]    = {H5S_UNLIMITED, NY};              /* One unlimited dimension */
    int               direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    size_t            buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int); /* Buffer size of a chk */
    unsigned          filter_mask  = 0;                                 /* Filter mask */
    unsigned          read_flt_msk = 0;                                 /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;                                         /* Dataset chunk index type */
    hsize_t           offset[2];                                        /* Offset coordinates of a chunk */
    hsize_t           out_offset[2] = {0, 0};                           /* Buffer to get offset coordinates */
    hsize_t           size          = 0; /* Size of an allocated/written chunk */
    hsize_t           nchunks       = 0; /* Number of chunks */
    haddr_t           addr          = 0; /* Address of an allocated/written chunk */
    hsize_t           index         = 0; /* Index of a chunk */
    int               n;                 /* Used on buffer, to avoid conversion warning */
    hsize_t           i, j;
    herr_t            ret;

    TESTING("   Extensible Array index");

    /* Open the file for reading/writing */
    if ((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(chunkfile, DSET_EXT_ARR_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if (idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("should be using Extensible Array index type");

    /* Close the dataset then... */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if ((dset = H5Dopen2(chunkfile, DSET_EXT_ARR_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 0)
        TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for (n = 0; n < NUM_CHUNKS; n++)
        for (i = 0; i < CHUNK_NX; i++)
            for (j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++) {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void *)direct_buf[n]);
            if (ret < 0)
                TEST_ERROR
        }

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != NUM_CHUNKS_WRITTEN)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 12)
        TEST_ERROR

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR

    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 12)
        TEST_ERROR

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i * CHUNK_NX, j * CHUNK_NY, (void *)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_extensible_array() */

/*-------------------------------------------------------------------------
 * Function:    test_chunk_info_version2_btrees
 *
 * Purpose:     Test getting various chunk information when Version 2 B-trees
 *              index type is used
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        November 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_info_version2_btrees(char *filename, hid_t fapl)
{
    hid_t             chunkfile     = H5I_INVALID_HID;                  /* File ID */
    hid_t             dspace        = H5I_INVALID_HID;                  /* Dataspace ID */
    hid_t             dset          = H5I_INVALID_HID;                  /* Dataset ID */
    hid_t             cparms        = H5I_INVALID_HID;                  /* Creation plist */
    hsize_t           dims[2]       = {NX, NY};                         /* Dataset dimensions */
    hsize_t           chunk_dims[2] = {CHUNK_NX, CHUNK_NY};             /* Chunk dimensions */
    hsize_t           maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};   /* Two unlimited dims */
    int               direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    size_t            buf_size     = CHUNK_NX * CHUNK_NY * sizeof(int); /* Buffer size of a chk */
    unsigned          filter_mask  = 0;                                 /* Filter mask */
    unsigned          read_flt_msk = 0;                                 /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;                                         /* Dataset chunk index type */
    hsize_t           offset[2];                                        /* Offset coordinates of a chunk */
    hsize_t           out_offset[2] = {0, 0};                           /* Buffer to get offset coordinates */
    hsize_t           size          = 0; /* Size of an allocated/written chunk */
    hsize_t           nchunks       = 0; /* Number of chunks */
    haddr_t           addr          = 0; /* Address of an allocated/written chunk */
    hsize_t           index         = 0; /* Index of a chunk */
    int               n;                 /* Used on buffer, to avoid conversion warning */
    hsize_t           i, j;
    herr_t            ret;

    TESTING("   Version 2 B-trees index");

    /* Open the file for reading/writing */
    if ((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(chunkfile, DSET_V2_BTREE_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if (H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if (idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("should be using Version 2 B-tree index type");

    /* Close the dataset then... */
    if (H5Dclose(dset) < 0)
        TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if ((dset = H5Dopen2(chunkfile, DSET_V2_BTREE_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != 0)
        TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for (n = 0; n < NUM_CHUNKS; n++)
        for (i = 0; i < CHUNK_NX; i++)
            for (j = 0; j < CHUNK_NY; j++)
                direct_buf[n][i][j] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++) {
            offset[0] = i * CHUNK_NX;
            offset[1] = j * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void *)direct_buf[n]);
            if (ret < 0)
                TEST_ERROR
        }

    /* Get and verify the number of chunks written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR
    if (nchunks != NUM_CHUNKS_WRITTEN)
        TEST_ERROR

    /* Get and verify info of the first chunk */
    index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the second chunk */
    index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 0 || out_offset[1] != 12)
        TEST_ERROR

    /* Get and verify info of the third chunk */
    index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR

    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 8)
        TEST_ERROR

    /* Get and verify info of the last chunk */
    index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR
    if (out_offset[0] != 6 || out_offset[1] != 12)
        TEST_ERROR

    /* Attempt to get info of empty chunk and verify the returned address and size */
    index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if (H5Dget_chunk_info(dset, dspace, index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (read_flt_msk != filter_mask)
        TEST_ERROR
    if (size != CHUNK_SIZE)
        TEST_ERROR

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if (H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    if (addr != HADDR_UNDEF)
        TEST_ERROR
    if (size != 0)
        TEST_ERROR

    /* Read each chunk and verify the values */
    n = 0;
    for (i = 0; i < 2; i++)
        for (j = 2; j < 4; j++, n++)
            if (read_each_chunk(dset, i * CHUNK_NX, j * CHUNK_NY, (void *)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR
    if (H5Sclose(dspace) < 0)
        TEST_ERROR
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_version2_btrees() */

typedef struct chunk_iter_info_t {
    hsize_t  offset[2];
    unsigned filter_mask;
    haddr_t  addr;
    hsize_t  size;
} chunk_iter_info_t;

typedef struct chunk_iter_udata_t {
    chunk_iter_info_t *chunk_info;
    int                last_index;
} chunk_iter_udata_t;

static int
iter_cb(const hsize_t *offset, unsigned filter_mask, haddr_t addr, hsize_t size, void *op_data)
{
    chunk_iter_udata_t *cidata = (chunk_iter_udata_t *)op_data;
    int                 idx    = cidata->last_index + 1;

    cidata->chunk_info[idx].offset[0]   = offset[0];
    cidata->chunk_info[idx].offset[1]   = offset[1];
    cidata->chunk_info[idx].filter_mask = filter_mask;
    cidata->chunk_info[idx].addr        = addr;
    cidata->chunk_info[idx].size        = size;

    cidata->last_index++;

    return H5_ITER_CONT;
}

static int
iter_cb_stop(const hsize_t H5_ATTR_UNUSED *offset, unsigned H5_ATTR_UNUSED filter_mask,
             haddr_t H5_ATTR_UNUSED addr, hsize_t H5_ATTR_UNUSED size, void *op_data)
{
    chunk_iter_info_t **chunk_info = (chunk_iter_info_t **)op_data;
    *chunk_info += 1;
    return H5_ITER_STOP;
}

static int
iter_cb_fail(const hsize_t H5_ATTR_UNUSED *offset, unsigned H5_ATTR_UNUSED filter_mask,
             haddr_t H5_ATTR_UNUSED addr, hsize_t H5_ATTR_UNUSED size, void *op_data)
{
    chunk_iter_info_t **chunk_info = (chunk_iter_info_t **)op_data;
    *chunk_info += 1;
    return H5_ITER_ERROR;
}

/*-------------------------------------------------------------------------
 * Function:    test_basic_query
 *
 * Purpose:     Tests basic operations to ensure the chunk query functions
 *              work properly.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_basic_query(hid_t fapl)
{
    char               filename[FILENAME_BUF_SIZE];          /* File name */
    hid_t              basicfile     = H5I_INVALID_HID;      /* File ID */
    hid_t              dspace        = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t              dset          = H5I_INVALID_HID;      /* Dataset ID */
    hid_t              cparms        = H5I_INVALID_HID;      /* Creation plist */
    hsize_t            dims[2]       = {NX, NY};             /* Dataset dimensions */
    hsize_t            chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    int                direct_buf[CHUNK_NX][CHUNK_NY];       /* Data in chunks */
    unsigned           flt_msk      = 0;                     /* Filter mask */
    unsigned           read_flt_msk = 0;                     /* Filter mask after direct read */
    hsize_t            offset[2];                            /* Offset coordinates of a chunk */
    hsize_t            out_offset[2] = {0, 0};               /* Buffer to get offset coordinates */
    hsize_t            size          = 0;                    /* Size of an allocated/written chunk */
    hsize_t            nchunks       = 0;                    /* Number of chunks */
    haddr_t            addr          = 0;                    /* Address of an allocated/written chunk */
    hsize_t            chk_index     = 0;                    /* Index of a chunk */
    hsize_t            ii, jj;                               /* Array indices */
    chunk_iter_info_t  chunk_infos[2];                       /* Chunk infos filled up by iterator */
    chunk_iter_info_t *cptr;                                 /* Pointer to array of chunks */
    chunk_iter_udata_t udata;                                /* udata for iteration */
    herr_t             ret; /* Temporary returned value for verifying failure */

    TESTING("basic operations");

    /* Create the file */
    h5_fixname(BASIC_FILE, fapl, filename, sizeof filename);

    /* Create a new file. */
    if ((basicfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* Enable chunking */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if (H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR;

    /* Create a new dataset using cparms creation properties */
    dset =
        H5Dcreate2(basicfile, DSET_SIMPLE_CHUNKED, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if (dset < 0)
        TEST_ERROR;

    /* Get the number of chunks and verify that no chunk has been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR;
    if (NO_CHUNK_WRITTEN != nchunks)
        FAIL_PUTS_ERROR("unexpected number of chunks");

    /* Initialize the array of chunk data for the single chunk */
    for (ii = 0; ii < CHUNK_NX; ii++)
        for (jj = 0; jj < CHUNK_NY; jj++)
            direct_buf[ii][jj] = (int)(ii * jj);

    /* Write the chunk of data */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if (H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, CHUNK_SIZE, direct_buf) < 0)
        TEST_ERROR;

    /* Get and verify that one chunk had been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR;
    if (ONE_CHUNK_WRITTEN != nchunks)
        FAIL_PUTS_ERROR("unexpected number of chunks");

    /* Get and verify info of the first and only chunk */
    if (verify_get_chunk_info(dset, dspace, 0, CHUNK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (CHUNK_NX,CHUNK_NY) */
    if (verify_get_chunk_info_by_coord(dset, offset, CHUNK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Attempt to get chunk info given an invalid chunk index and verify
     * that failure occurs */
    chk_index = INVALID_CHK_INDEX;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY
    {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    }
    H5E_END_TRY;
    /* In HDF5 1.10 this will not fail. It will fail in 1.12 and older. */
    /*
    if (ret != FAIL)
        TEST_ERROR;
    */

    /* Write the chunk of data to another location */
    offset[0] = 0;
    offset[1] = 0;
    if (H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, CHUNK_SIZE, direct_buf) < 0)
        TEST_ERROR;

    /* Get and verify that two chunks had been written */
    if (H5Dget_num_chunks(dset, dspace, &nchunks) < 0)
        TEST_ERROR;
    if (TWO_CHUNKS_WRITTEN != nchunks)
        FAIL_PUTS_ERROR("unexpected number of chunks");

    /* Get and verify info of the first written chunk in the dataset, its
       offset should be (0,0) */
    if (verify_get_chunk_info(dset, dspace, 0, CHUNK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (0,0) */
    if (verify_get_chunk_info_by_coord(dset, offset, CHUNK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Get and verify info of the second written chunk in the dataset, its
       offset should be (CHUNK_NX, CHUNK_NY) */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if (verify_get_chunk_info(dset, dspace, 1, CHUNK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (CHUNK_NX, CHUNK_NY) */
    if (verify_get_chunk_info_by_coord(dset, offset, CHUNK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Get and verify info of an empty chunk, at offset
       (2*CHUNK_NX, 2*CHUNK_NY) */
    offset[0] = 2 * CHUNK_NX;
    offset[1] = 2 * CHUNK_NY;
    /* Get and verify info of the chunk at the offset (CHUNK_NX, CHUNK_NY) */
    if (verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Iterate over all chunks */
    udata.chunk_info = chunk_infos;
    udata.last_index = -1;
    if (H5Dchunk_iter(dset, H5P_DEFAULT, &iter_cb, &udata) < 0)
        TEST_ERROR;

    printf("\nchunk_infos 1 offset 0: %lld\n", chunk_infos[1].offset[0]);

    if (udata.last_index != 1)
        FAIL_PUTS_ERROR("Iterator did not iterate over all chunks");
    if (chunk_infos[0].offset[0] != 0)
        FAIL_PUTS_ERROR("offset[0] mismatch");
    if (chunk_infos[0].offset[1] != 0)
        FAIL_PUTS_ERROR("offset[1] mismatch");
    if (chunk_infos[0].filter_mask != 0)
        FAIL_PUTS_ERROR("filter mask mismatch");
    if (chunk_infos[0].size != 96)
        FAIL_PUTS_ERROR("size mismatch");

    if (chunk_infos[1].offset[0] != CHUNK_NX)
        FAIL_PUTS_ERROR("offset[0] mismatch");
    if (chunk_infos[1].offset[1] != CHUNK_NY)
        FAIL_PUTS_ERROR("offset[1] mismatch");

    /* Iterate and stop after one iteration */
    cptr = &(chunk_infos[0]);
    if (H5Dchunk_iter(dset, H5P_DEFAULT, &iter_cb_stop, &cptr) < 0)
        TEST_ERROR;
    if (cptr != &(chunk_infos[1]))
        FAIL_PUTS_ERROR("Verification of halted iterator failed");

    /* Iterate and fail after one iteration */
    cptr = &(chunk_infos[0]);
    H5E_BEGIN_TRY
    {
        ret = H5Dchunk_iter(dset, H5P_DEFAULT, &iter_cb_fail, &cptr);
    }
    H5E_END_TRY;
    if (ret >= 0)
        TEST_ERROR;
    if (cptr != &(chunk_infos[1]))
        FAIL_PUTS_ERROR("Verification of halted iterator failed");

    /* Release resource */
    if (H5Dclose(dset) < 0)
        TEST_ERROR;
    if (H5Sclose(dspace) < 0)
        TEST_ERROR;
    if (H5Pclose(cparms) < 0)
        TEST_ERROR;
    if (H5Fclose(basicfile) < 0)
        TEST_ERROR;

    /* Remove the test file */
    HDremove(filename);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(basicfile);
    }
    H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_basic_query() */

/*-------------------------------------------------------------------------
 * Function:    test_get_chunk_info_110
 *
 * Purpose:     Test getting various chunk information in version 1.10.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Note:        Note that the dataspace argument in these new functions are
 *              currently not used.  The functionality involved the dataspace
 *              will be implemented in the next version.
 *
 * Description:
 *              This function tests the new API functions added for EED-343:
 *              H5Dget_num_chunks, H5Dget_chunk_info, and H5Dget_chunk_info_by_coord
 *              for high bound beyond 1.8.
 *
 * Date:        October 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_chunk_info_110(hid_t fapl)
{
    hid_t        chunkfile = -1; /* File ID */
    char         filename[FILENAME_BUF_SIZE];
    H5F_libver_t low, high; /* File format bounds */

    TESTING("getting chunk information in file with versions 1.10");
    HDprintf("\n"); /* to list sub-tests */

    /* Create the file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (chunkfile < 0)
        TEST_ERROR

    /* Close the file, individual tests will re-open the file with different
       libvers via the fapl */
    if (H5Fclose(chunkfile) < 0)
        TEST_ERROR

    /* Set high bound to the current latest version */
    high = H5F_LIBVER_LATEST;

    /* Test getting info of chunked datasets in version combo up to 1.10 */
    for (low = H5F_LIBVER_V110; low <= H5F_LIBVER_LATEST; low++) {
        /* Set version bounds for creating file */
        if (H5Pset_libver_bounds(fapl, low, high) < 0)
            TEST_ERROR

        /* Test getting chunk info when Single Chunk index type is used */
        if (test_chunk_info_single_chunk(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Implicit index type is used */
        if (test_chunk_info_implicit(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Fixed Array index type is used */
        if (test_chunk_info_fixed_array(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Extensible Array index type is used */
        if (test_chunk_info_extensible_array(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Version 2 B-trees index type is used */
        if (test_chunk_info_version2_btrees(filename, fapl) < 0)
            TEST_ERROR
    } /* for low libver bound */

    return SUCCEED;

error:
    H5_FAILED();
    return FAIL;
} /* test_get_chunk_info_110() */

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
 *              until now.  For the work in HDFFV-10615, the test function
 *              test_get_chunk_info was added to test the new query chunk
 *              API functions: H5Dget_num_chunk, H5Dget_chunk_info, and
 *              H5Dget_chunk_info_by_coord.  This code can be used at a
 *              later time, so it is kept here.
 *              -BMR, November 5, 2018
 *
 * Programmer:  Pedro Vicente
 *              April 7, 2008
 *
 *-------------------------------------------------------------------------
 */
static int
create_4x4_dset(hid_t fapl)
{
    hid_t   fid   = H5I_INVALID_HID; /* file ID */
    hid_t   did   = H5I_INVALID_HID; /* dataset ID */
    hid_t   f_sid = H5I_INVALID_HID; /* file space ID */
    hid_t   m_sid = H5I_INVALID_HID; /* memory space ID */
    hid_t   pid   = H5I_INVALID_HID; /* property list ID */
    hsize_t start[2];                /* chunk location to start writing */
    hsize_t dims[2]          = {4, 4};
    hsize_t chunk_dims[2]    = {2, 2};
    int     chunk_data[2][2] = {{1, 1}, {1, 1}};
    int     buf[4][4];
    int     fillvalue = 0;
    char    filename[FILENAME_BUF_SIZE];
    hsize_t i, j;

    /* create a new file using default properties. */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* create the file space */
    if ((f_sid = H5Screate_simple(RANK, dims, dims)) < 0)
        TEST_ERROR

    /* create the memory space with chunk dimensions */
    if ((m_sid = H5Screate_simple(RANK, chunk_dims, chunk_dims)) < 0)
        TEST_ERROR
    start[0] = 0;
    start[1] = 0;
    if (H5Sselect_hyperslab(m_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0)
        TEST_ERROR

    /*-------------------------------------------------------------------------
     * create a dataset
     *-------------------------------------------------------------------------
     */

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((pid = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR
    if (H5Pset_chunk(pid, RANK, chunk_dims) < 0)
        TEST_ERROR
    if (H5Pset_fill_value(pid, H5T_NATIVE_INT, &fillvalue) < 0)
        TEST_ERROR

    /* create a new dataset */
    did = H5Dcreate2(fid, DATASETNAME, H5T_NATIVE_INT, f_sid, H5P_DEFAULT, pid, H5P_DEFAULT);
    if (did < 0)
        TEST_ERROR

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
            if (H5Sselect_hyperslab(f_sid, H5S_SELECT_SET, start, NULL, chunk_dims, NULL) < 0)
                TEST_ERROR

            /* write the data to the hyperslab. */
            if (H5Dwrite(did, H5T_NATIVE_INT, m_sid, f_sid, H5P_DEFAULT, chunk_data) < 0)
                TEST_ERROR

            /* read back and display complete dataset 4x4 */
            if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
                TEST_ERROR

#if defined(PRINT_DATA)
            printf("\n");
            printf("dataset: \n");
            for (jj = 0; jj < dims[0]; jj++) {
                for (ii = 0; ii < dims[1]; ii++)
                    printf("%d ", buf[jj][ii]);
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
    if (H5Dclose(did) < 0)
        TEST_ERROR
    if (H5Sclose(f_sid) < 0)
        TEST_ERROR
    if (H5Sclose(m_sid) < 0)
        TEST_ERROR
    if (H5Pclose(pid) < 0)
        TEST_ERROR
    if (H5Fclose(fid) < 0)
        TEST_ERROR

    return SUCCEED;

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
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 * Programmer:  Binh-Minh Ribler
 *              November 5, 2018
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t fapl    = H5I_INVALID_HID; /* File access property list */
    int   nerrors = 0;               /* Number of errors so far */

    h5_reset();

    /* Create a copy of file access property list */
    if ((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0)
        TEST_ERROR

    /* Test basic operations on the chunk query functions */
    nerrors += test_basic_query(fapl) < 0 ? 1 : 0;

    /* Tests getting chunk information of version 1.8 and prior */
    nerrors += test_get_chunk_info_highest18(fapl) < 0 ? 1 : 0;

    /* Tests getting chunk information of version 1.10 */
    nerrors += test_get_chunk_info_110(fapl) < 0 ? 1 : 0;

    /* Create a 4x4 dataset (using the existing code to avoid compilation
       warnings for now) */
    nerrors += create_4x4_dset(fapl) < 0 ? 1 : 0;

    if (nerrors)
        goto error;

    HDprintf("All chunk query tests passed.\n");

    h5_cleanup(FILENAME, fapl);

    return EXIT_SUCCESS;

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d QUERY CHUNK INFO TEST%s FAILED! *****\n", nerrors, 1 == nerrors ? "" : "S");
    return EXIT_FAILURE;
}

/****************************************************************************
 Additional tests to be added:
- create/write to a dataset, do the query before closing the dataset
- do the query when extending the dataset (shrink or expand)
- verify that invalid input parameters are handled properly
- do the query on a non-chunked dataset...
- test for filter or non-filter

****************************************************************************/
