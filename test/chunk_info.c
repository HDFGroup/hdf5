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
 *
 * Modification:
 *              Many tests were added for HDFFV-10677. -BMR, August 2019
 *
 * Test structure:
 *          main()
 *              test_get_chunk_info_highest18()
 *              test_get_chunk_info_110()
 *                  test_chunk_info_single_chunk()
 *                  test_chunk_info_implicit()
 *                  test_chunk_info_fixed_array()
 *                  test_chunk_info_extensible_array()
 *                  test_chunk_info_version2_btrees()
 *                  test_failed_attempts()
 *              test_filter_mask_with_skip_compress()
 *
 */
#define H5D_FRIEND
#define H5D_TESTING     /* to use H5D__ functions */
#include "H5Dpkg.h"

#include "testhdf5.h"
#include "zlib.h"

/* Used to make certain an offset is as expected */
#define VERIFY_VAR(_x, _val, where, var) do {                                         \
    long __x = (long)_x, __val = (long)_val;                                        \
    if(VERBOSE_HI) {                                                                \
        print_func("   Call to routine: %15s at line %4d in %s had value "          \
            "%ld \n", (where), (int)__LINE__, __FILE__, __x);                       \
    }                                                                               \
    if((__x) != (__val)) {                                                          \
        TestErrPrintf("*** UNEXPECTED VALUE from %s, %s should be %ld,"         \
                      " but is %ld at line %4d in %s\n",                             \
                     (where), (var),  __val, __x, (int)__LINE__, __FILE__);                 \
        H5Eprint2(H5E_DEFAULT, stdout);                                             \
    }                                                                               \
} while(0)

/* Test file names, using H5F_libver_t as indices */
const char *FILENAME[] = {
        "tchunk_info_earliest",
        "tchunk_info_18",
        "tchunk_info_110",
        "tchunk_info_112",
        NULL
};
#define FILTERMASK_FILE         "tfilter_mask.h5"

/* From original test */
#define DATASETNAME "2d"

/* Parameters for testing chunk querying */
#define RANK                    2
#define FILENAME_BUF_SIZE       1024
#define DSET_SIMPLE_CHUNKED     "Chunked Dataset"
#define DSET_CONTIGUOUS         "Contiguous Dataset"
#define DSET_EMPTY              "Empty Dataset"
#define DSET_EMPTY_ALLOC        "Empty Dataset with ALLOC_TIME_EARLY"
#define DSET_SINGLE_CHUNK       "Single Chunk Index Dataset"
#define DSET_IMPLICIT_INDEX     "Implicit Index Dataset"
#define DSET_FIXED_ARR_INDEX    "Fixed Array Index Dataset"
#define DSET_EXT_ARR_INDEX      "Extensible Array Index Dataset"
#define DSET_V2_BTREE_INDEX     "Version 2 B-Tree Index Dataset"
#define DATASETNAME2        "skip_one_filter"
#define NX                      24
#define NY                      16
#define CHUNK_NX                6
#define CHUNK_NY                4
#define SINGLE_CHUNK_SIZE       (NX*NY*sizeof(int))
#define CHUNK_SIZE              96
#define NUM_CHUNKS              16
#define NUM_CHUNKS_WRITTEN      4
#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s))*1.001)+12)

/* Utility function to initialize arguments */
void reinit_vars(unsigned *read_flt_msk, haddr_t *addr, hsize_t *size);

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
//EIP - May be this function should take a pointer to an array of chunk dimensions
//EIP  and its size, so it is not restricted to 2 dims only?
static herr_t read_each_chunk(hid_t dset_id, hsize_t offset1, hsize_t offset2, void *direct_buf)
{
    int      read_buf[CHUNK_NX][CHUNK_NY];
    hsize_t  offset[2] = {offset1, offset2};
    unsigned read_flt_msk = 0;
    herr_t  ret;                        /* Return value */

    HDmemset(&read_buf, 0, sizeof(read_buf));

    /* Read the chunk specified by its offset */
    ret = H5Dread_chunk(dset_id, H5P_DEFAULT, offset, &read_flt_msk, read_buf);
    if(ret < 0) return(FAIL);

    /* Verify that read chunk is the same as the corresponding written one */
    if(HDmemcmp(direct_buf, read_buf, CHUNK_NX*CHUNK_NY) != 0)
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
void reinit_vars(unsigned *read_flt_msk, haddr_t *addr, hsize_t *size)
{
    if(read_flt_msk)
        *read_flt_msk = 0;
    if(addr)
        *addr = 0;
    if(size)
        *size = 0;
}

/*-------------------------------------------------------------------------
 * Function:    test_get_chunk_info_highest18
 *
 * Purpose:     Test getting various chunk information
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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  out_offset[2];      /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      out_buf[NX][NY];    /* Buffer to read data in */
    size_t   buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);  /* Buffer size of a chk */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    int      fillvalue = -1;     /* Fill value */
    int      aggression = 9;     /* Compression aggression setting */
    H5F_libver_t low, high;      /* File format bounds */
    hsize_t  offset[2] = {0, 0};          /* Offset coordinates of a chunk */
    int      n;                  /* Used on buffer, to avoid conversion warning */
    hsize_t  ii, jj;
    const Bytef *z_src = (const Bytef*)(direct_buf);
    Bytef       *z_dst;          /*destination buffer */
    uLongf       z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(buf_size);
    uLong        z_src_nbytes = (uLong)buf_size;
    void         *outbuf = NULL; /* Pointer to new buffer */

    herr_t   ret;

    TESTING("getting chunk information in file with version prior to 1.10");

    /* Create the file */
    h5_fixname(FILENAME[H5F_LIBVER_V18], fapl, filename, sizeof filename);

    /* Set high bound to V18 to test chunked dataset that use B-tree v1
       structures to index chunks */
    high = H5F_LIBVER_V18;

    /* Low bound can be anything below 1.10, which was when the new chunk storage
       was introduced */
    low = H5F_LIBVER_EARLIEST;

    /* Set version bounds for creating the file */
    if(H5Pset_libver_bounds(fapl, low, high) < 0)
        TEST_ERROR

    chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    if(chunkfile < 0)
        TEST_ERROR

    /* Create the file and memory dataspaces */
    if((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Set dset creation properties with chunking, compression, and fillvalue */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0) TEST_ERROR

#ifdef H5_HAVE_FILTER_DEFLATE
    if(H5Pset_deflate(cparms, (unsigned)aggression) < 0) TEST_ERROR
#endif /* end H5_HAVE_FILTER_DEFLATE */

    /* Set fill value */
    if(H5Pset_fill_value(cparms, H5T_NATIVE_INT, &fillvalue) < 0) TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_SIMPLE_CHUNKED, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = n + 1;

    /* Allocate output (compressed) buffer */
    outbuf = malloc(z_dst_nbytes);
    z_dst = (Bytef *)outbuf;

    /* Perform compression from the source to the destination buffer */
    ret = compress2(z_dst, &z_dst_nbytes, z_src, z_src_nbytes, aggression);

    /* Check for various zlib errors */
    if(Z_BUF_ERROR == ret) {
        fprintf(stderr, "overflow");
        TEST_ERROR
    } else if(Z_MEM_ERROR == ret) {
        fprintf(stderr, "deflate memory error");
        TEST_ERROR
    } else if(Z_OK != ret) {
        fprintf(stderr, "other deflate error");
        TEST_ERROR
    }

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
        {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void*)direct_buf[n]);
            if(ret < 0) TEST_ERROR
        }

    /* Read each chunk and verify the values */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
            if(read_each_chunk(dset, ii*CHUNK_NX, jj*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Free the read buffer */
    if(outbuf)
        HDfree(outbuf);

    if(H5Fflush(dset, H5F_SCOPE_LOCAL) < 0)
        TEST_ERROR

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_SIMPLE_CHUNKED, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS_WRITTEN) TEST_ERROR

    /* Go through all written chunks, get their info and verify the values */
    chk_index = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, chk_index++) {
            int kk;

            if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
                TEST_ERROR
            CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
            VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
            VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
            VERIFY(out_offset[0], ii * CHUNK_NX, "H5Dget_chunk_info, offset");
            VERIFY(out_offset[1], jj * CHUNK_NY, "H5Dget_chunk_info, offset");

            /* Reset variables to pass in to the next call */
            reinit_vars(&read_flt_msk, &addr, &size);

            /* Copy offsets to pass in to the next call */
            for(kk = 0; kk < RANK; kk++)
                offset[kk] = out_offset[kk];

            /* Get info of the chunk at the specified offsets and verify its info */
            if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
                TEST_ERROR
            CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
            VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
        }

    /* Get and verify info of the last chunk, passing in H5S_ALL */
    chk_index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    ret = H5Dget_chunk_info(dset, H5S_ALL, chk_index, out_offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 6, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 12, "H5Dget_chunk_info, offset");

    /* Attempt to get info of a non-existing chunk, should fail */
    chk_index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, H5S_ALL, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt to get info of a non-existing chunk.")

    /* Attempt to get info of empty chunks, verify the returned addr and size */
    offset[0] = 0;
    offset[1] = 0;
    ret = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    ret = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    /* Read each chunk and verify the values */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
            if(read_each_chunk(dset, ii*CHUNK_NX, jj*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Close the first dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, DSET_EMPTY, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if((dset = H5Dopen2(chunkfile, DSET_EMPTY, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is 0 */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 0) TEST_ERROR

    /* Attempt to get info of a chunk from an empty dataset, should fail */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt to get info of a non-existing chunk.")

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       should succeed with the returned address as HADDR_UNDEF and size as 0 */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    if(H5Dclose(dset) < 0) TEST_ERROR

    /************************************************************************
     * Test empty dataset with H5D_ALLOC_TIME_EARLY                         *
     ************************************************************************/

    /* Set space allocation to early so that chunk query functions will
       retrieve chunk information even though the dataset is empty */
    if(H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, DSET_EMPTY_ALLOC, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if((dset = H5Dopen2(chunkfile, DSET_EMPTY_ALLOC, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is NUM_CHUNKS */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS) TEST_ERROR

    /* Attempt to get info of a chunk from an empty dataset, verify the
       returned address and size in the case of H5D_ALLOC_TIME_EARLY */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR("Chunk address should not be HADDR_UNDEF because of H5D_ALLOC_TIME_EARLY.");
    if(size == 0)
        FAIL_PUTS_ERROR("Chunk size should not be 0 because of H5D_ALLOC_TIME_EARLY.");

    chk_index = 10;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR("Chunk address should not be HADDR_UNDEF because of H5D_ALLOC_TIME_EARLY.");
    if(size == 0)
        FAIL_PUTS_ERROR("Chunk size should not be 0 because of H5D_ALLOC_TIME_EARLY.");

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR("Chunk address should not be HADDR_UNDEF because of H5D_ALLOC_TIME_EARLY.");
    if(size == 0)
        FAIL_PUTS_ERROR("Chunk size should not be 0 because of H5D_ALLOC_TIME_EARLY.");

    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Close/release resources. */
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Pclose(cparms) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {NX, NY};      /* Chunk dimensions */
    int      in_buf[NX][NY];     /* Input buffer */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      ii, jj;
herr_t ret = 0;

    TESTING("   Single Chunk index");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_SINGLE_CHUNK, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if(idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("Should be using Single Chunk index type");

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions on a single empty
       chunk */
    if((dset = H5Dopen2(chunkfile, DSET_SINGLE_CHUNK, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 0) TEST_ERROR

    /* Initialize the array of chunk data for the single chunk */
    for(ii = 0; ii < NX; ii++)
        for(jj = 0; jj < NY; jj++)
            in_buf[ii][jj] = (ii*jj);

    /* Write the chunk */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, in_buf) < 0)
        TEST_ERROR

    /* Get and verify that one chunk had been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 1) TEST_ERROR

    /* Get and verify info of the first and only chunk */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, SINGLE_CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 0, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 0, "H5Dget_chunk_info, offset");

    /* Get info of the chunk at logical coordinates (0,0) */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, SINGLE_CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Attempt to get chunk info given an invalid chunk index and verify
     * that failure occurs */
    chk_index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        TEST_ERROR

    /* Get info of the chunk at logical coordinates (0,0) */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, SINGLE_CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      read_direct_chunk[CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      out_buf[NX][NY];    /* Buffer to read data in */
    size_t   buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);  /* Buffer size of a chk */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      aggression = 9;     /* Compression aggression setting */
    int      n;                  /* Used on buffer, to avoid conversion warning*/
    hsize_t  ii, jj;
    herr_t   ret;

    TESTING("   Implicit index");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    if(H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_IMPLICIT_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR

    if(idx_type != H5D_CHUNK_IDX_NONE) /* Implicit: No Index */
        FAIL_PUTS_ERROR("Should be using Implicit index type");

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_IMPLICIT_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR

    /* All chunks because of H5D_ALLOC_TIME_EARLY */
    if(nchunks != NUM_CHUNKS) TEST_ERROR

    /* Get and verify the number of chunks again, passing in H5S_ALL */
    if(H5Dget_num_chunks(dset, H5S_ALL, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS) TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
        {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void*)direct_buf[n]);
            if(ret < 0) TEST_ERROR
        }

    if(H5Fflush(dset, H5F_SCOPE_LOCAL) < 0) TEST_ERROR

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_IMPLICIT_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if(H5Dget_num_chunks(dset, H5S_ALL, &nchunks) < 0) TEST_ERROR

    /* Go through all chunks, and get their info and verify the values */
    chk_index = 0;
    for(ii = 0; ii < NX/CHUNK_NX; ii++)
        for(jj = 0; jj < NY/CHUNK_NY; jj++, chk_index++) {
            int kk;

            if(H5Dget_chunk_info(dset, H5S_ALL, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
                TEST_ERROR
            CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
            VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
            VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
            VERIFY(out_offset[0], ii * CHUNK_NX, "H5Dget_chunk_info, offset");
            VERIFY(out_offset[1], jj * CHUNK_NY, "H5Dget_chunk_info, offset");

            /* Reset variables to pass in to the next call */
            reinit_vars(&read_flt_msk, &addr, &size);

            /* Copy offsets to pass in to the next call */
            for(kk = 0; kk < RANK; kk++)
                offset[kk] = out_offset[kk];

            /* Get info of a chunk and verify its information.  Note that
               all chunks in this dataset are allocated because of the property
               H5D_ALLOC_TIME_EARLY */
            if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
                TEST_ERROR
            CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
            VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
        }

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Pclose(cparms) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      out_buf[NX][NY];    /* Buffer to read data in */
    size_t   buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);  /* Buffer size of a chk */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      n;                  /* Used on buffer, to avoid conversion warning */
    hsize_t  ii, jj;
    herr_t   ret;

    TESTING("   Fixed Array index");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_FIXED_ARR_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if(idx_type != H5D_CHUNK_IDX_FARRAY)
        FAIL_PUTS_ERROR("Should be using Fixed Array index type");

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_FIXED_ARR_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 0) TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
        {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void*)direct_buf[n]);
            if(ret < 0) TEST_ERROR
        }

    /* Read the entire dataset back */
    if(H5Dread(dset, H5T_NATIVE_INT, dspace, dspace, H5P_DEFAULT, out_buf) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS_WRITTEN) TEST_ERROR

    /* Get and verify info of the first chunk */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");

    /* Get and verify info of the second chunk */
    chk_index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");

    /* Get and verify info of the third chunk */
    chk_index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");

    /* Get and verify info of the last chunk */
    chk_index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");

    /* Attempt to get info of empty chunk, should fail */
    chk_index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    /* Read each chunk and verify the values */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
            if(read_each_chunk(dset, ii*CHUNK_NX, jj*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, NY}; /* One unlimited dimension */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      out_buf[NX][NY];    /* Buffer to read data in */
    size_t   buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);  /* Buffer size of a chk */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      n;                  /* Used on buffer, to avoid conversion warning */
    hsize_t  ii, jj;
    herr_t   ret;

    TESTING("   Extensible Array index");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_EXT_ARR_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if(idx_type != H5D_CHUNK_IDX_EARRAY)
        FAIL_PUTS_ERROR("Should be using Extensible Array index type");

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_EXT_ARR_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 0) TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
        {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void*)direct_buf[n]);
            if(ret < 0) TEST_ERROR
        }

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS_WRITTEN) TEST_ERROR

    /* Get and verify info of the first chunk */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 0, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 8, "H5Dget_chunk_info, offset");

    /* Get and verify info of the second chunk */
    chk_index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 0, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 12, "H5Dget_chunk_info, offset");

    /* Get and verify info of the third chunk */
    chk_index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 6, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 8, "H5Dget_chunk_info, offset");

    /* Get and verify info of the last chunk */
    chk_index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 6, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 12, "H5Dget_chunk_info, offset");

    /* Attempt to get info using an out-of-range index, should fail */
    chk_index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    /* Read each chunk and verify the values */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
            if(read_each_chunk(dset, ii*CHUNK_NX, jj*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

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
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* Two unlimited dims */
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    int      out_buf[NX][NY];    /* Buffer to read data in */
    size_t   buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);  /* Buffer size of a chk */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      n;                  /* Used on buffer, to avoid conversion warning */
    hsize_t  ii, jj;
    herr_t   ret;

    TESTING("   Version 2 B-trees index");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, DSET_V2_BTREE_INDEX, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Ensure we're using the correct chunk indexing scheme */
    if(H5D__layout_idx_type_test(dset, &idx_type) < 0)
        TEST_ERROR
    if(idx_type != H5D_CHUNK_IDX_BT2)
        FAIL_PUTS_ERROR("Should be using Version 2 B-tree index type");

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, DSET_V2_BTREE_INDEX, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != 0) TEST_ERROR

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = n + 1;

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
        {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, filter_mask, offset, buf_size, (void*)direct_buf[n]);
            if(ret < 0) TEST_ERROR
        }

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    if(nchunks != NUM_CHUNKS_WRITTEN) TEST_ERROR

    /* Get and verify info of the first chunk */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 0, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 8, "H5Dget_chunk_info, offset");

    /* Get and verify info of the second chunk */
    chk_index = 1;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 0, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 12, "H5Dget_chunk_info, offset");

    /* Get and verify info of the third chunk */
    chk_index = 2;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 6, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 8, "H5Dget_chunk_info, offset");
 
    /* Get and verify info of the last chunk */
    chk_index = 3;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], 6, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], 12, "H5Dget_chunk_info, offset");

    /* Attempt to provide out-of-range offsets, should fail */
    chk_index = 5;
    reinit_vars(&read_flt_msk, &addr, &size);
    out_offset[0] = out_offset[1] = 0;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Get info of the chunk at logical coordinates (0,2) */
    offset[0] = 0;
    offset[1] = 2 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Get info of the chunk at logical coordinates (1,3) */
    offset[0] = 1 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, CHUNK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, 0, "H5Dget_chunk_info_by_coord, chunk size");

    /* Read each chunk and verify the values */
    n = 0;
    for(ii = 0; ii < 2; ii++)
        for(jj = 2; jj < 4; jj++, n++)
            if(read_each_chunk(dset, ii*CHUNK_NX, jj*CHUNK_NY, (void*)direct_buf[n]) < 0)
                TEST_ERROR

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_chunk_info_version2_btrees() */

/*-------------------------------------------------------------------------
 * Function:    test_failed_attempts
 *
 * Purpose:     Test attempting to use chunk query functions incorrectly.
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
test_failed_attempts(char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;     /* File ID */
    hid_t    dspace = H5I_INVALID_HID;        /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;          /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;        /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {NX, NY};      /* Chunk dimensions */
    int      in_buf[NX][NY];     /* Input buffer */
    unsigned filter_mask = 0;    /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      ii, jj;
    herr_t   ret = 0;

    TESTING("   Invalid Operations");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR
 
    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Create a contiguous dataset */
    dset = H5Dcreate2(chunkfile, DSET_CONTIGUOUS, H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Initialize the array of data */
    for(ii = 0; ii < NX; ii++)
        for(jj = 0; jj < NY; jj++)
            in_buf[ii][jj] = (ii*jj);

    /* Write the data */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, in_buf) < 0)
        TEST_ERROR

    /* Close the dataset then... */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions on contiguous dataset */
    if((dset = H5Dopen2(chunkfile, DSET_CONTIGUOUS, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Attempt to get the number of chunks on contiguous dataset, should fail */
    H5E_BEGIN_TRY {
        ret = H5Dget_num_chunks(dset, dspace, &nchunks);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt a chunk query function on a contiguous dataset.")

    /* Attempt to get chunk info on contiguous data, should fail */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt a chunk query function on a contiguous dataset.")

    /* Attempt to get chunk info at logical coordinates (0,0) on contiguous
     * dataset, should fail */
    offset[0] = 0;
    offset[1] = 0;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt a chunk query function on a contiguous dataset.")

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Fclose(chunkfile) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Fclose(chunkfile);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_failed_attempts() */

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
 *              for low bound beyond 1.8.
 *
 * Date:        October 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_chunk_info_110(hid_t fapl)
{
    hid_t        chunkfile = H5I_INVALID_HID; /* File ID */
    char         filename[FILENAME_BUF_SIZE];
    H5F_libver_t low, high;      /* File format bounds */

    TESTING("getting chunk information in file with versions 1.10 and later");
    HDprintf("\n"); /* to list sub-tests */

    /* Set high bound to the current latest version */
    high = H5F_LIBVER_LATEST;

    /* Test getting info of chunked datasets in version combo up to 1.10 */
    for(low = H5F_LIBVER_V110; low <= H5F_LIBVER_LATEST; low++) {
        /* Set version bounds for creating file */
        if(H5Pset_libver_bounds(fapl, low, high) < 0)
            TEST_ERROR

        /* Create the file */
        h5_fixname(FILENAME[low], fapl, filename, sizeof filename);
        chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        if(chunkfile < 0) TEST_ERROR

        /* Close the file, individual tests will re-open the file with different
           libvers via the fapl */
        if(H5Fclose(chunkfile) < 0) TEST_ERROR

        /* Test getting chunk info when Single Chunk index type is used */
        if(test_chunk_info_single_chunk(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Implicit index type is used */
        if(test_chunk_info_implicit(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Fixed Array index type is used */
        if(test_chunk_info_fixed_array(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Extensible Array index type is used */
        if(test_chunk_info_extensible_array(filename, fapl) < 0)
            TEST_ERROR

        /* Test getting chunk info when Version 2 B-trees index type is used */
        if(test_chunk_info_version2_btrees(filename, fapl) < 0)
            TEST_ERROR

        /* Test various attempts to use the functions incorrectly */
        if(test_failed_attempts(filename, fapl) < 0)
            TEST_ERROR

    } /* for low libver bound */

    return SUCCEED;

error:
    H5_FAILED();
    return FAIL;
} /* test_get_chunk_info_110() */

/*-------------------------------------------------------------------------
 * Function:    test_filter_mask_with_skip_compress
 *
 * Purpose:     Test getting chunk info when compression filter is skipped.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019 (based on direct_chunk.c/test_skip_compress_write1)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_mask_with_skip_compress(hid_t fapl)
{
    hid_t       filter_file = H5I_INVALID_HID; /* File for filter mask */
    char        filename[FILENAME_BUF_SIZE];
    hid_t       dataspace = -1, dataset = -1;
    hid_t       mem_space = -1;
    hid_t       cparms = -1, dxpl = -1;
    hsize_t     dims[2]  = {NX, NY};
    hsize_t     maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t     chunk_dims[2] ={CHUNK_NX, CHUNK_NY};
    unsigned    filter_mask = 0;
    unsigned    read_flt_msk = 0;
    int         direct_buf[CHUNK_NX][CHUNK_NY];
    int         check_chunk[CHUNK_NX][CHUNK_NY];
    hsize_t     offset[2] = {0, 0};
    hsize_t     out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t     size = 0;           /* Size of an allocated/written chunk */
    hsize_t     nchunks = 0;        /* Number of chunks */
    haddr_t     addr = 0;           /* Address of an allocated/written chunk */
    hsize_t     chk_index = 0;      /* Index of a chunk */
    size_t      buf_size = CHUNK_NX*CHUNK_NY*sizeof(int);
    int         aggression = 9;     /* Compression aggression setting */
    unsigned    read_filter_mask = 0; /* filter mask after direct read */
    int         read_direct_buf[CHUNK_NX][CHUNK_NY];
    hsize_t     read_buf_size = 0; /* buf size */
    hsize_t     start[2];  /* Start of hyperslab */
    hsize_t     stride[2]; /* Stride of hyperslab */
    hsize_t     count[2];  /* Block count */
    hsize_t     block[2];  /* Block sizes */
    int         ii, jj, n;
    herr_t      status;

    TESTING("getting filter mask when compression filter is skipped");

    /* Create the file */
    h5_fixname(FILTERMASK_FILE, fapl, filename, sizeof filename);

    /* Create a new file. */
    if((filter_file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create file data space with unlimited dimensions. */
    if((dataspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create memory data space. */
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR;

    /* Create dataset create property list with chunking and compression
       enabled. */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if((status = H5Pset_chunk( cparms, RANK, chunk_dims)) < 0)
        TEST_ERROR;

    if((status = H5Pset_deflate( cparms, (unsigned ) aggression)) < 0)
        TEST_ERROR;

    /* Create a new dataset using cparms creation properties. */
    if((dataset = H5Dcreate2(filter_file, DATASETNAME2, H5T_NATIVE_INT, dataspace, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create transfer property list for writing */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Initialize data for one chunk */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++) {
            direct_buf[ii][jj] = n++;
            }

    /* Indicate the compression filter is to be skipped.     */
    filter_mask = 0x00000001;

    /* Write the uncompressed data chunk repeatedly to fill the dataset,
       using the direct writing function. */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if((status = H5Dwrite_chunk(dataset, dxpl, filter_mask, offset, buf_size, direct_buf)) < 0)
        TEST_ERROR;

    if(H5Fflush(dataset, H5F_SCOPE_LOCAL) < 0)
        TEST_ERROR;

    /* Close and re-open the dataset */
    if(H5Dclose(dataset) < 0)
        TEST_ERROR;
    if((dataset = H5Dopen2(filter_file, DATASETNAME2, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Select hyperslab for the chunk just written in the file */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if((status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, start, stride, count, block)) < 0)
        TEST_ERROR;

    /* Read the chunk back */
    if((status = H5Dread(dataset, H5T_NATIVE_INT, mem_space, dataspace, H5P_DEFAULT, check_chunk)) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(ii = 0; ii < CHUNK_NX; ii++) {
        for(jj = 0; jj < CHUNK_NY; jj++) {
            if(direct_buf[ii][jj] != check_chunk[ii][jj]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", ii, jj);
                HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[ii][jj], check_chunk[ii][jj]);
                TEST_ERROR;
            }
        }
    }

    /* Query chunk storage size */
    if((status = H5Dget_chunk_storage_size(dataset, offset, &read_buf_size)) < 0)
        TEST_ERROR;
    if(read_buf_size != buf_size)
        TEST_ERROR;

    /* Read the raw chunk back with H5Dread_chunk */
    HDmemset(&read_direct_buf, 0, sizeof(read_direct_buf));
    if((status = H5Dread_chunk(dataset, H5P_DEFAULT, offset, &read_filter_mask, read_direct_buf)) < 0)
        TEST_ERROR;
    if(read_filter_mask != filter_mask)
        TEST_ERROR;

    /* Check that the direct chunk read is the same as the chunk written */
    for(ii = 0; ii < CHUNK_NX; ii++) {
        for(jj = 0; jj < CHUNK_NY; jj++) {
            if(direct_buf[ii][jj] != read_direct_buf[ii][jj]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", ii, jj);
                HDprintf("    direct_buf=%d, read_direct_buf=%d\n", direct_buf[ii][jj], read_direct_buf[ii][jj]);
                TEST_ERROR;
            }
        }
    }

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dataset, H5S_ALL, &nchunks) < 0) TEST_ERROR
    if(nchunks != 1) TEST_ERROR

    /* Get and verify info of the first and only chunk */
    chk_index = 0;
    reinit_vars(&read_flt_msk, &addr, &size);
    if(H5Dget_chunk_info(dataset, H5S_ALL, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, buf_size, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], CHUNK_NX, "H5Dget_chunk_info, offset");
    VERIFY(out_offset[1], CHUNK_NY, "H5Dget_chunk_info, offset");

    /* Get info of the chunk at the specified offsets and verify its info */
    if(H5Dget_chunk_info_by_coord(dataset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, buf_size, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, filter_mask, "H5Dget_chunk_info_by_coord, filter mask");

    /* Release resourse */
    if(H5Dclose(dataset) < 0) TEST_ERROR
    if(H5Sclose(mem_space) < 0) TEST_ERROR
    if(H5Sclose(dataspace) < 0) TEST_ERROR
    if(H5Pclose(cparms) < 0) TEST_ERROR
    if(H5Pclose(dxpl) < 0) TEST_ERROR
    if(H5Fclose(filter_file) < 0) TEST_ERROR

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(mem_space);
        H5Sclose(dataspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Fclose(filter_file);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_filter_mask_with_skip_compress() */

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
    hid_t fapl = H5I_INVALID_HID;    /* File access property list */
    int   nerrors = 0;  /* Number of errors so far */

    h5_reset();

    /* Create a copy of file access property list */
    if((fapl = H5Pcreate(H5P_FILE_ACCESS)) < 0) TEST_ERROR

    /* Tests getting chunk information of version 1.8 and prior */
    nerrors += test_get_chunk_info_highest18(fapl) < 0 ? 1 : 0;

    /* Tests getting chunk information of version 1.10 */
    nerrors += test_get_chunk_info_110(fapl) < 0 ? 1 : 0;

    /* Tests getting filter mask when compression filter is skipped */
    nerrors += test_filter_mask_with_skip_compress(fapl) < 0 ? 1 : 0;

    if(nerrors)
        TEST_ERROR

    HDprintf("All chunk query tests passed.\n");

    h5_cleanup(FILENAME, fapl);

    return SUCCEED;

error:
    nerrors = MAX(1, nerrors);
    HDprintf("***** %d QUERY CHUNK INFO TEST%s FAILED! *****\n",
              nerrors, 1 == nerrors ? "" : "S");
    return FAIL;
}

/****************************************************************************
 Additional tests to be added:
- create/write to a dataset, do the query before closing the dataset
- do the query when extending the dataset (shrink or expand)
- verify that invalid input parameters are handled properly
- do the query on a non-chunked dataset...
- test for filter or non-filter

****************************************************************************/
