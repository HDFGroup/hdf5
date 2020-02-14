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
 *
 * Purpose:     Tests chunk query API functions
 *
 * Modification:
 *              Many tests were added for HDFFV-10677. -BMR, August 2019
 *
 * Test structure:
 *          main()
 *              test_basic_query()
 *              test_get_chunk_info_highest_v18()
 *              test_get_chunk_info_v110()
 *                  test_chunk_info_single_chunk()
 *                  test_chunk_info_implicit()
 *                  test_chunk_info_fixed_array()
 *                  test_chunk_info_extensible_array()
 *                  test_chunk_info_version2_btrees()
 *                  test_failed_attempts()
 *              test_flt_msk_with_skip_compress()
 *
 * Helper functions:
 *          verify_idx_nchunks()
 *          verify_get_chunk_info()
 *          verify_get_chunk_info_by_coord()
 *          verify_empty_chunk_info()
 *          index_type_str()
 *
 */
#define H5D_FRIEND
#define H5D_TESTING     /* to use H5D__ functions */
#include "H5Dpkg.h"

#include "testhdf5.h"
#ifdef H5_HAVE_FILTER_DEFLATE
#include "zlib.h"
#endif

/* Test file names, using H5F_libver_t as indices */
const char *FILENAME[] = {
        "tchunk_info_earliest",
        "tchunk_info_v18",
        "tchunk_info_v110",
        "tchunk_info_v112",
        "tchunk_info_v114",
        NULL
};

/* File to be used in test_failed_attempts */
#define FILTERMASK_FILE         "tflt_msk"
#define BASIC_FILE              "basic_query"

/* Parameters for testing chunk querying */
#define SIMPLE_CHUNKED_DSET_NAME     "Chunked Dataset"
#define CONTIGUOUS_DSET_NAME         "Contiguous Dataset"
#define EMPTY_DSET_NAME              "Empty Dataset"
#define EMPTY_EARLY_ALLOC_DSET_NAME  "Empty Dataset with ALLOC_TIME_EARLY"
#define SINGLE_CHUNK_DSET_NAME       "Single Chunk Index Dataset"
#define IMPLICIT_INDEX_DSET_NAME     "Implicit Index Dataset"
#define FIXED_ARR_INDEX_DSET_NAME    "Fixed Array Index Dataset"
#define EXT_ARR_INDEX_DSET_NAME      "Extensible Array Index Dataset"
#define V2_BTREE_INDEX_DSET_NAME     "Version 2 B-Tree Index Dataset"
#define SKIP_FILTER_DSET_NAME        "Dataset with Skipping One Filter"
#define FILENAME_BUF_SIZE       256  /* Size for file names */
#define RANK                    2    /* Rank for datasets */

/* Dimension of the dataset */
#define NX                      24
#define NY                      16

/* Dimension of the chunk */
#define CHUNK_NX                6
#define CHUNK_NY                4

/* X/Y coords of first chunk written */
#define START_CHK_X             0
#define START_CHK_Y             2

/* X/Y coord of last chunk written */
#define END_CHK_X               2
#define END_CHK_Y               4

/* X and Y coords of an empty chunk */
#define EMPTY_CHK_X             0
#define EMPTY_CHK_Y             0

/* Size of a chunk when the entire dataset is a one single chunk */
#define SINGLE_CHK_SIZE         (NX*NY*sizeof(int))

/* Size of a chunk */
#define CHK_SIZE                (CHUNK_NX*CHUNK_NY*sizeof(int))

/* Size of an empty chunk */
#define EMPTY_CHK_SIZE          0

/* Number of maximum chunks without extending */
#define NUM_CHUNKS              ((NX/CHUNK_NX)*(NY/CHUNK_NY))

/* Number of chunks that have been written */
#define NUM_CHUNKS_WRITTEN      4
#define ONE_CHUNK_WRITTEN       1
#define TWO_CHUNKS_WRITTEN      2
#define NO_CHUNK_WRITTEN        0

/* For testing invalid arguments */
#define NONEXIST_CHK_INDEX      3
#define OUTOFRANGE_CHK_INDEX    5
#define INVALID_CHK_INDEX       5

/* For compressed data */
#define DEFLATE_SIZE_ADJUST(s) (ceil(((double)(s))*1.001)+12.0)

/* For use in error reporting */
#define MSG_CHK_ADDR    "Chunk address should not be HADDR_UNDEF because of H5D_ALLOC_TIME_EARLY."
#define MSG_CHK_SIZE    "Chunk size should not be 0 because of H5D_ALLOC_TIME_EARLY."

/* Utility function to initialize arguments */
void reinit_vars(unsigned *read_flt_msk, haddr_t *addr, hsize_t *size);

/* Helper function containing common code that verifies indexing type
   and number of chunks */
static int verify_idx_nchunks(hid_t dset, hid_t dspace, H5D_chunk_index_t exp_idx_type, hsize_t exp_num_chunks);
static int verify_get_chunk_info(hid_t dset, hid_t dspace, hsize_t chk_index, hsize_t exp_chk_size, hsize_t *exp_offset, unsigned exp_flt_msk);
static int verify_get_chunk_info_by_coord(hid_t dset, hsize_t *offset, hsize_t exp_chk_size, unsigned exp_flt_msk);
static int verify_empty_chunk_info(hid_t dset, hsize_t *offset);
static const char* index_type_str(H5D_chunk_index_t idx_type);

/*-------------------------------------------------------------------------
 * Function:    reinit_vars (helper function)
 *
 * Purpose:     Wipes out variables for the next use, used in various tests.
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
verify_get_chunk_info(hid_t dset, hid_t dspace, hsize_t chk_index, hsize_t exp_chk_size, hsize_t *exp_offset, unsigned exp_flt_msk)
{
    unsigned read_flt_msk = 0;   /* Read filter mask */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */

    if(H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info");
    VERIFY(size, exp_chk_size, "H5Dget_chunk_info, chunk size");
    VERIFY(read_flt_msk, exp_flt_msk, "H5Dget_chunk_info, filter mask");
    VERIFY(out_offset[0], exp_offset[0], "H5Dget_chunk_info, offset[0]");
    VERIFY(out_offset[1], exp_offset[1], "H5Dget_chunk_info, offset[1]");
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
    unsigned read_flt_msk = 0;   /* Read filter mask */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */

    /* Get info of the chunk at logical coordinates specified by offset */
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    CHECK(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord");
    VERIFY(size, exp_chk_size, "H5Dget_chunk_info_by_coord, chunk size");
    VERIFY(read_flt_msk, exp_flt_msk, "H5Dget_chunk_info_by_coord, filter mask");
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
    unsigned read_flt_msk = 0;   /* Read filter mask */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */

    /* Get info of the chunk at logical coordinates specified by offset */
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    VERIFY(addr, HADDR_UNDEF, "H5Dget_chunk_info_by_coord, chunk address");
    VERIFY(size, EMPTY_CHK_SIZE, "H5Dget_chunk_info_by_coord, chunk size");
    return SUCCEED;

error:
    return FAIL;
}

/*-------------------------------------------------------------------------
 * Function:    index_type_str (helper function)
 *
 * Purpose:     Returns the string containing the text associated with the
 *              given indexing scheme.  For use in error messages.
 *
 * Return:      Success:    a valid indexing scheme string
 *              Failure:    a note indicating the indexing type is invalid
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static const char*
index_type_str(H5D_chunk_index_t idx_type)
{
    switch (idx_type) {
        case H5D_CHUNK_IDX_SINGLE:
            return("Single Chunk index type");
        case H5D_CHUNK_IDX_NONE:
            return("Implicit index type");
        case H5D_CHUNK_IDX_FARRAY:
            return("Fixed Array index type");
        case H5D_CHUNK_IDX_EARRAY:
            return("Extensible Array index type");
        case H5D_CHUNK_IDX_BT2:
            return("Version 2 B-tree index type");
        case H5D_CHUNK_IDX_BTREE:
            return("Version 1 B-tree index type (default)");
        case H5D_CHUNK_IDX_NTYPES:
        default:
            return("invalid index type");
    }
} /* index_type_str */

/*-------------------------------------------------------------------------
 * Function:    verify_selected_chunks (helper function)
 *
 * Purpose:     Reads the chunks within the boundery {start,end} and verify
 *              the values against the populated data.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static int
verify_selected_chunks(hid_t dset, hid_t plist, hsize_t *start, hsize_t *end)
{
    int      read_buf[CHUNK_NX][CHUNK_NY];
    int      expected_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Expected data */
    unsigned read_flt_msk = 0;          /* Filter mask read back */
    hsize_t  offset[2] = {0, 0};        /* Offset coordinates of a chunk */
    hsize_t  chk_index;                 /* Chunk index */
    hsize_t  ii, jj;                    /* Array indices */
    int      n;

    HDmemset(&read_buf, 0, sizeof(read_buf));

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks, this is
       the same as the written data and will be used to verify the read data */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                expected_buf[n][ii][jj] = (int)(ii*jj) + 1;

    /* Read each chunk within the boundery of {start,end} and verify the
       values against the expected data */
    chk_index = 0;
    for(ii = start[0]; ii < end[0]; ii++)
        for(jj = start[1]; jj < end[1]; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;

            /* Read the current chunk */
            if(H5Dread_chunk(dset, plist, offset, &read_flt_msk, read_buf) < 0)
                TEST_ERROR

            /* Verify that read chunk is the same as the corresponding written one */
            if(HDmemcmp(expected_buf[chk_index], read_buf, CHUNK_NX*CHUNK_NY) != 0)
            {
                HDfprintf(stderr, "Read chunk differs from written chunk at offset (%d,%d)\n", offset[0], offset[1]);
                return FAIL;
            }
        }

    return SUCCEED;

error:
    return FAIL;
} /* verify_selected_chunks */

/*-------------------------------------------------------------------------
 * Function:    write_selected_chunks (helper function)
 *
 * Purpose:     Verifies that chunk indexing scheme and number of chunks of
 *              the dataset matches the expected values, then write data to
 *              a subset of chunks.  This function opens the dataset then
 *              closes it after writing.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static int
write_selected_chunks(hid_t dset, hid_t plist, hsize_t *start, hsize_t *end, unsigned flt_msk)
{
    int      direct_buf[NUM_CHUNKS][CHUNK_NX][CHUNK_NY];/* Data in chunks */
    hsize_t  offset[2];                 /* Offset coordinates of a chunk */
    hsize_t  chk_index;                 /* Chunk index */
    hsize_t  ii, jj;                    /* Array indices */
    int      n;

    /* Initialize the array of chunk data for all NUM_CHUNKS chunks */
    for(n = 0; n < NUM_CHUNKS; n++)
        for(ii = 0; ii < CHUNK_NX; ii++)
            for(jj = 0; jj < CHUNK_NY; jj++)
                direct_buf[n][ii][jj] = (int)(ii*jj) + 1;

    /* Write NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    chk_index = 0;
    for(ii = start[0]; ii < end[0]; ii++)
        for(jj = start[1]; jj < end[1]; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            if(H5Dwrite_chunk(dset, plist, flt_msk, offset, CHK_SIZE, (void*)direct_buf[chk_index]) < 0)
                TEST_ERROR
        }

    return SUCCEED;

error:
    return FAIL;
} /* write_selected_chunks */

/*-------------------------------------------------------------------------
 * Function:    verify_idx_nchunks (helper function)
 *
 * Purpose:     Verifies that chunk indexing scheme and number of chunks of
 *              the dataset match the expected values.
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL
 *
 * Date:        August 2019
 *
 *-------------------------------------------------------------------------
 */
static int
verify_idx_nchunks(hid_t dset, hid_t dspace, H5D_chunk_index_t exp_idx_type, hsize_t exp_num_chunks)
{
    H5D_chunk_index_t idx_type;         /* Dataset chunk index type */
    hsize_t  nchunks = 0;               /* Number of chunks */

    /* Get the chunk indexing type of the dataset */
    if(H5Dget_chunk_index_type(dset, &idx_type) < 0)
        TEST_ERROR

    /* Ensure the correct chunk indexing scheme is used */
    if(idx_type != exp_idx_type)
    {
        char msg[256];
        sprintf(msg, "Should be using %s.\n", index_type_str(idx_type));
        FAIL_PUTS_ERROR(msg);
    }

    /* Get and verify the number of chunks */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, exp_num_chunks, "H5Dget_num_chunks, number of chunks");

    /* Get and verify the number of chunks again, passing in H5S_ALL */
    if(H5Dget_num_chunks(dset, H5S_ALL, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, exp_num_chunks, "H5Dget_num_chunks, number of chunks");

    return SUCCEED;

error:
    return FAIL;
} /* verify_idx_nchunks */

/*-------------------------------------------------------------------------
 * Function:    test_get_chunk_info_highest_v18
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
test_get_chunk_info_highest_v18(hid_t fapl)
{
    char     filename[FILENAME_BUF_SIZE];   /* File name */
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};        /* Chunk dimensions */
    int      direct_buf[CHUNK_NX][CHUNK_NY];  /* Data chunk */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* 2 unlimited dims */
    hsize_t  out_offset[2];      /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    int      fillvalue = -1;     /* Fill value */
    int      aggression = 9;     /* Compression aggression setting */
    hsize_t  offset[2] = {0, 0}; /* Offset coordinates of a chunk */
#ifdef H5_HAVE_FILTER_DEFLATE
    const Bytef *z_src = (const Bytef*)(direct_buf);
    Bytef       *z_dst;          /*destination buffer */
    uLongf       z_dst_nbytes = (uLongf)DEFLATE_SIZE_ADJUST(CHK_SIZE);
    uLong        z_src_nbytes = (uLong)CHK_SIZE;
#endif /* end H5_HAVE_FILTER_DEFLATE */
    void         *inbuf = NULL; /* Pointer to new buffer */
    hsize_t  chunk_size = CHK_SIZE; /* Size of a chunk, can be compressed or not */
    hsize_t  ii, jj;             /* Array indices */
    int      n;   /* Used as chunk index, but int to avoid conversion warning */
    herr_t   ret; /* Temporary returned value for verifying failure */

    TESTING("getting chunk information in file with version prior to 1.10");

    /* Create the file */
    h5_fixname(FILENAME[H5F_LIBVER_V18], fapl, filename, sizeof filename);

    /* Set version bounds for creating the file.  High bound to V18 to test
       chunked dataset that use B-tree v1 structures to index chunks. */
    if(H5Pset_libver_bounds(fapl, H5F_LIBVER_EARLIEST, H5F_LIBVER_V18) < 0)
        TEST_ERROR

    chunkfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl);
    if(chunkfile < 0)
        TEST_ERROR

    /* Create the file and memory dataspace */
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
    dset = H5Dcreate2(chunkfile, SIMPLE_CHUNKED_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Initialize a chunk of data */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++)
            direct_buf[ii][jj] = (int)(ii*jj) + 1;

#ifdef H5_HAVE_FILTER_DEFLATE
    /* Allocate input (compressed) buffer */
    inbuf = malloc(z_dst_nbytes);

    /* Set chunk size to the compressed chunk size and the chunk point
       to the compressed data chunk */
    chunk_size = (hsize_t)z_dst_nbytes;
    z_dst = (Bytef *)inbuf;

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
#else
    /* Allocate input (non-compressed) buffer */
    inbuf = malloc(CHK_SIZE);
    HDmemcpy(inbuf, direct_buf, CHK_SIZE);
#endif /* end H5_HAVE_FILTER_DEFLATE */

    /* Write only NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    n = 0;
    for(ii = START_CHK_X; ii < END_CHK_X; ii++)
        for(jj = START_CHK_Y; jj < END_CHK_Y; jj++, n++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            ret = H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, chunk_size, (void*)inbuf);
            if(ret < 0) TEST_ERROR
        }

    /* Free the input buffer */
    if(inbuf)
        HDfree(inbuf);

    if(H5Fflush(dset, H5F_SCOPE_LOCAL) < 0)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions */
    if((dset = H5Dopen2(chunkfile, SIMPLE_CHUNKED_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NUM_CHUNKS_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of the last written chunk again, passing in H5S_ALL
       this time */
    offset[0] = 6;
    offset[1] = 12;
    if(verify_get_chunk_info(dset, H5S_ALL, NUM_CHUNKS_WRITTEN-1, chunk_size, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

    /* Attempt to get info of a non-existing chunk, should fail */
    chk_index = OUTOFRANGE_CHK_INDEX;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, H5S_ALL, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt to get info of a non-existing chunk.");

    /* Attempt to get info of empty chunks, verify the returned addr and size */
    offset[0] = 0;
    offset[1] = 0;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Go through all written chunks, get their info and verify the values */
    chk_index = 0;
    for(ii = START_CHK_X; ii < END_CHK_X; ii++)
        for(jj = START_CHK_Y; jj < END_CHK_Y; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;

            if(verify_get_chunk_info(dset, dspace, chk_index, chunk_size, offset, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

            /* Use the same offset to pass into the next ...by_coord function */
            if(verify_get_chunk_info_by_coord(dset, offset, chunk_size, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");
        }

    /* Close the first dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, EMPTY_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if((dset = H5Dopen2(chunkfile, EMPTY_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is 0 */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NO_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Attempt to get info of a chunk from an empty dataset, should fail */
    chk_index = OUTOFRANGE_CHK_INDEX;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempt to get info of a non-existing chunk.");

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       should succeed with the returned address as HADDR_UNDEF and size as 0 */
    offset[0] = EMPTY_CHK_X;
    offset[1] = EMPTY_CHK_Y;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    if(H5Dclose(dset) < 0) TEST_ERROR

    /************************************************************************
     * Test empty dataset with H5D_ALLOC_TIME_EARLY                         *
     ************************************************************************/

    /* Set space allocation to early so that chunk query functions will
       retrieve chunk information even though the dataset is empty */
    if(H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create an empty dataset and close it */
    dset = H5Dcreate2(chunkfile, EMPTY_EARLY_ALLOC_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Reopen the empty dataset to verify the chunk query functions on it */
    if((dset = H5Dopen2(chunkfile, EMPTY_EARLY_ALLOC_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify that the number of chunks is NUM_CHUNKS */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NUM_CHUNKS, "H5Dget_num_chunks, number of chunks");

    /* Attempt to get info of a chunk from an empty dataset, verify the
       returned address and size in the case of H5D_ALLOC_TIME_EARLY */
    chk_index = NONEXIST_CHK_INDEX;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR(MSG_CHK_ADDR);
    if(size == EMPTY_CHK_SIZE)
        FAIL_PUTS_ERROR(MSG_CHK_SIZE);

    chk_index = 10;
    reinit_vars(&read_flt_msk, &addr, &size);
    ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    if(ret < 0) TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR(MSG_CHK_ADDR);
    if(size == EMPTY_CHK_SIZE)
        FAIL_PUTS_ERROR(MSG_CHK_SIZE);

    /* Attempt to get info of a chunk given its coords from an empty dataset,
       verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dget_chunk_info_by_coord(dset, offset, &read_flt_msk, &addr, &size) < 0)
        TEST_ERROR
    /* Because of H5D_ALLOC_TIME_EARLY, addr cannot be HADDR_UNDEF and size not 0 */
    if(addr == HADDR_UNDEF)
        FAIL_PUTS_ERROR(MSG_CHK_ADDR);
    if(size == 0)
        FAIL_PUTS_ERROR(MSG_CHK_SIZE);

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
} /* test_get_chunk_info_highest_v18() */

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
test_chunk_info_single_chunk(const char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};           /* Dataset dimensions */
    hsize_t  chunk_dims[2] = {NX, NY};      /* Chunk dimensions */
    int      data_buf[NX][NY];   /* Input buffer */
    H5D_chunk_index_t idx_type;  /* Dataset chunk index type */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

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
    dset = H5Dcreate2(chunkfile, SINGLE_CHUNK_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* ...open it again to test the chunk query functions on a single empty
       chunk */
    if((dset = H5Dopen2(chunkfile, SINGLE_CHUNK_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Ensure the correct chunk indexing scheme is used */
    if(H5Dget_chunk_index_type(dset, &idx_type) < 0)
        TEST_ERROR
    if(idx_type != H5D_CHUNK_IDX_SINGLE)
        FAIL_PUTS_ERROR("Should be using Single Chunk index type");

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NO_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Initialize the array of chunk data for the single chunk */
    for(ii = 0; ii < NX; ii++)
        for(jj = 0; jj < NY; jj++)
            data_buf[ii][jj] = (int)(ii*jj);

    /* Write the chunk */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_buf) < 0)
        TEST_ERROR

    /* Get and verify that one chunk had been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, ONE_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Offset of the only chunk */
    offset[0] = 0;
    offset[1] = 0;

    /* Get and verify info of the first and only chunk */
    if(verify_get_chunk_info(dset, H5S_ALL, 0, SINGLE_CHK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at logical coordinates (0,0) */
    if(verify_get_chunk_info_by_coord(dset, offset, SINGLE_CHK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Attempt to get chunk info given an invalid chunk index and verify
     * that failure occurs */
    chk_index = INVALID_CHK_INDEX;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
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
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};           /* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    unsigned flt_msk = 0;                   /* Filter mask */
    hsize_t  chk_index = 0;                 /* Index of a chunk */
    hsize_t  ii, jj;                        /* Array indices */
    hsize_t  start[2] = {START_CHK_X, START_CHK_Y}; /* Start position */
    hsize_t  end[2] = {END_CHK_X, END_CHK_Y};       /* End position */

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

    /* Set allocation time to early */
    if(H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(chunkfile, IMPLICIT_INDEX_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Open the dataset again to test getting chunk info */
    if((dset = H5Dopen2(chunkfile, IMPLICIT_INDEX_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify chunk indexing scheme and number of chunks */
    if(verify_idx_nchunks(dset, dspace, H5D_CHUNK_IDX_NONE, NUM_CHUNKS) == FAIL)
        FAIL_PUTS_ERROR("Verification and write failed\n");

    /* Write NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    if(write_selected_chunks(dset, H5P_DEFAULT, start, end, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Writing to selected chunks failed\n");

    /* Go through all chunks, and get their info and verify the values */
    chk_index = 0;
    for(ii = 0; ii < NX/CHUNK_NX; ii++)
        for(jj = 0; jj < NY/CHUNK_NY; jj++, chk_index++) {
            hsize_t offset[2] = {ii * CHUNK_NX, jj * CHUNK_NY};

            if(verify_get_chunk_info(dset, H5S_ALL, chk_index, CHK_SIZE, offset, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

            /* Get info of a chunk and verify its information.  Note that
               all chunks in this dataset are allocated because of the property
               H5D_ALLOC_TIME_EARLY */
            if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");
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
test_chunk_info_fixed_array(const char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2] = {NX, NY};            /* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  start[2] = {START_CHK_X, START_CHK_Y}; /* Start position */
    hsize_t  end[2] = {END_CHK_X, END_CHK_Y};       /* End position */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

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
    dset = H5Dcreate2(chunkfile, FIXED_ARR_INDEX_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Open the dataset again to test getting chunk info */
    if((dset = H5Dopen2(chunkfile, FIXED_ARR_INDEX_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify chunk indexing scheme and number of chunks */
    if(verify_idx_nchunks(dset, dspace, H5D_CHUNK_IDX_FARRAY, NO_CHUNK_WRITTEN) == FAIL)
        FAIL_PUTS_ERROR("Verification and write failed\n");

    /* Write NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    if(write_selected_chunks(dset, H5P_DEFAULT, start, end, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Writing to selected chunks failed\n");

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NUM_CHUNKS_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of each written chunk */
    chk_index = 0;
    for(ii = START_CHK_X; ii < END_CHK_X; ii++)
        for(jj = START_CHK_Y; jj < END_CHK_Y; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;
            if(verify_get_chunk_info(dset, dspace, chk_index, CHK_SIZE, offset, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");
        }

    /* Attempt to get info using an out-of-range index, chk_index is now > NUM_CHUNKS_WRITTEN.  should fail */
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Read and verify values of selected chunks */
    if(verify_selected_chunks(dset, H5P_DEFAULT, start, end) < 0)

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
test_chunk_info_extensible_array(const char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2] = {NX, NY};            /* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY}; /* Chunk dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, NY};     /* One unlimited dimension */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  start[2] = {START_CHK_X, START_CHK_Y}; /* Start position */
    hsize_t  end[2] = {END_CHK_X, END_CHK_Y};       /* End position */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

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
    dset = H5Dcreate2(chunkfile, EXT_ARR_INDEX_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Open the dataset again to test getting chunk info */
    if((dset = H5Dopen2(chunkfile, EXT_ARR_INDEX_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify chunk indexing scheme and number of chunks */
    if(verify_idx_nchunks(dset, dspace, H5D_CHUNK_IDX_EARRAY, NO_CHUNK_WRITTEN) == FAIL)
        FAIL_PUTS_ERROR("Verification and write failed\n");

    /* Write NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    if(write_selected_chunks(dset, H5P_DEFAULT, start, end, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Writing to selected chunks failed\n");

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NUM_CHUNKS_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of each written chunk */
    chk_index = 0;
    for(ii = START_CHK_X; ii < END_CHK_X; ii++)
        for(jj = START_CHK_Y; jj < END_CHK_Y; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;

            if(verify_get_chunk_info(dset, dspace, chk_index, CHK_SIZE, offset, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

            if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");
        }

    /* Attempt to get info using an out-of-range index, should fail */
    chk_index = OUTOFRANGE_CHK_INDEX;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Read and verify values of selected chunks */
    if(verify_selected_chunks(dset, H5P_DEFAULT, start, end) < 0)

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
test_chunk_info_version2_btrees(const char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* Two unlimited dims */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  start[2] = {START_CHK_X, START_CHK_Y}; /* Start position */
    hsize_t  end[2] = {END_CHK_X, END_CHK_Y};       /* End position */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

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
    dset = H5Dcreate2(chunkfile, V2_BTREE_INDEX_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Open the dataset again to test getting chunk info */
    if((dset = H5Dopen2(chunkfile, V2_BTREE_INDEX_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Verify chunk indexing scheme and number of chunks */
    if(verify_idx_nchunks(dset, dspace, H5D_CHUNK_IDX_BT2, NO_CHUNK_WRITTEN) == FAIL)
        FAIL_PUTS_ERROR("Verification and write failed\n");

    /* Write NUM_CHUNKS_WRITTEN chunks at the following logical coords:
       (0,2) (0,3) (1,2) (1,3) */
    if(write_selected_chunks(dset, H5P_DEFAULT, start, end, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Writing to selected chunks failed\n");

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NUM_CHUNKS_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Go through all written chunks, get their info and verify the values */
    chk_index = 0;
    for(ii = START_CHK_X; ii < END_CHK_X; ii++)
        for(jj = START_CHK_Y; jj < END_CHK_Y; jj++, chk_index++) {
            offset[0] = ii * CHUNK_NX;
            offset[1] = jj * CHUNK_NY;

            if(verify_get_chunk_info(dset, dspace, chk_index, CHK_SIZE, offset, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

            if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
                FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");
        }

    /* Attempt to provide out-of-range offsets, should fail */
    chk_index = OUTOFRANGE_CHK_INDEX;
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        FAIL_PUTS_ERROR("    Attempted to get info of a chunk using an out-of-range index.");

    /* Attempt to get info of empty chunks, verify the returned address and size */
    offset[0] = 0;
    offset[1] = 0;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    offset[0] = 3 * CHUNK_NX;
    offset[1] = 3 * CHUNK_NY;
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Read and verify values of selected chunks */
    if(verify_selected_chunks(dset, H5P_DEFAULT, start, end) < 0)

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
    char     filename[FILENAME_BUF_SIZE];   /* File name */
    hid_t    basicfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hsize_t  dims[2] = {NX, NY};            /* Dataset dimensions */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};      /* Chunk dimensions */
    int      direct_buf[CHUNK_NX][CHUNK_NY];/* Data in chunks */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

    TESTING("basic operations");

    /* Create the file */
    h5_fixname(BASIC_FILE, fapl, filename, sizeof filename);

    /* Create a new file. */
    if((basicfile = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Enable chunking */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR

    if(H5Pset_chunk(cparms, RANK, chunk_dims) < 0)
        TEST_ERROR

    /* Create a new dataset using cparms creation properties */
    dset = H5Dcreate2(basicfile, SIMPLE_CHUNKED_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Get the number of chunks and verify that no chunk has been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, NO_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Initialize the array of chunk data for the single chunk */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++)
            direct_buf[ii][jj] = (int)(ii*jj);

    /* Write the chunk of data */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if(H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, CHK_SIZE, direct_buf) < 0)
        TEST_ERROR;

    /* Get and verify that one chunk had been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, ONE_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of the first and only chunk */
    if(verify_get_chunk_info(dset, H5S_ALL, 0, CHK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (CHUNK_NX,CHUNK_NY) */
    if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Attempt to get chunk info given an invalid chunk index and verify
     * that failure occurs */
    chk_index = INVALID_CHK_INDEX;
    reinit_vars(&read_flt_msk, &addr, &size);
    H5E_BEGIN_TRY {
        ret = H5Dget_chunk_info(dset, dspace, chk_index, out_offset, &read_flt_msk, &addr, &size);
    } H5E_END_TRY;
    if(ret != FAIL)
        TEST_ERROR

    /* Write the chunk of data to another location */
    offset[0] = 0;
    offset[1] = 0;
    if(H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, CHK_SIZE, direct_buf) < 0)
        TEST_ERROR;

    /* Get and verify that two chunks had been written */
    if(H5Dget_num_chunks(dset, dspace, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, TWO_CHUNKS_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of the first written chunk in the dataset, its
       offset should be (0,0) */
    if(verify_get_chunk_info(dset, H5S_ALL, 0, CHK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (0,0) */
    if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Get and verify info of the second written chunk in the dataset, its
       offset should be (CHUNK_NX, CHUNK_NY) */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if(verify_get_chunk_info(dset, H5S_ALL, 1, CHK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification H5Dget_chunk_info failed\n");

    /* Get and verify info of the chunk at the offset (CHUNK_NX, CHUNK_NY) */
    if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Get and verify info of an empty chunk, at offset
       (2*CHUNK_NX, 2*CHUNK_NY) */
    offset[0] = 2*CHUNK_NX;
    offset[1] = 2*CHUNK_NY;
    /* Get and verify info of the chunk at the offset (CHUNK_NX, CHUNK_NY) */
    if(verify_empty_chunk_info(dset, offset) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord on empty chunk failed\n");

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Pclose(cparms) < 0) TEST_ERROR
    if(H5Fclose(basicfile) < 0) TEST_ERROR

    /* Remove the test file */
    remove(filename);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Fclose(basicfile);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_basic_query() */

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
test_failed_attempts(const char *filename, hid_t fapl)
{
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hsize_t  dims[2]  = {NX, NY};/* Dataset dimensions */
    int      data_buf[NX][NY];   /* Input buffer */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2];          /* Offset coordinates of a chunk */
    hsize_t  out_offset[2] = {0, 0}; /* Buffer to get offset coordinates */
    hsize_t  size = 0;           /* Size of an allocated/written chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    haddr_t  addr = 0;           /* Address of an allocated/written chunk */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    hsize_t  ii, jj;             /* Array indices */
    herr_t   ret; /* Temporary returned value for verifying failure */

    TESTING("   Invalid Operations");

    /* Open the file for reading/writing */
    if((chunkfile = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        TEST_ERROR

    /* Create dataspace */
    if((dspace = H5Screate_simple(RANK, dims, NULL)) < 0)
        TEST_ERROR

    /* Create a contiguous dataset */
    dset = H5Dcreate2(chunkfile, CONTIGUOUS_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dset < 0) TEST_ERROR

    /* Initialize the array of data */
    for(ii = 0; ii < NX; ii++)
        for(jj = 0; jj < NY; jj++)
            data_buf[ii][jj] = (int)(ii*jj);

    /* Write the data */
    if(H5Dwrite(dset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data_buf) < 0)
        TEST_ERROR

    /* Close the dataset */
    if(H5Dclose(dset) < 0) TEST_ERROR

    /* Open it again to test the chunk query functions on contiguous dataset */
    if((dset = H5Dopen2(chunkfile, CONTIGUOUS_DSET_NAME, H5P_DEFAULT)) < 0)
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
 * Function:    test_get_chunk_info_v110
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
 *              This function tests the new API functions added for HDFFV-10677:
 *              H5Dget_num_chunks, H5Dget_chunk_info, and
 *              H5Dget_chunk_info_by_coord for low bound beyond 1.8.
 *
 * Date:        October 2018
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_chunk_info_v110(hid_t fapl)
{
    char     filename[FILENAME_BUF_SIZE];   /* File name */
    hid_t    chunkfile = H5I_INVALID_HID;   /* File ID */
    H5F_libver_t low, high;                 /* File format bounds */

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
} /* test_get_chunk_info_v110() */

/*-------------------------------------------------------------------------
 * Function:    test_flt_msk_with_skip_compress
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
test_flt_msk_with_skip_compress(hid_t fapl)
{
    char     filename[FILENAME_BUF_SIZE];   /* File name */
    hid_t    filter_file = H5I_INVALID_HID; /* File ID for filter mask */
    hid_t    dspace = H5I_INVALID_HID;      /* Dataspace ID */
    hid_t    mem_space = H5I_INVALID_HID;   /* Dataspace ID */
    hid_t    dset = H5I_INVALID_HID;        /* Dataset ID */
    hid_t    cparms = H5I_INVALID_HID;      /* Creation plist */
    hid_t    dxpl = H5I_INVALID_HID;        /* Transfer plist */
    hsize_t  dims[2]  = {NX, NY};           /* Dataset dimensions */
    hsize_t  maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED}; /* 2 unlimited dims */
    hsize_t  chunk_dims[2] = {CHUNK_NX, CHUNK_NY};        /* Chunk dimensions */
    int      direct_buf[CHUNK_NX][CHUNK_NY];      /* One chunk of data */
    int      check_chunk[CHUNK_NX][CHUNK_NY];     /* Buffer to read data in */
    int      read_direct_buf[CHUNK_NX][CHUNK_NY]; /* Buffer to read a chunk */
    hsize_t  read_buf_size = 0;  /* buf size */
    unsigned flt_msk = 0;        /* Filter mask */
    unsigned read_flt_msk = 0;   /* Filter mask after direct read */
    hsize_t  offset[2] = {0, 0}; /* Offset coordinates of a chunk */
    hsize_t  nchunks = 0;        /* Number of chunks */
    hsize_t  chk_index = 0;      /* Index of a chunk */
    int      aggression = 9;     /* Compression aggression setting */
    hsize_t  start[2];           /* Start of hyperslab */
    hsize_t  stride[2];          /* Stride of hyperslab */
    hsize_t  count[2];           /* Block count */
    hsize_t  block[2];           /* Block sizes */
    int      ii, jj;             /* Array indices */

    TESTING("getting filter mask when compression filter is skipped");

    /* Create the file */
    h5_fixname(FILTERMASK_FILE, fapl, filename, sizeof filename);

    /* Create a new file. */
    if((filter_file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create file data space with unlimited dimensions. */
    if((dspace = H5Screate_simple(RANK, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create memory data space. */
    if((mem_space = H5Screate_simple(RANK, chunk_dims, NULL)) < 0)
        TEST_ERROR;

    /* Create dataset create property list with chunking and compression
       enabled. */
    if((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    if(H5Pset_chunk( cparms, RANK, chunk_dims) < 0)
        TEST_ERROR;

    if(H5Pset_deflate( cparms, (unsigned ) aggression) < 0)
        TEST_ERROR;

    /* Create a new dataset using cparms creation properties. */
    if((dset = H5Dcreate2(filter_file, SKIP_FILTER_DSET_NAME, H5T_NATIVE_INT, dspace, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Create transfer property list for writing */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0)
        TEST_ERROR;

    /* Initialize data for one chunk */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++)
            direct_buf[ii][jj] = (int)(ii*jj);

    /* Indicate the compression filter is to be skipped.     */
    flt_msk = 0x00000001;

    /* Write a chunk of uncompressed data */
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if(H5Dwrite_chunk(dset, H5P_DEFAULT, flt_msk, offset, CHK_SIZE, direct_buf) < 0)
        TEST_ERROR;

    if(H5Fflush(dset, H5F_SCOPE_LOCAL) < 0)
        TEST_ERROR;

    /* Close and re-open the dataset */
    if(H5Dclose(dset) < 0)
        TEST_ERROR;
    if((dset = H5Dopen2(filter_file, SKIP_FILTER_DSET_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Select hyperslab for the chunk just written in the file */
    start[0]  = CHUNK_NX; start[1]  = CHUNK_NY;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = CHUNK_NX; block[1]  = CHUNK_NY;
    if(H5Sselect_hyperslab(dspace, H5S_SELECT_SET, start, stride, count, block) < 0)
        TEST_ERROR;

    /* Read the chunk back */
    if(H5Dread(dset, H5T_NATIVE_INT, mem_space, dspace, H5P_DEFAULT, check_chunk) < 0)
        TEST_ERROR;

    /* Check that the values read are the same as the values written */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++)
            if(direct_buf[ii][jj] != check_chunk[ii][jj]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", ii, jj);
                HDprintf("    direct_buf=%d, check_chunk=%d\n", direct_buf[ii][jj], check_chunk[ii][jj]);
                TEST_ERROR;
            }

    /* Query chunk storage size */
    if(H5Dget_chunk_storage_size(dset, offset, &read_buf_size) < 0)
        TEST_ERROR;
    if(read_buf_size != CHK_SIZE)
        TEST_ERROR;

    /* Read the raw chunk back with H5Dread_chunk */
    HDmemset(&read_direct_buf, 0, sizeof(read_direct_buf));
    if(H5Dread_chunk(dset, H5P_DEFAULT, offset, &read_flt_msk, read_direct_buf) < 0)
        TEST_ERROR;
    if(read_flt_msk != flt_msk)
        TEST_ERROR;

    /* Check that the direct chunk read is the same as the chunk written */
    for(ii = 0; ii < CHUNK_NX; ii++)
        for(jj = 0; jj < CHUNK_NY; jj++)
            if(direct_buf[ii][jj] != read_direct_buf[ii][jj]) {
                HDprintf("    1. Read different values than written.");
                HDprintf("    At index %d,%d\n", ii, jj);
                HDprintf("    direct_buf=%d, read_direct_buf=%d\n", direct_buf[ii][jj], read_direct_buf[ii][jj]);
                TEST_ERROR;
            }

    /* Get and verify the number of chunks written */
    if(H5Dget_num_chunks(dset, H5S_ALL, &nchunks) < 0) TEST_ERROR
    VERIFY(nchunks, ONE_CHUNK_WRITTEN, "H5Dget_num_chunks, number of chunks");

    /* Get and verify info of the first and only chunk */
    chk_index = 0;
    offset[0] = CHUNK_NX;
    offset[1] = CHUNK_NY;
    if(verify_get_chunk_info(dset, H5S_ALL, chk_index, CHK_SIZE, offset, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info failed\n");

    /* Get info of the chunk at the specified offsets and verify its info */
    if(verify_get_chunk_info_by_coord(dset, offset, CHK_SIZE, flt_msk) == FAIL)
        FAIL_PUTS_ERROR("Verification of H5Dget_chunk_info_by_coord failed\n");

    /* Release resourse */
    if(H5Dclose(dset) < 0) TEST_ERROR
    if(H5Sclose(mem_space) < 0) TEST_ERROR
    if(H5Sclose(dspace) < 0) TEST_ERROR
    if(H5Pclose(cparms) < 0) TEST_ERROR
    if(H5Pclose(dxpl) < 0) TEST_ERROR
    if(H5Fclose(filter_file) < 0) TEST_ERROR

    /* Remove the test file */
    remove(filename);

    PASSED();
    return SUCCEED;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dset);
        H5Sclose(mem_space);
        H5Sclose(dspace);
        H5Pclose(cparms);
        H5Pclose(dxpl);
        H5Fclose(filter_file);
    } H5E_END_TRY;

    H5_FAILED();
    return FAIL;
} /* test_flt_msk_with_skip_compress() */

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

    /* Test basic operations on the chunk query functions */
    nerrors += test_basic_query(fapl) < 0 ? 1 : 0;

    /* Tests getting chunk information of version 1.8 and prior */
    nerrors += test_get_chunk_info_highest_v18(fapl) < 0 ? 1 : 0;

    /* Tests getting chunk information of version 1.10 */
    nerrors += test_get_chunk_info_v110(fapl) < 0 ? 1 : 0;

    /* Tests getting filter mask when compression filter is skipped */
    nerrors += test_flt_msk_with_skip_compress(fapl) < 0 ? 1 : 0;

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
- do the query when extending the dataset (shrink or expand)
- verify that invalid input parameters are handled properly

****************************************************************************/
