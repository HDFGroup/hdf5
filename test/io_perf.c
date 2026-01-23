/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the LICENSE file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * This file contains tests which attempt to catch I/O performance regressions
 * by scaling up problem sizes according to the current "TestExpress" setting.
 * While not easy to do reliably, tests should generally be designed to try
 * causing a timeout by running for an extraordinarily long duration when the
 * "TestExpress" value is set to level 0 and the performance problem in question
 * has been regressed on. When the performance problem has NOT been regressed
 * on, tests should run reasonably fast for a "TestExpress" level of 0 in order
 * to facilitate CI testing. For higher "TestExpress" values, tests should
 * generally run quickly, even when the performance problem in question has been
 * regressed on, so that the cumulative runtime of this test program is minimal.
 * The duration that may elapse before a timeout occurs is currently determined
 * by the CMake build system's CTEST_TEST_TIMEOUT / DART_TESTING_TIMEOUT
 * variables (currently 1200 seconds by default).
 */

#include "testframe.h"

/*
 * Test I/O on a dataset with chunks that are non-contiguous with respect to
 * memory layout. This test attempts to catch an I/O performance issue where
 * the library was skipping use of a sieve buffer and performing I/O on chunks
 * element by element, resulting in very bad performance. The number of chunks
 * being written and read scales up with lower values of TestExpress. At a
 * TestExpress setting of 0 the number of chunks should result in a very long
 * time spent in I/O if a sieve buffer isn't being used.
 */
#define FILE_NAME    "chunk_non_contig_mem_io.h5"
#define DATASET_NAME "chunked_dataset"
#define DATASET_TYPE int
#define CHUNK_DIM_0  4194304
static void
chunk_non_contig_mem_io(void H5_ATTR_UNUSED *params)
{
    hsize_t dims[2]       = {0};
    hsize_t mem_dims[2]   = {0};
    hsize_t chunk_dims[2] = {0};
    hsize_t start[2]      = {0};
    hsize_t count[2]      = {0};
    hsize_t block[2]      = {0};
    hid_t   file_id       = H5I_INVALID_HID;
    hid_t   dset_id       = H5I_INVALID_HID;
    hid_t   fapl_id       = H5I_INVALID_HID;
    hid_t   space_id      = H5I_INVALID_HID;
    hid_t   mem_space_id  = H5I_INVALID_HID;
    hid_t   dcpl_id       = H5I_INVALID_HID;
    size_t  num_chunks    = 0;
    size_t  data_size     = 0;
    void   *write_buf     = NULL;
    void   *read_buf      = NULL;
    int     TestExpress   = GetTestExpress();

    switch (TestExpress) {
        case H5_TEST_EXPRESS_EXHAUSTIVE:
            num_chunks = 1024;
            break;
        case H5_TEST_EXPRESS_FULL:
            num_chunks = 512;
            break;
        case H5_TEST_EXPRESS_QUICK:
            num_chunks = 128;
            break;
        case H5_TEST_EXPRESS_SMOKE_TEST:
        default:
            num_chunks = 64;
            break;
    }

    MESSAGE(VERBO_NONE, ("Express test mode set to %d. Testing with %zu chunks\n", TestExpress, num_chunks));

    if ((fapl_id = H5Pcreate(H5P_FILE_ACCESS)) < 0) {
        fprintf(stderr, "Failed to create FAPL\n");
        goto error;
    }

    /* Disable dataset chunk caching */
    if (H5Pset_cache(fapl_id, 0, 0, 0, 0.0) < 0) {
        fprintf(stderr, "Failed to disable dataset chunk caching\n");
        goto error;
    }

    /* Set sieve buffer size to size of a single chunk */
    if (H5Pset_sieve_buf_size(fapl_id, (size_t)CHUNK_DIM_0 * sizeof(DATASET_TYPE)) < 0) {
        fprintf(stderr, "Failed to set data sieve buffer size\n");
        goto error;
    }

    if ((file_id = H5Fcreate(FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0) {
        fprintf(stderr, "Failed to create file '%s'\n", FILE_NAME);
        goto error;
    }

    dims[0] = (hsize_t)CHUNK_DIM_0;
    dims[1] = (hsize_t)num_chunks;
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0) {
        fprintf(stderr, "Failed to create dataspace for dataset\n");
        goto error;
    }

    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        fprintf(stderr, "Failed to create DCPL\n");
        goto error;
    }

    if (H5Pset_fill_time(dcpl_id, H5D_FILL_TIME_NEVER) < 0) {
        fprintf(stderr, "Failed to set fill value writing time on DCPL\n");
        goto error;
    }

    chunk_dims[0] = (hsize_t)CHUNK_DIM_0;
    chunk_dims[1] = 1;
    if (H5Pset_chunk(dcpl_id, 2, chunk_dims) < 0) {
        fprintf(stderr, "Failed to set chunking on DCPL\n");
        goto error;
    }

    if ((dset_id = H5Dcreate2(file_id, DATASET_NAME, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id,
                              H5P_DEFAULT)) < 0) {
        fprintf(stderr, "Failed to create dataset\n");
        goto error;
    }

    data_size = (size_t)CHUNK_DIM_0 * sizeof(DATASET_TYPE);
    if (NULL == (write_buf = malloc(data_size))) {
        fprintf(stderr, "Failed to allocate write buffer\n");
        goto error;
    }

    for (size_t i = 0; i < data_size / sizeof(DATASET_TYPE); i++)
        ((DATASET_TYPE *)write_buf)[i] = (DATASET_TYPE)1;

    mem_dims[0] = 1;
    mem_dims[1] = (hsize_t)CHUNK_DIM_0;
    if ((mem_space_id = H5Screate_simple(2, mem_dims, NULL)) < 0) {
        fprintf(stderr, "Failed to create memory dataspace\n");
        goto error;
    }

    for (size_t chunk = 0; chunk < num_chunks; chunk++) {
        MESSAGE(VERBO_DEF, ("Writing chunk %zu\n", chunk));

        start[0] = 0;
        start[1] = chunk;
        count[0] = 1;
        count[1] = 1;
        block[0] = (hsize_t)CHUNK_DIM_0;
        block[1] = 1;
        if (H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, block) < 0) {
            fprintf(stderr, "Failed to select hyperslab for chunk\n");
            goto error;
        }

        if (H5Dwrite(dset_id, H5T_NATIVE_INT, mem_space_id, space_id, H5P_DEFAULT, write_buf) < 0) {
            fprintf(stderr, "Failed to write to dataset\n");
            goto error;
        }
    }

    if (H5Sclose(mem_space_id) < 0) {
        fprintf(stderr, "Failed to close dataspace\n");
        goto error;
    }

    if (H5Dclose(dset_id) < 0) {
        fprintf(stderr, "Failed to close dataset\n");
        goto error;
    }

    free(write_buf);
    write_buf = NULL;

    if ((dset_id = H5Dopen2(file_id, DATASET_NAME, H5P_DEFAULT)) < 0) {
        fprintf(stderr, "Failed to open dataset\n");
        goto error;
    }

    if (NULL == (read_buf = malloc(2 * data_size))) {
        fprintf(stderr, "Failed to allocate read buffer\n");
        goto error;
    }

    /* Use same shape selection in memory buffer as file selection to
     * avoid iterating element by element when setting up chunk dataspaces
     * for selections
     */
    mem_dims[0] = (hsize_t)CHUNK_DIM_0;
    mem_dims[1] = 2;
    if ((mem_space_id = H5Screate_simple(2, mem_dims, NULL)) < 0) {
        fprintf(stderr, "Failed to create memory dataspace\n");
        goto error;
    }

    start[0] = 0;
    start[1] = 0;
    count[0] = 1;
    count[1] = 1;
    block[0] = (hsize_t)CHUNK_DIM_0;
    block[1] = 1;
    if (H5Sselect_hyperslab(mem_space_id, H5S_SELECT_SET, start, NULL, count, block) < 0) {
        fprintf(stderr, "Failed to select hyperslab in memory dataspace\n");
        goto error;
    }

    for (size_t chunk = 0; chunk < num_chunks; chunk++) {
        MESSAGE(VERBO_DEF, ("Reading chunk %zu\n", chunk));

        start[0] = 0;
        start[1] = chunk;
        count[0] = 1;
        count[1] = 1;
        block[0] = (hsize_t)CHUNK_DIM_0;
        block[1] = 1;
        if (H5Sselect_hyperslab(space_id, H5S_SELECT_SET, start, NULL, count, block) < 0) {
            fprintf(stderr, "Failed to select hyperslab for chunk\n");
            goto error;
        }

        if (H5Dread(dset_id, H5T_NATIVE_INT, mem_space_id, space_id, H5P_DEFAULT, read_buf) < 0) {
            fprintf(stderr, "Failed to read from dataset\n");
            goto error;
        }
    }

    free(read_buf);
    read_buf = NULL;

    if (H5Pclose(dcpl_id) < 0)
        goto error;
    if (H5Sclose(mem_space_id) < 0)
        goto error;
    if (H5Sclose(space_id) < 0)
        goto error;
    if (H5Dclose(dset_id) < 0)
        goto error;
    if (H5Fclose(file_id) < 0)
        goto error;

    if (H5Fdelete(FILE_NAME, fapl_id) < 0)
        goto error;
    if (H5Pclose(fapl_id) < 0)
        goto error;

    return;

error:
    IncTestNumErrs();

    free(write_buf);
    free(read_buf);

    H5E_BEGIN_TRY
    {
        H5Pclose(dcpl_id);
        H5Sclose(mem_space_id);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);

        H5Fdelete(FILE_NAME, fapl_id);
        H5Pclose(fapl_id);
    }
    H5E_END_TRY
}
#undef FILE_NAME
#undef DATASET_NAME
#undef DATASET_TYPE
#undef CHUNK_DIM_0

int
main(int argc, char **argv)
{
    /* Initialize testing framework */
    if (TestInit(argv[0], NULL, NULL, NULL, NULL, 0) < 0) {
        fprintf(stderr, "couldn't initialize testing framework\n");
        exit(EXIT_FAILURE);
    }

    AddTest("chunk_non_contig_mem_io", chunk_non_contig_mem_io, NULL, NULL, NULL, 0,
            "I/O on chunks that are non-contiguous with respect to memory layout");

    /* Display testing information */
    TestInfo(stdout);

    /* Parse command line arguments */
    if (TestParseCmdLine(argc, argv) < 0) {
        fprintf(stderr, "couldn't parse command-line arguments\n");
        TestShutdown();
        exit(EXIT_FAILURE);
    }

    /* Perform requested testing */
    if (PerformTests() < 0) {
        fprintf(stderr, "couldn't run tests\n");
        TestShutdown();
        exit(EXIT_FAILURE);
    }

    /* Display test summary, if requested */
    if (GetTestSummary())
        TestSummary(stdout);

    /* Release test infrastructure */
    if (TestShutdown() < 0) {
        fprintf(stderr, "couldn't shut down testing framework\n");
        exit(EXIT_FAILURE);
    }

    /* Exit failure if errors encountered; else exit success. */
    /* No need to print anything since PerformTests() already does. */
    if (GetTestNumErrs() > 0)
        exit(EXIT_FAILURE);
    else
        exit(EXIT_SUCCESS);
}
