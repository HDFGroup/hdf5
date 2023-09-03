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

#include "H5_api_file_test_parallel.h"

static int test_create_file(void);
static int test_open_file(void);
static int test_split_comm_file_access(void);

/*
 * The array of parallel file tests to be performed.
 */
static int (*par_file_tests[])(void) = {
    test_create_file,
    test_open_file,
    test_split_comm_file_access,
};

/*
 * A test to ensure that a file can be created in parallel.
 */
#define FILE_CREATE_TEST_FILENAME "test_file_parallel.h5"
static int
test_create_file(void)
{
    hid_t file_id = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;

    TESTING("H5Fcreate");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fcreate(FILE_CREATE_TEST_FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't create file '%s'\n", FILE_CREATE_TEST_FILENAME);
        goto error;
    }

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a file can be opened in parallel.
 */
static int
test_open_file(void)
{
    hid_t file_id = H5I_INVALID_HID;
    hid_t fapl_id = H5I_INVALID_HID;

    TESTING_MULTIPART("H5Fopen");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Fopen_rdonly)
        {
            TESTING_2("H5Fopen in read-only mode");

            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDONLY, fapl_id)) < 0) {
                H5_FAILED();
                printf("    unable to open file '%s' in read-only mode\n", H5_api_test_parallel_filename);
                PART_ERROR(H5Fopen_rdonly);
            }

            PASSED();
        }
        PART_END(H5Fopen_rdonly);

        if (file_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Fclose(file_id);
            }
            H5E_END_TRY
            file_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Fopen_rdwrite)
        {
            TESTING_2("H5Fopen in read-write mode");

            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
                H5_FAILED();
                printf("    unable to open file '%s' in read-write mode\n", H5_api_test_parallel_filename);
                PART_ERROR(H5Fopen_rdwrite);
            }

            PASSED();
        }
        PART_END(H5Fopen_rdwrite);

        if (file_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Fclose(file_id);
            }
            H5E_END_TRY
            file_id = H5I_INVALID_HID;
        }

        /*
         * XXX: SWMR open flags
         */
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Tests file access by a communicator other than MPI_COMM_WORLD.
 *
 * Splits MPI_COMM_WORLD into two groups, where one (even_comm) contains
 * the original processes of even ranks. The other (odd_comm) contains
 * the original processes of odd ranks. Processes in even_comm create a
 * file, then close it, using even_comm. Processes in old_comm just do
 * a barrier using odd_comm. Then they all do a barrier using MPI_COMM_WORLD.
 * If the file creation and close does not do correct collective action
 * according to the communicator argument, the processes will freeze up
 * sooner or later due to MPI_Barrier calls being mixed up.
 */
#define SPLIT_FILE_COMM_TEST_FILE_NAME "split_comm_file.h5"
static int
test_split_comm_file_access(void)
{
    MPI_Comm comm;
    MPI_Info info    = MPI_INFO_NULL;
    hid_t    file_id = H5I_INVALID_HID;
    hid_t    fapl_id = H5I_INVALID_HID;
    int      is_old;
    int      newrank;
    int      err_occurred = 0;

    TESTING("file access with a split communicator");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file aren't supported with this connector\n");
        return 0;
    }

    /* set up MPI parameters */
    MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
    is_old = mpi_rank % 2;
    if (MPI_SUCCESS != MPI_Comm_split(MPI_COMM_WORLD, is_old, mpi_rank, &comm)) {
        H5_FAILED();
        printf("    failed to split communicator!\n");
        goto error;
    }
    MPI_Comm_rank(comm, &newrank);

    if (is_old) {
        /* odd-rank processes */
        if (MPI_SUCCESS != MPI_Barrier(comm)) {
            err_occurred = 1;
            goto access_end;
        }
    }
    else {
        /* even-rank processes */
        int sub_mpi_rank; /* rank in the sub-comm */

        MPI_Comm_rank(comm, &sub_mpi_rank);

        /* setup file access template */
        if ((fapl_id = create_mpi_fapl(comm, info, true)) < 0) {
            err_occurred = 1;
            goto access_end;
        }

        /* create the file collectively */
        if ((file_id = H5Fcreate(SPLIT_FILE_COMM_TEST_FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_id)) < 0) {
            H5_FAILED();
            printf("    couldn't create file '%s'\n", SPLIT_FILE_COMM_TEST_FILE_NAME);
            err_occurred = 1;
            goto access_end;
        }

        /* close the file */
        if (H5Fclose(file_id) < 0) {
            H5_FAILED();
            printf("    failed to close file '%s'\n", SPLIT_FILE_COMM_TEST_FILE_NAME);
            err_occurred = 1;
            goto access_end;
        }

        /* delete the test file */
        if (H5Fdelete(SPLIT_FILE_COMM_TEST_FILE_NAME, fapl_id) < 0) {
            H5_FAILED();
            printf("    failed to delete file '%s'\n", SPLIT_FILE_COMM_TEST_FILE_NAME);
            err_occurred = 1;
            goto access_end;
        }

        /* Release file-access template */
        if (H5Pclose(fapl_id) < 0) {
            err_occurred = 1;
            goto access_end;
        }
    }
access_end:

    /* Get the collective results about whether an error occurred */
    if (MPI_SUCCESS != MPI_Allreduce(MPI_IN_PLACE, &err_occurred, 1, MPI_INT, MPI_LOR, MPI_COMM_WORLD)) {
        H5_FAILED();
        printf("    MPI_Allreduce failed\n");
        goto error;
    }

    if (err_occurred) {
        H5_FAILED();
        printf("    an error occurred on only some ranks during split-communicator file access! - "
               "collectively failing\n");
        goto error;
    }

    if (MPI_SUCCESS != MPI_Comm_free(&comm)) {
        H5_FAILED();
        printf("    MPI_Comm_free failed\n");
        goto error;
    }

    if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
        H5_FAILED();
        printf("    MPI_Barrier on MPI_COMM_WORLD failed\n");
        goto error;
    }

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * Cleanup temporary test files
 */
static void
cleanup_files(void)
{
    hid_t fapl_id = H5I_INVALID_HID;

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0) {
        if (MAINPROCESS)
            printf("    failed to create FAPL for deleting test files\n");
        return;
    }

    H5Fdelete(FILE_CREATE_TEST_FILENAME, fapl_id);

    /* The below file is deleted as part of the test */
    /* H5Fdelete(SPLIT_FILE_COMM_TEST_FILE_NAME, H5P_DEFAULT); */

    if (H5Pclose(fapl_id) < 0) {
        if (MAINPROCESS)
            printf("    failed to close FAPL used for deleting test files\n");
        return;
    }
}

int
H5_api_file_test_parallel(void)
{
    size_t i;
    int    nerrors;

    if (MAINPROCESS) {
        printf("**********************************************\n");
        printf("*                                            *\n");
        printf("*          API Parallel File Tests           *\n");
        printf("*                                            *\n");
        printf("**********************************************\n\n");
    }

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(par_file_tests); i++) {
        nerrors += (*par_file_tests[i])() ? 1 : 0;

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            if (MAINPROCESS)
                printf("    MPI_Barrier() failed!\n");
        }
    }

    if (MAINPROCESS) {
        printf("\n");
        printf("Cleaning up testing files\n");
    }

    cleanup_files();

    return nerrors;
}
