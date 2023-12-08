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
 * XXX: Better documentation for each test about how the selections get
 * split up among MPI ranks.
 */
#include "H5_api_dataset_test_parallel.h"

static int test_write_dataset_data_verification(void);
static int test_write_dataset_independent(void);
static int test_write_dataset_one_proc_0_selection(void);
static int test_write_dataset_one_proc_none_selection(void);
static int test_write_dataset_one_proc_all_selection(void);
static int test_write_dataset_hyper_file_all_mem(void);
static int test_write_dataset_all_file_hyper_mem(void);
static int test_write_dataset_point_file_all_mem(void);
static int test_write_dataset_all_file_point_mem(void);
static int test_write_dataset_hyper_file_point_mem(void);
static int test_write_dataset_point_file_hyper_mem(void);
static int test_read_dataset_one_proc_0_selection(void);
static int test_read_dataset_one_proc_none_selection(void);
static int test_read_dataset_one_proc_all_selection(void);
static int test_read_dataset_hyper_file_all_mem(void);
static int test_read_dataset_all_file_hyper_mem(void);
static int test_read_dataset_point_file_all_mem(void);
static int test_read_dataset_all_file_point_mem(void);
static int test_read_dataset_hyper_file_point_mem(void);
static int test_read_dataset_point_file_hyper_mem(void);

/*
 * Chunking tests
 */
static int test_write_multi_chunk_dataset_same_shape_read(void);
static int test_write_multi_chunk_dataset_diff_shape_read(void);
static int test_overwrite_multi_chunk_dataset_same_shape_read(void);
static int test_overwrite_multi_chunk_dataset_diff_shape_read(void);

/*
 * The array of parallel dataset tests to be performed.
 */
static int (*par_dataset_tests[])(void) = {
    test_write_dataset_data_verification,
    test_write_dataset_independent,
    test_write_dataset_one_proc_0_selection,
    test_write_dataset_one_proc_none_selection,
    test_write_dataset_one_proc_all_selection,
    test_write_dataset_hyper_file_all_mem,
    test_write_dataset_all_file_hyper_mem,
    test_write_dataset_point_file_all_mem,
    test_write_dataset_all_file_point_mem,
    test_write_dataset_hyper_file_point_mem,
    test_write_dataset_point_file_hyper_mem,
    test_read_dataset_one_proc_0_selection,
    test_read_dataset_one_proc_none_selection,
    test_read_dataset_one_proc_all_selection,
    test_read_dataset_hyper_file_all_mem,
    test_read_dataset_all_file_hyper_mem,
    test_read_dataset_point_file_all_mem,
    test_read_dataset_all_file_point_mem,
    test_read_dataset_hyper_file_point_mem,
    test_read_dataset_point_file_hyper_mem,
    test_write_multi_chunk_dataset_same_shape_read,
    test_write_multi_chunk_dataset_diff_shape_read,
    test_overwrite_multi_chunk_dataset_same_shape_read,
    test_overwrite_multi_chunk_dataset_diff_shape_read,
};

/*
 * A test to ensure that data is read back correctly from
 * a dataset after it has been written in parallel. The test
 * covers simple examples of using H5S_ALL selections,
 * hyperslab selections and point selections.
 */
#define DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK 3
#define DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME "dataset_write_data_verification_test"
#define DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1 "dataset_write_data_verification_all"
#define DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2 "dataset_write_data_verification_hyperslab"
#define DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3 "dataset_write_data_verification_points"
static int
test_write_dataset_data_verification(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK];
    hsize_t  count[DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK];
    hsize_t  block[DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK];
    hsize_t *points = NULL;
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING_MULTIPART("verification of dataset data using H5Dwrite then H5Dread");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1,
                              DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1);
        goto error;
    }
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
    }
    H5E_END_TRY
    dset_id = H5I_INVALID_HID;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2,
                              DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2);
        goto error;
    }
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
    }
    H5E_END_TRY
    dset_id = H5I_INVALID_HID;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3,
                              DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                              H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3);
        goto error;
    }
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
    }
    H5E_END_TRY
    dset_id = H5I_INVALID_HID;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(H5Dwrite_all_read)
        {
            bool op_failed = false;

            TESTING_2("H5Dwrite using H5S_ALL then H5Dread");

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1);
                PART_ERROR(H5Dwrite_all_read);
            }

            /*
             * Write data to dataset on rank 0 only. All ranks will read the data back.
             */
            if (MAINPROCESS) {
                for (i = 0, data_size = 1; i < DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK; i++)
                    data_size *= dims[i];
                data_size *= DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE;

                if (NULL != (write_buf = malloc(data_size))) {
                    for (i = 0; i < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; i++)
                        ((int *)write_buf)[i] = (int)i;

                    if (H5Dwrite(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                                 H5P_DEFAULT, write_buf) < 0)
                        op_failed = true;
                }
                else
                    op_failed = true;

                if (write_buf) {
                    free(write_buf);
                    write_buf = NULL;
                }
            }

            if (MPI_SUCCESS !=
                MPI_Allreduce(MPI_IN_PLACE, &op_failed, 1, MPI_C_BOOL, MPI_LAND, MPI_COMM_WORLD)) {
                H5_FAILED();
                printf("    couldn't determine if dataset write on rank 0 succeeded\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (op_failed == true) {
                H5_FAILED();
                printf("    dataset write on rank 0 failed!\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                PART_ERROR(H5Dwrite_all_read);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                PART_ERROR(H5Dwrite_all_read);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                PART_ERROR(H5Dwrite_all_read);
            }
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
                H5_FAILED();
                printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
                PART_ERROR(H5Dwrite_all_read);
            }
            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }
            if ((group_id =
                     H5Gopen2(container_group, DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container sub-group '%s'\n",
                       DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1);
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (NULL ==
                (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_all_read);
            }

            if (H5Dread(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME1);
                PART_ERROR(H5Dwrite_all_read);
            }

            for (i = 0; i < (hsize_t)space_npoints; i++)
                if (((int *)read_buf)[i] != (int)i) {
                    H5_FAILED();
                    printf("    H5S_ALL selection data verification failed\n");
                    PART_ERROR(H5Dwrite_all_read);
                }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_all_read);

        if (write_buf) {
            free(write_buf);
            write_buf = NULL;
        }
        if (read_buf) {
            free(read_buf);
            read_buf = NULL;
        }
        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Dwrite_hyperslab_read)
        {
            TESTING_2("H5Dwrite using hyperslab selection then H5Dread");

            for (i = 1, data_size = 1; i < DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; i++)
                ((int *)write_buf)[i] = mpi_rank;

            /* Each MPI rank writes to a single row in the second dimension
             * and the entirety of the following dimensions. The combined
             * selections from all MPI ranks spans the first dimension.
             */
            for (i = 0; i < DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK; i++) {
                if (i == 0) {
                    start[i] = (hsize_t)mpi_rank;
                    block[i] = 1;
                }
                else {
                    start[i] = 0;
                    block[i] = dims[i];
                }

                stride[i] = 1;
                count[i]  = 1;
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            {
                hsize_t mdims[] = {data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_hyperslab_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
                H5_FAILED();
                printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }
            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }
            if ((group_id =
                     H5Gopen2(container_group, DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container sub-group '%s'\n",
                       DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (NULL ==
                (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            if (H5Dread(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME2);
                PART_ERROR(H5Dwrite_hyperslab_read);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;

                for (j = 0; j < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; j++) {
                    if (((int *)
                             read_buf)[j + (i * (data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE))] !=
                        (int)i) {
                        H5_FAILED();
                        printf("    hyperslab selection data verification failed\n");
                        PART_ERROR(H5Dwrite_hyperslab_read);
                    }
                }
            }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_hyperslab_read);

        if (write_buf) {
            free(write_buf);
            write_buf = NULL;
        }
        if (read_buf) {
            free(read_buf);
            read_buf = NULL;
        }
        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (mspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(mspace_id);
            }
            H5E_END_TRY
            mspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        PART_BEGIN(H5Dwrite_point_sel_read)
        {
            TESTING_2("H5Dwrite using point selection then H5Dread");

            for (i = 1, data_size = 1; i < DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            /* Use different data than the previous test to ensure that the data actually changed. */
            for (i = 0; i < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; i++)
                ((int *)write_buf)[i] = mpi_size - mpi_rank;

            if (NULL == (points = malloc(DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK *
                                         (data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE) *
                                         sizeof(hsize_t)))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for point selection\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            /* Each MPI rank writes to a single row in the second dimension
             * and the entirety of the following dimensions. The combined
             * selections from all MPI ranks spans the first dimension.
             */
            for (i = 0; i < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; i++) {
                size_t j;

                for (j = 0; j < DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK; j++) {
                    size_t idx = (i * DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK) + j;

                    if (j == 0)
                        points[idx] = (hsize_t)mpi_rank;
                    else if (j != DATASET_WRITE_DATA_VERIFY_TEST_SPACE_RANK - 1)
                        points[idx] = i / dims[j + 1];
                    else
                        points[idx] = i % dims[j];
                }
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Sselect_elements(fspace_id, H5S_SELECT_SET,
                                   data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE, points) < 0) {
                H5_FAILED();
                printf("    couldn't select elements in dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            {
                hsize_t mdims[] = {data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    PART_ERROR(H5Dwrite_point_sel_read);
                }
            }

            if (H5Dwrite(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
                H5_FAILED();
                printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
                PART_ERROR(H5Dwrite_point_sel_read);
            }
            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }
            if ((group_id =
                     H5Gopen2(container_group, DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container sub-group '%s'\n",
                       DATASET_WRITE_DATA_VERIFY_TEST_GROUP_NAME);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((fspace_id = H5Dget_space(dset_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataset dataspace\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
                H5_FAILED();
                printf("    couldn't get dataspace num points\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (NULL ==
                (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            if (H5Dread(dset_id, DATASET_WRITE_DATA_VERIFY_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                        read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_DATA_VERIFY_TEST_DSET_NAME3);
                PART_ERROR(H5Dwrite_point_sel_read);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;

                for (j = 0; j < data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE; j++) {
                    if (((int *)
                             read_buf)[j + (i * (data_size / DATASET_WRITE_DATA_VERIFY_TEST_DTYPE_SIZE))] !=
                        (mpi_size - (int)i)) {
                        H5_FAILED();
                        printf("    point selection data verification failed\n");
                        PART_ERROR(H5Dwrite_point_sel_read);
                    }
                }
            }

            if (read_buf) {
                free(read_buf);
                read_buf = NULL;
            }

            PASSED();
        }
        PART_END(H5Dwrite_point_sel_read);

        if (write_buf) {
            free(write_buf);
            write_buf = NULL;
        }
        if (read_buf) {
            free(read_buf);
            read_buf = NULL;
        }
        if (points) {
            free(points);
            points = NULL;
        }
        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (mspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(mspace_id);
            }
            H5E_END_TRY
            mspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (points) {
        free(points);
        points = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that independent dataset writes function
 * as expected. First, two datasets are created in the file.
 * Then, the even MPI ranks first write to dataset 1, followed
 * by dataset 2. The odd MPI ranks first write to dataset 2,
 * followed by dataset 1. After this, the data is read back from
 * each dataset and verified.
 */
#define DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK 3
#define DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_INDEPENDENT_WRITE_TEST_GROUP_NAME "independent_dataset_write_test"
#define DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME1 "dset1"
#define DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME2 "dset2"
static int
test_write_dataset_independent(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK];
    hsize_t  count[DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK];
    hsize_t  block[DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id1 = H5I_INVALID_HID, dset_id2 = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("independent writing to different datasets by different ranks");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_INDEPENDENT_WRITE_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    /*
     * Setup dimensions of overall datasets and slabs local
     * to the MPI rank.
     */
    if (generate_random_parallel_dimensions(DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    /* create a dataset collectively */
    if ((dset_id1 = H5Dcreate2(group_id, DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME1,
                               DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to create first dataset\n");
        goto error;
    }
    if ((dset_id2 = H5Dcreate2(group_id, DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME2,
                               DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT, H5P_DEFAULT,
                               H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    failed to create second dataset\n");
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE;

    if (NULL == (write_buf = malloc(data_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset write\n");
        goto error;
    }

    for (i = 0; i < data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE; i++)
        ((int *)write_buf)[i] = mpi_rank;

    for (i = 0; i < DATASET_INDEPENDENT_WRITE_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset write\n");
        goto error;
    }

    {
        hsize_t mdims[] = {data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE};

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    /*
     * To test the independent orders of writes between processes, all
     * even number processes write to dataset1 first, then dataset2.
     * All odd number processes write to dataset2 first, then dataset1.
     */
    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (mpi_rank % 2 == 0) {
            if (H5Dwrite(dset_id1, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    even ranks failed to write to dataset 1\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }
            if (H5Dwrite(dset_id2, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    even ranks failed to write to dataset 2\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }
        }
        else {
            if (H5Dwrite(dset_id2, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    odd ranks failed to write to dataset 2\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }
            if (H5Dwrite(dset_id1, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, mspace_id, fspace_id,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    odd ranks failed to write to dataset 1\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    H5Sclose(mspace_id);
    mspace_id = H5I_INVALID_HID;
    H5Sclose(fspace_id);
    fspace_id = H5I_INVALID_HID;
    H5Dclose(dset_id1);
    dset_id1 = H5I_INVALID_HID;
    H5Dclose(dset_id2);
    dset_id2 = H5I_INVALID_HID;

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_INDEPENDENT_WRITE_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id1 = H5Dopen2(group_id, DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME1, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME1);
        goto error;
    }
    if ((dset_id2 = H5Dopen2(group_id, DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME2);
        goto error;
    }

    /*
     * Verify that data has been written correctly.
     */
    if ((fspace_id = H5Dget_space(dset_id1)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL == (read_buf = malloc((hsize_t)space_npoints * DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id1, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME1);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        for (j = 0; j < data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE; j++) {
            if (((int *)read_buf)[j + (i * (data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE))] !=
                (int)i) {
                H5_FAILED();
                printf("    dataset 1 data verification failed\n");
                goto error;
            }
        }
    }

    if (H5Dread(dset_id2, DATASET_INDEPENDENT_WRITE_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_INDEPENDENT_WRITE_TEST_DSET_NAME2);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        for (j = 0; j < data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE; j++) {
            if (((int *)read_buf)[j + (i * (data_size / DATASET_INDEPENDENT_WRITE_TEST_DTYPE_SIZE))] !=
                (int)i) {
                H5_FAILED();
                printf("    dataset 2 data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id1) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id2) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id1);
        H5Dclose(dset_id2);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * one of the MPI ranks select 0 rows in a hyperslab selection.
 */
#define DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK 2
#define DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_ONE_PROC_0_SEL_TEST_GROUP_NAME "one_rank_0_sel_write_test"
#define DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME  "one_rank_0_sel_dset"
static int
test_write_dataset_one_proc_0_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  count[DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  block[DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with one rank selecting 0 rows");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_ONE_PROC_0_SEL_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_ONE_PROC_0_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME,
                              DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE;

    BEGIN_INDEPENDENT_OP(write_buf_alloc)
    {
        if (!MAINPROCESS) {
            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(write_buf_alloc);
            }

            for (i = 0; i < data_size / DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE; i++)
                ((int *)write_buf)[i] = mpi_rank;
        }
    }
    END_INDEPENDENT_OP(write_buf_alloc);

    for (i = 0; i < DATASET_WRITE_ONE_PROC_0_SEL_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = MAINPROCESS ? 0 : 1;
        }
        else {
            start[i] = 0;
            block[i] = MAINPROCESS ? 0 : dims[i];
        }

        stride[i] = 1;
        count[i]  = MAINPROCESS ? 0 : 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset write\n");
        goto error;
    }

    {
        hsize_t mdims[] = {data_size / DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE};

        if (MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (H5Dwrite(dset_id, DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                     write_buf) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_write);
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_WRITE_ONE_PROC_0_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n", DATASET_WRITE_ONE_PROC_0_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL == (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_ONE_PROC_0_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        if (i != 0) {
            for (j = 0; j < data_size / DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE; j++) {
                if (((int *)read_buf)[j + (i * (data_size / DATASET_WRITE_ONE_PROC_0_SEL_TEST_DTYPE_SIZE))] !=
                    (int)i) {
                    H5_FAILED();
                    printf("    data verification failed\n");
                    goto error;
                }
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * one of the MPI ranks call H5Sselect_none.
 */
#define DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK 2
#define DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_GROUP_NAME "one_rank_none_sel_write_test"
#define DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME  "one_rank_none_sel_dset"
static int
test_write_dataset_one_proc_none_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  count[DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  block[DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with one rank using 'none' selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME,
                              DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE;

    BEGIN_INDEPENDENT_OP(write_buf_alloc)
    {
        if (!MAINPROCESS) {
            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(write_buf_alloc);
            }

            for (i = 0; i < data_size / DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE; i++)
                ((int *)write_buf)[i] = mpi_rank;
        }
    }
    END_INDEPENDENT_OP(write_buf_alloc);

    for (i = 0; i < DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    BEGIN_INDEPENDENT_OP(set_space_sel)
    {
        if (MAINPROCESS) {
            if (H5Sselect_none(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'none' selection for dataset write\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
        else {
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
    }
    END_INDEPENDENT_OP(set_space_sel);

    {
        hsize_t mdims[] = {data_size / DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE};

        if (MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (H5Dwrite(dset_id, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id,
                     H5P_DEFAULT, write_buf) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_write);
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        if (i != 0) {
            for (j = 0; j < data_size / DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE; j++) {
                if (((int *)
                         read_buf)[j + (i * (data_size / DATASET_WRITE_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE))] !=
                    (int)i) {
                    H5_FAILED();
                    printf("    data verification failed\n");
                    goto error;
                }
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * one of the MPI ranks use an ALL selection, while the other
 * ranks write nothing.
 */
#define DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_SPACE_RANK 2
#define DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_GROUP_NAME "one_rank_all_sel_write_test"
#define DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME  "one_rank_all_sel_dset"
static int
test_write_dataset_one_proc_all_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with one rank using all selection; others none selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME,
                              DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE;

    BEGIN_INDEPENDENT_OP(write_buf_alloc)
    {
        if (MAINPROCESS) {
            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(write_buf_alloc);
            }

            for (i = 0; i < data_size / DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE; i++)
                ((int *)write_buf)[i] = (int)i;
        }
    }
    END_INDEPENDENT_OP(write_buf_alloc);

    BEGIN_INDEPENDENT_OP(set_space_sel)
    {
        if (MAINPROCESS) {
            if (H5Sselect_all(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'all' selection for dataset write\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
        else {
            if (H5Sselect_none(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'none' selection for dataset write\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
    }
    END_INDEPENDENT_OP(set_space_sel);

    {
        hsize_t mdims[] = {data_size / DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE};

        if (!MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (H5Dwrite(dset_id, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id,
                     H5P_DEFAULT, write_buf) < 0) {
            H5_FAILED();
            printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_write);
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < data_size / DATASET_WRITE_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE; i++) {
        if (((int *)read_buf)[i] != (int)i) {
            H5_FAILED();
            printf("    data verification failed\n");
            goto error;
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * a hyperslab selection in the file dataspace and an all selection
 * in the memory dataspace.
 *
 * XXX: Currently pulls from invalid memory locations.
 */
#ifdef BROKEN
#define DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK 2
#define DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_GROUP_NAME "hyper_sel_file_all_sel_mem_write_test"
#define DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME  "hyper_sel_file_all_sel_mem_dset"
#endif
static int
test_write_dataset_hyper_file_all_mem(void)
{
#ifdef BROKEN
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK];
    hsize_t  count[DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK];
    hsize_t  block[DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;
#endif

    TESTING("write to dataset with hyperslab sel. for file space; all sel. for memory");

#ifdef BROKEN
    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME,
                              DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE;

    if (NULL == (write_buf = malloc(data_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset write\n");
        goto error;
    }

    for (i = 0; i < data_size / DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE; i++)
        ((int *)write_buf)[i] = mpi_rank;

    for (i = 0; i < DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset write\n");
        goto error;
    }

    if (H5Dwrite(dset_id, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_DTYPE, H5S_ALL, fspace_id, H5P_DEFAULT,
                 write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id =
             H5Gopen2(container_group, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        for (j = 0; j < data_size / DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE; j++) {
            if (((int *)read_buf)[j + (i * (data_size / DATASET_WRITE_HYPER_FILE_ALL_MEM_TEST_DTYPE_SIZE))] !=
                (int)i) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();
#else
    SKIPPED();
#endif

    return 0;

#ifdef BROKEN
error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
#endif
}

/*
 * A test to ensure that a dataset can be written to by having
 * an all selection in the file dataspace and a hyperslab
 * selection in the memory dataspace.
 */
#define DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK 2
#define DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME "all_sel_file_hyper_sel_mem_write_test"
#define DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME  "all_sel_file_hyper_sel_mem_dset"
static int
test_write_dataset_all_file_hyper_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with all sel. for file space; hyperslab sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME,
                              DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE;

    BEGIN_INDEPENDENT_OP(write_buf_alloc)
    {
        if (MAINPROCESS) {
            /*
             * Allocate twice the amount of memory needed and leave "holes" in the memory
             * buffer in order to prove that the mapping from hyperslab selection <-> all
             * selection works correctly.
             */
            if (NULL == (write_buf = malloc(2 * data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(write_buf_alloc);
            }

            for (i = 0; i < 2 * (data_size / DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE); i++) {
                /* Write actual data to even indices */
                if (i % 2 == 0)
                    ((int *)write_buf)[i] = (int)((i / 2) + (i % 2));
                else
                    ((int *)write_buf)[i] = 0;
            }
        }
    }
    END_INDEPENDENT_OP(write_buf_alloc);

    /*
     * Only have rank 0 perform the dataset write, as writing the entire dataset on all ranks
     * might be stressful on system resources. There's also no guarantee as to what the outcome
     * would be, since the writes would be overlapping with each other.
     */
    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (MAINPROCESS) {
            hsize_t start[1]  = {0};
            hsize_t stride[1] = {2};
            hsize_t count[1]  = {data_size / DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE};
            hsize_t block[1]  = {1};
            hsize_t mdims[]   = {2 * (data_size / DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE)};

            if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                H5_FAILED();
                printf("    couldn't create memory dataspace\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }

            if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }

            if (H5Dwrite(dset_id, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_write);
            }
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id =
             H5Gopen2(container_group, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < data_size / DATASET_WRITE_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE; i++) {
        if (((int *)read_buf)[i] != (int)i) {
            H5_FAILED();
            printf("    data verification failed\n");
            goto error;
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * a point selection in the file dataspace and an all selection
 * in the memory dataspace.
 */
static int
test_write_dataset_point_file_all_mem(void)
{
    TESTING("write to dataset with point sel. for file space; all sel. for memory");

    SKIPPED();

    return 0;
}

/*
 * A test to ensure that a dataset can be written to by having
 * an all selection in the file dataspace and a point selection
 * in the memory dataspace.
 */
#define DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_SPACE_RANK 2
#define DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_GROUP_NAME "all_sel_file_point_sel_mem_write_test"
#define DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME  "all_sel_file_point_sel_mem_dset"
static int
test_write_dataset_all_file_point_mem(void)
{
    hssize_t space_npoints;
    hsize_t *points = NULL;
    hsize_t *dims   = NULL;
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with all sel. for file space; point sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_GROUP_NAME, H5P_DEFAULT,
                               H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME,
                              DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0, data_size = 1; i < DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE;

    BEGIN_INDEPENDENT_OP(write_buf_alloc)
    {
        if (MAINPROCESS) {
            /*
             * Allocate twice the amount of memory needed and leave "holes" in the memory
             * buffer in order to prove that the mapping from point selection <-> all
             * selection works correctly.
             */
            if (NULL == (write_buf = malloc(2 * data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(write_buf_alloc);
            }

            for (i = 0; i < 2 * (data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE); i++) {
                /* Write actual data to even indices */
                if (i % 2 == 0)
                    ((int *)write_buf)[i] = (int)((i / 2) + (i % 2));
                else
                    ((int *)write_buf)[i] = 0;
            }
        }
    }
    END_INDEPENDENT_OP(write_buf_alloc);

    /*
     * Only have rank 0 perform the dataset write, as writing the entire dataset on all ranks
     * might be stressful on system resources. There's also no guarantee as to what the outcome
     * would be, since the writes would be overlapping with each other.
     */
    BEGIN_INDEPENDENT_OP(dset_write)
    {
        if (MAINPROCESS) {
            hsize_t mdims[] = {2 * (data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE)};
            int     j;

            if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                H5_FAILED();
                printf("    couldn't create memory dataspace\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }

            if (NULL == (points = malloc((data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE) *
                                         sizeof(hsize_t)))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for point selection\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }

            /* Select every other point in the 1-dimensional memory dataspace */
            for (i = 0, j = 0; i < 2 * (data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE); i++) {
                if (i % 2 == 0)
                    points[j++] = (hsize_t)i;
            }

            if (H5Sselect_elements(mspace_id, H5S_SELECT_SET,
                                   data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE,
                                   points) < 0) {
                H5_FAILED();
                printf("    couldn't set point selection for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_write);
            }

            if (H5Dwrite(dset_id, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_write);
            }
        }
    }
    END_INDEPENDENT_OP(dset_write);

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (points) {
        free(points);
        points = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id =
             H5Gopen2(container_group, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < data_size / DATASET_WRITE_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE; i++) {
        if (((int *)read_buf)[i] != (int)i) {
            H5_FAILED();
            printf("    data verification failed\n");
            goto error;
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * a hyperslab selection in the file dataspace and a point
 * selection in the memory dataspace.
 */
#define DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK 2
#define DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME "hyper_sel_file_point_sel_mem_write_test"
#define DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME  "hyper_sel_file_point_sel_mem_dset"
static int
test_write_dataset_hyper_file_point_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims   = NULL;
    hsize_t *points = NULL;
    hsize_t  start[DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  count[DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  block[DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with hyperslab sel. for file space; point sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME,
                              DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE;

    /*
     * Allocate twice the amount of memory needed and leave "holes" in the memory
     * buffer in order to prove that the mapping from point selection <-> hyperslab
     * selection works correctly.
     */
    if (NULL == (write_buf = malloc(2 * data_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset write\n");
        goto error;
    }

    for (i = 0; i < 2 * (data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE); i++) {
        /* Write actual data to even indices */
        if (i % 2 == 0)
            ((int *)write_buf)[i] = mpi_rank;
        else
            ((int *)write_buf)[i] = 0;
    }

    for (i = 0; i < DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset write\n");
        goto error;
    }

    {
        hsize_t mdims[] = {2 * (data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE)};
        int     j;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }

        if (NULL == (points = malloc((data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE) *
                                     sizeof(hsize_t)))) {
            H5_FAILED();
            printf("    couldn't allocate buffer for point selection\n");
            goto error;
        }

        /* Select every other point in the 1-dimensional memory dataspace */
        for (i = 0, j = 0; i < 2 * (data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE); i++) {
            if (i % 2 == 0)
                points[j++] = (hsize_t)i;
        }

        if (H5Sselect_elements(mspace_id, H5S_SELECT_SET,
                               data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE, points) < 0) {
            H5_FAILED();
            printf("    couldn't set point selection for dataset write\n");
            goto error;
        }
    }

    if (H5Dwrite(dset_id, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, fspace_id,
                 H5P_DEFAULT, write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (points) {
        free(points);
        points = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        for (j = 0; j < data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE; j++) {
            if (((int *)
                     read_buf)[j + (i * (data_size / DATASET_WRITE_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE))] !=
                (int)i) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be written to by having
 * a point selection in the file dataspace and a hyperslab
 * selection in the memory dataspace.
 */
#define DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK 2
#define DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME "point_sel_file_hyper_sel_mem_write_test"
#define DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME  "point_sel_file_hyper_sel_mem_dset"
static int
test_write_dataset_point_file_hyper_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims   = NULL;
    hsize_t *points = NULL;
    size_t   i, data_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("write to dataset with point sel. for file space; hyperslab sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;

    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }

    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }

    if ((group_id = H5Gcreate2(container_group, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create container sub-group '%s'\n",
               DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if (generate_random_parallel_dimensions(DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    if ((fspace_id = H5Screate_simple(DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK, dims, NULL)) < 0)
        TEST_ERROR;

    if ((dset_id = H5Dcreate2(group_id, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME,
                              DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                              H5P_DEFAULT, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't create dataset '%s'\n", DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 1, data_size = 1; i < DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE;

    /*
     * Allocate twice the amount of memory needed and leave "holes" in the memory
     * buffer in order to prove that the mapping from hyperslab selection <-> point
     * selection works correctly.
     */
    if (NULL == (write_buf = malloc(2 * data_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset write\n");
        goto error;
    }

    for (i = 0; i < 2 * (data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE); i++) {
        /* Write actual data to even indices */
        if (i % 2 == 0)
            ((int *)write_buf)[i] = mpi_rank;
        else
            ((int *)write_buf)[i] = 0;
    }

    if (NULL == (points = malloc((data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE) *
                                 DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK * sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for point selection\n");
        goto error;
    }

    for (i = 0; i < data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE; i++) {
        size_t j;

        for (j = 0; j < DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK; j++) {
            size_t idx = (i * (size_t)DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK) + j;

            if (j == 0)
                points[idx] = (hsize_t)mpi_rank;
            else if (j != (size_t)DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK - 1)
                points[idx] = i / dims[j + 1];
            else
                points[idx] = i % dims[j];
        }
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET,
                           data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE, points) < 0) {
        H5_FAILED();
        printf("    couldn't set point selection for dataset write\n");
        goto error;
    }

    {
        hsize_t start[1]  = {0};
        hsize_t stride[1] = {2};
        hsize_t count[1]  = {data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE};
        hsize_t block[1]  = {1};
        hsize_t mdims[]   = {2 * (data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE)};

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }

        if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
            H5_FAILED();
            printf("    couldn't set hyperslab selection for dataset write\n");
            goto error;
        }
    }

    if (H5Dwrite(dset_id, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, fspace_id,
                 H5P_DEFAULT, write_buf) < 0) {
        H5_FAILED();
        printf("    couldn't write to dataset '%s'\n", DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }
    if (points) {
        free(points);
        points = NULL;
    }
    if (mspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(mspace_id);
        }
        H5E_END_TRY
        mspace_id = H5I_INVALID_HID;
    }
    if (fspace_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Sclose(fspace_id);
        }
        H5E_END_TRY
        fspace_id = H5I_INVALID_HID;
    }
    if (dset_id >= 0) {
        H5E_BEGIN_TRY
        {
            H5Dclose(dset_id);
        }
        H5E_END_TRY
        dset_id = H5I_INVALID_HID;
    }

    /*
     * Close and re-open the file to ensure that the data gets written.
     */
    if (H5Gclose(group_id) < 0) {
        H5_FAILED();
        printf("    failed to close test's container group\n");
        goto error;
    }
    if (H5Gclose(container_group) < 0) {
        H5_FAILED();
        printf("    failed to close container group\n");
        goto error;
    }
    if (H5Fclose(file_id) < 0) {
        H5_FAILED();
        printf("    failed to close file for data flushing\n");
        goto error;
    }
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    if (NULL ==
        (read_buf = malloc((hsize_t)space_npoints * DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (H5Dread(dset_id, DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)mpi_size; i++) {
        size_t j;

        for (j = 0; j < data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE; j++) {
            if (((int *)
                     read_buf)[j + (i * (data_size / DATASET_WRITE_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE))] !=
                (int)i) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * one of the MPI ranks select 0 rows in a hyperslab selection.
 */
#define DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK 2
#define DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_ONE_PROC_0_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_ONE_PROC_0_SEL_TEST_GROUP_NAME "one_rank_0_sel_read_test"
#define DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME  "one_rank_0_sel_dset"
static int
test_read_dataset_one_proc_0_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  count[DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    hsize_t  block[DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK];
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with one rank selecting 0 rows");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_ONE_PROC_0_SEL_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_ONE_PROC_0_SEL_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME,
                                      DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_ONE_PROC_0_SEL_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc = (data_size / DATASET_READ_ONE_PROC_0_SEL_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_ONE_PROC_0_SEL_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_READ_ONE_PROC_0_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n", DATASET_READ_ONE_PROC_0_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    BEGIN_INDEPENDENT_OP(read_buf_alloc)
    {
        if (!MAINPROCESS) {
            read_buf_size =
                ((size_t)(space_npoints / mpi_size) * DATASET_READ_ONE_PROC_0_SEL_TEST_DTYPE_SIZE);

            if (NULL == (read_buf = malloc(read_buf_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                INDEPENDENT_OP_ERROR(read_buf_alloc);
            }
        }
    }
    END_INDEPENDENT_OP(read_buf_alloc);

    {
        hsize_t mdims[] = {(hsize_t)space_npoints / (hsize_t)mpi_size};

        if (MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_READ_ONE_PROC_0_SEL_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = MAINPROCESS ? 0 : 1;
        }
        else {
            start[i] = 0;
            block[i] = MAINPROCESS ? 0 : dims[i];
        }

        stride[i] = 1;
        count[i]  = MAINPROCESS ? 0 : 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset read\n");
        goto error;
    }

    BEGIN_INDEPENDENT_OP(dset_read)
    {
        if (H5Dread(dset_id, DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                    read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n", DATASET_READ_ONE_PROC_0_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_read);
        }
    }
    END_INDEPENDENT_OP(dset_read);

    BEGIN_INDEPENDENT_OP(data_verify)
    {
        if (!MAINPROCESS) {
            for (i = 0; i < (size_t)space_npoints / (size_t)mpi_size; i++) {
                if (((int *)read_buf)[i] != mpi_rank) {
                    H5_FAILED();
                    printf("    data verification failed\n");
                    INDEPENDENT_OP_ERROR(data_verify);
                }
            }
        }
    }
    END_INDEPENDENT_OP(data_verify);

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * one of the MPI ranks call H5Sselect_none.
 */
#define DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK 2
#define DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_ONE_PROC_NONE_SEL_TEST_GROUP_NAME "one_rank_none_sel_read_test"
#define DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME  "one_rank_none_sel_dset"
static int
test_read_dataset_one_proc_none_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    hsize_t  start[DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  count[DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    hsize_t  block[DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK];
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with one rank using 'none' selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_ONE_PROC_NONE_SEL_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_ONE_PROC_NONE_SEL_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK, dims, NULL)) <
                0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME,
                                      DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc = (data_size / DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_READ_ONE_PROC_NONE_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_READ_ONE_PROC_NONE_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    BEGIN_INDEPENDENT_OP(read_buf_alloc)
    {
        if (!MAINPROCESS) {
            read_buf_size =
                ((size_t)(space_npoints / mpi_size) * DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE);

            if (NULL == (read_buf = malloc(read_buf_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                INDEPENDENT_OP_ERROR(read_buf_alloc);
            }
        }
    }
    END_INDEPENDENT_OP(read_buf_alloc);

    {
        hsize_t mdims[] = {(hsize_t)space_npoints / (hsize_t)mpi_size};

        if (MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_READ_ONE_PROC_NONE_SEL_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    BEGIN_INDEPENDENT_OP(set_space_sel)
    {
        if (MAINPROCESS) {
            if (H5Sselect_none(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'none' selection for dataset read\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
        else {
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset read\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
    }
    END_INDEPENDENT_OP(set_space_sel);

    BEGIN_INDEPENDENT_OP(dset_read)
    {
        if (H5Dread(dset_id, DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n", DATASET_READ_ONE_PROC_NONE_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_read);
        }
    }
    END_INDEPENDENT_OP(dset_read);

    BEGIN_INDEPENDENT_OP(data_verify)
    {
        if (!MAINPROCESS) {
            for (i = 0; i < (size_t)space_npoints / (size_t)mpi_size; i++) {
                if (((int *)read_buf)[i] != mpi_rank) {
                    H5_FAILED();
                    printf("    data verification failed\n");
                    INDEPENDENT_OP_ERROR(data_verify);
                }
            }
        }
    }
    END_INDEPENDENT_OP(data_verify);

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * one of the MPI ranks use an ALL selection, while the other
 * ranks read nothing.
 */
#define DATASET_READ_ONE_PROC_ALL_SEL_TEST_SPACE_RANK 2
#define DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_ONE_PROC_ALL_SEL_TEST_GROUP_NAME "one_rank_all_sel_read_test"
#define DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME  "one_rank_all_sel_dset"
static int
test_read_dataset_one_proc_all_selection(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with one rank using all selection; others none selection");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_ONE_PROC_ALL_SEL_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_ONE_PROC_ALL_SEL_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_ONE_PROC_ALL_SEL_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_READ_ONE_PROC_ALL_SEL_TEST_SPACE_RANK, dims, NULL)) <
                0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME,
                                      DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_ONE_PROC_ALL_SEL_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc = (data_size / DATASET_READ_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n", DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_READ_ONE_PROC_ALL_SEL_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n", DATASET_READ_ONE_PROC_ALL_SEL_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    BEGIN_INDEPENDENT_OP(read_buf_alloc)
    {
        if (MAINPROCESS) {
            read_buf_size = (size_t)space_npoints * DATASET_READ_ONE_PROC_ALL_SEL_TEST_DTYPE_SIZE;

            if (NULL == (read_buf = malloc(read_buf_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                INDEPENDENT_OP_ERROR(read_buf_alloc);
            }
        }
    }
    END_INDEPENDENT_OP(read_buf_alloc);

    {
        hsize_t mdims[] = {(hsize_t)space_npoints};

        if (!MAINPROCESS)
            mdims[0] = 0;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }
    }

    BEGIN_INDEPENDENT_OP(set_space_sel)
    {
        if (MAINPROCESS) {
            if (H5Sselect_all(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'all' selection for dataset read\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
        else {
            if (H5Sselect_none(fspace_id) < 0) {
                H5_FAILED();
                printf("    couldn't set 'none' selection for dataset read\n");
                INDEPENDENT_OP_ERROR(set_space_sel);
            }
        }
    }
    END_INDEPENDENT_OP(set_space_sel);

    BEGIN_INDEPENDENT_OP(dset_read)
    {
        if (H5Dread(dset_id, DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                    read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n", DATASET_READ_ONE_PROC_ALL_SEL_TEST_DSET_NAME);
            INDEPENDENT_OP_ERROR(dset_read);
        }
    }
    END_INDEPENDENT_OP(dset_read);

    BEGIN_INDEPENDENT_OP(data_verify)
    {
        if (MAINPROCESS) {
            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc = (size_t)(space_npoints / mpi_size);

                for (j = 0; j < elem_per_proc; j++) {
                    int idx = (int)((i * elem_per_proc) + j);

                    if (((int *)read_buf)[idx] != (int)i) {
                        H5_FAILED();
                        printf("    data verification failed\n");
                        INDEPENDENT_OP_ERROR(data_verify);
                    }
                }
            }
        }
    }
    END_INDEPENDENT_OP(data_verify);

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * a hyperslab selection in the file dataspace and an all
 * selection in the memory dataspace.
 */
static int
test_read_dataset_hyper_file_all_mem(void)
{
    TESTING("read from dataset with hyperslab sel. for file space; all sel. for memory");

    SKIPPED();

    return 0;
}

/*
 * A test to ensure that a dataset can be read from by having
 * an all selection in the file dataspace and a hyperslab
 * selection in the memory dataspace.
 */
#define DATASET_READ_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK 2
#define DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME "all_sel_file_hyper_sel_mem_read_test"
#define DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME  "all_sel_file_hyper_sel_mem_dset"
static int
test_read_dataset_all_file_hyper_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims = NULL;
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with all sel. for file space; hyperslab sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_READ_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK, dims, NULL)) <
                0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME,
                                      DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_ALL_FILE_HYPER_MEM_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc =
                    (data_size / DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_READ_ALL_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    /*
     * Only have rank 0 perform the dataset read, as reading the entire dataset on all ranks
     * might be stressful on system resources.
     */
    BEGIN_INDEPENDENT_OP(dset_read)
    {
        if (MAINPROCESS) {
            hsize_t start[1]  = {0};
            hsize_t stride[1] = {2};
            hsize_t count[1]  = {(hsize_t)space_npoints};
            hsize_t block[1]  = {1};
            hsize_t mdims[]   = {(hsize_t)(2 * space_npoints)};

            /*
             * Allocate twice the amount of memory needed and leave "holes" in the memory
             * buffer in order to prove that the mapping from all selection <-> hyperslab
             * selection works correctly.
             */
            read_buf_size = (size_t)(2 * space_npoints) * DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DTYPE_SIZE;
            if (NULL == (read_buf = calloc(1, read_buf_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                H5_FAILED();
                printf("    couldn't create memory dataspace\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
                H5_FAILED();
                printf("    couldn't select hyperslab for dataset read\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if (H5Dread(dset_id, DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                        H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_READ_ALL_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_read);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc = (size_t)(space_npoints / mpi_size);

                for (j = 0; j < 2 * elem_per_proc; j++) {
                    size_t idx = (i * 2 * elem_per_proc) + j;

                    if (j % 2 == 0) {
                        if (((int *)read_buf)[idx] != (int)i) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            INDEPENDENT_OP_ERROR(dset_read);
                        }
                    }
                    else {
                        if (((int *)read_buf)[idx] != 0) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            INDEPENDENT_OP_ERROR(dset_read);
                        }
                    }
                }
            }
        }
    }
    END_INDEPENDENT_OP(dset_read);

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * a point selection in the file dataspace and an all selection
 * in the memory dataspace.
 */
static int
test_read_dataset_point_file_all_mem(void)
{
    TESTING("read from dataset with point sel. for file space; all sel. for memory");

    SKIPPED();

    return 0;
}

/*
 * A test to ensure that a dataset can be read from by having
 * an all selection in the file dataspace and a point selection
 * in the memory dataspace.
 */
#define DATASET_READ_ALL_FILE_POINT_MEM_TEST_SPACE_RANK 2
#define DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_ALL_FILE_POINT_MEM_TEST_GROUP_NAME "all_sel_file_point_sel_mem_read_test"
#define DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME  "all_sel_file_point_sel_mem_dset"
static int
test_read_dataset_all_file_point_mem(void)
{
    hssize_t space_npoints;
    hsize_t *points = NULL;
    hsize_t *dims   = NULL;
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with all sel. for file space; point sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_ALL_FILE_POINT_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_ALL_FILE_POINT_MEM_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_ALL_FILE_POINT_MEM_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_READ_ALL_FILE_POINT_MEM_TEST_SPACE_RANK, dims, NULL)) <
                0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME,
                                      DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, fspace_id, H5P_DEFAULT,
                                      H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n", DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_ALL_FILE_POINT_MEM_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc =
                    (data_size / DATASET_READ_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_READ_ALL_FILE_POINT_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) <
        0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_READ_ALL_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    /*
     * Only have rank 0 perform the dataset read, as reading the entire dataset on all ranks
     * might be stressful on system resources.
     */
    BEGIN_INDEPENDENT_OP(dset_read)
    {
        if (MAINPROCESS) {
            hsize_t mdims[] = {(hsize_t)(2 * space_npoints)};
            size_t  j;

            /*
             * Allocate twice the amount of memory needed and leave "holes" in the memory
             * buffer in order to prove that the mapping from all selection <-> point
             * selection works correctly.
             */
            read_buf_size = (size_t)(2 * space_npoints) * DATASET_READ_ALL_FILE_POINT_MEM_TEST_DTYPE_SIZE;
            if (NULL == (read_buf = calloc(1, read_buf_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset read\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                H5_FAILED();
                printf("    couldn't create memory dataspace\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if (NULL == (points = malloc((size_t)space_npoints * sizeof(hsize_t)))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for point selection\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            /* Select every other point in the 1-dimensional memory dataspace */
            for (i = 0, j = 0; i < 2 * (size_t)space_npoints; i++) {
                if (i % 2 == 0)
                    points[j++] = (hsize_t)i;
            }

            if (H5Sselect_elements(mspace_id, H5S_SELECT_SET, (size_t)space_npoints, points) < 0) {
                H5_FAILED();
                printf("    couldn't set point selection for dataset read\n");
                INDEPENDENT_OP_ERROR(dset_read);
            }

            if (H5Dread(dset_id, DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                        H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_READ_ALL_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_read);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t elem_per_proc = (size_t)(space_npoints / mpi_size);

                for (j = 0; j < 2 * elem_per_proc; j++) {
                    size_t idx = (i * 2 * elem_per_proc) + j;

                    if (j % 2 == 0) {
                        if (((int *)read_buf)[idx] != (int)i) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            INDEPENDENT_OP_ERROR(dset_read);
                        }
                    }
                    else {
                        if (((int *)read_buf)[idx] != 0) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            INDEPENDENT_OP_ERROR(dset_read);
                        }
                    }
                }
            }
        }
    }
    END_INDEPENDENT_OP(dset_read);

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (points) {
        free(points);
        points = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * a hyperslab selection in the file dataspace and a point
 * selection in the memory dataspace.
 */
#define DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK 2
#define DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME "hyper_sel_file_point_sel_mem_read_test"
#define DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME  "hyper_sel_file_point_sel_mem_dset"
static int
test_read_dataset_hyper_file_point_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims   = NULL;
    hsize_t *points = NULL;
    hsize_t  start[DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  stride[DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  count[DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    hsize_t  block[DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK];
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with hyperslab sel. for file space; point sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id =
                     H5Screate_simple(DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME,
                                      DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc =
                    (data_size / DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id =
             H5Gopen2(container_group, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_READ_HYPER_FILE_POINT_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    /*
     * Allocate twice the amount of memory needed and leave "holes" in the memory
     * buffer in order to prove that the mapping from hyperslab selection <-> point
     * selection works correctly.
     */
    read_buf_size = (2 * (size_t)(space_npoints / mpi_size) * DATASET_READ_ONE_PROC_NONE_SEL_TEST_DTYPE_SIZE);
    if (NULL == (read_buf = calloc(1, read_buf_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    for (i = 0; i < DATASET_READ_HYPER_FILE_POINT_MEM_TEST_SPACE_RANK; i++) {
        if (i == 0) {
            start[i] = (hsize_t)mpi_rank;
            block[i] = 1;
        }
        else {
            start[i] = 0;
            block[i] = dims[i];
        }

        stride[i] = 1;
        count[i]  = 1;
    }

    if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
        H5_FAILED();
        printf("    couldn't select hyperslab for dataset read\n");
        goto error;
    }

    {
        hsize_t mdims[] = {(hsize_t)(2 * (space_npoints / mpi_size))};
        size_t  j;

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }

        if (NULL == (points = malloc((size_t)(space_npoints / mpi_size) * sizeof(hsize_t)))) {
            H5_FAILED();
            printf("    couldn't allocate buffer for point selection\n");
            goto error;
        }

        /* Select every other point in the 1-dimensional memory dataspace */
        for (i = 0, j = 0; i < (size_t)(2 * (space_npoints / mpi_size)); i++) {
            if (i % 2 == 0)
                points[j++] = (hsize_t)i;
        }

        if (H5Sselect_elements(mspace_id, H5S_SELECT_SET, (size_t)(space_npoints / mpi_size), points) < 0) {
            H5_FAILED();
            printf("    couldn't set point selection for dataset read\n");
            goto error;
        }
    }

    if (H5Dread(dset_id, DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_READ_HYPER_FILE_POINT_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)(2 * (space_npoints / mpi_size)); i++) {
        if (i % 2 == 0) {
            if (((int *)read_buf)[i] != (int)mpi_rank) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
        else {
            if (((int *)read_buf)[i] != 0) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (points) {
        free(points);
        points = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to ensure that a dataset can be read from by having
 * a point selection in the file dataspace and a hyperslab
 * selection in the memory dataspace.
 */
#define DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK 2
#define DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE H5T_NATIVE_INT
#define DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE sizeof(int)
#define DATASET_READ_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME "point_sel_file_hyper_sel_mem_read_test"
#define DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME  "point_sel_file_hyper_sel_mem_dset"
static int
test_read_dataset_point_file_hyper_mem(void)
{
    hssize_t space_npoints;
    hsize_t *dims   = NULL;
    hsize_t *points = NULL;
    size_t   i, data_size, read_buf_size;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    void    *read_buf  = NULL;

    TESTING("read from dataset with point sel. for file space; hyperslab sel. for memory");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    if (generate_random_parallel_dimensions(DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK, &dims) < 0)
        TEST_ERROR;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_READ_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id =
                     H5Screate_simple(DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME,
                                      DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, fspace_id,
                                      H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0, data_size = 1; i < DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK; i++)
                data_size *= dims[i];
            data_size *= DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE;

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < (size_t)mpi_size; i++) {
                size_t j;
                size_t elem_per_proc =
                    (data_size / DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE) / dims[0];

                for (j = 0; j < elem_per_proc; j++) {
                    size_t idx = (i * elem_per_proc) + j;

                    ((int *)write_buf)[idx] = (int)i;
                }
            }

            {
                hsize_t mdims[] = {data_size / DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE};

                if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
                    H5_FAILED();
                    printf("    couldn't create memory dataspace\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (H5Dwrite(dset_id, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }
            if (mspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(mspace_id);
                }
                H5E_END_TRY
                mspace_id = H5I_INVALID_HID;
            }
            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id =
             H5Gopen2(container_group, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_READ_POINT_FILE_HYPER_MEM_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id = H5Dopen2(group_id, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    if ((space_npoints = H5Sget_simple_extent_npoints(fspace_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataspace num points\n");
        goto error;
    }

    /*
     * Allocate twice the amount of memory needed and leave "holes" in the memory
     * buffer in order to prove that the mapping from point selection <-> hyperslab
     * selection works correctly.
     */
    read_buf_size =
        (2 * (size_t)(space_npoints / mpi_size) * DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DTYPE_SIZE);
    if (NULL == (read_buf = calloc(1, read_buf_size))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset read\n");
        goto error;
    }

    if (NULL == (points = malloc((size_t)((space_npoints / mpi_size) *
                                          DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK) *
                                 sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for point selection\n");
        goto error;
    }

    for (i = 0; i < (size_t)(space_npoints / mpi_size); i++) {
        size_t j;

        for (j = 0; j < DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK; j++) {
            size_t idx = (i * DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK) + j;

            if (j == 0)
                points[idx] = (hsize_t)mpi_rank;
            else if (j != DATASET_READ_POINT_FILE_HYPER_MEM_TEST_SPACE_RANK - 1)
                points[idx] = i / dims[j + 1];
            else
                points[idx] = i % dims[j];
        }
    }

    if (H5Sselect_elements(fspace_id, H5S_SELECT_SET, (size_t)(space_npoints / mpi_size), points) < 0) {
        H5_FAILED();
        printf("    couldn't set point selection for dataset read\n");
        goto error;
    }

    {
        hsize_t start[1]  = {0};
        hsize_t stride[1] = {2};
        hsize_t count[1]  = {(hsize_t)(space_npoints / mpi_size)};
        hsize_t block[1]  = {1};
        hsize_t mdims[]   = {(hsize_t)(2 * (space_npoints / mpi_size))};

        if ((mspace_id = H5Screate_simple(1, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    couldn't create memory dataspace\n");
            goto error;
        }

        if (H5Sselect_hyperslab(mspace_id, H5S_SELECT_SET, start, stride, count, block) < 0) {
            H5_FAILED();
            printf("    couldn't set hyperslab selection for dataset write\n");
            goto error;
        }
    }

    if (H5Dread(dset_id, DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_DTYPE, mspace_id, fspace_id, H5P_DEFAULT,
                read_buf) < 0) {
        H5_FAILED();
        printf("    couldn't read from dataset '%s'\n", DATASET_READ_POINT_FILE_HYPER_MEM_TEST_DSET_NAME);
        goto error;
    }

    for (i = 0; i < (size_t)(2 * (space_npoints / mpi_size)); i++) {
        if (i % 2 == 0) {
            if (((int *)read_buf)[i] != (int)mpi_rank) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
        else {
            if (((int *)read_buf)[i] != 0) {
                H5_FAILED();
                printf("    data verification failed\n");
                goto error;
            }
        }
    }

    if (read_buf) {
        free(read_buf);
        read_buf = NULL;
    }

    if (points) {
        free(points);
        points = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (read_buf)
            free(read_buf);
        if (write_buf)
            free(write_buf);
        if (points)
            free(points);
        if (dims)
            free(dims);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly. When reading back the
 * chunks of the dataset, the file dataspace and memory dataspace
 * used are the same shape. The dataset's first dimension grows
 * with the number of MPI ranks, while the other dimensions are fixed.
 */
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE                                   \
    100 /* Should be an even divisor of fixed dimension size */
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_FIXED_DIMSIZE   1000
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME                                            \
    "multi_chunk_dataset_write_same_space_read_test"
#define DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
static int
test_write_multi_chunk_dataset_same_shape_read(void)
{
    hsize_t *dims       = NULL;
    hsize_t *chunk_dims = NULL;
    hsize_t  retrieved_chunk_dims[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  start[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t   i, data_size, chunk_size, n_chunks_per_rank;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    int      read_buf[1][DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE];

    TESTING("write to dataset with multiple chunks using same shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    if (NULL ==
        (dims = malloc(DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK * sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    if (NULL == (chunk_dims = malloc(DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK *
                                     sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (i == 0) {
            dims[i]       = (hsize_t)mpi_size;
            chunk_dims[i] = 1;
        }
        else {
            dims[i]       = DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_FIXED_DIMSIZE;
            chunk_dims[i] = DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id =
                     H5Gcreate2(container_group, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                                              dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    failed to create DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                             chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to set chunking on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                                      DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                                      H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * See if a copy of the DCPL reports the correct chunking.
             */
            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    failed to close DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to retrieve copy of DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
            if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                             retrieved_chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to retrieve chunking info\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
                if (chunk_dims[i] != retrieved_chunk_dims[i]) {
                    H5_FAILED();
                    printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                           "dimensionality\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * Ensure that each underlying chunk contains the values
             *
             * chunk_index .. (chunk_nelemts - 1) + chunk_index.
             *
             * That is to say, for a chunk size of 10 x 10, chunk 0
             * contains the values
             *
             * 0 .. 99
             *
             * while the next chunk contains the values
             *
             * 1 .. 100
             *
             * and so on.
             */
            for (i = 0; i < data_size / DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
                size_t j;
                size_t base;
                size_t tot_adjust;

                /*
                 * Calculate a starting base value by taking the index value mod
                 * the size of a chunk in each dimension.
                 */
                for (j = 0, base = i; j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                    if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                        base %= chunk_dims[j];

                /*
                 * Calculate the adjustment in each dimension.
                 */
                for (j = 0, tot_adjust = 0;
                     j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                    if (j == (DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                        tot_adjust += (i % dims[j]) / chunk_dims[j];
                    else {
                        size_t k;
                        size_t n_faster_elemts;

                        /*
                         * Calculate the number of elements in faster dimensions.
                         */
                        for (k = j + 1, n_faster_elemts = 1;
                             k < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                            n_faster_elemts *= dims[k];

                        tot_adjust +=
                            (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                            (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                    }
                }

                ((int *)write_buf)[i] = (int)(base + tot_adjust);
            }

            /*
             * Write every chunk in the dataset.
             */
            if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id =
             H5Dopen2(group_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    /*
     * Create 2-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_dims[0], chunk_dims[1]};

        if ((mspace_id = H5Screate_simple(2, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    /*
     * Each rank reads their respective chunks in the dataset, checking the data for each one.
     */
    if (MAINPROCESS)
        printf("\n");
    for (i = 0, n_chunks_per_rank = (data_size / (size_t)mpi_size) / chunk_size; i < n_chunks_per_rank; i++) {
        size_t j, k;

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }

        if (MAINPROCESS)
            printf("\r All ranks reading chunk %zu", i);

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (j == 0)
                start[j] = (hsize_t)mpi_rank;
            else if (j == (DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                /* Fastest changing dimension */
                start[j] = (i * chunk_dims[j]) % dims[j];
            else
                start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
        }

        /*
         * Adjust file dataspace selection for next chunk.
         */
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to set hyperslab selection\n");
            goto error;
        }

        for (j = 0; j < chunk_dims[0]; j++)
            for (k = 0; k < chunk_dims[1]; k++)
                read_buf[j][k] = 0;

        if (H5Dread(dset_id, DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n",
                   DATASET_MULTI_CHUNK_WRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        for (j = 0; j < chunk_dims[0]; j++) {
            for (k = 0; k < chunk_dims[1]; k++) {
                size_t val =
                    ((j * chunk_dims[0]) + k + i) +
                    ((hsize_t)mpi_rank * n_chunks_per_rank); /* Additional value offset for each rank */
                if (read_buf[j][k] != (int)val) {
                    H5_FAILED();
                    printf("    data verification failed for chunk %lld\n", (long long)i);
                    goto error;
                }
            }
        }
    }

    if (chunk_dims) {
        free(chunk_dims);
        chunk_dims = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (write_buf)
            free(write_buf);
        if (chunk_dims)
            free(chunk_dims);
        if (dims)
            free(dims);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly. When reading back the
 * chunks of the dataset, the file dataspace and memory dataspace
 * used are differently shaped. The dataset's first dimension grows
 * with the number of MPI ranks, while the other dimensions are fixed.
 */
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE                                   \
    100 /* Should be an even divisor of fixed dimension size */
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE                                      \
    (DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE / 10)
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_FIXED_DIMSIZE   1000
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME                                            \
    "multi_chunk_dataset_write_diff_space_read_test"
#define DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
static int
test_write_multi_chunk_dataset_diff_shape_read(void)
{
    hsize_t *dims       = NULL;
    hsize_t *chunk_dims = NULL;
    hsize_t  retrieved_chunk_dims[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  start[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t   i, data_size, chunk_size, n_chunks_per_rank;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    int      read_buf[DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE]
                [DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE];

    TESTING("write to dataset with multiple chunks using differently shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    if (NULL ==
        (dims = malloc(DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK * sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    if (NULL == (chunk_dims = malloc(DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK *
                                     sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (i == 0) {
            dims[i]       = (hsize_t)mpi_size;
            chunk_dims[i] = 1;
        }
        else {
            dims[i]       = DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_FIXED_DIMSIZE;
            chunk_dims[i] = DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    /*
     * Have rank 0 create the dataset and completely fill it with data.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id =
                     H5Gcreate2(container_group, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                                              dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    failed to create DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                             chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to set chunking on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                                      DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, fspace_id,
                                      H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * See if a copy of the DCPL reports the correct chunking.
             */
            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    failed to close DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to retrieve copy of DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
            if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                             retrieved_chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to retrieve chunking info\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
                if (chunk_dims[i] != retrieved_chunk_dims[i]) {
                    H5_FAILED();
                    printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                           "dimensionality\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * Ensure that each underlying chunk contains the values
             *
             * chunk_index .. (chunk_nelemts - 1) + chunk_index.
             *
             * That is to say, for a chunk size of 10 x 10, chunk 0
             * contains the values
             *
             * 0 .. 99
             *
             * while the next chunk contains the values
             *
             * 1 .. 100
             *
             * and so on.
             */
            for (i = 0; i < data_size / DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE; i++) {
                size_t j;
                size_t base;
                size_t tot_adjust;

                /*
                 * Calculate a starting base value by taking the index value mod
                 * the size of a chunk in each dimension.
                 */
                for (j = 0, base = i; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                    if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                        base %= chunk_dims[j];

                /*
                 * Calculate the adjustment in each dimension.
                 */
                for (j = 0, tot_adjust = 0;
                     j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                    if (j == (DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                        tot_adjust += (i % dims[j]) / chunk_dims[j];
                    else {
                        size_t k;
                        size_t n_faster_elemts;

                        /*
                         * Calculate the number of elements in faster dimensions.
                         */
                        for (k = j + 1, n_faster_elemts = 1;
                             k < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                            n_faster_elemts *= dims[k];

                        tot_adjust +=
                            (((i / n_faster_elemts) / chunk_dims[j]) * (dims[j + 1] / chunk_dims[j + 1])) +
                            (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                    }
                }

                ((int *)write_buf)[i] = (int)(base + tot_adjust);
            }

            /*
             * Write every chunk in the dataset.
             */
            if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL, H5S_ALL,
                         H5P_DEFAULT, write_buf) < 0) {
                H5_FAILED();
                printf("    couldn't write to dataset '%s'\n",
                       DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (write_buf) {
                free(write_buf);
                write_buf = NULL;
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file to ensure that the data gets written.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    if ((dset_id =
             H5Dopen2(group_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open dataset '%s'\n", DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
        goto error;
    }

    if ((fspace_id = H5Dget_space(dset_id)) < 0) {
        H5_FAILED();
        printf("    couldn't get dataset dataspace\n");
        goto error;
    }

    /*
     * Create memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE,
                           DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE};

        if ((mspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                                          mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    /*
     * Each rank reads their respective chunks in the dataset, checking the data for each one.
     */
    if (MAINPROCESS)
        printf("\n");
    for (i = 0, n_chunks_per_rank = (data_size / (size_t)mpi_size) / chunk_size; i < n_chunks_per_rank; i++) {
        size_t j, k;

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }

        if (MAINPROCESS)
            printf("\r All ranks reading chunk %zu", i);

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
            if (j == 0)
                start[j] = (hsize_t)mpi_rank;
            else if (j == (DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                /* Fastest changing dimension */
                start[j] = (i * chunk_dims[j]) % dims[j];
            else
                start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
        }

        /*
         * Adjust file dataspace selection for next chunk.
         */
        if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
            H5_FAILED();
            printf("    failed to set hyperslab selection\n");
            goto error;
        }

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; j++)
            for (k = 0; k < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; k++)
                read_buf[j][k] = 0;

        if (H5Dread(dset_id, DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, mspace_id, fspace_id,
                    H5P_DEFAULT, read_buf) < 0) {
            H5_FAILED();
            printf("    couldn't read from dataset '%s'\n",
                   DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        for (j = 0; j < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; j++) {
            for (k = 0; k < DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; k++) {
                size_t val = ((j * DATASET_MULTI_CHUNK_WRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE) + k + i) +
                             ((hsize_t)mpi_rank * n_chunks_per_rank);

                if (read_buf[j][k] != (int)val) {
                    H5_FAILED();
                    printf("    data verification failed for chunk %lld\n", (long long)i);
                    goto error;
                }
            }
        }
    }

    if (chunk_dims) {
        free(chunk_dims);
        chunk_dims = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id) < 0)
        TEST_ERROR;
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (write_buf)
            free(write_buf);
        if (chunk_dims)
            free(chunk_dims);
        if (dims)
            free(dims);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly several times in a row.
 * When reading back the chunks of the dataset, the file
 * dataspace and memory dataspace used are the same shape.
 * The dataset's first dimension grows with the number of MPI
 * ranks, while the other dimensions are fixed.
 */
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE                               \
    100 /* Should be an even divisor of fixed dimension size */
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_FIXED_DIMSIZE   1000
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME                                        \
    "multi_chunk_dataset_same_space_overwrite_test"
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
#define DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_NITERS    10
static int
test_overwrite_multi_chunk_dataset_same_shape_read(void)
{
    hsize_t *dims       = NULL;
    hsize_t *chunk_dims = NULL;
    hsize_t  retrieved_chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  start[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t   i, data_size, chunk_size, n_chunks_per_rank;
    size_t   niter;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    int      read_buf[1][DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE];

    TESTING("several overwrites to dataset with multiple chunks using same shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    if (NULL == (dims = malloc(DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK *
                               sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    if (NULL == (chunk_dims = malloc(DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK *
                                     sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (i == 0) {
            dims[i]       = (hsize_t)mpi_size;
            chunk_dims[i] = 1;
        }
        else {
            dims[i]       = DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_FIXED_DIMSIZE;
            chunk_dims[i] = DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;

    /*
     * Have rank 0 create the dataset, but don't fill it with data yet.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group,
                                       DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(
                     DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    failed to create DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                             chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to set chunking on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /* Set dataset space allocation time to Early to ensure all chunk-related metadata is available to
             * all other processes when they open the dataset */
            if (H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0) {
                H5_FAILED();
                printf("    failed to set allocation time on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                                      DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE,
                                      fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * See if a copy of the DCPL reports the correct chunking.
             */
            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    failed to close DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to retrieve copy of DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
            if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK,
                             retrieved_chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to retrieve chunking info\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
                if (chunk_dims[i] != retrieved_chunk_dims[i]) {
                    H5_FAILED();
                    printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                           "dimensionality\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file on all ranks.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    /*
     * Create 2-dimensional memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {chunk_dims[0], chunk_dims[1]};

        if ((mspace_id = H5Screate_simple(2, mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    if (MAINPROCESS)
        printf("\n");
    for (niter = 0; niter < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_NITERS; niter++) {
        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        BEGIN_INDEPENDENT_OP(dset_write)
        {
            if (MAINPROCESS) {
                memset(write_buf, 0, data_size);

                /*
                 * Ensure that each underlying chunk contains the values
                 *
                 * chunk_index .. (chunk_nelemts - 1) + chunk_index.
                 *
                 * That is to say, for a chunk size of 10 x 10, chunk 0
                 * contains the values
                 *
                 * 0 .. 99
                 *
                 * while the next chunk contains the values
                 *
                 * 1 .. 100
                 *
                 * and so on. On each iteration, we add 1 to the previous
                 * values.
                 */
                for (i = 0; i < data_size / DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPESIZE;
                     i++) {
                    size_t j;
                    size_t base;
                    size_t tot_adjust;

                    /*
                     * Calculate a starting base value by taking the index value mod
                     * the size of a chunk in each dimension.
                     */
                    for (j = 0, base = i;
                         j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                        if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                            base %= chunk_dims[j];

                    /*
                     * Calculate the adjustment in each dimension.
                     */
                    for (j = 0, tot_adjust = 0;
                         j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                        if (j == (DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                            tot_adjust += (i % dims[j]) / chunk_dims[j];
                        else {
                            size_t k;
                            size_t n_faster_elemts;

                            /*
                             * Calculate the number of elements in faster dimensions.
                             */
                            for (k = j + 1, n_faster_elemts = 1;
                                 k < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                                n_faster_elemts *= dims[k];

                            tot_adjust += (((i / n_faster_elemts) / chunk_dims[j]) *
                                           (dims[j + 1] / chunk_dims[j + 1])) +
                                          (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                        }
                    }

                    ((int *)write_buf)[i] = (int)(base + tot_adjust + niter);
                }

                /*
                 * Write every chunk in the dataset.
                 */
                if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL,
                             H5S_ALL, H5P_DEFAULT, write_buf) < 0) {
                    H5_FAILED();
                    printf("    couldn't write to dataset '%s'\n",
                           DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                    INDEPENDENT_OP_ERROR(dset_write);
                }
            }
        }
        END_INDEPENDENT_OP(dset_write);

        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }

        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    couldn't get dataset dataspace\n");
            goto error;
        }

        /*
         * Each rank reads their respective chunks in the dataset, checking the data for each one.
         */
        for (i = 0, n_chunks_per_rank = (data_size / (size_t)mpi_size) / chunk_size; i < n_chunks_per_rank;
             i++) {
            size_t j, k;

            if (MAINPROCESS)
                printf("\r All ranks reading chunk %zu", i);

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (j == 0)
                    start[j] = (hsize_t)mpi_rank;
                else if (j == (DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    /* Fastest changing dimension */
                    start[j] = (i * chunk_dims[j]) % dims[j];
                else
                    start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
            }

            /*
             * Adjust file dataspace selection for next chunk.
             */
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
                H5_FAILED();
                printf("    failed to set hyperslab selection\n");
                goto error;
            }

            for (j = 0; j < chunk_dims[0]; j++)
                for (k = 0; k < chunk_dims[1]; k++)
                    read_buf[j][k] = 0;

            if (H5Dread(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_DTYPE, mspace_id,
                        fspace_id, H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_NAME);
                goto error;
            }

            for (j = 0; j < chunk_dims[0]; j++) {
                for (k = 0; k < chunk_dims[1]; k++) {
                    size_t val =
                        ((j * chunk_dims[0]) + k + i) +
                        ((hsize_t)mpi_rank * n_chunks_per_rank) /* Additional value offset for each rank */
                        + niter;
                    if (read_buf[j][k] != (int)val) {
                        H5_FAILED();
                        printf("    data verification failed for chunk %lld\n", (long long)i);
                        goto error;
                    }
                }
            }
        }

        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }
    }

    if (chunk_dims) {
        free(chunk_dims);
        chunk_dims = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (write_buf)
            free(write_buf);
        if (chunk_dims)
            free(chunk_dims);
        if (dims)
            free(dims);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

/*
 * A test to check that a dataset composed of multiple chunks
 * can be written and read correctly several times in a row.
 * When reading back the chunks of the dataset, the file
 * dataspace and memory dataspace used are differently shaped.
 * The dataset's first dimension grows with the number of MPI
 * ranks, while the other dimensions are fixed.
 */
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE                               \
    100 /* Should be an even divisor of fixed dimension size */
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE                                  \
    (DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE / 10)
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_FIXED_DIMSIZE   1000
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK 2
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE  sizeof(int)
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE      H5T_NATIVE_INT
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME                                        \
    "multi_chunk_dataset_diff_space_overwrite_test"
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME "multi_chunk_dataset"
#define DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_NITERS    10
static int
test_overwrite_multi_chunk_dataset_diff_shape_read(void)
{
    hsize_t *dims       = NULL;
    hsize_t *chunk_dims = NULL;
    hsize_t  retrieved_chunk_dims[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  start[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    hsize_t  count[DATASET_MULTI_CHUNK_OVERWRITE_SAME_SPACE_READ_TEST_DSET_SPACE_RANK];
    size_t   i, data_size, chunk_size, n_chunks_per_rank;
    size_t   niter;
    hid_t    file_id         = H5I_INVALID_HID;
    hid_t    fapl_id         = H5I_INVALID_HID;
    hid_t    container_group = H5I_INVALID_HID, group_id = H5I_INVALID_HID;
    hid_t    dset_id   = H5I_INVALID_HID;
    hid_t    dcpl_id   = H5I_INVALID_HID;
    hid_t    fspace_id = H5I_INVALID_HID;
    hid_t    mspace_id = H5I_INVALID_HID;
    void    *write_buf = NULL;
    int      read_buf[DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE]
                [DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE];

    TESTING("several overwrites to dataset with multiple chunks using differently shaped dataspaces");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GET_PLIST)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, or getting property list aren't "
               "supported with this connector\n");
        return 0;
    }

    if (NULL == (dims = malloc(DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK *
                               sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    if (NULL == (chunk_dims = malloc(DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK *
                                     sizeof(hsize_t)))) {
        H5_FAILED();
        printf("    couldn't allocate buffer for dataset dimensionality\n");
        goto error;
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        if (i == 0) {
            dims[i]       = (hsize_t)mpi_size;
            chunk_dims[i] = 1;
        }
        else {
            dims[i]       = DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_FIXED_DIMSIZE;
            chunk_dims[i] = DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_FIXED_CHUNK_DIMSIZE;
        }
    }

    for (i = 0, chunk_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        chunk_size *= chunk_dims[i];
    chunk_size *= DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    for (i = 0, data_size = 1; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++)
        data_size *= dims[i];
    data_size *= DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;

    /*
     * Have rank 0 create the dataset, but don't fill it with data yet.
     */
    BEGIN_INDEPENDENT_OP(dset_create)
    {
        if (MAINPROCESS) {
            if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open file '%s'\n", H5_api_test_parallel_filename);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((group_id = H5Gcreate2(container_group,
                                       DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                                       H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create container sub-group '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((fspace_id = H5Screate_simple(
                     DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK, dims, NULL)) < 0) {
                H5_FAILED();
                printf("    failed to create file dataspace for dataset\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
                H5_FAILED();
                printf("    failed to create DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (H5Pset_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                             chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to set chunking on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /* Set dataset space allocation time to Early to ensure all chunk-related metadata is available to
             * all other processes when they open the dataset */
            if (H5Pset_alloc_time(dcpl_id, H5D_ALLOC_TIME_EARLY) < 0) {
                H5_FAILED();
                printf("    failed to set allocation time on DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dset_id = H5Dcreate2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                                      DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE,
                                      fspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT)) < 0) {
                H5_FAILED();
                printf("    couldn't create dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                INDEPENDENT_OP_ERROR(dset_create);
            }

            /*
             * See if a copy of the DCPL reports the correct chunking.
             */
            if (H5Pclose(dcpl_id) < 0) {
                H5_FAILED();
                printf("    failed to close DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if ((dcpl_id = H5Dget_create_plist(dset_id)) < 0) {
                H5_FAILED();
                printf("    failed to retrieve copy of DCPL\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            memset(retrieved_chunk_dims, 0, sizeof(retrieved_chunk_dims));
            if (H5Pget_chunk(dcpl_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                             retrieved_chunk_dims) < 0) {
                H5_FAILED();
                printf("    failed to retrieve chunking info\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
                if (chunk_dims[i] != retrieved_chunk_dims[i]) {
                    H5_FAILED();
                    printf("    chunk dimensionality retrieved from DCPL didn't match originally specified "
                           "dimensionality\n");
                    INDEPENDENT_OP_ERROR(dset_create);
                }
            }

            if (NULL == (write_buf = malloc(data_size))) {
                H5_FAILED();
                printf("    couldn't allocate buffer for dataset write\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }

            if (fspace_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Sclose(fspace_id);
                }
                H5E_END_TRY
                fspace_id = H5I_INVALID_HID;
            }
            if (dcpl_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Pclose(dcpl_id);
                }
                H5E_END_TRY
                dcpl_id = H5I_INVALID_HID;
            }
            if (dset_id >= 0) {
                H5E_BEGIN_TRY
                {
                    H5Dclose(dset_id);
                }
                H5E_END_TRY
                dset_id = H5I_INVALID_HID;
            }

            /*
             * Close and re-open the file on all ranks.
             */
            if (H5Gclose(group_id) < 0) {
                H5_FAILED();
                printf("    failed to close test's container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Gclose(container_group) < 0) {
                H5_FAILED();
                printf("    failed to close container group\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
            if (H5Fclose(file_id) < 0) {
                H5_FAILED();
                printf("    failed to close file for data flushing\n");
                INDEPENDENT_OP_ERROR(dset_create);
            }
        }
    }
    END_INDEPENDENT_OP(dset_create);

    /*
     * Re-open file on all ranks.
     */
    if ((fapl_id = create_mpi_fapl(MPI_COMM_WORLD, MPI_INFO_NULL, true)) < 0)
        TEST_ERROR;
    if ((file_id = H5Fopen(H5_api_test_parallel_filename, H5F_ACC_RDWR, fapl_id)) < 0) {
        H5_FAILED();
        printf("    couldn't re-open file '%s'\n", H5_api_test_parallel_filename);
        goto error;
    }
    if ((container_group = H5Gopen2(file_id, DATASET_TEST_GROUP_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container group '%s'\n", DATASET_TEST_GROUP_NAME);
        goto error;
    }
    if ((group_id = H5Gopen2(container_group, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME,
                             H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    couldn't open container sub-group '%s'\n",
               DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_GROUP_NAME);
        goto error;
    }

    /*
     * Create memory dataspace for read buffer.
     */
    {
        hsize_t mdims[] = {DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE,
                           DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE};

        if ((mspace_id = H5Screate_simple(DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK,
                                          mdims, NULL)) < 0) {
            H5_FAILED();
            printf("    failed to create memory dataspace\n");
            goto error;
        }
    }

    for (i = 0; i < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; i++) {
        count[i] = chunk_dims[i];
    }

    if (MAINPROCESS)
        printf("\n");
    for (niter = 0; niter < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_NITERS; niter++) {
        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        BEGIN_INDEPENDENT_OP(dset_write)
        {
            if (MAINPROCESS) {
                memset(write_buf, 0, data_size);

                /*
                 * Ensure that each underlying chunk contains the values
                 *
                 * chunk_index .. (chunk_nelemts - 1) + chunk_index.
                 *
                 * That is to say, for a chunk size of 10 x 10, chunk 0
                 * contains the values
                 *
                 * 0 .. 99
                 *
                 * while the next chunk contains the values
                 *
                 * 1 .. 100
                 *
                 * and so on. On each iteration, we add 1 to the previous
                 * values.
                 */
                for (i = 0; i < data_size / DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPESIZE;
                     i++) {
                    size_t j;
                    size_t base;
                    size_t tot_adjust;

                    /*
                     * Calculate a starting base value by taking the index value mod
                     * the size of a chunk in each dimension.
                     */
                    for (j = 0, base = i;
                         j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++)
                        if (chunk_dims[j] > 1 && base >= chunk_dims[j])
                            base %= chunk_dims[j];

                    /*
                     * Calculate the adjustment in each dimension.
                     */
                    for (j = 0, tot_adjust = 0;
                         j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                        if (j == (DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                            tot_adjust += (i % dims[j]) / chunk_dims[j];
                        else {
                            size_t k;
                            size_t n_faster_elemts;

                            /*
                             * Calculate the number of elements in faster dimensions.
                             */
                            for (k = j + 1, n_faster_elemts = 1;
                                 k < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; k++)
                                n_faster_elemts *= dims[k];

                            tot_adjust += (((i / n_faster_elemts) / chunk_dims[j]) *
                                           (dims[j + 1] / chunk_dims[j + 1])) +
                                          (((i / n_faster_elemts) % chunk_dims[j]) * chunk_dims[j + 1]);
                        }
                    }

                    ((int *)write_buf)[i] = (int)(base + tot_adjust + niter);
                }

                /*
                 * Write every chunk in the dataset.
                 */
                if (H5Dwrite(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, H5S_ALL,
                             H5S_ALL, H5P_DEFAULT, write_buf) < 0) {
                    H5_FAILED();
                    printf("    couldn't write to dataset '%s'\n",
                           DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                    INDEPENDENT_OP_ERROR(dset_write);
                }
            }
        }
        END_INDEPENDENT_OP(dset_write);

        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }

        if ((dset_id = H5Dopen2(group_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME,
                                H5P_DEFAULT)) < 0) {
            H5_FAILED();
            printf("    couldn't open dataset '%s'\n",
                   DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
            goto error;
        }

        if ((fspace_id = H5Dget_space(dset_id)) < 0) {
            H5_FAILED();
            printf("    couldn't get dataset dataspace\n");
            goto error;
        }

        /*
         * Each rank reads their respective chunks in the dataset, checking the data for each one.
         */
        for (i = 0, n_chunks_per_rank = (data_size / (size_t)mpi_size) / chunk_size; i < n_chunks_per_rank;
             i++) {
            size_t j, k;

            if (MAINPROCESS)
                printf("\r All ranks reading chunk %zu", i);

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK; j++) {
                if (j == 0)
                    start[j] = (hsize_t)mpi_rank;
                else if (j == (DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_SPACE_RANK - 1))
                    /* Fastest changing dimension */
                    start[j] = (i * chunk_dims[j]) % dims[j];
                else
                    start[j] = ((i * chunk_dims[j + 1]) / dims[j + 1]) * (chunk_dims[j]);
            }

            /*
             * Adjust file dataspace selection for next chunk.
             */
            if (H5Sselect_hyperslab(fspace_id, H5S_SELECT_SET, start, NULL, count, NULL) < 0) {
                H5_FAILED();
                printf("    failed to set hyperslab selection\n");
                goto error;
            }

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; j++)
                for (k = 0; k < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; k++)
                    read_buf[j][k] = 0;

            if (H5Dread(dset_id, DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_DTYPE, mspace_id,
                        fspace_id, H5P_DEFAULT, read_buf) < 0) {
                H5_FAILED();
                printf("    couldn't read from dataset '%s'\n",
                       DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_DSET_NAME);
                goto error;
            }

            for (j = 0; j < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; j++) {
                for (k = 0; k < DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE; k++) {
                    size_t val =
                        ((j * DATASET_MULTI_CHUNK_OVERWRITE_DIFF_SPACE_READ_TEST_READ_BUF_DIMSIZE) + k + i) +
                        ((hsize_t)mpi_rank * n_chunks_per_rank) + niter;

                    if (read_buf[j][k] != (int)val) {
                        H5_FAILED();
                        printf("    data verification failed for chunk %lld\n", (long long)i);
                        goto error;
                    }
                }
            }
        }

        if (fspace_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Sclose(fspace_id);
            }
            H5E_END_TRY
            fspace_id = H5I_INVALID_HID;
        }
        if (dset_id >= 0) {
            H5E_BEGIN_TRY
            {
                H5Dclose(dset_id);
            }
            H5E_END_TRY
            dset_id = H5I_INVALID_HID;
        }

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            H5_FAILED();
            printf("    MPI_Barrier failed\n");
            goto error;
        }
    }

    if (chunk_dims) {
        free(chunk_dims);
        chunk_dims = NULL;
    }

    if (dims) {
        free(dims);
        dims = NULL;
    }

    if (write_buf) {
        free(write_buf);
        write_buf = NULL;
    }

    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Gclose(group_id) < 0)
        TEST_ERROR;
    if (H5Gclose(container_group) < 0)
        TEST_ERROR;
    if (H5Pclose(fapl_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        if (write_buf)
            free(write_buf);
        if (chunk_dims)
            free(chunk_dims);
        if (dims)
            free(dims);
        H5Pclose(dcpl_id);
        H5Sclose(mspace_id);
        H5Sclose(fspace_id);
        H5Dclose(dset_id);
        H5Gclose(group_id);
        H5Gclose(container_group);
        H5Pclose(fapl_id);
        H5Fclose(file_id);
    }
    H5E_END_TRY

    return 1;
}

int
H5_api_dataset_test_parallel(void)
{
    size_t i;
    int    nerrors;

    if (MAINPROCESS) {
        printf("**********************************************\n");
        printf("*                                            *\n");
        printf("*         API Parallel Dataset Tests         *\n");
        printf("*                                            *\n");
        printf("**********************************************\n\n");
    }

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(par_dataset_tests); i++) {
        nerrors += (*par_dataset_tests[i])() ? 1 : 0;

        if (MPI_SUCCESS != MPI_Barrier(MPI_COMM_WORLD)) {
            if (MAINPROCESS)
                printf("    MPI_Barrier() failed!\n");
        }
    }

    if (MAINPROCESS)
        printf("\n");

    return nerrors;
}
