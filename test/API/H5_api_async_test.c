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

#include "H5_api_async_test.h"

#ifdef H5ESpublic_H

static int test_one_dataset_io(void);
static int test_multi_dataset_io(void);
static int test_multi_file_dataset_io(void);
static int test_multi_file_grp_dset_io(void);
static int test_set_extent(void);
static int test_attribute_exists(void);
static int test_attribute_io(void);
static int test_attribute_io_tconv(void);
static int test_attribute_io_compound(void);
static int test_group(void);
static int test_link(void);
static int test_ocopy_orefresh(void);
static int test_file_reopen(void);

/*
 * The array of async tests to be performed.
 */
static int (*async_tests[])(void) = {
    test_one_dataset_io,
    test_multi_dataset_io,
    test_multi_file_dataset_io,
    test_multi_file_grp_dset_io,
    test_set_extent,
    test_attribute_exists,
    test_attribute_io,
    test_attribute_io_tconv,
    test_attribute_io_compound,
    test_group,
    test_link,
    test_ocopy_orefresh,
    test_file_reopen,
};

/* Highest "printf" file created (starting at 0) */
int max_printf_file = -1;

/*
 * Create file and dataset, write to dataset
 */
static int
test_one_dataset_io(void)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   dset_id  = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hid_t   es_id    = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    int     wbuf[6][10];
    int     rbuf[6][10];
    int     i, j;

    TESTING_MULTIPART("single dataset I/O");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, or flush aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Create file asynchronously */
    if ((file_id = H5Fcreate_async(ASYNC_API_TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the dataset asynchronously */
    if ((dset_id = H5Dcreate_async(file_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                   H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(single_dset_eswait)
        {
            TESTING_2("synchronization using H5ESwait()");

            /* Initialize wbuf */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    wbuf[i][j] = 10 * i + j;

            /* Write the dataset asynchronously */
            if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_eswait);

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(single_dset_eswait);
            if (op_failed)
                PART_TEST_ERROR(single_dset_eswait);

            /* Read the dataset asynchronously */
            if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_eswait);

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(single_dset_eswait);
            if (op_failed)
                PART_TEST_ERROR(single_dset_eswait);

            /* Verify the read data */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    if (wbuf[i][j] != rbuf[i][j]) {
                        H5_FAILED();
                        printf("    data verification failed\n");
                        PART_ERROR(single_dset_eswait);
                    } /* end if */

            PASSED();
        }
        PART_END(single_dset_eswait);

        PART_BEGIN(single_dset_dclose)
        {
            TESTING_2("synchronization using H5Dclose()");

            /* Update wbuf */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    wbuf[i][j] += 6 * 10;

            /* Write the dataset asynchronously */
            if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            /* Close the dataset synchronously */
            if (H5Dclose(dset_id) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            /* Re-open the dataset asynchronously */
            if ((dset_id = H5Dopen_async(file_id, "dset", H5P_DEFAULT, es_id)) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            /* Read the dataset asynchronously */
            if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            /* Close the dataset synchronously */
            if (H5Dclose(dset_id) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            /* Verify the read data */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    if (wbuf[i][j] != rbuf[i][j]) {
                        H5_FAILED();
                        printf("    data verification failed\n");
                        PART_ERROR(single_dset_dclose);
                    } /* end if */

            /* Re-open the dataset asynchronously */
            if ((dset_id = H5Dopen_async(file_id, "dset", H5P_DEFAULT, es_id)) < 0)
                PART_TEST_ERROR(single_dset_dclose);

            PASSED();
        }
        PART_END(single_dset_dclose);

        PART_BEGIN(single_dset_dflush)
        {
            TESTING_2("synchronization using H5Oflush_async()");

            /* Update wbuf */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    wbuf[i][j] += 6 * 10;

            /* Write the dataset asynchronously */
            if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_dflush);

            /* Flush the dataset asynchronously.  This will effectively work as a
             * barrier, guaranteeing the read takes place after the write. */
            if (H5Oflush_async(dset_id, es_id) < 0)
                PART_TEST_ERROR(single_dset_dflush);

            /* Read the dataset asynchronously */
            if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_dflush);

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(single_dset_dflush);
            if (op_failed)
                PART_TEST_ERROR(single_dset_dflush);

            /* Verify the read data */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    if (wbuf[i][j] != rbuf[i][j]) {
                        H5_FAILED();
                        printf("    data verification failed\n");
                        PART_ERROR(single_dset_dflush);
                    } /* end if */

            PASSED();
        }
        PART_END(single_dset_dflush);

        PART_BEGIN(single_dset_fclose)
        {
            TESTING_2("synchronization using H5Fclose()");

            /* Update wbuf */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    wbuf[i][j] += 6 * 10;

            /* Write the dataset asynchronously */
            if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Close the dataset asynchronously */
            if (H5Dclose_async(dset_id, es_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Close the file synchronously */
            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Reopen the file asynchronously. */
            if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDONLY, H5P_DEFAULT, es_id)) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Re-open the dataset asynchronously */
            if ((dset_id = H5Dopen_async(file_id, "dset", H5P_DEFAULT, es_id)) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Read the dataset asynchronously */
            if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf, es_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Close the dataset asynchronously */
            if (H5Dclose_async(dset_id, es_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Close the file synchronously */
            if (H5Fclose(file_id) < 0)
                PART_TEST_ERROR(single_dset_fclose);

            /* Verify the read data */
            for (i = 0; i < 6; i++)
                for (j = 0; j < 10; j++)
                    if (wbuf[i][j] != rbuf[i][j]) {
                        H5_FAILED();
                        printf("    data verification failed\n");
                        PART_ERROR(single_dset_fclose);
                    } /* end if */

            PASSED();
        }
        PART_END(single_dset_fclose);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_one_dataset_io() */

/*
 * Create file and multiple datasets, write to them and read from them
 */
static int
test_multi_dataset_io(void)
{
    hid_t file_id    = H5I_INVALID_HID;
    hid_t dset_id[5] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};
    hid_t space_id   = H5I_INVALID_HID;
    hid_t es_id      = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    char    dset_name[32];
    int     wbuf[5][6][10];
    int     rbuf[5][6][10];
    int     i, j, k;

    TESTING_MULTIPART("multi dataset I/O");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, or flush aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Create file asynchronously */
    if ((file_id = H5Fcreate_async(ASYNC_API_TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(multi_dset_open)
        {
            TESTING_2("keeping datasets open");

            /* Loop over datasets */
            for (i = 0; i < 5; i++) {
                /* Set dataset name */
                snprintf(dset_name, sizeof(dset_name), "dset%d", i);

                /* Create the dataset asynchronously */
                if ((dset_id[i] = H5Dcreate_async(file_id, dset_name, H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                                  H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_dset_open);

                /* Initialize wbuf.  Must use a new slice of wbuf for each dset
                 * since we can't overwrite the buffers until I/O is done. */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] = 6 * 10 * i + 10 * j + k;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id[i], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i],
                                   es_id) < 0)
                    PART_TEST_ERROR(multi_dset_open);
            } /* end for */

            /* Flush the file asynchronously.  This will effectively work as a
             * barrier, guaranteeing the read takes place after the write. */
            if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
                PART_TEST_ERROR(multi_dset_open);

            /* Loop over datasets */
            for (i = 0; i < 5; i++) {
                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id[i], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_dset_open);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_dset_open);
            if (op_failed)
                PART_TEST_ERROR(multi_dset_open);
            /*printf("\nwbuf:\n");
            for(i = 0; i < 5; i++) {
                for(j = 0; j < 6; j++) {
                    for(k = 0; k < 10; k++)
                        printf("%d ", wbuf[i][j][k]);
                    printf("\n");
                }
                printf("\n");
            }
            printf("\nrbuf:\n");
            for(i = 0; i < 5; i++) {
                for(j = 0; j < 6; j++) {
                    for(k = 0; k < 10; k++)
                        printf("%d ", rbuf[i][j][k]);
                    printf("\n");
                }
                printf("\n");
            }*/
            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_dset_open);
                        } /* end if */

            /* Close the datasets */
            for (i = 0; i < 5; i++)
                if (H5Dclose(dset_id[i]) < 0)
                    PART_TEST_ERROR(multi_dset_open);

            PASSED();
        }
        PART_END(multi_dset_open);

        PART_BEGIN(multi_dset_close)
        {
            TESTING_2("closing datasets between I/O");

            /* Loop over datasets */
            for (i = 0; i < 5; i++) {
                /* Set dataset name */
                snprintf(dset_name, sizeof(dset_name), "dset%d", i);

                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id, dset_name, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_dset_close);

                /* Update wbuf */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] += 5 * 6 * 10;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i],
                                   es_id) < 0)
                    PART_TEST_ERROR(multi_dset_close);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_dset_close);
            } /* end for */

            /* Flush the file asynchronously.  This will effectively work as a
             * barrier, guaranteeing the read takes place after the write. */
            if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
                PART_TEST_ERROR(multi_dset_close);

            /* Loop over datasets */
            for (i = 0; i < 5; i++) {
                /* Set dataset name */
                snprintf(dset_name, sizeof(dset_name), "dset%d", i);

                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id, dset_name, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_dset_close);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_dset_close);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_dset_close);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_dset_close);
            if (op_failed)
                PART_TEST_ERROR(multi_dset_close);
            /*printf("\nwbuf:\n");
            for(i = 0; i < 5; i++) {
                for(j = 0; j < 6; j++) {
                    for(k = 0; k < 10; k++)
                        printf("%d ", wbuf[i][j][k]);
                    printf("\n");
                }
                printf("\n");
            }
            printf("\nrbuf:\n");
            for(i = 0; i < 5; i++) {
                for(j = 0; j < 6; j++) {
                    for(k = 0; k < 10; k++)
                        printf("%d ", rbuf[i][j][k]);
                    printf("\n");
                }
                printf("\n");
            }*/
            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_dset_close);
                        } /* end if */

            PASSED();
        }
        PART_END(multi_dset_close);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        for (i = 0; i < 5; i++)
            H5Dclose(dset_id[i]);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_multi_dataset_io() */

/*
 * Create multiple files, each with a single dataset, write to them and read
 * from them
 */
static int
test_multi_file_dataset_io(void)
{
    hid_t file_id[5] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};
    hid_t dset_id[5] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};
    hid_t space_id   = H5I_INVALID_HID;
    hid_t es_id      = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    char    file_name[32];
    int     wbuf[5][6][10];
    int     rbuf[5][6][10];
    int     i, j, k;

    TESTING_MULTIPART("multi file dataset I/O");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, or flush aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(multi_file_dset_open)
        {
            TESTING_2("keeping files and datasets open");

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Create file asynchronously */
                if ((file_id[i] =
                         H5Fcreate_async(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);
                if (i > max_printf_file)
                    max_printf_file = i;

                /* Create the dataset asynchronously */
                if ((dset_id[i] = H5Dcreate_async(file_id[i], "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                                  H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);

                /* Initialize wbuf.  Must use a new slice of wbuf for each dset
                 * since we can't overwrite the buffers until I/O is done. */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] = 6 * 10 * i + 10 * j + k;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id[i], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i],
                                   es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);
            } /* end for */

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Flush the dataset asynchronously.  This will effectively work as a
                 * barrier, guaranteeing the read takes place after the write. */
                if (H5Oflush_async(dset_id[i], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id[i], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_file_dset_open);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_dset_open);
            if (op_failed)
                PART_TEST_ERROR(multi_file_dset_open);

            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_file_dset_open);
                        } /* end if */

            /* Close the datasets */
            for (i = 0; i < 5; i++)
                if (H5Dclose(dset_id[i]) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);

            PASSED();
        }
        PART_END(multi_file_dset_open);

        PART_BEGIN(multi_file_dset_dclose)
        {
            TESTING_2("closing datasets between I/O");

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id[i], "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);

                /* Update wbuf */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] += 5 * 6 * 10;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i],
                                   es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);
            } /* end for */

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Flush the file asynchronously.  This will effectively work as a
                 * barrier, guaranteeing the read takes place after the write. */
                if (H5Fflush_async(file_id[i], H5F_SCOPE_LOCAL, es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_open);

                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id[i], "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_file_dset_dclose);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_dset_dclose);
            if (op_failed)
                PART_TEST_ERROR(multi_file_dset_dclose);

            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_file_dset_dclose);
                        } /* end if */

            /* Close the files */
            for (i = 0; i < 5; i++)
                if (H5Fclose(file_id[i]) < 0)
                    PART_TEST_ERROR(multi_file_dset_dclose);

            PASSED();
        }
        PART_END(multi_file_dset_dclose);

        PART_BEGIN(multi_file_dset_fclose)
        {
            TESTING_2("closing files between I/O");

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Open the file asynchronously */
                if ((file_id[0] = H5Fopen_async(file_name, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id[0], "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Update wbuf */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] += 5 * 6 * 10;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i],
                                   es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_dset_fclose);
            if (op_failed)
                PART_TEST_ERROR(multi_file_dset_fclose);

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Open the file asynchronously */
                if ((file_id[0] = H5Fopen_async(file_name, H5F_ACC_RDONLY, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Open the dataset asynchronously */
                if ((dset_id[0] = H5Dopen_async(file_id[0], "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id[0], H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id[0], es_id) < 0)
                    PART_TEST_ERROR(multi_file_dset_fclose);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_dset_fclose);
            if (op_failed)
                PART_TEST_ERROR(multi_file_dset_fclose);

            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_file_dset_fclose);
                        } /* end if */

            PASSED();
        }
        PART_END(multi_file_dset_fclose);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        for (i = 0; i < 5; i++) {
            H5Dclose(dset_id[i]);
            H5Fclose(file_id[i]);
        } /* end for */
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_multi_file_dataset_io() */

/*
 * Create multiple files, each with a single group and dataset, write to them
 * and read from them
 */
static int
test_multi_file_grp_dset_io(void)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   grp_id   = H5I_INVALID_HID;
    hid_t   dset_id  = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hid_t   es_id    = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    char    file_name[32];
    int     wbuf[5][6][10];
    int     rbuf[5][6][10];
    int     i, j, k;

    TESTING_MULTIPART("multi file dataset I/O with groups");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, group, or dataset aren't supported with this connector\n");
        return 0;
    }

    TESTING_2("test setup");

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    PASSED();

    BEGIN_MULTIPART
    {
        PART_BEGIN(multi_file_grp_dset_no_kick)
        {
            TESTING_2("without intermediate calls to H5ESwait()");

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Create file asynchronously */
                if ((file_id = H5Fcreate_async(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);
                if (i > max_printf_file)
                    max_printf_file = i;

                /* Create the group asynchronously */
                if ((grp_id = H5Gcreate_async(file_id, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Create the dataset asynchronously */
                if ((dset_id = H5Dcreate_async(grp_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                               H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Initialize wbuf.  Must use a new slice of wbuf for each dset
                 * since we can't overwrite the buffers until I/O is done. */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] = 6 * 10 * i + 10 * j + k;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the group asynchronously */
                if (H5Gclose_async(grp_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_grp_dset_no_kick);
            if (op_failed)
                PART_TEST_ERROR(multi_file_grp_dset_no_kick);

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Open the file asynchronously */
                if ((file_id = H5Fopen_async(file_name, H5F_ACC_RDONLY, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Open the group asynchronously */
                if ((grp_id = H5Gopen_async(file_id, "grp", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Open the dataset asynchronously */
                if ((dset_id = H5Dopen_async(grp_id, "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the group asynchronously */
                if (H5Gclose_async(grp_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_no_kick);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_grp_dset_no_kick);
            if (op_failed)
                PART_TEST_ERROR(multi_file_grp_dset_no_kick);

            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_file_grp_dset_no_kick);
                        } /* end if */

            PASSED();
        }
        PART_END(multi_file_grp_dset_no_kick);

        PART_BEGIN(multi_file_grp_dset_kick)
        {
            TESTING_2("with intermediate calls to H5ESwait() (0 timeout)");

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Create file asynchronously */
                if ((file_id = H5Fcreate_async(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);
                if (i > max_printf_file)
                    max_printf_file = i;

                /* Create the group asynchronously */
                if ((grp_id = H5Gcreate_async(file_id, "grp", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Create the dataset asynchronously */
                if ((dset_id = H5Dcreate_async(grp_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                               H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Update wbuf */
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        wbuf[i][j][k] += 5 * 6 * 10;

                /* Write the dataset asynchronously */
                if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf[i], es_id) <
                    0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the group asynchronously */
                if (H5Gclose_async(grp_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Kick the event stack to make progress */
                if (H5ESwait(es_id, 0, &num_in_progress, &op_failed) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);
                if (op_failed)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_grp_dset_kick);
            if (op_failed)
                PART_TEST_ERROR(multi_file_grp_dset_kick);

            /* Loop over files */
            for (i = 0; i < 5; i++) {
                /* Set file name */
                snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);

                /* Open the file asynchronously */
                if ((file_id = H5Fopen_async(file_name, H5F_ACC_RDONLY, H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Open the group asynchronously */
                if ((grp_id = H5Gopen_async(file_id, "grp", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Open the dataset asynchronously */
                if ((dset_id = H5Dopen_async(grp_id, "dset", H5P_DEFAULT, es_id)) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Read the dataset asynchronously */
                if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf[i], es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the dataset asynchronously */
                if (H5Dclose_async(dset_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the group asynchronously */
                if (H5Gclose_async(grp_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Close the file asynchronously */
                if (H5Fclose_async(file_id, es_id) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);

                /* Kick the event stack to make progress */
                if (H5ESwait(es_id, 0, &num_in_progress, &op_failed) < 0)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);
                if (op_failed)
                    PART_TEST_ERROR(multi_file_grp_dset_kick);
            } /* end for */

            /* Wait for the event stack to complete */
            if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
                PART_TEST_ERROR(multi_file_grp_dset_kick);
            if (op_failed)
                PART_TEST_ERROR(multi_file_grp_dset_kick);

            /* Verify the read data */
            for (i = 0; i < 5; i++)
                for (j = 0; j < 6; j++)
                    for (k = 0; k < 10; k++)
                        if (wbuf[i][j][k] != rbuf[i][j][k]) {
                            H5_FAILED();
                            printf("    data verification failed\n");
                            PART_ERROR(multi_file_grp_dset_kick);
                        } /* end if */

            PASSED();
        }
        PART_END(multi_file_grp_dset_kick);
    }
    END_MULTIPART;

    TESTING_2("test cleanup");

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Gclose(grp_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_multi_file_grp_dset_io() */

/*
 * Create file and dataset, write to dataset
 */
static int
test_set_extent(void)
{
    hid_t   file_id       = H5I_INVALID_HID;
    hid_t   dset_id       = H5I_INVALID_HID;
    hid_t   fspace_id[6]  = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                          H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};
    hid_t   fspace_out[6] = {H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID,
                           H5I_INVALID_HID, H5I_INVALID_HID, H5I_INVALID_HID};
    hid_t   mspace_id     = H5I_INVALID_HID;
    hid_t   dcpl_id       = H5I_INVALID_HID;
    hid_t   es_id         = H5I_INVALID_HID;
    hsize_t dims[2]       = {1, 10};
    hsize_t mdims[2]      = {7, 10};
    hsize_t cdims[2]      = {2, 3};
    hsize_t start[2]      = {0, 0};
    hsize_t count[2]      = {1, 10};
    size_t  num_in_progress;
    bool    op_failed;
    htri_t  tri_ret;
    int     wbuf[6][10];
    int     rbuf[6][10];
    int     i, j;

    TESTING("H5Dset_extent() and H5Dget_space()");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_MORE)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, dataset more, or flush aren't supported with "
               "this connector\n");
        return 0;
    }

    /* Create file dataspace */
    if ((fspace_id[0] = H5Screate_simple(2, dims, mdims)) < 0)
        TEST_ERROR;

    /* Create memory dataspace */
    if ((mspace_id = H5Screate_simple(1, &dims[1], NULL)) < 0)
        TEST_ERROR;

    /* Create DCPL */
    if ((dcpl_id = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;

    /* Set chunking */
    if (H5Pset_chunk(dcpl_id, 2, cdims) < 0)
        TEST_ERROR;

    /* Initialize wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            wbuf[i][j] = 10 * i + j;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Create file asynchronously */
    if ((file_id = H5Fcreate_async(ASYNC_API_TEST_FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the dataset asynchronously */
    if ((dset_id = H5Dcreate_async(file_id, "dset", H5T_NATIVE_INT, fspace_id[0], H5P_DEFAULT, dcpl_id,
                                   H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Extend the first dataset from 1 to 6, 1 at a time */
    for (i = 0; i < 6; i++) {
        /* No need to extend on the first iteration */
        if (i) {
            /* Copy dataspace */
            if ((fspace_id[i] = H5Scopy(fspace_id[i - 1])) < 0)
                TEST_ERROR;

            /* Extend datapace */
            dims[0] = (hsize_t)(i + 1);
            if (H5Sset_extent_simple(fspace_id[i], 2, dims, mdims) < 0)
                TEST_ERROR;

            /* Extend dataset asynchronously */
            if (H5Dset_extent_async(dset_id, dims, es_id) < 0)
                TEST_ERROR;

            /* Select hyperslab in file space to match new region */
            start[0] = (hsize_t)i;
            if (H5Sselect_hyperslab(fspace_id[i], H5S_SELECT_SET, start, NULL, count, NULL) < 0)
                TEST_ERROR;
        } /* end if */

        /* Get dataset dataspace */
        if ((fspace_out[i] = H5Dget_space_async(dset_id, es_id)) < 0)
            TEST_ERROR;

        /* Write the dataset slice asynchronously */
        if (H5Dwrite_async(dset_id, H5T_NATIVE_INT, mspace_id, fspace_id[i], H5P_DEFAULT, wbuf[i], es_id) < 0)
            TEST_ERROR;
    } /* end for */

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Oflush_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Read the entire dataset asynchronously */
    if (H5Dread_async(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Verify extents are correct.  We do not need to wait because of the
     * "future id" capability. */
    for (i = 0; i < 6; i++) {
        if ((tri_ret = H5Sextent_equal(fspace_id[i], fspace_out[i])) < 0)
            TEST_ERROR;
        if (!tri_ret)
            FAIL_PUTS_ERROR("    dataspaces are not equal\n");
        if (i && H5Sclose(fspace_id[i]) < 0)
            TEST_ERROR;
        if (H5Sclose(fspace_out[i]) < 0)
            TEST_ERROR;
    } /* end for */

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            if (wbuf[i][j] != rbuf[i][j])
                FAIL_PUTS_ERROR("    data verification failed\n");

    /*
     * Now try extending the dataset, closing it, reopening it, and getting the
     * space.
     */
    /* Extend datapace */
    dims[0] = (hsize_t)7;
    if (H5Sset_extent_simple(fspace_id[0], 2, dims, mdims) < 0)
        TEST_ERROR;

    /* Extend dataset asynchronously */
    if (H5Dset_extent_async(dset_id, dims, es_id) < 0)
        TEST_ERROR;

    /* Close dataset asynchronously */
    if (H5Dclose_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Open dataset asynchronously */
    if ((dset_id = H5Dopen_async(file_id, "dset", H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Get dataset dataspace asynchronously */
    if ((fspace_out[0] = H5Dget_space_async(dset_id, es_id)) < 0)
        TEST_ERROR;

    /* Verify the extents match */
    if ((tri_ret = H5Sextent_equal(fspace_id[0], fspace_out[0])) < 0)
        TEST_ERROR;
    if (!tri_ret)
        FAIL_PUTS_ERROR("    dataspaces are not equal\n");

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Close */
    if (H5Dclose(dset_id) < 0)
        TEST_ERROR;
    if (H5Fclose(file_id) < 0)
        TEST_ERROR;
    if (H5Sclose(mspace_id) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_id[0]) < 0)
        TEST_ERROR;
    if (H5Sclose(fspace_out[0]) < 0)
        TEST_ERROR;
    if (H5Pclose(dcpl_id) < 0)
        TEST_ERROR;
    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(mspace_id);
        for (i = 0; i < 6; i++) {
            H5Sclose(fspace_id[i]);
            H5Sclose(fspace_out[i]);
        } /* end for */
        H5Pclose(dcpl_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_set_extent() */

/*
 * Test H5Aexists()
 */
static int
test_attribute_exists(void)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   dset_id  = H5I_INVALID_HID;
    hid_t   attr_id  = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hid_t   es_id    = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    bool    exists1;
    bool    exists2;
    size_t  num_in_progress;
    bool    op_failed;

    TESTING("H5Aexists()");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, dataset more, attribute, or flush aren't "
               "supported with this connector\n");
        return 0;
    }

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the dataset asynchronously */
    if ((dset_id = H5Dcreate_async(file_id, "attr_exists_dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                   H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Check if the attribute exists asynchronously */
    if (H5Aexists_async(dset_id, "attr", &exists1, es_id) < 0)
        TEST_ERROR;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the create takes place after the existence check
     */
    if (H5Oflush_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Create the attribute asynchronously */
    if ((attr_id =
             H5Acreate_async(dset_id, "attr", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the existence check takes place after the create.
     */
    if (H5Oflush_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Check if the attribute exists asynchronously */
    if (H5Aexists_async(dset_id, "attr", &exists2, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Check if H5Aexists returned the correct values */
    if (exists1)
        FAIL_PUTS_ERROR("    H5Aexists returned true for an attribute that should not exist");
    if (!exists2)
        FAIL_PUTS_ERROR("    H5Aexists returned false for an attribute that should exist");

    /* Close */
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;
    if (H5Dclose_async(dset_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_attribute_io() */

/*
 * Create file, dataset, and attribute, write to attribute
 */
static int
test_attribute_io(void)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   dset_id  = H5I_INVALID_HID;
    hid_t   attr_id  = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hid_t   es_id    = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    int     wbuf[6][10];
    int     rbuf[6][10];
    int     i, j;

    TESTING("attribute I/O");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf("    API functions for basic file, dataset, dataset more, attribute, or flush aren't "
               "supported with this connector\n");
        return 0;
    }

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the dataset asynchronously */
    if ((dset_id = H5Dcreate_async(file_id, "attr_dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT,
                                   H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the attribute asynchronously */
    if ((attr_id =
             H5Acreate_async(dset_id, "attr", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Initialize wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            wbuf[i][j] = 10 * i + j;

    /* Write the attribute asynchronously */
    if (H5Awrite_async(attr_id, H5T_NATIVE_INT, wbuf, es_id) < 0)
        TEST_ERROR;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Oflush_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, H5T_NATIVE_INT, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            if (wbuf[i][j] != rbuf[i][j])
                FAIL_PUTS_ERROR("    data verification failed\n");

    /* Close the attribute asynchronously */
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;

    /* Open the attribute asynchronously */
    if ((attr_id = H5Aopen_async(dset_id, "attr", H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, H5T_NATIVE_INT, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            if (wbuf[i][j] != rbuf[i][j])
                FAIL_PUTS_ERROR("    data verification failed\n");

    /* Close out of order to see if it trips things up */
    if (H5Dclose_async(dset_id, es_id) < 0)
        TEST_ERROR;
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Dclose(dset_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_attribute_io() */

/*
 * Create file, dataset, and attribute, write to attribute with type conversion
 */
static int
test_attribute_io_tconv(void)
{
    hid_t   file_id  = H5I_INVALID_HID;
    hid_t   attr_id  = H5I_INVALID_HID;
    hid_t   space_id = H5I_INVALID_HID;
    hid_t   es_id    = H5I_INVALID_HID;
    hsize_t dims[2]  = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;
    int     wbuf[6][10];
    int     rbuf[6][10];
    int     i, j;

    TESTING("attribute I/O with type conversion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, attribute, or flush aren't supported with this connector\n");
        return 0;
    }

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the attribute asynchronously by name */
    if ((attr_id = H5Acreate_by_name_async(file_id, "attr_dset", "attr_tconv", H5T_STD_U16BE, space_id,
                                           H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Initialize wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            wbuf[i][j] = 10 * i + j;

    /* Write the attribute asynchronously */
    if (H5Awrite_async(attr_id, H5T_NATIVE_INT, wbuf, es_id) < 0)
        TEST_ERROR;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
        TEST_ERROR;

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, H5T_NATIVE_INT, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            if (wbuf[i][j] != rbuf[i][j])
                FAIL_PUTS_ERROR("    data verification failed\n");

    /* Close the attribute asynchronously */
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;

    /* Open the attribute asynchronously */
    if ((attr_id =
             H5Aopen_by_name_async(file_id, "attr_dset", "attr_tconv", H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, H5T_NATIVE_INT, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            if (wbuf[i][j] != rbuf[i][j])
                FAIL_PUTS_ERROR("    data verification failed\n");

    /* Close */
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Aclose(attr_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_attribute_io_tconv() */

/*
 * Create file, dataset, and attribute, write to attribute with compound type
 * conversion
 */
typedef struct tattr_cmpd_t {
    int a;
    int b;
} tattr_cmpd_t;

static int
test_attribute_io_compound(void)
{
    hid_t        file_id   = H5I_INVALID_HID;
    hid_t        attr_id   = H5I_INVALID_HID;
    hid_t        space_id  = H5I_INVALID_HID;
    hid_t        mtype_id  = H5I_INVALID_HID;
    hid_t        ftype_id  = H5I_INVALID_HID;
    hid_t        mtypea_id = H5I_INVALID_HID;
    hid_t        mtypeb_id = H5I_INVALID_HID;
    hid_t        es_id     = H5I_INVALID_HID;
    hsize_t      dims[2]   = {6, 10};
    size_t       num_in_progress;
    bool         op_failed;
    tattr_cmpd_t wbuf[6][10];
    tattr_cmpd_t rbuf[6][10];
    tattr_cmpd_t fbuf[6][10];
    int          i, j;

    TESTING("attribute I/O with compound type conversion");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_ATTR_BASIC)) {
        SKIPPED();
        printf(
            "    API functions for basic file, attribute, or flush aren't supported with this connector\n");
        return 0;
    }

    /* Create datatype */
    if ((mtype_id = H5Tcreate(H5T_COMPOUND, sizeof(tattr_cmpd_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(mtype_id, "a_name", HOFFSET(tattr_cmpd_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;
    if (H5Tinsert(mtype_id, "b_name", HOFFSET(tattr_cmpd_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((mtypea_id = H5Tcreate(H5T_COMPOUND, sizeof(tattr_cmpd_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(mtypea_id, "a_name", HOFFSET(tattr_cmpd_t, a), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((mtypeb_id = H5Tcreate(H5T_COMPOUND, sizeof(tattr_cmpd_t))) < 0)
        TEST_ERROR;
    if (H5Tinsert(mtypeb_id, "b_name", HOFFSET(tattr_cmpd_t, b), H5T_NATIVE_INT) < 0)
        TEST_ERROR;

    if ((ftype_id = H5Tcreate(H5T_COMPOUND, 2 + 8)) < 0)
        TEST_ERROR;
    if (H5Tinsert(ftype_id, "a_name", 0, H5T_STD_U16BE) < 0)
        TEST_ERROR;
    if (H5Tinsert(ftype_id, "b_name", 2, H5T_STD_I64LE) < 0)
        TEST_ERROR;

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the attribute asynchronously by name */
    if ((attr_id = H5Acreate_by_name_async(file_id, "attr_dset", "attr_cmpd", ftype_id, space_id, H5P_DEFAULT,
                                           H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Initialize wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            wbuf[i][j].a = 2 * (10 * i + j);
            wbuf[i][j].b = 2 * (10 * i + j) + 1;
        } /* end for */

    /* Write the attribute asynchronously */
    if (H5Awrite_async(attr_id, mtype_id, wbuf, es_id) < 0)
        TEST_ERROR;

    /* Update fbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            fbuf[i][j].a = wbuf[i][j].a;
            fbuf[i][j].b = wbuf[i][j].b;
        } /* end for */

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
        TEST_ERROR;

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, mtype_id, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            if (rbuf[i][j].a != fbuf[i][j].a)
                FAIL_PUTS_ERROR("    data verification failed\n");
            if (rbuf[i][j].b != fbuf[i][j].b)
                FAIL_PUTS_ERROR("    data verification failed\n");
        } /* end for */

    /* Clear the read buffer */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            rbuf[i][j].a = -2;
            rbuf[i][j].b = -2;
        } /* end for */

    /* Read the attribute asynchronously (element a only) */
    if (H5Aread_async(attr_id, mtypea_id, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            if (rbuf[i][j].a != fbuf[i][j].a)
                FAIL_PUTS_ERROR("    data verification failed\n");
            if (rbuf[i][j].b != -2)
                FAIL_PUTS_ERROR("    data verification failed\n");
        } /* end for */

    /* Clear the read buffer */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            rbuf[i][j].a = -2;
            rbuf[i][j].b = -2;
        } /* end for */

    /* Read the attribute asynchronously (element b only) */
    if (H5Aread_async(attr_id, mtypeb_id, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            if (rbuf[i][j].a != -2)
                FAIL_PUTS_ERROR("    data verification failed\n");
            if (rbuf[i][j].b != fbuf[i][j].b)
                FAIL_PUTS_ERROR("    data verification failed\n");
        } /* end for */

    /* Update wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            wbuf[i][j].a += 2 * 6 * 10;
            wbuf[i][j].b += 2 * 6 * 10;
        } /* end for */

    /* Write the attribute asynchronously (element a only) */
    if (H5Awrite_async(attr_id, mtypea_id, wbuf, es_id) < 0)
        TEST_ERROR;

    /* Update fbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            fbuf[i][j].a = wbuf[i][j].a;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
        TEST_ERROR;

    /* Clear the read buffer */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            rbuf[i][j].a = -2;
            rbuf[i][j].b = -2;
        } /* end for */

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, mtype_id, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            if (rbuf[i][j].a != fbuf[i][j].a)
                FAIL_PUTS_ERROR("    data verification failed\n");
            if (rbuf[i][j].b != fbuf[i][j].b)
                FAIL_PUTS_ERROR("    data verification failed\n");
        } /* end for */

    /* Update wbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            wbuf[i][j].a += 2 * 6 * 10;
            wbuf[i][j].b += 2 * 6 * 10;
        } /* end for */

    /* Write the attribute asynchronously (element b only) */
    if (H5Awrite_async(attr_id, mtypeb_id, wbuf, es_id) < 0)
        TEST_ERROR;

    /* Update fbuf */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++)
            fbuf[i][j].b = wbuf[i][j].b;

    /* Flush the dataset asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the write. */
    if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
        TEST_ERROR;

    /* Clear the read buffer */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            rbuf[i][j].a = -2;
            rbuf[i][j].b = -2;
        } /* end for */

    /* Read the attribute asynchronously */
    if (H5Aread_async(attr_id, mtype_id, rbuf, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify the read data */
    for (i = 0; i < 6; i++)
        for (j = 0; j < 10; j++) {
            if (rbuf[i][j].a != fbuf[i][j].a)
                FAIL_PUTS_ERROR("    data verification failed\n");
            if (rbuf[i][j].b != fbuf[i][j].b)
                FAIL_PUTS_ERROR("    data verification failed\n");
        } /* end for */

    /* Close */
    if (H5Aclose_async(attr_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Sclose(space_id) < 0)
        TEST_ERROR;
    if (H5Tclose(mtype_id) < 0)
        TEST_ERROR;
    if (H5Tclose(ftype_id) < 0)
        TEST_ERROR;
    if (H5Tclose(mtypea_id) < 0)
        TEST_ERROR;
    if (H5Tclose(mtypeb_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Sclose(space_id);
        H5Tclose(mtype_id);
        H5Tclose(ftype_id);
        H5Tclose(mtypea_id);
        H5Tclose(mtypeb_id);
        H5Aclose(attr_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_attribute_io_compound() */

/*
 * Test group interfaces
 */
static int
test_group(void)
{
    hid_t      file_id         = H5I_INVALID_HID;
    hid_t      parent_group_id = H5I_INVALID_HID;
    hid_t      group_id        = H5I_INVALID_HID;
    hid_t      subgroup_id     = H5I_INVALID_HID;
    hid_t      gcpl_id         = H5I_INVALID_HID;
    hid_t      es_id           = H5I_INVALID_HID;
    H5G_info_t info1;
    H5G_info_t info2;
    H5G_info_t info3;
    size_t     num_in_progress;
    bool       op_failed;

    TESTING("group operations");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_MORE) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, group, or group more aren't supported "
               "with this connector\n");
        return 0;
    }

    /* Create GCPL */
    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        TEST_ERROR;

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        /* Track creation order */
        if (H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
            TEST_ERROR;
    }

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the parent group asynchronously */
    if ((parent_group_id =
             H5Gcreate_async(file_id, "group_parent", H5P_DEFAULT, gcpl_id, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create 3 subgroups asynchronously, the first with no sub-subgroups, the
     * second with 1, and the third with 2 */
    if ((group_id =
             H5Gcreate_async(parent_group_id, "group1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;
    if (H5Gclose_async(group_id, es_id) < 0)
        TEST_ERROR;

    if ((group_id =
             H5Gcreate_async(parent_group_id, "group2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;
    if ((subgroup_id = H5Gcreate_async(group_id, "subgroup1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
        0)
        TEST_ERROR;
    if (H5Gclose_async(subgroup_id, es_id) < 0)
        TEST_ERROR;
    if (H5Gclose_async(group_id, es_id) < 0)
        TEST_ERROR;

    if ((group_id =
             H5Gcreate_async(parent_group_id, "group3", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;
    if ((subgroup_id = H5Gcreate_async(group_id, "subgroup1", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
        0)
        TEST_ERROR;
    if (H5Gclose_async(subgroup_id, es_id) < 0)
        TEST_ERROR;
    if ((subgroup_id = H5Gcreate_async(group_id, "subgroup2", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
        0)
        TEST_ERROR;
    if (H5Gclose_async(subgroup_id, es_id) < 0)
        TEST_ERROR;
    if (H5Gclose_async(group_id, es_id) < 0)
        TEST_ERROR;

    /* Flush the file asynchronously.  This will effectively work as a barrier,
     * guaranteeing the read takes place after the write. */
    if (H5Fflush_async(file_id, H5F_SCOPE_LOCAL, es_id) < 0)
        TEST_ERROR;

    /* Test H5Gget_info_async */
    /* Open group1 asynchronously */
    if ((group_id = H5Gopen_async(parent_group_id, "group1", H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Get info */
    if (H5Gget_info_async(group_id, &info1, es_id) < 0)
        TEST_ERROR;

    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        /* Test H5Gget_info_by_idx_async */
        if (H5Gget_info_by_idx_async(parent_group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, 1, &info2,
                                     H5P_DEFAULT, es_id) < 0)
            TEST_ERROR;
    }

    /* Test H5Gget_info_by_name_async */
    if (H5Gget_info_by_name_async(parent_group_id, "group3", &info3, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Verify group infos */
    if (info1.nlinks != 0)
        FAIL_PUTS_ERROR("    incorrect number of links");
    if (vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER) {
        if (info2.nlinks != 1)
            FAIL_PUTS_ERROR("    incorrect number of links");
    }
    if (info3.nlinks != 2)
        FAIL_PUTS_ERROR("    incorrect number of links");

    /* Close */
    if (H5Gclose_async(group_id, es_id) < 0)
        TEST_ERROR;
    if (H5Gclose_async(parent_group_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(subgroup_id);
        H5Gclose(group_id);
        H5Gclose(parent_group_id);
        H5Fclose(file_id);
        H5Pclose(gcpl_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_group() */

/*
 * Test link interfaces
 */
static int
test_link(void)
{
    hid_t  file_id         = H5I_INVALID_HID;
    hid_t  parent_group_id = H5I_INVALID_HID;
    hid_t  group_id        = H5I_INVALID_HID;
    hid_t  gcpl_id         = H5I_INVALID_HID;
    hid_t  es_id           = H5I_INVALID_HID;
    bool   existsh1;
    bool   existsh2;
    bool   existsh3;
    bool   existss1;
    bool   existss2;
    bool   existss3;
    size_t num_in_progress;
    bool   op_failed;

    TESTING("link operations");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_LINK_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_HARD_LINKS) || !(vol_cap_flags_g & H5VL_CAP_FLAG_SOFT_LINKS) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_CREATION_ORDER)) {
        SKIPPED();
        printf("    API functions for basic file, link, hard link, soft link, flush, or creation order "
               "aren't supported with this connector\n");
        return 0;
    }

    /* Create GCPL */
    if ((gcpl_id = H5Pcreate(H5P_GROUP_CREATE)) < 0)
        TEST_ERROR;

    /* Track creation order */
    if (H5Pset_link_creation_order(gcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the parent group asynchronously */
    if ((parent_group_id =
             H5Gcreate_async(file_id, "link_parent", H5P_DEFAULT, gcpl_id, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create subgroup asynchronously. */
    if ((group_id = H5Gcreate_async(parent_group_id, "group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) <
        0)
        TEST_ERROR;
    if (H5Gclose_async(group_id, es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the link to the subgroup is visible to later tasks.
     */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Create hard link asynchronously */
    if (H5Lcreate_hard_async(parent_group_id, "group", parent_group_id, "hard_link", H5P_DEFAULT, H5P_DEFAULT,
                             es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the soft link create takes place after the hard
     * link create. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Create soft link asynchronously */
    if (H5Lcreate_soft_async("/link_parent/group", parent_group_id, "soft_link", H5P_DEFAULT, H5P_DEFAULT,
                             es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the writes. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Check if hard link exists */
    if (H5Lexists_async(parent_group_id, "hard_link", &existsh1, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Check if soft link exists */
    if (H5Lexists_async(parent_group_id, "soft_link", &existss1, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the delete takes place after the reads. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Delete soft link by index */
    if (H5Ldelete_by_idx_async(parent_group_id, ".", H5_INDEX_CRT_ORDER, H5_ITER_INC, 2, H5P_DEFAULT, es_id) <
        0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the delete. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Check if hard link exists */
    if (H5Lexists_async(parent_group_id, "hard_link", &existsh2, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Check if soft link exists */
    if (H5Lexists_async(parent_group_id, "soft_link", &existss2, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the delete takes place after the reads. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Delete hard link */
    if (H5Ldelete_async(parent_group_id, "hard_link", H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the read takes place after the delete. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Check if hard link exists */
    if (H5Lexists_async(parent_group_id, "hard_link", &existsh3, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Check if soft link exists */
    if (H5Lexists_async(parent_group_id, "soft_link", &existss3, H5P_DEFAULT, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Check if existence returns were correct */
    if (!existsh1)
        FAIL_PUTS_ERROR("    link exists returned false for link that should exist");
    if (!existss1)
        FAIL_PUTS_ERROR("    link exists returned false for link that should exist");
    if (!existsh2)
        FAIL_PUTS_ERROR("    link exists returned false for link that should exist");
    if (existss2)
        FAIL_PUTS_ERROR("    link exists returned true for link that should not exist");
    if (existsh3)
        FAIL_PUTS_ERROR("    link exists returned true for link that should not exist");
    if (existsh3)
        FAIL_PUTS_ERROR("    link exists returned true for link that should not exist");

    /* Close */
    if (H5Gclose_async(parent_group_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Pclose(gcpl_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Gclose(group_id);
        H5Gclose(parent_group_id);
        H5Fclose(file_id);
        H5Pclose(gcpl_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_link() */

/*
 * Test H5Ocopy() and H5Orefresh()
 */
static int
test_ocopy_orefresh(void)
{
    hid_t   file_id         = H5I_INVALID_HID;
    hid_t   parent_group_id = H5I_INVALID_HID;
    hid_t   dset_id         = H5I_INVALID_HID;
    hid_t   space_id        = H5I_INVALID_HID;
    hid_t   es_id           = H5I_INVALID_HID;
    hsize_t dims[2]         = {6, 10};
    size_t  num_in_progress;
    bool    op_failed;

    TESTING("H5Ocopy() and H5Orefresh()");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_GROUP_BASIC) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_DATASET_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_OBJECT_MORE) ||
        !(vol_cap_flags_g & H5VL_CAP_FLAG_FLUSH_REFRESH)) {
        SKIPPED();
        printf("    API functions for basic file, group, dataset, object more, flush, or refresh aren't "
               "supported with this connector\n");
        return 0;
    }

    /* Create dataspace */
    if ((space_id = H5Screate_simple(2, dims, NULL)) < 0)
        TEST_ERROR;

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create the parent group asynchronously */
    if ((parent_group_id =
             H5Gcreate_async(file_id, "ocopy_parent", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Create dataset asynchronously. */
    if ((dset_id = H5Dcreate_async(parent_group_id, "dset", H5T_NATIVE_INT, space_id, H5P_DEFAULT,
                                   H5P_DEFAULT, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;
    if (H5Dclose_async(dset_id, es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the copy takes place after dataset create. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Copy dataset */
    if (H5Ocopy_async(parent_group_id, "dset", parent_group_id, "copied_dset", H5P_DEFAULT, H5P_DEFAULT,
                      es_id) < 0)
        TEST_ERROR;

    /* Flush the parent group asynchronously.  This will effectively work as a
     * barrier, guaranteeing the dataset open takes place copy. */
    if (H5Oflush_async(parent_group_id, es_id) < 0)
        TEST_ERROR;

    /* Open the copied dataset asynchronously */
    if ((dset_id = H5Dopen_async(parent_group_id, "copied_dset", H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Refresh the copied dataset asynchronously */
    if (H5Orefresh(dset_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Close */
    if (H5Dclose_async(dset_id, es_id) < 0)
        TEST_ERROR;
    if (H5Gclose_async(parent_group_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Dclose(dset_id);
        H5Gclose(parent_group_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_ocopy_orefresh() */

/*
 * Test H5Freopen()
 */
static int
test_file_reopen(void)
{
    hid_t  file_id          = H5I_INVALID_HID;
    hid_t  reopened_file_id = H5I_INVALID_HID;
    hid_t  es_id            = H5I_INVALID_HID;
    size_t num_in_progress;
    bool   op_failed;

    TESTING("H5Freopen()");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_BASIC) || !(vol_cap_flags_g & H5VL_CAP_FLAG_FILE_MORE)) {
        SKIPPED();
        printf("    API functions for basic file or file more aren't supported with this connector\n");
        return 0;
    }

    /* Create event stack */
    if ((es_id = H5EScreate()) < 0)
        TEST_ERROR;

    /* Open file asynchronously */
    if ((file_id = H5Fopen_async(ASYNC_API_TEST_FILE, H5F_ACC_RDWR, H5P_DEFAULT, es_id)) < 0)
        TEST_ERROR;

    /* Reopen file asynchronously */
    if ((reopened_file_id = H5Freopen_async(file_id, es_id)) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    /* Close */
    if (H5Fclose_async(reopened_file_id, es_id) < 0)
        TEST_ERROR;
    if (H5Fclose_async(file_id, es_id) < 0)
        TEST_ERROR;

    /* Wait for the event stack to complete */
    if (H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed) < 0)
        TEST_ERROR;
    if (op_failed)
        TEST_ERROR;

    if (H5ESclose(es_id) < 0)
        TEST_ERROR;

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY
    {
        H5Fclose(reopened_file_id);
        H5Fclose(file_id);
        H5ESwait(es_id, H5_API_TEST_WAIT_FOREVER, &num_in_progress, &op_failed);
        H5ESclose(es_id);
    }
    H5E_END_TRY

    return 1;
} /* end test_file_reopen() */

/*
 * Cleanup temporary test files
 */
static void
cleanup_files(void)
{
    char file_name[64];
    int  i;

    H5Fdelete(ASYNC_API_TEST_FILE, H5P_DEFAULT);
    for (i = 0; i <= max_printf_file; i++) {
        snprintf(file_name, sizeof(file_name), ASYNC_API_TEST_FILE_PRINTF, i);
        H5Fdelete(file_name, H5P_DEFAULT);
    } /* end for */
}

int
H5_api_async_test(void)
{
    size_t i;
    int    nerrors;

    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*             API Async Tests                *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    /* Make sure the connector supports the API functions being tested */
    if (!(vol_cap_flags_g & H5VL_CAP_FLAG_ASYNC)) {
        SKIPPED();
        printf("    Async APIs aren't supported with this connector\n");
        return 0;
    }

    for (i = 0, nerrors = 0; i < ARRAY_LENGTH(async_tests); i++) {
        nerrors += (*async_tests[i])() ? 1 : 0;
    }

    printf("\n");

    printf("Cleaning up testing files\n");
    cleanup_files();

    return nerrors;
}

#else /* H5ESpublic_H */

int
H5_api_async_test(void)
{
    printf("**********************************************\n");
    printf("*                                            *\n");
    printf("*             API Async Tests                *\n");
    printf("*                                            *\n");
    printf("**********************************************\n\n");

    printf("SKIPPED due to no async support in HDF5 library\n");

    return 0;
}

#endif /* H5ESpublic_H */
