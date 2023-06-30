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
 * Purpose:	Tests extendible datasets.
 */

#include "h5test.h"

static const char *FILENAME[] = {"extend", NULL};

#define N1X 100 /* USE AN EVEN NUMBER! */
#define N1Y 100 /* USE AN EVEN NUMBER! */
#define N2X (N1X / 2)
#define N2Y (N1Y / 2)

/* Data buffers */
static int **buf1      = NULL;
static int  *buf1_data = NULL;
static int **buf2      = NULL;
static int  *buf2_data = NULL;

/*-------------------------------------------------------------------------
 * Function:	write_data
 *
 * Purpose:	Create extendible dataset and test extend/write/read
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
write_data(const char *msg, hid_t file, const char *name, hid_t cparms, hid_t mem_space)
{
    hid_t                dataset, file_space, half_space;
    static const hsize_t dims[2]      = {N1X, N1Y};
    static const hsize_t half_dims[2] = {N2X, N2Y};
    hsize_t              size[2];
    hsize_t              max_size[2] = {0, 0};
    hsize_t              offset[2];
    int                  i, j, k, m;

    TESTING(msg);

    /* Create the dataset */
    if ((dataset = H5Dcreate2(file, name, H5T_NATIVE_INT, mem_space, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++) {

            /* Extend the dataset */
            offset[0] = (hsize_t)(i * N1X);
            offset[1] = (hsize_t)(j * N1Y);
            size[0]   = offset[0] + N1X;
            size[1]   = offset[1] + N1Y;
            if (size[0] > max_size[0] || size[1] > max_size[1]) {
                if (size[0] > max_size[0])
                    max_size[0] = size[0];
                if (size[1] > max_size[1])
                    max_size[1] = size[1];
                if (H5Dset_extent(dataset, max_size) < 0)
                    TEST_ERROR;
            } /* end if */

            /* Select a hyperslab */
            if ((file_space = H5Dget_space(dataset)) < 0)
                TEST_ERROR;
            if (H5Sselect_hyperslab(file_space, H5S_SELECT_SET, offset, NULL, dims, NULL) < 0)
                TEST_ERROR;

            /* Write to the hyperslab */
            if (H5Dwrite(dataset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, buf1_data) < 0)
                TEST_ERROR;
            if (H5Sclose(file_space) < 0)
                TEST_ERROR;
        } /* end for */

    /* Read the data */
    if ((half_space = H5Screate_simple(2, half_dims, NULL)) < 0)
        TEST_ERROR;
    if ((file_space = H5Dget_space(dataset)) < 0)
        TEST_ERROR;
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {

            /* Select a hyperslab */
            offset[0] = (hsize_t)(i * N2X);
            offset[1] = (hsize_t)(j * N2Y);
            if (H5Sselect_hyperslab(file_space, H5S_SELECT_SET, offset, NULL, half_dims, NULL) < 0)
                TEST_ERROR;

            /* Read */
            if (H5Dread(dataset, H5T_NATIVE_INT, half_space, file_space, H5P_DEFAULT, buf2_data) < 0)
                TEST_ERROR;

            /* Compare */
            for (k = 0; k < N2X; k++)
                for (m = 0; m < N2Y; m++)
                    if (buf2[k][m] != buf1[(i % 2) * N2X + k][(j % 2) * N2Y + m]) {
                        printf("    i=%d, j=%d, k=%d, m=%d\n", i, j, k, m);
                        printf("    buf2[%d][%d]=%d\n", k, m, buf2[k][m]);
                        printf("    buf1[%d][%d]=%d\n", (i % 2) * N2X + k, (j % 2) * N2Y + m,
                               buf1[(i % 2) * N2X + k][(j % 2) * N2Y + m]);
                        TEST_ERROR;
                    } /* end if */
        }             /* end for */
    }                 /* end for */

    /* Cleanup */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;
    if (H5Sclose(file_space) < 0)
        TEST_ERROR;
    if (H5Sclose(half_space) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end write_data() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:	write_data_deprec
 *
 * Purpose:	Create extendible dataset and test extend/write/read, with
 *              deprecated API routine (H5Dextend)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 *-------------------------------------------------------------------------
 */
static int
write_data_deprec(const char *msg, hid_t file, const char *name, hid_t cparms, hid_t mem_space)
{
    hid_t                dataset, file_space, half_space;
    static const hsize_t dims[2]      = {N1X, N1Y};
    static const hsize_t half_dims[2] = {N2X, N2Y};
    static hsize_t       size[2];
    hsize_t              offset[2];
    int                  i, j, k, m;

    TESTING(msg);

    /* Create the dataset */
    if ((dataset = H5Dcreate2(file, name, H5T_NATIVE_INT, mem_space, H5P_DEFAULT, cparms, H5P_DEFAULT)) < 0)
        TEST_ERROR;

    /* Write the data */
    for (i = 0; i < 5; i++)
        for (j = 0; j < 5; j++) {

            /* Extend the dataset */
            offset[0] = (hsize_t)(i * N1X);
            offset[1] = (hsize_t)(j * N1Y);
            size[0]   = offset[0] + N1X;
            size[1]   = offset[1] + N1Y;
            if (H5Dextend(dataset, size) < 0)
                TEST_ERROR;

            /* Select a hyperslab */
            if ((file_space = H5Dget_space(dataset)) < 0)
                TEST_ERROR;
            if (H5Sselect_hyperslab(file_space, H5S_SELECT_SET, offset, NULL, dims, NULL) < 0)
                TEST_ERROR;

            /* Write to the hyperslab */
            if (H5Dwrite(dataset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, buf1_data) < 0)
                TEST_ERROR;
            if (H5Sclose(file_space) < 0)
                TEST_ERROR;
        } /* end for */

    /* Read the data */
    if ((half_space = H5Screate_simple(2, half_dims, NULL)) < 0)
        TEST_ERROR;
    if ((file_space = H5Dget_space(dataset)) < 0)
        TEST_ERROR;
    for (i = 0; i < 10; i++) {
        for (j = 0; j < 10; j++) {

            /* Select a hyperslab */
            offset[0] = (hsize_t)(i * N2X);
            offset[1] = (hsize_t)(j * N2Y);
            if (H5Sselect_hyperslab(file_space, H5S_SELECT_SET, offset, NULL, half_dims, NULL) < 0)
                TEST_ERROR;

            /* Read */
            if (H5Dread(dataset, H5T_NATIVE_INT, half_space, file_space, H5P_DEFAULT, buf2_data) < 0)
                TEST_ERROR;

            /* Compare */
            for (k = 0; k < N2X; k++)
                for (m = 0; m < N2Y; m++)
                    if (buf2[k][m] != buf1[(i % 2) * N2X + k][(j % 2) * N2Y + m]) {
                        printf("    i=%d, j=%d, k=%d, m=%d\n", i, j, k, m);
                        printf("    buf2[%d][%d]=%d\n", k, m, buf2[k][m]);
                        printf("    buf1[%d][%d]=%d\n", (i % 2) * N2X + k, (j % 2) * N2Y + m,
                               buf1[(i % 2) * N2X + k][(j % 2) * N2Y + m]);
                        TEST_ERROR;
                    } /* end if */
        }             /* end for */
    }                 /* end for */

    /* Cleanup */
    if (H5Dclose(dataset) < 0)
        TEST_ERROR;
    if (H5Sclose(file_space) < 0)
        TEST_ERROR;
    if (H5Sclose(half_space) < 0)
        TEST_ERROR;

    PASSED();
    return 0;

error:
    return -1;
} /* end write_data_deprec() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests extendible datasets
 *
 * Return:      EXIT_SUCCESS/EXIT_FAILURE
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t                file, mem_space, cparms;
    hid_t                fapl;
    int                  nerrors       = 0;
    static const hsize_t dims[2]       = {N1X, N1Y};
    static const hsize_t chunk_dims[2] = {N2X, N2Y};
    static hsize_t       maxdims[2]    = {H5S_UNLIMITED, H5S_UNLIMITED};
    char                 filename[1024];
    int                  i, j;

    h5_reset();
    fapl = h5_fileaccess();

    /* Initialize buffer and space */
    if (NULL == (buf1_data = (int *)calloc(N1X * N1Y, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf1 = (int **)calloc(N1X, sizeof(buf1_data))))
        TEST_ERROR;
    for (i = 0; i < N1X; i++)
        buf1[i] = buf1_data + (i * N1Y);

    if (NULL == (buf2_data = (int *)calloc(N2X * N2Y, sizeof(int))))
        TEST_ERROR;
    if (NULL == (buf2 = (int **)calloc(N2X, sizeof(buf2_data))))
        TEST_ERROR;
    for (i = 0; i < N2X; i++)
        buf2[i] = buf2_data + (i * N2Y);

    for (i = 0; i < N1X; i++)
        for (j = 0; j < N1Y; j++)
            buf1[i][j] = i * N1Y + j;

    if ((mem_space = H5Screate_simple(2, dims, maxdims)) < 0)
        TEST_ERROR;

    /* Create the file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR;

    /* Create the dataset creation property list */
    if ((cparms = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        TEST_ERROR;
    if (H5Pset_chunk(cparms, 2, chunk_dims) < 0)
        TEST_ERROR;

    /* Test with incremental allocation */
    nerrors +=
        write_data("extendible dataset with incr. allocation", file, "dataset1a", cparms, mem_space) < 0 ? 1
                                                                                                         : 0;
#ifndef H5_NO_DEPRECATED_SYMBOLS
    nerrors += write_data_deprec("extendible dataset with incr. allocation w/deprec. symbols", file,
                                 "dataset1b", cparms, mem_space) < 0
                   ? 1
                   : 0;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    /* Test with early allocation */
    if (H5Pset_alloc_time(cparms, H5D_ALLOC_TIME_EARLY) < 0)
        TEST_ERROR;
    nerrors +=
        write_data("extendible dataset with early allocation", file, "dataset2a", cparms, mem_space) < 0 ? 1
                                                                                                         : 0;
#ifndef H5_NO_DEPRECATED_SYMBOLS
    nerrors += write_data_deprec("extendible dataset with early allocation w/deprec. symbols", file,
                                 "dataset2b", cparms, mem_space) < 0
                   ? 1
                   : 0;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    if (H5Pclose(cparms) < 0)
        TEST_ERROR;
    if (H5Sclose(mem_space) < 0)
        TEST_ERROR;
    if (H5Fclose(file) < 0)
        TEST_ERROR;

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if (nerrors) {
        printf("***** %d FAILURE%s! *****\n", nerrors, (1 == nerrors) ? "" : "S");
        exit(EXIT_FAILURE);
    } /* end if */

    printf("All extend tests passed.\n");
    h5_cleanup(FILENAME, fapl);

    free(buf1);
    free(buf1_data);
    free(buf2);
    free(buf2_data);

    exit(EXIT_SUCCESS);

error:

    free(buf1);
    free(buf1_data);
    free(buf2);
    free(buf2_data);

    printf("*** One or more extend tests failed ***\n");
    exit(EXIT_FAILURE);
} /* end main() */
